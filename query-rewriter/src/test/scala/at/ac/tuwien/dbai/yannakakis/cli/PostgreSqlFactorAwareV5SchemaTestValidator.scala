package at.ac.tuwien.dbai.yannakakis.cli

import scala.util.matching.Regex

/** Draft 2020-12 subset used by the factor-aware v5 artifact schema.
  *
  * The older test validator intentionally implements the smaller v1-v4
  * keyword set. Keeping the v5 validator separate prevents a test helper
  * change from weakening the byte-frozen legacy contract.
  */
private[cli] object PostgreSqlFactorAwareV5SchemaTestValidator {
  final case class Violation(path: String, message: String)

  def validate(
      instance: ujson.Value,
      schema: ujson.Value
  ): Vector[Violation] = {
    require(instance != null, "JSON instance must not be null")
    require(schema != null, "JSON schema must not be null")
    validateAt(instance, schema, schema, "$")
  }

  private def validateAt(
      instance: ujson.Value,
      schema: ujson.Value,
      root: ujson.Value,
      path: String
  ): Vector[Violation] = schema match {
    case ujson.Bool(true)  => Vector.empty
    case ujson.Bool(false) => Vector(Violation(path, "false schema"))
    case _: ujson.Obj =>
      val fields = schema.obj
      val errors = Vector.newBuilder[Violation]

      // Draft 2020-12 permits assertion siblings next to $ref.
      fields.get("$ref").foreach { reference =>
        validateAt(instance, resolve(root, reference.str), root, path)
          .foreach(errors += _)
      }
      fields.get("type").foreach { expected =>
        val accepted = expected match {
          case ujson.Str(name) => matchesType(instance, name)
          case values: ujson.Arr =>
            values.arr.exists(value => matchesType(instance, value.str))
          case _ => false
        }
        if (!accepted) errors += Violation(path, "value has the wrong type")
      }
      fields.get("const").foreach { expected =>
        if (instance != expected)
          errors += Violation(path, "value differs from const")
      }
      fields.get("enum").foreach { values =>
        if (!values.arr.contains(instance))
          errors += Violation(path, "value is absent from enum")
      }
      fields.get("allOf").foreach { alternatives =>
        alternatives.arr.foreach { alternative =>
          validateAt(instance, alternative, root, path).foreach(errors += _)
        }
      }
      fields.get("oneOf").foreach { alternatives =>
        val matches = alternatives.arr.count(alternative =>
          validateAt(instance, alternative, root, path).isEmpty
        )
        if (matches != 1)
          errors += Violation(path, s"oneOf matched $matches alternatives")
      }
      fields.get("not").foreach { forbidden =>
        if (validateAt(instance, forbidden, root, path).isEmpty)
          errors += Violation(path, "not schema matched")
      }
      fields.get("if").foreach { condition =>
        val branch =
          if (validateAt(instance, condition, root, path).isEmpty) "then"
          else "else"
        fields.get(branch).foreach { selected =>
          validateAt(instance, selected, root, path).foreach(errors += _)
        }
      }

      validateObject(instance, fields, root, path).foreach(errors += _)
      validateArray(instance, fields, root, path).foreach(errors += _)
      validateString(instance, fields, path).foreach(errors += _)
      validateNumber(instance, fields, path).foreach(errors += _)
      errors.result()
    case _ => Vector(Violation(path, "schema must be an object or boolean"))
  }

  private def validateObject(
      instance: ujson.Value,
      schema: collection.Map[String, ujson.Value],
      root: ujson.Value,
      path: String
  ): Vector[Violation] = instance match {
    case value: ujson.Obj =>
      val errors = Vector.newBuilder[Violation]
      schema.get("required").foreach { required =>
        required.arr.map(_.str).foreach { field =>
          if (!value.obj.contains(field))
            errors += Violation(child(path, field), "required property is absent")
        }
      }
      val properties = schema
        .get("properties")
        .map(_.obj.toMap)
        .getOrElse(Map.empty[String, ujson.Value])
      schema.get("propertyNames").foreach { nameSchema =>
        value.obj.keys.foreach { name =>
          validateAt(ujson.Str(name), nameSchema, root, child(path, name))
            .foreach(errors += _)
        }
      }
      value.obj.foreach { case (name, supplied) =>
        properties.get(name) match {
          case Some(propertySchema) =>
            validateAt(supplied, propertySchema, root, child(path, name))
              .foreach(errors += _)
          case None =>
            schema.get("additionalProperties") match {
              case Some(ujson.Bool(false)) =>
                errors += Violation(child(path, name), "additional property")
              case Some(propertySchema: ujson.Obj) =>
                validateAt(supplied, propertySchema, root, child(path, name))
                  .foreach(errors += _)
              case _ => ()
            }
        }
      }
      errors.result()
    case _ => Vector.empty
  }

  private def validateArray(
      instance: ujson.Value,
      schema: collection.Map[String, ujson.Value],
      root: ujson.Value,
      path: String
  ): Vector[Violation] = instance match {
    case value: ujson.Arr =>
      val errors = Vector.newBuilder[Violation]
      schema.get("minItems").foreach { minimum =>
        if (value.arr.size < minimum.num.toInt)
          errors += Violation(path, s"fewer than ${minimum.num.toInt} items")
      }
      schema.get("maxItems").foreach { maximum =>
        if (value.arr.size > maximum.num.toInt)
          errors += Violation(path, s"more than ${maximum.num.toInt} items")
      }
      schema.get("uniqueItems").foreach { unique =>
        if (unique.bool && value.arr.distinct.size != value.arr.size)
          errors += Violation(path, "array items are not unique")
      }

      val prefixCount = schema.get("prefixItems") match {
        case Some(prefix: ujson.Arr) =>
          prefix.arr.zipWithIndex.foreach { case (itemSchema, index) =>
            value.arr.lift(index).foreach { item =>
              validateAt(item, itemSchema, root, s"$path[$index]")
                .foreach(errors += _)
            }
          }
          prefix.arr.size
        case _ => 0
      }
      schema.get("items").foreach { itemSchema =>
        value.arr.drop(prefixCount).zipWithIndex.foreach {
          case (item, relativeIndex) =>
            val index = prefixCount + relativeIndex
            validateAt(item, itemSchema, root, s"$path[$index]")
              .foreach(errors += _)
        }
      }
      schema.get("contains").foreach { containedSchema =>
        val matches = value.arr.zipWithIndex.count { case (item, index) =>
          validateAt(item, containedSchema, root, s"$path[$index]").isEmpty
        }
        val minimum = schema.get("minContains").fold(1)(_.num.toInt)
        val maximum = schema.get("maxContains").map(_.num.toInt)
        if (matches < minimum || maximum.exists(matches > _))
          errors += Violation(
            path,
            s"contains matched $matches items outside [$minimum,${maximum.fold("unbounded")(_.toString)}]"
          )
      }
      errors.result()
    case _ => Vector.empty
  }

  private def validateString(
      instance: ujson.Value,
      schema: collection.Map[String, ujson.Value],
      path: String
  ): Vector[Violation] = instance match {
    case ujson.Str(value) =>
      val errors = Vector.newBuilder[Violation]
      schema.get("minLength").foreach { minimum =>
        if (value.codePointCount(0, value.length) < minimum.num.toInt)
          errors += Violation(path, s"string is shorter than ${minimum.num.toInt}")
      }
      schema.get("pattern").foreach { pattern =>
        val expression: Regex = pattern.str.r
        if (expression.findFirstIn(value).isEmpty)
          errors += Violation(path, s"string does not match ${pattern.str}")
      }
      errors.result()
    case _ => Vector.empty
  }

  private def validateNumber(
      instance: ujson.Value,
      schema: collection.Map[String, ujson.Value],
      path: String
  ): Vector[Violation] = instance match {
    case ujson.Num(value) =>
      val errors = Vector.newBuilder[Violation]
      schema.get("minimum").foreach { minimum =>
        if (value < minimum.num)
          errors += Violation(path, s"number is below ${minimum.num}")
      }
      schema.get("maximum").foreach { maximum =>
        if (value > maximum.num)
          errors += Violation(path, s"number is above ${maximum.num}")
      }
      errors.result()
    case _ => Vector.empty
  }

  private def matchesType(value: ujson.Value, expected: String): Boolean =
    expected match {
      case "object"  => value.isInstanceOf[ujson.Obj]
      case "array"   => value.isInstanceOf[ujson.Arr]
      case "string"  => value.isInstanceOf[ujson.Str]
      case "boolean" => value.isInstanceOf[ujson.Bool]
      case "null"    => value == ujson.Null
      case "number"  => value.isInstanceOf[ujson.Num]
      case "integer" =>
        value match {
          case ujson.Num(number) => number.isWhole
          case _                 => false
        }
      case _ => false
    }

  private def resolve(root: ujson.Value, reference: String): ujson.Value = {
    require(reference.startsWith("#/"), s"unsupported schema reference $reference")
    reference
      .stripPrefix("#/")
      .split('/')
      .foldLeft(root) { (current, encoded) =>
        current(encoded.replace("~1", "/").replace("~0", "~"))
      }
  }

  private def child(path: String, name: String): String =
    s"$path.${name.replace(".", "\\.")}"
}
