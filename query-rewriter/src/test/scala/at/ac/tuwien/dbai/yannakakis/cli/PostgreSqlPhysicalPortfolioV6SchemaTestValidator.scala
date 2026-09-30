package at.ac.tuwien.dbai.yannakakis.cli

import scala.util.matching.Regex

/** Draft 2020-12 subset used by the physical-portfolio v6 schema.
  *
  * This stays separate from the byte-frozen legacy and v5 test validators.
  * V6 additionally needs `maxLength` for its collision-only SQL witness.
  */
private[cli] object PostgreSqlPhysicalPortfolioV6SchemaTestValidator {
  final case class Violation(path: String, message: String)

  /**
    * Identity-keyed validity memoization for combinator probes. Structural
    * hashing is deliberately avoided: v6 instances contain large rendering
    * and evidence vectors, and `oneOf` only needs a yes/no answer for each
    * exact instance/schema pair.
    */
  private final class ValidityCache {
    private val instances =
      new java.util.IdentityHashMap[
        ujson.Value,
        java.util.IdentityHashMap[ujson.Value, java.lang.Boolean]
      ]()

    def get(instance: ujson.Value, schema: ujson.Value): Option[Boolean] = {
      val schemas = instances.get(instance)
      if (schemas == null) None
      else Option(schemas.get(schema)).map(_.booleanValue())
    }

    def put(instance: ujson.Value, schema: ujson.Value, valid: Boolean): Unit = {
      var schemas = instances.get(instance)
      if (schemas == null) {
        schemas =
          new java.util.IdentityHashMap[ujson.Value, java.lang.Boolean]()
        instances.put(instance, schemas)
      }
      schemas.put(schema, java.lang.Boolean.valueOf(valid))
    }
  }

  def validate(
      instance: ujson.Value,
      schema: ujson.Value
  ): Vector[Violation] = {
    require(instance != null, "JSON instance must not be null")
    require(schema != null, "JSON schema must not be null")
    validateAt(instance, schema, schema, "$", new ValidityCache)
  }

  private def validateAt(
      instance: ujson.Value,
      schema: ujson.Value,
      root: ujson.Value,
      path: String,
      cache: ValidityCache
  ): Vector[Violation] = schema match {
    case ujson.Bool(true)  => Vector.empty
    case ujson.Bool(false) => Vector(Violation(path, "false schema"))
    case _: ujson.Obj =>
      val fields = schema.obj
      val errors = Vector.newBuilder[Violation]

      // A `properties` discriminator is conjunctive with `$ref` and the other
      // keywords. Checking its cheap local constraints first lets disjoint
      // oneOf arms fail before recursively traversing the large shared report
      // definition. The complete selected arm is still validated below.
      val cheapErrors = validateCheap(instance, fields, path)
      if (cheapErrors.nonEmpty) return cheapErrors

      fields.get("$ref").foreach { reference =>
        validateReferenced(
          instance,
          resolve(root, reference.str),
          root,
          path,
          cache
        ).foreach(errors += _)
      }
      fields.get("allOf").foreach { alternatives =>
        alternatives.arr.foreach { alternative =>
          validateReferenced(instance, alternative, root, path, cache)
            .foreach(errors += _)
        }
      }
      fields.get("oneOf").foreach { alternatives =>
        val iterator = alternatives.arr.iterator
        var matches = 0
        while (iterator.hasNext && matches < 2) {
          if (isValid(instance, iterator.next(), root, cache)) matches += 1
        }
        if (matches != 1)
          errors += Violation(path, s"oneOf matched $matches alternatives")
      }
      fields.get("not").foreach { forbidden =>
        if (isValid(instance, forbidden, root, cache))
          errors += Violation(path, "not schema matched")
      }
      fields.get("if").foreach { condition =>
        val branch =
          if (isValid(instance, condition, root, cache)) "then"
          else "else"
        fields.get(branch).foreach { selected =>
          validateReferenced(instance, selected, root, path, cache)
            .foreach(errors += _)
        }
      }

      validateObject(instance, fields, root, path, cache).foreach(errors += _)
      validateArray(instance, fields, root, path, cache).foreach(errors += _)
      validateString(instance, fields, path).foreach(errors += _)
      validateNumber(instance, fields, path).foreach(errors += _)
      errors.result()
    case _ => Vector(Violation(path, "schema must be an object or boolean"))
  }

  private def validateObject(
      instance: ujson.Value,
      schema: collection.Map[String, ujson.Value],
      root: ujson.Value,
      path: String,
      cache: ValidityCache
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
          validateReferenced(
            ujson.Str(name),
            nameSchema,
            root,
            child(path, name),
            cache
          ).foreach(errors += _)
        }
      }
      value.obj.foreach { case (name, supplied) =>
        properties.get(name) match {
          case Some(propertySchema) =>
            validateReferenced(
              supplied,
              propertySchema,
              root,
              child(path, name),
              cache
            )
              .foreach(errors += _)
          case None =>
            schema.get("additionalProperties") match {
              case Some(ujson.Bool(false)) =>
                errors += Violation(child(path, name), "additional property")
              case Some(propertySchema: ujson.Obj) =>
                validateReferenced(
                  supplied,
                  propertySchema,
                  root,
                  child(path, name),
                  cache
                )
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
      path: String,
      cache: ValidityCache
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
              validateReferenced(
                item,
                itemSchema,
                root,
                s"$path[$index]",
                cache
              )
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
            validateReferenced(
              item,
              itemSchema,
              root,
              s"$path[$index]",
              cache
            )
              .foreach(errors += _)
        }
      }
      schema.get("contains").foreach { containedSchema =>
        val matches = value.arr.count { item =>
          isValid(item, containedSchema, root, cache)
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
      val codePointCount = value.codePointCount(0, value.length)
      schema.get("minLength").foreach { minimum =>
        if (codePointCount < minimum.num.toInt)
          errors += Violation(path, s"string is shorter than ${minimum.num.toInt}")
      }
      schema.get("maxLength").foreach { maximum =>
        if (codePointCount > maximum.num.toInt)
          errors += Violation(path, s"string is longer than ${maximum.num.toInt}")
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

  /** Checks only local conjunctive keywords and nested `properties` paths. */
  private def validateCheap(
      instance: ujson.Value,
      schema: collection.Map[String, ujson.Value],
      path: String
  ): Vector[Violation] = {
    schema.get("type").foreach { expected =>
      val accepted = expected match {
        case ujson.Str(name) => matchesType(instance, name)
        case values: ujson.Arr =>
          values.arr.exists(value => matchesType(instance, value.str))
        case _ => false
      }
      if (!accepted) return Vector(Violation(path, "value has the wrong type"))
    }
    schema.get("const").foreach { expected =>
      if (instance != expected)
        return Vector(Violation(path, "value differs from const"))
    }
    schema.get("enum").foreach { values =>
      if (!values.arr.contains(instance))
        return Vector(Violation(path, "value is absent from enum"))
    }

    instance match {
      case value: ujson.Obj =>
        schema.get("required").foreach { required =>
          required.arr.iterator.map(_.str).find(!value.obj.contains(_)).foreach {
            field =>
              return Vector(
                Violation(child(path, field), "required property is absent")
              )
          }
        }
        schema.get("properties").foreach { properties =>
          properties.obj.iterator.foreach { case (name, propertySchema) =>
            value.obj.get(name).foreach { supplied =>
              propertySchema match {
                case propertyObject: ujson.Obj =>
                  val violations = validateCheap(
                    supplied,
                    propertyObject.obj,
                    child(path, name)
                  )
                  if (violations.nonEmpty) return violations
                case _ => ()
              }
            }
          }
        }
      case value: ujson.Arr =>
        schema.get("minItems").foreach { minimum =>
          if (value.arr.size < minimum.num.toInt)
            return Vector(
              Violation(path, s"fewer than ${minimum.num.toInt} items")
            )
        }
        schema.get("maxItems").foreach { maximum =>
          if (value.arr.size > maximum.num.toInt)
            return Vector(
              Violation(path, s"more than ${maximum.num.toInt} items")
            )
        }
      case _ => ()
    }

    val stringViolations = validateString(instance, schema, path)
    if (stringViolations.nonEmpty) return stringViolations
    validateNumber(instance, schema, path)
  }

  private def isValid(
      instance: ujson.Value,
      schema: ujson.Value,
      root: ujson.Value,
      cache: ValidityCache
  ): Boolean =
    cache.get(instance, schema).getOrElse {
      val valid = validateAt(instance, schema, root, "$", cache).isEmpty
      cache.put(instance, schema, valid)
      valid
    }

  /**
    * Valid references are traversed once through the identity cache. Invalid
    * references are replayed only to retain the exact primary violation path.
    */
  private def validateReferenced(
      instance: ujson.Value,
      schema: ujson.Value,
      root: ujson.Value,
      path: String,
      cache: ValidityCache
  ): Vector[Violation] =
    if (isValid(instance, schema, root, cache)) Vector.empty
    else validateAt(instance, schema, root, path, cache)

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
