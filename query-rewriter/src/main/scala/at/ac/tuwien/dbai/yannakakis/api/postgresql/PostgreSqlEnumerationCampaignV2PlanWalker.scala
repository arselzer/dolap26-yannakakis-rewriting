package at.ac.tuwien.dbai.yannakakis.api.postgresql

import java.math.{BigDecimal => JavaBigDecimal}
import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import java.text.Normalizer

import com.fasterxml.jackson.core.{JsonFactory, JsonParser, JsonToken, StreamReadConstraints}

import scala.collection.mutable.ArrayBuffer
import scala.util.control.NonFatal

/**
  * An exact numeric token from a validated PostgreSQL JSON plan.
  *
  * `raw` is the authoritative evidence. `exactValue` exists only so callers
  * can perform exact arithmetic without first passing through a binary float.
  */
final case class PostgreSqlEnumerationCampaignV2ExactPlanNumber private[postgresql] (
    raw: String,
    exactValue: JavaBigDecimal
) extends Serializable {
  require(raw != null && raw.nonEmpty, "exact plan number is empty")
  require(exactValue != null, "exact plan number value is null")
}

/** One plan node in deterministic preorder. Root depth is zero. */
final case class PostgreSqlEnumerationCampaignV2PlanNodeReceipt private[postgresql] (
    preorderIndex: Int,
    parentPreorderIndex: Option[Int],
    depth: Int,
    nodeType: String,
    startupCost: PostgreSqlEnumerationCampaignV2ExactPlanNumber,
    totalCost: PostgreSqlEnumerationCampaignV2ExactPlanNumber,
    planRows: PostgreSqlEnumerationCampaignV2ExactPlanNumber,
    planWidth: PostgreSqlEnumerationCampaignV2ExactPlanNumber,
    childPreorderIndices: Vector[Int]
) extends Serializable {
  require(preorderIndex >= 0, "plan-node preorder index is negative")
  require(
    parentPreorderIndex != null && parentPreorderIndex.forall(_ >= 0),
    "plan-node parent index is invalid"
  )
  require(depth >= 0, "plan-node depth is negative")
  require(nodeType != null && nodeType.nonEmpty, "plan-node type is empty")
  require(startupCost != null, "plan-node startup cost is null")
  require(totalCost != null, "plan-node total cost is null")
  require(planRows != null, "plan-node row estimate is null")
  require(planWidth != null, "plan-node width is null")
  require(
    childPreorderIndices != null &&
      childPreorderIndices.forall(_ > preorderIndex),
    "plan-node child indexes are invalid"
  )
}

/**
  * A lossless, outcome-free receipt derived from one whole PostgreSQL plan.
  * Numeric spellings remain exact JSON tokens and are never narrowed to
  * Double. This is not a runtime or reportability receipt.
  */
final case class PostgreSqlEnumerationCampaignV2PlanWalkerReceipt private[postgresql] (
    schemaVersion: String,
    nodeCount: Int,
    maximumPlanDepth: Int,
    nodes: Vector[PostgreSqlEnumerationCampaignV2PlanNodeReceipt]
) extends Serializable {
  require(
    schemaVersion == PostgreSqlEnumerationCampaignV2PlanWalkerReceipt.SchemaVersion,
    "plan-walker receipt schema is not the frozen v1 value"
  )
  require(nodeCount > 0, "plan-walker receipt has no nodes")
  require(maximumPlanDepth >= 0, "plan-walker maximum depth is negative")
  require(nodes != null && nodes.size == nodeCount, "plan-walker node count differs")
  require(
    nodes.map(_.preorderIndex) == nodes.indices.toVector,
    "plan-walker nodes are not in exact preorder"
  )
  require(
    nodes.head.parentPreorderIndex.isEmpty && nodes.head.depth == 0,
    "plan-walker root identity is invalid"
  )
  require(
    nodes.map(_.depth).max == maximumPlanDepth,
    "plan-walker maximum depth differs"
  )
}

object PostgreSqlEnumerationCampaignV2PlanWalkerReceipt {
  val SchemaVersion: String =
    "yrw-enumeration-campaign-v2-plan-walker-receipt/v1"
}

/**
  * Strictly validated PostgreSQL plan evidence as the exact UTF-8 bytes
  * returned for the JDBC result value. These are not claimed to be server
  * protocol or wire bytes.
  *
  * The exact bytes, rather than a Jackson value tree, are authoritative. This
  * deliberately does not claim compatibility with the dormant v2
  * `postgresql-plan` payload, whose ordinary JSON-number representation cannot
  * preserve every PostgreSQL numeric token exactly.
  */
final class PostgreSqlEnumerationCampaignV2WholePlanDocument private[postgresql] (
    bytes: Array[Byte],
    val rawUtf8Sha256: String,
    val walkerReceipt: PostgreSqlEnumerationCampaignV2PlanWalkerReceipt,
    val settings: Vector[PostgreSqlEnumerationCampaignV2PlanSettingReceipt]
) extends Serializable {
  require(bytes != null && bytes.nonEmpty, "whole-plan bytes are empty")
  require(
    rawUtf8Sha256 != null && rawUtf8Sha256.matches("[0-9a-f]{64}"),
    "whole-plan digest is not lowercase SHA-256"
  )
  require(walkerReceipt != null, "whole-plan walker receipt is null")
  require(
    settings != null && settings.forall(_ != null) &&
      settings.map(_.name) == settings.map(_.name).sorted.distinct,
    "whole-plan settings are not sorted and unique"
  )

  private val exactBytes: Array[Byte] = bytes.clone()

  val utf8ByteLength: Long = exactBytes.length.toLong

  /** Returns a defensive copy of the authoritative plan document bytes. */
  def rawUtf8Bytes: Array[Byte] = exactBytes.clone()

  /** Convenience view; validation has already required strict UTF-8. */
  def rawUtf8: String = new String(exactBytes, StandardCharsets.UTF_8)
}

final case class PostgreSqlEnumerationCampaignV2PlanSettingReceipt private[postgresql] (
    name: String,
    value: String
) extends Serializable {
  require(name != null && name.nonEmpty, "plan setting name is empty")
  require(value != null && value.nonEmpty, "plan setting value is empty")
}

private[postgresql] object PostgreSqlEnumerationCampaignV2PlanWalker {
  def validate(
      rawUtf8: Array[Byte],
      limits: PostgreSqlEnumerationCampaignV2WholePlanCaptureLimits
  ): Either[
    PostgreSqlEnumerationCampaignV2WholePlanFailureCode,
    PostgreSqlEnumerationCampaignV2WholePlanDocument
  ] = {
    if (
      rawUtf8 == null || rawUtf8.isEmpty ||
      rawUtf8.length.toLong > limits.maxPlanUtf8Bytes
    ) return Left(
      PostgreSqlEnumerationCampaignV2WholePlanFailureCode.PlanDocumentOversized
    )
    val json = strictUtf8(rawUtf8) match {
      case Left(code)   => return Left(code)
      case Right(value) => value
    }
    var parser: JsonParser = null
    var closeFailed = false
    val result =
      try {
        val factory = new JsonFactory()
        factory.enable(JsonParser.Feature.STRICT_DUPLICATE_DETECTION)
        factory.setStreamReadConstraints(
          StreamReadConstraints
            .builder()
            .maxNestingDepth(limits.maxJsonNestingDepth)
            .maxNumberLength(limits.maxNumericTokenCharacters)
            .maxStringLength(
              PostgreSqlEnumerationCampaignV2WholePlanCaptureLimits
                .MaximumPlanTextUtf8Bytes
            )
            .build()
        )
        parser = factory.createParser(json)
        new Walker(parser, limits).walk().map { validated =>
          new PostgreSqlEnumerationCampaignV2WholePlanDocument(
            rawUtf8,
            sha256(rawUtf8),
            validated.receipt,
            validated.settings
          )
        }
      } catch {
        case NonFatal(_) =>
          Left(
            PostgreSqlEnumerationCampaignV2WholePlanFailureCode.PlanJsonMalformed
          )
      } finally {
        if (parser != null) {
          try parser.close()
          catch { case NonFatal(_) => closeFailed = true }
        }
      }
    if (closeFailed)
      Left(PostgreSqlEnumerationCampaignV2WholePlanFailureCode.CleanupFailed)
    else result
  }

  private final class Walker(
      parser: JsonParser,
      limits: PostgreSqlEnumerationCampaignV2WholePlanCaptureLimits
  ) {
    private val nodes =
      ArrayBuffer.empty[Option[PostgreSqlEnumerationCampaignV2PlanNodeReceipt]]
    private var maximumDepth = 0
    private var jsonValueCount = 0
    private var settings = Vector.empty[PostgreSqlEnumerationCampaignV2PlanSettingReceipt]

    def walk(): Either[
      PostgreSqlEnumerationCampaignV2WholePlanFailureCode,
      ValidatedPlan
    ] = {
      if (parser.nextToken() != JsonToken.START_ARRAY) return malformed
      if (!recordJsonValue(JsonToken.START_ARRAY)) return jsonValuesExceeded
      if (parser.nextToken() != JsonToken.START_OBJECT) return malformed
      if (!recordJsonValue(JsonToken.START_OBJECT)) return jsonValuesExceeded

      var rootIndex: Option[Int] = None
      var settingsSeen = false
      var jitSeen = false
      var token = parser.nextToken()
      while (token != JsonToken.END_OBJECT) {
        if (token != JsonToken.FIELD_NAME) return malformed
        val name = parser.currentName()
        inspectFieldName(name) match {
          case Left(code) => return Left(code)
          case Right(_)   => ()
        }
        if (!AllowedEnvelopeFields(name)) return malformed
        val valueToken = parser.nextToken()
        if (valueToken == null) return malformed
        if (!recordJsonValue(valueToken)) return jsonValuesExceeded
        name match {
          case "Plan" =>
            if (rootIndex.nonEmpty || valueToken != JsonToken.START_OBJECT)
              return malformed
            parsePlanNode(depth = 0, parent = None) match {
              case Left(code)  => return Left(code)
              case Right(node) => rootIndex = Some(node.index)
            }
          case "Settings" =>
            if (settingsSeen || valueToken != JsonToken.START_OBJECT)
              return malformed
            settingsSeen = true
            parseSettings() match {
              case Left(code) => return Left(code)
              case Right(_)   => ()
            }
          case "JIT" =>
            if (jitSeen || valueToken != JsonToken.START_OBJECT)
              return malformed
            jitSeen = true
            parseJit() match {
              case Left(code) => return Left(code)
              case Right(_)   => ()
            }
        }
        token = parser.nextToken()
        if (token == null) return malformed
      }

      if (
        rootIndex != Some(0) || !settingsSeen ||
        parser.nextToken() != JsonToken.END_ARRAY ||
        parser.nextToken() != null
      ) return malformed

      val completed = nodes.iterator.map {
        case Some(node) => node
        case None       => return malformed
      }.toVector
      Right(
        ValidatedPlan(
          PostgreSqlEnumerationCampaignV2PlanWalkerReceipt(
          PostgreSqlEnumerationCampaignV2PlanWalkerReceipt.SchemaVersion,
          completed.size,
          maximumDepth,
          completed
          ),
          settings
        )
      )
    }

    private def parsePlanNode(
        depth: Int,
        parent: Option[Int]
    ): Either[
      PostgreSqlEnumerationCampaignV2WholePlanFailureCode,
      ParsedPlanNode
    ] = {
      if (depth > limits.maxPlanNodeDepth)
        return Left(
          PostgreSqlEnumerationCampaignV2WholePlanFailureCode.PlanDepthExceeded
        )
      if (nodes.size >= limits.maxPlanNodes)
        return Left(
          PostgreSqlEnumerationCampaignV2WholePlanFailureCode.PlanNodeLimitExceeded
        )

      val index = nodes.size
      nodes += None
      maximumDepth = math.max(maximumDepth, depth)

      var nodeType: Option[String] = None
      var startupCost: Option[PostgreSqlEnumerationCampaignV2ExactPlanNumber] = None
      var totalCost: Option[PostgreSqlEnumerationCampaignV2ExactPlanNumber] = None
      var planRows: Option[PostgreSqlEnumerationCampaignV2ExactPlanNumber] = None
      var planWidth: Option[PostgreSqlEnumerationCampaignV2ExactPlanNumber] = None
      var plansSeen = false
      var subplansRemoved: Option[
        PostgreSqlEnumerationCampaignV2ExactPlanNumber
      ] = None
      val fieldsSeen = scala.collection.mutable.Set.empty[String]
      val textValues = scala.collection.mutable.Map.empty[String, String]
      val children = ArrayBuffer.empty[Int]
      val childDescriptors = ArrayBuffer.empty[PlanChildDescriptor]

      var token = parser.nextToken()
      while (token != JsonToken.END_OBJECT) {
        if (token != JsonToken.FIELD_NAME) return malformed
        val name = parser.currentName()
        inspectFieldName(name) match {
          case Left(code) => return Left(code)
          case Right(_)   => ()
        }
        if (!AllowedNodeFields(name) || fieldsSeen(name)) return malformed
        fieldsSeen += name
        val valueToken = parser.nextToken()
        if (valueToken == null) return malformed
        if (!recordJsonValue(valueToken)) return jsonValuesExceeded
        name match {
          case "Node Type" =>
            if (
              nodeType.nonEmpty || valueToken != JsonToken.VALUE_STRING ||
              parser.getText == null || parser.getText.isEmpty ||
              parser.getText.length >
                PostgreSqlEnumerationCampaignV2WholePlanCaptureLimits
                  .MaximumNodeTypeCharacters ||
              !isNormalizedPrintableAscii(parser.getText) ||
              !AllowedNodeTypes(parser.getText)
            ) return malformed
            nodeType = Some(parser.getText)
          case "Startup Cost" =>
            parseNumber(valueToken, requireIntegral = false) match {
              case Right(value) if startupCost.isEmpty => startupCost = Some(value)
              case Right(_)                            => return malformed
              case Left(code)                         => return Left(code)
            }
          case "Total Cost" =>
            parseNumber(valueToken, requireIntegral = false) match {
              case Right(value) if totalCost.isEmpty => totalCost = Some(value)
              case Right(_)                          => return malformed
              case Left(code)                       => return Left(code)
            }
          case "Plan Rows" =>
            parseNumber(valueToken, requireIntegral = true) match {
              case Right(value) if planRows.isEmpty => planRows = Some(value)
              case Right(_)                         => return malformed
              case Left(code)                      => return Left(code)
            }
          case "Plan Width" =>
            parseNumber(valueToken, requireIntegral = true) match {
              case Right(value) if planWidth.isEmpty => planWidth = Some(value)
              case Right(_)                          => return malformed
              case Left(code)                       => return Left(code)
            }
          case "Plans" =>
            if (plansSeen || valueToken != JsonToken.START_ARRAY) return malformed
            plansSeen = true
            var childToken = parser.nextToken()
            while (childToken != JsonToken.END_ARRAY) {
              if (childToken != JsonToken.START_OBJECT) return malformed
              if (!recordJsonValue(childToken)) return jsonValuesExceeded
              parsePlanNode(depth + 1, Some(index)) match {
                case Left(code) => return Left(code)
                case Right(child) =>
                  child.relationship match {
                    case Some(relationship) =>
                      children += child.index
                      childDescriptors += PlanChildDescriptor(
                        relationship,
                        child.hasSubplanName
                      )
                    case None => return malformed
                  }
              }
              childToken = parser.nextToken()
              if (childToken == null) return malformed
            }
          case field if NodeBooleanFields(field) =>
            if (
              valueToken != JsonToken.VALUE_TRUE &&
              valueToken != JsonToken.VALUE_FALSE
            ) return malformed
          case "Subplans Removed" =>
            parseNumber(valueToken, requireIntegral = true) match {
              case Left(code) => return Left(code)
              case Right(value) => subplansRemoved = Some(value)
            }
          case field if NodeNonnegativeIntegralFields(field) =>
            parseNumber(valueToken, requireIntegral = true) match {
              case Left(code) => return Left(code)
              case Right(_)   => ()
            }
          case field if NodeTextFields(field) =>
            if (
              valueToken != JsonToken.VALUE_STRING ||
              !isValidPlannerText(parser.getText)
            ) return malformed
            textValues += field -> parser.getText
          case field if NodeTextArrayFields(field) =>
            parseTextArray(
              valueToken,
              mayBeEmpty = field == "Sampling Parameters"
            ) match {
              case Left(code) => return Left(code)
              case Right(_)   => ()
            }
          case _ => return malformed
        }
        token = parser.nextToken()
        if (token == null) return malformed
      }

      val receipt = for {
        _ <-
          if (RequiredNodeFields.subsetOf(fieldsSeen.toSet)) Right(())
          else
            Left(
              PostgreSqlEnumerationCampaignV2WholePlanFailureCode.PlanJsonMalformed
            )
        kind <- nodeType.toRight(
          PostgreSqlEnumerationCampaignV2WholePlanFailureCode.PlanJsonMalformed
        )
        _ <- validateNodeGrammar(
          kind,
          fieldsSeen.toSet,
          textValues.toMap,
          depth,
          plansSeen,
          childDescriptors.toVector,
          subplansRemoved
        )
        startup <- startupCost.toRight(
          PostgreSqlEnumerationCampaignV2WholePlanFailureCode.PlanJsonMalformed
        )
        total <- totalCost.toRight(
          PostgreSqlEnumerationCampaignV2WholePlanFailureCode.PlanJsonMalformed
        )
        rows <- planRows.toRight(
          PostgreSqlEnumerationCampaignV2WholePlanFailureCode.PlanJsonMalformed
        )
        width <- planWidth.toRight(
          PostgreSqlEnumerationCampaignV2WholePlanFailureCode.PlanJsonMalformed
        )
        _ <-
          if (total.exactValue.compareTo(startup.exactValue) >= 0) Right(())
          else
            Left(
              PostgreSqlEnumerationCampaignV2WholePlanFailureCode.NumericEvidenceUnsafe
            )
      } yield PostgreSqlEnumerationCampaignV2PlanNodeReceipt(
        index,
        parent,
        depth,
        kind,
        startup,
        total,
        rows,
        width,
        children.toVector
      )
      receipt.map { value =>
        nodes(index) = Some(value)
        ParsedPlanNode(
          index,
          textValues.get("Parent Relationship"),
          fieldsSeen("Subplan Name")
        )
      }
    }

    private def parseSettings(): Either[
      PostgreSqlEnumerationCampaignV2WholePlanFailureCode,
      Unit
    ] = {
      var settingCount = 0
      val parsed = Vector.newBuilder[PostgreSqlEnumerationCampaignV2PlanSettingReceipt]
      var token = parser.nextToken()
      while (token != JsonToken.END_OBJECT) {
        if (token != JsonToken.FIELD_NAME) return malformed
        val name = parser.currentName()
        if (!AllowedSettingNames(name)) return malformed
        settingCount += 1
        if (
          settingCount >
            PostgreSqlEnumerationCampaignV2WholePlanCaptureLimits.MaximumSettings
        ) return jsonValuesExceeded
        val valueToken = parser.nextToken()
        if (valueToken == null) return malformed
        if (!recordJsonValue(valueToken)) return jsonValuesExceeded
        if (
          valueToken != JsonToken.VALUE_STRING || parser.getText == null ||
          parser.getText.isEmpty ||
          parser.getText.getBytes(StandardCharsets.UTF_8).length >
            PostgreSqlEnumerationCampaignV2WholePlanCaptureLimits
              .MaximumSettingValueUtf8Bytes ||
          !isValidPlannerText(parser.getText)
        ) return malformed
        parsed += PostgreSqlEnumerationCampaignV2PlanSettingReceipt(
          name,
          parser.getText
        )
        token = parser.nextToken()
        if (token == null) return malformed
      }
      settings = parsed.result().sortBy(_.name)
      Right(())
    }

    private def parseJit(): Either[
      PostgreSqlEnumerationCampaignV2WholePlanFailureCode,
      Unit
    ] = {
      var functionsSeen = false
      var optionsSeen = false
      var token = parser.nextToken()
      while (token != JsonToken.END_OBJECT) {
        if (token != JsonToken.FIELD_NAME) return malformed
        val name = parser.currentName()
        inspectFieldName(name) match {
          case Left(code) => return Left(code)
          case Right(_)   => ()
        }
        if (!JitFields(name)) return malformed
        val valueToken = parser.nextToken()
        if (valueToken == null) return malformed
        if (!recordJsonValue(valueToken)) return jsonValuesExceeded
        name match {
          case "Functions" =>
            if (functionsSeen) return malformed
            functionsSeen = true
            parseNumber(valueToken, requireIntegral = true) match {
              case Left(code) => return Left(code)
              case Right(value) if value.exactValue.signum() > 0 => ()
              case Right(_) => return malformed
            }
          case "Options" =>
            if (optionsSeen || valueToken != JsonToken.START_OBJECT)
              return malformed
            optionsSeen = true
            parseJitOptions() match {
              case Left(code) => return Left(code)
              case Right(_)   => ()
            }
          case _ => return malformed
        }
        token = parser.nextToken()
        if (token == null) return malformed
      }
      if (functionsSeen && optionsSeen) Right(()) else malformed
    }

    private def parseJitOptions(): Either[
      PostgreSqlEnumerationCampaignV2WholePlanFailureCode,
      Unit
    ] = {
      val seen = scala.collection.mutable.Set.empty[String]
      var token = parser.nextToken()
      while (token != JsonToken.END_OBJECT) {
        if (token != JsonToken.FIELD_NAME) return malformed
        val name = parser.currentName()
        inspectFieldName(name) match {
          case Left(code) => return Left(code)
          case Right(_)   => ()
        }
        if (!JitOptionFields(name) || seen(name)) return malformed
        seen += name
        val valueToken = parser.nextToken()
        if (valueToken == null) return malformed
        if (!recordJsonValue(valueToken)) return jsonValuesExceeded
        if (
          valueToken != JsonToken.VALUE_TRUE &&
          valueToken != JsonToken.VALUE_FALSE
        ) return malformed
        token = parser.nextToken()
        if (token == null) return malformed
      }
      if (seen.toSet == JitOptionFields) Right(()) else malformed
    }

    private def parseTextArray(
        first: JsonToken,
        mayBeEmpty: Boolean
    ): Either[PostgreSqlEnumerationCampaignV2WholePlanFailureCode, Unit] = {
      if (first != JsonToken.START_ARRAY) return malformed
      var count = 0
      var token = parser.nextToken()
      while (token != JsonToken.END_ARRAY) {
        if (token == null) return malformed
        if (!recordJsonValue(token)) return jsonValuesExceeded
        if (token != JsonToken.VALUE_STRING || !isValidPlannerText(parser.getText))
          return malformed
        count += 1
        if (
          count >
            PostgreSqlEnumerationCampaignV2WholePlanCaptureLimits
              .MaximumPlanStringListItems
        ) return jsonValuesExceeded
        token = parser.nextToken()
      }
      if (count == 0 && !mayBeEmpty) malformed else Right(())
    }

    private def parseNumber(
        token: JsonToken,
        requireIntegral: Boolean
    ): Either[
      PostgreSqlEnumerationCampaignV2WholePlanFailureCode,
      PostgreSqlEnumerationCampaignV2ExactPlanNumber
    ] = {
      if (
        token != JsonToken.VALUE_NUMBER_INT &&
        token != JsonToken.VALUE_NUMBER_FLOAT
      ) return malformed
      val raw = parser.getText
      try {
        if (raw.length > limits.maxNumericTokenCharacters) return numericUnsafe
        val value = new JavaBigDecimal(raw)
        if (
          value.signum() < 0 ||
          decimalRenderedLength(value) >
            PostgreSqlEnumerationCampaignV2WholePlanCaptureLimits
              .MaximumCanonicalDecimalCharacters
        ) numericUnsafe
        else if (requireIntegral) {
          try {
            value.toBigIntegerExact
            Right(PostgreSqlEnumerationCampaignV2ExactPlanNumber(raw, value))
          } catch { case NonFatal(_) => numericUnsafe }
        } else Right(PostgreSqlEnumerationCampaignV2ExactPlanNumber(raw, value))
      } catch { case NonFatal(_) => numericUnsafe }
    }

    private def recordJsonValue(token: JsonToken): Boolean = {
      if (token == null || token == JsonToken.FIELD_NAME || token.isStructEnd)
        true
      else {
        jsonValueCount += 1
        jsonValueCount <= limits.maxJsonValues
      }
    }
  }

  private final case class ValidatedPlan(
      receipt: PostgreSqlEnumerationCampaignV2PlanWalkerReceipt,
      settings: Vector[PostgreSqlEnumerationCampaignV2PlanSettingReceipt]
  )

  private final case class ParsedPlanNode(
      index: Int,
      relationship: Option[String],
      hasSubplanName: Boolean
  )

  private final case class PlanChildDescriptor(
      relationship: String,
      hasSubplanName: Boolean
  )

  private val AllowedEnvelopeFields: Set[String] =
    Set("Plan", "Settings", "JIT")

  /**
    * PostgreSQL 18 core node labels reachable from a local read-only SELECT.
    * Extension nodes, row-locking, writes, and PostgreSQL's internal unknown
    * label are deliberately outside the frozen grammar.
    */
  private val AllowedNodeTypes: Set[String] =
    PostgreSqlEnumerationCampaignV2WholePlanCapture.AllowedPlanNodeTypes.toSet

  private val RequiredNodeFields: Set[String] = Set(
    "Node Type",
    "Parallel Aware",
    "Async Capable",
    "Startup Cost",
    "Total Cost",
    "Plan Rows",
    "Plan Width",
    "Disabled"
  )

  private val NodeBooleanFields: Set[String] = Set(
    "Parallel Aware",
    "Async Capable",
    "Disabled",
    "Inner Unique",
    "Single Copy"
  )

  private val NodeNonnegativeIntegralFields: Set[String] = Set(
    "Workers Planned",
    "Planned Partitions",
    "Subplans Removed"
  )

  private val NodeTextArrayFields: Set[String] = Set(
    "Group Key",
    "Presorted Key",
    "Sampling Parameters",
    "Sort Key"
  )

  private val NodeTextFields: Set[String] = Set(
    "Alias",
    "CTE Name",
    "Cache Key",
    "Cache Mode",
    "Command",
    "Filter",
    "Function Name",
    "Hash Cond",
    "Index Cond",
    "Index Name",
    "Join Filter",
    "Join Type",
    "Merge Cond",
    "One-Time Filter",
    "Order By",
    "Parent Relationship",
    "Partial Mode",
    "Recheck Cond",
    "Relation Name",
    "Repeatable Seed",
    "Run Condition",
    "Sampling Method",
    "Scan Direction",
    "Strategy",
    "Subplan Name",
    "TID Cond",
    "Table Function Name",
    "Tuplestore Name",
    "Window"
  )

  private val AllowedNodeFields: Set[String] =
    RequiredNodeFields ++ NodeBooleanFields ++
      NodeNonnegativeIntegralFields ++ NodeTextArrayFields ++ NodeTextFields +
      "Plans"

  private val NodeContextFields: Set[String] =
    Set("Parent Relationship", "Plans", "Subplan Name")

  private val ParentRelationships: Set[String] =
    Set("InitPlan", "Inner", "Member", "Outer", "SubPlan", "Subquery")

  private val JoinNodeTypes: Set[String] =
    Set("Hash Join", "Merge Join", "Nested Loop")
  private val JoinTypes: Set[String] =
    Set("Anti", "Full", "Inner", "Left", "Right", "Right Anti", "Right Semi", "Semi")
  private val RelationScanNodeTypes: Set[String] = Set(
    "Bitmap Heap Scan",
    "Index Only Scan",
    "Index Scan",
    "Sample Scan",
    "Seq Scan",
    "Tid Range Scan",
    "Tid Scan"
  )
  private val AliasScanNodeTypes: Set[String] = RelationScanNodeTypes ++ Set(
    "CTE Scan",
    "Function Scan",
    "Subquery Scan",
    "Table Function Scan",
    "Values Scan",
    "WorkTable Scan"
  )

  private val BinaryChildNodeTypes: Set[String] = Set(
    "Hash Join",
    "Merge Join",
    "Nested Loop",
    "Recursive Union",
    "SetOp"
  )
  private val UnaryChildNodeTypes: Set[String] = Set(
    "Aggregate",
    "Bitmap Heap Scan",
    "Gather",
    "Gather Merge",
    "Group",
    "Hash",
    "Incremental Sort",
    "Limit",
    "Materialize",
    "Memoize",
    "ProjectSet",
    "Sort",
    "Unique",
    "WindowAgg"
  )
  private val MemberChildNodeTypes: Set[String] = Set("BitmapAnd", "BitmapOr")
  private val PrunableMemberChildNodeTypes: Set[String] =
    Set("Append", "Merge Append")
  private val LeafNodeTypes: Set[String] = Set(
    "Bitmap Index Scan",
    "CTE Scan",
    "Function Scan",
    "Index Only Scan",
    "Index Scan",
    "Named Tuplestore Scan",
    "Sample Scan",
    "Seq Scan",
    "Table Function Scan",
    "Tid Range Scan",
    "Tid Scan",
    "Values Scan",
    "WorkTable Scan"
  )

  private val ChildNodeTypeFamilies = Vector(
    BinaryChildNodeTypes,
    UnaryChildNodeTypes,
    MemberChildNodeTypes,
    PrunableMemberChildNodeTypes,
    LeafNodeTypes,
    Set("Result"),
    Set("Subquery Scan")
  )
  require(
    ChildNodeTypeFamilies.flatten.toSet == AllowedNodeTypes &&
      ChildNodeTypeFamilies.map(_.size).sum == AllowedNodeTypes.size,
    "PostgreSQL 18 child grammar does not cover the node vocabulary"
  )

  private val NodeOptionalFields: Map[String, Set[String]] = Map(
    "Aggregate" -> Set(
      "Filter",
      "Group Key",
      "Partial Mode",
      "Planned Partitions",
      "Strategy"
    ),
    "Append" -> Set("Subplans Removed"),
    "Bitmap Heap Scan" -> Set("Alias", "Filter", "Recheck Cond", "Relation Name"),
    "Bitmap Index Scan" -> Set("Index Cond", "Index Name"),
    "BitmapAnd" -> Set.empty,
    "BitmapOr" -> Set.empty,
    "CTE Scan" -> Set("Alias", "CTE Name", "Filter"),
    "Function Scan" -> Set("Alias", "Filter", "Function Name"),
    "Gather" -> Set("Filter", "Single Copy", "Workers Planned"),
    "Gather Merge" -> Set("Filter", "Workers Planned"),
    "Group" -> Set("Filter", "Group Key"),
    "Hash" -> Set.empty,
    "Hash Join" -> Set("Filter", "Hash Cond", "Inner Unique", "Join Filter", "Join Type"),
    "Incremental Sort" -> Set("Presorted Key", "Sort Key"),
    "Index Only Scan" -> Set(
      "Alias",
      "Filter",
      "Index Cond",
      "Index Name",
      "Order By",
      "Relation Name",
      "Scan Direction"
    ),
    "Index Scan" -> Set(
      "Alias",
      "Filter",
      "Index Cond",
      "Index Name",
      "Order By",
      "Relation Name",
      "Scan Direction"
    ),
    "Limit" -> Set.empty,
    "Materialize" -> Set.empty,
    "Memoize" -> Set("Cache Key", "Cache Mode"),
    "Merge Append" -> Set("Sort Key", "Subplans Removed"),
    "Merge Join" -> Set(
      "Filter",
      "Inner Unique",
      "Join Filter",
      "Join Type",
      "Merge Cond"
    ),
    "Named Tuplestore Scan" -> Set("Filter"),
    "Nested Loop" -> Set("Filter", "Inner Unique", "Join Filter", "Join Type"),
    "ProjectSet" -> Set.empty,
    "Recursive Union" -> Set.empty,
    "Result" -> Set("Filter", "One-Time Filter"),
    "Sample Scan" -> Set(
      "Alias",
      "Filter",
      "Relation Name",
      "Repeatable Seed",
      "Sampling Method",
      "Sampling Parameters"
    ),
    "Seq Scan" -> Set("Alias", "Filter", "Relation Name"),
    "SetOp" -> Set("Command", "Strategy"),
    "Sort" -> Set("Sort Key"),
    "Subquery Scan" -> Set("Alias", "Filter"),
    "Table Function Scan" -> Set("Alias", "Filter", "Table Function Name"),
    "Tid Range Scan" -> Set("Alias", "Filter", "Relation Name", "TID Cond"),
    "Tid Scan" -> Set("Alias", "Filter", "Relation Name", "TID Cond"),
    "Unique" -> Set.empty,
    "Values Scan" -> Set("Alias", "Filter"),
    "WindowAgg" -> Set("Filter", "Run Condition", "Window"),
    "WorkTable Scan" -> Set("Alias", "CTE Name", "Filter")
  )

  require(
    NodeOptionalFields.keySet == AllowedNodeTypes,
    "PostgreSQL 18 node-field grammar does not cover the node vocabulary"
  )

  private val AllowedSettingNames: Set[String] =
    PostgreSqlEnumerationCampaignV2WholePlanCapture.PlannerSettingNames.toSet +
      "search_path"

  private val JitFields: Set[String] = Set("Functions", "Options")
  private val JitOptionFields: Set[String] =
    Set("Deforming", "Expressions", "Inlining", "Optimization")

  private def validateNodeGrammar(
      nodeType: String,
      fields: Set[String],
      textValues: Map[String, String],
      depth: Int,
      plansSeen: Boolean,
      children: Vector[PlanChildDescriptor],
      subplansRemoved: Option[PostgreSqlEnumerationCampaignV2ExactPlanNumber]
  ): Either[PostgreSqlEnumerationCampaignV2WholePlanFailureCode, Unit] = {
    val allowed =
      RequiredNodeFields ++ NodeContextFields ++ NodeOptionalFields(nodeType)
    if (!fields.subsetOf(allowed)) return malformed

    if (depth == 0) {
      if (fields("Parent Relationship") || fields("Subplan Name"))
        return malformed
    } else {
      val relationship = textValues.get("Parent Relationship") match {
        case Some(value) if ParentRelationships(value) => value
        case _                                         => return malformed
      }
      if (
        fields("Subplan Name") != Set("InitPlan", "SubPlan")(relationship)
      ) return malformed
    }

    if (
      JoinNodeTypes(nodeType) &&
      (!fields("Inner Unique") || !fields("Join Type") ||
        !textValues.get("Join Type").exists(JoinTypes))
    ) return malformed
    if (nodeType == "Hash Join" && !fields("Hash Cond")) return malformed
    if (nodeType == "Merge Join" && !fields("Merge Cond")) return malformed
    if (
      RelationScanNodeTypes(nodeType) &&
      (!fields("Alias") || !fields("Relation Name"))
    ) return malformed
    if (AliasScanNodeTypes(nodeType) && !fields("Alias")) return malformed
    if (
      Set("Index Scan", "Index Only Scan")(nodeType) &&
      (!fields("Index Name") || !fields("Scan Direction") ||
        !textValues
          .get("Scan Direction")
          .exists(Set("Backward", "Forward")))
    ) return malformed
    if (nodeType == "Bitmap Index Scan" && !fields("Index Name"))
      return malformed
    if (Set("Tid Scan", "Tid Range Scan")(nodeType) && !fields("TID Cond"))
      return malformed
    if (
      Set("CTE Scan", "WorkTable Scan")(nodeType) && !fields("CTE Name")
    ) return malformed
    if (nodeType == "Table Function Scan" && !fields("Table Function Name"))
      return malformed
    if (
      nodeType == "Sample Scan" &&
      (!fields("Sampling Method") || !fields("Sampling Parameters"))
    ) return malformed
    if (
      nodeType == "Gather" &&
      (!fields("Single Copy") || !fields("Workers Planned"))
    ) return malformed
    if (nodeType == "Gather Merge" && !fields("Workers Planned"))
      return malformed
    if (
      nodeType == "Memoize" &&
      (!fields("Cache Key") || !fields("Cache Mode") ||
        !textValues.get("Cache Mode").exists(Set("binary", "logical")))
    ) return malformed
    if (
      Set("Append", "Merge Append")(nodeType) &&
      !fields("Subplans Removed")
    ) return malformed
    if (
      Set("Sort", "Incremental Sort", "Merge Append")(nodeType) &&
      !fields("Sort Key")
    ) return malformed
    if (nodeType == "Incremental Sort" && !fields("Presorted Key"))
      return malformed
    if (nodeType == "Group" && !fields("Group Key")) return malformed
    if (nodeType == "WindowAgg" && !fields("Window")) return malformed

    if (nodeType == "SetOp") {
      if (
        !fields("Command") || !fields("Strategy") ||
        !textValues
          .get("Command")
          .exists(Set("Except", "Except All", "Intersect", "Intersect All")) ||
        !textValues.get("Strategy").exists(Set("Hashed", "Sorted"))
      ) return malformed
    }

    if (nodeType == "Aggregate") {
      val strategy = textValues.get("Strategy")
      if (
        !fields("Partial Mode") || !fields("Strategy") ||
        !textValues
          .get("Partial Mode")
          .exists(Set("Finalize", "Partial", "Simple")) ||
        !strategy.exists(Set("Hashed", "Plain", "Sorted"))
      ) return malformed
      if (
        strategy.contains("Plain") && fields("Group Key")
      ) return malformed
      if (
        strategy.exists(Set("Hashed", "Sorted")) && !fields("Group Key")
      ) return malformed
      if (
        strategy.contains("Hashed") &&
        !fields("Planned Partitions")
      ) return malformed
      if (!strategy.contains("Hashed") && fields("Planned Partitions"))
        return malformed
    }

    validatePlanChildren(
      nodeType,
      plansSeen,
      children,
      subplansRemoved
    )
  }

  /**
    * ExplainNode emits InitPlan children first, then the node's structural
    * children, then ordinary SubPlan children. Keep that exact ordering and
    * the source-defined structural relationship sequence.
    */
  private def validatePlanChildren(
      nodeType: String,
      plansSeen: Boolean,
      children: Vector[PlanChildDescriptor],
      subplansRemoved: Option[PostgreSqlEnumerationCampaignV2ExactPlanNumber]
  ): Either[PostgreSqlEnumerationCampaignV2WholePlanFailureCode, Unit] = {
    val initPlanCount = children.prefixLength(_.relationship == "InitPlan")
    var subPlanStart = children.size
    while (
      subPlanStart > initPlanCount &&
      children(subPlanStart - 1).relationship == "SubPlan"
    ) subPlanStart -= 1

    val initPlans = children.take(initPlanCount)
    val structural = children.slice(initPlanCount, subPlanStart)
    val subPlans = children.drop(subPlanStart)
    if (
      initPlans.exists(!_.hasSubplanName) ||
      subPlans.exists(!_.hasSubplanName) ||
      structural.exists(child =>
        child.hasSubplanName ||
          Set("InitPlan", "SubPlan")(child.relationship)
      )
    ) return malformed

    val relationships = structural.map(_.relationship)
    val prunedEmptyAppend =
      PrunableMemberChildNodeTypes(nodeType) && relationships.isEmpty &&
        subplansRemoved.exists(_.exactValue.signum() > 0)
    // ExplainSubPlans can open an empty array after globally deduplicating a
    // repeated physical subplan. That shape loses the local topology needed
    // by this receipt, so the conservative v1 subset rejects it. The one
    // accepted empty structural shape is source-evidenced partition pruning.
    if (plansSeen && children.isEmpty && !prunedEmptyAppend) return malformed

    val valid =
      if (BinaryChildNodeTypes(nodeType))
        relationships == Vector("Outer", "Inner")
      else if (UnaryChildNodeTypes(nodeType)) relationships == Vector("Outer")
      else if (nodeType == "Subquery Scan")
        relationships == Vector("Subquery")
      else if (MemberChildNodeTypes(nodeType))
        relationships.nonEmpty && relationships.forall(_ == "Member")
      else if (PrunableMemberChildNodeTypes(nodeType))
        plansSeen &&
          relationships.forall(_ == "Member") &&
          (relationships.nonEmpty || prunedEmptyAppend)
      else if (nodeType == "Result")
        relationships.isEmpty || relationships == Vector("Outer")
      else LeafNodeTypes(nodeType) && relationships.isEmpty

    if (valid) Right(()) else malformed
  }

  /**
    * Runtime fields known across PostgreSQL text/JSON EXPLAIN variants. The
    * comparison is case-insensitive to prevent hostile shadow spellings from
    * bypassing the ANALYZE-FALSE boundary.
    */
  private def inspectFieldName(
      name: String
  ): Either[PostgreSqlEnumerationCampaignV2WholePlanFailureCode, Unit] =
    normalizedKeyTokens(name).flatMap { tokens =>
      val joined = tokens.mkString(" ")
      val tokenSet = tokens.toSet
      val forbidden =
        tokens.isEmpty || tokens.contains("actual") || tokenSet("timing") ||
          Set(
            "execution time",
            "planning time",
            "triggers",
            "trigger",
            "workers"
          )(joined) ||
          tokens.startsWith(Vector("rows", "removed", "by")) ||
          tokenSet("buffers") || tokenSet("buffer") || tokenSet("wal") ||
          joined.startsWith("i o ") || tokenSet("io") ||
          Set(
            "cache evictions",
            "cache hits",
            "cache misses",
            "cache overflows",
            "exact heap blocks",
            "heap fetches",
            "lossy heap blocks",
            "partitions scanned",
            "workers launched"
          )(joined) ||
          tokenSet("blocks") ||
          (Set("worker", "workers")(tokens.head) &&
            joined != "workers planned") ||
          (tokenSet("memory") &&
            tokenSet.exists(Set("usage", "used", "peak", "maximum"))) ||
          (Set("memory", "disk", "storage")(tokens.head)) ||
          (tokenSet("storage") &&
            tokenSet.exists(Set("space", "usage", "used", "maximum"))) ||
          (tokenSet("sort") &&
            tokenSet.exists(Set("method", "methods", "space"))) ||
          Set("batches", "buckets")(joined) ||
          Set("conflict", "conflicting")(tokens.head) ||
          (tokens.head == "tuples" && tokens.lift(1).exists(
            Set("conflicting", "deleted", "inserted", "skipped", "updated")
          )) ||
          Set(
            Vector("shared", "hit"),
            Vector("shared", "read"),
            Vector("shared", "dirtied"),
            Vector("shared", "written"),
            Vector("local", "hit"),
            Vector("local", "read"),
            Vector("local", "dirtied"),
            Vector("local", "written"),
            Vector("temp", "read"),
            Vector("temp", "written")
          )(tokens.take(2)) ||
          Set(
            Vector("sort", "method"),
            Vector("sort", "space"),
            Vector("peak", "memory"),
            Vector("disk", "usage"),
            Vector("hash", "buckets"),
            Vector("hash", "batches"),
            Vector("hashagg", "batches"),
            Vector("original", "hash"),
            Vector("full", "sort"),
            Vector("pre", "sorted"),
            Vector("index", "searches"),
            Vector("tuples", "inserted"),
            Vector("conflicting", "tuples"),
            Vector("serialization", "time")
          )(tokens.take(2)) ||
          Set("planning", "execution", "trigger")(tokens.head) ||
          (tokenSet("time") &&
            tokenSet.exists(
              Set("planning", "execution", "read", "write", "serialization")
            ))
      if (forbidden) executionEvidence else Right(())
    }

  private def isNormalizedPrintableAscii(value: String): Boolean =
    value != null && value.nonEmpty &&
      Normalizer.normalize(value, Normalizer.Form.NFKC) == value &&
      value.forall(character => character >= 0x20 && character <= 0x7e)

  private def isValidPlannerText(value: String): Boolean =
    value != null && value.nonEmpty &&
      value.getBytes(StandardCharsets.UTF_8).length <=
        PostgreSqlEnumerationCampaignV2WholePlanCaptureLimits
          .MaximumPlanTextUtf8Bytes &&
      value.indexOf('\u0000') < 0 && hasValidUnicode(value)

  private def normalizedKeyTokens(
      name: String
  ): Either[
    PostgreSqlEnumerationCampaignV2WholePlanFailureCode,
    Vector[String]
  ] = {
    if (name == null || !hasValidUnicode(name)) return malformed
    val compatibility = Normalizer.normalize(name, Normalizer.Form.NFKC)
    val whitespaceCollapsed = new StringBuilder(compatibility.length)
    compatibility.foreach { character =>
      if (
        Character.isWhitespace(character) || Character.isSpaceChar(character)
      ) whitespaceCollapsed.append(' ')
      else whitespaceCollapsed.append(character)
    }
    // Java has no direct full-casefold primitive. Upper-then-lower under ROOT
    // supplies the relevant one-to-many fold (for example sharp-s -> ss)
    // before the ASCII property-name gate.
    val folded = whitespaceCollapsed.result()
      .toUpperCase(java.util.Locale.ROOT)
      .toLowerCase(java.util.Locale.ROOT)
    if (folded.exists(_ > 0x7f)) return malformed
    val tokens = Vector.newBuilder[String]
    val token = new StringBuilder
    folded.foreach { character =>
      if (
        (character >= 'a' && character <= 'z') ||
        (character >= '0' && character <= '9')
      ) token.append(character)
      else if (token.nonEmpty) {
        tokens += token.result()
        token.clear()
      }
    }
    if (token.nonEmpty) tokens += token.result()
    Right(tokens.result())
  }

  private def hasValidUnicode(value: String): Boolean = {
    if (value == null) return false
    var index = 0
    while (index < value.length) {
      val character = value.charAt(index)
      if (Character.isHighSurrogate(character)) {
        if (
          index + 1 >= value.length ||
          !Character.isLowSurrogate(value.charAt(index + 1))
        ) return false
        index += 2
      } else if (Character.isLowSurrogate(character)) return false
      else index += 1
    }
    true
  }

  private def decimalRenderedLength(value: JavaBigDecimal): Long = {
    if (value.signum() == 0) 1L
    else {
      val digits = value.precision().toLong
      val exponent = -value.scale().toLong
      val sign = if (value.signum() < 0) 1L else 0L
      if (exponent >= 0L) digits + exponent + sign
      else {
        val point = digits + exponent
        if (point > 0L) digits + 1L + sign
        else 2L + (-point) + digits + sign
      }
    }
  }

  private def strictUtf8(
      bytes: Array[Byte]
  ): Either[PostgreSqlEnumerationCampaignV2WholePlanFailureCode, String] = {
    if (bytes == null || bytes.isEmpty)
      Left(PostgreSqlEnumerationCampaignV2WholePlanFailureCode.PlanJsonMalformed)
    else {
      val decoder = StandardCharsets.UTF_8
        .newDecoder()
        .onMalformedInput(java.nio.charset.CodingErrorAction.REPORT)
        .onUnmappableCharacter(java.nio.charset.CodingErrorAction.REPORT)
      try Right(decoder.decode(java.nio.ByteBuffer.wrap(bytes)).toString)
      catch {
        case NonFatal(_) =>
          Left(
            PostgreSqlEnumerationCampaignV2WholePlanFailureCode.PlanUtf8Invalid
          )
      }
    }
  }

  private def sha256(bytes: Array[Byte]): String = {
    val digest = MessageDigest.getInstance("SHA-256").digest(bytes)
    val result = new StringBuilder(digest.length * 2)
    digest.foreach { byte =>
      val unsigned = byte & 0xff
      result.append("0123456789abcdef".charAt(unsigned >>> 4))
      result.append("0123456789abcdef".charAt(unsigned & 0x0f))
    }
    result.result()
  }

  private def malformed[A]
      : Left[PostgreSqlEnumerationCampaignV2WholePlanFailureCode, A] =
    Left(PostgreSqlEnumerationCampaignV2WholePlanFailureCode.PlanJsonMalformed)

  private def executionEvidence[A]
      : Left[PostgreSqlEnumerationCampaignV2WholePlanFailureCode, A] =
    Left(
      PostgreSqlEnumerationCampaignV2WholePlanFailureCode.ExecutionEvidencePresent
    )

  private def numericUnsafe[A]
      : Left[PostgreSqlEnumerationCampaignV2WholePlanFailureCode, A] =
    Left(
      PostgreSqlEnumerationCampaignV2WholePlanFailureCode.NumericEvidenceUnsafe
    )

  private def jsonValuesExceeded[A]
      : Left[PostgreSqlEnumerationCampaignV2WholePlanFailureCode, A] =
    Left(
      PostgreSqlEnumerationCampaignV2WholePlanFailureCode.JsonValueLimitExceeded
    )
}
