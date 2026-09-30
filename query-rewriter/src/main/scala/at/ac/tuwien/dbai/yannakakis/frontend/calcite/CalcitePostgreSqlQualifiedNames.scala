package at.ac.tuwien.dbai.yannakakis.frontend.calcite

import java.io.{Reader, StringReader}
import java.util.Locale
import org.apache.calcite.sql.parser.{SqlAbstractParserImpl, SqlParserImplFactory}
import org.apache.calcite.sql.parser.impl.{SimpleCharStream, SqlParserImpl, SqlParserImplConstants, SqlParserImplTokenManager, Token}

/** PostgreSQL accepts every keyword as an attribute name following a dot
  * (gram.y: indirection_el -> '.' attr_name; attr_name -> ColLabel).
  * Calcite 1.36's default and Babel grammars reject e.g. b.Date.
  *
  * Keep Calcite's lexer, source positions, quoting, comments and parser. Only
  * reclassify a lexer-recognized keyword immediately after a DOT token as an
  * unquoted identifier. No source text, token image or AST is rewritten and
  * no keyword list is maintained here. The ordinary frontend still determines
  * whether the parsed statement, relation and predicate are supported.
  * This adapter is opt-in and tested against the pinned Calcite lexer API.
  */
private[yannakakis] object CalcitePostgreSqlQualifiedNames extends SqlParserImplFactory {
  private lazy val metadata = new SqlParserImpl(new StringReader("")).getMetadata

  override def getParser(reader: Reader): SqlAbstractParserImpl = {
    val parser = new QualifiedNameParser(new CharacterStream(reader))
    reader match {
      case source: org.apache.calcite.util.SourceStringReader => parser.setOriginalSql(source.getSourceString)
      case _ => ()
    }
    parser
  }

  private final class CharacterStream(reader: Reader) extends SimpleCharStream(reader) {
    def configureTabs(size: Int): Unit = setTabSize(size)
  }

  // Calcite's token-manager constructor does not retain a character stream;
  // implement the two abstract-parser lifecycle hooks against our own stream.
  private final class QualifiedNameParser(stream: CharacterStream)
      extends SqlParserImpl(new QualifiedNameTokens(stream)) {
    override def setTabSize(size: Int): Unit = stream.configureTabs(size)
    override def ReInit(reader: Reader): Unit = {
      stream.ReInit(reader)
      super.ReInit(new QualifiedNameTokens(stream))
    }
    override def getMetadata: SqlAbstractParserImpl.Metadata = metadata
  }

  private final class QualifiedNameTokens(stream: SimpleCharStream)
      extends SqlParserImplTokenManager(stream) {
    private var afterDot = false

    override def ReInit(stream: SimpleCharStream): Unit = {
      super.ReInit(stream)
      afterDot = false
    }

    override def getNextToken(): Token = {
      val token = super.getNextToken()
      val isDot = token.kind == SqlParserImplConstants.DOT
      if (afterDot && metadata.isKeyword(token.image.toUpperCase(Locale.ROOT)))
        token.kind = SqlParserImplConstants.IDENTIFIER
      afterDot = isDot
      token
    }
  }
}
