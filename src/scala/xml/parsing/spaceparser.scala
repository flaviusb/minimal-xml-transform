
package scala.xml
package parsing

import java.io.File
import scala.io.Source

object SpaceParser extends {
  def fromFile(inp: File, preserveWS: Boolean) =
    new SpaceParser(Source.fromFile(inp), preserveWS) initialize

  def fromSource(inp: Source, preserveWS: Boolean) =
    new SpaceParser(inp, preserveWS) initialize
}

class SpaceParser(override val input: Source, override val preserveWS: Boolean) extends ConstructingParser(input, preserveWS) {
  /** parse a start or empty tag.
   *  [40] STag         ::= '<' Name { S Attribute } [S] 
   *  [44] EmptyElemTag ::= '<' Name { S Attribute } [S] 
   */
  protected override def xTag(pscope: NamespaceType): (String, AttributesType) = {
    val name = xName
     
    (name, mkAttributes(name, pscope))
  }
  override def mkAttributes(name: String, pscope: NamespaceBinding) =
    if (isNameStart(ch) || isSpace(ch)) xAttributes2(pscope)
    else (Null, pscope)
 
  /** parse attribute and create namespace scope, metadata
   *  [41] Attributes    ::= { S Name Eq AttValue }
   */
  def xAttributes2(pscope:NamespaceBinding): (MetaData,NamespaceBinding) = {
    var scope: NamespaceBindingS = if (pscope eq TopScope) TopScopeS else NamespaceBindingS(pscope.prefix, pscope.uri, pscope.parent)
    xAttributes2(scope);
  }
  def xAttributes2(pscope:NamespaceBindingS): (MetaData,NamespaceBinding) = {
    var scope: NamespaceBindingS = pscope
    var aMap: MetaData = Null
    if (isSpace(ch))
      aMap = WhiteSpace(xSpaceS, aMap); 
    while (isNameStart(ch)) {
      val pos = this.pos

      val qname = xName
      val _     = xEQ
      val value = xAttributeValue()

      Utility.prefix(qname) match {
        case Some("xmlns") =>
          val prefix = qname.substring(6 /*xmlns:*/ , qname.length);
          scope = new NamespaceBindingS(prefix, value, scope, space="");
          aMap = new fakePrefixedAttribute("xmlns", prefix, Text(value), aMap);
        
        case Some(prefix)       => 
          val key = qname.substring(prefix.length+1, qname.length);
          aMap = new PrefixedAttribute(prefix, key, Text(value), aMap);

        case _             => 
          if( qname == "xmlns" ) {
            scope = new NamespaceBindingS(null, value, scope, space="");
            aMap = new fakeUnprefixedAttribute("xmlns", Text(value), aMap)
          } else 
            aMap = new UnprefixedAttribute(qname, Text(value), aMap);
      }
            
      if (isSpace(ch))
        aMap = aMap match {
          case coalesce: WhiteSpace => { xSpaceS; coalesce }
          case other => WhiteSpace(xSpaceS, other)
        } 
    }

    if(!aMap.wellformed(scope))
        reportSyntaxError( "double attribute");

    (aMap,scope)
  }
  /** skip optional space S? */
  def xSpaceOptS: String = {val acc: StringBuilder = new StringBuilder ; while (isSpace(ch) && !eof) {acc append ch; nextch } ; acc.toString }

  /** scan [3] S ::= (#x20 | #x9 | #xD | #xA)+ */
  def xSpaceS: String =
    if (isSpace(ch)) { val acc: StringBuilder = new StringBuilder; acc append ch; nextch; acc append xSpaceOptS; acc.toString }
    else { xHandleError(ch, "whitespace expected"); "" }

}
