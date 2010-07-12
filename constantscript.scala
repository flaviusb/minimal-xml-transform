import java.io._
import scala.xml._
import scala.xml.transform._
import java.nio.channels.Channels
import scala.util.control.Exception.ultimately

object constantconverter extends RewriteRule {
  override def transform(node: Node) = node match {
    case n @ Elem(pre, "cn", att, scope, child@ _*) if n.attribute("type") == None => {
      // Split into elements and Text
      val nod = child.filter(b => b match {
        case Text(t) => false;
        case a => true
      })
      val txt = child.filter(b => b match {
        case Text(t) => true;
        case a => false
      }).foldLeft("")((a, b) => a + b)
      val enot = """\w*([\+\-]?[0-9]+(\.[0-9]+)?)[Ee]([\+\-]?[0-9]+(\.[0-9]+)?)\w*""".r
      txt match {
        case enot(mantissa, _, exponent, _) => {
          Elem(pre, n.label, att.append(new UnprefixedAttribute("type", "e-notation", Null)), scope, Text(mantissa) ++ Elem(n.prefix, "sep", null, n.scope) ++ Text(exponent) ++ nod: _*)
        }
        case _ => n
      }
    }
    case (mde: Elem) => {
      // Time for some yak shaving; we must reverse the attribute list, as it is kept reversed at the moment in scala
      def reverse(first: MetaData, next: MetaData): MetaData = {
        // First time through is null, first
        // Last time through is last, null
        if (next == null || next == scala.xml.Null)
          return first;

        val intermediate = next.copy(first)
        reverse(intermediate, next.next)
      }
      mde.copy(attributes=reverse(scala.xml.Null, mde.attributes))
    }
    case otherwise => otherwise
  }
  // Epic yak shaving
  def isAtomAndNotText(x: Node) = x.isAtom && !x.isInstanceOf[Text]
  def toXML(
    x: Node,
    pscope: NamespaceBinding = TopScope,
    sb: StringBuilder = new StringBuilder,
    stripComments: Boolean = false,
    decodeEntities: Boolean = true,
    preserveWhitespace: Boolean = false,
    minimizeTags: Boolean = false): StringBuilder =
  {
    x match {
      case c: Comment => if (!stripComments) c buildString sb else sb
      case x: SpecialNode => x buildString sb
      case g: Group =>
        g.nodes foreach {toXML(_, x.scope, sb, stripComments, decodeEntities, preserveWhitespace, minimizeTags)}
        sb
      case _  =>
        // print tag with namespace declarations
        sb.append('<')
        x.nameToString(sb)
        if (x.attributes ne null) x.attributes.buildString(sb)
        x.scope.buildString(sb, pscope)
        if (x.child.isEmpty && minimizeTags) {
          // no children, so use short form: <xyz .../>
          sb.append("/>")
        } else {
          // children, so use long form: <xyz ...>...</xyz>
          sb.append('>')
          sequenceToXML(x.child, x.scope, sb, stripComments, decodeEntities, preserveWhitespace, minimizeTags)
          sb.append("</")
          x.nameToString(sb)
          sb.append('>')
        }
    }
  }

  def sequenceToXML(
    children: Seq[Node],
    pscope: NamespaceBinding = TopScope,
    sb: StringBuilder = new StringBuilder,
    stripComments: Boolean = false,
    decodeEntities: Boolean = true,
    preserveWhitespace: Boolean = false,
    minimizeTags: Boolean = false): Unit =
  {
    if (children.isEmpty) return
    else if (children forall isAtomAndNotText) { // add space
      val it = children.iterator
      val f = it.next
      toXML(f, pscope, sb, stripComments, decodeEntities, preserveWhitespace, minimizeTags)
      while (it.hasNext) {
        val x = it.next
        sb.append(' ')
        toXML(x, pscope, sb, stripComments, decodeEntities, preserveWhitespace, minimizeTags)
      }
    }
    else children foreach { toXML(_, pscope, sb, stripComments, decodeEntities, preserveWhitespace, minimizeTags) }
  } 
  final def write(w: java.io.Writer, node: Node) {
    w.write("<?xml version=\"1.0\" encoding=\"utf-8\"?>\n")
    w.write(toXML(node, preserveWhitespace=true, minimizeTags=true).toString)
  }
  def main(args: Array[String]) = {
    for(file <- args) {
      var doc = XML.load(file)
      val fdoc = new RuleTransformer(constantconverter).transform(doc).head
      val fos = new FileOutputStream(file)
      val w = Channels.newWriter(fos.getChannel(), "utf-8")
      ultimately(w.close())(write(w, fdoc))
    }
  }
}
