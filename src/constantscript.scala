import java.io._
import scala.xml._
import scala.xml.transform._
import java.nio.channels.Channels
import scala.util.control.Exception.ultimately
import net.flaviusb.xml._

object fixer {
  object constantconverter extends RewriteWrapper {
    override def wrappedTransform(node: Node) = node match {
      case n @ Elem(pre, "cn", attmp, scope, child@ _*) if n.attribute("type") == None => {
        // We have to patch Attribute here, as Attribute.append makes use of getUniversalKey
        val att = attmp match {
          case n: WhiteSpace => new WhiteSpace(n.space, n.next) with whitespaceRootPatch;
          case n: UnprefixedAttribute => new UnprefixedAttribute(n.key, n.value, n.next) with whitespaceRootPatch;
          case n: PrefixedAttribute => new PrefixedAttribute(n.pre, n.key, n.value, n.next) with whitespaceRootPatch;
        }
        // Split into elements and Text
        val nod = child.filter(b => b match {
          case Text(t) => false;
          case a => true
        })
        val txt = child.filter(b => b match {
          case Text(t) => true;
          case a => false
        }).foldLeft("")((a, b) => a + b)
        val enot = """\W*([\-+]?[0-9]+(\.[0-9]+)?)[Ee]([\-+]?[0-9]+(\.[0-9]+)?)\W*""".r
        txt match {
          case enot(mantissa, _, exponent, _) => {
            Elem(pre, n.label, new WhiteSpace(" ", new UnprefixedAttribute("type", "e-notation", att)), scope, Text(mantissa) ++ Elem(n.prefix, "sep", null, n.scope) ++ Text(exponent) ++ nod: _*)
          }
          case _ => n
        }
      }
      case (otherwise: Node) => otherwise
    }
  }
  object xmlbaseremover extends RewriteWrapper {
    override def wrappedTransform(node: Node) = node match {
      case (nod: Elem) => {
        var mde = nod
        // First remove xml:base, if present
        // We have to do something stupid here, as Scala's support for removing a namespaced attribute is... shaky
        def rem(at: MetaData, uri: String, scope: NamespaceBinding, key: String): MetaData = at match {
          case p: PrefixedAttribute => {
            if (scope.getURI(p.pre) == uri)
              p.next;
            else
              if (p.next != null) {
                p.copy(rem(p.next, uri, scope, key));
              } else {
                p
              }
          }
          case Null => Null
          case _ => { if (at.next != null) at.copy(rem(at.next, uri, scope, key)); else at }
        }
          
        if (nod.attributes.get("http://www.w3.org/XML/1998/namespace", TopScope, "base").getOrElse(Text("nothing")).asInstanceOf[Text].toString == "")
          mde.copy(attributes=rem(nod.attributes, "http://www.w3.org/XML/1998/namespace", TopScope, "base"))
        else if (nod.attributes.get("http://www.w3.org/XML/1998/namespace", TopScope, "base").getOrElse(Text("")).asInstanceOf[Text].toString.startsWith("file:"))
          mde.copy(attributes=rem(nod.attributes, "http://www.w3.org/XML/1998/namespace", TopScope, "base"))
        else
          mde;
      }
      case (otherwise: Node) => otherwise
    }
  }
 def main(args: Array[String]) = {
    if (args.length == 0 || args(0) == "-h" || args(0) == "-?" || args(0) == "--help")
      println(
"""Usage: java -jar cellmlconverter.jar [-xe] file*
  Converts cellml files in place
  -x remove extraneous xml:base
  -e transform old e-notation to new e-notation
""")
    var x = false
    var e = false
    if (args.length > 1) {
      if (args(0) == "-x" || args(0) == "-xe" || args(0) == "-ex")
        x = true
      if (args(0) == "-e" || args(0) == "-xe" || args(0) == "-ex")
        e = true
    }
    if (args.length > 2) {
      if (args(1) == "-x" || args(1) == "-xe" || args(1) == "-ex")
        x = true
      if (args(1) == "-e" || args(1) == "-xe" || args(1) == "-ex")
        e = true
    }

    args.foreach(file => {
      if (!file.startsWith("-")) {
        if (x) {
          MinimalXMLTransformer.transformFile(file, xmlbaseremover);
        }
        if (e) {
          MinimalXMLTransformer.transformFile(file, constantconverter);
        }
      } 
    })
  }
}
