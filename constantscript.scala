import java.io.File
import scala.xml._
import scala.xml.transform._

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
        case _ => Text("")
      }
    }
    case otherwise => otherwise
  }
  def main(args: Array[String]) = {
    for(file <- args) {
      var doc = XML.load(file)
      val fdoc = new RuleTransformer(constantconverter).transform(doc).head
      println(fdoc)
      //XML.save(file, fdoc)
    }
  }
}
