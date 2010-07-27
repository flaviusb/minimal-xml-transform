import java.io._
import scala.xml._
import scala.xml.transform._
import java.nio.channels.Channels
import scala.util.control.Exception.ultimately

object fixer {
  object constantconverter extends RewriteRule {
    override def transform(node: Node) = node match {
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
        val int = txt match {
          case enot(mantissa, _, exponent, _) => {
            Elem(pre, n.label, new UnprefixedAttribute("type", "e-notation", WhiteSpace(" ", att)), scope, Text(mantissa) ++ Elem(n.prefix, "sep", null, n.scope) ++ Text(exponent) ++ nod: _*)
          }
          case _ => n
        }
        def reverse(first: MetaData, next: MetaData): MetaData = {
          // First time through is null, first
          // Last time through is last, null
          if (next == null || next == scala.xml.Null)
            return first;

          val intermediate = next.copy(first)
          reverse(intermediate, next.next)
        }
        int match {
          case i: Elem => i.copy(attributes=reverse(scala.xml.Null, i.attributes))
          case j => j
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
      case (otherwise: Node) => otherwise
    }
  }
  object xmlbaseremover extends RewriteRule {
    override def transform(node: Node) = node match {
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
          mde = mde.copy(attributes=rem(nod.attributes, "http://www.w3.org/XML/1998/namespace", TopScope, "base"))
        else if (nod.attributes.get("http://www.w3.org/XML/1998/namespace", TopScope, "base").getOrElse(Text("")).asInstanceOf[Text].toString.startsWith("file:"))
          mde = mde.copy(attributes=rem(nod.attributes, "http://www.w3.org/XML/1998/namespace", TopScope, "base"))

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
      case (otherwise: Node) => otherwise
    }
  }
  // Epic yak shaving
  def isAtomAndNotText(x: Node) = x.isAtom && !x.isInstanceOf[Text]
  def buildAttrString(m: MetaData, sb: StringBuilder): Unit = {
    if (m != null && m != Null) {
      m.toString1(sb)
      buildAttrString(m.next, sb)
    }
  }
  def buildNSString(ns: NamespaceBinding, sb: StringBuilder, stop: NamespaceBinding): Unit = {
    if (ns eq stop) return    // contains?

    val s = "xmlns%s=\"%s\"".format(
      (if (ns.prefix != null) ":" + ns.prefix else ""),
      (if (ns.uri != null) ns.uri else "")
    )
    if (ns.parent != stop) {
      buildNSString(ns.parent, sb, stop)
      ns.parent match {
        case ws: NamespaceBindingS => sb append ws.space
        case _ => sb append " "
      }
    }
    sb append s
  }

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
        if (x.attributes ne null) buildAttrString(x.attributes, sb)
        //buildNSString(x.scope, sb, pscope)
        if (x.scope != pscope)
          x.scope match {
            case ns: NamespaceBindingS => sb append ns.space
            case _ => 
          }
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
        //sb.append(' ')
        toXML(x, pscope, sb, stripComments, decodeEntities, preserveWhitespace, minimizeTags)
      }
    }
    else children foreach { toXML(_, pscope, sb, stripComments, decodeEntities, preserveWhitespace, minimizeTags) }
  } 
  final def write(w: java.io.Writer, node: Seq[Node]) {
    node.foreach(n => w.write(toXML(n, preserveWhitespace=true, minimizeTags=true).toString))
  }
  final def transformFile(file: String, rule: RewriteRule): Unit = {
    val cpa = scala.xml.parsing.SpaceParser.fromFile(new File(file), true)
    var info_prolog: Tuple3[Option[String], Option[String], Option[Boolean]] = Tuple3(None, None, None)
    cpa.nextch // is prolog ?
    var children: NodeSeq = null
    if ('?' == cpa.ch) {
      cpa.nextch;
      info_prolog = cpa.prolog()

      children = cpa.content(TopScope) // DTD handled as side effect
    }
    else {
      val ts = new NodeBuffer();
      cpa.content1(TopScope, ts); // DTD handled as side effect
      ts &+ cpa.content(TopScope);
      children = NodeSeq.fromSeq(ts);
    }
    val fdoc = new RuleTransformer(rule).transform(children)
    val fos = new FileOutputStream(file)
    val w = Channels.newWriter(fos.getChannel(), info_prolog._2.getOrElse("utf-8"))
    w.write("<?xml version=\""+info_prolog._1.getOrElse("1.0")+"\" encoding=\""+info_prolog._2.getOrElse("utf-8")+"\"?>\n")
    ultimately(w.close())(write(w, fdoc))
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
          transformFile(file, xmlbaseremover);
        }
        if (e) {
          transformFile(file, constantconverter);
        }
      } 
    })
  }
}
