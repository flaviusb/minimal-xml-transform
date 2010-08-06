package net.flaviusb.xml

import java.io._
import scala.xml._
import scala.xml.transform._
import java.nio.channels.Channels
import scala.util.control.Exception.ultimately

object MinimalXMLTransformer {
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
}
