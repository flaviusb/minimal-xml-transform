package net.flaviusb.xml

import java.io._
import scala.xml._
import scala.xml.transform._
import java.nio.channels.Channels
import scala.util.control.Exception.ultimately


// Extend RewriteWrapper, and override wrappedTransform
trait RewriteWrapper extends RewriteRule {
  def wrappedTransform(node: Node): Node
  override def transform(node: Node) = node match {
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
      wrappedTransform(mde.copy(attributes=reverse(scala.xml.Null, mde.attributes)))
    }
    case (otherwise: Node) => wrappedTransform(otherwise)
  }
}

