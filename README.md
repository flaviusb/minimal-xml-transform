minimal-xml-transform
=====================

This is designed to allow you to write wrapped RewriteRules which will not affect the formatting of the output xml as much as the vanilla scala libraries do.
For example, whitespace inside attribute lists is preserved, as is attribute order.

The scala scripts are under src/
Use sbt to build the example, a scala e-notation converter.
