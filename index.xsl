<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:output method="html"
	      indent="yes"
	      doctype-public="-//W3C//DTD HTML 4.01 Transitional//EN"
	      doctype-system="http://www.w3.org/TR/html4/loose.dtd"/>

  <xsl:template match="/">
    <html>
      <head>
	<title>
	  cxml-rng: Relax NG for Closure XML
	</title>
	<link rel="stylesheet" type="text/css" href="index.css"/>
	<meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
      </head>
      <body style="width: 60%">
	<xsl:call-template name="header"/>
	<div id="homepage" class="main">
	  <p>
	    An implementation
	    of <a href="http://relaxng.org/spec-20011203.html">Relax
	    NG</a> schema validation
	    written in Common Lisp.
	  </p>
	  <p>
	    cxml-rng was written
	    by <a href="mailto:david@lichteblau.com">David
	    Lichteblau</a> and is designed as an add-on library for
	    <a href="http://common-lisp.net/project/cxml">Closure XML</a>.
	    It is available under an X11-style license.
	  </p>
	  <p>
	    Please send bug reports
	    to <a href="mailto:cxml-devel@common-lisp.net">
	      cxml-devel@common-lisp.net</a>
	    (<a href="http://common-lisp.net/cgi-bin/mailman/listinfo/cxml-devel">list information</a>).
	  </p>

	  <h3>Download and Installation</h3>

	  <p>
	    Download
	    cxml-rng <a
	    href="http://www.lichteblau.com/cxml-rng/download/">tarballs</a>
	    or get it from
	    git: <tt>http://www.lichteblau.com/git/cxml-rng.git</tt>
	  </p>
	  <p>
	    cxml-rng
	    needs <a href="http://common-lisp.net/project/cxml">Closure
	    XML</a> and <a href="http://weitz.de/cl-ppcre/">CL-PPCE</a>.
	    <a href="http://www.cliki.net/asdf">ASDF</a> is used for
	    compilation.  Register the .asd file, e.g. by symlinking it,
	    then compile cxml-rng using <tt>asdf:operate</tt>.
	  </p>
	  <pre>$ ln -sf `pwd`/cxml-rng.asd /path/to/your/registry/
* (asdf:operate 'asdf:load-op :cxml-rng)</pre>

	  <h3>Implementation-specific notes</h3>
	  <p>
	    At this point, cxml-rng is written to work with Lisp strings
	    (as opposed to runes and rods), and is meant to be used on
	    Lisp implementations with Unicode support.
	  </p>

	  <h3>Example</h3>
	  <p>
	    Use <a href="doc/pages/cxml-rng__parse-relax-ng.html">cxml-rng:parse-relax-ng</a>
	    to parse a Relax NG schema file.  The
	    resulting <a
	    href="doc/pages/cxml-rng__parsed-grammar.html">parsed-grammar</a>
	    object is a representation of a simplified schema using Lisp
	    objects, which has gone through most stages of simplification as
	    described the Relax NG
	    specification.  <a href="doc/pages/cxml-rng__serialize-grammar.html">cxml-rng:serialize-grammar</a>
	    can be used to write a Relax NG file in XML syntax for this
	    grammar.
	  </p>
	  <p>
	    In order to validate XML against a schema, create a
	    validation handler for the grammar
	    using <a href="doc/pages/cxml-rng__make-validator.html">cxml-rng:make-validator</a>.
	    The validation
	    handler processes SAX events and can be used with any
	    function generating such events, in particular
	    with <a
	    href="http://common-lisp.net/project/cxml/sax.html#parser">cxml:parse-file</a>.
	  </p>
	  <pre class="code">(cxml:parse-file "test.xml"
                 (cxml-rng:make-validator
                  (cxml-rng:parse-relax-ng #p"test.rng")))</pre>

	  <h3>Documentation</h3>
	  <p>
	    Detailed <a href="doc/index.html">API documentation</a>
	    is available.
	  </p>
	</div>
      </body>
    </html>
  </xsl:template>

  <xsl:template name="header">
    <div id="header">
      <table cellspacing="0" cellpadding="0" width="100%">
	<tr>
	  <td width="176">
	    <img src="doc/logo.png"/>
	  </td>
	  <td valign="center">
	    &#x2014;
	    <b> Relax NG for Closure XML</b>
	  </td>
	</tr>
      </table>
    </div>
  </xsl:template>
</xsl:stylesheet>
