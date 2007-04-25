<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:output method="html"
	      indent="yes"
	      doctype-public="-//W3C//DTD HTML 4.01 Transitional//EN"
	      doctype-system="http://www.w3.org/TR/html4/loose.dtd"/>

  <xsl:template match="documentation">
    <html>
      <head>
	<title>
	  <xsl:value-of select="@title"/>
	</title>
	<link rel="stylesheet" type="text/css" href="doc.css"/>
	<meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
      </head>
      <body>
	<xsl:call-template name="header"/>
	<div class="main">
	  Index of packages:
	  <xsl:apply-templates/>
	</div>
      </body>
    </html>
  </xsl:template>

  <xsl:template match="arguments">
    <h3>Arguments</h3>
    <div class="indent">
      <ul>
	<xsl:for-each select="arg">
	  <li>
	    <tt>
	      <xsl:value-of select="@arg"/>
	    </tt>
	    <xsl:text> -- </xsl:text>
	    <xsl:apply-templates/>
	  </li>
	</xsl:for-each>
      </ul>
    </div>
  </xsl:template>

  <xsl:template name="main-left">
    <xsl:apply-templates select="arguments"/>
    <xsl:choose>
      <xsl:when test="documentation-string">
	<h3>Details<a name="details"/></h3>
	<xsl:apply-templates select="documentation-string"/>
      </xsl:when>
      <xsl:otherwise>
	<p style="color: red; font-weight: bold">
	  No documentation string.  Possibly unimplemented or incomplete.
	</p>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:apply-templates select="implementation-note"/>
  </xsl:template>

  <xsl:template name="main-right">
    <xsl:if test="see-also/constructor">
      <h3>Returned by</h3>
      <div class="indent">
	<table cellspacing="0" cellpadding="0">
	  <xsl:apply-templates select="see-also/constructor/see"/>
	</table>
      </div>
    </xsl:if>
    <xsl:if test="see-also/slot">
      <h3>Slot Access Functions</h3>
      <div class="indent">
	<table cellspacing="0" cellpadding="0">
	  <xsl:apply-templates select="see-also/slot/see"/>
	</table>
      </div>
    </xsl:if>
    <xsl:if test="see-also/other">
      <h3>See also</h3>
      <div class="indent">
	<table cellspacing="0" cellpadding="0">
	  <xsl:apply-templates select="see-also/other/see"/>
	</table>
      </div>
    </xsl:if>
  </xsl:template>

  <xsl:template name="main">
    <xsl:choose>
      <xsl:when test="see-also">
	<table cellspacing="0" cellpadding="0">
	  <tr>
	    <td valign="top" width="60%">
	      <xsl:call-template name="main-left"/>
	    </td>
	    <td valign="top" width="5%">
	      &#160;
	    </td>
	    <td valign="top" width="35%">
	      <xsl:call-template name="main-right"/>
	    </td>
	  </tr>
	</table>
      </xsl:when>
      <xsl:otherwise>
	<xsl:call-template name="main-left"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="package">
    <h2>
      <a href="pages/{@id}.html">
	Package
	<xsl:value-of select="@name"/>
      </a>
    </h2>
    <div style="left: 100px">
      <xsl:apply-templates select="documentation-string"/>
    </div>
    <xsl:document href="pages/{@id}.html"
		  method="html"
		  indent="yes"
		  doctype-public="-//W3C//DTD HTML 4.01 Transitional//EN"
		  doctype-system="http://www.w3.org/TR/html4/loose.dtd">
      <html>
	<head>
	  <title>
	    <xsl:text>Package </xsl:text>
	    <xsl:value-of select="@name"/>
	  </title>
	  <link rel="stylesheet" type="text/css" href="../doc.css"/>
	  <meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
	</head>
	<body>
	  <xsl:call-template name="page-header"/>
	  <div class="main">
	    <p class="noindent">
	      Up:
	      <a href="../index.html">
		<xsl:value-of select="/documentation/@title"/>
	      </a>
	    </p>
	    <h1>
	      Package
	      <xsl:value-of select="@name"/>
	    </h1>
	    <xsl:apply-templates select="documentation-string"/>
	    <table cellspacing="0" cellpadding="0">
	      <tr>
		<td valign="top">
		  <xsl:if test="sections">
		    <div style="margin-left: -30px">
		      <h3>About This Package</h3>
		    </div>
		    <xsl:apply-templates select="sections/section" mode="toc"/>
		    <br/>
		    <xsl:apply-templates select="sections"/>
		  </xsl:if>
		</td>
		<td valign="top">
		  <h3><a name="index"></a>Symbol Index</h3>
		  <xsl:apply-templates select="symbols" mode="symbol-index"/>
		</td>
	      </tr>
	    </table>
	  </div>
	</body>
      </html>
    </xsl:document>
  </xsl:template>

  <xsl:template match="*" mode="symbol-index"/>

  <xsl:template match="symbols" mode="symbol-index">
    <xsl:apply-templates mode="symbol-index">
      <xsl:sort select="@id" data-type="text" order="ascending"/>
    </xsl:apply-templates>
  </xsl:template>

  <xsl:template match="class" mode="symbol-index">
    <a href="{@id}.html">
      <tt><xsl:value-of select="@name"/></tt>
    </a>
    <xsl:text>, class</xsl:text>
    <xsl:call-template name="undocumented"/>
    <br/>

    <xsl:document href="{@id}.html"
		  method="html"
		  indent="yes"
		  doctype-public="-//W3C//DTD HTML 4.01 Transitional//EN"
		  doctype-system="http://www.w3.org/TR/html4/loose.dtd">
      <html>
	<head>
	  <title>
	    <xsl:text>Class </xsl:text>
	    <xsl:value-of select="@name"/>
	  </title>
	  <link rel="stylesheet" type="text/css" href="../doc.css"/>
	  <meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
	</head>
	<body>
	  <xsl:call-template name="page-header"/>
	  <div class="main">
	    <p class="noindent">
	      Package:
	      <a href="{../../@id}.html">
		<xsl:value-of select="../../@name"/>
	      </a>
	    </p>
	    <xsl:apply-templates select="." mode="page"/>
	  </div>
	</body>
      </html>
    </xsl:document>
  </xsl:template>

  <xsl:template match="function" mode="symbol-index">
    <a href="{@id}.html">
      <tt><xsl:value-of select="@name"/></tt>
    </a>
    <xsl:text>, function</xsl:text>
    <xsl:call-template name="undocumented"/>
    <br/>

    <xsl:document href="{@id}.html"
		  method="html"
		  indent="yes"
		  doctype-public="-//W3C//DTD HTML 4.01 Transitional//EN"
		  doctype-system="http://www.w3.org/TR/html4/loose.dtd">
      <html>
	<head>
	  <title>
	    <xsl:text>Function </xsl:text>
	    <xsl:value-of select="@name"/>
	  </title>
	  <link rel="stylesheet" type="text/css" href="../doc.css"/>
	  <meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
	</head>
	<body>
	  <xsl:call-template name="page-header"/>
	  <div class="main">
	    <p class="noindent">
	      Package:
	      <a href="{../../@id}.html">
		<xsl:value-of select="../../@name"/>
	      </a>
	    </p>
	    <xsl:apply-templates select="." mode="page"/>
	  </div>
	</body>
      </html>
    </xsl:document>
  </xsl:template>

  <xsl:template match="macro" mode="symbol-index">
    <a href="{@id}.html">
      <tt><xsl:value-of select="@name"/></tt>
    </a>
    <xsl:text>, macro</xsl:text>
    <xsl:call-template name="undocumented"/>
    <br/>

    <xsl:document href="{@id}.html"
		  method="html"
		  indent="yes"
		  doctype-public="-//W3C//DTD HTML 4.01 Transitional//EN"
		  doctype-system="http://www.w3.org/TR/html4/loose.dtd">
      <html>
	<head>
	  <title>
	    <xsl:text>Macro </xsl:text>
	    <xsl:value-of select="@name"/>
	  </title>
	  <link rel="stylesheet" type="text/css" href="../doc.css"/>
	  <meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
	</head>
	<body>
	  <xsl:call-template name="page-header"/>
	  <div class="main">
	    <p class="noindent">
	      Package:
	      <a href="{../../@id}.html">
		<xsl:value-of select="../../@name"/>
	      </a>
	    </p>
	    <xsl:apply-templates select="." mode="page"/>
	  </div>
	</body>
      </html>
    </xsl:document>
  </xsl:template>

  <xsl:template name="undocumented">
    <xsl:if test="not(documentation-string)">
      <xsl:text>&#160;</xsl:text>
      <span style="color: red">
	(undocumented)
      </span>
    </xsl:if>
  </xsl:template>

  <xsl:template match="variable" mode="symbol-index">
    <a href="{@id}.html">
      <tt><xsl:value-of select="@name"/></tt>
    </a>
    <xsl:text>, variable</xsl:text>
    <xsl:call-template name="undocumented"/>
    <br/>

    <xsl:document href="{@id}.html"
		  method="html"
		  indent="yes"
		  doctype-public="-//W3C//DTD HTML 4.01 Transitional//EN"
		  doctype-system="http://www.w3.org/TR/html4/loose.dtd">
      <html>
	<head>
	  <title>
	    <xsl:text>Variable </xsl:text>
	    <xsl:value-of select="@name"/>
	  </title>
	  <link rel="stylesheet" type="text/css" href="../doc.css"/>
	  <meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
	</head>
	<body>
	  <xsl:call-template name="page-header"/>
	  <div class="main">
	    <p class="noindent">
	      Package:
	      <a href="{../../@id}.html">
		<xsl:value-of select="../../@name"/>
	      </a>
	    </p>
	    <xsl:apply-templates select="." mode="page"/>
	  </div>
	</body>
      </html>
    </xsl:document>
  </xsl:template>

  <xsl:template match="class" mode="page">
    <h2>
      Class <xsl:value-of select="@name"/>
    </h2>
    <xsl:choose>
      <xsl:when test="see-also">
	<table cellspacing="0" cellpadding="0" width="100%">
	  <tr>
	    <td valign="top" width="60%">
	      <xsl:call-template name="class-left"/>
	    </td>
	    <td valign="top" width="5%">
	      &#160;
	    </td>
	    <td valign="top" width="35%">
	      <xsl:call-template name="main-right"/>
	    </td>
	  </tr>
	</table>
      </xsl:when>
      <xsl:otherwise>
	<xsl:call-template name="class-left"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="class-left">
    <h3>Superclasses</h3>
    <div class="indent">
      <xsl:for-each select="cpl/superclass">
	<xsl:call-template name="class-list"/>
      </xsl:for-each>
    </div>
    <h3>Documented Subclasses</h3>
    <div class="indent">
      <xsl:choose>
	<xsl:when test="subclasses/subclass">
	  <xsl:for-each select="subclasses/subclass">
	    <xsl:sort select="@id" data-type="text" order="ascending"/>
	    <xsl:call-template name="class-list"/>
	  </xsl:for-each>
	</xsl:when>
	<xsl:otherwise>
	  None
	</xsl:otherwise>
      </xsl:choose>
    </div>
    <xsl:call-template name="main-left"/>
  </xsl:template>
  
  <xsl:template name="class-list">
    <xsl:if test="position() != 1">
      <xsl:text>, </xsl:text>
    </xsl:if>
    <xsl:choose>
      <xsl:when test="@id">
	<a href="{@id}.html">
	  <tt>
	    <xsl:if test="@status = 'INTERNAL'">
	      <xsl:value-of select="@package"/>
	      <xsl:text>::</xsl:text>
	    </xsl:if>
	    <xsl:value-of select="@name"/>
	  </tt>
	</a>
      </xsl:when>
      <xsl:when test="@status = 'INTERNAL'">
	<tt style="color: #777777">
	  <xsl:value-of select="@package"/>
	  <xsl:text>::</xsl:text>
	  <xsl:value-of select="@name"/>
	</tt>
      </xsl:when>	  
      <xsl:otherwise>
	<tt style="color: #777777">
	  <xsl:value-of select="@package"/>
	  <xsl:text>:</xsl:text>
	  <xsl:value-of select="@name"/>
	</tt>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="function" mode="page">
    <h2>
      Function
      <xsl:value-of select="@name"/>
    </h2>
    <xsl:choose>
      <xsl:when test="see-also">
	<table cellspacing="0" cellpadding="0">
	  <tr>
	    <td valign="top" width="60%">
	      <xsl:call-template name="function-left"/>
	    </td>
	    <td valign="top" width="5%">
	      &#160;
	    </td>
	    <td valign="top" width="35%">
	      <xsl:call-template name="main-right"/>
	    </td>
	  </tr>
	</table>
      </xsl:when>
      <xsl:otherwise>
	<xsl:call-template name="function-left"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="function-left">
    <h3>Lambda List</h3>
    <div class="indent">
      <xsl:apply-templates select="lambda-list"/>
    </div>
    <xsl:apply-templates select="return"/>
    <xsl:call-template name="main-left"/>
  </xsl:template>

  <xsl:template match="macro" mode="page">
    <h2>
      Macro
      <xsl:value-of select="@name"/>
    </h2>
    <xsl:apply-templates select="lambda-list"/>
    <xsl:call-template name="main"/>
  </xsl:template>

  <xsl:template match="variable" mode="page">
    <h2>
      Variable
      <xsl:value-of select="@name"/>
    </h2>
    <xsl:call-template name="main"/>
  </xsl:template>

  <xsl:template match="lambda-list">
    <tt><xsl:value-of select="../@name"/></tt>
    <xsl:text> (</xsl:text>
    <xsl:for-each select="elt">
      <xsl:if test="position() != 1">
	<xsl:text>&#160;</xsl:text>
      </xsl:if>
      <b><xsl:value-of select="text()"/></b>
    </xsl:for-each>
    <xsl:text>)</xsl:text>
  </xsl:template>

  <xsl:template mode="about-arguments" match="lambda-list">
    <div class="def">
      <a href="{../@id}.html">
	Function
	<xsl:value-of select="../@name"/>
	<xsl:text> (</xsl:text>
	<xsl:for-each select="elt">
	  <xsl:if test="position() != 1">
	    <xsl:text>&#160;</xsl:text>
	  </xsl:if>
	  <xsl:value-of select="text()"/>
	</xsl:for-each>
	<xsl:text>)</xsl:text>
      </a>
    </div>
  </xsl:template>

  <xsl:template match="documentation-string">
    <div class="indent">
      <xsl:apply-templates/>
    </div>
  </xsl:template>

  <xsl:template match="short">
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="em">
    <i>
      <xsl:apply-templates/>
    </i>
  </xsl:template>

  <xsl:template match="var">
    <tt>
      <xsl:apply-templates/>
    </tt>
  </xsl:template>

  <xsl:template match="code">
    <tt>
      <xsl:apply-templates/>
    </tt>
  </xsl:template>

  <xsl:template match="fun">
    <a href="{@id}.html">
      <tt>
	<xsl:apply-templates/>
      </tt>
    </a>
  </xsl:template>

  <xsl:template match="a">
    <a href="{@a}">
      <xsl:apply-templates/>
    </a>
  </xsl:template>

  <xsl:template match="class">
    <a href="{@id}.html">
      <tt>
	<xsl:apply-templates/>
      </tt>
    </a>
  </xsl:template>

  <xsl:template match="variable">
    <a href="{@id}.html">
      <tt>
	<xsl:apply-templates/>
      </tt>
    </a>
  </xsl:template>

  <xsl:template match="itemize">
    <ul>
      <xsl:apply-templates/>
    </ul>
  </xsl:template>

  <xsl:template match="item">
    <li>
      <xsl:apply-templates/>
    </li>
  </xsl:template>

  <xsl:template match="see">
    <tr>
      <td>
	<a href="{@id}.html">
	  <tt>
	    <xsl:apply-templates/>
	  </tt>
	</a>
      </td>
      <xsl:if test="@see">
	<td>
	  &#160;&#160;&#160;&#160;
	  <i>
	    <xsl:value-of select="@see"/>
	  </i>
	</td>
      </xsl:if>
    </tr>
  </xsl:template>

  <xsl:template match="return">
    <h3>Returns</h3>
    <div class="indent">
      <xsl:apply-templates/>
    </div>
  </xsl:template>

  <xsl:template match="implementation-note">
    <h3>Implementation notes</h3>
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="break">
    <br/><br/>
  </xsl:template>

  <xsl:template match="sections">
    <xsl:for-each select="section">
      <h2>
	<a name="{generate-id()}"/>
	<xsl:value-of select="@section"/>
      </h2>
      <xsl:apply-templates/>
    </xsl:for-each>
  </xsl:template>

  <xsl:template match="section" mode="toc">
    <a href="#{generate-id()}" style="font-weight: bold">
      <xsl:value-of select="@section"/>
    </a>
    <br/>
  </xsl:template>

  <xsl:template match="aboutfun">
    <xsl:variable name="fun" select="text()"/>
    <xsl:apply-templates mode="about-arguments"
			 select="//function[@name=$fun]/lambda-list"/>
    <div style="margin-left: 3em">
      <xsl:choose>
	<xsl:when test="//function[@name=$fun]/documentation-string//short">
	  <xsl:for-each select="//function[@name=$fun]">
	    <xsl:apply-templates select="documentation-string//short"/>
	    <xsl:text> </xsl:text>
	    <a href="{@id}.html#details">...</a>
	  </xsl:for-each>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:apply-templates
	     select="//function[@name=$fun]/documentation-string"/>
	</xsl:otherwise>
      </xsl:choose>
    </div>
    <br/>
  </xsl:template>

  <xsl:template match="aboutclass">
    <xsl:variable name="name" select="text()"/>
    <xsl:for-each select="//class[@name=$name]">
      <div class="def">
	<a href="{@id}.html">
	  Class
	  <xsl:value-of select="@name"/>
	</a>
      </div>
    </xsl:for-each>
    <div style="margin-left: 3em">
      <xsl:choose>
	<xsl:when test="//class[@name=$name]/documentation-string//short">
	  <xsl:for-each select="//class[@name=$name]">
	    <xsl:apply-templates select="documentation-string//short"/>
	    <xsl:text> </xsl:text>
	    <a href="{@id}.html#details">...</a>
	  </xsl:for-each>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:apply-templates
	     select="//class[@name=$name]/documentation-string"/>
	</xsl:otherwise>
      </xsl:choose>
    </div>
    <br/>
  </xsl:template>

  <xsl:template name="page-header">
    <xsl:call-template name="header">
      <xsl:with-param name="base" select="'../'"/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template name="header">
    <xsl:param name="base"/>
    <div id="header">
      <table cellspacing="0" cellpadding="0" width="100%">
	<tr>
	  <td width="176">
	    <a id="headerlink" href="{$base}../index.html">
	      <img src="{$base}logo.png" border="0"/>
	    </a>
	  </td>
	  <td valign="center">
	    &#x2014;
	    <b> Relax NG for Closure XML</b>
	  </td>
	  <td valign="center" align="right">
            <b>API documentation</b>
	  </td>
	</tr>
      </table>
    </div>
  </xsl:template>
</xsl:stylesheet>
