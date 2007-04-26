<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
		xmlns:macro="http://lichteblau.com/macro"
		xmlns:extra="http://lichteblau.com/extra"
		version="1.0">
  <xsl:output method="xml" indent="yes"/>

  <xsl:template match="@*|node()">
    <xsl:copy>
      <xsl:apply-templates select="@*|node()"/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="macro:maybe-package-prefix">
    <extra:if test="$packagep">
      <span style="color: #777777">
	<extra:value-of select="../../@name"/>
	<extra:text>::</extra:text>
      </span>
    </extra:if>
  </xsl:template>
</xsl:stylesheet>
