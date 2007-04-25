<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:output method="xml" indent="yes"/>

  <xsl:template match="@*|node()">
    <xsl:copy>
      <xsl:apply-templates select="@*|node()"/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="see"/>
  <xsl:template match="see-slot"/>
  <xsl:template match="see-constructor"/>
  <xsl:template match="arg"/>
  <xsl:template match="return"/>
  <xsl:template match="implementation-note"/>
  <xsl:template match="section"/>

  <xsl:template mode="extract" match="@*|node()">
    <xsl:copy>
      <xsl:apply-templates select="@*|node()"/>
    </xsl:copy>
  </xsl:template>

  <xsl:template mode="extract" match="see-slot">
    <see>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </see>
  </xsl:template>

  <xsl:template mode="extract" match="see-constructor">
    <see>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </see>
  </xsl:template>

  <xsl:template match="documentation-string">
    <xsl:if test=".//arg">
      <arguments>
	<xsl:apply-templates mode="extract" select=".//arg"/>
      </arguments>
    </xsl:if>

    <xsl:if test=".//section">
      <sections>
	<xsl:apply-templates mode="extract" select=".//section"/>
      </sections>
    </xsl:if>

    <xsl:if test=".//see or .//see-slot or .//see-constructor">
      <see-also>
	<xsl:if test=".//see">
	  <other>
	    <xsl:apply-templates mode="extract" select=".//see"/>
	  </other>
	</xsl:if>

	<xsl:if test=".//see-slot">
	  <slot>
	    <xsl:apply-templates mode="extract" select=".//see-slot"/>
	  </slot>
	</xsl:if>

	<xsl:if test=".//see-constructor">
	  <constructor>
	    <xsl:apply-templates mode="extract" select=".//see-constructor"/>
	  </constructor>
	</xsl:if>
      </see-also>
    </xsl:if>

    <xsl:apply-templates mode="extract" select=".//implementation-note"/>
    <xsl:apply-templates mode="extract" select=".//return"/>

    <documentation-string>
      <xsl:apply-templates/>
    </documentation-string>
  </xsl:template>
</xsl:stylesheet>
