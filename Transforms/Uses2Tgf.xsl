<?xml version="1.0"?>
<xsl:stylesheet version = '1.0' xmlns:xsl='http://www.w3.org/1999/XSL/Transform'>
	<xsl:output method="text" />
	<xsl:key name="main" match="MainUnitName" use="." />
	<xsl:template match="/">
		<xsl:text>#
</xsl:text>
		<xsl:for-each select="//MainUsedUnitStatement">
			<xsl:if test="key('main', UsedUnitName)" >
				<xsl:value-of select="concat(../MainUnitName, ' ', UsedUnitName)" />
				<xsl:text> main
</xsl:text>
			</xsl:if>
		</xsl:for-each>
		<xsl:for-each select="//InterfaceSection/UsedUnitName">
			<xsl:if test="key('main', .)" >
				<xsl:value-of select="concat(../../MainUnitName, ' ', .)" />
				<xsl:text> interface
</xsl:text>
			</xsl:if>
		</xsl:for-each>
		<xsl:for-each select="//ImplementationSection/UsedUnitName">
			<xsl:if test="key('main', .)" >
				<xsl:value-of select="concat(../../MainUnitName, ' ', .)" />
				<xsl:text> implementation
</xsl:text>
			</xsl:if>
		</xsl:for-each>
		<xsl:for-each select="//ContainsStatement">
			<xsl:if test="key('main', ConstantIdentifier)" >
				<xsl:value-of select="concat(../MainUnitName, ' ', ContainsIdentifier)" />
				<xsl:text> contains
</xsl:text>
			</xsl:if>
		</xsl:for-each>
	</xsl:template>
</xsl:stylesheet>
