<?xml version="1.0"?>
<xsl:stylesheet version = '1.0' xmlns:xsl='http://www.w3.org/1999/XSL/Transform'>
	<xsl:output method="text" />
	<xsl:key name="main" match="MainUnitName" use="." />
	<xsl:template match="/">
		<xsl:text>digraph uses {
</xsl:text>
		<xsl:for-each select="//MainUsedUnitStatement[key('main', UsedUnitName)]">
			<xsl:value-of select="translate(concat(../MainUnitName, ' -&gt; ', UsedUnitName), '.', '_')" />
			<xsl:text> [color=red]
</xsl:text>
		</xsl:for-each>
		<xsl:for-each select="//InterfaceSection/UsedUnitName[key('main', .)]">
			<xsl:value-of select="translate(concat(../../MainUnitName, '  -&gt; ', .), '.', '_')" />
			<xsl:text> [color=green]
</xsl:text>
		</xsl:for-each>
		<!-- xsl:for-each select="//ImplementationSection/UsedUnitName[key('main', .)]">
			<xsl:value-of select="translate(concat(../../MainUnitName, '  -&gt; ', .), '.', '_')" />
			<xsl:text> [color=blue]
</xsl:text>
		</xsl:for-each -->
		<xsl:for-each select="//ContainsStatement[key('main', ConstantIdentifier)]">
			<xsl:value-of select="translate(concat(../MainUnitName, '  -&gt; ', ContainsIdentifier), '.', '_')" />
			<xsl:text> [color=yellow]
</xsl:text>
		</xsl:for-each>
		<xsl:text>}
</xsl:text>
	</xsl:template>
</xsl:stylesheet>
