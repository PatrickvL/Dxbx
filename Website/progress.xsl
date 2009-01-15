<?xml version='1.0'?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">


<xsl:template match="/DXBXTRANSLATION">
	
<html>


  <BODY bgcolor="black">
	

  
  <xsl:for-each select="UNIT">
	<div style="background-color:teal;color:white;padding:4px">
        <span style="font-weight:bold"><xsl:value-of select="@name"/></span>        
    </div>


	<xsl:for-each select="SYMBOL">
	<div style="background-color:#99FF33;color:black;padding:4px">
        <span style="font-weight:bold"><xsl:value-of select="@name"/></span>        
    </div>

	<div style="background-color:white;color:black;padding:4px">
	  <span style="font-weight:bold">Line: <xsl:value-of select="@line"/></span>
	</div>

	<div style="background-color:white;color:black;padding:4px">
      <span style="font-weight:bold">From branch: <xsl:value-of select="@frombranch"/></span>
	</div>

 	<div style="background-color:white;color:black;padding:4px">
      <span style="font-weight:bold">From source:	<xsl:value-of select="@fromsource"/></span>
	</div>

	<div style="background-color:white;color:black;padding:4px">
	  <span style="font-weight:bold">From revision: <xsl:value-of select="@fromrevision"/></span>
	</div>

    <div style="background-color:white;color:black;padding:4px">
	  <span style="font-weight:bold">Translator: <xsl:value-of select="@translator"/></span>
	</div>

	<div style="background-color:white;color:black;padding:4px">
	  <span style="font-weight:bold">Done: <xsl:value-of select="@done"/> </span>
	</div>

	</xsl:for-each>

  </xsl:for-each>



  </BODY>
  </html>
  
  
  
</xsl:template>

</xsl:stylesheet>