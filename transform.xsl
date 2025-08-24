<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:template match="/">
    <html>
        <head>
            <style>
                .xml-head-act {
                    font-weight: bold;
                    color: red;
                    font-size: 1.5em;
                    margin: 1em 0;
                    text-transform: uppercase;
                }
                .xml-p { display: block; margin: 0.5em 0; }
                .xml-stage { font-style: italic; color: #666; }
            </style>
        </head>
        <body>
            <div id="xml">
                <xsl:apply-templates/>
            </div>
        </body>
    </html>
</xsl:stylesheet>

<xsl:template match="head[@type='act']">
    <div class="xml-head-act">
        <xsl:value-of select="."/>
    </div>
</xsl:template>

<xsl:template match="p">
    <div class="xml-p">
        <xsl:value-of select="."/>
    </div>
</xsl:template>

<xsl:template match="stage">
    <div class="xml-stage">
        <xsl:value-of select="."/>
    </div>
</xsl:template>

</xsl:stylesheet>