<?xml version="1.0" encoding="utf-8"?>

<xsl:stylesheet version="1.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:output method="text" version="1.0"
encoding="utf-8" indent="yes"/>

<xsl:template match="/">
    digraph my_fsm {
     <!-- rankdir=LR -->

    <xsl:for-each select="definition/syntaxtree/node">
        "<xsl:value-of select="@name" />" [
            label="<xsl:value-of select="@name" />
        <xsl:for-each select="attributes/attribute">
            |<xsl:value-of select="@name" />
        </xsl:for-each>"
        shape="record"];
    </xsl:for-each>

    edge [color=blue]
    <xsl:for-each select="definition/nodesets/nodeset">
        <xsl:variable name="name">
            <xsl:value-of select="@name"/>
        </xsl:variable>
        <xsl:for-each select="target/node">
            <xsl:copy-of select="$name" /> -> <xsl:value-of select="@name"/>;
        </xsl:for-each>
    </xsl:for-each>

    edge [color=black]
    <xsl:for-each select="definition/syntaxtree/node">
        <xsl:variable name="name">
            <xsl:value-of select="@name"/>
        </xsl:variable>
        <xsl:for-each select="sons/son">
            <xsl:variable name="label">
                <xsl:value-of select="@name"/>
            </xsl:variable>
            <xsl:for-each select="targets/target">
                <xsl:variable name="mandatory">
                    <xsl:value-of select="@mandatory"/>
                </xsl:variable>
                <xsl:if test="$mandatory='yes'">
                    <xsl:for-each select="set">
                        <xsl:copy-of select="$name" /> -> <xsl:value-of select="@name"/>[label = "<xsl:copy-of select="$label"/>"];
                    </xsl:for-each>
                    <xsl:for-each select="node">
                        <xsl:copy-of select="$name" /> -> <xsl:value-of select="@name"/>[label = "<xsl:copy-of select="$label"/>"];
                    </xsl:for-each>
                </xsl:if>
            </xsl:for-each>
        </xsl:for-each>
    </xsl:for-each>

    edge [color=red]
    <xsl:for-each select="definition/syntaxtree/node">
        <xsl:variable name="name">
            <xsl:value-of select="@name"/>
        </xsl:variable>
        <xsl:for-each select="sons/son">
            <xsl:variable name="label">
                <xsl:value-of select="@name"/>
            </xsl:variable>
            <xsl:for-each select="targets/target">
                <xsl:variable name="mandatory">
                    <xsl:value-of select="@mandatory"/>
                </xsl:variable>
                <xsl:if test="$mandatory='no'">
                    <xsl:for-each select="set">
                        <xsl:copy-of select="$name" /> -> <xsl:value-of select="@name"/>[label = "<xsl:copy-of select="$label"/>"];
                    </xsl:for-each>
                    <xsl:for-each select="node">
                        <xsl:copy-of select="$name" /> -> <xsl:value-of select="@name"/>[label = "<xsl:copy-of select="$label"/>"];
                    </xsl:for-each>
                </xsl:if>
            </xsl:for-each>
        </xsl:for-each>
    </xsl:for-each>

    }
</xsl:template>

</xsl:stylesheet>
