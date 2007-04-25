.PHONY: index.html

index.html: doc.xml
	xsltproc html.xsl doc.xml >index.html

test.xml: doc.xml
	xsltproc cleanup.xsl doc.xml >test.xml
