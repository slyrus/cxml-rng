.PHONY: index.html

index.html: doc.xml
	xsltproc cleanup.xsl doc.xml | \
		xsltproc html.xsl - >index.html
