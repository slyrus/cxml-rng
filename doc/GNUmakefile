.PHONY: index.html

all: index.html

html.xsl: macros.xsl html.xslx rename.xsl
	xsltproc macros.xsl html.xslx | xsltproc rename.xsl - >html.xsl

index.html: doc.xml html.xsl
	xsltproc cleanup.xsl doc.xml \
		| xsltproc html.xsl - \
		| tee debug.xml \
		| xsltproc paginate.xsl - >index.html
