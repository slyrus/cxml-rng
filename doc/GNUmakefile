.PHONY: index.html

all: index.html

html.xsl.out: macros.xsl html.xsl rename.xsl
	xsltproc macros.xsl html.xsl | xsltproc rename.xsl - >html.xsl.out

index.html: doc.xml html.xsl.out
	xsltproc cleanup.xsl doc.xml \
		| xsltproc html.xsl.out - \
		| tee debug.xml \
		| xsltproc paginate.xsl - >index.html
