all: index.html doc

index.html: index.xml index.xsl
	xsltproc index.xsl $< >index.html

.PHONY: doc
doc:
	cd doc && make
