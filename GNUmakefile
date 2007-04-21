all: index.html doc

.PHONY: index.html
index.html:
	echo '<empty/>' | xsltproc index.xsl - >index.html

.PHONY: doc
doc:
	cd doc && make
