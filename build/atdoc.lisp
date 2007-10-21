(asdf:operate 'asdf:load-op :cxml-rng)
(asdf:operate 'asdf:load-op :atdoc)
(atdoc:generate-documentation '(:cxml-rng :cxml-types)
			      "/home/david/src/lisp/cxml-rng/doc/"
			      :index-title "cxml-rng API reference"
			      :heading "cxml-rng"
			      :css "cxml-rng.css")
