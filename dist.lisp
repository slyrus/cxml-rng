(asdf:operate 'asdf:load-op :atdoc)
(asdf:operate 'asdf:load-op :cxml-rng)

(atdoc:generate-html-documentation
 '(:cxml-rng :cxml-types)
 (merge-pathnames
  "doc/"
  (asdf:component-relative-pathname (asdf:find-system :cxml-rng)))
 :heading "cxml-rng")
