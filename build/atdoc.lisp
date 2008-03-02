(asdf:operate 'asdf:load-op :cxml-rng)
(asdf:operate 'asdf:load-op :atdoc)
(let* ((base (asdf:component-pathname (asdf:find-system :cxml-rng)))
       (atdoc-directory (merge-pathnames "doc/" base)))
  (ensure-directories-exist atdoc-directory)
  (atdoc:generate-documentation '(:cxml-rng :cxml-types)
				atdoc-directory
				:index-title "cxml-rng API reference"
				:heading "cxml-rng"
				:css "cxml-rng.css"))
