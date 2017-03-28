(defsystem "cxml-rng"
  :serial t
  :components
  ((:file "package")
   (:file "floats")
   (:file "unicode")
   (:file "nppcre")
   (:file "types")
   (:file "parse")
   (:file "validate")
   (:file "test")
   (:file "clex")
   (:file "compact"))
  :depends-on ("cxml" "cl-ppcre" "yacc" "parse-number" "cl-base64"))
