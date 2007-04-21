(defpackage :cxml-rng
  (:use :cl)
  (:export #:rng-error
	   #:parsed-grammar
	   #:parse-relax-ng
	   #:serialize-grammar
	   #:make-validator)
  (:documentation
   "@code{cxml-rng} implements @a[http://relaxng.org/spec-20011203.html]{
    Relax NG} schema validation for Closure XML.

    @begin[Example]{section}
    @begin{code}
    (cxml:parse-file \"test.xml\"
                     (cxml-rng:make-validator
                      (cxml-rng:parse-relax-ng #p\"test.rng\")))
    @end{code}
    @end{section}
    @begin[Classes]{section}
    @aboutclass{parsed-grammar}
    @aboutclass{rng-error}
    @end{section}
    @begin[Parsing and validating]{section}
    @aboutfun{parse-relax-ng}
    @aboutfun{make-validator}
    @aboutfun{serialize-grammar}
    @end{section}"))
