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

(defpackage :cxml-types
  (:use :cl)
  (:export #:data-type
	   #:find-type
	   #:type-library
	   #:type-name
	   #:type-context-dependent-p
	   #:parse
	   #:equal-using-type
	   #:validp
	   #:validation-context
	   #:sax-validation-context-mixin
	   #:klacks-validation-context
	   #:make-klacks-validation-context
	   #:context-find-namespace-binding)
  (:documentation
   "@code{cxml-types} defines an extensible interface for XML-related
    data types as required for use in Relax NG validation.
    It includes Relax NG's minimal built-in type library, which is named 
    @code{:||} and defines the types \"string\" and \"token\".
    In addition, it implements the built-in types of
    @a[http://www.w3.org/TR/xmlschema-2/]{XML Schema Datatypes}
    as specified in @a[http://relaxng.org/xsd-20010907.html]{Guidelines for
    using W3C XML Schema Datatypes with RELAX NG}.  The XSD type library
    is named @code{:|http://www.w3.org/2001/XMLSchema-datatypes|}.

    @begin[Type instances]{section}
      Each type, together with its parameters, is represented by an
      instance of @code{data-type}.  The generic function @fun{find-type},
      defined for each library, creates type instances.  A type's properties
      are accessible using @fun{type-name}, @fun{type-library}, and
      @fun{type-context-dependent-p}.

      @aboutclass{datatype}
      @aboutfun{find-type}
      @aboutfun{type-name}
      @aboutfun{type-library}
      @aboutfun{type-context-dependent-p}
    @end{section}

    @begin[Using types]{section}
      Types allow strings to be tested for validity and equality.
      @fun{validp} checks whether a string can be parsed.  If it is valid,
      @fun{parse} will compute the string's @emph{value}, and return a
      Lisp object of a type-specific class as a representation of that value.
      Values returned by @fun{parse} can be compared for equality using
      @fun{equal-using-type}.

      @aboutfun{validp}
      @aboutfun{parse}
      @aboutfun{equal-using-type}
    @end{section}

    @begin[The validation context]{section}
      Some types are context dependent, as indicated by
      @fun{type-context-dependent-p}.  Those type need access to state
      computed by the XML parser implicitly, like namespace bindings or
      the Base URI.

      An abstract class @class{validation-context} is defined.
      Users of this API can implement a subclass of @class{validation-context}
      and define method for the generic functions listed below.

      In addition, two pre-defined validation context implementations are
      provided, one for use with SAX, the other based on Klacks.

      @aboutclass{validation-context}
      @aboutclass{sax-validation-context-mixin}
      @aboutclass{klacks-validation-context}
      @aboutfun{context-find-namespace-binding}
    @end{section}"))
