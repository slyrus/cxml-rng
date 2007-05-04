(defpackage :cxml-rng
  (:use :cl)
  (:export #:rng-error
	   #:rng-error-line-number
	   #:rng-error-column-number
	   #:rng-error-system-id

	   #:schema
	   #:schema-start

	   #:parse-schema
	   #:parse-compact
	   #:serialize-schema
	   #:make-validator

	   #:pattern
	   #:element
	   #:attribute
	   #:group
	   #:interleave
	   #:choice
	   #:one-or-more
	   #:ref
	   #:empty
	   #:text
	   #:value
	   #:data
	   #:not-allowed
	   #:list-pattern

	   #:pattern-a
	   #:pattern-b
	   #:pattern-child
	   #:pattern-element
	   #:pattern-except
	   #:pattern-name
	   #:pattern-params
	   #:pattern-string
	   #:pattern-type
	   #:pattern-value

	   #:name-class
	   #:any-name
	   #:name
	   #:ns-name
	   #:name-class-choice

	   #:any-name-except
	   #:name-uri
	   #:name-lname
	   #:ns-name-uri
	   #:ns-name-except
	   #:name-class-choice-a
	   #:name-class-choice-b)
  (:documentation
   "@code{cxml-rng} implements @a[http://relaxng.org/spec-20011203.html]{
    Relax NG} schema validation for Closure XML.

    Support for @a[http://relaxng.org/compact-20021121.html]{Compact Syntax}
    is included.

    @begin[Example]{section}
    @begin{code}
    (cxml:parse-file \"test.xml\"
                     (cxml-rng:make-validator
                      (cxml-rng:parse-schema #p\"test.rng\")))
    @end{code}
    @end{section}
    @begin[Classes]{section}
    @aboutclass{schema}
    @aboutclass{rng-error}
    @end{section}
    @begin[Parsing and validating]{section}
    @aboutfun{parse-schema}
    @aboutfun{make-validator}
    @aboutfun{serialize-grammar}
    @end{section}
    @begin[Grammar introspection]{section}
    The following classes and function are exported so that users can
    take a peek at the internals of the parsed and simplified grammar.

    @aboutfun{schema-start}
    @aboutclass{attribute}
    @aboutclass{choice}
    @aboutclass{data}
    @aboutclass{element}
    @aboutclass{empty}
    @aboutclass{group}
    @aboutclass{interleave}
    @aboutclass{list-pattern}
    @aboutclass{not-allowed}
    @aboutclass{one-or-more}
    @aboutclass{pattern}
    @aboutclass{ref}
    @aboutclass{text}
    @aboutclass{value}
    @aboutfun{pattern-child}
    @aboutfun{pattern-a}
    @aboutfun{pattern-b}
    @aboutfun{pattern-name}
    @aboutfun{pattern-element}
    @aboutfun{pattern-type}
    @aboutfun{pattern-string}
    @aboutfun{pattern-value}
    @aboutfun{pattern-params}
    @aboutfun{pattern-except}
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
	   #:context-find-namespace-binding
	   #:rng-type
	   #:token-type
	   #:string-type
	   #:xsd-type)
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

    @begin[Example]{section}
    @begin{pre}
* (setf ttt (cxml-types:find-type :|| \"token\"))
#<CXML-TYPES:TOKEN-TYPE {1002D16B71@}>
* (cxml-types:parse ttt \"a b\")
\"a b\"
* (cxml-types:parse ttt \"a     b\")
\"a b\"
* (cxml-types:equal-using-type ttt ** *)
T
    @end{pre}
    @end{section}
    @begin[Type instances]{section}
      Each type, together with its parameters, is represented by an
      instance of @code{data-type}.  The generic function @fun{find-type},
      defined for each library, creates type instances.  A type's properties
      are accessible using @fun{type-name}, @fun{type-library}, and
      @fun{type-context-dependent-p}.

      @aboutclass{data-type}
      @aboutclass{rng-type}
      @aboutclass{xsd-type}
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
      @fun{type-context-dependent-p}.  Those types need access to state
      computed by the XML parser implicitly, like namespace bindings or
      the Base URI.

      An abstract class @class{validation-context} is defined that
      users of this API can implement a subclass of 
      to define methods for the generic functions listed below.

      In addition, two pre-defined validation context implementations are
      provided, one for use with SAX, the other based on Klacks.

      @aboutclass{validation-context}
      @aboutclass{sax-validation-context-mixin}
      @aboutclass{klacks-validation-context}
      @aboutfun{context-find-namespace-binding}
    @end{section}"))
