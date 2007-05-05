;;; -*- show-trailing-whitespace: t; indent-tabs: nil -*-
;;;
;;; Copyright (c) 2007 David Lichteblau. All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;
;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :cxml-types)

(defclass data-type () ()
  (:documentation
   "@short{The abstract superclass of all types.}

    Each type belongs to a datatype library, named by a keyword.  In each
    library, the types are named by strings.

    @see-constructor{find-type}
    @see-slot{type-name}
    @see-slot{type-library}
    @see-slot{type-context-dependent-p}
    @see{parse}
    @see{equal-using-type}
    @see{validp}"))

(defgeneric find-type (library name &key &allow-other-keys)
  (:documentation
   "@arg[library]{datatype library, a keyword symbol}
    @arg[name]{the type's name, a string}
    @arg[args]{type parameters, strings named by keyword arguments}
    @return{an instance of @class{data-type}, or @code{nil}}
    @short{Look up the type named @em{name} in datatype library @em{library}.}

    Return a type instance for this type and the additional parameters,
    or @code{nil} if the type does not exist.

    Additional parameters (knows as facets in XSD) can be passed to specify
    or restrict the type for the purposes of @fun{validp}.

    @see{data-type}"))

(defgeneric type-library (type)
  (:documentation
   "@arg[type]{an instance of @class{data-type}}
    @return{library name, a keyword}
    @short{Return the name of the library this type belongs to.}

    @see{type-name}
    @see{type-context-dependent-p}"))

(defgeneric type-name (type)
  (:documentation
   "@arg[type]{an instance of @class{data-type}}
    @return{type name, a string}
    @short{Return the name this type has within its library.}

    @see{type-library}
    @see{type-context-dependent-p}"))

(defmethod find-type ((library t) name &key &allow-other-keys)
  (declare (ignore name))
  nil)

(defgeneric type-context-dependent-p (type)
  (:documentation
   "@arg[type]{an instance of @class{data-type}}
    @return{a boolean}
    @short{Return true if parsing and validation of values by this type
      depends on the validation context.}

    In this case, the optional @code{context} argument to @fun{parse} and
    @fun{validp} is required, and an error will be signalled if it is missing.

    @see{validation-context}
    @see{type-name}
    @see{type-library}
    @see{type-context-dependent-p}"))

(defmethod type-context-dependent-p ((type data-type))
  nil)

(defgeneric equal-using-type (type u v)
  (:documentation
   "@arg[type]{an instance of @class{data-type}}
    @arg[u]{a parsed value as returned by @fun{parse}}
    @arg[v]{a parsed value as returned by @fun{parse}}
    @return{a boolean}
    @short{Compare the @emph{values} @code{u} and @code{v} using a
      data-type-dependent equality function.}

    @see{validp}"))

(defgeneric parse (type e &optional context)
  (:documentation
   "@arg[type]{an instance of @class{data-type}}
    @arg[e]{a string}
    @arg[context]{an instance of @class{validation-context}}
    @return{an object}
    @short{Parse string @code{e} and return a representation of its value
      as defined by the data type.}

    The @code{context} argument is required if @fun{type-context-dependent-p}
    is true for @code{type}, and will be ignored otherwise.

    @see{equal-using-type}
    @see{validp}"))

(defgeneric validp (type e &optional context)
  (:documentation
   "@arg[type]{an instance of @class{data-type}}
    @arg[e]{a string}
    @arg[context]{an instance of @class{validation-context}}
    @return{a boolean}
    @short{Determine whether a string is a valid lexical representation
    for a type.}

    The @code{context} argument is required if @fun{type-context-dependent-p}
    is true for @code{type}, and will be ignored otherwise.

    @see{parse}
    @see{equal-using-type}"))


;;; Validation context

(defclass validation-context () ()
  (:documentation
   "@short{This abstract class defines a protocol allowing data types
    to query the XML parser about its current state.}

    Some types are context dependent, as indicated by
    @fun{type-context-dependent-p}.  Those types need access to state
    computed by the XML parser implicitly, like namespace bindings or
    the Base URI.

    User-defined subclasses must implement a method
    for the @fun{context-find-namespace-binding} function.

    Two pre-defined validation context implementations are
    provided, one for use with SAX, the other based on Klacks."))

(defgeneric context-find-namespace-binding (context prefix)
  (:documentation
   "@arg[context]{an instance of @class{validation-context}}
    @arg[prefix]{name prefix, a string}
    @return{the namespace URI as a string, or NIL}
    @short{This function resolves a namespace prefix to a namespace URI in the
    current context.}
    All currently declared namespaces
    are taken into account, including those declared directly on the
    current element."))

(defclass klacks-validation-context (validation-context)
  ((source :initarg :source :accessor context-source))
  (:documentation
   "A validation-context implementation that queries
    a klacks source for information about the parser's current state.
    @see-constructor{make-klacks-validation-context}"))

(defun make-klacks-validation-context (source)
  "@arg[source]{a @a[http://common-lisp.net/project/cxml/klacks.html]{
     klacks source}}
   @return{a @class{klacks-validation-context}}
   Create a validation-context that will query the given klacks source for
   the current parser context."
  (make-instance 'klacks-validation-context :source source))

(defmethod context-find-namespace-binding
    ((context klacks-validation-context) prefix)
  (klacks:find-namespace-binding prefix (context-source context)))

(defclass sax-validation-context-mixin (validation-context)
  ((stack :initform nil :accessor context-stack))
  (:documentation
   "@short{A class that implements validation-context as a mixin for
     user-defined SAX handler classes.}

    The mixin will record namespace information
    automatically, and the user's SAX handler can simply be passed as a
    validation context to data type functions."))

(defmethod sax:start-prefix-mapping
    ((handler sax-validation-context-mixin) prefix uri)
  (push (cons prefix uri) (context-stack handler)))

(defmethod sax:end-prefix-mapping
    ((handler sax-validation-context-mixin) prefix)
  (setf (context-stack handler)
	(remove prefix
		(context-stack handler)
		:count 1
		:key #'car
		:test #'equal)))

(defmethod context-find-namespace-binding
    ((context sax-validation-context-mixin) prefix)
  (cdr (assoc prefix (context-stack context) :test #'equal)))


;;; Relax NG built-in type library

(defclass rng-type (data-type) ()
  (:documentation
   "@short{The class of Relax NG built-in types.}
    Relax NG defines two built-in data type: string and token.

    The Relax NG type library is named @code{:||}."))

(defclass string-type (rng-type) ()
  (:documentation
   "@short{The Relax NG 'string' type.}
    This data type allows arbitrary strings and interprets them as-is.

    For this type, @fun{parse} will return any string unchanged, and
    @fun{equal-using-type} compares strings using @code{equal}."))

(defclass token-type (rng-type) ()
  (:documentation
   "@short{The Relax NG 'token' type.}
    This data type allows arbitrary strings and normalizes all whitespaces.

    For this type, @fun{parse} will return the string with leading and
    trailing whitespace removed, and remaining sequences of spaces
    compressed down to one space character each.

    A method for @fun{equal-using-type} compares strings using @code{equal}."))

(defmethod type-library ((type rng-type))
  :||)

(defvar *string-data-type* (make-instance 'string-type))
(defvar *token-data-type* (make-instance 'token-type))

(defmethod find-type ((library (eql :||)) name &rest args &key)
  (cond
    ((eq name :probe) t)
    (args nil)
    ((equal name "string") *string-data-type*)
    ((equal name "token") *token-data-type*)
    (t nil)))

(defmethod equal-using-type ((type rng-type) u v)
  (equal u v))

(defmethod validp ((type rng-type) e &optional context)
  (declare (ignore e context))
  t)

(defmethod type-name ((type string-type)) "string")
(defmethod type-name ((type token-type)) "token")

(defmethod parse ((type string-type) e &optional context)
  (declare (ignore context))
  e)

(defmethod parse ((type token-type) e &optional context)
  (declare (ignore context))
  (normalize-whitespace e))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *whitespace*
    (format nil "~C~C~C~C"
	    (code-char 9)
	    (code-char 32)
	    (code-char 13)
	    (code-char 10))))

(defun normalize-whitespace (str)
  (cl-ppcre:regex-replace-all #.(format nil "[~A]+" *whitespace*)
			      (string-trim *whitespace* str)
			      " "))


;;; XML Schema Part 2: Datatypes Second Edition

(defparameter *xsd-types* (make-hash-table :test 'equal))

(defmacro defxsd
    ((class-name type-name) (&rest supers) (&rest slots) &rest args)
  `(progn
     (setf (gethash ,type-name *xsd-types*) ',class-name)
     (defclass ,class-name ,supers
	 ((type-name :initform ,type-name
		     :reader type-name
		     :allocation :class)
	  ,@slots)
       ,@args)))

(defclass xsd-type (data-type)
  ((min-length :initarg :min-length :accessor min-length)
   (max-length :initarg :max-length :accessor max-length)
   (exact-length :initarg :exact-length :accessor exact-length))
  (:documentation
   "@short{The class of XML Schema built-in types.}

    Subclasses of xsd-type provide the built-in types of
    @a[http://www.w3.org/TR/xmlschema-2/]{
      XML Schema Part 2: Datatypes Second Edition}
    as specified in @a[http://relaxng.org/xsd-20010907.html]{Guidelines for
    using W3C XML Schema Datatypes with RELAX NG}.

    The XSD type library
    is named @code{:|http://www.w3.org/2001/XMLSchema-datatypes|}."))

(defmethod initialize-instance ((instance xsd-type)
				&rest args
				&key ((:|minLength| min-length))
				     ((:|maxLength| max-length))
				     ((:|length| exact-length)))
  (apply #'call-next-method
	 instance
	 :min-length (when min-length
		       ;; fixme: richtigen fehler
		       (parse-integer min-length))
	 :max-length (when max-length
		       ;; fixme: richtigen fehler
		       (parse-integer max-length))
	 :exact-length (when exact-length
			 ;; fixme: richtigen fehler
			 (parse-integer exact-length))
	 args))

(defmethod type-library ((type xsd-type))
  :|http://www.w3.org/2001/XMLSchema-datatypes|)

(defmethod find-type
    ((library (eql :|http://www.w3.org/2001/XMLSchema-datatypes|))
     name
     &rest args &key)
  args					;fixme
  (if (eq name :probe)
      t
      (let ((class (gethash name *xsd-types*)))
	(if class
	    (apply #'make-instance class args)
	    nil))))

(defgeneric %parse (type e context))

(defmethod validp ((type xsd-type) e &optional context)
  (not (eq :error (%parse type e context))))

(defmethod parse ((type xsd-type) e &optional context)
  (let ((result (%parse type e context)))
    (when (eq result :error)
      (error "not valid for data type ~A: ~S" type e))
    result))


;;; duration

(defxsd (duration-type "duration") (xsd-type) ())

(defmethod equal-using-type ((type duration-type) u v)
  (equal u v))

(defmethod %parse ((type duration-type) e context)
  (declare (ignore context))
  (let ((strs
	 (cl-ppcre:scan-to-strings "(?x)
                                     ^(-)?            # minus
                                     P(?:(\\d+)Y)?    # years
                                     (?:(\\d+)M)?     # months
                                     (?:(\\d+)D)?     # days
                                     (T               # (time)
                                       (?:(\\d+)H)?   # hours
                                       (?:(\\d+)M)?   # minutes
                                       (?:(\\d+(?:[.]\\d+)?)S)?   # seconds
                                       )?$"
				   e)))
    (destructuring-bind (&optional minusp y m d tp h min s)
	(coerce strs 'list)
      (if (and (or y m d h min s)
	       (or (null tp) (or h min s)))
	  (let ((f (if minusp -1 1)))
	    (flet ((int (str)
		     (and str (* f (parse-integer str)))))
	      (list (int y) (int m) (int d) (int h) (int min)
		    (and s (* f (parse-number:parse-number s))))))
	  :error))))


;;; dateTime

(defxsd (date-time-type "dateTime") (xsd-type) ())

(defmethod equal-using-type ((type duration-type) u v)
  (equal u v))

;; FIXME: Was ist denn nun mit der Zeitzone?  Sollen wir die wegwerfen oder
;; hat das was mit timeOnTimeline zu tun?  Verstehe ich nicht.
(defmethod %parse ((type date-time-type) e context)
  (declare (ignore context))
  (let ((strs
	 (cl-ppcre:scan-to-strings "(?x)
                                      ^(-)?                     # opt. minus
                                      ((?:[1-9]\d*)?\d{4})      # year
                                      -(\d\d)                   # month
                                      -(\d\d)                   # day
                                      T                         # (time)
                                      (\d\d)                    # hour
                                      -(\d\d)                   # minute
                                      -(\d+(?:[.]\\d+)?)        # second
                                      (([+-])(\d\d):(\d\d)|Z)?  # opt timezone
                                      $"
				   e)))
    (destructuring-bind (&optional minusp y m d h min s tz tz-sign tz-h tz-m)
	(coerce strs 'list)
      ;; parse into numbers
      (flet ((int (str)
	       (and str (parse-integer str)))
	     (num (str)
	       (and str (parse-number:parse-number str))))
	(setf (values y m d h min s tz-h tz-m)
	      (values (* (int y) (if minusp -1 1))
		      (int m) (int d) (int h) (int min)
		      (num s)
		      (int tz-h) (int tz-m))))
      (let ((day-limit
	     (cond
	       ((and (eql m 2) (zerop (mod y 4)) (not (zerop (mod y 400)))) 29)
	       ((eql m 2) 28)
	       ((oddp y) 31)
	       (t 30))))
	;; check ranges
	(if (and y
		 (plusp z)
		 (<= 1 m 12)
		 (<= 1 d day-limit)
		 (<= 0 h 24)
		 (<= 0 m 59)
		 ;; zzz sind leap seconds immer erlaubt?
		 (<= 0 s 60))
	    (list (* y (if minusp -1 1)) m d h min s)
	    :error)))))


;;; time

(defxsd (time-type "time") (xsd-type) ())



;;; date

(defxsd (date-type "date") (xsd-type) ())



;;; gYearMonth

(defxsd (year-month-type "gYearMonth") (xsd-type) ())



;;; gYear

(defxsd (year-type "gYear") (xsd-type) ())



;;; gMonthDay

(defxsd (month-day-type "gMonthDay") (xsd-type) ())



;;; gDay

(defxsd (day-type "gDay") (xsd-type) ())



;;; gMonth

(defxsd (month-type "gMonth") (xsd-type) ())



;;; boolean

(defxsd (boolean-type "boolean") (xsd-type) ())


;;; base64Binary

(defxsd (base64-binary-type "base64Binary") (xsd-type) ())


;;; hexBinary

(defxsd (hex-binary-type "hexBinary") (xsd-type) ())


;;; float

(defxsd (float-type "float") (xsd-type) ())


;;; decimal

(defxsd (decimal-type "decimal") (xsd-type) ())


;;; double

(defxsd (double-type "double") (xsd-type) ())


;;; AnyURi

(defxsd (any-uri-type "AnyURI") (xsd-type) ())

(defmethod equal-using-type ((type any-uri-type) u v)
  (equal u v))

(defmethod %parse ((type any-uri-type) e context)
  (cxml-rng::escape-uri (normalize-whitespace e)))


;;; QName

(defxsd (qname-type "QName") (xsd-type) ())

(defstruct (qname (:constructor make-qname (uri lname)))
  uri
  lname)

(defmethod equal-using-type ((type qname-type) u v)
  (and (equal (qname-uri u) (qname-uri v))
       (equal (qname-lname u) (qname-lname v))))

(defun namep (str)
  (and (not (zerop (length str)))
       (cxml::name-start-rune-p (elt str 0))
       (every #'cxml::name-rune-p str)))

(defmethod %parse ((type qname-type) e context)
  (setf e (normalize-whitespace e))
  (handler-case
      (if (namep e)
	  (multiple-value-bind (prefix local-name) (cxml::split-qname e)
	    (let ((uri (when prefix
			 (context-find-namespace-binding context prefix))))
	      (if (and prefix (not uri))
		  :error
		  (make-qname uri local-name))))
	  :error)
    (cxml:well-formedness-violation ()
      :error)))


;;; NOTATION

(defxsd (notation-type "NOTATION") (xsd-type) ())




;;; string

(defxsd (xsd-string-type "string") (xsd-type) ())

(defmethod equal-using-type ((type xsd-string-type) u v)
  (equal u v))

(defmethod %parse ((type xsd-string-type) e context)
  (if (or (and (min-length type) (< (length e) (min-length type)))
	  (and (max-length type) (> (length e) (max-length type)))
	  (and (exact-length type) (/= (length e) (exact-length type))))
      :error
      e))


;;;;
;;;; Derived types
;;;;

;;; normalizedString

(defxsd (normalized-string-type "normalizedString") (xsd-string-type) ())



;;; token

(defxsd (xsd-token-type "token") (normalized-string-type) ())


;;; language

(defxsd (language-type "language") (xsd-token-type) ())


;;; Name

(defxsd (name-type "Name") (xsd-token-type) ())


;;; NCName

(defxsd (ncname-type "NCName") (name-type) ())

(defmethod equal-using-type ((type ncname-type) u v)
  (equal u v))

(defun nc-name-p (str)
  (and (namep str) (cxml::nc-name-p str)))

(defmethod %parse ((type ncname-type) e context)
  (setf e (normalize-whitespace e))
  (if (nc-name-p e)
      e
      :error))

;;; ID

(defxsd (id-type "ID") (ncname-type) ())


;;; IDREF

(defxsd (idref-type "IDREF") (id-type) ())


;;; IDREFS

;; fixme?
(defxsd (idrefs-type "IDREFS") (xsd-type) ())


;;; ENTITY

(defxsd (entity-type "ENTITY") (ncname-type) ())


;;; IDREFS

;; fixme?
(defxsd (entities-type "ENTITIES") (xsd-type) ())


;;; NMTOKEN

(defxsd (nmtoken-type "NMTOKEN") (xsd-token-type) ())


;;; NMTOKENS

(defxsd (nmtokens-type "NMTOKENS") (nmtoken-type) ())


;;; integer

(defxsd (integer-type "integer") (decimal-type) ())


;;; nonPositiveInteger

(defxsd (non-positive-integer-type "nonPositiveInteger") (integer-type) ())


;;; nonPositiveInteger

(defxsd (negative-integer-type "negativeInteger") (non-positive-integer-type)
  ())


;;; long

(defxsd (long-type "long") (integer-type) ())


;;; int

(defxsd (int-type "int") (long-type) ())


;;; short

(defxsd (short-type "short") (int-type) ())


;;; byte

(defxsd (bite-type "byte") (short-type) ())


;;; nonNegativeInteger

(defxsd (non-negative-integer-type "nonNegativeInteger") (integer-type) ())


;;; unsignedLong

(defxsd (unsigned-long-type "unsignedLong") (non-negative-integer-type) ())


;;; unsignedInt

(defxsd (unsigned-int-type "unsignedInt") (unsigned-long-type) ())


;;; unsignedShort

(defxsd (unsigned-short-type "unsignedShort") (unsigned-int-type) ())


;;; unsignedByte

(defxsd (unsigned-byte-type "unsignedByte") (unsigned-short-type) ())


;;; positiveInteger

(defxsd (positive-integer-type "positiveInteger") (non-negative-integer-type)
  ())
