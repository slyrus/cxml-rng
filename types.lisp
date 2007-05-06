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

(defun replace-whitespace (str)
  (cl-ppcre:regex-replace-all #.(format nil "[~A]" *whitespace*)
			      str
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
  ((pattern :initarg :pattern :reader pattern)
   (spec-pattern :initform nil :reader spec-pattern :allocation :class))
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

(defgeneric parse/xsd (type e context))

(defgeneric validp/xsd (type v context)
  (:method-combination and))

(defmethod validp/xsd and ((type xsd-type) v context)
  (declare (ignore context))
  (and (or (null (pattern type)
		 (cl-ppcre:all-matches (pattern type) v)))
       (or (null (spec-pattern type)
		 (cl-ppcre:all-matches (spec-pattern type) v)))))

(defmethod validp ((type xsd-type) e &optional context)
  (not (eq :error (parse/xsd type e context))))

(defmethod parse ((type xsd-type) e &optional context)
  (let ((result (parse/xsd type e context)))
    (when (eq result :error)
      (error "not valid for data type ~A: ~S" type e))
    result))

;; Handle the whiteSpace "facet" before the subclass sees it.
;; If parsing succeded, check other facets by asking validp/xsd.
(defmethod parse/xsd :around ((type xsd-type) e context)
  (let ((result (call-next-method type
				  (munge-whitespace type e)
				  context)))
    (if (or (eq result :error) (validp/xsd type e context))
	result
	:error)))

(defgeneric munge-whitespace (type e))

(defmethod munge-whitespace ((type xsd-type) e)
  (normalize-whitespace e))

(defmethod munge-whitespace ((type xsd-string-type) e)
  e)

(defmethod munge-whitespace ((type normalized-string-type) e)
  (replace-whitespace e))

(defmethod munge-whitespace ((type xsd-token-type) e)
  (normalize-whitespace e))


;;; ordering-mixin

(defclass ordering-mixin ()
    ((min-exclusive :initform nil
		    :initarg :min-exclusive
		    :reader min-exclusive)
     (max-exclusive :initform nil
		    :initarg :max-exclusive
		    :reader max-exclusive)
     (min-inclusive :initform nil
		    :initarg :min-inclusive
		    :reader min-inclusive)
     (max-inclusive :initform nil
		    :initarg :max-inclusive
		    :reader max-inclusive)))

(defgeneric lessp-using-type (type u v))

(defun <-using-type (type u v)
  (lessp-using-type type u v))

(defun <=-using-type (type u v)
  (or (lessp-using-type type u v) (equal-using-type type u v)))

;; it's only a partial ordering, so in general this is not the opposite of <=
(defun >-using-type (type u v)
  (lessp-using-type type v u))

;; it's only a partial ordering, so in general this is not the opposite of <
(defun >=-using-type (type u v)
  (or (lessp-using-type type v u) (equal-using-type type v u)))

(defmethod validp/xsd and ((type ordering-mixin) v context)
  (declare (ignore context))
  (with-slots (min-exclusive max-exclusive min-inclusive max-inclusive) type
    (and (or (null min-exclusive) (>-using-type type v min-exclusive))
	 (or (null max-exclusive) (<-using-type type v max-exclusive))
	 (or (null min-inclusive) (>=-using-type type v min-inclusive))
	 (or (null max-inclusive) (<=-using-type type v max-inclusive)))))


;;; length-mixin

(defclass length-mixin ()
    ((exact-length :initform nil :initarg :exact-length :reader exact-length)
     (min-length :initform nil :initarg :min-length :reader min-length)
     (max-length :initform nil :initarg :max-length :reader max-length)))

;; extra-hack fuer die "Laenge" eines QName...
(defgeneric length-using-type (type u v))
(defmethod length-using-type ((type length-mixin) e) (length e))

(defmethod validp/xsd and ((type length-mixin) v context)
  (declare (ignore context))
  (with-slots (exact-length min-length max-length) type
    (or (not (or exact-length min-length max-length))
	(let ((l (length-using-type type v)))
	  (and (or (null exact-length) (eql l exact-length))
	       (or (null min-length) (>= l min-length))
	       (or (null max-length) (<= l max-length)))))))


;;; enumeration-type

(defclass enumeration-type (xsd-type length-mixin)
    ((word-type :reader word-type)))

(defmethod initialize-instance :after ((type enumeration-type) &key)
  (setf (min-length type) (max* 1 (min-length type))))

(defmethod parse/xsd ((type enumeration-type) e context)
  (let ((wt (word-type type)))
    (loop
       for word in (cl-ppcre:split " " str)
       for v = (parse wt word context)
       collect v
       when (eq v) do (return :error))))



;;;; Primitive types

;;; duration

(defxsd (duration-type "duration") (xsd-type ordering-mixin) ())

(defmethod equal-using-type ((type duration-type) u v)
  (equal u v))

;; zzz das ist vielleicht ein bisschen zu woertlich implementiert
(defmethod lessp-using-type ((type duration-type) u v)
  (let ((dt (make-instance 'date-time-type)))
    (every (lambda (str)
	     (let ((s (%parse dt str nil)))
	       (lessp-using-type dt
				 (datetime+duration s u)
				 (datetime+duration s v))))
	   '("1696-09-01T00:00:00Z"
	     "1697-02-01T00:00:00Z"
	     "1903-03-01T00:00:00Z"
	     "1903-07-01T00:00:00Z"))))

(defun datetime+duration (s d)
  (destructuring-bind (syear smonth sday shour sminute ssecond szone) s
    (destructuring-bind (dyear dmonth dday dhour dminute dsecond) d
      (flet ((floor3 (a low high)
	       (multiple-value-bind (u v)
		   (floor (- a low) (- high low))
		 (values u (+ low v))))
	     (maximum-day-in-month-for (yearvalue monthvalue)
	       (let ((m (modulo3 monthvalue 1 13))
		     (y (+ yearvalue (floor3 monthvalue 1 13))))
		 (day-limit m y))))
	(multiple-value-bind (carry emonth) (floor3 (+ smonth dmonth) 1 13)
	  (let ((eyear (+ syear dyear carry))
		(ezone szone))
	    (multiple-value-bind (carry esecond) (floor (+ ssecond dsecond) 60)
	      (multiple-value-bind (carry eminute)
		  (floor (+ sminute dminute carry) 60)
		(multiple-value-bind (carry ehour)
		    (floor (+ shour dhour carry) 24)
		  (let* ((mdimf (maximum-day-in-month-for eyear emonth))
			 (tmpdays (max 1 (min sday mdimf)))
			 (eday (+ tmpdays dday carry)))
		    (loop
		       (let* ((mdimf (maximum-day-in-month-for eyear emonth))
			      (carry
			       (cond
				 ((< eday 1)
				  (setf eday (+ eday mdimf))
				  -1)
				 ((> eday mdimf)
				  (setf eday (- eday mdimf))
				  1)
				 (t
				  (return))))
			      (tmp (+ emonth carry)))
			 (multiple-value-bind (y m)
			     (floor3 tmp 1 13)
			   (setf emonth m)
			   (incf eyear y))))
		    (list eyear emonth day ehour eminute esecond)))))))))))

(defun scan-to-strings (&rest args)
  (coerce (apply #'cl-ppcre:scan-to-strings args) 'list))

(defmethod parse/xsd ((type duration-type) e context)
  (declare (ignore context))
  (destructuring-bind (&optional minusp y m d tp h min s)
      (scan-to-strings "(?x)
                         ^(-)?            # minus
                         P(?:(\\d+)Y)?    # years
                         (?:(\\d+)M)?     # months
                         (?:(\\d+)D)?     # days
                         (T               # (time)
                           (?:(\\d+)H)?   # hours
                           (?:(\\d+)M)?   # minutes
                           (?:(\\d+(?:[.]\\d+)?)S)?   # seconds
                           )?$"
		       e)
    (if (and (or y m d h min s)
	     (or (null tp) (or h min s)))
	(let ((f (if minusp -1 1)))
	  (flet ((int (str)
		   (and str (* f (parse-integer str)))))
	    (list (int y) (int m) (int d) (int h) (int min)
		  (and s (* f (parse-number:parse-number s))))))
	:error)))


;;; dateTime

(defclass time-ordering-mixin (ordering-mixin) ())

(defxsd (date-time-type "dateTime") (xsd-type time-ordering-mixin) ())

(defmethod equal-using-type ((type time-ordering-mixin) u v)
  (equal u v))

;; add zone-offset as a duration (if any), but keep a boolean in the
;; zone-offset field indicating whether there was a time-zone
(defun normalize-date-time (u)
  (destructuring-bind (year month day hour minute second zone-offset) u
    (let ((v (list year month day hour minute second (and zone-offset t))))
      (if zone-offset
	  (multiple-value-bind (h m)
	      (truncate zone-offset)
	    (datetime+timezone v h (* m 100)))
	  v))))

(defun datetime+timezone (u h m)
  (datetime+duration v (list 0 0 0 h m 0)))

(defmethod lessp-using-type ((type time-ordering-mixin) p q)
  (destructuring-bind (pyear pmonth pday phour pminute psecond pzonep)
      (normalize-date-time p)
    (destructuring-bind (qyear qmonth qday qhour qminute qsecond qzonep)
	(normalize-date-time q)
      (cond
	((and pzone (not qzone))
	 (lessp-using-type type p (datetime+timezone q 14)))
	((and (not pzone) qzone)
	 (lessp-using-type type (datetime+timezone p -14) q))
	(t
	 ;; zzz hier sollen wir <> liefern bei Feldern, die in genau einer
	 ;; der Zeiten fehlen.  Wir stellen aber fehlende Felder derzeit
	 ;; defaulted dar, koennen diese Situation also nicht feststellen.
	 ;; Einen Unterschied sollte das nur machen, wenn Werte verschiedener
	 ;; Datentypen miteinander verglichen werden.  Das bieten wir einfach
	 ;; nicht an.
	 (loop
	    for a in (list pyear pmonth pday phour pminute psecond)
	    for b in (list qyear qmonth qday qhour qminute qsecond)
	    do
	      (when (< a b)
		(return t))
	      (when (> a b)
		(return nil))))))))

(defun day-limit (m y)
  (cond
    ((and (eql m 2)
	  (or (zerop (mod y 400))
	      (and (zerop (mod y 4))
		   (not (zerop (mod y 100))))))
     29)
    ((eql m 2) 28)
    ((oddp y) 31)
    (t 30)))

(defmethod parse-time (minusp y m d h min s tz tz-sign tz-h tz-m
		       &key (start 0) end)
  (declare (ignore context start end))
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
  (let ((day-limit (day-limit m y)))
    ;; check ranges
    (cond
      ((and y
	    (plusp y)
	    (<= 1 m 12)
	    (<= 1 d day-limit)
	    (<= 0 h 24)
	    (<= 0 m 59)
	    ;; zzz sind leap seconds immer erlaubt?
	    (<= 0 s 60))
       ;; 24:00:00 must be canonicalized
       (when (and (eql h 24) (zerop min) (zerop s))
	 (incf h)
	 (incf d)
	 (when (> d day-limit)
	   (setf d 1)
	   (incf m)
	   (when (> m 12)
	     (incf y))))
       (let ((tz-offset
	      (when tz-h
		(* (if (equal tz-sign "-") -1 1)
		   (+ tz-h (/ hz-m 100))))))
	 (list (* y (if minusp -1 1)) m d h min s tz-offset)
	 ;; (subseq ... start end)
	 ))
      (t
       :error))))

(defmethod parse/xsd ((type date-time-type) e context)
  (declare (ignore context))
  (destructuring-bind (&optional minusp y m d h min s tz tz-sign tz-h tz-m)
      (scan-to-strings "(?x)
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
		       e)
    (parse-time minusp y m d h min s tz tz-sign tz-h tz-m)))


;;; time

(defxsd (time-type "time") (xsd-type time-ordering-mixin) ())

(defmethod parse/xsd ((type time-type) e context)
  (declare (ignore context))
  (destructuring-bind (&optional h min s tz tz-sign tz-h tz-m)
      (scan-to-strings "(?x)
                          ^(\d\d)                    # hour
                          -(\d\d)                   # minute
                          -(\d+(?:[.]\\d+)?)        # second
                          (([+-])(\d\d):(\d\d)|Z)?  # opt timezone
                          $"
		       e)
    (parse-time nil 1 1 1 h min s tz tz-sign tz-h tz-m
		:start 3)))


;;; date

(defxsd (date-type "date") (xsd-type time-ordering-mixin) ())

(defmethod parse/xsd ((type date-type) e context)
  (declare (ignore context))
  (destructuring-bind (&optional minusp y m d tz tz-sign tz-h tz-m)
      (scan-to-strings "(?x)
                          ^(-)?                     # opt. minus
                          ((?:[1-9]\d*)?\d{4})      # year
                          -(\d\d)                   # month
                          -(\d\d)                   # day
                          (([+-])(\d\d):(\d\d)|Z)?  # opt timezone
                          $"
		       e)
    (parse-time minusp y m d 0 0 0 tz tz-sign tz-h tz-m
		:end 3)))


;;; gYearMonth

(defxsd (year-month-type "gYearMonth") (xsd-type time-ordering-mixin) ())

(defmethod parse/xsd ((type year-month-type) e context)
  (declare (ignore context))
  (destructuring-bind (&optional minusp y m)
      (scan-to-strings "(?x)
                          ^(-)?                     # opt. minus
                          ((?:[1-9]\d*)?\d{4})      # year
                          -(\d\d)                   # month
                          $"
		       e)
    (parse-time minusp y m 1 0 0 0 nil nil nil nil
		:end 2)))


;;; gYear

(defxsd (year-type "gYear") (xsd-type time-ordering-mixin) ())

(defmethod parse/xsd ((type year-month-type) e context)
  (declare (ignore context))
  (destructuring-bind (&optional minusp y)
      (scan-to-strings "(?x)
                          ^(-)?                     # opt. minus
                          ((?:[1-9]\d*)?\d{4})      # year
                          $"
		       e)
    (parse-time minusp y 1 1 0 0 0 tz tz-sign tz-h tz-m
		:end 1)))


;;; gMonthDay

(defxsd (month-day-type "gMonthDay") (xsd-type time-ordering-mixin) ())

(defmethod parse/xsd ((type month-day-type) e context)
  (declare (ignore context))
  (destructuring-bind (&optional m d tz tz-sign tz-h tz-m)
      (scan-to-strings "(?x)
                          ^--(\d\d)                 # month
                          -(\d\d)                   # day
                          (([+-])(\d\d):(\d\d)|Z)?  # opt timezone
                          $"
		       e)
    (parse-time nil 1 m d nil nil nil nil nil nil nil
		:start 1 :end 3)))


;;; gDay

(defxsd (day-type "gDay") (xsd-type time-ordering-mixin) ())

(defmethod parse/xsd ((type day-type) e context)
  (declare (ignore context))
  (destructuring-bind (&optional d tz tz-sign tz-h tz-m)
      (scan-to-strings "(?x)
                          ---(\d\d)                   # day
                          (([+-])(\d\d):(\d\d)|Z)?  # opt timezone
                          $"
		       e)
    (parse-time nil 1 m d nil nil nil nil nil nil nil
		:start 3 :end 4)))


;;; gMonth

(defxsd (month-type "gMonth") (xsd-type time-ordering-mixin) ())

(defmethod parse/xsd ((type month-type) e context)
  (declare (ignore context))
  (destructuring-bind (&optional m tz tz-sign tz-h tz-m)
      (scan-to-strings "(?x)
                          ^--(\d\d)                 # month
                          (([+-])(\d\d):(\d\d)|Z)?  # opt timezone
                          $"
		       e)
    (parse-time nil 1 m d nil nil nil nil nil nil nil
		:start 2 :end 3)))


;;; boolean

(defxsd (boolean-type "boolean") (xsd-type) ())

(defmethod parse/xsd ((type boolean-type) e context)
  (declare (ignore context))
  (case (find-symbol e :keyword)
    ((:|true| :|1|) t)
    ((:|false| :|0|) nil)))


;;; base64Binary

(defxsd (base64-binary-type "base64Binary") (xsd-type length-mixin) ())

(defmethod equal-using-type ((type base64-binary-type) u v)
  (equalp u v))

(defmethod parse/xsd ((type base64-binary-type) e context)
  (declare (ignore context))
  (if (cl-ppcre:all-matches
       "(?x)
        ^(([A-Za-z0-9+/][ ]?[A-Za-z0-9+/][ ]?[A-Za-z0-9+/]
                  [ ]?[A-Za-z0-9+/][ ]?)*
           (([A-Za-z0-9+/][ ]?[A-Za-z0-9+/][ ]?[A-Za-z0-9+/][ ]?[A-Za-z0-9+/])
             | ([A-Za-z0-9+/][ ]?[A-Za-z0-9+/][ ]?[AEIMQUYcgkosw048][ ]?=)
             | ([A-Za-z0-9+/][ ]?[AQgw][ ]?=[ ]?=)))?$"
       e)
      (handler-case
	  (cl-base64:base64-string-to-usb8-array e)
	(warning (c)
	  (error "unexpected failure in Base64 decoding: ~A" c)))
      :error))


;;; hexBinary

(defxsd (hex-binary-type "hexBinary") (xsd-type length-mixin) ())

(defmethod equal-using-type ((type hex-binary-type) u v)
  (equalp u v))

(defmethod parse/xsd ((type hex-binary-type) e context)
  (declare (ignore context))
  (if (evenp (length e))
      (let ((result
	     (make-array (/ (length e) 2) :element-type '(unsigned-byte 8))))
	(loop
	   loop for i from 0 below (length e) by 2
	   do
	     (setf (elt result)
		   (handler-case
		       (parse-integer e :start i :end (+ i 2) :radix 16)
		     (error ()
		       (return :error))))
	   finally (return result)))
      :error))


;;; float

(defxsd (float-type "float") (xsd-type ordering-mixin) ())

(defmethod equal-using-type ((type float-type) u v)
  (= u v))

(defmethod lessp-using-type ((type float-type) u v)
  (< u v))

;; zzz nehme hier an, dass single-float in IEEE single float ist.
;; Das stimmt unter LispWorks bestimmt wieder nicht.
(defmethod parse/xsd ((type float-type) e context)
  (declare (ignore context))
  (if (cl-ppcre:all-matches "^[+-]\d+([.]\d+)?([eE][+-]\d+)?$" e)
      (coerce (parse-number:parse-number e) 'single-float)
      :error))


;;; decimal

(defxsd (decimal-type "decimal") (xsd-type ordering-mixin)
  ((fraction-digits :initform nil
		    :initarg :fraction-digits
		    :reader fraction-digits)
   (total-digits :initform nil
		 :initarg :total-digits
		 :reader total-digits)))

(defmethod lessp-using-type ((type decimal-type) u v)
  (< u v))

(defmethod equal-using-type ((type decimal-type) u v)
  (= u v))

(defmethod validp/xsd and ((type decimal-type) v context)
  (declare (ignore context))
  (with-slots (fraction-digits total-digits) type
    (and (or (null fraction-digits)
	     (let* ((betrag (abs v))
		    (fraction (- betrag (truncate betrag)))
		    (scaled (* fraction (expt 10 fraction-digits))))
	       (zerop (mod scaled 1))))
	 (or (null total-digits)
	     (let ((scaled (abs v)))
	       (loop until (zerop (mod scaled 1))
		    (setf scaled (* scaled 10)))
	       (< scaled (expt 10 total-digits)))))))

(defmethod parse/xsd ((type decimal-type) e context)
  (declare (ignore context))
  (destructuring-bind (&optional a b)
      (scan-to-strings "^([+-]\d+)(?:[.](\d+))?$" e)
    (if a
	(+ (parse-integer a)
	   (/ (parse-integer b) (expt 10 (length b))))
	:error)))


;;; double

(defxsd (double-type "double") (xsd-type ordering-mixin) ())

(defmethod equal-using-type ((type double-type) u v)
  (= u v))

(defmethod lessp-using-type ((type double-type) u v)
  (< u v))

;; zzz nehme hier an, dass double-float in IEEE double float ist.
;; Auch das ist nicht garantiert.
(defmethod parse/xsd ((type double-type) e context)
  (declare (ignore context))
  (if (cl-ppcre:all-matches "^[+-]\d+([.]\d+)?([eE][+-]\d+)?$" e)
      (coerce (parse-number:parse-number e) 'double-float)
      :error))


;;; AnyURi

(defxsd (any-uri-type "AnyURI") (xsd-type length-mixin) ())

(defmethod equal-using-type ((type any-uri-type) u v)
  (equal u v))

(defmethod parse/xsd ((type any-uri-type) e context)
  (cxml-rng::escape-uri e))


;;; QName
;;; NOTATION

(defclass qname-like (xsd-type length-mixin) ())

(defxsd (qname-type "QName") (qname-like) ())
(defxsd (notation-type "NOTATION") (qname-like) ())

(defstruct (qname (:constructor make-qname (uri lname length)))
  uri
  lname
  length)

(defmethod length-using-type ((type qname-like) e)
  (qname-length e))

(defmethod equal-using-type ((type qname-like) u v)
  (and (equal (qname-uri u) (qname-uri v))
       (equal (qname-lname u) (qname-lname v))))

(defun namep (str)
  (and (not (zerop (length str)))
       (cxml::name-start-rune-p (elt str 0))
       (every #'cxml::name-rune-p str)))

(defmethod parse/xsd ((type qname-like) e context)
  (handler-case
      (if (namep e)
	  (multiple-value-bind (prefix local-name) (cxml::split-qname e)
	    (let ((uri (when prefix
			 (context-find-namespace-binding context prefix))))
	      (if (and prefix (not uri))
		  :error
		  (make-qname uri local-name (length e)))))
	  :error)
    (cxml:well-formedness-violation ()
      :error)))


;;; string

(defxsd (xsd-string-type "string") (xsd-type length-mixin) ())

(defmethod equal-using-type ((type xsd-string-type) u v)
  (equal u v))

(defmethod parse/xsd ((type xsd-string-type) e context)
  e)


;;;;
;;;; Derived types
;;;;

;;; normalizedString

;;; (changes only the whiteSpace facet, defined above)

(defxsd (normalized-string-type "normalizedString") (xsd-string-type) ())


;;; token

;;; (changes only the whiteSpace facet, defined above)

(defxsd (xsd-token-type "token") (normalized-string-type) ())


;;; language

(defxsd (language-type "language") (xsd-token-type)
  ((spec-pattern :initform "[a-zA-Z]{1,8}(-[a-zA-Z0-9]{1,8})*"
		 :reader spec-pattern
		 :allocation :class)))


;;; Name

(defxsd (name-type "Name") (xsd-token-type)
  ((spec-pattern :initform "\\i\\c*"
		 :reader spec-pattern
		 :allocation :class)))


;;; NCName

(defxsd (ncname-type "NCName") (name-type)
  ((spec-pattern :initform "[\\i-[:]][\\c-[:]]*"
		 :reader spec-pattern
		 :allocation :class)))

(defmethod equal-using-type ((type ncname-type) u v)
  (equal u v))

(defun nc-name-p (str)
  (and (namep str) (cxml::nc-name-p str)))

(defmethod parse/xsd ((type ncname-type) e context)
  ;; zzz mit pattern machen
  (if (nc-name-p e)
      e
      :error))

;;; ID

(defxsd (id-type "ID") (ncname-type) ())


;;; IDREF

(defxsd (idref-type "IDREF") (id-type) ())


;;; IDREFS

(defxsd (idrefs-type "IDREFS") (enumeration-type)
  ((word-type :initform (make-instance 'idref-type))))


;;; ENTITY

(defxsd (entity-type "ENTITY") (ncname-type) ())


;;; ENTITIES

(defxsd (entities-type "ENTITIES") (enumeration-type)
  ((word-type :initform (make-instance 'entity-type))))


;;; NMTOKEN

(defxsd (nmtoken-type "NMTOKEN") (xsd-token-type)
  ((spec-pattern :initform "\\c+"
		 :reader spec-pattern
		 :allocation :class)))


;;; NMTOKENS

(defxsd (nmtokens-type "NMTOKENS") (enumeration-type)
  ((word-type :initform (make-instance 'nmtoken-type))))


;;; integer

(defxsd (integer-type "integer") (decimal-type) ())

;; period is forbidden, so there's no point in letting decimal handle parsing
;; fixme: sind fuehrende nullen nun erlaubt oder nicht?  die spec sagt ja,
;; das pattern im schema nicht.
(defmethod parse/xsd ((type integer-type) e context)
  (declare (ignore context))
  (if (cl-ppcre:all-matches "^[+-][1-9]\d*$" e)
      (parse-number:parse-number e)
      :error))


;;; nonPositiveInteger

(defxsd (non-positive-integer-type "nonPositiveInteger") (integer-type) ())

(defun min* (a b)
  (cond
    ((null a) b)
    ((null b) a)
    (t (min a b))))

(defun max* (a b)
  (cond
    ((null a) b)
    ((null b) a)
    (t (max a b))))

(defmethod initialize-instance :after ((type non-positive-integer-type) &key)
  (setf (max-inclusive type)
	(min* 0 (max-inclusive type))))


;;; nonPositiveInteger

(defxsd (negative-integer-type "negativeInteger") (non-positive-integer-type)
  ())

(defmethod initialize-instance :after ((type negative-integer-type) &key)
  (setf (max-inclusive type)
	(min* -1 (max-inclusive type))))


;;; long

(defxsd (long-type "long") (integer-type) ())

(defmethod initialize-instance :after ((type long-type) &key)
  (setf (max-inclusive type) (min* 9223372036854775807 (max-inclusive type)))
  (setf (min-inclusive type) (max* -9223372036854775808 (min-inclusive type))))


;;; int

(defxsd (int-type "int") (long-type) ())

(defmethod initialize-instance :after ((type int-type) &key)
  (setf (max-inclusive type) (min* 2147483647 (max-inclusive type)))
  (setf (min-inclusive type) (max* -2147483648 (min-inclusive type))))


;;; short

(defxsd (short-type "short") (int-type) ())

(defmethod initialize-instance :after ((type short-type) &key)
  (setf (max-inclusive type) (min* 32767 (max-inclusive type)))
  (setf (min-inclusive type) (max* -32768 (min-inclusive type))))


;;; byte

(defxsd (bite-type "byte") (short-type) ())

(defmethod initialize-instance :after ((type byte-type) &key)
  (setf (max-inclusive type) (min* 127 (max-inclusive type)))
  (setf (min-inclusive type) (max* -128 (min-inclusive type))))


;;; nonNegativeInteger

(defxsd (non-negative-integer-type "nonNegativeInteger") (integer-type) ())

(defmethod initialize-instance :after ((type non-negative-integer-type) &key)
  (setf (min-inclusive type) (max* 0 (min-inclusive type))))


;;; unsignedLong

(defxsd (unsigned-long-type "unsignedLong") (non-negative-integer-type) ())

(defmethod initialize-instance :after ((type unsigned-long-type) &key)
  (setf (max-inclusive type) (min* 18446744073709551615 (max-inclusive type))))


;;; unsignedInt

(defxsd (unsigned-int-type "unsignedInt") (unsigned-long-type) ())

(defmethod initialize-instance :after ((type unsigned-int-type) &key)
  (setf (max-inclusive type) (min* 4294967295 (max-inclusive type))))


;;; unsignedShort

(defxsd (unsigned-short-type "unsignedShort") (unsigned-int-type) ())

(defmethod initialize-instance :after ((type unsigned-short-type) &key)
  (setf (max-inclusive type) (min* 65535 (max-inclusive type))))


;;; unsignedByte

(defxsd (unsigned-byte-type "unsignedByte") (unsigned-short-type) ())

(defmethod initialize-instance :after ((type unsigned-byte-type) &key)
  (setf (max-inclusive type) (min* 255 (max-inclusive type))))


;;; positiveInteger

(defxsd (positive-integer-type "positiveInteger") (non-negative-integer-type)
  ())

(defmethod initialize-instance :after ((type postive-integer-type) &key)
  (setf (min-inclusive type) (max* 1 (min-inclusive type))))
