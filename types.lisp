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

    @see{find-type}
    @see{type-name}
    @see{type-library}
    @see{type-context-dependent-p}
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

    @see{parse}
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
    is thrue for @code{type}, and will be ignored otherwise.

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
    is thrue for @code{type}, and will be ignored otherwise.

    @see{parse}
    @see{equal-using-type}"))


;;; Validation context

(defclass validation-context () ())

(defgeneric context-find-namespace-binding (context prefix)
  (:documentation
   "@arg[context]{an instance of @class{validation-context}}
    @arg[prefix]{qname prefix, a string}
    @return{the namespace URI as a string, or NIL}"))

(defclass klacks-validation-context (validation-context)
  ((source :initarg :source :accessor context-source)))

(defun make-klacks-validation-context (source)
  (make-instance 'klacks-validation-context :source source))

(defmethod context-find-namespace-binding
    ((context klacks-validation-context) prefix)
  (klacks:find-namespace-binding prefix (context-source context)))

(defclass sax-validation-context-mixin (validation-context)
  ((stack :initform nil :accessor context-stack)))

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

(defclass rng-type (data-type) ())
(defclass string-type (rng-type) ())
(defclass token-type (rng-type) ())

(defmethod type-library ((type rng-type))
  :||)

(defvar *string-data-type* (make-instance 'string-type))
(defvar *token-data-type* (make-instance 'token-type))

(defmethod find-type ((library (eql :||)) name &rest args &key)
  (cond
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

(defclass xsd-type (data-type) ())

(defmethod type-library ((type xsd-type))
  :|http://www.w3.org/2001/XMLSchema-datatypes|)

(defmethod find-type
    ((library (eql :|http://www.w3.org/2001/XMLSchema-datatypes|))
     name
     &rest args &key)
  args					;fixme
  (case (find-symbol name :keyword)
    (:|QName| (make-instance 'qname-type))
    (:|NCName| (make-instance 'ncname-type))
    (:|anyURI| (make-instance 'any-uri-type))
    (t nil)))

(defgeneric %parse (type e context))

(defmethod validp ((type xsd-type) e &optional context)
  (not (eq :error (%parse type e context))))

(defmethod parse ((type xsd-type) e &optional context)
  (let ((result (%parse type e context)))
    (when (eq result :error)
      (error "not valid for data type ~A: ~S" type e))
    result))


;;; QName

(defclass qname-type (xsd-type) ())

(defmethod type-name ((type qname-type))
  "QName")

(defstruct (qname (:constructor make-qname (uri lname)))
  uri
  lname)

(defmethod equal-using-type ((type qname-type) u v)
  (and (equal (qname-uri u) (qname-uri v))
       (equal (qname-lname u) (qname-lname v))))

(defun nc-name-p (str)
  (and (every #'cxml::name-rune-p str)
       (cxml::nc-name-p str)))

(defmethod %parse ((type qname-type) e context)
  (handler-case
      (if (nc-name-p e)
	  (multiple-value-bind (prefix local-name) (cxml::split-qname e)
	    (let ((uri (when prefix
			 (context-find-namespace-binding context prefix))))
	      (if (and prefix (not uri))
		  :error
		  (make-qname uri local-name))))
	  :error)
    (cxml:well-formedness-violation ()
      :error)))


;;; NCName

(defclass ncname-type (xsd-type) ())

(defmethod type-name ((type ncname-type))
  "NCName")

(defmethod equal-using-type ((type ncname-type) u v)
  (equal u v))

(defmethod %parse ((type ncname-type) e context)
  (if (nc-name-p e)
      e
      :error))


;;; AnyURi

(defclass any-uri-type (xsd-type) ())

(defmethod type-name ((type any-uri-type))
  "AnyURI")

(defmethod equal-using-type ((type any-uri-type) u v)
  (equal u v))

(defmethod %parse ((type any-uri-type) e context)
  (cxml-rng::escape-uri e))
