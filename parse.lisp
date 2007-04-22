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

(in-package :cxml-rng)

#+sbcl
(declaim (optimize (debug 2)))


;;;; Errors

(define-condition rng-error (simple-error) ())

(defun rng-error (source fmt &rest args)
  (let ((s (make-string-output-stream)))
    (apply #'format s fmt args)
    (when source
      (etypecase source
	(klacks:source
	 (format s "~&  [ Error at line ~D, column ~D in ~S ]"
		 (klacks:current-line-number source)
		 (klacks:current-column-number source)
		 (klacks:current-system-id source)))
	(sax:sax-parser-mixin
	 (format s "~&  [ Error at line ~D, column ~D in ~S ]"
		 (sax:line-number source)
		 (sax:column-number source)
		 (sax:system-id source))) ))
    (error 'rng-error
	   :format-control "~A"
	   :format-arguments (list (get-output-stream-string s)))))


;;;; Parser

(defvar *datatype-library*)
(defvar *namespace-uri*)
(defvar *ns*)
(defvar *entity-resolver*)
(defvar *external-href-stack*)
(defvar *include-uri-stack*)
(defvar *include-body-p* nil)
(defvar *grammar*)

(defvar *debug* nil)

(defstruct (parsed-grammar
	     (:constructor make-parsed-grammar (pattern definitions)))
  (pattern (missing) :type pattern)
  (definitions (missing) :type list)
  (interned-start nil :type (or null pattern))
  (registratur nil :type (or null hash-table)))

(defmethod print-object ((object parsed-grammar) stream)
  (print-unreadable-object (object stream :type t :identity t)))

(defun invoke-with-klacks-handler (fn source)
  (if *debug*
      (funcall fn)
      (handler-case
	  (funcall fn)
	(cxml:xml-parse-error (c)
	  (rng-error source "Cannot parse schema: ~A" c)))))

(defvar *validate-grammar* t)
(defparameter *relax-ng-grammar* nil)

(defun make-validating-source (input)
  (let ((upstream (cxml:make-source input)))
    (if *validate-grammar*
	(klacks:make-tapping-source upstream
				    (make-validator *relax-ng-grammar*))
	upstream)))

(defun parse-relax-ng (input &key entity-resolver)
  (when *validate-grammar*
    (unless *relax-ng-grammar*
      (setf *relax-ng-grammar*
	    (let* ((*validate-grammar* nil)
		   (d (slot-value (asdf:find-system :cxml-rng)
				  'asdf::relative-pathname)))
	      (parse-relax-ng (merge-pathnames "rng.rng" d))))))
  (klacks:with-open-source (source (make-validating-source input))
    (invoke-with-klacks-handler
     (lambda ()
       (klacks:find-event source :start-element)
       (let* ((*datatype-library* "")
	      (*namespace-uri* "")
	      (*entity-resolver* entity-resolver)
	      (*external-href-stack* '())
	      (*include-uri-stack* '())
	      (*grammar* (make-grammar nil))
	      (result (p/pattern source)))
	 (unless result
	   (rng-error nil "empty grammar"))
	 (setf (grammar-start *grammar*)
	       (make-definition :name :start :child result))
	 (check-pattern-definitions source *grammar*)
	 (check-recursion result 0)
	 (let ((defns (finalize-definitions result)))
	   (setf result (fold-not-allowed result))
	   (dolist (defn defns)
	     (setf (defn-child defn) (fold-not-allowed (defn-child defn))))
	   (setf result (fold-empty result))
	   (dolist (defn defns)
	     (setf (defn-child defn) (fold-empty (defn-child defn)))))
	 (let ((defns (finalize-definitions result)))
	   (check-start-restrictions result)
	   (dolist (defn defns)
	     (check-restrictions (defn-child defn)))
	   (make-parsed-grammar result defns))))
     source)))


;;;; pattern structures

(defstruct pattern)

(defmethod print-object :around ((object pattern) stream)
  (if *debug*
      (let ((*print-circle* t))
	(call-next-method))
      (print-unreadable-object (object stream :type t :identity t))))

(defstruct (%parent (:include pattern) (:conc-name "PATTERN-"))
  child)

(defstruct (%named-pattern (:include %parent) (:conc-name "PATTERN-"))
  name)
(defstruct (element (:include %named-pattern) (:conc-name "PATTERN-")))
(defstruct (attribute (:include %named-pattern) (:conc-name "PATTERN-")))

(defstruct (%combination (:include pattern) (:conc-name "PATTERN-"))
  a b)
(defstruct (group
	    (:include %combination)
	    (:constructor make-group (a b))))
(defstruct (interleave
	    (:include %combination)
	    (:constructor make-interleave (a b))))
(defstruct (choice
	    (:include %combination)
	    (:constructor make-choice (a b))))
(defstruct (after
	    (:include %combination)
	    (:constructor make-after (a b))))

(defstruct (one-or-more
	    (:include %parent)
	    (:constructor make-one-or-more (child))))
(defstruct (list-pattern
	    (:include %parent)
	    (:constructor make-list-pattern (child))))

(defstruct (ref
	    (:include pattern)
	    (:conc-name "PATTERN-")
	    (:constructor make-ref (target)))
  crdepth
  target)

(defstruct (%leaf (:include pattern)))

(defstruct (empty (:include %leaf) (:conc-name "PATTERN-")))
(defstruct (text (:include %leaf) (:conc-name "PATTERN-")))

(defstruct (%typed-pattern (:include %leaf) (:conc-name "PATTERN-"))
  type)

(defstruct (value (:include %typed-pattern) (:conc-name "PATTERN-"))
  ns
  string
  value)

(defstruct (data (:include %typed-pattern) (:conc-name "PATTERN-"))
  params
  except)

(defstruct (not-allowed (:include %leaf) (:conc-name "PATTERN-")))


;;;; non-pattern

(defstruct (grammar (:constructor make-grammar (parent)))
  (start nil)
  parent
  (definitions (make-hash-table :test 'equal)))

(defstruct param
  name
  string)

;; Clark calls this structure "RefPattern"
(defstruct (definition (:conc-name "DEFN-"))
  name
  combine-method
  head-p
  redefinition
  child)


;;; name-class

(defun missing ()
  (error "missing arg"))

(defstruct name-class)

(defstruct (any-name (:include name-class)
		     (:constructor make-any-name (except)))
  (except (missing) :type (or null name-class)))

(defstruct (name (:include name-class)
		 (:constructor make-name (uri lname)))
  (uri (missing) :type string)
  (lname (missing) :type string))

(defstruct (ns-name (:include name-class)
		    (:constructor make-ns-name (uri except)))
  (uri (missing) :type string)
  (except (missing) :type (or null name-class)))

(defstruct (name-class-choice (:include name-class)
			      (:constructor make-name-class-choice (a b)))
  (a (missing) :type name-class)
  (b (missing) :type name-class))

(defun simplify-nc-choice (values)
  (zip #'make-name-class-choice values))


;;;; parser

(defvar *rng-namespace* "http://relaxng.org/ns/structure/1.0")

(defun skip-foreign* (source)
  (loop
    (case (klacks:peek-next source)
      (:start-element (skip-foreign source))
      (:end-element (return)))))

(defun skip-to-native (source)
  (loop
    (case (klacks:peek source)
      (:start-element
	(when (equal (klacks:current-uri source) *rng-namespace*)
	  (return))
	(klacks:serialize-element source nil))
      (:end-element (return)))
    (klacks:consume source)))

(defun consume-and-skip-to-native (source)
  (klacks:consume source)
  (skip-to-native source))

(defun skip-foreign (source)
  (when (equal (klacks:current-uri source) *rng-namespace*)
    (rng-error source
	       "invalid schema: ~A not allowed here"
	       (klacks:current-lname source)))
  (klacks:serialize-element source nil))

(defun attribute (lname attrs)
  (let ((a (sax:find-attribute-ns "" lname attrs)))
    (if a
	(sax:attribute-value a)
	nil)))

(defparameter *whitespace*
    (format nil "~C~C~C~C"
	    (code-char 9)
	    (code-char 32)
	    (code-char 13)
	    (code-char 10)))

(defun ntc (lname source-or-attrs)
  ;; used for (n)ame, (t)ype, and (c)ombine, this also strings whitespace
  (let* ((attrs
	  (if (listp source-or-attrs)
	      source-or-attrs
	      (klacks:list-attributes source-or-attrs)))
	 (a (sax:find-attribute-ns "" lname attrs)))
    (if a
	(string-trim *whitespace* (sax:attribute-value a))
	nil)))

(defmacro with-library-and-ns (attrs &body body)
  `(invoke-with-library-and-ns (lambda () ,@body) ,attrs))

(defun invoke-with-library-and-ns (fn attrs)
  (let* ((dl (attribute "datatypeLibrary" attrs))
	 (ns (attribute "ns" attrs))
	 (*datatype-library* (if dl (escape-uri dl) *datatype-library*))
	 (*namespace-uri* (or ns *namespace-uri*))
	 (*ns* ns))
    (funcall fn)))

(defun p/pattern (source)
  (let* ((lname (klacks:current-lname source))
	 (attrs (klacks:list-attributes source)))
    (with-library-and-ns attrs
      (case (find-symbol lname :keyword)
	(:|element|     (p/element source (ntc "name" attrs)))
	(:|attribute|   (p/attribute source (ntc "name" attrs)))
	(:|group|       (p/combination #'groupify source))
	(:|interleave|  (p/combination #'interleave-ify source))
	(:|choice|      (p/combination #'choice-ify source))
	(:|optional|    (p/optional source))
	(:|zeroOrMore|  (p/zero-or-more source))
	(:|oneOrMore|   (p/one-or-more source))
	(:|list|        (p/list source))
	(:|mixed|       (p/mixed source))
	(:|ref|         (p/ref source))
	(:|parentRef|   (p/parent-ref source))
	(:|empty|       (p/empty source))
	(:|text|        (p/text source))
	(:|value|       (p/value source))
	(:|data|        (p/data source))
	(:|notAllowed|  (p/not-allowed source))
	(:|externalRef| (p/external-ref source))
	(:|grammar|     (p/grammar source))
	(t (skip-foreign source))))))

(defun p/pattern+ (source)
  (let ((children nil))
    (loop
      (case (klacks:peek source)
	(:start-element
	  (let ((p (p/pattern source))) (when p (push p children))))
	(:end-element
	  (return))
	(t
	  (klacks:consume source))))
    (unless children
      (rng-error source "empty element"))
    (nreverse children)))

(defun p/pattern? (source)
  (let ((result nil))
    (loop
      (skip-to-native source)
      (case (klacks:peek source)
	(:start-element
	  (when result
	    (rng-error source "at most one pattern expected here"))
	  (setf result (p/pattern source)))
	(:end-element
	  (return))
	(t
	  (klacks:consume source))))
    result))

(defun p/element (source name)
  (klacks:expecting-element (source "element")
    (let ((elt (make-element)))
      (consume-and-skip-to-native source)
      (if name
	  (setf (pattern-name elt) (destructure-name source name))
	  (setf (pattern-name elt) (p/name-class source)))
      (skip-to-native source)
      (setf (pattern-child elt) (groupify (p/pattern+ source)))
      (make-ref (make-definition :name (gensym "ANONYMOUS") :child elt)))))

(defvar *attribute-namespace-p* nil)

(defun p/attribute (source name)
  (klacks:expecting-element (source "attribute")
    (let ((result (make-attribute)))
      (consume-and-skip-to-native source)
      (if name
	  (setf (pattern-name result)
		(let ((*namespace-uri* (or *ns* "")))
		  (destructure-name source name)))
	  (setf (pattern-name result)
		(let ((*attribute-namespace-p* t))
		  (p/name-class source))))
      (skip-to-native source)
      (setf (pattern-child result)
	    (or (p/pattern? source) (make-text)))
      result)))

(defun p/combination (zipper source)
  (klacks:expecting-element (source)
    (consume-and-skip-to-native source)
    (funcall zipper (p/pattern+ source))))

(defun p/one-or-more (source)
  (klacks:expecting-element (source "oneOrMore")
    (consume-and-skip-to-native source)
    (let ((children (p/pattern+ source)))
      (make-one-or-more (groupify children)))))

(defun p/zero-or-more (source)
  (klacks:expecting-element (source "zeroOrMore")
    (consume-and-skip-to-native source)
    (let ((children (p/pattern+ source)))
      (make-choice (make-one-or-more (groupify children))
		   (make-empty)))))

(defun p/optional (source)
  (klacks:expecting-element (source "optional")
    (consume-and-skip-to-native source)
    (let ((children (p/pattern+ source)))
      (make-choice (groupify children) (make-empty)))))

(defun p/list (source)
  (klacks:expecting-element (source "list")
    (consume-and-skip-to-native source)
    (let ((children (p/pattern+ source)))
      (make-list-pattern (groupify children)))))

(defun p/mixed (source)
  (klacks:expecting-element (source "mixed")
    (consume-and-skip-to-native source)
    (let ((children (p/pattern+ source)))
      (make-interleave (groupify children) (make-text)))))

(defun p/ref (source)
  (klacks:expecting-element (source "ref")
    (prog1
	(let* ((name (ntc "name" source))
	       (pdefinition
		(or (find-definition name)
		    (setf (find-definition name)
			  (make-definition :name name :child nil)))))
	  (make-ref pdefinition))
      (skip-foreign* source))))

(defun p/parent-ref (source)
  (klacks:expecting-element (source "parentRef")
    (prog1
	(let* ((name (ntc "name" source))
	       (grammar (grammar-parent *grammar*))
	       (pdefinition
		(or (find-definition name grammar)
		    (setf (find-definition name grammar)
			  (make-definition :name name :child nil)))))
	  (make-ref pdefinition))
      (skip-foreign* source))))

(defun p/empty (source)
  (klacks:expecting-element (source "empty")
    (skip-foreign* source)
    (make-empty)))

(defun p/text (source)
  (klacks:expecting-element (source "text")
    (skip-foreign* source)
    (make-text)))

(defun consume-and-parse-characters (source)
  ;; fixme
  (let ((tmp ""))
    (loop
      (multiple-value-bind (key data) (klacks:peek-next source)
	(case key
	  (:characters
	    (setf tmp (concatenate 'string tmp data)))
	  (:end-element (return)))))
    tmp))

(defun p/value (source)
  (klacks:expecting-element (source "value")
    (let* ((type (ntc "type" source))
	   (string (consume-and-parse-characters source))
	   (ns *namespace-uri*)
	   (dl *datatype-library*))
      (unless type
	(setf type "token")
	(setf dl ""))
      (let ((data-type
	     (cxml-types:find-type (and dl (find-symbol dl :keyword)) type))
	    (vc (cxml-types:make-klacks-validation-context source)))
	(unless data-type
	  (rng-error source "type not found: ~A/~A" type dl))
	(make-value :string string
		    :value (cxml-types:parse data-type string vc)
		    :type data-type
		    :ns ns)))))

(defun p/data (source)
  (klacks:expecting-element (source "data")
    (let* ((type (ntc "type" source))
	   (params '())
	   (except nil))
      (loop
	 (multiple-value-bind (key uri lname)
	     (klacks:peek-next source)
	   uri
	   (case key
	     (:start-element
	      (case (find-symbol lname :keyword)
		(:|param| (push (p/param source) params))
		(:|except|
		  (setf except (p/except-pattern source))
		  (skip-to-native source)
		  (return))
		(t (skip-foreign source))))
	     (:end-element
	      (return)))))
      (setf params (nreverse params))
      (let* ((dl *datatype-library*)
	     (data-type (apply #'cxml-types:find-type
			       (and dl (find-symbol dl :keyword))
			       type
			       (loop
				  for p in params
				  collect (find-symbol (param-name p)
						       :keyword)
				  collect (param-string p)))))
	(unless data-type
	  (rng-error source "type not found: ~A/~A" type dl))
	(make-data
	 :type data-type
	 :params params
	 :except except)))))

(defun p/param (source)
  (klacks:expecting-element (source "param")
    (let ((name (ntc "name" source))
	  (string (consume-and-parse-characters source)))
      (make-param :name name :string string))))

(defun p/except-pattern (source)
  (klacks:expecting-element (source "except")
    (with-library-and-ns (klacks:list-attributes source)
      (klacks:consume source)
      (choice-ify (p/pattern+ source)))))

(defun p/not-allowed (source)
  (klacks:expecting-element (source "notAllowed")
    (consume-and-skip-to-native source)
    (make-not-allowed)))

(defun safe-parse-uri (source str &optional base)
  (when (zerop (length str))
    (rng-error source "missing URI"))
  (handler-case
      (if base
	  (puri:merge-uris str base)
	  (puri:parse-uri str))
    (puri:uri-parse-error ()
      (rng-error source "invalid URI: ~A" str))))

(defun p/external-ref (source)
  (klacks:expecting-element (source "externalRef")
    (let* ((href
	    (escape-uri (attribute "href" (klacks:list-attributes source))))
	   (base (klacks:current-xml-base source))
	   (uri (safe-parse-uri source href base)))
      (when (find uri *include-uri-stack* :test #'puri:uri=)
	(rng-error source "looping include"))
      (prog1
	  (let* ((*include-uri-stack* (cons uri *include-uri-stack*))
		 (xstream
		  (cxml::xstream-open-extid* *entity-resolver* nil uri)))
	    (klacks:with-open-source (source (make-validating-source xstream))
	      (invoke-with-klacks-handler
	       (lambda ()
		 (klacks:find-event source :start-element)
		 (let ((*datatype-library* ""))
		   (p/pattern source)))
	       source)))
	(skip-foreign* source)))))

(defun p/grammar (source &optional grammar)
  (klacks:expecting-element (source "grammar")
    (consume-and-skip-to-native source)
    (let ((*grammar* (or grammar (make-grammar *grammar*)))
	  (includep grammar))
      (process-grammar-content* source)
      (unless (or includep (grammar-start *grammar*))
	(rng-error source "no <start> in grammar"))
      (unless includep
	(check-pattern-definitions source *grammar*)
	(defn-child (grammar-start *grammar*))))))

(defvar *include-start*)
(defvar *include-definitions*)

(defun process-grammar-content* (source &key disallow-include)
  (loop
    (multiple-value-bind (key uri lname) (klacks:peek source)
      uri
      (case key
	(:start-element
	  (with-library-and-ns (klacks:list-attributes source)
	    (case (find-symbol lname :keyword)
	      (:|start| (process-start source))
	      (:|define| (process-define source))
	      (:|div| (process-div source))
	      (:|include|
		(when disallow-include
		  (rng-error source "nested include not permitted"))
		(process-include source))
	      (t
		(skip-foreign source)))))
	(:end-element
	  (return))))
    (klacks:consume source)))

(defun process-start (source)
  (klacks:expecting-element (source "start")
    (let* ((combine0 (ntc "combine" source))
	   (combine
	    (when combine0
	      (find-symbol (string-upcase combine0) :keyword)))
	   (child
	    (progn
	      (consume-and-skip-to-native source)
	      (p/pattern source)))
	   (pdefinition (grammar-start *grammar*)))
      (skip-foreign* source)
      ;; fixme: shared code with process-define
      (unless pdefinition
	(setf pdefinition (make-definition :name :start :child nil))
	(setf (grammar-start *grammar*) pdefinition))
      (when *include-body-p*
	(setf *include-start* pdefinition))
      (cond
	((defn-child pdefinition)
	 (ecase (defn-redefinition pdefinition)
	   (:not-being-redefined
	     (when (and combine
			(defn-combine-method pdefinition)
			(not (eq combine
				 (defn-combine-method pdefinition))))
	       (rng-error source "conflicting combine values for <start>"))
	     (unless combine
	       (when (defn-head-p pdefinition)
		 (rng-error source "multiple definitions for <start>"))
	       (setf (defn-head-p pdefinition) t))
	     (unless (defn-combine-method pdefinition)
	       (setf (defn-combine-method pdefinition) combine))
	     (setf (defn-child pdefinition)
		   (case (defn-combine-method pdefinition)
		     (:choice
		       (make-choice (defn-child pdefinition) child))
		     (:interleave
		       (make-interleave (defn-child pdefinition) child)))))
	   (:being-redefined-and-no-original
	     (setf (defn-redefinition pdefinition)
		   :being-redefined-and-original))
	   (:being-redefined-and-original)))
	(t
	  (setf (defn-child pdefinition) child)
	  (setf (defn-combine-method pdefinition) combine)
	  (setf (defn-head-p pdefinition) (null combine))
	  (setf (defn-redefinition pdefinition) :not-being-redefined))))))

(defun zip (constructor children)
  (cond
    ((null children)
      (rng-error nil "empty choice?"))
    ((null (cdr children))
      (car children))
    (t
      (destructuring-bind (a b &rest rest)
	  children
	(zip constructor (cons (funcall constructor a b) rest))))))

(defun choice-ify (children) (zip #'make-choice children))
(defun groupify (children) (zip #'make-group children))
(defun interleave-ify (children) (zip #'make-interleave children))

(defun find-definition (name &optional (grammar *grammar*))
  (gethash name (grammar-definitions grammar)))

(defun (setf find-definition) (newval name &optional (grammar *grammar*))
  (setf (gethash name (grammar-definitions grammar)) newval))

(defun process-define (source)
  (klacks:expecting-element (source "define")
    (let* ((name (ntc "name" source))
	   (combine0 (ntc "combine" source))
	   (combine (when combine0
		      (find-symbol (string-upcase combine0) :keyword)))
	   (child (groupify
		   (progn
		     (consume-and-skip-to-native source)
		     (p/pattern+ source))))
	   (pdefinition (find-definition name)))
      (unless pdefinition
	(setf pdefinition (make-definition :name name :child nil))
	(setf (find-definition name) pdefinition))
      (when *include-body-p*
	(push pdefinition *include-definitions*))
      (cond
	((defn-child pdefinition)
	  (case (defn-redefinition pdefinition)
	    (:not-being-redefined
	      (when (and combine
			 (defn-combine-method pdefinition)
			 (not (eq combine
				  (defn-combine-method pdefinition))))
		(rng-error source "conflicting combine values for ~A" name))
	      (unless combine
		(when (defn-head-p pdefinition)
		  (rng-error source "multiple definitions for ~A" name))
		(setf (defn-head-p pdefinition) t))
	      (unless (defn-combine-method pdefinition)
		(setf (defn-combine-method pdefinition) combine))
	      (setf (defn-child pdefinition)
		    (case (defn-combine-method pdefinition)
		      (:choice
			(make-choice (defn-child pdefinition) child))
		      (:interleave
			(make-interleave (defn-child pdefinition) child)))))
	    (:being-redefined-and-no-original
	      (setf (defn-redefinition pdefinition)
		    :being-redefined-and-original))
	    (:being-redefined-and-original)))
	(t
	  (setf (defn-child pdefinition) child)
	  (setf (defn-combine-method pdefinition) combine)
	  (setf (defn-head-p pdefinition) (null combine))
	  (setf (defn-redefinition pdefinition) :not-being-redefined))))))

(defun process-div (source)
  (klacks:expecting-element (source "div")
    (consume-and-skip-to-native source)
    (process-grammar-content* source)))

(defun reset-definition-for-include (defn)
  (setf (defn-combine-method defn) nil)
  (setf (defn-redefinition defn) :being-redefined-and-no-original)
  (setf (defn-head-p defn) nil))

(defun restore-definition (defn original)
  (setf (defn-combine-method defn) (defn-combine-method original))
  (setf (defn-redefinition defn) (defn-redefinition original))
  (setf (defn-head-p defn) (defn-head-p original)))

(defun process-include (source)
  (klacks:expecting-element (source "include")
    (let* ((href
	    (escape-uri (attribute "href" (klacks:list-attributes source))))
	   (base (klacks:current-xml-base source))
	   (uri (safe-parse-uri source href base))
	   (*include-start* nil)
	   (*include-definitions* '()))
      (consume-and-skip-to-native source)
      (let ((*include-body-p* t))
	(process-grammar-content* source :disallow-include t))
      (let ((tmp-start
	     (when *include-start*
	       (prog1
		   (copy-structure *include-start*)
		 (reset-definition-for-include *include-start*))))
	    (tmp-defns
	     (loop
		 for defn in *include-definitions*
		 collect
		   (prog1
		       (copy-structure defn)
		     (reset-definition-for-include defn)))))
	(when (find uri *include-uri-stack* :test #'puri:uri=)
	  (rng-error source "looping include"))
	(let* ((*include-uri-stack* (cons uri *include-uri-stack*))
	       (xstream (cxml::xstream-open-extid* *entity-resolver* nil uri)))
	  (klacks:with-open-source (source (make-validating-source xstream))
	    (invoke-with-klacks-handler
	     (lambda ()
	       (klacks:find-event source :start-element)
	       (let ((*datatype-library* ""))
		 (p/grammar source *grammar*)))
	     source))
	  (when tmp-start
	    (when (eq (defn-redefinition *include-start*)
		      :being-redefined-and-no-original)
	      (rng-error source "start not found in redefinition of grammar"))
	    (restore-definition *include-start* tmp-start))
	  (dolist (copy tmp-defns)
	    (let ((defn (gethash (defn-name copy)
				 (grammar-definitions *grammar*))))
	      (when (eq (defn-redefinition defn)
			:being-redefined-and-no-original)
		(rng-error source "redefinition not found in grammar"))
	      (restore-definition defn copy)))
	  nil)))))

(defun check-pattern-definitions (source grammar)
  (when (and (grammar-start grammar)
	     (eq (defn-redefinition (grammar-start grammar))
		 :being-redefined-and-no-original))
    (rng-error source "start not found in redefinition of grammar"))
  (loop for defn being each hash-value in (grammar-definitions grammar) do
	(when (eq (defn-redefinition defn) :being-redefined-and-no-original)
	  (rng-error source "redefinition not found in grammar"))
	(unless (defn-child defn)
	  (rng-error source "unresolved reference to ~A" (defn-name defn)))))

(defvar *any-name-allowed-p* t)
(defvar *ns-name-allowed-p* t)

(defun destructure-name (source qname)
  (multiple-value-bind (uri lname)
      (klacks:decode-qname qname source)
    (setf uri (or uri *namespace-uri*))
    (when (and *attribute-namespace-p*
	       (or (and (equal lname "xmlns") (equal uri ""))
		   (equal uri "http://www.w3.org/2000/xmlns")))
      (rng-error source "namespace attribute not permitted"))
    (make-name uri lname)))

(defun p/name-class (source)
  (klacks:expecting-element (source)
    (with-library-and-ns (klacks:list-attributes source)
      (case (find-symbol (klacks:current-lname source) :keyword)
	(:|name|
	  (let ((qname (string-trim *whitespace*
				    (consume-and-parse-characters source))))
	    (destructure-name source qname)))
	(:|anyName|
	  (unless *any-name-allowed-p*
	    (rng-error source "anyname now permitted in except"))
	  (klacks:consume source)
	  (prog1
	      (let ((*any-name-allowed-p* nil))
		(make-any-name (p/except-name-class? source)))
	    (skip-to-native source)))
	(:|nsName|
	  (unless *ns-name-allowed-p*
	    (rng-error source "nsname now permitted in except"))
	  (let ((uri *namespace-uri*)
		(*any-name-allowed-p* nil)
		(*ns-name-allowed-p* nil))
	    (when (and *attribute-namespace-p*
		       (equal uri "http://www.w3.org/2000/xmlns"))
	      (rng-error source "namespace attribute not permitted"))
	    (klacks:consume source)
	    (prog1
		(make-ns-name uri (p/except-name-class? source))
	      (skip-to-native source))))
	(:|choice|
	  (klacks:consume source)
	  (simplify-nc-choice (p/name-class* source)))
	(t
	  (rng-error source "invalid child in except"))))))

(defun p/name-class* (source)
  (let ((results nil))
    (loop
      (skip-to-native source)
      (case (klacks:peek source)
	(:start-element (push (p/name-class source) results))
	(:end-element (return)))
      (klacks:consume source))
    (nreverse results)))

(defun p/except-name-class? (source)
  (skip-to-native source)
  (multiple-value-bind (key uri lname)
      (klacks:peek source)
    uri
    (if (and (eq key :start-element)
	     (string= (find-symbol lname :keyword) "except"))
	(p/except-name-class source)
	nil)))

(defun p/except-name-class (source)
  (klacks:expecting-element (source "except")
    (with-library-and-ns (klacks:list-attributes source)
      (klacks:consume source)
      (let ((x (p/name-class* source)))
	(if (cdr x)
	    (simplify-nc-choice x)
	    (car x))))))

(defun escape-uri (string)
  (with-output-to-string (out)
    (loop for c across (cxml::rod-to-utf8-string string) do
	  (let ((code (char-code c)))
	    ;; http://www.w3.org/TR/xlink/#link-locators
	    (if (or (>= code 127) (<= code 32) (find c "<>\"{}|\\^`"))
		(format out "%~2,'0X" code)
		(write-char c out))))))


;;;; unparsing

(defvar *definitions-to-names*)
(defvar *seen-names*)

(defun serialization-name (defn)
  (or (gethash defn *definitions-to-names*)
      (setf (gethash defn *definitions-to-names*)
	    (let ((name (if (gethash (defn-name defn) *seen-names*)
			    (format nil "~A-~D"
				    (defn-name defn)
				    (hash-table-count *seen-names*))
			    (defn-name defn))))
	      (setf (gethash name *seen-names*) defn)
	      name))))

(defun serialize-grammar (grammar sink)
  (cxml:with-xml-output sink
    (let ((*definitions-to-names* (make-hash-table))
	  (*seen-names* (make-hash-table :test 'equal)))
      (cxml:with-element "grammar"
	(cxml:with-element "start"
	  (serialize-pattern (parsed-grammar-pattern grammar)))
	(loop for defn being each hash-key in *definitions-to-names* do
	      (serialize-definition defn))))))

(defun serialize-pattern (pattern)
  (etypecase pattern
    (element
      (cxml:with-element "element"
	(serialize-name (pattern-name pattern))
	(serialize-pattern (pattern-child pattern))))
    (attribute
      (cxml:with-element "attribute"
	(serialize-name (pattern-name pattern))
	(serialize-pattern (pattern-child pattern))))
    (%combination
      (cxml:with-element
	  (etypecase pattern
	    (group "group")
	    (interleave "interleave")
	    (choice "choice"))
	(serialize-pattern (pattern-a pattern))
	(serialize-pattern (pattern-b pattern))))
    (one-or-more
      (cxml:with-element "oneOrMore"
	(serialize-pattern (pattern-child pattern))))
    (list-pattern
      (cxml:with-element "list"
	(serialize-pattern (pattern-child pattern))))
    (ref
      (cxml:with-element "ref"
	(cxml:attribute "name" (serialization-name (pattern-target pattern)))))
    (empty
      (cxml:with-element "empty"))
    (not-allowed
      (cxml:with-element "notAllowed"))
    (text
      (cxml:with-element "text"))
    (value
      (cxml:with-element "value"
	(let ((type (pattern-type pattern)))
	  (cxml:attribute "datatype-library"
			  (symbol-name (cxml-types:type-library type)))
	  (cxml:attribute "type" (cxml-types:type-name type)))
	(cxml:attribute "ns" (pattern-ns pattern))
	(cxml:text (pattern-string pattern))))
    (data
      (cxml:with-element "value"
	(let ((type (pattern-type pattern)))
	  (cxml:attribute "datatype-library"
			  (symbol-name (cxml-types:type-library type)))
	  (cxml:attribute "type" (cxml-types:type-name type)))
	(dolist (param (pattern-params pattern))
	  (cxml:with-element "param"
	    (cxml:attribute "name" (param-name param))
	    (cxml:text (param-string param))))
	(when (pattern-except pattern)
	  (cxml:with-element "except"
	    (serialize-pattern (pattern-except pattern))))))))

(defun serialize-definition (defn)
  (cxml:with-element "define"
    (cxml:attribute "name" (serialization-name defn))
    (serialize-pattern (defn-child defn))))

(defun serialize-name (name)
  (etypecase name
    (name
     (cxml:with-element "name"
       (cxml:attribute "ns" (name-uri name))
       (cxml:text (name-lname name))))
    (any-name
     (cxml:with-element "anyName"
       (when (any-name-except name)
	 (serialize-except-name (any-name-except name)))))
    (ns-name
     (cxml:with-element "anyName"
       (cxml:attribute "ns" (ns-name-uri name))
       (when (ns-name-except name)
	 (serialize-except-name (ns-name-except name)))))
    (name-class-choice
     (cxml:with-element "choice"
       (serialize-name (name-class-choice-a name))
       (serialize-name (name-class-choice-b name))))))

(defun serialize-except-name (spec)
  (cxml:with-element "except"
    (serialize-name spec)))


;;;; simplification

;;; 4.1 Annotations
;;;   Foreign attributes and elements are removed implicitly while parsing.

;;; 4.2 Whitespace
;;;   All character data is discarded while parsing (which can only be
;;;   whitespace after validation).
;;;
;;;   Whitespace in name, type, and combine attributes is stripped while
;;;   parsing.  Ditto for <name/>.

;;; 4.3. datatypeLibrary attribute
;;;   Escaping is done by p/pattern.
;;;   Attribute value defaulting is done using *datatype-library*; only
;;;   p/data and p/value record the computed value.

;;; 4.4. type attribute of value element
;;;   Done by p/value.

;;; 4.5. href attribute
;;;   Escaping is done by process-include and p/external-ref.
;;;
;;;   FIXME: Mime-type handling should be the job of the entity resolver,
;;;   but that requires xstream hacking.

;;; 4.6. externalRef element
;;;   Done by p/external-ref.

;;; 4.7. include element
;;;   Done by process-include.

;;; 4.8. name attribute of element and attribute elements
;;;   `name' is stored as a slot, not a child.  Done by p/element and
;;;    p/attribute.  

;;; 4.9. ns attribute
;;;    done by p/name-class, p/value, p/element, p/attribute

;;; 4.10. QNames
;;;    done by p/name-class

;;; 4.11. div element
;;;    Legen wir gar nicht erst an.

;;; 4.12. 4.13 4.14 4.15
;;;    beim anlegen

;;; 4.16
;;;    p/name-class
;;;    -- ausser der sache mit den datentypen

;;; 4.17, 4.18, 4.19
;;;    Ueber die Grammar-und Definition Objekte, wie von James Clark
;;;    beschrieben.
;;;
;;;    Dabei werden keine Umbenennungen vorgenommen, weil Referenzierung
;;;    durch Aufbei der Graphenstruktur zwischen ref und Definition
;;;    erfolgt und Namen dann bereits aufgeloest sind.  Wir benennen
;;;    dafuer beim Serialisieren um.

(defmethod check-recursion ((pattern element) depth)
  (check-recursion (pattern-child pattern) (1+ depth)))

(defmethod check-recursion ((pattern ref) depth)
  (when (eql (pattern-crdepth pattern) depth)
    (rng-error nil "infinite recursion in ~A"
	       (defn-name (pattern-target pattern))))
  (when (null (pattern-crdepth pattern))
    (setf (pattern-crdepth pattern) depth)
    (check-recursion (defn-child (pattern-target pattern)) depth)
    (setf (pattern-crdepth pattern) t)))

(defmethod check-recursion ((pattern %parent) depth)
  (check-recursion (pattern-child pattern) depth))

(defmethod check-recursion ((pattern %combination) depth)
  (check-recursion (pattern-a pattern) depth)
  (check-recursion (pattern-b pattern) depth))

(defmethod check-recursion ((pattern %leaf) depth)
  (declare (ignore depth)))

(defmethod check-recursion ((pattern data) depth)
  (when (pattern-except pattern)
    (check-recursion (pattern-except pattern) depth)))


;;;; 4.20

;;; %PARENT

(defmethod fold-not-allowed ((pattern element))
  (setf (pattern-child pattern) (fold-not-allowed (pattern-child pattern)))
  pattern)

(defmethod fold-not-allowed ((pattern %parent))
  (setf (pattern-child pattern) (fold-not-allowed (pattern-child pattern)))
  (if (typep (pattern-child pattern) 'not-allowed)
      (pattern-child pattern)
      pattern))

;;; %COMBINATION

(defmethod fold-not-allowed ((pattern %combination))
  (setf (pattern-a pattern) (fold-not-allowed (pattern-a pattern)))
  (setf (pattern-b pattern) (fold-not-allowed (pattern-b pattern)))
  pattern)

(defmethod fold-not-allowed ((pattern group))
  (call-next-method)
  (cond
    ;; remove if any child is not allowed
    ((typep (pattern-a pattern) 'not-allowed) (pattern-a pattern))
    ((typep (pattern-b pattern) 'not-allowed) (pattern-b pattern))
    (t pattern)))

(defmethod fold-not-allowed ((pattern interleave))
  (call-next-method)
  (cond
    ;; remove if any child is not allowed
    ((typep (pattern-a pattern) 'not-allowed) (pattern-a pattern))
    ((typep (pattern-b pattern) 'not-allowed) (pattern-b pattern))
    (t pattern)))

(defmethod fold-not-allowed ((pattern choice))
  (call-next-method)
  (cond
    ;; if any child is not allowed, choose the other
    ((typep (pattern-a pattern) 'not-allowed) (pattern-b pattern))
    ((typep (pattern-b pattern) 'not-allowed) (pattern-a pattern))
    (t pattern)))

;;; LEAF

(defmethod fold-not-allowed ((pattern %leaf))
  pattern)

(defmethod fold-not-allowed ((pattern data))
  (when (pattern-except pattern)
    (setf (pattern-except pattern) (fold-not-allowed (pattern-except pattern)))
    (when (typep (pattern-except pattern) 'not-allowed)
      (setf (pattern-except pattern) nil)))
  pattern)

;;; REF

(defmethod fold-not-allowed ((pattern ref))
  pattern)


;;;; 4.21

;;; %PARENT

(defmethod fold-empty ((pattern one-or-more))
  (call-next-method)
  (if (typep (pattern-child pattern) 'empty)
      (pattern-child pattern)
      pattern))

(defmethod fold-empty ((pattern %parent))
  (setf (pattern-child pattern) (fold-empty (pattern-child pattern)))
  pattern)

;;; %COMBINATION

(defmethod fold-empty ((pattern %combination))
  (setf (pattern-a pattern) (fold-empty (pattern-a pattern)))
  (setf (pattern-b pattern) (fold-empty (pattern-b pattern)))
  pattern)

(defmethod fold-empty ((pattern group))
  (call-next-method)
  (cond
    ;; if any child is empty, choose the other
    ((typep (pattern-a pattern) 'empty) (pattern-b pattern))
    ((typep (pattern-b pattern) 'empty) (pattern-a pattern))
    (t pattern)))

(defmethod fold-empty ((pattern interleave))
  (call-next-method)
  (cond
    ;; if any child is empty, choose the other
    ((typep (pattern-a pattern) 'empty) (pattern-b pattern))
    ((typep (pattern-b pattern) 'empty) (pattern-a pattern))
    (t pattern)))

(defmethod fold-empty ((pattern choice))
  (call-next-method)
  (if (typep (pattern-b pattern) 'empty)
      (cond
	((typep (pattern-a pattern) 'empty)
	  (pattern-a pattern))
	(t
	  (rotatef (pattern-a pattern) (pattern-b pattern))
	  pattern))
      pattern))

;;; LEAF

(defmethod fold-empty ((pattern %leaf))
  pattern)

(defmethod fold-empty ((pattern data))
  (when (pattern-except pattern)
    (setf (pattern-except pattern) (fold-empty (pattern-except pattern))))
  pattern)

;;; REF

(defmethod fold-empty ((pattern ref))
  pattern)


;;;; 7.1

(defun finalize-definitions (pattern)
  (let ((defns (make-hash-table)))
    (labels ((recurse (p)
	       (cond
		 ((typep p 'ref)
		  (let ((target (pattern-target p)))
		    (unless (gethash target defns)
		      (setf (gethash target defns) t)
		      (setf (defn-child target) (recurse (defn-child target))))
		    (if (typep (defn-child target) 'element)
			p
			(copy-pattern-tree (defn-child target)))))
		 (t
		  (etypecase p
		    (data
		     (when (pattern-except p)
		       (setf (pattern-except p) (recurse (pattern-except p)))))
		    (%parent
		     (setf (pattern-child p) (recurse (pattern-child p))))
		    (%combination
		     (setf (pattern-a p) (recurse (pattern-a p)))
		     (setf (pattern-b p) (recurse (pattern-b p))))
		    (%leaf))
		  p))))
      (recurse pattern))
    (loop
       for defn being each hash-key in defns
       collect defn)))

(defun copy-pattern-tree (pattern)
  (labels ((recurse (p)
	     (let ((q (copy-structure p)))
	       (etypecase p
		 (data
		  (when (pattern-except p)
		    (setf (pattern-except q) (recurse (pattern-except p)))))
		 (%parent
		  (setf (pattern-child q) (recurse (pattern-child p))))
		 (%combination
		  (setf (pattern-a q) (recurse (pattern-a p)))
		  (setf (pattern-b q) (recurse (pattern-b p))))
		 ((or %leaf ref)))
	       q)))
    (recurse pattern)))

(defparameter *in-attribute-p* nil)
(defparameter *in-one-or-more-p* nil)
(defparameter *in-one-or-more//group-or-interleave-p* nil)
(defparameter *in-list-p* nil)
(defparameter *in-data-except-p* nil)
(defparameter *in-start-p* nil)

(defun check-start-restrictions (pattern)
  (let ((*in-start-p* t))
    (check-restrictions pattern)))

(defun content-type-max (a b)
  (if (and a b)
      (cond
	((eq a :empty) b)
	((eq b :empty) a)
	((eq a :complex) b)
	(:simple))
      nil))

(defun groupable-max (a b)
  (if (or (eq a :empty)
	  (eq b :empty)
	  (and (eq a :complex)
	       (eq b :complex)))
      (content-type-max a b)
      nil))

(defmethod check-restrictions ((pattern attribute))
  (when *in-attribute-p*
    (rng-error nil "nested attribute not allowed"))
  (when *in-one-or-more//group-or-interleave-p*
    (rng-error nil "attribute not allowed in oneOrMore//group, oneOrMore//interleave"))
  (when *in-list-p*
    (rng-error nil "attribute in list not allowed"))
  (when *in-data-except-p*
    (rng-error nil "attribute in data/except not allowed"))
  (when *in-start-p*
    (rng-error nil "attribute in start not allowed"))
  (let ((*in-attribute-p* t))
    (if (check-restrictions (pattern-child pattern))
	:empty
	nil)))

(defmethod check-restrictions ((pattern ref))
  (when *in-attribute-p*
    (rng-error nil "ref in attribute not allowed"))
  (when *in-list-p*
    (rng-error nil "ref in list not allowed"))
  (when *in-data-except-p*
    (rng-error nil "ref in data/except not allowed"))
  :complex)

(defmethod check-restrictions ((pattern one-or-more))
  (when *in-data-except-p*
    (rng-error nil "oneOrMore in data/except not allowed"))
  (when *in-start-p*
    (rng-error nil "one-or-more in start not allowed"))
  (let* ((*in-one-or-more-p* t)
	 (x (check-restrictions (pattern-child pattern))))
    (groupable-max x x)))

(defmethod check-restrictions ((pattern group))
  (when *in-data-except-p*
    (rng-error nil "group in data/except not allowed"))
  (when *in-start-p*
    (rng-error nil "group in start not allowed"))
  (when *in-start-p*
    (rng-error nil "interleave in start not allowed"))
  (let ((*in-one-or-more//group-or-interleave-p*
	 *in-one-or-more-p*))
    (groupable-max (check-restrictions (pattern-a pattern))
		   (check-restrictions (pattern-b pattern)))))

(defmethod check-restrictions ((pattern interleave))
  (when *in-list-p*
    (rng-error nil "interleave in list not allowed"))
  (when *in-data-except-p*
    (rng-error nil "interleave in data/except not allowed"))
  (let ((*in-one-or-more//group-or-interleave-p*
	 *in-one-or-more-p*))
    (groupable-max (check-restrictions (pattern-a pattern))
		   (check-restrictions (pattern-b pattern)))))

(defmethod check-restrictions ((pattern choice))
  (content-type-max (check-restrictions (pattern-a pattern))
		    (check-restrictions (pattern-b pattern))))

(defmethod check-restrictions ((pattern list-pattern))
  (when *in-list-p*
    (rng-error nil "nested list not allowed"))
  (when *in-data-except-p*
    (rng-error nil "list in data/except not allowed"))
  (let ((*in-list-p* t))
    (check-restrictions (pattern-child pattern)))
  (when *in-start-p*
    (rng-error nil "list in start not allowed"))
  :simple)

(defmethod check-restrictions ((pattern text))
  (when *in-list-p*
    (rng-error nil "text in list not allowed"))
  (when *in-data-except-p*
    (rng-error nil "text in data/except not allowed"))
  (when *in-start-p*
    (rng-error nil "text in start not allowed"))
  :complex)

(defmethod check-restrictions ((pattern data))
  (when *in-start-p*
    (rng-error nil "data in start not allowed"))
  (when (pattern-except pattern)
    (let ((*in-data-except-p* t))
      (check-restrictions (pattern-except pattern))))
  :simple)

(defmethod check-restrictions ((pattern value))
  (when *in-start-p*
    (rng-error nil "value in start not allowed"))
  :simple)

(defmethod check-restrictions ((pattern empty))
  (when *in-data-except-p*
    (rng-error nil "empty in data/except not allowed"))
  (when *in-start-p*
    (rng-error nil "empty in start not allowed"))
  :empty)

(defmethod check-restrictions ((pattern element))
  (unless (check-restrictions (pattern-child pattern))
    (rng-error nil "restrictions on string sequences violated")))

(defmethod check-restrictions ((pattern not-allowed))
  nil)
