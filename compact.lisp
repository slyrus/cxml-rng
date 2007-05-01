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


(in-package :cxml-rng)

#+sbcl
(declaim (optimize (debug 2)))

(defparameter *keywords*
  '("attribute" "default" "datatypes" "div" "element" "empty" "external"
    "grammar" "include" "inherit" "list" "mixed" "namespace" "notAllowed"
    "parent" "start" "string" "text" "token"))

(defmacro double (x)
  `((lambda (x) (return (values x x))) ,x))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Lexer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(clex:deflexer test
    (
     ;; NCName
     (letter+extras
      (or (range #x0041 #x005A) (range #x0061 #x007A)
	  ;; just allow the rest of unicode, because clex can't deal with the
	  ;; complete definition of name-char:
	  (range #x00c0 #xd7ff)
	  (range #xe000 #xfffd)
	  (range #x10000 #x10ffff)))
     (digit (range #x0030 #x0039))	;ditto
     (name-start-char (or letter+extras #\_))
     (name-char (or letter+extras digit #\. #\- #\_ #\:))

     ;; some RNC ranges
     (char
      (or 9 10 13
	  (range 32 #xd7ff)
	  (range #xe000 #xfffd)
	  (range #x10000 #x10ffff)))
     (comment-char
      (or 9
	  (range 32 #xd7ff)
	  (range #xe000 #xfffd)
	  (range #x10000 #x10ffff)))
     (string-char
      (or 32
	  ;; #\"
	  (range 35 38)
	  ;; #\'
	  (range 40  #xd7ff)
	  (range #xe000 #xfffd)
	  (range #x10000 #x10ffff)))
     (space (or 9 10 13 32))
     (newline (or 10 13)))

  ((* space))

  (#\# (clex:begin 'comment))
  ((clex::in comment newline) (clex:begin 'clex:initial))
  ((clex::in comment comment-char))

  ((and "'''" (* (or string-char #\")) "'''")
   (return
     (values 'literal-segment (subseq clex:bag 3 (- (length clex:bag) 3)))))

  ((and #\' (* (or string-char #\")) #\')
   (when (or (find (code-char 13) clex:bag)
	     (find (code-char 10) clex:bag))
     (rng-error nil "disallowed newline in string literal"))
   (return
     (values 'literal-segment (subseq clex:bag 1 (- (length clex:bag) 1)))))

  ((and #\" #\" #\" (* (or string-char #\")) #\" #\" #\")
   (return
     (values 'literal-segment (subseq clex:bag 3 (- (length clex:bag) 3)))))

  ((and #\" (* (or string-char #\')) #\")
   (when (or (find (code-char 13) clex:bag)
	     (find (code-char 10) clex:bag))
     (rng-error nil "disallowed newline in string literal"))
   (return
     (values 'literal-segment (subseq clex:bag 1 (- (length clex:bag) 1)))))

  ((and name-start-char (* name-char))
   (return
     (cond
       ((find clex:bag *keywords* :test #'equal)
	(let ((sym (intern (string-upcase clex:bag) :keyword)))
	  (values sym sym)))
       ((find #\: clex:bag)
	(let* ((pos (position #\: clex:bag))
	       (prefix (subseq clex:bag 0 pos))
	       (lname (subseq clex:bag (1+ pos ))))
	  (when (find #\: lname)
	    (rng-error "too many colons"))
	  (unless (and (cxml-types::nc-name-p prefix))
	    (rng-error nil "not an ncname: ~A" prefix))
	  (let ((ch (clex::getch)))
	    (cond
	      ((and (equal lname "") (eql ch #\*))
	       (values 'nsname prefix))
	      (t
	       (clex::backup ch)
	       (unless (and (cxml-types::nc-name-p lname))
		 (rng-error nil "not an ncname: ~A" lname))
	       (values 'cname (cons prefix lname)))))))
       (t
	(unless (cxml-types::nc-name-p clex:bag)
	  (rng-error nil "not an ncname: ~A" clex:bag))
	(values 'identifier clex:bag)))))

  ((and #\\ name-start-char (* name-char))
   (let ((str (subseq clex:bag 1)))
     (unless (cxml-types::nc-name-p str)
       (rng-error nil "not an ncname: ~A" clex:bag))
     (return (values 'identifier str))))

  (#\= (double '=))
  (#\{ (double '{))
  (#\} (double '}))
  (#\[ (double '[))
  (#\] (double ']))
  (#\, (double '|,|))
  (#\& (double '&))
  (#\| (double '|\||))
  (#\? (double '?))
  (#\* (double '*))
  (#\+ (double '+))
  (#\( (double '|(|))
  (#\) (double '|)|))
  ((and "|=") (double '|\|=|))
  ((and "&=") (double '&=))
  ((and ">>") (double '>>))
  (#\~ (double '~))
  (#\- (double '-)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Parsing into S-Expressions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+(or)
  (defmacro lambda* ((&rest args) &body body)
    (setf args (mapcar (lambda (arg) (or arg (gensym))) args))
    `(lambda (,@args)
       (declare (ignorable ,@args))
       ,@body))

  (defmacro lambda* ((&rest args) &body body)
    (setf args (mapcar (lambda (arg) (or arg (gensym))) args))
    `(lambda (&rest .args.)
       (unless (equal (length .args.) ,(length args))
	 (error "expected ~A, got ~A" ',args .args.))
       (destructuring-bind (,@args) .args.
	 (declare (ignorable ,@args))
	 ,@body)))

  (defun wrap-decls (decls content)
    (if decls
	`(,@(car decls)
	    ,(wrap-decls (cadr decls) content))
	content)))

(yacc:define-parser *compact-parser*
  (:start-symbol top-level)
  (:terminals (:attribute :default :datatypes :div :element :empty
			  :external :grammar :include :inherit :list
			  :mixed :namespace :notAllowed :parent :start
			  :string :text :token
			  = { } |,| & |\|| ? * + |(| |)| |\|=| &= ~ -
			  [ ] >>
			  identifier literal-segment cname nsname
			  documentation))
  (:print-first-terminals t)
;;   (:print-states t)
;;   (:print-lookaheads t)
;;   (:print-goto-graph t)
  #+nil (:muffle-conflicts (57 10))		;hrmpf

  (top-level (decl* pattern #'wrap-decls)
	     (decl* grammar-content*
		    (lambda (a b) (wrap-decls a `(with-grammar () ,@b)))))

  (decl* () (decl decl*))

  (decl (:namespace identifier-or-keyword = namespace-uri-literal
		    (lambda* (nil name nil uri)
		      `(with-namespace (:uri ,uri :name ,name))))
	(:default :namespace = namespace-uri-literal
		  (lambda* (nil nil nil uri)
		    `(with-namespace (:uri ,uri :default t))))
	(:default :namespace identifier-or-keyword = namespace-uri-literal
		  (lambda* (nil nil name nil uri)
		    `(with-namespace (:uri ,uri :name ,name :default t))))
	(:datatypes identifier-or-keyword = literal
		    (lambda* (nil name nil uri)
		      `(with-data-type (:name ,name :uri ,uri)))))

  (pattern (inner-pattern
	    (lambda* (p) `(without-annotations ,p))))

  (particle (inner-particle
	     (lambda* (p) `(without-annotations ,p))))

  (inner-pattern inner-particle
		 (particle-choice (lambda* (p) `(%with-annotations ,p)))
		 (particle-group (lambda* (p) `(%with-annotations ,p)))
		 (particle-interleave (lambda* (p) `(%with-annotations ,p)))
		 (data-except (lambda* (p) `(%with-annotations-group ,p))))

  (primary (:element name-class { pattern }
		     (lambda* (nil name nil pattern nil)
		       `(with-element (:name ,name) ,pattern)))
	   (:attribute name-class { pattern }
		       (lambda* (nil name nil pattern nil)
			 `(with-attribute (:name ,name) ,pattern)))
	   (:list { pattern }
		  (lambda* (nil nil pattern nil)
		    `(list ,pattern)))
	   (:mixed { pattern }
		   (lambda* (nil nil pattern nil)
		     `(mixed ,pattern)))
	   (identifier (lambda* (x)
			 `(ref ,x)))
	   (:parent identifier
		    (lambda* (nil x)
		      `(parent-ref ,x)))
	   (:empty)
	   (:text)
	   (data-type-name [params]
			   (lambda* (name params)
			     `(data :data-type ,name :params ,params)))
	   (data-type-name data-type-value
			   (lambda* (name value)
			     `(value :data-type ,name :value ,value)))
	   (data-type-value (lambda* (value)
			      `(value :data-type nil :value ,value)))
	   (:notallowed)
	   (:external any-uri-literal [inherit]
		      (lambda* (nil uri inherit)
			`(external-ref :uri ,uri :inherit ,inherit)))
	   (:grammar { grammar-content* }
		     (lambda* (nil nil content nil)
		       `(with-grammar () ,@content)))
	   (\( pattern \) (lambda* (nil p nil) p)))

  (data-except (data-type-name [params] - lead-annotated-primary
			       (lambda* (name params nil p)
				 `(data :data-type ,name
					:params ,params
					:except p))))

  (inner-particle (annotated-primary
		   (lambda* (p) `(%with-annotations-group ,p)))
		  (repeated-primary follow-annotations
				    (lambda* (a b)
				      `(progn
					 (%with-annotations ,a)
					 ,b))))

  (repeated-primary (annotated-primary *
				       (lambda* (p nil) `(zero-or-more ,p)))
		    (annotated-primary +
				       (lambda* (p nil) `(one-or-more ,p)))
		    (annotated-primary ?
				       (lambda* (p nil) `(optional ,p))))

  (annotated-primary (lead-annotated-primary follow-annotations
					     (lambda* (a b)
					       `(progn ,a ,b))))

  (annotated-data-except (lead-annotated-data-except follow-annotations
						     (lambda* (a b)
						       `(progn ,a ,b))))

  (lead-annotated-data-except (annotations data-except
					   (lambda* (a p)
					     `(with-annotations ,a ,p))))

  (lead-annotated-primary (annotations primary
				       (lambda* (a p)
					 `(with-annotations ,a ,p)))
			  (annotations \( inner-pattern \)
				       (lambda* (a nil p nil)
					 `(let-annotations ,a ,p))))

  (particle-choice (particle \| particle
			     (lambda* (a nil b) `(choice ,a ,b)))
		   (particle \| particle-choice
			     (lambda* (a nil b) `(choice ,a ,@(cdr b)))))

  (particle-group (particle \, particle
			    (lambda* (a nil b) `(group ,a ,b)))
		  (particle \, particle-group
			    (lambda* (a nil b) `(group ,a ,@(cdr b)))))

  (particle-interleave (particle \& particle
				 (lambda* (a nil b) `(interleave ,a ,b)))
		       (particle \& particle-interleave
				 (lambda* (a nil b) `(interleave ,a ,@(cdr b)))))

  (param (annotations identifier-or-keyword = literal
		      (lambda* (a name nil value)
			`(with-annotations ,a (param ,name ,value)))))

  (grammar-content* ()
		    (member grammar-content* #'cons))

  (member annotated-component
	  annotated-element-not-keyword)

  (annotated-component (annotations component
				    (lambda* (a c)
				      `(with-annotations (,@a) ,c))))

  (component start
	     define
	     (:div { grammar-content* }
		   (lambda* (nil nil content nil)
		     `(with-div ,@content)))
	     (:include any-uri-literal [inherit] [include-content]
		       (lambda* (nil uri inherit content)
			 `(with-include (:inherit ,inherit)
			    ,@content))))

  (include-content* ()
		    (include-member include-content* #'cons))

  (include-member annotated-include-component
		  annotation-element-not-keyword)

  (annotated-include-component (annotations include-component
					    (lambda* (a c)
					      `(with-annotations (,@a) ,c))))

  (include-component start
		     define
		     (:div { grammar-content* }
			   (lambda* (nil nil content nil)
			     `(with-div ,@content))))

  (start (:start assign-method pattern
		 (lambda* (nil method pattern)
		   `(with-start (:combine-method ,method) ,pattern))))

  (define (identifier assign-method pattern
		      (lambda* (name method pattern)
			`(with-definition (:name ,name :combine-method ,method)
			   ,pattern))))

  (assign-method (= (constantly nil))
		 (\|= (constantly "choice"))
		 (&= (constantly "interleave")))

  (name-class (inner-name-class (lambda (nc) `(without-annotations ,nc))))

  (inner-name-class (annotated-simple-nc
		     (lambda (nc) `(%with-annotations-choice ,nc)))
		    (nc-choice
		     (lambda (nc) `(%with-annotations ,nc)))
		    (annotated-nc-except
		     (lambda (nc) `(%with-annotations-choice ,nc))))

  (simple-nc (name (lambda* (n) `(name ,n)))
	     (ns-name (lambda* (n) `(ns-name ,n)))
	     (* (constantly `(any-name)))
	     (\( name-class \) (lambda* (nil nc nil) nc)))

  (follow-annotations ()
		      (>> annotation-element follow-annotations))

  (annotations ()
	       (documentations)
	       ([ annotation-attributes annotation-elements ]
		  )
	       (documentations [ annotation-attributes annotation-elements ]
			       ))

  (annotation-attributes
   ((constantly '(annotation-attributes)))
   (foreign-attribute-name = literal annotation-attributes
			   (lambda* (name nil value rest)
			     `(annotation-attributes
			       (attribute ,name ,value)
			       ,@(cdr rest)))))

  (foreign-attribute-name prefixed-name)

  (annotation-elements ()
		       (annotation-element annotation-elements #'cons))

  (annotation-element (foreign-element-name annotation-attributes-content
					    (lambda (a b)
					      `(with-annotation-element
						   (:name ,a)
						 ,b))))

  (foreign-element-name identifier-or-keyword
			prefixed-name)

  (annotation-element-not-keyword (foreign-element-name-not-keyword
				   annotation-attributes-content
				   (lambda (a b)
				     `(with-annotation-element
					  (:name ,a)
					,b))))

  (foreign-element-name-not-keyword identifier prefixed-name)

  (annotation-attributes-content ([ nested-annotation-attributes
				     annotation-content ]))

  (nested-annotation-attributes
   ((constantly '(annotation-attributes)))
   (any-attribute-name = literal
		       nested-annotation-attributes
		       (lambda* (name nil value rest)
			 `(annotation-attributes
			   (attribute ,name ,value)
			   ,@(cdr rest)))))

  (any-attribute-name identifier-or-keyword prefixed-name)

  (annotation-content ()
		      (nested-annotation-element annotation-content #'cons)
		      (literal annotation-content #'cons))

  (nested-annotation-element (any-element-name annotation-attributes-content
					       (lambda (a b)
						 `(with-annotation-element
						      (:name ,a)
						    ,b))))

  (any-element-name identifier-or-keyword prefixed-name)

  (prefixed-name cname)

  (documentations (documentation)
		  (documentation documentations #'cons))

  (annotated-nc-except (lead-annotated-nc-except
			follow-annotations
			(lambda (p a)
			  `(progn ,p ,a))))

  (lead-annotated-nc-except (annotations nc-except
					 (lambda (a p)
					   `(with-annotations ,a ,p))))

  (annotated-simple-nc lead-annotated-simple-nc
		       (lead-annotated-simple-nc
			follow-annotations
			(lambda (p a) `(progn ,p ,a))))

  (lead-annotated-simple-nc
   (annotations simple-nc
		(lambda (a nc) `(with-annotations ,a ,nc)))
   (annotations \( inner-name-class \)
		(lambda (a nc) `(let-annotations ,a ,nc))))

  (nc-except (ns-name - simple-nc
		      (lambda* (nc1 nil nc2) `(ns-name ,nc1 :except ,nc2)))
	     (* - simple-nc
		(lambda* (nil nil nc) `(any-name :except ,nc))))

  (nc-choice (annotated-simple-nc \| annotated-simple-nc
				  (lambda* (a nil b)
				    `(name-choice ,a ,b)))
	     (annotated-simple-nc \| nc-choice
				  (lambda* (a nil b)
				    `(name-choice ,a ,@(cdr b)))))

  (name identifier-or-keyword cname)

  (data-type-name cname :string :token)

  (data-type-value literal)
  (any-uri-literal literal)

  (namespace-uri-literal literal :inherit)

  (inherit (:inherit = identifier-or-keyword
		     (lambda* (nil nil x) x)))

  (identifier-or-keyword identifier keyword)

  ;; identifier ::= (ncname - keyword) | quotedidentifier
  ;; quotedidentifier ::= "\" ncname

  ;; (ns-name (ncname \:*))
  (ns-name nsname)

  (ncname identifier-or-keyword)

  (literal literal-segment
	   (literal-segment ~ literal
			    (lambda* (a nil b)
			      (concatenate 'string a b))))

  ;; literalsegment ::= ...

  (keyword :attribute :default :datatypes :div :element :empty :external
	   :grammar :include :inherit :list :mixed :namespace :notAllowed
	   :parent :start :string :text :token)

  ;; optional stuff
  ([data-type-name] () data-type-name)
  ([inherit] () inherit)
  ([params] () ({ params } (lambda* (nil p nil) p)))
  (params () (param params #'cons))
  ([include-content] () ({ include-content* }
			   (lambda* (nil content nil) content))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Conversion of sexps into SAX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun uncompact (list)
  (funcall (or (get (car list) 'uncompactor)
	       (error "no uncompactor for ~A" (car list)))
	   (cdr list)))

(defmacro define-uncompactor (name (&rest args) &body body)
  `(setf (get ',name 'uncompactor)
	 (lambda (.form.) (destructuring-bind ,args .form. ,@body))))

(defvar *namespaces* '(("xml" . "http://www.w3.org/XML/1998/namespace")))
(defvar *default-namespace* "")
(defvar *data-types* '(("xsd" . "http://www.w3.org/2001/XMLSchema-datatypes")))

(defvar *parent-namespaces* nil)
(defvar *parent-default-namespace* nil)

(defun xor (a b)
  (if a (not b) b))

(defun lookup-prefix (prefix)
  (cdr (assoc prefix *namespaces* :test 'equal)))

(defun lookup-data-type (name)
  (assoc name *data-types* :test 'equal))

(define-uncompactor with-namespace ((&key uri name default) &body body)
  (when (xor (equal name "xml")
	     (equal uri "http://www.w3.org/XML/1998/namespace"))
    (rng-error nil "invalid redeclaration of `xml' namespace"))
  (when (equal name "xmlns")
    (rng-error nil "invalid declaration of `xmlns' namespace"))
  (when (eq uri :inherit)
    (setf uri (if name
		  (cdr (assoc name *parent-namespaces* :test 'equal))
		  *parent-default-namespace*)))
  (let ((*namespaces* *namespaces*)
	(*default-namespace* *default-namespace*))
    (when name
      (when (lookup-prefix name)
	(rng-error nil "duplicate declaration of prefix ~A" name))
      (push (cons name uri) *namespaces*))
    (when default
      (when *default-namespace*
	(rng-error nil "multiple default namespaces declared"))
      (push uri *default-namespace*))
    (mapc #'uncompact body)))

(define-uncompactor with-data-type ((&key name uri) &body body)
  (when (and (equal name "xsd")
	     (not (equal uri "http://www.w3.org/2001/XMLSchema-datatypes")))
    (rng-error nil "invalid redeclaration of `xml' namespace"))
  (when (lookup-data-type name)
    (rng-error nil "duplicate declaration of library ~A" name))
  (let ((*data-types* (acons name uri *data-types*)))
    (mapc #'uncompact body)))

(define-uncompactor with-grammar ((&optional) &body body)
  (cxml:with-element "grammar"
    (mapc #'uncompact body)))

(define-uncompactor with-start ((&key combine-method) &body body)
  (cxml:with-element "start"
    (cxml:attribute "combine" combine-method)
    (mapc #'uncompact body)))

(define-uncompactor ref (name)
  (cxml:with-element "ref"
    (cxml:attribute "name" name)))

(define-uncompactor parent-ref (name)
  (cxml:with-element "parentRef"
    (cxml:attribute "name" name)))

(define-uncompactor parent-ref (name)
  (cxml:with-element "parentRef"
    (cxml:attribute "name" name)))

(define-uncompactor external-ref (&key uri inherit)
  (cxml:with-element "externalRef"
    (cxml:attribute "uri" uri)
    (cxml:attribute "ns" (if inherit
			     (lookup-prefix inherit)
			     *default-namespace*))))

(define-uncompactor with-element ((&key name) pattern)
  (cxml:with-element "element"
    (uncompact name)
    (uncompact pattern)))

(define-uncompactor with-attribute ((&key name) pattern)
  (cxml:with-element "attribute"
    (uncompact name)
    (uncompact pattern)))

(define-uncompactor list (pattern)
  (cxml:with-element "list"
    (uncompact pattern)))

(define-uncompactor mixed (pattern)
  (cxml:with-element "mixed"
    (uncompact pattern)))

(define-uncompactor :empty ()
  (cxml:with-element "empty"))

(define-uncompactor :text ()
  (cxml:with-element "text"))

(define-uncompactor data (&key data-type params except)
  (cxml:with-element "data"
    (case data-type
      (:string
       (cxml:attribute "datatypeLibrary" "")
       (cxml:attribute "type" "string"))
      (:token
       (cxml:attribute "datatypeLibrary" "")
       (cxml:attribute "type" "token"))
      (t
       (cxml:attribute "datatypeLibrary" (lookup-data-type (car data-type)))
       (cxml:attribute "type" (cadr data-type))))
    (mapc #'uncompact params)
    (when except
      (uncompact except))))

(define-uncompactor value (&key data-type value)
  (cxml:with-element "value"
    (when data-type
      (cxml:attribute "type" data-type))
    (cxml:text value)))

(define-uncompactor :notallowed ()
  (cxml:with-element "notAllowed"))

(define-uncompactor with-definition ((&key name combine-method) &body body)
  (cxml:with-element "define"
    (cxml:attribute "name" name)
    (cxml:attribute "combine" combine-method)
    (mapc #'uncompact body)))

(define-uncompactor with-div (&body body)
  (cxml:with-element "div"
    (mapc #'uncompact body)))

(define-uncompactor any-name (&key except)
  (cxml:with-element "anyName"
    (when except
      (uncompact except))))

(define-uncompactor ns-name (nc &key except)
  (cxml:with-element "nsName"
    (cxml:attribute "ns" (lookup-prefix nc))
    (when except
      (uncompact except))))

(define-uncompactor name-choice (&rest ncs)
  (cxml:with-element "choice"
    (mapc #'uncompact ncs)))

(defun destructure-cname-like (x)
  (when (keywordp x) (setf x (string-downcase (symbol-name x))))
  (when (atom x) (setf x (list "" x)))
  (values (lookup-prefix (car x))
	  (cdr x)))

(define-uncompactor name (x)
  (cxml:with-element "name"
    (multiple-value-bind (uri lname) (destructure-cname-like x)
      (cxml:attribute "ns" uri)
      (cxml:text lname))))

(define-uncompactor choice (&rest body)
  (cxml:with-element "choice"
    (mapc #'uncompact body)))

(define-uncompactor group (&rest body)
  (cxml:with-element "group"
    (mapc #'uncompact body)))

(define-uncompactor interleave (&rest body)
  (cxml:with-element "interleave"
    (mapc #'uncompact body)))

(define-uncompactor one-or-more (p)
  (cxml:with-element "oneOrMore"
    (uncompact p)))

(define-uncompactor optional (p)
  (cxml:with-element "optional"
    (uncompact p)))

(define-uncompactor zero-or-more (p)
  (cxml:with-element "zeroOrMore"
    (uncompact p)))

(define-uncompactor with-include ((&key inherit) &body body)
  (cxml:with-element "include"
    (cxml:attribute "ns" (if inherit
			   (lookup-prefix inherit)
			   *default-namespace*))
    (mapc #'uncompact body)))

(defun compact (&optional (p #p"/home/david/src/lisp/cxml-rng/rng.rnc"))
  (flet ((doit (s)
	   (handler-case
	       (let ((lexer (make-test-lexer s)))
		 (yacc:parse-with-lexer
		  (lambda ()
		    (multiple-value-bind (cat sem) (funcall lexer)
		      #+nil (print (list cat sem))
		      (if (eq cat :eof)
			  nil
			  (values cat sem))))
		  *compact-parser*))
	     (error (c)
	       (error "~A ~A" (file-position s) c)))))
    (let ((tree
	   (if (pathnamep p)
	       (with-open-file (s p) (doit s))
	       (with-input-from-string (s p) (doit s)))))
      (print tree)
      (cxml:with-xml-output (cxml:make-character-stream-sink
			     *standard-output*
			     :indentation 2
			     :canonical nil)
	(uncompact tree)))))

#+(or)
(compact)
