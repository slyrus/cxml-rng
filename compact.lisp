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

(defparameter *keywords*
  '("attribute" "default" "datatypes" "div" "element" "empty" "external"
    "grammar" "include" "inherit" "list" "mixed" "namespace" "notAllowed"
    "parent" "start" "string" "text" "token"))

(defmacro double (x)
  `((lambda (x) (return (values x x))) ,x))

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
	       (values 'cname prefix lname))))))
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
  (#\~ (double '~))
  (#\- (double '-)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun sxfc (&rest args)
    #+nil (print args)
    args))

(yacc:define-parser *compact-parser*
  (:start-symbol top-level)
  (:terminals (:attribute :default :datatypes :div :element :empty
			  :external :grammar :include :inherit :list
			  :mixed :namespace :notAllowed :parent :start
			  :string :text :token
			  = { } |,| & |\|| ? * + |(| |)| |\|=| &= ~ -
			  identifier literal-segment cname nsname))
  #+nil (:print-states t)
  #+nil (:print-goto-graph t)

  (top-level (decl* pattern #'sxfc)
	     (decl* grammar-content* #'sxfc))

  (decl* (#'sxfc)
	 (decl decl* #'sxfc))

  (decl (:namespace identifier-or-keyword = namespace-uri-literal #'sxfc)
	(:default :namespace = namespace-uri-literal #'sxfc)
	(:default :namespace identifier-or-keyword = namespace-uri-literal
		  #'sxfc)
	(:datatypes identifier-or-keyword = literal #'sxfc))

  (pattern particle
	   (particle-choice #'sxfc)
	   (particle-group #'sxfc)
	   (particle-interleave #'sxfc)
	   (data-except))

  (primary (:element name-class { pattern } #'sxfc)
	   (:attribute name-class { pattern } #'sxfc)
	   (:list { pattern } #'sxfc)
	   (:mixed { pattern } #'sxfc)
	   (identifier #'sxfc)
	   (:parent identifier #'sxfc)
	   (:empty #'sxfc)
	   (:text #'sxfc)
	   (data-type-name [params])
	   (data-type-name data-type-value #'sxfc)
	   (data-type-value #'sxfc)
	   (:notallowed #'sxfc)
	   (:external any-uri-literal [inherit] #'sxfc)
	   (:grammar { grammar-content* } #'sxfc)
	   (\( pattern \) #'sxfc))

  (data-except (data-type-name [params] - primary))

  (particle (primary)
	    (repeated-particle))

  (repeated-particle (primary *)
		     (primary +)
		     (primary ?))

  (particle-choice (particle \| particle #'sxfc)
		   (particle \| particle-choice #'sxfc))

  (particle-group (particle \, particle #'sxfc)
		  (particle \, particle-group #'sxfc))

  (particle-interleave (particle \& particle #'sxfc)
		       (particle \& particle-interleave #'sxfc))

  (param (identifier-or-keyword = literal #'sxfc))

  (except-pattern (- pattern #'sxfc))

  (grammar-content* (#'sxfc)
		    (grammar-content grammar-content* #'sxfc))

  (grammar-content (start #'sxfc)
		   (define #'sxfc)
		   (:div { grammar-content* } #'sxfc)
		   (:include any-uri-literal [inherit] [include-content]
			     #'sxfc))

  (include-content* (#'sxfc)
		    (include-content include-content* #'sxfc))

  (include-content (start #'sxfc)
		   (define #'sxfc)
		   (:div { grammar-content* } #'sxfc))

  (start (:start assign-method pattern #'sxfc))

  (define (identifier assign-method pattern #'sxfc))

  (assign-method (= #'sxfc)
		 (\|= #'sxfc)
		 (&= #'sxfc))

  (name-class (simple-nc)
	      (nc-choice)
	      (nc-except))

  (simple-nc (name #'sxfc)
	     (ns-name #'sxfc)
	     (* #'sxfc)
	     (\( name-class \) #'sxfc))

  (nc-except (ns-name - simple-nc #'sxfc)
	     (* - simple-nc #'sxfc))

  (nc-choice (simple-nc \| simple-nc #'sxfc)
	     (simple-nc \| nc-choice #'sxfc))

  (name (identifier-or-keyword #'sxfc)
	(cname #'sxfc))

  (data-type-name (cname #'sxfc)
		  (:string #'sxfc)
		  (:token #'sxfc))

  (data-type-value (literal #'sxfc))
  (any-uri-literal (literal #'sxfc))

  (namespace-uri-literal (literal #'sxfc)
			 (:inherit #'sxfc))

  (inherit (:inherit = identifier-or-keyword #'sxfc))

  (identifier-or-keyword (identifier #'sxfc)
			 (keyword #'sxfc))

  ;; identifier ::= (ncname - keyword) | quotedidentifier
  ;; quotedidentifier ::= "\" ncname

  ;; (ns-name (ncname \:* #'sxfc))
  (ns-name (nsname #'sxfc))

  (ncname (identifier-or-keyword #'sxfc))

  (literal (literal-segment #'sxfc)
	   (literal-segment ~ literal #'sxfc))

  ;; literalsegment ::= ...

  (keyword :attribute :default :datatypes :div :element :empty :external
	   :grammar :include :inherit :list :mixed :namespace :notAllowed
	   :parent :start :string :text :token)

  ;; optional stuff
  ([data-type-name] (#'sxfc) (data-type-name #'sxfc))
  ([inherit] (#'sxfc) (inherit #'sxfc))
  ([params] (#'sxfc) ({ params } #'sxfc))
  (params (#'sxfc) (param params #'sxfc))
  ([except-pattern] (#'sxfc) (except-pattern #'sxfc))
  ([include-content] (#'sxfc) ({ include-content* } #'sxfc)))

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
    (if (pathnamep p)
	(with-open-file (s p) (doit s))
	(with-input-from-string (s p) (doit s)))))

#+(or)
(compact)
