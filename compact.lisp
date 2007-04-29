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
     (nc-name-start-char (or letter+extras #\_))
     (nc-name-char (or letter+extras digit #\. #\- #\_))

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

  ((and nc-name-start-char (* nc-name-char))
   (return
     (cond
       ((find clex:bag *keywords* :test #'equal)
	(let ((sym (intern (string-upcase clex:bag) :keyword)))
	  (values sym sym)))
       (t
	(unless (cxml-types::nc-name-p clex:bag)
	  (rng-error nil "not an ncname: ~A" clex:bag))
	(values 'identifier clex:bag)))))

  ((and #\\ nc-name-start-char (* nc-name-char))
   (let ((str (subseq clex:bag 1)))
     (unless (cxml-types::nc-name-p str)
       (rng-error nil "not an ncname: ~A" clex:bag))
     (return (values 'identifier str))))

  (#\= (return '=))
  (#\{ (return '{))
  (#\} (return '}))
  (#\, (return '|,|))
  (#\& (return '&))
  (#\| (return '|\||))
  (#\? (return '?))
  (#\* (return '*))
  (#\+ (return '+))
  (#\( (return '|(|))
  (#\) (return '|)|))
  (#\| (return '|\|=|))
  (#\& (return '&=))
  (#\: (return '|:|))
  (#\: (return '|:*|))
  (#\~ (return '~))
  (#\- (return '-)))

(yacc:define-parser *compact-parser*
  (:start-symbol top-level)
  (:terminals (:attribute :default :datatypes :div :element :empty
			  :external :grammar :include :inherit :list
			  :mixed :namespace :notAllowed :parent :start
			  :string :text :token
			  = { } |,| & |\|| ? * + |(| |)| |\|=| &= |:| |:*| ~
			  identifier literal-segment))

  (top-level (decl* pattern)
	     (decl* grammmar-content*))

  (decl* ()
	 (decl decl*))

  (decl (:namespace identifier-or-keyword = namespace-uri-literal
		    #'(lambda (a b c d)
			(declare (ignorable a b c d))
			(print (list :saw-namespace b d))))
	(:default :namespace = namespace-uri-literal)
	(:default :namespace identifier-or-keyword = namespace-uri-literal)
	(:datatypes identifier-or-keyword = literal))

  (pattern (:element name-class { pattern })
	   (:attribute name-class { pattern })
	   (pattern \, pattern)
	   (pattern & pattern)
	   (pattern \| pattern)
	   (pattern ?)
	   (pattern *)
	   (pattern +)
	   (:list { pattern })
	   (:mixed { pattern } )
	   identifier
	   (:parent identifier)
	   :empty
	   :text
	   ([data-type-name] data-type-value [params] [except-pattern])
	   :not-allowed
	   (:external any-uri-literal [inherit])
	   (:grammar { grammar-content* })
	   (\( pattern \)))

  (param (identifier-or-keyword = literal))

  (except-pattern (- pattern))

  (grammar-content* ()
		    (grammar-content grammar-content*))

  (grammar-content (start)
		   (define)
		   (:div { grammar-content* })
		   (:include any-uri-literal [inherit] [include-content]))

  (include-content* ()
		    (include-content include-content*))

  (include-content (start)
		   (define)
		   (:div { grammar-content* }))

  (start (:start assign-method pattern))

  (define (identifier assign-method pattern))

  (assign-method (=) (\|=) (&=))

  (name-class (name)
	      (ns-name [except-name-class])
	      (any-name [except-name-class])
	      (name-class \| name-class)
	      (\( name-class \)))

  (name (identifier-or-keyword)
	(cname))

  (except-name-class (- name-class))

  (data-type-name (cname)
		  (:string)
		  (:token))

  (data-type-value literal)
  (any-uri-literal literal)

  (namespace-uri-literal literal
			 :inherit)

  (inherit (:inherit = identifier-or-keyword))

  (identifier-or-keyword identifier
			 keyword)

  ;; identifier ::= (ncname - keyword) | quotedidentifier
  ;; quotedidentifier ::= "\" ncname

  (cname (ncname \: ncname))

  (ns-name (ncname \:*))

  (any-name (*))

  (literal literal-segment
	   (literal-segment ~ literal))

  ;; literalsegment ::= ...

  (keyword :default :datatypes :div :element :empty :external :grammar :include
	   :inherit :list :mixed :namespace :notAllowed :parent :start :string
	   :text :token)

  ;; optional stuff
  ([data-type-name] () data-type-name)
  ([inherit] () inherit)
  ([params] () ({ params }))
  (params () (param params))
  ([except-pattern] () (except-pattern))
  ([include-content] () ({ include-content* }))
  ([except-name-class] () except-name-class))

(defun compact (&optional (p #p"/home/david/src/lisp/cxml-rng/rng.rnc"))
  (flet ((doit (s)
	   (let ((lexer (make-test-lexer s)))
	     (yacc:parse-with-lexer
	      (lambda ()
		(multiple-value-bind (cat sem) (funcall lexer)
		  (if (eq cat :eof)
		      nil
		      (values cat sem))))
	      *compact-parser*))))
    (if (pathnamep p)
	(with-open-file (s p) (doit s))
	(with-input-from-string (s p) (doit s)))))

#+(or)
(compact)
