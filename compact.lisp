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
   (return (subseq clex:bag 3 (- (length clex:bag) 3))))

  ((and #\' (* (or string-char #\")) #\')
   (when (or (find (code-char 13) clex:bag)
	     (find (code-char 10) clex:bag))
     (rng-error nil "disallowed newline in string literal"))
   (return
     (subseq clex:bag 1 (- (length clex:bag) 1))))

  ((and #\" #\" #\" (* (or string-char #\")) #\" #\" #\")
   (return (subseq clex:bag 3 (- (length clex:bag) 3))))

  ((and #\" (* (or string-char #\')) #\")
   (when (or (find (code-char 13) clex:bag)
	     (find (code-char 10) clex:bag))
     (rng-error nil "disallowed newline in string literal"))
   (return
     (subseq clex:bag 1 (- (length clex:bag) 1))))

  ((and nc-name-start-char (* nc-name-char))
   (return
     (cond
       ((find clex:bag *keywords* :test #'equal)
	(intern (string-upcase clex:bag) :keyword))
       (t
	(unless (cxml-types::nc-name-p clex:bag)
	  (rng-error nil "not an ncname: ~A" clex:bag))
	clex:bag))))

  ((and #\\ nc-name-start-char (* nc-name-char))
   (let ((str (subseq clex:bag 1)))
     (unless (cxml-types::nc-name-p str)
       (rng-error nil "not an ncname: ~A" clex:bag))
     (return str)))

  (#\= (return :=))
  (#\{ (return :{))
  (#\} (return :}))
  (#\, (return :|,|))
  (#\& (return :&))
  (#\| (return :|\||))
  (#\? (return :?))
  (#\* (return :*))
  (#\+ (return :+))
  (#\( (return :|(|))
  (#\) (return :|)|))
  (#\| (return :|\|=|))
  (#\& (return :&=))
  (#\: (return :|:|))
  (#\: (return :|:*|))
  (#\~ (return :~))
  (#\- (return :-)))

(defun compact (&optional (p #p"/home/david/src/lisp/cxml-rng/rng.rnc"))
  (if (pathnamep p)
      (with-open-file (s p)
	(let ((f (make-test-lexer s)))
	  (loop
	     for k = (funcall f)
	     until (eq k :eof)
	     collect k)))
      (with-input-from-string (s p)
	(let ((f (make-test-lexer s)))
	  (loop
	     for k = (funcall f)
	     until (eq k :eof)
	     collect k)))))

#+(or)
(compact)
