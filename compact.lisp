(in-package :cxml-rng)

(defparameter *keywords*
  '("attribute" "default" "datatypes" "div" "element" "empty" "external"
    "grammar" "include" "inherit" "list" "mixed" "namespace" "notAllowed"
    "parent" "start" "string" "text" "token"
    ;; keywordlike:
    "=" "{" "}" "," "&" "|" "?" "*" "+" "(" ")" "|=" "&=" ":" ":*" "~"))

(clex:deflexer test
    ((space
      (or #.(code-char 9)
	  #.(code-char 10)
	  #.(code-char 13)
	  #.(code-char 32)))
     (normal-char
      (or #.(code-char 33)
	  (range #.(code-char 35) #.(code-char 38))
	  (range #.(code-char 40) #.(code-char #xd7ff))
	  (range #.(code-char #xe000) #.(code-char #xfffd))
	  (range #.(code-char #x10000) #.(code-char #x10ffff))))
     (string-char (or normal-char space))
     (non-space
      (or #.(code-char 34) #.(code-char 39) normal-char))
     (char
      (or space non-space))
     (newline
      (or #.(code-char 10) #.(code-char 13))))
  ((* space))

  (#\# (clex:begin 'comment))
  ((clex::in comment newline) (clex:begin 'clex:initial))
  ((clex::in comment char))

  ((and "'''" (* (or string-char #\")) "'''")
   (return (subseq clex:bag 3 (- (length clex:bag) 3))))

  ((and #\' (* (or string-char #\")) #\')
   (when (or (find (code-char 13) clex:bag)
	     (find (code-char 10) clex:bag))
     (rng-error "disallowed newline in string literal"))
   (return
     (subseq clex:bag 1 (- (length clex:bag) 1))))
	   
  ((and #\" #\" #\" (* (or string-char #\")) #\" #\" #\")
   (return (subseq clex:bag 3 (- (length clex:bag) 3))))

  ((and #\" (* (or string-char #\')) #\")
   (when (or (find (code-char 13) clex:bag)
	     (find (code-char 10) clex:bag))
     (rng-error "disallowed newline in string literal"))
   (return
     (subseq clex:bag 1 (- (length clex:bag) 1))))
	   
  ((* non-space)
   (return
     (if (find clex:bag *keywords* :test #'equal)
	 (intern (string-upcase clex:bag) :keyword)
	 clex:bag))))

(defun compact (&optional (p #"/home/david/src/lisp/cxml-rng/test.rnc"))
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
(compact "\"foo'")
