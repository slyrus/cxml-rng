;;; -*- show-trailing-whitespace: t; indent-tabs: nil -*-
;;;
;;; An implementation of James Clark's algorithm for RELAX NG validation.
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

(defvar *empty* (make-empty))
(defvar *not-allowed* (make-not-allowed))


(defun make-validator (grammar)
  (let* ((table (ensure-registratur grammar))
	 (start (schema-interned-start grammar))
	 (validator
	  (make-instance 'validator
			 :registratur table
			 :current-pattern start)))
    (make-instance 'text-normalizer :chained-handler validator)))


;;;; CONTAINS

(defgeneric contains (nc uri lname))

(defmethod contains ((nc any-name) uri lname)
  (let ((except (any-name-except nc)))
    (if except
        (not (contains except uri lname))
        t)))

(defmethod contains ((nc ns-name) uri lname)
  (and (equal (ns-name-uri nc) uri)
       (let ((except (ns-name-except nc)))
         (if except
             (not (contains except uri lname))
             t))))

(defmethod contains ((nc name) uri lname)
  (and (equal (name-uri nc) uri)
       (equal (name-lname nc) lname)))

(defmethod contains ((nc name-class-choice) uri lname)
  (or (contains (name-class-choice-a nc) uri lname)
      (contains (name-class-choice-b nc) uri lname)))


;;;; NULLABLE

(defgeneric nullable (pattern))

(defmethod nullable ((pattern group))
  (and (nullable (pattern-a pattern))
       (nullable (pattern-b pattern))))

(defmethod nullable  ((pattern interleave))
  (and (nullable (pattern-a pattern))
       (nullable (pattern-b pattern))))

(defmethod nullable ((pattern choice))
  (or (nullable (pattern-a pattern))
      (nullable (pattern-b pattern))))

(defmethod nullable ((pattern one-or-more))
  (nullable (pattern-child pattern)))

(defmethod nullable ((pattern element)) nil)
(defmethod nullable ((pattern attribute)) nil)
(defmethod nullable ((pattern list-pattern)) nil)
(defmethod nullable ((pattern value)) nil)
(defmethod nullable ((pattern data)) nil)
(defmethod nullable ((pattern not-allowed)) nil)
(defmethod nullable ((pattern after)) nil)

(defmethod nullable ((pattern empty)) t)
(defmethod nullable ((pattern text)) t)


;;;; VALIDATOR

(defclass validator (sax:sax-parser-mixin
		     cxml-types:sax-validation-context-mixin)
  ((current-pattern :initarg :current-pattern :accessor current-pattern)
   (after-start-tag-p :accessor after-start-tag-p)
   (pending-text-node :initform nil :accessor pending-text-node)
   (registratur :initarg :registratur :accessor registratur)))

(defun advance (hsx pattern message)
  (when (typep pattern 'not-allowed)
    (rng-error hsx "~A, was expecting a ~A"
	       message
	       (replace-scary-characters (current-pattern hsx))))
  (setf (current-pattern hsx) pattern))

;; make sure slime doesn't die
(defun replace-scary-characters (pattern)
  (let ((str (write-to-string pattern
			      :circle t
			      :escape nil
			      :pretty nil)))
    (loop
       for c across str
       for i from 0
       when (>= (char-code c) 128)
       do (setf (elt str i) #\?))
    str))

(defmethod sax:characters ((hsx validator) data)
  (assert (null (pending-text-node hsx))) ;parser must be normalize
  (if (after-start-tag-p hsx)
      (setf (pending-text-node hsx) data)
      (unless (whitespacep data)
	;; we already saw an element sibling, so discard whitespace
	(advance hsx
		 (text\' hsx (current-pattern hsx) data)
		 "text node not valid")))
  (setf (after-start-tag-p hsx) nil))

(defmethod sax:start-element ((hsx validator) uri lname qname attributes)
  (declare (ignore qname))
  (when (pending-text-node hsx)
    ;; text node was the previous child, and we're in element content.
    ;; process non-whitespace now; discard whitespace completely
    (let ((data (pending-text-node hsx)))
      (unless (whitespacep data)
	(advance hsx
		 (text\' hsx (current-pattern hsx) data)
		 "text node")))
    (setf (pending-text-node hsx) nil))
  (setf attributes
	(remove-if (cxml::compose #'cxml::xmlns-attr-p #'sax:attribute-qname)
		   attributes))
  (let* ((p0 (current-pattern hsx))
	 (p1 (open-start-tag\' hsx p0 uri lname))
	 (p2 (progn
	       (advance hsx p1 "element not valid")
	       (attributes\' hsx p1 attributes)))
	 (p3 (progn
	       (advance hsx p2 "attributes not valid")
	       (close-start-tag\' hsx p2))))
    (advance hsx p3 "attributes not valid")
    (setf (after-start-tag-p hsx) t)))

(defmethod sax:end-element ((hsx validator) uri lname qname)
  (declare (ignore uri lname qname))
  (when (after-start-tag-p hsx)
    ;; nothing at all?  pretend we saw whitespace.
    (sax:characters hsx ""))
  (when (pending-text-node hsx)
    ;; text node was the only child?
    ;; process it and handle whitespace specially
    (let* ((current (current-pattern hsx))
	   (data (pending-text-node hsx))
	   (next (text\' hsx current data)))
      (advance hsx
	       (if (whitespacep data)
		   (intern-choice hsx current next)
		   next)
	       "text node not valid"))
    (setf (pending-text-node hsx) nil))
  (advance hsx
	   (end-tag\' hsx (current-pattern hsx))
	   "end of element not valid"))


;;;; TEXT'

(defgeneric text\' (handler pattern data))

(defmethod text\' (hsx (pattern choice) data)
  (intern-choice hsx
		 (text\' hsx (pattern-a pattern) data)
                 (text\' hsx (pattern-b pattern) data)))

(defmethod text\' (hsx (pattern interleave) data)
  (let ((a (pattern-a pattern))
        (b (pattern-b pattern)))
    (intern-choice hsx
                   (intern-interleave hsx (text\' hsx a data) b)
                   (intern-interleave hsx a (text\' hsx b data)))))

(defmethod text\' (hsx (pattern group) data)
  (let* ((a (pattern-a pattern))
         (b (pattern-b pattern))
         (p (intern-group hsx (text\' hsx a data) b)))
    (if (nullable a)
        (intern-choice hsx p (text\' hsx b data))
        p)))

(defmethod text\' (hsx (pattern after) data)
  (intern-after hsx
                (text\' hsx (pattern-a pattern) data)
                (pattern-b pattern)))

(defmethod text\' (hsx (pattern one-or-more) data)
  (let ((child (pattern-child pattern)))
    (intern-group hsx
                  (text\' hsx child data)
		  (intern-zero-or-more hsx child))))

(defmethod text\' (hsx (pattern text) data)
  (declare (ignore data))
  pattern)

(defun eat (ok)
  (if ok *empty* *not-allowed*))

(defmethod text\' (hsx (pattern value) data)
  (let ((data-type (pattern-type pattern)))
    (eat (cxml-types:equal-using-type
	  data-type
	  (pattern-value pattern)
	  (cxml-types:parse data-type data hsx)))))

(defmethod text\' (hsx (pattern data) data)
  (eat (and (cxml-types:validp (pattern-type pattern) data hsx)
	    (let ((except (pattern-except pattern)))
	      (not (and except (nullable (text\' hsx except data))))))))

(defmethod text\' (hsx (pattern list-pattern) data)
  (eat (nullable (list\' hsx (pattern-child pattern) (words data)))))

(defmethod text\' (hsx pattern data)
  (declare (ignore pattern data))
  *not-allowed*)

(defun list\' (hsx pattern words)
  (dolist (word words)
    (setf pattern (text\' hsx pattern word)))
  pattern)

(defun words (str)
  (cl-ppcre:split #.(format nil "[~A]+" *whitespace*)
		  (string-trim *whitespace* str)))


;;;; INTERN

(defmacro ensuref (key table value)
  `(ensure-hash ,key ,table (lambda () ,value)))

(defun ensure-hash (key table fn)
  (or (gethash key table)
      (setf (gethash key table) (funcall fn))))

(defgeneric intern-choice (handler a b))
(defmethod intern-choice (hsx a (b not-allowed)) a)
(defmethod intern-choice (hsx (a not-allowed) b) b)
(defmethod intern-choice (hsx a b)
  (ensuref (list 'choice a b) (registratur hsx) (make-choice a b)))

(defgeneric intern-group (handler a b))
(defmethod intern-group (hsx (a pattern) (b not-allowed)) b)
(defmethod intern-group (hsx (a not-allowed) (b pattern)) a)
(defmethod intern-group (hsx a (b empty)) a)
(defmethod intern-group (hsx (a empty) b) b)
(defmethod intern-group (hsx a b)
  (ensuref (list 'group a b) (registratur hsx) (make-group a b)))

(defgeneric intern-interleave (handler a b))
(defmethod intern-interleave (hsx (a pattern) (b not-allowed)) b)
(defmethod intern-interleave (hsx (a not-allowed) (b pattern)) a)
(defmethod intern-interleave (hsx a (b empty)) a)
(defmethod intern-interleave (hsx (a empty) b) b)
(defmethod intern-interleave (hsx a b)
  (ensuref (list 'interleave a b) (registratur hsx) (make-interleave a b)))

(defgeneric intern-after (handler a b))
(defmethod intern-after (hsx (a pattern) (b not-allowed)) b)
(defmethod intern-after (hsx (a not-allowed) (b pattern)) a)
(defmethod intern-after (hsx a b)
  (ensuref (list 'after a b) (registratur hsx) (make-after a b)))

(defgeneric intern-one-or-more (handler c))
(defmethod intern-one-or-more (hsx (c not-allowed)) c)
(defmethod intern-one-or-more (hsx c)
  (ensuref (list 'one-or-more c) (registratur hsx) (make-one-or-more c)))


;;;; ENSURE-REGISTRATUR

(defvar *seen-elements*)

(defun ensure-registratur (grammar)
  (or (schema-registratur grammar)
      (setf (schema-registratur grammar)
	    (let ((table (make-hash-table :test 'equal))
		  (*seen-elements* '())
		  (done-elements '()))
	      (setf (schema-interned-start grammar)
		    (intern-pattern (schema-start grammar) table))
	      (loop
		 for elements = *seen-elements*
		 while elements do
		   (setf *seen-elements* nil)
		   (dolist (pattern elements)
		     (unless (find pattern done-elements)
		       (push pattern done-elements)
		       (setf (pattern-child pattern)
			     (intern-pattern (pattern-child pattern) table)))))
	      table))))

;;; FIXME: misnamed.  we don't really intern the originals pattern yet.

(defgeneric intern-pattern (pattern table))

(defmethod intern-pattern ((pattern element) table)
  (pushnew pattern *seen-elements*)
  pattern)

(defmethod intern-pattern ((pattern %parent) table)
  (let ((c (intern-pattern (pattern-child pattern) table)))
    (if (eq c (pattern-child pattern))
	pattern
	(let ((copy (copy-structure pattern)))
	  (setf (pattern-child copy) c)
	  copy))))

(defmethod intern-pattern ((pattern %combination) table)
  (let ((a (intern-pattern (pattern-a pattern) table))
	(b (intern-pattern (pattern-b pattern) table)))
    (if (and (eq a (pattern-a pattern)) (eq b (pattern-b pattern)))
	pattern
	(let ((copy (copy-structure pattern)))
	  (setf (pattern-a copy) a)
	  (setf (pattern-b copy) b)
	  copy))))

(defmethod intern-pattern ((pattern data) table)
  (let ((e (when (pattern-except pattern)
	     (intern-pattern (pattern-except pattern) table))))
    (if (eq e (pattern-except pattern))
	pattern
	(let ((copy (copy-structure pattern)))
	  (setf (pattern-except copy) e)
	  copy))))

(defmethod intern-pattern ((pattern ref) table)
  (intern-pattern (defn-child (pattern-target pattern)) table))

(defmethod intern-pattern ((pattern empty) table)
  *empty*)

(defmethod intern-pattern ((pattern not-allowed) table)
  *not-allowed*)

(defmethod intern-pattern ((pattern %leaf) table)
  pattern)


;;;; APPLY-AFTER

(defgeneric apply-after (handler fn pattern))

(defmethod apply-after (hsx fn (pattern after))
  (intern-after hsx
                (pattern-a pattern)
                (funcall fn (pattern-b pattern))))

(defmethod apply-after (hsx fn (pattern choice))
  (intern-choice hsx
                 (apply-after hsx fn (pattern-a pattern))
                 (apply-after hsx fn (pattern-b pattern))))

(defmethod apply-after (hsx fn (pattern not-allowed))
  (declare (ignore hsx fn))
  pattern)


;;;; OPEN-START-TAG'

(defgeneric open-start-tag\' (handler pattern uri lname))

(defmethod open-start-tag\' (hsx (pattern choice) uri lname)
  (intern-choice hsx
                 (open-start-tag\' hsx (pattern-a pattern) uri lname)
                 (open-start-tag\' hsx (pattern-b pattern) uri lname)))

(defmethod open-start-tag\' (hsx (pattern element) uri lname)
  (if (contains (pattern-name pattern) (or uri "") lname)
      (intern-after hsx (pattern-child pattern) *empty*)
      *not-allowed*))

(defmethod open-start-tag\' (hsx (pattern interleave) uri lname)
  (intern-choice hsx
                 (apply-after
                  hsx
                  (lambda (p) (intern-interleave hsx p (pattern-b pattern)))
                  (open-start-tag\' hsx (pattern-a pattern) uri lname))
                 (apply-after
                  hsx
                  (lambda (p) (intern-interleave hsx (pattern-a pattern) p))
                  (open-start-tag\' hsx (pattern-b pattern) uri lname))))

(defun intern-zero-or-more (hsx c)
  (intern-choice hsx (intern-one-or-more hsx c) *empty*))

(defmethod open-start-tag\' (hsx (pattern one-or-more) uri lname)
  (let ((c (intern-zero-or-more hsx (pattern-child pattern))))
    (apply-after hsx
                 (lambda (p) (intern-group hsx p c))
                 (open-start-tag\' hsx (pattern-child pattern) uri lname))))

(defmethod open-start-tag\' (hsx (pattern group) uri lname)
  (let ((x (apply-after hsx
                        (lambda (p)
                          (intern-group hsx p (pattern-b pattern)))
                        (open-start-tag\' hsx (pattern-a pattern) uri lname))))
    (if (nullable (pattern-a pattern))
        (intern-choice hsx
                       x
                       (open-start-tag\' hsx (pattern-b pattern) uri lname))
        x)))

(defmethod open-start-tag\' (hsx (pattern after) uri lname)
  (apply-after hsx
	       (lambda (p)
		 (intern-after hsx p (pattern-b pattern)))
	       (open-start-tag\' hsx (pattern-a pattern) uri lname)))

(defmethod open-start-tag\' (hsx pattern uri lname)
  (declare (ignore hsx pattern uri lname))
  *not-allowed*)


;;;; ATTRIBUTES'

(defun attributes\' (handler pattern attributes)
  (dolist (a attributes)
    (setf pattern (attribute\' handler pattern a)))
  pattern)

(defgeneric attribute\' (handler pattern attribute))

(defmethod attribute\' (hsx (pattern after) a)
  (intern-after hsx
		(attribute\' hsx (pattern-a pattern) a)
		(pattern-b pattern)))

(defmethod attribute\' (hsx (pattern choice) a)
  (intern-choice hsx
		 (attribute\' hsx (pattern-a pattern) a)
		 (attribute\' hsx (pattern-b pattern) a)))

(defmethod attribute\' (hsx (pattern group) a)
  (intern-choice hsx
		 (intern-group hsx
			       (attribute\' hsx (pattern-a pattern) a)
			       (pattern-b pattern))
		 (intern-group hsx
			       (pattern-a pattern)
			       (attribute\' hsx (pattern-b pattern) a))))

(defmethod attribute\' (hsx (pattern interleave) a)
  (intern-choice hsx
		 (intern-interleave hsx
				    (attribute\' hsx (pattern-a pattern) a)
				    (pattern-b pattern))
		 (intern-interleave hsx
				    (pattern-a pattern)
				    (attribute\' hsx (pattern-b pattern) a))))

(defmethod attribute\' (hsx (pattern one-or-more) a)
  (intern-group hsx
		(attribute\' hsx (pattern-child pattern) a)
		(intern-zero-or-more hsx (pattern-child pattern))))

(defmethod attribute\' (hsx (pattern attribute) a)
  (eat (and (contains (pattern-name pattern)
		      (or (sax:attribute-namespace-uri a) "")
		      (sax:attribute-local-name a))
	    (value-matches-p hsx
			     (pattern-child pattern)
			     (sax:attribute-value a)))))

(defun value-matches-p (hsx pattern value)
  (or (and (nullable pattern) (whitespacep value))
      (nullable (text\' hsx pattern value))))

(defun whitespacep (str)
  (zerop (length (string-trim *whitespace* str))))

(defmethod attribute\' (hsx pattern a)
  (declare (ignore hsx pattern a))
  *not-allowed*)


;;;; CLOSE-START-TAG'

(defgeneric close-start-tag\' (handler pattern))

(defmethod close-start-tag\' (hsx (pattern after))
  (intern-after hsx
		(close-start-tag\' hsx (pattern-a pattern))
		(pattern-b pattern)))

(defmethod close-start-tag\' (hsx (pattern choice))
  (intern-choice hsx
		(close-start-tag\' hsx (pattern-a pattern))
		(close-start-tag\' hsx (pattern-b pattern))))

(defmethod close-start-tag\' (hsx (pattern group))
  (intern-group hsx
		(close-start-tag\' hsx (pattern-a pattern))
		(close-start-tag\' hsx (pattern-b pattern))))

(defmethod close-start-tag\' (hsx (pattern interleave))
  (intern-interleave hsx
		     (close-start-tag\' hsx (pattern-a pattern))
		     (close-start-tag\' hsx (pattern-b pattern))))

(defmethod close-start-tag\' (hsx (pattern one-or-more))
  (intern-one-or-more hsx (close-start-tag\' hsx (pattern-child pattern))))

(defmethod close-start-tag\' (hsx (pattern attribute))
  (declare (ignore hsx))
  *not-allowed*)

(defmethod close-start-tag\' (hsx pattern)
  (declare (ignore hsx))
  pattern)


;;;; END-TAG\'

(defgeneric end-tag\' (handler pattern))

(defmethod end-tag\' (hsx (pattern choice))
  (intern-choice hsx
		 (end-tag\' hsx (pattern-a pattern))
		 (end-tag\' hsx (pattern-b pattern))))

(defmethod end-tag\' (hsx (pattern after))
  (if (nullable (pattern-a pattern))
      (pattern-b pattern)
      *not-allowed*))

(defmethod end-tag\' (hsx pattern)
  (declare (ignore hsx pattern))
  *not-allowed*)


;;;; TEXT NORMALIZER

;;; FIXME: cxml should do that

;;; FIXME: since we ignore PI, CDATA, and comment events, we should probably
;;; discard them properly.

(defclass text-normalizer (cxml:sax-proxy)
  ((pending-text-node :initform (make-string-output-stream)
		      :accessor pending-text-node)))

(defmethod sax:characters ((handler text-normalizer) data)
  (write-string data (pending-text-node handler)))

(defun flush-pending (handler)
  (let ((str (get-output-stream-string (pending-text-node handler))))
    (unless (zerop (length str))
      (sax:characters (cxml:proxy-chained-handler handler) str))))

(defmethod sax:start-element :before
    ((handler text-normalizer) uri lname qname attributes)
  (declare (ignore uri lname qname attributes))
  (flush-pending handler))

(defmethod sax:end-element :before
    ((handler text-normalizer) uri lname qname)
  (declare (ignore uri lname qname))
  (flush-pending handler))
