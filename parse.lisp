(in-package :cxml-rng)

#+sbcl
(declaim (optimize (debug 2)))


;;;; Errors

(define-condition rng-error (simple-error) ())

(defun rng-error (source fmt &rest args)
  (let ((s (make-string-output-stream)))
    (apply #'format s fmt args)
    (when source
      (format s "~&  [ Error at line ~D, column ~D in ~S ]"
	      (klacks:current-line-number source)
	      (klacks:current-column-number source)
	      (klacks:current-system-id source)))
    (error 'rng-error
	   :format-control "~A"
	   :format-arguments (list (get-output-stream-string s)))))


;;;; Parser

(defvar *datatype-library*)
(defvar *namespace-uri*)
(defvar *entity-resolver*)
(defvar *external-href-stack*)
(defvar *include-uri-stack*)

(defvar *debug* nil)

(defun invoke-with-klacks-handler (fn source)
  (if *debug*
      (funcall fn)
      (handler-case
	  (funcall fn)
	(cxml:xml-parse-error (c)
	  (rng-error source "Cannot parse schema: ~A" c)))))

(defun parse-relax-ng (input &key entity-resolver)
  (klacks:with-open-source (source (cxml:make-source input))
    (invoke-with-klacks-handler
     (lambda ()
       (klacks:find-event source :start-element)
       (let ((*datatype-library* "")
	     (*namespace-uri* "")
	     (*entity-resolver* entity-resolver)
	     (*external-href-stack* '())
	     (*include-uri-stack* '()))
	 (p/pattern source)))
      source)))


;;;; pattern structures

(defstruct pattern)

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

(defstruct (one-or-more
	    (:include %parent)
	    (:constructor make-one-or-more (child))))
(defstruct (list-pattern
	    (:include %parent)
	    (:constructor make-list-pattern (child))))

(defstruct (%ref (:include pattern) (:conc-name "PATTERN-"))
  ref-name)
(defstruct (ref (:include %ref) (:conc-name "PATTERN-")))
(defstruct (parent-ref (:include %ref) (:conc-name "PATTERN-")))

(defstruct (empty (:include pattern) (:conc-name "PATTERN-")))
(defstruct (text (:include pattern) (:conc-name "PATTERN-")))

(defstruct (%typed-pattern (:include pattern) (:conc-name "PATTERN-"))
  datatype-library
  type)

(defstruct (value (:include %typed-pattern) (:conc-name "PATTERN-"))
  ns
  string)

(defstruct (data (:include %typed-pattern) (:conc-name "PATTERN-"))
  params
  except)

(defstruct (not-allowed (:include pattern) (:conc-name "PATTERN-")))

(defstruct (grammar (:include pattern) (:conc-name "PATTERN-"))
  content)


;;;; non-pattern

(defstruct param
  name
  string)

(defstruct start
  combine
  child)

(defstruct define
  name
  combine
  child)


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

(defvar *whitespace*
    (format nil "~C~C~C"
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
	 (*namespace-uri* (or ns *namespace-uri*)))
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
    (let ((result (make-element)))
      (consume-and-skip-to-native source)
      (if name
	  (setf (pattern-name result)
		(list :name name :uri *namespace-uri*))
	  (setf (pattern-name result) (p/name-class source)))
      (skip-to-native source)
      (setf (pattern-child result) (groupify (p/pattern+ source)))
      result)))

(defvar *attribute-namespace-p* nil)

(defun p/attribute (source name)
  (klacks:expecting-element (source "attribute")
    (let ((result (make-attribute)))
      (consume-and-skip-to-native source)
      (if name
	  (setf (pattern-name result)
		(list :name name :uri ""))
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
	(make-ref :ref-name (ntc "name" source))
      (skip-foreign* source))))

(defun p/parent-ref (source)
  (klacks:expecting-element (source "parentRef")
    (prog1
	(make-parent-ref :ref-name (ntc "name" source))
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
      (make-value :string string :type type :ns ns :datatype-library dl))))

(defun p/data (source)
  (klacks:expecting-element (source "data")
    (let* ((type (ntc "type" source))
	   (result (make-data :type type
			      :datatype-library *datatype-library*
			     ))
	   (params '()))
      (loop
	(multiple-value-bind (key uri lname)
	    (klacks:peek-next source)
	  uri
	  (case key
	    (:start-element
	      (case (find-symbol lname :keyword)
		(:|param| (push (p/param source) params))
		(:|except|
		  (setf (pattern-except result) (p/except-pattern source))
		  (skip-to-native source)
		  (return))
		(t (skip-foreign source))))
	    (:end-element
	      (return)))))
      (setf (pattern-params result) (nreverse params))
      result)))

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
	    (klacks:with-open-source (source (cxml:make-source xstream))
	      (invoke-with-klacks-handler
	       (lambda ()
		 (klacks:find-event source :start-element)
		 (let ((*datatype-library* ""))
		   (p/pattern source)))
	       source)))
	(skip-foreign* source)))))

(defun p/grammar (source)
  (klacks:expecting-element (source "grammar")
    (consume-and-skip-to-native source)
    (make-grammar :content (p/grammar-content* source))))

(defun p/grammar-content* (source &key disallow-include)
  (loop
      append
	(prog1
	    (multiple-value-bind (key uri lname) (klacks:peek source)
	      uri
	      (case key
		(:start-element
		  (with-library-and-ns (klacks:list-attributes source)
		    (case (find-symbol lname :keyword)
		      (:|start| (list (p/start source)))
		      (:|define| (list (p/define source)))
		      (:|div| (p/div source))
		      (:|include|
			(when disallow-include
			  (rng-error source "nested include not permitted"))
			(p/include source))
		      (t
			(skip-foreign source)
			nil))))
		(:end-element
		  (loop-finish))))
	  (klacks:consume source))))

(defun p/start (source)
  (klacks:expecting-element (source "start")
    (let ((combine (ntc "combine" source))
	  (child
	   (progn
	     (consume-and-skip-to-native source)
	     (p/pattern source))))
      (skip-foreign* source)
      (make-start :combine (find-symbol (string-upcase combine) :keyword)
		  :child child))))

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

(defun p/define (source)
  (klacks:expecting-element (source "define")
    (let ((name (ntc "name" source))
	  (combine (ntc "combine" source))
	  (children (progn
		      (consume-and-skip-to-native source)
		      (p/pattern+ source))))
      (make-define :name name
		   :combine (find-symbol (string-upcase combine) :keyword)
		   :child (groupify children)))))

(defun p/div (source)
  (klacks:expecting-element (source "div")
    (consume-and-skip-to-native source)
    (p/grammar-content* source)))

(defun p/include (source)
  (klacks:expecting-element (source "include")
    (let* ((href
	    (escape-uri (attribute "href" (klacks:list-attributes source))))
	   (base (klacks:current-xml-base source))
	   (uri (safe-parse-uri source href base))
	   (include-content
	    (progn
	      (consume-and-skip-to-native source)
	      (p/grammar-content* source :disallow-include t))))
      (when (find uri *include-uri-stack* :test #'puri:uri=)
	(rng-error source "looping include"))
      (let* ((*include-uri-stack* (cons uri *include-uri-stack*))
	     (xstream (cxml::xstream-open-extid* *entity-resolver* nil uri))
	     (grammar
	      (klacks:with-open-source (source (cxml:make-source xstream))
		(invoke-with-klacks-handler
		 (lambda ()
		   (klacks:find-event source :start-element)
		   (let ((*datatype-library* ""))
		     (p/grammar source)))
		 source)))
	     (grammar-content (pattern-content grammar)))
	(append
	 (simplify-include source grammar-content include-content)
	 include-content)))))

(defun simplify-include/map (fn l)
  (remove nil (mapcar fn l)))

(defun simplify-include/start (source grammar-content include-content)
  (let ((startp (some (lambda (x) (typep x 'start)) include-content)))
    (if startp
	(let ((ok nil))
	  (prog1
	      (remove-if (lambda (x)
			   (when (typep x 'start)
			     (setf ok t)
			     t))
			 grammar-content)
	    (unless ok
	      (rng-error source "expected start in grammar"))))
	grammar-content)))

(defun simplify-include/define (source grammar-content include-content)
  (let ((defines '()))
    (dolist (x include-content)
      (when (typep x 'define)
	(push (cons x nil) defines)))
    (prog1
	(remove-if (lambda (x)
		     (when (typep x 'define)
		       (let ((cons (find (define-name x)
					 defines
					 :key (lambda (y)
						(define-name (car y)))
					 :test #'equal)))
			 (when cons
			   (setf (cdr cons) t)
			   t))))
		   grammar-content)
      (loop for (define . okp) in defines do
	    (unless okp
	      (rng-error source "expected matching ~A in grammar" define))))))

(defun simplify-include (source grammar-content include-content)
  (simplify-include/define
   source
   (simplify-include/start source grammar-content include-content)
   include-content))

(defvar *any-name-allowed-p* t)
(defvar *ns-name-allowed-p* t)

(defun p/name-class (source)
  (klacks:expecting-element (source)
    (with-library-and-ns (klacks:list-attributes source)
      (case (find-symbol (klacks:current-lname source) :keyword)
	(:|name|
	  (let ((qname (string-trim *whitespace*
				    (consume-and-parse-characters source))))
	    (multiple-value-bind (uri lname)
		(klacks:decode-qname qname source)
	      (setf uri (or uri *namespace-uri*))
	      (when (and *attribute-namespace-p*
			 (or (and (equal lname "xmlns") (equal uri ""))
			     (equal uri "http://www.w3.org/2000/xmlns")))
		(rng-error source "namespace attribute not permitted"))
	      (list :name lname :uri uri))))
	(:|anyName|
	  (unless *any-name-allowed-p*
	    (rng-error source "anyname now permitted in except"))
	  (klacks:consume source)
	  (prog1
	      (let ((*any-name-allowed-p* nil))
		(cons :any (p/except-name-class? source)))
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
		(list :nsname (p/except-name-class? source) :uri uri)
	      (skip-to-native source))))
	(:|choice|
	  (klacks:consume source)
	  (cons :choice (p/name-class* source)))
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
      (cons :except (p/name-class* source)))))

(defun escape-uri (string)
  (with-output-to-string (out)
    (loop for c across (cxml::rod-to-utf8-string string) do
	  (let ((code (char-code c)))
	    ;; http://www.w3.org/TR/xlink/#link-locators
	    (if (or (>= code 127) (<= code 32) (find c "<>\"{}|\\^`"))
		(format out "%~2,'0X" code)
		(write-char c out))))))


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
;;;   Escaping is done by p/include and p/external-ref.
;;;
;;;   FIXME: Mime-type handling should be the job of the entity resolver,
;;;   but that requires xstream hacking.

;;; 4.6. externalRef element
;;;   Done by p/external-ref.

;;; 4.7. include element
;;;   Done by p/include.

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

;;;; tests

(defun run-tests (&optional (p "/home/david/src/lisp/cxml-rng/spec-split/*"))
  (dribble "/home/david/src/lisp/cxml-rng/TEST" :if-exists :rename-and-delete)
  (let ((pass 0)
	(total 0)
	(*package* (find-package :cxml-rng)))
    (dolist (d (directory p))
      (let ((name (car (last (pathname-directory d)))))
	(when (parse-integer name :junk-allowed t)
	  (incf total)
	  (when (test1 d)
	    (incf pass)))))
    (format t "Passed ~D/~D tests.~%" pass total))
  (dribble))

(defun run-test (n &optional (p "/home/david/src/lisp/cxml-rng/spec-split/"))
  (test1 (merge-pathnames (format nil "~3,'0D/" n) p)))

(defun parse-test (n &optional (p "/home/david/src/lisp/cxml-rng/spec-split/"))
  (let* ((*debug* t)
	 (d (merge-pathnames (format nil "~3,'0D/" n) p))
	 (i (merge-pathnames "i.rng" d))
	 (c (merge-pathnames "c.rng" d))
	 (rng (if (probe-file c) c i)))
    (format t "~A: " (car (last (pathname-directory d))))
    (print rng)
    (parse-relax-ng rng)))

(defun test1 (d)
  (let* ((i (merge-pathnames "i.rng" d))
	 (c (merge-pathnames "c.rng" d)))
    (format t "~A: " (car (last (pathname-directory d))))
    (if (probe-file c)
	(handler-case
	    (progn
	      (parse-relax-ng c)
	      (format t " PASS~%")
	      t)
	  (error (c)
	    (format t " FAIL: ~A~%" c)
	    nil))
	(handler-case
	    (progn
	      (parse-relax-ng i)
	      (format t " FAIL: didn't detect invalid schema~%")
	      nil)
	  (rng-error (c)
	    (format t " PASS: ~S~%" (type-of c))
	    t)
	  (error (c)
	    (format t " FAIL: incorrect condition type: ~A~%" c)
	    nil)))))
