(in-package :cxml-rng)


(defun parse-relax-ng (input)
  (klacks:with-open-source (source (cxml:make-source input))
    (klacks:find-event source :start-element)
    (p/pattern source)))


;;;; pattern structures

(defstruct pattern
  ns
  dl)

(defstruct (%combination (:include pattern) (:conc-name "PATTERN-"))
  possibilities)

(defstruct (%named-pattern (:include pattern) (:conc-name "PATTERN-"))
  name)

(defstruct (element (:include %named-pattern) (:conc-name "PATTERN-"))
  children)

(defstruct (attribute (:include %named-pattern) (:conc-name "PATTERN-"))
  child)

(defstruct (group (:include %combination) (:conc-name "PATTERN-")))
(defstruct (interleave (:include %combination) (:conc-name "PATTERN-")))
(defstruct (choice (:include %combination) (:conc-name "PATTERN-")))
(defstruct (optional (:include %combination) (:conc-name "PATTERN-")))
(defstruct (zero-or-more (:include %combination) (:conc-name "PATTERN-")))
(defstruct (one-or-more (:include %combination) (:conc-name "PATTERN-")))
(defstruct (list-pattern (:include %combination) (:conc-name "PATTERN-")))
(defstruct (mixed (:include %combination) (:conc-name "PATTERN-")))

(defstruct (ref (:include %named-pattern) (:conc-name "PATTERN-")))

(defstruct (parent-ref (:include %named-pattern) (:conc-name "PATTERN-")))

(defstruct (empty (:include pattern) (:conc-name "PATTERN-")))
(defstruct (text (:include pattern) (:conc-name "PATTERN-")))

(defstruct (value (:include pattern) (:conc-name "PATTERN-"))
  string)

(defstruct (data (:include pattern) (:conc-name "PATTERN-"))
  type
  params
  except)

(defstruct (not-allowed (:include pattern) (:conc-name "PATTERN-")))

(defstruct (external-ref (:include pattern) (:conc-name "PATTERN-"))
  href)

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
  children)

(defstruct div
  content)

(defstruct include
  href
  content)


;;;; parser

(defvar *rng-namespace* "http://relaxng.org/ns/structure/1.0")

(defun skip-foreign (source)
  (print (klacks:current-lname source))
  (assert (not (equal (klacks:current-uri source) *rng-namespace*)))
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

(defun ntc (lname attrs)
  ;; used for (n)ame, (t)ype, and (c)ombine, this also strings whitespace
  (let ((a (sax:find-attribute-ns "" lname attrs)))
    (if a
	(string-trim *whitespace* (sax:attribute-value a))
	nil)))

(defun p/pattern (source)
  (let* ((lname (klacks:current-lname source))
	 (attrs (klacks:list-attributes source))
	 (ns (attribute "ns" attrs))
	 (dl (attribute "datatypeLibrary" attrs)))
    (when dl
      (setf dl (escape-uri dl)))
    (case (find-symbol lname :keyword)
      (:|element|     (p/element source (ntc "name" attrs) ns dl))
      (:|attribute|   (p/attribute source (ntc "name" attrs) ns dl))
      (:|group|       (p/combination #'make-group source ns dl))
      (:|interleave|  (p/combination #'make-interleave source ns dl))
      (:|choice|      (p/combination #'make-choice source ns dl))
      (:|optional|    (p/combination #'make-optional source ns dl))
      (:|zeroOrMore|  (p/combination #'make-zero-or-more source ns dl))
      (:|oneOrMore|   (p/combination #'make-one-or-more source ns dl))
      (:|list|        (p/combination #'make-list-pattern source ns dl))
      (:|mixed|       (p/combination #'make-mixed source ns dl))
      (:|ref|         (p/ref source ns dl))
      (:|parentRef|   (p/parent-ref source ns dl))
      (:|empty|       (p/empty source ns dl))
      (:|text|        (p/text source ns dl))
      (:|value|       (p/value source ns dl))
      (:|data|        (p/data source ns dl))
      (:|externalRef| (p/external-ref source ns dl))
      (:|grammar|     (p/grammar source ns dl))
      (t (skip-foreign source)))))

(defun p/pattern+ (source)
  (let ((children nil))
    (loop
      (case (klacks:peek-next source)
	(:start-element
	  (let ((p (p/pattern source))) (when p (push p children))))
	(:end-element (return))))
    (unless children
      (error "empty element"))
    (nreverse children)))

(defun p/pattern? (source)
  (loop
    (case (klacks:peek-next source)
      (:start-element (return (p/pattern source)))
      (:end-element (return)))))

(defun p/element (source name ns dl)
  (klacks:expecting-element (source "element")
    (let ((result (make-element :ns ns)))
      (if name
	  (setf (pattern-name result) (list :name name))
	  (setf (pattern-name result) (p/name-class source)))
      (setf (pattern-children result) (p/pattern+ source))
      result)))

(defun p/attribute (source name ns dl)
  (klacks:expecting-element (source "attribute")
    (let ((result (make-attribute :ns ns)))
      (if name
	  (setf (pattern-name result) (list :name name))
	  (setf (pattern-name result) (p/name-class source)))
      (setf (pattern-child result) (p/pattern? source))
      result)))

(defun p/combination (constructor source ns dl)
  (klacks:expecting-element (source)
    (let ((possibility (p/pattern+ source)))
      (funcall constructor :possibility possibility :ns ns))))

(defun p/ref (source ns dl)
  (klacks:expecting-element (source "ref")
    (make-ref :name (ntc "name" (klacks:list-attributes source))
	      :ns ns)))

(defun p/parent-ref (source ns dl)
  (klacks:expecting-element (source "parentRef")
    (make-parent-ref :name (ntc "name" (klacks:list-attributes source))
		     :ns ns)))

(defun p/empty (source ns dl)
  (klacks:expecting-element (source "empty")
    (klacks:consume source)
    (make-empty :ns ns)))

(defun p/text (source ns dl)
  (klacks:expecting-element (source "text")
    (klacks:consume source)
    (make-text :ns ns)))

(defun parse-characters (source)
  ;; fixme
  (let ((tmp ""))
    (loop
      (multiple-value-bind (key data) (klacks:peek-next source)
	(case key
	  (:characters
	    (setf tmp (concatenate 'string tmp data)))
	  (:end-element (return)))))
    tmp))

(defun p/value (source ns dl)
  (klacks:expecting-element (source "value")
    (let* ((type (ntc "type" (klacks:list-attributes source)))
	   (string (parse-characters source)))
      (make-value :string string :type type :ns ns :dl dl))))

(defun p/data (source ns dl)
  (klacks:expecting-element (source "data")
    (let* ((type (ntc "type" (klacks:list-attributes source)))
	   (result (make-data :type type :ns ns :dl dl))
	   (params '()))
      (loop
	(multiple-value-bind (key lname)
	    (klacks:peek-next source)
	  (case key
	    (:start-element
	      (case (find-symbol lname :keyword)
		(:|param| (push (p/param source) params))
		(:|except|
		  (setf (pattern-except result) (p/except-pattern source))
		  (return))
		(t (skip-foreign source))))
	    (:end-element
	      (return)))))
      (setf (pattern-params result) (nreverse params))
      result)))

(defun p/param (source)
  (klacks:expecting-element (source "param")
    (let ((name (ntc "name" (klacks:list-attributes source)))
	  (string (parse-characters source)))
      (make-param :name name :string string))))

(defun p/except-pattern (source)
  (klacks:expecting-element (source "except")
    (p/pattern+ source)))

(defun p/not-allowed (source ns dl)
  (klacks:expecting-element (source "notAllowed")
    (make-not-allowed :ns ns)))

(defun p/external-ref (source ns dl)
  (klacks:expecting-element (source "externalRef")
    (make-external-ref
     :href (attribute "href" (klacks:list-attributes source))
     :ns ns)))

(defun p/grammar (source ns dl)
  (klacks:expecting-element (source "grammar")
    (make-grammar :content (p/grammar-content* source) :ns ns)))

(defun p/grammar-content* (source &key disallow-include)
  (let ((content nil))
    (loop
      (multiple-value-bind (key lname) (klacks:peek-next source)
	(case key
	  (:start-element
	    (case (find-symbol lname :keyword)
	      (:|start| (push (p/start source) content))
	      (:|define| (push (p/define source) content))
	      (:|div| (push (p/div source) content))
	      (:|include|
		(when disallow-include
		  (error "nested include not permitted"))
		(push (p/include source) content))
	      (t (skip-foreign source))))
	  (:end-element (return)))))
    (nreverse content)))

(defun p/start (source)
  (klacks:expecting-element (source "start")
    (let ((combine (ntc "combine" source))
	  (child (p/pattern source)))
      (make-start :combine (find-symbol (string-upcase combine) :keyword)
		  :child child))))

(defun p/define (source)
  (klacks:expecting-element (source "define")
    (let ((name (ntc "name" source))
	  (combine (ntc "combine" source))
	  (children (p/pattern+ source)))
      (make-define :name name
		   :combine (find-symbol (string-upcase combine) :keyword)
		   :children children))))

(defun p/div (source)
  (klacks:expecting-element (source "div")
    (make-div :content (p/grammar-content* source))))

(defun p/include (source)
  (klacks:expecting-element (source "div")
    (let ((href (attribute "href" source))
	  (content (p/grammar-content* source :disallow-include t)))
      (make-include :href href :content content))))

(defun p/name-class (source)
  (klacks:expecting-element (source)
    (case (find-symbol (klacks:current-lname source) :keyword)
      (:|name|
	(list :name (string-trim *whitespace* (parse-characters source))))
      (:|anyName|
	(cons :any (p/except-name-class? source)))
      (:|nsName|
	(cons :ns (p/except-name-class? source)))
      (:|choice|
	(cons :choice (p/name-class* source)))
      (t
	(skip-foreign source)))))

(defun p/name-class* (source)
  (let ((results nil))
    (loop
      (case (klacks:peek-next source)
	(:start-element (push (p/name-class source) results))
	(:end-element (return))))
    (nreverse results)))

(defun p/except-name-class? (source)
  (loop
    (multiple-value-bind (key lname)
	(klacks:peek-next source)
      (unless (eq key :start-element)
	(return))
      (when (string= (find-symbol lname :keyword) "except")
	(return (p/except-name-class source)))
      (skip-foreign source))))

(defun p/except-name-class (source)
  (klacks:expecting-element (source "except")
    (cons :except (p/name-class source))))

(defun escape-uri (string)
  (with-output-to-string (out)
    (loop for c across (cxml::rod-to-utf8-string string) do
	  (let ((code (char-code c)))
	    ;; http://www.w3.org/TR/xlink/#link-locators
	    (if (or (>= code 127) ;; 127 itself is forbidden, too
		    (<= code 32)  ;; space is forbidden
		    (find c "<>\"{}|\\^`"))
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
;;;
;;; Escaping is done by p/pattern.
