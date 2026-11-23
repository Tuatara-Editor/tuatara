;;;; src/text/xml.lisp --- XML's entities and tags, read from text, and its trees walked.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.text)

(defun xml-entity (name)
  "The character the entity NAME (lt, #233, #xE9) stands for, as a string; NIL
for any other, which is left as written. A character's number is ASCII digits
alone, as XML writes it: a sign, a blank or the number 0 make it none."
  (cond ((string= name "lt") "<") ((string= name "gt") ">") ((string= name "amp") "&")
        ((string= name "quot") "\"") ((string= name "apos") "'")
        ((and (> (length name) 1) (char= (char name 0) #\#))
         (let* ((hex (char-equal (char name 1) #\x))
                (start (if hex 2 1))
                (radix (if hex 16 10))
                (code (and (< start (length name))
                           (loop for i from start below (length name)
                                 always (let ((c (char name i))) (and (< (char-code c) 128) (digit-char-p c radix))))
                           (parse-integer name :start start :radix radix))))
           (and code (< 0 code char-code-limit) (not (<= #xD800 code #xDFFF)) (string (code-char code)))))))

(defun xml-tag-end (text start)
  "Where the tag of XML TEXT beginning at START (its <) ends: the position of
its >, a > within a quoted attribute's value passed over; NIL when it has
none, the text cut short."
  (let ((quote nil))
    (loop for i from (1+ start) below (length text)
          for c = (char text i)
          do (cond (quote (when (char= c quote) (setf quote nil)))
                   ((or (char= c #\") (char= c #\')) (setf quote c))
                   ((char= c #\>) (return i))))))

(defun xml-text-value (text &optional (entity #'xml-entity))
  "TEXT of XML with its entities as the characters they stand for: what ENTITY,
a function of an entity's name, gives (XML-ENTITY unless given); one it gives
NIL for is left as written."
  (if (not (find #\& text))
      text
      (with-output-to-string (out)
        (loop with i = 0
              while (< i (length text))
              do (let ((c (char text i)))
                   (if (char= c #\&)
                       (let* ((semi (position #\; text :start i :end (min (length text) (+ i 12))))
                              (value (and semi (funcall entity (subseq text (1+ i) semi)))))
                         (if value
                             (progn (write-string value out) (setf i (1+ semi)))
                             (progn (write-char c out) (incf i))))
                       (progn (write-char c out) (incf i))))))))

;;; Walking a tree of XML, as the editor's XML reader makes it: each element
;;; (NAME ATTRIBUTES . CHILDREN), ATTRIBUTES an alist of (NAME . VALUE), its
;;; children elements and strings of text.

(defun xml-name (node)
  "The name of the element NODE, NIL for text."
  (and (consp node) (first node)))

(defun xml-attribute (node name)
  "The value of NODE's attribute NAME, or NIL."
  (cdr (assoc name (second node) :test #'string=)))

(defun xml-children (node)
  "The children of the element NODE."
  (cddr node))

(defun xml-child (node name)
  "NODE's first child element called NAME, or NIL."
  (find name (xml-children node) :key #'xml-name :test #'equal))

(defun xml-root (tree)
  "The root element of TREE, an XML document as XML-PARSE reads it, or NIL."
  (find-if #'consp (xml-children tree)))

(defun xml-unprefixed (name)
  "NAME, an element's or attribute's name, without its namespace's prefix:
sheet for x:sheet."
  (subseq name (1+ (or (position #\: name) -1))))

(defun xml-local-name (node)
  "The name of the element NODE without its namespace's prefix (sheet for
x:sheet), NIL for text."
  (let ((name (xml-name node)))
    (and name (xml-unprefixed name))))

(defun xml-local-children (node local)
  "NODE's child elements whose name is LOCAL, whatever their prefix."
  (remove-if-not (lambda (child) (equal (xml-local-name child) local)) (xml-children node)))

(defun xml-find-all (node name &key local)
  "Every element called NAME in NODE and under it, in order; with LOCAL,
every element whose name is NAME whatever its prefix (sheet, x:sheet)."
  (let ((found '()))
    (labels ((walk (n) (when (consp n)
                         (when (equal (if local (xml-local-name n) (xml-name n)) name) (push n found))
                         (mapc #'walk (xml-children n)))))
      (walk node))
    (nreverse found)))

(defun xml-all-text (node &optional limit)
  "The text of NODE and all under it, joined; with LIMIT, no more than its
first LIMIT characters, the rest not walked."
  (let ((count 0))
    (with-output-to-string (s)
      (labels ((walk (n)
                 (cond ((and limit (>= count limit)))
                       ((stringp n)
                        (let ((part (if limit (subseq n 0 (min (length n) (- limit count))) n)))
                          (write-string part s)
                          (incf count (length part))))
                       (t (mapc #'walk (xml-children n))))))
        (walk node)))))

(defun xml-first-text (tree name)
  "The text of the first element called NAME in TREE, without the spaces
around it, or NIL when TREE is NIL or holds none."
  (let ((element (and tree (first (xml-find-all tree name)))))
    (and element (string-trim " " (xml-all-text element)))))

;;; XML read as a tree, nothing fetched or expanded

(defun xml-name-end-p (c)
  "True when the character C ends the name of an element in its tag: a blank
\(BLANK-CHAR-P), > or /."
  (or (blank-char-p c) (char= c #\>) (char= c #\/)))

(defun xml-map-attributes (function text start &optional (end (length text)))
  "Call FUNCTION with the name and value, as written, of each attribute in TEXT
from START below END: name=\"value\" or name='value', blanks allowed around
the =. What is not an attribute is passed over."
  (let ((i start) (n end))
    (flet ((skip-blanks () (loop while (and (< i n) (blank-char-p (char text i))) do (incf i))))
      (loop
        (skip-blanks)
        (when (>= i n) (return))
        (let ((name-start i))
          (loop while (and (< i n) (not (blank-char-p (char text i))) (char/= (char text i) #\=)) do (incf i))
          (let ((name-end i))
            (skip-blanks)
            (when (and (< i n) (char= (char text i) #\=))
              (incf i)
              (skip-blanks)
              (when (and (> name-end name-start) (< i n) (find (char text i) "\"'"))
                (let ((close (position (char text i) text :start (1+ i) :end n)))
                  (unless close (return))
                  (funcall function (subseq text name-start name-end) (subseq text (1+ i) close))
                  (setf i (1+ close)))))))))))

(defparameter *xml-deepest* 512
  "The most elements an XML document read by XML-PARSE may nest; a deeper one
is refused, as made to exhaust the reader.")

(defparameter *xml-most-elements* 2000000
  "The most elements an XML document read by XML-PARSE may have, as standard:
past it the document is refused, as made to exhaust the memory.")

(defun xml-parse (text &key (most *xml-most-elements*))
  "TEXT, an XML document, as a tree: each element (NAME ATTRIBUTES .
CHILDREN), ATTRIBUTES an alist of (NAME . VALUE), its children elements and
strings of text. Declarations, processing instructions and comments are
passed over, a DTD with them: no entity it declares is known, so nothing is
fetched or expanded. Signals an error for elements nested deeper than
*XML-DEEPEST*, or more of them than MOST (*XML-MOST-ELEMENTS* unless given);
a tag cut short at the end is left out. The number of elements read is the
second value."
  (let ((root (list "#document" '())) (stack '()) (depth 0) (i 0) (n (length text)) (elements 0)
        ;; the few names of elements and attributes, each made once however often it is met
        (names (make-hash-table :test 'equal)))
    (flet ((add (child) (let ((parent (or (first stack) root))) (setf (cddr parent) (cons child (cddr parent))))))
      (loop while (< i n)
            do (let ((lt (position #\< text :start i)))
                 (when (and stack (> (or lt n) i))
                   (add (xml-text-value (subseq text i (or lt n)))))
                 (unless lt (return))
                 (setf i lt)
                 (cond ((and (< (+ i 3) n) (string= "<!--" text :start2 i :end2 (+ i 4)))
                        (setf i (let ((e (search "-->" text :start2 i))) (if e (+ e 3) n))))
                       ((and (< (+ i 8) n) (string= "<![CDATA[" text :start2 i :end2 (+ i 9)))
                        (let ((e (or (search "]]>" text :start2 i) n)))
                          (when stack (add (subseq text (+ i 9) e)))
                          (setf i (min n (+ e 3)))))
                       ((and (< (1+ i) n) (member (char text (1+ i)) '(#\? #\!)))
                        ;; a declaration, an instruction, a DTD: passed over, its brackets counted
                        (let ((brackets 0) (j (1+ i)))
                          (loop while (< j n)
                                do (case (char text j)
                                     (#\[ (incf brackets)) (#\] (decf brackets))
                                     (#\> (when (<= brackets 0) (return))))
                                   (incf j))
                          (setf i (1+ j))))
                       ((and (< (1+ i) n) (char= (char text (1+ i)) #\/))
                        (let ((e (or (xml-tag-end text i) n)))
                          (when stack
                            (let ((element (pop stack)))
                              (decf depth)
                              (setf (cddr element) (nreverse (cddr element)))))
                          (setf i (1+ e))))
                       (t
                        (let* ((e (or (xml-tag-end text i) (return)))
                               (self (and (> e (1+ i)) (char= (char text (1- e)) #\/)))
                               (inside (subseq text (1+ i) (if self (1- e) e)))
                               (space (position-if #'blank-char-p inside))
                               (name (let ((name (subseq inside 0 (or space (length inside)))))
                                       (or (gethash name names) (setf (gethash name names) name))))
                               (attributes '()))
                          (when space
                            (xml-map-attributes
                             (lambda (key value)
                               (push (cons (or (gethash key names) (setf (gethash key names) key))
                                           (xml-text-value value)) attributes))
                             inside space))
                          (when (> (incf elements) most)
                            (error "XML of more than ~:d elements" most))
                          (let ((element (list* name (nreverse attributes) '())))
                            (add element)
                            (unless self
                              (when (>= depth *xml-deepest*)
                                (error "XML nested deeper than ~d elements" *xml-deepest*))
                              (push element stack)
                              (incf depth)))
                          (setf i (1+ e))))))))
    ;; what was left open, closed
    (dolist (element stack) (setf (cddr element) (nreverse (cddr element))))
    (setf (cddr root) (nreverse (cddr root)))
    (values root elements)))

;;; XML written

(defun xml-escape (string)
  "STRING as text in XML or HTML, for an element's text or an attribute's value
written between double quotes: & < > and the double quote as entities, the
single quote kept as it is, a character XML has no place for (a control
character, a raw byte's) as U+FFFD."
  (with-output-to-string (out)
    (loop for c across string
          for code = (char-code c)
          do (case c
               (#\& (write-string "&amp;" out))
               (#\< (write-string "&lt;" out))
               (#\> (write-string "&gt;" out))
               (#\" (write-string "&quot;" out))
               (t (if (or (and (< code 32) (not (member code '(9 10 13))))
                          (<= #xD800 code #xDFFF)
                          (member code '(#xFFFE #xFFFF)))
                      (write-char (code-char #xFFFD) out)
                      (write-char c out)))))))

;;; XML as text, its elements found by their tags: for changing a document's
;;; XML in place, what is not touched kept as it was

(defun xml-hidden-test (xml start end)
  "A function of a position in XML from START below END, true when the
position falls within a comment or a CDATA section, whose text is no tags.
XML is read for them only as far as the furthest position asked about, each
part of it once."
  (let ((ranges (make-array 8 :adjustable t :fill-pointer 0))
        (p start)
        ;; of comments and of CDATA sections: the opening and closing, the next opening found
        ;; from P or NIL, and the position below which no opening begins
        (kinds (list (vector "<!--" "-->" nil start) (vector "<![CDATA[" "]]>" nil start))))
    (labels ((next-opening (kind limit)
               ;; KIND's next opening from P, looked for no further than one beginning at LIMIT
               (let ((open (svref kind 0)) (found (svref kind 2)))
                 (when (and found (< found p)) (setf found nil (svref kind 2) nil))
                 (or found
                     (let ((from (max p (svref kind 3))) (stop (min end (+ limit (length open)))))
                       (when (< from stop)
                         (setf found (search open xml :start2 from :end2 stop)))
                       (if found
                           (setf (svref kind 2) found)
                           (setf (svref kind 3) (max (svref kind 3) (1+ (- stop (length open))))))
                       found))))
             (read-to (i)
               ;; the comments and CDATA sections beginning at I or before, taken into RANGES
               (loop while (<= p i)
                     do (let* ((comment (next-opening (first kinds) i))
                               (cdata (next-opening (second kinds) i))
                               (kind (cond ((and comment (<= comment i) (or (null cdata) (< comment cdata)))
                                            (first kinds))
                                           ((and cdata (<= cdata i)) (second kinds)))))
                          (unless kind (return))
                          (let* ((at (svref kind 2))
                                 (closer (svref kind 1))
                                 (close (search closer xml :start2 (min end (+ at (length (svref kind 0)))) :end2 end)))
                            (vector-push-extend (cons at (if close (+ close (length closer)) end)) ranges)
                            (setf p (cdr (aref ranges (1- (fill-pointer ranges))))))))))
      (lambda (i)
        (read-to i)
        ;; the last range beginning at I or before, by bisection
        (let ((low 0) (high (fill-pointer ranges)))
          (loop while (< low high)
                do (let ((middle (floor (+ low high) 2)))
                     (if (<= (car (aref ranges middle)) i) (setf low (1+ middle)) (setf high middle))))
          (and (plusp low) (< i (cdr (aref ranges (1- low))))))))))

(defun xml-elements (xml name &key (start 0) (end (length xml)) count)
  "The elements of XML from START below END called NAME (w:p), as (BEGIN .
END) of each, the outermost only, in order, no more than COUNT of them when
it is given; one cut short at the end left out. XML nesting NAME deeper than
*XML-DEEPEST*, or holding more than *XML-MOST-ELEMENTS* of them, is refused,
as XML-PARSE refuses it. Tags within comments and CDATA sections are none, as
XML-PARSE has them. XML is read only as far as the elements given, each part
of it once."
  (let ((found '()) (taken 0) (seen 0) (counted -1) (p start)
        (open (format nil "<~a" name)) (close (format nil "</~a>" name))
        (hidden-p (xml-hidden-test xml start end)))
    (labels ((name-ends-at (i)
               (and (< i (length xml)) (xml-name-end-p (char xml i))))
             (outside (needle from &optional (before end))
               ;; the next NEEDLE from FROM, beginning before BEFORE, not within a comment or CDATA
               (let ((stop (min end (+ before (length needle) -1))))
                 (when (<= from stop)
                   (loop for s = (search needle xml :start2 from :end2 stop)
                           then (search needle xml :start2 (1+ s) :end2 stop)
                         while (and s (funcall hidden-p s))
                         finally (return s)))))
             (open-from (q &optional (before end))
               ;; the next opening tag of NAME from Q, beginning before BEFORE, each counted once against
               ;; the most allowed
               (loop for s = (outside open q before) then (outside open (1+ s) before)
                     while (and s (not (name-ends-at (+ s (length open)))))
                     finally (when (and s (> s counted))
                               (setf counted s)
                               (when (> (incf seen) *xml-most-elements*)
                                 (error "XML of more than ~:d elements" *xml-most-elements*)))
                             (return s))))
      (loop
        (when (and count (>= taken count)) (return))
        (let ((at (open-from p)))
          (unless at (return))
          (let ((tag-end (xml-tag-end xml at)))
            (cond
              ((null tag-end) (return))
              ((char= (char xml (1- tag-end)) #\/)
               (push (cons at (1+ tag-end)) found) (incf taken) (setf p (1+ tag-end)))
              ;; to the close that matches, nested ones of the same name counted: the next
              ;; closing tag looked for again only once passed, an opening only before it
              (t
               (let ((depth 1) (q (1+ tag-end)) (next-close nil))
                 (loop while (plusp depth)
                       do (unless (and next-close (>= next-close q)) (setf next-close (outside close q)))
                          (unless next-close (return-from xml-elements (nreverse found)))
                          ;; an opening of the same name is looked for only up to the close
                          (let ((next-open (open-from q next-close)))
                            (cond (next-open
                                   (let ((e (xml-tag-end xml next-open)))
                                     (unless e (return-from xml-elements (nreverse found)))
                                     (unless (char= (char xml (1- e)) #\/)
                                       (when (>= (incf depth) *xml-deepest*)
                                         (error "XML nested deeper than ~d elements" *xml-deepest*)))
                                     (setf q (1+ e))))
                                  (t (decf depth)
                                     (setf q (+ next-close (length close)))))))
                 (push (cons at q) found) (incf taken)
                 (setf p q))))))))
    (nreverse found)))

(defun xml-element (xml name &key (start 0) (end (length xml)))
  "The first element of XML from START below END called NAME, as (BEGIN .
END) as XML-ELEMENTS gives them, or NIL; what follows it is not looked at."
  (first (xml-elements xml name :start start :end end :count 1)))

(defun xml-top-elements (xml start end)
  "The elements directly within XML from START below END, each (NAME BEGIN . END)."
  (let ((found '()) (p start))
    (loop
      (let ((at (position #\< xml :start p :end end)))
        (unless (and at (< (1+ at) end)) (return))
        (cond
          ((string= "<!--" xml :start2 at :end2 (min end (+ at 4)))
           (setf p (let ((close (search "-->" xml :start2 (+ at 4) :end2 end))) (if close (+ close 3) end))))
          ((string= "<![CDATA[" xml :start2 at :end2 (min end (+ at 9)))
           (setf p (let ((close (search "]]>" xml :start2 (+ at 9) :end2 end))) (if close (+ close 3) end))))
          ((member (char xml (1+ at)) '(#\/ #\? #\!))
           (setf p (1+ at)))
          (t
            (let* ((name-end (position-if #'xml-name-end-p xml :start (1+ at)))
                   (name (subseq xml (1+ at) name-end))
                   (element (xml-element xml name :start at :end end)))
              (unless element (return))
              (push (list* name element) found)
              (setf p (cdr element)))))))
    (nreverse found)))

(defun xml-element-inner (xml element)
  "Where the inside of ELEMENT, (BEGIN . END) of an element of XML as
XML-ELEMENTS gives them, begins and ends in XML, two values: from after its
opening tag to before its closing one; both after its tag when it closes
itself (<w:sdtContent/>), its inside empty."
  (let ((open-end (1+ (xml-tag-end xml (car element)))))
    (if (char= (char xml (- open-end 2)) #\/)
        (values open-end open-end)
        (values open-end (or (search "</" xml :from-end t :start2 open-end :end2 (cdr element)) open-end)))))

(defun xml-element-inside (xml element)
  "The inside of ELEMENT of XML (XML-ELEMENT-INNER) as a string."
  (multiple-value-bind (start end) (xml-element-inner xml element)
    (subseq xml start end)))

(defun xml-replace-elements (xml name function &key (start 0))
  "XML with each element called NAME from START on (the outermost, as
XML-ELEMENTS finds them) replaced by what FUNCTION, of the element's XML,
returns: a string, or NIL to take the element away; the rest as it is."
  (let ((out (make-string-output-stream)) (p 0))
    (dolist (element (xml-elements xml name :start start))
      (write-string xml out :start p :end (car element))
      (let ((new (funcall function (subseq xml (car element) (cdr element)))))
        (when new (write-string new out)))
      (setf p (cdr element)))
    (write-string xml out :start p)
    (get-output-stream-string out)))

(defun xml-value (xml element &optional (attribute "w:val"))
  "The value of ATTRIBUTE (w:val unless given) of the first ELEMENT in XML
that has it, read as text: in either quotes, wherever among its attributes,
its entities read (&amp; as &); NIL when there is none."
  (let ((open (concatenate 'string "<" element)) (p 0))
    (loop for at = (search open xml :start2 p)
          while at
          do (let ((after (+ at (length open))))
               (setf p (1+ at))
               (when (and (< after (length xml))
                          (let ((c (char xml after))) (or (blank-char-p c) (char= c #\/) (char= c #\>))))
                 (let ((end (xml-tag-end xml at)))
                   (unless end (return nil))
                   (xml-map-attributes (lambda (name value)
                                         (when (string= name attribute)
                                           (return-from xml-value (xml-text-value value))))
                                       xml after end)
                   (setf p end)))))))
