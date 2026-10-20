;;;; lisp/editing/own-files.lisp --- Tuatara's own data files, and folders of yours that add to them or take their place.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Snippets, capture templates and LaTeX templates come with Tuatara, read
;;;; from data/ into the program when it is built; yours are files in folders
;;;; of your own, read as they are needed. One of yours takes the place of one
;;;; of Tuatara's of the same name, and a command copies one of Tuatara's into
;;;; your folder for you to change.

(in-package #:tuatara.defaults)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *data-directory*
    (merge-pathnames "../../data/" (uiop:pathname-directory-pathname #.(or *compile-file-truename* *load-truename*)))
    "Tuatara's data/ folder in its source, read when Tuatara is built; NIL in
a saved program (FORGET-DATA-DIRECTORY).")

  (defun data-file-text (name)
    "The text of NAME, a file of Tuatara's data/ folder (\"init.lisp\"), read
when Tuatara is built."
    (alexandria:read-file-into-string (merge-pathnames name *data-directory*) :external-format :utf-8)))

(defun forget-data-directory ()
  "Forget where Tuatara's data/ folder was when it was built, so that a saved
program does not keep that place: what it holds is in the program already."
  (setf *data-directory* nil))

(pushnew 'forget-data-directory sb-ext:*save-hooks*)

(defmacro built-in-data-files (folder &key by-folder type)
  "The files of FOLDER under Tuatara's data/ folder (\"snippets/\"), read when
the file using this is compiled and kept in the program: each (FILE-NAME
TEXT), or with BY-FOLDER, those of the folders in it, each (FOLDER-NAME
FILE-NAME TEXT); in order of their names. With TYPE, only the files with that
ending (\"tex\")."
  (let ((root (merge-pathnames folder *data-directory*)))
    `',(loop for file in (sort (directory (merge-pathnames (make-pathname :directory (if by-folder '(:relative :wild) nil)
                                                                          :name :wild :type (or type :wild))
                                                           root))
                               #'string< :key #'namestring)
             when (pathname-name file)
               collect (append (and by-folder (list (car (last (pathname-directory file)))))
                               (list (file-namestring file)
                                     (alexandria:read-file-into-string file :external-format :utf-8))))))

(defun template-head (text)
  "The head of TEXT, a snippet's or a capture template's file, as two values:
its # NAME: VALUE lines before a line # --, each (NAME . VALUE), NAME in small
letters; and the text after that line, its last line end left out. NIL and
TEXT when it has no such line. Lines may end in a carriage return and a line
end; the text has line ends alone."
  (let* ((lines (mapcar (lambda (line) (string-right-trim '(#\Return) line))
                        (uiop:split-string text :separator '(#\Newline))))
         (head-end (position "# --" lines :test (lambda (mark line) (string= mark (string-right-trim " " line))))))
    (if (null head-end)
        (values nil (format nil "~{~a~^~%~}" lines))
        (values (loop for line in (subseq lines 0 head-end)
                      for match = (nth-value 1 (cl-ppcre:scan-to-strings "^#\\s*([A-Za-z-]+):\\s*(.*?)\\s*$" line))
                      when match collect (cons (string-downcase (aref match 0)) (aref match 1)))
                ;; a file's last line end is not part of the text
                (string-right-trim '(#\Newline) (format nil "~{~a~^~%~}" (subseq lines (1+ head-end))))))))

(defun own-file-name-p (name)
  "True for NAME, a file's name in a folder of yours, when the file is read:
not a hidden file (.name) nor a backup (name~)."
  (not (or (uiop:string-prefix-p "." name) (uiop:string-suffix-p name "~"))))

(defun own-folder (directory &optional folder)
  "The folder DIRECTORY (a folder of yours, as a setting writes it), or the
folder called FOLDER in it, as an absolute path."
  (tuatara.io:expand-path (format nil "~a/~@[~a/~]" (string-right-trim "/" directory) folder)))

(defun folder-files (directories &optional folder)
  "The files of your DIRECTORIES (a setting's list of folders), or of the
folder called FOLDER in each, each (FILE-NAME . TEXT); hidden files and
backups left out (OWN-FILE-NAME-P), a file of more than 1 MB or one that
cannot be read as NIL. A folder that is not there has none."
  (loop for directory in directories
        for place = (tuatara.io:native (own-folder directory folder))
        append (loop for (name kind) in (tuatara.io:directory-entries place)
                     when (and (eq kind :regular) (own-file-name-p name))
                       collect (cons name (handler-case (tuatara.io:file-text (concatenate 'string place name)
                                                                              :limit (* 1024 1024) :too-large (constantly nil))
                                            (tuatara.io:file-system-error () nil))))))

(defun in-place-of-own (yours own key)
  "YOURS, then those of OWN (Tuatara's) that none of YOURS takes the place of:
one of yours takes the place of one of Tuatara's when KEY, a function giving
each its name or key as text, gives the same for both."
  (append yours (remove-if (lambda (one) (find (funcall key one) yours :key key :test #'string=)) own)))

(defun copy-own-file (directories folder file-name text what)
  "Write TEXT, one of Tuatara's own WHAT (\"snippet\") called FILE-NAME, into the
first of DIRECTORIES (in its folder FOLDER, when given) and visit it; one there
already is visited as it is, never written over."
  (let* ((place (tuatara.io:native (own-folder (first directories) folder)))
         (path (concatenate 'string place file-name)))
    (if (probe-file (sb-ext:parse-native-namestring path))
        (progn (visit-path path) (message "~a is yours already" (abbreviated-path path)))
        (progn (tuatara.io:ensure-directories place)
               (tuatara.io:write-private-file path text)
               (visit-path path)
               (message "Copied the ~a ~a to ~a: yours takes the place of Tuatara's" what file-name (abbreviated-path path))))))

(defun copy-chosen-own-file (prompt candidates directories what)
  "Ask with PROMPT for one of CANDIDATES, each one of Tuatara's own WHAT
\(\"snippet\") whose value is (FOLDER FILE-NAME TEXT), FOLDER NIL for none,
and copy it into the first of DIRECTORIES (COPY-OWN-FILE)."
  (let ((chosen (completing-read-value prompt candidates :require-match t)))
    (when (consp chosen)
      (destructuring-bind (folder file-name text) chosen
        (copy-own-file directories folder file-name text what)))))
