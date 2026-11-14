;;;; lisp/files/external.lisp --- A file opened in the desktop's own program, on asking.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A picture, a PDF, a sound or a video can be handed to the program the
;;;; desktop opens that kind of file with (xdg-open), for what the editor does
;;;; not do: printing, forms, playing. That program is not shut in: it reads the
;;;; file with all it can reach. So it is only done when asked, only for the
;;;; kinds of file *OPEN-EXTERNALLY-KINDS* names, told by the file's first
;;;; octets, and only when its name does not say it is something else (a
;;;; program named .jpg is refused); and a question comes first, unless
;;;; *OPEN-EXTERNALLY-ASK* is NIL. The program runs in a session of its own, so
;;;; that it stays when the editor or its terminal goes.
;;;; The program is chosen by the file's kind, never by its name alone.

(in-package #:tuatara.defaults)

(define-setting *open-externally-program* "xdg-open"
  "The program that opens a file in the desktop's own program for its kind, given
the file's name.
It is looked for on the PATH. A kind given its own program in
*OPEN-EXTERNALLY-PROGRAMS* goes there instead.
- a program's name: xdg-open as standard."
  :values string)

(define-setting *open-externally-programs* '()
  "Programs to open files and addresses in instead of the one the desktop would
choose (xdg-open), by Multipurpose Internet Mail Extensions (MIME) type as
xdg-mime names them.
A file's type is told by its contents, not its name (*KIND-MIME-TYPES*); the
entry for the exact type wins over one for its group, and a type with none goes
to the desktop's program. The program is run as it is, never by a shell. For
example: '((\"application/pdf\" . \"zathura\") (\"video/*\" \"mpv\" \"--fs\")).
- nil: the desktop's program for every type.
- a list of (TYPE . PROGRAM): TYPE is a MIME type, \"application/pdf\", or a
  group of them, \"video/*\"; for web and mail addresses in Org links,
  \"x-scheme-handler/https\" or \"x-scheme-handler/mailto\". PROGRAM is a
  program's name, looked for on the PATH, or its full path, or a list of it and
  the arguments to give before the file's name."
  :values (list-of "(TYPE . PROGRAM)"))

(defparameter *kind-mime-types*
  '((:png "image/png") (:jpeg "image/jpeg") (:gif "image/gif") (:webp "image/webp") (:bmp "image/bmp")
    (:tiff "image/tiff")
    (:raw "image/x-dcraw")
    (:ico "image/vnd.microsoft.icon")
    (:heif "image/heif")
    (:avif "image/avif")
    (:jp2 "image/jp2") (:j2k "image/x-jp2-codestream") (:jxl "image/jxl")
    (:xcf "image/x-xcf") (:psd "image/vnd.adobe.photoshop") (:svg "image/svg+xml") (:pdf "application/pdf")
    (:mp4 "video/mp4" ("m4a" . "audio/mp4") ("m4b" . "audio/mp4") ("mov" . "video/quicktime") ("qt" . "video/quicktime")
     ("3gp" . "video/3gpp") ("3g2" . "video/3gpp2"))
    (:matroska "video/x-matroska" ("mka" . "audio/x-matroska") ("webm" . "video/webm"))
    (:avi "video/x-msvideo") (:mpeg "video/mpeg") (:mp3 "audio/mpeg")
    (:ogg "audio/ogg" ("ogv" . "video/ogg") ("opus" . "audio/x-opus+ogg") ("spx" . "audio/x-speex+ogg"))
    (:flac "audio/flac") (:wav "audio/x-wav") (:aiff "audio/x-aiff") (:midi "audio/midi")
    (:docx "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
     ("docm" . "application/vnd.ms-word.document.macroEnabled.12")
     ("dotx" . "application/vnd.openxmlformats-officedocument.wordprocessingml.template")
     ("dotm" . "application/vnd.ms-word.template.macroEnabled.12"))
    (:xlsx "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
     ("xlsm" . "application/vnd.ms-excel.sheet.macroEnabled.12")
     ("xltx" . "application/vnd.openxmlformats-officedocument.spreadsheetml.template")
     ("xltm" . "application/vnd.ms-excel.template.macroEnabled.12"))
    (:pptx "application/vnd.openxmlformats-officedocument.presentationml.presentation"
     ("pptm" . "application/vnd.ms-powerpoint.presentation.macroEnabled.12")
     ("potx" . "application/vnd.openxmlformats-officedocument.presentationml.template")
     ("potm" . "application/vnd.ms-powerpoint.template.macroEnabled.12")
     ("ppsx" . "application/vnd.openxmlformats-officedocument.presentationml.slideshow")
     ("ppsm" . "application/vnd.ms-powerpoint.slideshow.macroEnabled.12"))
    (:ole "application/x-ole-storage" ("doc" . "application/msword") ("dot" . "application/msword")
     ("xls" . "application/vnd.ms-excel") ("xlt" . "application/vnd.ms-excel")
     ("ppt" . "application/vnd.ms-powerpoint")
     ("pot" . "application/vnd.ms-powerpoint")
     ("pps" . "application/vnd.ms-powerpoint"))
    (:odt "application/vnd.oasis.opendocument.text") (:ods "application/vnd.oasis.opendocument.spreadsheet")
    (:odp "application/vnd.oasis.opendocument.presentation"))
  "The Multipurpose Internet Mail Extensions (MIME) type of each kind of file
\\[open-externally] may open, by *OPEN-EXTERNALLY-PROGRAMS*: (KIND TYPE
(EXTENSION . TYPE)...), KIND as TUATARA.IO:OCTETS-KIND tells it by the
file's contents, TYPE its type, and where the kind holds more than one sort
of thing (sound or video, a document or its template, an Office document of
before 2007), the type a name ending in EXTENSION has instead (an Ogg file
named .ogv is video/ogg, a compound file named .xls
application/vnd.ms-excel).")

(defun file-mime-type (kind name)
  "The Multipurpose Internet Mail Extensions (MIME) type of a file of KIND, as
its contents tell, named NAME (*KIND-MIME-TYPES*); NIL for a kind with
none."
  (let ((entry (assoc kind *kind-mime-types*)))
    (when entry
      (let ((extension (tuatara.io:native-file-type name)))
        (or (and extension (cdr (assoc extension (cddr entry) :test #'string-equal)))
            (second entry))))))

(defun chosen-program (type)
  "The program *OPEN-EXTERNALLY-PROGRAMS* gives the Multipurpose Internet Mail
Extensions (MIME) type TYPE, the exact type before its group, as three
values: its full path (NIL when it is not found), the arguments to give
before the name, and the program as given; NIL when it gives none."
  (when type
    (let* ((group (concatenate 'string (subseq type 0 (or (position #\/ type) (length type))) "/*"))
           (entry (or (assoc type *open-externally-programs* :test #'string-equal)
                      (assoc group *open-externally-programs* :test #'string-equal))))
      (when entry
        (let ((words (if (listp (cdr entry)) (cdr entry) (list (cdr entry)))))
          (unless (and words (every #'stringp words) (plusp (length (first words))))
            (refuse "*open-externally-programs*: the program for ~a is not a name, or a list of a name and its arguments"
                   (car entry)))
          (values (tuatara.io:find-program (first words)) (rest words) (first words)))))))

(defun external-opener (type)
  "What opens a file or an address of the Multipurpose Internet Mail
Extensions (MIME) type TYPE outside Tuatara, as four values: the program's
full path (NIL when not found), the arguments to give before the name, the
program as given, and how to speak of it (the desktop's program, or the
program's name). A program of your choosing (*OPEN-EXTERNALLY-PROGRAMS*) comes
before the desktop's (*OPEN-EXTERNALLY-PROGRAM*)."
  (multiple-value-bind (path arguments given) (chosen-program type)
    (if given
        (values path arguments given (tuatara.io:native-file-name given))
        (values (and *open-externally-program* (tuatara.io:find-program *open-externally-program*))
                '() *open-externally-program* "the desktop's program"))))

(defun opener-missing (given type)
  "What to say when GIVEN, the program to open the Multipurpose Internet Mail
Extensions (MIME) type TYPE with, is not found."
  (if (nth-value 2 (chosen-program type))
      (format nil "~a, the program for ~a in *open-externally-programs*, was not found" given type)
      (format nil "~a was not found: install xdg-utils to open files in the desktop's programs" given)))

(define-setting *open-externally-kinds*
  '(:png :jpeg :gif :webp :bmp :tiff :raw :ico :heif :avif :jp2 :j2k :jxl :xcf :psd :pdf
    :mp4 :matroska :avi :mpeg :mp3 :ogg :flac :wav :aiff :midi
    :docx :xlsx :pptx :odt :ods :odp)
  "The kinds of file \\[open-externally] opens in the desktop's own program, as
their first octets tell them.
Pictures, PDFs, sound and video, and Office documents are the standard. Each is
opened after asking, from a copy only you can read, never the file itself. An
Office document can hold macros, which the program opening it may offer to run;
answer no there. :svg is left out of the standard list: an SVG image is a
document that can hold a program, which a web browser, the program many
desktops open it in, runs. Office documents of before 2007 (:ole) are left out:
the same kind is a mail item or an installer.
- a list of keywords, as TUATARA.IO:OCTETS-KIND gives them: :png, :jpeg, :pdf,
  :docx and the like."
  :values (list-of "a keyword naming a kind of file"))

(define-setting *open-externally-ask* t
  "Whether \\[open-externally] asks before opening a file in the desktop's own
program, which reads it outside the sandbox.
- t: it asks.
- nil: it opens the file at once."
  :values boolean)

(defun open-externally-refusal (path &key also)
  "Why the file at PATH is not opened in the desktop's program, as a sentence,
or NIL when it may be, with its kind in words as a second value; ALSO, kinds
opened this once besides *OPEN-EXTERNALLY-KINDS*."
  (let ((name (tuatara.io:native-file-name path)))
    (multiple-value-bind (words kind why) (file-kind path)
      (cond (why (format nil "~a cannot be read: ~a" name why))
            ((null kind) (format nil "~a is not a picture, a PDF, a sound or a video by its contents: not opened outside Tuatara" name))
            ((not (or (member kind *open-externally-kinds*) (member kind also)))
             (format nil "~a is ~a by its contents: not opened outside Tuatara (*open-externally-kinds*)" name words))
            ((tuatara.io:kind-name-disagrees-p kind name)
             (format nil "~a is ~a by its contents, not what its name says: not opened outside Tuatara" name words))
            (t (values nil words kind))))))

(defvar *openings* '()
  "Files being copied to be opened in the desktop's program, each (THREAD
. SAID), SAID set to what to say once it is over.")

(defun opened-copies-directory ()
  "Where the copies of files opened in the desktop's programs are kept, for
you alone: opened/ in the cache directory."
  (private-cache-directory "opened/"))

(defun private-copy-path (name)
  "Where a private copy of a file called NAME goes: in a new directory of its own
in OPENED-COPIES-DIRECTORY (NEW-PRIVATE-DIRECTORY), gone a day later, NAME taken as it is, so it
must be a file's name alone. The directory is made; the copy is not."
  (tuatara.io:path-join (new-private-directory (opened-copies-directory) :hold nil) name))

(defun write-private-copy (name octets)
  "Write OCTETS as a new file called NAME (PRIVATE-COPY-PATH), readable by you
alone and by nothing written again (0400) from the start (WRITE-NEW-FILE); its
path."
  (tuatara.io:write-new-file (private-copy-path name) octets))

(defun start-detached (program arguments &key (label "a program"))
  "Start PROGRAM with ARGUMENTS in a session of its own (setsid), so that it
stays when the editor or its terminal goes, nothing of the editor's given to
it on standard input, output or error; it is waited for in the background.
Returns its process."
  (let ((process (tuatara.io:start-program (cons program arguments) :session t)))
    (tuatara.runtime:make-guarded-thread (lambda ()
                                           (ignoring-errors-reported ("waiting for ~a" label)
                                             (sb-ext:process-wait process)
                                             (sb-ext:process-close process)))
                                         :name (format nil "tuatara ~a" label))
    process))

(defun open-copy-externally (native name kind words program &key arguments (label "the desktop's program"))
  "What a thread does to open the file NATIVE, told to be KIND, in PROGRAM,
given ARGUMENTS before its name: copy it where only you can read it
(OPENED-COPIES-DIRECTORY), read only; check the copy is still KIND, so that a
file changed after it was looked at is not opened; and open the copy. Returns
what to say of it, LABEL naming the program."
  (handler-case
      (reporting-errors ("opening ~a outside Tuatara" native)
        (let* ((copy (private-copy-path name))
               (place (tuatara.io:parent-path copy)))
          ;; the content copied into a new file of its own: a link is followed to its file, never made again
          (unless (eq :regular (tuatara.io:path-kind native))
            (return-from open-copy-externally (format nil "~a is not a plain file: not opened" name)))
          (unless (tuatara.io:copy-regular-file native copy)
            (return-from open-copy-externally (format nil "~a could not be copied whole: not opened" name)))
          (sb-posix:chmod copy #o400)
          (unless (eq kind (nth-value 1 (file-kind copy)))
            (ignoring-errors-reported ("deleting ~a" place) (tuatara.io:delete-tree place))
            (return-from open-copy-externally (format nil "~a changed after it was looked at: not opened" name)))
          ;; a session of its own (setsid): the program stays when the editor or its terminal goes
          (start-detached program (append arguments (list copy)) :label "opened file")
          (tuatara.runtime:debug-report :file "opened outside Tuatara: ~a (~a) as its copy ~a, with ~a"
                                        native words copy program)
          (format nil "Opened ~a in ~a (a copy of it, read only)" name label)))
    (error (e) (format nil "~a not opened: ~a" name e))))

(defun open-externally-file (path &key also)
  "Open the file at PATH in the desktop's own program for its kind, or the one
*OPEN-EXTERNALLY-PROGRAMS* gives its type, when it is one
*OPEN-EXTERNALLY-KINDS* names, or ALSO (a Word document Tuatara has just
written, :docx), and its name agrees, after asking unless
*OPEN-EXTERNALLY-ASK* is NIL; say why not otherwise. The program gets a copy
of it, read only and yours alone, made in the background: it can change
nothing of the file, not even when it was read, and a file changed after it
was looked at is not opened. True when the opening was started."
  (let* ((native (tuatara.io:native (tuatara.io:expand-path path)))
         (name (tuatara.io:native-file-name native)))
    (multiple-value-bind (refusal words kind) (open-externally-refusal native :also also)
      (if refusal
          (progn (message "~a" refusal) nil)
          (let ((type (file-mime-type kind name)))
            (multiple-value-bind (program arguments given label) (external-opener type)
              (cond ((null program) (message "~a" (opener-missing given type)) nil)
                    ((and *open-externally-ask*
                          (not (yes-or-no-p* (format nil "Open ~a (~a) in ~a, outside the sandbox?" name words label))))
                     (message "Not opened")
                     nil)
                    (t
                     (let ((editor *editor*)
                           (opening (cons nil nil)))
                       (setf (car opening)
                             (tuatara.runtime:make-guarded-thread
                              (lambda ()
                                (unwind-protect
                                     (setf (cdr opening)
                                           (handler-case (open-copy-externally native name kind words program
                                                                               :arguments arguments :label label)
                                             (serious-condition (e) (format nil "~a not opened: ~a" name e))))
                                  ;; however it ends, the opening is over and said
                                  (unless (cdr opening) (setf (cdr opening) (format nil "~a not opened" name)))
                                  (wake-editor editor)))
                              :name "tuatara open outside"))
                       (push opening *openings*)
                       (message "Opening ~a in ~a..." name label)
                       t)))))))))

(defun openings-tick (editor)
  "Tick function: say how the openings in the desktop's programs went; T while one is under way."
  (declare (ignore editor))
  (dolist (opening (reverse *openings*))
    (when (cdr opening)
      (setf *openings* (remove opening *openings*))
      (message "~a" (cdr opening))))
  (and *openings* t))

(defun shown-file (buffer)
  "The file BUFFER shows: the PDF or picture in its view, the entry on the
line in a list of a directory, or the file it visits; NIL for none."
  (or (tuatara.buffer:buffer-local buffer :viewed-file)
      (mode-file-name-at-point (buffer-major-mode buffer) buffer)
      (tuatara.buffer:buffer-file buffer)))

(defun shown-file-argument (prompt)
  "The file a command works on, its native name: the one the current buffer
shows (SHOWN-FILE), else, or with \\[universal-argument], one asked for with
PROMPT."
  (let ((shown (and (not (prefix nil)) (shown-file (current-buffer)))))
    (tuatara.io:native (tuatara.io:expand-path (or shown (read-path prompt))))))

(define-command open-externally ()
  "Open the file shown, in the desktop's own program for its kind (xdg-open),
or the one *OPEN-EXTERNALLY-PROGRAMS* gives its Multipurpose Internet Mail
Extensions (MIME) type, for what Tuatara does not do: printing, forms,
playing. In a list of a directory, the entry on the line; in a PDF's view, the
PDF. That program is not shut in the sandbox: only a picture, a PDF, a sound
or a video (*OPEN-EXTERNALLY-KINDS*), by its contents and with a name that
agrees, is opened, after asking (*OPEN-EXTERNALLY-ASK*)."
  (let ((path (shown-file (current-buffer))))
    (if (null path)
        (message "~a shows no file" (tuatara.buffer:buffer-name (current-buffer)))
        (open-externally-file path))))

(defun external-program-words ()
  "What opens files outside Tuatara here, as M-x check-health says it."
  (let ((program (and *open-externally-program* (tuatara.io:find-program *open-externally-program*))))
    (format nil "~a~@[; ~{~a~^, ~} to programs of your choosing (*open-externally-programs*)~]"
            (program-health-words "outside Tuatara" program (format nil "~a not found: ~a" *open-externally-program*
                                                                     (outside-program-install-words :open))
                                  :how "on asking (O in a list or a PDF), for pictures, PDFs, sound and video")
            (mapcar #'car *open-externally-programs*))))

(defun install-open-externally ()
  "Bind \\`O' in lists of directories and PDFs' views to open the file in the desktop's program."
  (define-key (symbol-value '*rimu-keymap*) "O" 'open-externally)
  (define-key (symbol-value '*pdf-view-keymap*) "O" 'open-externally)
  (pushnew 'openings-tick *tick-functions*))

(defun mail-address-without-attachments (address)
  "ADDRESS, a mailto: address, without the files it asks the mail program to
attach (attach=, attachment=, in any case, %XX written or not), for a link from a file
not trusted could name any file of yours. As it is when it asks for none."
  (let ((query (position #\? address)))
    (if (or (null query) (not (uiop:string-prefix-p "mailto:" (string-downcase address))))
        address
        (let ((kept (remove-if (lambda (part)
                                 ;; the name as the mail program reads it: %61ttach is attach
                                 (member (string-trim '(#\Space #\Tab)
                                                      (string-downcase (tuatara.io:percent-decoded
                                                                        (subseq part 0 (or (position #\= part) (length part))))))
                                         '("attach" "attachment") :test #'string=))
                               (uiop:split-string (subseq address (1+ query)) :separator "&"))))
          (format nil "~a~@[?~{~a~^&~}~]" (subseq address 0 query) kept)))))

(defparameter *open-address-schemes* '("http" "https" "ftp" "mailto")
  "The kinds of address, by the scheme they begin with, that OPEN-ADDRESS hands
to another program: web and mail addresses. Any other kind, a file or smb address among them, is refused.")

(defun address-scheme (address)
  "The scheme ADDRESS begins with, before its first colon, in lower case; an
empty string when it has none."
  (string-downcase (subseq address 0 (or (position #\: address) 0))))

(defun open-address-p (address)
  "True when ADDRESS is of a kind OPEN-ADDRESS hands on (*OPEN-ADDRESS-SCHEMES*)."
  (and (member (address-scheme address) *open-address-schemes* :test #'string=) t))

(defun open-address (address)
  "Hand ADDRESS, a web or mail address, to the desktop's program after asking
\(a link followed in Org or a terminal),
or to the one *OPEN-EXTERNALLY-PROGRAMS* gives its scheme
(x-scheme-handler/https, x-scheme-handler/mailto). An address of any other
kind (*OPEN-ADDRESS-SCHEMES*) is refused, and that is said."
  (let* ((asked address)
         (address (mail-address-without-attachments address))
         (type (format nil "x-scheme-handler/~a" (address-scheme address))))
    (multiple-value-bind (program arguments given label) (and (open-address-p address) (external-opener type))
      (cond ((not (open-address-p address))
             (message "Links of the kind ~a are not opened: ~a" (address-scheme address) address))
            ((null program) (message "~a" (opener-missing given type)))
            ((not (yes-or-no-p* (format nil
                                        "Open ~a in ~a, outside Tuatara?~:[ (the files it asked to attach left out)~;~]"
                                        address label (string= address asked))))
             (message "Not opened"))
            (t (start-detached program (append arguments (list address)) :label "open address")
               (message "Opened ~a" address))))))
