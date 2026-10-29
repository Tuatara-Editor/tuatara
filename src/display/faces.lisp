;;;; src/display/faces.lisp --- Faces: named, inheritable display attributes.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Layout gives each printed character a face name, or a list of names with the
;;;; highest priority first; painters resolve them here to colours and flags.
;;;; The defaults define the faces (lisp/theme.lisp); an init file can change them
;;;; with SET-FACE-ATTRIBUTES.

(in-package #:tuatara.display)

(defstruct (face (:constructor %make-face (name)) (:copier nil))
  "A named set of display attributes. FG and BG are colours: a keyword such as
:red or :bright-blue, an integer 0-255, a string \"#rrggbb\", or NIL for
unspecified. BOLD, ITALIC, UNDERLINE, REVERSE and DIM are true or NIL
(unspecified). EXTEND true carries the face on past the end of a line it
covers the newline of, to the window's edge. FONT is the name of a font
without its size (\"DejaVu Sans Mono\") that a window of its own paints
the text in, at its own size; a terminal shows its own. HEIGHT, 2 or 3, has a
window of its own draw a line beginning in the face that many rows tall, its
letters as many times as wide (a headline drawn large); NIL, or 1, for one row.
INHERIT names a face whose
attributes fill in what this one leaves unspecified. DOC says what the face is
used for. BASE is the attribute list its definition gave it, which a theme
starts from."
  (name nil :read-only t)
  (doc "")
  (fg nil) (bg nil)
  (bold nil) (italic nil) (underline nil) (underline-colour nil) (reverse nil) (dim nil)
  (extend nil)
  (font nil)
  (height nil)
  (inherit nil)
  (base '()))

(setf (documentation 'face-p 'function) "True when OBJECT is a FACE."
      (documentation 'face-name 'function) "The name of FACE, a keyword."
      (documentation 'face-doc 'function) "What FACE is used for."
      (documentation 'face-fg 'function) "The foreground colour of FACE, or NIL."
      (documentation 'face-bg 'function) "The background colour of FACE, or NIL."
      (documentation 'face-bold 'function) "True when FACE is bold."
      (documentation 'face-italic 'function) "True when FACE is italic."
      (documentation 'face-underline 'function) "T when FACE is underlined with a straight line, :WAVE with a wavy one, :DOTTED with a dotted one, :BOX with a box round each character (where a window of its own draws it; a terminal draws a straight line), NIL when not."
      (documentation 'face-underline-colour 'function) "The colour of FACE's underline, or NIL for its text's."
      (documentation 'face-reverse 'function) "True when FACE swaps foreground and background."
      (documentation 'face-dim 'function) "True when FACE is dimmed."
      (documentation 'face-extend 'function) "True when FACE goes on past the end of a line whose newline it covers, to the window's edge."
      (documentation 'face-font 'function) "The font FACE's text is painted in by a window of its own, a name without a size; NIL for the window's."
      (documentation 'face-height 'function) "How many rows tall a window of its own draws a line beginning in FACE, and its letters as many times as wide: more than 1, at most 3 (5/4, 3/2, 2); NIL for one."
      (documentation 'face-inherit 'function) "The face FACE inherits unspecified attributes from, or NIL."
      (documentation 'face-base 'function) "The attributes FACE's definition gave it, as a property list; themes start from these.")

(defvar *faces* (make-hash-table :test 'eq :synchronized t)
  "All faces, keyed by name; read by the window's thread as it paints while the editor's changes them.")

(defvar *face-generation* 0
  "Incremented whenever a face changes, so that painters can drop cached translations.")

(defstruct (face-cache (:constructor make-face-cache ()) (:copier nil))
  "What a painter has worked out from face specs, kept until a face changes
\(FACE-CACHE-TABLE)."
  (entries (make-hash-table :test 'equal :synchronized t) :read-only t)
  (generation -1)
  (context nil))

(setf (documentation 'make-face-cache 'function)
      "A new, empty store for what a painter works out from face specs (FACE-CACHE-TABLE).")

(defun face-cache-table (cache &optional context)
  "The hash table of CACHE (MAKE-FACE-CACHE), keyed by face spec, emptied first
when a face has changed since it was last asked for, or CONTEXT (anything
else the values in it depend on, such as the desktop's colours) is not EQUAL
to that time's; a second value is true when it was emptied."
  (let ((entries (face-cache-entries cache))
        (generation *face-generation*))
    (if (and (= (face-cache-generation cache) generation)
             (equal (face-cache-context cache) context))
        (values entries nil)
        (progn (clrhash entries)
               (setf (face-cache-generation cache) generation
                     (face-cache-context cache) context)
               (values entries t)))))

(defun find-face (name)
  "The face called NAME, or NIL."
  (gethash name *faces*))

(defun set-face-attributes (name &key (doc nil doc-p) (fg nil fg-p) (bg nil bg-p) (bold nil bold-p)
                                      (italic nil italic-p) (underline nil underline-p) (underline-colour nil underline-colour-p) (reverse nil reverse-p)
                                      (dim nil dim-p) (extend nil extend-p) (font nil font-p) (height nil height-p)
                                      (inherit nil inherit-p))
  "Set the given attributes of face NAME, creating the face if needed; attributes
not given are left as they are. For themes and init files:
(set-face-attributes :comment :fg :cyan :italic nil)."
  (let ((face (or (find-face name) (setf (gethash name *faces*) (%make-face name)))))
    (when doc-p (setf (face-doc face) doc))
    (when fg-p (setf (face-fg face) fg))
    (when bg-p (setf (face-bg face) bg))
    (when bold-p (setf (face-bold face) bold))
    (when italic-p (setf (face-italic face) italic))
    (when underline-p (setf (face-underline face) underline))
    (when underline-colour-p (setf (face-underline-colour face) underline-colour))
    (when reverse-p (setf (face-reverse face) reverse))
    (when dim-p (setf (face-dim face) dim))
    (when extend-p (setf (face-extend face) extend))
    (when font-p (setf (face-font face) font))
    (when height-p (setf (face-height face) (and (realp height) (> height 1) (min (rationalize height) 3))))
    (when inherit-p (setf (face-inherit face) inherit))
    (incf *face-generation*)
    face))

(defvar *themes* (make-hash-table :test 'eq :synchronized t)
  "Every theme, keyed by name.")

(defvar *current-theme* nil
  "The name of the theme in force, or NIL for the faces as defined.")

(defstruct (theme (:constructor make-theme (name doc settings &key colourless palette faces-from)) (:copier nil))
  "A theme: NAME, DOC, and SETTINGS, a list of (FACE . ATTRIBUTES) where
ATTRIBUTES is a property list as for SET-FACE-ATTRIBUTES. A colour there may be
a role, a symbol such as GROUND or COMMENT, which the theme's PALETTE, a list of
(ROLE COLOUR), turns into a colour; a COLOUR in the palette may itself be
another role of it. FACES-FROM names a theme whose settings apply before this
one's own, with their roles taken from this theme's palette, and whose palette
gives the roles this one leaves out. A COLOURLESS theme removes every colour,
for terminals without colours: only bold, italic, underline, reverse video and
dim remain."
  (name nil :read-only t)
  (doc "" :read-only t)
  (settings '() :read-only t)
  (palette '() :read-only t)
  (faces-from nil :read-only t)
  (colourless nil :read-only t))

(setf (documentation 'theme-p 'function) "True when OBJECT is a THEME."
      (documentation 'theme-name 'function) "The theme's name, a symbol."
      (documentation 'theme-doc 'function) "What the theme looks like and is for."
      (documentation 'theme-settings 'function) "The theme's own face settings as written, as a list of (FACE . ATTRIBUTES), their colours perhaps roles."
      (documentation 'theme-palette 'function) "The theme's roles and their colours, as a list of (ROLE COLOUR)."
      (documentation 'theme-faces-from 'function) "The name of the theme whose face settings this one starts from, or NIL."
      (documentation 'theme-colourless 'function) "True when the theme removes every colour.")

(defun role-p (value)
  "True when VALUE, given as a colour in a theme, names a role of the theme's
palette: a symbol other than a keyword, NIL or T."
  (and value (symbolp value) (not (keywordp value)) (not (eq value t))))

(defun theme-source (theme)
  "The theme THEME takes its faces from, or NIL; an error when THEME names one
that is not defined."
  (let ((name (theme-faces-from theme)))
    (and name (or (find-theme name)
                  (error "Theme ~(~a~) takes its faces from ~(~a~), which is not a theme." (theme-name theme) name)))))

(defun palette-entry (theme role)
  "What ROLE (a symbol, matched by name) stands for in the palette of THEME or,
when THEME's leaves it out, of the theme it takes its faces from, and so on:
two values, the colour or role and T, or NIL and NIL when no palette names it."
  (loop repeat 16
        while theme
        do (let ((entry (find (symbol-name role) (theme-palette theme)
                              :key (lambda (entry) (symbol-name (first entry))) :test #'string=)))
             (when entry (return-from palette-entry (values (second entry) t)))
             (setf theme (theme-source theme))))
  (values nil nil))

(defun theme-role-colour (theme role)
  "The colour ROLE stands for in THEME, following a role whose colour is
another role; an error when no palette of THEME names it, or the roles go
round in a circle."
  (let ((value role))
    (loop repeat 16
          while (role-p value)
          do (multiple-value-bind (colour found) (palette-entry theme value)
               (unless found (error "Theme ~(~a~) gives no colour for the role ~(~a~)." (theme-name theme) value))
               (setf value colour)))
    (when (role-p value) (error "Theme ~(~a~): the role ~(~a~) leads round in a circle." (theme-name theme) role))
    value))

(defun theme-specs (theme &optional (face nil face-p))
  "THEME's face settings, as a list of (FACE . ATTRIBUTES) to apply in order:
those of the theme it takes its faces from first, then its own, every role
turned into its colour in THEME's palette. Only FACE's settings when FACE is
given. Linear in the settings times the palette."
  (labels ((settings (source depth)
             (let ((from (and (< depth 16) (theme-source source))))
               (append (and from (settings from (1+ depth)))
                       (if face-p
                           (remove face (theme-settings source) :key #'car :test-not #'eq)
                           (theme-settings source))))))
    (mapcar (lambda (setting)
              (cons (car setting)
                    (loop for (key value) on (cdr setting) by #'cddr
                          collect key
                          collect (if (and (member key '(:fg :bg :underline-colour)) (role-p value)) (theme-role-colour theme value) value))))
            (settings theme 0))))

(defun reset-face (face)
  "Return FACE to the attributes its definition gave it."
  (setf (face-fg face) nil (face-bg face) nil (face-bold face) nil (face-italic face) nil
        (face-underline face) nil (face-underline-colour face) nil (face-reverse face) nil (face-dim face) nil (face-extend face) nil
        (face-font face) nil (face-height face) nil (face-inherit face) nil)
  (apply #'set-face-attributes (face-name face) (face-base face))
  face)

(defun register-face (name doc attributes)
  "Define face NAME with DOC and ATTRIBUTES as its base, then reapply the
current theme's settings for it, so redefining a face keeps the theme."
  (let ((face (set-face-attributes name :doc doc)))
    (setf (face-base face) attributes)
    (reset-face face)
    (let ((theme (and *current-theme* (gethash *current-theme* *themes*))))
      (when theme
        (dolist (spec (theme-specs theme name))
          (apply #'set-face-attributes name (cdr spec)))))
    (incf *face-generation*)
    face))

(defmacro define-face (name doc &rest attributes)
  "Define face NAME (a keyword) used for what DOC says, with ATTRIBUTES as for
SET-FACE-ATTRIBUTES. These are the face's base, which themes start from. Every
face documents itself."
  (check-type doc string "a docstring saying what the face is used for")
  `(register-face ,name ,doc (list ,@attributes)))

(defun theme-takes-from-p (theme name)
  "True when THEME is the theme called NAME or takes its faces from it, directly or through others."
  (loop repeat 16
        while theme
        thereis (eq (theme-name theme) name)
        do (setf theme (and (theme-faces-from theme) (find-theme (theme-faces-from theme))))))

(defun register-theme (name doc settings &key colourless palette faces-from)
  "Record theme NAME with DOC and SETTINGS, its PALETTE of roles, the theme
FACES-FROM it starts from and COLOURLESS when it removes every colour, as
DEFINE-THEME describes them; an error when a role has no colour. Defining
again the theme in force, or one it takes its faces from, applies it again."
  (let ((theme (make-theme name doc settings :colourless colourless :palette palette :faces-from faces-from)))
    (theme-specs theme)
    (setf (gethash name *themes*) theme))
  (when (theme-takes-from-p (and *current-theme* (gethash *current-theme* *themes*)) name)
    (apply-theme *current-theme*))
  name)

(defmacro define-theme (name doc &body body)
  "Define theme NAME, documented by DOC, as face settings (FACE ATTRIBUTE VALUE
...), for example (:comment :fg \"#6b7385\" :italic t). A face a theme does not
mention keeps its base attributes; to turn off a base attribute, give it NIL
(:region :reverse nil :bg \"#3b4252\").

Options may come before the settings:
- :PALETTE ((ROLE COLOUR) ...) names the theme's colours by role, so that a
  setting can give a role in place of a colour, (:comment :fg comment); a
  COLOUR may be another role, (comment grey);
- :FACES-FROM THEME starts from the settings of THEME, their roles in this
  theme's palette, and takes from THEME's palette the roles this one leaves
  out: a light variant of a dark theme is its palette alone, and a theme of
  your own can be another with a role or two changed,
  (define-theme my-ao \"Ao on black.\" :faces-from ao-dark :palette ((ground \"#000000\")));
- :COLOURLESS makes a theme for terminals without colours: every colour is
  removed."
  (check-type doc string "a docstring saying what the theme looks like")
  (let ((colourless nil) (palette '()) (faces-from nil))
    (loop while (keywordp (first body))
          do (ecase (pop body)
               (:colourless (setf colourless t))
               (:palette (setf palette (pop body)))
               (:faces-from (setf faces-from (pop body)))))
    `(register-theme ',name ,doc
                     ',(mapcar (lambda (spec) (cons (first spec) (rest spec))) body)
                     :colourless ,colourless :palette ',palette :faces-from ',faces-from)))

(defvar *user-face-settings* '()
  "Face settings made by you (SET-USER-FACE, the init file's SET-FACE), as
a list of (FACE . ATTRIBUTES) in the order made; applied after any theme, so
your own choices always win.")

(defun set-user-face (name &rest attributes)
  "Set ATTRIBUTES of face NAME as your own choice: applied now, and again
after every theme change."
  (let ((entry (assoc name *user-face-settings*)))
    (if entry
        (setf (cdr entry) (append attributes (cdr entry)))
        (setf *user-face-settings* (append *user-face-settings* (list (cons name attributes))))))
  (apply #'set-face-attributes name attributes))

(defun theme-names ()
  "The names of all themes, sorted."
  (sort (loop for k being the hash-keys of *themes* collect k) #'string< :key #'symbol-name))

(defun find-theme (name)
  "The theme called NAME (a symbol from any package, matched by name, or a string), or NIL."
  (let ((target (if (symbolp name) (symbol-name name) (string-upcase name))))
    (loop for theme being the hash-values of *themes*
          when (string= (symbol-name (theme-name theme)) target) return theme)))

(defun apply-theme (name)
  "Make theme NAME the one in force: every face returns to its base, then the
theme's settings apply, then your own (*USER-FACE-SETTINGS*). NIL leaves
out the theme. Returns the theme's name."
  (let* ((theme (and name (or (find-theme name) (error "No theme named ~(~a~)." name))))
         (specs (and theme (theme-specs theme))))
    (maphash (lambda (face-name face) (declare (ignore face-name)) (reset-face face)) *faces*)
    (when theme
      (dolist (spec specs)
        (apply #'set-face-attributes (car spec) (cdr spec)))
      (when (theme-colourless theme)
        (maphash (lambda (face-name face)
                   (declare (ignore face-name))
                   (setf (face-fg face) nil (face-bg face) nil (face-underline-colour face) nil))
                 *faces*)))
    (dolist (setting *user-face-settings*)
      (apply #'set-face-attributes (car setting) (cdr setting)))
    (setf *current-theme* (and theme (theme-name theme)))
    (incf *face-generation*)
    *current-theme*))

(defun face-attribute (name attribute)
  "The value of ATTRIBUTE (:fg :bg :bold :italic :underline :underline-colour
:reverse :dim :extend :font or :height) for face NAME, following :inherit; NIL when no face
in the chain specifies it. The name of the face that gives it is a second value."
  (loop repeat 16                       ; guards against inheritance cycles
        for face = (find-face name)
        while face
        do (let ((value (ecase attribute
                          (:fg (face-fg face)) (:bg (face-bg face)) (:bold (face-bold face))
                          (:italic (face-italic face)) (:underline (face-underline face))
                          (:underline-colour (face-underline-colour face))
                          (:reverse (face-reverse face)) (:dim (face-dim face)) (:extend (face-extend face))
                          (:font (face-font face)) (:height (face-height face)))))
             (when value (return (values value name)))
             (setf name (face-inherit face)))
        finally (return nil)))

(defun combine-faces (span-face row-face)
  "The face spec for a span painted over a row: the span's faces first, then the row's."
  (if row-face
      (append (if (listp span-face) span-face (list span-face))
              (if (listp row-face) row-face (list row-face)))
      span-face))

(defun resolve-face-spec (spec)
  "Resolve SPEC (NIL, a face name, or a list of face names with the highest
priority first) to three values: the foreground colour, the background colour
(each from the highest-priority face that sets one, or NIL) and the list of
flags (:bold :italic :underline :reverse :dim) any of the faces turns on, with
:wave, :dotted or :box too when the highest-priority face underlining underlines so
\(RESOLVE-FACE-UNDERLINE-COLOUR gives its colour). The :default face always
takes part with the lowest priority, so a theme's text colours and background
reach everything."
  (let ((names (append (if (listp spec) spec (list spec)) (list :default))) (fg nil) (bg nil) (flags '()) (underline nil))
    (dolist (name names)
      (unless fg (setf fg (face-attribute name :fg)))
      (unless bg (setf bg (face-attribute name :bg)))
      (dolist (flag '(:bold :italic :underline :reverse :dim))
        (when (face-attribute name flag) (pushnew flag flags)))
      (unless underline (setf underline (face-attribute name :underline))))
    (when (member underline '(:wave :dotted :box)) (push underline flags))
    (values fg bg (nreverse flags))))

(defun resolve-face-underline-colour (spec)
  "The colour the highest-priority face of SPEC (as for RESOLVE-FACE-SPEC)
that underlines gives its underline, or NIL for the text's."
  (dolist (name (if (listp spec) spec (list spec)) nil)
    (when (and name (face-attribute name :underline))
      (return (face-attribute name :underline-colour)))))

(defvar *row-scaling* nil
  "True while the frontend draws rows of more than one row's height (a window
of its own, not a terminal): a face's HEIGHT then has a line drawn large.")

(defun resolve-face-font (spec)
  "The font (FACE-FONT) of the highest-priority face of SPEC (as for
RESOLVE-FACE-SPEC) that names one, or NIL for the window's own."
  (dolist (name (if (listp spec) spec (list spec)) nil)
    (let ((font (and name (face-attribute name :font))))
      (when font (return font)))))

;;; Colours for terminals

(defun parse-hex-colour (colour)
  "The red, green and blue components (0-255) of COLOUR, a string \"#rrggbb\", as three values; NIL when it is not one."
  (when (and (stringp colour) (= (length colour) 7) (char= (char colour 0) #\#))
    (let ((r (parse-integer colour :start 1 :end 3 :radix 16 :junk-allowed t))
          (g (parse-integer colour :start 3 :end 5 :radix 16 :junk-allowed t))
          (b (parse-integer colour :start 5 :end 7 :radix 16 :junk-allowed t)))
      (when (and r g b) (values r g b)))))

(defparameter *basic-palette*
  #((0 0 0) (205 0 0) (0 205 0) (205 205 0) (0 0 238) (205 0 205) (0 205 205) (229 229 229)
    (127 127 127) (255 0 0) (0 255 0) (255 255 0) (92 92 255) (255 0 255) (0 255 255) (255 255 255))
  "The usual RGB values of the sixteen basic terminal colours (xterm's defaults).")

(defun palette-colour (index)
  "The RGB components of colour INDEX of the xterm 256-colour palette, as a list."
  (cond ((< index 16) (aref *basic-palette* index))
        ((< index 232) (let* ((i (- index 16))
                              (steps #(0 95 135 175 215 255)))
                         (list (aref steps (floor i 36)) (aref steps (mod (floor i 6) 6)) (aref steps (mod i 6)))))
        (t (let ((level (+ 8 (* 10 (- index 232))))) (list level level level)))))

(defparameter *basic-colour-names*
  #(:black :red :green :yellow :blue :magenta :cyan :white
    :bright-black :bright-red :bright-green :bright-yellow :bright-blue :bright-magenta :bright-cyan :bright-white)
  "The names of the sixteen basic colours, in the order of the terminal palette.")

(defun face-attribute-in-theme (name attribute theme)
  "The value of ATTRIBUTE (as FACE-ATTRIBUTE takes it) of face NAME as THEME,
a theme or its name, would make it, following :inherit, without applying
THEME: the face's own definition, then THEME's settings for it, then yours
\(*USER-FACE-SETTINGS*). NIL when no face in the chain gives it, or for a
colour when THEME has none."
  (let ((theme (if (theme-p theme) theme (find-theme theme))))
    (unless (and theme (member attribute '(:fg :bg :underline-colour)) (theme-colourless theme))
      (loop repeat 16                       ; guards against inheritance cycles
            for face = (find-face name)
            while face
            do (let ((attributes (copy-list (face-base face))))
                 (when theme
                   (dolist (spec (theme-specs theme name))
                     (loop for (key value) on (cdr spec) by #'cddr do (setf (getf attributes key) value))))
                 (loop for (key value) on (cdr (assoc name *user-face-settings*)) by #'cddr
                       do (setf (getf attributes key) value))
                 (let ((value (getf attributes attribute)))
                   (when value (return value)))
                 (setf name (or (getf attributes :inherit) (face-inherit face))))
            finally (return nil)))))

(defun basic-colour-index (name)
  "The number in the terminal palette of the basic colour NAME (:red 1,
:bright-black 8), or NIL when NAME is none of the sixteen."
  (position name *basic-colour-names*))

(defun colour-components (colour)
  "COLOUR, as faces take colours (a name such as :red, an index into the 256
colours, or \"#rrggbb\"), as a list of its red, green and blue, each 0 to 255;
NIL when it is none of those."
  (typecase colour
    (string (multiple-value-bind (r g b) (parse-hex-colour colour) (and r (list r g b))))
    (integer (and (<= 0 colour 255) (palette-colour colour)))
    (keyword (let ((index (basic-colour-index colour)))
               (and index (palette-colour index))))))

(defun face-ground (face)
  "The ground FACE shows, as faces take colours: its background when it sets
one, else what it shows once reverse video is taken into account; NIL when
that is the window's or the terminal's own."
  (or (face-attribute face :bg)
      (multiple-value-bind (fg bg flags) (resolve-face-spec face)
        (if (member :reverse flags) fg bg))))

(defun cursor-colour (colour)
  "The colour of the cursor for COLOUR, as faces take colours: COLOUR itself,
or for NIL the :cursor face's ground (FACE-GROUND), the text's colour as
standard; NIL when that is the window's or the terminal's own."
  (or colour (face-ground :cursor)))

(defun colour-hex (colour)
  "COLOUR, as faces take colours (COLOUR-COMPONENTS) or as a list of its red,
green and blue, each 0 to 255, written #rrggbb; NIL when it is none."
  (let ((rgb (if (consp colour) colour (colour-components colour))))
    (and rgb (format nil "#~(~{~2,'0x~}~)" rgb))))

(defun mix-colours (from to fraction)
  "The colour FRACTION of the way from FROM to TO, both as faces take colours
\(COLOUR-COMPONENTS), written #rrggbb: 0 FROM itself, 1 TO; NIL when either
is no colour."
  (let ((a (colour-components from)) (b (colour-components to)))
    (and a b (colour-hex (mapcar (lambda (x y) (round (+ x (* fraction (- y x))))) a b)))))

(defun nearest-palette-index (colour count)
  "The index of the colour among the first COUNT colours of the xterm palette
(8, 16 or 256) nearest to COLOUR, a string \"#rrggbb\", by squared distance in RGB."
  (multiple-value-bind (r g b) (parse-hex-colour colour)
    (unless r (error "Not a colour of the form #rrggbb: ~s." colour))
    (let ((best 0) (best-distance most-positive-fixnum))
      (dotimes (i (min count 256) best)
        (destructuring-bind (pr pg pb) (palette-colour i)
          (let ((d (+ (expt (- r pr) 2) (expt (- g pg) 2) (expt (- b pb) 2))))
            (when (< d best-distance) (setf best i best-distance d))))))))
