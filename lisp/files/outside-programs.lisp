;;;; lisp/files/outside-programs.lisp --- The software from outside that features run, and how to install it.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; *OUTSIDE-PROGRAMS* names each program or library a feature needs, the
;;;; packages it comes in on Fedora, Debian and Arch, and M-x check-health
;;;; says from it what runs each feature here and what to install.

(in-package #:tuatara.defaults)

(defun program-health-words (what program missing &key (how "shut in"))
  "What a program does here, as \\[check-health] says it: WHAT by PROGRAM,
HOW (mailboxes by readpst, shut in), or with PROGRAM NIL, WHAT by none;
MISSING, what to do about it (install readpst)."
  (if program
      (format nil "~a by ~a~@[, ~a~]" what program how)
      (format nil "~a by none: ~a" what missing)))

(defparameter *outside-programs*
  '((:spelling :name "Enchant" :what "spelling"
     :found enchant-p
     :fedora ("enchant2") :debian ("libenchant-2-2") :arch ("enchant") :weight :recommends)
    (:sandbox :name "bubblewrap" :what "pictures, programs and grammars run shut in"
     :programs ("bwrap")
     :fedora ("bubblewrap") :debian ("bubblewrap") :arch ("bubblewrap") :weight :recommends)
    (:pictures :name "ImageMagick" :what "pictures"
     :programs ("magick")
     :fedora ("ImageMagick")
     :debian ("imagemagick")
     :arch ("imagemagick" "libjpeg-turbo" "libtiff" "libwebp" "libheif" "librsvg" "libraw" "openjpeg2" "libjxl")
     :weight :recommends)
    (:pdf :name "poppler" :what "PDF files"
     :programs ("pdftotext" "pdftoppm")
     :fedora ("poppler-utils") :debian ("poppler-utils") :arch ("poppler") :weight :recommends)
    (:git :name "git" :what "the git interface"
     :programs ("git")
     :fedora ("git-core") :debian ("git") :arch ("git") :weight :recommends)
    (:man :name "man-db and groff" :what "manual pages"
     :programs ("man" "groff")
     :fedora ("man-db" "groff-base") :debian ("man-db" "groff-base") :arch ("man-db" "groff") :weight :recommends)
    (:notify :name "libnotify" :what "notifications told on the desktop"
     :programs ("notify-send")
     :fedora ("libnotify") :debian ("libnotify-bin") :arch ("libnotify") :weight :recommends)
    (:archives :name "bsdtar" :what "zip, 7z, rar, ISO 9660 and cpio archives"
     :programs ("bsdtar")
     :fedora ("bsdtar") :debian ("libarchive-tools") :arch ("libarchive") :weight :recommends)
    (:seven-zip :name "7-Zip" :what "encrypted 7z and rar, more zip methods"
     :programs (("7zz" "7z" "7za"))
     :fedora ("7zip") :debian ("7zip") :arch ("7zip") :weight :recommends)
    (:gpg :name "GnuPG" :what "encrypted files"
     :programs ("gpg")
     :fedora ("gnupg2") :debian ("gnupg") :arch ("gnupg") :weight :suggests)
    (:open :name "xdg-utils" :what "files opened in the desktop programs"
     :programs ("xdg-open")
     :fedora ("xdg-utils") :debian ("xdg-utils") :arch ("xdg-utils") :weight :suggests)
    (:office :name "LibreOffice" :what "pages of Office documents"
     :programs ("soffice")
     :fedora ("libreoffice-writer" "libreoffice-calc" "libreoffice-impress")
     :debian ("libreoffice-writer" "libreoffice-calc" "libreoffice-impress")
     :arch ("libreoffice-fresh")
     :weight :suggests)
    (:latex :name "TeX Live" :what "PDFs of Org and LaTeX files"
     :found tuatara.export:latex-engines-found
     :fedora ("texlive-xetex" "texlive-luatex")
     :debian ("texlive-xetex" "texlive-luatex")
     :arch ("texlive-xetex" "texlive-luatex")
     :weight :suggests))
  "The software from outside that Tuatara's features run, each (KEY . PLIST):
:NAME what it is called, :WHAT what it is for, the packages it comes in on
Fedora, on Debian and Ubuntu, and on Arch (:FEDORA, :DEBIAN, :ARCH), and
whether Tuatara's own packages recommend it or only suggest it (:WEIGHT).
:PROGRAMS lists the programs it is found by, each a name or a list of names
of which any one will do (HEALTH-NEEDS-MISSING); :FOUND, a function of no
arguments returning true when it is there, stands in for :PROGRAMS for what
is not a program.
Arch's ImageMagick reads most kinds of picture only with their libraries
installed beside it (JPEG, TIFF, WebP, HEIF and AVIF, SVG, camera raws,
JPEG 2000, JPEG XL: the Arch packages of *IMAGE-FORMAT-LIBRARIES*), so they
are named with it.
\\[check-health] says what to install from it; tools/distro-packages.sh names
the same packages.")

(defparameter *system-programs*
  '((:ssh :name "OpenSSH and setpriv" :what "a daemon or files on another machine"
     :programs ("ssh" "setpriv")
     :fedora ("openssh-clients" "util-linux") :debian ("openssh-client" "util-linux") :arch ("openssh" "util-linux")
     :weight :suggests))
  "Programs of the system that Tuatara uses and its packages do not pull in,
as *OUTSIDE-PROGRAMS* lists the software that they do.")

(defun outside-program (key)
  "The entry of *OUTSIDE-PROGRAMS* or *SYSTEM-PROGRAMS* for KEY, a property
list; an error when there is none."
  (or (rest (or (assoc key *outside-programs*) (assoc key *system-programs*)))
      (refuse "No outside program is known as ~s" key)))

(defun system-package-family ()
  "Which packages this system takes, by its /etc/os-release: :FEDORA,
:DEBIAN (Ubuntu too) or :ARCH; NIL for another or when it does not say."
  (let ((text (ignoring-errors-reported ("reading /etc/os-release")
                (tuatara.io:file-text "/etc/os-release" :limit 65536))))
    (when text
      (let ((ids (loop for line in (uiop:split-string text :separator '(#\Newline))
                       when (or (uiop:string-prefix-p "ID=" line) (uiop:string-prefix-p "ID_LIKE=" line))
                         append (tuatara.text:split-words (string-trim "\"'" (subseq line (1+ (position #\= line))))))))
        (cond ((intersection ids '("fedora" "rhel" "centos") :test #'string=) :fedora)
              ((intersection ids '("debian" "ubuntu") :test #'string=) :debian)
              ((intersection ids '("arch") :test #'string=) :arch))))))

(defun outside-program-install-words (key &optional (family (system-package-family)))
  "What to install for the outside program KEY (*OUTSIDE-PROGRAMS*), in words:
install its name, with its packages on this system (FAMILY, as
SYSTEM-PACKAGE-FAMILY gives it), or on each system when it is not known."
  (let ((entry (outside-program key)))
    (format nil "install ~a (~{~a~^; ~})" (getf entry :name)
            (loop for (system label) in '((:fedora "Fedora") (:debian "Debian and Ubuntu") (:arch "Arch"))
                  when (or (null family) (eq family system))
                    collect (format nil "~a: ~{~a~^ ~}" label (getf entry system))))))

(defun outside-program-health-words (key what program &key (how "shut in"))
  "What the outside program KEY (*OUTSIDE-PROGRAMS*) does here, as
\\[check-health] says it (PROGRAM-HEALTH-WORDS): WHAT by PROGRAM, its path
found or NIL, HOW, or by none, what to install."
  (program-health-words what program (outside-program-install-words key) :how how))

(defun git-health-words ()
  "What runs git here, as \\[check-health] says it."
  (outside-program-health-words :git "repositories, their changes and history" (tuatara.io:find-program "git")
                                :how "never through a shell, only for projects trusted"))
