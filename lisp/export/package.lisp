;;;; lisp/export/package.lisp --- Package definition for tuatara.export.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

;; UIOP's define-package, as the names taken from the defaults may be defined
;; in files loaded after this one: each is made in TUATARA.DEFAULTS here, the
;; symbol that file then defines
(uiop:define-package #:tuatara.export
    (:use #:cl #:tuatara.command #:tuatara.modes #:tuatara.dates #:tuatara-user)
  (:import-from #:tuatara.runtime #:define-record #:jobs-of #:ignoring-errors-reported #:refusal #:refuse
                #:*secret-at-hand* #:define-setting)
  (:import-from #:tuatara.text #:name-char-p)
  (:import-from #:tuatara.defaults
                #:buffer-in-mode #:*built-in-latex-templates* #:call-with-details-shown #:commands-for-mode
                #:copy-chosen-own-file #:decode-pdf-page #:*decoding-carried-settings* #:decoding-job #:docx-import
                #:docx-templates-directory #:drop-selection #:end-safe-visit-words #:entry-lines #:entry-list-keys
                #:entry-place #:entry-places #:evidence-refusal #:export-directory-p #:export-language
                #:*export-languages* #:export-trusted-path-p #:file-kind #:file-sha256 #:find-pdf-decoder
                #:forget-old-copies #:forget-pictures-past #:formatted-template-text #:formula #:formula-inline
                #:formula-key #:formula-preamble #:formula-text #:*inline-picture-uses*
                #:keeping-list-selected #:latex-import #:latex-mode #:latex-templates-directory #:logos-directory
                #:map-org-hiding-calls #:mouse-key-cell #:mouse-select-word #:new-empty-buffer
                #:note-templates-directory #:octets-sha256 #:office-file-format #:office-reading #:open-externally-file
                #:org-buffer-parse #:org-current-headline #:org-docx-document #:org-export-file-name #:org-export-path
                #:org-headline-property #:org-hidden-kinds #:org-hiding-kind #:org-html-document
                #:org-insert-export-drawer #:org-latex-parts #:*org-macro-call-re* #:org-macro-definitions
                #:org-mode #:org-node-begin #:org-node-end #:org-node-post-blank #:org-node-type #:org-parse-char
                #:org-parse-point #:org-parse-text #:org-parse-tree #:org-property #:org-text-lines
                #:org-walk-tree #:outside-program-install-words #:page-size #:pdf-page-boxes
                #:pdf-view-info #:*pdf-view-keymap* #:pdf-view-maker #:pdf-view-mode #:pdf-view-of #:pdf-view-path
                #:pdf-view-place-at #:pdf-view-place-shown #:pdf-view-reload #:pdf-view-show-place #:pdf-view-task
                #:pdf-view-text-p #:pdf-view-visit #:*pdf-views* #:picture-source-image #:picture-source-size
                #:picture-source-words #:private-cache-directory #:read-native-path #:read-synctex #:region-active-p
                #:render-pieces #:replaced-type #:save-buffer-with-care #:selection-range #:set-search-filtering
                #:show-buffer-position #:show-file-line #:special-mode #:*synctex-largest* #:synctex-line
                #:synctex-octets #:synctex-path #:synctex-place #:title-slug #:use-places #:visit-entry
                #:with-org-note-keywords)
  (:documentation "Org notes and LaTeX files made into PDFs, Word documents and web pages in the
background, with the templates, the redactions checked and the macros that
mark them. The rest of the editor installs it and asks, for the health
report, what the LaTeX engines and languages are here.")
  (:export #:install-latex-export #:latex-export-words #:latex-language-words
           #:latex-engines-found))
