;;;; lisp/export/package.lisp --- Package definition for tuatara.export.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

;; UIOP's define-package, as the names taken from the defaults may be defined
;; in files loaded after this one: each is made in TUATARA.DEFAULTS here, the
;; symbol that file then defines
(uiop:define-package #:tuatara.export
  (:use #:cl #:tuatara.command #:tuatara.modes #:tuatara.dates #:tuatara-user)
  (:import-from #:tuatara.runtime #:jobs-of #:ignoring-errors-reported #:refusal #:refuse #:*secret-at-hand*
                #:define-setting)
  (:import-from #:tuatara.text #:word-char-p)
  (:import-from #:tuatara.defaults
                #:*built-in-latex-templates* #:*export-languages* #:*org-macro-call-re*
                #:*pdf-views*
                #:buffer-in-mode #:call-with-details-shown #:commands-for-mode #:copy-chosen-own-file #:docx-import #:docx-templates-directory
                #:drop-selection #:end-safe-visit-words #:entry-lines #:entry-list-keys #:entry-place #:entry-places
                #:evidence-refusal
                #:export-directory-p #:export-language #:export-trusted-path-p #:file-kind #:file-sha256
                #:forget-old-copies
                #:formatted-template-text #:goto-shown #:latex-import #:latex-mode #:latex-templates-directory
                #:logos-directory #:new-empty-buffer #:note-templates-directory #:octets-sha256 #:office-file-format
                #:office-reading
                #:open-externally-file #:org-buffer-parse #:org-current-headline #:org-docx-document
                #:org-export-file-name #:org-export-path #:org-headline-property #:org-hidden-kinds #:org-hiding-kind
                #:map-org-hiding-calls
                #:org-insert-export-drawer #:org-latex-document #:org-macro-definitions #:org-mode #:org-node-begin
                #:org-html-document #:org-node-end #:org-node-post-blank #:org-node-type #:org-parse-char
                #:org-parse-point
                #:org-parse-text #:org-parse-tree #:org-path-beside-p #:org-property #:org-text-lines
                #:org-walk-tree #:outside-program-install-words #:path-in-directories-p #:pdf-view-maker #:pdf-view-of
                #:pdf-view-path #:pdf-view-reload #:pdf-view-visit #:private-cache-directory #:read-native-path
                #:region-active-p #:render-pieces #:replaced-type #:save-buffer-with-care #:selection-range
                #:set-search-filtering #:show-file-line #:special-mode #:title-slug #:use-places #:visit-entry #:visit-path
                #:with-org-note-keywords
                #:*pdf-view-keymap* #:*synctex-largest* #:mouse-key-cell #:mouse-select-word #:pdf-page-boxes
                #:pdf-view-mode #:pdf-view-place-at #:pdf-view-place-shown #:pdf-view-show-place #:pdf-view-task
                #:pdf-view-text-p #:read-synctex #:synctex-line #:synctex-place #:synctex-path #:synctex-octets #:page-size
                #:pdf-view-info
                #:formula #:formula-key #:formula-text #:formula-preamble #:formula-inline #:picture-source-size
                #:picture-source-image #:picture-source-words #:*inline-picture-uses* #:forget-pictures-past
                #:decoding-job #:*decoding-carried-settings* #:find-pdf-decoder #:decode-pdf-page)
  (:documentation "Org notes and LaTeX files made into PDFs, Word documents and web pages in the
background, with the templates, the redactions checked and the macros that
mark them. The rest of the editor installs it and asks, for the health
report, what the LaTeX engines and languages are here.")
  (:export #:install-latex-export #:latex-export-words #:latex-language-words
           #:latex-engines-found))
