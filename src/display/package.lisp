;;;; src/display/package.lisp --- Package definition for tuatara.display.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(defpackage #:tuatara.display
  (:use #:cl #:alexandria)
  (:import-from #:tuatara-user #:*window-min-height* #:*window-min-width* #:*scroll-margin* #:*scroll-recenter*)
  (:documentation "The layout engine producing glyph rows from snapshots and window state; faces, fonts, rich cells, windows and frames.")
  (:export #:*direction-controls* #:in-ranges-p #:fit-columns #:pad-columns #:shorten-string #:fill-words #:*screen-column-reach*)
  (:export #:char-columns #:cluster-columns #:mark-code-p #:*mark-base* #:string-columns #:string-column-starts #:chars-in-columns #:next-tab-stop #:char-display #:control-char-display #:raw-byte-display #:*invisible-characters* #:invisible-code-p #:invisible-code-display #:*invisible-characters-function* #:invisible-characters-of
           #:row #:row-p #:row-string #:row-bytes #:row-start #:row-end #:row-columns #:row-continuation-p #:row-last-p #:row-shown-end #:row-truncated-p
           #:row-column-of #:buffer-folds #:fold-region #:unfold-region #:unfold-ranges #:unfold-all #:fold-at #:fold-hiding #:fold-from #:fold-before #:folds-cut #:folds-hiding #:hold-folds #:release-folds #:folds-now #:hide-ranges #:hidden-bounds #:hidden-touched-p #:buffer-hides-p #:hidden-edit-p #:shown-parts #:shown-ranges #:lines-left-out #:visible-line-start #:visible-line-end #:point-out-of-folds #:*fold-ellipsis* #:row-faces #:row-char-bytes #:row-spans #:row-extend #:merge-face #:apply-highlights #:layout #:point-row-column
           #:face #:define-face #:find-face #:set-face-attributes #:resolve-face-spec #:face-ground #:cursor-colour #:resolve-face-underline-colour #:combine-faces #:resolve-face-font #:*row-scaling* #:row-scale #:face-attribute #:*faces* #:*face-generation* #:make-face-cache #:face-cache-table
           #:face-doc #:face-fg #:face-bg #:face-bold #:face-italic #:face-underline #:face-reverse #:face-dim #:face-extend #:face-font #:face-inherit
           #:face-base #:face-name #:reset-face #:register-face
           #:theme #:define-theme #:register-theme #:apply-theme #:find-theme #:theme-names #:*themes* #:*current-theme*
           #:theme-name #:theme-doc #:theme-specs #:theme-settings #:theme-palette #:theme-faces-from #:theme-colourless #:theme-role-colour #:face-attribute-in-theme #:theme-source #:role-p
           #:parse-hex-colour #:colour-hex #:mix-colours #:displayable #:colour-components #:basic-colour-index #:nearest-palette-index #:palette-colour #:set-user-face #:*user-face-settings*
           #:reader #:make-reader #:reader-next #:reader-position
           #:window #:make-window #:window-buffer #:window-start #:call-keeping-window-starts #:call-keeping-window-lines #:window-width #:window-height #:window-wrap
           #:window-hscroll #:line-prefix-columns #:*layout-probe* #:window-goal-column #:window-rows #:window-margin #:window-layout #:window-point-position #:large-column #:window-line-scale #:window-text-width #:window-rows-drawn
           #:window-ensure-point-visible #:step-lines #:window-scroll #:window-recenter #:display-column #:code-columns #:screen-slice-keys #:buffer-slice-keys #:buffer-screen-column #:buffer-move-to-screen-column #:buffer-screen-slice #:scroll-lines
           #:*tab-width-function* #:sensible-tab-width #:tab-width-of #:*soft-wrap-function* #:soft-wrap-of #:window-wraps-p
           #:window-layout-rows #:window-row-start #:rows-back
           #:window-point #:window-point-anchor #:window-start-anchor #:window-divider #:window-use-time #:window-full-height
           #:window-header-rows #:window-text-top
           #:window-kept #:window-quit-restore #:window-history #:*window-history-length*
           #:set-window-buffer #:remember-window-buffer #:forget-window-buffer #:window-state #:set-window-state
           ;; the layout of windows
           #:area #:area-parent #:area-share #:area-left #:area-top #:area-width #:area-height
           #:area-root #:area-windows #:area-extent #:area-least #:arrangement-of
           #:split #:split-arrangement #:split-children #:*window-min-height* #:*window-min-width*
           #:layout-area #:trim-area-bottom #:split-area #:replace-area #:delete-area #:window-in-direction
           #:turn-layout #:balance-area #:resize-area
           ;; the hex view
           #:hex-view-p #:hex-point #:hex-layout #:hex-row-start #:+hex-row+ #:row-mixed))
