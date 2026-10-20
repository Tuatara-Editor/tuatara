;;;; test/hex.lisp --- The hex view, and SHA-256 sums.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test hex-and-sums
  :description "M-x hex-mode shows the octets as a hex dump and moves, goes and types by octets; M-x sha256-sum sums the file on disk, the selection or the text, in the background when large.")

(defparameter *hex-sample* (format nil "Hello, world!~%caf~a!" (code-char #xe9))
  "Text for the hex view: ASCII, a newline and a two-octet character (é is c3 a9).")

(defun session-hex-point (editor)
  "The hex view's cursor in the editor's current buffer."
  (tuatara.display:hex-point (session-buffer editor)))

(define-test (hex-and-sums hex-layout)
  (multiple-value-bind (f editor) (run-keys "M-x h e x - m o d e RET" :text *hex-sample* :width 80)
    (true (tuatara.display:hex-view-p (session-buffer editor)))
    (is string= "00000000  48 65 6c 6c 6f 2c 20 77  6f 72 6c 64 21 0a 63 61  Hello, world!.ca"
        (string-right-trim " " (tuatara.frontend.fake:fake-screen-line f 0)))
    (is string= "00000010  66 c3 a9 21                                       f..!"
        (string-right-trim " " (tuatara.frontend.fake:fake-screen-line f 1)))
    (true (search "Hex 0x00000000" (tuatara.frontend.fake:fake-screen-line f 6)))
    ;; showing the octets changes nothing
    (false (tuatara.buffer:buffer-modified-p (session-buffer editor))))
  ;; and back to text
  (multiple-value-bind (f editor) (run-keys "M-x h e x - m o d e RET M-x h e x - m o d e RET" :text *hex-sample* :width 80)
    (false (tuatara.display:hex-view-p (session-buffer editor)))
    (is string= "Hello, world!" (string-right-trim " " (tuatara.frontend.fake:fake-screen-line f 0)))))

(define-test (hex-and-sums hex-motion)
  ;; NORMAL state's h j k l, g h, g l, g g and g e move by octets and rows of sixteen
  (flet ((hex-after (keys)
           (session-hex-point (nth-value 1 (run-keys (format nil "M-x h e x - m o d e RET ~a" keys) :text *hex-sample* :width 80)))))
    (is = 3 (hex-after "l l l"))
    (is = 16 (hex-after "j"))
    (is = 18 (hex-after "j l l"))       ; the second octet of é
    (is = 17 (hex-after "j l l h"))
    (is = 2 (hex-after "j l l k"))
    (is = 15 (hex-after "g l"))
    (is = 16 (hex-after "j l l g h"))
    (is = 20 (hex-after "g e"))
    (is = 0 (hex-after "g e g g")))
  ;; point stays on the character the octet is part of
  (multiple-value-bind (f editor) (run-keys "M-x h e x - m o d e RET j l l" :text *hex-sample* :width 80)
    (declare (ignore f))
    (is = 17 (session-point editor))
    (is = 18 (session-hex-point editor)))
  ;; a screen at a time: the cursor moves with the rows, and back
  (let ((long (make-string 1000 :initial-element #\a)))
    (multiple-value-bind (f editor) (run-keys "M-x h e x - m o d e RET l C-v" :text long :width 80)
      (true (> (session-hex-point editor) 16))
      (is = 1 (mod (session-hex-point editor) 16))
      (true (search (format nil "~(~8,'0x~)" (tuatara.display:hex-row-start (session-hex-point editor)))
                    (with-output-to-string (s) (dotimes (r 6) (write-line (tuatara.frontend.fake:fake-screen-line f r) s))))))
    (is = 1 (session-hex-point (nth-value 1 (run-keys "M-x h e x - m o d e RET l C-v M-v" :text long :width 80)))))
  ;; M-g g asks for an offset: 0x for hex, else decimal
  (is = 16 (session-hex-point (nth-value 1 (run-keys "M-x h e x - m o d e RET M-g g 0 x 1 0 RET" :text *hex-sample* :width 80))))
  (is = 5 (session-hex-point (nth-value 1 (run-keys "M-x h e x - m o d e RET M-g g 5 RET" :text *hex-sample* :width 80))))
  (is = 20 (session-hex-point (nth-value 1 (run-keys "M-x h e x - m o d e RET M-g g 9 9 9 RET" :text *hex-sample* :width 80)))))

(define-test (hex-and-sums hex-editing)
  (flet ((octets-after (keys)
           (let ((editor (nth-value 1 (run-keys (format nil "M-x h e x - m o d e RET ~a" keys) :text *hex-sample* :width 80))))
             (values (tuatara.text:rope-octets (tuatara.buffer:buffer-text (session-buffer editor)))
                     (session-hex-point editor)))))
    ;; in the hex digits, typing writes hex digits: two an octet, then on to the next
    (multiple-value-bind (octets p) (octets-after "i 4 a 5 9 C-g")
      (is equalp (to-octets (format nil "JYllo, world!~%caf~a!" (code-char #xe9))) octets)
      (is = 2 p))
    ;; an octet typed in hex digits is one step to undo, both its halves, though the second changes nothing
    (is equalp (to-octets *hex-sample*) (octets-after "i 4 8 C-g u"))
    (is equalp (to-octets *hex-sample*) (octets-after "i 4 1 C-g u"))
    ;; and an INSERT session as a whole, as in any text: both octets typed go with one u
    (is equalp (to-octets *hex-sample*) (octets-after "i 4 1 4 1 C-g u"))
    ;; one digit: the octet's high half written, the cursor still on it, on its low half
    (multiple-value-bind (octets p) (octets-after "i f C-g")
      (is = #xf8 (aref octets 0))
      (is = 0 p))
    ;; what is no hex digit writes nothing there, and says so
    (is equalp (to-octets *hex-sample*) (octets-after "i z C-g"))
    ;; TAB: typing writes characters, in the text beside the digits; TAB again, back to digits
    (multiple-value-bind (octets p) (octets-after "i TAB J Y C-g")
      (is equalp (to-octets (format nil "JYllo, world!~%caf~a!" (code-char #xe9))) octets)
      (is = 2 p))
    (is equalp (to-octets (format nil "JYllo, world!~%caf~a!" (code-char #xe9))) (octets-after "TAB i J TAB 5 9 C-g"))
    ;; over the second octet of é: the first stays, a raw byte now
    (is equalp (concatenate '(vector (unsigned-byte 8)) (to-octets (format nil "Hello, world!~%caf")) #(#xc3 #x41 #x21))
        (octets-after "j l l i TAB A C-g"))
    ;; past the end they are added
    (is equalp (concatenate '(vector (unsigned-byte 8)) (to-octets *hex-sample*) (to-octets "xy"))
        (octets-after "g e i TAB x y C-g"))
    ;; hex-set-byte writes one octet given in hex
    (is equalp (concatenate '(vector (unsigned-byte 8)) #(#xff) (subseq (to-octets *hex-sample*) 1))
        (octets-after "M-x h e x - s e t - b y t e RET f f RET"))
    ;; one undo takes an overwrite back
    (is equalp (to-octets *hex-sample*) (octets-after "M-x h e x - s e t - b y t e RET f f RET u")))
  ;; the octet at the cursor marked in its hex digits and its character both; the cursor on
  ;; its first digit, its second once one is typed, its character after TAB
  (flet ((look (keys)
           (let ((f (run-keys (format nil "M-x h e x - m o d e RET ~a" keys) :text *hex-sample* :width 80)))
             (values (loop for (start end face) in (aref (tuatara.frontend.fake:fake-spans f) 0)
                           when (eq face :hex-mirror) collect (list start end))
                     (tuatara.frontend.fake:fake-cursor f)))))
    (multiple-value-bind (marked cursor) (look "l")
      ;; e at offset 1: its digits 65 at columns 13-14, its character at 61
      (is equal '((13 15) (61 62)) marked)
      (is equal '(0 . 13) cursor))
    (is equal '(0 . 14) (nth-value 1 (look "l i 6")))
    (is equal '(0 . 61) (nth-value 1 (look "l TAB"))))
  ;; with line numbers on, the hex view has none: no colour of theirs over its offsets, which the
  ;; terminal wrote twice, the rows a column to the right of the cursor
  (let ((tuatara-user:*line-numbers-default* :relative))
    (let ((f (run-keys "M-x h e x - m o d e RET l l l j" :text *hex-sample* :width 80)))
      (false (find :current-line-number (aref (tuatara.frontend.fake:fake-spans f) 1) :key #'third))
      (false (find :line-number (aref (tuatara.frontend.fake:fake-spans f) 0) :key #'third))
      (is equal '(1 . 19) (tuatara.frontend.fake:fake-cursor f))))
  ;; a read-only buffer is not written
  (multiple-value-bind (f editor) (run-keys "C-x C-q M-x h e x - m o d e RET i 4 1 C-g" :text *hex-sample* :width 80)
    (declare (ignore f))
    (is string= *hex-sample* (session-text editor))))

(define-test (hex-and-sums hex-view-keeps-size)
  ;; the keys that would insert or delete octets do nothing in the hex view, and say why
  (dolist (keys '("l d" "l o C-g" "l O C-g" "l i RET C-g" "y p" "l y p" "l D" "l c C-g" "C-k" "C-d" "l i DEL C-g"))
    (multiple-value-bind (f editor) (run-keys (format nil "M-x h e x - m o d e RET ~a" keys) :text *hex-sample* :width 80)
      (is string= *hex-sample* (session-text editor) "~a changes nothing" keys)
      (false (tuatara.buffer:buffer-modified-p (session-buffer editor)) "~a leaves the buffer unmodified" keys)
      (true (tuatara.display:hex-view-p (session-buffer editor)))
      (when (member keys '("l d" "y p" "l D") :test #'string=)
        (true (tuatara.frontend.fake:fake-frame-showing f "The hex view types over octets") "~a says why" keys))))
  ;; as the buffer is edited from anywhere: refused while the hex view is on, not after
  (let ((b (tuatara.buffer:make-buffer "*hex size*" :text "abc")))
    (unwind-protect
         (progn
           (setf (tuatara.buffer:buffer-local b :hex-view) t)
           (fail (tuatara.buffer:buffer-insert b 1 "x"))
           (fail (tuatara.buffer:buffer-delete b 0 1))
           (is = 2 (tuatara.buffer:buffer-replace b 1 2 "Z"))
           (is string= "aZc" (tuatara.buffer:buffer-string b))
           (setf (tuatara.buffer:buffer-local b :hex-view) nil)
           (tuatara.buffer:buffer-insert b 3 "d")
           (is string= "aZcd" (tuatara.buffer:buffer-string b)))
      (tuatara.buffer:kill-buffer b)))
  ;; typing over octets, its undo and redo go on as before
  (multiple-value-bind (f editor) (run-keys "M-x h e x - m o d e RET i 4 1 C-g u U" :text *hex-sample* :width 80)
    (declare (ignore f))
    (is string= (format nil "Aello, world!~%caf~a!" (code-char #xe9)) (session-text editor)))
  ;; RET in NORMAL state does nothing, not even Org's ticking of a checkbox
  (multiple-value-bind (f editor) (run-keys "M-x o r g - m o d e RET M-x h e x - m o d e RET RET"
                                            :text (format nil "- [ ] task~%") :width 80)
    (declare (ignore f))
    (is string= (format nil "- [ ] task~%") (session-text editor))))

(define-test (hex-and-sums octets-shown-in-hex)
  ;; octets made by a command, in a buffer of their own: read only, in the hex view; shown again, replaced
  (let* ((shown nil)
         (buffer (tuatara.defaults::show-octets-in-hex "*hex octets*" (coerce #(0 1 2 255) 'tuatara.text:octets)
                                                       :show (lambda (b) (setf shown b)))))
    (unwind-protect
         (progn
           (is eq buffer shown)
           (true (tuatara.display:hex-view-p buffer))
           (is = 0 (tuatara.display:hex-point buffer))
           (true (tuatara.buffer:buffer-local buffer :read-only))
           (is equalp #(0 1 2 255) (tuatara.text:rope-octets (tuatara.buffer:buffer-text buffer)))
           (let ((again (tuatara.defaults::show-octets-in-hex "*hex octets*" (coerce #(9) 'tuatara.text:octets) :show #'identity)))
             (false (tuatara.buffer:live-buffer-p buffer))
             (setf buffer again)
             (is = 1 (tuatara.buffer:buffer-size again))))
      (tuatara.buffer:kill-buffer buffer))))

(define-test (hex-and-sums hex-search)
  (flet ((found (keys)
           (multiple-value-bind (f editor) (run-keys keys :text *hex-sample* :width 80)
             (values (session-hex-point editor) f))))
    ;; forward from the cursor: "o" is 6f, "wo" is 77 6f
    (is = 4 (found "M-x h e x - m o d e RET M-x h e x - s e a r c h - f o r w a r d RET 6 f RET"))
    (is = 8 (found "M-x h e x - m o d e RET M-x h e x - s e a r c h - f o r w a r d RET 6 f RET M-x h e x - s e a r c h - f o r w a r d RET 6 f RET"))
    (is = 7 (found "M-x h e x - m o d e RET M-x h e x - s e a r c h - f o r w a r d RET 7 7 SPC 6 f RET"))
    ;; octets that are not UTF-8 on their own: the second of é and the !
    (is = 18 (found "M-x h e x - m o d e RET M-x h e x - s e a r c h - f o r w a r d RET a 9 2 1 RET"))
    ;; backward from the end
    (is = 8 (found "M-x h e x - m o d e RET g e M-x h e x - s e a r c h - b a c k w a r d RET 6 f RET"))
    ;; from text, a find shows the hex view
    (multiple-value-bind (p f) (found "M-x h e x - s e a r c h - f o r w a r d RET c 3 RET")
      (is = 17 p)
      (true (tuatara.frontend.fake:fake-frame-showing f "Found at 0x11")))
    (multiple-value-bind (p f) (found "M-x h e x - m o d e RET M-x h e x - s e a r c h - f o r w a r d RET 0 0 RET")
      (is = 0 p)
      (true (tuatara.frontend.fake:fake-frame-showing f "Not found")))
    (true (nth-value 1 (found "M-x h e x - m o d e RET M-x h e x - s e a r c h - f o r w a r d RET 6 RET")))))

(defparameter *abc-sum* "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
  "The SHA-256 sum of abc (FIPS 180-2's first example).")

(define-test (hex-and-sums sha256)
  ;; a buffer with no file: its whole text; the sum is shown, kept and put on the kill ring
  (multiple-value-bind (f editor) (run-keys "M-x s h a 2 5 6 - s u m RET" :text "abc" :width 140)
    (true (tuatara.frontend.fake:fake-frame-showing f "SHA-256 of *scratch*'s text (3 octets): " *abc-sum*))
    (is string= *abc-sum* (first (tuatara.command:editor-kill-ring editor)))
    (true (search *abc-sum* (tuatara.buffer:buffer-string (tuatara.buffer:find-buffer "*Messages*")))))
  ;; the selection only
  (multiple-value-bind (f editor) (run-keys "C-SPC C-f C-f C-f M-x s h a 2 5 6 - s u m RET" :text "abcdef" :width 140)
    (declare (ignore editor))
    (true (tuatara.frontend.fake:fake-frame-showing f "SHA-256 of the selection (3 octets): " *abc-sum*)))
  ;; a file: as it is on disk, whatever the buffer holds
  (with-test-directory (dir)
    (let ((path (put-file (merge-pathnames "evidence.bin" dir) "abc")))
      (multiple-value-bind (f editor) (run-keys "i x C-g M-x s h a 2 5 6 - s u m RET" :file path :width 160)
        (true (tuatara.frontend.fake:fake-frame-showing f "evidence.bin on disk, not the buffer's unsaved changes: " *abc-sum*))
        (tuatara.buffer:kill-buffer (session-buffer editor)))
      (is equal '("evidence.bin") (directory-names dir))))
  ;; large: in the background, and still right
  (let ((tuatara.defaults::*hash-wait* 0)
        (text (make-string 3000000 :initial-element #\a)))
    (multiple-value-bind (f editor) (run-keys "M-x s h a 2 5 6 - s u m RET" :text text :width 160)
      (declare (ignore editor))
      (true (tuatara.frontend.fake:fake-frame-showing f "Summing *scratch*'s text (3,000,000 octets) in the background"))
      (true (tuatara.frontend.fake:fake-frame-showing f "SHA-256 of *scratch*'s text (3,000,000 octets): "
                                                      "2a152c894398719c0570f83fac34ac03a0f6e8e474b995c2403aa5434f7b9dd4")))))

(define-test (hex-and-sums sha256-program)
  ;; without the library, the program sums, and gets the same
  (let ((tuatara.defaults::*hash-library* nil))
    (multiple-value-bind (f editor) (run-keys "M-x s h a 2 5 6 - s u m RET" :text "abc" :width 140)
      (declare (ignore editor))
      (true (tuatara.frontend.fake:fake-frame-showing f "SHA-256 of *scratch*'s text (3 octets): " *abc-sum*)))
    (let ((tuatara.defaults::*hash-piece* 1000) (text (make-string 3000000 :initial-element #\a)))
      (multiple-value-bind (f editor) (run-keys "M-x s h a 2 5 6 - s u m RET" :text text :width 160)
        (declare (ignore editor))
        (true (tuatara.frontend.fake:fake-frame-showing f "SHA-256 of *scratch*'s text (3,000,000 octets): "
                                                        "2a152c894398719c0570f83fac34ac03a0f6e8e474b995c2403aa5434f7b9dd4"))))))

(define-test (hex-and-sums sha256-failing-and-stopped)
  ;; a program that does not print a sum: what it said is shown
  (let ((tuatara.defaults::*hash-library* nil) (tuatara.defaults::*hash-program* "false"))
    (multiple-value-bind (f editor) (run-keys "M-x s h a 2 5 6 - s u m RET" :text "abc" :width 140)
      (declare (ignore editor))
      (true (tuatara.frontend.fake:fake-frame-showing f "Could not sum *scratch*'s text (3 octets): false failed"))))
  ;; stopped at the start, nothing is summed
  (let ((job (tuatara.defaults::start-hash nil "nothing" 3 (tuatara.defaults::text-source (tuatara.text:string->rope "abc") 0 3))))
    (tuatara.runtime:job-stop job)
    (sb-thread:join-thread (tuatara.defaults::hash-job-thread job) :default nil)
    (true (member (tuatara.defaults::hash-job-status job) '(:stopped :done)))
    (tuatara.runtime:forget-job job))
  ;; a file the kernel makes up as it is read: summed to its end, not its size (0)
  (let* ((b (tuatara.buffer:make-buffer "hash-test" :text ""))
         (job (tuatara.defaults::hash-file b #p"/proc/self/cmdline")))
    (sb-thread:join-thread (tuatara.defaults::hash-job-thread job) :default nil)
    (is eq :done (tuatara.defaults::hash-job-status job))
    (true (plusp (car (tuatara.defaults::hash-job-done job))))
    (tuatara.runtime:forget-job job)
    (tuatara.buffer:kill-buffer b))
  ;; one that runs until stopped: M-x stop-job
  (let ((tuatara.defaults::*hash-library* nil) (tuatara.defaults::*hash-program* "yes") (tuatara.defaults::*hash-wait* 0))
    (multiple-value-bind (f editor) (run-keys "M-x s h a 2 5 6 - s u m RET M-x s t o p - j o b RET y RET" :text "abc" :width 140)
      (declare (ignore editor))
      (true (tuatara.frontend.fake:fake-frame-showing f "Stop summing at"))
      (true (tuatara.frontend.fake:fake-frame-showing f "Stopped: nothing summed"))
      (false (find-if #'tuatara.defaults::hash-job-p tuatara.runtime:*jobs*)))))

(define-test (hex-and-sums made-up-files-read-in-pieces)
  ;; a file the kernel makes up says it holds nothing: it is read a piece at a time, not an octet
  (let ((tuatara.defaults::*hash-piece* 65536))
    (is = 65536 (tuatara.defaults::hash-piece-octets 0))
    (is = 100 (tuatara.defaults::hash-piece-octets 100))
    (is = 65536 (tuatara.defaults::hash-piece-octets 1000000))))

(define-test (hex-and-sums hex-rows-cheaply)
  ;; a row of the hex view is made without writing text through a stream per octet
  (let ((rope (tuatara.text:make-rope (coerce (loop for i below 4096 collect (mod i 256)) '(vector (unsigned-byte 8))))))
    (flet ((per-row (rows)
             (tuatara.display::hex-layout rope 0 rows)
             (/ (nth-value 2 (tuatara.bench::measuring (tuatara.display::hex-layout rope 0 rows))) rows)))
      (let ((few (per-row 16)) (many (per-row 200)))
        (true (< few 10000) "~d octets made for a row of 16" few)
        (true (< many 10000) "~d octets made for a row of 200" many)))
    (is string= "0000001f" (tuatara.display::hex-digits 31 8))
    (is string= "123456789a" (tuatara.display::hex-digits #x123456789a 8))
    (is string= "00000000" (tuatara.display::hex-digits 0 8))))
