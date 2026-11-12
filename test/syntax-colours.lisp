;;;; test/syntax-colours.lisp --- Tree-sitter: other languages, local names and colours on screen.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test (syntax text-in-other-languages)
  ;; the commands after RUN in a Containerfile are coloured as shell, and the
  ;; body of a Rust macro as Rust, over the colours around them
  (let* ((f (colour-session ""
                            (format nil "FROM fedora:44~%RUN dnf install -y gcc && echo \"$HOME\"~%") "containerfile"))
         (spans (aref (tuatara.frontend.fake:fake-spans f) 1)))
    (is equal '(0 3 :keyword) (first spans))
    (true (member '(4 7 :function) spans :test #'equal))
    (true (member '(23 25 :operator) spans :test #'equal))
    (true (member '(26 30 :function) spans :test #'equal)))
  (let* ((f (colour-session "" (format nil "fn f() { m!(if a { b } else { c }); }~%") "rs"))
         (spans (aref (tuatara.frontend.fake:fake-spans f) 0)))
    (flet ((top (start end)
             ;; the face painted last over the text, where colours lie over others
             (let ((face (third (find-if (lambda (s) (and (= (first s) start) (= (second s) end))) spans))))
               (if (listp face) (first face) face))))
      (is eq :keyword (top 12 14))
      (is eq :keyword (top 23 27))))
  ;; a comment with no marks in it (TODO, FIXME) keeps its colour alone
  (let ((f (colour-session "" (format nil "// a comment~%fn f() {}~%") "rs")))
    (is equal '((0 12 :comment)) (aref (tuatara.frontend.fake:fake-spans f) 0)))
  ;; a #! line names the language where no name is given: a Markdown code
  ;; block, a justfile's recipe
  (let ((spans (aref (tuatara.frontend.fake:fake-spans
                      (colour-session "" (format nil "```~%#!/bin/bash~%echo $HOME~%```~%") "md"))
                     2)))
    (true (find :function spans :key (lambda (s) (let ((f (third s))) (if (listp f) (first f) f))))))
  (let ((spans (let ((tuatara-user:*grammars-on* '(:common just)))
                 (aref (tuatara.frontend.fake:fake-spans
                        (colour-session "" (format nil "run:~%    #!/usr/bin/env python3~%    def f(): pass~%") "just"))
                       2))))
    (true (find :keyword spans :key (lambda (s) (let ((f (third s))) (if (listp f) (first f) f))))))
  ;; names of languages this editor does not colour are passed over: the
  ;; block has Markdown's colour alone
  (let ((f (colour-session "" (format nil "```nosuchlang~%x + 1~%```~%") "md")))
    (is equal '((0 5 :markup.raw.block)) (aref (tuatara.frontend.fake:fake-spans f) 1)))
  ;; a file name given in the text (Nix's writeText) is taken literally: a [ or * in it is only a character
  (true (tuatara.defaults::language-for-injection '(:filename "x.py")))
  (is eq (tuatara.defaults::language-for-injection '(:filename "x.py"))
      (tuatara.defaults::language-for-injection '(:filename "[x] *?.py"))))

(define-test (syntax long-text-in-other-languages)
  ;; a long text in another language (a code block of megabytes) is parsed
  ;; only near the lines being coloured, which are coloured all the same
  (let* ((text (with-output-to-string (o)
                 (format o "```python~%")
                 (dotimes (i 100000) (format o "def f~d(x):~%    return x~%" i))
                 (format o "```~%")))
         (markdown (tuatara.defaults::find-language 'markdown))
         (injections (list (tuatara.defaults::language-compiled-query markdown :injection)
                           (tuatara.defaults::injection-resolver :rich 3))))
    (multiple-value-bind (tree rope parser) (parse-string "markdown" text)
      (unwind-protect
           (let* ((middle (tuatara.text:rope-line-start rope (floor (length text) 2)))
                  (ranges (let ((tuatara.syntax:*helper* :shared))
                            (tuatara.syntax::injected-highlights injections tree rope middle (+ middle 60)))))
             ;; def as a keyword, the function's name as a function, in the middle
             (true (find-if (lambda (r) (and (<= middle (first r)) (eq (third r) :keyword))) ranges))
             (true (find-if (lambda (r) (and (<= middle (first r)) (eq (third r) :function))) ranges))
             ;; the text parsed is what lies near: the block's lines, cut near the window
             (destructuring-bind ((name spans)) (tuatara.syntax::injection-layers
                                                 (first injections) tree rope middle (+ middle 60)
                                                 (- middle 1000) (+ middle 1000))
               (is string= "python" name)
               (true (every (lambda (span) (and (<= (- middle 1000) (car span)) (<= (cdr span) (+ middle 1000)))) spans))))
        (release-parse tree parser)))))

(define-test (syntax local-names)
  ;; a parameter is coloured as a parameter wherever its function uses it;
  ;; other names keep their colours
  (let* ((f (colour-session "" (format nil "fn f(count: i32) -> i32 { let x = count + 1; x }~%") "rs"))
         (spans (aref (tuatara.frontend.fake:fake-spans f) 0)))
    (flet ((top (start end)
             (let ((face (third (find-if (lambda (s) (and (= (first s) start) (= (second s) end))) spans))))
               (if (listp face) (first face) face))))
      (is eq :variable.parameter (top 5 10))
      (is eq :variable.parameter (top 34 39))
      (is eq :variable (top 30 31))
      (is eq :variable (top 45 46))))
  ;; a name is its definition's in the innermost scope that has it: count in g
  ;; is g's own, not f's parameter
  (let* ((f (colour-session "" (format nil "fn f(count: i32) {} fn g() { let count = 1; count; }~%") "rs"))
         (spans (aref (tuatara.frontend.fake:fake-spans f) 0)))
    (flet ((top (start end)
             (let ((face (third (find-if (lambda (s) (and (= (first s) start) (= (second s) end))) spans))))
               (if (listp face) (first face) face))))
      (is eq :variable.parameter (top 5 10))
      (is eq :variable (top 44 49)))))

(define-test (syntax local-names-in-embedded-text)
  ;; in a Rust block of a Markdown file the parameter is coloured as one where
  ;; the function uses it, as in a Rust file
  (let* ((f (colour-session "" (format nil "```rust~%fn f(count: i32) -> i32 { count + 1 }~%```~%") "md"))
         (spans (aref (tuatara.frontend.fake:fake-spans f) 1)))
    (flet ((top (start end)
             (let ((face (third (find-if (lambda (s) (and (= (first s) start) (= (second s) end))) spans))))
               (if (listp face) (first face) face))))
      (is eq :variable.parameter (top 5 10))
      (is eq :variable.parameter (top 26 31)))))

(define-test (syntax local-names-in-rules)
  ;; #is-not? local: a pattern for a builtin passes over a name that is local
  ;; where it is used; and a scope whose rules say local.scope-inherits false
  ;; (a Ruby method) sees no name defined outside it
  (is equal '(:local nil nil t) (tuatara.syntax::parse-predicate '("is-not?" "local")))
  (flet ((setup (language text)
           (let* ((language (tuatara.defaults::find-language language))
                  (grammar (tuatara.syntax:find-grammar (tuatara.defaults::language-grammar language)))
                  (rope (tuatara.text:string->rope text))
                  (parser (tuatara.syntax:make-parser grammar (tuatara.syntax:kept-helper :shared))))
             (values (tuatara.syntax:parse-rope parser rope) rope language parser)))
         (resolved-starts (table)
           (sort (loop for key being the hash-keys of table collect (ash key -31)) #'<)))
    (let ((text (format nil "console.log(1);~%function f(console) { return console; }~%")))
      (multiple-value-bind (tree rope language parser) (setup 'javascript text)
        (unwind-protect
             (multiple-value-bind (ranges resolved)
                 (tuatara.syntax::local-highlights (tuatara.defaults::language-compiled-query language :locals) tree rope 0 0 (length text)
                                                   (constantly nil))
               (declare (ignore ranges))
               (let* ((used (search "return console" text))
                      (at (+ used 7))
                      (query (tuatara.defaults::language-compiled-query language :colouring))
                      (faces (map 'vector (lambda (name) (intern (string-upcase name) :keyword)) (tuatara.syntax:query-captures query))))
                 (is equal (list at) (resolved-starts resolved))
                 (flet ((face-at (start local)
                          (let ((tuatara.syntax::*local-references* local))
                            (third (find start (tuatara.syntax::rich-highlights query tree rope 0 (length text) faces)
                                         :key #'first)))))
                   (is eq :variable.builtin (face-at 0 resolved))
                   (is eq :variable.builtin (face-at at nil))
                   (is eq :variable (face-at at resolved)))))
          (tuatara.syntax:delete-tree tree)
          (tuatara.syntax:delete-parser parser))))
    (let ((text (format nil "proc { |a|~%  def m~%    a~%  end~%  a~%}~%")))
      (multiple-value-bind (tree rope language parser) (setup 'ruby text)
        (unwind-protect
             (multiple-value-bind (ranges resolved)
                 (tuatara.syntax::local-highlights (tuatara.defaults::language-compiled-query language :locals) tree rope 0 0 (length text)
                                                   (constantly nil))
               (declare (ignore ranges))
               ;; the a after the method is the block's parameter; the one in it is not
               (is equal (list (search (format nil "a~%}") text)) (resolved-starts resolved)))
          (tuatara.syntax:delete-tree tree)
          (tuatara.syntax:delete-parser parser))))
    ;; every local name used gets its colour, not only the last found
    (let ((text (format nil "function f(a, b) { return a + b; }~%")))
      (multiple-value-bind (tree rope language parser) (setup 'javascript text)
        (unwind-protect
             (let ((ranges (tuatara.syntax::local-highlights (tuatara.defaults::language-compiled-query language :locals) tree rope 0 0 (length text)
                                                             (constantly :variable))))
               (is equal (list (search "a + b" text) (+ 4 (search "a + b" text)))
                   (remove-if (lambda (start) (< start (search "return" text))) (mapcar #'first ranges))))
          (tuatara.syntax:delete-tree tree)
          (tuatara.syntax:delete-parser parser))))))

(define-modal-test (syntax colours-on-screen)
  (let ((f (colour-session "" (format nil "fn main() {~%    let x = 1; // one~%}~%") "rs")))
    (is equal '((0 2 :keyword) (3 7 :function) (7 9 :punctuation.bracket) (10 11 :punctuation.bracket))
        (aref (tuatara.frontend.fake:fake-spans f) 0))
    (is equal
        '((4 7 :keyword)
          (8 9 :variable)
          (10 11 :operator)
          (12 13 :constant.numeric)
          (13 14 :punctuation)
          (15 21 :comment))
        (aref (tuatara.frontend.fake:fake-spans f) 1)))
  ;; typed text is coloured in the frame that shows it
  (let ((f (colour-session "i p u b SPC C-g" (format nil "fn main() {}~%") "rs")))
    (is string= "pub fn main() {}" (tuatara.frontend.fake:fake-screen-line f 0))
    (is equal '(0 3 :keyword) (first (aref (tuatara.frontend.fake:fake-spans f) 0))))
  ;; the sparse style, chosen for the language, colours less
  (unwind-protect
       (progn
         (tuatara-user:set-colouring 'rust :sparse)
         (let ((f (colour-session "" (format nil "fn main() {~%    let x = 1; // one~%}~%") "rs")))
           (is equal '((0 2 :keyword) (3 7 :function)) (aref (tuatara.frontend.fake:fake-spans f) 0))
           (is equal '((4 7 :keyword) (12 13 :constant.numeric) (15 21 :comment))
               (aref (tuatara.frontend.fake:fake-spans f) 1)))
         (tuatara-user:set-colouring 'rust :sparse :level 1)
         (let ((f (colour-session "" (format nil "fn main() {~%    let x = 1; // one~%}~%") "rs")))
           (is equal '() (aref (tuatara.frontend.fake:fake-spans f) 0))
           (is equal '((15 21 :comment)) (aref (tuatara.frontend.fake:fake-spans f) 1)))
         (tuatara-user:set-colouring 'rust :sparse :level 4)
         (let ((f (colour-session "" (format nil "fn main() {~%}~%") "rs")))
           (is equal '((0 2 :keyword) (3 7 :function) (7 9 :punctuation.bracket) (10 11 :punctuation.bracket))
               (aref (tuatara.frontend.fake:fake-spans f) 0))))
    (tuatara-user:set-colouring 'rust :rich :level 3))
  ;; M-x rotate-colouring goes from rich to sparse to none and round again
  (unwind-protect
       (let ((text (format nil "fn main() {~%    let x = 1; // one~%}~%")))
         (let ((f (colour-session "M-x r o t a t e - c o l o u r i n g RET" text "rs")))
           (true (tuatara.frontend.fake:fake-frame-showing f "Rust colouring: sparse"))
           (is equal '((0 2 :keyword) (3 7 :function)) (aref (tuatara.frontend.fake:fake-spans f) 0)))
         (let ((f (colour-session "M-x r o t a t e - c o l o u r i n g RET" text "rs")))
           (true (tuatara.frontend.fake:fake-frame-showing f "Rust colouring: none"))
           (is equal '() (aref (tuatara.frontend.fake:fake-spans f) 0)))
         (let ((f (colour-session "M-x r o t a t e - c o l o u r i n g RET" text "rs")))
           (true (tuatara.frontend.fake:fake-frame-showing f "Rust colouring: rich"))
           (is equal '((0 2 :keyword) (3 7 :function) (7 9 :punctuation.bracket) (10 11 :punctuation.bracket))
               (aref (tuatara.frontend.fake:fake-spans f) 0)))
         (let ((f (colour-session "M-x r o t a t e - c o l o u r i n g RET" "plain" "txt")))
           (true (tuatara.frontend.fake:fake-frame-showing f "is not in the mode of a language"))))
    (tuatara-user:set-colouring 'rust :rich))
  (fail (tuatara-user:set-colouring 'cobol :rich))
  (fail (tuatara-user:set-colouring 'rust :loud))
  (fail (tuatara-user:set-colouring 'lisp :sparse :level 5)))

(define-test (syntax colouring-by-mode)
  ;; a style for every buffer (t), one for the modes made from a mode
  ;; (prog-mode) and a language's own: the language's first, then the nearest
  ;; mode's, then every buffer's
  (let ((rust (tuatara.defaults::find-language 'rust))
        (bash (tuatara.defaults::find-language 'bash))
        (csv (make-instance 'tuatara.defaults::csv-mode)))
    (flet ((colouring (language) (multiple-value-list (tuatara.defaults::language-colouring language)))
           (unset ()
             (setf tuatara.defaults::*colouring-rules* '()
                   tuatara.defaults::*csv-colouring* nil
                   (tuatara.defaults::language-style rust) nil (tuatara.defaults::language-level rust) nil
                   (tuatara.defaults::language-style bash) nil (tuatara.defaults::language-level bash) nil)))
      (unwind-protect
           (progn
             (unset)
             (is equal '(:rich 3) (colouring bash))
             (tuatara-user:set-colouring t :none)
             (is equal '(:none 3) (colouring bash))
             (is eq :none (tuatara.defaults::csv-colouring csv))
             ;; text in no mode of its own, as Markdown's inline text, goes by every buffer's
             (is equal '(:none 3) (colouring (tuatara.defaults::find-language 'markdown-inline)))
             (tuatara-user:set-colouring 'prog-mode :sparse :level 1)
             (is equal '(:sparse 1) (colouring bash))
             (is eq :none (tuatara.defaults::csv-colouring csv))
             (tuatara-user:set-colouring 'rust :rich)
             (is equal '(:rich 3) (colouring rust))
             ;; on the screen: Rust rich, a shell script sparse at level 1
             (let ((f (colour-session "" (format nil "fn main() {}~%") "rs")))
               (is equal '(0 2 :keyword) (first (aref (tuatara.frontend.fake:fake-spans f) 0))))
             (let ((f (colour-session "" (format nil "echo hi # note~%") "sh")))
               (is equal '((8 14 :comment)) (aref (tuatara.frontend.fake:fake-spans f) 0)))
             ;; rotate-colouring goes on from what the buffer shows, as the language's own
             (let ((f (colour-session "M-x r o t a t e - c o l o u r i n g RET" (format nil "echo hi~%") "sh")))
               (true (tuatara.frontend.fake:fake-frame-showing f "Bash colouring: none")))
             (is eq :none (tuatara.defaults::language-style bash))
             ;; a mode no language is coloured in is refused
             (fail (tuatara-user:set-colouring 'special-mode :sparse))
             (fail (tuatara-user:set-colouring 'calendar-mode :sparse))
             ;; Org, coloured by rules of its own, takes a style as a language does
             (is eq :sparse (tuatara-user:set-colouring 'org-mode :sparse))
             (is equal
                 '(:sparse 3)
                 (multiple-value-list (tuatara.defaults::mode-colouring (make-instance 'tuatara.defaults::org-mode)))))
        (unset)))))

(define-modal-test (syntax c-language)
  ;; .c and .h files are C
  (is eq 'tuatara.defaults::c-mode (tuatara.modes:mode-for-file #p"/src/main.c"))
  (is eq 'tuatara.defaults::c-mode (tuatara.modes:mode-for-file #p"/src/main.h"))
  ;; coloured by the rules for C
  (let* ((f (colour-session "" (format nil "int main(void) {~%    return 0; // done~%}~%") "c"))
         (first-row (aref (tuatara.frontend.fake:fake-spans f) 0))
         (second-row (aref (tuatara.frontend.fake:fake-spans f) 1)))
    (flet ((face-at (spans start end)
             (let ((face (third (find-if (lambda (s) (and (= (first s) start) (= (second s) end))) spans))))
               (if (listp face) (first face) face))))
      (true (search "TYPE" (symbol-name (face-at first-row 0 3))))
      (is eq :function (face-at first-row 4 8))
      (true (search "KEYWORD" (symbol-name (face-at second-row 4 10))))
      (is eq :comment (face-at second-row 14 21))))
  ;; a parameter is coloured as one wherever the function uses it
  (let* ((f (colour-session "" (format nil "int f(int count) { return count + 1; }~%") "c"))
         (spans (aref (tuatara.frontend.fake:fake-spans f) 0)))
    (flet ((face-at (start end)
             (let ((face (third (find-if (lambda (s) (and (= (first s) start) (= (second s) end))) spans))))
               (if (listp face) (first face) face))))
      (is eq :variable.parameter (face-at 10 15))
      (is eq :variable.parameter (face-at 26 31))))
  ;; a function, a parameter and a type selected, and commented with //
  (flet ((region (keys text)
           (let (result)
             (with-file-session ((f editor) keys text "c")
               (setf result (session-region-text editor)))
             result))
         (after (keys text)
           (let (result)
             (with-file-session ((f editor) keys text "c")
               (setf result (session-text editor)))
             result)))
    (let ((c (format nil "int add(int a, int b) {~%    return a + b;~%}~%struct p { int x; };~%")))
      (is string= (format nil "{~%    return a + b;~%}") (region "j m f" c))
      (is string= "int b" (region "l l l l l l l l l l l l l l l l m r" c))
      (is string= "{ int x; }" (region "j j j l l l l l l l l l l m t" c))
      (is string= "struct p { int x; }" (region "j j j l l l l l l l l l l m T" c)))
    (is string= (format nil "// x = 1;~%") (after "C" (format nil "x = 1;~%")))
    ;; > indents four columns a level
    (is string= (format nil "int f() {~%    return 1;~%}~%") (after "j >" (format nil "int f() {~%return 1;~%}~%")))))

(define-test (syntax colours-far-into-a-large-text)
  ;; lines beyond the first 2 and 4 GiB of a text are coloured where they stand
  (let* ((filler (tuatara.text:string->rope
                  (with-output-to-string (s) (loop repeat (* 16 1024) do (format s "// ~60,,,'-a~%" "")))))
         (code (format nil "fn f(p: u8) -> u8 {~%    g(p, \"line\")~%}~%"))
         (grammar (tuatara.syntax:find-grammar "rust"))
         (query (tuatara.syntax:compile-query grammar (tuatara.defaults::language-rules (language-named 'rust))))
         (faces (tuatara.defaults::query-faces query :rich 0)))
    (dolist (copies (list (ceiling (* 5/2 (expt 2 30)) (tuatara.text:rope-octet-length filler))
                          (ceiling (* 9/2 (expt 2 30)) (tuatara.text:rope-octet-length filler))))
      (let* ((rope (apply #'tuatara.text:splice-rope (tuatara.text:make-rope) 0 0
                          (append (make-list copies :initial-element filler)
                                  (list (tuatara.text:string->rope code) filler))))
             (at (* copies (tuatara.text:rope-octet-length filler)))
             (buffer (tuatara.buffer:make-buffer "*syntax-far*" :text rope)))
        (unwind-protect
             (let* ((syntax (tuatara.syntax:attach-syntax buffer grammar))
                    (shown (tuatara.syntax:syntax-highlights syntax query faces (list (cons at (+ at (length code))))))
                    (string (+ at (search "\"line\"" code))))
               (true (tuatara.syntax:syntax-too-big syntax))
               (true (find-if (lambda (range) (and (= string (first range)) (= (+ string 6) (second range)))) shown)
                     "the string at ~d is coloured" string)
               (true (every (lambda (range) (<= at (first range) (second range) (+ at (length code)))) shown)))
          (tuatara.syntax:detach-syntax buffer)
          (tuatara.buffer:kill-buffer buffer))))))

(define-test (syntax grammar-names-name-files-there)
  ;; a grammar's name never leads out of the grammar directories
  (let ((looked '()))
    (sb-int:encapsulate 'tuatara.syntax::find-grammar-file 'noting-looks
                        (lambda (function name) (push name looked) (funcall function name)))
    (unwind-protect
         (progn
           (false (tuatara.syntax:find-grammar "../grammars/rust"))
           (false (tuatara.syntax:find-grammar "/tmp/x"))
           (false (tuatara.syntax:find-grammar "Rust"))
           (false (find-if (lambda (name) (find #\/ name)) looked))
           (true (tuatara.syntax:find-grammar "rust"))
           (true (tuatara.syntax:find-grammar "c-sharp")))
      (sb-int:unencapsulate 'tuatara.syntax::find-grammar-file 'noting-looks))))

(define-test (syntax predicates-copy-no-long-text)
  ;; #any-of? on a long node and #eq? with a text make no copy of either
  (let* ((test-any (tuatara.syntax::parse-predicate (list "any-of?" 0 "fn" "let")))
         (test-eq (tuatara.syntax::parse-predicate (list "eq?" 0 "fn"))))
    (flet ((consed (test length)
             (let* ((rope (tuatara.text:string->rope (make-string length :initial-element #\a)))
                    (captures (make-array 4 :element-type 'fixnum :initial-contents (list 0 0 length 0))))
               (nth-value 2 (tuatara.bench::measuring
                              (loop repeat 100 do (tuatara.syntax::text-test-passes-p
                                                   (first test) 0 (third test) nil rope captures 1)))))))
      (true (< (consed test-any (* 1024 1024)) 4096) "#any-of? on a megabyte")
      (true (< (consed test-any (* 64 1024)) 4096) "#any-of? on 64 KB")
      (true (< (consed test-eq 2) 1024) "#eq? on two octets"))))

(define-test (syntax predicates-tested-without-copies)
  ;; the tests of a rule on its nodes' text read short text on the stack: matching the
  ;; rules of colour over a file makes next to nothing for each match
  (let* ((test-match (tuatara.syntax::parse-predicate (list "match?" 0 "^[A-Z]")))
         (test-any (tuatara.syntax::parse-predicate (list "any-of?" 0 "fn" "Kea")))
         (test-eq (tuatara.syntax::parse-predicate (list "eq?" 0 1)))
         (rope (tuatara.text:string->rope "Kea kea é Kea"))
         (captures (make-array 8 :element-type 'fixnum :initial-contents (list 0 0 3 0 1 11 14 0))))
    (flet ((passes (test) (tuatara.syntax::text-test-passes-p (first test) 0 (third test) nil rope captures 2)))
      (true (passes test-match))
      (true (passes test-any))
      (true (passes test-eq))
      (setf (aref captures 5) 4 (aref captures 6) 7)
      (false (passes test-eq))
      (is = 0 (nth-value 2 (tuatara.bench::measuring
                             (loop repeat 100 do (passes test-match) (passes test-any) (passes test-eq)))))))
  (let ((language (tuatara.defaults::find-language 'rust)))
    (when (and language (tuatara.defaults::language-compiled-query language :colouring))
      (let* ((query (tuatara.defaults::language-compiled-query language :colouring))
             (text (with-output-to-string (o)
                     (dotimes (i 300)
                       (format o "pub fn kea_~d(x: &str) -> Result<String, Error> {~%    if x == \"kea\" { return Ok(None); }~%    self.call(true, 'c')?~%}~%" i))))
             (rope (tuatara.text:string->rope text))
             (parser (tuatara.syntax:make-parser (tuatara.syntax:find-grammar (tuatara.defaults::language-grammar language))
                                                 (tuatara.syntax:kept-helper :shared)))
             (tree (tuatara.syntax:parse-rope parser rope))
             (matches 0))
        (unwind-protect
             (flet ((run () (tuatara.syntax:map-query-matches (lambda (p c n) (declare (ignore p c n)) (incf matches))
                                                              query tree rope 0 (length text))))
               (run)
               (let ((consed (loop repeat 3 minimize (nth-value 2 (tuatara.bench::measuring (run))))))
                 (true (< consed (* 16 1024)) "~:d matches consed ~:d octets" (floor matches 4) consed)))
          (release-parse tree parser))))))

(define-test (syntax colouring-garbage-flat-with-rows)
  ;; typing in a coloured buffer: the garbage of its colours, worked out
  ;; afresh after each key, does not grow with the rows the window shows
  (let ((text (with-output-to-string (s)
                (dotimes (i 300)
                  (format s "(defun f~d (x &key (y ~d))~%  \"Doc ~d.\" (list :k x y 'f~d #\\a \"s~d\" (+ 1 2)))~%"
                          i i i i i)))))
    (multiple-value-bind (low low-worked) (typing-garbage-per-key text "lisp" 20 *colouring-functions*)
      (multiple-value-bind (high high-worked) (typing-garbage-per-key text "lisp" 60 *colouring-functions*)
        ;; the colours were worked out again for each key that changed the text
        (true (>= low-worked 16) "~d colourings" low-worked)
        (true (>= high-worked 16) "~d colourings" high-worked)
        (true (< (/ (- high low) 40) 64) "~,1f octets a key at 20 rows, ~,1f at 60: ~,1f more for each row"
              low high (/ (- high low) 40))))))

(define-test (syntax comments-coloured-again-only-when-changed)
  ;; typing on one line of a Lisp text with a comment on every line: the
  ;; comments, read as text in another language, are not parsed and coloured
  ;; again while their text is the same, so a taller window colours no more
  ;; for each key, and makes far less garbage for each row
  (let ((text (with-output-to-string (s)
                (dotimes (i 300)
                  (format s "(defun f~d (x) (list x 'a)) ; note ~d: TODO look again~%" i i)))))
    (multiple-value-bind (low low-worked) (typing-garbage-per-key text "lisp" 20 *colouring-functions*)
      (multiple-value-bind (high high-worked) (typing-garbage-per-key text "lisp" 60 *colouring-functions*)
        (true (>= low-worked 16) "~d colourings" low-worked)
        (is = low-worked high-worked "~d colourings at 20 rows, ~d at 60" low-worked high-worked)
        (true (< (/ (- high low) 40) 1024) "~,1f octets a key at 20 rows, ~,1f at 60: ~,1f more for each row"
              low high (/ (- high low) 40))))))

(define-test (syntax local-names-garbage-flat-with-rows)
  ;; typing in Rust: the local names shown are told apart by their octets in
  ;; room used again, so a taller window makes no more garbage for each key
  (let ((text (with-output-to-string (s)
                (dotimes (i 300) (format s "fn f~d(p: u8, q: u8) -> u8 { let a = p + q; g(a, p) }~%" i)))))
    (multiple-value-bind (low low-worked) (typing-garbage-per-key text "rs" 20 '(tuatara.syntax::local-highlights))
      (multiple-value-bind (high high-worked) (typing-garbage-per-key text "rs" 60 '(tuatara.syntax::local-highlights))
        (true (>= low-worked 16) "~d colourings" low-worked)
        (true (>= high-worked 16) "~d colourings" high-worked)
        (true (< (/ (- high low) 40) 64) "~,1f octets a key at 20 rows, ~,1f at 60: ~,1f more for each row"
              low high (/ (- high low) 40))))))

(define-test (syntax edits-told-in-one-request)
  ;; the edits of one change reach the tree, and the copy a parse begins
  ;; from is made, in one request to the helper however many places change
  (let* ((text (with-output-to-string (s) (dotimes (i 40) (format s "fn f~d() {}~%" i))))
         (buffer (tuatara.buffer:make-buffer "*syntax-edits*" :text text))
         (requests 0))
    (unwind-protect
         (let ((syntax (tuatara.syntax:attach-syntax buffer (tuatara.syntax:find-grammar "rust"))))
           (parse-done syntax)
           (flet ((requests-for (places)
                    (parse-done syntax)
                    (setf requests 0)
                    ;; this thread's, not those of the parse it starts
                    (let ((here sb-thread:*current-thread*))
                      (sb-int:encapsulate 'tuatara.syntax::call-send 'counting
                                          (lambda (function &rest arguments)
                                            (when (eq sb-thread:*current-thread* here) (incf requests))
                                            (apply function arguments))))
                    (unwind-protect
                         (tuatara.buffer:buffer-edit
                          buffer (coerce (loop for i below places
                                               for at = (search (format nil "f~d(" i)
                                                                (tuatara.buffer:buffer-string buffer))
                                               collect (tuatara.text:make-edit at at (sb-ext:string-to-octets "x")))
                                         'vector))
                      (sb-int:unencapsulate 'tuatara.syntax::call-send 'counting))
                    requests))
             (let ((one (requests-for 1)) (twenty (requests-for 20)))
               (is = 1 one "~d requests for an edit in one place" one)
               (is = one twenty "~d requests for one place, ~d for twenty" one twenty)
               ;; the tree followed the edits
               (parse-done syntax)
               (is equal (tuatara.buffer:buffer-string buffer)
                   (tuatara.text:rope-string (tuatara.syntax:syntax-rope syntax)))
               (false (tuatara.syntax:tree-has-error-p (tuatara.syntax:syntax-tree syntax))))))
      (tuatara.syntax:detach-syntax buffer)
      (tuatara.buffer:kill-buffer buffer))))

(define-test (syntax colours-worked-out-once-for-a-slow-parse)
  ;; a key whose parse is not done in time to be shown shows the colours of
  ;; before moved with the text; they are worked out once, as the parse is done
  (let ((path (records-file (with-output-to-string (s)
                              (dotimes (i 200) (format s "(defun f~d (x) (list x \"s\" 'a))~%" i)))
                            :type "lisp"))
        (buffer nil) (counting nil) (worked-out 0) (keys 0))
    (flet ((parsed ()
             ;; the parse of the key done, and the window drawn after it
             (let ((syntax (tuatara.syntax:buffer-syntax buffer)))
               (loop repeat 500
                     for job = (tuatara.syntax:syntax-job syntax)
                     while (and job (not (tuatara.syntax::job-finished-p syntax job)))
                     do (sleep 0.01)))))
      (sb-int:encapsulate 'tuatara.syntax::run-parse-job 'slow
                          (lambda (function syntax job) (sleep 0.1) (funcall function syntax job)))
      (sb-int:encapsulate 'tuatara.syntax:rich-highlights 'counting
                          (lambda (function &rest arguments)
                            (when counting (incf worked-out))
                            (apply function arguments)))
      (unwind-protect
           (let ((tuatara.syntax:*syntax-wait* 0.01) (tuatara.defaults::*rules-wait* 30))
             (run-keys (list* (lambda ()
                                (setf buffer (tuatara.command:current-buffer))
                                (loop repeat 600
                                      for language = (tuatara.buffer:buffer-local buffer :syntax-pending)
                                      until (or (null language) (tuatara.syntax:buffer-syntax buffer))
                                      do (tuatara.defaults::start-parsing buffer language) (sleep 0.05))
                                (parsed))
                              :tick "C-v i" (lambda () (parsed) (setf counting t))
                              (loop for key in '("a" "b" "c" "DEL" "DEL" "DEL")
                                    append (list key (lambda () (incf keys) (parsed)) :tick)))
                       :file path :width 100 :height 20))
        (sb-int:unencapsulate 'tuatara.syntax::run-parse-job 'slow)
        (sb-int:unencapsulate 'tuatara.syntax:rich-highlights 'counting)
        (kill-records buffer path)))
    (is = 6 keys)
    (true (<= keys worked-out (1+ keys)) "colours worked out ~d times for ~d keys" worked-out keys)))

(define-test (syntax parts-coloured-from-one-tree)
  ;; a window showing two parts of a text (a fold between them): a parse
  ;; that comes in while the first part's colours are worked out does not
  ;; take their room for the second's, so both are painted as they are
  (let ((path (records-file (with-output-to-string (s)
                              (dotimes (i 120) (format s "(defun f~d (x) (list x \"s~d\" 'a))~%" i i)))
                            :type "lisp"))
        (buffer nil) (gate nil) (ready nil) (painted nil) (settled nil) (before 0))
    (flet ((wait-for (test) (loop repeat 1000 until (funcall test) do (sleep 0.01)))
           ;; the faces of the text rows of the frame N frames ago, not the mode line's
           (frame (n) (let ((history (tuatara.frontend.fake:fake-face-history
                                      (tuatara.command:editor-frontend tuatara.command:*editor*))))
                        (subseq (coerce (car (nth n history)) 'list) 0 16))))
      (sb-int:encapsulate 'tuatara.syntax::run-parse-job 'gated
                          (lambda (function syntax job)
                            (when gate (loop repeat 1000 until ready do (sleep 0.01)))
                            (funcall function syntax job)))
      (sb-int:encapsulate 'tuatara.syntax:rich-highlights 'gated
                          (lambda (function &rest arguments)
                            (multiple-value-prog1 (apply function arguments)
                              ;; the parse comes in after the first part's colours
                              (when (and gate (not ready))
                                (setf ready t)
                                (let ((syntax (tuatara.syntax:buffer-syntax buffer)))
                                  (wait-for (lambda ()
                                              (let ((job (tuatara.syntax:syntax-job syntax)))
                                                (or (null job) (tuatara.syntax::job-finished-p syntax job))))))))))
      (unwind-protect
           (let ((tuatara.syntax:*syntax-wait* 0) (tuatara.defaults::*rules-wait* 30))
             (run-keys
                       (list (lambda ()
                               (setf buffer (tuatara.command:current-buffer))
                               (loop repeat 600
                                     for language = (tuatara.buffer:buffer-local buffer :syntax-pending)
                                     until (or (null language) (tuatara.syntax:buffer-syntax buffer))
                                     do (tuatara.defaults::start-parsing buffer language) (sleep 0.05))
                               (parse-done (tuatara.syntax:buffer-syntax buffer))
                               (let ((text (tuatara.buffer:buffer-string buffer)))
                                 (tuatara.display:fold-region buffer
                                                              (search "(defun f4 " text)
                                                              (search "(defun f9 " text))))
                             :tick
                             (lambda ()
                               ;; an edit past what is shown, its parse held back
                               (setf gate t
                                     before (length (tuatara.frontend.fake:fake-face-history
                                                     (tuatara.command:editor-frontend tuatara.command:*editor*))))
                               (tuatara.buffer:buffer-insert buffer (tuatara.buffer:buffer-size buffer) " ")
                               ;; none kept to move: as when many windows have taken their room
                               (setf (tuatara.syntax::syntax-cache (tuatara.syntax:buffer-syntax buffer)) nil))
                             :tick
                             ;; the first frame drawn after the edit
                             (lambda ()
                               (setf painted (frame (- (length (tuatara.frontend.fake:fake-face-history
                                                                (tuatara.command:editor-frontend tuatara.command:*editor*)))
                                                       before 1))))
                             (lambda () (setf gate nil ready t))
                             :tick
                             (lambda () (setf settled (frame 0))))
                       :file path :width 100 :height 20))
        (sb-int:unencapsulate 'tuatara.syntax::run-parse-job 'gated)
        (sb-int:unencapsulate 'tuatara.syntax:rich-highlights 'gated)
        (kill-records buffer path)))
    (true painted)
    (is equal settled painted)))

(define-test (syntax inner-text-not-parsed-coloured-later)
  ;; Rust code in a doc comment's code block, its parse given up once (too
  ;; slow): the doc comment's colours are not kept without it, so that the
  ;; code is coloured at the next edit
  (let ((path (records-file (format nil "/// Adds.~%///~%/// ```~%/// let total = add(1, 2);~%/// ```~%fn add(a: u8, b: u8) -> u8 { a + b }~%")
                            :type "rs"))
        (buffer nil) (failing t))
    (sb-int:encapsulate 'tuatara.syntax::parse-texts 'slow
                        (lambda (function rope parses then &rest more)
                          ;; the parse of Rust inside the comment, given up the first time the
                          ;; colours of the parsed text are worked out
                          (if (and failing tuatara.syntax::*injected-colours* (some (lambda (parse)
                                                   (string= "rust" (tuatara.syntax:grammar-name
                                                                    (tuatara.syntax::text-parse-grammar parse))))
                                                 parses))
                              (progn (setf failing nil)
                                     (funcall then (mapcar (lambda (parse) (declare (ignore parse)) (list nil :slow)) parses)))
                              (apply function rope parses then more))))
    (unwind-protect
         (let* ((tuatara.syntax:*syntax-wait* 30) (tuatara.defaults::*rules-wait* 30)
                (f (run-keys (list (lambda ()
                                     (setf buffer (tuatara.command:current-buffer))
                                     (loop repeat 600
                                           for language = (tuatara.buffer:buffer-local buffer :syntax-pending)
                                           until (or (null language) (tuatara.syntax:buffer-syntax buffer))
                                           do (tuatara.defaults::start-parsing buffer language) (sleep 0.05))
                                     (parse-done (tuatara.syntax:buffer-syntax buffer)))
                                   :tick "M->" "i x DEL C-g" :tick)
                             :file path :width 100 :height 10)))
           ;; the code's own colours on its row of the last frame
           (let* ((row (loop for r below 8
                             when (search "let total" (tuatara.frontend.fake:fake-screen-line f r)) return r))
                  (spans (and row (aref (car (first (tuatara.frontend.fake:fake-face-history f))) row))))
             (true row)
             (true (find-if (lambda (span) (let ((face (third span)))
                                             (member :keyword (if (listp face) face (list face)))))
                            spans)
                   "~s" spans)))
      (sb-int:unencapsulate 'tuatara.syntax::parse-texts 'slow)
      (kill-records buffer path))))

(define-test (syntax edits-refused-parse-afresh)
  ;; a helper that refuses to be told of an edit (one older than the editor)
  ;; leaves no tree at the old places: the text is parsed afresh
  (let* ((text (with-output-to-string (s) (dotimes (i 40) (format s "fn f~d() {}~%" i))))
         (buffer (tuatara.buffer:make-buffer "*syntax-refused*" :text text))
         (refusing nil) (left nil))
    (sb-int:encapsulate 'tuatara.syntax::call-send 'refusing
                        (lambda (function call &rest arguments)
                          (if (and refusing (= (sb-sys:sap-ref-32 (tuatara.syntax::call-out call) 8)
                                               tuatara.syntax::+op-edits+))
                              (setf (tuatara.syntax::call-status call) 1)
                              (apply function call arguments))))
    (unwind-protect
         (let ((syntax (tuatara.syntax:attach-syntax buffer (tuatara.syntax:find-grammar "rust"))))
           (parse-done syntax)
           (setf refusing t)
           (tuatara.buffer:buffer-insert buffer 0 "fn first() {}")
           (setf left (tuatara.syntax:syntax-tree syntax) refusing nil)
           (false left "the tree of the text before the edit is kept")
           (parse-done syntax)
           (true (tuatara.syntax:syntax-tree syntax))
           (false (tuatara.syntax:tree-has-error-p (tuatara.syntax:syntax-tree syntax))))
      (sb-int:unencapsulate 'tuatara.syntax::call-send 'refusing)
      (tuatara.syntax:detach-syntax buffer)
      (tuatara.buffer:kill-buffer buffer))))

(define-test (syntax kept-layers-held-to-column-and-depth)
  ;; the colours of a text in another language are given again only for the
  ;; same octets starting at the same column of a line (a grammar may read
  ;; columns: a heredoc's end) and as deep in texts in other languages
  (let* ((rope (tuatara.text:string->rope (format nil "x = 1~%    x = 1~%x = 1~%")))
         (first-line (list (cons 0 5))) (indented (list (cons 10 15))) (third-line (list (cons 16 21)))
         (tuatara.syntax::*injected-colours* (make-hash-table)))
    (tuatara.syntax::keep-layer "python" :query :faces 1 rope first-line 0 22 '((0 1 :variable)))
    (true (tuatara.syntax::kept-layer "python" :query :faces 1 rope third-line 0 22))
    (false (tuatara.syntax::kept-layer "python" :query :faces 1 rope indented 0 22))
    (false (tuatara.syntax::kept-layer "python" :query :faces 2 rope third-line 0 22))))

(define-test (syntax edits-logged-in-their-room)
  ;; the latest edits are kept, for colours to move with the text, in room of
  ;; their own: logging one makes no garbage, and only the latest are given
  (let* ((buffer (tuatara.buffer:make-buffer "*syntax-log*" :text "fn f() {}"))
         (syntax (tuatara.syntax::%make-syntax buffer (tuatara.syntax:find-grammar "rust") nil))
         (batch :batch))
    (dotimes (i 20)
      (incf (tuatara.syntax:syntax-generation syntax))
      (tuatara.syntax::log-edit syntax batch))
    (is = 0 (nth-value 2 (tuatara.bench::measuring
                           (dotimes (i 100)
                             (incf (tuatara.syntax:syntax-generation syntax))
                             (tuatara.syntax::log-edit syntax batch)))))
    (let ((now (tuatara.syntax:syntax-generation syntax)))
      (is = 3 (length (tuatara.syntax::edits-since syntax (- now 3))))
      (false (tuatara.syntax::edits-since syntax (- now (1+ tuatara.syntax::*edits-logged*)))))
    (tuatara.buffer:kill-buffer buffer)))

(define-test (syntax tree-copy-let-go-when-the-parse-is-not-asked)
  ;; the copy of a tree made for a parse along with an edit is let go when
  ;; asking for the parse fails before the parse has it
  (let* ((text (with-output-to-string (s) (dotimes (i 20) (format s "fn f~d() {}~%" i))))
         (buffer (tuatara.buffer:make-buffer "*syntax-copy*" :text text))
         (made nil) (let-go '()))
    (unwind-protect
         (let ((syntax (tuatara.syntax:attach-syntax buffer (tuatara.syntax:find-grammar "rust"))))
           (parse-done syntax)
           (sb-int:encapsulate 'tuatara.syntax:edit-tree 'watching
                               (lambda (function &rest arguments)
                                 (multiple-value-bind (copy told) (apply function arguments)
                                   (setf made copy)
                                   (values copy told))))
           (sb-int:encapsulate 'tuatara.syntax:delete-tree 'watching
                               (lambda (function tree) (push tree let-go) (funcall function tree)))
           (sb-int:encapsulate 'tuatara.syntax::parse-time-limit 'failing
                               (lambda (function &rest arguments)
                                 (declare (ignore function arguments))
                                 (error "Asking for the parse failed.")))
           (unwind-protect
                (handler-case (tuatara.buffer:buffer-insert buffer 0 " ") (simple-error () nil))
             (sb-int:unencapsulate 'tuatara.syntax::parse-time-limit 'failing)
             (sb-int:unencapsulate 'tuatara.syntax:delete-tree 'watching)
             (sb-int:unencapsulate 'tuatara.syntax:edit-tree 'watching))
           (true made)
           (true (member made let-go)))
      (tuatara.syntax:detach-syntax buffer)
      (tuatara.buffer:kill-buffer buffer))))

(define-test (syntax tree-copy-let-go-once)
  ;; the copy of a tree made with an edit, let go and then stopped there (an
  ;; error, or C-g), is let go once: its number, perhaps given to another tree
  ;; since, is not let go again
  (let* ((text (with-output-to-string (s) (dotimes (i 20) (format s "fn f~d() {}~%" i))))
         (buffer (tuatara.buffer:make-buffer "*syntax-once*" :text text))
         (made nil) (stopping t) (requests '()))
    (unwind-protect
         (let ((syntax (tuatara.syntax:attach-syntax buffer (tuatara.syntax:find-grammar "rust"))))
           (parse-done syntax)
           (sb-int:encapsulate 'tuatara.syntax:edit-tree 'watching
                               (lambda (function &rest arguments)
                                 (multiple-value-bind (copy told) (apply function arguments)
                                   (setf made copy)
                                   (values copy told))))
           ;; stopped just after the copy is let go
           (sb-int:encapsulate 'tuatara.syntax:delete-tree 'stopping
                               (lambda (function tree)
                                 (funcall function tree)
                                 (when (and stopping (eq tree made))
                                   (setf stopping nil)
                                   (error "Stopped."))))
           (sb-int:encapsulate 'tuatara.syntax::call-send 'counting
                               (lambda (function call &rest arguments)
                                 (when (= (sb-sys:sap-ref-32 (tuatara.syntax::call-out call) 8)
                                          tuatara.syntax::+op-tree-delete+)
                                   (push (sb-sys:sap-ref-32 (tuatara.syntax::call-out call) 12) requests))
                                 (apply function call arguments)))
           (unwind-protect
                ;; too large to parse now: the copy is let go at once
                (let ((tuatara.syntax:*syntax-size-limit* 10))
                  (handler-case (tuatara.buffer:buffer-insert buffer 0 " ") (simple-error () nil)))
             (sb-int:unencapsulate 'tuatara.syntax::call-send 'counting)
             (sb-int:unencapsulate 'tuatara.syntax:delete-tree 'stopping)
             (sb-int:unencapsulate 'tuatara.syntax:edit-tree 'watching))
           (true made)
           (is = 1 (count (tuatara.syntax::tree-number made) requests) "requests to let go of ~s" requests))
      (tuatara.syntax:detach-syntax buffer)
      (tuatara.buffer:kill-buffer buffer))))

(define-test (syntax colours-kept-for-several-ranges)
  ;; two parts of a buffer shown at once keep their colours, each worked out once
  (let* ((text (with-output-to-string (s) (dotimes (i 40) (format s "fn f~d() {}~%" i))))
         (buffer (tuatara.buffer:make-buffer "*syntax-kept*" :text text))
         (grammar (tuatara.syntax:find-grammar "rust"))
         (query (tuatara.syntax:compile-query grammar (tuatara.defaults::language-rules (language-named 'rust))))
         (faces (tuatara.defaults::query-faces query :rich 0)))
    (unwind-protect
         (let* ((syntax (tuatara.syntax:attach-syntax buffer grammar))
                (tuatara.syntax:*syntax-wait* 30)
                (top (tuatara.syntax:syntax-highlights syntax query faces (list (cons 0 100))))
                (bottom (tuatara.syntax:syntax-highlights syntax query faces (list (cons 300 400)))))
           (true top)
           (true bottom)
           (is eq top (tuatara.syntax:syntax-highlights syntax query faces (list (cons 0 100))))
           (is eq bottom (tuatara.syntax:syntax-highlights syntax query faces (list (cons 300 400)))))
      (tuatara.syntax:detach-syntax buffer)
      (tuatara.buffer:kill-buffer buffer))))

(define-test (syntax slow-lines-of-a-large-text-parsed-once)
  ;; a text too large to parse whole whose lines parse too slowly is not parsed again on every scroll
  (let* ((text (with-output-to-string (s) (dotimes (i 400) (format s "fn f~d(p: u8) -> u8 { g(p) }~%" i))))
         (buffer (tuatara.buffer:make-buffer "*syntax-slow-slice*" :text text))
         (grammar (tuatara.syntax:find-grammar "rust"))
         (query (tuatara.syntax:compile-query grammar (tuatara.defaults::language-rules (language-named 'rust))))
         (faces (tuatara.defaults::query-faces query :rich 0))
         (parses 0))
    (sb-int:encapsulate 'tuatara.syntax::parse-ranges 'counting
                        (lambda (function &rest arguments) (incf parses) (apply function arguments)))
    (unwind-protect
         (let* ((tuatara.syntax:*syntax-size-limit* 1000)
                (tuatara.syntax::*injection-time-limit* 1d-9)
                (syntax (tuatara.syntax:attach-syntax buffer grammar)))
           (true (tuatara.syntax:syntax-too-big syntax))
           (false (tuatara.syntax:syntax-highlights syntax query faces (list (cons 0 100))))
           (is eq :slow (tuatara.syntax:syntax-broken syntax))
           (false (tuatara.syntax:syntax-highlights syntax query faces (list (cons 3000 3100))))
           (is = 1 parses))
      (sb-int:unencapsulate 'tuatara.syntax::parse-ranges 'counting)
      (tuatara.syntax:detach-syntax buffer)
      (tuatara.buffer:kill-buffer buffer))))

(define-test (syntax defaults-name-only-exported-syntax)
  ;; the limits, child ranges, group counts and bracket reach the defaults use are
  ;; exported, and no file of the defaults reaches into tuatara.syntax with two colons
  (dolist (name '("PATTERN-GROUP-COUNT" "+BRACKET-REACH+" "NODE-CHILD-RANGES" "PARSE-MEMORY-LIMIT" "PARSE-TIME-LIMIT"))
    (is eq :external (nth-value 1 (find-symbol name :tuatara.syntax)) name))
  (true (< 100 (length (source-files "lisp"))))
  (dolist (file (source-files "lisp"))
    (false (search "tuatara.syntax::" (uiop:read-file-string file)) "~a" (enough-namestring file))))

(define-test (syntax no-sandbox-said-as-it-is)
  ;; the message without a sandbox says that text is not parsed, in words that
  ;; read right for both kinds of text
  (flet ((said (trusted)
           (let ((tuatara.command::*messages-kept* (list '())))
             (tuatara.defaults::report-no-sandbox-for-grammars trusted))))
    (true (search "Files outside the projects you trust are not coloured or parsed" (said nil)))
    (true (search "No text is coloured or parsed" (said t)))))

(define-test (syntax deeply-nested-text-parsed)
  ;; text nested hundreds of levels deep, a string open across lines in it:
  ;; the scanner keeps the indents it saves within the room the parser gives
  ;; it, and Svelte's scanner follows template strings nested thousands deep
  ;; without a call within a call for each; the text is parsed, not the
  ;; program parsing it ended
  (flet ((deep-yaml (n)
           (with-output-to-string (o)
             (dotimes (i n) (format o "~va:~%" i ""))
             (format o "~vax: 1~%" n "")))
         (deep-python (n)
           (with-output-to-string (o)
             (dotimes (i n) (format o "~vaif x:~%" i ""))
             (format o "~vax = f\"\"\"a~%b~%\"\"\"~%" n ""))))
    (loop for (grammar text) in (list (list "yaml" (deep-yaml 300)) (list "python" (deep-python 600))
                                      (list "mojo" (deep-python 600))
                                      ;; Typst's list items, each inside the one before
                                      (list "typst" (with-output-to-string (o)
                                                      (dotimes (i 400) (format o "~va- item~%" (* 2 i) ""))))
                                      ;; Svelte's template strings, each in a ${...} of the one before
                                      (list "svelte" (with-output-to-string (o)
                                                       (write-string "{" o)
                                                       (dotimes (i 300000) (write-string "`${" o))
                                                       (write-string "x" o)
                                                       (dotimes (i 300000) (write-string "}`" o))
                                                       (format o "}~%"))))
          do (let* ((parser (tuatara.syntax:make-parser (tuatara.syntax:find-grammar grammar)
                                                        (tuatara.syntax:kept-helper :shared))))
               (multiple-value-bind (tree why) (tuatara.syntax:parse-rope parser (tuatara.text:string->rope text))
                 (true tree "~a nested deeply is parsed (~s)" grammar why)
                 (when tree (tuatara.syntax:delete-tree tree)))
               (ignore-errors (tuatara.syntax:delete-parser parser))))))

(define-test (syntax brace-left-open-parsed-at-once)
  ;; a brace left open in a long run of text: while Svelte's parser recovers
  ;; from the error, its scanner does not read the rest of the text again
  ;; from each place, so the parse takes a moment, not minutes
  (let* ((run (make-string 100000 :initial-element #\M))
         (parser (tuatara.syntax:make-parser (tuatara.syntax:find-grammar "svelte")
                                             (tuatara.syntax:kept-helper :shared))))
    (multiple-value-bind (tree why)
        (tuatara.syntax:parse-rope parser (tuatara.text:string->rope (concatenate 'string run "{" run)) :limit 2)
      (true tree "parsed within two seconds (~s)" why)
      (when tree (tuatara.syntax:delete-tree tree)))
    (ignore-errors (tuatara.syntax:delete-parser parser))))

(define-test (syntax rules-not-answered-asked-again)
  ;; rules the program compiling them did not answer for (a busy moment) are
  ;; not taken for failed: asked for again after a wait, they compile
  (let* ((language (tuatara.defaults::find-language 'toml))
         (kept (tuatara.defaults::language-query language))
         (tuatara.defaults::*rules-unanswered-wait* 0.2))
    (tuatara.syntax:kept-helper :rules)
    (setf (tuatara.defaults::language-query language) nil)
    (remhash language tuatara.defaults::*rules-unanswered*)
    (unwind-protect
         (progn
           (sb-int:encapsulate 'tuatara.syntax::send-query 'dead
                               (lambda (function call &rest arguments)
                                 (declare (ignore function arguments))
                                 (setf (tuatara.syntax::call-status call) :dead)))
           (unwind-protect
                (false (tuatara.defaults::language-compiled-query language :colouring))
             (sb-int:unencapsulate 'tuatara.syntax::send-query 'dead))
           (false (tuatara.defaults::language-query language) "not kept as failed")
           (false (tuatara.defaults::language-compiled-query language :colouring) "not asked again at once")
           (sleep 0.3)
           (true (tuatara.syntax:query-p (tuatara.defaults::language-compiled-query language :colouring))
                 "compiled when asked again"))
      (remhash language tuatara.defaults::*rules-unanswered*)
      (unless (tuatara.syntax:query-p (tuatara.defaults::language-query language))
        (setf (tuatara.defaults::language-query language) kept)))))

(define-test (syntax large-rules-given-time)
  ;; compiling rules waits longer than other calls, the longer the rules, and
  ;; lateness there is slowness, not a hang
  (let ((asked '())
        (grammar (tuatara.syntax:find-grammar "toml"))
        (source (tuatara.defaults::language-rules (tuatara.defaults::find-language 'toml))))
    (sb-int:encapsulate 'tuatara.syntax::call-send 'seen
                        (lambda (function call &rest keys)
                          (push keys asked)
                          (apply function call keys)))
    (unwind-protect (tuatara.syntax:compile-query grammar source)
      (sb-int:unencapsulate 'tuatara.syntax::call-send 'seen))
    (let ((keys (first asked)))
      (is eq :slow (getf keys :late))
      (true (and (getf keys :seconds) (> (getf keys :seconds) tuatara.syntax:*syntax-helper-wait*))))))

(define-test (syntax slow-rules-never-hold-the-editor)
  ;; a key needing rules not compiled yet (RET indenting) waits no longer
  ;; than *rules-editor-wait*, however slow the compiling, and the rules
  ;; are kept once compiled in the background
  (let* ((language (tuatara.defaults::find-language 'rust))
         (kept (tuatara.defaults::language-indent-query language))
         (tuatara.defaults::*rules-editor-wait* 0.2))
    (tuatara.syntax:kept-helper :rules)
    (setf (tuatara.defaults::language-indent-query language) nil)
    (sb-int:encapsulate 'tuatara.syntax:compile-query 'slow
                        (lambda (function grammar source &rest more)
                          (when (equal (first more) "indentation") (sleep 3))
                          (apply function grammar source more)))
    (unwind-protect
         (let ((began (get-internal-real-time)))
           (with-file-session ((f editor) "C-e i RET" (format nil "fn f() {~%}~%") "rs")
             (true (< (tuatara.runtime:seconds-since began) 2.5) "~,2f s" (tuatara.runtime:seconds-since began)))
           (true (loop repeat 100 thereis (tuatara.syntax:query-p (tuatara.defaults::language-indent-query language))
                       do (sleep 0.1))
                 "kept once compiled"))
      (sb-int:unencapsulate 'tuatara.syntax:compile-query 'slow)
      (unless (tuatara.syntax:query-p (tuatara.defaults::language-indent-query language))
        (setf (tuatara.defaults::language-indent-query language) kept)))))

(define-test (syntax rules-unanswered-given-up-at-last)
  ;; rules the program compiling them never answers for are given up on
  ;; after *rules-unanswered-most* times, as each time it is ended
  (let* ((language (tuatara.defaults::find-language 'toml))
         (kept (tuatara.defaults::language-query language))
         (tuatara.defaults::*rules-unanswered-wait* 0.01)
         (tuatara.defaults::*rules-unanswered-most* 2)
         (tuatara.defaults::*rules-quiet* t))
    (tuatara.syntax:kept-helper :rules)
    (setf (tuatara.defaults::language-query language) nil)
    (remhash language tuatara.defaults::*rules-unanswered*)
    (sb-int:encapsulate 'tuatara.syntax::send-query 'dead
                        (lambda (function call &rest arguments)
                          (declare (ignore function arguments))
                          (setf (tuatara.syntax::call-status call) :dead)))
    (unwind-protect
         (progn
           (tuatara.defaults::language-compiled-query language :colouring)
           (false (tuatara.defaults::language-query language) "asked again after the first")
           (sleep 0.05)
           (tuatara.defaults::language-compiled-query language :colouring)
           (is eq :failed (tuatara.defaults::language-query language) "given up on after the second"))
      (sb-int:unencapsulate 'tuatara.syntax::send-query 'dead)
      (remhash language tuatara.defaults::*rules-unanswered*)
      (setf (tuatara.defaults::language-query language) kept))))

(define-test (syntax helper-run-where-its-directory-is-shown)
  ;; the program that parses, found in a directory written through a link, is run, shut in, at a
  ;; path within the place that directory is shown: through a link in /tmp, which the sandbox has
  ;; as its own, the path as written; through one elsewhere, the place the link leads to
  (with-test-directory (directory)
    (let* ((real (ensure-directories-exist (merge-pathnames "grammars/" directory)))
           (elsewhere (uiop:native-namestring (merge-pathnames "linked" directory)))
           (in-tmp (format nil "/tmp/tuatara-test-~d-~36r" (sb-posix:getpid) (random (expt 36 8)))))
      (alexandria:write-string-into-file "" (merge-pathnames "tuatara-syntax-helper" real))
      (tuatara.io:make-symbolic-link (string-right-trim "/" (uiop:native-namestring real)) elsewhere)
      (tuatara.io:make-symbolic-link (string-right-trim "/" (uiop:native-namestring real)) in-tmp)
      (unwind-protect
           (dolist (link (list in-tmp elsewhere))
             (let* ((program (tuatara.syntax:syntax-helper-program
                              (list (uiop:ensure-directory-pathname link))))
                    (run (tuatara.syntax::shut-in-helper-program program))
                    (arguments (tuatara.io::sandbox-arguments run '() nil nil :directories (list link)))
                    (shown (tuatara.io:real-place link)))
               (true (member shown arguments :test #'string=) "~a shown" shown)
               (true (uiop:string-prefix-p (concatenate 'string shown "/") run) "~a run within ~a" run shown)))
        (tuatara.io:remove-file in-tmp)))))
