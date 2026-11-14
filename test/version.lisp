;;;; test/version.lisp --- Versions, position mapping and anchors.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(defun map-position-slowly (position edits bias)
  "Oracle for MAP-POSITION: apply the edits one at a time, from the last to the
first so that earlier edits' positions stay valid, tracking POSITION through
each with the documented rules."
  (let ((delta-before (make-array (length edits) :element-type 'fixnum))
        (acc 0))
    (loop for k from 0 below (length edits)
          for e = (aref edits k)
          do (setf (aref delta-before k) acc)
             (incf acc (- (tuatara.text:edit-insert-length e) (tuatara.text:edit-delete-length e))))
    ;; Find the edits that "contain" the position and apply the rule for the relevant one.
    (let ((result position) (handled nil))
      (loop for k from (1- (length edits)) downto 0
            for e = (aref edits k)
            for start = (tuatara.text:edit-start e)
            for end = (tuatara.text:edit-end e)
            do (cond (handled)
                     ((> position end)
                      (setf result (+ position (aref delta-before k)
                                      (- (tuatara.text:edit-insert-length e) (tuatara.text:edit-delete-length e)))
                            handled t))
                     ((or (< start position) (and (eq bias :after) (= start position)))
                      (setf result (+ start (aref delta-before k)
                                      (if (eq bias :after) (tuatara.text:edit-insert-length e) 0))
                            handled t))))
      result)))

(define-test version
  :description "Versions link edits into a history; positions and anchors map forward through it.")

(define-test (version map-position-matches-oracle)
  (let ((*random-state* (sb-ext:seed-random-state 1302)))
    (dotimes (round 40)
      (let* ((s (random-text 400))
             (v0 (tuatara.text:make-version (tuatara.text:string->rope s))))
        (multiple-value-bind (edits expected) (random-batch s)
          (declare (ignore expected))
          (multiple-value-bind (v1 batch) (tuatara.text:version-commit v0 edits)
            (let ((old-len (tuatara.text:rope-octet-length (tuatara.text:version-rope v0)))
                  (new-len (tuatara.text:rope-octet-length (tuatara.text:version-rope v1)))
                  (last-before -1) (last-after -1))
              (loop for p from 0 to old-len
                    do (let ((b (tuatara.text:map-position p batch :before))
                             (a (tuatara.text:map-position p batch :after)))
                         (is = (map-position-slowly p edits :before) b)
                         (is = (map-position-slowly p edits :after) a)
                         (true (<= 0 b new-len)) (true (<= 0 a new-len))
                         (true (<= b a) "before ~a should not exceed after ~a at ~a" b a p)
                         ;; monotonic in p
                         (true (>= b last-before)) (true (>= a last-after))
                         (setf last-before b last-after a))))))))))

(define-test (version anchors-follow-edits)
  (let* ((s "hello world")
         (v0 (tuatara.text:make-version (tuatara.text:string->rope s)))
         (a-before (tuatara.text:make-anchor v0 5))
         (a-after (tuatara.text:make-anchor v0 5 :bias :after))
         (a-end (tuatara.text:make-anchor v0 11))
         (v1 (tuatara.text:version-commit v0 (vector (tuatara.text:make-edit 5 5 (to-octets ", big")))))
         (v2 (tuatara.text:version-commit v1 (vector (tuatara.text:make-edit 0 6))))) ; delete "hello,"
    (is string= "hello, big world" (tuatara.text:rope-string (tuatara.text:version-rope v1)))
    (is string= " big world" (tuatara.text:rope-string (tuatara.text:version-rope v2)))
    (is = 5 (tuatara.text:anchor-position a-before v1))
    (is = 10 (tuatara.text:anchor-position a-after v1))
    (is = 16 (tuatara.text:anchor-position a-end v1))
    ;; resolving straight from v0 to v2 walks two batches
    (let ((fresh (tuatara.text:make-anchor v0 5 :bias :after)))
      (is = 4 (tuatara.text:anchor-position fresh v2)))
    (is = 0 (tuatara.text:anchor-position a-before v2))
    (is = 4 (tuatara.text:anchor-position a-after v2))
    (is = 10 (tuatara.text:anchor-position a-end v2))
    ;; versions are linked and only the current one accepts edits
    (is equal (list (tuatara.text:version-batch v1)) (tuatara.text:version-batches v0 v1))
    (false (tuatara.text:version-current-p v0))
    (true (tuatara.text:version-current-p v2))
    (fail (tuatara.text:version-commit v0 (vector (tuatara.text:make-edit 0 0 (to-octets "x")))))
    ;; undo edits of v2 restore v1's text
    (let ((v3 (tuatara.text:version-commit v2 (tuatara.text:version-undo-edits v2))))
      (is string= "hello, big world" (tuatara.text:rope-string (tuatara.text:version-rope v3))))
    ;; an anchor cannot be resolved against an unrelated version
    (fail (tuatara.text:anchor-position (tuatara.text:make-anchor v2 0) v0))
    (fail (tuatara.text:make-anchor v0 99))))

(define-test (version random-history)
  ;; many versions; anchors created at random points resolve to the same result
  ;; as resolving step by step
  (let* ((s (random-text 300))
         (v (tuatara.text:make-version (tuatara.text:string->rope s)))
         (versions (list v))
         (anchors '()))
    (dotimes (step 30)
      (multiple-value-bind (edits expected) (random-batch s)
        (setf v (tuatara.text:version-commit v edits) s expected)
        (push v versions)
        ;; anchors sit on character boundaries, as every caller's positions do
        (let ((offsets (byte-offsets s)))
          (push (cons (tuatara.text:make-anchor v (aref offsets (random (length offsets)))
                                                :bias (if (zerop (random 2)) :before :after))
                      v)
                anchors))))
    (let ((current v))
      (loop for (a . made-in) in anchors
            do (let* ((stepwise (let ((p (tuatara.text:anchor-position a made-in)))
                                  (dolist (batch (tuatara.text:version-batches made-in current) p)
                                    (setf p (tuatara.text:map-position p batch (tuatara.text:anchor-bias a))))))
                      (resolved (tuatara.text:anchor-position a current)))
                 (is = stepwise resolved)
                 (true (<= 0 resolved (tuatara.text:rope-octet-length (tuatara.text:version-rope current))))
                 (true (tuatara.text:char-boundary-p (tuatara.text:version-rope current) resolved)))))))

(define-test (version anchors-hold-no-text)
  ;; an anchor left unresolved keeps the edits made since, never the texts:
  ;; the rope it was made in, and a whole text swapped since, are let go
  (let* ((v (tuatara.text:make-version (tuatara.text:make-rope (make-string 100000 :initial-element #\a))))
         (anchor (tuatara.text:make-anchor v 5))
         (first-text (sb-ext:make-weak-pointer (tuatara.text:version-rope v))))
    (setf v (tuatara.text:version-commit v (list (tuatara.text:make-edit 0 0 (tuatara.text:string-octets "xy")))))
    (setf v (tuatara.text:version-swap v (tuatara.text:make-rope "whole")
                                       (lambda (p bias) (declare (ignore bias)) (min p 5))
                                       (lambda (p bias) (declare (ignore bias)) p)))
    (setf v (tuatara.text:version-commit v (list (tuatara.text:make-edit 0 0 (tuatara.text:string-octets "z")))))
    (loop repeat 5 while (sb-ext:weak-pointer-value first-text) do (sb-ext:gc :full t))
    (false (sb-ext:weak-pointer-value first-text))
    (is = 6 (tuatara.text:anchor-position anchor v))))

(define-test (version old-version-holds-no-later-text)
  ;; a version kept by a job holds its own text and the batches since, never a later text
  (let* ((v0 (tuatara.text:make-version (tuatara.text:make-rope "abc")))
         (v1 (tuatara.text:version-commit v0 (list (tuatara.text:make-edit 0 0 (tuatara.text:string-octets "x")))))
         (later (sb-ext:make-weak-pointer (tuatara.text:version-rope v1))))
    (setf v1 (tuatara.text:version-commit v1 (list (tuatara.text:make-edit 0 0 (tuatara.text:string-octets "y")))))
    (setf v1 nil)
    (loop repeat 5 while (sb-ext:weak-pointer-value later) do (sb-ext:gc :full t))
    (false (sb-ext:weak-pointer-value later))
    (is string= "abc" (tuatara.text:rope-string (tuatara.text:version-rope v0)))))

(define-test (version batch-rope-before-undoes-a-batch)
  (let* ((v0 (tuatara.text:make-version (tuatara.text:make-rope "hello world")))
         (v1 (tuatara.text:version-commit v0 (list (tuatara.text:make-edit 0 5 (tuatara.text:string-octets "bye"))
                                                   (tuatara.text:make-edit 6 11 nil)))))
    (is string= "bye " (tuatara.text:rope-string (tuatara.text:version-rope v1)))
    (is string= "hello world"
        (tuatara.text:rope-string (tuatara.text:batch-rope-before (tuatara.text:version-batch v1)
                                                                  (tuatara.text:version-rope v1))))))

(define-test (version each-way-advances-alike)
  (let* ((v0 (tuatara.text:make-version (tuatara.text:make-rope "abc")))
         (v1 (tuatara.text:version-commit v0 (list (tuatara.text:make-edit 1 2 (tuatara.text:string-octets "X")))))
         (v2 (tuatara.text:version-swap v1 (tuatara.text:make-rope "whole") (lambda (p bias) (declare (ignore bias)) p)
                                        (lambda (p bias) (declare (ignore bias)) p)))
         (v3 (tuatara.text:version-adopt v2 (tuatara.text:version-rope v2))))
    (is equal '(0 1 2 3) (mapcar #'tuatara.text:version-number (list v0 v1 v2 v3)))
    (is equal (list (tuatara.text:version-batch v1)) (tuatara.text:version-batches v0 v1))
    (is eq :whole (tuatara.text:version-batches v1 v2))
    (is equal (list (tuatara.text:version-batch v3)) (tuatara.text:version-batches v2 v3))
    (true (tuatara.text:version-current-p v3))
    (is string= "aXc" (tuatara.text:rope-string (tuatara.text:version-rope v1)))
    (multiple-value-bind (v4 batch) (tuatara.text:version-adopt v3 (tuatara.text:version-rope v3))
      (is eq batch (tuatara.text:version-batch v4)))
    ;; only the newest version moves on, whichever way
    (fail (tuatara.text:version-commit v0 '()))
    (fail (tuatara.text:version-swap v1 (tuatara.text:make-rope "") #'identity #'identity))
    (fail (tuatara.text:version-adopt v2 (tuatara.text:version-rope v2)))))

(define-test (version changed-stretches-make-the-text)
  ;; the stretches changed across many versions, each put in place of what it was, make the
  ;; text of the last; the text between them the same then and now
  (let ((*random-state* (sb-ext:seed-random-state 1303)))
    (dotimes (round 60)
      (let* ((s (random-text 200))
             (first (tuatara.text:make-version (tuatara.text:string->rope s)))
             (v first)
             (steps (random 8)))
        (dotimes (step steps)
          (multiple-value-bind (edits expected) (random-batch s)
            (setf v (tuatara.text:version-commit v edits) s expected)))
        (let* ((before (tuatara.text:string-octets (tuatara.text:rope-string (tuatara.text:version-rope first))))
               (after (tuatara.text:string-octets s))
               (stretches (tuatara.text:changed-stretches first v))
               (made (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
               (old-at 0) (new-at 0))
          (loop for (old-start old-end new-start new-end) in stretches
                do (true (<= old-at old-start old-end))
                   (true (<= new-at new-start new-end))
                   (is = (- old-start old-at) (- new-start new-at))
                   (loop for i from old-at below old-start do (vector-push-extend (aref before i) made))
                   (loop for i from new-start below new-end do (vector-push-extend (aref after i) made))
                   (setf old-at old-end new-at new-end))
          (loop for i from old-at below (length before) do (vector-push-extend (aref before i) made))
          (is equalp after (coerce made '(simple-array (unsigned-byte 8) (*))))
          (when (zerop steps) (is equal '() stretches)))))
    ;; a whole text swapped between: not known
    (let* ((v0 (tuatara.text:make-version (tuatara.text:make-rope "abc")))
           (v1 (tuatara.text:version-swap v0
                                          (tuatara.text:make-rope "whole")
                                          (lambda (p bias) (declare (ignore bias)) p)
                                          (lambda (p bias) (declare (ignore bias)) p))))
      (is eq :whole (tuatara.text:changed-stretches v0 v1)))))
