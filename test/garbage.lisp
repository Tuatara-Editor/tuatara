;;;; test/garbage.lisp --- When garbage is collected, and the room the heap has.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test garbage)

(define-test (garbage collected-when-idle)
  ;; started with the editor, as a tick
  (run-keys "")
  (true (member 'tuatara.defaults::idle-garbage-tick tuatara.command:*tick-functions*))
  (let ((tuatara.defaults::*nursery-octets* (* 4 1024 1024))
        (tuatara-user:*idle-collection-seconds* 0.3)
        (garbage nil))
    (sb-ext:gc)
    ;; little made since the last collection: nothing to do, not called again before a key
    (false (tuatara.defaults::idle-garbage-tick nil))
    (setf garbage (loop repeat 4096 collect (make-array 1024 :element-type '(unsigned-byte 8))))
    (is = 4096 (length garbage))
    (setf garbage nil)
    (true (>= (tuatara.defaults::consed-since-gc) (* 2 1024 1024)))
    ;; a key just typed: not now, looked at again when the quiet time is up
    (let ((tuatara.command:*last-input-time* (get-internal-real-time))
          (before sb-ext:*gc-run-time*))
      (let ((again (tuatara.defaults::idle-garbage-tick nil)))
        (true (and (realp again) (< 0 again 0.31)) "~a" again))
      (is = before sb-ext:*gc-run-time*))
    ;; left alone for longer: collected now, and nothing waits after it
    (let ((tuatara.command:*last-input-time* (- (get-internal-real-time) internal-time-units-per-second))
          (before sb-ext:*gc-run-time*))
      (false (tuatara.defaults::idle-garbage-tick nil))
      (true (> sb-ext:*gc-run-time* before))
      (true (< (tuatara.defaults::consed-since-gc) (* 2 1024 1024)))
      (false (tuatara.defaults::idle-garbage-tick nil)))
    ;; off: never
    (let ((tuatara-user:*idle-collection-seconds* nil))
      (setf garbage (loop repeat 4096 collect (make-array 1024 :element-type '(unsigned-byte 8))))
      (is = 4096 (length garbage))
      (setf garbage nil)
      (false (tuatara.defaults::idle-garbage-tick nil)))))

(define-test (garbage none-collected-while-a-key-waits)
  ;; a key waiting to be answered is answered first: neither the start-up collection nor an idle one comes before it
  (let ((f (tuatara.frontend.fake:make-fake-frontend :width 80 :height 8))
        (results nil))
    (setf (tuatara.frontend.fake:fake-pending f) t)
    (queue-test-keys
     f (list (lambda ()
               (let ((tuatara.defaults::*start-up-garbage-collected* nil)
                     (tuatara.defaults::*start-up-tick-time*
                       (- (get-internal-real-time) (* 2 internal-time-units-per-second)))
                     (tuatara.defaults::*nursery-octets* (* 4 1024 1024))
                     (tuatara-user:*idle-collection-seconds* 0.3)
                     (tuatara.command:*last-input-time* (- (get-internal-real-time) internal-time-units-per-second))
                     (garbage (loop repeat 4096 collect (make-array 1024 :element-type '(unsigned-byte 8))))
                     (before sb-ext:*gc-run-time*))
                 (setf results (list (tuatara.defaults::start-up-garbage-tick nil)
                                     tuatara.defaults::*start-up-garbage-collected*
                                     (tuatara.defaults::idle-garbage-tick nil)
                                     (- sb-ext:*gc-run-time* before)
                                     (length garbage)))))))
    (call-without-grammar-questions
     (lambda () (tuatara.command:run-editor f :setup #'tuatara.defaults:install-defaults)))
    (destructuring-bind (start-up collected idle collecting made) results
      (is = 4096 made)
      (is eql 1 start-up)
      (false collected)
      (true (realp idle) "~a" idle)
      (is = 0 collecting))))

(define-test (garbage room-in-the-heap)
  ;; room asked for: there, or after collecting; never more than the heap
  (true (tuatara.defaults::heap-room-p 1024))
  (false (tuatara.defaults::heap-room-p (* 2 (sb-ext:dynamic-space-size))))
  (let ((budget (tuatara.defaults::heap-budget nil)))
    (true (< 0 budget (tuatara.defaults::heap-free))))
  (is = 12345 (tuatara.defaults::heap-budget 12345)))
