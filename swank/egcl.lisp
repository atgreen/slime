;;;; swank-egcl.lisp --- Swank backend for the EGCL Common Lisp system.
;;;;
;;;; Single-threaded (communication style NIL): EGCL has threads, but the
;;;; nil style needs nothing from them and is what this backend is tested at.
;;;;
;;;; This code has been placed in the Public Domain.  All warranties
;;;; are disclaimed.

(defpackage swank/egcl
  (:use cl swank/backend))

(in-package swank/egcl)

;;;; Communication style

(defimplementation preferred-communication-style ()
  nil)

;;;; TCP sockets
;;;
;;; Over EGCL's own socket primitives. A "socket" here is whatever
;;; %SOCKET-LISTEN answered; the backend only ever hands it back.

(defimplementation create-socket (host port &key backlog)
  (declare (ignore host))
  (egcl::%socket-listen "127.0.0.1" port (or backlog 5)))

(defimplementation local-port (socket)
  (egcl::%socket-local-port socket))

(defimplementation close-socket (socket)
  (egcl::%socket-close socket))

(defimplementation accept-connection (socket &key external-format buffering timeout)
  (declare (ignore external-format buffering timeout))
  (egcl::%socket-accept socket))

(defimplementation socket-fd (socket)
  (if (integerp socket) socket (egcl::%socket-fd socket)))

(defimplementation wait-for-input (streams &optional timeout)
  ;; The ready streams, or NIL on timeout. TIMEOUT: NIL blocks, T polls once, a
  ;; real is seconds. The nil-style serve loop waits on a single stream, which
  ;; is why that case can block in the runtime rather than spin here.
  (let ((milliseconds (cond ((null timeout) nil)
                            ((eq timeout t) 0)
                            (t (max 0 (round (* timeout 1000)))))))
    (if (and streams (null (cdr streams)))
        (when (egcl::%socket-wait-for-input (car streams) milliseconds)
          streams)
        (remove-if-not (lambda (stream) (egcl::%socket-wait-for-input stream 0))
                       streams))))

;;;; Streams

(defimplementation make-fd-stream (fd external-format)
  (declare (ignore fd external-format))
  ;; The nil communication style never needs one, and answering a stream that
  ;; is not connected to FD would be worse than saying so.
  (error "MAKE-FD-STREAM is not implemented on EGCL"))

;;;; Process

(defimplementation getpid ()
  (egcl::%getpid))

(defimplementation quit-lisp ()
  (egcl::%exit 0))

(defimplementation lisp-implementation-type-name ()
  "egcl")

;;;; Compilation
;;;
;;; The file path uses EGCL's compiler and preserves its result values.
;;; Source-position mapping and conversion to Swank compiler notes are not
;;; implemented yet; compiler conditions retain their ordinary Lisp handling.

(defimplementation call-with-compilation-hooks (function)
  (funcall function))

(defimplementation swank-compile-string (string &key buffer position filename line column policy)
  (declare (ignore buffer position filename line column policy))
  (with-input-from-string (stream string)
    (loop for form = (read stream nil :eof)
          until (eq form :eof)
          do (eval form)))
  (values t nil nil))

(defimplementation swank-compile-file (input-file output-file load-p external-format
                                       &key policy)
  (declare (ignore policy))
  (multiple-value-bind (artifact warnings-p failure-p)
      (compile-file input-file :output-file output-file :external-format external-format)
    (when (and load-p artifact (not failure-p))
      (load artifact))
    (values artifact warnings-p failure-p)))

;;;; Gray streams

(defimplementation gray-package-name ()
  "EGCL-GRAY-STREAMS")
