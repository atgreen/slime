;;;; swank-torcl.lisp --- Swank backend for the TorCL Common Lisp system.
;;;;
;;;; Single-threaded (communication style NIL): TorCL has threads, but the
;;;; nil style needs nothing from them and is what this backend is tested at.
;;;;
;;;; This code has been placed in the Public Domain.  All warranties
;;;; are disclaimed.

(defpackage swank/torcl
  (:use cl swank/backend))

(in-package swank/torcl)

;;;; Communication style

(defimplementation preferred-communication-style ()
  nil)

;;;; TCP sockets
;;;
;;; Over TorCL's own socket primitives. A "socket" here is whatever
;;; %SOCKET-LISTEN answered; the backend only ever hands it back.

(defimplementation create-socket (host port &key backlog)
  (declare (ignore host))
  (torcl::%socket-listen "127.0.0.1" port (or backlog 5)))

(defimplementation local-port (socket)
  (torcl::%socket-local-port socket))

(defimplementation close-socket (socket)
  (torcl::%socket-close socket))

(defimplementation accept-connection (socket &key external-format buffering timeout)
  (declare (ignore external-format buffering timeout))
  (torcl::%socket-accept socket))

(defimplementation socket-fd (socket)
  (if (integerp socket) socket (torcl::%socket-fd socket)))

(defimplementation wait-for-input (streams &optional timeout)
  ;; The ready streams, or NIL on timeout. TIMEOUT: NIL blocks, T polls once, a
  ;; real is seconds. The nil-style serve loop waits on a single stream, which
  ;; is why that case can block in the runtime rather than spin here.
  (let ((milliseconds (cond ((null timeout) nil)
                            ((eq timeout t) 0)
                            (t (max 0 (round (* timeout 1000)))))))
    (if (and streams (null (cdr streams)))
        (when (torcl::%socket-wait-for-input (car streams) milliseconds)
          streams)
        (remove-if-not (lambda (stream) (torcl::%socket-wait-for-input stream 0))
                       streams))))

;;;; Streams

(defimplementation make-output-stream (write-string)
  (declare (ignore write-string))
  *standard-output*)

(defimplementation make-input-stream (read-string)
  (declare (ignore read-string))
  *standard-input*)

(defimplementation make-fd-stream (fd external-format)
  (declare (ignore fd external-format))
  ;; The nil communication style never needs one, and answering a stream that
  ;; is not connected to FD would be worse than saying so.
  (error "MAKE-FD-STREAM is not implemented on TorCL"))

(defimplementation dup (fd)
  fd)

;;;; Process

(defimplementation getpid ()
  (torcl::%getpid))

(defimplementation quit-lisp ()
  (torcl::%exit 0))

(defimplementation lisp-implementation-type-name ()
  "torcl")

;;;; Compilation
;;;
;;; TorCL compiles as it evaluates and tiers up on its own, so there is no
;;; separate compilation step to hook. Neither entry point reports conditions
;;; as compiler notes: nothing here parses them out of the evaluator yet, so
;;; the third value is NIL rather than a guess.

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
  (declare (ignore output-file external-format policy))
  (when load-p
    (load input-file))
  (values t nil nil))

;;;; Gray streams

(defimplementation gray-package-name ()
  "TORCL-GRAY-STREAMS")
