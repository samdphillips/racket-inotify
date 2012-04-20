#lang racket/base

(require "inotify.rkt")

(define args (current-command-line-arguments))

(when (zero? (vector-length args))
  (error 'inotify-spy "need an argument"))

(define p (inotify-init))

(inotify-add-watch! p (vector-ref args 0)
                    '(IN_MODIFY      IN_MOVED_FROM  IN_MOVED_TO 
                      IN_CREATE      IN_DELETE      
                      IN_DELETE_SELF IN_MOVE_SELF))

(let loop ()
  (define e (sync p))
  (printf "~a ~a~%" 
          (inotify-event-mask e)
          (inotify-event-path e))
  (loop))

