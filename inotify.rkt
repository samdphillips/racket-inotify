#lang racket/base

(require (only-in '#%foreign 
                  ctype-c->scheme)
         ffi/unsafe
                  
         (rename-in "errno.h" 
                    [errno-lookup-table errno-ext-table])
         (rename-in "errno-base.h" 
                    [errno-lookup-table errno-base-table]))
         

(define _inotify-init-flags
  (_bitmask '(NONE        = 0
              IN_CLOEXEC  = 02000000
              IN_NONBLOCK = 00004000)))

(define errno-lookup
  (let ([errs (for/fold ([h errno-base-table]) ([(k v) (in-hash errno-ext-table)])
                (hash-set h k v))])
    (lambda (n)
      (hash-ref errs n (lambda () (format "UNKNOWN errno: ~a" n))))))

(struct inotify (port watches)
  #:property prop:evt 
  (lambda (in)
    (handle-evt (inotify-port in)
                (lambda (x)
                  (read-inotify in)))))                        

(define (inotify-watches-add! in watch)
  (hash-set! (inotify-watches in) (inotify-watch-descriptor watch) watch))

(define (inotify-watches-remove! in watch)
  (hash-remove! (inotify-watches in) (inotify-watch-descriptor watch)))

(define fd->port
  (let ([f (get-ffi-obj "scheme_make_fd_input_port" #f
                        (_fun _int _racket _int _int -> _racket))])
    (lambda (fd)
      (f fd #f 0 0))))

(define port->fd
  (get-ffi-obj "scheme_get_port_fd" #f
               (_fun _racket -> _int)))

(define _inotify
  (make-ctype _int              
              (lambda (p)
                (unless (inotify? p)
                  (error '_inotify "Not an inotify instance: ~a" p))
                (port->fd (inotify-port p)))
              (lambda (fd)
                (when (< fd 0)
                  (error '_inotify "~a" (errno-lookup (saved-errno))))
                (inotify (fd->port fd) (make-hash)))))
                                      
(define inotify-init
  (let ([f (get-ffi-obj "inotify_init1" #f
                        (_fun #:save-errno 'posix
                              _inotify-init-flags 
                              -> _inotify))])
    (lambda ([flags 'NONE])
      (f flags))))

(define _inotify-event-mask
  (_bitmask
   '(IN_ACCESS              = #x00000001 ;     /* File was accessed */
     IN_MODIFY              = #x00000002 ;     /* File was modified */
     IN_ATTRIB              = #x00000004 ;     /* Metadata changed */
     IN_CLOSE_WRITE         = #x00000008 ;     /* Writtable file was closed */
     IN_CLOSE_NOWRITE       = #x00000010 ;     /* Unwrittable file closed */
     IN_OPEN                = #x00000020 ;     /* File was opened */
     IN_MOVED_FROM          = #x00000040 ;     /* File was moved from X */
     IN_MOVED_TO            = #x00000080 ;     /* File was moved to Y */
     IN_CREATE              = #x00000100 ;     /* Subfile was created */
     IN_DELETE              = #x00000200 ;     /* Subfile was deleted */
     IN_DELETE_SELF         = #x00000400 ;     /* Self was deleted */
     IN_MOVE_SELF           = #x00000800 ;     /* Self was moved */
     ; /* the following are legal events.  they are sent as needed to any watch */
     IN_UNMOUNT             = #x00002000 ;     /* Backing fs was unmounted */
     IN_Q_OVERFLOW          = #x00004000 ;     /* Event queued overflowed */
     IN_IGNORED             = #x00008000 ;     /* File was ignored */
     ; /* special flags */
     IN_ONLYDIR             = #x01000000 ;     /* only watch the path if it is a directory */
     IN_DONT_FOLLOW         = #x02000000 ;     /* don't follow a sym link */
     IN_EXCL_UNLINK         = #x04000000 ;     /* exclude events on unlinked objects */
     IN_MASK_ADD            = #x20000000 ;     /* add to the mask of an already existing watch */
     IN_ISDIR               = #x40000000 ;     /* event occurred against dir */
     IN_ONESHOT             = #x80000000 ;     /* only send event once */
     )))

(define decode-inotify-event-mask (ctype-c->scheme _inotify-event-mask))

(struct inotify-watch (descriptor path))

(define _inotify-watch
  (make-ctype _int
              (lambda (w)
                (unless (inotify-watch? w)
                  (error '_inotify-watch "Expected inotify-watch, got: ~s"
                         w))
                  (inotify-watch-descriptor w))
              #f))

(define inotify-add-watch!
  (let ([add-watch (get-ffi-obj "inotify_add_watch" #f
                                (_fun #:save-errno 'posix
                                      _inotify _file _inotify-event-mask
                                      -> (wd : _inotify-watch)
                                      -> (if (< wd 0)
                                             (error 'inotify-add-watch "~a"
                                                    (errno-lookup (saved-errno)))
                                             wd)))])
    (lambda (in file events)
      (define path  (path->complete-path file))
      (define wd    (add-watch in file events))
      (define watch (inotify-watch wd path))
      (inotify-watches-add! in watch)
      watch)))      

(define (find-watch-path in w)
  (define watch (if path? (path->bytes w) w))
  (for/or ([v (in-hash-values (inotify-watches in))])
    (and (bytes=? watch (path->bytes (inotify-watch-path v))) v)))

(define inotify-rm-watch!
  (let ([rm-watch (get-ffi-obj "inotify_rm_watch" #f
                               (_fun #:save-errno 'posix
                                     _inotify _inotify-watch
                                     -> (result : _int)
                                     -> (if (< result 0)
                                            (error 'inotify-rm-watch "~a"
                                                   (errno-lookup (saved-errno)))
                                            (void))))])
    (lambda (in w)
      (define watch 
        (cond [(inotify-watch? w) w]
              [(and (path-string? w) (find-watch-path in (path->complete-path w)))
               => values]
              [else
                (error 'inotify-rm-watch
                       "could not find path associated with inotify: ~a" w)]))
      (rm-watch in watch)
      (inotify-watches-remove! in watch))))

(struct inotify-event (watch mask cookie path))

(define (read-inotify in)
  (define inp (inotify-port in))
  (define hdr (read-bytes 16 inp))
  
  (define (bytes->integer signed? offset)
    (integer-bytes->integer hdr signed? (system-big-endian?) offset (+ offset 4)))
  
  (define watch  (hash-ref (inotify-watches in) (bytes->integer #t 0)))
  (define mask   (decode-inotify-event-mask (bytes->integer #f 4)))
  (define cookie (bytes->integer #f 8))
  (define len    (bytes->integer #f 12))
  (define path   (if (zero? len)
                     (inotify-watch-path watch)
                     (bytes->path (read-bytes len inp))))
  
  (inotify-event watch mask cookie path))

#|

(define p (inotify-init))
(define w (inotify-add-watch! p "/var/log/syslog" 'IN_MODIFY))


|#
