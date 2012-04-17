#lang racket/base

(require (only-in racket/match
                  match)
         syntax/strip-context)

(define define-regex
  (pregexp "^#define\\s+(\\w+)\\s+(\\d+)\\s+/\\*\\s+(.+?)\\s+\\*/"))

(define (read-errno path port)
  (syntax->datum (read-errno-syntax path port)))

(define (read-errno-syntax path port)
  (define pairs       
    (for/fold ([l null]) ([line (in-lines port)])
      (match (regexp-match define-regex line)
        [(list _ id v desc) 
         (cons #`(cons  #,(string->number v)
                        #,(format "~a: ~a"
                                  (string->symbol id)
                                  desc))
               l)]
        [_ l])))
  
  (strip-context
   #`(module some-module racket/base 
     (define errno-lookup-table
       (make-immutable-hasheq (list #,@pairs)))
     
     (provide errno-lookup-table))))

(provide (rename-out [read-errno read]
                     [read-errno-syntax read-syntax]))