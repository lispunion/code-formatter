; Author: Juergen Lorenz
; ju (at) jugilo (dot) de
;
; Last update: Sep 03, 2011
;
; Copyright (c) 2011, Juergen Lorenz
; All rights reserved.
;
; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions are
; met:
;
; Redistributions of source code must retain the above copyright
; notice, this list of conditions and the following disclaimer.
;
; Redistributions in binary form must reproduce the above copyright
; notice, this list of conditions and the following disclaimer in the
; documentation and/or other materials provided with the distribution.
;
; Neither the name of the author nor the names of its contributors may be
; used to endorse or promote products derived from this software without
; specific prior written permission.
;
; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
; IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
; TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
; PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
; HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
; TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
; PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;
(module loops *
 (import scheme (only chicken unless case-lambda print))

 (define-syntax do-for
  (syntax-rules ()
   ((_ var (start stop step) xpr xpr1 ...)
    (let ((%stop stop))
     (let loop ((var start))
      (unless (>= var %stop)
       xpr
       xpr1
       ...
       (loop (+ step var))))))
   ((_ var (start stop) xpr . xprs)
    (do-for var (start stop 1) xpr . xprs))))

 (define-syntax do-list
  (syntax-rules ()
   ((_ i lst xpr xpr1 ...)
    (let loop ((sublst lst))
     (if (not (null? sublst))
      (let ((i (car sublst)))
       xpr
       xpr1
       ...
       (loop (cdr sublst))))))))

 (define-syntax do-times
  (syntax-rules ()
   ((_ i upto xpr0 xpr1 ...)
    (let ((n upto))
     (let loop ((i 0))
      (unless (>= i n)
       xpr0
       xpr1
       ...
       (loop (+ i 1))))))))

 (define-syntax do-until
  (syntax-rules ()
   ((_ test? xpr xpr1 ...)
    (let loop ()
     (if (not test?)
      (begin
       xpr
       xpr1
       ...
       (loop)))))))

 (define-syntax do-while
  (syntax-rules ()
   ((_ test? xpr xpr1 ...)
    (let loop ()
     (if test?
      (begin
       xpr
       xpr1
       ...
       (loop)))))))

 ;;; the following macro is unhygienic on purpose,
 ;;; it exports the exit symbol behind the scene.
 ;;; So it can not be defined with syntax-rules
 (define-syntax do-forever
  (ir-macro-transformer
   (lambda (form inject compare?)
    (let ((xpr (cadr form))
          (xprs (cddr form)))
     `(call-with-current-continuation (lambda (,(inject 'exit))
                                       (let loop ()
                                        ,xpr
                                        ,@xprs
                                        (loop))))))))

 ;;; documentation
 (define loops
  (let ((alist '((do-forever "endless loop"
                             (do-forever xpr . xprs)
                             "executes body xpr . xprs until exit is called")
                 (do-times "loops a fixed number of times" (do-times i upto xpr . xprs)
                  "execute xpr . xprs for i in [0 upto[")
                 (do-list "loop along a list" (do-list i lst xpr . xprs)
                  "execute xpr . xprs for i in lst")
                 (do-for
                  "for-loop"
                  (do-for var (start stop step) xpr . xprs)
                  "do xpr . xprs for var in [start stop[ with steps (default 1)")
                 (do-while "while-loop"
                  (do-while test? xpr . xprs)
                  "execute xpr . xprs while test? is true")
                 (do-until "until-loop"
                  (do-until test? xpr . xprs)
                  "execute xpr . xprs while test? is false"))))
   (case-lambda (() (map car alist))
                ((sym) (let ((pair (assq sym alist)))
                        (if pair
                         (cdr pair)
                         (print "Choose one of " (map car alist)))))))))

; Module loops
