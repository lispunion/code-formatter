(module format
  (read/comments
    tidy
    write/comments
    blank-symbol
    comment-symbol
    widths)

  (import scheme)
  (import simple-loops)
  (import chicken.base)
  (import chicken.string)
  (import chicken.io)
  (import chicken.port)
  (import chicken.sort)
  (import srfi-1)
  (import srfi-13)
  (import srfi-69)
  (import etc)

  (define (read/comments)
   (define (identifier)
    (list->string (collect
                   (if (subsequent? (peek-char))
                    (list (read-char))
                    #f))))

   (define (initial? c)
    (or (char-alphabetic? c)
        (special-initial? c)))

   (define (peek? s)
    (and (not (eof-object? (peek-char)))
         (char=? (peek-char) (string-ref s 0))))

   (define (read*)
    (whitespace)
    (cond
     ((eof-object? (peek-char))
      (peek-char))
     ((peek? "'")
      (read-char)
      (list 'quote (read*)))
     ((peek? "(")
      (read-char)
      (let loop ()
       (whitespace)
       (cond
        ((eof-object? (peek-char))
         (error "unterminated list"))
        ((peek? ")")
         (read-char)
         '())
        ((peek? ".")
         (let ((s (identifier)))
          (if (string= "." s)
           (let ((x (read)))
            (do-while (not (peek? ")"))
             (whitespace)
             (cond
              ((eof-object? (peek-char))
               (error "expected ')'"))
              ((peek? ";")
               (read-line))))
            (read-char)
            x)
           (cons (string->symbol s) (loop)))))
        (else
         (cons (read*) (loop))))))
     ((peek? ",")
      (read-char)
      (if (peek? "@")
       (begin
        (read-char)
        (list 'unquote-splicing (read*)))
       (list 'unquote (read*))))
     ((peek? ";")
      (list comment-symbol (read-line)))
     ((peek? "`")
      (read-char)
      (list 'quasiquote (read*)))
     (else
      (read))))

   (define (special-initial? c)
    (string-contains "!$%&*/:<=>?^_~" (make-string 1 c)))

   (define (special-subsequent? c)
    (string-contains "+-.@" (make-string 1 c)))

   (define (subsequent? c)
    (or (initial? c)
        (char-numeric? c)
        (special-subsequent? c)))

   (define (whitespace)
    (when (and (not (eof-object? (peek-char)))
               (char-whitespace? (peek-char)))
     (read-char)
     (whitespace)))

   (collect
    (let ((x (read*)))
      (if (eof-object? x)
        #f
        (list x)))))

  (define (tidy x)
   ; Space at start of comment
   (set! x
    (map-rec y x
     (if (and (car? comment-symbol y)
              (length? 2 (cadr y))
              (char-alphabetic? (string-ref (cadr y) 1)))
      (list comment-symbol
            (string-append "; " (substring (cadr y) 1 (string-length (cadr y)))))
      y)))

   ; Sort cases
   (set! x
         (map-rec y x
          (if (and (car? 'case y)
                   (length? 2 y))
           (cons* (car y) (cadr y) (sort (cddr y) value<))
           y)))

   ; Sort functions
   (set! x
         (map-rec y x
          (if (list? y)
           (concatenate (for zs (frag defun? y)
                         (if (defun? (car zs))
                          (sort zs value<)
                          zs)))
           y)))

   ; Sort macros
   (set! x
         (map-rec y x
          (if (list? y)
           (concatenate (for zs (frag (curry car? 'define-syntax) y)
                         (if (car? 'define-syntax (car zs))
                          (sort zs value<)
                          zs)))
           y)))

   ; Sort memq
   (set! x
         (map-rec y x
          (if (and (car? 'memq y)
                   (length? 3 y)
                   (car? 'quote (caddr y)))
           (list (car y) (cadr y) (list 'quote (sort (cadr (caddr y)) value<)))
           y)))

   ; Blank line after import
   (set! x
         (map-rec y x
          (transform zs y
           (values (if (and (car? 'import (car zs))
                            (pair? (cdr zs))
                            (not (car? 'import (cadr zs))))
                    (list (car zs) blank-symbol)
                    (list (car zs)))
                   (cdr zs)))))

   ; Blank line after use
   (set! x
         (map-rec y x
          (transform zs y
           (values (if (and (car? 'use (car zs))
                            (pair? (cdr zs))
                            (not (car? 'use (cadr zs))))
                    (list (car zs) blank-symbol)
                    (list (car zs)))
                   (cdr zs)))))

   ; Blank line before include
   (set! x
         (map-rec y x
          (transform zs y
           (values (if (and (not (car? 'include (car zs)))
                            (not (car? comment-symbol (car zs)))
                            (pair? (cdr zs))
                            (car? 'include (cadr zs)))
                    (list (car zs) blank-symbol)
                    (list (car zs)))
                   (cdr zs)))))

   ; Blank line after include
   (set! x
         (map-rec y x
          (transform zs y
           (values (if (and (car? 'include (car zs))
                            (pair? (cdr zs))
                            (not (car? 'include (cadr zs))))
                    (list (car zs) blank-symbol)
                    (list (car zs)))
                   (cdr zs)))))

   ; Blank line before comment
   (set! x
         (map-rec y x
          (transform zs y
           (values (if (and (not (car? comment-symbol (car zs)))
                            (pair? (cdr zs))
                            (car? comment-symbol (cadr zs)))
                    (list (car zs) blank-symbol)
                    (list (car zs)))
                   (cdr zs)))))

   ; Blank line after macro
   (set! x
         (map-rec y x
          (transform zs y
           (values (if (and (car? 'define-syntax (car zs))
                            (not (cadr? blank-symbol zs)))
                    (list (car zs) blank-symbol)
                    (list (car zs)))
                   (cdr zs)))))

   ; Blank line after record
   (set! x
         (map-rec y x
          (transform zs y
           (values (if (and (car? 'define-record-type (car zs))
                            (not (cadr? blank-symbol zs)))
                    (list (car zs) blank-symbol)
                    (list (car zs)))
                   (cdr zs)))))
   (set! x
         (map-rec y x
          (transform zs y
           (values (if (and (not (car? 'define-record-printer (car zs)))
                            (not (car? comment-symbol (car zs)))
                            (pair? (cdr zs))
                            (car? 'define-record-printer (cadr zs)))
                    (list (car zs) blank-symbol)
                    (list (car zs)))
                   (cdr zs)))))
   (set! x
         (map-rec y x
          (transform zs y
           (values (if (and (car? 'define-record-printer (car zs))
                            (not (cadr? blank-symbol zs)))
                    (list (car zs) blank-symbol)
                    (list (car zs)))
                   (cdr zs)))))
   (set! x
         (map-rec y x
          (transform zs y
           (values (if (and (car? 'define (car zs))
                            (pair? (cdr zs))
                            (car? 'defstruct (cadr zs)))
                    (list (car zs) blank-symbol)
                    (list (car zs)))
                   (cdr zs)))))
   (set! x
         (map-rec y x
          (transform zs y
           (values (if (and (car? 'defstruct (car zs))
                            (not (cadr? blank-symbol zs)))
                    (list (car zs) blank-symbol)
                    (list (car zs)))
                   (cdr zs)))))

   ; Blank line after function
   (set! x
         (map-rec y x
          (transform zs y
           (values (if (and (defun? (car zs))
                            (not (cadr? blank-symbol zs)))
                    (list (car zs) blank-symbol)
                    (list (car zs)))
                   (cdr zs)))))

   ; Remove multiple blanks
   (set! x
         (map-rec y x
          (transform zs y
           (values (list (car zs))
                   (if (car? blank-symbol zs)
                    (drop-while (curry eq? blank-symbol) zs)
                    (cdr zs))))))

   ; Remove trailing blanks
   (set! x
         (map-rec y x
          (transform zs y
           (values (if (and (car? blank-symbol zs)
                            (null? (cdr zs)))
                    '()
                    (list (car zs)))
                   (cdr zs)))))

   ; Result
   x)

  (define (write/comments xs)
   (define (abbrev-prefix x)
    (and (list? x)
         (= (length x) 2)
         (case (car x)
          ((quasiquote)
           "`")
          ((quote)
           "'")
          ((unquote)
           ",")
          ((unquote-splicing)
           ",@")
          (else
           #f))))

   (define (args xs col)
    (if (car? blank-symbol xs)
     (set! xs (cdr xs)))
    (for x xs
     (newline)
     (unless (eq? blank-symbol x)
      (indent col))
     (block x col))
    (display ")"))

   (define (bindings xs col)
    (if (car? blank-symbol xs)
     (set! xs (cdr xs)))
    (for* x xs xs
     (if (atom? x)
      (write x)
      (begin
       (display "(")
       (inline (car x))
       (display " ")
       (block (cadr x) (+ col 1 (width (car x)) 1))
       (display ")")))
     (when (pair? (cdr xs))
      (newline)
      (indent col)))
    (display ")"))

   (define (block x col)
    (cond
     ((eq? blank-symbol x))
     ((not (list? x))
      (inline x))
     ((null? x)
      (inline x))
     ((car? comment-symbol x)
      (display (cadr x)))
     ((and (abbrev-prefix x)
           (list? (cadr x))
           (every atom? (cadr x)))
      (inline x))
     ((abbrev-prefix x)
      (display (abbrev-prefix x))
      (block (cadr x) (+ col (string-length (abbrev-prefix x)))))

     ; 0 special args
     ((memq (car x) '(begin collect))
      (display "(")
      (write (car x))
      (args (cdr x) (add1 col)))
     ((memq (car x) '(cond))
      (display "(")
      (write (car x))
      (clauses (cdr x) (add1 col)))

     ; 1 special arg
     ((and (length? 2 x)
           (memq (car x) '(case match syntax-rules)))
      (display "(")
      (inline (car x))
      (display " ")
      (block (cadr x) (+ col 1 (width (car x)) 1))
      (clauses (cddr x) (add1 col)))
     ((and (length? 2 x)
           (or (defun? x)
               (memq (car x)
                     (quote 
                            (define-record-printer define-record-type
                             define-syntax
                             defstruct
                             lambda
                             receive)))))
      (display "(")
      (write (car x))
      (display " ")
      (inline (cadr x))
      (args (cddr x) (add1 col)))
     ((and (length? 2 x)
           (memq (car x) '(do-until do-while if unless when)))
      (display "(")
      (write (car x))
      (display " ")
      (block (cadr x) (+ col 1 (width (car x)) 1))
      (args (cddr x) (add1 col)))

     ; 2 special args
     ((and
       (length? 3 x)
       (memq (car x)
             '(any-rec? do-list do-times filt for map-rec module transform)))
      (display "(")
      (write (car x))
      (display " ")
      (write (cadr x))
      (display " ")
      (inline (caddr x))
      (args (cdddr x) (add1 col)))

     ; 3 special args
     ((and (length? 4 x)
           (memq (car x) '(for*)))
      (display "(")
      (write (car x))
      (display " ")
      (write (cadr x))
      (display " ")
      (write (caddr x))
      (display " ")
      (inline (cadddr x))
      (args (cddddr x) (add1 col)))

     ; Let
     ((and (length? 3 x)
           (memq (car x) '(let let* letrec letrec*))
           (pair? (cadr x)))
      (display "(")
      (write (car x))
      (display " (")
      (bindings (cadr x) (+ col 1 (width (car x)) 2))
      (args (cddr x) (add1 col)))
     ((and (length? 3 x)
           (memq (car x) '(let)))
      (display "(")
      (write (car x))
      (display " ")
      (write (cadr x))
      (display " (")
      (bindings (caddr x) (+ col 1 (width (car x)) 1 (width (cadr x)) 2))
      (args (cdddr x) (add1 col)))

     ; Args inline
     ((and (not (memq (car x) '(and or)))
           (every inline? x)
           (< (+ col 1 (length x) (apply + (map width x))) 80))
      (inline x))

     ; Args aligned with first
     ((and (length? 2 x)
           (inline? (car x))
           (every (lambda (y)
                   (< (+ col 1 (width (car x)) 1 (width y)) 80))
                  (cdr x))
           (cdr x))
      (display "(")
      (inline (car x))
      (display " ")
      (inc! col (+ 1 (width (car x)) 1))
      (block (cadr x) col)
      (args (cddr x) col))

     ; First arg inline anyway
     ((and (length? 2 x)
           (memq (car x) '(define set!)))
      (display "(")
      (inc! col)
      (inline (car x))
      (display " ")
      (inline (cadr x))
      (args (cddr x) col))

     ; Args unaligned
     (else
      (display "(")
      (inc! col)
      (block (car x) col)
      (args (cdr x) col))))

   (define (clauses xs col)
    (if (car? blank-symbol xs)
     (set! xs (cdr xs)))
    (for clause xs
     (newline)
     (cond
      ((eq? blank-symbol clause))
      ((car? comment-symbol clause)
       (indent col)
       (display (cadr clause)))
      ((atom? clause)
       (indent col)
       (write clause))
      ((improper-list? clause)
       (indent col)
       (inline clause))
      (else
       (indent col)
       (display "(")
       (block (car clause) (add1 col))
       (args (cdr clause) (add1 col)))))
    (display ")"))

   (define (inline x)
    (cond
     ((abbrev-prefix x)
      (display (abbrev-prefix x))
      (inline (cadr x)))
     ((list? x)
      (display "(")
      (do-while (pair? x)
       (inline (car x))
       (set! x (cdr x))
       (if (pair? x)
        (display " ")))
      (display ")"))
     ((pair? x)
      (display "(")
      (do-while (pair? x)
       (inline (car x))
       (set! x (cdr x))
       (if (pair? x)
        (display " ")
        (unless (null? x)
         (display " . ")
         (inline x))))
      (display ")"))
     (else
      (write x))))

   (define (inline? x)
    (and (not (any-rec? y x
               (eq? y blank-symbol)))
         (not (any-rec? y x
               (eq? y comment-symbol)))
         (not (string-any #\newline (with-output-to-string (curry block x 0))))))

   (define (max-line-width s)
    (if (string-null? s)
     0
     (apply max (map string-length (string-split s "\n")))))

   (define (width x)
    (if (hash-table-exists? widths x)
     (hash-table-ref widths x)
     (let ((w (max-line-width (with-output-to-string (curry block x 0)))))
      (hash-table-set! widths x w)
      w)))

   (for x xs
    (block x 0)
    (newline)))

  (define blank-symbol (gensym))
  (define comment-symbol (gensym))
  (define widths (make-hash-table))

)
