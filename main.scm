(module main ()
 (import chicken)
 (import scheme)

 (use data-structures)
 (use extras)
 (use files)
 (use ports)
 (use posix)
 (use srfi-1)
 (use srfi-13)

 ; Include macros first
 (include "etc.scm")

 ; Include everything else
 (include "format.scm")

 ; Options
 (define end-options #f)
 (define format-code #f)
 (define files
         (filt s (cdr (argv))
          (or (not (string-prefix? "-" s))
              end-options
              (begin
               (cond
                ((or (string-prefix? "--f" s)
                     (string-prefix? "-f" s))
                 (set! format-code #t))
                ((or (string-prefix? "--h" s)
                     (string-prefix? "-h" s))
                 (print "Usage: ayane [options] files")
                 (print)
                 (print "-f  Format code")
                 (print "-h  Show help")
                 (print "-v  Show version")
                 (exit 0))
                ((or (string-prefix? "--v" s)
                     (string-prefix? "-V" s)
                     (string-prefix? "-v" s))
                 (print "ayane version 0")
                 (exit 0))
                ((string= "--" s)
                 (set! end-options #t))
                (else
                 (print s ": unknown option")
                 (exit 1)))
               #f))))
 (when format-code
  ; Backup
  (unless (directory-exists? "backup")
   (create-directory "backup"))
  (for file files
   (delete-file* (pathname-replace-directory file "backup")))

  ; Format
  (for file files
   (define xs (with-input-from-file file read/comments))
   (set! xs (tidy xs))
   (rename-file file (pathname-replace-directory file "backup"))
   (with-output-to-file file (curry write/comments xs) binary:))

  ; Exit
  (exit 0)))
