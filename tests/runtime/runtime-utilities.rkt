(module runtime-utilities racket
  (require racket/main)
  (provide (all-defined-out))
  
  (define (make-timestamp (sep ","))
    (define date (seconds->date (current-seconds) #f))
    (string-join
     (map number->string
          (list (date-year date)
                (date-month date)
                (date-day date)
                (date-hour date)
                (date-minute date)
                (date-second date)))
     sep))

  (define (generate-entry reps cpu real gc (sep ","))
    (string-join
     (cons (make-timestamp)
           (map number->string (list reps cpu real gc)))
     sep))

  (define (append-data-to-file reps cpu real gc file-name)
    (set! file-name (expand-user-path file-name))
    (when (not (file-exists? file-name))
      (display-to-file
       "# year, month, day, hour, minute, second, n, cpu, real, gc\n"
       file-name))
    (define entry (generate-entry reps cpu real gc))
    (set! entry (string-append entry "\n"))
    (display-to-file entry file-name #:exists 'append))
  )
