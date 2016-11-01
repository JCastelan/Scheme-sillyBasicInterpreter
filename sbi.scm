#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr
;; $Id: sbi.scm,v 1.4 2016-10-10 17:00:54-07 - - $
;;Joseph Castelan
;;jgcastel@ucsc.edu
;;
;; NAME
;;    sbi.scm - silly basic interpreter
;;
;; SYNOPSIS
;;    sbi.scm filename.sbir
;;
;; DESCRIPTION
;;    The file mentioned in argv[1] is read and assumed to be an SBIR
;;    program, which is the executed.  Currently it is only printed.
;;

(define *stderr* (current-error-port))

(define *run-file*
    (let-values
        (((dirpath basepath root?)
            (split-path (find-system-path 'run-file))))
        (path->string basepath))
)

(define (die list)
    (for-each (lambda (item) (display item *stderr*)) list)
    (newline *stderr*)
    (exit 1)
)

(define (usage-exit)
    (die `("Usage: " ,*run-file* " filename"))
)

(define (readlist-from-inputfile filename)
    (let ((inputfile (open-input-file filename)))
         (if (not (input-port? inputfile))
             (die `(,*run-file* ": " ,filename ": open failed"))
             (let ((program (read inputfile)))
                  (close-input-port inputfile)
                         program))))

(define (write-program-by-line filename program)
    (printf "==================================================~n")
    (printf "~a: ~s~n" *run-file* filename)
    (printf "==================================================~n")
    (printf "(~n")
    (map (lambda (line) (printf "~s~n" line)) program)
    (printf ")~n~n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;function table
(define *function-table* (make-hash))
(define (function-get key)
        (hash-ref *function-table* key #f))
(define (function-put! key value)
        (hash-set! *function-table* key value))

(for-each
    (lambda (pair)
            (function-put! (car pair) (cadr pair)))
    `(

        (log10_2 0.301029995663981195213738894724493026768189881)
        (sqrt_2  1.414213562373095048801688724209698078569671875)
        (atan   ,atan)
        (sin        ,sin)
        (cos        ,cos)
        (tan        ,tan)
        (acos       ,acos)
        (asin       ,asin)
        (abs        ,abs)
        (round      ,round)
        ;;(e       2.718281828459045235360287471352662497757247093)
        ;;(pi      3.141592653589793238462643383279502884197169399)
        (div     ,(lambda (x y) (floor (/ x y))))
        (log10   ,(lambda (x) (/ (log x) (log 10.0))))
        (mod     ,(lambda (x y) (- x (* (div x y) y))))
        (quot    ,(lambda (x y) (truncate (/ x y))))
        (rem     ,(lambda (x y) (- x (* (quot x y) y))))
        (+       ,+)
        (-      ,-)
        (*      ,*)
        (/      ,/)
        (^       ,expt)
        (ceil    ,ceiling)
        (exp     ,exp)
        (floor   ,floor)
        (log     ,log)
        (sqrt    ,sqrt)
        (print    ,(lambda x (display x) (newline)))
        ;(goto        ,(lambda x (handle-lines )))

     ))
     
 ;;label table    
 (define *label-table* (make-hash))
(define (label-get key)
        (hash-ref *label-table* key #f))
(define (label-put! key value)
        (hash-set! *label-table* key value))
        
;;variable table
(define *variable-table* (make-hash))
(define (variable-get key)
        (hash-ref *variable-table* key #f))
(define (variable-put! key value)
        (hash-set! *variable-table* key value))

(for-each
    (lambda (pair)
            (variable-put! (car pair) (cadr pair)))
    `(

        (e       2.718281828459045235360287471352662497757247093)
        (pi      3.141592653589793238462643383279502884197169399)

     ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (find-labels list) 
        (if (and (not (null? (car list))) (not( null? (cdar list)))) ;;true if cdar has a label
                (label-put! (car (cdar list))   list);;(display (symbol? (car (cdar list))));;~~~~~~~~~~~~~~~~~PICK UP HERE!!! Use these labels as keys. And then set up a goto function
                (display ""))
                
        (if (null? (cdr list))
                (display "")
                (find-labels (cdr list)))
)

(define (handle-lines list) 
        ;;(display  (caar list))      ;;prints the line number
        (if (and (pair? list) (not( null? list)) (pair? (car list) ) (not (null? (cdar list)))) 
             (next-level (cdar list))       
             (display "" ))
         ;(if (and (pair? list) (not (null? (car list))) (not( null? (cdar list))) (not( null? (cadar list))) (not (null? (cdr (cadar list)))) (eqv? (car (cadar list)) 'goto )) ;;true if has a goto
          ;      (display (cdr (label-get (cadr (cadar list)))));(handle-lines (cdr (label-get (cadr (cadar list)))));;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!Finish working on this
                (if (not (null? (cdr list)))        ;;checks if there is a next item in the list
                    (handle-lines (cdr list))
                    (display "DONE      "))       ;; '(done)
);)
        
(define (next-level list)      
        (if (and (not (null? list)) (pair? (car list))) 
                (handle-statement (car list))
                (display "")
        )
)

(define (handle-statement list)  
         (cond ((has-let? list) (handle-let list))
               ((has-dim? list) (handle-dim list))
               ((has-goto? list) ( display ""))
               (else (evalexpr list)))
)
;;;;;;;Has/Handle statements~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(define (has-let? list)     
        (if ( and (not (null? list)) (pair? list))
             ;; (if   ( and (not (null? (car list))) (pair? (car list)))
                     (eqv? (car list) 'let )
              ;;       (#f))
              (#f)))
(define (handle-let list) 
        ;;(display (cadr list))
        ;;(display (pair? (cadr list)))
        (if ( not (pair? (cadr list) ))
                (variable-put! (cadr list) (evalexpr (caddr list)))
                (vector-set! (variable-get (caadr list)) (inexact->exact (evalexpr (car (cdadr list)))) (caddr list))
                ))

        
(define (has-dim? list)
        (if (and (not (null? list)) (pair? list))
                     (eqv? (car list) 'dim)
              (#f)))
(define (handle-dim list)
        (variable-put! (caadr list) (make-vector (car (cdadr list)))))

        
(define (has-goto? list)
       (if (and (not (null? list)) (pair? list))
                     (eqv? (car list) 'goto)
              (#f)))
(define (handle-goto list)
        ;(display (cadr list))
        (display (label-get (cadr list)))
        ;(handle-lines (label-get (cadr list)))
        (exit 0)
        )
        

;;;;;;;;evalexpr
(define (evalexpr list)    
   (cond ((number? list) (+ list 0.0))      ;;converts numbers
         ((and(symbol? list) ) (hash-ref *variable-table* list #f))       ;;returns variables
         ((and (pair? list) (symbol? (car list)) (function-get (car list)) (not (label-get (car list))))  (apply (hash-ref *function-table* (car list))  ;;does a function
                                (map evalexpr (cdr list))))
         ((and (pair? list) (symbol? (car list)) (variable-get (car list))) (+ 0.0 (vector-ref (variable-get (car list)) (inexact->exact (evalexpr(cadr list)))))) ;;works for vectors
         (else list))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Main
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (main arglist)
    (if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit)
        (let* ((sbprogfile (car arglist))
               (program (readlist-from-inputfile sbprogfile)))
              (write-program-by-line sbprogfile program)
              (find-labels program);;
              (handle-lines program))));;;;;;;;

(main (vector->list (current-command-line-arguments)))

