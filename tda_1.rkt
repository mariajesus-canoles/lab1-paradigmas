#lang racket

; ----------------------- < TDA ARCHIVO > -----------------------

;<CONSTRUCTOR>

(define archivo (lambda (nombre contenido)
                  (cons nombre contenido)))

;<PERTENENCIA>

(define archivo? (lambda (a)
                   (if (null? a)
                       (if (list? a)
                           (if (and (string? (car a)) (string? (cdr a)))
                               #t
                               #f)
                           #f)
                       #f)))

;<SELECTORES>

(define get-nombre (lambda (a)
                     (car a)))

(define get-contenido (lambda (a)
                        (cdr a)))

;<MODIFICADORES>

(define set-nombre (lambda (a nombre-nuevo)
                     (cons nombre-nuevo (cdr a))))

(define set-contenido (lambda (a contenido-nuevo)
                        (cons (car a) contenido-nuevo)))