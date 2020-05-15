#lang racket

(require "listas.rkt")
(provide (all-defined-out))

; ----------------------- < TDA LOCAL REPOSITORY > -----------------------



;-----<CONSTRUCTOR>-----

;Descripción: Función que 
;Dominio: 
;Recorrido: 
;Recursión:
(define (local-repository rama . commit)
  (if (string? rama)
      (if (> (calcular-largo-lista commit) 1)
          (if (aplicar-funcion-lista string? (calcular-largo-lista commit) commit)
              (cons rama (list commit))
              '())
          '())
      '()))

;-----<PERTENENCIA>-----

;Descripción: 
;Dominio: 
;Recorrido: 
;Recursión:
(define local-repository? (lambda (L)
                            (if (list? L)
                               (if (null? L)
                                   #f
                                   (if (> (calcular-largo-lista (cdr L)) 1)
                                       (if (aplicar-funcion-lista-de-listas string? (calcular-largo-lista (cdr L)) (cdr L))
                                           #t
                                           #f)
                                       #f))
                               #f)))
                                       
                           

;-----<SELECTORES>-----

;Descripción: 
;Dominio: 
;Recorrido: 
;Recursión:
(define get-commit-local-repository (lambda (n L)
                                      (if (local-repository? L)
                                          (if (< n (calcular-largo-lista L))
                                              (obtener-elemento-lista n (cdr L))
                                              -1)
                                          -1)))


;-----<MODIFICADORES>-----

;Descripción: 
;Dominio: 
;Recorrido: 
;Recursión:

(define agregar-commit-local-repository (lambda (commit L)
                                          (define agregar-commit-local-repository-aux (lambda (commit L nuevo-L)
                                                                                        (if (null? L)
                                                                                            (agregar-elemento-final-lista commit nuevo-L)
                                                                                            (agregar-commit-local-repository-aux commit (cdr L) (agregar-elemento-final-lista (car L) nuevo-L)))))
                                          (if (local-repository? L)
                                              (if (> (calcular-largo-lista commit) 1)
                                                  (if (aplicar-funcion-lista string? (calcular-largo-lista commit) commit)
                                                      (agregar-commit-local-repository-aux commit L '())
                                                      -1)
                                                  -1)
                                              -1)))
                                              
                                                                                                                        
