#lang racket

(require "listas.rkt")
(provide (all-defined-out))

; ----------------------- < TDA WORKSPACE > -----------------------



;-----<CONSTRUCTOR>-----

;Descripción: Función que crea un workspace
;Dominio: Uno o más strings, representando los archivos
;Recorrido: Lista de archivos contenidos en el workspace
(define (workspace . archivo)
  (if (> (calcular-largo-lista archivo) 0)
      (if (aplicar-funcion-lista string? (calcular-largo-lista archivo) archivo)
          archivo
          '())
      '()))
     

;-----<PERTENENCIA>-----

;Descripción:
;Dominio:
;Recorrido:
;Recursión:
(define workspace? (lambda (L)
                     (if (null? L)
                         #f
                         (if (aplicar-funcion-lista string? (calcular-largo-lista L) L)
                             #t
                             #f))))
                         

;-----<SELECTORES>-----


;Descripción:
;Dominio:
;Recorrido:
;Recursión:
(define get-archivo (lambda (n L)
                      (if (workspace? L)
                          (if (= n 0)
                              (car L)
                              (get-archivo (- n 1) (cdr L)))
                          -1)))
                          
                          
;-----<MODIFICADORES>-----

;Descripción:
;Dominio:
;Recorrido:
;Recursión:
(define agregar-archivo (lambda (archivo L)
                          (define agregar-archivo-aux (lambda (archivo L nuevo-L)
                                                        (if (null? L)
                                                            (agregar-elemento-final-lista archivo nuevo-L)
                                                            (agregar-archivo-aux archivo (cdr L) (agregar-elemento-final-lista (car L) nuevo-L)))))
                              (if (workspace? L)
                                  (agregar-archivo-aux archivo L '())
                                  -1)))


;Descripción:
;Dominio:
;Recorrido:
;Recursión:
(define eliminar-archivo (lambda (archivo L)
                           (define eliminar-archivo-aux (lambda (archivo L nuevo-L)
                                                          (if (equal? archivo (car L))
                                                              (agregar-lista-final-lista (cdr L) nuevo-L)
                                                              (eliminar-archivo-aux archivo (cdr L) (agregar-elemento-final-lista (car L) nuevo-L)))))
                           (if (workspace? L)
                               (eliminar-archivo-aux archivo L '())
                               -1)))
                                                             
                                                          
                                                                            
                                                                            
                                 