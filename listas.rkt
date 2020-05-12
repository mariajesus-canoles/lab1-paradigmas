#lang racket

(provide (all-defined-out))

;Descripción:
;Dominio:
;Recorrido:
;Recursión:
(define calcular-largo-lista (lambda (L)
                               (define calcular-largo-lista-aux (lambda (L n)
                                                                  (if (null? L)
                                                                      n
                                                                      (calcular-largo-lista-aux (cdr L) (+ n 1)))))
                               (calcular-largo-lista-aux L 0)))

;Descripción:
;Dominio:
;Recorrido:
;Recursión:
(define aplicar-funcion-elemento (lambda (f elemento)
                                   (if (f elemento)
                                       #t
                                       #f)))

;Descripción:
;Dominio:
;Recorrido:
;Recursión:
(define aplicar-funcion-lista (lambda (f n L)
                                (if (null? L)
                                    #t
                                    (if (aplicar-funcion-elemento f (car L))
                                        (aplicar-funcion-lista f (- n 1) (cdr L))
                                        #f))))

;Descripción:
;Dominio:
;Recorrido:
;Recursión:
(define agregar-elemento-final-lista (lambda (elemento L)
                                       (append L (list elemento))))


;Descripción:
;Dominio:
;Recorrido:
;Recursión:
(define agregar-lista-final-lista (lambda (lista L)
                                    (define agregar-lista-final-lista-aux (lambda (largo lista L)
                                                                            (if (= largo 1)
                                                                                (agregar-elemento-final-lista (car lista) L)
                                                                                (agregar-lista-final-lista-aux (- largo 1) (cdr lista) (agregar-elemento-final-lista (car lista) L)))))
                                    (agregar-lista-final-lista-aux (calcular-largo-lista lista) lista L)))
                                    
