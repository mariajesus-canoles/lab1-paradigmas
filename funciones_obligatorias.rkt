#lang racket
;(define zonas1 '(("archivo9" "archivo 8") ("archivo7" "archivo6") ("master" ("Tercer commit" "archivo3" "archivo3.5")) ("master" ("Primer commit" "archivo5") ("Segundo commit" "archivo4" "archivo4.5"))))
(require "listas.rkt")
(require "tda_1.rkt")
(require "tda_2.rkt")
(require "tda_3.rkt")
(require "tda_4.rkt")
(require "tda_5.rkt")
(provide (all-defined-out))

;Descripcion:
;Dominio:
;Recorrido:
(define git (lambda (comando)
              (lambda (zonas)
                (if (equal? comando pull)
                    (pull zonas)
                    (if (equal? comando "add")
                        -1
                        (if (equal? comando "commit")
                            -1
                            (if (equal? comando "push")
                                -1
                                '("error"))))))))

;Descripcion: Función que trae el contenido del remote repository al workspace
;Dominio: Zonas
;Recorrido: Zonas
;Recursion: Cola
(define pull (lambda (zonas)
               (define pull-aux (lambda (zonas remote-repository archivos)
                                  (if (null? remote-repository)
                                      (cons archivos (cons (get-index-zonas zonas) (cons (get-local-repository-zonas zonas) (cons (get-remote-repository-zonas zonas) null))))
                                      (pull-aux zonas (cdr remote-repository) (agregar-lista-final-lista (cdr (car remote-repository)) archivos)))))
               (if (zonas? zonas)
                   (pull-aux zonas (cdr (get-remote-repository-zonas zonas)) null)
                   null)))
;(pull zonas1)
                                  
               
               
                      