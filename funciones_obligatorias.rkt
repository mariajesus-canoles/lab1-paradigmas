#lang racket
;(define zonas1 '(("archivo9" "archivo 8") ("archivo7" "archivo6") ("master" ("Tercer commit" "archivo3" "archivo3.5")) ("master" ("Primer commit" "archivo5") ("Segundo commit" "archivo4" "archivo4.5"))))
(require "listas.rkt")
(require "tda_1.rkt")
(require "tda_2.rkt")
(require "tda_3.rkt")
(require "tda_4.rkt")
(require "tda_5.rkt")
(provide (all-defined-out))
(require racket/date)

;Descripcion: Función que permite aplicar los comandos en git 
;Dominio: Función
;Recorrido: Función
(define git (lambda (comando)
                (if (equal? comando pull)
                    pull
                    (if (equal? comando "add")
                        -1
                        (if (equal? comando "commit")
                            -1
                            (if (equal? comando "push")
                                -1
                                (list "error")))))))

;Descripcion: Función que trae el contenido del remote repository al workspace 
;Dominio: Zonas
;Recorrido: Zonas
;Recursion: Cola
(define pull (lambda (zonas)
               (define pull-aux (lambda (inf zonas remote-repository archivos)
                                  (if (null? remote-repository)
                                      (list (agregar-elemento-final-lista (cons "PULL" (date->string (current-date) second)) inf) (set-workspace-zonas (agregar-lista-final-lista archivos (get-workspace-zonas zonas)) zonas))
                                      (pull-aux inf zonas (cdr remote-repository) (agregar-lista-final-lista (cdr (car remote-repository)) archivos)))))
               (if (zonas? zonas)
                   (pull-aux null zonas (cdr (get-remote-repository-zonas zonas)) null)
                   (if (zonas? (car (cdr zonas)))
                       (pull-aux (car zonas) (car (cdr zonas)) (cdr (get-remote-repository-zonas (car (cdr zonas)))) null)
                       null))))

;Descripción: Función que agrega los cambios del Workspace al Index
;Dominio: Lista String o Null X Zonas
;Recorrido: Zonas
;Recursión: Natural
(define add (lambda (archivos)
              (lambda (zonas)
                (if (equal? archivos null)
                    (agregar-archivos-index-zonas (get-workspace-zonas zonas) zonas)
                    (if (null? archivos)
                        zonas
                        ((add (cdr archivos)) (agregar-archivos-index-zonas archivos zonas)))))))

;archivos-en-workspace
                    
                    
                                  
               






                
                      