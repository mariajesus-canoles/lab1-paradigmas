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
                    (if (equal? comando add)
                        add
                        (if (equal? comando commit)
                            commit
                            (if (equal? comando push)
                                push
                                null))))))


;Descripcion: Función que trae el contenido del remote repository al workspace 
;Dominio: Zonas
;Recorrido: Zonas
;Recursion: Cola
(define pull-aux (lambda (inf zonas remote-repository archivos)
                   (if (null? remote-repository)
                       (list (agregar-elemento-final-lista (cons "PULL" (date->string (current-date) second)) inf) (set-workspace-zonas (agregar-lista-final-lista archivos (get-workspace-zonas zonas)) zonas))
                       (pull-aux inf zonas (cdr remote-repository) (agregar-lista-final-lista (cdr (car remote-repository)) archivos)))))



;Descripcion: Función que trae el contenido del remote repository al workspace 
;Dominio: Zonas
;Recorrido: Zonas
;Recursion: Cola (funcion complementaria)
(define pull (lambda (zonas)
               (if (zonas? zonas)
                   (pull-aux null zonas (cdr (get-remote-repository-zonas zonas)) null)
                   (if (zonas? (cadr zonas))
                       (pull-aux (car zonas) (cadr zonas) (cdr (get-remote-repository-zonas (cadr zonas))) null)
                       null))))


;Descripción: Función que agrega los cambios del Workspace al Index
;Dominio: Lista String o Null X Zonas
;Recorrido: Zonas
;Recursión: Natural
(define add-aux (lambda (archivos index)
                  (if (null? archivos)
                      (if (null? index)
                          null
                          (cons (car index) (add-aux null (cdr index))))
                      (if (archivo-en-index (car archivos) index)
                          (add-aux (cdr archivos) index)
                          (cons (car archivos) (add-aux (cdr archivos) index))))))


;Descripción: Función que agrega los cambios del Workspace al Index
;Dominio: Lista String o Null X Zonas
;Recorrido: Zonas
;Recursión: Natural (función complementaria)
(define add (lambda (archivos)
              (lambda (zonas)
                (if (zonas? zonas)
                    (if (null? archivos)
                        (list (agregar-elemento-final-lista (cons "ADD" (date->string (current-date) second)) null) (set-index-zonas (add-aux (get-workspace-zonas zonas) (get-index-zonas zonas)) zonas))
                        (if (archivos-en-workspace archivos (get-workspace-zonas zonas))
                            (list (agregar-elemento-final-lista (cons "ADD" (date->string (current-date) second)) null) (set-index-zonas (add-aux archivos (get-index-zonas zonas)) zonas))
                            null))
                    (if (zonas? (cadr zonas))
                        (if (null? archivos)
                            (list (agregar-elemento-final-lista (cons "ADD" (date->string (current-date) second)) (car zonas)) (set-index-zonas (add-aux (get-workspace-zonas (cadr zonas)) (get-index-zonas (cadr zonas))) (cadr zonas)))
                            (if (archivos-en-workspace archivos (get-workspace-zonas (cadr zonas)))
                                (list (agregar-elemento-final-lista (cons "ADD" (date->string (current-date) second)) (car zonas)) (set-index-zonas (add-aux archivos (get-index-zonas (cadr zonas))) (cadr zonas)))
                                null))
                        null)))))


;Descripción: Función que genera un commit
;Dominio: Lista o Null X String X Zonas
;Recorrido: Zonas
(define commit-aux (lambda (inf mensaje zonas)
                     (list (agregar-elemento-final-lista (cons "COMMIT" (date->string (current-date) second)) inf)
                           (set-local-repository-zonas (agregar-commit-local-repository (agregar-elemento-principio-lista mensaje (get-index-zonas zonas)) (get-local-repository-zonas zonas)) zonas))))
                         

;Descripción: Función que genera un commit
;Dominio: String X Zonas
;Recorrido: Zonas
(define commit (lambda (mensaje)
                 (lambda (zonas)
                   (if (string? mensaje)
                       (if (zonas? zonas)
                           (commit-aux null mensaje zonas)
                           (if (zonas? (cadr zonas))
                               (commit-aux (car zonas) mensaje (cadr zonas))
                               null))
                       null))))


;Descripción: Funcion que envia los commits al local repository
;Dominio: Lista o Null X Zonas
;Recorrido: Zonas
(define push-aux (lambda (inf zonas)
                   (list (agregar-elemento-final-lista (cons "COMMIT" (date->string (current-date) second)) inf)
                   (set-remote-repository-zonas (agregar-commits-remote-repository (cdr (get-local-repository-zonas zonas)) (get-remote-repository-zonas zonas)) zonas))))
                   

;Descripción: Funcion que envia los commits al local repository
;Dominio: Zonas
;Recorrido: Zonas
(define push (lambda (zonas)
               (if (zonas? zonas)
                   (push-aux null zonas)
                   (if (zonas? (cadr zonas))
                       (push-aux (car zonas) (cadr zonas))
                       null))))
                               
                       
                   
                            
                          
                                                            

                    
                    
                                  
               






                
                      