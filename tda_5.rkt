#lang racket
;ej: (zonas '("archivo9" "archivo 8") '("archivo7" "archivo6") '("master" ("Tercer commit" "archivo3" "archivo3.5")) '("master" ("Primer commit" "archivo5") ("Segundo commit" "archivo4" "archivo4.5")))
(require "listas.rkt")
(require "tda_1.rkt")
(require "tda_2.rkt")
(require "tda_3.rkt")
(require "tda_4.rkt")
(provide (all-defined-out))

; ----------------------- < TDA ZONAS > -----------------------



;-----<CONSTRUCTOR>-----

;Descripción: Función que crea la zona de trabajo que contiene al workspace, index, local y remote repository
;Dominio: Workspace X Index X Local-Repository X Remote-Repository
;Recorrido: Zonas
(define zonas (lambda (workspace index local-repository remote-repository)
                (if (workspace? workspace)
                    (if (index? index)
                        (if (local-repository? local-repository)
                            (if (remote-repository? remote-repository)
                                (cons workspace (cons index (cons local-repository (cons remote-repository null))))
                                null)
                            null)
                        null)
                    null)))


;-----<PERTENENCIA>-----

;Descripción: Función que comprueba si una lista ingresada cumple con los requisitos para considerarse zonas
;Dominio: 'a type
;Recorrido: Booleano
(define zonas? (lambda (L)
                 (if (list? L)
                     (if (null? L)
                         #f
                         (if (workspace? (car L))
                             (if (index? (cadr L))
                                 (if (local-repository? (caddr L))
                                     (if (remote-repository? (cadddr L))
                                         #t
                                         #f)
                                     #f)
                                 #f)
                             #f))
                     #f)))
                           

;-----<SELECTORES>-----

;Descripción: Función que obtiene el workspace de zonas
;Dominio: Zonas
;Recorrido: Workspace
(define get-workspace-zonas (lambda (L)
                              (if (zonas? L)
                                  (car L)
                                  null)))


;Descripción: Función que obtiene el index de zonas
;Dominio: Zonas
;Recorrido: Index
(define get-index-zonas (lambda (L)
                          (if (zonas? L)
                              (cadr L)
                              null)))


;Descripción: Función que obtiene el local repository de zonas
;Dominio: Zonas
;Recorrido: Local-Repository
(define get-local-repository-zonas (lambda (L)
                                     (if (zonas? L)
                                         (caddr L)
                                         null)))


;Descripción: Función que obtiene el remote repository de zonas
;Dominio: Zonas
;Recorrido: Remote-Repository
(define get-remote-repository-zonas (lambda (L)
                                      (if (zonas? L)
                                          (cadddr L)
                                          null)))


;-----<MODIFICADORES>-----

;Descripción: Función que modifica el workspace de las zonas
;Dominio: Workspace X Zonas
;Recorrido: Zonas
(define set-workspace-zonas (lambda (workspace L)
                              (if (zonas? L)
                                  (if (workspace? workspace)
                                      (cons workspace (cons (get-index-zonas L) (cons (get-local-repository-zonas L) (cons (get-remote-repository-zonas L) null))))
                                      null)
                                  null)))


;Descripción: Función que modifica el index de las zonas
;Dominio: Index X Zonas
;Recorrido: Zonas
(define set-index-zonas (lambda (index L)
                              (if (zonas? L)
                                  (if (index? index)
                                      (cons (get-workspace-zonas L) (cons index (cons (get-local-repository-zonas L) (cons (get-remote-repository-zonas L) null))))
                                      null)
                                  null)))

;Descripcion: Funcion que agrega archivos al index
;Dominio: Lista Strings X Zonas
;Recorrido: Zonas
(define agregar-archivos-index-zonas (lambda (archivos L)
                                       (if (zonas? L)
                                           (if (index? archivos)
                                               (cons (get-workspace-zonas L) (cons (agregar-lista-final-lista archivos (get-index-zonas L)) (cons (get-local-repository-zonas L) (cons (get-remote-repository-zonas L) null))))
                                               null)
                                           null)))


;Descripción: Función que modifica el local repository de las zonas
;Dominio: Local-Repository X Zonas
;Recorrido: Zonas
(define set-local-repository-zonas (lambda (local-repository L)
                              (if (zonas? L)
                                  (if (local-repository? local-repository)
                                      (cons (get-workspace-zonas L) (cons (get-index-zonas L) (cons local-repository (cons (get-remote-repository-zonas L) null))))
                                      null)
                                  null)))


;Descripción: Función que modifica el remote repository de las zonas
;Dominio: Remote-Repository X Zonas
;Recorrido: Zonas
(define set-remote-repository-zonas (lambda (remote-repository L)
                              (if (zonas? L)
                                  (if (remote-repository? remote-repository)
                                      (cons (get-workspace-zonas L) (cons (get-index-zonas L) (cons (get-local-repository-zonas L) (cons remote-repository null))))
                                      null)
                                  null)))
                                              