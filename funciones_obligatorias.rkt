#lang racket


;(define zonas1 '(("archivo9" "archivo 8") ("archivo7" "archivo6") ("master" ("Tercer commit" "archivo3" "archivo3.5") ("Cuarto commit" "archivo23")) ("master" ("Primer commit" "archivo5") ("Segundo commit" "archivo4" "archivo4.5"))))
;'((("PULL" . "Friday, May 29th, 2020 6:47:13pm") ("COMMIT" . "Friday, May 29th, 2020 6:48:43pm"))(("archivo9" "archivo 8" "archivo5" "archivo4" "archivo4.5") ("archivo7" "archivo6")("master" ("Tercer commit" "archivo3" "archivo3.5") ("Cuarto commit" "archivo23"))("master" ("Primer commit" "archivo5") ("Segundo commit" "archivo4" "archivo4.5") ("Tercer commit" "archivo3" "archivo3.5") ("Cuarto commit" "archivo23"))))


(require "listas.rkt")
(require "tda_1.rkt")
(require "tda_2.rkt")
(require "tda_3.rkt")
(require "tda_4.rkt")
(require "tda_5.rkt")
(provide (all-defined-out))
(require racket/date)


;Descripción: Función que permite aplicar los comandos en git 
;Dominio: Comando
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


;Descripción: Función auxiliar de la función "pull"
;Dominio: Lista Strings o Null X Zonas X Remote-Repository X Lista Strings
;Recorrido: Zonas
;Recursión: Cola
(define pull-aux (lambda (inf zonas remote-repository archivos)
                   (if (null? remote-repository)
                       (list (agregar-elemento-final-lista (cons "PULL" (date->string (current-date) second)) inf) (set-workspace-zonas (agregar-lista-final-lista archivos (get-workspace-zonas zonas)) zonas))
                       (pull-aux inf zonas (cdr remote-repository) (agregar-lista-final-lista (cdr (car remote-repository)) archivos)))))


;Descripción: Función que trae el contenido del remote repository al workspace 
;Dominio: Zonas
;Recorrido: Zonas
;Recursión: Cola (funcion auxiliar)
(define pull (lambda (zonas)
               (if (zonas? zonas)
                   (pull-aux null zonas (cdr (get-remote-repository-zonas zonas)) null)
                   (if (zonas? (cadr zonas))
                       (pull-aux (car zonas) (cadr zonas) (cdr (get-remote-repository-zonas (cadr zonas))) null)
                       null))))


;Descripción: Función auxiliar de la función "add"
;Dominio: Lista String o Null X Index
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
;Recursión: Natural (función auxiliar)
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


;Descripción: Función auxiliar de la función "commit"
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


;Descripción: Función auxiliar de la función "push"
;Dominio: Lista o Null X Zonas
;Recorrido: Zonas
(define push-aux (lambda (inf zonas)
                   (list (agregar-elemento-final-lista (cons "PUSH" (date->string (current-date) second)) inf)
                   (set-remote-repository-zonas (agregar-commits-remote-repository (verificar-cambios-repositorys (cdr (caddr zonas)) (cdr (cadddr zonas)) null) (get-remote-repository-zonas zonas)) zonas))))
                   

;Descripción: Función que envia los commits al local repository
;Dominio: Zonas
;Recorrido: Zonas
(define push (lambda (zonas)
               (if (zonas? zonas)
                   (push-aux null zonas)
                   (if (zonas? (cadr zonas))
                       (push-aux (car zonas) (cadr zonas))
                       null))))


;Descripcion: Función auxiliar de la función "zonas->string"
;Dominio: Lista String
;Recorrido: String
;Recursion: Cola
(define zonas->string-aux (lambda (L)
                            (if (null? (cdr L))
                                (car L)
                                (string-append (car L) "\n" (zonas->string-aux (cdr L))))))


;Descripcion: Función auxiliar 2 de la función "zonas->string"
;Dominio: Lista String X Sring
;Recorrido: String
;Recursion: Cola
(define zonas->string-aux2 (lambda (L texto)
                             (if (null? L)
                                 texto
                                 (zonas->string-aux2 (cdr L) (string-append texto "\n\n" (zonas->string-aux (car L)))))))


;Descripcion: Función auxiliar 3 de la función "zonas->string"
;Dominio: Lista String 
;Recorrido: String                            
(define zonas->string-aux3 (lambda (L)
                             (string-append "{" (cdr L) "} se aplica el comando " (car L) "\n")))


;Descripcion: Función auxiliar 4 de la función "zonas->string"
;Dominio: Lista String X String
;Recorrido: String
;Recursion: Cola
(define zonas->string-aux4 (lambda (L texto)
                             (if (null? L)
                                 texto
                                 (zonas->string-aux4 (cdr L) (string-append texto (zonas->string-aux3 (car L)))))))


;Descripcion: Función que genera un string con el contenido de Zonas
;Dominio: Zonas
;Recorrido: String
(define zonas->string (lambda (zonas)
                        (if (zonas? zonas)
                            (string-append "\n\n══════════════«  WORKSPACE  »═══════════════\n\n" (zonas->string-aux (car zonas))
                                           "\n\n════════════════«  INDEX  »═════════════════\n\n" (zonas->string-aux (cadr zonas))
                                           "\n\n═══════════«  LOCAL REPOSITORY  »═══════════\n\n" "RAMA: " (car (caddr zonas)) (zonas->string-aux2 (invertir-lista (cdr (caddr zonas)) null) "")
                                           "\n\n══════════«  REMOTE REPOSITORY  »═══════════\n\n" "RAMA: " (car (cadddr zonas)) (zonas->string-aux2 (invertir-lista (cdr (cadddr zonas)) null) ""))
                            (if (zonas? (cadr zonas))
                                (string-append "\n\n══════════════«  WORKSPACE  »═══════════════\n\n" (zonas->string-aux (car (cadr zonas)))
                                               "\n\n════════════════«  INDEX  »═════════════════\n\n" (zonas->string-aux (cadr (cadr zonas)))
                                               "\n\n═══════════«  LOCAL REPOSITORY  »═══════════\n\n" "RAMA: " (car (caddr (cadr zonas))) (zonas->string-aux2 (invertir-lista (cdr (caddr (cadr zonas))) null) "")
                                               "\n\n══════════«  REMOTE REPOSITORY  »═══════════\n\n" "RAMA: " (car (cadddr (cadr zonas))) (zonas->string-aux2 (invertir-lista (cdr (cadddr (cadr zonas))) null) "")
                                               "\n\n══════════«  HISTORY COMMANDS  »════════════\n\n" (zonas->string-aux4 (invertir-lista (car zonas) null) "") "\n\n")
                                null))))
                                               
 
                                                            
                                                     
                      