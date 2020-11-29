#lang racket
;Practica 10
;William Ortega Garcia




; --------------------funcion para crear empleado---------------------------------------
(define (crear-empleado nombre  codigo salario)
	(list   (list 'nombre nombre)
		(list 'codigo codigo)
		(list 'salario salario)
	)
)
;fin... funcion para crear empleado


;----------funciones para acceder a cada atributo de empleado--------------------------
(define (nombre empleado)
	(cadr (assoc 'nombre empleado))
)
(define (codigo empleado)
	(cadr (assoc 'codigo empleado))
)
(define (salario empleado)
	(cadr (assoc 'salario empleado))
)
;fin...funciones para acceder a cada atributo de empleado


;----------------------programa principal --------------------------------------------------
(define (programa)  
  
  ;-----------------------funcion para mostrar las opciones del menu ------------------------
     (define (pedir-opcion)
		(newline)
		(display "****** MENU ******" )
		(newline)
		(display "(1)    Mostrar el numero de empleados" )
		(newline)
		(display "(2)   Nombre del empleado con el código ingresado" )
		(newline)
		(display "(3)   El mayor y el menor salario de la empresa" )
		(newline)
       	(display "(4)   Empleados con mayor salario" )
		(newline)
		(display "(5)   Mostrar los empleados por pantalla" )
		(newline)
		(display "(0)   Salir" )
		(newline)
		(newline)
              (display " ----> ")
              ;para leer la opcion elegida
		(read)
	)
   ;fin ... funcion para mostrar las opciones del menu
  
   
  
;-------------funcion que el dato desde teclado  y lo retorna -----------------------------
   (define (leer-teclado mensaje)
    (display mensaje)
    (display " ----> ")
    (read)
    )
;fin... funcion que el dato desde teclado  y lo retorna

  
  
  ;-------------------funcion para poner comillas a un texto --------------------------------
  (define (poner-comillas texto)
    (string-append (string #\") texto (string #\"))
    )
 ;fin ... funcion para poner comillas a un texto
  
  
  ;--------------- funcion que define el nombre del archivo --------------------------------
  (define nombreArchivo "Nomina.txt")
   ;fin ... funcion que define el nombre del archivo
  
  
  
  ;------------------------ funcion para cargar datos desde el archivo ---------------------
 (define (leer-empleados-fichero)
   (define puerto (open-input-file nombreArchivo))
  (do
    (
     (lista_empleados '() (append lista_empleados
                                (list (crear-empleado (poner-comillas nombre) 
                                                     (read puerto) 
                                                     (read puerto) 
                                                     )
                                      )
                                )
                     )
     ; Al leer el nombre desde un archivo, le quita las comillas
     (nombre         (read puerto)  (read puerto))
     )
    
    ((eof-object? nombre) 
       ; Se cierra el puerto asociado al fichero de entrada
        (close-input-port puerto) 
        ;Se devuelve la lista de empleados
	lista_empleados                   
	)
    )
  )
    ;fin... funcion para cargar datos desde el archivo
  
  
  ;--------------------funcion que devuelve el numero de empleados --------------------------
  (define (numero-empleados lista)
         (length lista)
)
  ;fin... funcion que devuelve el numero de empleados

  
  ;-------------------funcion que muestra el mayor y el menor salario ------------------------
 (define (mostrar-salarios lista_empleados mayor menor)
   (newline)
  (do
      (         
       (lista_auxiliar lista_empleados(cdr lista_auxiliar))
     ) 
    ;Condicion de salida del bucle
    ((null? lista_auxiliar) (newline))
    
    ;Cuerpo del bucle   
      ( if(< (salario(car lista_auxiliar)) (salario menor)) (set! menor (car lista_auxiliar)) 0 )
      ( if(> (salario(car lista_auxiliar)) (salario mayor)) (set! mayor (car lista_auxiliar)) 0 )
    ) 
    (display "Mayor Salario:      ")
    (display (salario mayor))
     (newline)
    (display "Menor Salario:      ")
    (display (salario menor))
     (newline)  
 )
  ;funcion que muestra el mayor y el menor salario
  
  
  
    ;-------------- funcion que muestra los empleados con mayor salario ---------------------------
 (define (mostrar-empleados-salarios lista_empleados mayor )
   (newline)
  (do
      (         
       (lista_auxiliar lista_empleados(cdr lista_auxiliar))
     ) 
   ;Condicion de salida del bucle
    ((null? lista_auxiliar) (newline))
     ;Cuerpo del bucle 
      ( if(> (salario(car lista_auxiliar)) (salario mayor)) (set! mayor (car lista_auxiliar)) 0 )
    ) 
   
    (do
      (         
       (lista_auxiliar2 lista_empleados(cdr lista_auxiliar2))
     ) 
   ;Condicion de salida del bucle
    ((null? lista_auxiliar2) (newline))
         ;Cuerpo del bucle 
      (cond
        ((= (salario(car lista_auxiliar2)) (salario mayor))
         (display "Nombre: ")
         ( display (nombre (car lista_auxiliar2)))
         (newline)
         (display "Salario:  ")
         ( display (salario (car lista_auxiliar2)))
         (newline)
         (newline)
                 
        )
        (else '())
        )
    
    ) 
 )
  ;fin... funcion que muestra los empleados con mayor salario
    
  
  
  
  ;--------------- funcion para mostrar un empleado por el codigo digitado ------------------------------
 (define (mostrar-por-codigo lista_empleados cod)
  (do
    (
     (lista_auxiliar lista_empleados (cdr lista_auxiliar))
     )
    ;Condicion de salida del bucle
    ((null? lista_auxiliar) (newline))
    
    (cond
      ((= (codigo (car lista_auxiliar)) cod) (display (nombre (car lista_auxiliar))) (set! cod -1))
      (else '())
           )
      )
   (if(= cod -1)
      '()
      (display "Codigo No encontrado !")
      )
     
    )
  ;fin... funcion para mostrar un empleado por el codigo digitado
 
  

 ;---------------- funcion para mostrar los datos de los empleado que estan en el archivo ---------------
(define (mostrar-empleados lista_empleados)
  (do
    (
     (lista_auxiliar lista_empleados (cdr lista_auxiliar))
     )
    ;Condicion de salida del bucle
    ((null? lista_auxiliar) (newline))
    
    ;Cuerpo del bucle
     (display "Nombre:   ")
     (display (nombre (car lista_auxiliar)))
     (newline)
     (display "Codigo:       ")
     (display (codigo (car lista_auxiliar)))
     (newline)
     (display "Salario:      ")
     (display (salario (car lista_auxiliar)))
     (newline)
     (newline)
    )
  )
  ;fin... funcion para mostrar los datos de los empleado que estan en el archivo
  
 

  ;----------------------- cuerpo del programa de empleados -----------------------------
  (do
	(
         ;lista donde van a estar almacenados los empleados
	(empleados '())  
	 (opcion (pedir-opcion) (pedir-opcion))  
	 )
	;condicion de salida
	((= opcion 0) (newline)(display "            Adios !"))
    
      ;cargar los empleados desde el archivo
    (if(null? empleados)
        (set! empleados (append empleados (leer-empleados-fichero)))
        0
     )
    
	;cuerpo del bucle
	(cond
          ;opcion para mostrar el numero de empleado
	  ((= opcion 1) 
              (display "Numero de Empleados: ")
	        (display  ( numero-empleados empleados))
                (newline)
          )
          
          ;opcion para buscar el empleado por el codigo ingresado
	  ((= opcion 2)
	     (define opt  (leer-teclado "Digite el codigo: "))
             (mostrar-por-codigo empleados opt)
	     (newline)
	  )

          ;opcion para mostrar el mayor y el menor de los salarios
	  ((= opcion 3)
	     (mostrar-salarios empleados (car empleados) (car empleados) )
             (newline)
          )
          	 
          ;opcion para mostrar los empleados con mayor salario
          ((= opcion 4) 
	        ( mostrar-empleados-salarios empleados  (car empleados))
          )
          
          ;opcion para mostrar los empleados por pantalla
	  ((= opcion 5) 
	        (mostrar-empleados empleados)
          )
          
          ;control de errores
	  (else (display "La opcion es incorrecta !")
		(newline)
	  )
	)
       
    )
  )
;fin... programa

;llamado al programa
(programa)

 

 