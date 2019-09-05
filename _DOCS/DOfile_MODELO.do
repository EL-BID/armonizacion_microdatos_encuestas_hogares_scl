/*BANCO INTERAMERICANO DE DESARROLLO - SCL/SCL
Elaboracion: Yessenia Loayza (desloay@hotmail.com | yessenial@iadb.org); 
Fecha: Abril, 2014 (ultima actualizacion)
Notes: Este dofile constituye un modelo a seguir para el calculo de los indicadores en serie
de las bases armonizadas. */

clear
set more off
qui do "\\Sdssrv03\surveys\sociometro_BID\Program\current_directory.do" 
/*Encontrar el do-file current_directory.do en: -> \\Sdssrv03\surveys\sociometro_BID\Program\ */

*PASOS A SEGUIR:
*---------------
*1.- Escoger los paises y años que deseo en base al do-file "current_directory.do"

foreach i of numlist  710/723 1010/1011  { /*En este caso escogi ECU:2000-2012 y MEX:2010-2012*/
use ${in`i'} , clear

/*2.- Realizar el calculo con las variables armonizadas.
      Esta seccion puede ser modificada y reemplazada por cualquier otro calculo*/

/*Genero la formalidad laboral para los ocupados de 18-64 años de edad*/
keep if edad_ci>=18 & edad_ci<=64
generat form=0 if condocup_ci==1
replace form=1 if formal_ci==1 & condocup_ci==1 

sum form [w=factor_ci]
gen formalidad = r(mean)*100

*3.- realizo un keep para quedarme con las varibales de interes.
keep pais_c anio_c formalidad
duplicates drop
tempfile pob`i'   
save `pob`i'', replace /*almacena en la memoria del stata. Puedo escojer grabarlo en otro lugar tambien*/
}

*4.- A traves del append une los años para los que he calculado el inidcador.
forvalues j=710/1010 {
cap append using `pob`j''
}
sort pais_c anio_c
br

