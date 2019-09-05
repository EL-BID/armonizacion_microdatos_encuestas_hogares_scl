* (Versi�n Stata 12)
clear
set more off


*________________________________________________________________________________________________________________*

 * Activar si es necesario (dejar desactivado para evitar sobreescribir la base y dejar la posibilidad de 
 * utilizar un loop)
 * Los datos se obtienen de las carpetas que se encuentran en el servidor: \\Sdssrv03\surveys
 * Se tiene acceso al servidor �nicamente al interior del BID.
 * El servidor contiene las bases de datos MECOVI.
 *________________________________________________________________________________________________________________*
 
global ruta = "\\Sdssrv03\surveys\\survey\PRY\EPH\2012\m10_m12\data_orig"

local PAIS PRY
local ENCUESTA EPH
local ANO "2012"
local ronda m10_m12

local log_file = "\\Sdssrv03\surveys\harmonized\\`PAIS'\\`ENCUESTA'\\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"
local base_out = "\\Sdssrv03\surveys\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\\data_merge\\`PAIS'_`ANO'`ronda'.dta"

capture log close
*log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
Pa�s: Paraguay
Encuesta: EPH 
Round: Octubre-Diciembre
Autores:
Versi�n 2013: Mayra S�enz
�ltima versi�n: Mayra S�enz - Email: mayras@iadb.org, saenzmayra.a@gmail.com
Fecha �ltima modificaci�n: 4 de Septiembre de 2013

							SCL/LMK - IADB
****************************************************************************/


/* lo comento para no correrlo varias veces
forvalues i = 1/9 {
use "$ruta\r0`i'_eph12.dta", clear
cap sort upm nvivi nhoga 
cap sort upm nvivi nhoga l02
save, replace
}
forvalues i = 10/13 {
use "$ruta\r`i'_eph12.dta", clear
cap sort upm nvivi nhoga 
cap sort upm nvivi nhoga l02
save, replace
}*/

/*
hay que transformar la base para poder incluir estas ya que al tener la variable mes tiene mas de una observacion por hogar
lo que hace que se multipliquen los individuos y se expanda la muestra con duplicados
*/
use "$ruta\r13_eph12.dta", replace
reshape wide e02l1 e02l1b e02l1c e02l1d e02l2 e02l2b e02l2c e02l2d fex, i(upm nvivi nhoga dptorep area) j(e02lin) 
save "$ruta\r13_eph12_mod.dta", replace


/*Unifico los modulos de inter�s para el sociometro:
  Vivienda, ingresos y personas*/
 
use "$ruta\r02_eph12.dta", clear
*merge m:m upm nvivi nhoga using "$ruta\r01_eph12.dta"
merge m:1 upm nvivi nhoga using "$ruta\r01_eph12.dta"
tab _merge
drop _merge
sort upm nvivi nhoga l02

merge m:m upm nvivi nhoga using "$ruta\r03_eph12.dta"
tab _merge
drop _merge
sort upm nvivi nhoga

merge m:m upm nvivi nhoga using "$ruta\r13_eph12_mod.dta"
tab _merge
drop _merge
sort upm nvivi nhoga

drop fex1-fex12

saveold "`base_out'", version(12) replace


log close


