* (Versión Stata 12)
clear
set more off

*________________________________________________________________________________________________________________*

 * Activar si es necesario (dejar desactivado para evitar sobreescribir la base y dejar la posibilidad de 
 * utilizar un loop)
 * Los datos se obtienen de las carpetas que se encuentran en el servidor: ${surveysFolder}
 * Se tiene acceso al servidor únicamente al interior del BID.
 * El servidor contiene las bases de datos MECOVI.
 *________________________________________________________________________________________________________________*
 
global ruta = "${surveysFolder}\\survey\PRY\EPH\2015\m10_m12\data_orig"

local PAIS PRY
local ENCUESTA EPH
local ANO "2015"
local ronda m10_m12

local log_file = "${surveysFolder}\harmonized\\`PAIS'\\`ENCUESTA'\\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"
local base_out = "${surveysFolder}\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\\data_merge\\`PAIS'_`ANO'`ronda'.dta"

*log using "`log_file'", replace 

/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Paraguay
Encuesta: EPH 
Round: Octubre-Diciembre
Autores:
Versión 2015: Marcela G. Rubio
Última versión: Marcela G. Rubio - Email: mrubio@iadb.org, marcelarubio28@gmail.com
Fecha última modificación: 5 de Mayo de 2015

							SCL/LMK - IADB
****************************************************************************/


forvalues i = 1/9 {
use "$ruta\reg0`i'_eph2015.dta", clear
cap sort upm nvivi nhoga
cap sort upm nvivi nhoga l02
save, replace
}
forvalues i = 10/11  {
use "$ruta\reg`i'_eph2015.dta", clear
cap sort upm nvivi nhoga 
cap sort upm nvivi nhoga l02
save, replace
}

forvalues i = 13/13  {
use "$ruta\reg`i'_eph2015.dta", clear
cap sort upm nvivi nhoga 
cap sort upm nvivi nhoga l02
save, replace
}


/*
hay que transformar la base para poder incluir estas ya que al tener la variable mes tiene mas de una observacion por hogar
lo que hace que se multipliquen los individuos y se expanda la muestra con duplicados
*/

use "$ruta\reg13_eph2015.dta", replace
reshape wide e02l1 e02l1b e02l1c e02l1d e02l2 e02l2b e02l2c e02l2d fex, i(upm nvivi nhoga dpto area) j(e02lin) 
duplicates report  upm nvivi nhoga
save "$ruta\reg13_eph2015_mod.dta", replace

/*Unifico los modulos de interés para el sociometro:
  Vivienda, ingresos y personas*/
 
use "$ruta\reg02_eph2015.dta", clear
merge m:1 upm nvivi nhoga using "$ruta\reg01_eph2015.dta"
drop _merge
sort upm nvivi nhoga l02

merge m:m upm nvivi nhoga using "$ruta\reg03_eph2015.dta", force
drop _merge
sort upm nvivi nhoga

merge m:m upm nvivi nhoga using "$ruta\reg13_eph2015_mod.dta", force
drop _merge
sort upm nvivi nhoga

merge m:1 upm nvivi nhoga using "$ruta\ingrefam_eph_2015.dta", force
drop _merge
sort upm nvivi nhoga

drop fex1-fex12

saveold "`base_out'", version(12) replace


