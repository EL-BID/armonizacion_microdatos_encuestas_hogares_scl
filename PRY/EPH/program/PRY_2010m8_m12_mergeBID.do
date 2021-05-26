* (Versi󮠓tata 12)
clear
set more off


*________________________________________________________________________________________________________________*

 * Activar si es necesario (dejar desactivado para evitar sobreescribir la base y dejar la posibilidad de 
 * utilizar un loop)
 * Los datos se obtienen de las carpetas que se encuentran en el servidor: ${surveysFolder}
 * Se tiene acceso al servidor 򮩣amente al interior del BID.
 * El servidor contiene las bases de datos MECOVI.
 *________________________________________________________________________________________________________________*
 
global ruta = "${surveysFolder}\\survey\PRY\EPH\2010\m10_m12\data_orig"

local PAIS PRY
local ENCUESTA EPH
local ANO "2010"
local ronda m10_m12

local log_file = "${surveysFolder}\harmonized\\`PAIS'\\`ENCUESTA'\\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"
local base_out = "${surveysFolder}\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\\data_merge\\`PAIS'_`ANO'`ronda'.dta"

capture log close
*log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SIMS/SOCIOMETRO 
País Paraguay
Encuesta: EPH 
Round: Octubre-Diciembre
Autores: Alvaro Altamirano
Última versión: Alvaro Altamirano - Email: alvaroalt@iadb.org
Fecha última modificación: Junio de 2018

							SCL/LMK - IADB
****************************************************************************/


/*lo comento para no correrlo varias veces
forvalues i = 1/9 {
use "$ruta\r0`i'_eph10.dta", clear
cap sort upm nvivi nhoga 
cap sort upm nvivi nhoga l02
save, replace
}

forvalues i = 10/12 {
use "$ruta\r`i'_eph10.dta", clear
cap sort upm nvivi nhoga 
cap sort upm nvivi nhoga l02
save, replace
}
*/

/*Merging los modulos de inter고para el sociometro:
  Vivienda, ingresos y personas*/
 
use "$ruta\r02_eph10.dta", clear
merge m:1 upm nvivi nhoga using "$ruta\r01_eph10.dta"
tab _merge
drop _merge
sort upm nvivi nhoga l02

merge m:m upm nvivi nhoga using "$ruta\r03_eph10.dta"
tab _merge
drop _merge
sort upm nvivi nhoga

saveold "`base_out'", version(12) replace


log close
