* (Versi󮠓tata 12)
clear
set more off


*________________________________________________________________________________________________________________*

 * Activar si es necesario (dejar desactivado para evitar sobreescribir la base y dejar la posibilidad de 
 * utilizar un loop)
 * Los datos se obtienen de las carpetas que se encuentran en el servidor: \\Sdssrv03\surveys
 * Se tiene acceso al servidor 򮩣amente al interior del BID.
 * El servidor contiene las bases de datos MECOVI.
 *________________________________________________________________________________________________________________*
 
global ruta = "\\Sdssrv03\surveys\\survey\PRY\EPH\2011\m10_m12\data_orig"

local PAIS PRY
local ENCUESTA EPH
local ANO "2011"
local ronda m10_m12

local log_file = "\\Sdssrv03\surveys\harmonized\\`PAIS'\\`ENCUESTA'\\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"
local base_out = "\\Sdssrv03\surveys\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\\data_merge\\`PAIS'_`ANO'`ronda'.dta"

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


lo comento para no correrlo varias veces
forvalues i = 1/13 {
use "$ruta\r`i'_eph11.dta", clear
cap sort upm nvivi nhoga 
cap sort upm nvivi nhoga l02
save, replace
}

*/

/*Merging los modulos de inter고para el sociometro:
  Vivienda, ingresos y personas*/
 
use "$ruta\r2_eph11.dta", clear
merge m:1 upm nvivi nhoga using "$ruta\r1_eph11.dta"
tab _merge
drop _merge
sort upm nvivi nhoga l02

merge m:m upm nvivi nhoga using "$ruta\r3_eph11.dta"
tab _merge
drop _merge
sort upm nvivi nhoga

saveold "`base_out'", version(12) replace


log close
