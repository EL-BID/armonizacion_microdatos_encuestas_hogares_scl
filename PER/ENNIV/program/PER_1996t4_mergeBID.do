************************************
* (Versión Stata 13)
clear
set more off
*________________________________________________________________________________________________________________*

 * Activar si es necesario (dejar desactivado para evitar sobreescribir la base y dejar la posibilidad de 
 * utilizar un loop)
 * Los datos se obtienen de las carpetas que se encuentran en el servidor: ${surveysFolder}
 * Se tiene acceso al servidor únicamente al interior del BID.
 * El servidor contiene las bases de datos MECOVI.
 *________________________________________________________________________________________________________________*
 

global ruta = "${surveysFolder}"

local PAIS PER
local ENCUESTA ENNIV
local ANO "1996"
local ronda m10_m12 
local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in  = "$ruta\\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig"
*local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
                        
capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Perú
Encuesta: ENNIV
Round: a
Autores: Mayra Sáenz
Última versión: Mayra Sáenz - Email: mayras@iadb.org - saenzmayra.a@gmail.com
Fecha última modificación: octubre 2013

							SCL/LMK - IADB
****************************************************************************/
****************************************************************************/


forvalues  i = 1(1)2 {
use `base_in'\\ena964`i'.dta, clear
foreach j of varlist _all {
local lowname=lower("`j'")
capture rename `j' `lowname'
}
capture drop idhogar
egen idhogar = concat(ubi seg viv hog)
destring idhogar, replace
format %25.0g idhogar
sort idhogar
saveold `base_in'\\ena964`i'.dta, replace
}




*Para bases 1 y 2 el identificador se genera con ubi seg viv hog (archivos de hogares)
*Para bases 3 - 8 el identificador se genera con ubi seg viv hog per (archivos de personas)
/*
clear
use `base_in'\\ena9541.dta, clear
merge m:m idhogar using `base_in'\\ena9542.dta
capture drop _merge
merge m:m idhogar using `base_in'\\ena9548.dta"
sort ubi seg viv hog
drop _merge
saveold `base_in'\\hogares.dta, replace
*/

clear
use "${surveysFolder}\survey\PER\ENNIV\1996\m10_m12\data_orig\ena9641.dta", clear
merge m:m idhogar using "${surveysFolder}\survey\PER\ENNIV\1996\m10_m12\data_orig\ena9642.dta"
tab _merge
capture drop _merge
merge m:m idhogar using "${surveysFolder}\survey\PER\ENNIV\1996\m10_m12\data_orig\ena9648.dta"
tab _merge
capture drop _merge
sort ubi seg viv hog
saveold "${surveysFolder}\survey\PER\ENNIV\1996\m10_m12\data_orig\hogares.dta", replace
*Existen más observaciones en la base master que en la using al momento de hacer el merge, de acuerdo a encuestas posteriores
* pueden ser las encuestas incompletas o vacías.
clear
use "${surveysFolder}\survey\PER\ENNIV\1996\m10_m12\data_orig\ena9643.dta", clear
sort ubi seg viv hog 
merge m:m ubi seg viv hog using "${surveysFolder}\survey\PER\ENNIV\1996\m10_m12\data_orig\hogares.dta", force
tab _merge
drop if _merge != 3
drop _merge
compress
saveold "${surveysFolder}\survey\PER\ENNIV\1996\m10_m12\data_merge\PER_1996m10_m12.dta", replace


