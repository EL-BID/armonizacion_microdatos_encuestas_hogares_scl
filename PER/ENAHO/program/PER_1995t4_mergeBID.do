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
local ENCUESTA ENAHO
local ANO "1995"
local ronda t4 
local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in  = "$ruta\\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig"
*local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
                        
capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Perú
Encuesta: ENAHO
Round: a
Autores: Mayra Sáenz
Última versión: Mayra Sáenz - Email: mayras@iadb.org - saenzmayra.a@gmail.com
Fecha última modificación: octubre 2013

							SCL/LMK - IADB
****************************************************************************/
****************************************************************************/

/*
forvalues  i = 1(1)8 {
use `base_in'\\ena954`i'.dta, clear
foreach j of varlist _all {
local lowname=lower("`j'")
capture rename `j' `lowname'
}

egen idhogar = concat(ubi seg viv hog)
destring idhogar, replace
format %25.0g idhogar
sort idhogar
saveold `base_in'\\ena954`i'.dta, replace
}
*/



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
use "${surveysFolder}\survey\PER\ENAHO\1995\t4\data_orig\ena9543.dta", clear
drop idhogar
egen idhogar = concat(ubi seg hog giiian)
saveold "${surveysFolder}\survey\PER\ENAHO\1995\t4\data_orig\ena9543.dta", replace


clear
use "${surveysFolder}\survey\PER\ENAHO\1995\t4\data_orig\ena9544.dta", clear
drop idhogar
egen idhogar = concat(ubi seg hog gvin)
saveold "${surveysFolder}\survey\PER\ENAHO\1995\t4\data_orig\ena9544.dta", replace


clear
use "${surveysFolder}\survey\PER\ENAHO\1995\t4\data_orig\ena9545.dta", clear
drop idhogar
egen idhogar = concat(ubi seg hog gviin)
saveold "${surveysFolder}\survey\PER\ENAHO\1995\t4\data_orig\ena9545.dta", replace


use "${surveysFolder}\survey\PER\ENAHO\1995\t4\data_orig\ena9543.dta", clear
merge m:m idhogar using "${surveysFolder}\survey\PER\ENAHO\1995\t4\data_orig\ena9544.dta"
tab _merge
capture drop _merge
merge m:m idhogar using "${surveysFolder}\survey\PER\ENAHO\1995\t4\data_orig\ena9545.dta"
tab _merge
capture drop _merge
sort ubi seg viv hog
saveold "${surveysFolder}\survey\PER\ENAHO\1995\t4\data_orig\personas.dta", replace

sort ubi seg viv hog giiian
merge m:m ubi seg viv hog using "${surveysFolder}\survey\PER\ENAHO\1995\t4\data_orig\hogares.dta", force
compress
saveold "${surveysFolder}\survey\PER\ENAHO\1995\t4\data_merge\PER_1995t4.dta", replace


