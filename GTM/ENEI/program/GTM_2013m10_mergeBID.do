* (Versión Stata 12)
clear
set more off


*________________________________________________________________________________________________________________*

 * Activar si es necesario (dejar desactivado para evitar sobreescribir la base y dejar la posibilidad de 
 * utilizar un loop)
 * Los datos se obtienen de las carpetas que se encuentran en el servidor: \\Sdssrv03\surveys
 * Se tiene acceso al servidor únicamente al interior del BID.
 * El servidor contiene las bases de datos MECOVI.
 *________________________________________________________________________________________________________________*
 
global ruta = "\\Sdssrv03\surveys\\survey\GTM\ENEI\2013\m10\data_orig"

local PAIS GTM
local ENCUESTA ENEI
local ANO "2013"
local ronda m10

local log_file = "\\Sdssrv03\surveys\harmonized\\`PAIS'\\`ENCUESTA'\\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"
local base_out = "\\Sdssrv03\surveys\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\\data_merge\\`PAIS'_`ANO'`ronda'.dta"

capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Guatemala
Encuesta: ENEI
Round: Octubre
Versión 2014: Mayra Sáenz
Última versión: Mayra Sáenz - Email: mayras@iadb.org, saenzmayra.a@gmail.com
Fecha última modificación: 26 de Agosto de 2014

							SCL/LMK - IADB
****************************************************************************/


use "$ruta\ENEI_2013_10_hogares.dta", clear
sort num_hog
saveold "$ruta\enei_2013hogares.dta", replace

use "$ruta\ENEI_2013_10_personas.dta", clear
foreach x of varlist _all {
local name = lower("`x'")
rename `x' `name'
}

sort num_hog
merge m:1 num_hog using "$ruta\enei_2013hogares.dta"
tab _merge
drop _merge

saveold "`base_out'", replace


log close

