* (Version Stata 12)
clear
set more off
*________________________________________________________________________________________________________________*

 * Activar si es necesario (dejar desactivado para evitar sobreescribir la base y dejar la posibilidad de 
 * utilizar un loop)
 * Los datos se obtienen de las carpetas que se encuentran en el servidor: \\Sdssrv03\surveys
 * Se tiene acceso al servidor únicamente al interior del BID.
 * El servidor contiene las bases de datos MECOVI.
 *________________________________________________________________________________________________________________*
 
global ruta = "\\Sdssrv03\surveys\\survey\GTM\ENEI\2018\m6\data_orig"
local PAIS GTM
local ENCUESTA ENEI
local ANO "2018"
local ronda m6

local log_file = "\\Sdssrv03\surveys\harmonized\\`PAIS'\\`ENCUESTA'\\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"
local base_out = "\\Sdssrv03\surveys\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\\data_merge\\`PAIS'_`ANO'`ronda'.dta"

capture log close
log using "`log_file'", replace 

*"Z:\survey\GTM\ENEI\2018\m6\data_orig\enei_2_2018_base_de_hogares.dta" 

/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Guatemala
Encuesta: ENEI
Round: Octubre
Autores: Mélany Gualavisí
Última versión: Daniela Zuluaga -danielazu@iadb.org
Fecha última modificación: Marzo 2019

							SCL/LMK - IADB
****************************************************************************/

use "$ruta\ENEI 1-2018 Base Hogares.dta", clear
sort num_hogar
save "$ruta\ENEI 1-2018 Base Hogares.dta", replace

use "$ruta\ENEI 1-2018 Base Personas.dta", clear
sort num_hog
merge m:1 num_hogar using "$ruta\ENEI 1-2018 Base Hogares.dta"
drop _merge
saveold "`base_out'", version(12) replace

log close

