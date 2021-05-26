* (Version Stata 12)
clear
set more off
*________________________________________________________________________________________________________________*

 * Activar si es necesario (dejar desactivado para evitar sobreescribir la base y dejar la posibilidad de 
 * utilizar un loop)
 * Los datos se obtienen de las carpetas que se encuentran en el servidor: ${surveysFolder}
 * Se tiene acceso al servidor únicamente al interior del BID.
 * El servidor contiene las bases de datos MECOVI.
 *________________________________________________________________________________________________________________*
 
global ruta = "${surveysFolder}\\survey\GTM\ENEI\2019\m6\data_orig"
local PAIS GTM
local ENCUESTA ENEI
local ANO "2019"
local ronda m6

local log_file = "${surveysFolder}\harmonized\\`PAIS'\\`ENCUESTA'\\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"
local base_out = "${surveysFolder}\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\\data_merge\\`PAIS'_`ANO'`ronda'.dta"

capture log close
log using "`log_file'", replace 

/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Guatemala
Encuesta: ENEI
Round: Octubre
Autores: Mélany Gualavisí
Última versión: Alvaro Altamirano -alvaroalt@iadb.org
Fecha última modificación: Mayo 2020

							SCL/LMK - IADB
****************************************************************************/

use "$ruta\ENEI 1-2019 Base Hogares.dta", clear
sort num_hogar
save "$ruta\ENEI 1-2019 Base Hogares.dta", replace

use "$ruta\ENEI 1-2019 Base Personas.dta", clear
sort num_hog
merge m:1 num_hogar using "$ruta\ENEI 1-2019 Base Hogares.dta"
drop _merge
saveold "`base_out'", version(12) replace

log close

