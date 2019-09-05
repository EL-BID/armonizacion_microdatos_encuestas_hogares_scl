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
 
 global ruta = "\\Sdssrv03\surveys\\survey\GTM\ENEI\2015\m10\data_orig"
local PAIS GTM
local ENCUESTA ENEI
local ANO "2015"
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
Autores: Mélany Gualavisí
Última versión: Mélany Gualavisí - Email: melanyg@iadb.org
Fecha última modificación: Noviembre 2016

							SCL/LMK - IADB
****************************************************************************/

use "$ruta\enei_ii_2015_hogares.dta", clear
sort numhog 
saveold "$ruta\enei_ii_2015_hogares_.dta", replace

use "$ruta\enei_ii_2015_personas.dta", clear
sort numhog
merge m:1 numhog using "$ruta\enei_ii_2015_hogares.dta"
drop _merge

saveold "`base_out'", replace

log close

