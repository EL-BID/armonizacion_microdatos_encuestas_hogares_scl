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
 
global ruta = "\\Sdssrv03\surveys\\survey\GTM\ENEI\2014\m10\data_orig"

local PAIS GTM
local ENCUESTA ENEI
local ANO "2014"
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
Última versión: Marcela G. Rubio - Email: mrubio@iadb.org, marcelarubio28@gmail.com
Fecha última modificación: 27 de Abril de 2015

							SCL/LMK - IADB
****************************************************************************/

use "$ruta\enei_ii_2014_hogares_.dta", clear
sort num_hogar 
saveold "$ruta\enei_ii_2014_hogares_.dta", replace

use "$ruta\enei_ii_2014_personas_.dta", clear

sort num_hogar
merge m:1 num_hog using "$ruta\enei_ii_2014_hogares_.dta"
drop _merge

saveold "`base_out'", replace

log close

