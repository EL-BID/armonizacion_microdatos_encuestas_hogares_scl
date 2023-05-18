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
 

local PAIS GTM
local ENCUESTA ENEI
local ANO "2021"
local ronda m10

global ruta = "${surveysFolder}\\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\\data_orig"

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

*1. Import las bases originales 
import excel "$ruta\ENEI-1-2021_HOGARES.xlsx", sheet("Sheet1") firstrow clear case(lower)
save "$ruta\ENEI-1-2021_HOGARES.dta", replace 
*
import excel "$ruta\ENEI-1-2021_PERSONAS.xlsx", sheet("Sheet1") firstrow clear case(lower)
save "$ruta\ENEI-1-2021_PERSONAS.dta", replace 

*2.MERGE DE LAS BASES
use "$ruta\ENEI-1-2021_PERSONAS.dta", clear
sort num_hog
merge m:1 num_hogar using "$ruta\ENEI-1-2021_HOGARES.dta"
drop _merge
saveold "`base_out'", version(12) replace

log close

