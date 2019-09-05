* (VersiÃ³n Stata 12)
clear
set more off
*________________________________________________________________________________________________________________*

 * Activar si es necesario (dejar desactivado para evitar sobreescribir la base y dejar la posibilidad de 
 * utilizar un loop)
 * Los datos se obtienen de las carpetas que se encuentran en el servidor: \\Sdssrv03\surveys
 * Se tiene acceso al servidor únicamente al interior del BID.
 * El servidor contiene las bases de datos MECOVI.
 *________________________________________________________________________________________________________________*
 
global ruta = "\\Sdssrv03\surveys\\survey\GTM\ENEI\2017\m10\data_orig"
local PAIS GTM
local ENCUESTA ENEI
local ANO "2017"
local ronda m10

local log_file = "\\Sdssrv03\surveys\harmonized\\`PAIS'\\`ENCUESTA'\\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"
local base_out = "\\Sdssrv03\surveys\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\\data_merge\\`PAIS'_`ANO'`ronda'.dta"

capture log close
log using "`log_file'", replace 

*"Z:\survey\GTM\ENEI\2017\m10\data_orig\enei_2_2017_base_de_hogares.dta" 

/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Guatemala
Encuesta: ENEI
Round: Octubre
Autores: Mélany Gualavisí
Última versión: Stephanie GonzÃ¡lez Rubio - Email: stephaniego@iadb.org
Fecha última modificación: Mayo 2017

							SCL/LMK - IADB
****************************************************************************/

use "$ruta\enei_2_2017_hogares.dta", clear
sort num_hogar
saveold "$ruta\enei_2_2017_hogares.dta", version(12) replace

use "$ruta\enei_2_2017_personas.dta", clear
sort num_hog
merge m:1 num_hogar using "$ruta\enei_2_2017_hogares.dta"
drop _merge
saveold "`base_out'", version(12) replace

log close

