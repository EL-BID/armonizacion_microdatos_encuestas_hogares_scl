* (Versión Stata 12)
clear
set more off


*________________________________________________________________________________________________________________*

 * Activar si es necesario (dejar desactivado para evitar sobreescribir la base y dejar la posibilidad de 
 * utilizar un loop)
 * Los datos se obtienen de las carpetas que se encuentran en el servidor: ${surveysFolder}
 * Se tiene acceso al servidor únicamente al interior del BID.
 * El servidor contiene las bases de datos MECOVI.
 *________________________________________________________________________________________________________________*
 
global ruta = "${surveysFolder}\\survey\GTM\ENCOVI\2000\m7_m11\data_orig"

local PAIS GTM
local ENCUESTA ENCOVI
local ANO "2000"
local ronda m7_m11

local log_file = "${surveysFolder}\harmonized\\`PAIS'\\`ENCUESTA'\\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"
local base_out = "${surveysFolder}\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\\data_merge\\`PAIS'_`ANO'`ronda'.dta"

capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Guatemala
Encuesta: ENEI
Round: Anual
Autores:
Versión 2013: Mayra Sáenz
Última versión: Mayra Sáenz - Email: mayras@iadb.org, saenzmayra.a@gmail.com
Fecha última modificación: 24 de Septiembre de 2013

							SCL/LMK - IADB
*******************************************************************************/

use "$ruta\ecv14.dta", clear
ren hogar HOGAR
ren factor factorh
keep HOGAR factorh item p14a01 p14a02 p14a03 p14a04 p14a05
reshape wide p14a01 p14a02 p14a03 p14a04 p14a05, i(HOGAR factorh) j(item)


sort HOGAR
save "$ruta\gua00_housing_1.dta", replace

use "$ruta\vangua00.dta", clear

sort HOGAR

merge HOGAR using "$ruta\gua00_housing_1.dta"
tab _merge /* son todos 3 */
drop _merge


saveold "`base_out'", replace


log close
