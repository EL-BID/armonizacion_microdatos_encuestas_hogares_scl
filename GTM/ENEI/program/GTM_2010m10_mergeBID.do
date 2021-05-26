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
 
global ruta = "${surveysFolder}\\survey\GTM\ENEI\2010\m10\data_orig"

local PAIS GTM
local ENCUESTA ENEI
local ANO "2010"
local ronda m10

local log_file = "${surveysFolder}\harmonized\\`PAIS'\\`ENCUESTA'\\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"
local base_out = "${surveysFolder}\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\\data_merge\\`PAIS'_`ANO'`ronda'.dta"

capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Guatemala
Encuesta: ENEI
Round: Octubre
Autores:Melisa Morales
Versión 2013: Mayra Sáenz
Última versión: Mayra Sáenz - Email: mayras@iadb.org, saenzmayra.a@gmail.com
Fecha última modificación: 24 de Septiembre de 2013

							SCL/LMK - IADB
****************************************************************************/

	*CONFORMACIÓN BASE ÚNICA GUATEMALA 2010*


use  "$ruta\base_hogares_enei2010.dta", clear
sort dominio num_hog
save "$ruta\base_hogares_enei2010.dta", replace

use "$ruta\base_personas_enei2010.dta", clear
sort dominio num_hog
merge dominio num_hog using "$ruta\base_hogares_enei2010.dta"
tab _merge
drop _merge

*Esta encuesta tiene información incompleta de personas y de hogares. Existe una variable que controla por
*esa condición. Se deja la base completa porque sólo así se replican las cifras oficiales. No obstante, se 
*debe tener cuidado manejándola!


saveold "`base_out'", replace


log close

