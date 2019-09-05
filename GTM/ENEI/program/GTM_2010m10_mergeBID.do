* (Versi�n Stata 12)
clear
set more off


*________________________________________________________________________________________________________________*

 * Activar si es necesario (dejar desactivado para evitar sobreescribir la base y dejar la posibilidad de 
 * utilizar un loop)
 * Los datos se obtienen de las carpetas que se encuentran en el servidor: \\Sdssrv03\surveys
 * Se tiene acceso al servidor �nicamente al interior del BID.
 * El servidor contiene las bases de datos MECOVI.
 *________________________________________________________________________________________________________________*
 
global ruta = "\\Sdssrv03\surveys\\survey\GTM\ENEI\2010\m10\data_orig"

local PAIS GTM
local ENCUESTA ENEI
local ANO "2010"
local ronda m10

local log_file = "\\Sdssrv03\surveys\harmonized\\`PAIS'\\`ENCUESTA'\\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"
local base_out = "\\Sdssrv03\surveys\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\\data_merge\\`PAIS'_`ANO'`ronda'.dta"

capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
Pa�s: Guatemala
Encuesta: ENEI
Round: Octubre
Autores:Melisa Morales
Versi�n 2013: Mayra S�enz
�ltima versi�n: Mayra S�enz - Email: mayras@iadb.org, saenzmayra.a@gmail.com
Fecha �ltima modificaci�n: 24 de Septiembre de 2013

							SCL/LMK - IADB
****************************************************************************/

	*CONFORMACI�N BASE �NICA GUATEMALA 2010*


use  "$ruta\base_hogares_enei2010.dta", clear
sort dominio num_hog
save "$ruta\base_hogares_enei2010.dta", replace

use "$ruta\base_personas_enei2010.dta", clear
sort dominio num_hog
merge dominio num_hog using "$ruta\base_hogares_enei2010.dta"
tab _merge
drop _merge

*Esta encuesta tiene informaci�n incompleta de personas y de hogares. Existe una variable que controla por
*esa condici�n. Se deja la base completa porque s�lo as� se replican las cifras oficiales. No obstante, se 
*debe tener cuidado manej�ndola!


saveold "`base_out'", replace


log close

