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
 
global ruta = "\\Sdssrv03\surveys\\survey\GTM\ENEI\2012\m6_m7\data_orig"

local PAIS GTM
local ENCUESTA ENEI
local ANO "2012"
local ronda m6_m7

local log_file = "\\Sdssrv03\surveys\harmonized\\`PAIS'\\`ENCUESTA'\\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"
local base_out = "\\Sdssrv03\surveys\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\\data_merge\\`PAIS'_`ANO'`ronda'.dta"

capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
Pa�s: Guatemala
Encuesta: ENEI
Round: Junio-Julio
Autores:Yessenia Loayza
Versi�n 2013: Mayra S�enz
�ltima versi�n: Mayra S�enz - Email: mayras@iadb.org, saenzmayra.a@gmail.com
Fecha �ltima modificaci�n: 24 de Septiembre de 2013

							SCL/LMK - IADB
****************************************************************************/


use "$ruta\enei_2012hogares.dta", clear
sort num_hog
saveold "$ruta\enei_2012hogares.dta", replace

use "$ruta\enei_2012personas.dta", clear
sort num_hog
merge num_hog using "$ruta\enei_2012hogares.dta"
tab _merge
drop _merge

saveold "`base_out'", replace


log close

