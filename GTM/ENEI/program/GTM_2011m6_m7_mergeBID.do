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
 
global ruta = "\\Sdssrv03\surveys\\survey\GTM\ENEI\2011\m6_m7\data_orig"

local PAIS GTM
local ENCUESTA ENEI
local ANO "2011"
local ronda m6_m7

local log_file = "\\Sdssrv03\surveys\harmonized\\`PAIS'\\`ENCUESTA'\\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"
local base_out = "\\Sdssrv03\surveys\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\\data_merge\\`PAIS'_`ANO'`ronda'.dta"

capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Guatemala
Encuesta: ENEI
Round: Junio-Julio
Autores:Yanira Oviedo
Versión 2013: Mayra Sáenz
Última versión: Mayra Sáenz - Email: mayras@iadb.org, saenzmayra.a@gmail.com
Fecha última modificación: 24 de Septiembre de 2013

							SCL/LMK - IADB
****************************************************************************/


use "$ruta\base_de_datos_hogares_enei_2011.dta", clear
sort formulario
saveold "$ruta\base_de_datos_hogares_enei_2011.dta", replace

use "$ruta\base_de_personas_enei_2011.dta", clear
*La variable de id de personas tiene datos perdidos, por ello, se construye un número de orden
sort formulario ppa06
bysort formulario: gen orden=_n
label var orden "Número de orden de la persona"
move orden ppa06
sort formulario

merge formulario using "$ruta\base_de_datos_hogares_enei_2011.dta"
tab _merge
drop _merge

saveold "`base_out'", replace


log close

