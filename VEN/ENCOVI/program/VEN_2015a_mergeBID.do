* (Versión Stata 13)
clear
set more off
*________________________________________________________________________________________________________________*

 * Activar si es necesario (dejar desactivado para evitar sobreescribir la base y dejar la posibilidad de 
 * utilizar un loop)
 * Los datos se obtienen de las carpetas que se encuentran en el servidor: \\Sdssrv03\surveys
 * Se tiene acceso al servidor únicamente al interior del BID.
 * El servidor contiene las bases de datos MECOVI.
 *________________________________________________________________________________________________________________*
 
global ruta = "\\Sdssrv03\surveys"

local PAIS VEN
local ENCUESTA ENCOVI
local ANO "2015"
local ronda a 

local base_out = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
                        
capture log close


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: 
Encuesta: ENCOVI
Round: a
Autores: Mayra Sáenz - saenzmayra.a@gmail.com - mayras@iadb.org 
Fecha última modificación: Marzo 2017

							SCL/LMK - IADB
****************************************************************************/
****************************************************************************/


use "$ruta\survey\VEN\ENCOVI\2015\a\data_orig\hogaresENCOVI2015.dta"
foreach v of varlist _all {
	local lowname=lower("`v'")
	rename `v' `lowname'
}
saveold "$ruta\survey\VEN\ENCOVI\2015\a\data_orig\hogaresENCOVI2015_.dta", replace


use "$ruta\survey\VEN\ENCOVI\2015\a\data_orig\personasENCOVI2015.dta", clear
foreach v of varlist _all {
	local lowname=lower("`v'")
	rename `v' `lowname'
}

merge m:1 control using  "$ruta\survey\VEN\ENCOVI\2015\a\data_orig\hogaresENCOVI2015_.dta"

compress

drop _merge

saveold "`base_out'", replace



