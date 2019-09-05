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
local ANO "2017"
local ronda a 

local base_out = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
                        
capture log close


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: 
Encuesta: ENCOVI
Round: a
Autores: Daniela Zuluaga - danielazu@iadb.org - da.zuluaga@hotmail.com
Fecha última modificación: Marzo 2018

							SCL/LMK - IADB
****************************************************************************/
****************************************************************************/


use "$ruta\survey\VEN\ENCOVI\2017\a\data_orig\persona_encovi_2017.dta", clear

**Se corrije caso de id_persona incorrecto**
replace LIN=3 if (CONTROL==3397 & CMHP17==3 & LIN==2)

foreach v of varlist _all {
	local lowname=lower("`v'")
	rename `v' `lowname'
}

tempfile persona

save `persona' , replace

use "$ruta\survey\VEN\ENCOVI\2017\a\data_orig\hogar_encovi_2017.dta", clear
foreach v of varlist _all {
	local lowname=lower("`v'")
	rename `v' `lowname'
}

mmerge control using `persona', t(1:n)

compress

drop _merge

saveold "`base_out'", replace



