* (Versión Stata 13)
clear
set more off
*________________________________________________________________________________________________________________*

 * Activar si es necesario (dejar desactivado para evitar sobreescribir la base y dejar la posibilidad de 
 * utilizar un loop)
 * Los datos se obtienen de las carpetas que se encuentran en el servidor: ${surveysFolder}
 * Se tiene acceso al servidor únicamente al interior del BID.
 * El servidor contiene las bases de datos MECOVI.
 *________________________________________________________________________________________________________________*
 
global ruta = "${surveysFolder}"
local PAIS VEN
local ENCUESTA ENCOVI
local ANO "2018"
local ronda a 

local base_out = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
                        
capture log close


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: 
Encuesta: ENCOVI
Round: a
Autores: Daniela Zuluaga - danielazu@iadb.org - da.zuluaga@hotmail.com
Fecha última modificación: Febrero 2019

							SCL/LMK - IADB
****************************************************************************/
****************************************************************************/
*Nota: Hay 11 duplicados- no se eliminan y se cree que esto ocurre por la migración*

use "$ruta\survey\VEN\ENCOVI\2018\a\data_orig\personas_encovi_2018_vers_23_2.dta", clear

*hago el merge con viviendas
mmerge ennum  using "\${surveysFolder}\survey\VEN\ENCOVI\2018\a\data_orig\hogares_encovi_2018_vers_23_2.dta", t(n:1)

compress

drop _merge

saveold "`base_out'", replace
