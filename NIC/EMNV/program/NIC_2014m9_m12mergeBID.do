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
 


global ruta = "\\Sdssrv03\surveys"

local PAIS NIC
local ENCUESTA EMNV
local ANO "2014"
local ronda m9_m12


local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\\"
*local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
                                                    


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Nicaragua
Encuesta: EMNV
Round: Julio-Octubre
Autores:Mayra Sáenz - Email: mayras@iadb.org, saenzmayra.a@gmail.com
Fecha última modificación: 16 de marzo de 2016

							SCL/LMK - IADB
****************************************************************************/
/***************************************************************************
Detalle de procesamientos o modificaciones anteriores:

****************************************************************************/		



*Ordenando las bases por sus llaves:

use "`base_in'emnv14_02_datos_de_la_vivienda_y_el_hogar.dta", clear
sort  i00
save, replace

use "`base_in'pobreza", clear
sort  i00 
save, replace

use "`base_in'emnv14_04_poblacion", clear
sort  i00 s2p00
save, replace

*Uniendo las bases:

merge m:1 i00 using "`base_in'emnv14_02_datos_de_la_vivienda_y_el_hogar.dta"
tab _merge
drop _merge
sort  i00

merge m:1 i00 using "`base_in'pobreza.dta"
tab _merge
drop _merge
sort  i00

saveold "`base_out'", replace
