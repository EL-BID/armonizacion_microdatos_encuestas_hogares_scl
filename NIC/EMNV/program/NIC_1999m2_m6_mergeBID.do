
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
 


global ruta = "\\Sdssrv03\surveys\survey\NIC\EMNV\1999\m2_m6"

local PAIS NIC
local ENCUESTA EMNV
local ANO "1999"
local ronda m2_m6

local log_file = "Y:\harmonized\NIC\EMNV\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"

                                                    
capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Nicaragua
Encuesta: EMNV
Round: Febrero-Junio
Autores:
Versión 2013: Mayra Sáenz
Última versión: Mayra Sáenz - Email: mayras@iadb.org, saenzmayra.a@gmail.com
Fecha última modificación: 10 de Septiembre de 2013

							SCL/LMK - IADB
****************************************************************************/
/***************************************************************************
Detalle de procesamientos o modificaciones anteriores:

****************************************************************************/
clear

use "\\Sdssrv03\surveys\survey\NIC\EMNV\1999\m2_m6\data_orig\VIVIENDM.DTA"

gen id_hogar=real(string(i00aM)+string(i00bM))
sort id_hogar
saveold "\\Sdssrv03\surveys\survey\NIC\EMNV\1999\m2_m6\data_orig\viv.dta", replace

clear
use "\\Sdssrv03\surveys\survey\NIC\EMNV\1999\m2_m6\data_orig\POBLAM.DTA"
gen id_hogar=real(string(i00a)+string(i00b))
sort id_hogar
saveold "\\Sdssrv03\surveys\survey\NIC\EMNV\1999\m2_m6\data_orig\pobla.dta", replace

clear
use "\\Sdssrv03\surveys\survey\NIC\EMNV\1999\m2_m6\data_orig\GASTOSMB.DTA"
capture drop id_hogar
gen id_hogar=real(string(id1a)+string(id1b))
sort id_hogar
saveold "\\Sdssrv03\surveys\survey\NIC\EMNV\1999\m2_m6\data_orig\gastosmb.dta", replace

clear
use "\\Sdssrv03\surveys\survey\NIC\EMNV\1999\m2_m6\data_orig\GASTOSMA.DTA"
capture drop id_hogar
gen id_hogar=real(string(id1a)+string(id1b))
sort id_hogar
saveold "\\Sdssrv03\surveys\survey\NIC\EMNV\1999\m2_m6\data_orig\gastosma.dta", replace

clear
use "\\Sdssrv03\surveys\survey\NIC\EMNV\1999\m2_m6\data_orig\EQUIPO1.DTA"
capture drop id_hogar
gen id_hogar=real(string(i00a)+string(i00b))
sort id_hogar
saveold "\\Sdssrv03\surveys\survey\NIC\EMNV\1999\m2_m6\data_orig\equipo1.dta", replace

clear
use "\\Sdssrv03\surveys\survey\NIC\EMNV\1999\m2_m6\data_orig\CONSUMO9.DTA"
capture drop id_hogar
gen id_hogar=real(string(i00a)+string(i00b))
sort id_hogar
saveold "\\Sdssrv03\surveys\survey\NIC\EMNV\1999\m2_m6\data_orig\consumo9.dta", replace

clear
use "\\Sdssrv03\surveys\survey\NIC\EMNV\1999\m2_m6\data_orig\AHORRO-M.DTA"
capture drop id_hogar
gen id_hogar=real(string(id1a)+string(id1b))
sort id_hogar
saveold "\\Sdssrv03\surveys\survey\NIC\EMNV\1999\m2_m6\data_orig\ahorrom.dta", replace

clear
use "\\Sdssrv03\surveys\survey\NIC\EMNV\1999\m2_m6\data_orig\AHORRO2M"
capture drop id_hogar
gen id_hogar=real(string(id1a)+string(id1b))
sort id_hogar
saveold "\\Sdssrv03\surveys\survey\NIC\EMNV\1999\m2_m6\data_orig\ahorro2m.dta", replace

clear
use "\\Sdssrv03\surveys\survey\NIC\EMNV\1999\m2_m6\data_orig\AHORRO3M.DTA"
capture drop id_hogar
gen id_hogar=real(string(id1a)+string(id1b))
sort id_hogar
saveold "\\Sdssrv03\surveys\survey\NIC\EMNV\1999\m2_m6\data_orig\ahorro3m.dta", replace



 * Generar identificadores en cada base y luego s'i correr el loop del merge.

local in1  "\\Sdssrv03\surveys\survey\NIC\EMNV\1999\m2_m6\data_orig\pobla.dta"
local in2  "\\Sdssrv03\surveys\survey\NIC\EMNV\1999\m2_m6\data_orig\viv.dta"
*local in3 "\\Sdssrv03\surveys\survey\NIC\EMNV\1999\m2_m6\data_orig\gastosmb.dta"
*local in4 "\\Sdssrv03\surveys\survey\NIC\EMNV\1999\m2_m6\data_orig\gastosma.dta"
local in3 "\\Sdssrv03\surveys\survey\NIC\EMNV\1999\m2_m6\data_orig\Equipos99.dta"
*local in4 "\\Sdssrv03\surveys\survey\NIC\EMNV\1999\m2_m6\data_orig\consumo9.dta"
*local in7 "\\Sdssrv03\surveys\survey\NIC\EMNV\1999\m2_m6\data_orig\ahorrom.dta"
*local in8 "\\Sdssrv03\surveys\survey\NIC\EMNV\1999\m2_m6\data_orig\ahorro2m.dta"
*local in9 "\\Sdssrv03\surveys\survey\NIC\EMNV\1999\m2_m6\data_orig\ahorro3m.dta"



use `in1'
forvalues h=2(1)3 {
merge m:m id_hogar using `in`h''
tab _merge
drop _merge
sort id_hogar
}



* Activar solo si es necesario
*keep *_ci  *_c  idh_ch 
qui compress

saveold "\\Sdssrv03\surveys\survey\NIC\EMNV\1999\m2_m6\data_merge\NIC_1999m2_m6.dta", replace


log close
