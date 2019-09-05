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
 
global ruta = "\\Sdssrv03\surveys\\survey\GTM\ENCOVI\2006\a\data_orig"

local PAIS GTM
local ENCUESTA ENCOVI
local ANO "2006"
local ronda m7_m11

local log_file = "\\Sdssrv03\surveys\harmonized\\`PAIS'\\`ENCUESTA'\\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"
local base_out = "\\Sdssrv03\surveys\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\\data_merge\\`PAIS'_`ANO'`ronda'.dta"

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

clear
set more off


use "$ruta\01_Vivienda_ENC06.dta", clear
sort NUM_HOG
save "$ruta\01_Vivienda_ENC06_1.dta", replace



use "$ruta\11_EquipamientoHogar_ENC06.dta", clear
sort NUM_HOG

forvalues i=1(1)44 {
gen equip`i'=0
replace equip`i'=1 if   P14A01A==`i' &  P14A01B==1
by NUM_HOG, sort: egen P14A01A`i'=sum(equip`i')
} 

drop equip*

collapse (mean) P14A01A1-P14A01A44, by (NUM_HOG)

sort NUM_HOG

save "$ruta\11_EquipamientoHogar_ENC06_1.dta", replace




use "$ruta\03_Base_de_Personas_ENC06.dta", clear
sort NUM_HOG 


merge NUM_HOG using "$ruta\01_Vivienda_ENC06_1.dta"
tab _merge
drop _merge
sort NUM_HOG


merge NUM_HOG using "$ruta\11_EquipamientoHogar_ENC06_1.dta"
tab _merge
drop _merge


sort NUM_HOG ID
compress
saveold "`base_out'", replace


log close
