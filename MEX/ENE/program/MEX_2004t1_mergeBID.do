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
 
global ruta = "${surveysFolder}\\survey\MEX\ENE\2004\t1\data_orig"

local PAIS MEX
local ENCUESTA ENE
local ANO "2004"
local ronda t1

local log_file = "${surveysFolder}\harmonized\\`PAIS'\\`ENCUESTA'\\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"
local base_out = "${surveysFolder}\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\\data_merge\\`PAIS'_`ANO'`ronda'.dta"

*capture log close
*log using "`log_file'", replace 


use "$ruta\ene104.dta"
replace c_res="" if c_res=="A"
destring eda c_res, replace
drop if c_res<1 | c_res>7 | eda<=11 | eda==99				
sort  a_met ent con v_sel hog h_mud	
saveold "$ruta\ene_aux.dta" , replace

use "$ruta\hog-104.dta"
sort  a_met ent con v_sel hog h_mud	
merge a_met ent con v_sel hog h_mud	 using "$ruta\ene_aux.dta"
tab _merge
keep if _merge==3
drop _merge
saveold "`base_out'", replace

