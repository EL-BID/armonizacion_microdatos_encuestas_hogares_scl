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
 
global ruta = "\\Sdssrv03\surveys\\survey\MEX\ENEU\2004\t4\data_orig"

local PAIS MEX
local ENCUESTA ENEU
local ANO "2004"
local ronda t4

local log_file = "\\Sdssrv03\surveys\harmonized\\`PAIS'\\`ENCUESTA'\\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"
local base_out = "\\Sdssrv03\surveys\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\\data_merge\\`PAIS'_`ANO'`ronda'.dta"

*capture log close
*log using "`log_file'", replace 


use "$ruta\eneu404.dta"
sort  a_met ent mun con v_sel hog h_mud	
destring eda, replace
drop if eda<=11 | eda==99
saveold "$ruta\eneu_aux.dta" , replace

use "$ruta\hog_404.dta"
sort  a_met ent mun con v_sel hog h_mud	
merge a_met ent mun con v_sel hog h_mud using "$ruta\eneu_aux.dta"
tab _merge
keep if _merge==3
drop _merge
saveold "`base_out'", replace

