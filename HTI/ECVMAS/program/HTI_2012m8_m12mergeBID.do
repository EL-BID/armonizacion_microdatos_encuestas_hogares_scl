*Usando stata v. 12
clear

*Alvaro Altamirano 10/24/2017

global ruta = "\\Sdssrv03\surveys"

local PAIS HTI
local ENCUESTA ECVMAS
local ANO "2012"
local ronda m8_m12

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
   
capture log close
log using "`log_file'", replace 

set more off

 /*Merge */
use "`base_in'\ECVMAS 2012\2_ECVMAS_BASE DE DONNEES\Hogares2012.dta", clear
sort hh_id2new
merge 1:m hh_id2new using "\\Sdssrv03\surveys\survey\HTI\ECVMAS\2012\m8_m12\data_orig\ECVMAS 2012\2_ECVMAS_BASE DE DONNEES\Individuos2012.dta"
merge m:1 hh_id2new using "\\Sdssrv03\surveys\survey\HTI\ECVMAS\2012\m8_m12\data_orig\ECVMAS 2012\2_ECVMAS_BASE DE DONNEES\pesos2012.dta", gen(mergepesos)

save "\\Sdssrv03\surveys\survey\HTI\ECVMAS\2012\m8_m12\data_merge\HTI_2012m8_m12.dta", replace
