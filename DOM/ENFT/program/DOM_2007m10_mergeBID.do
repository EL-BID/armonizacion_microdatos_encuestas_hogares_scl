		*************************************************************
		***PROGRAMA PARA PEGAR LOS MÓDULOS DE LA ENFT OCTUBRE 2007***
		*************************************************************
*Elaborado por: Yanira Oviedo - Mayo 2010 


clear

*global ruta = "${surveysFolder}"

local PAIS DOM
local ENCUESTA ENFT
local ANO "2007"
local ronda m10 

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\"
local base_out = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
   
capture log close
log using "`log_file'", replace 




*Pegando la información de individuos*
**************************************

use "`base_in'\calculadas07.dta"
sort eft_vivienda eft_hogar eft_miembro
save, replace
clear

use "`base_in'\ocupacion07.dta"
sort eft_vivienda eft_hogar eft_miembro
save, replace
clear

use "`base_in'\ingexterior07.dta"
sort eft_vivienda eft_hogar eft_miembro
save, replace
clear

use "`base_in'\remesas07.dta",replace
sort eft_vivienda eft_hogar eft_miembro
save, replace
clear

use "`base_in'\miembros07.dta"
sort eft_vivienda eft_hogar eft_miembro
save, replace

merge eft_vivienda eft_hogar eft_miembro using "`base_in'\remesas07.dta"
tab _merge
drop _merge
sort eft_vivienda eft_hogar eft_miembro
save "`base_in'\dom07_october.dta", replace

merge eft_vivienda eft_hogar eft_miembro using "`base_in'\ingexterior07.dta"
tab _merge
drop _merge
sort eft_vivienda eft_hogar eft_miembro
save "`base_in'\dom07_october.dta", replace

merge eft_vivienda eft_hogar eft_miembro using "`base_in'\ocupacion07.dta"
tab _merge
drop _merge
sort eft_vivienda eft_hogar eft_miembro
save "`base_in'\dom07_october.dta", replace

merge eft_vivienda eft_hogar eft_miembro using "`base_in'\calculadas07.dta"
tab _merge
drop _merge
sort eft_vivienda eft_hogar eft_miembro
save "`base_in'\dom07_october.dta", replace


*Pegando la información de hogares y viviendas*
***********************************************

clear 
use "`base_in'\hogar07.dta"
sort eft_vivienda eft_hogar 
save, replace

clear 
use "`base_in'\dom07_october.dta", replace
sort eft_vivienda eft_hogar 

merge eft_vivienda eft_hogar using "`base_in'\hogar07.dta"
tab _merge
drop _merge
save "`base_in'\dom07_october.dta", replace

clear 
use "`base_in'\vivienda07.dta"
sort eft_vivienda
save, replace

clear
use "`base_in'\dom07_october.dta"
sort eft_vivienda 

merge eft_vivienda using "`base_in'\vivienda07.dta"
tab _merge
drop _merge
save "`base_in'\dom07_october.dta", replace




* Comprime y guarda base
compress
renpfix eft_
saveold "`base_out'", replace

log close
















