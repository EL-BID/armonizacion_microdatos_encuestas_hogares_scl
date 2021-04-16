		***********************************************************
		***PROGRAMA PARA PEGAR LOS MÓDULOS DE LA ENFT ABRIL 2006***
		***********************************************************
*Elaborado por: Yanira Oviedo - Mayo 2010 


clear

*global ruta = "${surveysFolder}"

local PAIS DOM
local ENCUESTA ENFT
local ANO "2006"
local ronda m4 

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\"
local base_out = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
   
capture log close
log using "`log_file'", replace 


*Pegando la información de individuos*
**************************************

use "`base_in'\calculadas06.dta"
sort eft_vivienda eft_hogar eft_miembro
save, replace
clear

use "`base_in'\ocupacion06.dta"
sort eft_vivienda eft_hogar eft_miembro
save, replace
clear

use "`base_in'\ingexterior06.dta"
sort eft_vivienda eft_hogar eft_miembro
save, replace
clear

use "`base_in'\remesas06.dta"
sort eft_vivienda eft_hogar eft_miembro
save, replace
clear

use "`base_in'\miembros06.dta"
sort eft_vivienda eft_hogar eft_miembro
save, replace

merge eft_vivienda eft_hogar eft_miembro using "`base_in'\remesas06.dta"
tab _merge
drop _merge
sort eft_vivienda eft_hogar eft_miembro
save "`base_in'\dom06_april.dta", replace

merge eft_vivienda eft_hogar eft_miembro using "`base_in'\ingexterior06.dta"
tab _merge
drop _merge
sort eft_vivienda eft_hogar eft_miembro
save "`base_in'\dom06_april.dta", replace

merge eft_vivienda eft_hogar eft_miembro using "`base_in'\ocupacion06.dta"
tab _merge
drop _merge
sort eft_vivienda eft_hogar eft_miembro
save "`base_in'\dom06_april.dta", replace

merge eft_vivienda eft_hogar eft_miembro using "`base_in'\calculadas06.dta"
tab _merge
drop _merge
sort eft_vivienda eft_hogar eft_miembro
save "`base_in'\dom06_april.dta", replace


*Pegando la información de hogares y viviendas*
***********************************************

clear 
use "`base_in'\hogar06.dta"
sort eft_vivienda eft_hogar 
save, replace

clear 
use "`base_in'\dom06_april.dta"
sort eft_vivienda eft_hogar 

merge eft_vivienda eft_hogar using "`base_in'\hogar06.dta"
tab _merge
drop _merge
save "`base_in'\dom06_april.dta", replace

clear 
use "`base_in'\vivienda06.dta"
sort eft_vivienda
save, replace

clear
use "`base_in'\dom06_april.dta"
sort eft_vivienda 

merge eft_vivienda using "`base_in'\vivienda06.dta"
tab _merge
drop _merge
save "`base_in'\dom06_april.dta", replace




* Comprime y guarda base
compress
renpfix eft_
saveold "`base_out'", replace

log close
















