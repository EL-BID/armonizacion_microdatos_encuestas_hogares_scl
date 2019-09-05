		***********************************************************
		***PROGRAMA PARA PEGAR LOS MÓDULOS DE LA ENFT ABRIL 2005***
		***********************************************************
*Elaborado por: Yanira Oviedo - Mayo 2010 


clear

*global ruta = "\\Sdssrv03\surveys"

local PAIS DOM
local ENCUESTA ENFT
local ANO "2005"
local ronda m4 

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\"
local base_out = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
   
capture log close
log using "`log_file'", replace 


*Pegando la información de individuos*
**************************************

use "`base_in'\calculadas05.dta"
sort eft_vivienda eft_hogar eft_miembro
save, replace
clear

use "`base_in'\ocupacion05.dta"
sort eft_vivienda eft_hogar eft_miembro
save, replace
clear

use "`base_in'\ingexterior05.dta"
sort eft_vivienda eft_hogar eft_miembro
save, replace
clear

use "`base_in'\remesas05.dta"
sort eft_vivienda eft_hogar eft_miembro
save, replace
clear

use "`base_in'\miembros05.dta"
sort eft_vivienda eft_hogar eft_miembro
save, replace

merge eft_vivienda eft_hogar eft_miembro using "`base_in'\remesas05.dta"
tab _merge
drop _merge
sort eft_vivienda eft_hogar eft_miembro
save "`base_in'\dom05_april.dta", replace

merge eft_vivienda eft_hogar eft_miembro using "`base_in'\ingexterior05.dta"
tab _merge
drop _merge
sort eft_vivienda eft_hogar eft_miembro
save "`base_in'\dom05_april.dta", replace

merge eft_vivienda eft_hogar eft_miembro using "`base_in'\ocupacion05.dta"
tab _merge
drop _merge
sort eft_vivienda eft_hogar eft_miembro
save "`base_in'\dom05_april.dta", replace

merge eft_vivienda eft_hogar eft_miembro using "`base_in'\calculadas05.dta"
tab _merge
drop _merge
sort eft_vivienda eft_hogar eft_miembro
save "`base_in'\dom05_april.dta", replace


*Pegando la información de hogares y viviendas*
***********************************************

clear 
use "`base_in'\hogar05.dta"
sort eft_vivienda eft_hogar 
save, replace

clear 
use "`base_in'\dom05_april.dta"
sort eft_vivienda eft_hogar 

merge eft_vivienda eft_hogar using "`base_in'\hogar05.dta"
tab _merge
drop _merge
save "`base_in'\dom05_april.dta", replace

clear 
use "`base_in'\vivienda05.dta"
sort eft_vivienda
save, replace

clear
use "`base_in'\dom05_april.dta"
sort eft_vivienda 

merge eft_vivienda using "`base_in'\vivienda05.dta"
tab _merge
drop _merge
save "`base_in'\dom05_april.dta", replace




* Comprime y guarda base
compress
renpfix eft_
saveold "`base_out'", replace

log close
















