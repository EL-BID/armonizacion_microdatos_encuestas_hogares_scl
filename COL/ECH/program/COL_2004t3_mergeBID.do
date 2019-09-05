*Elaboración: Yessenia Loayza (desloay@hotmail.com | yessenial@iadb.org)
*Mayo, 2014

*** MERGE COLOMBIA ECH 2004 ****
*------------------------------*	

clear
set more off
local anio = 2004
local anioab ="04" 	
local ruta "\\sdssrv03\Surveys\survey\COL\ECH\\`anio'\a\data_orig\"
                         
local m7   ="`ruta'm7\data_orig\" 
local m8   ="`ruta'm8\data_orig\" 
local m9   ="`ruta'm9\data_orig\" 
local t3   ="\\sdssrv03\Surveys\survey\COL\ECH\2004\t3\data_orig\"
local out  ="\\sdssrv03\Surveys\survey\COL\ECH\2004\t3\data_merge\"


*1. Bases anuales con homologacion de ingresos
*----------------------------------------------

clear
use "\\sdssrv03\Surveys\survey\COL\ECH\2004\a\data_orig\anual_homologado DANE\data_orig\personas 2004.dta", clear
gen idh_ch =llave_hog
sort idh_ch
merge idh_ch using "\\sdssrv03\Surveys\survey\COL\ECH\2004\a\data_orig\anual_homologado DANE\data_orig\hogares2004.dta"
tab _merge
drop _merge
destring mes, replace
drop idh_ch
egen id=concat(llave_hog orden)
keep if mes>=7 & mes<=9
keep  id pet- fex_c 
sort id
save "Y:\survey\COL\ECH\2004\a\data_orig\anual_homologado DANE\data_merge\t3.dta", replace


*2. Append entre meses
*------------------------

foreach zona in cab res {

*Personas
         use "`m7'\sas10`zona'0407.dta", clear
append using "`m8'\sas10`zona'0408.dta"
append using "`m9'\sas10`zona'0409.dta"
egen id =concat(llave_hog orden)
sort id

saveold "`t3'col_`zona'_personas.dta", replace

*Fuerza Trabajo
         use "`m7'\sas50`zona'0407.dta", clear
append using "`m8'\sas50`zona'0408.dta"
append using "`m9'\sas50`zona'0409.dta"
egen id =concat(llave_hog orden)
sort id
saveold "`t3'col_`zona'_ft.dta", replace

*ocupados
         use "`m7'\sas60`zona'0407.dta", clear
append using "`m8'\sas60`zona'0408.dta"
append using "`m9'\sas60`zona'0409.dta"
egen id =concat(llave_hog orden)
sort id
saveold "`t3'col_`zona'_ocupados.dta", replace

*Subempleados
         use "`m7'\sas61`zona'0407.dta", clear
append using "`m8'\sas61`zona'0408.dta"
append using "`m9'\sas61`zona'0409.dta"
egen id =concat(llave_hog orden)
sort id
saveold "`t3'col_`zona'_subemp.dta", replace

*Trabajo Secundario
         use "`m7'\sas62`zona'0407.dta", clear
append using "`m8'\sas62`zona'0408.dta"
append using "`m9'\sas62`zona'0409.dta"
egen id =concat(llave_hog orden)
sort id
saveold "`t3'col_`zona'_secund.dta", replace

*Desocupados
         use "`m7'\sas70`zona'0407.dta", clear
append using "`m8'\sas70`zona'0408.dta"
append using "`m9'\sas70`zona'0409.dta"
egen id =concat(llave_hog orden)
sort id
saveold "`t3'col_`zona'_desocupados.dta", replace
 
*Inactivos
         use "`m7'\sas80`zona'0407.dta", clear
append using "`m8'\sas80`zona'0408.dta"
append using "`m9'\sas80`zona'0409.dta"
egen id =concat(llave_hog orden)
sort id
saveold "`t3'col_`zona'_inact.dta", replace
}


*3. Merge de los 8 modulos trimestrales por zona
*-----------------------------------------------
foreach zona in cab res {
use `t3'col_`zona'_personas.dta, clear
merge id using `t3'col_`zona'_desocupados.dta  
tab _merge
drop _merge
sort id

merge  id using `t3'col_`zona'_ft.dta 
tab _merge
drop _merge
sort id

merge  id using `t3'col_`zona'_inact.dta
tab _merge
drop _merge
sort id

merge id using `t3'col_`zona'_ocupados.dta
tab _merge
drop _merge
sort id

merge id using `t3'col_`zona'_secund.dta
tab _merge
drop _merge
sort id

merge id using `t3'col_`zona'_subemp.dta
tab _merge
drop _merge
sort id

merge id using `t3'col_`zona'_subemp.dta
tab _merge
drop _merge
sort id

saveold "`out'COL_`anio't3`zona'.dta", replace
}

*4. Append zonas & base de ingresos homologadas
*----------------------------------------------
clear
use "`out'COL_`anio't3cab.dta"
append using "`out'COL_`anio't3res.dta"
sort id
merge 1:m  id using "Y:\survey\COL\ECH\2004\a\data_orig\anual_homologado DANE\data_merge\t3.dta"
tab _merge
drop _merge
replace fex_c=fex_c/3
label var fex_c "factor de expansion anual dividido 3"
saveold "`out'COL_`anio't3.dta", replace






















