*Elaboración: Yessenia Loayza (desloay@hotmail.com | yessenial@iadb.org)
*September, 2014

*** MERGE COLOMBIA ECH 2003 ****
*------------------------------*	

clear
set more off
local anio = 2003
local anioab ="03" 	
local ruta "\\sdssrv03\Surveys\survey\COL\ECH\\`anio'\a\data_orig\"
                         
local m7   ="`ruta'm7\data_orig\" 
local m8   ="`ruta'm8\data_orig\" 
local m9   ="`ruta'm9\data_orig\" 
local t3   ="\\sdssrv03\Surveys\survey\COL\ECH\2003\t3\data_orig\"
local out  ="\\sdssrv03\Surveys\survey\COL\ECH\2003\t3\data_merge\"


*1. Bases anuales con homologacion de ingresos
*----------------------------------------------
/*
clear
use "\\sdssrv03\Surveys\survey\COL\ECH\2003\a\data_orig\anual_homologado DANE\data_orig\personas 2003.dta", clear
gen idh_ch =llave_hog
sort idh_ch
merge idh_ch using "\\sdssrv03\Surveys\survey\COL\ECH\2003\a\data_orig\anual_homologado DANE\data_orig\hogares 2003.dta"
tab _merge
drop _merge
destring mes, replace
drop idh_ch
egen id=concat(llave_hog orden)
keep if mes>=7 & mes<=9
keep  id pet- fex_c 
sort id
save "Y:\survey\COL\ECH\2003\a\data_orig\anual_homologado DANE\data_merge\t3.dta", replace

*/
*2. Append entre meses
*------------------------

foreach zona in cabecera resto {

*Personas
         use "`m7'\`zona' - características generales - educación.dta", clear
append using "`m8'\`zona' - características generales - educación.dta"
append using "`m9'\`zona' - características generales - educación.dta"
egen id =concat(llave_hog orden)
sort id
saveold "`t3'col_`zona'_caracteristicas_generales.dta", replace

*Fuerza Trabajo
         use "`m7'\`zona' - fuerza de trabajo.dta", clear
append using "`m8'\`zona' - fuerza de trabajo.dta"
append using "`m9'\`zona' - fuerza de trabajo.dta"
egen id =concat(llave_hog orden)
sort id
saveold "`t3'col_`zona'_ft.dta", replace

*ocupados
         use "`m7'\`zona' - ocupados empleo principal.dta", clear
append using "`m8'\`zona' - ocupados empleo principal.dta"
append using "`m9'\`zona' - ocupados empleo principal.dta"
egen id =concat(llave_hog orden)
sort id
saveold "`t3'col_`zona'_ocupados.dta", replace

*Subempleados
         use "`m7'\`zona' - ocupados - subempleo.dta", clear
append using "`m8'\`zona' - ocupados - subempleo.dta"
append using "`m9'\`zona' - ocupados - subempleo.dta"
egen id =concat(llave_hog orden)
sort id
saveold "`t3'col_`zona'_subemp.dta", replace

*Trabajo Secundario
         use "`m7'\`zona' - ocupados - empleo secundario.dta", clear
append using "`m8'\`zona' - ocupados - empleo secundario.dta"
append using "`m9'\`zona' - ocupados - empleo secundario.dta"
egen id =concat(llave_hog orden)
sort id
saveold "`t3'col_`zona'_secund.dta", replace

*Desocupados
         use "`m7'\`zona' - desocupados.dta", clear
append using "`m8'\`zona' - desocupados.dta"
append using "`m9'\`zona' - desocupados.dta"
egen id =concat(llave_hog orden)
sort id
saveold "`t3'col_`zona'_desocupados.dta", replace
 
*Inactivos
         use "`m7'\`zona' - inactivos.dta", clear
append using "`m8'\`zona' - inactivos.dta"
append using "`m9'\`zona' - inactivos.dta"
egen id =concat(llave_hog orden)
sort id
saveold "`t3'col_`zona'_inact.dta", replace
}

*3. Merge de los 8 modulos trimestrales por zona
*-----------------------------------------------
foreach zona in cabecera resto {
use `t3'col_`zona'_caracteristicas_generales.dta, clear
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
use "`out'COL_`anio't3cabecera.dta"
append using "`out'COL_`anio't3resto.dta"
sort id
merge 1:m  id using "Y:\survey\COL\ECH\2003\a\data_orig\anual_homologado DANE\data_merge\t3.dta"
tab _merge
drop _merge
replace fex_c=fex_c/3
label var fex_c "factor de expansion anual dividido 3"
saveold "`out'COL_`anio't3.dta", replace






















