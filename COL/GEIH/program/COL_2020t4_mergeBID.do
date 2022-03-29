
/*
Autor: Nathalia Maya
Ultima version: Angela Lopez(alop@iadb.org)
Mayo, 2021 Version Stata 16 */

*** MERGE COLOMBIA GEIH 2020 (t4) ****
*------------------------------*	

clear
set more off
local anio =2020
local ronda1 a
local ronda2 t4
local ruta "${surveysFolder}\survey\COL\GEIH\\`anio'\"
local m10 ="`ruta'\`ronda1'\data_orig\m10\" 
local m11 ="`ruta'\`ronda1'\data_orig\m11\" 
local m12 ="`ruta'\`ronda1'\data_orig\m12\" 
local t4 ="`ruta'\`ronda2'\data_orig\"
local out ="`ruta'\`ronda2'\data_merge\"


*1. Bases anuales con homologacion de ingresos:
*----------------------------------------------

clear
use "`ruta'\`ronda1'\data_orig\anual_homologado_DANE\Hogares.dta", clear
*La base de hogares tiene duplicados de los meses 3,4 y 5, sin embargo, se borran ya que corresponden a meses que no hacen parte de este trimestre
duplicates tag directorio secuencia_p , gen(dup)
drop if dup==1

merge 1:m directorio secuencia_p using "`ruta'\`ronda1'\data_orig\anual_homologado_DANE\Personas.dta", force
drop if _merge==2 //meses que no se usarán
drop _merge
egen id =concat (directorio secuencia_p orden)
sort id
saveold "`ruta'\`ronda1'\data_merge\pov_anual.dta", replace
destring mes, replace
keep if mes>=10 & mes<=12

keep  impaes- id impa-iof6 nper-fex_c dominio
save "`ruta'\`ronda1'\data_merge\pov_t4.dta", replace

*2. Append entre meses
*------------------------

foreach zona in Cabecera Resto {

*Personas
use "`m10'\`zona' - Características generales (Personas).dta", clear
append using "`m11'\`zona' - Características generales (Personas).dta"
append using "`m12'\`zona' - Características generales (Personas).dta"
egen id = concat(DIRECTORIO SECUENCIA_P ORDEN)
sort id
saveold "`t4'col_`zona'_personas.dta", replace

*Desocupados
use "`m10'\`zona' - Desocupados.dta", clear
append using "`m11'\`zona' - Desocupados.dta"
append using "`m12'\`zona' - Desocupados.dta"
egen id = concat(DIRECTORIO SECUENCIA_P ORDEN)
sort id
saveold "`t4'col_`zona'_desocupados.dta", replace
				
*Fuerza Trabajo
use "`m10'\`zona' - Fuerza de trabajo.dta", clear
append using "`m11'\`zona' - Fuerza de trabajo.dta"
append using "`m12'\`zona' - Fuerza de trabajo.dta"
egen id = concat(DIRECTORIO SECUENCIA_P ORDEN)
sort id
saveold "`t4'col_`zona'_ft.dta", replace

*Inactivos
use "`m10'\`zona' - Inactivos.dta", clear
append using "`m11'\`zona' - Inactivos.dta"
append using "`m12'\`zona' - Inactivos.dta"
egen id = concat(DIRECTORIO SECUENCIA_P ORDEN)
sort id
saveold "`t4'col_`zona'_inactivos.dta", replace

*ocupados
use "`m10'\`zona' - Ocupados.dta", clear
append using "`m11'\`zona' - Ocupados.dta"
append using "`m12'\`zona' - Ocupados.dta"
egen id = concat(DIRECTORIO SECUENCIA_P ORDEN)
sort id
saveold "`t4'col_`zona'_ocupados.dta", replace

*otrasactv
use "`m10'\`zona' - Otras actividades y ayudas en la semana.dta", clear
append using "`m11'\`zona' - Otras actividades y ayudas en la semana.dta"
append using "`m12'\`zona' - Otras actividades y ayudas en la semana.dta"
egen id = concat(DIRECTORIO SECUENCIA_P ORDEN)
rename clase CLASE
sort id
saveold "`t4'col_`zona'_otrasactv.dta", replace

*otros ingresos
use "`m10'\`zona' - Otros ingresos.dta", clear
append using "`m11'\`zona' - Otros ingresos.dta"
append using "`m12'\`zona' - Otros ingresos.dta"
egen id = concat(DIRECTORIO SECUENCIA_P ORDEN)
rename clase CLASE
sort id
saveold "`t4'col_`zona'_otrosing.dta", replace
 
*Vivienda y Hogares
use "`m10'\`zona' - Vivienda y Hogares.dta", clear
append using "`m11'\`zona' - Vivienda y Hogares.dta"
append using "`m12'\`zona' - Vivienda y Hogares.dta"
egen idh = concat(DIRECTORIO SECUENCIA_P)
rename clase CLASE
sort idh
saveold "`t4'col_`zona'_viv.dta", replace
}


** Módulo de migración 

* Sección incluida por SCL/MIG Fernando Morales 

use "`m10'\Octubre_mig.dta", clear
append using "`m11'\Noviembre_mig.dta"
append using "`m12'\Diciembre_mig.dta"

ren (Mes Directorio Secuencia_p Orden Fex_c_2011) (MES DIRECTORIO SECUENCIA_P ORDEN fex_c_2011)
egen id = concat(DIRECTORIO SECUENCIA_P ORDEN)
sort id
saveold "`out'\COL_`anio't4migracion.dta", replace


*3. Merge de los 8 modulos trimestrales por zona
*-----------------------------------------------
foreach zona in cabecera resto {
use `t4'col_`zona'_personas.dta, clear
merge 1:1 id using `t4'col_`zona'_desocupados.dta  
drop _merge
sort id

merge 1:1 id using `t4'col_`zona'_ft.dta 
drop _merge
sort id

merge 1:1 id using `t4'col_`zona'_inactivos.dta
drop _merge
sort id

merge 1:1 id using `t4'col_`zona'_ocupados.dta
drop _merge
sort id

merge 1:1 id using `t4'col_`zona'_otrasactv.dta
drop _merge
sort id

merge 1:1 id using `t4'col_`zona'_otrosing.dta
drop _merge
egen idh = concat(DIRECTORIO SECUENCIA_P)
sort idh

merge m:1 idh using `t4'col_`zona'_viv.dta
drop _merge 
sort id
saveold "`out'COL_`anio't4`zona'.dta", replace
}

*4. Append zonas
*---------------

clear
use "${surveysFolder}\survey\COL\GEIH\2020\t4\data_merge\COL_2020t4cabecera.dta", clear
append using "${surveysFolder}\survey\COL\GEIH\2020\t4\data_merge\COL_2020t4resto.dta" 
merge 1:1 id using "${surveysFolder}\survey\COL\GEIH\2020\t4\data_merge\COL_2020t4migracion.dta", nogen
replace fex_c_2011=fex_c_2011/3
sort id

merge 1:1 id using "${surveysFolder}\survey\COL\GEIH\2020\a\data_merge\pov_t4.dta"
drop _merge

foreach v of varlist _all {
	local lowname=lower("`v'")
	rename `v' `lowname'
}

saveold "${surveysFolder}\survey\COL\GEIH\2020\t4\data_merge\COL_2020t4.dta", replace
















