/*
Autor: Marcela G. Rubio - Email: marcelarubio28@gmail.com | mrubio@iadb.org
Ultima version: Stephanie González Rubio (stephaniego@iadb.org)
Junio, 2018 Version Stata 14 */
* Added ETNIA module. Cesar Lins (SCL/GDI) Marzo 2021

*** MERGE COLOMBIA GEIH 2017 ****
*------------------------------*	

clear
set more off
local anio =2017
local ronda1 a
local ronda2 t3	
local ruta "\\sdssrv03\Surveys\survey\COL\GEIH\\`anio'\"
local m7 ="`ruta'\`ronda1'\data_orig\m7\" 
local m8 ="`ruta'\`ronda1'\data_orig\m8\" 
local m9 ="`ruta'\`ronda1'\data_orig\m9\" 
local t3 ="`ruta'\`ronda2'\data_orig\"
local out ="`ruta'\`ronda2'\data_merge\"


*1. Bases anuales con homologacion de ingresos:
*----------------------------------------------

clear
use "`ruta'\`ronda1'\data_orig\anual_homologado_DANE\PERSONAS.dta", clear
merge m:1 Directorio Secuencia_p using "`ruta'\`ronda1'\data_orig\anual_homologado_DANE\HOGARES 2017.dta", force
drop _merge
merge 1:1 directorio secuencia_p orden using "`ruta'\`ronda1'\data_orig\anual_homologado_DANE\ETNIA17.dta"
drop _merge

egen id =concat (Directorio Secuencia_p Orden)
sort id
saveold "`ruta'\`ronda1'\data_merge\pov_anual.dta", replace
destring Mes, replace
keep if Mes>=7 & Mes<=9

keep  id Impa-Iof6 Impaes-Fex_c Nper-id
saveold "`ruta'\`ronda1'\data_merge\pov_t3.dta", replace


*2. Append entre meses
*------------------------

foreach zona in cabecera resto {

*Personas
use "`m7'\`zona'_caracteristicas_generales_personas.dta", clear
append using "`m8'\`zona'_caracteristicas_generales_personas.dta"
append using "`m9'\`zona'_caracteristicas_generales_personas.dta"
egen id = concat(directorio secuencia_p orden)
sort id
saveold "`t3'col_`zona'_personas.dta", replace

*Desocupados
use "`m7'\`zona'_desocupados.dta", clear
append using "`m8'\`zona'_desocupados.dta"
append using "`m9'\`zona'_desocupados.dta"
egen id = concat(directorio secuencia_p orden)
sort id
saveold "`t3'col_`zona'_desocupados.dta", replace
				
*Fuerza Trabajo
use "`m7'\`zona'_fuerza_de_trabajo.dta", clear
append using "`m8'\`zona'_fuerza_de_trabajo.dta"
append using "`m9'\`zona'_fuerza_de_trabajo.dta"
egen id = concat(directorio secuencia_p orden)
sort id
saveold "`t3'col_`zona'_ft.dta", replace

*Inactivos
use "`m7'\`zona'_inactivos.dta", clear
append using "`m8'\`zona'_inactivos.dta"
append using "`m9'\`zona'_inactivos.dta"
egen id = concat(directorio secuencia_p orden)
sort id
saveold "`t3'col_`zona'_inactivos.dta", replace

*ocupados
use "`m7'\`zona'_ocupados.dta", clear
append using "`m8'\`zona'_ocupados.dta"
append using "`m9'\`zona'_ocupados.dta"
egen id = concat(directorio secuencia_p orden)
sort id
saveold "`t3'col_`zona'_ocupados.dta", replace

*otrasactv
use "`m7'\`zona'_otras_actividades_y_ayudas_en_la_semana.dta", clear
append using "`m8'\`zona'_otras_actividades_y_ayudas_en_la_semana.dta"
append using "`m9'\`zona'_otras_actividades_y_ayudas_en_la_semana.dta"
egen id = concat(directorio secuencia_p orden)
sort id
saveold "`t3'col_`zona'_otrasactv.dta", replace

*otros ingresos
use "`m7'\`zona'_otros_ingresos.dta", clear
append using "`m8'\`zona'_otros_ingresos.dta"
append using "`m9'\`zona'_otros_ingresos.dta"
egen id = concat(directorio secuencia_p orden)
sort id
saveold "`t3'col_`zona'_otrosing.dta", replace
 
*Vivienda y Hogares
use "`m7'\`zona'_vivienda_y_hogares.dta", clear
append using "`m8'\`zona'_vivienda_y_hogares.dta"
append using "`m9'\`zona'_vivienda_y_hogares.dta"
egen idh = concat(directorio secuencia_p)
sort idh
saveold "`t3'col_`zona'_viv.dta", replace
}
*/
*3. Merge de los 8 modulos trimestrales por zona
*-----------------------------------------------
foreach zona in cabecera resto {
use `t3'col_`zona'_personas.dta, clear
merge 1:1 id using `t3'col_`zona'_desocupados.dta  
drop _merge
sort id

merge 1:1 id using `t3'col_`zona'_ft.dta 
drop _merge
sort id

merge 1:1 id using `t3'col_`zona'_inactivos.dta
drop _merge
sort id

merge 1:1 id using `t3'col_`zona'_ocupados.dta
drop _merge
sort id

merge 1:1 id using `t3'col_`zona'_otrasactv.dta
drop _merge
sort id

merge 1:1 id using `t3'col_`zona'_otrosing.dta
drop _merge
egen idh = concat(directorio secuencia_p)
sort idh

merge m:1 idh using `t3'col_`zona'_viv.dta
drop _merge 
sort id
saveold "`out'COL_`anio't3`zona'.dta", replace
}

*4. Append zonas
*---------------

clear
use "Z:\survey\COL\GEIH\2017\t3\data_merge\COL_2017t3cabecera.dta", clear
append using "Z:\survey\COL\GEIH\2017\t3\data_merge\COL_2017t3resto.dta" 
replace fex_c_2011=fex_c_2011/3
sort id

merge 1:1 id using "Z:\survey\COL\GEIH\2017\a\data_merge\pov_t3.dta"
drop _merge

saveold "Z:\survey\COL\GEIH\2017\t3\data_merge\COL_2017t3.dta", replace























