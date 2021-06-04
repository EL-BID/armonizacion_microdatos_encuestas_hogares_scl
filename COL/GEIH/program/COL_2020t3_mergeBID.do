/*
Autor: Nathalia Maya S
Ultima version: Angela Lopez(alop@iadb.org)
Junio, 2021 Version Stata 16 */

*** MERGE COLOMBIA GEIH 2020 ****
*------------------------------*	

clear
set more off
local anio =2020
local ronda1 a
local ronda2 t3	
local ruta "${surveysFolder}\survey\COL\GEIH\\`anio'\"
local m7 ="`ruta'\`ronda1'\data_orig\m7\" 
local m8 ="`ruta'\`ronda1'\data_orig\m8\" 
local m9 ="`ruta'\`ronda1'\data_orig\m9\" 
local t3 ="`ruta'\`ronda2'\data_orig\"
local out ="`ruta'\`ronda2'\data_merge\"


*1. Bases anuales con homologacion de ingresos:
* http://microdatos.dane.gov.co/index.php/catalog/708/get_microdata
*----------------------------------------------

clear
use "`ruta'\`ronda1'\data_orig\anual_homologado_DANE\Hogares.dta", clear
*La base de hogares tiene duplicados de los meses 3,4 y 5, sin embargo, se borran ya que corresponden a meses que no hacen parte de este trimestre
duplicates tag directorio secuencia_p , gen(dup)
drop if dup==1
drop dup

merge 1:m directorio secuencia_p using "`ruta'\`ronda1'\data_orig\anual_homologado_DANE\Personas.dta", force
drop if _merge==2 //meses que no se usarán
drop _merge
egen id =concat (directorio secuencia_p orden)
sort id
save "`ruta'\`ronda1'\data_merge\pov_anual.dta", replace
destring mes, replace
keep if mes>=7 & mes<=9

keep  impaes- id impa-iof6 nper-fex_c
save "`ruta'\`ronda1'\data_merge\pov_t3.dta", replace


*2. Append entre meses
*------------------------

foreach zona in Cabecera Resto {

		*Personas
		use "`m7'\`zona' - Características generales (Personas).dta", clear
		rename *, upper /* son minúsculas solamente en m7 */
		rename FEX_C_2011 fex_c_2011
		append using "`m8'\`zona' - Características generales (Personas).dta", nolabel
		append using "`m9'\`zona' - Características generales (Personas).dta", nolabel
		egen id = concat(DIRECTORIO SECUENCIA_P ORDEN)
		sort id
		save "`t3'col_`zona'_personas.dta", replace

		*Desocupados
		use "`m7'\`zona' - Desocupados.dta", clear
		rename *, upper /* son minúsculas solamente en m7 */
		rename FEX_C_2011 fex_c_2011
		append using "`m8'\`zona' - Desocupados.dta", nolabel
		append using "`m9'\`zona' - Desocupados.dta", nolabel
		egen id = concat(DIRECTORIO SECUENCIA_P ORDEN)		
		sort id
		save "`t3'col_`zona'_desocupados.dta", replace
						
		*Fuerza Trabajo
		use "`m7'\`zona' - Fuerza de trabajo.dta", clear
		rename *, upper /* son minúsculas solamente en m7 */
		rename FEX_C_2011 fex_c_2011
		append using "`m8'\`zona' - Fuerza de trabajo.dta", nolabel
		append using "`m9'\`zona' - Fuerza de trabajo.dta", nolabel
		egen id = concat(DIRECTORIO SECUENCIA_P ORDEN)
		sort id
		save "`t3'col_`zona'_ft.dta", replace

		*Inactivos
		use "`m7'\`zona' - Inactivos.dta", clear
		rename *, upper /* son minúsculas solamente en m7 */
		rename FEX_C_2011 fex_c_2011
		rename P7440 p7440 /* esta es minúscula en m8 y m9 */
		append using "`m8'\`zona' - Inactivos.dta", nolabel
		append using "`m9'\`zona' - Inactivos.dta", nolabel
		egen id = concat(DIRECTORIO SECUENCIA_P ORDEN)
		sort id
		save "`t3'col_`zona'_inactivos.dta", replace

		*ocupados
		use "`m7'\`zona' - Ocupados.dta", clear
		rename *, upper /* son minúsculas solamente en m7 */
		rename FEX_C_2011 fex_c_2011
		append using "`m8'\`zona' - Ocupados.dta", nolabel
		append using "`m9'\`zona' - Ocupados.dta", nolabel
		egen id = concat(DIRECTORIO SECUENCIA_P ORDEN)
		sort id
		save "`t3'col_`zona'_ocupados.dta", replace

		*otrasactv
		use "`m7'\Resto - Otras actividades y ayudas en la semana.dta", clear
		rename *, upper /* son minúsculas solamente en m7 */
		rename FEX_C_2011 fex_c_2011
		append using "`m8'\`zona' - Otras actividades y ayudas en la semana.dta", nolabel
		append using "`m9'\`zona' - Otras actividades y ayudas en la semana.dta", nolabel
		egen id = concat(DIRECTORIO SECUENCIA_P ORDEN)
		sort id
		save "`t3'col_`zona'_otrasactv.dta", replace

		*otros ingresos
		use "`m7'\Resto - Otros ingresos.dta", clear
		rename *, upper /* son minúsculas solamente en m7 */
		rename FEX_C_2011 fex_c_2011
		append using "`m8'\`zona' - Otros ingresos.dta", nolabel
		append using "`m9'\`zona' - Otros ingresos.dta", nolabel
		egen id = concat(DIRECTORIO SECUENCIA_P ORDEN)
		sort id
		save "`t3'col_`zona'_otrosing.dta", replace
		 
		*Vivienda y Hogares
		use "`m7'\Resto - Vivienda y Hogares.dta", clear
		rename *, upper /* son minúsculas solamente en m7 */
		rename FEX_C_2011 fex_c_2011
		append using "`m8'\`zona' - Vivienda y Hogares.dta", nolabel
		append using "`m9'\`zona' - Vivienda y Hogares.dta", nolabel
		egen idh = concat(DIRECTORIO SECUENCIA_P)
		sort idh
		save "`t3'col_`zona'_viv.dta", replace
}



** Módulo de migración 
* Sección incluida por SCL/MIG Fernando Morales 
* http://microdatos.dane.gov.co/index.php/catalog/662/get_microdata

use "`m7'\Julio_mig.dta", clear
append using "`m8'\Agosto_mig.dta"
append using "`m9'\Septiembre_mig.dta"

ren (Mes Directorio Secuencia_p Orden Fex_c_2011) (MES DIRECTORIO SECUENCIA_P ORDEN fex_c_2011)
egen id = concat(DIRECTORIO SECUENCIA_P ORDEN)
sort id

save "`out'\COL_`anio't3migracion.dta", replace

*3. Merge de los 8 modulos trimestrales por zona
*-----------------------------------------------
foreach zona in cabecera resto {
		use `t3'col_`zona'_personas.dta, clear
		merge 1:1 id using `t3'col_`zona'_desocupados.dta, keep(match master)  
		drop _merge
		* sort id

		merge 1:1 id using `t3'col_`zona'_ft.dta, keep(match master) 
		drop _merge
		* sort id

		merge 1:1 id using `t3'col_`zona'_inactivos.dta, keep(match master)
		drop _merge
		* sort id

		merge 1:1 id using `t3'col_`zona'_ocupados.dta, keep(match master)
		drop _merge
		* sort id

		merge 1:1 id using `t3'col_`zona'_otrasactv.dta, keep(match master)
		drop _merge
		* sort id

		merge 1:1 id using `t3'col_`zona'_otrosing.dta, keep(match master)
		drop _merge
		egen idh = concat(DIRECTORIO SECUENCIA_P)
		* sort idh

		merge m:1 idh using `t3'col_`zona'_viv.dta, keep(match master)
		drop _merge 
		sort id
		save "`out'COL_`anio't3`zona'.dta", replace
		}

*4. Append zonas
*---------------

clear
use "${surveysFolder}\survey\COL\GEIH\2020\t3\data_merge\COL_2020t3cabecera.dta", clear
append using "${surveysFolder}\survey\COL\GEIH\2020\t3\data_merge\COL_2020t3resto.dta", nolabel 
merge 1:1 id using "${surveysFolder}\survey\COL\GEIH\2020\t3\data_merge\COL_2020t3migracion.dta", nogen
replace fex_c_2011=fex_c_2011/3
sort id

rename *, lower /* poverty data is lowercase and final output is planned to be lowercase too */

merge 1:1 id using "${surveysFolder}\survey\COL\GEIH\2020\a\data_merge\pov_t3.dta", keep(match master)
drop _merge



save "${surveysFolder}\survey\COL\GEIH\2020\t3\data_merge\COL_2020t3.dta", replace























