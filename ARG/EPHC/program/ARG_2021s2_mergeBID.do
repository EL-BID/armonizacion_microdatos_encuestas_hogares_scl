* MERGE
* ARGENTINA EPH. CONTINUA. 2oSem. 2021

clear
set more off

global ruta = "${surveysFolder}"
local ano 21
local trims t3 t4 
local modulos hogar individual

* Abre base agregada de hogares 

* Bases originales 

foreach modulo of local modulos {

		import delimited "$ruta\survey\ARG\EPHC\20`ano'\t3\data_orig\usu_`modulo'_T3`ano'.txt", encoding(ISO-8859-2) clear 
		save "$ruta\survey\ARG\EPHC\20`ano'\t3\data_orig\usu_`modulo'_t320`ano'.dta", replace

		import delimited "$ruta\survey\ARG\EPHC\20`ano'\t4\data_orig\usu_`modulo'_T4`ano'.txt", encoding(ISO-8859-2) clear 
		save "$ruta\survey\ARG\EPHC\20`ano'\t4\data_orig\usu_`modulo'_t420`ano'.dta", replace
}


* Modulos trimestres
foreach trim of local trims {

		local base_in  = "$ruta\survey\ARG\EPHC\20`ano'\\`trim'\data_orig"
		local base_out = "$ruta\survey\ARG\EPHC\20`ano'\\`trim'\data_merge"
	
		use "`base_in'\usu_individual_`trim'20`ano'.dta", replace
		sort codusu nro_hogar aglomerado
		save, replace

		use "`base_in'\usu_hogar_`trim'20`ano'.dta", replace
		sort codusu nro_hogar aglomerado

		merge codusu nro_hogar aglomerado using "`base_in'\usu_individual_`trim'20`ano'.dta"

		tab _merge

		save "`base_out'\ARG_20`ano'`trim'.dta", replace
		clear
}

use "$ruta\survey\ARG\EPHC\20`ano'\t3\data_merge\ARG_20`ano't3.dta"
append using "$ruta\survey\ARG\EPHC\20`ano'\t4\data_merge\ARG_20`ano't4.dta", force

* Arma ponderador semestral
replace pondera=pondera/2
replace pondera=round(pondera)

rename codusu codusu

* Elimina observaciones inconsistentes 
drop if _merge==1

more
drop _merge

* Importante, comparo que la cantidad de individuos creada por mi sea igual a la del INDEC
sort codusu nro_hogar
capture drop id
egen id = group(codusu nro_hogar trimestre)  

gen uno = 1
egen miembros = sum(uno) if ch03 != ., by(id)
replace miembros = 0 if miembros == .

compare miembros ix_tot
more

drop id uno miembros

* Comprime y guarda base
compress
save "$ruta\survey\ARG\EPHC\20`ano'\s2\data_merge\ARG_20`ano's2.dta", replace


