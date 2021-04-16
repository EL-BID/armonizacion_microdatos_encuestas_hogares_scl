* MERGE
* ARGENTINA EPH. CONTINUA. Primeros tres trimestres de 2021
*Ultima actualización: Alvaro Altamirano, Febrero de 2021
clear all
set more off

global ruta = "${surveysFolder}"

local log_file = "$ruta\harmonized\ARG\EPHC\log\ARG_2021t1_t3_mergeBID.log"

*bases originales (I trim.)
local base_in_t1  = "$ruta\survey\ARG\EPHC\2020\t1\data_orig"
*bases originales (II trim.)
local base_in_t2  = "$ruta\survey\ARG\EPHC\2020\t2\data_orig"
*bases originales (III trim.)
local base_in_t3  = "$ruta\survey\ARG\EPHC\2020\t3\data_orig"
*Local de salida
local base_out_t1 = "$ruta\survey\ARG\EPHC\2020\t1\data_merge\ARG_2020t1.dta"
local base_out_t2 = "$ruta\survey\ARG\EPHC\2020\t2\data_merge\ARG_2020t2.dta"
local base_out_t3 = "$ruta\survey\ARG\EPHC\2020\t3\data_merge\ARG_2020t3.dta"

local base_out = "$ruta\survey\ARG\EPHC\2020\s1\data_merge\ARG_2020t1_t3.dta"

capture log close
log using "`log_file'", replace 

log off

clear
set more off
* Abre base agregada de hogares 

* base primer trimestre *
import delimited "`base_in_t1'\usu_individual_T120.txt", delimiter(";") clear 
sort codusu nro_hogar aglomerado
save "`base_in_t1'\usu_individual_T120.dta", replace

import delimited "`base_in_t1'\usu_hogar_T120.txt", delimiter(";") clear 
sort codusu nro_hogar aglomerado

merge codusu nro_hogar aglomerado using "`base_in_t1'\usu_individual_T120.dta"

tab _merge

save "`base_out_t1'", replace
clear

* base segundo trimestre *
import delimited "`base_in_t2'\usu_individual_T220.txt", delimiter(";") clear 
sort codusu nro_hogar aglomerado
save "`base_in_t2'\usu_individual_T220.dta", replace

import delimited "`base_in_t2'\usu_hogar_T220.txt", delimiter(";") clear 
sort codusu nro_hogar aglomerado

merge codusu nro_hogar aglomerado using "`base_in_t2'\usu_individual_T220.dta"

tab _merge

save "`base_out_t2'", replace

more
append using "`base_out_t2'"


* base tercer trimestre *
import delimited "`base_in_t3'\usu_individual_T320.txt", delimiter(";") clear 
sort codusu nro_hogar aglomerado
save "`base_in_t3'\usu_individual_T320.dta", replace

import delimited "`base_in_t3'\usu_hogar_T320.txt", delimiter(";") clear 
sort codusu nro_hogar aglomerado

merge codusu nro_hogar aglomerado using "`base_in_t3'\usu_individual_T320.dta"

tab _merge

save "`base_out_t3'", replace


*Carga bases y arma ponderador para los tres trimestes
use "$ruta\survey\ARG\EPHC\2020\t1\data_merge\ARG_2020t1.dta", clear

append using "$ruta\survey\ARG\EPHC\2020\t2\data_merge\ARG_2020t2.dta", force
append using "$ruta\survey\ARG\EPHC\2020\t3\data_merge\ARG_2020t3.dta", force

replace pondera=pondera/3
replace pondera=round(pondera)

rename codusu codusu

* Elimina observaciones inconsistentes 
drop if _merge==1

more
drop _merge

*Importante, comparo que la cantidad de individuos creada por mi sea igual a la del INDEC
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
saveold "${surveysFolder}\survey\ARG\EPHC\2020\t1_t3\data_merge\ARG_2020t1_t3.dta"

log close
