* MERGE
* ARGENTINA EPH. CONTINUA. 2oSem. 2019

global ruta = "\\Sdssrv03\surveys"

local log_file = "$ruta\harmonized\ARG\EPHC\log\ARG_2019s2_mergeBID.log"

*bases originales (I trim.)
local base_in_t3  = "$ruta\survey\ARG\EPHC\2019\t3\data_orig"

*bases originales (II trim.)
local base_in_t4  = "$ruta\survey\ARG\EPHC\2019\t4\data_orig"


local base_out_t3 = "$ruta\survey\ARG\EPHC\2019\t3\data_merge\ARG_2019t3.dta"
local base_out_t4 = "$ruta\survey\ARG\EPHC\2019\t4\data_merge\ARG_2019t4.dta"

local base_out = "$ruta\survey\ARG\EPHC\2019\s2\data_merge\ARG_2019s2.dta"

capture log close
log using "`log_file'", replace 

log off

clear
set more off
* Abre base agregada de hogares 

* base primer trimestre *
use "`base_in_t3'\usu_individual_t319.dta", replace
sort codusu nro_hogar aglomerado
save, replace

use "`base_in_t3'\usu_hogar_t319.dta", replace
sort codusu nro_hogar aglomerado

merge codusu nro_hogar aglomerado using "`base_in_t3'\usu_individual_t319.dta"

tab _merge

save "`base_out_t3'", replace
clear

* base segundo trimestre *
use "`base_in_t4'\usu_individual_t419.dta", replace
sort codusu nro_hogar aglomerado
save, replace

use "`base_in_t4'\usu_hogar_t419.dta", replace
sort codusu nro_hogar aglomerado

merge codusu nro_hogar aglomerado using "`base_in_t4'\usu_individual_t419.dta"

tab _merge

save "`base_out_t4'", replace

more
append using "`base_out_t3'"

*Arma ponderador semestral
replace pondera=pondera/2
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
saveold "`base_out'", replace

log close
