* MERGE
* ARGENTINA EPH. CONTINUA. 1erSem. 2012

*global ruta = "${surveysFolder}"

local log_file = "$ruta\harmonized\ARG\EPHC\log\ARG_2012s1_mergeBID.log"

*bases originales (I trim.)
local base_in_t1  = "$ruta\survey\ARG\EPHC\2012\t1\data_orig"

*bases originales (II trim.)
local base_in_t2  = "$ruta\survey\ARG\EPHC\2012\t2\data_orig"


local base_out_t1 = "$ruta\survey\ARG\EPHC\2012\t1\data_merge\ARG_2012t1.dta"
local base_out_t2 = "$ruta\survey\ARG\EPHC\2012\t2\data_merge\ARG_2012t2.dta"

local base_out = "$ruta\survey\ARG\EPHC\2012\s1\data_merge\ARG_2012s1.dta"

capture log close
log using "`log_file'", replace 

log off

clear
set mem 300m
set more off
* Abre base agregada de hogares 

* base primer trimestre *
use "`base_in_t1'\Individual_t112.dta", replace
sort CODUSU nro_hogar aglomerado
save, replace

use "`base_in_t1'\Hogar_t112.dta", replace
sort CODUSU nro_hogar aglomerado

merge CODUSU nro_hogar aglomerado using "`base_in_t1'\Individual_t112.dta"

tab _merge

saveold "`base_out_t1'", replace
clear

* base segundo trimestre *
use "`base_in_t2'\Individual_t212.dta", replace
sort CODUSU nro_hogar aglomerado
save, replace

use "`base_in_t2'\Hogar_t212.dta", replace
sort CODUSU nro_hogar aglomerado

merge CODUSU nro_hogar aglomerado using "`base_in_t2'\Individual_t212.dta"

tab _merge

saveold "`base_out_t2'", replace

more
append using "`base_out_t1'"

* Arma ponderador semestral
replace pondera=pondera/2
replace pondera=round(pondera)

rename CODUSU codusu

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
