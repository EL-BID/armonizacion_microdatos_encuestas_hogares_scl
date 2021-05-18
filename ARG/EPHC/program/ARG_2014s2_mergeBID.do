* MERGE
* ARGENTINA EPH. CONTINUA. 2oSem. 2014

global ruta = "${surveysFolder}"

local log_file = "$ruta\harmonized\ARG\EPHC\log\ARG_2014s2_mergeBID.log"

*bases originales (III trim.)
local base_in_t3  = "$ruta\survey\ARG\EPHC\2014\t3\data_orig"

*bases originales (IV trim.)
local base_in_t4  = "$ruta\survey\ARG\EPHC\2014\t4\data_orig"


local base_out_t3 = "$ruta\survey\ARG\EPHC\2014\t3\data_merge\ARG_2014t3.dta"
local base_out_t4 = "$ruta\survey\ARG\EPHC\2014\t4\data_merge\ARG_2014t4.dta"

local base_out = "$ruta\survey\ARG\EPHC\2014\s2\data_merge\ARG_2014s2.dta"

capture log close
log using "`log_file'", replace 

log off

clear
set mem 300m
set more off

* Abre base agregada de hogares 

* base primer trimestre *
use "`base_in_t3'\Individual_t314.dta", replace
sort CODUSU nro_hogar aglomerado
save, replace

use "`base_in_t3'\Hogar_t314.dta", replace
sort CODUSU nro_hogar aglomerado

merge CODUSU nro_hogar aglomerado using "`base_in_t3'\Individual_t314.dta"

tab _merge

saveold "`base_out_t3'", replace
clear

* base segundo trimestre *
use "`base_in_t4'\Individual_t414.dta", replace
sort CODUSU nro_hogar aglomerado
save, replace

use "`base_in_t4'\Hogar_t414.dta", replace
sort CODUSU nro_hogar aglomerado

merge CODUSU nro_hogar aglomerado using "`base_in_t4'\Individual_t414.dta"

tab _merge

saveold "`base_out_t4'", replace

more
append using "`base_out_t3'"

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
