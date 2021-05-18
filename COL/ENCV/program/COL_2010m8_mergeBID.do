*Elaboración: Daniela Zuluaga (da.zuluaga@hotmail.com | danielazu@iadb.org)
*Febrero, 2019

*ENCV 2010
clear
clear all
clear matrix
set more off
set matsize 2000

global ruta = "${surveysFolder}"

local PAIS COL
local ENCUESTA ENCV
local ANO "2010"
local ronda m8

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\\STATA"
local base_out = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"




*** MERGE COLOMBIA ENCV 2010 ****
*------------------------------*	

**Viviendas**
use "`base_in'\dbfp_encv_545_1.dta", clear
mmerge periodo directorio secuencia_encuesta using "${surveysFolder}\survey\COL\ENCV\2010\m8\data_orig\STATA\dbfp_encv_545_2.dta", t(1:1)
drop _merge 
sort periodo directorio secuencia_encuesta

**Hogares**
mmerge periodo directorio secuencia_encuesta using "${surveysFolder}\survey\COL\ENCV\2010\m8\data_orig\STATA\dbfp_encv_546_1.dta", t(1:1)
drop _merge 
sort periodo directorio secuencia_encuesta

mmerge periodo directorio secuencia_encuesta using "${surveysFolder}\survey\COL\ENCV\2010\m8\data_orig\STATA\dbfp_encv_546_2.dta", t(1:1)
drop _merge 
sort periodo directorio secuencia_encuesta

mmerge periodo directorio secuencia_encuesta using "${surveysFolder}\survey\COL\ENCV\2010\m8\data_orig\STATA\dbfp_encv_546_3.dta", t(1:1)
drop _merge 
sort periodo directorio secuencia_encuesta

mmerge periodo directorio secuencia_encuesta using "${surveysFolder}\survey\COL\ENCV\2010\m8\data_orig\STATA\dbfp_encv_546_4.dta", t(1:1)
drop _merge 
sort  periodo directorio secuencia_encuesta

mmerge periodo directorio secuencia_encuesta using "${surveysFolder}\survey\COL\ENCV\2010\m8\data_orig\STATA\dbfp_encv_546_5.dta", t(1:1)
drop _merge 
sort  periodo directorio secuencia_encuesta

mmerge periodo directorio secuencia_encuesta using "${surveysFolder}\survey\COL\ENCV\2010\m8\data_orig\STATA\dbfp_encv_546_6.dta", t(1:1)
drop _merge 
sort  periodo directorio secuencia_encuesta

mmerge periodo directorio secuencia_encuesta using "${surveysFolder}\survey\COL\ENCV\2010\m8\data_orig\STATA\dbfp_encv_546_7.dta", t(1:1)
drop _merge 
sort  periodo directorio secuencia_encuesta

mmerge periodo directorio secuencia_encuesta using "${surveysFolder}\survey\COL\ENCV\2010\m8\data_orig\STATA\dbfp_encv_546_8.dta", t(1:1)
drop _merge 
sort periodo directorio secuencia_encuesta

mmerge periodo directorio secuencia_encuesta using "${surveysFolder}\survey\COL\ENCV\2010\m8\data_orig\STATA\dbfp_encv_546_9.dta", t(1:1)
drop _merge 
sort periodo directorio secuencia_encuesta

***Se genera llave**
egen llave_h=concat(periodo directorio secuencia_encuesta)

**Se guarda temporalmente**
tempfile hogares
save `hogares' , replace

**Personas**
use "${surveysFolder}\survey\COL\ENCV\2010\m8\data_orig\STATA\dbfp_encv_547_1.dta", clear

mmerge periodo directorio secuencia_encuesta secuencia_p using "${surveysFolder}\survey\COL\ENCV\2010\m8\data_orig\STATA\dbfp_encv_547_2.dta", t(1:1)
drop _merge 
sort periodo directorio secuencia_encuesta secuencia_p

mmerge periodo directorio secuencia_encuesta secuencia_p using "${surveysFolder}\survey\COL\ENCV\2010\m8\data_orig\STATA\dbfp_encv_547_3.dta", t(1:1)
drop _merge 
sort periodo directorio secuencia_encuesta secuencia_p

mmerge periodo directorio secuencia_encuesta secuencia_p using "${surveysFolder}\survey\COL\ENCV\2010\m8\data_orig\STATA\dbfp_encv_547_4.dta", t(1:1)
drop _merge 
sort periodo directorio secuencia_encuesta secuencia_p

mmerge periodo directorio secuencia_encuesta secuencia_p using "${surveysFolder}\survey\COL\ENCV\2010\m8\data_orig\STATA\dbfp_encv_547_5.dta", t(1:1)
drop _merge 
sort periodo directorio secuencia_encuesta secuencia_p


***Se genera llave**
egen llave_h=concat(periodo directorio secuencia_p)

gen orden_persona=orden

**Se guarda temporalmente**
tempfile personas
save `personas' , replace


**Se juntan hogares y personas 

use `hogares', clear
mmerge llave_h using `personas', t(1:n)


* comprime y guarda base
compress
saveold "`base_out'", replace











