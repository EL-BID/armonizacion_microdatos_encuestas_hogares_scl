*Elaboración: Daniela Zuluaga (da.zuluaga@hotmail.com | danielazu@iadb.org)
*Febrero, 2019

*ENCV 2017
clear
clear all
clear matrix
set more off
set matsize 2000

global ruta = "${surveysFolder}"

local PAIS COL
local ENCUESTA ENCV
local ANO "2017"
local ronda m10_m11

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig"
local base_out = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"




*** MERGE COLOMBIA ENCV 2017 ****
*------------------------------*	

**Viviendas**
use "`base_in'\Datos de la vivienda", clear


**Hogares**
mmerge DIRECTORIO SECUENCIA_ENCUESTA using "${surveysFolder}\survey\COL\ENCV\2017\m10_m11\data_orig\Servicios del hogar", t(1:1)
drop _merge 
sort DIRECTORIO SECUENCIA_ENCUESTA

mmerge DIRECTORIO SECUENCIA_ENCUESTA using "${surveysFolder}\survey\COL\ENCV\2017\m10_m11\data_orig\Tenencia y financiación de la vivienda que ocupa el hogar.dta", t(1:1)
drop _merge 
sort DIRECTORIO SECUENCIA_ENCUESTA

mmerge DIRECTORIO SECUENCIA_ENCUESTA using "${surveysFolder}\survey\COL\ENCV\2017\m10_m11\data_orig\Condiciones de vida del hogar y tenencia de bienes (programas).dta", t(1:1)
drop _merge 
sort DIRECTORIO SECUENCIA_ENCUESTA

mmerge DIRECTORIO SECUENCIA_ENCUESTA using "${surveysFolder}\survey\COL\ENCV\2017\m10_m11\data_orig\Condiciones de vida del hogar y tenencia de bienes", t(1:1)
drop _merge 
sort  DIRECTORIO SECUENCIA_ENCUESTA

mmerge DIRECTORIO SECUENCIA_ENCUESTA using "${surveysFolder}\survey\COL\ENCV\2017\m10_m11\data_orig\Seguridad alimentaria.dta", t(1:1)
drop _merge 
sort  DIRECTORIO SECUENCIA_ENCUESTA


***Se genera llave**
egen llave_h=concat(DIRECTORIO SECUENCIA_ENCUESTA)

**Se guarda temporalmente**
tempfile hogares
save `hogares' , replace

**Personas**
use "${surveysFolder}\survey\COL\ENCV\2017\m10_m11\data_orig\Caracteristicas y composicion del hogar.dta", clear

mmerge LLAVEHOG ORDEN using "${surveysFolder}\survey\COL\ENCV\2017\m10_m11\data_orig\Salud.dta", t(1:1)
drop _merge 

mmerge LLAVEHOG ORDEN  using "${surveysFolder}\survey\COL\ENCV\2017\m10_m11\data_orig\Atención integral de los niños y niñas menores de 5 años.dta", t(1:1)
drop _merge 


mmerge LLAVEHOG ORDEN using "${surveysFolder}\survey\COL\ENCV\2017\m10_m11\data_orig\Educación", t(1:1)
drop _merge 


mmerge LLAVEHOG ORDEN  using "${surveysFolder}\survey\COL\ENCV\2017\m10_m11\data_orig\Fuerza de trabajo", t(1:1)
drop _merge 
 

mmerge LLAVEHOG ORDEN using "${surveysFolder}\survey\COL\ENCV\2017\m10_m11\data_orig\Tecnologías de información y comunicación", t(1:1)
drop _merge 


mmerge LLAVEHOG ORDEN using "${surveysFolder}\survey\COL\ENCV\2017\m10_m11\data_orig\Trabajo infantil", t(1:1)
drop _merge 

**Llave_h*

gen llave_h=LLAVEHOG
gen orden_persona=ORDEN

**Se guarda temporalmente**
tempfile personas
save `personas' , replace


**Se juntan hogares y personas 

use `hogares', clear
mmerge llave_h using `personas', t(1:n)

  foreach v of varlist _all {
      capture rename `v' `=lower("`v'")'
   }
   
* comprime y guarda base
compress
saveold "`base_out'", replace











