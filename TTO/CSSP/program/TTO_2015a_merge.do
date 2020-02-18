*Elaboración: Mayra Sáenz
*Marzo,2017
global ruta = "\\Sdssrv03\surveys"

local PAIS TTO
local ENCUESTA CSSP
local ANO "2015"
local ronda a

*local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in  = "$ruta\\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\\data_orig"
local base_out = "$ruta\\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\\data_merge\\`PAIS'_`ANO'`ronda'.dta"

*** Base a nivel de hogar
use "`base_in'\Households.dta", clear

* Hay duplicados en base a nivel de hogar

egen id_hog = group(cward ed hhnum quarter)
sort id_hog
duplicates drop id_hog, force
drop id_hog

egen idh_ch= group(cward ed hhnum quarter)
sort idh_ch


saveold "`base_in'\Household_2014.dta", replace


*** Base a nivel de individuos
use "`base_in'\Individuals.dta", clear

* Hay duplicados en base a nivel individual
egen id_indiv = group(cward ed hhnum indivno quarter)

duplicates drop id_indiv, force
drop id_indiv

egen idh_ch = group(cward ed hhnum quarter)
sort idh_ch
gen idp_ci = indivno

duplicates report idh_ch idp_ci
sort idh_ch idp_ci

saveold "`base_in'\Persons_2014.dta", replace


*** MERGE ***

merge m:1 idh_ch using "`base_in'\Household_2014.dta"
keep if _merge==3

drop _merge idh_ch  idp_ci

saveold "`base_out'", replace
