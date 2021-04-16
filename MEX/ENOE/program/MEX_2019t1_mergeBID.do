* (Versi�n Stata 12)
clear
set more off

*Última Modificación: Alvaro Altamirano / Octubre 2019.
*________________________________________________________________________________________________________________*

 * Activar si es necesario (dejar desactivado para evitar sobreescribir la base y dejar la posibilidad de 
 * utilizar un loop)
 * Los datos se obtienen de las carpetas que se encuentran en el servidor: ${surveysFolder}
 * Se tiene acceso al servidor �nicamente al interior del BID.
 * El servidor contiene las bases de datos MECOVI.
 *________________________________________________________________________________________________________________*
 
global ruta = "${surveysFolder}\\survey\MEX\ENOE\2019\t1\data_orig"

local PAIS MEX
local ENCUESTA ENOE
local ANO "2019"
local ronda t1

local log_file = "${surveysFolder}\harmonized\\`PAIS'\\`ENCUESTA'\\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"
local base_out = "${surveysFolder}\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\\data_merge\\`PAIS'_`ANO'`ronda'.dta"

*capture log close
*log using "`log_file'", replace 


use "$ruta\COE1T119.dta"
rename _all, lower
destring eda, replace
drop if r_def!=0 | eda<=11 | eda==99		// Se elimina a aquellos que no sean residentes permanentes, no tengasn cuestionario completo o sean menores de 12 a�os (s�lo se hace cuestionario de ocupaci�n a los residentes mayores de 12 a�os)		
sort  cd_a  ent  con  v_sel  n_hog h_mud n_ren	
*Limit variable labels' length
foreach i of varlist _all {
local longlabel: var label `i'
local shortlabel = substr(`"`longlabel'"',1,79)
label var `i' `"`shortlabel'"'
}
saveold "$ruta\COE1T119.dta" , replace

use "$ruta\COE2T119.dta"
rename _all, lower
destring eda, replace
drop if eda<=11 | eda==99		// Se elimina a aquellos que no sean residentes permanentes, no tengasn cuestionario completo o sean menores de 12 a�os (s�lo se hace cuestionario de ocupaci�n a los residentes mayores de 12 a�os)		
sort  cd_a  ent  con  v_sel  n_hog h_mud n_ren	
foreach i of varlist _all {
local longlabel: var label `i'
local shortlabel = substr(`"`longlabel'"',1,79)
label var `i' `"`shortlabel'"'
}
saveold "$ruta\COE2T119.dta", replace

use "$ruta\SDEMT119.dta"
rename _all, lower
destring eda, replace
drop if r_def!=0 | eda<=11 | eda==99		// Se elimina a aquellos que no sean residentes permanentes, no tengasn cuestionario completo o sean menores de 12 a�os (s�lo se hace cuestionario de ocupaci�n a los residentes mayores de 12 a�os)		
sort  cd_a  ent  con  v_sel  n_hog h_mud n_ren	
merge cd_a  ent  con  v_sel  n_hog  h_mud  n_ren using "$ruta\COE1T119.dta"
tab _merge
keep if _merge==3
drop _merge
sort  cd_a  ent  con  v_sel  n_hog h_mud n_ren	
merge cd_a  ent  con  v_sel  n_hog  h_mud  n_ren using "$ruta\COE2T119.dta"
tab _merge
drop _merge
foreach i of varlist _all {
local longlabel: var label `i'
local shortlabel = substr(`"`longlabel'"',1,79)
label var `i' `"`shortlabel'"'
}
saveold "`base_out'", version(12) replace
