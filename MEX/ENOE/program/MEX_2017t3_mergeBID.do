* (Versión Stata 12)
clear
set more off

*Ãšltima ModificaciÃ³n: Alvaro Altamirano / Octubre 2019.
*________________________________________________________________________________________________________________*

 * Activar si es necesario (dejar desactivado para evitar sobreescribir la base y dejar la posibilidad de 
 * utilizar un loop)
 * Los datos se obtienen de las carpetas que se encuentran en el servidor: \\Sdssrv03\surveys
 * Se tiene acceso al servidor únicamente al interior del BID.
 * El servidor contiene las bases de datos MECOVI.
 *________________________________________________________________________________________________________________*
 
global ruta = "\\Sdssrv03\surveys\\survey\MEX\ENOE\2017\t3\data_orig"

local PAIS MEX
local ENCUESTA ENOE
local ANO "2017"
local ronda t3

local log_file = "\\Sdssrv03\surveys\harmonized\\`PAIS'\\`ENCUESTA'\\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"
local base_out = "\\Sdssrv03\surveys\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\\data_merge\\`PAIS'_`ANO'`ronda'.dta"

*capture log close
*log using "`log_file'", replace 


use "$ruta\COE1T317.dta"
destring eda, replace
drop if r_def!=0 | eda<=11 | eda==99		// Se elimina a aquellos que no sean residentes permanentes, no tengasn cuestionario completo o sean menores de 12 años (sólo se hace cuestionario de ocupación a los residentes mayores de 12 años)		
sort  cd_a  ent  con  v_sel  n_hog h_mud n_ren	
*Limit variable labels' length
foreach i of varlist _all {
local longlabel: var label `i'
local shortlabel = substr(`"`longlabel'"',1,79)
label var `i' `"`shortlabel'"'
}
saveold "$ruta\COE1T317.dta" , replace

use "$ruta\COE2T317.dta"
destring eda, replace
drop if eda<=11 | eda==99		// Se elimina a aquellos que no sean residentes permanentes, no tengasn cuestionario completo o sean menores de 12 años (sólo se hace cuestionario de ocupación a los residentes mayores de 12 años)		
sort  cd_a  ent  con  v_sel  n_hog h_mud n_ren	
foreach i of varlist _all {
local longlabel: var label `i'
local shortlabel = substr(`"`longlabel'"',1,79)
label var `i' `"`shortlabel'"'
}
saveold "$ruta\COE2T317.dta", replace

use "$ruta\SDEMT317.dta"
destring eda, replace
drop if r_def!=0 | eda<=11 | eda==99		// Se elimina a aquellos que no sean residentes permanentes, no tengasn cuestionario completo o sean menores de 12 años (sólo se hace cuestionario de ocupación a los residentes mayores de 12 años)		
sort  cd_a  ent  con  v_sel  n_hog h_mud n_ren	
merge cd_a  ent  con  v_sel  n_hog  h_mud  n_ren using "$ruta\COE1T317.dta"
tab _merge
keep if _merge==3
drop _merge
sort  cd_a  ent  con  v_sel  n_hog h_mud n_ren	
merge cd_a  ent  con  v_sel  n_hog  h_mud  n_ren using "$ruta\COE2T317.dta"
tab _merge
drop _merge
foreach i of varlist _all {
local longlabel: var label `i'
local shortlabel = substr(`"`longlabel'"',1,79)
label var `i' `"`shortlabel'"'
}
saveold "`base_out'", version(12) replace
