* (Versión Stata 12)
clear
set more off


*________________________________________________________________________________________________________________*

 * Activar si es necesario (dejar desactivado para evitar sobreescribir la base y dejar la posibilidad de 
 * utilizar un loop)
 * Los datos se obtienen de las carpetas que se encuentran en el servidor: ${surveysFolder}
 * Se tiene acceso al servidor únicamente al interior del BID.
 * El servidor contiene las bases de datos MECOVI.
 *________________________________________________________________________________________________________________*
 
global ruta = "${surveysFolder}\\survey\MEX\ENOE\2006\t3\data_orig"

local PAIS MEX
local ENCUESTA ENOE
local ANO "2006"
local ronda t3

local log_file = "${surveysFolder}\harmonized\\`PAIS'\\`ENCUESTA'\\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"
local base_out = "${surveysFolder}\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\\data_merge\\`PAIS'_`ANO'`ronda'.dta"

*capture log close
*log using "`log_file'", replace 


use "$ruta\coe1t306.dta"
destring eda, replace
drop if r_def!="00" | eda<=11 | eda==99		// Se elimina a aquellos que no sean residentes permanentes, no tengasn cuestionario completo o sean menores de 12 años (sólo se hace cuestionario de ocupación a los residentes mayores de 12 años)		
sort  cd_a  ent  con  v_sel  n_hog h_mud n_ren	
saveold "$ruta\coe1t3.dta" , replace

use "$ruta\coe2t306.dta"
destring eda, replace
drop if eda<=11 | eda==99		// Se elimina a aquellos que no sean residentes permanentes, no tengasn cuestionario completo o sean menores de 12 años (sólo se hace cuestionario de ocupación a los residentes mayores de 12 años)		
sort  cd_a  ent  con  v_sel  n_hog h_mud n_ren	
saveold "$ruta\coe2t3.dta", replace

use "$ruta\sdemt306.dta"
destring eda, replace
drop if r_def!="00" | eda<=11 | eda==99		// Se elimina a aquellos que no sean residentes permanentes, no tengasn cuestionario completo o sean menores de 12 años (sólo se hace cuestionario de ocupación a los residentes mayores de 12 años)		
sort  cd_a  ent  con  v_sel  n_hog h_mud n_ren	
merge cd_a  ent  con  v_sel  n_hog  h_mud  n_ren using "$ruta\coe1t3.dta"
tab _merge
keep if _merge==3
drop _merge
sort  cd_a  ent  con  v_sel  n_hog h_mud n_ren	
merge cd_a  ent  con  v_sel  n_hog  h_mud  n_ren using "$ruta\coe2t3.dta"
tab _merge
drop _merge
saveold "`base_out'", replace

