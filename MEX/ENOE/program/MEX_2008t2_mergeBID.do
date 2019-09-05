* (Versión Stata 12)
clear
set more off


*________________________________________________________________________________________________________________*

 * Activar si es necesario (dejar desactivado para evitar sobreescribir la base y dejar la posibilidad de 
 * utilizar un loop)
 * Los datos se obtienen de las carpetas que se encuentran en el servidor: \\Sdssrv03\surveys
 * Se tiene acceso al servidor únicamente al interior del BID.
 * El servidor contiene las bases de datos MECOVI.
 *________________________________________________________________________________________________________________*
 
global ruta = "\\Sdssrv03\surveys\\survey\MEX\ENOE\2008\t2\data_orig"

local PAIS MEX
local ENCUESTA ENOE
local ANO "2008"
local ronda t2

local log_file = "\\Sdssrv03\surveys\harmonized\\`PAIS'\\`ENCUESTA'\\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"
local base_out = "\\Sdssrv03\surveys\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\\data_merge\\`PAIS'_`ANO'`ronda'.dta"

*capture log close
*log using "`log_file'", replace 


use "$ruta\coe1t208.dta"
destring eda, replace
drop if r_def!="00" | eda<=11 | eda==99		// Se elimina a aquellos que no sean residentes permanentes, no tengasn cuestionario completo o sean menores de 12 años (sólo se hace cuestionario de ocupación a los residentes mayores de 12 años)		
sort  cd_a  ent  con  v_sel  n_hog h_mud n_ren	
saveold "$ruta\coe1t2.dta" , replace

use "$ruta\coe2t208.dta"
destring eda, replace
drop if eda<=11 | eda==99		// Se elimina a aquellos que no sean residentes permanentes, no tengasn cuestionario completo o sean menores de 12 años (sólo se hace cuestionario de ocupación a los residentes mayores de 12 años)		
sort  cd_a  ent  con  v_sel  n_hog h_mud n_ren	
saveold "$ruta\coe2t2.dta", replace

use "$ruta\sdemt208.dta"
destring eda, replace
drop if r_def!="00" | eda<=11 | eda==99		// Se elimina a aquellos que no sean residentes permanentes, no tengasn cuestionario completo o sean menores de 12 años (sólo se hace cuestionario de ocupación a los residentes mayores de 12 años)		
sort  cd_a  ent  con  v_sel  n_hog h_mud n_ren	
merge cd_a  ent  con  v_sel  n_hog  h_mud  n_ren using "$ruta\coe1t2.dta"
tab _merge
keep if _merge==3
drop _merge
sort  cd_a  ent  con  v_sel  n_hog h_mud n_ren	
merge cd_a  ent  con  v_sel  n_hog  h_mud  n_ren using "$ruta\coe2t2.dta"
tab _merge
drop _merge
saveold "`base_out'", replace

