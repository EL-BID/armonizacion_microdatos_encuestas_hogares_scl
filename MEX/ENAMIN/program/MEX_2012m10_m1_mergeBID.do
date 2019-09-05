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
 
global ruta = "\\Sdssrv03\surveys\\survey\MEX\ENAMIN\2012\m10_m1\data_orig"

local PAIS MEX
local ENCUESTA ENAMIN
local ANO "2012"
local ronda m10_m1

local log_file = "\\Sdssrv03\surveys\harmonized\\`PAIS'\\`ENCUESTA'\\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"
local base_out = "\\Sdssrv03\surveys\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\\data_merge\\`PAIS'_`ANO'`ronda'.dta"

*capture log close
*log using "`log_file'", replace 


use "$ruta\caratula.dta"
sort  t_loc ent  con  v_sel  n_hog h_mud n_ren	
saveold "$ruta\caratula1.dta" , replace

use "$ruta\princip1.dta"
sort t_loc ent  con  v_sel  n_hog h_mud n_ren
saveold "$ruta\princip1_1.dta", replace

use "$ruta\princip2.dta"
sort t_loc ent  con  v_sel  n_hog h_mud n_ren	
saveold "$ruta\princip2_1.dta", replace

use "$ruta\personal.dta"
sort  t_loc ent  con  v_sel  n_hog h_mud n_ren	
saveold "$ruta\personal_1.dta", replace

use "$ruta\ing_gast.dta"
sort  t_loc ent  con  v_sel  n_hog h_mud n_ren	
saveold "$ruta\ing_gast_1.dta", replace

use "$ruta\caratula1.dta"
sort  t_loc ent  con  v_sel  n_hog h_mud n_ren	
merge t_loc ent  con  v_sel  n_hog h_mud n_ren using "$ruta\princip1_1.dta"
tab _merge
keep if _merge==3
drop _merge
sort  t_loc  ent  con  v_sel  n_hog h_mud n_ren	
merge t_loc  ent  con  v_sel  n_hog  h_mud  n_ren using "$ruta\princip2_1.dta"
tab _merge
keep if _merge==3
drop _merge
sort  t_loc  ent  con  v_sel  n_hog h_mud n_ren	
merge t_loc  ent  con  v_sel  n_hog  h_mud  n_ren using "$ruta\ing_gast_1.dta"
tab _merge
keep if _merge==3
drop _merge
sort  t_loc  ent  con  v_sel  n_hog h_mud n_ren	

*merge t_loc  ent  con  v_sel  n_hog  h_mud  n_ren using "$ruta\personal_1.dta"
*tab _merge
*keep if _merge==3
*drop _merge

saveold "`base_out'", replace

erase "$ruta\caratula1.dta"
erase "$ruta\princip1_1.dta"
erase "$ruta\princip2_1.dta"
erase "$ruta\personal_1.dta"

