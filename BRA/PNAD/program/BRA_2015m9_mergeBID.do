* (Versión Stata 13)
clear
set more off
*________________________________________________________________________________________________________________*

 * Activar si es necesario (dejar desactivado para evitar sobreescribir la base y dejar la posibilidad de 
 * utilizar un loop)
 * Los datos se obtienen de las carpetas que se encuentran en el servidor: ${surveysFolder}
 * Se tiene acceso al servidor únicamente al interior del BID.
 * El servidor contiene las bases de datos MECOVI.
 *________________________________________________________________________________________________________________*
 

global ruta = "${surveysFolder}"

local PAIS BRA
local ENCUESTA PNAD
local ANO "2015"
local ronda m9 
*local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\\"
local base_out = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\"
                        
capture log close
*log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Brasil
Encuesta: PNAD
Round: m9
Autores: Mayra Saenz mayras@iadb.org | mayrasaenz.a@gmail.com
Fecha última modificación: 

							
****************************************************************************/
****************************************************************************/



/*Estas bases de datos fueron descargadas el 30 de noviembre de 2016*/

*1 - Conversion Bases de datos

infile using "`base_in'input PES2015_.do", using("`base_in'PES2015.txt")
foreach v of varlist _all {
      capture rename `v' `=lower("`v'")'
   }

destring uf v0102 v0103, replace
sort uf v0102 v0103
saveold "`base_in'pes2015.dta", replace

clear
set more off
infile using "`base_in'input DOM2015_.do", using("`base_in'DOM2015.txt")
foreach v of varlist _all {
      capture rename `v' `=lower("`v'")'
   }

destring uf v0102 v0103, replace
sort uf v0102 v0103

*En la base de hogares existen 784 observaciones duplicadas, que equivalen al 0.52% de la base. Se eliminan los duplicados 392 observaciones (0.25%) para hacer el merge
duplicates tag v0102 v0103, g(tag)
tab tag
duplicates drop v0102 v0103, force
drop tag
saveold "`base_in'dom2015.dta", replace


*2.- Merge
clear
use "`base_in'pes2015.dta" , clear
merge m:1 uf v0102 v0103 using "`base_in'dom2015.dta"
*drop _merge

tab v0201
tab v0104
more
keep if v0201==1 & v0104==1 /*Hogares permanentes y encuestas realizadas*/
tab _merge
more
drop _merge



saveold "${surveysFolder}\survey\BRA\PNAD\2015\m9\data_merge\\BRA_2015m9.dta", replace

