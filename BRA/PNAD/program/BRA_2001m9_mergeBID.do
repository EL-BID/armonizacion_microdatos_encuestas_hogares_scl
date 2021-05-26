*Elaboración: Yessenia Loayza (desloay@hotmail.com | yessenial@iadb.org)
*Octubre, 2013


*** MERGE BRASIL 2001 ****
*------------------------*

/*Estas bases de datos fueron descargadas el 30 septiembre 2013 e incluyen
los nuevos pesos con la proyeccion de poblacion del año 2010
*/

*1 - Conversion Bases de datos
clear
set more off
cd "${surveysFolder}\survey\BRA\PNAD\2001\m9\data_orig\"
infile using "INPUT PES2001.do", using("PES2001.TXT")
sort uf v0102 v0103
saveold "pes2001.dta", replace

clear
infile using "INPUT DOM2001.do", using("DOM2001.TXT")
sort uf v0102 v0103
saveold "dom2001.dta", replace


*2.- Merge
clear
use "pes2001.dta" , clear
merge uf v0102 v0103 using "dom2001.dta"
keep if v0201==1 & v0104==1 /*Hogares permanentes y encuestas realizadas*/
tab _merge
drop _merge
saveold "${surveysFolder}\survey\BRA\PNAD\2001\m9\data_merge\BRA_2001m9.dta", replace

