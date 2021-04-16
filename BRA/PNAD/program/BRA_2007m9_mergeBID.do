*Elaboración: Yessenia Loayza (desloay@hotmail.com | yessenial@iadb.org)
*Octubre, 2013


*** MERGE BRASIL 2007 ****
*------------------------*

/*Estas bases de datos fueron descargadas el 30 septiembre 2013 e incluyen
los nuevos pesos con la proyeccion de poblacion del año 2010
*/

*1 - Conversion Bases de datos
clear
set more off
cd "${surveysFolder}\survey\BRA\PNAD\2007\m9\data_orig\"
infile using "INPUT PES2007.do", using("PES2007.txt")
sort uf v0102 v0103
saveold "pes2007.dta", replace

clear
infile using "INPUT DOM2007.do", using("DOM2007.txt")
sort uf v0102 v0103
saveold "dom2007.dta", replace


*2.- Merge
clear
use "pes2007.dta" , clear
merge uf v0102 v0103 using "dom2007.dta"
tab _merge
keep if v0201==1 & v0104==1 /*Hogares permanentes y encuestas realizadas*/
tab _merge
drop _merge
saveold "${surveysFolder}\survey\BRA\PNAD\2007\m9\data_merge\BRA_2007m9.dta", replace

