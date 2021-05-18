*Elaboración: Yessenia Loayza (desloay@hotmail.com | yessenial@iadb.org)
*Octubre, 2013


*** MERGE BRASIL 2004 ****
*------------------------*

/*Estas bases de datos fueron descargadas el 30 septiembre 2013 e incluyen
los nuevos pesos con la proyeccion de poblacion del año 2010
*/

*1 - Conversion Bases de datos
clear
set more off
cd "${surveysFolder}\survey\BRA\PNAD\2004\m9\data_orig\"
infile using "Input_Pes2004.do", using("PES2004.txt")
sort uf v0102 v0103
saveold "pes2004.dta", replace

clear
infile using "Input_dom2004.do", using("DOM2004.txt")
sort uf v0102 v0103
saveold "dom2004.dta", replace


*2.- Merge
clear
use "pes2004.dta" , clear
merge uf v0102 v0103 using "dom2004.dta"
tab _merge
keep if v0201==1 & v0104==1 /*Hogares permanentes y encuestas realizadas*/
tab _merge
drop _merge
saveold "${surveysFolder}\survey\BRA\PNAD\2004\m9\data_merge\BRA_2004m9.dta", replace

