*Elaboración: Yessenia Loayza (desloay@hotmail.com | yessenial@iadb.org)
*Octubre, 2013


*** MERGE BRASIL 2003 ****
*------------------------*

/*Estas bases de datos fueron descargadas el 30 septiembre 2013 e incluyen
los nuevos pesos con la proyeccion de poblacion del año 2010
*/

*1 - Conversion Bases de datos
clear
set more off
cd "\\Sdssrv03\surveys\survey\BRA\PNAD\2003\m9\data_orig\"
infile using "INPUT PES2003.do", using("PES2003.txt")
sort uf v0102 v0103
saveold "pes2003.dta", replace

clear
infile using "INPUT DOM2003.do", using("DOM2003.txt")
sort uf v0102 v0103
saveold "dom2003.dta", replace


*2.- Merge
clear
use "pes2003.dta" , clear
merge uf v0102 v0103 using "dom2003.dta"
tab _merge
keep if v0201==1 & v0104==1 /*Hogares permanentes y encuestas realizadas*/
tab _merge
drop _merge
saveold "\\Sdssrv03\surveys\survey\BRA\PNAD\2003\m9\data_merge\BRA_2003m9.dta", replace

