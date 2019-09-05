*Elaboración: Yessenia Loayza (desloay@hotmail.com | yessenial@iadb.org)
*Octubre, 2013
*MLO se cambió la base el 8/2014 (base reponderada publicada el 8/2014)

*** MERGE BRASIL 2012 ****
*------------------------*

/*Estas bases de datos fueron descargadas el 30 septiembre 2013*/

*1 - Conversion Bases de datos
clear
set more off
cd "\\Sdssrv03\surveys\survey\BRA\PNAD\2012\m9\data_orig\"
infile using "Input PES2012.do", using("PES2012.txt")
sort uf v0102 v0103
saveold "pes2012.dta", replace

clear
infile using "Input DOM2012.do", using("DOM2012.txt")
sort uf v0102 v0103
saveold "dom2012.dta", replace


*2.- Merge
clear
use "pes2012.dta" , clear
merge uf v0102 v0103 using "dom2012.dta"
tab _merge

tab v0201
tab v0104
more
keep if v0201==1 & v0104==1 /*Hogares permanentes y encuestas realizadas*/
tab _merge
more
drop _merge
saveold "\\Sdssrv03\surveys\survey\BRA\PNAD\2012\m9\data_merge\BRA_2012m9.dta", replace

