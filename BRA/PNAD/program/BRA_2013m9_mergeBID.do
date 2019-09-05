* Elaboración: Marcela Rubio (mrubio@iadb.org | marcelarubio28@gmail.com)
* Septiembre 2014
* Modificacion 2014, 01 MLO (variables de identificacion a string)
*** MERGE BRASIL 2013 ****
*------------------------*

/*Estas bases de datos fueron descargadas el 25 de septiembre de 2014*/

*1 - Conversion Bases de datos
clear
set more off
cd "\\Sdssrv03\surveys\survey\BRA\PNAD\2013\m9\data_orig\"
infile using "Input PES2013.do", using("PES2013.txt")
sort uf v0102 v0103
saveold "pes2013.dta", replace

clear
infile using "Input DOM2013.do", using("DOM2013.txt")
sort uf v0102 v0103
saveold "dom2013.dta", replace


*2.- Merge
clear
use "pes2013.dta" , clear
merge uf v0102 v0103 using "dom2013.dta"
tab _merge

tab v0201
tab v0104
more
keep if v0201==1 & v0104==1 /*Hogares permanentes y encuestas realizadas*/
tab _merge
more
drop _merge
saveold "\\Sdssrv03\surveys\survey\BRA\PNAD\2013\m9\data_merge\BRA_2013m9.dta", replace

