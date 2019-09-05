* Elaboración: Marcela Rubio (mrubio@iadb.org | marcelarubio28@gmail.com)
* Diciembre 2015
*** MERGE BRASIL 2014 ****
*------------------------*


/*Estas bases de datos fueron descargadas el 25 de septiembre de 2014*/

*1 - Conversion Bases de datos
clear
set more off
cd "\\Sdssrv03\surveys\survey\BRA\PNAD\2014\m9\data_orig\"
infile using "input_PES_2014.do", using("PES2014.txt")
foreach v of varlist _all {
      capture rename `v' `=lower("`v'")'
   }

sort uf v0102 v0103
saveold "pes2014.dta", replace

clear
set more off
infile using "input_DOM_2014.do", using("DOM2014.txt")
foreach v of varlist _all {
      capture rename `v' `=lower("`v'")'
   }
destring uf v0102 v0103, replace
egen id=group(uf v0102 v0103)
bys id: g x=_n
keep if x==1
sort uf v0102 v0103
saveold "dom2014.dta", replace


*2.- Merge
clear
use "pes2014.dta" , clear
format v0102 %18.0g

merge m:1 uf v0102 v0103 using "dom2014.dta"
*drop _merge

tab v0201
tab v0104
more
keep if v0201==1 & v0104==1 /*Hogares permanentes y encuestas realizadas*/
tab _merge
more
drop _merge
saveold "\\Sdssrv03\surveys\survey\BRA\PNAD\2014\m9\data_merge\BRA_2014m9.dta", replace

