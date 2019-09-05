
clear
use "Y:\survey\PER\ENAHO\2000\t4\data_orig\per2000_4f.dta", clear
*Traigo la variable etnia de la base de gobernabilidad
sort conglome vivienda hogar 
egen id= concat(conglome vivienda hogar) 
sort id
merge m:m id using "Y:\survey\PER\ENAHO\2000\t4\data_orig\enaho01b-2000iv-opinión_etnia.dta"
keep if result01==1 | result01==2
keep if p217==.
tab _merge
drop id _merge
*renombro las variable para hacerlas homogeneas en la serie hasta el 2011
rename q28 p46
rename q34 p47

save "Y:\survey\PER\ENAHO\2000\t4\data_merge\PER_2000t4.dta", replace
