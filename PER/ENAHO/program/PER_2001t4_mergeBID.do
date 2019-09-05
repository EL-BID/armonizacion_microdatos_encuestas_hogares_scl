clear
use "Y:\survey\PER\ENAHO\2001\t4\data_orig\per01_iv.dta", clear
*Traigo la variable etnia de la base de gobernabilidad
sort conglome vivienda hogar 
egen id= concat(conglome vivienda hogar) 
sort id
merge m:m id using "Y:\survey\PER\ENAHO\2001\t4\data_orig\enaho01b-2001iv_etnia.dta"
keep if result01==1 | result01==2
drop id _merge
*renombro las variable para hacerlas homogeneas en la serie hasta el 2011
rename q25 p46
rename q32 p47
save "Y:\survey\PER\ENAHO\2001\t4\data_merge\PER_2001t4.dta", replace
