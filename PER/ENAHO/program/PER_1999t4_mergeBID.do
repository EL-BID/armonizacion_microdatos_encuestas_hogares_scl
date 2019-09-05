*Mayo, 2013
*Yessenia Loayza

***Merge para incorporar la base de datos sumaria 1999-iv**

clear
cd "Y:\survey\PER\ENAHO\1999\t4\data_orig\"
/*
use "sumaria 1999-iv.dta", clear
sort conglome vivienda hogar
drop id
egen id= concat(conglome vivienda hogar)   
sort id
save, replace
*/
clear
use per99_4f.dta, clear
sort CONGLOME VIVIENDA HOGAR
egen id= concat(CONGLOME VIVIENDA HOGAR)
sort id 

merge m:1 id using "sumaria 1999-iv.dta"
drop id _merge

save "Y:\survey\PER\ENAHO\1999\t4\data_merge\PER_1999t4.dta.dta", replace

                                           
