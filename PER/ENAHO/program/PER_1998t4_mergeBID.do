*Mayo, 2013
*Yessenia Loayza

***Merge para incorporar la base de datos sumaria 1999-iv**

clear
cd "Y:\survey\PER\ENAHO\1998\t4\data_orig\"
/*
use "sumaria 1998-iv.dta", clear
sort conglome vivienda hogar
save, replace
*/
clear
use per98_4f.dta, clear
rename CONGLOME conglome
rename VIVIENDA vivienda
rename HOGAR  hogar
sort conglome vivienda hogar
joinby conglome vivienda hogar using "sumaria 1998-iv.dta",  unmatched(using)
tab _merge
drop id _merge

save "Y:\survey\PER\ENAHO\1998\t4\data_merge\PER_1998t4.dta", replace

                                           
