
clear all
local ruta "Y:\survey\JAM\LFS\1990\m4\data_orig\"

*Base 1


use "`ruta'\aprlbf90_rt1a.dta"

rename resp1 resp
rename strat1 strat
rename replic1 replic
rename reln1 reln
rename sex1  sex
rename age1 age

egen idbase = concat( par dwell hhld indv)
sort idbase

save "`ruta'\aprlbf90_rt1amerge.dta", replace

*Base 2

clear all
local ruta "Y:\survey\JAM\LFS\1990\m4\data_orig\"

use "`ruta'\aprlbf90_rt2a.dta"

rename resp2 resp
rename strat2 strat
rename replic2 replic
rename reln2 reln
rename sex2  sex
rename age2 age

egen idbase = concat( par dwell hhld indv)
sort idbase

save "`ruta'\aprlbf90_rt2amerge.dta", replace

*Base 3

clear all
local ruta "Y:\survey\JAM\LFS\1990\m4\data_orig\"

use "`ruta'\aprlbf90_rt3a.dta"

   
rename resr resp
rename strav strat
rename replie replic
rename relp reln



egen idbase = concat( par dwell hhld indv)
sort idbase

save "`ruta'\aprlbf90_rt3amerge.dta", replace


* Merge
clear all
local ruta "Y:\survey\JAM\LFS\1990\m4\data_orig\"
use "`ruta'\aprlbf90_rt1amerge.dta"

merge m:m idbase using "`ruta'\aprlbf90_rt2amerge.dta"
tab _merge
rename _merge _merge2

merge m:m idbase using "`ruta'\aprlbf90_rt3amerge.dta"
tab _merge
rename _merge _merge3

drop _merge* idbase
save "Y:\survey\JAM\LFS\1990\m4\data_merge\JAM_1990m4.dta", replace
