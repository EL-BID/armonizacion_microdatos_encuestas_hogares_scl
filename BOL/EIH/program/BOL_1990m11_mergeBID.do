/* To read the ascii files from Bolivia 93 */
capture log close
clear
version 7.0
set mem 50m
set more 1

local in = "${surveysFolder}\ARM\BOL\1990\Programs\Van_prog\"
local out = "${surveysFolder}\ARM\BOL\1990\Van_data\"


local basesi = "aseta cupro datge desoc educa inact migra ocup1 ocup2 salu1 salu2 salu3 salu4 salu5"
local basesh = "carat gedal inlab gaho1 gaho2 vivi1 vivi2 "


foreach y of local basesh {

use "${surveysFolder}\ARM\BOL\1990\Orig_data\390`y'.dta"
renpfix v `y'_
rename `y'_1 identifi
sort identifi
desc
summ
save "${surveysFolder}\ARM\BOL\1990\Van_data\390`y'.dta", replace
clear }

foreach y of local basesi {

use "${surveysFolder}\ARM\BOL\1990\Orig_data\390`y'.dta"
renpfix v `y'_
rename `y'_1 identifi
rename `y'_2 hogar
sort hogar identifi
desc
summ
save "${surveysFolder}\ARM\BOL\1990\Van_data\390`y'.dta", replace
clear }


log using "`in'vanbol90.log", replace

            
use "`out'390aseta.dta", clear
sort hogar identifi
save "`out'390aseta.dta", replace

merge hogar identifi using "`out'390cupro.dta"
tab _merge
drop _merge
sort hogar identifi

merge hogar identifi using "`out'390datge.dta"
tab _merge
drop _merge
sort hogar identifi

merge hogar identifi using "`out'390desoc.dta"
tab _merge
drop _merge
sort hogar identifi 

merge hogar identifi using "`out'390educa.dta"
tab _merge
drop _merge
sort hogar identifi

merge hogar identifi using "`out'390inact.dta"
tab _merge
drop _merge
sort hogar identifi

merge hogar identifi using "`out'390migra.dta"
tab _merge
drop _merge
sort hogar identifi

merge hogar identifi using "`out'390ocup1.dta"
tab _merge
drop _merge
sort hogar identifi 

merge hogar identifi using "`out'390ocup2.dta"
tab _merge
drop _merge
sort hogar identifi

merge hogar identifi using "`out'390salu1.dta"
tab _merge
drop _merge
sort hogar identifi 

merge hogar identifi using "`out'390salu2.dta"
tab _merge
drop _merge
sort hogar identifi 

merge hogar identifi using "`out'390salu3.dta"
tab _merge
drop _merge
sort hogar identifi

merge hogar identifi using "`out'390salu4.dta"
tab _merge
drop _merge
sort hogar identifi

merge hogar identifi using "`out'390salu5.dta"
tab _merge
drop _merge
sort hogar identifi

desc
sum

save "`out'persons.dta", replace
clear



use "`out'390carat.dta"
sort identifi
save "`out'390carat.dta", replace


merge identifi using "`out'390gaho1.dta"
tab _merge
drop _merge
sort identifi 

merge identifi using "`out'390gaho2.dta"
tab _merge
drop _merge
sort identifi 

merge identifi using "`out'390gedal.dta"
tab _merge
drop _merge
sort identifi 

merge identifi using "`out'390inlab.dta"
tab _merge
drop _merge
sort identifi 

merge identifi using "`out'390vivi1.dta"
tab _merge
drop _merge
sort identifi

merge identifi using "`out'390vivi2.dta"
tab _merge
drop _merge
sort identifi 

rename identifi hogar
sort hogar
desc
sum

save "`out'houses.dta", replace

use "`out'persons.dta", replace
merge hogar using "`out'houses.dta"
tab _merge
drop _merge

sort hogar identifi
desc
sum

save "`out'vanbol90.dta", replace

log close

