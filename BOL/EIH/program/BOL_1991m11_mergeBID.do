/* To read the ascii files from Bolivia 93 */
capture log close
clear
version 7.0
set mem 50m
set more 1

local in = "${surveysFolder}\ARM\BOL\1991\Programs\Van_prog\"
local out = "${surveysFolder}\ARM\BOL\1991\Van_data\"


local basesi = "aseta cupa1 cupa2 damie deso1 deso2 inac1 inac2 ocup1 ocup2 salu1 salu2"
local basesh = "carat gaho1 gaho2 gaho3 gaho4 gaho5 gaho6 gaho7 vivi1 vivi2 vivi3"


foreach y of local basesh {

use "${surveysFolder}\ARM\BOL\1991\Orig_data\491`y'.dta"
renpfix v `y'_
rename `y'_1 identifi
sort identifi
desc
summ
save "${surveysFolder}\ARM\BOL\1991\Van_data\491`y'.dta", replace
clear }

foreach y of local basesi {

use "${surveysFolder}\ARM\BOL\1991\Orig_data\491`y'.dta"
renpfix v `y'_
rename `y'_1 identifi
rename `y'_2 hogar
sort hogar identifi
desc
summ
save "${surveysFolder}\ARM\BOL\1991\Van_data\491`y'.dta", replace
clear }

log using "`in'vanbol91.log", replace
            
use "`out'491aseta.dta", clear
sort hogar identifi
save "`out'491aseta.dta", replace

merge hogar identifi using "`out'491cupa1.dta"
tab _merge
drop _merge
sort hogar identifi

merge hogar identifi using "`out'491cupa2.dta"
tab _merge
drop _merge
sort hogar identifi

merge hogar identifi using "`out'491damie.dta"
tab _merge
drop _merge
sort hogar identifi 

merge hogar identifi using "`out'491deso1.dta"
tab _merge
drop _merge
sort hogar identifi

merge hogar identifi using "`out'491deso2.dta"
tab _merge
drop _merge
sort hogar identifi

merge hogar identifi using "`out'491inac1.dta"
tab _merge
drop _merge
sort hogar identifi

merge hogar identifi using "`out'491inac2.dta"
tab _merge
drop _merge
sort hogar identifi 

merge hogar identifi using "`out'491ocup1.dta"
tab _merge
drop _merge
sort hogar identifi

merge hogar identifi using "`out'491ocup2.dta"
tab _merge
drop _merge
sort hogar identifi 

merge hogar identifi using "`out'491salu1.dta"
tab _merge
drop _merge
sort hogar identifi 

merge hogar identifi using "`out'491salu2.dta"
tab _merge
drop _merge
sort hogar identifi

desc
sum

save "`out'persons.dta", replace
clear



use "`out'491carat.dta"
sort identifi
save "`out'491carat.dta", replace


merge identifi using "`out'491gaho1.dta"
tab _merge
drop _merge
sort identifi 

merge identifi using "`out'491gaho2.dta"
tab _merge
drop _merge
sort identifi 

merge identifi using "`out'491gaho3.dta"
tab _merge
drop _merge
sort identifi 

merge identifi using "`out'491gaho4.dta"
tab _merge
drop _merge
sort identifi 

merge identifi using "`out'491gaho5.dta"
tab _merge
drop _merge
sort identifi 

merge identifi using "`out'491gaho6.dta"
tab _merge
drop _merge
sort identifi 

merge identifi using "`out'491gaho7.dta"
tab _merge
drop _merge
sort identifi 

merge identifi using "`out'491vivi1.dta"
tab _merge
drop _merge
sort identifi

merge identifi using "`out'491vivi2.dta"
tab _merge
drop _merge
sort identifi 

merge identifi using "`out'491vivi3.dta"
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

save "`out'vanbol91.dta", replace

log close

