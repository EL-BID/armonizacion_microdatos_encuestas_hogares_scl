clear
set mem 50m
set more off
use "T:\Uruguay\2000\Datos originales\h00s1int.dta", clear
sort ident
by ident : assert _N==1
gen double id_hogar=ident
gen region=2
la def region 1"Montevideo" 2"Interior"
la val region region
compress
sort id_hogar
save "T:\Uruguay\2000\Datos originales\ah00s1int.dta", replace
use "T:\Uruguay\2000\Datos originales\h00s1mon.dta", clear
sort ident
by ident : assert _N==1
gen double id_hogar=ident
gen region=1
compress
sort id_hogar
save "T:\Uruguay\2000\Datos originales\ah00s1mon.dta", replace
use "T:\Uruguay\2000\Datos originales\h00s2int.dta", clear
sort ident
by ident : assert _N==1
gen double id_hogar=ident
gen region=2
compress
sort id_hogar
save "T:\Uruguay\2000\Datos originales\ah00s2int.dta", replace
use "T:\Uruguay\2000\Datos originales\h00s2mon.dta", clear
sort ident
by ident : assert _N==1
gen double id_hogar=ident
gen region=1
compress
sort id_hogar
save "T:\Uruguay\2000\Datos originales\ah00s2mon.dta", replace

use "T:\Uruguay\2000\Datos originales\p00s1int.dta", clear
gen double id_hogar=correlat
gen double id_pers=persona
sort id_hogar id_pers
by id_hogar id_pers : assert _N==1
gen region=2
compress
save "T:\Uruguay\2000\Datos originales\ap00s1int.dta", replace
use "T:\Uruguay\2000\Datos originales\p00s1mon.dta", clear
gen double id_hogar=correlat
gen double id_pers=persona
sort id_hogar id_pers
by id_hogar id_pers : assert _N==1
gen region=1
compress
save "T:\Uruguay\2000\Datos originales\ap00s1mon.dta", replace
use "T:\Uruguay\2000\Datos originales\p00s2int.dta", clear
gen double id_hogar=correlat
gen double id_pers=persona
sort id_hogar id_pers
by id_hogar id_pers : assert _N==1
gen region=2
compress
save "T:\Uruguay\2000\Datos originales\ap00s2int.dta", replace
use "T:\Uruguay\2000\Datos originales\p00s2mon.dta", clear
gen double id_hogar=correlat
gen double id_pers=persona
sort id_hogar id_pers
by id_hogar id_pers : assert _N==1
gen region=1
compress
save "T:\Uruguay\2000\Datos originales\ap00s2mon.dta", replace

**************************.
**************************.

tempfile temp1 temp2 temp3 temp4
use "T:\Uruguay\2000\Datos originales\ah00s1int.dta", clear
merge id_hogar using "T:\Uruguay\2000\Datos originales\ap00s1int.dta"
drop _merge
sort id_hogar id_pers
save "T:\Uruguay\2000\Datos originales\`temp1'.dta",replace
use "T:\Uruguay\2000\Datos originales\ah00s1mon.dta", clear
merge id_hogar using "T:\Uruguay\2000\Datos originales\ap00s1mon.dta"
drop _merge
sort id_hogar id_pers
save "T:\Uruguay\2000\Datos originales\`temp2'.dta",replace
use "T:\Uruguay\2000\Datos originales\ah00s2int.dta", clear
merge id_hogar using "T:\Uruguay\2000\Datos originales\ap00s2int.dta"
drop _merge
sort id_hogar id_pers
save "T:\Uruguay\2000\Datos originales\`temp3'.dta",replace
use "T:\Uruguay\2000\Datos originales\ah00s2mon.dta", clear
merge id_hogar using "T:\Uruguay\2000\Datos originales\ap00s2mon.dta"
drop _merge
sort id_hogar id_pers
save "T:\Uruguay\2000\Datos originales\`temp4'.dta",replace

use "T:\Uruguay\2000\Datos originales\`temp1'.dta", clear
local i=2
while `i'<5 {
merge id_hogar id_pers using "T:\Uruguay\2000\Datos originales\`temp`i''.dta"
ta _merge
drop _merge
sort id_hogar id_pers
local i=`i'+1
}
save "T:\Uruguay\2000\Datos originales\ury2000.dta", replace

/*

clear
set mem 100m
use "X:\ARM\URU\ECH\2000\Orig_data\h00s1mon.dta"
append using "X:\ARM\URU\ECH\2000\Orig_data\h00s2mon.dta"
append using "X:\ARM\URU\ECH\2000\Orig_data\h00s1int.dta"
append using "X:\ARM\URU\ECH\2000\Orig_data\h00s2int.dta"
compress
sort ident
save "X:\ARM\URU\ECH\2000\Van_data\uryhog00.dta", replace
clear

use "X:\ARM\URU\ECH\2000\Orig_data\p00s1mon.dta"
append using "X:\ARM\URU\ECH\2000\Orig_data\p00s2mon.dta"
append using "X:\ARM\URU\ECH\2000\Orig_data\p00s1int.dta"
append using "X:\ARM\URU\ECH\2000\Orig_data\p00s2int.dta"
compress
rename correlat ident
sort ident
save "X:\ARM\URU\ECH\2000\Van_data\uryper00.dta", replace

merge ident using "X:\ARM\URU\ECH\2000\Van_data\uryhog00.dta"
save "X:\ARM\URU\ECH\2000\Van_data\ury00.dta", replace
