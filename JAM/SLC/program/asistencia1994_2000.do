*Mayra Sáenz
*Mayo, 2015

*___________*
*  1994     *
*___________*

clear all
use "Z:\survey\JAM\SLC\1994\m11\data_merge\JAM_1994m11.dta"

foreach v of varlist _all {
	local lowname=lower("`v'")
	rename `v' `lowname'
}
ds, has(type string)
    foreach var of varlist `r(varlist)' {
	    replace `var' = "." if strpos(`var',"N")
        replace `var' = "." if strpos(`var',"NN")
		replace `var' = "." if strpos(`var',"NN")
        replace `var' = "." if strpos(`var',"NNN")
		replace `var' = "." if strpos(`var',"NNNN")
		replace `var' = "." if strpos(`var',"NNNNN")
				       destring `var', replace
    }


g pais_c = "JAM"
g anio_c = 1994

g edad_ci = age 
g asiste_ci = b01 <15

*Jamaica no necesita factor de expansión.
g factor_ch =1

saveold "Z:\survey\JAM\SLC\1994\m11\data_merge\JAM_1994m11old.dta", replace

*___________*
*  1995     *
*___________*


clear all
use "Z:\survey\JAM\SLC\1995\m5\data_merge\JAM_1995m5.dta",clear
foreach v of varlist _all {
	local lowname=lower("`v'")
	rename `v' `lowname'
}
ds, has(type string)
    foreach var of varlist `r(varlist)' {
	    replace `var' = "." if strpos(`var',"N")
        replace `var' = "." if strpos(`var',"NN")
		replace `var' = "." if strpos(`var',"NN")
        replace `var' = "." if strpos(`var',"NNN")
		replace `var' = "." if strpos(`var',"NNNN")
		replace `var' = "." if strpos(`var',"NNNNN")
				       destring `var', replace
    }


g pais_c = "JAM"
g anio_c = 1995

g edad_ci = age 
g asiste_ci = b01 <15

*Jamaica no necesita factor de expansión.
g factor_ch =1

saveold "Z:\survey\JAM\SLC\1995\m5\data_merge\JAM_1995m5old.dta",replace

*___________*
*  1996     *
*___________*

use "Z:\survey\JAM\SLC\1996\m5\data_merge\JAM_1996m5.dta", clear
g pais_c = "JAM"
g anio_c = 1996

g edad_ci = age 
g asiste_ci = b01 <19
g factor_ch =1

saveold "Z:\survey\JAM\SLC\1996\m5\data_merge\JAM_1996m5old.dta", replace

*___________*
*  1998     *
*___________*

use "Z:\survey\JAM\SLC\1998\m5\data_merge\JAM_1998m5.dta", clear

g pais_c = "JAM"
g anio_c = 1998

g edad_ci = age 
g asiste_ci = b01 <19
g factor_ch =1

saveold "Z:\survey\JAM\SLC\1998\m5\data_merge\JAM_1998m5old.dta", replace


*___________*
*  1999     *
*___________*

use  "Z:\survey\JAM\SLC\1999\m5\data_merge\JAM_1999m5.dta", clear

g pais_c = "JAM"
g anio_c = 1999
g edad_ci = age 
g asiste_ci = b01 <19
g factor_ch =1
rename quintile popquint

saveold  "Z:\survey\JAM\SLC\1999\m5\data_merge\JAM_1999m5old.dta", replace 


*___________*
*  2000     *
*___________*

use "Z:\survey\JAM\SLC\2000\m5\data_merge\JAM_2000m5.dta", clear
g pais_c = "JAM"
g anio_c = 2000

g edad_ci = age 
g asiste_ci = b01 <19
g factor_ch =1
saveold "Z:\survey\JAM\SLC\2000\m5\data_merge\JAM_2000m5old.dta", replace



