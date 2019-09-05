
set more off

clear

*global ruta = "\\Sdssrv03\surveys"

local PAIS HND
local ENCUESTA EPHPM
local ANO "2006"
local ronda m9 

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\"
local base_out = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
   
capture log close
log using "`log_file'", replace 



use "`base_in'\EPHPM_XXXIII_STATA_Otrosingresos.dta", clear
compress
keep if PRG==144
rename PRG pensi
rename LPS lps_pensi
rename DLS dls_pensi
rename Cambio_ cambio_pensi
rename DlsLps dlslps_pensi
rename TotaLps totalps_pensi
rename Ymensual Ymensual_pensi
sort HOGAR NPER

save "`base_in'\EPHPM_XXXIII_STATA_Otrosingresos_1.dta", replace

use "`base_in'\EPHPM_XXXIII_STATA_Otrosingresos.dta", clear
compress
keep if PRG==145
rename PRG jubi
rename LPS lps_jubi
rename DLS dls_jubi
rename Cambio_ cambio_jubi
rename DlsLps dlslps_jubi
rename TotaLps totalps_jubi
rename Ymensual Ymensual_jubi
sort HOGAR NPER

save "`base_in'\EPHPM_XXXIII_STATA_Otrosingresos_2.dta", replace

use "`base_in'\EPHPM_XXXIII_STATA_Otrosingresos.dta", clear
compress
keep if PRG==146
rename PRG alqui
rename LPS lps_alqui
rename DLS dls_alqui
rename Cambio_ cambio_alqui
rename DlsLps dlslps_alqui
rename TotaLps totalps_alqui
rename Ymensual Ymensual_alqui
sort HOGAR NPER

save "`base_in'\EPHPM_XXXIII_STATA_Otrosingresos_3.dta", replace

use "`base_in'\EPHPM_XXXIII_STATA_Otrosingresos.dta", clear
compress
keep if PRG==147
rename PRG subsi
rename LPS lps_subsi
rename DLS dls_subsi
rename Cambio_ cambio_subsi
rename DlsLps dlslps_subsi
rename TotaLps totalps_subsi
rename Ymensual Ymensual_subsi
sort HOGAR NPER

save "`base_in'\EPHPM_XXXIII_STATA_Otrosingresos_4.dta", replace

use "`base_in'\EPHPM_XXXIII_STATA_Otrosingresos.dta", clear
compress
keep if PRG==148
rename PRG intban
rename LPS lps_intban
rename DLS dls_intban
rename Cambio_ cambio_intban
rename DlsLps dlslps_intban
rename TotaLps totalps_intban
rename Ymensual Ymensual_intban
sort HOGAR NPER

save "`base_in'\EPHPM_XXXIII_STATA_Otrosingresos_5.dta", replace

use "`base_in'\EPHPM_XXXIII_STATA_Otrosingresos.dta", clear
compress
keep if PRG==149
rename PRG remefe
rename LPS lps_remefe
rename DLS dls_remefe
rename Cambio_ cambio_remefe
rename DlsLps dlslps_remefe
rename TotaLps totalps_remefe
rename Ymensual Ymensual_remefe
sort HOGAR NPER

save "`base_in'\EPHPM_XXXIII_STATA_Otrosingresos_6.dta", replace

use "`base_in'\EPHPM_XXXIII_STATA_Otrosingresos.dta", clear
compress
keep if PRG==150
rename PRG remesp
rename LPS lps_remesp
rename DLS dls_remesp
rename Cambio_ cambio_remesp
rename DlsLps dlslps_remesp
rename TotaLps totalps_remesp
rename Ymensual Ymensual_remesp
sort HOGAR NPER

save "`base_in'\EPHPM_XXXIII_STATA_Otrosingresos_7.dta", replace

use "`base_in'\EPHPM_XXXIII_STATA_Otrosingresos.dta", clear
compress
keep if PRG==151
rename PRG pendiv
rename LPS lps_pendiv
rename DLS dls_pendiv
rename Cambio_ cambio_pendiv
rename DlsLps dlslps_pendiv
rename TotaLps totalps_pendiv
rename Ymensual Ymensual_pendiv
sort HOGAR NPER

save "`base_in'\EPHPM_XXXIII_STATA_Otrosingresos_8.dta", replace

use "`base_in'\EPHPM_XXXIII_STATA_Otrosingresos.dta", clear
compress
keep if PRG==152
rename PRG ayuefe
rename LPS lps_ayuefe
rename DLS dls_ayuefe
rename Cambio_ cambio_ayuefe
rename DlsLps dlslps_ayuefe
rename TotaLps totalps_ayuefe
rename Ymensual Ymensual_ayuefe
sort HOGAR NPER

save "`base_in'\EPHPM_XXXIII_STATA_Otrosingresos_9.dta", replace

use "`base_in'\EPHPM_XXXIII_STATA_Otrosingresos.dta", clear
compress
keep if PRG==153
rename PRG ayuesp
rename LPS lps_ayuesp
rename DLS dls_ayuesp
rename Cambio_ cambio_ayuesp
rename DlsLps dlslps_ayuesp
rename TotaLps totalps_ayuesp
rename Ymensual Ymensual_ayuesp
sort HOGAR NPER

save "`base_in'\EPHPM_XXXIII_STATA_Otrosingresos_10.dta", replace

use "`base_in'\EPHPM_XXXIII_STATA_Otrosingresos.dta", clear
compress
keep if PRG==154
rename PRG ayuparef
rename LPS lps_ayuparef
rename DLS dls_ayuparef
rename Cambio_ cambio_ayuparef
rename DlsLps dlslps_ayuparef
rename TotaLps totalps_ayuparef
rename Ymensual Ymensual_ayuparef
sort HOGAR NPER

save "`base_in'\EPHPM_XXXIII_STATA_Otrosingresos_11.dta", replace

use "`base_in'\EPHPM_XXXIII_STATA_Otrosingresos.dta", clear
compress
keep if PRG==155
rename PRG ayupares
rename LPS lps_ayupares
rename DLS dls_ayupares
rename Cambio_ cambio_ayupares
rename DlsLps dlslps_ayupares
rename TotaLps totalps_ayupares
rename Ymensual Ymensual_ayupares
sort HOGAR NPER

save "`base_in'\EPHPM_XXXIII_STATA_Otrosingresos_12.dta", replace

use "`base_in'\EPHPM_XXXIII_STATA_Otrosingresos.dta", clear
compress
keep if PRG==156
rename PRG bonos
rename LPS lps_bonos
rename DLS dls_bonos
rename Cambio_ cambio_bonos
rename DlsLps dlslps_bonos
rename TotaLps totalps_bonos
rename Ymensual Ymensual_bonos
sort HOGAR NPER

save "`base_in'\EPHPM_XXXIII_STATA_Otrosingresos_13.dta", replace

use "`base_in'\EPHPM_XXXIII_STATA_Otrosingresos.dta", clear
compress
keep if PRG==157
rename PRG preslab
rename LPS lps_preslab
rename DLS dls_preslab
rename Cambio_ cambio_preslab
rename DlsLps dlslps_preslab
rename TotaLps totalps_preslab
rename Ymensual Ymensual_preslab
sort HOGAR NPER

save "`base_in'\EPHPM_XXXIII_STATA_Otrosingresos_14.dta", replace

use "`base_in'\EPHPM_XXXIII_STATA_Otrosingresos.dta", clear
compress
keep if PRG==158
rename PRG otrasfu
rename LPS lps_otrasfu
rename DLS dls_otrasfu
rename Cambio_ cambio_otrasfu
rename DlsLps dlslps_otrasfu
rename TotaLps totalps_otrasfu
rename Ymensual Ymensual_otrasfu
sort HOGAR NPER

save "`base_in'\EPHPM_XXXIII_STATA_Otrosingresos_15.dta", replace

* union de bases  
use "`base_in'\EPHPM_XXXIII_STATA_Otrosingresos_1.dta"

merge HOGAR using "`base_in'\EPHPM_XXXIII_STATA_Otrosingresos_2.dta", 
drop _merge

sort HOGAR
merge HOGAR using "`base_in'\EPHPM_XXXIII_STATA_Otrosingresos_3.dta", 
drop _merge

sort HOGAR
merge HOGAR using "`base_in'\EPHPM_XXXIII_STATA_Otrosingresos_4.dta", 
drop _merge

sort HOGAR
merge HOGAR using "`base_in'\EPHPM_XXXIII_STATA_Otrosingresos_5.dta", 
drop _merge

sort HOGAR
merge HOGAR using "`base_in'\EPHPM_XXXIII_STATA_Otrosingresos_6.dta", 
drop _merge

sort HOGAR
merge HOGAR using "`base_in'\EPHPM_XXXIII_STATA_Otrosingresos_7.dta", 
drop _merge

sort HOGAR
merge HOGAR using "`base_in'\EPHPM_XXXIII_STATA_Otrosingresos_8.dta", 
drop _merge

sort HOGAR
merge HOGAR using "`base_in'\EPHPM_XXXIII_STATA_Otrosingresos_9.dta", 
drop _merge

sort HOGAR
merge HOGAR using "`base_in'\EPHPM_XXXIII_STATA_Otrosingresos_10.dta", 
drop _merge

sort HOGAR
merge HOGAR using "`base_in'\EPHPM_XXXIII_STATA_Otrosingresos_11.dta", 
drop _merge

sort HOGAR
merge HOGAR using "`base_in'\EPHPM_XXXIII_STATA_Otrosingresos_12.dta", 
drop _merge

sort HOGAR
merge HOGAR using "`base_in'\EPHPM_XXXIII_STATA_Otrosingresos_13.dta", 
drop _merge

sort HOGAR
merge HOGAR using "`base_in'\EPHPM_XXXIII_STATA_Otrosingresos_14.dta", 
drop _merge

sort HOGAR
merge HOGAR using "`base_in'\EPHPM_XXXIII_STATA_Otrosingresos_15.dta", 
drop _merge


local x= "pensi jubi alqui subsi intban remefe remesp pendiv ayuefe ayuesp ayuparef ayupares bonos preslab otrasfu"

foreach i of local x {
label variable `i' "PRG-`i'"
label variable lps_`i' "Valor Declarado en Lempiras - `i'"
label variable dls_`i' "Valor Declarado en Dolares - `i'"
label variable cambio_`i' "Cambio del Hogar - `i'"
label variable dlslps_`i' "Valor en lempiras de lo Declarado en Dls - `i'"
label variable totalps_`i' "Valor Total En Lempiras - `i'"
label variable Ymensual_`i' "Valor En Lempiras Mensual - `i'"
}

rename HOGAR hogar
sort hogar
save "`base_in'\otrosingresos_mod.dta", replace


use "`base_in'\EPHPM_XXXIII_STATA_Personas.dta", clear

* id hogar

sort Hogar
ren Hogar hogar

save "`base_in'\personas_mod.dta", replace


use "`base_in'\EPHPM_XXXIII_STATA_Viviendas.dta", clear

* id hogar

sort Hogar
ren Hogar hogar

save "`base_in'\viviendas_mod.dta", replace
**************************************************

* TODOS LOS MODULOS

use "`base_in'\viviendas_mod.dta", clear

merge hogar using "`base_in'\personas_mod.dta"
tab _merge
drop _merge
sort hogar

merge hogar using "`base_in'\otrosingresos_mod.dta", 
drop _merge

save "`base_out'", replace
forvalues i=1/15 {
erase "`base_in'\EPHPM_XXXIII_STATA_Otrosingresos_`i'.dta"
}

erase "`base_in'\otrosingresos_mod.dta"
erase "`base_in'\personas_mod.dta"
erase "`base_in'\viviendas_mod.dta"
log close


