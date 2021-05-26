*Elaborado por: Yessenia Loayza - july 2012
* modificado por Laura Oliveri .9-2013

set more off

clear

global ruta = "${surveysFolder}"

local PAIS HND
local ENCUESTA EPHPM
local ANO "2007"
local ronda m9 

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\"
local base_out = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
   
capture log close
log using "`log_file'", replace 


* Individuals

use "`base_in'\EPHPM_XXXV_STATA_Personas.dta", clear

* id hogar

sort HOGAR

save "`base_in'\personas_mod.dta", replace

* Households

use "`base_in'\EPHPM_XXXV_STATA_Viviendas.dta", clear

sort HOGAR

save "`base_in'\viviendas_mod.dta", replace


* MODULO OTROS INGRESOS

use "`base_in'\EPHPM_XXXV_STATA_Otrosingresos.dta", clear

**Daniela Zuluaga-Noviembre 2017: Se modifica el merge para la creación adecuada de ingreo no monetario (en especie y monetario)**

**Se crean variables en especie y efectivo**
recode LPS(9999999=.)
recode DLS(9999999=.)
recode ELPS(9999999=.)
recode EDLS(9999999=.)


gen us_cambio=DLS*Cambio_

egen ymensual_efectivo=rowtotal(LPS us_cambio),m
replace ymensual_efectivo=ymensual_efectivo/3

gen use_cambio=EDLS*Cambio_

egen ymensual_especie=rowtotal(ELPS use_cambio), m
replace ymensual_especie=ymensual_especie/3

collapse (sum) ymensual_especie ymensual_efectivo,  by (HOGAR NPER PRG)

tostring HOGAR NPER, replace force
sort HOGAR NPER
egen idper = concat(HOGAR NPER)
reshape wide ymensual_especie ymensual_efectivo, i(idper) j(PRG) 


cap	rename ymensual_especie1   pension_esp
cap	rename ymensual_efectivo1  pension_efe
cap	rename ymensual_especie2   jubilacion_esp
cap	rename ymensual_efectivo2  jubilacion_efe
cap	rename ymensual_especie3   alquileres_esp
cap	rename ymensual_efectivo3  alquileres_efe
cap	rename ymensual_especie4   destos_3edad_esp
cap	rename ymensual_efectivo4  destos_3edad_efe
cap	rename ymensual_especie5   subs_enee_esp
cap	rename ymensual_efectivo5  subs_enee_efe
cap	rename ymensual_especie6   bono80_esp
cap	rename ymensual_efectivo6  bono80_efe
cap	rename ymensual_especie7   int_bancarios_esp
cap	rename ymensual_efectivo7  int_bancarios_efe
cap	rename ymensual_especie8   remesaext_esp 
cap	rename ymensual_efectivo8  remesaext_efe
cap	rename ymensual_especie9   pens_divorcio_esp
cap	rename ymensual_efectivo9  pens_divorcio_efe
cap	rename ymensual_especie10  ayud_fam_esp
cap	rename ymensual_efectivo10 ayud_fam_efe 
cap	rename ymensual_especie11  ayud_part_esp
cap	rename ymensual_efectivo11 ayud_part_efe
cap	rename ymensual_especie12  bonpraf_esp
cap	rename ymensual_efectivo12 bonpraf_efe
cap	rename ymensual_especie13  meresc_esp
cap	rename ymensual_efectivo13 meresc_efe
cap	rename ymensual_especie14  bolspraf_esp
cap	rename ymensual_efectivo14 bolspraf_efe
cap	rename ymensual_especie15  becas_esp
cap	rename ymensual_efectivo15 becas_efe
cap	rename ymensual_especie16  otbon_esp
cap	rename ymensual_efectivo16 otbon_efe
cap	rename ymensual_especie17  otros_esp
cap	rename ymensual_efectivo17 otros_efe

drop idper
destring HOGAR NPER, replace
sort HOGAR NPER

save "`base_in'/otrosingresos_mod", replace

* MODULO CALIDAD DE VIDA
use "`base_in'\EPHPM_XXXV_STATA_Calidaddevida.dta", clear
sort HOGAR NPER
save "`base_in'\calidaddevida_mod.dta", replace


** MODULO DE CALIDAD DE VIDA PARTE 1 **
use "`base_in'\EPHPM_XXXV_STATA_Calidaddevida_Seguridad1.dta", clear

gen n=_n
gen four=1 if CV48a==4
sort HOGAR n
by HOGAR, sort: gen N=_n if four==1
by HOGAR, sort: egen NN=min(N) if four==1

tab N
sum CV49 if N==5
sum CV49 if N==6

* Paso Importante
replace CV48a=7 if N==6

label define CV48aa 1"Entraron los ladrones a su casa" ///
		    3"Le robaron el carro o una moto" ///
		    2"Le atacaron con violencia, amenazas o armas para robarle" ///
		    6"ÂżLe han robado. (sin violencia) estando fuera de la casa?" ///
		    4"ÂżLesiones personales?" ///
		    7"ÂżSecuestro?" ///
		    5"ÂżEstafa o chantaje?"
label values CV48a CV48aa
tab CV48a
drop four n N NN

gen text=""
replace text="_a" if CV48a==1
replace text="_b" if CV48a==2
replace text="_c" if CV48a==3
replace text="_d" if CV48a==4
replace text="_e" if CV48a==5
replace text="_f" if CV48a==6
replace text="_g" if CV48a==7

drop CV48a

reshape wide  CV48 CV49 CV50 CV51, i(HOGAR) j(text) string
label drop _all

order  HOGAR CV48_a CV48_b CV48_c CV48_d CV48_e CV48_f CV48_g	///
             CV49_a CV49_b CV49_c CV49_d CV49_e CV49_f CV49_g	///
             CV50_a CV50_b CV50_c CV50_d CV50_e CV50_f CV50_g	///
             CV51_a CV51_b CV51_c CV51_d CV51_e CV51_f CV51_g	
compress
sort HOGAR
save "`base_in'\calidaddevida_seguridad1_mod.dta", replace


** MODULO DE CALIDAD DE VIDA PARTE 2 **
    
use "`base_in'\EPHPM_XXXV_STATA_Calidaddevida_Seguridad2.dta", clear
    
    ** Valores Perdidos
    drop if CV52a==.

* Paso Importante
gen text=""
replace text="_a" if CV52a==2
replace text="_b" if CV52a==1
    
drop CV52a
drop if text==""
reshape wide CV52 CV53 CV54 CV55, i(HOGAR) j(text) string

order HOGAR CV52_a CV52_b CV53_a CV53_b CV54_a CV54_b CV55_a CV55_b
compress
sort HOGAR
save "`base_in'\calidaddevida_seguridad2_mod.dta", replace


* union modulos

** UNION DE MODULOS DE CALIDAD DE VIDA **

use "`base_in'\calidaddevida_mod.dta", clear
sort HOGAR
merge HOGAR using "`base_in'\calidaddevida_seguridad1_mod.dta"
tab _merge
drop _merge
sort HOGAR
merge HOGAR using "`base_in'\calidaddevida_seguridad2_mod.dta"
drop _merge
gen NPER=NPER_CV
sort HOGAR NPER
save "`base_in'\calidaddevida_mod.dta", replace

* TODOS LOS MODULOS

use "`base_in'\viviendas_mod.dta", clear

sort HOGAR

merge HOGAR using "`base_in'\personas_mod.dta"
tab _merge
drop _merge
sort HOGAR NPER


merge HOGAR NPER using "`base_in'\otrosingresos_mod.dta"
tab _merge
drop if _merge==2
drop _merge

sort HOGAR NPER
merge HOGAR NPER using "`base_in'\calidaddevida_mod.dta"
tab _merge
drop _merge
compress


ren HOGAR hogar
ren NPER nper


save "`base_out'", replace
erase "`base_in'\personas_mod.dta"
erase "`base_in'\viviendas_mod.dta"
erase "`base_in'\otrosingresos_mod.dta"
erase "`base_in'\calidaddevida_mod.dta"
erase "`base_in'\calidaddevida_seguridad1_mod.dta"
erase "`base_in'\calidaddevida_seguridad2_mod.dta"
log close
