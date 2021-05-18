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

gen y=""

replace y="_1" if PRG== 1 // Pension
replace y="_2" if PRG== 2 // Jubilacion
replace y="_3" if PRG== 3 // Alquileres
replace y="_4" if PRG== 4 // Descuentos x la Tercera edad
replace y="_5" if PRG== 5 // Subsidio de la ENEE
replace y="_6" if PRG== 6 // Bono 80 (ENEE)
replace y="_7" if PRG== 7 // Intereses Bancarios
replace y="_8" if PRG== 8 // Remesas del Exterior
replace y="_9" if PRG== 9 // Pension por Divorcio
replace y="_10" if PRG==10 // Ayudas Familiares
replace y="_11" if PRG==11 // Ayudas Particulares
replace y="_12" if PRG==12 // Bonos del PRAF
replace y="_13" if PRG==13 // Merienda Escolar
replace y="_14" if PRG==14 // Bolson del PRAF
replace y="_15" if PRG==15 // Becas
replace y="_16" if PRG==16 // Otros Bonos
replace y="_17" if PRG==17 // Otros

drop PRG

reshape wide LPS - Ymensual, i(HOGAR NPER) j(y) string
compress 

sort HOGAR NPER

order  	LPS_1  DLS_1   ELPS_1  EDLS_1  AH_1  AM_1  Cambio__1  DlsLps_1  TotaLps_1  Ymensual_1 	///
	LPS_2  DLS_2   ELPS_2  EDLS_2  AH_2  AM_2  Cambio__2  DlsLps_2  TotaLps_2  Ymensual_2 	///
	LPS_3  DLS_3   ELPS_3  EDLS_3  AH_3  AM_3  Cambio__3  DlsLps_3  TotaLps_3  Ymensual_3 	///
	LPS_4  DLS_4   ELPS_4  EDLS_4  AH_4  AM_4  Cambio__4  DlsLps_4  TotaLps_4  Ymensual_4 	///
	LPS_5  DLS_5   ELPS_5  EDLS_5  AH_5  AM_5  Cambio__5  DlsLps_5  TotaLps_5  Ymensual_5 	///
	LPS_6  DLS_6   ELPS_6  EDLS_6  AH_6  AM_6  Cambio__6  DlsLps_6  TotaLps_6  Ymensual_6 	///
	LPS_7  DLS_7   ELPS_7  EDLS_7  AH_7  AM_7  Cambio__7  DlsLps_7  TotaLps_7  Ymensual_7 	///
	LPS_8  DLS_8   ELPS_8  EDLS_8  AH_8  AM_8  Cambio__8  DlsLps_8  TotaLps_8  Ymensual_8 	///
	LPS_9  DLS_9   ELPS_9  EDLS_9  AH_9  AM_9  Cambio__9  DlsLps_9  TotaLps_9  Ymensual_9 	///
	LPS_10 DLS_10  ELPS_10 EDLS_10 AH_10 AM_10 Cambio__10 DlsLps_10 TotaLps_10 Ymensual_10	///
	LPS_11 DLS_11  ELPS_11 EDLS_11 AH_11 AM_11 Cambio__11 DlsLps_11 TotaLps_11 Ymensual_11	///
	LPS_12 DLS_12  ELPS_12 EDLS_12 AH_12 AM_12 Cambio__12 DlsLps_12 TotaLps_12 Ymensual_12	///
	LPS_13 DLS_13  ELPS_13 EDLS_13 AH_13 AM_13 Cambio__13 DlsLps_13 TotaLps_13 Ymensual_13	///
	LPS_14 DLS_14  ELPS_14 EDLS_14 AH_14 AM_14 Cambio__14 DlsLps_14 TotaLps_14 Ymensual_14	///
	LPS_15 DLS_15  ELPS_15 EDLS_15 AH_15 AM_15 Cambio__15 DlsLps_15 TotaLps_15 Ymensual_15	///
	LPS_16 DLS_16  ELPS_16 EDLS_16 AH_16 AM_16 Cambio__16 DlsLps_16 TotaLps_16 Ymensual_16	///
	LPS_17 DLS_17  ELPS_17 EDLS_17 AH_17 AM_17 Cambio__17 DlsLps_17 TotaLps_17 Ymensual_17	


foreach xxx in ///
        LPS_1  DLS_1   ELPS_1  EDLS_1  AH_1  AM_1  Cambio__1  DlsLps_1  TotaLps_1  Ymensual_1 	///
	LPS_2  DLS_2   ELPS_2  EDLS_2  AH_2  AM_2  Cambio__2  DlsLps_2  TotaLps_2  Ymensual_2 	///
	LPS_3  DLS_3   ELPS_3  EDLS_3  AH_3  AM_3  Cambio__3  DlsLps_3  TotaLps_3  Ymensual_3 	///
	LPS_4  DLS_4   ELPS_4  EDLS_4  AH_4  AM_4  Cambio__4  DlsLps_4  TotaLps_4  Ymensual_4 	///
	LPS_5  DLS_5   ELPS_5  EDLS_5  AH_5  AM_5  Cambio__5  DlsLps_5  TotaLps_5  Ymensual_5 	///
	LPS_6  DLS_6   ELPS_6  EDLS_6  AH_6  AM_6  Cambio__6  DlsLps_6  TotaLps_6  Ymensual_6 	///
	LPS_7  DLS_7   ELPS_7  EDLS_7  AH_7  AM_7  Cambio__7  DlsLps_7  TotaLps_7  Ymensual_7 	///
	LPS_8  DLS_8   ELPS_8  EDLS_8  AH_8  AM_8  Cambio__8  DlsLps_8  TotaLps_8  Ymensual_8 	///
	LPS_9  DLS_9   ELPS_9  EDLS_9  AH_9  AM_9  Cambio__9  DlsLps_9  TotaLps_9  Ymensual_9 	///
	LPS_10 DLS_10  ELPS_10 EDLS_10 AH_10 AM_10 Cambio__10 DlsLps_10 TotaLps_10 Ymensual_10	///
	LPS_11 DLS_11  ELPS_11 EDLS_11 AH_11 AM_11 Cambio__11 DlsLps_11 TotaLps_11 Ymensual_11	///
	LPS_12 DLS_12  ELPS_12 EDLS_12 AH_12 AM_12 Cambio__12 DlsLps_12 TotaLps_12 Ymensual_12	///
	LPS_13 DLS_13  ELPS_13 EDLS_13 AH_13 AM_13 Cambio__13 DlsLps_13 TotaLps_13 Ymensual_13	///
	LPS_14 DLS_14  ELPS_14 EDLS_14 AH_14 AM_14 Cambio__14 DlsLps_14 TotaLps_14 Ymensual_14	///
	LPS_15 DLS_15  ELPS_15 EDLS_15 AH_15 AM_15 Cambio__15 DlsLps_15 TotaLps_15 Ymensual_15	///
	LPS_16 DLS_16  ELPS_16 EDLS_16 AH_16 AM_16 Cambio__16 DlsLps_16 TotaLps_16 Ymensual_16	///
	LPS_17 DLS_17  ELPS_17 EDLS_17 AH_17 AM_17 Cambio__17 DlsLps_17 TotaLps_17 Ymensual_17	{
	
	rename `xxx' p122_`xxx'
	}

forval i=1/17 {
gen p122_dls2_`i' = p122_DLS_`i' * p122_Cambio__`i'
}

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
		    6"żLe han robado. (sin violencia) estando fuera de la casa?" ///
		    4"żLesiones personales?" ///
		    7"żSecuestro?" ///
		    5"żEstafa o chantaje?"
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
sort HOGAR

sort HOGAR NPER
merge HOGAR NPER using "`base_in'\otrosingresos_mod.dta"
drop if _merge==2 /*se elimina 0 observacion*/
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
