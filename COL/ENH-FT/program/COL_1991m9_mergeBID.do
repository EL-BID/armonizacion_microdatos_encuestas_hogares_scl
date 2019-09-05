*Elaboración: ?
*Actualizacion y cambio de rutas: Yessenia Loayza (desloay@hotmail.com | yessenial@iadb.org)
*Noviembre, 2013

*** MERGE COLOMBIA 1991 ****
*------------------------*

* - Conversion Bases de datos


clear
set more off
local ruta ="\\Sdssrv03\surveys\"
local in   ="`ruta'survey\COL\ENH-FT\1991\m9\data_orig\"
local out  ="`ruta'\survey\COL\ENH-FT\1991\m9\data_merge\"
             
/* clear
*** VIVIENDA
infile using "`in'col91_T00C.dct"
keep if ca_TIPOD=="0"
sort  ca_VIVI 

gen uno=1
bysort ca_VIVI: gen po=sum(uno) 
drop if po>1

save `in'colc91vivi.dta,  replace
clear

*** HOGAR
infile using "`in'col91_H00C.dct"
keep if ca_TIPOD=="0"
sort ca_VIVI 
save `in'colc91hoga.dta,  replace
clear

*** caracteristicas generales (EDUCA, LABORES MENORES, FECUNDIDAD)
infile using  "`in'col91_T01C.dct"
keep if ca_TIPOD=="1"
sort  ca_IDENT ca_009
save `in'colc91edu.dta,  replace
clear

*** FUERZA DE TRABAJO
infile using  "`in'col91_T05C.dct"
keep if ca_TIPOD=="5"
sort  ca_IDENT ca_009
save `in'colc91ft.dta,  replace
clear


*** OCUPADOS EMPLEO PRINCIPAL
infile using  "`in'col91_T06C.dct"
keep if ca_TIPOD=="6"
sort  ca_IDENT ca_009
save `in'colc91ocup.dta,  replace
clear

*** DESOCUPADOS 
infile using  "`in'col91_T07C.dct"
keep if ca_TIPOD=="7"
sort  ca_IDENT ca_009
save `in'colc91deso.dta,  replace
clear

*** INACTIVOS 
infile using  "`in'col91_T08C.dct"
keep if ca_TIPOD=="8"
sort  ca_IDENT ca_009
save `in'colc91inac.dta,  replace
*/

*2 - Merge bases de datos
*** NOW WE MERGE THE FILES

clear
use `in'colc91hoga.dta, clear
merge ca_VIVI  using `in'colc91vivi.dta, uniqusing
rename _merge merge_viv
tab merge_viv, m

egen ca_IDENT = concat(ca_VIVI ca_HOGAR)
sort ca_IDENT
save `in'temp.dta, replace 

use `in'colc91edu.dta 
sort  ca_IDENT ca_009
merge ca_IDENT using `in'temp.dta
rename _merge merge_temp
tab merge_temp,m

/* Se  pierden 160 observaciones....ponderadas  solo son 408...
LAS BORRO con el comando: drop if ca_TIPOD=="0" del final del programa
tab  _merge [fw=fex]

     _merge |      Freq.     Percent        Cum.
------------+-----------------------------------
          2 |        408        0.00        0.00
          3 | 20,448,380      100.00      100.00
------------+-----------------------------------
      Total | 20,448,788      100.00
*/

sort  ca_IDENT ca_009
merge ca_IDENT ca_009 using `in'colc91ft.dta
rename _merge mergeft
tab mergeft

sort  ca_IDENT ca_009
merge ca_IDENT ca_009 using `in'colc91ocup.dta
rename _merge mergeocup
tab mergeocup


sort  ca_IDENT ca_009
merge ca_IDENT ca_009 using `in'colc91deso.dta
rename _merge mergedeso
tab mergedeso

sort  ca_IDENT ca_009
merge ca_IDENT ca_009 using `in'colc91inac.dta
rename _merge mergeinac
tab mergeinac

drop if ca_TIPOD=="0"
drop merge*
gen factor_ci=real(FEX_DANE)
drop FEX_DANE

saveold "`out'COL_1991m9_cabecera.dta",  replace


*AREA RESTO
*----------*

*- Conversion Bases de datos

/* clear
*** VIVIENDA
infile using "`in'col91_T01R.dct"
keep if re_TIPOD=="1"
sort  re_VIVI
gen uno=1
bysort re_VIVI: gen po=sum(uno) 
drop if po>1
save `in'colr91vivi.dta,  replace
clear

*** HOGAR
infile using "`in'col91_H01R.dct"
keep if re_TIPOD=="1"
sort  re_VIVI re_HOGA
save `in'colr91hoga.dta,  replace
clear

*** ACTIVIDAD PRODUCTIVA DEL HOGAR
infile using "`in'col91_T08R.dct"
keep if re_TIPOD=="8"
sort  re_VIVI re_HOGA
save `in'colr91hogapro.dta,  replace
clear

*** ENERGIA 
infile using "`in'col91_T09R.dct"
keep if re_TIPOD=="9"
sort  re_VIVI re_HOGA
save `in'colr91ener.dta,  replace
clear

*** caracteristicas generales (EDUCA, LABORES MENORES, FECUNDIDAD)
infile using  "`in'col91_T02R.dct"
keep if re_TIPOD=="2"
sort  re_IDENT re_009
save `in'colr91edu.dta,  replace
clear

*** FUERZA DE TRABAJO
infile using  "`in'col91_T03R.dct"
keep if re_TIPOD=="3"
sort  re_IDENT re_009
save `in'colr91ft.dta,  replace
clear

*** OCUPADOS EMPLEO PRINCIPAL
infile using  "`in'col91_T04R.dct"
keep if re_TIPOD=="4"
sort  re_IDENT re_009
save `in'colr91ocup.dta,  replace
clear

*** OCUPADOS EMPLEO CONTINUACION
infile using  "`in'col91_T05R.dct"
keep if re_TIPOD=="5"
sort  re_IDENT re_009
save `in'colr91ocupc.dta,  replace
clear

*** DESOCUPADOS 
infile using  "`in'col91_T06R.dct"
keep if re_TIPOD=="6"
sort  re_IDENT re_009
save `in'colr91deso.dta,  replace
clear

*** ACTIVIDAD SECUNDARIA  
infile using  "`in'col91_T07R.dct"
keep if re_TIPOD=="7"
sort  re_IDENT re_009
save `in'colr91sec.dta,  replace
*/


*- Merge bases de datos
clear
*** NOW WE MERGE THE FILES
use `in'colr91hoga.dta, clear
merge re_VIVI re_HOGA using `in'colr91hogapro.dta
rename _merge merge_hogapro
tab merge_hogapro, m

sort re_VIVI re_HOGA
merge re_VIVI re_HOGA using `in'colr91ener.dta
rename _merge merge_ener
tab merge_ener, m

sort re_VIVI 
merge re_VIVI  using `in'colr91vivi.dta, uniqusing
rename _merge merge_viv
tab merge_viv, m
egen re_IDENT = concat(re_VIVI re_HOGA)
sort re_IDENT
save `in'tempr.dta, replace 

use `in'colr91edu.dta 
merge re_IDENT using `in'tempr.dta
rename _merge merge_temp
tab merge_temp

sort  re_IDENT re_009
merge re_IDENT re_009 using `in'colr91ft.dta
rename _merge mergeft
tab mergeft

sort  re_IDENT re_009
merge re_IDENT re_009 using `in'colr91ocup.dta
rename _merge mergeocup
tab mergeocup

sort  re_IDENT re_009
merge re_IDENT re_009 using `in'colr91ocupc.dta
rename _merge mergeocupc
tab mergeocupc

sort  re_IDENT re_009
merge re_IDENT re_009 using `in'colr91deso.dta
rename _merge mergedeso
tab mergedeso

sort  re_IDENT re_009
merge re_IDENT re_009 using `in'colr91sec.dta
rename _merge mergesec
tab mergesec

drop merge*
gen factor_ci=real(FEX_DANE)
drop FEX_DANE

saveold "`out'COL_1991m9_resto.dta",  replace





