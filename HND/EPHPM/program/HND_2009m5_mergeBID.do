*Elaborado por: Yessenia Loayza - july 2012
*Modificado por: Daniela Zuluaga-Noviembre 2017

clear

global ruta = "${surveysFolder}"

local PAIS HND
local ENCUESTA EPHPM
local ANO "2009"
local ronda m5 

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\"
local base_out = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
   
capture log close
log using "`log_file'", replace 

*Yessenia Loayza
*Abril, 2013

		***************************************
		*CONFORMACIÓN BASE ÚNICA HONDURAS 2010*
		***************************************
*Nota: se armoniza esta base (mayo) hasta que se obtenga la de septiembre
clear	

**********************************************
**********  OBTENGO LA BASE DE OTROS   *******
********** INGRESOS A NIVEL DE PERSONAS ******
**********        ANTES DEL MERGE       ******
**********************************************

*Base de otros ingresos
clear
use "`base_in'\otrosyngresos.dta", clear
sort  hogar nper

**Daniela Zuluaga-Noviembre 2017: Se modifica el merge dado que había un error en su generación**

**Se crean variables en especie y efectivo**

gen us_cambio=us*cambio

egen ymensual_efectivo=rowtotal(lps us_cambio)
replace ymensual_efectivo=ymensual_efectivo/3

gen use_cambio=us_e*cambio

egen ymensual_especie=rowtotal(lps_e use_cambio)
replace ymensual_especie=ymensual_especie/3

collapse (sum) ymensual_especie ymensual_efectivo,  by (hogar nper prg)

tostring hogar nper, replace force
sort hogar nper
egen idper = concat(hogar nper)
reshape wide ymensual_especie ymensual_efectivo, i(idper) j(prg) 



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
cap	rename ymensual_especie6   int_bancarios_esp
cap	rename ymensual_efectivo6  int_bancarios_efe
cap	rename ymensual_especie7   pens_divorcio_esp
cap	rename ymensual_efectivo7  pens_divorcio_efe
cap	rename ymensual_especie8   ayud_fam_esp
cap	rename ymensual_efectivo8  ayud_fam_efe
cap	rename ymensual_especie9   ayud_part_esp
cap	rename ymensual_efectivo9  ayud_part_efe
cap	rename ymensual_especie10  bonpraf_esp
cap	rename ymensual_efectivo10 bonpraf_efe
cap	rename ymensual_especie11  meresc_esp
cap	rename ymensual_efectivo11 meresc_efe
cap	rename ymensual_especie12  bolspraf_esp
cap	rename ymensual_efectivo12 bolspraf_efe
cap	rename ymensual_especie13  becas_esp
cap	rename ymensual_efectivo13 becas_efe
cap	rename ymensual_especie14  remesaext_esp 
cap	rename ymensual_efectivo14 remesaext_efe
cap	rename ymensual_especie15  otbon_esp
cap	rename ymensual_efectivo15 otbon_efe
cap	rename ymensual_especie16  otros_esp
cap	rename ymensual_efectivo16 otros_efe

drop ymensual_especie17 ymensual_efectivo17

save "`base_in'\otrosyngresos_rshape", replace

***********************************
* 2) ********** MERGE *************
***********************************

use  "`base_in'\hon0509.dta", clear
tostring hogar nper, replace force
sort hogar nper
egen idper = concat(hogar nper)
merge 1:1 idper using "`base_in'\otrosyngresos_rshape.dta"
drop if _merge==2 /*se elimina 0 observacion*/
drop _merge idper

save "`base_out'", replace

log close
