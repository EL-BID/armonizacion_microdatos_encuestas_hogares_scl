		*************************************************************
		***PROGRAMA PARA PEGAR LOS MÓDULOS DE LA ENFT OCTUBRE 202004***
		*************************************************************
*Elaborado por: Yessenia Loayza - july 2012



clear

global ruta = "${surveysFolder}"

local PAIS HND
local ENCUESTA EPHPM
local ANO "2011"
local ronda m5 

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\"
local base_out = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
   
capture log close
log using "`log_file'", replace 


	
	*Yessenia Loayza
*Abril, 2013

		***************************************
		*CONFORMACIÓN BASE ÚNICA HONDURAS 2011*
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
collapse (sum) ymensual,  by (hogar nper prg)

tostring hogar nper, replace force
sort hogar nper
egen idper = concat(hogar nper)
reshape wide ymensual, i(idper) j(prg) 

cap	rename 	ymensual1	pension
cap	rename 	ymensual2	jubilacion
cap	rename 	ymensual3	alquileres
cap	rename 	ymensual4	destos_tra_edad
cap	rename 	ymensual5	subsidio_enee
cap	rename 	ymensual6	int_bancarios
cap	rename 	ymensual7	pens_divorcio
cap	rename 	ymensual8	pens_divorcio_propor
cap	rename 	ymensual9	ayud_fam_efectivo
cap	rename 	ymensual10	ayudfam_especie
cap	rename 	ymensual11	ayudfam_qper
cap	rename 	ymensual12	ayudpart_efec
cap	rename 	ymensual13	ayudpart_esp
cap	rename 	ymensual14	ayudpart_qper
cap	rename 	ymensual15	remesaext_efec
cap	rename 	ymensual16	remesaext_esp
cap	rename 	ymensual17	ayudinst_efec
cap	rename 	ymensual18	ayudinst_esp
cap	rename 	ymensual19	bonpraf
cap	rename 	ymensual20	merescolar
cap	rename 	ymensual21	bolspraf
cap	rename 	ymensual22	becas
cap	rename 	ymensual23	bontecn
cap	rename 	ymensual24	otrosbon
cap	rename 	ymensual25	otros
save "`base_in'\otrosyngresos_rshape", replace

***********************************
* 2) ********** MERGE *************
***********************************

use  "`base_in'\hogar0511.dta", clear
tostring hogar nper, replace force
sort hogar nper
egen idper = concat(hogar nper)
merge 1:1 idper using "`base_in'\otrosyngresos_rshape.dta"
drop if _merge==2 /*se elimina 6 observaciones*/
drop _merge idper


* Comprime y guarda base
compress
save "`base_out'", replace

log close

