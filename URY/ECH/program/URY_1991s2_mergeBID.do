
* (Versión Stata 12)
clear
set more off
*________________________________________________________________________________________________________________*

 * Activar si es necesario (dejar desactivado para evitar sobreescribir la base y dejar la posibilidad de 
 * utilizar un loop)
 * Los datos se obtienen de las carpetas que se encuentran en el servidor: ${surveysFolder}
 * Se tiene acceso al servidor únicamente al interior del BID.
 * El servidor contiene las bases de datos MECOVI.
 *________________________________________________________________________________________________________________*
 global ruta = "${surveysFolder}\survey\URY\ECH\1991\s2\data_orig"

local PAIS URY
local ENCUESTA ECH
local ANO "1991"
local ronda s2 


local log_file = "${surveysFolder}\harmonized\\`PAIS'\\`ENCUESTA'\\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"

                                                    
capture log close
log using "`log_file'", replace 

/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Uruguay
Encuesta: ECH
Round: s2
Autores: 
Última versión: Mayra Sáenz E-mail: mayras@iadb.org / saenzmayra.a@gmail.com
Fecha última modificación: 30 de Octubre de 2013

							SCL/LMK - IADB
****************************************************************************/
/***************************************************************************
Detalle de procesamientos o modificaciones anteriores:

****************************************************************************/
*HOGARES
import excel "$ruta\h1991s1i.xls", sheet("h1991s1i") firstrow

sort ha0
by ha0 : assert _N==1
gen id_hogar = ha0
gen region=2
la def region 1"Montevideo" 2"Interior"
la val region region
compress
sort id_hogar
save "$ruta\h1991s1i.dta", replace

clear all
import excel "$ruta\h1991s1m.xls", sheet("h1991s1m") firstrow
sort ha0
by ha0 : assert _N==1
gen  id_hogar=ha0
gen region=1
compress
sort id_hogar
save "$ruta\h1991s1m.dta", replace

clear all
import excel "$ruta\h1991s2i.xls", sheet("h1991s2i") firstrow
sort ha0
by ha0 : assert _N==1
gen id_hogar=ha0
gen region=2
compress
sort id_hogar
save "$ruta\h1991s2i.dta", replace

clear all
import excel "$ruta\h1991s2m.xls", sheet("h1991s2m") firstrow
sort ha0
by ha0 : assert _N==1
gen id_hogar=ha0
gen region=1
compress
sort id_hogar
save "$ruta\h1991s2m.dta", replace

*PERSONAS

clear all
import excel "$ruta\p1991s1i.xls", sheet("p1991s1i") firstrow
gen  id_hogar=ha0
gen  id_pers=pe1
sort id_hogar id_pers
by id_hogar id_pers : assert _N==1
gen region=2
compress
save "$ruta\p1991s1i.dta", replace



clear all
import excel "$ruta\p1991s1m.xls", sheet("p1991s1m") firstrow
gen  id_hogar=ha0
gen  id_pers=pe1
sort id_hogar id_pers
by id_hogar id_pers : assert _N==1
gen region=1
compress
save "$ruta\p1991s1m.dta", replace


clear all
import excel "$ruta\p1991s2i.xls", sheet("p1991s2i") firstrow
gen  id_hogar=ha0
gen  id_pers=pe1
sort id_hogar id_pers
by id_hogar id_pers : assert _N==1
gen region=2
compress
save "$ruta\p1991s2i.dta", replace


clear all
import excel "$ruta\p1991s2m.xls", sheet("p1991s2m") firstrow
gen  id_hogar=ha0
gen  id_pers=pe1
sort id_hogar id_pers
by id_hogar id_pers : assert _N==1
gen region=1
compress
save "$ruta\p1991s2m.dta", replace

**************************.
**************************.
clear all
*tempfile temp1 temp2 temp3 temp4
use "$ruta\h1991s1i.dta", clear
merge m:m id_hogar using "$ruta\p1991s1i.dta"
drop _merge
sort id_hogar id_pers
save "$ruta\\temp1.dta",replace

clear all
use "$ruta\h1991s1m.dta", clear
merge id_hogar using "$ruta\p1991s1m.dta",
drop _merge
sort id_hogar id_pers
save "$ruta\\temp2.dta",replace

clear all
use "$ruta\h1991s2i.dta", clear
merge id_hogar using "$ruta\p1991s2i.dta"
drop _merge
sort id_hogar id_pers
save "$ruta\\temp3.dta",replace

clear all
use "$ruta\h1991s2m.dta", clear
merge id_hogar using "$ruta\p1991s2m.dta"
drop _merge
sort id_hogar id_pers
save "$ruta\\temp4.dta",replace

clear all
use "$ruta\\temp1.dta", clear

local i=2
while `i'<5 {
merge m:m id_hogar id_pers using "$ruta\\temp`i'.dta"
ta _merge
drop _merge
sort id_hogar id_pers
local i=`i'+1
}
* Activar solo si es necesario
*keep *_ci  *_c  idh_ch 
compress


*do ruta\labelsBID.do, modify

saveold "${surveysFolder}\survey\URY\ECH\1991\s2\data_merge\\URY_1991s2.dta"


log close


