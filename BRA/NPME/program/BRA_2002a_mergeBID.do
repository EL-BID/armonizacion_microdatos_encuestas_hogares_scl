*Elaboración: María Laura Oliveri (mloliveri@iadb.org / lauraoliveri@yahoo.com)
*Diciembre, 2015


global ruta = "\\Sdssrv03\surveys"

*bases originales (mensuales)
local base_in = "$ruta\survey\BRA\NPME"
local base_out = "$ruta\survey\BRA\NPME/2002/a\data_merge\BRA_2002a.dta"

local anio = 2002

*** Append BRASIL (une bases mensuales) ****
*------------------------*

*1 - Append 
clear
set more off
use "`base_in'/`anio'/m01/data_orig/PME20021.dta" /*esta base no se descargó de la web*/
foreach v of varlist _all {
      capture rename `v' `=lower("`v'")'
   }
   gen mes_c=1   
     destring v035 v040 v050 v055 v063 v070 v075, replace
save  "`base_in'/`anio'/m01/data_orig/PME20021_aux.dta", replace
   
use "`base_in'/`anio'/m02/data_orig/PME20022.dta"   /*esta base no se descargó de la web*/
gen mes_c=2 
foreach v of varlist _all {
      capture rename `v' `=lower("`v'")'
   }
   destring v035 v040 v050 v055 v063 v070 v075, replace
save     "`base_in'/`anio'/m02/data_orig/PME20022_aux.dta", replace

use   "`base_in'/`anio'/m01/data_orig/PME20021_aux.dta"
append using  "`base_in'/`anio'/m02/data_orig/PME20022_aux.dta"
   

local mes "03 04 05 06 07 08 09 10 11 12"
foreach i of local mes {

append using "`base_in'/`anio'/m`i'/data_orig/PMEnova.`i'`anio'.dta"
replace mes_c=`i' if mes_c==.   

}

gen anio_c = `anio'

gen trimestre_c=1 if mes_c==1 | mes_c==2 | mes_c==3
replace trimestre_c=2 if mes_c==4 | mes_c==5 | mes_c==6
replace trimestre_c=3 if mes_c==7 | mes_c==8 | mes_c==9
replace trimestre_c=4 if mes_c==10 | mes_c==11 | mes_c==12
saveold "`base_out'", replace

erase  "`base_in'/`anio'/m01/data_orig/PME20021_aux.dta" 
erase  "`base_in'/`anio'/m02/data_orig/PME20022_aux.dta"
