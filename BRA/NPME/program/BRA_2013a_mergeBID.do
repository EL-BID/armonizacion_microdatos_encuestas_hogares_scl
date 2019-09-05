*Elaboración: María Laura Oliveri (mloliveri@iadb.org / lauraoliveri@yahoo.com)
*Diciembre, 2015


global ruta = "\\Sdssrv03\surveys"

*bases originales (mensuales)
local base_in = "$ruta\survey\BRA\NPME"
local anio = 2013
local base_out = "$ruta\survey\BRA\NPME/`anio'/a\data_merge\BRA_`anio'a.dta"

*** Append BRASIL (une bases mensuales) ****
*------------------------*

*1 - Append 
clear
set more off
use "`base_in'/`anio'/m01/data_orig/PMEnova.01`anio'.dta"
gen mes_c=1   

local mes "02 03 04 05 06 07 08 09 10 11 12"
foreach i of local mes {

append using "`base_in'/`anio'/m`i'/data_orig/PMEnova.`i'`anio'.dta", force
replace mes_c=`i' if mes_c==.   

}

gen anio_c = `anio'

gen trimestre_c=1 if mes_c==1 | mes_c==2 | mes_c==3
replace trimestre_c=2 if mes_c==4 | mes_c==5 | mes_c==6
replace trimestre_c=3 if mes_c==7 | mes_c==8 | mes_c==9
replace trimestre_c=4 if mes_c==10 | mes_c==11 | mes_c==12
saveold "`base_out'", version(12) replace

