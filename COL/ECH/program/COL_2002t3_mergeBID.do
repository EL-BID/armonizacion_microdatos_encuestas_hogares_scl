*Elaboración: Marcela Rubio (mrubio@iadb.org | marcelarubio28@gmail.com)
*Mayo 12, 2014

*** MERGE COLOMBIA ECH 2002 ****
*------------------------------*

clear
set more off
local anio = 2002
local ruta "\\SDSSRV03\surveys\survey\COL\ECH\\`anio'\"
local m7 ="`ruta'a\data_orig\m7\data_orig\" 
local m8 ="`ruta'a\data_orig\m8\data_orig\" 
local m9 ="`ruta'a\data_orig\m9\data_orig\" 
local dane = "`ruta'a\data_orig\anual_homologado DANE\"
local t3 = "`ruta't3\data_orig\"
local out = "`ruta't3\data_merge\"

*1. Bases anuales con homologacion de ingresos
clear
use "`dane'hogares2002.dta", clear
sort idh_ch
save "`dane'hogares2002.dta", replace
use "`dane'personas 2002.dta", clear
gen idh_ch = llave_hog
sort idh_ch
merge m:1 idh_ch using "`dane'hogares2002.dta"
tab _merge
drop _merge
egen id =concat (llave_viv llave_hog orden)
sort id
destring mes dpto p3-p30 p37 p30a p30b p53 p57, replace
keep if mes>=7 & mes<=9
rename mes mes_c1
save "`dane'COL_`anio't3.dta", replace


*2 - APPEND BASES POR MODULO Y TRIMESTRE
*------------------------------

foreach zona in cabecera resto {

*** EDUCACION

use "`m7'\`zona' - características generales - educación.dta", clear
append using "`m8'\`zona' - características generales - educación.dta"
append using "`m9'\`zona' - características generales - educación.dta"
egen id = concat (llave_viv llave_hog orden)
sort id
saveold "`t3'\`zona' - características generales - educación.dta", replace

*** FUERZA DE TRABAJO

use "`m7'\`zona' - fuerza de trabajo.dta", clear
append using "`m8'\`zona' - fuerza de trabajo.dta"
append using "`m9'\`zona' - fuerza de trabajo.dta"
egen id = concat (llave_viv llave_hog orden)
sort id
saveold "`t3'\`zona' - fuerza de trabajo.dta", replace

*** OCUPADOS EMPLEO PRINCIPAL

use "`m7'\`zona' - ocupados empleo principal.dta", clear
append using "`m8'\`zona' - ocupados empleo principal.dta"
append using "`m9'\`zona' - ocupados empleo principal.dta"
egen id = concat (llave_viv llave_hog orden)
sort id
saveold "`t3'\`zona' - ocupados empleo principal.dta", replace

*** OCUPADOS SUBEMPLEO

use "`m7'\`zona' - ocupados - subempleo.dta", clear
append using "`m8'\`zona' - ocupados - subempleo.dta"
append using "`m9'\`zona' - ocupados - subempleo.dta"
egen id = concat (llave_viv llave_hog orden)
sort id
saveold "`t3'\`zona' - ocupados - subempleo.dta", replace

*** OCUPADOS EMPLEO SECUNDARIOS

use "`m7'\`zona' - ocupados - empleo secundario.dta", clear
append using "`m8'\`zona' - ocupados - empleo secundario.dta"
append using "`m9'\`zona' - ocupados - empleo secundario.dta"
egen id = concat (llave_viv llave_hog orden)
sort id
saveold "`t3'\`zona' - ocupados - empleo secundario.dta", replace

*** DESOCUPADOS 

use "`m7'\`zona' - desocupados.dta", clear
append using "`m8'\`zona' - desocupados.dta"
append using "`m9'\`zona' - desocupados.dta"
egen id = concat (llave_viv llave_hog orden)
sort id
saveold "`t3'\`zona' - desocupados.dta", replace

*** INACTIVOS

use "`m7'\`zona' - inactivos.dta", clear
append using "`m8'\`zona' - inactivos.dta"
append using "`m9'\`zona' - inactivos.dta"
egen id = concat (llave_viv llave_hog orden)
sort id
saveold "`t3'\`zona' - inactivos.dta", replace
}

*3 - Merge de los diferentes modulos trimestrales por zona
*-----------------------------------

foreach zona in cabecera resto {

use "`t3'\`zona' - características generales - educación.dta", clear
merge 1:1 id using "`t3'\`zona' - fuerza de trabajo.dta"
drop _merge
sort id

merge 1:1 id using "`t3'\`zona' - ocupados empleo principal.dta"
tab _merge
drop _merge
sort id

merge 1:1 id using "`t3'\`zona' - ocupados - subempleo.dta"
drop _merge
sort id

merge 1:1 id using "`t3'\`zona' - ocupados - empleo secundario.dta"
drop _merge
sort id

merge 1:1 id using "`t3'\`zona' - desocupados.dta"
drop _merge
sort id

merge 1:1 id using "`t3'\`zona' - inactivos.dta"
drop _merge
sort id

saveold "`out'\COL_`anio't3`zona'.dta", replace
}

*4. Append zonas
*---------------

clear
use "`out'COL_`anio't3cabecera.dta"
append using "`out'COL_`anio't3resto.dta"
destring p3-p10u dpto p11-p64, replace
sort id

*5. Merge con base de ingresos homologados
merge 1:1 id using "`dane'COL_`anio't3.dta"
tab _merge
drop _merge

replace fex_c_2011=fex_c_2011/3
label var fex_c_2011 "factor de expansion anual dividido 3"

saveold "`out'COL_`anio't3.dta", replace
