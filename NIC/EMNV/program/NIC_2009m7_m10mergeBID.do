				*****************************************
				***PEGANDO LOS MÓDULOS DE LA EMNV 2009***
				*****************************************
				
cd "Y:\Nicaragua\2009\Datos Originales\"

*Ordenando las bases por sus llaves:

use "emnv09_01_datos_de_la_caratula.dta", clear
sort  i00
save, replace

use emnv09_02_datos_de_la_vivienda_y_el_hogar.dta, clear
sort  i00
save, replace

use consumo4_2009.dta, clear
sort  i00
save, replace

use ingresos_final_2009.dta, clear
sort  i00
save, replace


use emnv09_04_poblacion.dta, clear
sort  i00 s2p00
save, replace

*Uniendo las bases:

merge i00 using emnv09_02_datos_de_la_vivienda_y_el_hogar.dta
tab _merge
drop _merge
sort  i00

merge i00 using consumo4_2009.dta
tab _merge
drop _merge
sort  i00

merge i00 using ingresos_final_2009.dta
tab _merge
drop _merge
sort  i00


merge i00 using "emnv09_01_datos_de_la_caratula.dta"
tab _merge
tab resultado
drop if _merge==2
drop _merge
tab resultado
keep if resultado==9

save "Y:\Nicaragua\2009\Data\nic09.dta", replace
save "X:\ARM\NIC\2009\Orig_data\nic09.dta", replace
