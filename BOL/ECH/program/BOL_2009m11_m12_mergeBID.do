*Activar el global cuando se corre por separado este do file.


global ruta = "${surveysFolder}\Bolivia\2009\EH\Data"
clear

cd "${surveysFolder}\ARM\BOL\2009\Orig_data"
set mem 400m


use "$ruta/eh09_poblacion corr cód..dta", clear
ren  nro nroper
destring folio, replace
	sort folio nroper
	save xxx.dta, replace

use "$ruta/eh09_vivienda.dta", clear
	sort folio
	merge folio using xxx.dta
	tab _merge
	more
	drop _merge
erase xxx.dta
save bol09.dta, replace
save "$ruta/bol09.dta", replace
