*Activar el global cuando se corre por separado este do file.


global ruta = "Y:\Bolivia\2008\EH/Data"
clear

set mem 400m
cd "X:\ARM\BOL\2008\Orig_data"

use "$ruta/eh08_poblacion.dta", clear
destring folio, replace
	sort folio nroper
	save xxx.dta, replace

use "$ruta/eh08_vivienda.dta", clear
	sort folio
	drop _merge
	merge folio using xxx.dta
	tab _merge
	more
	drop _merge
erase xxx.dta


save bol08.dta, replace
save "$ruta\bol08.dta", replace
