*08/24/11
*Yanira Oviedo

					****************
					***MERGE 2008***
					****************
					
cd "Z:\Panama\2008\EPH\Datos originales"

use hogar.dta, clear
sort prov dist corre estra unidad cuest hogar
save, replace

use vivienda.dta, clear
sort prov dist corre estra unidad cuest 
save, replace

use persona.dta, clear
sort prov dist corre estra unidad cuest hogar
merge prov dist corre estra unidad cuest hogar using hogar.dta
tab _merge
drop _merge

sort prov dist corre estra unidad cuest hogar
merge prov dist corre estra unidad cuest using vivienda.dta
tab _merge
drop _merge

save pan08_eph.dta, replace
