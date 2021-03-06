	***********************************************
	*** CONFORMACION BASE TERCER TRIMESTRE 2008 ***
	*********         RESTO         ***************
	***********************************************


*************************
**BASE DEL MES DE JULIO**
*************************

clear

cd "${surveysFolder}\Colombia\2008\GEIH\Datos originales\0807\RESTO\"


*Pegando los archivos de personas*
**********************************

clear 
use desocupados7.dta
sort directorio secuencia_p orden
save, replace

clear 
use inactivos7.dta
sort directorio secuencia_p orden
save, replace

clear 
use ocupados7.dta
sort directorio secuencia_p orden
save, replace

clear 
use otrosingresos7.dta
sort directorio secuencia_p orden
save, replace

clear 
use otrasactividades7.dta
sort directorio secuencia_p orden
save, replace

clear
use ftrab7.dta
sort directorio secuencia_p orden
save, replace

clear
use educacion7.dta    
sort directorio secuencia_p orden
save, replace

merge directorio secuencia_p orden using ftrab7.dta
tab _merge
drop _merge
sort directorio secuencia_p orden 
saveold "${surveysFolder}\Colombia\2008\GEIH\Datos\july08_res.dta", replace

merge directorio secuencia_p orden using otrasactividades7.dta
tab _merge
drop _merge
sort directorio secuencia_p orden 
saveold "${surveysFolder}\Colombia\2008\GEIH\Datos\july08_res.dta", replace

merge directorio secuencia_p orden using otrosingresos7.dta
tab _merge
drop _merge
sort directorio secuencia_p orden
saveold "${surveysFolder}\Colombia\2008\GEIH\Datos\july08_res.dta", replace

merge directorio secuencia_p orden using ocupados7.dta
tab _merge
drop _merge
sort directorio secuencia_p orden
saveold "${surveysFolder}\Colombia\2008\GEIH\Datos\july08_res.dta", replace

merge directorio secuencia_p orden using inactivos7.dta
tab _merge
drop _merge
sort directorio secuencia_p orden
saveold "${surveysFolder}\Colombia\2008\GEIH\Datos\july08_res.dta", replace

merge directorio secuencia_p orden using desocupados7.dta
tab _merge
drop _merge
sort directorio secuencia_p orden
saveold "${surveysFolder}\Colombia\2008\GEIH\Datos\july08_res.dta", replace




*Pegando el archivo de vivienda y hogar*
****************************************

clear
use vivienda7.dta
sort directorio secuencia_p  
save, replace

use "${surveysFolder}\Colombia\2008\GEIH\Datos\july08_res.dta"
sort directorio secuencia_p 
merge directorio secuencia_p  using vivienda7.dta
tab _merge 
drop _merge
compress
saveold "${surveysFolder}\Colombia\2008\GEIH\Datos\july08_res.dta", replace




**************************
**BASE DEL MES DE AGOSTO**
**************************

clear

cd "${surveysFolder}\Colombia\2008\GEIH\Datos originales\0808\RESTO\"


*Pegando los archivos de personas*
**********************************

clear 
use desocupados8.dta
sort directorio secuencia_p orden
save, replace

clear 
use inactivos8.dta
sort directorio secuencia_p orden
save, replace

clear 
use ocupados8.dta
sort directorio secuencia_p orden
save, replace

clear 
use otrosingresos8.dta
sort directorio secuencia_p orden
save, replace

clear 
use otrasactividades8.dta
sort directorio secuencia_p orden
save, replace

clear
use ftrab8.dta
sort directorio secuencia_p orden
save, replace

clear
use educacion8.dta    
sort directorio secuencia_p orden
save, replace

merge directorio secuencia_p orden using ftrab8.dta
tab _merge
drop _merge
sort directorio secuencia_p orden 
saveold "${surveysFolder}\Colombia\2008\GEIH\Datos\august08_res.dta", replace


merge directorio secuencia_p orden using otrasactividades8.dta
tab _merge
drop _merge
sort directorio secuencia_p orden 
saveold "${surveysFolder}\Colombia\2008\GEIH\Datos\august08_res.dta", replace

merge directorio secuencia_p orden using otrosingresos8.dta
tab _merge
drop _merge
sort directorio secuencia_p orden
saveold "${surveysFolder}\Colombia\2008\GEIH\Datos\august08_res.dta", replace

merge directorio secuencia_p orden using ocupados8.dta
tab _merge
drop _merge
sort directorio secuencia_p orden
saveold "${surveysFolder}\Colombia\2008\GEIH\Datos\august08_res.dta", replace

merge directorio secuencia_p orden using inactivos8.dta
tab _merge
drop _merge
sort directorio secuencia_p orden
saveold "${surveysFolder}\Colombia\2008\GEIH\Datos\august08_res.dta", replace

merge directorio secuencia_p orden using desocupados8.dta
tab _merge
drop _merge
sort directorio secuencia_p orden
saveold "${surveysFolder}\Colombia\2008\GEIH\Datos\august08_res.dta", replace


*Pegando el archivo de vivienda y hogar*
****************************************

clear
use vivienda8.dta
sort directorio secuencia_p  
save, replace

use "${surveysFolder}\Colombia\2008\GEIH\Datos\august08_res.dta"
sort directorio secuencia_p 
merge directorio secuencia_p  using vivienda8.dta
tab _merge 
drop _merge
compress
saveold "${surveysFolder}\Colombia\2008\GEIH\Datos\august08_res.dta", replace
clear


******************************
**BASE DEL MES DE SEPTIEMBRE**
******************************

clear

cd "${surveysFolder}\Colombia\2008\GEIH\Datos originales\0809\RESTO\"


*Pegando los archivos de personas*
**********************************

clear 
use desocupados9.dta
sort directorio secuencia_p orden
save, replace

clear 
use inactivos9.dta
sort directorio secuencia_p orden
save, replace

clear 
use ocupados9.dta
sort directorio secuencia_p orden
save, replace

clear 
use otrosingresos9.dta
sort directorio secuencia_p orden
save, replace

clear 
use otrasactividades9.dta
sort directorio secuencia_p orden
save, replace

clear
use ftrab9.dta
sort directorio secuencia_p orden
save, replace

clear
use educacion9.dta    
sort directorio secuencia_p orden
save, replace

merge directorio secuencia_p orden using ftrab9.dta
tab _merge
drop _merge
sort directorio secuencia_p orden 
saveold "${surveysFolder}\Colombia\2008\GEIH\Datos\september08_res.dta", replace


merge directorio secuencia_p orden using otrasactividades9.dta
tab _merge
drop _merge
sort directorio secuencia_p orden 
saveold "${surveysFolder}\Colombia\2008\GEIH\Datos\september08_res.dta", replace

merge directorio secuencia_p orden using otrosingresos9.dta
tab _merge
drop _merge
sort directorio secuencia_p orden
saveold "${surveysFolder}\Colombia\2008\GEIH\Datos\september08_res.dta", replace

merge directorio secuencia_p orden using ocupados9.dta
tab _merge
drop _merge
sort directorio secuencia_p orden
saveold "${surveysFolder}\Colombia\2008\GEIH\Datos\september08_res.dta", replace

merge directorio secuencia_p orden using inactivos9.dta
tab _merge
drop _merge
sort directorio secuencia_p orden
saveold "${surveysFolder}\Colombia\2008\GEIH\Datos\september08_res.dta", replace

merge directorio secuencia_p orden using desocupados9.dta
tab _merge
drop _merge
sort directorio secuencia_p orden
saveold "${surveysFolder}\Colombia\2008\GEIH\Datos\september08_res.dta", replace


*Pegando el archivo de vivienda y hogar*
****************************************

clear
use vivienda9.dta
sort directorio secuencia_p  
save, replace

use "${surveysFolder}\Colombia\2008\GEIH\Datos\september08_res.dta"
sort directorio secuencia_p 
merge directorio secuencia_p  using vivienda9.dta
tab _merge 
drop _merge
compress
saveold "${surveysFolder}\Colombia\2008\GEIH\Datos\september08_res.dta", replace
clear


****************************
**SE CONFORMA EL TRIMESTRE**
****************************

cd "${surveysFolder}\Colombia\2008\GEIH\Datos\"

use july08_res.dta
append using august08_res.dta
append using september08_res.dta
compress
saveold "${surveysFolder}\Colombia\2008\GEIH\Datos\IIItrimestre_res.dta", replace





