	***********************************************
	*** CONFORMACION BASE TERCER TRIMESTRE 2010 ***
	*********         RESTO         ***************
	***********************************************


*************************
**BASE DEL MES DE JULIO**
*************************

clear

cd "${surveysFolder}\Colombia\2010\GEIH\3thquarter\Datos originales\1007\RESTO\"


*Pegando los archivos de personas*
**********************************

clear 
use sas70res1007.dta
sort directorio secuencia_p orden
save, replace

clear 
use sas80res1007.dta
sort directorio secuencia_p orden
save, replace

clear 
use sas60res1007.dta
sort directorio secuencia_p orden
save, replace

clear 
use sas95res1007.dta
sort directorio secuencia_p orden
save, replace

clear 
use sas90res1007.dta
sort directorio secuencia_p orden
save, replace

clear
use sas50res1007.dta
sort directorio secuencia_p orden
save, replace

clear
use sas10res1007.dta    
sort directorio secuencia_p orden
save, replace

merge directorio secuencia_p orden using sas50res1007.dta
tab _merge
drop _merge
sort directorio secuencia_p orden 
saveold "${surveysFolder}\Colombia\2010\GEIH\3thquarter\Datos\july10_res.dta", replace

merge directorio secuencia_p orden using sas90res1007.dta
tab _merge
drop _merge
sort directorio secuencia_p orden 
saveold "${surveysFolder}\Colombia\2010\GEIH\3thquarter\Datos\july10_res.dta", replace

merge directorio secuencia_p orden using sas95res1007.dta
tab _merge
drop _merge
sort directorio secuencia_p orden
saveold "${surveysFolder}\Colombia\2010\GEIH\3thquarter\Datos\july10_res.dta", replace

merge directorio secuencia_p orden using sas60res1007.dta
tab _merge
drop _merge
sort directorio secuencia_p orden
saveold "${surveysFolder}\Colombia\2010\GEIH\3thquarter\Datos\july10_res.dta", replace

merge directorio secuencia_p orden using sas80res1007.dta
tab _merge
drop _merge
sort directorio secuencia_p orden
saveold "${surveysFolder}\Colombia\2010\GEIH\3thquarter\Datos\july10_res.dta", replace

merge directorio secuencia_p orden using sas70res1007.dta
tab _merge
drop _merge
sort directorio secuencia_p orden
saveold "${surveysFolder}\Colombia\2010\GEIH\3thquarter\Datos\july10_res.dta", replace


*Pegando el archivo de vivienda y hogar*
****************************************
clear
use sas01res1007.dta
sort directorio secuencia_p 
save, replace

use "${surveysFolder}\Colombia\2010\GEIH\3thquarter\Datos\july10_res.dta"
sort directorio secuencia_p
merge directorio secuencia_p using sas01res1007.dta
tab _merge 
drop _merge
compress
saveold "${surveysFolder}\Colombia\2010\GEIH\3thquarter\Datos\july10_res.dta", replace




**************************
**BASE DEL MES DE AGOSTO**
**************************

clear

cd "${surveysFolder}\Colombia\2010\GEIH\3thquarter\Datos originales\1008\RESTO\"


*Pegando los archivos de personas*
**********************************

clear 
use sas70res1008.dta
sort directorio secuencia_p orden
save, replace

clear 
use sas80res1008.dta
sort directorio secuencia_p orden
save, replace

clear 
use sas60res1008.dta
sort directorio secuencia_p orden
save, replace

clear 
use sas95res1008.dta
sort directorio secuencia_p orden
save, replace

clear 
use sas90res1008.dta
sort directorio secuencia_p orden
save, replace

clear
use sas50res1008.dta
sort directorio secuencia_p orden
save, replace

clear
use sas10res1008.dta    
sort directorio secuencia_p orden
save, replace

merge directorio secuencia_p orden using sas50res1008.dta
tab _merge
drop _merge
sort directorio secuencia_p orden 
saveold "${surveysFolder}\Colombia\2010\GEIH\3thquarter\Datos\august10_res.dta", replace

merge directorio secuencia_p orden using sas90res1008.dta
tab _merge
drop _merge
sort directorio secuencia_p orden 
saveold "${surveysFolder}\Colombia\2010\GEIH\3thquarter\Datos\august10_res.dta", replace

merge directorio secuencia_p orden using sas95res1008.dta
tab _merge
drop _merge
sort directorio secuencia_p orden
saveold "${surveysFolder}\Colombia\2010\GEIH\3thquarter\Datos\august10_res.dta", replace

merge directorio secuencia_p orden using sas60res1008.dta
tab _merge
drop _merge
sort directorio secuencia_p orden
saveold "${surveysFolder}\Colombia\2010\GEIH\3thquarter\Datos\august10_res.dta", replace

merge directorio secuencia_p orden using sas80res1008.dta
tab _merge
drop _merge
sort directorio secuencia_p orden
saveold "${surveysFolder}\Colombia\2010\GEIH\3thquarter\Datos\august10_res.dta", replace

merge directorio secuencia_p orden using sas70res1008.dta
tab _merge
drop _merge
sort directorio secuencia_p orden
saveold "${surveysFolder}\Colombia\2010\GEIH\3thquarter\Datos\august10_res.dta", replace


*Pegando el archivo de vivienda y hogar*
****************************************

clear
use sas01res1008.dta
sort directorio secuencia_p  
save, replace

use "${surveysFolder}\Colombia\2010\GEIH\3thquarter\Datos\august10_res.dta"
sort directorio secuencia_p 
merge directorio secuencia_p using sas01res1008.dta
tab _merge 
drop _merge
compress
saveold "${surveysFolder}\Colombia\2010\GEIH\3thquarter\Datos\august10_res.dta", replace
clear


******************************
**BASE DEL MES DE SEPTIEMBRE**
******************************

clear

cd "${surveysFolder}\Colombia\2010\GEIH\3thquarter\Datos originales\1009\RESTO\"


*Pegando los archivos de personas*
**********************************

clear 
use sas70res1009.dta
sort directorio secuencia_p orden
save, replace

clear 
use sas80res1009.dta
sort directorio secuencia_p orden
save, replace

clear 
use sas60res1009.dta
sort directorio secuencia_p orden
save, replace

clear 
use sas95res1009.dta
sort directorio secuencia_p orden
save, replace

clear 
use sas90res1009.dta
sort directorio secuencia_p orden
save, replace

clear
use sas50res1009.dta
sort directorio secuencia_p orden
save, replace

clear
use sas10res1009.dta    
sort directorio secuencia_p orden
save, replace

merge directorio secuencia_p orden using sas50res1009.dta
tab _merge
drop _merge
sort directorio secuencia_p orden 
saveold "${surveysFolder}\Colombia\2010\GEIH\3thquarter\Datos\september10_res.dta", replace


merge directorio secuencia_p orden using sas95res1009.dta
tab _merge
drop _merge
sort directorio secuencia_p orden 
saveold "${surveysFolder}\Colombia\2010\GEIH\3thquarter\Datos\september10_res.dta", replace

merge directorio secuencia_p orden using sas90res1009.dta
tab _merge
drop _merge
sort directorio secuencia_p orden
saveold "${surveysFolder}\Colombia\2010\GEIH\3thquarter\Datos\september10_res.dta", replace

merge directorio secuencia_p orden using sas60res1009.dta
tab _merge
drop _merge
sort directorio secuencia_p orden
saveold "${surveysFolder}\Colombia\2010\GEIH\3thquarter\Datos\september10_res.dta", replace

merge directorio secuencia_p orden using sas80res1009.dta
tab _merge
drop _merge
sort directorio secuencia_p orden
saveold "${surveysFolder}\Colombia\2010\GEIH\3thquarter\Datos\september10_res.dta", replace

merge directorio secuencia_p orden using sas70res1009.dta
tab _merge
drop _merge
sort directorio secuencia_p orden
saveold "${surveysFolder}\Colombia\2010\GEIH\3thquarter\Datos\september10_res.dta", replace


*Pegando el archivo de vivienda y hogar*
****************************************

clear
use sas01res1009.dta
sort directorio secuencia_p 
save, replace

use "${surveysFolder}\Colombia\2010\GEIH\3thquarter\Datos\september10_res.dta"
sort directorio secuencia_p
merge directorio secuencia_p using sas01res1009.dta
tab _merge 
drop _merge
compress
saveold "${surveysFolder}\Colombia\2010\GEIH\3thquarter\Datos\september10_res.dta", replace
clear


****************************
**SE CONFORMA EL TRIMESTRE**
****************************

cd "${surveysFolder}\Colombia\2010\GEIH\3thquarter\Datos\"

use july10_res.dta
append using august10_res.dta
append using september10_res.dta
compress
saveold "${surveysFolder}\Colombia\2010\GEIH\3thquarter\Datos\IIItrimestre_res.dta", replace





