*Elaboración: ?
*Actualizacion y cambio de rutas: Yessenia Loayza (desloay@hotmail.com | yessenial@iadb.org)
*Noviembre, 2013

*** MERGE COLOMBIA 1989 ****
*------------------------*

*1 - Conversion Bases de datos

**********************************************************
***PROGRAMA PARA CREAR LA VANILLA PARA COLOMBIA 1989******
**********************************************************

clear
set more off
local ruta = "\\Sdssrv03\surveys\"
cd "`ruta'survey\COL\ENH-FT\1989\m9\data_orig\"
/*
*** IDENTIFICACION
infile using "register1.dct"
keep if register==1
sort  municipal sector section stratum block segment dwelling home persons orderpers  
saveold "col89ident.dta",  replace
clear

*** TRABAJO
infile using "register5.dct"
keep if register==5
sort  municipal sector section stratum block segment dwelling home persons orderpers  
saveold "col89trab.dta",  replace
clear

** OCUPADOS
infile using "register6.dct"
keep if register==6
sort  municipal sector section stratum block segment dwelling home persons orderpers  
saveold "col89ocup.dta",  replace
clear

*** DESOCUPADOS
infile using "register7.dct"
keep if register==7
sort  municipal sector section stratum block segment dwelling home persons orderpers  
saveold "col89desocup.dta",  replace
clear

*** INACTIVOS
infile using "register8.dct"
keep if register==8
sort  municipal sector section stratum block segment dwelling home persons orderpers  
saveold "col89inactiv.dta",  replace
clear

*** VIVIENDA
infile using "register0.dct"
keep if register==0
sort  municipal sector section stratum block segment dwelling home persons orderpers  
saveold "col89vivi.dta",  replace
clear
*/

*2 - Merge bases de datos
* NOW WE MERGE THE FILES

use "col89ident.dta"
sort  municipal sector section stratum block segment dwelling home persons orderpers  
merge municipal sector section stratum block segment dwelling home persons orderpers  using "col89trab.dta"
rename _merge merge1
tab merge1
sort  municipal sector section stratum block segment dwelling home persons orderpers  

merge municipal sector section stratum block segment dwelling home persons orderpers using "col89ocup.dta"
rename _merge merge2
tab merge2
sort  municipal sector section stratum block segment dwelling home persons orderpers  

merge municipal sector section stratum block segment dwelling home persons orderpers  using "col89desocup.dta"
rename _merge merge3
tab merge3
sort  municipal sector section stratum block segment dwelling home persons orderpers  

merge municipal sector section stratum block segment dwelling home persons orderpers  using "col89inactiv.dta"
rename _merge merge4
tab merge4
sort  municipal sector section stratum block segment dwelling home persons orderpers  

merge municipal sector section stratum block segment dwelling home persons orderpers  using "col89vivi.dta"
rename _merge merge5
tab merge5
sort  municipal sector section stratum block segment dwelling home persons orderpers  
drop merge*
saveold "`ruta'\survey\COL\ENH-FT\1989\m9\data_merge\COL_1989m9.dta",  replace




