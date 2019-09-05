*Elaboración: ?
*Actualizacion y cambio de rutas: Yessenia Loayza (desloay@hotmail.com | yessenial@iadb.org)
*Noviembre, 2013

*** MERGE COLOMBIA 1990 ****
*------------------------*

*1 - Conversion Bases de datos

**********************************************************
***PROGRAMA PARA CREAR LA VANILLA PARA COLOMBIA 1990***
**********************************************************

clear
set more off
local ruta = "\\Sdssrv03\surveys\"
cd "`ruta'survey\COL\ENH-FT\1990\m9\data_orig\"

/*
*** IDENTIFICACION
infile using "register1.dct"
keep if register==1
compress
sort municipal sector section stratum block segment dwelling home persons orderpers
saveold "col90ident.dta",  replace
clear

*** TRABAJO
infile using "register5.dct"
keep if register==5
compress
sort municipal sector section stratum block segment dwelling home persons orderpers
save "col90trab.dta",  replace
clear

** OCUPADOS
infile using "register6.dct"
keep if register==6
compress
sort municipal sector section stratum block segment dwelling home persons orderpers
save "col90ocup.dta",  replace
clear

*** DESOCUPADOS
infile using "register7.dct"
keep if register==7
compress
sort municipal sector section stratum block segment dwelling home persons orderpers
save "col90desocup.dta",  replace
clear

*** INACTIVOS
infile using "register8.dct"
keep if register==8
compress
sort municipal sector section stratum block segment dwelling home persons orderpers
save "col90inactiv.dta",  replace
clear

*** VIVIENDA
infile using "register0.dct"
keep if register==0
compress
sort municipal sector section stratum block segment dwelling home persons orderpers
save "col90vivi.dta",  replace
*/

*2 - Merge bases de datos

use "col90ident.dta"

sort municipal sector section stratum block segment dwelling home persons orderpers
merge municipal sector section stratum block segment dwelling home persons orderpers using "col90trab.dta"
rename _merge merge1
tab merge1
sort municipal sector section stratum block segment dwelling home persons orderpers

merge municipal sector section stratum block segment dwelling home persons orderpers using "col90ocup.dta"
rename _merge merge2
tab merge2
sort municipal sector section stratum block segment dwelling home persons orderpers

merge municipal sector section stratum block segment dwelling home persons orderpers using "col90desocup.dta"
rename _merge merge3
tab merge3
sort municipal sector section stratum block segment dwelling home persons orderpers

merge municipal sector section stratum block segment dwelling home persons orderpers using "col90inactiv.dta"
rename _merge merge4
tab merge4
sort municipal sector section stratum block segment dwelling home persons orderpers

merge municipal sector section stratum block segment dwelling home persons orderpers using "col90vivi.dta"
rename _merge merge5
tab merge5
sort municipal sector section stratum block segment dwelling home persons orderpers
drop merge*
saveold "`ruta'\survey\COL\ENH-FT\1990\m9\data_merge\COL_1990m9.dta",  replace


