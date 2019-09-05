set more off
set mem 150m

*Este programa es para crear el archivo final.

*Ordenamiento de los datos

*SEC00
use sec00.dta, clear
sort tipo folio viv
save sec00.dta, replace

*SEC01
use sec01.dta, clear
*gen double id_pers=r101
sort tipo folio viv id_pers
save sec01.dta, replace

*SEC02
use sec02.dta, clear
*gen double id_pers=r201
sort tipo folio viv id_pers
save sec02.dta, replace

*SEC03
use sec03.dta, clear
sort tipo folio viv
save sec03.dta, replace

*SEC04
use sec04.dta, clear
*gen double id_pers=r401
sort tipo folio viv id_pers
save sec04.dta, replace

*SEC06
use sec06.dta, clear
*gen double id_pers=r601
sort tipo folio viv id_pers
save sec06.dta, replace

*SEC07
use sec07.dta, clear
*gen double id_pers=r701
sort tipo folio viv id_pers
save sec07.dta, replace

*SEC09
use sec09.dta, clear
*gen double id_pers=r900a
sort tipo folio viv id_pers
save sec09.dta, replace

log using revision.smcl,replace
****CREACION DEL ARCHIVO DE HOGARES.

use sec03.dta, clear
merge tipo folio viv using sec00.dta
tab _merge
drop _merge
sort tipo folio viv
save slv03_hou.dta, replace

*****CREACION DEL ARCHIVO DE PERSONAS

use  sec01.dta, clear
merge tipo folio viv id_pers using sec02.dta
tab _merge
drop _merge
sort tipo folio viv id_pers
merge tipo folio viv id_pers using sec04.dta
tab _merge
drop _merge
sort tipo folio viv id_pers
merge tipo folio viv id_pers using sec07.dta
tab _merge
drop if _merge==2
drop _merge
sort tipo folio viv id_pers
merge tipo folio viv id_pers using sec06.dta
tab _merge
drop _merge
sort tipo folio viv id_pers
merge tipo folio viv id_pers using sec09.dta
tab _merge
drop _merge
sort tipo folio viv id_pers
save slv03_per.dta, replace

******CREACION DEL ARCHIVO FINAL

merge tipo folio viv using slv03_hou.dta
tab _merge
drop _merge
sort tipo folio viv id_per
save slv03.dta, replace

log close
