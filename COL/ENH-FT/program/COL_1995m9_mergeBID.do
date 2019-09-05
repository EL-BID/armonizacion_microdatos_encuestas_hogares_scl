*Elaboración (se reelabora en base a varios do-files anteriores): Yessenia Loayza (desloay@hotmail.com | yessenial@iadb.org)
*Noviembre, 2013

*** MERGE COLOMBIA 1995 ****
*---------------------------*

clear
set more off
local ruta ="\\Sdssrv03\surveys\"
local in   ="`ruta'survey\COL\ENH-FT\1995\m9\data_orig\"
local out  ="`ruta'\survey\COL\ENH-FT\1995\m9\data_merge\"

*1. AREA CABECERA
*------------------

*- Conversion Bases de datos

/* clear
*** VIVIENDA
infile using "`in'col95_T01C.dct"
keep if ca_TIPOD=="01"

/* esto saca las observaciones de otros hogares dentro de la vivienda...que son missing en este archivo*/
gen uno=1
bysort ca_VIVI: gen po=sum(uno) 
* pilas si pongo egen le pone el total a todos, necesito que sea conteo
drop if po>1
*
sort  ca_VIVI  
save `in'colc95vivi.dta,  replace
clear

*** HOGAR
infile using "`in'col95_H01C.dct"
keep if ca_TIPOD=="01"
sort  ca_VIVI  ca_HOGAR
save `in'colc95hoga.dta,  replace
clear

*** CARACTERISTICAS GENERALES
infile using  "`in'col95_T10C.dct"
keep if ca_TIPOD=="10"
sort  ca_IDENT ca_009
save `in'colc95edu.dta,  replace
clear

*** FUERZA DE TRABAJO
infile using  "`in'col95_T50C.dct"
keep if ca_TIPOD=="50"
sort  ca_IDENT ca_009
save `in'colc95ft.dta,  replace
clear


*** OCUPADOS EMPLEO PRINCIPAL
infile using  "`in'col95_T60C.dct"
keep if ca_TIPOD=="60"
sort  ca_IDENT ca_009
save `in'colc95ocup.dta,  replace
clear

*** DESOCUPADOS 
infile using  "`in'col95_T70C.dct"
keep if ca_TIPOD=="70"
sort  ca_IDENT ca_009
save `in'colc95deso.dta,  replace
clear

*** INACTIVOS 
infile using  "`in'col95_T80C.dct"
keep if ca_TIPOD=="80"
sort  ca_IDENT ca_009
save `in'colc95inac.dta,  replace
*/


*- Merge bases de datos
*-------------------------

clear
*** NOW WE MERGE THE FILES
use `in'colc95edu.dta, clear
gen ca_VIVI=substr(ca_IDENT,1,17)
gen ca_HOGAR=substr(ca_IDENT,18,2)
sort ca_VIVI 
merge ca_VIVI using `in'colc95vivi.dta
rename _merge merge_viv
drop if merge_viv==2
tab merge_viv, m
/*
 tab merge_viv, m

  merge_viv |      Freq.     Percent        Cum.
------------+-----------------------------------
          1 |         14        0.02        0.02
          2 |          1        0.00        0.02
          3 |     78,998       99.98      100.00
------------+-----------------------------------
      Total |     79,013      100.00

merge_viv==1
	Estas observaciones se quedan porque tiene todas las demas variables menos 
	vivienda y hogar...pierdo obs de vivienda en 14 casos PERO SE QUEDAN !!!
	son las siguientes: 
		ca_IDENT
			0103522201091100201
			6309630003070101901
merge_viv==2
	DATO QUE BORRE PORQUE SOLO TIENE VALOR EN LAS VARIABLES DE VIVIENDA
		ca_VIVI
		05005113010303002
*/

sort ca_VIVI ca_HOGAR
merge ca_VIVI ca_HOGAR using `in'colc95hoga.dta
rename _merge merge_hoga
drop if merge_hoga==2
tab merge_hoga
save `in'temp.dta, replace 


sort  ca_IDENT ca_009
merge ca_IDENT ca_009 using `in'colc95ft.dta
rename _merge mergeft
tab mergeft

sort  ca_IDENT ca_009
merge ca_IDENT ca_009 using `in'colc95ocup.dta
rename _merge mergeocup
tab mergeocup


sort  ca_IDENT ca_009
merge ca_IDENT ca_009 using `in'colc95deso.dta
rename _merge mergedeso
tab mergedeso

sort  ca_IDENT ca_009
merge ca_IDENT ca_009 using `in'colc95inac.dta
rename _merge mergeinac
tab mergeinac

gen factor_ci=real(FEX_DANE)
drop FEX_DANE


drop merge* 
saveold "`out'COL_1995m9_cabecera.dta",  replace

*** CABECERA RESTO
*------------------*

*- Conversion Bases de datos
*----------------------------

/* clear
*** VIVIENDA
infile using "`in'col95_T01R.dct"
keep if re_TIPOD=="01"
* este comando se deshace de los hogares que comparten vivienda, sirve para hacer un buen merge 
gen uno=1
bysort re_VIVI: gen po=sum(uno) 
drop if po>1
*
sort  re_VIVI
save `in'colr95vivi.dta,  replace
clear

*** HOGAR
infile using "`in'col95_H01R.dct"
keep if re_TIPOD=="01"
sort  re_VIVI re_HOGA 
save `in'colr95hoga.dta,  replace
clear

*** caracteristicas generales (EDUCA)
infile using  "`in'col95_T10R.dct"
keep if re_TIPOD=="10"
sort  re_IDENT re_009
save `in'colr95edu.dta,  replace
clear

*** FUERZA DE TRABAJO
infile using  "`in'col95_T50R.dct"
keep if re_TIPOD=="50"
sort  re_IDENT re_009
save `in'colr95ft.dta,  replace
clear


*** OCUPADOS EMPLEO PRINCIPAL
infile using  "`in'col95_T60R.dct"
keep if re_TIPOD=="60"
sort  re_IDENT re_009
save `in'colr95ocup.dta,  replace
clear

*** OCUPADOS EMPLEO CONTINUACION
infile using  "`in'col95_T61R.dct"
keep if re_TIPOD=="61"
sort  re_IDENT re_009
save `in'colr95ocupc.dta,  replace
clear

*** DESOCUPADOS 
infile using  "`in'col95_T70R.dct"
keep if re_TIPOD=="70"
sort  re_IDENT re_009
save `in'colr95deso.dta,  replace
*/



*- Merge bases de datos
*-------------------------

clear
*** NOW WE MERGE THE FILES

use `in'colr95hoga.dta
merge re_VIVI using `in'colr95vivi.dta
rename _merge merge_viv
tab merge_viv

/* estas viviendas no aparecen en el archivo de vivi ni en el de hogares ...aparecen solo en el de personas 
esto significa que habrá 2 viviendas sin info de vivienda, ni de hogar, 8 individuos 
los dejo !!

10   101340501401
10   110641001102
*/

capture drop _merge
egen re_IDENT = concat(re_VIVI re_HOGA)
sort re_IDENT 
save `in'temp.dta, replace 

use `in'colr95edu.dta 
capture drop _merge
merge re_IDENT using `in'temp.dta
rename _merge mergetemp
tab mergetemp

sort  re_IDENT re_009
merge re_IDENT re_009 using `in'colr95ft.dta
rename _merge mergeft
tab mergeft

sort  re_IDENT re_009
merge re_IDENT re_009 using `in'colr95ocup.dta
rename _merge mergeocup
tab mergeocup

sort  re_IDENT re_009
merge re_IDENT re_009 using `in'colr95ocupc.dta
rename _merge mergeocupc
tab mergeocupc

sort  re_IDENT re_009
merge re_IDENT re_009 using `in'colr95deso.dta
rename _merge mergedeso
tab mergedeso
/*
sort  re_IDENT re_009
merge re_IDENT re_009 using `in'colr95sec.dta
rename _merge mergesec
tab mergesec
*/
tab merge_viv mergetemp, m

gen factor_ci=real(FEX_DANE)
drop FEX_DANE

drop merge* 
saveold "`out'COL_1995m9_resto.dta",  replace








