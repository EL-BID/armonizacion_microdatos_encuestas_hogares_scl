* (versión Stata 13)
clear
set more off
*________________________________________________________________________________________________________________*

 * Activar si es necesario (dejar desactivado para evitar sobreescribir la base y dejar la posibilidad de 
 * utilizar un loop)
 * Los datos se obtienen de las carpetas que se encuentran en el servidor: \\Sdssrv03\surveys
 * Se tiene acceso al servidor únicamente al interior del BID.
 * El servidor contiene las bases de datos MECOvI.
 *________________________________________________________________________________________________________________*
 

global ruta = "\\Sdssrv03\surveys"

local PAIS MEX
local ENCUESTA ENAMIN
local ANO "2012"
local ronda m10_m1 
local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
                        
capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: México
Encuesta: ENOE
Round: t1
Autores: Melany Gualavisi (melanyg@iadb.org)
Fecha última modificación: Diciembre, 2015

							SCL/LMK - IADB
****************************************************************************/
****************************************************************************/

use `base_in', clear

/* solo se generaron las siguientes variables - 
region_BID_c
idh_ch
idp_ci
edad_ci
sexo_ci
aedu_ci
condocup_ci
formal_ci
categopri_ci
spublico_ci 
horaspri_ci
tamemp_ci
ylmpri_ci

el resto está pendiente de armonizar*/

************************
*** region según BID ***
************************
gen region_BID_c=1 
label var region_BID_c "Regiones BID"
label define region_BID_c 1 "Centroamérica_(CID)" 2 "Caribe_(CCB)" 3 "Andinos_(CAN)" 4 "Cono_Sur_(CSC)"
label value region_BID_c region_BID_c


***************
****idh_ch*****
***************
egen idh_ch=group(cd_a  ent  con  v_sel  n_hog  h_mud) 
label var idh_ch "ID del hogar"


***************
****idp_ci*****
***************
egen idp_ci=group(idh_ch n_ren)
label var idp_ci "ID de la persona en el hogar"

******************************
*	pais_c
******************************
gen str3 pais_c="MEX"

******************************
*	anio_c
******************************
gen anio_c=2012
label var anio_c "Año de la encuesta"

******************************
*	factor_ci
******************************
gen factor_ci=fac
label var factor_ci "Factor de expansión"

***************
****edad_ci****
***************
gen edad_ci=eda
label var edad_ci "Edad del individuo en años"

***************
****sexo_ci****
***************
destring sex, replace
gen sexo_ci=sex
label var sexo_ci "Sexo del individuo" 
label define sexo_ci 1 "Hombre" 2 "Mujer", add modify
label value sexo_ci sexo_ci


*****TABLAS
tab p37 [iw=factor_ci] 
tab p39 [iw=factor_ci] 
tab p40_1 [iw=factor_ci] if preg40==1
tab p40_2 [iw=factor_ci] if preg40==1
tab p40_3 [iw=factor_ci] if preg40==1
tab p40_4 [iw=factor_ci] if preg40==1
tab p40_5 [iw=factor_ci] if preg40==1

tab p40_1 [iw=factor_ci] if preg40==2
tab p40_2 [iw=factor_ci] if preg40==2
tab p40_3 [iw=factor_ci] if preg40==2
tab p40_4 [iw=factor_ci] if preg40==2
tab p40_5 [iw=factor_ci] if preg40==2

destring p40_*, replace
g p401=(p40_1==1)
g p402=(p40_2==2)
g p403=(p40_3==3)
g p404=(p40_4==4)
g p405=(p40_5==5)

egen preg40=rsum( p401 p402 p403 p404 p405)

tab p61 [iw=factor_ci] 
tab p62 [iw=factor_ci]
tab p63 [iw=factor_ci]

recode p63_cant (999999=.)
xtile decil = p63_cant [w=factor_ci] if  p63_cant!=. & p63_cant!=0, nq(10)

sum p63_cant [iw=factor_ci]
tab p80 [iw=factor_ci]
tab p83  [iw=factor_ci]

*qui destring $var, replace


* Activar solo si es necesario
*keep *_ci  *_c  idh_ch 
set more off
compress


*do "$ruta\harmonized\_DOCS\\Labels_Harmonized_DataBank.do"


saveold "`base_out'", replace


log close



