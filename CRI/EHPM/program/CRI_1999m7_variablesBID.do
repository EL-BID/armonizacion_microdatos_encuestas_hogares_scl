
* (Versión Stata 12)
clear
set more off
*________________________________________________________________________________________________________________*

 * Activar si es necesario (dejar desactivado para evitar sobreescribir la base y dejar la posibilidad de 
 * utilizar un loop)
 * Los datos se obtienen de las carpetas que se encuentran en el servidor: ${surveysFolder}
 * Se tiene acceso al servidor únicamente al interior del BID.
 * El servidor contiene las bases de datos MECOVI.
 *________________________________________________________________________________________________________________*
 


global ruta = "${surveysFolder}"

local PAIS CRI
local ENCUESTA EHPM
local ANO "1999"
local ronda m7 

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\\`PAIS'_`ANO'`ronda'.dta"
*local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
   


capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Costa Rica
Encuesta: EHPM
Round: m7
Autores: 
Última versión: Mayra Sáenz E-mail: mayras@iadb.org / saenzmayra.a@gmail.com
Fecha última modificación: 1 de octubre de 2013

							SCL/LMK - IADB
****************************************************************************/
/***************************************************************************
Detalle de procesamientos o modificaciones anteriores:

****************************************************************************/


use `base_in', clear

************
* Region_BID *
************
gen region_BID_c=.
replace region_BID_c=1 
label var region_BID_c "Regiones BID"
label define region_BID_c 1 "Centroamérica_(CID)" 2 "Caribe_(CCB)" 3 "Andinos_(CAN)" 4 "Cono_Sur_(CSC)"
label value region_BID_c region_BID_c


************
* Region_c *
************
*Inclusión Mayra Sáenz - Julio 2013

gen region_c=  .
label var region_c "División política"

*1999*

***************
***factor_ch***
***************

gen factor_ch=factorex
label variable factor_ch "Factor de expansion del hogar"


***************
****idh_ch*****
**************

gen double idh_ch =id_hogar
label variable idh_ch "ID del hogar"


*************
****idp_ci****
**************

gen idp_ci=nrolinea
label variable idp_ci "ID de la persona en el hogar"

**********
***zona***
**********

gen byte zona_c=0 if zona>=2 & zona<=4
replace zona_c=1 if zona==1

label variable zona_c "Zona del pais"
label define zona_c 1 "Urbana" 0 "Rural"
label value zona_c zona_c


***************
***upm_ci***
***************
gen upm_ci=. 

***************
***estrato_ci***
***************
gen estrato_ci=.


************
****pais****
************

gen str3 pais_c="CRI"
label variable pais_c "Pais"

**********
***anio***
**********

gen anio_c=1999
label variable anio_c "Anio de la encuesta"

*********
***mes***
*********

gen byte mes_c=7

label variable mes_c "Mes de la encuesta"
label define mes_c 7 "Julio" 
label value mes_c mes_c


*****************
***relacion_ci***
*****************

gen relacion_ci=1 if parentco==1
replace relacion_ci=2 if parentco==2
replace relacion_ci=3 if parentco==3
replace relacion_ci=4 if (parentco>=4 & parentco<=7) | parentco==11
replace relacion_ci=5 if parentco==9 | parentco==10
replace relacion_ci=6 if parentco==8

label variable relacion_ci "Relacion con el jefe del hogar"
label define relacion_ci 1 "Jefe/a" 2 "Esposo/a" 3 "Hijo/a" 4 "Otros parientes" 5 "Otros no parientes"
label define relacion_ci 6 "Empleado/a domestico/a", add

label value relacion_ci relacion_ci



****************************
***VARIABLES DEMOGRAFICAS***
****************************

***************
***factor_ci***
***************

gen factor_ci=factorex
label variable factor_ci "Factor de expansion del individuo"

**********
***sexo***
**********

gen sexo_ci=sexo

label define sexo_ci 1 "Hombre" 2 "Mujer"
label value sexo_ci sexo_ci

**********
***edad***
**********

gen edad_ci=edad
replace edad_ci=. if edad==98 | edad==99 /*Casos ignorados*/
label variable edad_ci "Edad del individuo"

*************************
*** VARIABLES DE RAZA ***
*************************

* MGR Oct. 2015: modificaciones realizadas en base a metodología enviada por SCL/GDI Maria Olga Peña

gen raza_idioma_ci = . 
gen id_ind_ci = .
gen id_afro_ci = .
*En este año  no se dispone de esta variable.
gen raza_ci=.
label define raza_ci 1 "Indígena" 2 "Afro-descendiente" 3 "Otros"
label value raza_ci raza_ci 
label var raza_ci "Raza o etnia del individuo" 

*****************
***civil_ci***
*****************

gen civil_ci=.
replace civil_ci=1 if estcivil==6
replace civil_ci=2 if estcivil==1 | estcivil==2
replace civil_ci=3 if estcivil==3| estcivil==4
replace civil_ci=4 if estcivil==5

label variable civil_ci "Estado civil"
label define civil_ci 1 "Soltero" 2 "Union formal o informal"
label define civil_ci 3 "Divorciado o separado" 4 "Viudo" , add
label value civil_ci civil_ci


**************
***jefe_ci***
*************

gen jefe_ci=(relacion_ci==1)
label variable jefe_ci "Jefe de hogar"


******************
***nconyuges_ch***
******************

by idh_ch, sort: egen nconyuges_ch=sum(relacion_ci==2)
label variable nconyuges_ch "Numero de conyuges"

***************
***nhijos_ch***
***************

by idh_ch, sort: egen nhijos_ch=sum(relacion_ci==3)
label variable nhijos_ch "Numero de hijos"

******************
***notropari_ch***
******************

by idh_ch, sort: egen notropari_ch=sum(relacion_ci==4)
label variable notropari_ch "Numero de otros familiares"

********************
***notronopari_ch***
********************

by idh_ch, sort: egen notronopari_ch=sum(relacion_ci==5)
label variable notronopari_ch "Numero de no familiares"


****************
***nempdom_ch***
****************

by idh_ch, sort: egen nempdom_ch=sum(relacion_ci==6)
label variable nempdom_ch "Numero de empleados domesticos"

*****************
***clasehog_ch***
*****************

gen byte clasehog_ch=0
**** unipersonal
replace clasehog_ch=1 if nhijos_ch==0 & nconyuges_ch==0 & notropari_ch==0 & notronopari_ch==0
**** nuclear   (child with or without spouse but without other relatives)
replace clasehog_ch=2 if (nhijos_ch>0| nconyuges_ch>0) & (notropari_ch==0 & notronopari_ch==0)
**** ampliado
replace clasehog_ch=3 if ((clasehog_ch ==2 & notropari_ch>0) & notronopari_ch==0) |(notropari_ch>0 & notronopari_ch==0) 
**** compuesto  (some relatives plus non relative)
replace clasehog_ch=4 if ((nconyuges_ch>0 | nhijos_ch>0 | notropari_ch>0) & (notronopari_ch>0))
**** corresidente
replace clasehog_ch=5 if nhijos_ch==0 & nconyuges_ch==0 & notropari_ch==0 & notronopari_ch>0

label variable clasehog_ch "Tipo de hogar"
label define clasehog_ch 1 " Unipersonal" 2 "Nuclear" 3 "Ampliado" 
label define clasehog_ch 4 "Compuesto" 5 " Corresidente", add
label value clasehog_ch clasehog_ch

******************
***nmiembros_ch***
******************

by idh_ch, sort: egen nmiembros_ch=sum(relacion_ci>=1 & relacion_ci<=4)
label variable nmiembros_ch "Numero de familiares en el hogar"

*****************
***nmayor21_ch***
*****************

by idh_ch, sort: egen nmayor21_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad_ci>=21)
label variable nmayor21_ch "Numero de familiares mayores a 21 anios"

*****************
***nmenor21_ch***
*****************

by idh_ch, sort: egen nmenor21_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad_ci<21)
label variable nmenor21_ch "Numero de familiares menores a 21 anios"

*****************
***nmayor65_ch***
*****************

by idh_ch, sort: egen nmayor65_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad_ci>=65)
label variable nmayor65_ch "Numero de familiares mayores a 65 anios"

****************
***nmenor6_ch***
****************

by idh_ch, sort: egen nmenor6_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad_ci<6)
label variable nmenor6_ch "Numero de familiares menores a 6 anios"

****************
***nmenor1_ch***
****************

by idh_ch, sort: egen nmenor1_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad_ci<1)
label variable nmenor1_ch "Numero de familiares menores a 1 anio"

****************
***miembros_ci***
****************

gen miembros_ci=(relacion_ci<5)
label variable miembros_ci "Miembro del hogar"


************************************
*** VARIABLES DEL MERCADO LABORAL***
************************************

/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/

*********
*lp_ci***
*********
capture drop lp_ci
gen lp_ci = .
replace lp_ci =       22151 if zona_c==1
replace lp_ci =       17509 if zona_c==0

label var lp_ci "Linea de pobreza oficial del pais"

*********
*lpe_ci***
*********

gen lpe_ci = .
replace lpe_ci =      10161 if zona_c==1
replace lpe_ci =      8888 if zona_c==0

label var lpe_ci "Linea de indigencia oficial del pais"

****************
*cotizando_ci***
****************
gen cotizando_ci=.
*replace cotizando_ci=1 if asegura>=1 & asegura<=3 |(afilpenc>=1 & afilpenc<=6)

*recode cotizando_ci .=0 
label var cotizando_ci "Cotizante a la Seguridad Social"
label define cotizando_ci 0"No cotiza" 1"Cotiza a la SS" 
label value cotizando_ci cotizando_ci

****************
*afiliado_ci****
****************
gen afiliado_ci=.

label var afiliado_ci "Afiliado a la Seguridad Social"

****************
*tipopen_ci*****
****************
gen tipopen_ci=.

label var tipopen_ci "Tipo de pension - variable original de cada pais" 

****************
*instpen_ci*****
****************
gen instpen_ci=.
label var instpen_ci "Institucion proveedora de la pension - variable original de cada pais" 

****************
*instcot_ci*****
****************
gen instcot_ci=.

label var instcot_ci "Institucion a la que cotiza - variable original de cada pais" 

****************
****condocup_ci*
****************

gen condocup_ci=.
replace condocup_ci=1 if condiact==1
replace condocup_ci=2 if condiact>=2 & condiact<=7
recode condocup_ci .=3 if condiact>=8 & condiact<=14 & edad_ci>=12
recode condocup_ci .=4 if  condiact==0 | edad_ci<12
label var condocup_ci "Condicion de ocupación de acuerdo a def de cada pais"
label define condocup_ci 1 "Ocupado" 2 "Desocupado" 3 "Inactivo" 4 "Menor que 12" 
label value condocup_ci condocup_ci

*************
*cesante_ci* 
*************
* MGD 12/4/2015 No hay variable, se corrije el cambio de .=0 ya que todos salian como aspirantes.
gen cesante_ci=.
*gen cesante_ci=1 if c13==1
*recode cesante_ci .=0 if condocup_ci==2
* No todos los desempleados respondieron si han trabajado antes
label var cesante_ci "Desocupado - definicion oficial del pais"	

*************
**pension_ci*
*************
gen aux_p=.
/*
gen aux_p= pension if per06==1

replace aux_p = pension/2 if per06==2
replace aux_p = pension/3 if per06==3
replace aux_p = pension/4 if per06==4
replace aux_p = pension/6 if per06==6
replace aux_p = pension/12 if per06==7
replace aux_p = . if pension==99999999

gen pension_ci=1 if (aux_p>0 & aux_p!=.) 
recode pension_ci .=0 
*/
*Modificación Alvaro AM - julio 2019
gen pension_ci=(yotros<999999 & (inactiv==1 | parentco==9 | condact==4)) if yotros!=. 
label var pension_ci "1=Recibe pension contributiva"

*************
**ypen_ci*
*************

gen ypen_ci=aux_p
label var ypen_ci "Valor de la pension contributiva"

***************
*pensionsub_ci*
***************
gen aux_ps=.
/*
gen aux_ps= c35b if c35b1==5

replace aux_ps = c35b*4 if c35b1==3
replace aux_ps = c35b*2 if c35b1==4
replace aux_ps = c35b/4 if c35b1==6
replace aux_ps = c35b/6 if c35b1==7
replace aux_ps = c35b/12 if c35b1==8
replace aux_ps = . if c35b==9999999
*/

gen byte pensionsub_ci= 1 if aux_ps>0 & aux_ps!=.
recode pension_ci .=0
label var pensionsub_ci "1=recibe pension subsidiada / no contributiva"

*****************
**ypensub_ci*
*****************
destring aux_ps, replace
gen  ypensub_ci=aux_ps
label var ypensub_ci "Valor de la pension subsidiada / no contributiva"


*************
**salmm_ci***
*************

* 1999 junio (fuente OIT-CEPAL observatorio de la crisis)
gen salmm_ci= 57460
label var salmm_ci "Salario minimo legal"

************
***emp_ci***
************
gen byte emp_ci=(condocup_ci==1)
label var emp_ci "Ocupado (empleado)"

****************
***desemp_ci***
****************
gen desemp_ci=(condocup_ci==2)
label var desemp_ci "Desempleado que buscó empleo en el periodo de referencia"
  
*************
***pea_ci***
*************
gen pea_ci=0
replace pea_ci=1 if emp_ci==1 |desemp_ci==1
label var pea_ci "Población Económicamente Activa"

*****************
***desalent_ci***
*****************

gen desalent_ci=.
/*Esta variable no está en el archivo (no fue suministrada por el país)*/

*****************
***horaspri_ci***
*****************

gen horaspri_ci=hrsocpr 
replace horaspri_ci=. if hrsocpr==99
replace horaspri_ci=. if emp_ci==0


*****************
***horastot_ci***
*****************

egen horastot_ci=rsum(horaspri_ci hrsocse), missing 
replace horastot_ci=. if horaspri_ci==. & hrsocse==.
replace horastot_ci=horaspri_ci if hrsocse==99 & horaspri_ci~=.
replace horastot_ci=. if horaspri_ci==.
replace horastot_ci=. if emp_ci==0


***************
***subemp_ci***
***************
gen subemp_ci=.
/*Esta variable no está en el archivo (no fue suministrada por el país)*/

*******************
***tiempoparc_ci***
*******************

gen tiempoparc_ci=.
/*Esta variable no está en el archivo (no fue suministrada por el país)*/


******************
***categopri_ci***
******************

gen categopri_ci=.
replace categopri_ci=1 if categp==1
replace categopri_ci=2 if categp==2 
replace categopri_ci=3 if categp==3 | categp==4 |categp==5 
replace categopri_ci=4 if categp==6 | categp==7
replace categopri_ci=. if emp_ci==0

label define categopri_ci 1"Patron" 2"Cuenta propia" 
label define categopri_ci 3"Empleado" 4" No remunerado" , add
label value categopri_ci categopri_ci
label variable categopri_ci "Categoria ocupacional"



******************
***categosec_ci***
******************

gen categosec_ci=.
/*Esta variable no está en el archivo (no fue suministrada por el país)*/

*****************
*tipocontrato_ci*
*****************
/*
gen tipocontrato_ci=.
label var tipocontrato_ci "Tipo de contrato segun su duracion"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci
*/
*MGD 06/13/2014
* Alternativa con p25. ¿El trabajo que realizó la semana pasada en su ocupación principal...(todo el anio, ocasional/estacional, otro)
gen tipocontrato_ci=.
replace tipocontrato_ci=1 if estabili==1 & categopri_ci==3
replace tipocontrato_ci=2 if (estabili==2 | estabili==3 | estabili==4 | estabili==9) & categopri_ci==3
label var tipocontrato_ci "Tipo de contrato segun su duracion"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci


*****************
***nempleos_ci***
*****************

gen nempleos_ci=.
replace nempleos_ci=1 if emp_ci==1 & nempleos==2
replace nempleos_ci=2 if emp_ci==1 & nempleos==1
replace nempleos_ci=. if emp_ci==0


*****************
***firmapeq_ci***
*****************
/* Y.L. -> comento firmapeq_ci
gen firmapeq_ci=.
replace firmapeq_ci=1 if tamestpr<=5
replace firmapeq_ci=0 if tamestpr>5 & tamestpr<99
replace firmapeq_ci=. if emp_ci==0
*/
*****************
***spublico_ci***
*****************

gen spublico_ci=((secintpr>=1 & secintpr<=5) | secintpr==10)
replace spublico_ci=. if emp_ci==0 | secintpr==9


**************
***ocupa_ci***
**************

gen ocupa_ci=.
replace ocupa_ci=1 if ocupacpr>=0 & ocupacpr<=95 & emp_ci==1
replace ocupa_ci=2 if ocupacpr>=100 & ocupacpr<=128 & emp_ci==1
replace ocupa_ci=3 if ocupacpr>=200 & ocupacpr<=279 & emp_ci==1
replace ocupa_ci=4 if ocupacpr>=300 & ocupacpr<=340 & emp_ci==1
replace ocupa_ci=5 if ocupacpr>=900 & ocupacpr<=954 & emp_ci==1
replace ocupa_ci=6 if ocupacpr>=400 & ocupacpr<=451 & emp_ci==1
replace ocupa_ci=7 if ocupacpr>=500 & ocupacpr<=808 & emp_ci==1
replace ocupa_ci=9 if ocupacpr==980 & emp_ci==1


*************
***rama_ci***
*************

gen rama_ci=.
replace rama_ci=1 if ramapr>=1000 & ramapr<=1900 & emp_ci==1
replace rama_ci=2 if ramapr>=2200 & ramapr<=2903 & emp_ci==1
replace rama_ci=3 if ramapr>=3111 & ramapr<=3909 & emp_ci==1
replace rama_ci=4 if ramapr>=4101 & ramapr<=4200 & emp_ci==1
replace rama_ci=5 if ramapr>=5000 & emp_ci==1
replace rama_ci=6 if ramapr>=6100 & ramapr<=6320 & emp_ci==1
replace rama_ci=7 if ramapr>=7111 & ramapr<=7200 & emp_ci==1
replace rama_ci=8 if ramapr>=8101 & ramapr<=8330 & emp_ci==1
replace rama_ci=9 if ramapr>=9100 & ramapr<=9600 & emp_ci==1



****************
***durades_ci***
****************

gen durades_ci=.
/*Esta variable no está en el archivo (no fue suministrada por el país)*/
 
*******************
***antiguedad_ci***
*******************

gen antiguedad_ci=.

*******************
***tamemp_ci***
*******************
         
*Costa Rica Pequeña 1 a 5, Mediana 6 a 19, Grande Más de 19
/*
gen tamemp_ci = 1 if (tamestpr>=1 & tamestpr<=5)
replace tamemp_ci = 2 if (tamestpr>=6 & tamestpr<=19)
replace tamemp_ci = 3 if (tamestpr>19)
*/
* 2014, 06 modificacion MLO
gen tamemp_ci = 1 if (tamestpr>=1 & tamestpr<=5)
replace tamemp_ci = 2 if (tamestpr>=6 & tamestpr<=10)
replace tamemp_ci = 3 if (tamestpr>10 & tamestpr!=.)
replace tamemp_ci = . if (tamestpr==99)

label define tamemp_ci 1 "Pequeña" 2 "Mediana" 3 "Grande"
label value tamemp_ci tamemp_ci
label var tamemp_ci "Tamaño de empresa"

*******************
***categoinac_ci***
*******************
/*
inactiv:
           0 no aplic
           1 pensiona
           2 rentista
           3 estudian
           4 labores 
           5 incapaci
           6 otro ina
*/


gen categoinac_ci = 1 if (inactiv == 1 & condocup_ci==3)
replace categoinac_ci = 2 if (inactiv == 3 & condocup_ci==3)
replace categoinac_ci = 3 if (inactiv == 4 & condocup_ci==3)
replace categoinac_ci = 4 if  ((categoinac_ci ~=1 & categoinac_ci ~=2 & categoinac_ci ~=3) & condocup_ci==3)
label var categoinac_ci "Categoría de inactividad"
label define categoinac_ci 1 "jubilados o pensionados" 2 "Estudiantes" 3 "Quehaceres domésticos" 4 "Otros" 

*******************
***formal***
*******************
*Modificación Mayra Sáenz - Septiembre 2014
*Las variables afiliado y cotizando son misssing.
/*
gen formal=1 if cotizando_ci==1

gen byte formal_ci=1 if formal==1 & (condocup_ci==1 | condocup_ci==2)
recode formal_ci .=0 if (condocup_ci==1 | condocup_ci==2)
label var formal_ci "1=afiliado o cotizante / PEA"*/
gen byte formal_ci=.
label var formal_ci "1=afiliado o cotizante / PEA"
*************************************************************************************
*******************************INGRESOS**********************************************
*************************************************************************************

***************
***ylmpri_ci***
***************


gen ylmpri_ci=ymenppal
replace ylmpri_ci=. if ymenppal==999999
replace ylmpri_ci=. if emp_ci==0


*****************
***nrylmpri_ci***
*****************

gen nrylmpri_ci=(ymenppal==999999)
replace nrylmpri_ci=. if emp_ci==0 

***************
***ylmsec_ci***
***************

gen ylmsec_ci=ymensec
replace ylmsec_ci=. if  ymensec==999999
replace ylmsec_ci=. if emp_ci==0 | nempleos_ci==1


**********************************************************************************************
***TCYLMPRI_CH : Identificador de los hogares en donde alguno de los miembros reporta como
*** top-code el ingreso de la actividad principal. .
***********************************************************************************************
gen tcylmpri_ch = .
label var tcylmpri_ch "Id hogar donde algún miembro reporta como top-code el ingr de activ. principal"

***********************************************************************************************
***TCYLMPRI_CI : Identificador de top-code del ingreso de la actividad principal.
***********************************************************************************************
gen tcylmpri_ci = .
label var tcylmpri_ci "Identificador de top-code del ingreso de la actividad principal"


************
***ylm_ci***
************

egen ylm_ci=rsum(ylmpri_ci ylmsec_ci), missing
replace ylm_ci=. if ylmpri_ci==. & ylmsec_ci==.


*************
***ynlm_ci***
*************

gen ynlm_ci=otrosing
replace ynlm_ci=. if otrosing==999999


***N/A***
gen ylnmpri_ci=.
gen ylnmsec_ci=.
gen ylnm_ci=.
gen ynlnm_ci=.
gen autocons_ci=.
gen remesas_ci=.


************************
*** HOUSEHOLD INCOME ***
************************

*******************
*** nrylmpri_ch ***
*******************

*Creating a Flag label for those households where someone has a ylmpri_ci as missing

by idh_ch, sort: egen nrylmpri_ch=sum(nrylmpri_ci) if miembros_ci==1, missing
replace nrylmpri_ch=1 if nrylmpri_ch>0 & nrylmpri_ch<.
replace nrylmpri_ch=. if nrylmpri_ch==.

**************
*** ylm_ch ***
**************

by idh_ch, sort: egen ylm_ch=sum(ylm_ci) if miembros_ci==1, missing

****************
*** ylmnr_ch ***
****************

by idh_ch, sort: egen ylmnr_ch=sum(ylm_ci) if miembros_ci==1, missing
replace ylmnr_ch=. if nrylmpri_ch==1


***************
*** ynlm_ch ***
***************

by idh_ch, sort: egen ynlm_ch=sum(ynlm_ci) if miembros_ci==1, missing

* N/A*
gen ylnm_ch=.
gen ynlnm_ch=.
gen autocons_ch=.
gen rentaimp_ch=.
gen remesas_ch=.

*****************
***ylhopri_ci ***
*****************

gen ylmhopri_ci=ylmpri_ci/(horaspri_ci*4.3)


***************
***ylmho_ci ***
***************

gen ylmho_ci=ylm_ci/(horastot_ci*4.3)

*****************************************************************
**YLMOTROS_CI : Ingreso laboral monetario otros trabajos.    ***
*********************************************************************

gen ylmotros_ci=.


*********************************************************************
***  YLNMOTROS_CI : Ingreso laboral no monetario otros trabajos.
*********************************************************************

gen ylnmotros_ci=.


****************************
***VARIABLES DE EDUCACION***
****************************

gen byte aedu_ci=.

replace aedu_ci=0 if niveduc==0 | niveduc==1 

*Primaria
replace aedu_ci=1 if niveduc==11 
replace aedu_ci=2 if niveduc==12
replace aedu_ci=3 if niveduc==13
replace aedu_ci=4 if niveduc==14
replace aedu_ci=5 if niveduc==15
replace aedu_ci=6 if niveduc==16

*Secundaria (académica y técnica)
replace aedu_ci=7 if niveduc==21 | niveduc==31
replace aedu_ci=8 if niveduc==22 | niveduc==32
replace aedu_ci=9 if niveduc==23 | niveduc==33
replace aedu_ci=10 if niveduc==24 | niveduc==34
replace aedu_ci=11 if niveduc==25 | niveduc==35
replace aedu_ci=12 if niveduc==36 

*Superior (universitario o para-universitario)
replace aedu_ci=13 if niveduc==41 | niveduc==51
replace aedu_ci=14 if niveduc==42 | niveduc==52
replace aedu_ci=15 if niveduc==43 | niveduc==53
replace aedu_ci=16 if niveduc==44 | niveduc==54
replace aedu_ci=17 if niveduc==55
replace aedu_ci=18 if niveduc==56
replace aedu_ci=19 if niveduc==57
replace aedu_ci=20 if niveduc==58

**************
***eduno_ci***
**************

gen byte eduno_ci=0
replace eduno_ci=1 if niveduc==0 | niveduc==1 
label variable eduno_ci "Cero anios de educacion"

**************
***edupi_ci***
**************

gen byte edupi_ci=0
replace edupi_ci=1 if (niveduc>=11 & niveduc<16) 
label variable edupi_ci "Primaria incompleta"

**************
***edupc_ci***
**************

gen byte edupc_ci=0
replace edupc_ci=1 if niveduc==16
label variable edupc_ci "Primaria completa"

**************
***edusi_ci***
**************

gen byte edusi_ci=0
replace edusi_ci=1 if (niveduc>=21 & niveduc<=25) 
replace edusi_ci=1 if (niveduc>=31 & niveduc<=35) 
label variable edusi_ci "Secundaria incompleta"

**************
***edusc_ci***
**************

gen byte edusc_ci=0
replace edusc_ci=1 if niveduc==36
label variable edusc_ci "Secundaria completa"

***************
***edus1i_ci***
***************

gen byte edus1i_ci=0
replace edus1i_ci=1 if (niveduc>=21 & niveduc<=22)
replace edus1i_ci=1 if (niveduc>=31 & niveduc<=32)
label variable edus1i_ci "1er ciclo de la secundaria incompleto"

***************
***edus1c_ci***
***************

gen byte edus1c_ci=0
replace edus1c_ci=1 if niveduc==23 | niveduc==33
label variable edus1c_ci "1er ciclo de la secundaria completo"

***************
***edus2i_ci***
***************

gen byte edus2i_ci=0
replace edus2i_ci=1 if (niveduc>=24 & niveduc<=25)
replace edus2i_ci=1 if (niveduc>=34 & niveduc<=35)
label variable edus2i_ci "2do ciclo de la secundaria incompleto"

***************
***edus2c_ci***
***************

gen byte edus2c_ci=0
replace edus2c_ci=1 if niveduc==36
label variable edus2c_ci "2do ciclo de la secundaria completo"

**************
***eduui_ci***
**************

gen byte eduui_ci=0
replace eduui_ci=1 if (niveduc>=41 & niveduc<=44)
replace eduui_ci=1 if (niveduc>=51 & niveduc<=54)
label variable eduui_ci "Superior incompleto"

***************
***eduuc_ci***
***************

gen byte eduuc_ci=0
replace eduuc_ci=1 if (niveduc>=55 & niveduc<=58)
label variable eduuc_ci "Superior completo"

local var = "eduno edupi edupc edusi edusc edusc eduui eduuc edus1i edus1c edus2i edus2c"
foreach x of local var {
replace `x'_ci=. if aedu_ci==.
}



***************
***edupre_ci***
***************

gen byte edupre_ci=.
replace edupre_ci=(niveduc==1)
label variable edupre_ci "Educacion preescolar"


**************
***eduac_ci***
**************
gen byte eduac_ci=.
replace eduac_ci=0 if (niveduc>=41 & niveduc<=49) 
replace eduac_ci=1 if (niveduc>=51 & niveduc<=59) 
label variable eduac_ci "Superior universitario vs superior no universitario"

***************
***asiste_ci***
***************

gen asiste_ci=.
replace asiste_ci=1 if asiste>=1 & asiste<=6
replace asiste_ci=0 if asiste==7
label variable asiste_ci "Asiste actualmente a la escuela"

**************
***pqnoasis***
**************

gen pqnoasis_ci=.
/*Esta variable no está en el archivo (no fue suministrada por el país)*/

**************
*pqnoasis1_ci*
**************
**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**

g       pqnoasis1_ci = 1 if pqnoasis_ci==3
replace pqnoasis1_ci = 2 if pqnoasis_ci==1
replace pqnoasis1_ci = 3 if pqnoasis_ci==6 | pqnoasis_ci==7
replace pqnoasis1_ci = 4 if pqnoasis_ci==8
replace pqnoasis1_ci = 5 if pqnoasis_ci==2 | pqnoasis_ci==5
replace pqnoasis1_ci = 7 if pqnoasis_ci==9 
replace pqnoasis1_ci = 8 if pqnoasis_ci==4
replace pqnoasis1_ci = 9 if pqnoasis_ci==10

label define pqnoasis1_ci 1 "Problemas económicos" 2 "Por trabajo" 3 "Problemas familiares o de salud" 4 "Falta de interés" 5	"Quehaceres domésticos/embarazo/cuidado de niños/as" 6 "Terminó sus estudios" 7	"Edad" 8 "Problemas de acceso"  9 "Otros"
label value  pqnoasis1_ci pqnoasis1_ci


***************
***repite_ci***
***************

gen repite_ci=.
gen repiteult_ci=.

***************
***edupub_ci***
***************

gen edupub_ci=.

*************
***tecnica_ci**
*************
gen tecnica_ci=.
replace tecnica_ci=1 if  niveduc>=41 &  niveduc<=44
recode tecnica_ci .=0 
label var tecnica_ci "=1 formacion terciaria tecnica"


**********************************
**** VARIABLES DE LA VIVIENDA ****
**********************************

/*En este año no hay sección sobre las caracteristicas de la vivienda*/


gen aguared_ch=.
gen aguamala_ch=.
gen aguadist_ch=.
gen aguamide_ch=.
gen luz_ch=.
gen luzmide_ch=.
gen combust_ch=.
gen bano_ch=.
gen banoex_ch=.
gen des1_ch=.
gen des2_ch=.
gen piso_ch=.
gen pared_ch=.
gen techo_ch=.
gen resid_ch=.
**Daniela Zuluaga- Enero 2018: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**
g       aguamejorada_ch =.
g       banomejorado_ch = .
gen dorm_ch=.
gen cuartos_ch=.
gen cocina_ch=.
gen telef_ch=.
gen refrig_ch=.
gen freez_ch=.
gen auto_ch=.
gen compu_ch=.
gen internet_ch=.
gen cel_ch=.
gen vivi1_ch=.
gen vivi2_ch=.
gen viviprop_ch=.
gen vivitit_ch=.
gen vivialq_ch=.
gen vivialqimp_ch=.
ren ocup ocup_old


	**************************
	** REGIONES **************
	**************************

	gen ine01=.   
	replace ine01=1 if  region==1				/*Área Met*/
	replace ine01=2 if  region==2				/*Resto re*/
	replace ine01=3 if  region==3				/*Chorotega*/
	replace ine01=4 if  region==4				/*Pacífico central*/
	replace ine01=5 if  region==5				/*Brunca*/
	replace ine01=6 if  region==6		    	/*Huetar Atlántica*/
	replace ine01=7 if  region==7				/*Huetar Norte*/
	
	label define ine01 1"Área Met" 2"Resto re" 3"Chorotega" 4"Pacífico central" 5"Brunca" 6"Huetar Atlántica" 7"Huetar Norte" 
	label value ine01 ine01
	label var ine01 " Primera division politico-administrativa, Región"

	gen geolev1=.
	replace geolev1=18801 if  region==1 | region==2	/*Central*/
	replace geolev1=18802 if  region==3				/*Chorotega*/
	replace geolev1=18803 if  region==4				/*Pacífico central*/
	replace geolev1=18804 if  region==5				/*Brunca*/
	replace geolev1=18805 if  region==6		    	/*Huetar Atlántica*/
	replace geolev1=18806 if  region==7				/*Huetar Norte*/
	
	label define geolev1 18801"Central" 18802"Chorotega" 18803"Pacífico central" 18804"Brunca" 18805"Huetar Atlántica" 18806"Huetar Norte" 
	label value geolev1 geolev1
	label var geolev1 " Primera division politico-administrativa, Región"


/*_____________________________________________________________________________________________________*/
* Asignación de etiquetas e inserción de variables externas: tipo de cambio, Indice de Precios al 
* Consumidor (2011=100), Paridad de Poder Adquisitivo (PPA 2011),  líneas de pobreza
/*_____________________________________________________________________________________________________*/


do "$gitFolder\armonizacion_microdatos_encuestas_hogares_scl\_DOCS\\Labels&ExternalVars_Harmonized_DataBank.do"

/*_____________________________________________________________________________________________________*/
* Verificación de que se encuentren todas las variables armonizadas 
/*_____________________________________________________________________________________________________*/

order region_BID_c region_c pais_c anio_c mes_c zona_c factor_ch	idh_ch	idp_ci	factor_ci sexo_ci edad_ci ///
raza_idioma_ci  id_ind_ci id_afro_ci raza_ci  relacion_ci civil_ci jefe_ci nconyuges_ch nhijos_ch notropari_ch notronopari_ch nempdom_ch ///
clasehog_ch nmiembros_ch miembros_ci nmayor21_ch nmenor21_ch nmayor65_ch nmenor6_ch	nmenor1_ch	condocup_ci ///
categoinac_ci nempleos_ci emp_ci antiguedad_ci	desemp_ci cesante_ci durades_ci	pea_ci desalent_ci subemp_ci ///
tiempoparc_ci categopri_ci categosec_ci rama_ci spublico_ci tamemp_ci cotizando_ci instcot_ci	afiliado_ci ///
formal_ci tipocontrato_ci ocupa_ci horaspri_ci horastot_ci	pensionsub_ci pension_ci tipopen_ci instpen_ci	ylmpri_ci nrylmpri_ci ///
tcylmpri_ci ylnmpri_ci ylmsec_ci ylnmsec_ci	ylmotros_ci	ylnmotros_ci ylm_ci	ylnm_ci	ynlm_ci	ynlnm_ci ylm_ch	ylnm_ch	ylmnr_ch  ///
ynlm_ch	ynlnm_ch ylmhopri_ci ylmho_ci rentaimp_ch autocons_ci autocons_ch nrylmpri_ch tcylmpri_ch remesas_ci remesas_ch	ypen_ci	ypensub_ci ///
salmm_ci tc_c ipc_c lp19_c lp31_c lp5_c lp_ci lpe_ci aedu_ci eduno_ci edupi_ci edupc_ci	edusi_ci edusc_ci eduui_ci eduuc_ci	edus1i_ci ///
edus1c_ci edus2i_ci edus2c_ci edupre_ci eduac_ci asiste_ci pqnoasis_ci pqnoasis1_ci	repite_ci repiteult_ci edupub_ci tecnica_ci ///
aguared_ch aguadist_ch aguamala_ch aguamide_ch luz_ch luzmide_ch combust_ch	bano_ch banoex_ch des1_ch des2_ch piso_ch aguamejorada_ch banomejorado_ch  ///
pared_ch techo_ch resid_ch dorm_ch cuartos_ch cocina_ch telef_ch refrig_ch freez_ch auto_ch compu_ch internet_ch cel_ch ///
vivi1_ch vivi2_ch viviprop_ch vivitit_ch vivialq_ch	vivialqimp_ch , first


rename ramapr codindustria
rename ocupacpr codocupa
compress


saveold "`base_out'", replace


log close








