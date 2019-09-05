
* (Versión Stata 12)
clear
set more off
*________________________________________________________________________________________________________________*

 * Activar si es necesario (dejar desactivado para evitar sobreescribir la base y dejar la posibilidad de 
 * utilizar un loop)
 * Los datos se obtienen de las carpetas que se encuentran en el servidor: \\Sdssrv03\surveys
 * Se tiene acceso al servidor únicamente al interior del BID.
 * El servidor contiene las bases de datos MECOVI.
 *________________________________________________________________________________________________________________*
* global ruta = "\\Sdssrv03\surveys"

local PAIS BOL
local ENCUESTA ECH
local ANO "2001"
local ronda m11_m12 


local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\\`PAIS'_`ANO'`ronda'.dta"
*local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
                                                    
capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Bolivia
Encuesta: ECH
Round: m11_m12
Autores: 
Última versión: Mayra Sáenz E-mail: mayras@iadb.org / saenzmayra.a@gmail.com
Fecha última modificación: 4 de Octubre de 2013

							SCL/LMK - IADB
****************************************************************************/
/***************************************************************************
Detalle de procesamientos o modificaciones anteriores:

****************************************************************************/


use `base_in', clear


	****************
	* region_BID_c *
	****************
	
gen region_BID_c=3

label var region_BID_c "Regiones BID"
label define region_BID_c 1 "Centroamérica_(CID)" 2 "Caribe_(CCB)" 3 "Andinos_(CAN)" 4 "Cono_Sur_(CSC)"
label value region_BID_c region_BID_c
***************
***factor_ch***
***************

gen factor_ch=factor
label variable factor_ch "Factor de expansion del hogar"


**************
****idh_ch****
**************
sort folio miembr_1 gconsum
egen idh_ch=group(folio miembr_1 gconsum)
format idh_ch %20.0f
label variable idh_ch "ID del hogar"

*************
****idp_ci***
*************

gen idp_ci=nro1a
label variable idp_ci "ID de la persona en el hogar"

**********
***zona***
**********
gen byte zona_c=0 if urbrur==2
replace zona_c=1 if urbrur==1

label variable zona_c "Zona del pais"
label define zona_c 1 "Urbana" 0 "Rural"
label value zona_c zona_c


**************
***region_c***
**************

gen region_c=.
label var region_c "Region" 

************
****pais****
************

gen str3 pais_c="BOL"
label variable pais_c "Pais"

**********
***anio***
**********

gen anio_c=2001
label variable anio_c "Anio de la encuesta"

*********
***mes***
*********

gen mes_c=10
label variable mes_c "Mes de la encuesta"

*****************
***relacion_ci***
*****************

gen relacion_ci=.
replace relacion_ci=1 if s105==1
replace relacion_ci=2 if s105==2
replace relacion_ci=3 if s105==3
replace relacion_ci=4 if s105>=4 & s105<=8
replace relacion_ci=5 if s105==10 | s105==11
replace relacion_ci=6 if s105==9

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

gen factor_ci=factor
label variable factor_ci "Factor de expansion del individuo"

**********
***sexo***
**********

gen sexo_ci=s102

label define sexo_ci 1 "Hombre" 2 "Mujer"
label value sexo_ci sexo_ci

**********
***edad***
**********

gen edad_ci=s103
label variable edad_ci "Edad del individuo"


*****************
***civil_ci***
*****************

gen civil_ci=.
replace civil_ci=1 if s110==1
replace civil_ci=2 if s110==2 | s110==3
replace civil_ci=3 if s110==4 | s110==5
replace civil_ci=4 if s110==6

label variable civil_ci "Estado civil"
label define civil_ci 1 "Soltero" 2 "Union formal o informal"
label define civil_ci 3 "Divorciado o separado" 4 "Viudo" , add
label value civil_ci civil_ci


*************
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


*************
***raza_ci***
*************
/*
gen raza_ci=.
replace raza_ci= 1 if  (s111a >=1 & s111a <=6)
replace raza_ci= 3 if (s111a  ==7) 
bys idh_ch: gen aux=raza_ci if relacion_ci==1
bys idh_ch: egen aux1 = max(aux)
replace raza_ci=aux1 if (raza_ci ==. & relacion_ci ==3)  
replace raza_ci=3 if raza_ci==. 
drop aux aux1
label define raza_ci 1 "Indígena" 2 "Afro-descendiente" 3 "Otros"
label value raza_ci raza_ci 
label value raza_ci raza_ci
label var raza_ci "Raza o etnia del individuo" 
*/


/* Actualizacion LCM (introducido por YL):
Para el grupo indígena se toma <=5 y otros = 6 | =7 
Se puede recuperar afros a partir de la pregunta s111b LCM dic2013*/

gen raza_ci=.
replace raza_ci= 1 if  (s111a >=1 & s111a <=5)
tab s111b, gen(puebl_)
replace raza_ci= 2 if (puebl_1==1 | puebl_2==1) & raza_ci==.
drop puebl_* 
replace raza_ci= 3 if (s111a==6 | s111a==7) & raza_ci==.
bys idh_ch: gen aux=raza_ci if relacion_ci==1
bys idh_ch: egen aux1 = max(aux)
replace raza_ci=aux1 if (raza_ci ==. & relacion_ci ==3)  
replace raza_ci=3 if raza_ci==. 
drop aux aux1
label define raza_ci 1 "Indígena" 2 "Afro-descendiente" 3 "Otros"
label value raza_ci raza_ci 
label var raza_ci "Raza o etnia del individuo" 

************************************
*** VARIABLES DEL MERCADO LABORAL***
************************************
/* Esta sección es para los residentes habituales del hogar mayores a 7 años*/ 

*******************
****** tc_c *******
*******************

gen tc_c = 6.606917

label var tc_c "Tasa de cambio LCU/USD"

*******************
****** ipc_c ******
*******************

gen ipc_c = 63.62639


label var ipc_c "Índice de precios al consumidor"

*******************
****** ppp_c ******
*******************

gen ppp_c = 2.139067

label var ppp_c "Factor de conversión PPP LCU/USD"

****************
*lp25_2005_ci***
****************

gen lp25_2005_ci = 170.2867

label var lp25_2005_ci  "Línea de pobreza USD2.5 por día en moneda local a precios corrientes a PPP 2005"

***************
*lp4_2005_ci***
***************

gen lp4_2005_ci =  272.4586
  
label var lp4_2005_ci "Línea de pobreza USD4 por día en moneda local a precios corrientes a PPP 2005"

********* 
*lpl25_ci
*********

gen lp25_ci = 140.6049
 
label var lp25_ci  "Línea de pobreza USD2.5 por día en moneda local a precios corrientes a PPP 2011"

*********
*lpl4_ci*
*********

gen lp4_ci = 224.9678

label var lp4_ci "Línea de pobreza USD4 por día en moneda local a precios corrientes a PPP 2011"

*********
*lp_ci***
*********
gen lp_ci =.
replace lp_ci= 333.256465112295 if ciudad==1 & zona_c==1
replace lp_ci= 327.4177151355 if ciudad==2 & zona_c==1
replace lp_ci= 348.838773914489 if ciudad==3 & zona_c==1
replace lp_ci= 297.733522825274 if ciudad==4 & zona_c==1
replace lp_ci= 273.832248708337 if ciudad==5 & zona_c==1
replace lp_ci= 348.838773914489 if ciudad==6 & zona_c==1
replace lp_ci= 343.201658757807 if ciudad==7 & zona_c==1
replace lp_ci= 343.201658757807 if ciudad==8 & zona_c==1
replace lp_ci= 271.523841740591 if ciudad==10 & zona_c==1
replace lp_ci= 343.201658757807 if ciudad==9 & zona_c==1



replace lp_ci=   231.47 if  zona_c==0




label var lp_ci "Linea de pobreza oficial del pais"

*********
*lpe_ci***
*********

gen lpe_ci =.
replace lpe_ci= 168.294514881709 if ciudad==1 & zona_c==1
replace lpe_ci= 182.044249615338 if ciudad==2 & zona_c==1
replace lpe_ci= 176.163580826817 if ciudad==3 & zona_c==1
replace lpe_ci= 165.539838690853 if ciudad==4 & zona_c==1
replace lpe_ci= 152.250730281835 if ciudad==5 & zona_c==1
replace lpe_ci= 176.163580826817 if ciudad==6 & zona_c==1
replace lpe_ci= 174.346442648966 if ciudad==7 & zona_c==1
replace lpe_ci= 174.346442648966 if ciudad==8 & zona_c==1
replace lpe_ci= 164.814971936539 if ciudad==10 & zona_c==1
replace lpe_ci= 174.346442648966 if ciudad==9 & zona_c==1



replace lpe_ci=    131.53    if  zona_c==0



label var lpe_ci "Linea de indigencia oficial del pais"

*************
**salmm_ci***
*************

*BOL 2001
gen salmm_ci=400.00
* revisado por Lourdes Montesdeoca dic/2013
label var salmm_ci "Salario minimo legal"

****************
*cotizando_ci***
****************
gen cotizando_ci=.

*recode cotizando_ci .=0 if condact>=1 & condact<=3
label var cotizando_ci "Cotizante a la Seguridad Social"

****************
*afiliado_ci****
****************
gen afiliado_ci= 1 if   s651b ==1	

recode afiliado_ci .=0 if ocup>=1 & ocup<=3
label var afiliado_ci "Afiliado a la Seguridad Social"

****************
*tipopen_ci*****
****************
gen tipopen_ci=.


replace tipopen_ci=1 if s702c>0 &  s702c~=.
replace tipopen_ci=2 if s702f>0 & s702f~=.
replace tipopen_ci=3 if s702d>0 & s702d~=.
replace tipopen_ci=4 if s702e>0 & s702e~=. 
replace tipopen_ci=12 if (s702c>0 & s702f>0) & (s702c~=. & s702f~=.)
replace tipopen_ci=13 if (s702c>0 & s702d>0) & (s702c~=. & s702d~=.)
replace tipopen_ci=23 if (s702f>0 & s702d>0) & (s702f~=. & s702d~=.)
replace tipopen_ci=123 if (s702c>0 & s702f>0 & s702d>0) & (s702c~=. & s702f~=. & s702d~=.)
label define  t 1 "Jubilacion" 2 "Viudez/orfandad" 3 "Benemerito" 4 "Invalidez" 12 "Jub y viudez" 13 "Jub y benem" 23 "Viudez y benem" 123 "Todas"
label value tipopen_ci t
label var tipopen_ci "Tipo de pension - variable original de cada pais" 

****************
*instpen_ci*****
****************
gen instpen_ci=.
label var instpen_ci "Institucion proveedora de la pension - variable original de cada pais" 
gen instcot_ci=. 



****************
****condocup_ci*
****************
/*
gen condocup_ci=.
replace condocup_ci=1 if ocup==1
replace condocup_ci=2 if ocup==2 | ocup==3
replace condocup_ci=3 if (ocup!=1 & ocup!=2 & ocup!=3) & edad_ci>=10
replace condocup_ci=4 if edad_ci<10
label var condocup_ci "Condicion de ocupación de acuerdo a def de cada pais"
label define condocup_ci 1 "Ocupado" 2 "Desocupado" 3 "Inactivo" 4 "Menor que 10" 
label value condocup_ci condocup_ci
*/

* Homologacion toda la serie 05/22/2014 MGD

gen condocup_ci=.
replace condocup_ci=1 if s601==1 | s602<=6  | s603a==1
replace condocup_ci=2 if (s601==2 | s602==7 | s603a>1) & (s605==1 | s606==1) & (s604==1)
recode condocup_ci .=3 if edad_ci>=10
recode condocup_ci .=4 if edad_ci<10
label var condocup_ci "Condicion de ocupación de acuerdo a def de cada pais"
label define condocup_ci 1 "Ocupado" 2 "Desocupado" 3 "Inactivo" 4 "Menor que 10" 
label value condocup_ci condocup_ci

*************
*cesante_ci* 
*************
*Mayra Sáenz Octubre 2013 - Se cambia la s650 por la s610 que corresponde a la sección de condición de actividad.
gen cesante_ci=1 if  s610==1 & condocup_ci==2
* 2014, 03 Modificacion siguiente linea MLO
replace cesante_ci=0 if s610==2 & condocup_ci==2
*replace cesante_ci=0 if  s610==0 & condocup_ci==3
*replace cesante_ci=0 if condocup_ci==3 & cesante_ci != 1

label var cesante_ci "Desocupado - definicion oficial del pais"	

*************
*tamemp_ci
*************
*Bolivia Pequeña 1 a 5 Mediana 6 a 49 Grande Más de 49
gen tamemp_ci=.
replace tamemp_ci=1 if s623==1 | s623==2
replace tamemp_ci=2 if s623>=3 & s623<=6
replace tamemp_ci=3 if s623>=7 & s623<=8
label var tamemp_ci "# empleados en la empresa segun rangos"
label define tamemp_ci 1 "Pequena" 2 "Mediana" 3 "Grande"
label value tamemp_ci tamemp_ci

*************
**pension_ci*
*************

egen aux_p=rsum(s702c s702d s702e s702f s702g), missing
gen pension_ci=1 if aux_p>0 & aux_p!=.
recode pension_ci .=0 
label var pension_ci "1=Recibe pension contributiva"

*************
**ypen_ci*
*************

gen ypen_ci=aux_p 
recode ypen_ci .=0 
label var ypen_ci "Valor de la pension contributiva"

***************
*pensionsub_ci*
***************
*el monto es anual
*2012, 07 modificacion MLO
gen aux_ps= s704d/12  if s704d >0 & s704d !=. 
gen byte pensionsub_ci=1 if aux_ps>0 & aux_ps!=.
recode pensionsub_ci .=0
label var pensionsub_ci "1=recibe pension subsidiada / no contributiva"

*****************
**ypensub_ci*
*****************
destring aux_ps, replace
gen  ypensub_ci=aux_ps

label var ypensub_ci "Valor de la pension subsidiada / no contributiva"
	
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

gen desalent_ci=(emp_ci==0 & s605==2 & (s609a==3 | s609a==4))
replace desalent_ci=. if emp_ci==.

*****************
***horaspri_ci***
*****************

gen horas1=s624min/60

egen horaspri_ci=rsum(s624hrs horas1), missing
replace horaspri_ci=. if s624hrs==. & horas1==.
replace horaspri_ci=horaspri_ci*s624a
replace horaspri_ci=. if emp_ci~=1

drop horas1


*****************
***horastot_ci***
*****************

gen horas2=s638min/60

egen horassec=rsum(s638hrs horas2), missing
replace horassec=. if s638hrs==. & horas2==.
replace horassec=horassec*s638a
replace horassec=. if emp_ci~=1

egen horastot_ci=rsum(horaspri_ci horassec), missing
replace horastot_ci=. if horaspri_ci==. & horassec==.
replace horastot_ci=. if emp_ci~=1

drop horas2
drop horassec

***************
***subemp_ci***
***************
/*
gen subemp_ci=(horastot_ci<=30 & s645==1)
replace subemp_ci=. if emp_ci~=1
*/

* Segun definicion del documento metodologico: horas de la actividad principal y si esta disponible a trabajar mas horas. MGD 06/18/2014
* Se podria considerar a las dos alternativas: desea trabajar y esta dispuesto a trabajar.
gen subemp_ci=0
*replace subemp_ci=1 if s646==1  & horaspri_ci <= 30 & emp_ci==1
replace subemp_ci=1 if (s646==1 & s645==1)  & horaspri_ci <= 30 & emp_ci==1
label var subemp_ci "Personas en subempleo por horas"

*******************
***tiempoparc_ci***
*******************

gen tiempoparc_ci=(horastot_ci<=30 & s645==2)
replace tiempoparc_ci=. if emp_ci~=1


******************
***categopri_ci***
******************

gen categopri_ci=.
replace categopri_ci=1 if s617>=4 & s617<=6
replace categopri_ci=2 if s617==3
replace categopri_ci=3 if s617==1 | s617==2 | s617==8
replace categopri_ci=4 if s617==7
replace categopri_ci=. if emp_ci~=1

label define categopri_ci 1"Patron" 2"Cuenta propia" 
label define categopri_ci 3"Empleado" 4" No remunerado", add
label value categopri_ci categopri_ci
label variable categopri_ci "Categoria ocupacional trabajo principal"


******************
***categosec_ci***
******************

gen categosec_ci=.
replace categosec_ci=1 if s635>=4 & s635<=6
replace categosec_ci=2 if s635==3
replace categosec_ci=3 if s635==1 | s635==2 | s635==8
replace categosec_ci=4 if s635==7

label define categosec_ci 1"Patron" 2"Cuenta propia" 
label define categosec_ci 3"Empleado" 4 "No remunerado" , add
label value categosec_ci categosec_ci
label variable categosec_ci "Categoria ocupacional trabajo secundario"


*****************
*tipocontrato_ci*
*****************

gen tipocontrato_ci=.
replace tipocontrato_ci=1 if  s618==3 & categopri_ci==3
replace tipocontrato_ci=2 if s618==1 & categopri_ci==3
replace tipocontrato_ci=3 if ((s618==2 | s618==4) | tipocontrato_ci==.) & categopri_ci==3
label var tipocontrato_ci "Tipo de contrato segun su duracion"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci


*****************
***nempleos_ci***
*****************

gen nempleos_ci=.
replace nempleos_ci=1 if emp_ci==1
replace nempleos_ci=2 if emp_ci==1 & s632==1
/*

*****************
***firmapeq_ci***
*****************

gen firmapeq_ci=.
replace firmapeq_ci=1 if s623>=1 & s623<=2 /*1 a 4 personas*/
replace firmapeq_ci=0 if s623>=3 & s623<=8 /*5 o más personas*/
*/

*****************
***spublico_ci***
*****************
/*Para los obreros y empleados*/

gen spublico_ci=.
replace spublico_ci=1 if s619==2
replace spublico_ci=0 if s619==1
replace spublico_ci=. if emp_ci~=1


**************
***ocupa_ci***
**************

* Modificacion MGD 07/24/2014: clasificacion CIUO -88
gen aux= s612cod 
gen ocupa_ci=.
replace ocupa_ci=1 if (aux>=210 & aux<=348)  & emp_ci==1
replace ocupa_ci=2 if (aux>=110 & aux<=131)  & emp_ci==1
replace ocupa_ci=3 if (aux>=410 & aux<=422) & emp_ci==1
replace ocupa_ci=4 if ((aux>=520 & aux<=529) | (aux>=910 & aux<=911)) & emp_ci==1
replace ocupa_ci=5 if ((aux>=510 & aux<=519) | (aux>=912 & aux<=916)) & emp_ci==1
replace ocupa_ci=6 if ((aux>=610 & aux<=621) | (aux>=920 & aux<=921)) & emp_ci==1
replace ocupa_ci=7 if ((aux>=710 & aux<=851) | (aux>=930 & aux<=933))& emp_ci==1
replace ocupa_ci=8 if (aux>=0 & aux<=11) & emp_ci==1

drop aux

label define ocupa_ci 1"profesional y tecnico" 2"director o funcionario sup" 3"administrativo y nivel intermedio"
label define ocupa_ci  4 "comerciantes y vendedores" 5 "en servicios" 6 "trabajadores agricolas", add
label define ocupa_ci  7 "obreros no agricolas, conductores de maq y ss de transporte", add
label define ocupa_ci  8 "FFAA" 9 "Otras ", add
label value ocupa_ci ocupa_ci
label variable ocupa_ci "Ocupacion laboral"



*************
***rama_ci***
*************

gen rama_ci=.
replace rama_ci=1 if (s613cod>=11 & s613cod<=500) & emp_ci==1
replace rama_ci=2 if (s613cod>=1110 & s613cod<=1429) & emp_ci==1
replace rama_ci=3 if (s613cod>=1511 & s613cod<=3699) & emp_ci==1
replace rama_ci=4 if (s613cod>=4010 & s613cod<=4100) & emp_ci==1
replace rama_ci=5 if (s613cod>=4510 & s613cod<=4540) & emp_ci==1
replace rama_ci=6 if (s613cod>=5010 & s613cod<=5521) & emp_ci==1 
replace rama_ci=7 if (s613cod>=6010 & s613cod<=6420) & emp_ci==1
replace rama_ci=8 if (s613cod>=6519 & s613cod<=7020) & emp_ci==1
replace rama_ci=9 if (s613cod>=7111 & s613cod<=9900) & emp_ci==1



****************
***durades_ci***
****************
/*En meses*/

gen durades_ci=.
replace durades_ci=s649a/30   if s649b==1
replace durades_ci=s649a/4.3  if s649b==2
replace durades_ci=s649a/2    if s649b==3
replace durades_ci=s649a      if s649b==4
replace durades_ci=s649a*6    if s649b==5
replace durades_ci=s649a*12   if s649b==6


*******************
***antiguedad_ci***
*******************
/*Tiempo que trabaja en la empresa*/
/*En años*/

gen antiguedad_ci=.
replace antiguedad_ci=s615a/(4.3*12) if s615b==2 & emp_ci==1
replace antiguedad_ci=s615a/12 if s615b==4  & emp_ci==1
replace antiguedad_ci=s615a if s615b==6 & emp_ci==1

*******************
***categoinac_ci***
*******************
gen categoinac_ci =1 if (s608a==3 & condocup_ci==3)
replace categoinac_ci = 2 if  (s608a==1 & condocup_ci==3)
replace categoinac_ci = 3 if  (s608a==2 & condocup_ci==3)
replace categoinac_ci = 4 if  ((categoinac_ci ~=1 & categoinac_ci ~=2 & categoinac_ci ~=3) & condocup_ci==3)
label var categoinac_ci "Categoría de inactividad"
label define categoinac_ci 1 "jubilados o pensionados" 2 "Estudiantes" 3 "Quehaceres domésticos" 4 "Otros" 

*******************
***formal***
*******************
gen formal=1 if cotizando_ci==1

replace formal=1 if afiliado_ci==1 & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1 & pais_c=="BOL"   /* si se usa afiliado, se restringiendo a ocupados solamente*/
replace formal=1 if afiliado_ci==1 & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1 & pais_c=="CRI"
replace formal=1 if afiliado_ci==1 & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1 & pais_c=="GTM" & anio_c>1998
replace formal=1 if afiliado_ci==1 & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1 & pais_c=="PAN"
replace formal=1 if afiliado_ci==1 & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1 & pais_c=="PRY" & anio_c<=2006
replace formal=1 if afiliado_ci==1 & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1 & pais_c=="DOM"
replace formal=1 if afiliado_ci==1 & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1 & pais_c=="MEX" & anio_c>=2008

gen byte formal_ci=.
replace formal_ci=1 if formal==1 & (condocup_ci==1 | condocup_ci==2)
replace formal_ci=0 if formal_ci==. & (condocup_ci==1 | condocup_ci==2) 
label var formal_ci "1=afiliado o cotizante / PEA"
*************************************************************************************
*******************************INGRESOS**********************************************
*************************************************************************************

***************
***ylmpri_ci***
***************

*Para los trabajadores dependientes

*Ingreso basico
gen ypridb=.
replace ypridb=s626a*30 if s626b==1 
replace ypridb=s626a*4.3 if s626b==2 
replace ypridb=s626a*2 if s626b==3 
replace ypridb=s626a if s626b==4 
replace ypridb=s626a/6 if s626b==5
replace ypridb=s626a/12 if s626b==6

replace ypridb=0 if categopri_ci==4 

*Ingresos extras
local sub="a b c"
foreach i of local sub {
gen ypriex`i'=.
replace ypriex`i'=s627`i'2/12 if s627`i'1==1
replace ypriex`i'=0 if s627`i'1==2
}

egen ypridbd=rsum(ypridb ypriexa ypriexb ypriexc), missing
replace ypridbd=. if ypridb==. & ypriexa==. & ypriexb==. & ypriexc==.

*Para los trabajadores independientes 

gen yprijbi=.
replace yprijbi=s631a*30 if s631b==1 
replace yprijbi=s631a*4.3 if s631b==2 
replace yprijbi=s631a*2 if s631b==3 
replace yprijbi=s631a if s631b==4 
replace yprijbi=s631a/6 if s631b==5
replace yprijbi=s631a/12 if s631b==6


*Ingreso laboral monetario para todos

egen ylmpri_ci=rsum(yprijbi ypridbd), missing
replace ylmpri_ci=. if ypridbd==. & yprijbi==. 
replace ylmpri_ci=. if emp_ci~=1


*******************
*** nrylmpri_ci ***
*******************

gen nrylmpri_ci=(ylmpri_ci==. & emp_ci==1)


******************
*** ylnmpri_ci ***
******************

*Ingreso laboral no monetario de los dependientes

local nnn="a b c d e"
foreach i of local nnn {

gen especie`i'=.
replace especie`i'=s628`i'3*30  if s628`i'2==1
replace especie`i'=s628`i'3*4.3 if s628`i'2==2
replace especie`i'=s628`i'3*2   if s628`i'2==3
replace especie`i'=s628`i'3     if s628`i'2==4
replace especie`i'=s628`i'3/3   if s628`i'2==5
replace especie`i'=s628`i'3/6   if s628`i'2==6
replace especie`i'=s628`i'3/12   if s628`i'2==7
replace especie`i'=0 if s628`i'1==2
}

egen ylnmprid=rsum(especiea especieb especiec especied especiee), missing
replace ylnmprid=. if especiea==. &  especieb==. & especiec==. & especied==. & especiee==. 
replace ylnmprid=0 if categopri_ci==4


*Ingreso laboral no monetario de los independientes (autoconsumo)

gen ylnmprii=.

*Ingreso laboral no monetario para todos

egen ylnmpri_ci=rsum(ylnmprid ylnmprii), missing
replace ylnmpri_ci=. if ylnmprid==. & ylnmprii==.
replace ylnmpri_ci=. if emp_ci~=1


***************
***ylmsec_ci***
***************

*Para los trabajadores dependientes

*Ingreso basico
gen ysecb=.
replace ysecb=s640a*30 if s640b==1 
replace ysecb=s640a*4.3 if s640b==2 
replace ysecb=s640a*2 if s640b==3 
replace ysecb=s640a if s640b==4 
replace ysecb=s640a/6 if s640b==5
replace ysecb=s640a/12 if s640b==6

replace ysecb=0 if categosec_ci==4 

*Ingresos extras

gen yxsa=s641b/12 
gen yxsa1=s641b/12 


egen ysecbd=rsum(ysecb yxsa yxsa1), missing
replace ysecbd=. if ysecb==. & yxsa==. & yxsa1==.


*Para los trabajadores independientes

gen ysecjbi=.
replace ysecjbi=s644a*30 if s644b==1 
replace ysecjbi=s644a*4.3 if s644b==2 
replace ysecjbi=s644a*2 if s644b==3 
replace ysecjbi=s644a if s644b==4 
replace ysecjbi=s644a/6 if s644b==5
replace ysecjbi=s644a/12 if s644b==6

*Ingreso laboral monetario para todos

egen ylmsec_ci=rsum(ysecjbi ysecbd), missing
replace ylmsec_ci=. if ysecjbi==. & ysecbd==.
replace ylmsec_ci=. if emp_ci~=1


******************
****ylnmsec_ci****
******************

*Ingreso laboral no monetario de los dependientes


foreach i of local nnn {

gen especiesec`i'=.
replace especiesec`i'=s642`i'3*30  if s642`i'2==1
replace especiesec`i'=s642`i'3*4.3 if s642`i'2==2
replace especiesec`i'=s642`i'3*2   if s642`i'2==3
replace especiesec`i'=s642`i'3     if s642`i'2==4
replace especiesec`i'=s642`i'3/3   if s642`i'2==5
replace especiesec`i'=s642`i'3/6   if s642`i'2==6
replace especiesec`i'=s642`i'3/12  if s642`i'2==7
replace especiesec`i'=0 if s642`i'1==2
}

egen ylnmsecd=rsum(especieseca especiesecb especiesecc especiesecd especiesece), missing
replace ylnmsecd=. if especieseca==. &  especiesecb==. & especiesecc==. & especiesecd==. & especiesece==. 
replace ylnmsecd=0 if categosec_ci==4
replace ylnmsecd=. if emp_ci~=1


*Ingreso laboral no monetario de los independientes (autoconsumo)

gen ylnmseci=.

*Ingreso laboral no monetario para todos

egen ylnmsec_ci=rsum(ylnmsecd ylnmseci), missing
replace ylnmsec_ci=. if ylnmsecd==. & ylnmseci==.
replace ylnmsec_ci=. if emp_ci==0


************
***ylm_ci***
************

egen ylm_ci=rsum(ylmpri_ci ylmsec_ci), missing
replace ylm_ci=. if ylmpri_ci==. & ylmsec_ci==.


*************
***ylnm_ci***
*************

egen ylnm_ci=rsum(ylnmpri_ci ylnmsec_ci), missing
replace ylnm_ci=. if ylnmpri_ci==. & ylnmsec_ci==.


*************
***ynlm_ci***
*************

*Alquileres, dividendos, etc.
gen transl1=s701a/12
gen transl2=s701b/12
gen transl3=s701c/12

egen transltot=rsum(transl1 transl2 transl3), missing
replace transltot=. if transl1==. & transl2==. & transl3==. 


*Rentas mensuales
egen rentastot=rsum(s702a s702b s702c s702d s702e s702f s702g s702h1), missing
replace rentastot=. if s702a==. & s702b==. & s702c==. & s702d==. & s702e==. & s702f==. & s702h1==. 


* Otros ingresos no laborales

foreach i of local sub {

gen otrosnl`i'=.
replace otrosnl`i'=s703`i'1*30  if s703`i'2==1
replace otrosnl`i'=s703`i'1*4.3 if s703`i'2==2
replace otrosnl`i'=s703`i'1*2   if s703`i'2==3
replace otrosnl`i'=s703`i'1     if s703`i'2==4
replace otrosnl`i'=s703`i'1/6   if s703`i'2==5
replace otrosnl`i'=s703`i'1/12  if s703`i'2==6

}

egen yotrosnl=rsum(otrosnla otrosnlb otrosnlc), missing
replace yotrosnl=. if otrosnla==. &  otrosnlb==. & otrosnlc==. 


egen yotrosnl2=rsum(s704a s704b s704c s704d s704e), missing
replace yotrosnl2=. if s704a==. & s704b==. & s704c==. & s704d==. & s704e==. 
replace yotrosnl2=yotrosnl2/12

*Ingresos no laborales totales monetarios

egen ynlm_ci=rsum(transltot rentastot yotrosnl yotrosnl2), missing
replace ynlm_ci=. if transltot==. & rentastot==. & yotrosnl==. & yotrosnl2==.


**************
***ynlnm_ci***
**************

gen ynlnm_ci=.


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

*****************
***ylmotros_ci***
*****************

gen ylmotros_ci=.
label var ylmotros_ci "Ingreso laboral monetario de otros trabajos" 


******************
***ylnmotros_ci***
******************

gen ylnmotros_ci=.
label var ylnmotros_ci "Ingreso laboral NO monetario de otros trabajos" 



****************
***remesas_ci***
****************

gen remesas_ci=otrosnlc

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
*** ylnm_ch ***
***************

by idh_ch, sort: egen ylnm_ch=sum(ylnm_ci) if miembros_ci==1, missing

*******************
*** remesas_ch ***
*******************

by idh_ch, sort: egen remesas_ch=sum(remesas_ci) if miembros_ci==1, missing


***************
*** ynlm_ch ***
***************

by idh_ch, sort: egen ynlm_ch=sum(ynlm_ci) if miembros_ci==1, missing

****************
*** ynlnm_ch ***
****************

gen ynlnm_ch=.

*******************
*** autocons_ci ***
*******************

gen autocons_ci=.

*******************
*** autocons_ch ***
*******************

gen autocons_ch=.

*******************
*** rentaimp_ch ***
*******************

gen rentaimp_ch=s1004a
replace rentaimp_ch=rentaimp_ch*6.76 if s1004b==2


*****************
***ylhopri_ci ***
*****************

gen ylmhopri_ci=ylmpri_ci/(horaspri_ci*4.3)


***************
***ylmho_ci ***
***************

gen ylmho_ci=ylm_ci/(horastot_ci*4.3)



****************************
***VARIABLES DE EDUCACION***
****************************

/*En esta sección es sólo para los mayores a los 5 años de edad*/

gen byte aedu_ci=.

replace aedu_ci=0 if s402a==11 | s402a==12 | s402a==13

replace aedu_ci=1 if (s402a==14 | s402a==17) & s402b==1
replace aedu_ci=2 if (s402a==14 | s402a==17) & s402b==2
replace aedu_ci=3 if (s402a==14 | s402a==17) & s402b==3
replace aedu_ci=4 if (s402a==14 | s402a==17) & s402b==4
replace aedu_ci=5 if (s402a==14 | s402a==17) & s402b==5

replace aedu_ci=6 if (s402a==17 & s402b==6) | (s402a==15 & s402b==1)
replace aedu_ci=7 if (s402a==17 & s402b==7) | (s402a==15 & s402b==2)
replace aedu_ci=8 if (s402a==17 & s402b==8) | (s402a==15 & s402b==3)

replace aedu_ci=9 if (s402a==16 | s402a==18) & s402b==1
replace aedu_ci=10 if (s402a==16 | s402a==18) & s402b==2
replace aedu_ci=11 if (s402a==16 | s402a==18) & s402b==3
replace aedu_ci=12 if (s402a==16 | s402a==18) & s402b==4

replace aedu_ci=12 if (s402a>=21 & s402a<=25) & s402b==0
replace aedu_ci=13 if (s402a>=21 & s402a<=25) & s402b==1 
replace aedu_ci=14 if (s402a>=21 & s402a<=25) & s402b==2
replace aedu_ci=15 if (s402a>=21 & s402a<=25) & s402b==3
replace aedu_ci=16 if (s402a>=21 & s402a<=25) & s402b==4
replace aedu_ci=16 if (s402a==21 | s402a==23 | s402a==24) & s402b==5
replace aedu_ci=17 if (s402a==22 | s402a==25) & s402b==5

replace aedu_ci=16 if (s402a==21 | s402a==23 | s402a==24) & s402b==8
replace aedu_ci=17 if (s402a==22 | s402a==25) & s402b==8

/*El nivel 26 "Otros cursos" incluye no sólo cursos informales que 
siguen a la secundaria, sino también los cursos de postgrado. Por ende,
como no podemos distinguir entre ambos, no los consideramos y hacemos
los años de educación de estas personas missing*/

**************
***eduno_ci***
**************

gen byte eduno_ci=(s402a==11 | s402a==12 | s402a==13) 
replace eduno_ci=. if aedu_ci==.
label variable eduno_ci "Cero anios de educacion"

**************
***edupi_ci***
**************

gen byte edupi_ci=(aedu>=1 & aedu_ci<=4)
replace edupi_ci=. if aedu_ci==.
label variable edupi_ci "Primaria incompleta"

**************
***edupc_ci***
**************

gen byte edupc_ci=(aedu_ci==5)
replace edupc_ci=. if aedu_ci==.
label variable edupc_ci "Primaria completa"

**************
***edusi_ci***
**************

gen byte edusi_ci=(aedu_ci>=6 & aedu_ci<=11)
replace edusi_ci=. if aedu_ci==.
label variable edusi_ci "Secundaria incompleta"

**************
***edusc_ci***
**************

gen byte edusc_ci=(aedu_ci==12)
replace edusc_ci=. if aedu_ci==.
label variable edusc_ci "Secundaria completa"

***************
***edus1i_ci***
***************

gen byte edus1i_ci=(aedu_ci>=6 & aedu_ci<=7)
replace edus1i_ci=. if aedu_ci==.
label variable edus1i_ci "1er ciclo de la secundaria incompleto"

***************
***edus1c_ci***
***************

gen byte edus1c_ci=(aedu_ci==8)
replace edus1c_ci=. if aedu_ci==.
label variable edus1c_ci "1er ciclo de la secundaria completo"

***************
***edus2i_ci***
***************

gen byte edus2i_ci=(aedu_ci>=9 & aedu_ci<=11)
replace edus2i_ci=. if aedu_ci==.
label variable edus2i_ci "2do ciclo de la secundaria incompleto"

***************
***edus2c_ci***
***************

gen byte edus2c_ci=(aedu_ci==12)
replace edus2c_ci=. if aedu_ci==.
label variable edus2c_ci "2do ciclo de la secundaria completo"

**************
***eduui_ci***
**************

gen byte eduui_ci=0
replace eduui_ci=1 if (s402a==21 | s402a==23 | s402a==24) & (s402b>=1 & s402b<=3)
replace eduui_ci=1 if (s402a==22 | s402a==25) & (s402b>=1 & s402b<=4)
replace eduui_ci=. if aedu_ci==.
label variable eduui_ci "Universitaria incompleta"

***************
***eduuc_ci***
***************

gen byte eduuc_ci=0
replace eduuc_ci=1 if (s402a==21 | s402a==23 | s402a==24) & (s402b>=4 & s402b<=8)
replace eduuc_ci=1 if (s402a==22 | s402a==25) & (s402b>=5 & s402b<=8)
replace eduuc_ci=. if aedu_ci==.
label variable eduuc_ci "Universitaria incompleta o mas"


***************
***edupre_ci***
***************

gen byte edupre_ci=(s402a==13)
replace edupre_ci=. if aedu_ci==.
label variable edupre_ci "Educacion preescolar"


**************
***eduac_ci***
**************
gen byte eduac_ci=.
replace eduac_ci=1 if (s402a==22 | s402a==23)
replace eduac_ci=0 if (s402a==21 | (s402a>=24 & s402a<=25))
label variable eduac_ci "Superior universitario vs superior no universitario"

***************
***asiste_ci***
***************

gen asiste_ci=(s404==1)
label variable asiste_ci "Asiste actualmente a la escuela"

**************
***pqnoasis***
**************

gen pqnoasis_ci=s408a if s407==2

 
***************
***repite_ci***
***************

gen repite_ci=.
gen repiteult_ci=.


***************
***edupub_ci***
***************
/*Sobre los que se matricularon ese año*/

gen edupub_ci=(s406==2)
replace edupub_ci=. if s406==.

**************
***tecnica_ci*
**************
/*
gen tecnica_ci=.
replace tecnica_ci=1 if s402a==25 | s402a==26
recode tecnica_ci .=0 
label var tecnica_ci "1=formacion terciaria tecnica"
*/
*Modificación Mayra Sáenz-Octubre 2013
gen tecnica_ci=.
replace tecnica_ci=1 if s402a==23 | s402a==24
recode tecnica_ci .=0 
label var tecnica_ci "1=formacion terciaria tecnica"

**********************************
**** VARIABLES DE LA VIVIENDA ****
**********************************

gen aguared_ch=.
gen aguadist_ch=.
/*NA en la base, si existe la pregunta*/

gen aguamala_ch=.
/*NA*/

gen aguamide_ch=.
/*NA*/

gen luz_ch=.
/*NA en la base, si existe la pregunta*/

gen luzmide_ch=.
/*NA*/

gen combust_ch=.
gen bano_ch=.
gen banoex_ch=.
gen des1_ch=.
gen des2_ch=.
gen piso_ch=.
gen techo_ch=.
/*NA en la base, si existe la pregunta*/

gen pared_ch=0 if s1005a==2 | s1005a==3 | s1005a==6
replace pared_ch=1 if s1005a==1 | s1005a==4 | s1005a==5
replace pared_ch=2 if s1005a==7

gen resid_ch=.
/*NA*/

gen dorm_ch=.
gen cuartos_ch=.
gen cocina_ch=.
gen telef_ch=.
/*NA en la base, si existe la pregunta*/


gen cel_ch=.
gen refrig_ch=.
gen freez_ch=.
gen auto_ch=.
gen compu_ch=.
gen internet_ch=.
/*NA*/


gen vivi1_ch=1 if s1001==1
replace vivi1_ch=2 if s1001==2
replace vivi1_ch=3 if s1001>=3 & s1001<=5


gen vivi2_ch=(s1001<=2)

gen viviprop_ch=0 if s1002a==1
replace viviprop_ch=1 if s1002a==2
replace viviprop_ch=2 if s1002a==3
replace viviprop_ch=3 if s1002a>=4 & s1002a<=8

gen vivitit_ch=.
/*NA*/

gen vivialq_ch=s1003a if viviprop_ch==0
replace vivialq_ch=vivialq_ch*6.76 if s1003b==2

gen vivialqimp_ch=s1004a
replace vivialqimp_ch=vivialqimp_ch*6.76 if s1004b==2


ren ocup ocup_old

/*_____________________________________________________________________________________________________*/
* Verificación de que se encuentren todas las variables del SOCIOMETRO y las nuevas de mercado laboral
* También se incluyen variables que se manejaban en versiones anteriores, estas son:
* firmapeq_ci nrylmpri_ch nrylmpri_ci tcylmpri_ch tcylmpri_ci tipopen_ci
/*_____________________________________________________________________________________________________*/

order region_BID_c region_c pais_c anio_c mes_c zona_c factor_ch	idh_ch	idp_ci	factor_ci sexo_ci edad_ci ///
raza_ci relacion_ci civil_ci jefe_ci nconyuges_ch nhijos_ch	notropari_ch notronopari_ch	nempdom_ch ///
clasehog_ch nmiembros_ch miembros_ci nmayor21_ch nmenor21_ch nmayor65_ch nmenor6_ch	nmenor1_ch	condocup_ci ///
categoinac_ci nempleos_ci emp_ci antiguedad_ci	desemp_ci cesante_ci durades_ci	pea_ci desalent_ci subemp_ci ///
tiempoparc_ci categopri_ci categosec_ci rama_ci spublico_ci tamemp_ci cotizando_ci instcot_ci	afiliado_ci ///
formal_ci tipocontrato_ci ocupa_ci horaspri_ci horastot_ci	pensionsub_ci pension_ci tipopen_ci instpen_ci	ylmpri_ci nrylmpri_ci ///
tcylmpri_ci ylnmpri_ci ylmsec_ci ylnmsec_ci	ylmotros_ci	ylnmotros_ci ylm_ci	ylnm_ci	ynlm_ci	ynlnm_ci ylm_ch	ylnm_ch	ylmnr_ch  ///
ynlm_ch	ynlnm_ch ylmhopri_ci ylmho_ci rentaimp_ch autocons_ci autocons_ch nrylmpri_ch tcylmpri_ch remesas_ci remesas_ch	ypen_ci	ypensub_ci ///
salmm_ci lp25_ci lp4_ci	lp_ci lpe_ci aedu_ci eduno_ci edupi_ci edupc_ci	edusi_ci edusc_ci eduui_ci eduuc_ci	edus1i_ci ///
edus1c_ci edus2i_ci edus2c_ci edupre_ci eduac_ci asiste_ci pqnoasis_ci	repite_ci repiteult_ci edupub_ci tecnica_ci ///
aguared_ch aguadist_ch aguamala_ch aguamide_ch luz_ch luzmide_ch combust_ch	bano_ch banoex_ch des1_ch des2_ch piso_ch ///
pared_ch techo_ch resid_ch dorm_ch cuartos_ch cocina_ch telef_ch refrig_ch freez_ch auto_ch compu_ch internet_ch cel_ch ///
vivi1_ch vivi2_ch viviprop_ch vivitit_ch vivialq_ch	vivialqimp_ch tc_c ipc_c ppp_c lp25_2005_ci lp4_2005_ci, first

*firmapeq_ci





* Activar solo si es necesario
*keep *_ci  *_c  idh_ch 
compress


do "$ruta\harmonized\_DOCS\\Labels_Harmonized_DataBank.do"

saveold "`base_out'", replace


log close

