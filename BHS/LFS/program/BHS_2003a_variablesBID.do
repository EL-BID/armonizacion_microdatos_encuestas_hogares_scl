
* (Versi�n Stata 12)
clear
set more off
*________________________________________________________________________________________________________________*

 * Activar si es necesario (dejar desactivado para evitar sobreescribir la base y dejar la posibilidad de 
 * utilizar un loop)
 * Los datos se obtienen de las carpetas que se encuentran en el servidor: \\Sdssrv03\surveys
 * Se tiene acceso al servidor �nicamente al interior del BID.
 * El servidor contiene las bases de datos MECOVI.
 *________________________________________________________________________________________________________________*
 global ruta = "\\Sdssrv03\surveys"

local PAIS BHS
local ENCUESTA LFS
local ANO "2003"
local ronda a


local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
                                                    
capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
Pa�s: Bahamas
Encuesta: LFS
Round: a
Autores: Melany Gualavisi
�ltima versi�n: Melany Gualavisi E-mail: melanyg@iadb.org
�ltima modificaci�n: Daniela Zuluaga E-mail: danielazu@iadb.org, da.zuluaga@hotmail.com
Fecha �ltima modificaci�n: Enero 2018

							SCL/LMK - IADB
****************************************************************************/
/***************************************************************************
Detalle de procesamientos o modificaciones anteriores:

****************************************************************************/


use `base_in', clear

****************
* region_BID_c *
****************
gen region_BID_c=2
label var region_BID_c "Regiones BID"
label define region_BID_c 1 "Centroam�rica_(CID)" 2 "Caribe_(CCB)" 3 "Andinos_(CAN)" 4 "Cono_Sur_(CSC)"
label value region_BID_c region_BID_c


***************
*	region_c  * 
***************

gen region_c=.
label var region_c "division politico-administrativa, dominio estudio encuesta"


***************
***factor_ch***
***************

gen factor_ch= hweight
label variable factor_ch "Factor de expansion del hogar"


******************************
*	idh_ch
******************************
capture drop sup_dist2 enum_dist2
gen sup_dist2  = string(sup_dist,"%03.0f")
gen enum_dist2  = string(enum_dist,"%02.0f")
egen idh_ch= concat(island sup_dist2 enum_dist2 hhno)
la var idh_ch "Household ID"


******************************
*	idp_cI
******************************
egen idp_ci= concat(idh_ch Ind)
la var idp_ci "Individual ID"

**********
***zona***
**********

g zona_c=1
label variable zona_c "Zona del pais"


************
****pais****
************

gen str3 pais_c="BHS"
label variable pais_c "Pais"

**********
***anio***
**********

gen anio_c=2003
label variable anio_c "Anio de la encuesta"

*********
***mes***
*********

gen byte mes_c=.
label variable mes_c "Mes de la encuesta"

*****************
***relacion_ci***
*****************

gen relacion_ci=.
replace relacion_ci=1 if  i_q1==1
replace relacion_ci=2 if  i_q1==2
replace relacion_ci=3 if  i_q1==3
replace relacion_ci=4 if  i_q1==4
replace relacion_ci=5 if  i_q1==5

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
gen factor_ci=hweight 
label variable factor_ci "Factor de expansion del individuo"

**********
***sexo***
**********
gen sexo_ci = i_q2
label var sexo_ci "Sexo del individuo" 
label define sexo_ci 1 "Hombre" 2 "Mujer"
label value sexo_ci sexo_ci

**********
***edad***
**********
gen edad_ci= i_q3
label variable edad_ci "Edad del individuo"

*****************
***civil_ci***
*****************
* No hay categorias para viudo o divorciado
gen civil_ci=.
replace civil_ci=1 if i_q5 ==1
replace civil_ci=2 if i_q5 ==2 | i_q5 ==3
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
/*Daniela Zuluaga-Enero 2018: La encuesta solo identifica personas mayores de 15 a�os, sin embargo hay dos preguntas que permiten identificar el numero total de mujeres (nfemale)
y el numero total de hombres (nmale) en el hogar. Restandole a estas variables el numero de mujeres mayores de 15 a�os (nfemales_15) y hombres mayores de 15 a�os (nmales_15) 
respectivamente podr�amos identificar el numero total de ni�os en el hogar e incluirlos en la suma de miembros del hogar. Se estar�a asumiendo entonces que todos los ni�os
hacen parte del hogar. */

gen ninas=h_q6a -h_q6b
gen ninos=h_q5a -h_q5b

by idh_ch, sort: egen aux=sum(relacion_ci>=1 & relacion_ci<=4)
egen nmiembros_ch=rowtotal(aux ninos ninas), m
label variable nmiembros_ch "Numero de familiares en el hogar"

drop ninos ninas aux

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
gen raza_ci=.
label define raza_ci 1 "Ind�gena" 2 "Afro-descendiente" 3 "Otros"
label value raza_ci raza_ci 
label var raza_ci "Raza o etnia del individuo"
*Modificaci�n Mayra S�enz 10/20/2015: modificaciones realizadas en base a metodolog�a enviada por SCL/GDI Maria Olga Pe�a

gen raza_idioma_ci = .
gen id_ind_ci      = .
gen id_afro_ci     = .

************************************
*** VARIABLES DEL MERCADO LABORAL***
************************************
/************************************************************************************************************
* 3. Creaci�n de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/

*********
*lp_ci***
*********

gen lp_ci =.
label var lp_ci "Linea de pobreza oficial del pais"

*********
*lpe_ci***
*********

gen lpe_ci =.
label var lpe_ci "Linea de indigencia oficial del pais"

*************
**salmm_ci***
*************

gen salmm_ci= .
label var salmm_ci "Salario minimo legal"

****************
*cotizando_ci***
****************
gen cotizando_ci=.
label var cotizando_ci "Cotizante a la Seguridad Social"

****************
*afiliado_ci****
****************
gen afiliado_ci=.	
label var afiliado_ci "Afiliado a la Seguridad Social"

****************
*tipopen_ci*****
****************
gen tipopen_ci=.

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
* Encuesta no incluye a menores y se usa la clasificaci�n del pa�s con variable creada.
gen condocup_ci=.
replace condocup_ci=1 if employ==1
replace condocup_ci=2 if employ==2
replace condocup_ci=3 if employ==3
recode condocup_ci .=4 if edad_ci<15
label var condocup_ci "Condicion de ocupaci�n de acuerdo a def de cada pais"
label define condocup_ci 1 "Ocupado" 2 "Desocupado" 3 "Inactivo" 4 "Menor que 10" 
label value condocup_ci condocup_ci

*************
*cesante_ci* 
*************

gen cesante_ci=1 if i_q20==1 & condocup_ci==2
replace cesante_ci=0 if i_q20==2 & condocup_ci==2
label var cesante_ci "Desocupado - definicion oficial del pais"	

*************
*tamemp_ci
*************
gen tamemp_ci=.
label var tamemp_ci "# empleados en la empresa segun rangos"
label define tamemp_ci 1 "Peque�a" 2 "Mediana" 3 "Grande"
label value tamemp_ci tamemp_ci

*************
**pension_ci*
*************
gen pension_ci=.
label var pension_ci "1=Recibe pension contributiva"

*************
**ypen_ci*
*************

gen ypen_ci=. 
label var ypen_ci "Valor de la pension contributiva"

***************
*pensionsub_ci*
***************

gen pensionsub_ci =.
label var pensionsub_ci "1=recibe pension subsidiada / no contributiva"

*****************
**ypensub_ci*
*****************

gen  ypensub_ci=.
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
label var desemp_ci "Desempleado que busc� empleo en el periodo de referencia"
  
*************
***pea_ci***
*************
gen pea_ci=0
replace pea_ci=1 if emp_ci==1 |desemp_ci==1
label var pea_ci "Poblaci�n Econ�micamente Activa"

*****************
***desalent_ci***
*****************
gen desalent_ci=.
label var desalent_ci "Trabajadores desalentados"

*****************
***horaspri_ci***
*****************
gen horaspri_ci=.
label var horaspri_ci "Horas trabajadas semanalmente en el trabajo principal"


*****************
***horastot_ci***
*****************

gen horastot_ci= .


***************
***subemp_ci***
***************

gen subemp_ci=.
label var subemp_ci "Personas en subempleo por horas"

*******************
***tiempoparc_ci***
*******************
gen tiempoparc_ci=.
label var tiempoparc_c "Personas que trabajan medio tiempo" 

******************
***categopri_ci***
******************
gen categopri_ci=.
replace categopri_ci=1 if i_q32==6
replace categopri_ci=2 if i_q32==4 | i_q32==5
replace categopri_ci=3 if i_q32>=1 & i_q32<=3
replace categopri_ci=4 if i_q32==7
replace categopri_ci=. if emp_ci~=1
label define categopri_ci 1"Patron" 2"Cuenta propia" 
label define categopri_ci 3"Empleado" 4" No remunerado", add
label value categopri_ci categopri_ci
label variable categopri_ci "Categoria ocupacional trabajo principal"


******************
***categosec_ci***
******************
gen categosec_ci=.
label define categosec_ci 1"Patron" 2"Cuenta propia" 
label define categosec_ci 3"Empleado" 4 "No remunerado" , add
label value categosec_ci categosec_ci
label variable categosec_ci "Categoria ocupacional trabajo secundario"

*****************
*tipocontrato_ci*
*****************

gen tipocontrato_ci=.
label var tipocontrato_ci "Tipo de contrato segun su duracion"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci

*****************
***nempleos_ci***
*****************
* La categor�a dos indica 2 o mas empleos
gen nempleos_ci=.
replace nempleos_ci=1 if i_q12==2
replace nempleos_ci=2 if i_q12==3
label var nempleos_ci "N�mero de empleos" 


*****************
***spublico_ci***
*****************
gen spublico_ci=1 if ((i_q32==1 | i_q32==2) & condocup_ci==1)
replace spublico_ci=0 if spublico_ci==. & condocup_ci==1
label var spublico_ci "Personas que trabajan en el sector p�blico"

**************
***ocupa_ci***
**************
gen ocupa_ci=.
replace ocupa_ci=1 if ((i_q31>=2111 & i_q31<=3522))& emp_ci==1
replace ocupa_ci=2 if ((i_q31>=1110 & i_q31<=1439)) & emp_ci==1
replace ocupa_ci=3 if ((i_q31>=4110 & i_q31<=4419))& emp_ci==1
replace ocupa_ci=4 if ((i_q31>=5200 & i_q31<=5249) | (i_q31>=9110 & i_q31<=9113)) & emp_ci==1
replace ocupa_ci=5 if ((i_q31>=5110 & i_q31<=5169) | (i_q31>=5310 & i_q31<=5419) | (i_q31>=9120 & i_q31<=9162) | (i_q31>=9500 & i_q31<=9711)) & emp_ci==1
replace ocupa_ci=6 if ((i_q31>=6110 & i_q31<=6349) | (i_q31>=9211 & i_q31<=9214)) & emp_ci==1
replace ocupa_ci=7 if ((i_q31>=7111 & i_q31<=8350) | (i_q31>=9310 & i_q31<=9414))& emp_ci==1

label define ocupa_ci  1 "profesional y tecnico" 2"director o funcionario sup" 3"administrativo y nivel intermedio"
label define ocupa_ci  4 "comerciantes y vendedores" 5 "en servicios" 6 "trabajadores agricolas", add
label define ocupa_ci  7 "obreros no agricolas, conductores de maq y ss de transporte", add
label define ocupa_ci  8 "FFAA" 9 "Otras ", add
label value ocupa_ci ocupa_ci
label variable ocupa_ci "Ocupacion laboral"

*************
***rama_ci***
*************
* CIIU rev 4.

gen rama_ci=.
replace rama_ci=1 if (i_q30>=111 & i_q30<=399) & emp_ci==1
replace rama_ci=2 if i_q30>=500 & i_q30<=999 & emp_ci==1
replace rama_ci=3 if i_q30>=1000 & i_q30<=3499 & emp_ci==1
replace rama_ci=4 if i_q30>=3500 & i_q30<=3999 & emp_ci==1
replace rama_ci=5 if i_q30>=4010 & i_q30<=4399 & emp_ci==1
replace rama_ci=6 if ((i_q30>=4500 & i_q30<=4799) | (i_q30>=5500 & i_q30<=5699)) & emp_ci==1
replace rama_ci=7 if i_q30>=4900 & i_q30<=5399 & emp_ci==1
replace rama_ci=8 if (i_q30>=6400 & i_q30<=6899) & emp_ci==1
replace rama_ci=9 if ((i_q30>=5800 & i_q30<=6399) | (i_q30>=6900 & i_q30<=9999)) & emp_ci==1
label var rama_ci "Rama de actividad"
label def rama_ci 1"Agricultura, caza, silvicultura y pesca" 2"Explotaci�n de minas y canteras" 3"Industrias manufactureras"
label def rama_ci 4"Electricidad, gas y agua" 5"Construcci�n" 6"Comercio, restaurantes y hoteles" 7"Transporte y almacenamiento", add
label def rama_ci 8"Establecimientos financieros, seguros e inmuebles" 9"Servicios sociales y comunales", add
label val rama_ci rama_ci

****************
***durades_ci***
****************
* Variable categorica
gen durades_ci=.
replace durades= 0.5  if i_q25  ==1
replace durades= 2    if i_q25  ==2
replace durades= 4.5  if i_q25  ==3
replace durades= 9    if i_q25  ==4
replace durades= 12   if i_q25  ==5
label variable durades_ci "Duracion del desempleo en meses"

*******************
***antiguedad_ci***
*******************
gen antiguedad_ci=.	
label var antiguedad_ci "Antiguedad en la actividad actual en anios"

*******************
***categoinac_ci***
*******************
gen categoinac_ci =1 if (i_q15 ==6 & condocup_ci==3)
replace categoinac_ci = 2 if  (i_q15 ==5 & condocup_ci==3)
replace categoinac_ci = 3 if  (i_q15 ==4 & condocup_ci==3)
replace categoinac_ci = 4 if  (i_q15 ==7 | i_q15 ==3) & condocup_ci==3
label var categoinac_ci "Categor�a de inactividad"
label define categoinac_ci 1 "jubilados o pensionados" 2 "Estudiantes" 3 "Quehaceres dom�sticos" 4 "Otros"

*******************
***formal***
*******************
gen byte formal_ci=.
label var formal_ci "1=afiliado o cotizante / PEA"


**************
***INGRESOS***
**************

***************
***ylmpri_ci***
***************
recode i_q33a (999999=.) 
g ylmpri_ci=i_q33a/12
replace ylmpri_ci=. if emp_ci~=1
label var ylmpri_ci "Ingreso laboral monetario actividad principal" 

*******************
*** nrylmpri_ci ***
*******************

gen nrylmpri_ci=(ylmpri_ci==. & emp_ci==1)
label var nrylmpri_ci "Id no respuesta ingreso de la actividad principal"  


******************
*** ylnmpri_ci ***
******************

gen ylnmpri_ci=.
label var ylnmpri_ci "Ingreso laboral NO monetario actividad principal"   


***************
***ylmsec_ci***
***************
g ylmsec_ci=i_q33b/12
replace ylmsec_ci=. if emp_ci~=1
label var ylmsec_ci "Ingreso laboral monetario segunda actividad" 


******************
****ylnmsec_ci****
******************

gen ylnmsec_ci=.
label var ylnmsec_ci "Ingreso laboral NO monetario actividad secundaria"

**********************************************************************************************
***TCYLMPRI_CH : Identificador de los hogares en donde alguno de los miembros reporta como
*** top-code el ingreso de la actividad principal. .
***********************************************************************************************
gen tcylmpri_ch = .
label var tcylmpri_ch "Id hogar donde alg�n miembro reporta como top-code el ingr de activ. principal"

***********************************************************************************************
***TCYLMPRI_CI : Identificador de top-code del ingreso de la actividad principal.
***********************************************************************************************
gen tcylmpri_ci = .
label var tcylmpri_ci "Identificador de top-code del ingreso de la actividad principal"

*****************
***ylmotros_ci***
*****************
gen ylmotros_ci=     i_q33c/12
replace ylmotros_ci=. if emp_ci~=1
label var ylmotros_ci "Ingreso laboral monetario de otros trabajos" 

******************
***ylnmotros_ci***
******************

gen ylnmotros_ci= .
label var ylnmotros_ci "Ingreso laboral NO monetario de otros trabajos" 


************
***ylm_ci***
************

egen ylm_ci=rsum(ylmpri_ci ylmsec_ci ylmotros_ci), missing
replace ylm_ci=. if ylmpri_ci==. & ylmsec_ci==. & ylmotros_ci==.
label var ylm_ci "Ingreso laboral monetario total"  


*************
***ylnm_ci***
*************

gen ylnm_ci=.
label var ylnm_ci "Ingreso laboral NO monetario total"  


 
*************
***ynlm_ci***
*************

gen ynlm_ci=. 
label var ynlm_ci "Ingreso no laboral monetario"  


**************
***ynlnm_ci***
**************

gen ynlnm_ci=.
label var ynlnm_ci "Ingreso no laboral no monetario" 


*****************
***remesas_ci***
*****************

gen remesas_ci=.
label var remesas_ci "Remesas mensuales reportadas por el individuo" 




************************
*** HOUSEHOLD INCOME ***
************************

*******************
*** nrylmpri_ch ***
*******************
by idh_ch, sort: egen nrylmpri_ch=sum(nrylmpri_ci) if miembros_ci==1, missing
replace nrylmpri_ch=1 if nrylmpri_ch>0 & nrylmpri_ch<.
replace nrylmpri_ch=. if nrylmpri_ch==.
label var nrylmpri_ch "Hogares con alg�n miembro que no respondi� por ingresos"

**************
*** ylm_ch ***
**************
by idh_ch, sort: egen ylm_ch=sum(ylm_ci) if miembros_ci==1, missing
label var ylm_ch "Ingreso laboral monetario del hogar"

****************
*** ylmnr_ch ***
****************
by idh_ch, sort: egen ylmnr_ch=sum(ylm_ci) if miembros_ci==1, missing
replace ylmnr_ch=. if nrylmpri_ch==1
label var ylmnr_ch "Ingreso laboral monetario del hogar"

***************
*** ylnm_ch ***
***************
by idh_ch, sort: egen ylnm_ch=sum(ylnm_ci) if miembros_ci==1, missing
label var ylnm_ch "Ingreso laboral no monetario del hogar"

*******************
*** remesas_ch ***
*******************
by idh_ch, sort: egen remesas_ch=sum(remesas_ci) if miembros_ci==1, missing
label var remesas_ch "Remesas mensuales del hogar" 

***************
*** ynlm_ch ***
***************
by idh_ch, sort: egen ynlm_ch=sum(ynlm_ci) if miembros_ci==1, missing
label var ynlm_ch "Ingreso no laboral monetario del hogar"

****************
*** ynlnm_ch ***
****************
gen ynlnm_ch=.
label var ynlnm_ch "Ingreso no laboral no monetario del hogar"

*******************
*** autocons_ci ***
*******************
gen autocons_ci=.
label var autocons_ci "Autoconsumo reportado por el individuo"

*******************
*** autocons_ch ***
*******************
gen autocons_ch=.
label var autocons_ch "Autoconsumo reportado por el hogar"

*******************
*** rentaimp_ch ***
*******************
gen rentaimp_ch= .
label var rentaimp_ch "Rentas imputadas del hogar"

*****************
***ylhopri_ci ***
*****************
gen ylmhopri_ci=.
label var ylmhopri_ci "Salario monetario de la actividad principal" 

***************
***ylmho_ci ***
***************
gen ylmho_ci=.
label var ylmho_ci "Salario monetario de todas las actividades" 

**************************INGRESOS-TRANSFERENCIAS**************************************

* Daniela Zuluaga-Enero 2018: Se genera una nueva clasificacion para el ingreso no laboral monetario y no monetario*

***************
***trapri_ci***
***************
gen trapri_ci=.
label var trapri_ci "Ingreso por transferencias privadas" 

***************
***trapri_ch***
***************
bys idh_ch: egen trapri_ch=sum(trapri_ci) if miembros_ci==1, missing
label var trapri_ch "Ingreso del hogar por transferencias privadas" 

***************
***progpub_ci***
***************
gen progpub_ci= .
label var progpub_ci "Ingreso por programas sociales de transferencias condicionadas" 

***************
***progpub_ch***
***************
bys idh_ch: egen progpub_ch=sum(progpub_ci) if miembros_ci==1, missing
label var progpub_ch "Ingreso del hogar por programas sociales de transferencias condicionadas" 

***************
***trapub_ci***
***************
gen trapub_ci= .
label var trapub_ci "Ingreso por transferencias publicas" 

***************
***trapub_ch***
***************
bys idh_ch: egen trapub_ch=sum(trapub_ci) if miembros_ci==1, missing
label var trapub_ch "Ingreso del hogar por transferencias publicas" 

***************
***capital_ci***
***************
gen capital_ci= .
label var capital_ci "Ingreso por renta del capital" 

***************
***capital_ch***
***************
bys idh_ch: egen capital_ch=sum(capital_ci) if miembros_ci==1, missing
label var capital_ch "Ingreso del hogar por renta del capital" 

***************
***otros_ci***
***************
gen otros_ci= .
label var otros_ci "Otros Ingresos" 

***************
***otros_ch***
***************
bys idh_ch: egen otros_ch=sum(otros_ci) if miembros_ci==1, missing
label var otros_ch "Otros Ingresos del hogar" 

***************
***ypen_ch***
***************
bys idh_ch: egen ypen_ch=sum(ypen_ci) if miembros_ci==1, missing
label var ypen_ch "Ingresos del hogar por jubilaciones y pensiones contributivas" 


***************
***ytotal_ci***
***************
egen ytotal_ci= rowtotal (ylm_ci ylnm_ci trapri_ci trapub_ci capital_ci otros_ci ypen_ci), missing
label var ytotal_ci "Ingreso total individual" 

***************
***ytotal_ch***
***************
egen ytotal_ch=rowtotal(ylm_ch  ylnm_ch  trapri_ch  trapub_ch  capital_ch  otros_ch  ypen_ch) if miembros_ci==1, missing
label var ytotal_ch "Ingreso total del hogar"

***************
***ytotalpc_ch***
***************
gen ytotalpc_ch=(ytotal_ch/nmiembros_ch) if miembros_ci==1
label var ytotalpc_ch "Ingreso per capita del hogar"


***************
***quintil_ci***
***************
xtile quintil_ci=ytotalpc_ch if ytotalpc_ch>0 & ytotalpc_ch!=. [pw=(factor_ch)], nq(5)
label var quintil_ci "Quintil de ingreso"
label define quintil_ci 1 "Quintil 1" 2 "Quintil 2" 3 "Quintil 3" 4 "Quintil 4" 5 "Quintil 5"
label values quintil_ci quintil_ci

****************************
***VARIABLES DE EDUCACION***
****************************

/*NO es posible saber cuantos a�os de educaci�n tienen los individuos, ni tampoco definir cual es 
el nivel mas alto alcanzado completo o incompleto*/


gen aedu_ci = .

**************
***eduno_ci***
**************

gen byte eduno_ci= .
label variable eduno_ci "Cero anios de educacion"

**************
***edupi_ci***
**************

gen byte edupi_ci= .
label variable edupi_ci "Primaria incompleta"

**************
***edupc_ci***
**************

gen byte edupc_ci=.
label variable edupc_ci "Primaria completa"

**************
***edusi_ci***
**************

gen byte edusi_ci=.
label variable edusi_ci "Secundaria incompleta"

**************
***edusc_ci***
**************

gen byte edusc_ci=.
label variable edusc_ci "Secundaria completa"

***************
***edus1i_ci***
***************

gen byte edus1i_ci=.
label variable edus1i_ci "1er ciclo de la secundaria incompleto"

***************
***edus1c_ci***
***************

gen byte edus1c_ci=.
label variable edus1c_ci "1er ciclo de la secundaria completo"

***************
***edus2i_ci***
***************

gen byte edus2i_ci=.
label variable edus2i_ci "2do ciclo de la secundaria incompleto"

***************
***edus2c_ci***
***************

gen byte edus2c_ci=.
label variable edus2c_ci "2do ciclo de la secundaria completo"

**************
***eduui_ci***
**************
gen byte eduui_ci=.
label variable eduui_ci "Universitaria incompleta"

***************
***eduuc_ci***
***************
gen byte eduuc_ci=.
label variable eduuc_ci "Universitaria completa"

***************
***edupre_ci***
***************

gen byte edupre_ci=.
label variable edupre_ci "Educacion preescolar"


**************
***eduac_ci***
**************

gen byte eduac_ci=.
label variable eduac_ci "Superior universitario vs superior no universitario"

***************
***asiste_ci***
***************

gen asiste_ci=.
label variable asiste_ci "Asiste actualmente a la escuela"

*****************
***pqnoasis_ci***
*****************

gen pqnoasis_ci=.
label var pqnoasis_ci "Razones para no asistir a la escuela"
label def pqnoasis_ci 1"vacaci�n/receso" 2"falta de dinero" 3"por trabajo" 4"por enfermedad/accidente/discapacidad"
label def pqnoasis_ci 5"los establecimientos son distantes" 6"culmin� sus estudios" 7"edad temprana/ edad avanzada", add
label def pqnoasis_ci 8"falta de inter�s" 9"labores de casa/ embarazo/cuidado de ni�os/as" 10"otra", add
label val pqnoasis_ci pqnoasis 

**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci**

******************
***pqnoasis1_ci***
******************

gen pqnoasis1_ci=.


***************
***repite_ci***
***************

gen repite_ci=.
label var repite_ci "Ha repetido al menos un grado"


******************
***repiteult_ci***
******************

gen repiteult_ci=.
label var repiteult "Ha repetido el �ltimo grado"


***************
***edupub_ci***
***************

gen edupub_ci=.
label var edupub_ci "Asiste a un centro de ensenanza p�blico"

**************
***tecnica_ci*
**************

gen tecnica_ci=.
label var tecnica_ci "1=formacion terciaria tecnica"


**********************************
**** VARIABLES DE LA VIVIENDA ****
**********************************

* Daniela Zuluaga-Enero 2018: Se generan variables de vivienda que no hab�an sido creadas pero cuya informaci�n est� disponible en la base.

****************
***aguared_ch***
****************

gen aguared_ch=.
replace aguared_ch=1 if h_q4==1
replace aguared_ch=0 if (h_q4>=2 & h_q4<=4)
label var aguared_ch "Acceso a fuente de agua por red"


****************
***aguadist_ch***
****************


gen aguadist_ch=.
label var aguadist_ch "Ubicaci�n de la principal fuente de agua"
label def aguadist_ch 1"Dentro de la vivienda" 2"Fuera de la vivienda pero en el terreno"
label def aguadist_ch 3"Fuera de la vivienda y del terreno", add
label val aguadist_ch aguadist_ch


*****************
***aguamala_ch***
*****************

gen aguamala_ch=.
replace aguamala_ch=1 if h_q4==4
replace aguamala_ch=0 if (h_q4>=1 & h_q4<=3)
label var aguamala_ch "Agua unimproved seg�n MDG" 


*****************
***aguamide_ch***
*****************

gen aguamide_ch=.
label var aguamide_ch "Usan medidor para pagar consumo de agua"


************
***luz_ch***
************

gen luz_ch= .
replace luz_ch=1 if h_q3==1
replace luz_ch=0 if (h_q3==2 | h_q3==3 | h_q3==4)
label var luz_ch  "La principal fuente de iluminaci�n es electricidad"


****************
***luzmide_ch***
****************

gen luzmide_ch=.
label var luzmide_ch "Usan medidor para pagar consumo de electricidad"


****************
***combust_ch***
****************

gen combust_ch= .
label var combust_ch "Principal combustible gas o electricidad" 


*************
***bano_ch***
*************

gen bano_ch= .
label var bano_ch "El hogar tiene servicio sanitario"


***************
***banoex_ch***
***************

gen banoex_ch=.
label var banoex_ch "El servicio sanitario es exclusivo del hogar"


*************
***des1_ch***
*************

gen des1_ch=.
label var des1_ch "Tipo de desague seg�n unimproved de MDG"
label def des1_ch 0"No tiene servicio sanitario" 1"Conectado a red general o c�mara s�ptica"
label def des1_ch 2"Letrina o conectado a pozo ciego" 3"Desemboca en r�o o calle", add
label val des1_ch des1_ch


*************
***des2_ch***
*************

gen des2_ch=.
label var des2_ch "Tipo de desague sin incluir definici�n MDG"
label def des2_ch 0"No tiene servicio sanitario" 1"Conectado a red general, c�mara s�ptica, pozo o letrina"
label def des2_ch 2"Cualquier otro caso", add
label val des2_ch des2_ch


*************
***piso_ch***
*************

gen piso_ch=.
label var piso_ch "Materiales de construcci�n del piso"  
label def piso_ch 0"Piso de tierra" 1"Materiales permanentes" 2 "Otros materiales"
label val piso_ch piso_ch


**************
***pared_ch***
**************

gen pared_ch=.
label var pared_ch "Materiales de construcci�n de las paredes"
label def pared_ch 0"No permanentes" 1"Permanentes" 2 "Otros materiales"
label val pared_ch pared_ch


**************
***techo_ch***
**************

gen techo_ch=.
label var techo_ch "Materiales de construcci�n del techo"
label def techo_ch 0"No permanentes" 1"Permanentes" 2 "Otros materiales"
label val techo_ch techo_ch


**************
***resid_ch***
**************


gen resid_ch =.
label var resid_ch "M�todo de eliminaci�n de residuos"
label def resid_ch 0"Recolecci�n p�blica o privada" 1"Quemados o enterrados"
label def resid_ch 2"Tirados a un espacio abierto" 3"Otros", add
label val resid_ch resid_ch


*************
***dorm_ch***
*************

gen dorm_ch=.
label var dorm_ch "Habitaciones para dormir"


****************
***cuartos_ch***
****************

gen cuartos_ch=.
label var cuartos_ch "Habitaciones en el hogar"
 

***************
***cocina_ch***
***************

gen cocina_ch=.
label var cocina_ch "Cuarto separado y exclusivo para cocinar"


**************
***telef_ch***
**************

gen telef_ch=.
label var telef_ch "El hogar tiene servicio telef�nico fijo"


**************
***compu_ch***
**************

gen compu_ch=.

********
***NA***
********
gen refrig_ch=.
label var refrig_ch "El hogar posee refrigerador o heladera"

gen freez_ch=.
label var freez_ch "El hogar posee congelador"

gen auto_ch=.
label var auto_ch "El hogar posee automovil particular"



*****************
***internet_ch***
*****************
gen internet_ch=.
label var internet_ch "El hogar posee conexi�n a Internet"



************
***cel_ch***
************

gen cel_ch= .
label var cel_ch "El hogar tiene servicio telefonico celular"


**************
***vivi1_ch***
**************

gen vivi1_ch=.
replace vivi1_ch=1 if (h_q1==1 | h_q1==2)
replace vivi1_ch=2 if h_q1==3
replace vivi1_ch=3 if (h_q1==4 | h_q1==5)
label var vivi1_ch "Tipo de vivienda en la que reside el hogar"
label def vivi1_ch 1"Casa" 2"Departamento" 3"Otros"
label val vivi1_ch vivi1_ch


*************
***vivi2_ch***
*************

gen vivi2_ch= (vivi1_ch==1 | vivi1_ch==2)
replace vivi2_ch=. if vivi1_ch==.
label var vivi2_ch "La vivienda es casa o departamento"


*****************
***viviprop_ch***
*****************
gen viviprop_ch=.
replace viviprop_ch=0 if h_q2==2
replace viviprop_ch=1 if h_q2==1
replace viviprop_ch=3 if h_q2==3

label var viviprop_ch "Propiedad de la vivienda"
label def viviprop_ch 0"Alquilada" 1"Propia" 
label def viviprop_ch 3"Ocupada (propia de facto)", add
label val viviprop_ch viviprop_ch

****************
***vivitit_ch***
****************

gen vivitit_ch=.
label var vivitit_ch "El hogar posee un t�tulo de propiedad"


****************
***vivialq_ch***
****************

gen vivialq_ch= .
label var vivialq_ch "Alquiler mensual"


*******************
***vivialqimp_ch***
*******************

gen vivialqimp_ch=.
label var vivialqimp_ch "Alquiler mensual imputado"

**Daniela Zuluaga- Enero 2018: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**

*********************
***aguamejorada_ch***
*********************

gen aguamejorada_ch=.
replace aguamejorada_ch=1 if h_q4>=1 & h_q4<=3
replace aguamejorada_ch=0 if h_q4==4
	
*********************
***banomejorado_ch***
*********************
gen banomejorado_ch=.

*******************
*** benefdes_ci ***
*******************
g benefdes_ci=.
label var benefdes_ci "=1 si tiene seguro de desempleo"

*******************
*** ybenefdes_ci***
*******************
g ybenefdes_ci=.
label var ybenefdes_ci "Monto de seguro de desempleo"



/*_____________________________________________________________________________________________________*/
* Asignaci�n de etiquetas e inserci�n de variables externas: tipo de cambio, Indice de Precios al 
* Consumidor (2011=100), Paridad de Poder Adquisitivo (PPA 2011),  l�neas de pobreza
/*_____________________________________________________________________________________________________*/


do "$ruta\harmonized\_DOCS\\Labels&ExternalVars_Harmonized_DataBank.do"

/*_____________________________________________________________________________________________________*/
* Verificaci�n de que se encuentren todas las variables armonizadas 
/*_____________________________________________________________________________________________________*/

order region_BID_c region_c pais_c anio_c mes_c zona_c factor_ch	idh_ch	idp_ci	factor_ci sexo_ci edad_ci ///
raza_idioma_ci  id_ind_ci id_afro_ci raza_ci  relacion_ci civil_ci jefe_ci nconyuges_ch nhijos_ch notropari_ch notronopari_ch nempdom_ch ///
clasehog_ch nmiembros_ch miembros_ci nmayor21_ch nmenor21_ch nmayor65_ch nmenor6_ch	nmenor1_ch	condocup_ci ///
categoinac_ci nempleos_ci emp_ci antiguedad_ci	desemp_ci cesante_ci durades_ci	pea_ci desalent_ci subemp_ci ///
tiempoparc_ci categopri_ci categosec_ci rama_ci spublico_ci tamemp_ci cotizando_ci instcot_ci	afiliado_ci ///
formal_ci tipocontrato_ci ocupa_ci horaspri_ci horastot_ci	pensionsub_ci pension_ci tipopen_ci instpen_ci	ylmpri_ci nrylmpri_ci ///
tcylmpri_ci ylnmpri_ci ylmsec_ci ylnmsec_ci	ylmotros_ci	ylnmotros_ci ylm_ci	ylnm_ci	ynlm_ci	ynlnm_ci ylm_ch	ylnm_ch	ylmnr_ch  ///
ynlm_ch	ynlnm_ch ylmhopri_ci ylmho_ci rentaimp_ch autocons_ci autocons_ch nrylmpri_ch tcylmpri_ch remesas_ci remesas_ch	ypen_ci	ypensub_ci ///
trapri_ci trapri_ch progpub_ci progpub_ch trapub_ci  trapub_ch capital_ci capital_ch otros_ci otros_ch ypen_ch ytotal_ci  ytotal_ch ytotalpc_ch quintil_ci ///
salmm_ci tc_c ipc_c lp19_c lp31_c lp5_c lp_ci lpe_ci aedu_ci eduno_ci edupi_ci edupc_ci	edusi_ci edusc_ci eduui_ci eduuc_ci	edus1i_ci ///
edus1c_ci edus2i_ci edus2c_ci edupre_ci eduac_ci asiste_ci pqnoasis_ci pqnoasis1_ci	repite_ci repiteult_ci edupub_ci tecnica_ci ///
aguared_ch aguadist_ch aguamala_ch aguamide_ch luz_ch luzmide_ch combust_ch	bano_ch banoex_ch des1_ch des2_ch piso_ch aguamejorada_ch banomejorado_ch ///
pared_ch techo_ch resid_ch dorm_ch cuartos_ch cocina_ch telef_ch refrig_ch freez_ch auto_ch compu_ch internet_ch cel_ch ///
vivi1_ch vivi2_ch viviprop_ch vivitit_ch vivialq_ch	vivialqimp_ch , first



compress


saveold "`base_out'", replace


log close
