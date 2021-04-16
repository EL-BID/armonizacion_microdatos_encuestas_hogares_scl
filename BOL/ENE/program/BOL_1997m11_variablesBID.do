
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
 *global ruta = "${surveysFolder}"

local PAIS BOL
local ENCUESTA ENE
local ANO "1997"
local ronda m11 


local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\\`PAIS'_`ANO'`ronda'.dta"
*local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
                                                    
capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Bolivia
Encuesta: ENE
Round: m11
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

gen factor_ch=factorex
label variable factor_ch "Factor de expansion del hogar"


**************
****idh_ch****
**************
gen idh_ch=id_hogar
label variable idh_ch "ID del hogar"

*************
****idp_ci***
*************

gen idp_ci=nrolinea
label variable idp_ci "ID de la persona en el hogar"

**********
***zona***
**********
gen byte zona_c=1 
replace zona_c=0 if contexto==14 | contexto==24 | contexto==34 | contexto==44
replace zona_c=0 if contexto==54 | contexto==64 | contexto==74 | contexto==94

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

gen anio_c=1997
label variable anio_c "Anio de la encuesta"

*********
***mes***
*********

gen mes_c=11
label variable mes_c "Mes de la encuesta"

*****************
***relacion_ci***
*****************

gen relacion_ci=.
replace relacion_ci=1 if parentco==1
replace relacion_ci=2 if parentco==2
replace relacion_ci=3 if parentco==3
replace relacion_ci=4 if parentco>=4 & parentco<=8
replace relacion_ci=5 if parentco==10 
replace relacion_ci=6 if parentco==9

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
label variable edad_ci "Edad del individuo"


*****************
***civil_ci***
*****************

gen civil_ci=.
replace civil_ci=1 if estcivil==4
replace civil_ci=2 if estcivil==1 
replace civil_ci=3 if estcivil==2
replace civil_ci=4 if estcivil==3

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

*************************
*** VARIABLES DE RAZA ***
*************************

* MGR Oct. 2015: modificaciones realizadas en base a metodología enviada por SCL/GDI Maria Olga Peña

/*
gen raza_ci=.
replace raza_ci= 1 if  idioma ==2 |  idioma ==4 |  idioma ==8 |  idioma ==16 
replace raza_ci= 3 if  idioma ==0 | idioma ==1 |idioma ==32
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

gen raza_idioma_ci=.
replace raza_idioma_ci= 1 if  (idioma >=2 & idioma<=17) | (idioma>=20 & idioma<=23) | idioma==35 |idioma==37
replace raza_idioma_ci= 3 if  idioma ==0 | idioma ==1 |idioma ==32 | idioma==33
bys idh_ch, sort: gen aux=raza_idioma_ci if parentco==1
bys idh_ch,sort: egen aux1 = max(aux)
replace raza_idioma_ci=aux1 if (raza_idioma_ci ==. & (parentco==5 | parentco==3))  
replace raza_idioma_ci=3 if raza_idioma_ci==. 
drop aux aux1
label define raza_idioma_ci 1 "Indígena" 2 "Afro-descendiente" 3 "Otros" 
label value raza_idioma_ci raza_idioma_ci 
label value raza_idioma_ci raza_idioma_ci
label var raza_idioma_ci "Raza o etnia del individuo" 

gen id_ind_ci = 0
replace id_ind_ci=1 if raza_idioma_ci==1
label define id_ind_ci 1 "Indígena" 0 "Otros" 
label value id_ind_ci id_ind_ci 
label var id_ind_ci "Indigena" 

gen id_afro_ci = 0
label define id_afro_ci 1 "Afro-descendiente" 0 "Otros" 
label value id_afro_ci id_afro_ci 
label var id_afro_ci "Afro-descendiente" 

gen raza_ci=.

************************************
*** VARIABLES DEL MERCADO LABORAL***
************************************
/* Esta sección es para los residentes habituales del hogar mayores a 7 años*/ 
	
/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/


*********
*lp_ci***
*********
gen lp_ci =.
replace lp_ci= 325.039520466411 if contexto==11 |  contexto==13
replace lp_ci= 319.010791366906 if contexto==21 |  contexto==23
replace lp_ci= 340.237623762376 if contexto==31 |  contexto==33
replace lp_ci= 290.088783661692 if contexto==41 |  contexto==43
replace lp_ci= 266.801209354462 if contexto==51 |  contexto==53
replace lp_ci= 345.791315819016 if contexto==61 |  contexto==63
replace lp_ci= 338.188976377953 if contexto==71 |  contexto==73
replace lp_ci= 338.188976377953 if contexto==81 |  contexto==83
replace lp_ci= 261.153212520593 if contexto==22
replace lp_ci= 338.188976377953 if contexto==91


replace lp_ci=    230.00     if contexto==14 |  contexto==24 |  contexto==34 |  contexto==44 |  contexto==54 |  contexto==64 |  contexto==74 |  contexto==94


label var lp_ci "Linea de pobreza oficial del pais"

*********
*lpe_ci***
*********

gen lpe_ci =.
replace lpe_ci= 164.144957835538 if contexto==11 |  contexto==13
replace lpe_ci= 177.37 if contexto==21 |  contexto==23
replace lpe_ci= 171.82 if contexto==31 |  contexto==33
replace lpe_ci= 161.289363715901 if contexto==41 |  contexto==43
replace lpe_ci= 148.341472401081 if contexto==51 |  contexto==53
replace lpe_ci= 174.624614488603 if contexto==61 |  contexto==63
replace lpe_ci= 171.8 if contexto==71 |  contexto==73
replace lpe_ci= 171.8 if contexto==81 |  contexto==83
replace lpe_ci= 158.52 if contexto==22
replace lpe_ci= 171.8 if contexto==91


replace lpe_ci=     130.7     if contexto==14 |  contexto==24 |  contexto==34 |  contexto==44 |  contexto==54 |  contexto==64 |  contexto==74 |  contexto==94

label var lpe_ci "Linea de indigencia oficial del pais"

*************
**salmm_ci***
*************

*BOL 1997
gen salmm_ci=240.00 
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
gen afiliado_ci= .
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

*****************
*tipocontrato_ci*
*****************

gen tipocontrato_ci=.
label var tipocontrato_ci "Tipo de contrato segun su duracion"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci

****************
****condocup_ci*
****************
/*
gen condocup_ci=.
replace condocup_ci=1 if condact==1
replace condocup_ci=2 if condact==2 | condact==3
replace condocup_ci=3 if (condact>3 & condact<=8) & edad_ci>=10
replace condocup_ci=4 if edad_ci<10
label var condocup_ci "Condicion de ocupación de acuerdo a def de cada pais"
label define condocup_ci 1 "Ocupado" 2 "Desocupado" 3 "Inactivo" 4 "Menor que 10" 
label value condocup_ci condocup_ci
*/

* Homologacion toda la serie 05/22/2014 MGD

gen condocup_ci=.
replace condocup_ci=1 if trabajo==1 | sondeo1>=1  | sondeo2==1
replace condocup_ci=2 if (trabajo==2 | sondeo1==0 | sondeo2==2) & (bustrab==1 | bus4sem==1)
recode condocup_ci .=3 if edad_ci>=7  /* MLO mod 12/2015 cambie edad a 7+*/
recode condocup_ci .=4 if edad_ci<7
label var condocup_ci "Condicion de ocupación de acuerdo a def de cada pais"
label define condocup_ci 1 "Ocupado" 2 "Desocupado" 3 "Inactivo" 4 "Menor que 10" 
label value condocup_ci condocup_ci

*************
*cesante_ci* 
*************

gen cesante_ci=1 if  cesante==1 & condocup_ci==2
replace cesante_ci=0 if  cesante==2 & condocup_ci==2

label var cesante_ci "Desocupado - definicion oficial del pais"	

*************
*tamemp_ci
*************
*Bolivia Pequeña 1 a 5 Mediana 6 a 49 Grande Más de 49
gen tamemp_ci=.
replace tamemp_ci=1 if tamestp>0 & tamestp<=5
replace tamemp_ci=2 if tamestp>5 & tamestp<=49
replace tamemp_ci=3 if tamestp>49 & tamestp<9999 & tamestp!=.
label var tamemp_ci "# empleados en la empresa segun rangos"
label define tamemp_ci 1 "Pequena" 2 "Mediana" 3 "Grande"
label value tamemp_ci tamemp_ci

*************
**pension_ci*
*************

egen aux_p=rsum(ingpenju ingpenbe), missing
gen pension_ci=1 if aux_p>0 & aux_p!=.
recode pension_ci .=0 
label var pension_ci "1=Recibe pension contributiva"

*************
**ypen_ci*
*************

gen ypen_ci=aux_p 
*recode ypen_ci .=0 
label var ypen_ci "Valor de la pension contributiva"

***************
*pensionsub_ci*
***************
gen pensionsub_ci= .
label var pensionsub_ci "1=recibe pension subsidiada / no contributiva"

*****************
**ypensub_ci*
*****************

gen byte ypensub_ci=.

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

gen desalent_ci=(emp_ci==0 & bustrab==2 & (pqnobus==3 | pqnobus==4))
replace desalent_ci=. if emp_ci==.


*****************
***horaspri_ci***
*****************

gen horaspri_ci=hrstra*diastra
replace horaspri_ci=. if emp_ci~=1

*****************
***horastot_ci***
*****************

gen horassec=hrsxdia*diasxsem
replace horassec=. if emp_ci~=1

egen horastot_ci=rsum(horaspri_ci horassec), missing
replace horastot_ci=. if horaspri_ci==. & horassec==.
replace horastot_ci=. if emp_ci~=1

drop horassec

***************
***subemp_ci***
***************
/*
gen subemp_ci=(horastot_ci<=30 & mashrs==1)
replace subemp_ci=. if emp_ci~=1
*/
* Segun definicion del documento metodologico: horas de la actividad principal y si esta disponible a trabajar mas horas. MGD 06/18/2014
* Se podria considerar a las dos alternativas: desea trabajar y esta dispuesto a trabajar.
gen subemp_ci=0
*replace subemp_ci=1 if disponi==1  & horaspri_ci <= 30 & emp_ci==1
replace subemp_ci=1 if (mashrs==1)  & horaspri_ci <= 30 & horaspri_ci!=. & emp_ci==1
label var subemp_ci "Personas en subempleo por horas"


*******************
***tiempoparc_ci***
*******************

gen tiempoparc_ci=(horastot_ci<=30 & mashrs==2)
replace tiempoparc_ci=. if emp_ci~=1


******************
***categopri_ci***
******************

gen categopri_ci=.
replace categopri_ci=1 if categp==4 | categp==7
replace categopri_ci=2 if categp==3 | categp==6
replace categopri_ci=3 if categp==1 | categp==2 | categp==8
replace categopri_ci=4 if categp==5
replace categopri_ci=. if emp_ci~=1

label define categopri_ci 1"Patron" 2"Cuenta propia" 
label define categopri_ci 3"Empleado" 4" No remunerado", add
label value categopri_ci categopri_ci
label variable categopri_ci "Categoria ocupacional trabajo principal"


******************
***categosec_ci***
******************

gen categosec_ci=.
replace categosec_ci=1 if categs==4 | categs==7
replace categosec_ci=2 if categs==3 | categs==6
replace categosec_ci=3 if categs==1 | categs==2 | categs==8
replace categosec_ci=4 if categs==5

label define categosec_ci 1"Patron" 2"Cuenta propia" 
label define categosec_ci 3"Empleado" 4 "No remunerado" , add
label value categosec_ci categosec_ci
label variable categosec_ci "Categoria ocupacional trabajo secundario"

*****************
***nempleos_ci***
*****************

gen nempleos_ci=.
replace nempleos_ci=1 if emp_ci==1
replace nempleos_ci=2 if emp_ci==1 & trabsec==1

/*
*****************
***firmapeq_ci***
*****************


gen firmapeq_ci=(tamestp>=1 & tamestp<=5) 
replace firmapeq_ci=. if emp~=1 | tamestp==0 
*/

*****************
***spublico_ci***
*****************
/*Para los obreros y empleados*/

gen spublico_ci=(sectorp==2)
replace spublico_ci=. if emp_ci~=1 | sectorp==0


**************
***ocupa_ci***
**************
* Modificacion MGD 07/24/2014: clasificacion CIUO -88
gen aux= ocupp 
gen ocupa_ci=.
replace ocupa_ci=1 if (aux>=210 & aux<=348)  & emp_ci==1
replace ocupa_ci=2 if (aux>=110 & aux<=131)  & emp_ci==1
replace ocupa_ci=3 if (aux>=410 & aux<=422) & emp_ci==1
replace ocupa_ci=4 if ((aux>=520 & aux<=529) | (aux>=910 & aux<=911)) & emp_ci==1
replace ocupa_ci=5 if ((aux>=510 & aux<=519) | (aux>=912 & aux<=916)) & emp_ci==1
replace ocupa_ci=6 if ((aux>=610 & aux<=625) | (aux>=920 & aux<=921)) & emp_ci==1
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
replace rama_ci=1 if (ramap>=11 & ramap<=50) & emp_ci==1
replace rama_ci=2 if (ramap>=111 & ramap<=142) & emp_ci==1
replace rama_ci=3 if (ramap>=151 & ramap<=369) & emp_ci==1
replace rama_ci=4 if (ramap>=401 & ramap<=410) & emp_ci==1
replace rama_ci=5 if (ramap>=451 & ramap<=454) & emp_ci==1
replace rama_ci=6 if (ramap>=501 & ramap<=553) & emp_ci==1 
replace rama_ci=7 if (ramap>=601 & ramap<=642) & emp_ci==1
replace rama_ci=8 if (ramap>=651 & ramap<=713) & emp_ci==1
replace rama_ci=9 if (ramap>=722 & ramap<=980) & emp_ci==1


****************
***durades_ci***
****************
/*En meses*/

gen durades_ci=tpobusca/4.3 if perbus==1
replace durades_ci=tpobusca if perbus==2
replace durades_ci=tpobusca*12 if perbus==3
replace durades_ci=. if emp_ci~=0

*******************
***antiguedad_ci***
*******************
/*En años*/

gen antiguedad_ci=.
replace antiguedad_ci=ctotpop/(4.3*12) if ctotpopp==1 & emp_ci==1
replace antiguedad_ci=ctotpop/12       if ctotpopp==2 & emp_ci==1
replace antiguedad_ci=ctotpop          if ctotpopp==3 & emp_ci==1

*******************
***categoinac_ci***
*******************
gen categoinac_ci =1 if (inactivo==1 & condocup_ci==3)
replace categoinac_ci = 2 if  (inactivo==2 & condocup_ci==3)
replace categoinac_ci = 3 if  (inactivo==3 & condocup_ci==3)
replace categoinac_ci = 4 if  ((categoinac_ci ~=1 & categoinac_ci ~=2 & categoinac_ci ~=3) & condocup_ci==3)
label var categoinac_ci "Categoría de inactividad"
label define categoinac_ci 1 "jubilados o pensionados" 2 "Estudiantes" 3 "Quehaceres domésticos" 4 "Otros" 

*******************
***formal***
*******************
*Modificación Mayra Sáenz - Septiembre 2014
*Las variables afiliado y cotizando son missing.
gen byte formal_ci=.
label var formal_ci "1=afiliado o cotizante / PEA"
*************************************************************************************
*******************************INGRESOS**********************************************
*************************************************************************************

***************
***ylmpri_ci***
***************

*Ingreso laboral monetario de la actividad principal

gen ylmpri_ci=.
replace ylmpri_ci=ingreso*30  if frecp==1 
replace ylmpri_ci=ingreso*4.3 if frecp==2 
replace ylmpri_ci=ingreso*2   if frecp==3 
replace ylmpri_ci=ingreso     if frecp==4 
replace ylmpri_ci=ingreso/6   if frecp==5
replace ylmpri_ci=ingreso/12  if frecp==6

replace ylmpri_ci=0 if categopri_ci==4 | ingreso==999997 | ingreso==999998
replace ylmpri_ci=. if ingreso==999999
replace ylmpri_ci=. if emp_ci~=1


*******************
*** nrylmpri_ci ***
*******************

gen nrylmpri_ci=(ylmpri_ci==. & emp_ci==1)


******************
*** ylnmpri_ci ***
******************

gen ylnmpri_ci=.


***************
***ylmsec_ci***
***************

*Ingreso laboral monetario de la actividad secundaria

gen ylmsec_ci=.
replace ylmsec_ci=ysecun*30  if frecs==1 
replace ylmsec_ci=ysecun*4.3 if frecs==2 
replace ylmsec_ci=ysecun*2   if frecs==3 
replace ylmsec_ci=ysecun     if frecs==4 
replace ylmsec_ci=ysecun/6   if frecs==5
replace ylmsec_ci=ysecun/12  if frecs==6

replace ylmsec_ci=0 if categosec_ci==4 | ysecun==999997 | ysecun==999998
replace ylmsec_ci=. if ysecun==999999
replace ylmsec_ci=. if emp_ci~=1


******************
****ylnmsec_ci****
******************

gen ylnmsec_ci=.


************
***ylm_ci***
************

egen ylm_ci=rsum(ylmpri_ci ylmsec_ci), missing
replace ylm_ci=. if ylmpri_ci==. & ylmsec_ci==.


*************
***ylnm_ci***
*************

gen ylnm_ci=.

*************
***ynlm_ci***
*************

*Ingresos no laborales monetarios

local jjj="penju penbe asisf otrho alqui alpag inte otro"
foreach i of local jjj {
replace ing`i'=0 if ing`i'==99998 
replace ing`i'=. if ing`i'==99999
}

egen ynlm_ci=rsum(ingpenju ingpenbe ingasisf ingotrho ingalqui ingalpag inginte ingotro), missing
replace ynlm_ci=. if ingpenju==. & ingpenbe==. & ingasisf==. & ingotrho==. & ingalqui==. & ingalpag==. & inginte==. & ingotro==. 


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
*** ylnm_ch ***
***************

gen ylnm_ch=.

*******************
*** remesas_ch ***
*******************

gen remesas_ch=.


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

gen rentaimp_ch=.


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

replace aedu_ci=0 if nivel==0 | nivel==1 

replace aedu_ci=1 if nivel==2 & curso==1
replace aedu_ci=2 if nivel==2 & curso==2
replace aedu_ci=3 if nivel==2 & curso==3
replace aedu_ci=4 if nivel==2 & curso==4
replace aedu_ci=5 if nivel==2 & curso==5

replace aedu_ci=6 if nivel==2 & curso==6
replace aedu_ci=7 if nivel==2 & curso==7
replace aedu_ci=8 if nivel==2 & curso==8
replace aedu_ci=9 if nivel==3 & curso==1
replace aedu_ci=10 if nivel==3 & curso==2
replace aedu_ci=11 if nivel==3 & curso==3
replace aedu_ci=12 if nivel==3 & curso==4

replace aedu_ci=13 if nivel>=6 & nivel<=9 & curso==1 
replace aedu_ci=14 if nivel>=6 & nivel<=9 & curso==2 
replace aedu_ci=15 if nivel>=6 & nivel<=9 & curso==3 
replace aedu_ci=16 if nivel>=6 & nivel<=9 & curso==4 

replace aedu_ci=16 if (nivel==6 | nivel==8) & (curso==7 | curso==8) 
replace aedu_ci=17 if (nivel==7 | nivel==9) & (curso==7 | curso==8) 


**************
***eduno_ci***
**************

gen byte eduno_ci=(nivel==0 | nivel==1) 
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

gen byte eduui_ci=(aedu_ci>=13 & aedu_ci<=16 & curso<7)
replace eduui_ci=. if aedu_ci==.
label variable eduui_ci "Universitaria incompleta"

***************
***eduuc_ci***
***************

gen byte eduuc_ci=(aedu_ci>=16 & aedu_ci<. & curso>=7 & curso<=8)
replace eduuc_ci=. if aedu_ci==.
label variable eduuc_ci "Universitaria incompleta o mas"


***************
***edupre_ci***
***************

gen byte edupre_ci=(nivel==1)
replace edupre_ci=. if aedu_ci==.
label variable edupre_ci "Educacion preescolar"


**************
***eduac_ci***
**************
gen byte eduac_ci=.
replace eduac_ci=1 if (nivel==7)
replace eduac_ci=0 if (nivel==6 | nivel==8 | nivel==9)
label variable eduac_ci "Superior universitario vs superior no universitario"

***************
***asiste_ci***
***************

gen asiste_ci=(asist==1)
label variable asiste_ci "Asiste actualmente a la escuela"

**************
***pqnoasis***
**************

gen pqnoasis_ci=pqnoasi if asiste_ci==2

**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**
	
**************
*pqnoasis1_ci*
**************

gen pqnoasis1_ci =.
 
***************
***repite_ci***
***************

gen repite_ci=.
gen repiteult_ci=.


***************
***edupub_ci***
***************
/*Sobre los que se matricularon ese año*/

gen edupub_ci=(estabes==1 | estabes==3)
replace edupub_ci=. if estabes==. | estabes==0

**************
***tecnica_ci*
**************

gen tecnica_ci=.
replace tecnica_ci=1 if nivel==8
recode tecnica_ci .=0 
label var tecnica_ci "1=formacion terciaria tecnica"


**********************************
**** VARIABLES DE LA VIVIENDA ****
**********************************

gen aguared_ch=(agua==1 | agua==2 | agua==3)

gen aguadist_ch=1 if agua==1
replace aguadist_ch=2 if agua==2
replace aguadist_ch=3 if agua>=3 & agua<=7

*Inclusión Mayra Sáenz Julio 2013
gen aguamala_ch=(agua==6)
replace aguamala_ch=. if agua==.
label var aguamala_ch "Agua unimproved según MDG" 
/*NA*/

gen aguamide_ch=.
/*NA*/

gen luz_ch=.
gen luzmide_ch=.
/*NA*/

gen combust_ch=.
/*NA*/

gen bano_ch=(inodoro==1 | inodoro==2)

gen banoex_ch=(usoserv==1)
replace banoex_ch=. if usoserv==0

gen des1_ch=.

gen des2_ch=.
replace des2_ch=0 if desague==0
replace des2_ch=1 if desague==1 | desague==2
replace des2_ch=2 if desague==3

gen piso_ch=0 if pisos==5
replace piso_ch=1 if pisos>=1 & pisos<=4
replace piso_ch=2 if pisos==6

gen techo_ch=0 if techos==4
replace techo_ch=1 if techos>=1 & techos<=3
replace techo_ch=2 if techos==5

gen pared_ch=0 if paredes==1 | paredes==2 | paredes==6
replace pared_ch=1 if paredes==3 | paredes==4 | paredes==5
replace pared_ch=2 if paredes==7

gen resid_ch=.
/*NA*/

**Daniela Zuluaga- Enero 2018: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**
	
*********************
***aguamejorada_ch***
*********************
gen  aguamejorada_ch = 1 if (agua >= 1 & agua <=3) | agua==5
replace aguamejorada_ch = 0 if  agua==4 | (agua >= 6 & agua <=7)
		
			
*********************
***banomejorado_ch***
*********************
gen banomejorado_ch = 1 if  (inodoro == 1 | inodoro == 2 ) & (desague >= 1 & desague <=3) & usoserv == 1
replace banomejorado_ch = 0 if ((inodoro == 1 | inodoro == 2 ) & (desague >= 1 & desague <=3) & usoserv == 2) | inodoro ==3 | ((inodoro == 1 | inodoro == 2 ) & desague == 4)
	
	
gen dorm_ch=nrodorm

gen cuartos_ch=nrocuart

gen cocina_ch=(cocina==1)

gen telef_ch=.
gen cel_ch=.
gen refrig_ch=.
gen freez_ch=.
gen auto_ch=.
gen compu_ch=.
gen internet_ch=.
/*NA*/


gen vivi1_ch=1 if tipoviv==1
replace vivi1_ch=2 if tipoviv==2
replace vivi1_ch=3 if tipoviv>=3 & tipoviv<=5


gen vivi2_ch=(tipoviv<=2)

gen viviprop_ch=0 if tenencia==2
replace viviprop_ch=1 if tenencia==1
replace viviprop_ch=3 if tenencia>=3 & tenencia<=6

gen vivitit_ch=.
/*NA*/

gen vivialq_ch=.
gen vivialqimp_ch=.





********************
*** BOLIVIA 1997 ***
********************

/*
parentco
4. ¿Qué relación de parentesco tiene con el jefe del hogar?
  1. Jefe
  2. Esposo(a) o conviviente
  3. Hijo(a) o entenado(a)
  4. Yerno o nuera
  5. Nieto(a)
  6. Hermano(a) o cuñado(a)
  7: Padre, madre o suegro(a)
  8. Otro pariente
  9. Servicio doméstico cama adentro
 10. Otro no pariente
 */

 gen     incl=1 if (parentco>=1 &  parentco<=10)
 replace incl=0 if (parentco==9)

* Variables

/*
contexto
Contexto Geográfico
DEPARTAMENTO DE CHUQUISACA
 11: Ciudad de Sucre
 13: Resto urbano
 14: Área rural
DEPARTAMENTO DE LA PAZ
 21: Ciudad de La Paz
 22: Ciudad de El Alto
 23: Resto urbano
 24: Área rural
DEPARTAMENTO DE COCHABAMBA
 31: Ciudad de Cochabamba
 33: Resto urbano
 34: Área rural
DEPARTAMENTO DE ORURO
 41: Ciudad de Oruro
 43: Resto urbano
 44: Área rural
DEPARTAMENTO DE POTOSÍ
 51: Ciudad de Potosí
 53: Resto urbano
 54: Área rural
DEPARTAMENTO DE TARIJA
 61: Ciudad de Tarija
 63: Resto urbano
 64: Área rural
DEPARTAMENTO DE SANTA CRUZ
 71: Ciudad de Santa Cruz
 73: Resto urbano
 74: Área rural
DEPARTAMENTO DE BENI
 81: Ciudad de Trinidad
 83: Resto urbano
DEPARTAMENTO DE PANDO
 91: Ciudad de Cobija
 94: Área rural
*/

 gen str2 cont_str=string(contexto)
 gen str1 area_= substr(cont_str,2,1)
 destring area_, replace

 gen	 area=1 if (area_>=1 & area_<=3)
 replace area=2 if (area_==4)


* Gender classification of the population refering to the head of the household.

 sort id_hogar  id_pers 

 gen	 sexo_d_=1 if parentco==1 & sexo==1
 replace sexo_d_=2 if parentco==1 & sexo==2

 egen sexo_d=max(sexo_d_), by(id_hogar)

 sort id_hogar  id_pers 

** Years of education. 
* Included in the database 
* -1 (0-4 years old)

 recode anoest (-1=99)

** Economic Active Population 
* Included in the database 
* For the population aged 7 years or more

/* condact
CONDACT. CONDICIÓN DE ACTIVIDAD
 0. No aplicable (menores de 7 años)
 1. Ocupados
 2. Cesantes
 3. Buscan trabajo por primera vez
 4. Jubilados o benemérito
 5. Estudiantes
 6. Amas de casa
 7. Incapacitado para trabajar
 8. Otros inactivos
*/

 gen	 peaa=1 if (condact==1) & edad>=10
 replace peaa=2 if (condact>=2 & condact<=3) & edad>=10
 replace peaa=3 if (condact>=4 & condact<=8) & edad>=10

 gen TASADESO=0 if peaa==1
 replace TASADESO=1 if peaa==2 


************
** ETHNIC **
************

/*
idioma
11.¿Qué idiomas habla habitualmente?
-1. No aplicable (menores de 5 años)
  0. Ninguno
  1. Castellano
  2. Quechua
  3. Castellano y quechua
  4. Aymará
  5. Castellano y aymará
  6. Quechua y aymará
  7. Castellano, quechua y aymará
  8. Guaraní
  9. Castellano y guaraní
 10. Quechua y guaraní
 11. Castellano, quechua y guaraní
 12. Aymará y guaraní
 13. Castellano, aymará y guaraní
 16. Otro nativo
 17. Castellano y otro nativo
 20. Aymará y otro nativo
 21. Castellano, aymará y otro nativo
 22. Quechua, aymará y otro nativo
 23. Castellano, quechua, aymará y otro nativo
 32. Extranjero
 33. Castellano y extranjero
 35. Castellano, quechua y extranjero
 37. Castellano, aymará y extranjero
*/

 gen	 indigena=.
 replace indigena=1 if (idioma>=2 & idioma<=23)
 replace indigena=0 if (idioma==1 | (idioma>=32 & idioma<=37))

** Missings. Persons with less than 5 years of age
* Filling missings using the mother auto identification.
* 3. "Hijo (a) 5. Nieto (a)

 gen pertn_m=indigena if (parentco==2 & sexo==2) | (parentco==1 & sexo==2)
 egen pertn_mh=max(pertn_m), by(id_hogar)
 replace indigena=pertn_mh if edad<=4 & (parentco==3 | parentco==5)

 
***************
*** REGIONS ***
***************

/*
Región
Área geográfica utilizada para agrupar los Departamentos de acuerdo a su tipo ecológico
predominante.
La región clasifica los Departamentos en:
 a) Altiplano comprende los departamentos de La Paz, Oruro, y Potosí.
 b) Valle comprende los departamentos Cochabamba, Chuquisaca y Tarija.
 c) Llano comprende los departamentos de Santa Cruz, Beni y Pando.
*/

/*
depto
 1. Chuquisaca 
 2. La Paz 
 3. Cochabamba
 4. Oruro
 5. Potosi
 6. Tarija
 7. Sta cruz
 8. Beni
 9. Pando
*/

 gen	 region=1 if depto==2 | depto==4 | depto==5
 replace region=2 if depto==1 | depto==3 | depto==6
 replace region=3 if depto==7 | depto==8 | depto==9

************************
*** MDGs CALCULATION ***
************************

** For further information on this do file contact Pavel Luengas (pavell@iadb.org)

*** GOAL 2. ACHIEVE UNIVERSAL PRIMARY EDUCATION

/*
asist 
14. ¿Este año asiste o asistió a algún establecimiento educativo?)
 1.Si
 2.No==>17

cursoins
16. ¿A qué curso y nivel fue inscrito o se inscribió este año?
-1. No aplicable (menores de 5 años)
 0. Ninguno
 1. Un año
 6. Seis años
 7. Siete años (primaria), egresado (normal, universidad, enseñanza técnica, institución militar o religiosa), siete meses
(otros cursos)
 8. Ocho años (primaria), titulado (normal, universidad, enseñanza técnica), ocho meses (otros cursos)
 9. Nueve meses (otros cursos)
 10. Diez meses (otros cursos)
 12. Doce meses (otros cursos)
 13. Trece meses (otros cursos)
 24. Veinte y cuatro meses (otros cursos)

nivelins
16. ¿A qué curso y nivel fue inscrito o se inscribió este año?)
 0. No aplicable
SISTEMA ESCOLAR
 1. Pre - kinder o kinder==> 18
 2. Primaria (básico, intermedio)
 3. Secundaria (medio)
EDUCACIÓN DE ADULTOS
 4. Educación básica de adultos (E.B.A.)
 5. Centro de educación media de adultos (C.E.M.A.)
EDUCACIÓN SUPERIOR O TÉCNICA
 6. Normal
 7. Universitario
 8. Enseñanza técnica
 9. Institución militar o religiosa
OTROS
 10. Otros cursos

pqnoasist
17. ¿Cuál es la causa principal por la que no asiste o no asistió?)
 0. No aplicable
 1. Culminó sus estudios
 2. No hay cursos y niveles superiores
 3. Falta de dinero
 4. Problemas familiares
 5. Por trabajo
 6. Enfermedad o defecto físico
 7. Inasistencia de maestros
 8. Otro
*/

** Target 3, Indicator: Net Attendance Ratio in Primary
* ISCED 1

 gen	 NERP=0 if  (edad>=6 & edad<=11) & (asist==1 | asist==2)
 replace NERP=1 if  (edad>=6 & edad<=11) & (asist==1) & (nivelins==2 & (cursoins>=1 & cursoins<=6)) 

** Target 3, Additional Indicator: Net Attendance Ratio in Secondary
* ISCED 2 & 3

 gen	 NERS=0 if  (edad>=12 & edad<=17) & (asist==1 | asist==2)
 replace NERS=1 if  (edad>=12 & edad<=17) & (asist==1) & ((nivelins==2 & (cursoins>=7 & cursoins<=8)) | (nivelins==3 & (cursoins>=1 & cursoins<=4)))
	
* Upper secondary
* Secundaria (1 a 4 grado)
 gen	 NERS2=0 if  (edad>=14 & edad<=17) & (asist==1 | asist==2)
 replace NERS2=1 if  (edad>=14 & edad<=17) & (asist==1) & (nivelins==3 & (cursoins>=1 & cursoins<=4))

** Target 3, Indicator: Literacy Rate of 15-24 Years Old
* At least 5 years of formal education

 gen	 ALFABET=0 if  (edad>=15 & edad<=24) & (anoest>=0 & anoest<99) 
 replace ALFABET=1 if  (edad>=15 & edad<=24) & (anoest>=5 & anoest<99) 

** Target 3, Indicator: Literacy Rate of 15-24 Years Old
* Read & write

 gen	 ALFABET2=0 if  (edad>=15 & edad<=24) & (alfabet==1 | alfabet==2)
 replace ALFABET2=1 if  (edad>=15 & edad<=24) & (alfabet==1)
	
*** GOAL 3 PROMOTE GENDER EQUALITY AND EMPOWER WOMEN

 gen prim=1 if  ((asist==1) & (nivelins==2 & (cursoins>=1 & cursoins<=6)))
 gen sec=1  if  ((asist==1) & (((nivelins==2 & (cursoins>=7 & cursoins<=8)) | (nivelins==3 & (cursoins>=1 & cursoins<=4)))))
 gen ter=1  if  ((asist==1) & (nivelins>=6 & nivelins<=9))

** Target 4, Indicator: Ratio Girls to boys in primary, secondary and tertiary (%)

** Target 4, Ratio of Girls to Boys in Primary*

 gen RPRIMM=1 if (prim==1) & sexo==2 
 replace RPRIMM=0 if RPRIMM==. 
 gen RPRIMH=1 if (prim==1) & sexo==1 
 replace RPRIMH=0 if RPRIMH==.
 gen RATIOPRIM=0 if     (prim==1) & sexo==2  
 replace RATIOPRIM=1 if (prim==1)  & sexo==1   
	
** Target 4, Ratio of Girls to Boys in Secondary*

 gen RSECM=1 if (sec==1) & sexo==2 
 replace RSECM=0 if RSECM==.
 gen RSECH=1 if (sec==1) & sexo==1 
 replace RSECH=0 if RSECH==.

 gen RATIOSEC=0     if (sec==1) & sexo==2 
 replace RATIOSEC=1 if (sec==1) & sexo==1  
	
** Target 4, Indicator: Ratio of Girls to Boys in Tertiary*

 gen RTERM=1 if (ter==1) & sexo==2 
 replace RTERM=0 if RTERM==.
 gen RTERH=1 if (ter==1) & sexo==1 
 replace RTERH=0 if RTERH==.

 gen RATIOTER=0     if (ter==1) & sexo==2 
 replace RATIOTER=1 if (ter==1) & sexo==1  

** Target 4, Indicator: Ratio of Girls to Boys in Primary, Secondary and Tertiary*

 gen RALLM=1 if (prim==1 | sec==1 | ter==1) & sexo==2 
 replace RALLM=0 if RALLM==.
 gen RALLH=1 if (prim==1 | sec==1 | ter==1) & sexo==1 
 replace RALLH=0 if RALLH==.

 gen RATIOALL=0 if     (prim==1 | sec==1 | ter==1) & sexo==2  
 replace RATIOALL=1 if (prim==1 | sec==1 | ter==1) & sexo==1    

** Target 4, Indicator: Ratio of literate women to men 15-24 year olds*
* Knows how to read & write

 gen MA2=1 if ((alfabet==1) & (edad>=15 & edad<=24) & (sexo==2)) 
 replace MA2=0 if MA2==.
 gen HA2=1 if ((alfabet==1) & (edad>=15 & edad<=24) & (sexo==1)) 
 replace HA2=0 if HA2==.

 gen RATIOLIT2=0     if ((alfabet==1) & (edad>=15 & edad<=24) & (sexo==2)) 
 replace RATIOLIT2=1 if ((alfabet==1) & (edad>=15 & edad<=24) & (sexo==1)) 

** Target 4, Indicator: Ratio of literate women to men 15-24 year olds*
* At least 5 years of formal education

 gen MA=1 if ((anoest>=5 & anoest<99) & (edad>=15 & edad<=24) & (sexo==2)) 
 replace MA=0 if MA==.
 gen HA=1 if ((anoest>=5 & anoest<99) & (edad>=15 & edad<=24) & (sexo==1)) 
 replace HA=0 if HA==.

 gen RATIOLIT=0 if     ((anoest>=5 & anoest<99) & (edad>=15 & edad<=24) & (sexo==2)) 
 replace RATIOLIT=1 if ((anoest>=5 & anoest<99) & (edad>=15 & edad<=24) & (sexo==1)) 

** Target 4, Indicator: Share of women in wage employment in the non-agricultural sector (%)
* Without Domestic Service (7 years old or more)
/*
categp
32. ¿En esta ocupación Ud. Trabaja (trabajaba) como:		ramap
								actividad económica ocupación principal
 0. No aplicable
 1. Obrero?
 2. Empleado?
 3. Cuenta propia?
 4. Patrón, empleador o socio?
 5. Trabajador familiar?
 6. Profesional independiente?
 7. Cooperativista por dividendos?
 8. Empleada (o) del hogar?
*/

 gen	 WENAS=0 if  (edad>=15 & edad<=64) & (categp==1 | categp==2) & (ramap>=111 & ramar<=980) & (condact==1)
 replace WENAS=1 if  (edad>=15 & edad<=64) & (categp==1 | categp==2) & (ramap>=111 & ramar<=980) & (condact==1) & (sexo==2)

** Target 4, Indicator: Share of women in wage employment in the non-agricultural sector (%)
* With domestic servants

 gen	 WENASD=0 if  (edad>=15 & edad<=64) & (categp==1 | categp==2 |categp==8) & (ramap>=111 & ramar<=980) & (condact==1)
 replace WENASD=1 if  (edad>=15 & edad<=64) & (categp==1 | categp==2 |categp==8) & (ramap>=111 & ramar<=980) & (condact==1) & (sexo==2)

*Proportion of 1 Year Old Children Immunized Against Measles*
* 1 - 2
* Age range in the survey 0-2

/*
codsar
75. ¿De las siguientes vacunas, cuáles ha recibido ( )? Vacuna antisarampión. contra el sarampión
se aplica una dosis generalmente a partir de los nueve meses.
 0. No aplicable
 1. Si
 2. No
 9. Ignorado
*/

 gen MEASLES=0 if     (edad>=1 & edad<=2) & (codsar==1 | codsar==2)
 replace MEASLES=1 if (edad>=1 & edad<=2) & (codsar==1)

*** GOAL 7 ENSURE ENVIROMENTAL SUSTAINABILITY

* Access to Electricity ** Additional Indicator
/*
elec 
95. ¿Tiene energía eléctrica?
 1: Si
 2: No
*/

* Gender classification of the population refers to the head of the household.

 gen	 ELEC=0 if  (elec==1 | elec==2)   /* Total population excluding missing information */
 replace ELEC=1 if  (elec==1)
	
** Target 10, Indicator: Proportion of the population with sustainable access to an improved water source (%)
/*
agua
91. ¿Cómo se abastece de agua su hogar?
 1. Por cañería (de red pública) dentro de la vivienda
 2. Por cañería (de red pública) fuera de la vivienda, pero dentro del edificio, lote o terreno
 3. Por cañería (de red pública) fuera del edificio, lote o terreno
 4. Carro repartidor
 5. Pozo o noria
 6. Río, lago, vertiente o acequia
 7. Otro
*/

* Gender classification of the population refers to the head of the household.

 gen	 WATER=0 if  (agua>=1 & agua<=7)  /* Total population excluding missing information */
 replace WATER=1 if  ((agua>=1 & agua<=3) | agua==5)
	
** Target 10, Indicator: Proportion of Population with Access to Improved Sanitation, Urban and Rural (%)

/*
inodoro						usoserv					
92. ¿Tiene inodoro,water, letrina o excusado? 	93. ¿El uso del serv. sanitario es...	
 1. Sí, con descarga instantánea		 1. Sólo de este hogar?			
 2. Sí, sin descarga instantánea		 2. Compartido con otros hogares?
 3. No ==>95						
											
desague											
94. ¿El desagüe del servicio sanitario se realiza a
 1. Alcantarillado público?
 2. Cámara séptica?
 3. Pozo ciego?
 4. Otro
*/

* Gender classification of the population refers to the head of the household.

 gen	 SANITATION=0 if (desague>=1 & desague<=4) & (inodoro>=1 & inodoro<=3) /* Total population excluding missing information */ 
 replace SANITATION=1 if (desague>=1 & desague<=2)

** Target 11, Indicator: Proportion of the population with access to secure tenure (%)
/*
tipoviv						tenencia
82. La vivienda es:				86: ¿Su vivienda es:

1. Casa independiente?				1. Propia?
2. Departamento					2. Alquilada?
3. Cuarto (s) en casa de vecindad?		3. Contrato anticrético?
4. Choza, pahuichi				4. Contrato mixto?
5. Vivienda improvisada				5. Cedida por servicios?
						6. Cedida por parentesco?
						7. Otro
						

paredes						pisos
83. Qué material predomina en las paredes 	85. ¿Qué material predomina en el piso de su vivienda?
nteriores de su vivienda?		

1. Adobe revocado				1. Madera
2. Adobe sin revocar				2. Mosaico / baldosas
3. Ladrillo, bloques de cemento, hormigón, etc.	3. Ladrillo
4. Piedra					4. Cemento
5. Madera					5. Tierra
6. Caña, palma, troncos				6. Otro
7. Otro 			
						
nrocuart
87: ¿Cuántos cuartos o habitaciones ocupa su hogar (sin contar baño y cocina)?)
*/

 gen persroom=pers/nrocuart

** Target 11, Indicator: Proportion of the population with access to secure tenure (%)

* Indicator components

* 1. Non secure tenure or type of dwelling.

 gen	 secten_1=0 if (tipoviv>=1 & tipoviv<=5) & (tenencia>=1 & tenencia<=7) /* Total population excluding missing information */
 replace secten_1=1 if (tipoviv==4 | tipoviv==5) | (tenencia==5 | tenencia==6 | tenencia==7)

* 2. Low quality of the floor or walls materials.

 gen	 secten_2=0 if (paredes>=1 & paredes<=7) & (pisos>=1 & pisos<=6) /* Total population excluding missing information */
 replace secten_2=1 if (paredes==6 | paredes==7) | (pisos==5 | pisos==6)

* 3. Crowding (defined as not more than two people sharing the same room)

 gen secten_3=1     if (persroom>2) 

* 4. Lack of basic services

 gen secten_4=1	   if (SANITATION==0 | WATER==0)

** Target 11, Indicator: Proportion of the population with access to secure tenure (%)

* Gender classification of the population refers to the head of the household.

 gen	 SECTEN=1 if  (secten_1>=0 & secten_1<=1) & (secten_2>=0 & secten_2<=1) /* Total population excluding missing information */
 replace SECTEN=0 if  (secten_1==1 | secten_2==1 | secten_3==1 | secten_4==1)

* Dirt floors ** Addtional indicator
* 85. ¿Qué material predomina en el piso de su vivienda?

* Gender classification of the population refers to the head of the household.

 gen	 DIRT=0 if  (pisos>=1 & pisos<=6)  /* Total population excluding missing information */ 
 replace DIRT=1 if  (pisos==5)

** GOAL 8. DEVELOP A GLOBAL PARTNERSHIP FOR DEVELOPMENT

** Target 16, Indicator: Unemployment Rate of 15 year-olds (%)

 gen	 UNMPLYMENT15=0 if  (edad>=15 & edad<=24) & (TASADESO==0 | TASADESO==1) 
 replace UNMPLYMENT15=1 if  (edad>=15 & edad<=24) & (TASADESO==1 )

************************************************************************
**** ADDITIONAL SOCIO - ECONOMIC COMMON COUNTRY ASESSMENT INDICATORS ****
************************************************************************

** CCA 19. Proportion of children under 15 who are working
* 12 to 14

 gen	 CHILDREN=0 if  (edad>=12 & edad<15) 
 replace CHILDREN=1 if  (edad>=12 & edad<15) & (peaa==1)

** CCA 41 Number of Persons per Room*

 gen PERSROOM2=persroom if parentco==1

 gen	 popinlessthan2=1 if persroom<=2
 replace popinlessthan2=0 if popinlessthan2==.

* Gender classification of the population refers to the head of the household.

 gen     PLT2=0 if persroom<.		/* Total population excluding missing information */
 replace PLT2=1 if (popinlessthan2==1)
	
** Disconnected Youths
/*
inactivo
25. Es usted:
 1. Jubilado o benemérito?
 2. Estudiante?
 3. Ama de casa o realiza labores de casa?
 4. Incapacitado para trabajar?
 5. Otro

pqnobus
 1. Tiene trabajo asegurado que comenzará en menos de cuatro semanas
 2. Buscó antes y espera respuesta
 3. No cree poder encontrar trabajo
 4. Se cansó de buscar
 5. Espera mayor período de actividad económica
 6. Enfermedad o vejez
 7. Es estudiante
 8. Motivos familiares
 9. Otro
*/

 gen rznobus=pqnobus if edad>=10 & (inactivo==1 | inactivo==5)

 gen	 DISCONN=0 if (edad>=15 & edad<=24)
 replace DISCONN=1 if (edad>=15 & edad<=24) & (rznobus==9 | (rznobus>=3 & rznobus<=5))

*** Rezago escolar

 gen 	rezago=0	if (anoest>=0 & anoest<99)  & edad==6 /* This year of age is not included in the calculations */
	 
 replace rezago=1 	if (anoest>=0 & anoest<1 )  & edad==7
 replace rezago=0 	if (anoest>=1 & anoest<99)  & edad==7

 replace rezago=1 	if (anoest>=0 & anoest<2 )  & edad==8
 replace rezago=0	if (anoest>=2 & anoest<99)  & edad==8

 replace rezago=1 	if (anoest>=0 & anoest<3 )  & edad==9
 replace rezago=0	if (anoest>=3 & anoest<99)  & edad==9

 replace rezago=1 	if (anoest>=0 & anoest<4 )  & edad==10
 replace rezago=0	if (anoest>=4 & anoest<99)  & edad==10

 replace rezago=1 	if (anoest>=0 & anoest<5 )  & edad==11
 replace rezago=0	if (anoest>=5 & anoest<99)  & edad==11

 replace rezago=1	if (anoest>=0 & anoest<6)   & edad==12
 replace rezago=0	if (anoest>=6 & anoest<99)  & edad==12

 replace rezago=1 	if (anoest>=0 & anoest<7)   & edad==13
 replace rezago=0	if (anoest>=7 & anoest<99)  & edad==13

 replace rezago=1 	if (anoest>=0 & anoest<8)   & edad==14
 replace rezago=0	if (anoest>=8 & anoest<99)  & edad==14

 replace rezago=1 	if (anoest>=0 & anoest<9 )  & edad==15
 replace rezago=0	if (anoest>=9 & anoest<99)  & edad==15

 replace rezago=1 	if (anoest>=0  & anoest<10) & edad==16
 replace rezago=0	if (anoest>=10 & anoest<99) & edad==16

 replace rezago=1 	if (anoest>=0  & anoest<11) & edad==17
 replace rezago=0	if (anoest>=11 & anoest<99) & edad==17

* Primary and Secondary [ISCED 1, 2 & 3]

 gen     REZ=0 if (edad>=7 & edad<=17) & (rezago==1 | rezago==0)
 replace REZ=1 if (edad>=7 & edad<=17) & (rezago==1)
		
* Primary completion rate [15 - 24 years of age]

 gen     PRIMCOMP=0 if (edad>=15 & edad<=24) & (anoest>=0  & anoest<99)
 replace PRIMCOMP=1 if (edad>=15 & edad<=24) & (anoest>=6  & anoest<99)

* Average years of education of the population 15+

 gen     AEDUC_15=anoest if ((edad>=15 & edad<.) & (anoest>=0 & anoest<99))

 gen     AEDUC_15_24=anoest if ((edad>=15 & edad<=24) & (anoest>=0 & anoest<99))

 gen     AEDUC_25=anoest if ((edad>=25 & edad<.) & (anoest>=0 & anoest<99))

* Grade for age

 gen GFA=(anoest/(edad-6)) if (edad>=7 & edad<=17) & (anoest>=0 & anoest<99)
* Grade for age primary
 gen GFAP=(anoest/(edad-6)) if (edad>=7 & edad<=11) & (anoest>=0 & anoest<99)

* Grade for age Secondary

 gen GFAS=(anoest/(edad-6)) if (edad>=12 & edad<=17) & (anoest>=0 & anoest<99)

ren ocup ocup_old


/*_____________________________________________________________________________________________________*/
* Asignación de etiquetas e inserción de variables externas: tipo de cambio, Indice de Precios al 
* Consumidor (2011=100), Paridad de Poder Adquisitivo (PPA 2011),  líneas de pobreza
/*_____________________________________________________________________________________________________*/


do "$ruta\harmonized\_DOCS\\Labels&ExternalVars_Harmonized_DataBank.do"

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

/*Homologar nombre del identificador de ocupaciones (isco, ciuo, etc.) y de industrias y dejarlo en base armonizada 
para análisis de trends (en el marco de estudios sobre el futuro del trabajo) 
BOLIVIA usaba para las EIHs usaba como referencia el CIUO -88 */
rename ocupp codocupa
rename ramap codindustria

compress


saveold "`base_out'", replace


log close



