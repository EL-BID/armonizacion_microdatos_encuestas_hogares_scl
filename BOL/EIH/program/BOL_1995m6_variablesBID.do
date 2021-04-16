
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

local PAIS BOL
local ENCUESTA EIH
local ANO "1995"
local ronda m6 

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\\`PAIS'_`ANO'`ronda'.dta"
*local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
                                                    
capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Bolivia
Encuesta: EIH
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

gen factor_ch=factorh
label variable factor_ch "Factor de expansion del hogar"


**************
****idh_ch****
**************
gen idh_ch=id_hogar
label variable idh_ch "ID del hogar"

*************
****idp_ci***
*************

gen idp_ci=id_pers
label variable idp_ci "ID de la persona en el hogar"

**********
***zona***
**********

/*La encuesta solo cubre ciudades capitales*/

gen byte zona_c=1

label variable zona_c "Zona del pais"
label define zona_c 1 "Urbana" 0 "Rural"
label value zona_c zona_c


*************************
*** VARIABLES DE RAZA ***
*************************

* MGR Oct. 2015: modificaciones realizadas en base a metodología enviada por SCL/GDI Maria Olga Peña

gen raza_idioma_ci = . 
gen id_ind_ci = .
gen id_afro_ci = .
gen raza_ci=.
label var raza_ci "Raza" 


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

gen anio_c=1995
label variable anio_c "Anio de la encuesta"

*********
***mes***
*********

gen mes_c=6
label variable mes_c "Mes de la encuesta"

*****************
***relacion_ci***
*****************

gen relacion_ci=.
replace relacion_ci=1 if parentco==1
replace relacion_ci=2 if parentco==2
replace relacion_ci=3 if parentco==3
replace relacion_ci=4 if parentco>=4 & parentco<=8
/*replace relacion_ci=5 if parentco==10 
replace relacion_ci=6 if parentco==9*/

*actualizado por LCM
replace relacion_ci=5 if parentco==9 
replace relacion_ci=6 if parentco==10

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

gen factor_ci=factorp
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
replace civil_ci=1 if estcivil==1
replace civil_ci=2 if estcivil==2 
replace civil_ci=3 if estcivil==4
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


************************************
*** VARIABLES DEL MERCADO LABORAL***
************************************
/* Esta sección es para los residentes habituales del hogar mayores a 7 años*/ 
	************************************
	*** VARIABLES DEL MERCADO LABORAL***
	************************************
	
/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/


*********
*lp_ci***
*********
gen lp_ci =.

replace lp_ci= 258.223108739758 if ciudad==11
replace lp_ci= 255.755395683453 if ciudad==21
replace lp_ci= 270.29702970297 if ciudad==31
replace lp_ci= 232.56821918415 if ciudad==41
replace lp_ci= 213.898246435161 if ciudad==51
replace lp_ci= 274.70908281513 if ciudad==61
replace lp_ci= 264.173228346457 if ciudad==71
replace lp_ci= 264.173228346457 if ciudad==81
replace lp_ci= 211.367380560132 if ciudad==22
replace lp_ci= 264.173228346457 if ciudad==91



label var lp_ci "Linea de pobreza oficial del pais"

*********
*lpe_ci***
*********

gen lpe_ci =.

replace lpe_ci= 130.402669913578 if ciudad==11
replace lpe_ci= 142.2 if ciudad==21
replace lpe_ci= 136.5 if ciudad==31
replace lpe_ci= 129.307929866387 if ciudad==41
replace lpe_ci= 118.927425017949 if ciudad==51
replace lpe_ci= 138.728086821641 if ciudad==61
replace lpe_ci= 134.2 if ciudad==71
replace lpe_ci= 134.2 if ciudad==81
replace lpe_ci= 128.3 if ciudad==22
replace lpe_ci= 134.2 if ciudad==91


label var lpe_ci "Linea de indigencia oficial del pais"

*************
**salmm_ci***
*************

*BOL 1995
gen salmm_ci=205.00
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
replace condocup_ci=1 if trabajo==1 | sondeo==1 
replace condocup_ci=2 if (trabajo==2 | sondeo==1) & (bustrab==1 | bus3sem==1)
recode condocup_ci .=3 if edad_ci>=10
recode condocup_ci .=4 if edad_ci<10
label var condocup_ci "Condicion de ocupación de acuerdo a def de cada pais"
label define condocup_ci 1 "Ocupado" 2 "Desocupado" 3 "Inactivo" 4 "Menor que 10" 
label value condocup_ci condocup_ci

*************
*cesante_ci* 
*************

gen cesante_ci=1 if  condact==2 & condocup_ci==2
* 2014, 03 Modificacion MLO
replace cesante_ci=0 if  cesante_ci!=1 & condocup_ci==2
*replace cesante_ci=0 if  inactiv==0 & condocup_ci==3
*replace cesante_ci=0 if condocup_ci==3 & cesante_ci != 1

label var cesante_ci "Desocupado - definicion oficial del pais"	

*************
*tamemp_ci
*************

*Bolivia Pequeña 1 a 5 Mediana 6 a 49 Grande Más de 49
gen tamemp_ci=.
replace tamemp_ci=1 if (tamest>=2 & tamest<=5) | (nperocup>=1 & nperocup<=5)| (tamestpa>=1 & tamestpa<=5)
replace tamemp_ci=2 if (tamest>=6 & tamest<=49) | (nperocup>=6 & nperocup<=49)| (tamestpa>=6 & tamestpa<=49)
replace tamemp_ci=3 if (tamest>49 & tamest<9999 & tamest!=.)  | (nperocup>49 & nperocup!=.)| (tamestpa>49 & tamestpa<999 & tamestpa!=. )
label var tamemp_ci "# empleados en la empresa segun rangos"
label define tamemp_ci 1 "Pequena" 2 "Mediana" 3 "Grande"
label value tamemp_ci tamemp_ci

*************
**pension_ci*
*************

egen aux_p=rsum(yjubcor), missing
gen pension_ci=1 if aux_p>0 & aux_p!=.
recode pension_ci .=0 
label var pension_ci "1=Recibe pension contributiva"

*************
**ypen_ci*
*************

gen ypen_ci=aux_p 
* MGD12/2/2015: No recodificar missings con ceros / recode ypen_ci .=0 
label var ypen_ci "Valor de la pension contributiva"

drop aux_p
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

gen desalent_ci=(emp_ci==0 & bustrab==2 & (pqno==1 | pqno==2))
replace desalent_ci=. if emp_ci==.


*****************
***horaspri_ci***
*****************

gen horaspri_ci=hrstrp*diastrp
replace horaspri_ci=. if emp_ci~=1 | hrstrp==99 | diastrp==9

*****************
***horastot_ci***
*****************

gen horassec=hrstrs*diastrs
replace horassec=. if emp_ci~=1 | hrstrs==99 | diastrs==9

egen horastot_ci=rsum(horaspri_ci horassec), missing
replace horastot_ci=. if horaspri_ci==. & horassec==.
replace horastot_ci=. if emp_ci~=1

drop horassec

***************
***subemp_ci***
***************
/*
/*Esta variable no es comparable con los años siguientes*/
gen subemp_ci=(horaspri_ci<=30 & quierema==1)
replace subemp_ci=. if emp_ci~=1 | quierema==9 | quierema==0
*/

* Segun definicion del documento metodologico: subempleo visible. MGD 06/18/2014
gen subemp_ci=0
replace subemp_ci=1 if quierema==1  & horaspri_ci <= 30 & emp_ci==1
label var subemp_ci "Personas en subempleo por horas"



*******************
***tiempoparc_ci***
*******************
/*Esta variable no es comparable con los años siguientes*/

gen tiempoparc_ci=(horaspri_ci<=30 & quierema==2)
replace tiempoparc_ci=. if emp_ci~=1 | quierema==9 | quierema==0


******************
***categopri_ci***
******************

gen categopri_ci=.
replace categopri_ci=1 if categ==4 
replace categopri_ci=2 if categ==3 | categ==6
replace categopri_ci=3 if categ==1 | categ==2 | categ==7
replace categopri_ci=4 if categ==5
replace categopri_ci=. if emp_ci~=1

label define categopri_ci 1"Patron" 2"Cuenta propia" 
label define categopri_ci 3"Empleado" 4" No remunerado", add
label value categopri_ci categopri_ci
label variable categopri_ci "Categoria ocupacional trabajo principal"


******************
***categosec_ci***
******************
/*NA*/
gen categosec_ci=.

*****************
***nempleos_ci***
*****************

gen nempleos_ci=.
replace nempleos_ci=1 if emp_ci==1
replace nempleos_ci=2 if emp_ci==1 & trabsec==1

*****************
*tipocontrato_ci*
*****************
/*
gen tipocontrato_ci=.
label var tipocontrato_ci "Tipo de contrato segun su duracion"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci
*/
* Alternativa considerando la variable tipo de trabajo MGD 6/11/2014
gen tipocontrato_ci=.
replace tipocontrato_ci =1 if tipotra==1 & categopri_ci==3
replace tipocontrato_ci =2 if tipotra==2 & categopri_ci==3
replace tipocontrato_ci =3 if tipocontrato_ci==. & categopri_ci==3
label var tipocontrato_ci "Tipo de contrato segun su duracion"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci


/*
*****************
***firmapeq_ci***
*****************
gen firmapeq_ci=.
replace firmapeq_ci=1 if tamest>=2 & tamest<=5
replace firmapeq_ci=1 if nperocup>=0 & nperocup<=5
replace firmapeq_ci=1 if tamestpa>=1 & tamestpa<=5
replace firmapeq_ci=0 if tamest>=6 & tamest<9999
replace firmapeq_ci=0 if nperocup>=6 
replace firmapeq_ci=0 if tamestpa>=6 & tamestpa<999
*/
*****************
***spublico_ci***
*****************
/*Para los obreros y empleados*/

gen spublico_ci=(sector==1 | sector==2)
replace spublico_ci=. if emp_ci~=1 | sector==0 | sector==9


**************
***ocupa_ci***
**************
* Modificacion MGD 07/24/2014: clasificacion CIUO -88
gen aux= ocup 
ren ocup ocup_orig
gen ocupa_ci=.
replace ocupa_ci=1 if (aux>=210 & aux<=348)  & emp_ci==1
replace ocupa_ci=2 if (aux>=110 & aux<=131)  & emp_ci==1
replace ocupa_ci=3 if (aux>=410 & aux<=422) & emp_ci==1
replace ocupa_ci=4 if ((aux>=520 & aux<=523) | (aux>=910 & aux<=911)) & emp_ci==1
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
* Indicador corregido: serie coincide con la tendencia hasta antes de 1996.
gen rama_ci=.
replace rama_ci=1 if (rama>=1 & rama<=5) & emp_ci==1
replace rama_ci=2 if (rama>=10 & rama<=14) & emp_ci==1
replace rama_ci=3 if (rama>=15 & rama<=37) & emp_ci==1
replace rama_ci=4 if (rama>=40 & rama<=41) & emp_ci==1
replace rama_ci=5 if (rama==45) & emp_ci==1
replace rama_ci=6 if (rama>=50 & rama<=56) & emp_ci==1 
replace rama_ci=7 if (rama>=60 & rama<=64) & emp_ci==1
replace rama_ci=8 if (rama>=65 & rama<=74) & emp_ci==1
replace rama_ci=9 if (rama>=75 & rama<=99) & emp_ci==1



****************
***durades_ci***
****************
/*En meses*/

gen durades_ci=tpobusca/30 if codtbus==1
replace durades_ci=tpobusca/4.3 if codtbus==2
replace durades_ci=tpobusca if codtbus==3
replace durades_ci=tpobusca*12 if codtbus==5


*******************
***antiguedad_ci***
*******************
/*En años*/
/*Sólo para los obreros y empleados
Esta variable no es comparable con los años siguientes al
menos que se la acote a este grupo*/

recode tpoempre (99=.)
gen antiguedad_ci=.
replace antiguedad_ci=tpoempre/(12*4.3) if codtiemp==2 & emp_ci==1
replace antiguedad_ci=tpoempre/12 if codtiemp==4 & emp_ci==1
replace antiguedad_ci=tpoempre if codtiemp==5 & emp_ci==1
*replace antiguedad_ci=. if categopri_ci~=3

*******************
***categoinac_ci***
*******************
gen categoinac_ci =1 if (pqnotrab==9 & condocup_ci==3)& pension_ci==1
replace categoinac_ci = 2 if  (pqnotrab==8 & condocup_ci==3)
replace categoinac_ci = 3 if  (pqnotrab==7 & condocup_ci==3)
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
*Para los obreros y empleados 

gen ylmpri_ci=.
replace ylmpri_ci=ingtrpr*30  if codingp==1 
replace ylmpri_ci=ingtrpr*4.3 if codingp==2 
replace ylmpri_ci=ingtrpr*2   if codingp==3 
replace ylmpri_ci=ingtrpr     if codingp==4 
replace ylmpri_ci=0 if ingtrpr==99998
replace ylmpri_ci=0 if categopri_ci==4 
replace ylmpri_ci=. if ingtrpr==0 | ingtrpr==99999
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
replace ylmsec_ci=ingtrse*30  if codingse==1 
replace ylmsec_ci=ingtrse*4.3 if codingse==2 
replace ylmsec_ci=ingtrse*2   if codingse==3 
replace ylmsec_ci=ingtrse     if codingse==4 
replace ylmsec_ci=0 if ingtrse==99998
replace ylmsec_ci=0 if categosec_ci==4 
replace ylmsec_ci=. if ingtrse==0 | ingtrse==99999
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

*Ingresos no laborales monetarios (inactivos)

gen ynlm1=yinac1 if codyinac==4
replace ynlm1=yinac1/12 if codyinac==5
replace ynlm1=0 if yinac1==99998
*2014,02 MLO
*replace ynlm1=. if yinac1==11975 | yinac1==99999
replace ynlm1=. if yinac1==99999


gen ynlm2=yinac2 if codyinac==4
replace ynlm2=yinac2/12 if codyinac==5
replace ynlm2=0 if yinac2==99998
replace ynlm2=. if yinac2==95800 | yinac2==99999

gen ynlm3=yinac3 if codyina3==4
replace ynlm3=yinac3/12 if codyina3==5
replace ynlm3=0 if yinac3==99998
*replace ynlm3=. if yinac3==21555 | yinac3==99999

replace ynlm3=. if yinac3==99999

egen ynlm_ci=rsum(ynlm1 ynlm2 ynlm3), missing
replace ynlm_ci=. if ynlm1==. & ynlm2==. & ynlm3==. 


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

replace aedu_ci=6 if nivel==3 & curso==1
replace aedu_ci=7 if nivel==3 & curso==2
replace aedu_ci=8 if nivel==3 & curso==3
replace aedu_ci=9 if (nivel==4 | nivel==7) & curso==1
replace aedu_ci=10 if (nivel==4 | nivel==7) & curso==2
replace aedu_ci=11 if (nivel==4 | nivel==7) & curso==3
replace aedu_ci=12 if (nivel==4 | nivel==7) & (curso==4 | curso==7 | curso==8)

replace aedu_ci=13 if (nivel==5 | nivel==6 | nivel==8) & curso==1 
replace aedu_ci=14 if (nivel==5 | nivel==6 | nivel==8) & curso==2 
replace aedu_ci=15 if (nivel==5 | nivel==6 | nivel==8) & curso==3 

replace aedu_ci=16 if (nivel==5 | nivel==8) & (curso==7 | curso==8)
replace aedu_ci=16 if (nivel==6) & curso==4

replace aedu_ci=17 if (nivel==6) & (curso==7 | curso==8)


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

gen byte eduuc_ci=0
replace eduuc_ci=1 if (aedu_ci==16 & curso>=7 & curso<=8)
replace eduuc_ci=1 if aedu>=17 & aedu<.
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
replace eduac_ci=1 if (nivel==6)
replace eduac_ci=0 if (nivel==5 | nivel==8)
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
/*NA*/
gen edupub_ci=.

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
/*La encuesta no tiene módulo de vivienda*/

gen aguared_ch=.
gen aguadist_ch=.
gen aguamala_ch=.
gen aguamide_ch=.
gen luz_ch=.
gen luzmide_ch=.
gen combust_ch=.
gen bano_ch=.
gen banoex_ch=.
gen des1_ch=.
gen des2_ch=.
gen piso_ch=.
gen techo_ch=.
gen pared_ch=.
gen resid_ch=.
gen  aguamejorada_ch = .
gen   banomejorado_ch = .
gen dorm_ch=.
gen cuartos_ch=.
gen cocina_ch=.
gen telef_ch=.
gen cel_ch=.
gen refrig_ch=.
gen freez_ch=.
gen auto_ch=.
gen compu_ch=.
gen internet_ch=.
gen vivi1_ch=.
gen vivi2_ch=.
gen viviprop_ch=.
gen vivitit_ch=.
gen vivialq_ch=.
gen vivialqimp_ch=.



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
rename ocup_orig codocupa
rename rama codindustria

compress


saveold "`base_out'", replace


log close






