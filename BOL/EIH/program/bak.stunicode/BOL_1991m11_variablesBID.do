
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
local ANO "1991"
local ronda m11 


local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
*local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\\`PAIS'_`ANO'`ronda'.dta"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
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

gen factor_ch=pondera
label variable factor_ch "Factor de expansion del hogar"


**************
****idh_ch****
**************
gen idh_ch=hogar
label variable idh_ch "ID del hogar"

*************
****idp_ci***
*************

sort hogar identifi
by hogar, sort: gen idp_ci=_n
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

gen anio_c=1991
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
replace relacion_ci=1 if damie_103==1
replace relacion_ci=2 if damie_103==2
replace relacion_ci=3 if damie_103==3
replace relacion_ci=4 if damie_103>=4 & damie_103<=8
replace relacion_ci=5 if damie_103==10 
replace relacion_ci=6 if damie_103==9

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
                                                                                                                                                                        
gen factor_ci=pondera
label variable factor_ci "Factor de expansion del individuo"

**********
***sexo***
**********

gen sexo_ci=damie_101

label define sexo_ci 1 "Hombre" 2 "Mujer"
label value sexo_ci sexo_ci

**********
***edad***
**********

gen edad_ci=damie_102
label variable edad_ci "Edad del individuo"


*****************
***civil_ci***
*****************

gen civil_ci=.
replace civil_ci=1 if damie_104==1
replace civil_ci=2 if damie_104==2 
replace civil_ci=3 if damie_104==4
replace civil_ci=4 if damie_104==3

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

/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/

*********
*lp_ci***
*********
/* Esta sección es para los residentes habituales del hogar mayores a 10 años*/ 
gen lp_ci =.

replace lp_ci= 181.607461091698 if carat_10==1 /*Sucre*/
replace lp_ci= 180.755395683453 if carat_10==2  & carat_100==1     /*La Paz*/
replace lp_ci= 190.09900990099 if carat_10== 3            /*Cochabamba*/
replace lp_ci= 164.367834233524 if carat_10==4           /*Oruro*/
replace lp_ci= 151.172811299111 if carat_10==5          /* Potosí*/
replace lp_ci= 193.201992309542 if carat_10==6        /*Tarija*/
replace lp_ci= 189.370078740157 if carat_10==7               /*Santa Cruz de la Sierra*/
replace lp_ci= 189.370078740157 if carat_10==8            /*Trinidad*/
replace lp_ci= 155.683690280066 if carat_10==2         & carat_100==2         /*El Alto*/
replace lp_ci= 189.370078740157 if carat_10==9 	/*Cobija*/





label var lp_ci "Linea de pobreza oficial del pais"

*********
*lpe_ci***
*********

gen lpe_ci =.

replace lpe_ci= 91.7117678513073 if carat_10== 1 /*Sucre*/
replace lpe_ci= 100.5 if carat_10==2   & carat_100==1     /*La Paz*/
replace lpe_ci= 96 if carat_10==3            /*Cochabamba*/
replace lpe_ci= 91.3885158338391 if carat_10==4           /*Oruro*/
replace lpe_ci= 84.052083082306 if carat_10==5          /* Potosí*/
replace lpe_ci= 97.5670061163188 if carat_10==6        /*Tarija*/
replace lpe_ci= 96.2 if carat_10==7               /*Santa Cruz de la Sierra*/
replace lpe_ci= 96.2 if carat_10==8            /*Trinidad*/
replace lpe_ci= 94.5 if carat_10==2         & carat_100==2         /*El Alto*/
replace lpe_ci= 96.2 if carat_10==9 /*Cobija*/



label var lpe_ci "Linea de indigencia oficial del pais"

*************
**salmm_ci***
*************

*BOL 1991
gen salmm_ci= 120.00
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
replace condocup_ci=1 if ocup1_114==1 | ocup1_115==1 | ocup1_117==1
replace condocup_ci=2 if ocup1_116==1 
recode condocup_ci .=3 if edad_ci>=10
recode condocup_ci .=4 if edad_ci<10
label var condocup_ci "Condicion de ocupación de acuerdo a def de cada pais"
label define condocup_ci 1 "Ocupado" 2 "Desocupado" 3 "Inactivo" 4 "Menor que 10" 
label value condocup_ci condocup_ci
*/

* Homologacion toda la serie 05/22/2014 MGD

gen condocup_ci=.
replace condocup_ci=1 if ocup1_114==1 | ocup1_115==1 
replace condocup_ci=2 if (ocup1_114==2 | ocup1_115==2) & ocup1_116==1 
recode condocup_ci .=3 if edad_ci>=10
recode condocup_ci .=4 if edad_ci<10
label var condocup_ci "Condicion de ocupación de acuerdo a def de cada pais"
label define condocup_ci 1 "Ocupado" 2 "Desocupado" 3 "Inactivo" 4 "Menor que 10" 
label value condocup_ci condocup_ci

*************
*cesante_ci* 
*************
* Modificacion MGD 07/25//2014: se corrije la variable ya que se estaba generando con la pregunta para inactivos.
gen cesante_ci=1 if  deso1_154==2 & condocup_ci==2
* 2014, 03 Modificacion MLO
replace cesante_ci=0 if  deso1_154==1 & condocup_ci==2
*replace cesante_ci=0 if  inac1_167==2 & condocup_ci==3
*replace cesante_ci=0 if condocup_ci==3 & cesante_ci != 1

label var cesante_ci "Desocupado - definicion oficial del pais"	

*************
*tamemp_ci
*************
*Bolivia Pequeña 1 a 5 Mediana 6 a 49 Grande Más de 49
gen tamemp_ci=.
replace tamemp_ci=1 if ocup1_123>0 & ocup1_123<=5
replace tamemp_ci=2 if ocup1_123>5 & ocup1_123<=49
replace tamemp_ci=3 if ocup1_123>49 & ocup1_123<999
label var tamemp_ci "# empleados en la empresa segun rangos"
label define tamemp_ci 1 "Pequena" 2 "Mediana" 3 "Grande"
label value tamemp_ci tamemp_ci


*************
**pension_ci*
*************

egen aux_p=rsum(inac2_177), missing
gen pension_ci=1 if aux_p>0 & aux_p!=.
recode pension_ci .=0 

label var pension_ci "1=Recibe pension contributiva"

*************
**ypen_ci*
*************

gen ypen_ci=aux_p 
* MGD 12/2/15: falta recodificar los missings
recode ypen_ci (99999=.)
*recode ypen_ci .=0 
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
	

/* Esta sección es para los residentes habituales del hogar mayores a 10 años
OJO QUE EN LAS EMCUESTAS SIGUIENTES LA MUESTRA ES MAYORES DE 7 AÑOS*/ 

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

gen desalent_ci=(emp_ci==0 & ocup1_116==2 & (inac2_174==1 | inac2_174==2))
replace desalent_ci=. if emp_ci==.


*****************
***horaspri_ci***
*****************

gen horaspri_ci=cupa1_135*cupa1_136
replace horaspri_ci=. if emp_ci~=1 | cupa1_135==99 | cupa1_136==9

*****************
***horastot_ci***
*****************

gen horassec=aseta_146
replace horassec=. if emp_ci~=1 | aseta_146==99

egen horastot_ci=rsum(horaspri_ci horassec), missing 
replace horastot_ci=. if horaspri_ci==. & horassec==.
replace horastot_ci=. if emp_ci~=1

drop horassec

***************
***subemp_ci***
***************
/*NA*/

gen subemp_ci=.


*******************
***tiempoparc_ci***
*******************
/*NA*/

gen tiempoparc_ci=.


******************
***categopri_ci***
******************

gen categopri_ci=.
replace categopri_ci=1 if ocup1_120==4 
replace categopri_ci=2 if ocup1_120==3 | ocup1_120==6
replace categopri_ci=3 if ocup1_120==1 | ocup1_120==2 | ocup1_120==7
replace categopri_ci=4 if ocup1_120==5
replace categopri_ci=. if emp_ci~=1

label define categopri_ci 1"Patron" 2"Cuenta propia" 
label define categopri_ci 3"Empleado" 4" No remunerado", add
label value categopri_ci categopri_ci
label variable categopri_ci "Categoria ocupacional trabajo principal"


******************
***categosec_ci***
******************

gen categosec_ci=.
replace categosec_ci=1 if aseta_144==4 
replace categosec_ci=2 if aseta_144==3 | aseta_144==6
replace categosec_ci=3 if aseta_144==1 | aseta_144==2 | aseta_144==7
replace categosec_ci=4 if aseta_144==5

label define categosec_ci 1"Patron" 2"Cuenta propia" 
label define categosec_ci 3"Empleado" 4" No remunerado", add
label value categosec_ci categosec_ci
label variable categosec_ci "Categoria ocupacional trabajo principal"

*****************
***nempleos_ci***
*****************

gen nempleos_ci=.
replace nempleos_ci=1 if emp_ci==1
replace nempleos_ci=2 if emp_ci==1 & aseta_142==1

/*
*****************
***firmapeq_ci***
*****************
gen firmapeq_ci=((ocup1_123>=1 & ocup1_123<=5) | (cupa1_133>=1 & cupa1_133<=5))
replace firmapeq_ci=. if ocup1_123==999 | emp_ci~=1 | cupa1_133==999
*/
*****************
***spublico_ci***
*****************
/*Para los obreros y empleados*/

gen spublico_ci=(ocup1_121==1 | ocup1_121==2)
replace spublico_ci=. if emp_ci~=1 | ocup1_121==9


**************
***ocupa_ci***
**************

* Modificacion MGD 07/24/2014: clasificacion CIUO -88
gen aux= ocup1_118 
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
replace rama_ci=1 if (ocup1_119>=1 & ocup1_119<=5)   & emp_ci==1
replace rama_ci=2 if (ocup1_119>=11 & ocup1_119<=14) & emp_ci==1
replace rama_ci=3 if (ocup1_119>=15 & ocup1_119<=37) & emp_ci==1
replace rama_ci=4 if (ocup1_119>=40 & ocup1_119<=41) & emp_ci==1
replace rama_ci=5 if (ocup1_119>=45) & emp_ci==1
replace rama_ci=6 if (ocup1_119>=50 & ocup1_119<=56) & emp_ci==1 
replace rama_ci=7 if (ocup1_119>=60 & ocup1_119<=64) & emp_ci==1
replace rama_ci=8 if (ocup1_119>=65 & ocup1_119<=74) & emp_ci==1
replace rama_ci=9 if (ocup1_119>=75 & ocup1_119<=98) & emp_ci==1



****************
***durades_ci***
****************
/*En meses*/

gen durades_ci=deso2_162/30      if deso2_200==1
replace durades_ci=deso2_162/4.3 if deso2_200==2
replace durades_ci=deso2_162     if deso2_200==4
replace durades_ci=deso2_162*12  if deso2_200==5
replace durades_ci=. if emp_ci~=0 | deso2_162==99 | deso2_200==9


*******************
***antiguedad_ci***
*******************
/*En años*/

gen antiguedad_ci=.
/*No está disponible la variable que indica la periodicidad*/

*******************
***categoinac_ci***
*******************

gen categoinac_ci =1 if ( ocup1_117==5 & condocup_ci==3)& pension_ci==1
replace categoinac_ci = 2 if  (ocup1_117==4 & condocup_ci==3)
replace categoinac_ci = 3 if  (ocup1_117==3 & condocup_ci==3)
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
replace ylmpri_ci=cupa1_137*30  if cupa1_197==1 
replace ylmpri_ci=cupa1_137*4.3 if cupa1_197==2 
replace ylmpri_ci=cupa1_137*2   if cupa1_197==3 
replace ylmpri_ci=cupa1_137     if cupa1_197==4 
replace ylmpri_ci=0 if cupa1_137==99998 
replace ylmpri_ci=0 if categopri_ci==4 
replace ylmpri_ci=. if cupa1_137==99999  | cupa1_197==9
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
replace ylmsec_ci=aseta_147*30  if aseta_198==1 
replace ylmsec_ci=aseta_147*4.3 if aseta_198==2 
replace ylmsec_ci=aseta_147*2   if aseta_198==3 
replace ylmsec_ci=aseta_147     if aseta_198==4 
replace ylmsec_ci=0 if aseta_147==99998 
replace ylmsec_ci=0 if categosec_ci==4 
replace ylmsec_ci=. if aseta_147==99999
replace ylmsec_ci=. if emp_ci~=1


******************
****ylnmsec_ci****
******************

gen ylnmsec_ci=.



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

gen ynlm1=inac2_177 if inac2_176>=1 & inac2_176<=3
replace ynlm1=0  if inac2_176==4
replace ynlm1=.  if inac2_177==99999 | inac2_176==0

gen ynlm2=inac2_179 if inac2_178>=1 & inac2_178<=41 
replace ynlm2=0 if inac2_179==99998 | inac2_178==32
replace ynlm2=. if inac2_179==99999

/*Estos ingresos por otras transferencias pueden ser mensuales
o anuales. No se provee la variable que permite distinguir entre
los mismos. Comparando con la encuesta de 1993 que tiene la misma pregunta 
el 93% declara ingresos mensuales en estas categorías, por ende,
asumo que son montos mensuales*/

egen ynlm_ci=rsum(ynlm1 ynlm2), missing
replace ynlm_ci=. if ynlm1==. & ynlm2==.


**************
***ynlnm_ci***
**************

gen ynlnm_ci=.


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

/*En esta sección es sólo para los mayores a los 6 años de edad*/

gen byte nivel=real(substr(string(damie_110),2,1))
gen byte curso=real(substr(string(damie_110),1,1))

gen byte aedu_ci=.

replace aedu_ci=0 if damie_110==0 

replace aedu_ci=1 if nivel==2 & curso==1
replace aedu_ci=2 if nivel==2 & curso==2
replace aedu_ci=3 if nivel==2 & curso==3
replace aedu_ci=4 if nivel==2 & curso==4
replace aedu_ci=5 if nivel==2 & curso==5

replace aedu_ci=6 if nivel==3 & curso==1
replace aedu_ci=7 if nivel==3 & curso==2
replace aedu_ci=8 if nivel==3 & curso==3

replace aedu_ci=9 if  nivel==4 & curso==1
replace aedu_ci=10 if nivel==4 & curso==2
replace aedu_ci=11 if nivel==4 & curso==3
replace aedu_ci=12 if nivel==4 & curso==4 

replace aedu_ci=13 if (nivel==6 | nivel==7) & curso==1 
replace aedu_ci=14 if (nivel==6 | nivel==7) & curso==2 
replace aedu_ci=16 if (nivel==6) & curso==3
replace aedu_ci=17 if (nivel==7) & curso==3


/*El nivel 5 que es enseñanza técnica no distingue entre aquellos que son de
secundaria de aquellos que son terciarios. Por ende los hago missing
Adicionalmente, en esta encuesta el códido curso==3 corresponde al nivel
completo para el caso de la educación superior, eso hace que los años de educación
presenten un quiebre*/

**************
***eduno_ci***
**************

gen byte eduno_ci=(damie_110==0) 
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

gen byte eduui_ci=(aedu_ci>=13 & aedu_ci<=17 & curso<3)
replace eduui_ci=. if aedu_ci==.
label variable eduui_ci "Universitaria incompleta"

***************
***eduuc_ci***
***************

gen byte eduuc_ci=0
replace eduuc_ci=1 if aedu_ci==16 | aedu_ci==17
replace eduuc_ci=. if aedu_ci==.
label variable eduuc_ci "Universitaria incompleta o mas"


***************
***edupre_ci***
***************

gen byte edupre_ci=.
label variable edupre_ci "Educacion preescolar"


**************
***eduac_ci***
**************
gen byte eduac_ci=.
replace eduac_ci=1 if (nivel==7)
replace eduac_ci=0 if (nivel==6)
label variable eduac_ci "Superior universitario vs superior no universitario"

***************
***asiste_ci***
***************

gen asiste_ci=(damie_111==1)
label variable asiste_ci "Asiste actualmente a la escuela"

**************
***pqnoasis***
**************

gen pqnoasis_ci=.

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

gen aguared_ch=(vivi2_120>=1 & vivi2_120<=4)
replace aguared_ch=. if vivi2_120==. 

gen aguadist_ch=1 if vivi2_120==1 | vivi2_120==4 
replace aguadist_ch=2 if vivi2_120==2 | vivi2_120==5
replace aguadist_ch=3 if (vivi2_120>=6 & vivi2_120<=9) | vivi2_120==3

*Inclusión Mayra Sáenz Julio 2013
gen aguamala_ch=(vivi2_120==7)
replace aguamala_ch=. if vivi2_120==.
label var aguamala_ch "Agua unimproved según MDG"
/*NA*/

gen aguamide_ch=.
/*NA*/

gen luz_ch=.
gen luzmide_ch=.
/*NA*/

gen combust_ch=.
/*NA*/

gen bano_ch=(vivi2_122==1 | vivi2_123==1 | vivi2_123==2)
replace bano_ch=. if vivi2_122==.

gen banoex_ch=(vivi2_122==1)
replace banoex_ch=. if vivi2_122==. | vivi2_123==.


gen des1_ch=.

gen des2_ch=.
replace des2_ch=0 if vivi2_121==4
replace des2_ch=1 if vivi2_121==1 | vivi2_121==2
replace des2_ch=2 if vivi2_121==3


gen piso_ch=0 if vivi1_108==5
replace piso_ch=1 if vivi1_108>=1 & vivi1_108<=4
replace piso_ch=2 if vivi1_108==6

gen techo_ch=0 if vivi1_106==4
replace techo_ch=1 if (vivi1_106>=1 & vivi1_106<=3) | vivi1_106==5
replace techo_ch=2 if vivi1_106==6

gen pared_ch=0 if vivi1_107==2 | vivi1_107==3 | vivi1_107==6
replace pared_ch=1 if vivi1_107==1 | vivi1_107==4 | vivi1_107==5
replace pared_ch=2 if vivi1_107==7

gen resid_ch=.
/*NA*/

**Daniela Zuluaga- Enero 2018: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**
	
*********************
***aguamejorada_ch***
*********************
gen aguamejorada_ch = 1 if (vivi2_120 >= 1 & vivi2_120 <=6)
replace aguamejorada_ch = 0 if (vivi2_120 >= 7 & vivi2_120 <=9)
		
*********************
***banomejorado_ch***
*********************
gen banomejorado_ch = 1 if  vivi2_122 == 1 & (vivi2_121 >= 1 & vivi2_121 <=3)
replace banomejorado_ch = 0 if (vivi2_122 == 1 & (vivi2_121 >= 1 & vivi2_121 <=3) & (vivi2_123 >= 1 & vivi2_123 <=4) ) | vivi2_122 == 2 | (vivi2_122 == 1 & vivi2_121 ==4)
	
gen dorm_ch=vivi1_110
recode dorm_ch (0=1)

gen cuartos_ch=vivi1_109

gen cocina_ch=(vivi1_111==1)
replace cocina_ch=. if vivi1_111==.

gen telef_ch=.
gen cel_ch=.
gen refrig_ch=.
gen freez_ch=.
gen auto_ch=.
gen compu_ch=.
gen internet_ch=.
/*NA*/


gen vivi1_ch=.

gen vivi2_ch=.

gen viviprop_ch=.
gen vivitit_ch=.
/*NA*/

gen vivialq_ch=.

gen vivialqimp_ch=.


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



compress


saveold "`base_out'", replace


log close




