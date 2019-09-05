
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
 global ruta = "\\Sdssrv03\surveys"

local PAIS BOL
local ENCUESTA EIH
local ANO "1990"
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

gen anio_c=1990
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
replace relacion_ci=1 if datge_102==1
replace relacion_ci=2 if datge_102==2
replace relacion_ci=3 if datge_102==3
replace relacion_ci=4 if datge_102>=4 & datge_102<=8
replace relacion_ci=5 if datge_102==10 
replace relacion_ci=6 if datge_102==9
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

gen sexo_ci=datge_103

label define sexo_ci 1 "Hombre" 2 "Mujer"
label value sexo_ci sexo_ci

**********
***edad***
**********

gen edad_ci=datge_101
label variable edad_ci "Edad del individuo"


*****************
***estcivil_ci***
*****************

gen civil_ci=.
replace civil_ci=1 if datge_104==1
replace civil_ci=2 if datge_104==2 
replace civil_ci=3 if datge_104==4
replace civil_ci=4 if datge_104==3

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

* MGR Oct. 2015: incorporación de variables de raza en base a metodología enviada por SCL/GDI Maria Olga Peña
/*
gen raza_ci=.
replace raza_ci= 1 if  educa_112 ==2 |  educa_112 ==3 |  educa_112 ==4 |  educa_112 ==5 
replace raza_ci= 3 if  educa_112 ==1 | educa_112 ==6 
by idh_ch: gen aux=raza_ci if relacion_ci==1
by idh_ch: egen aux1 = max(aux)
replace raza_ci=aux1 if (raza_ci ==. & relacion_ci ==3)  
replace raza_ci=3 if raza_ci==. 
drop aux aux1
label define raza_ci 1 "Indígena" 2 "Afro-descendiente" 3 "Otros"
label value raza_ci raza_ci 
label value raza_ci raza_ci
label var raza_ci "Raza o etnia del individuo" 
*/

gen raza_idioma_ci=.
replace raza_idioma_ci= 1 if  educa_112 ==2 |  educa_112 ==3 |  educa_112 ==4 |  educa_112 ==5 
replace raza_idioma_ci= 3 if  educa_112 ==1 | educa_112 ==6 

by idh_ch, sort: gen aux=raza_idioma_ci if datge_102==1
by idh_ch, sort: egen aux1 = max(aux)
replace raza_idioma_ci=aux1 if (raza_idioma_ci ==. & (datge_102 ==3|datge_102==5))  
replace raza_idioma_ci=3 if raza_idioma_ci==. 

label define raza_idioma_ci 1 "Indígena" 2 "Afro-descendiente" 3 "Otros" 
label value raza_idioma_ci raza_idioma_ci 
label value raza_idioma_ci raza_idioma_ci
label var raza_idioma_ci "Raza o etnia del individuo" 

gen id_ind_ci = 0
replace id_ind_ci=1 if raza_idioma_ci==1
label define id_ind_ci 1 "Indígena" 0 "Otros" 
label value id_ind_ci id_ind_ci 
label var id_ind_ci "Indigena" 

gen raza_ci=.

gen id_afro_ci = 0
label define id_afro_ci 1 "Afro-descendiente" 0 "Otros" 
label value id_afro_ci id_afro_ci 
label var id_afro_ci "Afro-descendiente" 

drop aux aux1

************************************
*** VARIABLES DEL MERCADO LABORAL***
************************************
/* Esta sección es para los residentes habituales del hogar mayores a 6 años
OJO QUE EN LAS EMCUESTAS SIGUIENTES LA MUESTRA ES MAYORES DE 7 AÑOS*/ 
/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/
*********
*lp_ci***
*********
/* Esta sección es para los residentes habituales del hogar mayores a 10 años*/ 

gen lp_ci =.

replace lp_ci= 153.631216556719 if carat_10== 1 /*Sucre*/
replace lp_ci= 152.910410018994 if carat_10==2  & carat_100==1     /*La Paz*/
replace lp_ci= 160.814660266467 if carat_10== 3            /*Cochabamba*/
replace lp_ci= 139.04731768338 if carat_10== 4           /*Oruro*/
replace lp_ci= 127.884960070308 if carat_10==5          /* Potosí*/
replace lp_ci= 163.439634810542 if carat_10== 6        /*Tarija*/
replace lp_ci= 160.198019406275  if carat_10== 7               /*Santa Cruz de la Sierra*/
replace lp_ci= 160.198019406275 if carat_10==    8            /*Trinidad*/
replace lp_ci= 131.700947703296 if carat_10== 2         & carat_100==2         /*El Alto*/
replace lp_ci= 160.198019406275 if carat_10==9          /*Cobija*/


label var lp_ci "Linea de pobreza oficial del pais"

*********
*lpe_ci***
*********

gen lpe_ci =.

replace lpe_ci= 77.5837643611432 if carat_10== 1 /*Sucre*/
replace lpe_ci= 85.0181879705609 if carat_10==2  & carat_100==1     /*La Paz*/
replace lpe_ci= 81.2114034345656 if carat_10== 3            /*Cochabamba*/
replace lpe_ci= 77.3103086319593 if carat_10== 4           /*Oruro*/
replace lpe_ci= 71.1040377990914 if carat_10==5          /* Potosí*/
replace lpe_ci= 82.5370155793239 if carat_10== 6        /*Tarija*/
replace lpe_ci= 81.3805938583876  if carat_10== 7               /*Santa Cruz de la Sierra*/
replace lpe_ci= 81.3805938583876 if carat_10==    8            /*Trinidad*/
replace lpe_ci= 79.9424752559005 if carat_10== 2         & carat_100==2         /*El Alto*/
replace lpe_ci= 81.3805938583876 if  carat_10==9  /*Cobija*/


label var lpe_ci "Linea de indigencia oficial del pais"


*************
**salmm_ci***
*************

*BOL 1990
gen salmm_ci= 77.28
* Actualizado por LCM dic 2013
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
replace condocup_ci=1 if ocup1_132==1  | ocup1_134==1 
replace condocup_ci=2 if ocup1_133==1
recode condocup_ci .=3 if edad_ci>=10
recode condocup_ci .=4 if edad_ci<10
label var condocup_ci "Condicion de ocupación de acuerdo a def de cada pais"
label define condocup_ci 1 "Ocupado" 2 "Desocupado" 3 "Inactivo" 4 "Menor que 10" 
label value condocup_ci condocup_ci
*/

* Homologacion toda la serie 05/22/2014 MGD
gen condocup_ci=.
replace condocup_ci=1 if ocup1_132==1  | ocup1_134==1 
replace condocup_ci=2 if ocup1_132==2 & ocup1_133==1
recode condocup_ci .=3 if edad_ci>=10
recode condocup_ci .=4 if edad_ci<10
label var condocup_ci "Condicion de ocupación de acuerdo a def de cada pais"
label define condocup_ci 1 "Ocupado" 2 "Desocupado" 3 "Inactivo" 4 "Menor que 10" 
label value condocup_ci condocup_ci

*************
*cesante_ci* 
*************
* Modificacion MGD 07/25//2014: se corrije la variable ya que se estaba generando con la pregunta para inactivos.
gen cesante_ci=1 if   desoc_166==2 & condocup_ci==2
replace cesante_ci=0 if   desoc_166==1 & condocup_ci==2
*replace cesante_ci=0 if   inact_178==2 & condocup_ci==3
*replace cesante_ci=0 if condocup_ci==3 & cesante_ci != 1

label var cesante_ci "Desocupado - definicion oficial del pais"	


*************
**pension_ci*
*************

gen aux_p=.
/*
gen pension_ci=1 if aux_p>0 & aux_p!=.
recode pension_ci .=0 
label var pension_ci "1=Recibe pension contributiva"
*/
*Modificación Mayra Sáenz - Septiembre 2014
gen pension_ci=.
*************
**ypen_ci*
*************

gen ypen_ci=aux_p 
recode ypen_ci .=0 
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

gen desalent_ci=(emp_ci==0 & ocup1_133==2 & (inact_185==1 | inact_185==2))
replace desalent_ci=. if emp_ci==.


*****************
***horaspri_ci***
*****************

gen horaspri_ci=cupro_154*cupro_155
replace horaspri_ci=. if emp_ci~=1 | cupro_154==99 

*****************
***horastot_ci***
*****************

gen horassec=aseta_161
replace horassec=. if emp_ci~=1 | aseta_161==99

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
replace categopri_ci=1 if ocup1_137==4 
replace categopri_ci=2 if ocup1_137==3 | ocup1_137==6
replace categopri_ci=3 if ocup1_137==1 | ocup1_137==2 | ocup1_137==5
replace categopri_ci=4 if ocup1_137==7
replace categopri_ci=. if emp_ci~=1


label define categopri_ci 1"Patron" 2"Cuenta propia" 
label define categopri_ci 3"Empleado" 4" No remunerado", add
label value categopri_ci categopri_ci
label variable categopri_ci "Categoria ocupacional trabajo principal"


******************
***categosec_ci***
******************

gen categosec_ci=.
replace categosec_ci=1 if aseta_159==4 
replace categosec_ci=2 if aseta_159==5 | aseta_159==7
replace categosec_ci=3 if aseta_159==1 | aseta_159==2 | aseta_159==3
replace categosec_ci=4 if aseta_159==6

label define categosec_ci 1"Patron" 2"Cuenta propia" 
label define categosec_ci 3"Empleado" 4" No remunerado", add
label value categosec_ci categosec_ci
label variable categosec_ci "Categoria ocupacional trabajo principal"

*****************
***nempleos_ci***
*****************

gen nempleos_ci=.
replace nempleos_ci=1 if emp_ci==1
replace nempleos_ci=2 if emp_ci==1 & aseta_157==1

/*
*****************
***firmapeq_ci***
*****************
/*No se le pregunta a los empleados públicos*/

gen firmapeq_ci=(ocup2_231==1 | cupro_233==1)
replace firmapeq_ci=. if ocup2_231==9 | emp_ci~=1 | cupro_233==9
*/
*****************
***spublico_ci***
*****************
/*Para los obreros y empleados*/

gen spublico_ci=(ocup2_138==1 | ocup2_138==2)
replace spublico_ci=. if emp_ci~=1 | ocup2_138==9


**************
***ocupa_ci***
**************

*Ver variable ocup1_135

gen ocupa_ci=.


*************
***rama_ci***
*************
* La variable estaba generada como missing; sin embargo, si existe la variable de rama de actividad para este anio. MGD
* para la clasificacion se utiliza la CIIU rev. 2 MGD 04/07/2014
gen rama_ci=.
replace rama_ci=1 if (ocup1_136>=11 & ocup1_136<=13)   & emp_ci==1
replace rama_ci=2 if (ocup1_136>=21 & ocup1_136<=29) & emp_ci==1
replace rama_ci=3 if (ocup1_136>=31 & ocup1_136<=39) & emp_ci==1
replace rama_ci=4 if (ocup1_136>=41 & ocup1_136<=42) & emp_ci==1
replace rama_ci=5 if (ocup1_136==50) & emp_ci==1
replace rama_ci=6 if (ocup1_136>=61 & ocup1_136<=63) & emp_ci==1
replace rama_ci=7 if (ocup1_136>=71 & ocup1_136<=72) & emp_ci==1
replace rama_ci=8 if (ocup1_136>=81 & ocup1_136<=83) & emp_ci==1
replace rama_ci=9 if (ocup1_136>=91 & ocup1_136<=99) & emp_ci==1


****************
***durades_ci***
****************
/*En meses*/

gen durades_ci=desoc_174/30      if desoc_239==1
replace durades_ci=desoc_174/4.3 if desoc_239==2
replace durades_ci=desoc_174     if desoc_239==3
replace durades_ci=desoc_174*12  if desoc_239==4
replace durades_ci=. if emp_ci~=0 | desoc_174==99 | desoc_239==9


*******************
***antiguedad_ci***
*******************
/*En años*/
/*Sólo para los obreros o empleados*/
*Como no es comparable se genera como missing
recode ocup2_141 (999=.)
gen antiguedad_ci=. 
replace antiguedad_ci=ocup2_141/(365) if ocup2_232==1 & emp_ci==1
replace antiguedad_ci=ocup2_141/12 if ocup2_232==2 & emp_ci==1
replace antiguedad_ci=ocup2_141 if ocup2_232==3 & emp_ci==1
*replace antiguedad_ci=. if emp_ci~=1 | ocup2_141==999 



*************
*tamemp_ci
*************
*1 "Menos de 5 personas" 2 "De 5 a 19 personas" 3 "20 y más personas"
/*gen tamemp_ci=    ocup2_140
replace tamemp_ci=. if  ocup2_140==999
*/
* Actualizado por LCM 

gen tamemp_ci=1 if      ocup2_140<5
replace tamemp_ci=2 if  ocup2_140>=5 & ocup2_140<=49
replace tamemp_ci=3 if  ocup2_140>=50 & ocup2_140<999
label define tamemp_ci 1 "Pequeña" 2 "Mediana" 3 "Grande"
label value tamemp_ci tamemp_ci
label var tamemp_ci "# empleados en la empresa"

*******************
***categoinac_ci***
*******************
gen categoinac_ci =1 if (ocup1_134==4 & condocup_ci==3)& pension_ci==1
replace categoinac_ci = 2 if  (ocup1_134==3 & condocup_ci==3)
replace categoinac_ci = 3 if  (ocup1_134==2 & condocup_ci==3)
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
/*No puedo crear esta variable porque no está la variable que
indica periodicidad*/

gen ylmpri_ci=.


*******************
*** nrylmpri_ci ***
*******************

gen nrylmpri_ci=.


******************
*** ylnmpri_ci ***
******************

gen ylnmpri_ci=.


***************
***ylmsec_ci***
***************

*Ingreso laboral monetario de la actividad secundaria

gen ylmsec_ci=.
replace ylmsec_ci=aseta_162*30  if aseta_237==1 
replace ylmsec_ci=aseta_162*4.3 if aseta_237==2 
replace ylmsec_ci=aseta_162     if aseta_237==4 
replace ylmsec_ci=0 if aseta_162==99998 
replace ylmsec_ci=0 if categosec_ci==4 
replace ylmsec_ci=. if aseta_162==99999
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

gen ylm_ci=.

*************
***ylnm_ci***
*************

gen ylnm_ci=.

*************
***ynlm_ci***
*************

*Ingresos no laborales monetarios (inactivos)

gen ynlm1=inlab_159

gen ynlm2=inlab_164/12

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

gen nrylmpri_ch=.


**************
*** ylm_ch ***
**************

gen ylm_ch=.


****************
*** ylmnr_ch ***
****************

gen ylmnr_ch=.


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

gen ylmhopri_ci=.


***************
***ylmho_ci ***
***************

gen ylmho_ci=.



****************************
***VARIABLES DE EDUCACION***
****************************

/*En esta sección es sólo para los mayores a los 6 años de edad*/

gen byte curso=real(substr(string(educa_114),2,1))
gen byte nivel=real(substr(string(educa_114),1,1))

gen byte aedu_ci=.

replace aedu_ci=0 if educa_114==0 | educa_114==10

replace aedu_ci=1 if nivel==1 & curso==1
replace aedu_ci=2 if nivel==1 & curso==2
replace aedu_ci=3 if nivel==1 & curso==3
replace aedu_ci=4 if nivel==1 & curso==4
replace aedu_ci=5 if nivel==1 & curso==5

replace aedu_ci=6 if nivel==2 & curso==1
replace aedu_ci=7 if nivel==2 & curso==2
replace aedu_ci=8 if nivel==2 & curso==3

replace aedu_ci=9 if  (nivel==3 | nivel==4) & curso==1
replace aedu_ci=10 if (nivel==3 | nivel==4) & curso==2
replace aedu_ci=11 if nivel==3  & curso==3
replace aedu_ci=12 if nivel==3 & curso==4 
replace aedu_ci=12 if nivel==4 & curso==3 

replace aedu_ci=13 if (nivel>=5 & nivel<=7) & curso==1 
replace aedu_ci=14 if (nivel>=5 & nivel<=7) & curso==2 
replace aedu_ci=16 if (nivel==5| nivel==6) & curso==3
replace aedu_ci=17 if (nivel==7) & curso==3


/* En esta encuesta el códido curso==3 a partir del nivel tecnico medio corresponde al nivel
completo, eso hace que los años de educación presenten un quiebre*/

**************
***eduno_ci***
**************

gen byte eduno_ci=(educa_114==0 | educa_114==10) 
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
replace eduac_ci=0 if (nivel==6 | nivel==5)
label variable eduac_ci "Superior universitario vs superior no universitario"

***************
***asiste_ci***
***************

gen asiste_ci=(educa_115==1)
label variable asiste_ci "Asiste actualmente a la escuela"

***************
***asispre_ci***
***************

*Variable añadida por Ángela López - 31/08/2018
g asispre_ci=.	
la var asispre_ci "Asiste a educacion prescolar"

**************
***pqnoasis***
**************

gen pqnoasis_ci=educa_116 if asiste_ci==0

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
replace tecnica_ci=1 if nivel==5
recode tecnica_ci .=0 
label var tecnica_ci "1=formacion terciaria tecnica"


**********************************
**** VARIABLES DE LA VIVIENDA ****
**********************************
/*La encuesta no tiene módulo de vivienda*/

gen aguared_ch=(vivi2_147>=1 & vivi2_147<=4)
replace aguared_ch=. if vivi2_147==. 

gen aguadist_ch=1 if vivi2_147==1 | vivi2_147==4 
replace aguadist_ch=2 if vivi2_147==2 | vivi2_147==5
replace aguadist_ch=3 if (vivi2_147>=6 & vivi2_147<=9) | vivi2_147==3
/*Si bien en este año hay una variable que nos indica
exactamente la distancia de la fuente de agua, a los efectos de
hacer esta variable comparable con los demás años, la definimos
de esta manera*/

*Inclusión Mayra Sáenz Julio 2013
gen aguamala_ch=(vivi2_147==7)
replace aguamala_ch=. if vivi2_147==.
label var aguamala_ch "Agua unimproved según MDG"
/*NA*/

gen aguamide_ch=.
/*NA*/

gen luz_ch=.
gen luzmide_ch=.
/*NA*/

gen combust_ch=.
/*NA*/

gen bano_ch=(vivi2_150==1)
replace bano_ch=. if vivi2_150==.

gen banoex_ch=(vivi2_151==1)
replace banoex_ch=. if vivi2_151==. 


gen des1_ch=.

gen des2_ch=.
replace des2_ch=0 if vivi2_149==4
replace des2_ch=1 if vivi2_149==1 | vivi2_149==2
replace des2_ch=2 if vivi2_149==3


gen piso_ch=0 if vivi1_135==5
replace piso_ch=1 if vivi1_135>=1 & vivi1_135<=4
replace piso_ch=2 if vivi1_135==6

gen techo_ch=0 if vivi1_133==4
replace techo_ch=1 if (vivi1_133>=1 & vivi1_133<=3) | vivi1_133==5
replace techo_ch=2 if vivi1_133==6

gen pared_ch=0 if vivi1_134==2 | vivi1_134==3 | vivi1_134==6
replace pared_ch=1 if vivi1_134==1 | vivi1_134==4 | vivi1_134==5
replace pared_ch=2 if vivi1_134==7

gen resid_ch=.
/*NA*/

**Daniela Zuluaga- Enero 2018: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**
	
*********************
***aguamejorada_ch***
*********************
gen aguamejorada_ch =.

		
*********************
***banomejorado_ch***
*********************
gen banomejorado_ch =.

gen dorm_ch=vivi1_137
recode dorm_ch (0=1)


gen cuartos_ch=vivi1_136

gen cocina_ch=(vivi1_138==1)
replace cocina_ch=. if vivi1_138==.

gen telef_ch=.
gen cel_ch=.
gen refrig_ch=.
gen freez_ch=.
gen auto_ch=.
gen compu_ch=.
gen internet_ch=.
/*NA*/


gen vivi1_ch=.
replace vivi1_ch=1 if vivi1_132==1 | vivi1_132==4
replace vivi1_ch=2 if vivi1_132==2
replace vivi1_ch=3 if vivi1_132==3 | vivi1_132==5 | vivi1_132==6

gen vivi2_ch=(vivi1_132==1 | vivi1_132==2)
replace vivi2_ch=. if vivi1_132==.

gen viviprop_ch=.
replace viviprop_ch=0 if vivi1_140==5
replace viviprop_ch=1 if vivi1_140==1
replace viviprop_ch=3 if vivi1_140>=2 & vivi1_140<=6 & vivi1_140~=5

gen vivitit_ch=.
/*NA*/

gen vivialq_ch=vivi1_141
replace vivialq_ch=. if vivi1_141==99999

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
rename ocup1_135 codocupa
rename ocup1_136 codindustria

compress


saveold "`base_out'", replace


log close




