
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

local PAIS BLZ
local ENCUESTA LFS
local ANO "2005"
local ronda a

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
   



capture log close
log using "`log_file'", replace 

*log off
/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Belize
Encuesta: LFS
Round: Octubre
Autores: 
Modificación 2014: Melany Gualavisi melanyg@iadb.org
Versión 2012: Guillermo Marroquin
Fecha última modificación: Septiembre 2014

							SCL/LMK - IADB
****************************************************************************/
/***************************************************************************
Detalle de procesamientos o modificaciones anteriores:

****************************************************************************/


use `base_in', clear

**********************
* AÑO DE LA ENCUESTA *
**********************
gen anio_c=2005
label variable anio_c "Año de la Encuesta"

*************************
* FACTORES DE EXPANSION *
*************************
gen factor_ch=weight
label var factor_ch "Factor de Expansion del Hogar"
gen factor_ci=weight
label var factor_ci "Factor de Expansion del Individuo"

**************
* REGION BID *
**************
gen region_BID_c=1
label var region_BID_c "Region BID"
label define region_BID 1"Centroamérica" 2"Caribe" 3"Andinos" 4"Cono Sur"
label values region_BID_c region_BID

***************
* REGION PAIS *
***************
g region_c=.
gen ine01= district


***************
*    ZONA     *
***************
gen byte zona_c=1 if urbrur==1 | urbrur==2 | urbrur==3 | urbrur==4 /* Urbana */
replace zona_c=0 if urbrur==5 /* Rural */
label variable zona_c "Zona geográfica"
label define zona_c 0 "Rural" 1 "Urbana"
label value zona_c zona_c

***********
*  PAIS   *
***********
gen pais_c="BLZ"
label var pais_c "Acrónimo del país"

******************************
*  IDENTIFICADOR DEL HOGAR   *
******************************
egen idh_ch=group(district urbrur ednumber hhnumber week)
label var idh_ch "Identificador Unico del Hogar"

*******************************
* IDENTIFICADOR DEL INDIVIDUO *
*******************************
gen idp_ci=person
label var idp_ci "Identificador Individual dentro del Hogar"

************************************
*  RELACION CON EL JEFE DE HOGAR   *
************************************
gen relacion_ci=1 if relation==1
replace relacion_ci=2 if relation==2
replace relacion_ci=3 if relation==3
replace relacion_ci=4 if relation==4 | relation==5 | relation==6 | relation==7
replace relacion_ci=5 if relation==8
replace relacion_ci=. if relation==9 /* No sabe */
label var relacion_ci "relación con el jefe de hogar"
label define relacion 1"Jefe" 2"Cónguye, Esposo/a, Compañero/a" 3"Hijo/a" 4"Otros parientes" 5"Otros no parientes" 6"Servicio doméstico" 
label values relacion_ci relacion

************************************
* DUMMY PARA NO MIEMBROS DEL HOGAR *
************************************
gen miembros_ci=0
replace miembros_ci=1 if (relacion_ci>=1 & relacion_ci<=4)
label variable miembros_ci "Variable dummy que indica las personas que son miembros del Hogar"



*******************************
*******************************
*******************************
*   VARIABLES DEMOGRÁFICAS    *
*******************************
*******************************
*******************************

***********
*  SEXO   *
***********
gen sexo_ci=sex
label var sexo_ci "sexo del individuo"
label define sexo 1"Masculino" 2"Femenino" 
label values sexo_ci sexo

***********
*  EDAD   *
***********
gen edad_ci=age
label var edad_ci "edad del individuo"

			
*******************************************************
***           VARIABLES DE DIVERSIDAD               ***
*******************************************************				
* Maria Antonella Pereira & Nathalia Maya - Marzo 2021	

			
	***************
	***afroind_ci***
	***************
gen afroind_ci=. 

	***************
	***afroind_ch***
	***************
gen afroind_ch=. 

	*******************
	***afroind_ano_c***
	*******************
gen afroind_ano_c=.		

	*******************
	***dis_ci***
	*******************
gen dis_ci=. 

	*******************
	***dis_ch***
	*******************
gen dis_ch=. 


*******************
*  ESTADO CIVIL   *
*******************
* No hay la pregunta. MGD 08/27/2014
gen civil_ci=.
label var civil_ci "Estado civil del individuo"
label define civil 1"Soltero" 2"Unión formal o informal" 3"Divorciado o separado" 4"Viudo" 
label values civil_ci civil

*******************
*  JEFE DE HOGAR  *
*******************
gen jefe_ci=0
replace jefe_ci=1 if relacion_ci==1
label var jefe_ci "Jefe de hogar"
label define jefe 1"Jefe de Hogar" 2"Otro" 
label values jefe_ci jefe

************************************
*  NUMERO DE CONYUGES EN EL HOGAR  *
************************************
egen nconyuges_ch=sum(relacion_ci==2), by (idh_ch)
label var nconyuges_ch "Número de Conyuges en el hogar"

************************************
*  NUMERO DE HIJOS EN EL HOGAR  *
************************************
egen nhijos_ch=sum(relacion_ci==3), by (idh_ch)
label var nhijos_ch "Número de hijos en el hogar"

*******************************************
*  NUMERO DE OTROS PARIENTES EN EL HOGAR  *
*******************************************
egen notropari_ch=sum(relacion_ci==4), by (idh_ch)
label var notropari_ch "Número de otros parientes en el hogar"

*******************************************
*  NUMERO DE OTROS NO PARIENTES EN EL HOGAR  *
*******************************************
egen notronopari_ch=sum(relacion_ci==5), by (idh_ch)
label var notronopari_ch "Número de otros parientes en el hogar"

*************************************
*  NUMERO DE EMPLEADOS EN EL HOGAR  *
*************************************
egen nempdom_ch=sum(relacion_ci==6), by (idh_ch)
label var nempdom_ch "Número de empleados en el hogar"

*********************
*  CLASE DE HOGAR   *
*********************
gen clasehog_ch=.
replace clasehog_ch=1 if nhijos_ch==0 & nconyuges_ch==0 & notropari_ch==0 & notronopari_ch==0 /* unipersonal*/
replace clasehog_ch=2 if nhijos_ch>0 & nhijos_ch!=. & notropari_ch==0 & notronopari_ch==0 /* nuclear (child with or without spouse but without other relatives)*/
replace clasehog_ch=2 if nconyuges_ch>0 & nconyuges_ch!=. & notropari_ch==0 & notronopari_ch==0 /* nuclear (spouse with or without children but without other relatives)*/
replace clasehog_ch=3 if notropari_ch>0 & notropari_ch!=. & notronopari_ch==0 /* ampliado*/
replace clasehog_ch=4 if nhijos_ch>0 & nhijos_ch!=. & notropari_ch==0 & notronopari_ch>0 & notronopari_ch!=. /* nuclear (child with or without spouse but without other relatives)*/
replace clasehog_ch=4 if nconyuges_ch>0 & nconyuges_ch!=. & notropari_ch==0 & notronopari_ch>0 & notronopari_ch!=. /* nuclear (spouse with or without children but without other relatives)*/
replace clasehog_ch=4 if notropari_ch>0 & notropari_ch!=. & notronopari_ch>0 & notronopari_ch!=. /* ampliado*/
*replace clasehog_ch=4 if nconyuges_ch>0 | nhijos_ch>0 | (notropari_ch>0 & notropari_ch<.)) & (notronopari_ch>0 & notronopari_ch<.) /* compuesto  (some relatives plus non relative)*/
replace clasehog_ch=5 if nhijos_ch==0 & nconyuges_ch==0 & notropari_ch==0 & notronopari_ch>0 & notronopari_ch!=./** corresidente*/
label var clasehog_ch "Clase de hogar"
label define clasehog 1"Unipersonal" 2"Nuclear" 3"Ampliado" 4"Compuesto" 5"Corresidente" 
label values clasehog_ch clasehog


*************************************
*  NUMERO DE MIEMBROS EN EL HOGAR  *
*************************************
egen nmiembros_ch=sum(relacion_ci>0 & relacion_ci<5), by (idh_ch)
label variable nmiembros_ch "Numero de miembros en el Hogar"

**************************
*  MIEMBROS EN EL HOGAR  *
**************************
g miembros_ch=0
replace miembros_ch=1 if relacion_ci>=1 & relacion_ci<=4
label var miembros_ch "Miembros en el hogar"
label define miembros 1"Miembro" 2"No miembro"  
label values miembros_ch miembros

********************************************
*  MIEMBROS EN EL HOGAR MAYORES DE 21 AÑOS *
********************************************
egen nmayor21_ch=sum((relacion_ci>0 & relacion_ci<5) & (edad_ci>=21)), by (idh_ch)
label variable nmayor21_ch "Numero de personas de 21 años o mas dentro del Hogar"

********************************************
*  MIEMBROS EN EL HOGAR MENORES DE 21 AÑOS *
********************************************
egen nmenor21_ch=sum((relacion_ci>0 & relacion_ci<5) & (edad_ci<21)), by (idh_ch)
label variable nmenor21_ch "Numero de personas menores a 21 años dentro del Hogar"

********************************************
*  MIEMBROS EN EL HOGAR MAYORES DE 65 AÑOS *
********************************************
egen nmayor65_ch=sum((relacion_ci>0 & relacion_ci<5) & (edad_ci>=65)), by (idh_ch)
label variable nmayor65_ch "Numero de personas de 65 años o mas dentro del Hogar"

********************************************
*  MIEMBROS EN EL HOGAR MENORES DE 65 AÑOS *
********************************************
/*
egen nmenor65_ch=sum((relacion_ci>0 & relacion_ci<5) & (edad_ci<=65)), by (idh_ch)
label variable nmenor65_ch "Miembros de 65 años o menos dentro del Hogar"
*/
********************************************
*  MIEMBROS EN EL HOGAR MENORES DE 6 AÑOS *
********************************************
egen nmenor6_ch=sum((relacion_ci>0 & relacion_ci<5) & (edad_ci<6)), by (idh_ch)
label variable nmenor6_ch "Miembros menores a 6 años dentro del Hogar"

******************************************
*  MIEMBROS EN EL HOGAR MENORES DE 1 AÑO *
******************************************
egen nmenor1_ch=sum((relacion_ci>0 & relacion_ci<5) & (edad_ci<1)),  by (idh_ch)
label variable nmenor1_ch "Miembros menores a 1 año dentro del Hogar"





*******************************
*******************************
*******************************
*     VARIABLES LABORALES     *
*******************************
*******************************
*******************************


**************************
* CONDICION DE OCUPACION *
**************************
*Ocupado*
gen condocup_ci=.
replace condocup_ci=1 if econact==1 | othecon==1 | (tempabs==1 & whyabs<=2 & whyabs!=. & returnwk==1) | (tempabs==1 & whyabs>=3 & whyabs<=8)
*Desocupado*
replace condocup_ci=2 if condocup_ci!=1 & ((looked==1 | (wantwork<=8 & wantwork!=.) | applyjob==1 | friends==1 | newspape==1 | labdept==1 | makearrg==1 | other==1) & availabl==1)
*Inactivo*
replace condocup_ci=3 if (condocup_ci~=1 & condocup_ci~=2) & edad_ci>=14
*menores que PET
recode condocup_ci (.=4) if edad<14
label define condocup_ci 1"Ocupado" 2"Desocupado" 3"Inactivo" 4"Menores de 14 años"
label values condocup_ci condocup
label var condocup_ci "Condición de ocupación"



**************************
* CATEGORIA DE INACTIVIDAD  *
**************************
/*
*Jubilados, pensionados
gen categoinac_ci=1 if whynot==5 & condocup_ci==3
label var  categoinac_ci "Condición de Inactividad" 
*Estudiantes
replace categoinac_ci=2 if whynot==1 & condocup_ci==3
*Quehaceres del Hogar
replace categoinac_ci=3 if whynot==4 & condocup_ci==3
*Otra razon
replace categoinac_ci=4 if (whynot==2 | whynot==3 |whynot==6) & condocup_ci==3
label define inactivo 1"Hogar o Pensionado" 2"Estudiante" 3"Hogar" 4"Otros"
label values categoinac_ci inactivo
*/

* MGD 09*03/2014: se consideran otras variables que indican categorias de inactividad.
gen categoinac_ci=.
label var  categoinac_ci "Condición de Inactividad" 
*Jubilados, pensionados
replace categoinac_ci=1 if (whynot==5 | reason==4) & condocup_ci==3
*Estudiantes
replace categoinac_ci=2 if (whynot==1 | usactv==1 | reason==3) & condocup_ci==3
*Quehaceres del Hogar
replace categoinac_ci=3 if (whynot==4 | usactv==2)& condocup_ci==3
*Otra razon
recode categoinac_ci (.=4) if categoinac_ci!=1 & categoinac_ci!=2 & categoinac_ci!=3 & condocup_ci==3 & whynot!=.
label define inactivo 1"Jubilados o Pensionado" 2"Estudiante" 3"Hogar" 4"Otros"
label values categoinac_ci inactivo

**********************
*  NÚMERO DE EMPLEOS *
**********************
gen nempleos_ci=.
replace nempleos_ci=1 if multijob==2
replace nempleos_ci=2 if multijob==1
label var nempleos_ci "Numero de empleos"
label define nempleos_ci 1 "un trabajo" 2 "dos o mas trabajos"
label values nempleos_ci nempleos_ci

************
* OCUPADO  *
************
gen emp_ci=0 
replace emp_ci=1 if condocup_ci==1
label var emp_ci "Ocupado"
label define ocupado 1"Ocupado" 0"No ocupado"  
label values emp_ci ocupado


*****************************************
* ANTIGUEDAD EN LA ACTIVIDAD PRINCIPAL  *
*****************************************
gen antiguedad_ci=yrsmain if yrsmain<99 & emp_ci==1
label var antiguedad_ci "Años de trabajo en la actividad principal"

***************
* DESOCUPADO  *
***************
gen desemp_ci=0 
replace desemp_ci=1 if condocup_ci==2
label var emp_ci "Desocupado"
label define desocupado 1"Desocupado" 0"No desocupado"  
label values desemp_ci desocupado

***********
* CESANTE *
***********
gen cesante_ci=0 if condocup_ci==2
replace cesante_ci=1 if everwork==1 & condocup_ci==2
label var cesante_ci "Cesante"

***********************************
* DURACION DEL DESEMPLEO EN MESES *
***********************************
*La varible esta definida por intervalos de tiempo. Se uso el punto medio del intervalo
gen durades_ci=.
replace durades_ci=0.5 if condocup_ci==2 & wowork==1
replace durades_ci=2 if condocup_ci==2 & wowork==2
replace durades_ci=5 if condocup_ci==2 & wowork==3
replace durades_ci=10 if condocup_ci==2 & wowork==4
replace durades_ci=12 if condocup_ci==2 & wowork==5
label var durades_ci "Duración de desempleo o búsqueda de empleo"


***********************************
* POBLACION ECONOMICAMENTE ACTIVA *
***********************************
gen pea_ci=0
replace pea_ci=1 if condocup_ci==1 | condocup_ci==2
label var pea_ci "Población económicamente activa"

****************
* DESALENTADOS *
****************
/*
gen desalent_ci1=.
replace desalent_ci1=0 if condocup_ci==2 | condocup_ci==1 | reason<99
replace desalent_ci1=1 if reason==5 | reason==6 | reason==8
label var desalent_ci "Trabajadores desalentados, personas que creen que por alguna razon no conseguiran trabajo" 
*/
* MGD 09/03/2014: calculo desgun la definicion del documento metodologico.
gen desalent_ci=0 if condocup_ci==3
replace desalent_ci=1 if (reason>=10 & reason<=14) & condocup_ci==3


*****************************
* TRABAJA MENOS DE 30 HORAS *
*****************************

* MGD 08/29/2014: no hay la pregunta de si desea trabajar mas horas, pero se utiliza disponibilidad para otro trabajo.
gen subemp_ci=0 
replace subemp_ci=1 if uslmain<=30 & addwork==1 & condocup_ci==1
label var subemp_ci "Trabaja menos de 30 horas"


****************************************************
* TRABAJA MENOS DE 30 HORAS Y NO DESEA TRABAJAR MAS*
****************************************************
* MGD 08/29/2014: no hay la pregunta de si desea trabajar mas horas, pero se utiliza disponibilidad para otro trabajo.
gen tiempoparc_ci=0 
replace tiempoparc=1 if uslmain<=30 & addwork==2 & condocup_ci==1
label var tiempoparc_ci "Trabaja menos de 30 horas y no desea trabajar más"

*********************************
* CATEGORIA OCUPACION PRINCIPAL *
*********************************
gen categopri_ci=.
replace categopri_ci=1 if empmain==1 & condocup_ci==1
replace categopri_ci=2 if empmain==2 & condocup_ci==1
replace categopri_ci=3 if (empmain==3 | empmain==4 | empmain==5) & condocup_ci==1
replace categopri_ci=4 if empmain==6 & condocup_ci==1
label var categopri_ci "Categoría ocupación principal"
label define categopri 1"Patrón o empleador" 2"Cuenta propia o independiente" 3"Empleado o asalariado" 4"Trabajador no remunerado"  
label values categopri_ci categopri

*********************************
* CATEGORIA OCUPACION SECUNDARIA*
*********************************
gen categosec_ci=.
replace categosec_ci=1 if empother==1 & condocup_ci==1
replace categosec_ci=2 if empother==2 & condocup_ci==1
replace categosec_ci=3 if (empother==3 | empother==4 | empother==5) & condocup_ci==1
replace categosec_ci=4 if empother==6 & condocup_ci==1
label var categosec_ci "Categoría ocupación secundaria"
label define categosec 1"Patrón o empleador" 2"Cuenta propia o independiente" 3"Empleado o asalariado" 4"Trabajador no remunerado"  
label values categosec_ci categosec

*********************************
*  RAMA DE ACTIVIDAD PRINCIPAL  *
*********************************
*** MGD 08/29/2014: CIIU REV. 3

gen rama_ci=.
replace rama_ci=1 if (indmisic>=0 & indmisic<=502) & emp_ci==1 /* indmisic for 2004 */
replace rama_ci=2 if (indmisic>=1000 & indmisic<=1421) & emp_ci==1
replace rama_ci=3 if (indmisic>=1500 & indmisic<=3799) & emp_ci==1
replace rama_ci=4 if (indmisic>=4010 & indmisic<=4100) & emp_ci==1
replace rama_ci=5 if (indmisic>=4500 & indmisic<=4599) & emp_ci==1
replace rama_ci=6 if (indmisic>=5000 & indmisic<=5599) & emp_ci==1
replace rama_ci=7 if (indmisic>=6000 & indmisic<=6420) & emp_ci==1
replace rama_ci=8 if (indmisic>=6500 & indmisic<=7499) & emp_ci==1
replace rama_ci=9 if (indmisic>=7500 & indmisic<=9900) & emp_ci==1
label define rama 1"Agricultura, caza, silvicultura o pesca" 2"Minas y Canteras" 3"Manufactura" 4"Electricidad, gas o agua" 5"Construcción" 6"Comercio al por mayor, restaurantes o hoteles" 7"Transporte o almacenamiento" 8"Establecimientos financieros, seguros o bienes inmuebles" 9"Servicios sociales, comunales o personales" 
label values rama_ci rama


*********************************
*  TRABAJA EN EL SECTOR PUBLICO *
*********************************
gen spublico_ci=0 
replace spublico_ci=1 if (empmain==3 | empmain==4) & condocup_ci==1
label var spublico_ci "Personas que trabajan en el sector publico"

********************
* TAMAÑO DE EMPRESA*
********************
* MGD 08/29/2014: esta variable tiene demasiados valores missing reportados; por lo que no es confiable el indicador.
g tamemp_ci=.
/*
gen tamemp_ci=1 if yearly==1
label var  tamemp_ci "Tamaño de Empresa" 
*Empresas medianas
replace tamemp_ci=2 if yearly==2 | yearly==3
*Empresas grandes
replace tamemp_ci=3 if yearly==4
label define tamaño 1"Pequeña" 2"Mediana" 3"Grande"
label values tamemp_ci tamaño
*/

*********************************
*  COTIZA A LA SEGURIDAD SOCIAL *
*********************************
gen cotizando_ci=.
label var cotizando_ci "Cotizando a la seguridad social"

****************************************************
*  INSTITUCION DE SEGURIDAD SOCIAL A LA QUE COTIZA *
****************************************************
gen inscot_ci=.
label var inscot_ci "Institución de seguridad social a la que cotiza"

**********************************
* AFILIADO A LA SEGURIDAD SOCIAL *
**********************************
gen afiliado_ci=.
*replace afiliado_ci=1 if 
label var afiliado_ci "Afiliado a la seguridad social"

*********************
* TRABAJADOR FORMAL *
*********************
gen formal_ci=.
replace formal_ci=1 if condocup_ci==1 & afiliado_ci==0 & cotizando_ci==0
replace formal_ci=1 if condocup_ci==1 & (afiliado_ci==1 | cotizando_ci==1)
label var afiliado_ci "Afiliado a la seguridad social"

********************
* TIPO DE CONTRATO *
********************
gen tipocontrato_ci=. 
label var tipocontrato_ci "Tipo de contrato"
label define tipocontrato 1"Permanente / Indefinido" 2"Temporal / Tiempo definido" 3"Sin contrato / Verbal"  
label values tipocontrato_ci tipocontrato

*****************************
* TIPO DE OCUPACION LABORAL *
*****************************
* MGD 09/02/2014: CIUO-88
gen ocupa_ci=.
replace ocupa_ci=1 if (occmain>=2000 & occmain<=3999) & emp_ci==1
replace ocupa_ci=2 if (occmain>=1000 & occmain<=1999) & emp_ci==1
replace ocupa_ci=3 if (occmain>=4000 & occmain<=4999) & emp_ci==1
replace ocupa_ci=4 if ((occmain>=5200 & occmain<=5999) | (occmain>=9111 & occmain<=9113)) & emp_ci==1
replace ocupa_ci=5 if ((occmain>=5000 & occmain<=5199) | (occmain>=9120 & occmain<=9162)) & emp_ci==1
replace ocupa_ci=6 if ((occmain>=6000 & occmain<=6999) | (occmain>=9200 & occmain<=9213)) & emp_ci==1
replace ocupa_ci=7 if ((occmain>=7000 & occmain<=8999) | (occmain>=9300 & occmain<=9333))& emp_ci==1
replace ocupa_ci=8 if (occmain>=0 & occmain<=999)  & emp_ci==1
replace ocupa_ci=9 if (occmain==9119 | occmain==9999) & emp_ci==1
label var ocupa_ci "Tipo de ocupacion laboral"
label define ocupa 1"Profesional o técnico" 2"Director o funcionario superior" 3"Personal administrativo o nivel intermedio" 4"Comerciante o vendedor" 5"Trabajador en servicios" 6"Trabajador agrícola o afines" 7"Obrero no agrícola, conductores de máquinas y vehículos de transporte y similares" 8"Fuerzas armadas" 9"Otras ocupaciones no clasificadas"
label values ocupa_ci ocupa


**********************************************
* HORAS TRABAJADAS EN LA ACTIVIDAD PRINCIPAL *
**********************************************
gen horaspri_ci=.
replace horaspri_ci=uslmain if uslmain!=99
label var horaspri_ci "Horas trabajadas en la actividad principal"

**************************
* TOTAL HORAS TRABAJADAS *
**************************
gen horastot_ci=uslmain+uslother
replace horastot_ci=. if uslmain==. & uslother==.
label var horastot_ci "Total horas trabajadas"

***********************************************
* RECIBE PENSION O JUBILACION NO CONTRIBUTIVA *
***********************************************
gen pensionsub_ci=.
*replace pensionsub_ci=1 if
label var pensionsub_ci "Recibe pensión o ubilación NO contributiva"

********************************************
* RECIBE PENSION O JUBILACION CONTRIBUTIVA *
********************************************
gen pension_ci=.
*replace pension_ci=1 if
label var pension_ci "Recibe pensión o jubilación contributiva"

************************************************
*INSTITUCION QUE OTORGA LA PENSION O JUBILACION*
************************************************
gen instpen_ci=.
label var instpen_ci "Institución que otorga la pensión o jubilación"





*******************************
*******************************
*******************************
*     VARIABLES DE INGRESO    *
*******************************
*******************************
*******************************




*************************************
* DUMMIES DE INDIVIDUO Y HOGAR *
*************************************
*** Dummy Individual si no reporta el ingreso laboral monetario de la actividad principal
gen byte nrylmpri_ci=0
*replace nrylmpri_ci=1 if p51==9 | (p51==7 & empmain>0)
label var nrylmpri_ci "Identificador de No Respuesta del Ingreso Monetario de la Actividad Principal"

*** Dummy para el Hogar
capture drop nrylmpri_ch
sort idh
egen nrylmpri_ch=sum(nrylmpri_ci) if miembros_ci==1, by(idh_ch)
replace nrylmpri_ch=1 if nrylmpri_ch>1 & nrylmpri_ch~=. & miembros_ci==1 
label var nrylmpri_ch "Identificador de Hogares en donde alguno de los miembros No Responde el Ingreso Monetario de la Actividad Principal"


*************************************
* INGRESO MONETARIO MENSUAL LABORAL *
*************************************
/*gen ylmpri_ci=incgroup
label var ylmpri_ci "Monto mensual de ingreso laboral de la actividad principal"
*/
gen ylmpri_ci=.
replace ylmpri_ci=incoave*30 if payperd ==1
replace ylmpri_ci= incoave*4.3 if payperd ==2
replace ylmpri_ci= incoave*2 if payperd ==3
replace ylmpri_ci= incoave if payperd ==4 | payperd ==8
replace ylmpri_ci= incoave/12 if payperd ==5
replace ylmpri_ci= 0 if payperd ==6
*replace ylmpri_ci= cq142 if incoave==. & emp_ci==1 
label var ylmpri_ci "Monto mensual de ingreso laboral de la actividad principal"

*******************************
* INGRESO MENSUAL NO MONETARIO*
*******************************
gen ylnmpri_ci=.
label var ylnmpri_ci "Monto mensual de ingreso NO monetario de la actividad principal"

*************************************************
* INGRESO MONETARIO MENSUAL ACTIVIDAD SECUNDARIA*
*************************************************
gen ylmsec_ci=.
label var ylmsec_ci "Monto mensual de ingreso laboral de la actividad secundaria"

****************************************************
* INGRESO NO MONETARIO MENSUAL ACTIVIDAD SECUNDARIA*
****************************************************
gen ylnmsec_ci=.
label var ylnmsec_ci "Ingreso mensual laboral NO monetario de la actividad secundaria"

************************************
* INGRESO MENSUAL OTRAS ACTIVIDADES*
************************************
gen ylmotros_ci=.
label var ylmotros_ci "Ingreso mensual por otras actividades"

*************************************************
* INGRESO MENSUAL NO MONETARIO OTRAS ACTIVIDADES*
*************************************************
gen ylnmotros_ci=.
label var ylnmotros_ci "Ingreso mensual NO monetario por otras actividades"

************************************
* INGRESO MENSUAL TODAS ACTIVIDADES*
************************************
gen ylm_ci=ylmpri_ci

*************************************************
* INGRESO MENSUAL NO MONETARIO TODAS ACTIVIDADES*
*************************************************
gen ylnm_ci= ylnmpri_ci + ylnmsec_ci + ylnmotros_ci
label var ylnm_ci "Ingreso mensual NO monetario todas actividades"

*************************************************
* INGRESO MENSUAL NO LABORAL OTRAS ACTIVIDADES  *
*************************************************
gen ynlm_ci=. 
label var ylnm_ci "Ingreso mensual NO laboral otras actividades"

**************************************************************
* INGRESO MENSUAL NO LABORAL NO MONETARIO OTRAS ACTIVIDADES  *
**************************************************************
gen ynlnm_ci= .
label var ylnm_ci "Ingreso mensual NO laboral NO monetario otras actividades"

************************************
* INGRESO MENSUAL LABORAL DEL HOGAR*
************************************
egen ylm_ch=sum(ylm_ci) if miembros_ci==1, by(idh_ch)
label var ylm_ch "Ingreso Laboral Monetario del Hogar (Bruto)"

**************************************************
* INGRESO MENSUAL LABORAL NO MONETARIO DEL HOGAR *
**************************************************
egen ylnm_ch=sum(ylnm_ci) if miembros_ci==1, by(idh_ch)
label var ylnm_ch "Ingreso Laboral No Monetario del Hogar"

**************************************************************
* INGRESO MENSUAL NO LABORAL OTRAS ACTIVIDADES no respuesta  *
**************************************************************
egen ylmnr_ch=sum(ylm_ci) if miembros_ci==1 & nrylmpri_ch==0, by(idh_ch)
label var ylmnr_ch "Ingreso Laboral Monetario del Hogar, considera 'missing' la No Respuesta"

**************************************************
* INGRESO MENSUAL NO LABORAL MONETARIO DEL HOGAR *
**************************************************
egen ynlm_ch=sum(ynlm_ci) if miembros_ci==1, by(idh_ch)
label var ynlm_ch "Ingreso No Laboral Monetario del Hogar"

*****************************************************
* INGRESO MENSUAL NO LABORAL NO MONETARIO DEL HOGAR *
*****************************************************
egen ynlnm_ch=sum(ynlnm_ci) if miembros_ci==1, by(idh_ch)
label var ynlnm_ch "Ingreso No Laboral No Monetario del Hogar"

*****************************************************
* INGRESO LABORAL POR HORA EN LA ACTIVIDAD PRINCIPA *
*****************************************************
gen ylmhopri_ci=.
label var ylmhopri_ci "Salario horario monetario de la actividad principal"

*****************************************************
* INGRESO LABORAL POR HORA EN TODAS LAS ACTIVIDADES *
*****************************************************
gen ylmho_ci=.
label var ylmho_ci "Salario horario monetario de todas las actividades"

************************************************
* RENTA MENSUAL IMPUTADA DE LA VIVIENDA PROPIA *
************************************************
gen rentaimp_ch=.
label var rentaimp_ch "Renta imputada de la vivienda propia"

*********************************************************
* MONTO MENSUAL DE INGRESO POR AUTOCONSUMO DEL INDIVIDUO*
*********************************************************
gen autocons_ci=.
label var autocons_ci "Monto mensual de ingreso por autoconsumo individuo"

*****************************************************
* MONTO MENSUAL DE INGRESO POR AUTOCONSUMO DEL HOGAR*
*****************************************************
egen autocons_ch=sum(autocons_ci) if miembros_ci==1, by(idh_ch)
label var autocons_ch "Autoconsumo del Hogar"

***************************
* REMESES EN MONEDA LOCAL *
***************************
gen remesas_ci=.
label var remesas_ci "Remesas en moneda local"

************************************
* REMESES EN MONEDA LOCAL DEL HOGAR*
************************************
egen remesas_ch=sum(remesas_ci) if miembros_ci==1, by(idh_ch)
label var remesas_ch "Remesas en moneda local"

************************************
* INGRESO POR PENSION CONTRIBUTIVA *
************************************
gen ypen_ci=.
label var ypen_ci "Ingreso por pensionc contributiva"

***************************************
* INGRESO POR PENSION NO CONTRIBUTIVA *
***************************************
gen ypensub_ci=.
label var ypensub_ci "Ingreso por pensionc NO contributiva"

********************************
* SALARIO MINIMO MENSUAL LEGAL *
********************************
gen salmm_ci =.
label var salmm_ci "salario mínimo mensual legal"

****************************************
* LINEA DE POBREZA OFICIAL MONEDA LOCAL*
****************************************
gen lp_ci =.
label var lp_ci "Línea de pobreza oficial en moneda local"

************************************************
* LINEA DE POBREZA EXTREMA OFICIAL MONEDA LOCAL*
************************************************
gen lpe_ci =.
label var lpe_ci "Línea de pobreza extrema oficial en moneda local"




*******************************
*******************************
*******************************
*    VARIABLES DE EDUCACION   *
*******************************
*******************************
*******************************

******************************************
* NUMERO DE AÑOS DE EDUCACION CULMINADOS *
******************************************
* MGD 09/09/2014: Clasificacion de añ de educacion en BLZ.  La ultima categoria es mas de dos años de universidad y se asumen 16 completos
*Primary Primary School - 8 years
*Secondary CSEC (Caribbean Secondary Education Certificate) Examinations - 4 years
*Post-secondary CXC Caribbean Advanced Placement Examination (CAPE)- 2 years (para quienes no culminaron al secundaria)   
*Tertiary University            

gen aedu_ci =.
replace aedu_ci=yrscompl if yrscompl <=12 & yrscompl!=99
replace aedu_ci=12 if yrscompl==13 | yrscompl==14
replace aedu_ci=13 if yrscompl==15
replace aedu_ci=14 if yrscompl==16
replace aedu_ci=16 if yrscompl==17
label var aedu_ci "número de años de educación culminados"


******************************************
*  NO TIENE NINGUN NIVEL DE INSTRUCCION  *
******************************************
gen eduno_ci=.
replace eduno_ci=1 if yrscompl==0 
replace eduno_ci=0 if yrscompl>=1 & yrscompl!=99 
label var eduno_ci "No tiene ningún nivel de instrucción"

******************************************
* NO HA COMPLETADO LA EDUCACION PRIMARIA *
******************************************
gen edupi_ci=.
replace edupi_ci=1 if yrscompl<8
replace edupi_ci=0 if yrscompl>=8 & yrscompl!=99 
label var edupi_ci "No ha completado la educación primaria"


******************************************
*  HA COMPLETADO LA EDUCACION PRIMARIA   *
******************************************
gen edupc_ci=.
replace edupc_ci=1 if yrscompl>=8 & yrscompl!=99  
replace edupc_ci=0 if yrscompl<8 
label var edupc_ci "Ha completado la educación primaria"

******************************************
*NO HA COMPLETADO LA EDUCACION SECUNDARIA*
******************************************
gen edusi_ci=.
replace edusi_ci=1 if yrscompl<12
replace edusi_ci=0 if yrscompl>=12 & yrscompl!=99
label var edusi_ci "No ha completado la educación secundaria"

******************************************
* HA COMPLETADO LA EDUCACION SECUNDARIA  *
******************************************
gen edusc_ci =. 
replace edusc_ci=1 if yrscompl>=12 & yrscompl!=99
replace edusc_ci=0 if yrscompl<12
label var edusc_ci "Ha completado la educación secundaria"

*******************************************
* NO HA COMPLETADO LA EDUCACION TERCIARIA *
*******************************************
gen eduui_ci=. 
/*replace eduui_ci=1 if yrscompl<4
replace eduui_ci=0 if yrscompl>=4 & yrscompl<=6*/
label var eduui_ci "No ha completado la educación terciaria"

*******************************************
*  HA COMPLETADO LA EDUCACION TERCIARIA   *
*******************************************
gen eduuc_ci =.
/*replace edusc_ci=1 if yrscompl>=4 & yrscompl<=6
replace edusc_ci=0 if yrscompl<4*/
label var eduuc_ci "Ha completado la educación terciaria"

**************************************************
* NO HA COMPLETADO EL PRIMER CICLO DE SECUNDARIA *
**************************************************
gen edusli_ci =. 

label var edusli_ci "No ha completado el primer ciclo de la secundaria"

**************************************************
*  HA COMPLETADO EL PRIMER CICLO DE SECUNDARIA   *
**************************************************
gen eduslc_ci =. 

label var eduslc_ci "Ha completado el primer ciclo de la secundaria"

**************************************************
* NO HA COMPLETADO EL SEGUNDO CICLO DE SECUNDARIA *
**************************************************
gen edus2i_ci =.

label var edus2i_ci "No ha completado el segundo ciclo de la secundaria"

**************************************************
*  HA COMPLETADO EL SEGUNDO CICLO DE SECUNDARIA  *
**************************************************
gen edus2c_ci=. 

label var edus2c_ci "Ha completado el segundo ciclo de la secundaria"

****************************************
*  HA COMPLETADO EDUCACION PREESCOLAR  *
****************************************
gen edupre_ci=. 
replace edupre_ci=1 if yrscompl>=1
replace edupre_ci=0 if yrscompl==0 
label var edupre_ci "Ha completado educación preescolar"

************************************************
*  HA COMPLETADO EDUCACION TERCIARIA ACADEMICA *
************************************************
gen eduac_ci=.
label var eduac_ci "Ha completado educación terciaria académica"

************************************
*  ASISTE A UN CENTRO DE ENSEÑANZA *
************************************
gen asiste_ci=.
replace asiste_ci=1 if attdsch==1 | attdsch==2
replace asiste_ci=0 if attdsch==3
label var asiste_ci "Asiste a algún centro de enseñanza"

*********************************************
* PORQUE NO ASISTE A UN CENTRO DE ENSEÑANZA *
*********************************************
gen pqnoasis_ci=.
label var pqnoasis_ci "Porque no asiste a algún centro de enseñanza"
label define pqnoasis 1"Muy joven" 2"Razones financieras" 3"Trabaja en casa o negocio familiar" 4"Distancia a la escuela/transporte" 5"Enfermedad/inhabilidad" 6"falta de especio en la escuela" 7"Otra" 9"NS/NR"  
label values pqnoasis_ci pqnoasis

**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci**

******************
***pqnoasis1_ci***
******************

gen pqnoasis1_ci=.


************************************
*  HA REPETIDO ALGUN AÑO O GRADO   *
************************************
gen repite_ci=.
*replace repite_ci=1 if
label var repite_ci "Ha repetido algún año o grado"

******************************
*  HA REPETIDO EL ULTIMO AÑO *
******************************
gen repiteult_ci=.
*replace repiteult_ci=1 if
label var repiteult_ci "Ha repetido el último grado"

***************************************
*ASISTE A CENTRO DE ENSEÑANZA PUBLICA *
***************************************
gen edupub_ci=.
*replace edupub_ci=0 if
label var repiteult_ci "Asiste a centro de enseñanza pública"
label define edupub 1"Pública" 0"Privada"  
label values edupub_ci edupub

**************************
*  TIENE CARRERA TECNICA *
**************************
gen tecnica_ci=.
*replace tecnica_ci=1 if
label var tecnica_ci "Tiene carrera técnica"


*******************************
*******************************
*******************************
*    VARIABLES DE VIVIENDA    *
*******************************
*******************************
*******************************

*********************
gen aguared_ch=.
replace aguared_ch= 1 if water==1 | water==2 | pipewate==1 | stdpipe==1
replace aguared_ch= 0 if water>2 | hdpump==1 | covvat==1 | uncvat==1 | purifywa==1 | river==1 | otherfac==1 
*********************
gen aguafconsumo_ch = 0
replace aguafconsumo_ch= 1 if water==1 | water==2
replace aguafconsumo_ch= 3 if water==6
replace aguafconsumo_ch= 4 if water==4
replace aguafconsumo_ch= 7 if water==3
replace aguafconsumo_ch= 8 if water==7
replace aguafconsumo_ch= 9 if water==5
replace aguafconsumo_ch= 10 if water==8

*********************
gen aguafuente_ch =.
replace aguafuente_ch= 1 if water==1 | water==2 | pipewate==1 | stdpipe==1
replace aguafuente_ch= 3 if water==6 | purifywa==1
replace aguafuente_ch= 4 if water==4 | covvat==1
replace aguafuente_ch= 7 if water==3 | hdpump==1
replace aguafuente_ch= 8 if water==7 | river==1
replace aguafuente_ch= 9 if water==5 | uncvat==1
replace aaguafuente_ch= 10 if water==8 | otherfac==1

*********************

gen aguadist_ch=0

*********************
gen aguadisp1_ch = 9

*********************
gen aguadisp2_ch = 9

*********************
gen aguamala_ch = 2
replace aguamala_ch = 0 if aguafuente_ch<=7
replace aguamala_ch = 1 if aguafuente_ch>7 & aguafuente_ch!=10
*label var aguamala_ch "= 1 si la fuente de agua no es mejorada"
*********************


gen aguamejorada_ch = 2
replace aguamejorada_ch = 0 if aguafuente_ch>7 & aguafuente_ch!=10
replace aguamejorada_ch = 1 if aguafuente_ch<=7
*label var aguamejorada_ch "= 1 si la fuente de agua es mejorada"

*********************
gen aguamide_ch = .
*********************

gen bano_ch=.
replace bano_ch=0 if toilet==8
replace bano_ch=1 if toilet==1
replace bano_ch=2 if toilet==2
replace bano_ch=3 if toilet==3 | toilet==4 | toilet==5
replace bano_ch=5 if toilet==6
replace bano_ch=6 if toilet==7

*********************
generate banoex_ch=9

*********************
gen banomejorado_ch= 2
replace banomejorado_ch =1 if bano_ch<=3 & bano_ch!=0
replace banomejorado_ch =0 if (bano_ch ==0 | bano_ch>=4) & bano_ch!=6

*********************
gen sinbano_ch =0 if toilet!=8
replace sinbano_ch =3 if toilet==8

*********************
gen aguatrat_ch =9

*********************


*****************************
*  ILUMINACION ES ELÉCTRICA *
*****************************
gen luz_ch=0
replace luz_ch=1 if light==1 | light==2 | light==3
label var luz_ch "La iluminación del hogar es eléctrica"

************************
*  USA MEDIDOR DE LUZ  *
************************
gen luzmide_ch=.
*replace luzmide_ch=1 if
label var luzmide_ch "Usa medidor de luz para pagar por su consumo"

********************************************
*  USA COMBUSTIBLE COMO FUENTE DE ENERGIA  *
********************************************
gen combust_ch=0
replace combust_ch=1 if cooking==1 | cooking==4 
label var combust_ch "Usa combustible como fuente de energía"


*******************************************
*  TIPO DE DESAGÜE incluyendo Unimproved  *
*******************************************
* MGD 09/08/2014: revisar categorias con SCL.
gen des1_ch=.
replace des1_ch=0 if toilet==8
replace des1_ch=1 if toilet==1
replace des1_ch=2 if toilet==2
replace des1_ch=3 if toilet>=3 & toilet<=6
label var des1_ch "Tipo de desague incluyendo Unimproved"
label define des1 0"El hogar no tiene servicio higienico" 1"Desagüe conectado a la red general" 2"Desagüe conectado a un pozo o letrina" 3"El desagüe se comunica con la superficie"
label values des1_ch des1

*******************************************
* TIPO DE DESAGÜE sin incluir Unimproved  *
*******************************************
* MGD 09/08/2014: revisar categorias con SCL.
gen des2_ch=.
replace des1_ch=0 if toilet==8
replace des1_ch=1 if toilet==1
replace des1_ch=2 if toilet>=2 & toilet<=7
label var des2_ch "Tipo de desague sin incluir Unimproved"
label define des2 0"El hogar no tiene servicio higienico" 1"Desagüe conectado a la red general" 2"Resto de alternativas"
label values des2_ch des2

**********************************
* MATERIAL PREDOMINANTE DEL PISO *
**********************************
gen piso_ch=.
label var piso_ch "Material predominante del piso"
label define piso 0"No permanentes / Tierra" 1"Permanentes: Cemento, cerámica, mosaico, madera" 2"Otros materiales"
label values piso_ch piso

****************************************
* MATERIAL PREDOMINANTE DE LAS PAREDES *
****************************************
gen pared_ch=.
label var pared_ch "Material predominante de las paredes"
label define pared 0"No permanentes / naturales o desechos" 1"Permanentes: ladrillo, madera, prefabricado, zinc, cemento" 2"Otros materiales"
label values pared_ch pared

***********************************
* MATERIAL PREDOMINANTE DEL TECHO *
***********************************
gen techo_ch=.
label var techo_ch "Material predominante del techo"
label define techo 0"No permanentes / naturales o desechos" 1"Permanentes: lámina de metal o zinc, cemento o madera" 2"Otros materiales"
label values techo_ch techo

*************************************
* MÉTODO DE ELIMINACION DE RESIDUOS *
*************************************
gen resid_ch=.
label var resid_ch "Material predominante del techo"
label define resid 0"Recolección pública o privada" 1"Quemados o enterrados" 2"Tirados en un espacio abierto"
label values resid_ch resid



*****************************************
*  CANTIDAD DE DORMITORIOS EN EL HOGAR  *
*****************************************
gen dorm_ch=.
label var dorm_ch "Cantidad de dormitorios en el hogar"

*****************************************
*  CANTIDAD DE CUARTOS EN EL HOGAR  *
*****************************************
gen cuartos_ch=.
label var cuartos_ch "Cantidad de cuartos en el hogar"

**********************************
*  CUARTO EXCLUSIVO A LA COCINA  *
**********************************
gen cocina_ch=.
*replace cocina_ch=1 if
label var cuartos_ch "Cuarto exclusivo a la cocina"

*************************
*  TIENE TELEFONO FIJO  *
*************************
gen telef_ch=.
replace telef_ch=1 if telephon==1
replace telef_ch=0 if telephon==2
label var telef_ch "Tiene teléfono fijo"

***********************************
*  TIENE HELADERA O REFRIGERADOR  *
***********************************
gen refrig_ch=.
*replace refrig_ch=1 if
label var refrig_ch "Tiene heladera o refrigerador"

********************************
*  TIENE FREEZER O CONGELADOR  *
********************************
gen freez_ch=.
*replace freez_ch=1 if
label var freez_ch "Tiene freezer o congelador"

*********************
*  TIENE AUTOMOVIL  *
*********************
gen auto_ch=.
*replace auto_ch=1 if
label var auto_ch "Tiene automovil"

*********************
*  TIENE COMPUTADOR  *
*********************
gen compu_ch=.
replace compu_ch=1 if computer==1
replace compu_ch=0 if computer==2
label var compu_ch "Tiene computador"

*******************************
*  TIENE CONEXION A INTERNET  *
*******************************
gen internet_ch=.
*replace internet_ch=1 if
label var internet_ch "Tiene acceso a internet"

*******************
*  TIENE CELULAR  *
*******************
gen cel_ch=.
*replace cel_ch=1 if
label var cel_ch "Tiene celular"

**********************
*  TIPO DE VIVIENDA  *
**********************
gen vivi1_ch=.
label var vivi1_ch "Tipo de vivienda"
label define vivi1 1"Casa" 2"Departamento" 3"Otro tipo"
label values vivi1_ch vivi1

************************
*  CASA O DEPARTAMENTO *
************************
gen vivi2_ch=.
*replace vivi2_ch=1 if
label var vivi2_ch "Casa o departamento"

********************
*  VIVIENDA PROPIA *
********************
* MGD 09/08/2014: revisar categorias con SCL.
gen viviprop_ch=.
replace viviprop_ch=0 if rent==3 | rent==4 | rent==5 | rent==2
replace viviprop_ch=1 if rent==1
replace viviprop_ch=3 if rent==6 | rent==7
label var viviprop_ch "Vivienda propia"
label define viviprop 0"Arrendada" 1"Propia y totalmente pagada" 3 "Ocupada(propia de facto)"
label values viviprop_ch viviprop

********************************
*  POSEE TITULO DE PROPIEDAD   *
********************************
gen vivitit_ch=.
*replace vivitit_ch=1 if
label var vivitit_ch "El hogar posee un título de propiedad"

********************************
*  MONTO DE PAGO POR ALQUILER   *
********************************
gen vivialq_ch=.
label var vivialq_ch "Monto pagado por el alquiler"

***********************************
*  VALOR ESTIMADO DE LA VIVIENDA  *
***********************************
gen vivialqimp_ch=.
label var vivialqimp_ch "Monto ud cree le pagarían por su vivienda"

* Variables no generadas
g tipopen_ci=.
g tcylmpri_ci=.
g tcylmpri_ch=.
g instcot_ci=.
g edus1i_ci=.
g edus1c_ci=.
g mes_c=.

/*_____________________________________________________________________________________________________*/
* Asignación de etiquetas e inserción de variables externas: tipo de cambio, Indice de Precios al 
* Consumidor (2011=100), Paridad de Poder Adquisitivo (PPA 2011),  líneas de pobreza
/*_____________________________________________________________________________________________________*/


do "$gitFolder\armonizacion_microdatos_encuestas_hogares_scl\_DOCS\\Labels&ExternalVars_Harmonized_DataBank.do"

/*_____________________________________________________________________________________________________*/
* Verificación de que se encuentren todas las variables armonizadas 
/*_____________________________________________________________________________________________________*/

order region_BID_c region_c pais_c anio_c mes_c zona_c factor_ch	idh_ch	idp_ci	factor_ci sexo_ci edad_ci ///
afroind_ci afroind_ch afroind_ano_c dis_ci dis_ch relacion_ci civil_ci jefe_ci nconyuges_ch nhijos_ch notropari_ch notronopari_ch nempdom_ch ///
clasehog_ch nmiembros_ch miembros_ci nmayor21_ch nmenor21_ch nmayor65_ch nmenor6_ch	nmenor1_ch	condocup_ci ///
categoinac_ci nempleos_ci emp_ci antiguedad_ci	desemp_ci cesante_ci durades_ci	pea_ci desalent_ci subemp_ci ///
tiempoparc_ci categopri_ci categosec_ci rama_ci spublico_ci tamemp_ci cotizando_ci instcot_ci	afiliado_ci ///
formal_ci tipocontrato_ci ocupa_ci horaspri_ci horastot_ci	pensionsub_ci pension_ci tipopen_ci instpen_ci	ylmpri_ci nrylmpri_ci ///
tcylmpri_ci ylnmpri_ci ylmsec_ci ylnmsec_ci	ylmotros_ci	ylnmotros_ci ylm_ci	ylnm_ci	ynlm_ci	ynlnm_ci ylm_ch	ylnm_ch	ylmnr_ch  ///
ynlm_ch	ynlnm_ch ylmhopri_ci ylmho_ci rentaimp_ch autocons_ci autocons_ch nrylmpri_ch tcylmpri_ch remesas_ci remesas_ch	ypen_ci	ypensub_ci ///
salmm_ci tc_c ipc_c lp19_c lp31_c lp5_c lp_ci lpe_ci aedu_ci eduno_ci edupi_ci edupc_ci	edusi_ci edusc_ci eduui_ci eduuc_ci	edus1i_ci ///
edus1c_ci edus2i_ci edus2c_ci edupre_ci eduac_ci asiste_ci pqnoasis_ci pqnoasis1_ci	repite_ci repiteult_ci edupub_ci tecnica_ci ///
aguared_ch aguafconsumo_ch aguafuente_ch aguadist_ch aguadisp1_ch aguadisp2_ch aguamala_ch aguamejorada_ch aguamide_ch bano_ch banoex_ch banomejorado_ch sinbano_ch aguatrat_ch luz_ch luzmide_ch combust_ch des1_ch des2_ch piso_ch  ///
pared_ch techo_ch resid_ch dorm_ch cuartos_ch cocina_ch telef_ch refrig_ch freez_ch auto_ch compu_ch internet_ch cel_ch ///
vivi1_ch vivi2_ch viviprop_ch vivitit_ch vivialq_ch	vivialqimp_ch , first



compress


saveold "`base_out'", replace


log close
