
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
local ANO "2018"
local ronda m4

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
Round: April
Autores: 
Modificación 2023: Natalia Tosi y Mayte Ysique
Fecha última modificación: Agosto 2023

							SCL/LMK - IADB
****************************************************************************/
/***************************************************************************
Detalle de procesamientos o modificaciones anteriores:

****************************************************************************/


use `base_in', clear


**********************
* AÑO DE LA ENCUESTA *
**********************
gen anio_c = 2018
label variable anio_c "Año de la Encuesta"

*************************
* FACTORES DE EXPANSION *
*************************
gen factor_ch = Weight
label var factor_ch "Factor de Expansion del Hogar"

gen factor_ci = Weight
label var factor_ci "Factor de Expansion del Individuo"

**************
* REGION BID *
**************
gen region_BID_c = 1
label var region_BID_c "Region BID"
label define region_BID 1"Centroamérica" 2"Caribe" 3"Andinos" 4"Cono Sur"
label values region_BID_c region_BID

***************
* REGION PAIS *
***************
gen ine01 = DISTRICT
g region_c =.

***************
*    ZONA     *
***************
gen byte zona_c = (URBAN_RURAL == 1)
replace zona_c = . if missing(URBAN_RURAL)

label variable zona_c "Zona geográfica"
label define zona_c 0 "Rural" 1 "Urbana"
label value zona_c zona_c

***********
*  MES   *
***********
g mes_c = 4

***********
*  PAIS   *
***********
gen pais_c = "BLZ"
label var pais_c "Acrónimo del país"

******************************
*  IDENTIFICADOR DEL HOGAR   *
******************************
egen idh_ch = group(HHID)
label var idh_ch "Identificador Unico del Hogar"

*sort HHID PNUM
*quietly by HHID PNUM: gen rep = cond(_N == 1, 0, _n)

*******************************
* IDENTIFICADOR DEL INDIVIDUO *
*******************************
gen idp_ci = PNUM
label var idp_ci "Identificador Individual dentro del Hogar"

************************************
*  RELACION CON EL JEFE DE HOGAR   *
************************************

gen relacion_ci = .
/*
gen relacion_ci = 1 if cq11 == 1 
replace relacion_ci=2 if cq11 == 2
replace relacion_ci=3 if cq11 == 3
replace relacion_ci=4 if cq11==4 | cq11==5 | cq11==6 | cq11==7
replace relacion_ci=5 if cq11==8
replace relacion_ci=. if cq11==9 /* No sabe */
*/

label var relacion_ci "relación con el jefe de hogar"
label define relacion 1"Jefe" 2"Cónguye, Esposo/a, Compañero/a" 3"Hijo/a" 4"Otros parientes" 5"Otros no parientes" 6"Servicio doméstico" 
label values relacion_ci relacion


************************************
* DUMMY PARA NO MIEMBROS DEL HOGAR *
************************************

* Create a dummy indicating this person's income should NOT be included 
gen miembros_ci = 0
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
gen sexo_ci = HL5
label var sexo_ci "sexo del individuo"
label define sexo 1"Masculino" 2"Femenino" 
label values sexo_ci sexo

***********
*  EDAD   *
***********
gen edad_ci = HL3 
label var edad_ci "edad del individuo"

			
*******************************************************
***           VARIABLES DE DIVERSIDAD               ***
*******************************************************				
* Maria Antonella Pereira & Nathalia Maya - Marzo 2021	

			
	***************
	***afroind_ci***
	*************** 
gen afroind_ci = .
replace afroind_ci = 1 if inlist(HL6, 3, 4)/* Indigena */
replace afroind_ci = 2 if inlist(HL6, 1, 2)/* Afro */
replace afroind_ci = 3 if inlist(HL6, 5, 6, 7)/* Otros */ 

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
egen nmenor65_ch=sum((relacion_ci>0 & relacion_ci<5) & (cq13<=65)), by (idh_ch)
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
replace condocup_ci=1 if cq112==1 | cq113==1 | (cq114==1 & cq115<=2 & cq115!=. & cq115a==1) | (cq114==1 & cq115>=3 & cq115<=8)
*Desocupado*
*Desocupado*
replace condocup_ci=2 if condocup_ci!=1 & ((cq16==1 | (cq110b<=8 & cq110b!=.) | cq1171==1 | cq1172==1 | cq1173==1 | cq1174==1 | cq1175==1 | cq1176==1) & cq119==1)
*Inactivo*
replace condocup_ci=3 if (condocup_ci~=1 & condocup_ci~=2) & edad_ci>=14
*menores que PET
recode condocup_ci (.=4) if edad<14
label define condocup 1"Ocupado" 2"Desocupado" 3"Inactivo" 4"Menores de 14 años"
label values condocup_ci condocup
label var condocup_ci "Condición de ocupación"



**************************
* CATEGORIA DE INACTIVIDAD  *
**************************

gen categoinac_ci=.
label var  categoinac_ci "Condición de Inactividad" 
*Jubilados, pensionados
replace categoinac_ci=1 if (cq120==3 | cq118==4) & condocup_ci==3
*Estudiantes
replace categoinac_ci=2 if (cq120==2 | cq111==2 | cq118==3) & condocup_ci==3
*Quehaceres del Hogar
replace categoinac_ci=3 if (cq120==1 | cq111==1)& condocup_ci==3
*Otra razon
recode categoinac_ci (.=4) if categoinac_ci!=1 & categoinac_ci!=2 & categoinac_ci!=3 & condocup_ci==3 & cq120!=.
label define inactivo 1"Jubilados o Pensionado" 2"Estudiante" 3"Hogar" 4"Otros"
label values categoinac_ci inactivo


**********************
*  NÚMERO DE EMPLEOS *
**********************
gen nempleos_ci=.
replace nempleos_ci=1 if cq127==2 
replace nempleos_ci=2 if cq127==1
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
gen antiguedad_ci=cq141 if cq141<99 & emp_ci==1
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
replace cesante_ci=1 if cq125==1 & condocup_ci==2
label var cesante_ci "Cesante"

***********************************
* DURACION DEL DESEMPLEO EN MESES *
***********************************
*La variable esta definida por intervalos de tiempo. Se uso el punto medio del intervalo
gen durades_ci=.
replace durades_ci=0.5 if condocup_ci==2 & cq124==1
replace durades_ci=2 if condocup_ci==2 & cq124==2
replace durades_ci=5 if condocup_ci==2 & cq124==3
replace durades_ci=10 if condocup_ci==2 & cq124==4
replace durades_ci=12 if condocup_ci==2 & cq124==5
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
gen desalent_ci=0 if condocup_ci==3
replace desalent_ci=1 if (cq118>=10 & cq118<=14) & condocup_ci==3
label var desalent_ci "Trabajadores desalentados, personas que creen que por alguna razon no conseguiran trabajo" 

*****************************
* TRABAJA MENOS DE 30 HORAS *
*****************************
* MGD 08/29/2014: no hay la pregunta de si desea trabajar mas horas, pero se utiliza disponibilidad para otro trabajo.
gen subemp_ci=0 
replace subemp_ci=1 if cq132m<=30 & cq138==1 & condocup_ci==1
label var subemp_ci "Trabaja menos de 30 horas"



****************************************************
* TRABAJA MENOS DE 30 HORAS Y NO DESEA TRABAJAR MAS*
****************************************************
* MGD 08/29/2014: no hay la pregunta de si desea trabajar mas horas, pero se utiliza disponibilidad para otro trabajo.
gen tiempoparc_ci=0 
replace tiempoparc_ci=1 if cq132m<=30 & cq138==2 & condocup_ci==1
label var tiempoparc_ci "Trabaja menos de 30 horas"


*********************************
* CATEGORIA OCUPACION PRINCIPAL *
*********************************
gen categopri_ci=.
replace categopri_ci=1 if cq128m==1 & condocup_ci==1
replace categopri_ci=2 if cq128m==2 & condocup_ci==1
replace categopri_ci=3 if (cq128m==3 | cq128m==4 | cq128m==5) & condocup_ci==1
replace categopri_ci=4 if cq128m==6 & condocup_ci==1
label var categopri_ci "Categoría ocupación principal"
label define categopri 1"Patrón o empleador" 2"Cuenta propia o independiente" 3"Empleado o asalariado" 4"Trabajador no remunerado"  
label values categopri_ci categopri

*********************************
* CATEGORIA OCUPACION SECUNDARIA*
*********************************
gen categosec_ci=.
replace categosec_ci=1 if cq128o==1 & condocup_ci==1
replace categosec_ci=2 if cq128o==2 & condocup_ci==1
replace categosec_ci=3 if (cq128o==3 | cq128o==4 | cq128o==5) & condocup_ci==1
replace categosec_ci=4 if cq128o==6 & condocup_ci==1
label var categosec_ci "Categoría ocupación secundaria"
label define categosec 1"Patrón o empleador" 2"Cuenta propia o independiente" 3"Empleado o asalariado" 4"Trabajador no remunerado"  
label values categosec_ci categosec

*********************************
*  RAMA DE ACTIVIDAD PRINCIPAL  *
*********************************
*** MGD 08/29/2014: CIIU REV. 3
gen rama_ci=.
replace rama_ci=1 if (cq131misi>=0 & cq131misi<=502) & emp_ci==1 /* indmisic for 2004 */
replace rama_ci=2 if (cq131misi>=1000 & cq131misi<=1421) & emp_ci==1
replace rama_ci=3 if (cq131misi>=1500 & cq131misi<=3799) & emp_ci==1
replace rama_ci=4 if (cq131misi>=4010 & cq131misi<=4100) & emp_ci==1
replace rama_ci=5 if (cq131misi>=4500 & cq131misi<=4599) & emp_ci==1
replace rama_ci=6 if (cq131misi>=5000 & cq131misi<=5599) & emp_ci==1
replace rama_ci=7 if (cq131misi>=6000 & cq131misi<=6420) & emp_ci==1
replace rama_ci=8 if (cq131misi>=6500 & cq131misi<=7499) & emp_ci==1
replace rama_ci=9 if (cq131misi>=7500 & cq131misi<=9900) & emp_ci==1
label define rama_ci 1"Agricultura, caza, silvicultura o pesca" 2"Minas y Canteras" 3"Manufactura" 4"Electricidad, gas o agua" 5"Construcción" 6"Comercio al por mayor, restaurantes o hoteles" 7"Transporte o almacenamiento" 8"Establecimientos financieros, seguros o bienes inmuebles" 9"Servicios sociales, comunales o personales" 
label values rama_ci rama_ci


* rama secundaria
gen ramasec_ci=.
replace ramasec_ci=1 if (cq131oisi>=0 & cq131oisi<=502) & emp_ci==1 /* indmisic for 2004 */
replace ramasec_ci=2 if (cq131oisi>=1000 & cq131oisi<=1421) & emp_ci==1
replace ramasec_ci=3 if (cq131oisi>=1500 & cq131oisi<=3799) & emp_ci==1
replace ramasec_ci=4 if (cq131oisi>=4010 & cq131oisi<=4100) & emp_ci==1
replace ramasec_ci=5 if (cq131oisi>=4500 & cq131oisi<=4599) & emp_ci==1
replace ramasec_ci=6 if (cq131oisi>=5000 & cq131oisi<=5599) & emp_ci==1
replace ramasec_ci=7 if (cq131oisi>=6000 & cq131oisi<=6420) & emp_ci==1
replace ramasec_ci=8 if (cq131oisi>=6500 & cq131oisi<=7499) & emp_ci==1
replace ramasec_ci=9 if (cq131oisi>=7500 & cq131oisi<=9900) & emp_ci==1
label define ramasec_ci 1"Agricultura, caza, silvicultura o pesca" 2"Minas y Canteras" 3"Manufactura" 4"Electricidad, gas o agua" 5"Construcción" 6"Comercio al por mayor, restaurantes o hoteles" 7"Transporte o almacenamiento" 8"Establecimientos financieros, seguros o bienes inmuebles" 9"Servicios sociales, comunales o personales" 
label values ramasec_ci ramasec_ci


*********************************
*  TRABAJA EN EL SECTOR PUBLICO *
*********************************
gen spublico_ci=0 
replace spublico_ci=1 if (cq128m==3 | cq128m==4) & condocup_ci==1
label var spublico_ci "Personas que trabajan en el sector publico"
 
********************
* TAMAÑO DE EMPRESA*
********************
* MGD 08/29/2014: esta variable tiene demasiados valores missing reportados; por lo que no es confiable el indicador.
g tamemp_ci=.
/*
gen tamemp_ci=1 if cq129y==1
label var  tamemp_ci "Tamaño de Empresa" 
*Empresas medianas
replace tamemp_ci=2 if cq129y==2 | cq129y==3
*Empresas grandes
replace tamemp_ci=3 if cq129y==4
label define tamaño 1"Pequeña" 2"Mediana" 3"Grande"
label values tamemp_ci tamaño
*/

*********************************
*  COTIZA A LA SEGURIDAD SOCIAL *
*********************************
* MGD 09/02/2014: no hay variable de cotizacion en la encuesta LFS.
gen cotizando_ci=.
/*replace cotizando_ci=1 if q432==1
replace cotizando_ci=0 if q432==2
label var cotizando_ci "Cotizando a la seguridad social"
*/

****************************************************
*  INSTITUCION DE SEGURIDAD SOCIAL A LA QUE COTIZA *
****************************************************
gen inscot_ci=.
label var inscot_ci "Institución de seguridad social a la que cotiza"

**********************************
* AFILIADO A LA SEGURIDAD SOCIAL *
**********************************
* MGD 09/02/2014: no hay variable de afiliacion en la encuesta LFS.
gen afiliado_ci=.
/*replace afiliado_ci=1 if q429==1
replace afiliado_ci=0 if q429==2
label var afiliado_ci "Afiliado a la seguridad social"
*/

g pension_ci=.
g tipopen_ci=.

*********************
* TRABAJADOR FORMAL *
*********************
* MGD 09/02/2014: no hay variable de afiliacion en la encuesta LFS.
gen formal_ci=.
g formal_1=.
/*replace formal_ci=0 if condocup_ci==1 & afiliado_ci==0 & cotizando_ci==0
replace formal_ci=1 if condocup_ci==1 & (afiliado_ci==1 | cotizando_ci==1)
label var afiliado_ci "Afiliado a la seguridad social"
*/

********************
* TIPO DE CONTRATO *
********************
* MGD 09/02/2014: no hay informacion de tipo de contrato en LFS
gen tipocontrato_ci=. 
/*replace tipocontrato_ci=3 if q426==2 | (q426==1 & q427==2)
label var tipocontrato_ci "Tipo de contrato"
label define tipocontrato 1"Permanente / Indefinido" 2"Temporal / Tiempo definido" 3"Sin contrato / Verbal"  
label values tipocontrato_ci tipocontrato
*/

*****************************
* TIPO DE OCUPACION LABORAL *
*****************************
* MGD 09/02/2014: CIUO-88
gen ocupa_ci=.
replace ocupa_ci=1 if (cq130m>=2000 & cq130m<=3999) & emp_ci==1
replace ocupa_ci=2 if (cq130m>=1000 & cq130m<=1999) & emp_ci==1
replace ocupa_ci=3 if (cq130m>=4000 & cq130m<=4999) & emp_ci==1
replace ocupa_ci=4 if ((cq130m>=5200 & cq130m<=5999) | (cq130m>=9111 & cq130m<=9113)) & emp_ci==1
replace ocupa_ci=5 if ((cq130m>=5000 & cq130m<=5199) | (cq130m>=9120 & cq130m<=9162)) & emp_ci==1
replace ocupa_ci=6 if ((cq130m>=6000 & cq130m<=6999) | (cq130m>=9200 & cq130m<=9213)) & emp_ci==1
replace ocupa_ci=7 if ((cq130m>=7000 & cq130m<=8999) | (cq130m>=9300 & cq130m<=9333))& emp_ci==1
replace ocupa_ci=8 if (cq130m>=0 & cq130m<=999)  & emp_ci==1
replace ocupa_ci=9 if (cq130m==9119 | cq130m==9999) & emp_ci==1
label var ocupa_ci "Tipo de ocupacion laboral"
label define ocupa 1"Profesional o técnico" 2"Director o funcionario superior" 3"Personal administrativo o nivel intermedio" 4"Comerciante o vendedor" 5"Trabajador en servicios" 6"Trabajador agrícola o afines" 7"Obrero no agrícola, conductores de máquinas y vehículos de transporte y similares" 8"Fuerzas armadas" 9"Otras ocupaciones no clasificadas"
label values ocupa_ci ocupa

**********************************************
* HORAS TRABAJADAS EN LA ACTIVIDAD PRINCIPAL *
**********************************************
gen horaspri_ci=.
replace horaspri_ci=cq132m if cq132m!=99
label var horaspri_ci "Horas trabajadas en la actividad principal"

**************************
* TOTAL HORAS TRABAJADAS *
**************************
gen horastot_ci=cq132m+cq132o
replace horastot_ci=. if cq132m==. & cq132o==.
label var horastot_ci "Total horas trabajadas"

***********************************************
* RECIBE PENSION O JUBILACION NO CONTRIBUTIVA *
***********************************************
gen pensionsub_ci=.
*replace pensionsub_ci=1 if
label var pensionsub_ci "Recibe pensión o ubilación NO contributiva"


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
*replace nrylmpri_ci=1 if cq51==9 | (cq51==7 & cq128m>0)
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
gen ylmpri_ci=.
replace ylmpri_ci=incoave*30 if cq143==1
replace ylmpri_ci= incoave*4.3 if cq143==2
replace ylmpri_ci= incoave*2 if cq143==3
replace ylmpri_ci= incoave if cq143==4 | cq143==8
replace ylmpri_ci= incoave/12 if cq143==5
replace ylmpri_ci= 0 if cq143==6
*replace ylmpri_ci= cq142 if incoave==. & emp_ci==1 

label var ylmpri_ci "Monto mensual de ingreso laboral de la actividad principal"
/*
gen income=.
replace income= (119/2) if cq142==1
replace income= (120+239)/2 if cq142==2
replace income= ((240+359)/2) if cq142==3
replace income= ((360+479)/2) if cq142==4
replace income= ((480+599)/2) if cq142==5
replace income= ((600+719)/2) if cq142==6
replace income= ((720+839)/2) if cq142==7
replace income= ((840+959)/2) if cq142==8
replace income= ((960+1079)/2) if cq142==9
replace income= ((1080+1199)/2) if cq142==10
replace income= ((1200+1319)/2) if cq142==11
replace income= ((1320+1439)/2) if cq142==12
replace income= ((1440+1559)/2) if cq142==13
replace income= ((1560+1679)/2) if cq142==14
replace income= ((1680+1799)/2) if cq142==15
replace income= ((1800+1919)/2) if cq142==16
replace income= ((1920+2039)/2) if cq142==17
replace income= ((2040+2159)/2) if cq142==18
replace income= ((2160+2279)/2) if cq142==19
replace income= ((2280+2399)/2) if cq142==20
replace income= ((2400+2519)/2) if cq142==21
replace income= ((2520+2639)/2) if cq142==22
replace income= ((2640+2759)/2) if cq142==23
replace income= ((2760+2879)/2) if cq142==24
replace income= ((2880+2999)/2) if cq142==25

gen ylmpri_ci=.
replace ylmpri_ci=income*30 if cq143==1
replace ylmpri_ci= income*4.3 if cq143==2
replace ylmpri_ci= income*2 if cq143==3
replace ylmpri_ci= income if cq143==4 | cq143==8
replace ylmpri_ci= income/12 if cq143==5
replace ylmpri_ci= 0 if cq143==6 | cq143==9*/

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
gen ylnm_ci= .
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
*gen ylm_ch=.
egen ylm_ch=sum(ylm_ci) if miembros_ci==1, by(idh_ch)
label var ylm_ch "Ingreso Laboral Monetario del Hogar (Bruto)"

**************************************************
* INGRESO MENSUAL LABORAL NO MONETARIO DEL HOGAR *
**************************************************
gen ylnm_ch=.
label var ylnm_ch "Ingreso Laboral No Monetario del Hogar"

**************************************************************
* INGRESO MENSUAL NO LABORAL OTRAS ACTIVIDADES no respuesta  *
**************************************************************
gen ylmnr_ch=.
label var ylmnr_ch "Ingreso Laboral Monetario del Hogar, considera 'missing' la No Respuesta"

**************************************************
* INGRESO MENSUAL NO LABORAL MONETARIO DEL HOGAR *
**************************************************
gen ynlm_ch=.
label var ynlm_ch "Ingreso No Laboral Monetario del Hogar"

*****************************************************
* INGRESO MENSUAL NO LABORAL NO MONETARIO DEL HOGAR *
*****************************************************
gen ynlnm_ch=.
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
gen autocons_ch=.
label var autocons_ch "Autoconsumo del Hogar"

***************************
* REMESES EN MONEDA LOCAL *
***************************
gen remesas_ci=.
label var remesas_ci "Remesas en moneda local"

************************************
* REMESES EN MONEDA LOCAL DEL HOGAR*
************************************
by idh_ch: gen remesas_ch=.
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
* MGD 09/02/2014: Fuente: Doing Bussiness
gen salmm_ci =585
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


// dk/ns va a missing
replace cq13 = . if cq13 == 99
replace q51b = . if q51b == 99
replace cq16a = . if cq16a == 17
replace cq16b = . if cq16b == 99

*************
** aedu_ci **
*************
/*
MGD 09/09/2014: Clasificacion de añ de educacion en BLZ.  
	
Se toma como referencia:
	
	- Primary Primary School - 8 years
	- Secondary CSEC (Caribbean Secondary Education Certificate) Examinations - 4 years
	- Post-secondary CXC Caribbean Advanced Placement Examination (CAPE)- 2 years (para quienes no culminaron la secundaria)   
	- Tertiary University  

La ultima categoria es mas de dos años de universidad y se asumen 16 completos.

Debido a inconsistencia en la encuesta entre las preguntas q51a q51b y cq16a cq16b se condiciona su uso por edad 
siguiendo la clasificacion del cuestionario q51x es para menores de 14 anios y las cq16x de 14 anios en adelante. 
	
*/


gen aedu_ci = .
// Menores de 14 anios 
replace aedu_ci = q51a if cq13 < 14 // Asistentes
replace aedu_ci = q51b if cq13 < 14 // No asistentes

// Mayores de 14 anios
replace aedu_ci = cq16a if (cq16a < 16 & cq13 >= 14) // Asistentes 
replace aedu_ci = cq16b if (cq16b < 17 & cq13 >= 14) // No asistentes

// Universitarios de mas de 2 anios - se imputa universitario completo
replace aedu_ci = 16 if cq16a == 16 & cq13 >= 14 
replace aedu_ci = 16 if cq16b == 17 & cq13 >= 14 
label var aedu_ci "número de años de educación culminados"

**************
** eduno_ci **
**************
gen eduno_ci = (aedu_ci == 0)
replace eduno_ci = . if aedu_ci == . 
label var eduno_ci "No tiene ningún nivel de instrucción"

**************
** edupi_ci **
**************
gen edupi_ci = (aedu_ci > 0 & aedu_ci < 6)
replace edupi_ci = . if aedu_ci == .
label var edupi_ci "No ha completado la educación primaria"

***************
** edupc_ci  **
***************
gen edupc_ci = (aedu_ci == 6)
replace edupc_ci = . if aedu_ci == .
label var edupc_ci "Ha completado la educación primaria"

**************
** edusi_ci **
**************
gen edusi_ci = (aedu_ci > 6 & aedu_ci < 12)
replace edusi_ci = . if aedu_ci == . 
label var edusi_ci "No ha completado la educación secundaria"

**************
** edusc_ci **
**************
gen edusc_ci = (aedu_ci == 12)
replace edusc_ci = . if aedu_ci == .
label var edusc_ci "Ha completado la educación secundaria"

**************
** eduui_ci **
**************
// NO se identifica estudios no universitarios.
gen eduui_ci = (aedu_ci > 12 & aedu_ci < 16)
replace eduui_ci = . if aedu_ci == . 
label var eduui_ci "No ha completado la educación terciaria"

**************
** eduuc_ci **
**************
gen eduuc_ci = (aedu_ci == 16)
replace eduuc_ci = . if aedu_ci == .
label var eduuc_ci "Ha completado la educación terciaria"

***************
** edus1i_ci **
***************
gen edus1i_ci = (aedu_ci > 6 & aedu_ci < 10)
replace edus1i_ci = . if aedu_ci == . 
label var edus1i_ci "No ha completado el primer ciclo de la secundaria"

***************
** edus1c_ci **
***************
gen edus1c_ci = (aedu_ci == 10)
replace edus1c_ci = . if aedu_ci == . 
label var edus1c_ci "Ha completado el primer ciclo de la secundaria"

***************
** edus2i_ci **
***************
gen edus2i_ci = (aedu_ci == 11)
replace edus2i_ci = . if aedu_ci == .
label var edus2i_ci "No ha completado el segundo ciclo de la secundaria"

***************
** edus2c_ci **
***************
gen edus2c_ci = (aedu_ci == 12)
replace edus2c_ci = . if aedu_ci == .
label var edus2c_ci "Ha completado el segundo ciclo de la secundaria"

***************
** edupre_ci **
***************
gen edupre_ci = . 
label var edupre_ci "Ha completado educación preescolar"

***************
** asispre_ci **
***************
gen asispre_ci = . 
label var asispre_ci "Ha completado educación preescolar"

**************
** eduac_ci **
**************
// No se distingue entre universitario y no universitario
gen eduac_ci=.
label var eduac_ci "Ha completado educación terciaria académica"

***************
** asiste_ci **
***************
gen asiste_ci=.
replace asiste_ci = 1 if ((cq16 == 1 | cq16 == 2) & cq13 > 14) | ((q51 == 1 | q51 == 2) & cq13 <= 14)
replace asiste_ci = 0 if (cq16 == 3  & cq13 > 14) |  (q51 == 3  & cq13 <= 14)
label var asiste_ci "Asiste a algún centro de enseñanza"


*****************
** pqnoasis_ci **
*****************
gen pqnoasis_ci=.
label var pqnoasis_ci "Porque no asiste a algún centro de enseñanza"
label define pqnoasis 1"Muy joven" 2"Razones financieras" 3"Trabaja en casa o negocio familiar" 4"Distancia a la escuela/transporte" 5"Enfermedad/inhabilidad" 6"falta de especio en la escuela" 7"Otra" 9"NS/NR"  
label values pqnoasis_ci pqnoasis


******************
***pqnoasis1_ci***
******************
**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci**
gen pqnoasis1_ci=.

***************
** repite_ci **
***************
gen repite_ci=.
label var repite_ci "Ha repetido algún año o grado"

******************
** repiteult_ci **
******************
gen repiteult_ci=.
label var repiteult_ci "Ha repetido el último grado"

**************
** edupub_ci **
***************
gen edupub_ci=.
label var edupub_ci "Asiste a centro de enseñanza pública"
label define edupub 1"Pública" 0"Privada"  
label values edupub_ci edupub



*******************************
*******************************
*******************************
*    VARIABLES DE VIVIENDA    *
*******************************
*******************************
*******************************


*********************
gen aguared_ch=.
replace aguared_ch= 1 if h8==1 | h8==2 | h71==1 | h72==1
replace aguared_ch= 0 if h8>2 | h73==1 | h74==1 | h75==1 | h76==1 | h77==1 | h78==1 
*********************
gen aguafconsumo_ch = 0
replace aguafconsumo_ch= 1 if h8==1 | h8==2
replace aguafconsumo_ch= 3 if h8==6
replace aguafconsumo_ch= 4 if h8==4
replace aguafconsumo_ch= 7 if h8==3
replace aguafconsumo_ch= 8 if h8==7
replace aguafconsumo_ch= 9 if h8==5
replace aguafconsumo_ch= 10 if h8==8

*********************
gen aguafuente_ch =.
replace aguafuente_ch= 1 if h8==1 | h8==2 | h71==1 | h72==1
replace aguafuente_ch= 3 if h8==6 | h76==1
replace aguafuente_ch= 4 if h8==4 | h74==1
replace aguafuente_ch= 7 if h8==3 | h73==1
replace aguafuente_ch= 8 if h8==7 | h77==1
replace aguafuente_ch= 9 if h8==5 | h75==1
replace aguafuente_ch= 10 if h8==8 | h78==1
replace aguafuente_ch = 10 if aguafuente_ch ==. & jefe_ci==1

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
replace bano_ch=0 if h3==8
replace bano_ch=1 if h3==1
replace bano_ch=2 if h3==2
replace bano_ch=3 if h3==3 | h3==4 | h3==5
replace bano_ch=5 if h3==6
replace bano_ch=6 if h3==7
replace bano_ch=6 if bano_ch ==. & jefe_ci==1

*********************
generate banoex_ch=9

*********************
gen banomejorado_ch= 2
replace banomejorado_ch =1 if bano_ch<=3 & bano_ch!=0
replace banomejorado_ch =0 if (bano_ch ==0 | bano_ch>=4) & bano_ch!=6

*********************
gen sinbano_ch =0 if h3!=8
replace sinbano_ch =3 if h3==8

*********************
gen aguatrat_ch =9

*********************


*****************************
*  ILUMINACION ES ELÉCTRICA *
*****************************
gen luz_ch=.
replace luz_ch=1 if h4<=3
replace luz_ch=0 if h4>3 & h4!=9
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
gen combust_ch=0 if h5!=.
replace combust_ch=1 if h5==1 | h5==4 
label var combust_ch "Usa combustible como fuente de energía"



*******************************************
*  TIPO DE DESAGÜE incluyendo Unimproved  *
*******************************************
gen des1_ch=.
replace des1_ch=0 if h3==8
replace des1_ch=1 if h3==1
replace des1_ch=2 if h3==2
replace des1_ch=3 if h3>=3 & h3<=6
label var des1_ch "Tipo de desague incluyendo Unimproved"
label define des1 0"El hogar no tiene servicio higienico" 1"Desagüe conectado a la red general" 2"Desagüe conectado a un pozo o letrina" 3"El desagüe se comunica con la superficie"
label values des1_ch des1

*******************************************
* TIPO DE DESAGÜE sin incluir Unimproved  *
*******************************************
gen des2_ch=.
replace des1_ch=0 if h3==8
replace des1_ch=1 if h3==1
replace des1_ch=2 if h3>=2 & h3<=7
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
label define pared_ch 0"No permanentes / naturales o desechos" 1"Permanentes: ladrillo, madera, prefabricado, zinc, cemento" 2"Otros materiales"
label values pared_ch pared_ch

***********************************
* MATERIAL PREDOMINANTE DEL TECHO *
***********************************
gen techo_ch=.
label var techo_ch "Material predominante del techo"
label define techo_ch 0"No permanentes / naturales o desechos" 1"Permanentes: lámina de metal o zinc, cemento o madera" 2"Otros materiales"
label values techo_ch techo_ch

*************************************
* MÉTODO DE ELIMINACION DE RESIDUOS *
*************************************
* MGD 09/02/2014: no hay variable en LFS
gen resid_ch=.
/*replace resid_ch=0 if q29==1 
replace resid_ch=1 if q29==4 | q29==5
replace resid_ch=2 if q29==2 | q29==3
replace resid_ch=3 if q29==6 | q29==7
label var resid_ch "Metodo de eliminacion de residuos"
label define resid 0"Recolección pública o privada" 1"Quemados o enterrados" 2"Tirados en un espacio abierto" 3"Otros"
label values resid_ch resid
*/




*****************************************
*  CANTIDAD DE DORMITORIOS EN EL HOGAR  *
*****************************************
* MGD 09/02/2014: no hay variable en LFS
gen dorm_ch=.
label var dorm_ch "Cantidad de dormitorios en el hogar"

*****************************************
*  CANTIDAD DE CUARTOS EN EL HOGAR  *
*****************************************
* MGD 09/02/2014: no hay variable en LFS
gen cuartos_ch=.
label var cuartos_ch "Cantidad de cuartos en el hogar"

**********************************
*  CUARTO EXCLUSIVO A LA COCINA  *
**********************************
* MGD 09/02/2014: no hay variable en LFS
gen cocina_ch=.
*label define cocina 1"Si" 2"No" 9"NS/NR"
*label values cocina_ch cocina
label var cocina_ch "Cuarto exclusivo a la cocina"

*************************
*  TIENE TELEFONO FIJO  *
*************************
gen telef_ch=0 if h61!=.
replace telef_ch=1 if h61==1
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
replace compu_ch=1 if h62==1
replace compu_ch=0 if h62==2
label var compu_ch "Tiene computador"

*******************************
*  TIENE CONEXION A INTERNET  *
*******************************
gen internet_ch=.
/*replace internet_ch=1 if q210e==1
replace internet_ch=0 if q210e==2
label var internet_ch "Tiene acceso a internet"
*/
*******************
*  TIENE CELULAR  *
*******************
gen cel_ch=.
*replace cel_ch=1 if
label var cel_ch "Tiene celular"

**********************
*  TIPO DE VIVIENDA  *
**********************
* MGD 09/02/2014: no hay variable.
gen vivi1_ch=.
/*replace vivi1_ch=1 if q11==1 | q11==2 | q11==4
replace  vivi1_ch=2 if q11==3
replace vivi1_ch=3 if q11==5 | q11==6 | q11==7
label var vivi1_ch "Tipo de vivienda"
label define vivi1_ch 1"Casa" 2"Departamento" 3"Otro tipo"
label values vivi1_ch vivi1_ch
*/
************************
*  CASA O DEPARTAMENTO *
************************
* MGD 09/02/2014: no hay variable.
gen vivi2_ch=.
/*replace vivi2_ch=1 if q11==1 | q11==2 | q11==4 | q11==3
replace vivi2_ch=0 if q11==5 | q11==6 | q11==7
label var vivi2_ch "Casa o departamento"
*/
********************
*  VIVIENDA PROPIA *
********************
gen viviprop_ch=.
replace viviprop_ch=0 if h1==3 | h1==4 | h1==5 | h1==2
replace viviprop_ch=1 if h1==1
replace viviprop_ch=3 if h1==6 | h1==7
label var viviprop_ch "Vivienda propia"
label define viviprop 0"Arrendada" 1"Propia y totalmente pagada" 3 "Ocupada(propia de facto)"
label values viviprop_ch viviprop


********************************
*  POSEE TITULO DE PROPIEDAD   *
********************************
* MGD 09/02/2014: no hay variable.
gen vivitit_ch=.
/*replace vivitit_ch=1 if q14==1
replace vivitit_ch=0 if q14==2
label var vivitit_ch "El hogar posee un título de propiedad"
*/
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
g tcylmpri_ci=.
g tcylmpri_ch=.
g instcot_ci=.

******************************
*** VARIABLES DE MIGRACION ***
******************************

	*******************
	*** migrante_ci ***
	*******************
	gen migrante_ci=.
	label var migrante_ci "=1 si es migrante"
	
	**********************
	*** migantiguo5_ci ***
	**********************
	gen migantiguo5_ci=.
	label var migantiguo5_ci "=1 si es migrante antiguo (5 anos o mas)"
		
	**********************
	*** migrantelac_ci ***
	**********************
	gen migrantelac_ci=.
	label var migrantelac_ci "=1 si es migrante proveniente de un pais LAC"

	**********************
	*** migrantiguo5_ci **
	**********************
	gen migrantiguo5_ci=.
	label var migrantiguo5_ci "=1 si es migrante antiguo (5 anos o mas)"
		
	**********************
	*** miglac_ci ***
	**********************
	gen miglac_ci=.
	label var miglac_ci "=1 si es migrante proveniente de un pais LAC"


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
edus1c_ci edus2i_ci edus2c_ci edupre_ci eduac_ci asiste_ci pqnoasis_ci pqnoasis1_ci	repite_ci repiteult_ci edupub_ci ///
aguared_ch aguadist_ch aguamala_ch aguamide_ch luz_ch luzmide_ch combust_ch	bano_ch banoex_ch des1_ch des2_ch piso_ch aguamejorada_ch banomejorado_ch ///
pared_ch techo_ch resid_ch dorm_ch cuartos_ch cocina_ch telef_ch refrig_ch freez_ch auto_ch compu_ch internet_ch cel_ch ///
vivi1_ch vivi2_ch viviprop_ch vivitit_ch vivialq_ch	vivialqimp_ch , first

rename cq131misi codindustria
rename cq130m codocupa

compress


saveold "`base_out'", replace


log close
