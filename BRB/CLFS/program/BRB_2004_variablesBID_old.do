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

local PAIS BRB
local ENCUESTA CLFS
local ANO "2004"
local ronda a

*local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
   
*capture log close
*log using "`log_file'", replace 

*log off
/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Barbados
Encuesta: CLFS
Round: 
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
gen anio_c=2004
label variable anio_c "Año de la Encuesta"

**********************
* MES DE LA ENCUESTA *
**********************
*gen mes_c=.
*label variable mes_c "Mes de la Encuesta"

*************************
* FACTORES DE EXPANSION *
*************************
gen factor_ch= wtfactor
label var factor_ch "Factor de Expansion del Hogar"
gen factor_ci=  wtfactor
label var factor_ci "Factor de Expansion del Individuo"

**************
* REGION BID *
**************
gen region_BID_c=2
label var region_BID_c "Region BID"
label define region_BID 1"Centroamérica" 2"Caribe" 3"Andinos" 4"Cono Sur"
label values region_BID_c region_BID

***************
* REGION PAIS *
***************
g region_ci=.
g ine01 =.

***************
*    ZONA     *
***************
gen byte zona_c=.
*replace zona_c=1 if area==1 /* Urbana */
*replace zona_c=0 if area==2 /* Rural */
label variable zona_c "Zona geográfica"
label define zona_c 0"Rural" 1"Urbana"
label value zona_c zona_c

***********
*  PAIS   *
***********
gen pais_c="BRB"
label var pais_c "Acrónimo del país"

******************************
*  IDENTIFICADOR DEL HOGAR   *
******************************
gen idh_ch=hhno
label var idh_ch "Identificador Unico del Hogar"

*******************************
* IDENTIFICADOR DEL INDIVIDUO *
*******************************
gen idp_ci=indivno
label var idp_ci "Identificador Individual dentro del Hogar"

************************************
*  RELACION CON EL JEFE DE HOGAR   *
************************************
/*gen relacion_ci=1 if relhd==1
replace relacion_ci=2 if relhd==2
replace relacion_ci=3 if relhd==3 | relhd==4
replace relacion_ci=4 if relhd==5 
replace relacion_ci=5 if relhd==6 | relhd==7 
replace relacion_ci=. if relhd==9 /* No sabe */
label var relacion_ci "relación con el jefe de hogar"
label define relacion 1"Jefe" 2"Cónguye, Esposo/a, Compañero/a" 3"Hijo/a" 4"Otros parientes" 5"Otros no parientes" 6"Servicio doméstico" 
label values relacion_ci relacion*/

gen relacion_ci=1 if relhd==0
replace relacion_ci=2 if relhd==1
replace relacion_ci=3 if relhd==2 | relhd==3
replace relacion_ci=4 if relhd==4 
replace relacion_ci=5 if relhd==5 | relhd==8 | relhd==6
replace relacion_ci=. if relhd==9 /* No sabe */
label var relacion_ci "relación con el jefe de hogar"
label define relacion 1"Jefe" 2"Cónguye, Esposo/a, Compañero/a" 3"Hijo/a" 4"Otros parientes" 5"Otros no parientes" 6"Servicio doméstico" 
label values relacion_ci relacion

************************************
* DUMMY PARA NO MIEMBROS DEL HOGAR *
************************************
* Create a dummy indicating this person's income should NOT be included 
/*gen miembros_ci=1
replace miembros_ci=0 if  (relacion_ci==0 | relacion_ci==6 | relacion_ci==.)
replace miembros_ci=0 if factor_ci==.
label variable miembros_ci "Variable dummy que indica las personas que son miembros del Hogar"*/

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

***********
*  RAZA   *
***********
gen raza_ci=.
*replace raza_ci=1 if ethnic==2
*replace raza_ci=2 if ethnic==1
*replace raza_ci=3 if ethnic>=3 & ethnic!=9
label var raza_ci "raza del individuo"
label define raza 1"Indígena" 2"afro-descendiente" 3"Resto"
label values raza_ci raza

*******************
*  ESTADO CIVIL   *
*******************
/*gen civil_ci=.
replace civil_ci=1 if marstat==1
replace civil_ci=2 if marstat==2
replace civil_ci=3 if marstat==4 | marstat==5
replace civil_ci=4 if marstat==3
label var civil_ci "Estado civil del individuo"
label define civil 1"Soltero" 2"Unión formal o informal" 3"Divorciado o separado" 4"Viudo" 
label values civil_ci civil*/

gen civil_ci=.
replace civil_ci=1 if marstat==0
replace civil_ci=2 if marstat==1
replace civil_ci=3 if marstat==3 | marstat==4
replace civil_ci=4 if marstat==2
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
egen nmayor21_ch=sum((relacion_ci>0 & relacion_ci<5) & (age>=21)), by (idh_ch)
label variable nmayor21_ch "Numero de personas de 21 años o mas dentro del Hogar"

********************************************
*  MIEMBROS EN EL HOGAR MENORES DE 21 AÑOS *
********************************************
egen nmenor21_ch=sum((relacion_ci>0 & relacion_ci<5) & (age<21)), by (idh_ch)
label variable nmenor21_ch "Numero de personas menores a 21 años dentro del Hogar"

********************************************
*  MIEMBROS EN EL HOGAR MAYORES DE 65 AÑOS *
********************************************
egen nmayor65_ch=sum((relacion_ci>0 & relacion_ci<5) & (age>=65)), by (idh_ch)
label variable nmayor65_ch "Numero de personas de 65 años o mas dentro del Hogar"

********************************************
*  MIEMBROS EN EL HOGAR MENORES DE 65 AÑOS *
********************************************
*egen nmenor65_ch=sum((relacion_ci>0 & relacion_ci<5) & (age<=65)), by (idh_ch)
*label variable nmenor65_ch "Miembros de 65 años o menos dentro del Hogar"

********************************************
*  MIEMBROS EN EL HOGAR MENORES DE 6 AÑOS *
********************************************
egen nmenor6_ch=sum((relacion_ci>0 & relacion_ci<5) & (age<6)), by (idh_ch)
label variable nmenor6_ch "Miembros menores a 6 años dentro del Hogar"

******************************************
*  MIEMBROS EN EL HOGAR MENORES DE 1 AÑO *
******************************************
egen nmenor1_ch=sum((relacion_ci>0 & relacion_ci<5) & (age<1)),  by (idh_ch)
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
/*Ocupado*
gen condocup_ci=1 if actvstat==10
*Desocupado*
replace condocup_ci=2 if actvstat==20
*Inactivo*
replace condocup_ci=3 if (condocup_ci ~=1 & condocup_ci ~=2)
*menores que PET
replace condocup_ci=4 if age<15
label define condocup 1"Ocupado" 2"Desocupado" 3"Inactivo" 4"Menores de 15 años"
label values condocup_ci condocup
label var condocup_ci "Condición de ocupación"*/

* Nota MGD 09/15/2014: no se recolecta informacion sobre los menores de edad en la encuesta.
g condocup_ci=.
replace condocup_ci=1 if actvstat==10 | (reasab>=1 & reasab<=3)
replace condocup_ci=2 if condocup_ci!=1 & ((seeking==1 | (method>=1 & method<=6) | lstlook==2) & willing==1)
replace condocup_ci=3 if (condocup_ci~=1 & condocup_ci~=2) & edad>=10
recode condocup_ci (.=4) if edad<10
label define condocup 1"Ocupado" 2"Desocupado" 3"Inactivo" 4"Menores de 15 años"
label values condocup_ci condocup
label var condocup_ci "Condición de ocupación"

**************************
* CATEGORIA DE INACTIVIDAD  *
**************************
*Jubilados, pensionados
gen categoinac_ci=1 if majactv==3 & condocup_ci==3
label var  categoinac_ci "Condición de Inactividad" 
*Estudiantes
replace categoinac_ci=2 if majactv==2 & condocup_ci==3
*Quehaceres del Hogar
replace categoinac_ci=3 if majactv==1 & condocup_ci==3
*Otra razon
replace categoinac_ci=4 if (majactv==4 | majactv==6 | majactv==9) & condocup_ci==3
label define inactivo 1"Hogar o Pensionado" 2"Estudiante" 3"Hogar" 4"Otros"
label values categoinac_ci inactivo



**********************
*  NÚMERO DE EMPLEOS *
**********************
gen nempleos_ci=.
replace nempleos_ci=1 if condocup==1 & twojobs==2
replace nempleos_ci=2 if condocup==1 & twojobs==1
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
/*gen antiguedad_ci=.
replace  antiguedad_ci=(1+5)/2 if exprnce==1
replace  antiguedad_ci=(10+6)/2 if exprnce==2
replace  antiguedad_ci=(11+15)/2 if exprnce==3
replace  antiguedad_ci=(16+20)/2 if exprnce==4
replace  antiguedad_ci=25 if exprnce==5
label var antiguedad_ci "Años de trabajo en la actividad principal"*/

gen antiguedad_ci=.
replace  antiguedad_ci=0.5 if exprnce==0
replace  antiguedad_ci=(1+5)/2 if exprnce==1
replace  antiguedad_ci=(10+6)/2 if exprnce==2
replace  antiguedad_ci=(11+15)/2 if exprnce==3
replace  antiguedad_ci=(16+20)/2 if exprnce==4
replace  antiguedad_ci=(21+25)/2 if exprnce==5
label var antiguedad_ci "Años de trabajo en la actividad principal"


***************
* DESOCUPADO  *
***************
gen desemp_ci=0 
replace desemp_ci=1 if condocup_ci==2
label var desemp_ci "Desocupado"
label define desocupado 1"Desocupado" 0"No desocupado"  
label values desemp_ci desocupado

***********
* CESANTE *
***********
/*gen cesante_ci=1 if condocup==2 & everwrkd ==1
replace cesante_ci=0 if condocup==2 | everwrkd ==2 
label var cesante_ci "Cesante"*/

gen cesante_ci=0 if condocup_ci==2
replace cesante_ci=1 if everwrkd==1 & condocup_ci==2
label var cesante_ci "Cesante"

***********************************
* DURACION DEL DESEMPLEO EN MESES *
***********************************
/*gen durades_ci=lstwrkd
label var durades_ci "Duración de desempleo o búsqueda de empleo"*/

gen durades_ci=0.5 if lstwrkd==1
replace durades_ci=2 if lstwrkd==2
replace durades_ci=4.5 if lstwrkd==3
replace durades_ci=9 if lstwrkd==4
replace durades_ci=18 if lstwrkd==5
replace durades_ci=30 if lstwrkd==6 | lstwrkd==7
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
/*gen desalent_ci=.
replace desalent_ci=0 if condocup_ci==2 | condocup_ci==1
replace desalent_ci=1 if reasnsk==2
label var desalent_ci "Trabajadores desalentados, personas que creen que por alguna razon no conseguiran trabajo" */

gen desalent_ci=0 if condocup_ci==3
replace desalent_ci=1 if (reasnsk==2 | reasnsk==3) & condocup_ci==3
label var desalent_ci "Trabajadores desalentados, personas que creen que por alguna razon no conseguiran trabajo" 


*****************************
* TRABAJA MENOS DE 30 HORAS *
*****************************
/*gen subemp_ci=.
replace subemp_ci=1 if hrswrkd<=7
replace subemp_ci=0 if hrswrkd>7
label var subemp_ci "Trabaja menos de 30 horas"*/

* MGD 08/29/2014: no hay la pregunta de si desea trabajar mas horas, pero se utiliza disponibilidad para otro trabajo.
gen subemp_ci=0 
replace subemp_ci=1 if nhrswrkd>=1 & nhrswrkd<=6 & addwrk==1 & condocup_ci==1
label var subemp_ci "Trabaja menos de 30 horas"


****************************************************
* TRABAJA MENOS DE 30 HORAS Y NO DESEA TRABAJAR MAS*
****************************************************
/*gen tiempoparc_ci=.
replace tiempoparc_ci=0 if subemp_ci==1
replace tiempoparc=1 if subemp_ci==1 &  addwrk==2
label var tiempoparc_ci "Trabaja menos de 30 horas y no desea trabajar más"*/

* MGD 08/29/2014: no hay la pregunta de si desea trabajar mas horas, pero se utiliza disponibilidad para otro trabajo.
gen tiempoparc_ci=0 
replace tiempoparc_ci=1 if nhrswrkd>=1 & nhrswrkd<=6 & addwrk==2 & condocup_ci==1
label var tiempoparc_ci "Trabaja menos de 30 horas"


*********************************
* CATEGORIA OCUPACION PRINCIPAL *
*********************************
gen categopri_ci=.
replace categopri_ci=1 if emplstat==1 & emp_ci==1
replace categopri_ci=2 if emplstat==4 & emp_ci==1
replace categopri_ci=3 if (emplstat==2 | emplstat==3 | emplstat==6) & emp_ci==1 
replace categopri_ci=4 if emplstat==5 & emp_ci==1
label var categopri_ci "Categoría ocupación principal"
label define categopri 1"Patrón o empleador" 2"Cuenta propia o independiente" 3"Empleado o asalariado" 4"Trabajador no remunerado"  
label values categopri_ci categopri

*********************************
* CATEGORIA OCUPACION SECUNDARIA*
*********************************
gen categosec_ci=.
replace categosec_ci=1 if emplsta2==1 & emp_ci==1
replace categosec_ci=2 if emplsta2==4 & emp_ci==1
replace categosec_ci=3 if (emplsta2==2 | emplsta2==3 | emplsta2==6) & emp_ci==1
replace categosec_ci=4 if emplsta2==5 & emp_ci==1
label var categosec_ci "Categoría ocupación secundaria"
label define categosec 1"Patrón o empleador" 2"Cuenta propia o independiente" 3"Empleado o asalariado" 4"Trabajador no remunerado"  
label values categosec_ci categosec

*********************************
*  RAMA DE ACTIVIDAD PRINCIPAL  *
*********************************
gen rama_ci=.
label var rama_ci "Rama de actividad principal"
*replace rama_ci=1 if mwrktype>=0 & mwrktype<1000 /* indmisic for 2004 */
*replace rama_ci=2 if mwrktype>=1000 & mwrktype<2000
*replace rama_ci=3 if mwrktype>=2000 & mwrktype<4000
*replace rama_ci=4 if mwrktype>=4000 & mwrktype<5000
*replace rama_ci=5 if mwrktype>=5000 & mwrktype<6000
*replace rama_ci=6 if mwrktype>=6000 & mwrktype<7000
*replace rama_ci=7 if mwrktype>=7000 & mwrktype<8000
*replace rama_ci=8 if mwrktype>=8000 & mwrktype<9000
*replace rama_ci=9 if mwrktype>=9000 & mwrktype<10000
label define rama 1"Agricultura, caza, silvicultura o pesca" 2"Minas y Canteras" 3"Manufactura" 4"Electricidad, gas o agua" 5"Construcción" 6"Comercio al por mayor, restaurantes o hoteles" 7"Transporte o almacenamiento" 8"Establecimientos financieros, seguros o bienes inmuebles" 9"Servicios sociales, comunales o personales" 
label values rama_ci rama

*********************************
*  TRABAJA EN EL SECTOR PUBLICO *
*********************************
gen spublico_ci=0 
replace spublico_ci=1 if emplstat==2 & condocup_ci==1
label var spublico_ci "Personas que trabajan en el sector publico"
 
 
********************
* TAMAÑO DE EMPRESA*
********************
gen tamemp_ci=.
label var  tamemp_ci "Tamaño de Empresa" 
label define tamaño 1"Pequeña" 2"Mediana" 3"Grande"
label values tamemp_ci tamaño

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
label var afiliado_ci "Afiliado a la seguridad social"

*********************
* TRABAJADOR FORMAL *
*********************
gen formal_ci=.
replace formal_ci=0 if condocup_ci==1 & afiliado_ci==0 & cotizando_ci==0
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
replace ocupa_ci=1 if (occup>=2000 & occup<=3999) & emp_ci==1
replace ocupa_ci=2 if (occup>=1000 & occup<=1999) & emp_ci==1
replace ocupa_ci=3 if (occup>=4000 & occup<=4999) & emp_ci==1
replace ocupa_ci=4 if ((occup>=5200 & occup<=5999) | (occup>=9111 & occup<=9113)) & emp_ci==1
replace ocupa_ci=5 if ((occup>=5000 & occup<=5199) | (occup>=9120 & occup<=9162)) & emp_ci==1
replace ocupa_ci=6 if ((occup>=6000 & occup<=6999) | (occup>=9200 & occup<=9213)) & emp_ci==1
replace ocupa_ci=7 if ((occup>=7000 & occup<=8999) | (occup>=9300 & occup<=9333))& emp_ci==1
replace ocupa_ci=8 if (occup>=0 & occup<=999)  & emp_ci==1
replace ocupa_ci=9 if (occup==9119 | occup==9999) & emp_ci==1
label var ocupa_ci "Tipo de ocupacion laboral"
label define ocupa 1"Profesional o técnico" 2"Director o funcionario superior" 3"Personal administrativo o nivel intermedio" 4"Comerciante o vendedor" 5"Trabajador en servicios" 6"Trabajador agrícola o afines" 7"Obrero no agrícola, conductores de máquinas y vehículos de transporte y similares" 8"Fuerzas armadas" 9"Otras ocupaciones no clasificadas"
label values ocupa_ci ocupa


**********************************************
* HORAS TRABAJADAS EN LA ACTIVIDAD PRINCIPAL *
**********************************************
gen horaspri_ci=.
replace horaspri_ci=0 if nhrswrkd==1
replace horaspri_ci=2.5 if nhrswrkd==2
replace horaspri_ci=(5+9)/2 if nhrswrkd==3
replace horaspri_ci=(10+14)/2 if nhrswrkd==4
replace horaspri_ci=(15+19)/2 if nhrswrkd==5
replace horaspri_ci=(20+24)/2 if nhrswrkd==6
replace horaspri_ci=(25+29)/2 if nhrswrkd==7
replace horaspri_ci=(30+34)/2 if nhrswrkd==8
replace horaspri_ci=(35+39)/2 if nhrswrkd==9
replace horaspri_ci=(40+44)/2 if nhrswrkd==10
replace horaspri_ci=(45+49)/2 if nhrswrkd==11
label var horaspri_ci "Horas trabajadas en la actividad principal"

**************************
* TOTAL HORAS TRABAJADAS *
**************************
gen horastot_ci=horaspri_ci

***********************************************
* RECIBE PENSION O JUBILACION NO CONTRIBUTIVA *
***********************************************
gen pensionsub_ci=.
*replace pensionsub_ci=1 if
label var pensionsub_ci "Recibe pensión o jubilación NO contributiva"

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


g tipopen_ci=.


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
gen byte nrylmpri_ci=.
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
/*gen ylmpri_ci= earngs*4
label var ylmpri_ci "Monto mensual de ingreso laboral de la actividad principal"*/

gen ylmpri_ci= ((1+200)/2)*4.3 if earngs==1 & emp_ci==1
replace ylmpri_ci=((200+299)/2)*4.3 if earngs==2 & emp_ci==1
replace ylmpri_ci=((300+399)/2)*4.3 if earngs==3 & emp_ci==1
replace ylmpri_ci=((400+499)/2)*4.3 if earngs==4 & emp_ci==1
replace ylmpri_ci=((500+599)/2)*4.3 if earngs==5 & emp_ci==1
replace ylmpri_ci=((600+699)/2)*4.3 if earngs==6 & emp_ci==1
replace ylmpri_ci=((700+799)/2)*4.3 if earngs==7 & emp_ci==1
replace ylmpri_ci=((800+899)/2)*4.3 if earngs==8 & emp_ci==1
replace ylmpri_ci=((900+999)/2)*4.3 if earngs==9 & emp_ci==1
replace ylmpri_ci=((1000+1300)/2)*4.3 if earngs==10 & emp_ci==1
replace ylmpri_ci=((1300+1600)/2)*4.3 if earngs==11 & emp_ci==1

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
label var ylm_ci "Ingreso mensual todas actividades"

*************************************************
* INGRESO MENSUAL NO MONETARIO TODAS ACTIVIDADES*
*************************************************
gen ylnm_ci= ylnmpri_ci + ylnmsec_ci + ylnmotros_ci
label var ylnm_ci "Ingreso mensual NO monetario todas actividades"

*************************************************
* INGRESO MENSUAL NO LABORAL OTRAS ACTIVIDADES  *
*************************************************
gen ynlm_ci=. 
*replace ynlm_ci= if apgvthlp==1
label var ynlm_ci "Ingreso mensual NO laboral otras actividades"

**************************************************************
* INGRESO MENSUAL NO LABORAL NO MONETARIO OTRAS ACTIVIDADES  *
**************************************************************
gen ynlnm_ci= .
label var ynlnm_ci "Ingreso mensual NO laboral NO monetario otras actividades"

************************************
* INGRESO MENSUAL LABORAL DEL HOGAR*
************************************
gen ylm_ch=.
by idh_ch: replace ylm_ch=sum(ylm_ci) if miembros_ci==1 
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
replace ylmhopri_ci= earngs/hrswrkd 
label var ylmhopri_ci "Salario horario monetario de la actividad principal"

*****************************************************
* INGRESO LABORAL POR HORA EN TODAS LAS ACTIVIDADES *
*****************************************************
gen ylmho_ci=.
replace ylmho_ci=ylmhopri_ci
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
* REMESAS EN MONEDA LOCAL *
***************************
gen remesas_ci=.
label var remesas_ci "Remesas en moneda local"

************************************
* REMESES EN MONEDA LOCAL DEL HOGAR*
************************************
by idh_ch: gen remesas_ch=sum(remesas_ci) if miembros_ci==1
label var remesas_ch "Remesas en moneda local"

************************************
* INGRESO POR PENSION CONTRIBUTIVA *
************************************
gen ypen_ci=.
label var ypen_ci "Ingreso por pension contributiva"

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

********************************************
* LINEA DE POBREZA 2.5 DOLARES MONEDA LOCAL*
********************************************
gen lp25_ci =.
label var lp25_ci "Línea de pobreza 2.5 dolares en moneda local"

********************************************
* LINEA DE POBREZA 4 DOLARES MONEDA LOCAL*
********************************************
gen lp4_ci =.
label var lp4_ci "Línea de pobreza 4 dolares en moneda local"

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
gen aedu_ci =.
replace aedu_ci=0 if educlev==0
replace aedu_ci=8 if educlev==1
replace aedu_ci=14 if educlev==2
replace aedu_ci=18 if educlev==3
replace aedu_ci=16 if educlev==4
label var aedu_ci "número de años de educación culminados"

******************************************
*  NO TIENE NINGUN NIVEL DE INSTRUCCION  *
******************************************
gen eduno_ci=.
replace eduno_ci=1 if educlev==0 
replace eduno_ci=0 if educlev>=1 & educlev<9
label var eduno_ci "No tiene ningún nivel de instrucción"

******************************************
* NO HA COMPLETADO LA EDUCACION PRIMARIA *
******************************************
gen edupi_ci=.
replace edupi_ci=1 if educlev==0
replace edupi_ci=0 if educlev>=1 & educlev<9
label var edupi_ci "No ha completado la educación primaria"

******************************************
*  HA COMPLETADO LA EDUCACION PRIMARIA   *
******************************************
gen edupc_ci=.
replace edupc_ci=1 if educlev>=1 & educlev<9
replace edupc_ci=0 if educlev==0
label var edupc_ci "Ha completado la educación primaria"


******************************************
*NO HA COMPLETADO LA EDUCACION SECUNDARIA*
******************************************
gen edusi_ci=.
replace edusi_ci=1 if aedu_ci<14 & aedu_ci!=.
replace edusi_ci=0 if aedu_ci>=14 & aedu_ci!=.
label var edusi_ci "No ha completado la educación secundaria"

******************************************
* HA COMPLETADO LA EDUCACION SECUNDARIA  *
******************************************
gen edusc_ci =. 
replace edusc_ci=1 if aedu_ci>=14 & aedu_ci!=.
replace edusc_ci=0 if aedu_ci<14 & aedu_ci!=.
label var edusc_ci "Ha completado la educación secundaria"

*******************************************
* NO HA COMPLETADO LA EDUCACION TERCIARIA *
*******************************************
gen eduui_ci=. 
replace eduui_ci=1 if aedu_ci<18 & aedu_ci!=.
replace eduui_ci=0 if aedu_ci>=18 & aedu_ci!=.
label var eduui_ci "No ha completado la educación terciaria"

*******************************************
*  HA COMPLETADO LA EDUCACION TERCIARIA   *
*******************************************
gen eduuc_ci=.
replace eduuc_ci=1 if aedu_ci>=18 & aedu_ci!=.
replace eduuc_ci=0 if aedu_ci<18 & aedu_ci!=.
label var eduuc_ci "Ha completado la educación terciaria"

**************************************************
* NO HA COMPLETADO EL PRIMER CICLO DE SECUNDARIA *
**************************************************
gen edus1i_ci=. 
*replace edus1i_ci=1 if 
label var edus1i_ci "No ha completado el primer ciclo de la secundaria"

**************************************************
*  HA COMPLETADO EL PRIMER CICLO DE SECUNDARIA   *
**************************************************
gen edus1c_ci =. 
*replace edus1c_ci=1 if 
label var edus1c_ci "Ha completado el primer ciclo de la secundaria"

**************************************************
* NO HA COMPLETADO EL SEGUNDO CICLO DE SECUNDARIA *
**************************************************
gen edus2i_ci =.
*replace edus2i_ci=1 if 
label var edus2i_ci "No ha completado el segundo ciclo de la secundaria"

**************************************************
*  HA COMPLETADO EL SEGUNDO CICLO DE SECUNDARIA  *
**************************************************
gen edus2c_ci=. 
*replace edus2c_ci=1 if 
label var edus2c_ci "Ha completado el segundo ciclo de la secundaria"

****************************************
*  HA COMPLETADO EDUCACION PREESCOLAR  *
****************************************
gen edupre_ci=. 
*replace edupre_ci=0 if
label var edupre_ci "Ha completado educación preescolar"

************************************************
*  HA COMPLETADO EDUCACION TERCIARIA ACADEMICA *
************************************************
gen eduac_ci=.
replace eduac_ci=1 if educlev==3 | educlev==5
replace eduac_ci=0 if educlev==4
label var eduac_ci "Ha completado educación terciaria académica"

************************************
*  ASISTE A UN CENTRO DE ENSEÑANZA *
************************************
gen asiste_ci=.
*replace asiste_ci=1 if educlev==3 | educlev==5
label var asiste_ci "Asiste a algún centro de enseñanza"

*********************************************
* PORQUE NO ASISTE A UN CENTRO DE ENSEÑANZA *
*********************************************
gen pqnoasis_ci=.
*replace pqnoasis_ci=1 if wnattsch
label var pqnoasis_ci "Porque no asiste a algún centro de enseñanza"
label define pqnoasis 1"Muy joven" 2"Razones financieras" 3"Trabaja en casa o negocio familiar" 4"Distancia a la escuela/transporte" 5"Enfermedad/inhabilidad" 6"falta de especio en la escuela" 7"Otra" 9"NS/NR"  
label values pqnoasis_ci pqnoasis


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
*replace edupub_ci=1 if 
label var edupub_ci "Asiste a centro de enseñanza pública"
label define edupub 1"Pública" 0"Privada"  
label values edupub_ci edupub

**************************
*  TIENE CARRERA TECNICA *
**************************
gen tecnica_ci=.
replace tecnica_ci=1 if educlev==4
replace tecnica_ci=0 if educlev!=4 & educlev!=9
label var tecnica_ci "Tiene carrera técnica"





*******************************
*******************************
*******************************
*    VARIABLES DE VIVIENDA    *
*******************************
*******************************
*******************************


**************************
*  ACCEDE A AGUA POR RED *
**************************
gen aguared_ch=.
*replace aguared_ch=1 if watersc<=3 | watersc==5 
label var tecnica_ci "Tiene acceso a agua por red"

***********************************
*  UBICACION DE LA FUENTE DE AGUA *
***********************************

gen aguadist_ch=.
*replace aguadist_ch=1 if aguared_ch==1 /* Adentro de la casa */
*replace aguadist_ch=2 if water==4 | water==2 /* Afuera de la casa pero adentro del terrno (o a menos de 1000mts de distancia) */
*replace aguadist_ch=3 if distwate>=1 & distwate!=99 /* Afuera de la casa y afuera del terreno (o a más de 1000mts de distancia) */
label var aguadist_ch "Ubicación de la fuente de agua"
label define aguadist 1"Adentro de la vivienda" 2"Fuera de la vivienda pero dentro del terreno" 3"Fuera de la vivienda y fuera del terreno"
label values aguadist_ch aguadist

********************************
*  FUENTE DE AGUA "Unimproved" *
********************************
gen aguamala_ch=.
* river/ stream / creek / pond / spring *
*replace aguamala_ch=1 if watersc==7 | watersc==8 
*replace aguamala_ch=0 if watersc<=6
label var aguamala_ch "Fuente de agua es Unimproved"

************************
*  USA MEDIDOR DE AGUA *
************************
gen aguamide_ch=.
*replace aguamide_ch=1 if
label var aguamide_ch "Usa medidor de agua para pagar por su consumo"

*****************************
*  ILUMINACION ES ELÉCTRICA *
*****************************
gen luz_ch=.
*replace luz_ch=1 if light==1
*replace luz_ch=0 if light!=1
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
gen combust_ch=.
*replace combust_ch=1 if
label var combust_ch "Usa combustible como fuente de energía"

****************
*  TIENE BAÑO  *
****************
gen bano_ch=.
label var bano_ch "Tiene baño, inodoro, letrina o pozo ciego"

*********************************
*  TIENE BAÑO DE USO EXCLUSIVO  *
*********************************
gen banoex_ch=.
label var banoex_ch "Tiene baño, inodoro, letrina o pozo ciego de uso exclusivo del hogar"

*******************************************
*  TIPO DE DESAGÜE incluyendo Unimproved  *
*******************************************
gen des1_ch=.
label var des1_ch "Tipo de desague incluyendo Unimproved"
label define des1 0"El hogar no tiene servicio higienico" 1"Desagüe conectado a la red general" 2"Desagüe conectado a un pozo o letrina" 3"El desagüe se comunica con la superficie"
label values des1_ch des1

*******************************************
* TIPO DE DESAGÜE sin incluir Unimproved  *
*******************************************
gen des2_ch=.
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
label var resid_ch "Metodo de eliminacion de residuos"
label define resid 0"Recolección pública o privada" 1"Quemados o enterrados" 2"Tirados en un espacio abierto" 3"Otros"
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
label define cocina 1"Si" 2"No" 9"NS/NR"
label values cocina_ch cocina
label var cocina_ch "Cuarto exclusivo a la cocina"

*************************
*  TIENE TELEFONO FIJO  *
*************************
gen telef_ch=.
label var telef_ch "Tiene teléfono fijo"

***********************************
*  TIENE HELADERA O REFRIGERADOR  *
***********************************
gen refrig_ch=.
label var refrig_ch "Tiene heladera o refrigerador"

********************************
*  TIENE FREEZER O CONGELADOR  *
********************************
gen freez_ch=.
label var freez_ch "Tiene freezer o congelador"

*********************
*  TIENE AUTOMOVIL  *
*********************
gen auto_ch=.
label var auto_ch "Tiene automovil"

*********************
*  TIENE COMPUTADOR  *
*********************
gen compu_ch=.
*replace compu_ch=1 if 
*replace compu_ch=0 if 
label var compu_ch "Tiene computador"

*******************************
*  TIENE CONEXION A INTERNET  *
*******************************
gen internet_ch=.
*replace internet_ch=1 if 
*replace internet_ch=0 if 
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
label var vivi2_ch "Casa o departamento"

********************
*  VIVIENDA PROPIA *
********************
gen viviprop_ch=.
label var viviprop_ch "Vivienda propia"
label define viviprop 0"Arrendada" 1"Propia" 2"Otra"
label values viviprop_ch viviprop

********************************
*  POSEE TITULO DE PROPIEDAD   *
********************************
gen vivitit_ch=.
label var vivitit_ch "El hogar posee un título de propiedad"

********************************
*  MONTO DE PAGO POR ALQUILER  *
********************************
gen vivialq_ch=.
label var vivialq_ch "Monto pagado por el alquiler"

***********************************
*  VALOR ESTIMADO DE LA VIVIENDA  *
***********************************
gen vivialqimp_ch=.
label var vivialqimp_ch "Monto ud cree le pagarían por su vivienda"


order region_BID_c region_c pais_c anio_c  zona_c factor_ch	idh_ch	idp_ci	factor_ci sexo_ci edad_ci ///
raza_ci relacion_ci civil_ci jefe_ci nconyuges_ch nhijos_ch	notropari_ch notronopari_ch	nempdom_ch ///
clasehog_ch nmiembros_ch miembros_ci nmayor21_ch nmenor21_ch nmayor65_ch nmenor6_ch	nmenor1_ch	condocup_ci ///
categoinac_ci nempleos_ci emp_ci antiguedad_ci	desemp_ci cesante_ci durades_ci	pea_ci desalent_ci subemp_ci ///
tiempoparc_ci categopri_ci categosec_ci rama_ci spublico_ci tamemp_ci cotizando_ci 	afiliado_ci ///
formal_ci tipocontrato_ci ocupa_ci horaspri_ci horastot_ci	pensionsub_ci pension_ci tipopen_ci instpen_ci	ylmpri_ci nrylmpri_ci ///
ylnmpri_ci ylmsec_ci ylnmsec_ci	ylmotros_ci	ylnmotros_ci ylm_ci	ylnm_ci	ynlm_ci	ynlnm_ci ylm_ch	ylnm_ch	ylmnr_ch  ///
ynlm_ch	ynlnm_ch ylmhopri_ci ylmho_ci rentaimp_ch autocons_ci autocons_ch nrylmpri_ch remesas_ci remesas_ch	ypen_ci	ypensub_ci ///
salmm_ci lp25_ci lp4_ci	lp_ci lpe_ci aedu_ci eduno_ci edupi_ci edupc_ci	edusi_ci edusc_ci eduui_ci eduuc_ci	 ///
edus2i_ci edus2c_ci edupre_ci eduac_ci asiste_ci pqnoasis_ci	repite_ci repiteult_ci edupub_ci tecnica_ci ///
aguared_ch aguadist_ch aguamala_ch aguamide_ch luz_ch luzmide_ch combust_ch	bano_ch banoex_ch des1_ch des2_ch piso_ch ///
pared_ch techo_ch resid_ch dorm_ch cuartos_ch cocina_ch telef_ch refrig_ch freez_ch auto_ch compu_ch internet_ch cel_ch ///
vivi1_ch vivi2_ch viviprop_ch vivitit_ch vivialq_ch	vivialqimp_ch , first

saveold "`base_out'", replace
