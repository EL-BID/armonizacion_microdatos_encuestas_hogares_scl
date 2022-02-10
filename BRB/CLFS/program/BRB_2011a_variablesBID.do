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
local ANO "2011"
local ronda a

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"

capture log close
log using "`log_file'", replace 
   

/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Barbados
Encuesta: LFS
Round: 
Autores: Melany Gualavisi melanyg@iadb.org


							SCL/LMK - IADB
****************************************************************************/
/***************************************************************************
Detalle de procesamientos o modificaciones anteriores:

****************************************************************************/


use `base_in', clear

**********************
* AÑO DE LA ENCUESTA *
**********************
gen anio_c=2011
label variable anio_c "Año de la Encuesta"

**********************
* MES DE LA ENCUESTA *
**********************
gen mes_c=.
*label variable mes_c "Mes de la Encuesta"

*************************
* FACTORES DE EXPANSION *
*************************
sum wtfactor
scalar pob=r(sum)
gen pop=wtfactor*(277000/pob)
sum pop
ret list
gen factor_ch=pop 
drop pop

label var factor_ch "Factor de Expansion del Hogar"
gen factor_ci=  factor_ch
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

tostring hhno, replace
gen hh_id = string(real(hhno),"%03.0f")

egen idh_ch= concat(edno hh_id rndno)
destring idh_ch, replace
sort idh

label var idh_ch "Identificador Unico del Hogar"

*******************************
* IDENTIFICADOR DEL INDIVIDUO *
*******************************
egen idp_ci= concat(idh_ch indivno)
destring idp_ci, replace
sort idp_ci

label var idp_ci "Identificador Individual dentro del Hogar"

************************************
*  RELACION CON EL JEFE DE HOGAR   *
************************************
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

*******************
*  ESTADO CIVIL   *
*******************
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
label define jefe 1"Jefe de Hogar" 0"Otro" 
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
*  MIEMBROS EN EL HOGAR MENORES DE 6 AÑOS *
********************************************
* No hay menores de 7 años en la encuesta
gen nmenor6_ch=.
label variable nmenor6_ch "Miembros menores a 6 años dentro del Hogar"


******************************************
*  MIEMBROS EN EL HOGAR MENORES DE 1 AÑO *
******************************************
* No hay menores de 7 años en la encuesta
gen nmenor1_ch=.
label variable nmenor1_ch "Miembros menores a 1 año dentro del Hogar"


********************************************
*  MIEMBROS EN EL HOGAR MENORES DE 15 AÑOS *
********************************************
* No se encuesta a menores de edad.
egen nchild=rsum(mchildrn fchildrn)

egen nmenor15_ch=sum(nchild), by (idh_ch)



			
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
* Nota MGD 1/14/2015: no se recolecta informacion sobre los menores de edad en la encuesta.
g condocup_ci=.
replace condocup_ci=1 if actvstat==10
replace condocup_ci=2 if (worked==2 & condocup_ci!=1) & ((seeking==1 | lstlook==2) & willing==1)
replace condocup_ci=3 if (condocup_ci~=1 & condocup_ci~=2) & edad>=15
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
label define inactivo 1"Jubilados o Pensionado" 2"Estudiante" 3"Hogar" 4"Otros"
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
gen cesante_ci=0 if condocup_ci==2
replace cesante_ci=1 if everwrkd==1 & condocup_ci==2
label var cesante_ci "Cesante"

***********************************
* DURACION DEL DESEMPLEO EN MESES *
***********************************

gen durades_ci=0.5 if frstlook==1
replace durades_ci=2 if frstlook==2
replace durades_ci=4.5 if frstlook==3
replace durades_ci=9 if frstlook==4
replace durades_ci=18 if frstlook==5
replace durades_ci=30 if frstlook==6 
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

* MGD 08/29/2014: no hay la pregunta de si desea trabajar mas horas, pero se utiliza disponibilidad para otro trabajo.
gen subemp_ci=0 
replace subemp_ci=1 if nhrswrkd>=1 & nhrswrkd<=7 & addwrk==1 & condocup_ci==1
label var subemp_ci "Trabaja menos de 30 horas"


****************************************************
* TRABAJA MENOS DE 30 HORAS Y NO DESEA TRABAJAR MAS*
****************************************************
* MGD 08/29/2014: no hay la pregunta de si desea trabajar mas horas, pero se utiliza disponibilidad para otro trabajo.
gen tiempoparc_ci=0 
replace tiempoparc_ci=1 if nhrswrkd>=1 & nhrswrkd<=7 & addwrk==2 & condocup_ci==1
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
* Nota MGD 01/2015: no es posible categorizar la varibale en las desagregaciones armonizadas, por eso se genera una alternativa.
gen rama_ci=.
label var rama_ci "Rama de actividad principal"
label define rama 1"Agricultura, caza, silvicultura o pesca" 2"Minas y Canteras" 3"Manufactura" 4"Electricidad, gas o agua" 5"Construcción" 6"Comercio al por mayor, restaurantes o hoteles" 7"Transporte o almacenamiento" 8"Establecimientos financieros, seguros o bienes inmuebles" 9"Servicios sociales, comunales o personales" 
label values rama_ci rama

g rama_ci1=.
replace rama_ci1=1 if (indus>=1 & indus<=3) & condocup_ci==1
replace rama_ci1=2 if ((indus>=6 & indus<=9)) & condocup_ci==1
replace rama_ci1=3 if (indus>=10 & indus<=32)  & condocup_ci==1
replace rama_ci1=4 if (indus>=35 & indus<=39)   & condocup_ci==1
replace rama_ci1=5 if (indus>=41 & indus<=43)  & condocup_ci==1
replace rama_ci1=6 if ((indus>=45 & indus<=47)  | (indus>=55 & indus<=56))  & condocup_ci==1
replace rama_ci1=7 if ((indus>=49 & indus<=53) | (indus>=58 & indus<=63)) & condocup_ci==1
replace rama_ci1=8 if (indus>=64 & indus<=68)  & condocup_ci==1
replace rama_ci1=9 if (indus>=69 & indus<=99) & condocup_ci==1

label define rama_ci1 1"Agricultura" 2"Explotación de minas y canteras" 3"Industrias manufactureras" 4"Electricidad, gas y agua" 5"Construcción" 6"Comercio, rest. y hoteles" 7"Transporte y comunicaciones" 8"Establecimientos financieros" 9 "Servicios sociales, comunales y personales"
label values rama_ci1 rama_ci1

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
* MGD 001/2015: CIUO-88
gen ocupa_ci=.
replace ocupa_ci=1 if (occup>=2000 & occup<=3999) & emp_ci==1
replace ocupa_ci=2 if (occup>=1000 & occup<=1999) & emp_ci==1
replace ocupa_ci=3 if (occup>=4000 & occup<=4999) & emp_ci==1
replace ocupa_ci=4 if ((occup>=5200 & occup<=5999) | (occup>=9111 & occup<=9113)) & emp_ci==1
replace ocupa_ci=5 if ((occup>=5000 & occup<=5199) | (occup>=9120 & occup<=9162)) & emp_ci==1
replace ocupa_ci=6 if ((occup>=6000 & occup<=6999) | (occup>=9200 & occup<=9213)) & emp_ci==1
replace ocupa_ci=7 if ((occup>=7000 & occup<=8999) | (occup>=9300 & occup<=9333))& emp_ci==1
replace ocupa_ci=9 if (occup==9999) & emp_ci==1
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
label var pensionsub_ci "Recibe pensión o jubilación NO contributiva"

********************************************
* RECIBE PENSION O JUBILACION CONTRIBUTIVA *
********************************************
gen pension_ci=.
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

/*
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
*/

*************************************
* INGRESO MONETARIO MENSUAL LABORAL *
*************************************
* Nota MGD 01/15: El ingreso se reporta por rangos establecidos en la encuesta,no es variable continua.
gen ylmpri_ci= 0 if earngs==0 & emp_ci==1
replace ylmpri_ci=((0+200)/2)*4.3 if earngs==1 & emp_ci==1
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
bys idh_ch: replace ylm_ch=sum(ylm_ci) if miembros_ci==1 
label var ylm_ch "Ingreso Laboral Monetario del Hogar (Bruto)"

**************************************************
* INGRESO MENSUAL LABORAL NO MONETARIO DEL HOGAR *
**************************************************
egen ylnm_ch=sum(ylnm_ci) if miembros_ci==1, by(idh_ch)
label var ylnm_ch "Ingreso Laboral No Monetario del Hogar"

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
*Nota: numero de años-->  http://www.classbase.com/countries/Barbados/Education-System
gen aedu_ci =.
replace aedu_ci=0 if educlev==0
replace aedu_ci=6 if educlev==1
replace aedu_ci=11 if educlev==2
replace aedu_ci=16 if educlev==4
replace aedu_ci=13 if educlev==3
label var aedu_ci "número de años de educación culminados"

******************************************
*  NO TIENE NINGUN NIVEL DE INSTRUCCION  *
******************************************
gen eduno_ci=.
replace eduno_ci=1 if educlev==0 
replace eduno_ci=0 if educlev>=1 & educlev<5
label var eduno_ci "No tiene ningún nivel de instrucción"

******************************************
* NO HA COMPLETADO LA EDUCACION PRIMARIA *
******************************************
gen edupi_ci=.
/*replace edupi_ci=1 if educlev==0
replace edupi_ci=0 if educlev>=1 & educlev<9*/
label var edupi_ci "No ha completado la educación primaria"

******************************************
*  HA COMPLETADO LA EDUCACION PRIMARIA   *
******************************************
g edupc_ci=((educlev>=1 & educlev<=4) & educlev!=. & educlev!=9)
label var edupc_ci "Ha completado la educación primaria"


******************************************
*NO HA COMPLETADO LA EDUCACION SECUNDARIA*
******************************************
gen edusi_ci=.
/*replace edusi_ci=1 if aedu_ci<14 & aedu_ci!=.
replace edusi_ci=0 if aedu_ci>=14 & aedu_ci!=.*/
label var edusi_ci "No ha completado la educación secundaria"

******************************************
* HA COMPLETADO LA EDUCACION SECUNDARIA  *
******************************************
g edusc_ci=((educlev>=2 & educlev<=4) & educlev!=. & educlev!=9)
label var edusc_ci "Ha completado la educación secundaria"

*******************************************
* NO HA COMPLETADO LA EDUCACION TERCIARIA *
*******************************************
gen eduui_ci=. 
/*replace eduui_ci=1 if aedu_ci<18 & aedu_ci!=.
replace eduui_ci=0 if aedu_ci>=18 & aedu_ci!=.*/
label var eduui_ci "No ha completado la educación terciaria"

*******************************************
*  HA COMPLETADO LA EDUCACION TERCIARIA   *
*******************************************
g eduuc_ci=((educlev>=3 & educlev<=4) & educlev!=. & educlev!=9)
label var eduuc_ci "Ha completado la educación terciaria"


****************************************
*  HA COMPLETADO EDUCACION PREESCOLAR  *
****************************************
gen edupre_ci=. 
*replace edupre_ci=0 if
label var edupre_ci "Ha completado educación preescolar"

***************
***asipre_ci***
***************

gen byte asispre_ci=.
label variable asispre_ci "Asistencia a Educacion preescolar"

************************************************
*  HA COMPLETADO EDUCACION TERCIARIA ACADEMICA *
************************************************
gen eduac_ci=.
/*replace eduac_ci=1 if educlev==3 | educlev==5
replace eduac_ci=0 if educlev==4*/
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

**************
*pqnoasis1_ci*
**************
gen pqnoasis1_ci = .

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
label var tecnica_ci "Tiene carrera técnica"


**************************
*       TRAINING         *
**************************
g capact=0
replace capact=1 if ctrainng==1


**************************
*  TRAINING OCCUPATION   *
**************************
g train_ocup=.
replace train_ocup=1 if (coccuptr>=2000 & coccuptr<=3999)
replace train_ocup=2 if (coccuptr>=1000 & coccuptr<=1999) 
replace train_ocup=3 if (coccuptr>=4000 & coccuptr<=4999) 
replace train_ocup=4 if ((coccuptr>=5200 & coccuptr<=5999) | (coccuptr>=9111 & coccuptr<=9113)) 
replace train_ocup=5 if ((coccuptr>=5000 & coccuptr<=5199) | (coccuptr>=9120 & coccuptr<=9162)) 
replace train_ocup=6 if ((coccuptr>=6000 & coccuptr<=6999) | (coccuptr>=9200 & coccuptr<=9213))
replace train_ocup=7 if ((coccuptr>=7000 & coccuptr<=8999) | (coccuptr>=9300 & coccuptr<=9333))
replace train_ocup=9 if (coccuptr==9999) 

**************************
*    TRAINING BEFORE     *
**************************
g capact_ant=0
replace capact_ant=1 if training==1

******************************
* TRAINING OCCUPATION BEFORE *
******************************
g train_ocup2=.
replace train_ocup2=1 if (occuptr>=2000 & occuptr<=3999)
replace train_ocup2=2 if (occuptr>=1000 & occuptr<=1999) 
replace train_ocup2=3 if (occuptr>=4000 & occuptr<=4999) 
replace train_ocup2=4 if ((occuptr>=5200 & occuptr<=5999) | (occuptr>=9111 & occuptr<=9113)) 
replace train_ocup2=5 if ((occuptr>=5000 & occuptr<=5199) | (occuptr>=9120 & occuptr<=9162)) 
replace train_ocup2=6 if ((occuptr>=6000 & occuptr<=6999) | (occuptr>=9200 & occuptr<=9213))
replace train_ocup2=7 if ((occuptr>=7000 & occuptr<=8999) | (occuptr>=9300 & occuptr<=9333))
replace train_ocup2=9 if (occuptr==9999)



g edus2i_ci=.
g edus2c_ci=.
g nrylmpri_ci=.
g ylmnr_ch=.
g nrylmpri_ch=.
* Variables de vivienda
g aguared_ch=.
g aguadist_ch=.
g aguamala_ch=.
g aguamide_ch=.
g luz_ch=.
g luzmide_ch=.
g combust_ch=.
g bano_ch=.
g banoex_ch=.
g des1_ch=.
g des2_ch=.
g piso_ch=.
g pared_ch=.
g techo_ch=.
g resid_ch=.
g aguamejorada_ch =.
g banomejorado_ch =.
g dorm_ch=.
g cuartos_ch=.
g cocina_ch=.
g telef_ch=.
g refrig_ch=.
g freez_ch=.
g auto_ch=.
g compu_ch=.
g internet_ch=.
g cel_ch=.
g vivi1_ch=.
g vivi2_ch=. 
g viviprop_ch=.
g vivitit_ch=.
g vivialq_ch=.
g vivialqimp_ch=.
g tcylmpri_ci=.
g tcylmpri_ch=.
g instcot_ci=.
g edus1i_ci=.
g edus1c_ci=.
rename x1 x_1
rename x2 x_2
rename x3 x_3

******************************
*** VARIABLES DE MIGRACION ***
******************************

* Variables incluidas por SCL/MIG Fernando Morales

	*******************
	*** migrante_ci ***
	*******************
	
	gen migrante_ci = (ntlty != 0) if ntlty != 9
	label var migrante_ci "=1 si es migrante"
	
	**********************
	*** migantiguo5_ci ***
	**********************
	
	gen migantiguo5_ci=.
	label var migantiguo5_ci "=1 si es migrante antiguo (5 anos o mas)"
		
	**********************
	*** migrantelac_ci ***
	**********************
	
	gen migrantelac_ci= (ntlty == 1 | ntlty == 2)
	label var migrantelac_ci "=1 si es migrante proveniente de un pais LAC"
	
* Variables incluidas por SCL/MIG Juan Camilo Perdomo
	
	**********************
	*** migrantiguo5_ci ***
	**********************
	
	gen migrantiguo5_ci = .
	label var migrantiguo5_ci "=1 si es migrante antiguo (5 anos o mas)"
		
	**********************
	*** miglac_ci ***
	**********************
	
	gen miglac_ci= (ntlty == 1 | ntlty == 2) if migrante_ci == 1
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
edus1c_ci edus2i_ci edus2c_ci edupre_ci eduac_ci asiste_ci pqnoasis_ci pqnoasis1_ci	repite_ci repiteult_ci edupub_ci tecnica_ci ///
aguared_ch aguadist_ch aguamala_ch aguamide_ch luz_ch luzmide_ch combust_ch	bano_ch banoex_ch des1_ch des2_ch piso_ch aguamejorada_ch banomejorado_ch  ///
pared_ch techo_ch resid_ch dorm_ch cuartos_ch cocina_ch telef_ch refrig_ch freez_ch auto_ch compu_ch internet_ch cel_ch ///
vivi1_ch vivi2_ch viviprop_ch vivitit_ch vivialq_ch	vivialqimp_ch , first




compress


saveold "`base_out'", replace


log close
