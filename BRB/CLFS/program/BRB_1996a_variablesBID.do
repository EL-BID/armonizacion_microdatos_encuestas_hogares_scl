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
local ANO "1996"
local ronda a

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
   
capture log close
log using "`log_file'", replace 

*log off

/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Barbados
Encuesta: CLFS
Round: Octubre
Autores: 
Versión 2022: Agustina Thailinger SCL/EDU

****************************************************************************/
/***************************************************************************
Detalle de procesamientos o modificaciones anteriores:
****************************************************************************/

use `base_in', clear

**********************
* AÑO DE LA ENCUESTA *
**********************
gen anio_c=1996
label variable anio_c "Año de la Encuesta"

**********************
* MES DE LA ENCUESTA *
**********************
gen mes_c=.
label variable mes_c "Mes de la Encuesta"

*************************
* FACTORES DE EXPANSION *
*************************
sum wtfact_a
scalar pob=r(sum)
gen pop=wtfact_a*(267047/pob) // población BRB 1996
sum pop
ret list
gen factor_ch=pop 
drop pop
label var factor_ch "Factor de Expansion del Hogar"

gen factor_ci=factor_ch
label var factor_ci "Factor de Expansion del Individuo"

**************
* REGION BID *
**************
gen region_BID_c=2
label var region_BID_c "Region BID"
label define region_BID 1 "Centroamérica" 2 "Caribe" 3 "Andinos" 4 "Cono Sur"
label values region_BID_c region_BID

***************
* REGION PAIS *
***************
g region_c=.

***************
*    ZONA     *
***************
gen byte zona_c=.
label variable zona_c "Zona geográfica"
label define zona_c 0 "Rural" 1 "Urbana"
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
gen hh_id=string(real(hhno),"%03.0f")

egen idh_ch= concat(edno hh_id round)
destring idh_ch, replace
sort idh

label var idh_ch "Identificador Unico del Hogar"

*******************************
* IDENTIFICADOR DEL INDIVIDUO *
*******************************
egen idp_ci=concat(idh_ch indivno)
destring idp_ci, replace
sort idp_ci idh_ch
quietly by idp_ci idh_ch:  gen dup = cond(_N==1,0,_n)
tab dup // hay 23 obs duplicadas (0.17%)
drop if dup==2
drop dup
label var idp_ci "Identificador Individual dentro del Hogar"

************************************
*  RELACION CON EL JEFE DE HOGAR   *
************************************
gen relacion_ci=1 if relhd==0
replace relacion_ci=2 if relhd==1
replace relacion_ci=3 if relhd==2 | relhd==3
replace relacion_ci=4 if relhd==4 
replace relacion_ci=5 if relhd==5 | relhd==8 
replace relacion_ci=. if relhd==9 /* No sabe */
label var relacion_ci "relación con el jefe de hogar"
label define relacion 1 "Jefe" 2 "Cónguye, Esposo/a, Compañero/a" 3 "Hijo/a" 4 "Otros parientes" 5 "Otros no parientes" 6 "Servicio doméstico" 
label values relacion_ci relacion

************************************
* DUMMY PARA NO MIEMBROS DEL HOGAR *
************************************
* Create a dummy indicating this person's income should NOT be included 
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
label define sexo 1 "Masculino" 2 "Femenino" 
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
label define civil 1 "Soltero" 2 "Unión formal o informal" 3 "Divorciado o separado" 4 "Viudo" 
label values civil_ci civil

*******************
*  JEFE DE HOGAR  *
*******************
gen jefe_ci=0
replace jefe_ci=1 if relacion_ci==1
label var jefe_ci "Jefe de hogar"
label define jefe 1 "Jefe de Hogar" 0 "Otro" 
label values jefe_ci jefe

************************************
*  NUMERO DE CONYUGES EN EL HOGAR  *
************************************
egen nconyuges_ch=sum(relacion_ci==2), by (idh_ch)
label var nconyuges_ch "Número de Conyuges en el hogar"

**********************************
*  NUMERO DE HIJOS EN EL HOGAR  *
**********************************
egen nhijos_ch=sum(relacion_ci==3), by (idh_ch)
label var nhijos_ch "Número de hijos en el hogar"

*******************************************
*  NUMERO DE OTROS PARIENTES EN EL HOGAR  *
*******************************************
egen notropari_ch=sum(relacion_ci==4), by (idh_ch)
label var notropari_ch "Número de otros parientes en el hogar"

**********************************************
*  NUMERO DE OTROS NO PARIENTES EN EL HOGAR  *
**********************************************
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
label define clasehog 1 "Unipersonal" 2 "Nuclear" 3 "Ampliado" 4 "Compuesto" 5 "Corresidente" 
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
* Menores de 15 años no se entrevistan
egen nchild=rsum(mchildrn fchildrn)
egen nmenor15_ch=sum(nchild), by (idh_ch)

*******************************************************
***           VARIABLES DE DIVERSIDAD               ***
*******************************************************				
* Maria Antonella Pereira & Nathalia Maya - Marzo 2021	
			
******************
*** afroind_ci ***
******************
gen afroind_ci=. 

******************
*** afroind_ch ***
******************
gen afroind_ch=. 

*********************
*** afroind_ano_c ***
*********************
gen afroind_ano_c=.		

**************
*** dis_ci ***
**************
gen dis_ci=. 

**************
*** dis_ch ***
**************
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
g condocup_ci=.
replace condocup_ci=1 if actvstat==10
replace condocup_ci=2 if actvstat==20 & majactv==6
replace condocup_ci=3 if (condocup_ci!=1 & condocup_ci!=2) & edad>=15
recode condocup_ci (.=4) if edad<15
label define condocup 1 "Ocupado" 2 "Desocupado" 3 "Inactivo" 4 "Menores de 15 años"
label values condocup_ci condocup
label var condocup_ci "Condición de ocupación"

**************************
* CATEGORIA DE INACTIVIDAD  *
**************************
*Jubilados, pensionados 
gen categoinac_ci=1 if majactv==4 & condocup_ci==3
label var  categoinac_ci "Condición de Inactividad" 
*Estudiantes
replace categoinac_ci=2 if majactv==3 & condocup_ci==3
*Quehaceres del Hogar
replace categoinac_ci=3 if majactv==2 & condocup_ci==3
*Otra razon
replace categoinac_ci=4 if (majactv==5 | majactv==7) & condocup_ci==3

label define inactivo 1 "Jubilados o Pensionado" 2 "Estudiante" 3 "Hogar" 4 "Otros"
label values categoinac_ci inactivo

**********************
*  NÚMERO DE EMPLEOS *
**********************
gen nempleos_ci=.
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
gen cesante_ci=.
label var cesante_ci "Cesante"

***********************************
* DURACION DEL DESEMPLEO EN MESES *
***********************************
gen durades_ci=.
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
gen desalent_ci=.
label var desalent_ci "Trabajadores desalentados, personas que creen que por alguna razon no conseguiran trabajo" 

*****************************
* TRABAJA MENOS DE 30 HORAS *
*****************************
gen subemp_ci=.
label var subemp_ci "Trabaja menos de 30 horas"

****************************************************
* TRABAJA MENOS DE 30 HORAS Y NO DESEA TRABAJAR MAS*
****************************************************
gen tiempoparc_ci=.
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
label var categosec_ci "Categoría ocupación secundaria"
label define categosec 1"Patrón o empleador" 2"Cuenta propia o independiente" 3"Empleado o asalariado" 4"Trabajador no remunerado"  
label values categosec_ci categosec

*********************************
*  RAMA DE ACTIVIDAD PRINCIPAL  *
*********************************
gen rama_ci=.
replace rama_ci=1 if indus==10 | indus==11 & condocup_ci==1 // 10: sugar, 11: other agriculture & fishing
replace rama_ci=2 if indus==50 & condocup_ci==1 // 50: construction & quarrying
replace rama_ci=3 if indus==30 & condocup_ci==1 // 30: manufacturing
replace rama_ci=4 if indus==40 & condocup_ci==1 // 40: electricity, gas & water
replace rama_ci=6 if indus==60 | indus==70 & condocup_ci==1 // 60: wholesale and retail, 70: turism
replace rama_ci=7 if indus==72 & condocup_ci==1 // 72: transport & communications
replace rama_ci=8 if indus==80 & condocup_ci==1 // 80: finance, insurance & business
replace rama_ci=9 if indus==90 | indus==93 & condocup_ci==1 // 90: general services, 92: government services

label var rama_ci "Rama de actividad principal"
label define rama 1 "Agricultura, caza, silvicultura o pesca" 2 "Minas y Canteras" 3 "Manufactura" 4 "Electricidad, gas o agua" 5 "Construcción" 6 "Comercio al por mayor, restaurantes o hoteles" 7 "Transporte o almacenamiento" 8 "Establecimientos financieros, seguros o bienes inmuebles" 9 "Servicios sociales, comunales o personales" 
label values rama_ci rama

*********************************
*  TRABAJA EN EL SECTOR PUBLICO *
*********************************
gen spublico_ci=.
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
* MGD 09/02/2014: CIUO-88
gen ocupa_ci=.
replace ocupa_ci=1 if occup==2 | occup==3 & emp_ci==1 // 2: professionals (eg doctors), 3: associate professionals & technicians
replace ocupa_ci=1 if occup==1 & emp_ci==1 // 1: legislators, senior officials
replace ocupa_ci=3 if occup==4 & emp_ci==1 // 4: clerks
replace ocupa_ci=5 if occup==5 & emp_ci==1 // 5: services/shop workers
replace ocupa_ci=6 if occup==6 & emp_ci==1 // 6: skilled agricultural workers
replace ocupa_ci=7 if occup==8 & emp_ci==1 // 8: plant & mechanical opertors/ assemblers
replace ocupa_ci=9 if occup==7 | occup==9 | occup==10 & emp_ci==1 // 7:craft and related workers, 9: elementary occupation, 10: not stated
label var ocupa_ci "Tipo de ocupacion laboral"
label define ocupa 1 "Profesional o técnico" 2 "Director o funcionario superior" 3 "Personal administrativo o nivel intermedio" 4 "Comerciante o vendedor" 5 "Trabajador en servicios" 6 "Trabajador agrícola o afines" 7 "Obrero no agrícola, conductores de máquinas y vehículos de transporte y similares" 8 "Fuerzas armadas" 9 "Otras ocupaciones no clasificadas"
label values ocupa_ci ocupa

**********************************************
* HORAS TRABAJADAS EN LA ACTIVIDAD PRINCIPAL *
**********************************************
gen horaspri_ci=.
label var horaspri_ci "Horas trabajadas en la actividad principal"

**************************
* TOTAL HORAS TRABAJADAS *
**************************
gen horastot_ci=.

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

*******************************
*******************************
*******************************
*     VARIABLES DE INGRESO    *
*******************************
*******************************
*******************************

*************************************
* INGRESO MONETARIO MENSUAL LABORAL *
*************************************
*Nota MGD 01/15: El ingreso se reporta por rangos establecidos en la encuesta, no es variable continua.
*Earnings denota ingresos de todas las fuentes pero se asume que es el de la actividad principal
gen ylmpri_ci=      ((1+200)/2) if earngs==1 & emp_ci==1
replace ylmpri_ci=((200+299)/2) if earngs==2 & emp_ci==1
replace ylmpri_ci=((300+399)/2) if earngs==3 & emp_ci==1
replace ylmpri_ci=((400+499)/2) if earngs==4 & emp_ci==1
replace ylmpri_ci=((500+599)/2) if earngs==5 & emp_ci==1
replace ylmpri_ci=((600+699)/2) if earngs==6 & emp_ci==1
replace ylmpri_ci=((700+799)/2) if earngs==7 & emp_ci==1
replace ylmpri_ci=((800+899)/2) if earngs==8 & emp_ci==1
replace ylmpri_ci=((900+999)/2) if earngs==9 & emp_ci==1
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
label var ynlm_ci "Ingreso mensual NO laboral otras actividades"

**************************************************************
* INGRESO MENSUAL NO LABORAL NO MONETARIO OTRAS ACTIVIDADES  *
**************************************************************
gen ynlnm_ci= .
label var ynlnm_ci "Ingreso mensual NO laboral NO monetario otras actividades"

*************************************
* INGRESO MENSUAL LABORAL DEL HOGAR *
*************************************
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
replace ylmhopri_ci=ylmpri_ci/horaspri_ci 
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
gen salmm_ci=.
label var salmm_ci "salario mínimo mensual legal"

****************************************
* LINEA DE POBREZA OFICIAL MONEDA LOCAL*
****************************************
gen lp_ci=.
label var lp_ci "Línea de pobreza oficial en moneda local"

************************************************
* LINEA DE POBREZA EXTREMA OFICIAL MONEDA LOCAL*
************************************************
gen lpe_ci=.
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
*Modificado por Agustina Thailinger 12/2020
*No es posible saber cuantos años de educación tienen los individuos pero si definir cual es el nivel mas alto alcanzado
gen aedu_ci=.
label var aedu_ci "número de años de educación culminados"

******************************************
*  NO TIENE NINGUN NIVEL DE INSTRUCCION  *
******************************************
gen eduno_ci=(educlev==0)
replace eduno_ci=. if educlev==9
label var eduno_ci "No tiene ningún nivel de instrucción"

******************************************
* NO HA COMPLETADO LA EDUCACION PRIMARIA *
******************************************
gen edupi_ci=.
label var edupi_ci "No ha completado la educación primaria"

******************************************
*  HA COMPLETADO LA EDUCACION PRIMARIA   *
******************************************
gen edupc_ci=(educlev==1)
replace edupc_ci=. if educlev==9
label var edupc_ci "No ha completado la educación primaria"

******************************************
*NO HA COMPLETADO LA EDUCACION SECUNDARIA*
******************************************
gen edusi_ci=.
label var edusi_ci "No ha completado la educación secundaria"

******************************************
* HA COMPLETADO LA EDUCACION SECUNDARIA  *
******************************************
gen edusc_ci=(educlev==2)
replace edusc_ci=. if educlev==9
label var edusc_ci "Ha completado la educación secundaria"

*******************************************
* NO HA COMPLETADO LA EDUCACION TERCIARIA *
*******************************************
gen eduui_ci=.
label var eduui_ci "No ha completado la educación terciaria"

*******************************************
*  HA COMPLETADO LA EDUCACION TERCIARIA   *
*******************************************
gen eduuc_ci=(educlev==3 | educlev==4)
replace eduuc_ci=. if educlev==9
label var eduuc_ci "Ha completado la educación terciaria"

**************************************************
* NO HA COMPLETADO EL PRIMER CICLO DE SECUNDARIA *
**************************************************
gen edus1i_ci=. 
label var edus1i_ci "No ha completado el primer ciclo de la secundaria"

**************************************************
*  HA COMPLETADO EL PRIMER CICLO DE SECUNDARIA   *
**************************************************
gen edus1c_ci=. 
label var edus1c_ci "Ha completado el primer ciclo de la secundaria"

**************************************************
* NO HA COMPLETADO EL SEGUNDO CICLO DE SECUNDARIA *
**************************************************
gen edus2i_ci =.
label var edus2i_ci "No ha completado el segundo ciclo de la secundaria"

**************************************************
*  HA COMPLETADO EL SEGUNDO CICLO DE SECUNDARIA  *
**************************************************
gen edus2c_ci=.
replace edus2c_ci=1 if educlev==2
replace edus2c_ci=0 if educlev!=2 & educlev!=.
label var edus2c_ci "Ha completado el segundo ciclo de la secundaria"

**************
***eduac_ci***
**************
gen eduac_ci=.
replace eduac_ci=1 if educlev==4
replace eduac_ci=0 if educlev==3
label var eduac_ci "Ha completado educación terciaria académica"

****************************************
*  HA COMPLETADO EDUCACION PREESCOLAR  *
****************************************
gen edupre_ci=. 
label var edupre_ci "Ha completado educación preescolar"

***************
***asipre_ci***
***************
gen byte asispre_ci=.
label variable asispre_ci "Asistencia a Educacion preescolar"

************************************
*  ASISTE A UN CENTRO DE ENSEÑANZA *
************************************
gen asiste_ci=.
label var asiste_ci "Asiste a algún centro de enseñanza"

*********************************************
* PORQUE NO ASISTE A UN CENTRO DE ENSEÑANZA *
*********************************************
gen pqnoasis_ci=.
label var pqnoasis_ci "Porque no asiste a algún centro de enseñanza"
label define pqnoasis 1"Muy joven" 2"Razones financieras" 3"Trabaja en casa o negocio familiar" 4"Distancia a la escuela/transporte" 5"Enfermedad/inhabilidad" 6"falta de especio en la escuela" 7"Otra" 9"NS/NR"  
label values pqnoasis_ci pqnoasis

**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**
**************
*pqnoasis1_ci*
**************
gen pqnoasis1_ci = .

************************************
*  HA REPETIDO ALGUN AÑO O GRADO   *
************************************
gen repite_ci=.
label var repite_ci "Ha repetido algún año o grado"

******************************
*  HA REPETIDO EL ULTIMO AÑO *
******************************
gen repiteult_ci=.
label var repiteult_ci "Ha repetido el último grado"

***************************************
*ASISTE A CENTRO DE ENSEÑANZA PUBLICA *
***************************************
gen edupub_ci=.
label var edupub_ci "Asiste a centro de enseñanza pública"
label define edupub 1"Pública" 0"Privada"  
label values edupub_ci edupub

*******************************
*******************************
*******************************
*    VARIABLES DE VIVIENDA    * // hay variables pero no se cuenta con la codificación necesaria para crear otras.
*******************************
*******************************
*******************************
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

******************************
*** VARIABLES DE MIGRACION ***
******************************
* Variables incluidas por SCL/MIG Fernando Morales

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
	
* Variables incluidas por SCL/MIG Juan Camilo Perdomo
***********************
*** migrantiguo5_ci ***
***********************
gen migrantiguo5_ci=.
label var migrantiguo5_ci "=1 si es migrante antiguo (5 anos o mas)"
		
*****************
*** miglac_ci ***
*****************
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
edus1c_ci edus2i_ci edus2c_ci edupre_ci eduac_ci asiste_ci pqnoasis_ci pqnoasis1_ci	repite_ci repiteult_ci edupub_ci tecnica_ci ///
aguared_ch aguadist_ch aguamala_ch aguamide_ch luz_ch luzmide_ch combust_ch	bano_ch banoex_ch des1_ch des2_ch piso_ch aguamejorada_ch banomejorado_ch  ///
pared_ch techo_ch resid_ch dorm_ch cuartos_ch cocina_ch telef_ch refrig_ch freez_ch auto_ch compu_ch internet_ch cel_ch ///
vivi1_ch vivi2_ch viviprop_ch vivitit_ch vivialq_ch	vivialqimp_ch , first

*armonizar las variables originales de códigos de industria y ocupacion
rename occup codocupa
rename indus codindustria

compress

saveold "`base_out'", replace

log close
