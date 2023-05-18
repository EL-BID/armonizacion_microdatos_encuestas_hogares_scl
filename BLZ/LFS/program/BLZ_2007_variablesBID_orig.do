cd "${surveysFolder}\Users\HP-ADMIN\Downloads\BID"
log using BLZ_2007_variablesBID, text replace

use BLZ2007EA_BID

**********************
* AÑO DE LA ENCUESTA *
**********************
gen anio_c=2007
label variable anio_c "Año de la Encuesta"

*************************
* FACTORES DE EXPANSION *
*************************
gen factor_ch= weights
label var factor_ch "Factor de Expansion del Hogar"
gen factor_ci= weights
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
egen idh_ch=group(district urbrur ctv ednumber hhnumber)
label var idh_ch "Identificador Unico del Hogar"

*******************************
* IDENTIFICADOR DEL INDIVIDUO *
*******************************
gen idp_ci=cq10
label var idp_ci "Identificador Individual dentro del Hogar"

************************************
* DUMMY PARA NO MIEMBROS DEL HOGAR *
************************************
* Create a dummy indicating this person's income should NOT be included 
gen miembros_ci=1
replace miembros_ci=0 if  (relacion_ci==0 | relacion_ci==6 | relacion_ci==.)
replace miembros_ci=0 if factor_ci==.
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
gen sexo_ci=cq12
label var sexo_ci "sexo del individuo"
label define sexo 1"Masculino" 2"Femenino" 
label values sexo_ci sexo

***********
*  EDAD   *
***********
gen edad_ci=cq13
label var edad_ci "edad del individuo"

***********
*  RAZA   *
***********
gen raza_ci=.
label var raza_ci "raza del individuo"
label define raza 1"Indígena" 2"afro-descendiente" 3"Resto"
label values raza_ci raza
*la variable cq14 tiene una clasificación
*1= Creole
*2= East Indian
*3= Garifuna
*4= Maya
*5= Mennonite
*6= Mestizo
*7= Chinese
*8= Caucasian/White
*9= Other
*99= DK/NS

************************************
*  RELACION CON EL JEFE DE HOGAR   *
************************************
gen relacion_ci=1 if cq11==1
replace relacion_ci=2 if cq11==2
replace relacion_ci=3 if cq11==3
replace relacion_ci=4 if cq11==4 | cq11==5 | cq11==6 | cq11==7
replace relacion_ci=5 if cq11==8
replace relacion_ci=. if cq11==9 /* No sabe */
label var relacion_ci "relación con el jefe de hogar"
label define relacion 1"Jefe" 2"Cónguye, Esposo/a, Compañero/a" 3"Hijo/a" 4"Otros parientes" 5"Otros no parientes" 6"Servicio doméstico" 
label values relacion_ci relacion

*******************
*  ESTADO CIVIL   *
*******************
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
egen notropari_ch=sum(relacion_ci>3 & relacion_ci<5), by (idh_ch)
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
replace clasehog_ch=2 if nhijos_ch>0 & notropari_ch==0 & notronopari_ch==0 /* nuclear (child with or without spouse but without other relatives)*/
replace clasehog_ch=2 if nconyuges_ch>0 & notropari_ch==0 & notronopari_ch==0 /* nuclear (spouse with or without children but without other relatives)*/
replace clasehog_ch=3 if notropari_ch>0 & notronopari_ch==0 /* ampliado*/
replace clasehog_ch=4 if (nconyuges_ch>0 | nhijos_ch>0 | (notropari_ch>0 & notropari_ch<.)) & (notronopari_ch>0 & notronopari_ch<.) /* compuesto  (some relatives plus non relative)*/
replace clasehog_ch=5 if nhijos_ch==0 & nconyuges_ch==0 & notropari_ch==0 & notronopari_ch>0 & notronopari_ch<./** corresidente*/
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
gen miembros_ch=1 if relacion_ci<=4
replace miembros_ch=2 if relacion_ci==5 | relacion_ci==6
label var miembros_ch "Miembros en el hogar"
label define miembros 1"Miembro" 2"No miembro"  
label values miembros_ch miembros

********************************************
*  MIEMBROS EN EL HOGAR MAYORES DE 21 AÑOS *
********************************************
egen nmayor21_ch=sum((relacion_ci>0 & relacion_ci<5) & (cq13>=21)), by (idh_ch)
label variable nmayor21_ch "Numero de personas de 21 años o mas dentro del Hogar"

********************************************
*  MIEMBROS EN EL HOGAR MENORES DE 21 AÑOS *
********************************************
egen nmenor21_ch=sum((relacion_ci>0 & relacion_ci<5) & (cq13<21)), by (idh_ch)
label variable nmenor21_ch "Numero de personas menores a 21 años dentro del Hogar"

********************************************
*  MIEMBROS EN EL HOGAR MAYORES DE 65 AÑOS *
********************************************
egen nmayor65_ch=sum((relacion_ci>0 & relacion_ci<5) & (cq13>=65)), by (idh_ch)
label variable nmayor65_ch "Numero de personas de 65 años o mas dentro del Hogar"

********************************************
*  MIEMBROS EN EL HOGAR MENORES DE 65 AÑOS *
********************************************
egen nmenor65_ch=sum((relacion_ci>0 & relacion_ci<5) & (cq13<=65)), by (idh_ch)
label variable nmenor65_ch "Miembros de 65 años o menos dentro del Hogar"

********************************************
*  MIEMBROS EN EL HOGAR MENORES DE 6 AÑOS *
********************************************
egen nmenor6_ch=sum((relacion_ci>0 & relacion_ci<5) & (cq13<6)), by (idh_ch)
label variable nmenor6_ch "Miembros menores a 6 años dentro del Hogar"

******************************************
*  MIEMBROS EN EL HOGAR MENORES DE 1 AÑO *
******************************************
egen nmenor1_ch=sum((relacion_ci>0 & relacion_ci<5) & (cq13<1)),  by (idh_ch)
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
gen condocup_ci=1 if cq112==1 | cq114==1
*Desocupado*
replace condocup_ci=2 if cq112==2 & cq114!=1 & cq119==1 & cq116==1
*Inactivo*
replace condocup_ci=3 if (condocup_ci ~=1 & condocup_ci ~=2)
*menores que PET
replace condocup_ci=4 if cq13<14
label define condocup 1"Ocupado" 2"Desocupado" 3"Inactivo" 4"Menores de 14 años"
label values condocup_ci condocup
label var condocup_ci "Condición de ocupación"

**************************
* CATEGORIA DE INACTIVIDAD  *
**************************
*Jubilados, pensionados
gen categoinac_ci=1 if cq120==3 & condocup_ci==3
label var  categoinac_ci "Condición de Inactividad" 
*Estudiantes
replace categoinac_ci=2 if cq120==2 & condocup_ci==3
*Quehaceres del Hogar
replace categoinac_ci=3 if cq120==1 & condocup_ci==3
*Otra razon
replace categoinac_ci=4 if (cq120==4 | cq120==5 | cq120==6) & condocup_ci==3
label define inactivo 1"Hogar o Pensionado" 2"Estudiante" 3"Hogar" 4"Otros"
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
gen antiguedad_ci=cq141 if cq141<99
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
gen cesante_ci=1 if cq125==1 & cq116==1 & cq112==2
replace cesante_ci=0 if cq112==1 | cq116==2 
label var cesante_ci "Cesante"

***********************************
* DURACION DEL DESEMPLEO EN MESES *
***********************************
*La varible esta definida por intervalos de tiempo. Se uso el punto medio del intervalo
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
gen desalent_ci=.
replace desalent_ci=0 if condocup_ci==2 | condocup_ci==1 | cq118<99
replace desalent_ci=1 if cq118==10 | cq118==11 | cq118==12
label var desalent_ci "Trabajadores desalentados, personas que creen que por alguna razon no conseguiran trabajo" 

*****************************
* TRABAJA MENOS DE 30 HORAS *
*****************************
gen subemp_ci=.
replace subemp_ci=1 if cq132m<=29
replace subemp_ci=0 if cq132m>29
label var subemp_ci "Trabaja menos de 30 horas"

****************************************************
* TRABAJA MENOS DE 30 HORAS Y NO DESEA TRABAJAR MAS*
****************************************************
gen tiempoparc_ci=.
replace tiempoparc=1 if cq132m<=29 & cq138==2
replace tiempoparc_ci=0 if cq132m>=29 | cq138==1
label var tiempoparc_ci "Trabaja menos de 30 horas y no desea trabajar más"

*********************************
* CATEGORIA OCUPACION PRINCIPAL *
*********************************
gen categopri_ci=.
replace categopri_ci=1 if cq128m==1
replace categopri_ci=2 if cq128m==2
replace categopri_ci=3 if cq128m==3 | cq128m==4 | cq128m==5
replace categopri_ci=4 if cq128m==6
label var categopri_ci "Categoría ocupación principal"
label define categopri 1"Patrón o empleador" 2"Cuenta propia o independiente" 3"Empleado o asalariado" 4"Trabajador no remunerado"  
label values categopri_ci categopri

*********************************
* CATEGORIA OCUPACION SECUNDARIA*
*********************************
gen categosec_ci=.
replace categosec_ci=1 if cq128o==1
replace categosec_ci=2 if cq128o==2
replace categosec_ci=3 if cq128o==3 | cq128o==4 | cq128o==5
replace categosec_ci=4 if cq128o==6
label var categosec_ci "Categoría ocupación secundaria"
label define categosec 1"Patrón o empleador" 2"Cuenta propia o independiente" 3"Empleado o asalariado" 4"Trabajador no remunerado"  
label values categosec_ci categosec

*********************************
*  RAMA DE ACTIVIDAD PRINCIPAL  *
*********************************
gen rama=.
replace rama_ci=1 if cq131misi>=0 & cq131misi<1000 /* indmisic for 2004 */
replace rama_ci=2 if cq131misi>=1000 & cq131misi<2000
replace rama_ci=3 if cq131misi>=2000 & cq131misi<4000
replace rama_ci=4 if cq131misi>=4000 & cq131misi<5000
replace rama_ci=5 if cq131misi>=5000 & cq131misi<6000
replace rama_ci=6 if cq131misi>=6000 & cq131misi<7000
replace rama_ci=7 if cq131misi>=7000 & cq131misi<8000
replace rama_ci=8 if cq131misi>=8000 & cq131misi<9000
replace rama_ci=9 if cq131misi>=9000 & cq131misi<10000
label define rama 1"Agricultura, caza, silvicultura o pesca" 2"Minas y Canteras" 3"Manufactura" 4"Electricidad, gas o agua" 5"Construcción" 6"Comercio al por mayor, restaurantes o hoteles" 7"Transporte o almacenamiento" 8"Establecimientos financieros, seguros o bienes inmuebles" 9"Servicios sociales, comunales o personales" 
label values rama_ci rama

*********************************
*  TRABAJA EN EL SECTOR PUBLICO *
*********************************
gen spublico_ci=.
replace spublico_ci=1 if cq128m==3 | cq128m==4
replace spublico_ci=0 if cq128m==1 | cq128m==2 | cq128m==5 | cq128m==6
label var spublico_ci "Personas que trabajan en el sector publico"
 
********************
* TAMAÑO DE EMPRESA*
********************
gen tamemp_ci=1 if cq129y==1
label var  tamemp_ci "Tamaño de Empresa" 
*Empresas medianas
replace tamemp_ci=2 if cq129y==2 | cq129y==3
*Empresas grandes
replace tamemp_ci=3 if cq129y==4
label define tamaño 1"Pequeña" 2"Mediana" 3"Grande"
label values tamemp_ci tamaño

*********************************
*  COTIZA A LA SEGURIDAD SOCIAL *
*********************************
gen cotizando_ci=.
replace cotizando_ci=1 if q432==1
replace cotizando_ci=0 if q432==2
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
replace afiliado_ci=1 if q429==1
replace afiliado_ci=0 if q429==2
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
replace tipocontrato_ci=3 if q426==2 | (q426==1 & q427==2)
label var tipocontrato_ci "Tipo de contrato"
label define tipocontrato 1"Permanente / Indefinido" 2"Temporal / Tiempo definido" 3"Sin contrato / Verbal"  
label values tipocontrato_ci tipocontrato

*****************************
* TIPO DE OCUPACION LABORAL *
*****************************
gen ocupa_ci=.
replace ocupa_ci=1 if cq130m>=2000 & cq130m<=3999
replace ocupa_ci=2 if cq130m>=1000 & cq130m<=1999
replace ocupa_ci=3 if cq130m>=4000 & cq130m<=4999
replace ocupa_ci=4 if cq130m>=5200 & cq130m<=5999
replace ocupa_ci=5 if cq130m>=5000 & cq130m<=5199
replace ocupa_ci=6 if cq130m>=6000 & cq130m<=6999
replace ocupa_ci=7 if cq130m>=7000 & cq130m<=8999
replace ocupa_ci=8 if cq130m>=0 & cq130m<=999 
replace ocupa_ci=9 if (cq130m>=9000 & cq130m<=9996) | cq130m==9999
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
gen ylmpri_ci=cq142
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
gen ylm_ci=.
replace ylm_ci=cq142*(365/12) if cq143==1 /* diario */
replace ylm_ci=cq142*(30/7) if cq143==2 /* semanal */
replace ylm_ci=cq142*(15/7) if cq143==3 /* cada dos semanas */
replace ylm_ci=cq142*2 if cq143==4 /* dos veces al mes */
replace ylm_ci=cq142 if cq143==5 /* mensual */
replace ylm_ci=cq142/(365/30) if cq143==6 /* anual */
replace ylm_ci=cq142 if cq143==7 & cq143==4
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
label var ylnm_ci "Ingreso mensual NO laboral otras actividades"

**************************************************************
* INGRESO MENSUAL NO LABORAL NO MONETARIO OTRAS ACTIVIDADES  *
**************************************************************
gen ynlnm_ci= .
label var ylnm_ci "Ingreso mensual NO laboral NO monetario otras actividades"

************************************
* INGRESO MENSUAL LABORAL DEL HOGAR*
************************************
gen ylm_ch=.
replace ylm_ch=sum(ylm_ci) if miembros_ci==1, by idh_ch
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
egen remesas_ci=.
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
replace aedu_ci=cq16b if cq16b!=99
label var aedu_ci "número de años de educación culminados"

******************************************
*  NO TIENE NINGUN NIVEL DE INSTRUCCION  *
******************************************
gen eduno_ci=.
replace eduno_ci=1 if cq16b==0 & cq16a==0
replace eduno_ci=0 if cq16b>=1 & cq16b!=99
label var eduno_ci "No tiene ningún nivel de instrucción"

******************************************
* NO HA COMPLETADO LA EDUCACION PRIMARIA *
******************************************
gen edupi_ci=.
replace edupi_ci=1 if cq16b<8
replace edupi_ci=0 if cq16b>=8 & cq16b!=99
label var edupi_ci "No ha completado la educación primaria"

******************************************
*  HA COMPLETADO LA EDUCACION PRIMARIA   *
******************************************
gen edupc_ci=.
replace edupc_ci=1 if cq16b>=8 & cq16b!=99
replace edupc_ci=0 if cq16b<8 
label var edupc_ci "Ha completado la educación primaria"

******************************************
*NO HA COMPLETADO LA EDUCACION SECUNDARIA*
******************************************
gen edusi_ci=.
replace edusi_ci=1 if cq16b<14
replace edusi_ci=0 if cq16b>=14 & cq16b!=99
label var edusi_ci "No ha completado la educación secundaria"

******************************************
* HA COMPLETADO LA EDUCACION SECUNDARIA  *
******************************************
gen edusc_ci =. 
replace edusc_ci=1 if cq16b>=14 & cq16b!=99
replace edusc_ci=0 if cq16<14
label var edusc_ci "Ha completado la educación secundaria"

*******************************************
* NO HA COMPLETADO LA EDUCACION TERCIARIA *
*******************************************
gen eduui_ci=. 
*replace eduui_ci=1 if cq16b<16
*replace eduui_ci=0 if cq16b>=16 & cq16b!=99
label var eduui_ci "No ha completado la educación terciaria"

*******************************************
*  HA COMPLETADO LA EDUCACION TERCIARIA   *
*******************************************
gen eduuc_ci =.
*replace edusc_ci=1 if cq16b>=14 & cq16b!=99
*replace edusc_ci=0 if cq16b<4
label var eduuc_ci "Ha completado la educación terciaria"

**************************************************
* NO HA COMPLETADO EL PRIMER CICLO DE SECUNDARIA *
**************************************************
gen edusli_ci =. 
*replace edusli_ci=1 if 
label var edusli_ci "No ha completado el primer ciclo de la secundaria"

**************************************************
*  HA COMPLETADO EL PRIMER CICLO DE SECUNDARIA   *
**************************************************
gen eduslc_ci =. 
*replace eduslc_ci=1 if 
label var eduslc_ci "Ha completado el primer ciclo de la secundaria"

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
replace edupre_ci=1 if cq16b>=2
replace edupre_ci=0 if cq16b==0 | cq16b==1
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
replace asiste_ci=1 if cq16==1 | cq16==2
replace asiste_ci=0 if cq16==3
label var asiste_ci "Asiste a algún centro de enseñanza"

*********************************************
* PORQUE NO ASISTE A UN CENTRO DE ENSEÑANZA *
*********************************************
gen pqnoasis_ci=.
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


**************************
*  ACCEDE A AGUA POR RED *
**************************
gen aguared_ch=.
replace aguared_ch=1 if h71==1 | h72==1 
replace aguared_ch=0 if (h73==1 | h74==1 | h75==1 | h76==1 | h77==1 | h78==1) & (h71!=1 | h72!=1) 
label var tecnica_ci "Tiene acceso a agua por red"

***********************************
*  UBICACION DE LA FUENTE DE AGUA *
***********************************

gen aguadist_ch=.
*replace aguadist_ch=1 if water==1 | water==3 | water==7 /* Adentro de la casa */
*replace aguadist_ch=2 if water==4 | water==2 /* Afuera de la casa pero adentro del terrno (o a menos de 1000mts de distancia) */
*replace aguadist_ch=3 if water==5 | water==6 | water==8 /* Afuera de la casa y afuera del terreno (o a más de 1000mts de distancia) */
*label var aguadist_ch "Ubicación de la fuente de agua"
*label define aguadist 1"Adentro de la vivienda" 2"Fuera de la vivienda pero dentro del terreno" 3"Fuera de la vivienda y fuera del terreno"
label values aguadist_ch aguadist

********************************
*  FUENTE DE AGUA "Unimproved" *
********************************
gen aguamala_ch=.
* river/ stream / creek / pond / spring *
replace aguamala_ch=1 if h73==1 | h77==1 
replace aguamala_ch=0 if h71==1 | h72==1 | h74==1 | h75==1 | h76==1
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
gen combust_ch=.
replace combust_ch=1 if h4==4 & h4==5 
label var combust_ch "Usa combustible como fuente de energía"

****************
*  TIENE BAÑO  *
****************
gen bano_ch=.
replace bano_ch=1 if h3<=7 
replace bano_ch=0 if h3==8 
label var bano_ch "Tiene baño, inodoro, letrina o pozo ciego"

*********************************
*  TIENE BAÑO DE USO EXCLUSIVO  *
*********************************
gen banoex_ch=.
*replace banoex_ch=1 if toilet>=1 & toilet<=6
*replace banoex_ch=0 if toilet==7 | toilet==8
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
label define pared 0"No permanentes / naturales o desechos" 1"Permanentes: lámina de metal o zinc, cemento o madera" 2"Otros materiales"
label values techo_ch techo

*************************************
* MÉTODO DE ELIMINACION DE RESIDUOS *
*************************************
gen resid_ch=.
replace resid_ch=0 if q29==1 
replace resid_ch=1 if q29==4 | q29==5
replace resid_ch=2 if q29==2 | q29==3
replace resid_ch=3 if q29==6 | q29==7
label var resid_ch "Metodo de eliminacion de residuos"
label define resid 0"Recolección pública o privada" 1"Quemados o enterrados" 2"Tirados en un espacio abierto" 3"Otros"
label values resid_ch resid

*****************************************
*  CANTIDAD DE DORMITORIOS EN EL HOGAR  *
*****************************************
gen dorm_ch=q26
label var dorm_ch "Cantidad de dormitorios en el hogar"

*****************************************
*  CANTIDAD DE CUARTOS EN EL HOGAR  *
*****************************************
gen cuartos_ch=q25
label var cuartos_ch "Cantidad de cuartos en el hogar"

**********************************
*  CUARTO EXCLUSIVO A LA COCINA  *
**********************************
gen cocina_ch=q27
label define cocina 1"Si" 2"No" 9"NS/NR"
label values cocina_ch cocina
label var cocina_ch "Cuarto exclusivo a la cocina"

*************************
*  TIENE TELEFONO FIJO  *
*************************
gen telef_ch=.
replace telef_ch=1 if q210b==1
replace telef_ch=0 if q210b==2
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
replace internet_ch=1 if q210e==1
replace internet_ch=0 if q210e==2
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
gen vivi1=.
replace vivi1=1 if q11==1 | q11==2 | q11==4
replace  vivi1=2 if q11==3
replace vivi1=3 if q11==5 | q11==6 | q11==7
label var vivi1_ch "Tipo de vivienda"
label define vivi1 1"Casa" 2"Departamento" 3"Otro tipo"
label values vivi1_ch vivi1

************************
*  CASA O DEPARTAMENTO *
************************
gen vivi2=.
replace vivi2=1 if q11==1 | q11==2 | q11==4 | q11==3
replace vivi2=0 if q11==5 | q11==6 | q11==7
label var vivi2_ch "Casa o departamento"

********************
*  VIVIENDA PROPIA *
********************
gen viviprop_ch=.
replace viviprop_ch=0 if h1==3 | h1==4 | h1==5
replace viviprop_ch=1 if h1==1
replace viviprop_ch=2 if h1==2 | h1==6 | h1==7
label var viviprop_ch "Vivienda propia"
label define viviprop 0"Arrendada" 1"Propia" 2"Otra"
label values viviprop_ch viviprop

********************************
*  POSEE TITULO DE PROPIEDAD   *
********************************
gen vivitit=.
replace vivitit=1 if q14==1
replace vivitit=0 if q14==2
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


save BLZ2007EA_BID, replace



sum pais_c anio_c mes_c zona_c factor_ch idh_ch  idp_ci factor_ci sexo_ci edad_ci raza_ci relacion_ci civil_ci jefe_ci nconyuges_ch nhijos_ch notropari_ch notronopari_ch nempdom_ch clasehog_ch nmiembros_ch miembros_ci nmayor21_ch nmenor21_ch nmayor65_ch nmenor6_ch nmenor1_ch condocup_ci categoinac_ci nempleos_ci emp_ci antiguedad_ci desemp_ci
sum cesante_ci durades_ci pea_ci desalent_ci subemp_ci tiempoparc_ci categopri_ci categosec_ci rama_ci spublico_ci tamemp_ci cotizando_ci inscot_ci afiliado_ci formal_ci tipocontrato_ci ocupa_ci horaspri_ci horastot_ci pensionsub_ci pension_ci instpen_ci ylmpri_ci ylnmpri_ci ylmsec_ci ylnmsec_ci ylmotros_ci ylnmotros_ci ylm_ci ylnm_ci ynlm_ci
sum ynlnm_ci ylm_ch ylnm_ch ylmnr_ch ynlm_ch ynlnm_ch ylmhopri_ci ylmho_ci rentaimp_ch autocons_ci autocons_ch remesas_ci remesas_ch ypen_ci ypensub_ci salmm_ci lp25_ci lp4_ci lp_ci lpe_ci aedu_ci eduno_ci edupi_ci edupc_ci edusi_ci edusc_ci eduui_ci eduuc_ci edus1i_ci edus1c_ci edus2i_ci edus2c_ci edupre_ci eduac_ci asiste_ci pqnoasis_ci repite_ci
sum repiteult_ci edupub_ci tecnica_ci aguared_ch aguadist_ch aguamala_ch aguamide_ch luz_ch luzmide_ch combust_ch bano_ch banoex_ch des1_ch des2_ch piso_ch pared_ch techo_ch resid_ch dorm_ch cuartos_ch cocina_ch telef_ch refrig_ch freez_ch auto_ch compu_ch internet_ch cel_ch vivi1_ch vivi2_ch viviprop_ch vivitit_ch vivialq_ch vivialqimp_ch 
