
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

local PAIS GTM
local ENCUESTA ENEI
local ANO "2004"
local ronda m9_m11 


local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
*local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\\`PAIS'_`ANO'`ronda'.dta"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
                                                    
capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Guatemala
Encuesta: ENEI
Round: m9_m11
Autores: 
Última versión: Mayra Sáenz E-mail: mayras@iadb.org / saenzmayra.a@gmail.com
Fecha última modificación: 24 de Septiembre de 2013

							SCL/LMK - IADB
****************************************************************************/
/***************************************************************************
Detalle de procesamientos o modificaciones anteriores:

****************************************************************************/


use `base_in', clear


		**********************************
		***VARIABLES DEL IDENTIFICACION***
		**********************************
		
	****************
	* region_BID_c *
	****************
	gen region_BID_c=1
label var region_BID_c "Regiones BID"
label define region_BID_c 1 "Centroamérica_(CID)" 2 "Caribe_(CCB)" 3 "Andinos_(CAN)" 4 "Cono_Sur_(CSC)"
label value region_BID_c region_BID_c

***************
*** region_c **
***************
gen region_c=.
label var region_c "Division politico-administrativa"

***************
***factor_ch***
***************

gen factor_ch= factor
label variable factor_ch "Factor de expansion del hogar"


***************
****idh_ch*****
***************

gen idh_ch = id_hog
label variable idh_ch "ID del hogar"


*************
****idp_ci****
**************

gen idp_ci=id_per
label variable idp_ci "ID de la persona en el hogar"

**********
***zona***
**********
/*
             dominio:
           1 guatemala urbano
           2 el progreso urbano
           3 sacatepequez urbano
           4 chimaltenango urbano
           5 escuintla urbano
           6 santa rosa urbano
           7 solola urbano
           8 totonicapan urbano
           9 quetzaltenango urbano
          10 suchitepequez urbano
          11 retalhuleu urbano
          12 san marcos urbano
          13 huehuetenango urbano
          14 quiche urbano
          15 baja verapaz urbano
          16 alta verapaz urbano
          17 peten urbano
          18 izabal urbano
          19 zacapa urbano
          20 chiquimula urbano
          21 jalapa urbano
          22 jutiapa urbano
          23 rural nacional


*/			  
gen byte zona_c=0 if dominio==23
replace zona_c=1 if dominio>=1 & dominio <=22

label variable zona_c "Zona del pais"
label define zona_c 1 "Urbana" 0 "Rural"
label value zona_c zona_c


************
****pais****
************

gen str3 pais_c="GTM"
label variable pais_c "Pais"

**********
***anio***
**********

gen anio_c=2004
label variable anio_c "Anio de la encuesta"

*********
***mes***
*********

gen byte mes_c=9 
label variable mes_c "Mes de la encuesta"

*****************
***relacion_ci***
*****************

/*
PPA06 RELACION DE PARENTESCO? 12
1 Jefe o jefa del hagar
2 Esposo(a) o compañero(a)
3 Hija(o)
4 Yerno o Nuera
5 Nieta(o)
6 Padre o Madre
7 Suegra(o)
8 Hermana(o)
9 Cuñada (o)
10 Otro Pariente
11 Empleada (o) Doméstica (o)
12 Pensionista o Huesped
13 Otro no pariente
*/


gen relacion_ci=1 if ppa06==1
replace relacion_ci=2 if ppa06==2
replace relacion_ci=3 if ppa06==3 
replace relacion_ci=4 if ppa06>=4 & ppa06<=10
replace relacion_ci=5 if ppa06==12 | ppa06==13
replace relacion_ci=6 if ppa06==11

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

gen factor_ci=factor
label variable factor_ci "Factor de expansion del individuo"

**********
***sexo***
**********
/*
PPA02 Sexo 
1 Hombre
2 Mujer
*/
gen sexo_ci=ppa02
label var sexo_ci "Sexo del individuo" 
label define sexo_ci 1 "Hombre" 2 "Mujer"
label value sexo_ci sexo_ci

**********
***edad***
**********
gen edad_ci=ppa03
label variable edad_ci "Edad del individuo"


*****************
***civil_ci***
*****************
/*
p03a02:
           1 unido o unida
           2 casado o casada
           3 separado(a) de matrimonio
           4 separado(a) de unión
           5 divorciado o divorciada
           6 viudo o viuda
           7 soltero o soltera


 
*/


gen civil_ci=.
replace civil_ci=1 if p03a02==7
replace civil_ci=2 if p03a02==1 | p03a02==2 
replace civil_ci=3 if p03a02==3 | p03a02==4 | p03a02==5
replace civil_ci=4 if p03a02==6

label variable civil_ci "Estado civil"
label define civil_ci 1 "Soltero" 2 "Union formal o informal"
label define civil_ci 3 "Divorciado o separado" 4 "Viudo" , add
label value civil_ci civil_ci


**************
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
replace clasehog_ch=3 if notropari_ch>0 & notronopari_ch==0
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

gen miembros_ci=(relacion_ci<6)
label variable miembros_ci "Miembro del hogar"

		  ******************************
          *** VARIABLES DE DIVERSIDAD **
          ******************************
*Nathalia Maya & Antonella Pereira
*Julio 2021	

	
	***************
	***afroind_ci***
	***************
**Pregunta: Usted se considera perteneciente a uno de los siguientes pueblos índigenas del país? (p03a04) (1 kiché; 2 Qeqchí; 3 Kaqchikel; 4 Mam; 98 Otro pueblo índígena, ¿cuál? 5 Garífuna 6 Ladino 7 Extranjero 98 Otro, ¿cual?) 
gen afroind_ci=. 
replace afroind_ci=1  if p03a03 != 6 & p03a03 !=8
replace afroind_ci=3 if p03a03 ==6 
replace afroind_ci=9 if p03a03 ==8
replace afroind_ci=. if p03a03 ==.

	***************
	***afroind_ch***
	***************
gen afroind_jefe= afroind_ci if relacion_ci==1
egen afroind_ch  = min(afroind_jefe), by(idh_ch) 
drop afroind_jefe

	*******************
	***afroind_ano_c***
	*******************
gen afroind_ano_c=2002

	*******************
	***dis_ci***
	*******************
gen dis_ci=. 

	*******************
	***dis_ch***
	*******************
gen dis_ch=. 


	************************************
	*** VARIABLES DEL MERCADO LABORAL***
	************************************
*************
**salmm_ci***
*************

*1 = GUA 2004
gen salmm_ci= 	1174.05
label var salmm_ci "Salario minimo legal"



*********
*lp_ci***
*********

gen lp_ci =.
label var lp_ci "Linea de pobreza oficial del pais"

*********
*lpe_ci**
*********

gen lpe_ci =.
label var lpe_ci "Linea de indigencia oficial del pais"

/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/

****************
*cotizando_ci***
****************
gen cotizando_ci=.
label var cotizando_ci "Cotizante a la Seguridad Social"

****************
*afiliado_ci****
****************
/*

p05a29:
           1 afiliado
           2 beneficiario
           3 pensionado
           4 ninguna de las anteriores
*/

****************
*tipopen_ci*****
****************
gen tipopen_ci=.
label var tipopen_ci "Tipo de pension - variable original de cada pais" 


****************
*instpen_ci*****
****************
gen instpen_ci=.
label var instpen_ci "Institucion proveedora de la pension - variable original de cada pais" 

****************
*instcot_ci*****
****************

gen instcot_ci=p05a29
label var instcot_ci "Institucion proveedora de la pension - variable original de cada pais" 


****************
****condocup_ci*
****************
/*
gen condocup_ci=.
replace condocup_ci=1 if (ocupados==1)
replace condocup_ci=2 if (desatota==1 | btpasivo==1 | btactivo ==1) & ocupados!=1
recode condocup_ci .=3 if pobinac==1
replace condocup_ci=4 if edad<7
label var condocup_ci "Condicion de ocupación de acuerdo a def de cada pais"
label define condocup_ci 1 "Ocupado" 2 "Desocupado" 3 "Inactivo" 4 "Menor que 7" 
label value condocup_ci condocup_ci
*/

* No se utilizan variables creadas sino las originales de la base para homologar con la serie anterior. 05/19/2014 MGD
* Toma en cuentra trabajo+unque no trabajoa tiene trabajo y busqueda.
* Si existe la categoria de menore de edad.

* MGR: Modifico serie en base a correcciones Laura Castrillo: delimitar la condición de edad para que no tome los missing en caso que existan
gen condocup_ci=.
replace condocup_ci=1 if (p04a02==1 | p04a06==1)
replace condocup_ci=2 if (p04a02==2 & p04a06==2) & (p04a08==1 | p04a09==1)
recode condocup_ci .=3 if edad_ci>=7 & edad_ci!=.
replace condocup_ci=4 if edad<7
label var condocup_ci "Condicion de ocupación de acuerdo a def de cada pais"
label define condocup_ci 1 "Ocupado" 2 "Desocupado" 3 "Inactivo" 4 "Menor que 7" 
label value condocup_ci condocup_ci

gen afiliado_ci=.	
replace afiliado_ci=1 if p05a29==1
replace afiliado_ci=0 if condocup_ci==2
label var afiliado_ci "Afiliado a la Seguridad Social"
*************
*cesante_ci* 
*************
* Correccion MGD 07/17/2014: mal generada la variable, se consideraba a inactivos p07a01 y no desempleados.
generat cesante_ci=0 if condocup_ci==2
replace cesante_ci=1 if p06a06==2 & condocup_ci==2
label var cesante_ci "Desocupado - definicion oficial del pais"
*************
**pension_ci*
*************
/*gen pension_ci=1 if p10a04a>0 & p10a04a!=.
recode pension_ci .=0 
label var pension_ci "1=Recibe pension contributiva"
*/
*Modificación Mayra Sáenz - Septiembre 2014
gen pension_ci=1 if p10a04b>0 & p10a04b!=.
recode pension_ci .=0 
label var pension_ci "1=Recibe pension contributiva"

*************
**ypen_ci*
*************

gen ypen_ci=p10a04b/3 if p10a04b>0 & p10a04b!=.

label var ypen_ci "Valor de la pension contributiva"

***************
*pensionsub_ci*
***************

gen pensionsub_ci=.
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

gen desalent_ci=(p04a11 ==2 | p04a11 ==3  | p04a11 ==4)
replace desalent_ci=. if p04a11 ==.
label var desalent_ci "Trabajadores desalentados"

 ***************
 ***subemp_ci***
 ***************    
 * Horas trabajadas a la semana en el primer empleo
 /*

p05a33a         byte    %8.0g                 cuántas horas trabaja normalmente el día lunes?
p05a33b         byte    %8.0g                 cuántas horas trabaja normalmente el día martes?
p05a33c         byte    %8.0g                 cuántas horas trabaja normalmente el día miércoles?
p05a33d         byte    %8.0g                 cuántas horas trabaja normalmente el día jueves?
p05a33e         byte    %8.0g                 cuántas horas trabaja normalmente el día viernes?
p05a33f         byte    %8.0g                 cuántas horas trabaja normalmente el día sábado?
p05a33g         byte    %8.0g                 cuántas horas trabaja normalmente el día domingo?
         
 */       
 *Total de horas a la semana en la actividad principal
 egen horpri = rsum(p05a33a  p05a33b p05a33c p05a33d p05a33e p05a33f p05a33g) if emp_ci==1, missing
 
 *Total de horas a la semana en la actividad secundaria
 gen horsec = p05b14 if emp_ci==1

egen tothoras=rowtotal(horpri horsec)
replace tothoras=. if horpri==. & horsec==. 
replace tothoras=. if tothoras>=168

* Modificacion: subempleo visible (desea trabajar mas horas y esta disponible para hacerlo). MGD 06/19/2014
g subemp_ci=0 
replace subemp_ci=1 if emp_ci==1 & (horpri>=1 & horpri<=30) & p05d01==1 & p05d05==1
label var subemp_ci "Personas en subempleo por horas"

*****************
***horaspri_ci***
*****************

gen horaspri_ci=horpri
replace horaspri_ci=. if emp_ci==0
label var horaspri_ci "Horas trabajadas semanalmente en el trabajo principal"

*****************
***horastot_ci***
*****************

gen horastot_ci=tothoras  if emp_ci==1 
label var horastot_ci "Horas trabajadas semanalmente en todos los empleos"

*******************
***tiempoparc_ci***
*******************

* MGR: Modifico serie en base a correcciones Laura Castrillo: se debe utilizar horaspri en lugar de horastot como había sido generada antes
gen tiempoparc_ci=((horaspri_ci>=1 & horaspri_ci<30) &  emp_ci==1 & p05d01==2)
replace tiempoparc_ci=. if emp_ci!=1 | horaspri_ci==.
label var tiempoparc_c "Personas que trabajan medio tiempo" 

******************
***categopri_ci***
******************

gen categopri_ci=.
replace categopri_ci=1 if p05a07 ==6| p05a07 ==8 
replace categopri_ci=2 if p05a07 == 5 | p05a07 ==7 
replace categopri_ci=3 if (p05a07 ==1 | p05a07 ==2 | p05a07 ==3 | p05a07 ==4) 
replace categopri_ci=4 if (p05a07 ==9) 


label define categopri_ci 1"Patron" 2"Cuenta propia" 
label define categopri_ci 3"Empleado" 4" No remunerado" , add
label value categopri_ci categopri_ci
label variable categopri_ci "Categoria ocupacional"

/*

p05a07 En el trabajo al que dedicó más horas la semana pasada, uste 
1 Empleado(a) del gobierno
2 Empleado(a) privado(a)
3 Jornalero(a) o peón
4 Empleado(a) doméstico(a)
5 Trabajador(a) no agrícola
6 Patrón(a) no agrícola
7 Trabajador(a) agrícola
8 Patrón(a) agrícola
9 Trabajador(a) familiar sin pago
			  
			  
*/

******************
***categosec_ci***
******************

/*
p05b04 En este segundo trabajo usted es? 103
1 Empleado(a) del gobierno
2 Empleado(a) privado(a)
3 Jornalero(a) o peón
4 Empleado(a) doméstico(a)
5 Trabajador(a) por cuenta propia no agrícola
6 Patrón(a) empleador(a), socio(a) no agrícola
7 Trabajador(a) por cuenta propia agrícola
8 Patrón(a) empleador(a) socio(a) agrícola
9 Trabajador familiar sin pago
*/




gen categosec_ci=.
replace categosec_ci=1 if p05b04  ==6 | p05b04  ==8 
replace categosec_ci=2 if p05b04  == 5 | p05b04  ==7
replace categosec_ci=3 if (p05b04  ==1 | p05b04  ==2 | p05b04  ==3 | p05b04  ==4) 
replace categosec_ci=4 if (p05b04  ==9) 

label define categosec_ci 1"Patron" 2"Cuenta propia" 
label define categosec_ci 3"Empleado" 4" No remunerado" , add
label value categosec_ci categosec_ci
label variable categosec_ci "Categoria ocupacional trabajo secundario"

*****************
*tipocontrato_ci*
*****************

gen tipocontrato_ci=.
replace tipocontrato_ci=1 if (p05a09a ==1 & p05a08 ==1) & categopri_ci==3
replace tipocontrato_ci=2 if (p05a09a ==2 & p05a08 ==1) & categopri_ci==3
replace tipocontrato_ci=3 if (p05a08 ==2 | tipocontrato_ci==.) & categopri_ci==3
label var tipocontrato_ci "Tipo de contrato segun su duracion"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci

*****************
***nempleos_ci***
*****************

gen nempleos_ci=.
replace nempleos_ci=1 if emp_ci==1 & p05a01==1
replace nempleos_ci=2 if emp_ci==1 & (p05a01==2 | p05a01==3)
replace nempleos_ci=. if emp_ci==0
label var nempleos_ci "Número de empleos" 

*****************
***firmapeq_ci***
*****************
/*
gen firmapeq_ci= .
replace firmapeq_ci= 1 if p05a32<=5
replace firmapeq_ci = 0 if p05a32>5
label var firmapeq_ci "Trabajadores informales"
*/
*****************
***spublico_ci***
*****************

gen spublico_ci=(p05a07 ==1) 
replace spublico_ci=. if emp_ci==0 
label var spublico_ci "Personas que trabajan en el sector público"


**************
***ocupa_ci***
**************

/*
p05a02d2
   OCUPACIÓN A 2 DIGITOS                                              35
              1    Fuerzas Armadas
             11    Miembros de la Admon. Pública y Gerentes de Empresas
             12    Directores de Empresa
             13    Gerentes de Empresa
             21    Profesionales de las Ciencias Físicas y Afines
             22    Prof. de las Ciencias Biológicas, la Medicina y la Salud
             23    Profesionales de la Enseñanza
             24    Especialistas en Admon., Admon. de empresas y afines
             31    Tec. y Prof. de las Ciencias Físicas, Quím e Ing
             32    Profesionales de la Medicina y la Salud
             33    Instructores
             34    Otros Técnicos y Profesionales
             41    Oficinistas
             42    Empleados de trato directo con el público
             51    Trab. de Servicios Personales, de Proteccion y Seguridad
             52    Modelos, Vendedores y demostradores
             61    Agricultores y trab. calificados agropecuarios y pesqueros
             62    Trabajadores Agropecuarios y Pesqueros de subsistencia
             63    Agricultores de cultivo de café
             64    Administrador de finca de café
             71    Oficiales y oper. de indus. extractivas y de la construc.
             72    Oficiales y oper. de la metalurgia, la construc. Mecánica
             73    Mecánicos de precisión, artesanos y oper. de artes gráf
             74    Operarios y artesanos de artes mecánicas y otros oficios
             75    Supervisores de línea en maquiladora
             76    Operarios de maquiladora
             81    Operador de instalaciones fijas y afines
             82    Operador de máquinas y montadores
             83    Conductor de vehículos y oper. de equipos pesados móviles
             91    Trabajadores no calificados de ventas y servicios
             92    Peones agropecuarios, forestales, pesqueros y afines
             93    Peones mineros, de la cons. la ind. manufac. y el trans
             94    Peones agrícolas del café.
             99    Ocupación no bien especificada

*/

gen ocupa_ci=.
replace ocupa_ci=1 if (p05a02d2 >=21 & p05a02d2 <=35) & emp_ci==1
replace ocupa_ci=2 if (p05a02d2 >=11 & p05a02d2 <=19) & emp_ci==1
replace ocupa_ci=3 if (p05a02d2 >=41 & p05a02d2 <=49) & emp_ci==1
replace ocupa_ci=4 if (p05a02d2 ==52 | p05a02d2 ==95) & emp_ci==1
replace ocupa_ci=5 if (p05a02d2 ==51 | p05a02d2 ==53 | p05a02d2 ==50 | p05a02d2 ==54 | p05a02d2 ==91) & emp_ci==1
replace ocupa_ci=6 if ((p05a02d2 >=61 & p05a02d2 <=67) | p05a02d2 ==92) & emp_ci==1
replace ocupa_ci=7 if ((p05a02d2 >=71 & p05a02d2 <=86) | p05a02d2 ==93) & emp_ci==1
replace ocupa_ci=8 if (p05a02d2 >=1 & p05a02d2 <=3) & emp_ci==1
replace ocupa_ci=9 if (p05a02d2 ==94 | p05a02d2 ==96 | p05a02d2 ==99) & emp_ci==1

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
replace rama_ci = 1 if p05a03d2>=1 & p05a03d2<=5
replace rama_ci = 2 if p05a03d2>=10 & p05a03d2<=14
replace rama_ci = 3 if p05a03d2>=15 & p05a03d2<=37
replace rama_ci = 4 if p05a03d2>=40 & p05a03d2<=41
replace rama_ci = 5 if p05a03d2==45
replace rama_ci = 6 if p05a03d2>=50 & p05a03d2<=55
replace rama_ci = 7 if p05a03d2>=60 & p05a03d2<=64
replace rama_ci = 8 if p05a03d2>=65 & p05a03d2<=74
replace rama_ci = 9 if p05a03d2>=75 & p05a03d2<=99

label var rama_ci "Rama de actividad"
label def rama_ci 1"Agricultura, caza, silvicultura y pesca" 2"Explotación de minas y canteras" 3"Industrias manufactureras"
label def rama_ci 4"Electricidad, gas y agua" 5"Construcción" 6"Comercio, restaurantes y hoteles" 7"Transporte y almacenamiento", add
label def rama_ci 8"Establecimientos financieros, seguros e inmuebles" 9"Servicios sociales y comunales", add
label val rama_ci rama_ci

************
*durades_ci*
************
* p06a07   Cuántas semanas hace que dejó su último trabajo?
* MGD 04/02/2015: Se recodifica 996=96 y 997=97 para hacer comparable con la serie 2002-2013
recode p06a02 (997=97) (996=96)
gen durades_ci=.
replace durades_ci= (p06a02/4.3) 
replace durades_ci= 0.23 if p06a02==0 
label variable durades_ci "Duracion del desempleo en meses"


*******************
***antiguedad_ci***
*******************

gen antiguedad_ci= p05a35
replace antiguedad_ci=. if emp_ci==0
replace antiguedad_ci=. if p05a35==. 
label var antiguedad_ci "Antiguedad en la actividad actual en anios"


*******************
***tamemp_ci***
*******************

         
*Guatemala Pequeña 1 a 5, Mediana 6 a 50, Grande Más de 50

gen tamemp_ci = 1 if p05a32>=1 & p05a32<=5
replace tamemp_ci = 2 if (p05a32>=6 & p05a32<=50)
replace tamemp_ci = 3 if p05a32>50 & (p05a32!=. | p05a32<=998)

label define tamemp_ci 1 "Pequeña" 2 "Mediana" 3 "Grande"
label value tamemp_ci tamemp_ci
label var tamemp_ci "Tamaño de empresa"

*******************
***categoinac_ci***
*******************

/*
p04a02 Actividad principal de la semana pasada? 24
1 Trabajar
2 Buscar trabajo
3 Estudiar
4 Quehaceres de hogar
5 Incapacitado
6 Jubilado o pensionado
7 Rentista
8 Enfermos / convalecientes
9 Ocioso
10 Vacacionar
11 Jugar o Hacer deporte
12 Nada por edad muy pequeño / grande.
98 Otro
*/



gen categoinac_ci =1 if (p04a02==6 & condocup_ci==3)
replace categoinac_ci = 2 if  (p04a02==3 & condocup_ci==3)
replace categoinac_ci = 3 if  (p04a02==4 & condocup_ci==3)
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

************************************************************************
**************************INGRESOS**************************************
************************************************************************

local var = "p05a12 p05a13b p05a14b p05a15b p05a17b p05a16b p05a18b p05a19b p05a20b p05b05 p05b06b p05a21c p05a22b p05a23b p05a24b p05b09b p05b10b p05b07b p05b08b p10a11b p10b01b p10b02b p10b03b p10a01b p10a02b p10a03b p10a04b p10a05b p10a06b p10a07b p10a08b p10a09b p10a10b p10d01b" 
foreach x of local var {
recode `x'  (99999=.) (999999=.)
}




*Ingresos del trabajo principal
gen ymensual= 	p05a12 
gen hextras= p05a13b
gen comision=p05a14b
gen diferidos=p05a15b/12

gen bono14 = p05a17b/12
gen aguinaldo=p05a16b/12
gen vacaciones = p05a18b/12
gen trabvac= p05a19b/12
gen bonprod= p05a20b/12


*ingresos en especies trabajo principal
gen ropa = (p05a21c*p05a21b)/12
gen alimentos=p05a22b
gen vivienda1=p05a23b
gen transporte=p05a24b
  
 
  
* ingresos actividad secundaria
gen ymensual2=p05b05 
gen hextras2= p05b06b

gen bono142 = p05b09b/12
gen bonprod2=p05b10b/12



*ingresos en especies trabajo secundario
gen alimentos2=p05b07b
gen vivtrans2=p05b08b


*otros ingresos del trabajo distintos a los declarados
gen vtact = p10a11b/12
gen ventas = p10b01b/12
gen otrostrab1 =p10b02b/12
gen otrostrab2 =p10b03b/12



*ingresos distintos al trabajo (en el último trimestre)
gen alquiler = p10a01b/3
gen intereses = p10a02b/3
gen dividendos=p10a03b/3
gen jubilacion = p10a04b/3
gen ayudas = p10a05b/3
gen remesas1 = p10a06b/3


gen beca= p10a07b /3
gen pension = p10a08b/3
gen indemni = p10a09b/3
gen herencia = p10a10b/3


*autoconsumo
gen autocons= p10d01b


***************
***ylmpri_ci***
***************
egen ylmpri_ci=rsum(ymensual hextras comision diferidos bono14 aguinaldo vacaciones trabvac bonprod), missing
replace ylmpri_ci=. if ymensual==. & hextras==. & comision==. & diferidos==. & bono14==. & aguinaldo==. & vacaciones==. & trabvac==. & bonprod==. 
replace ylmpri_ci=0 if categopri_ci==4
label var ylmpri_ci "Ingreso laboral monetario actividad principal" 


*****************
***nrylmpri_ci***
*****************

gen nrylmpri_ci=(ylmpri_ci==. & emp_ci==1)
label var nrylmpri_ci "Id no respuesta ingreso de la actividad principal"  


****************
***ylnmpri_ci***
****************

egen ylnmpri_ci=rsum(ropa alimentos vivienda1 transporte), missing
replace ylnmpri_ci=. if ropa==. & alimentos==. & vivienda1==. & transporte==. 
label var ylnmpri_ci "Ingreso laboral NO monetario actividad principal"   



***************
***ylmsec_ci***
***************

egen ylmsec_ci= rsum(ymensual2 hextras2 bono142 bonprod2) if emp_ci==1 , missing
replace ylmsec_ci=. if (ymensual2==. & hextras2==. & bono142==. & bonprod2==.) & emp_ci==1
label var ylmsec_ci "Ingreso laboral monetario segunda actividad" 


****************
***ylnmsec_ci***
****************

egen ylnmsec_ci=rsum(alimentos2 vivtrans2), missing
replace ylnmsec_ci= . if alimentos2==. & vivtrans2==.
label var ylnmsec_ci "Ingreso laboral NO monetario actividad secundaria"


*****************
***ylmotros_ci***
*****************
egen ylmotros_ci=rsum(vtact ventas otrostrab1 otrostrab2), missing
replace ylmotros_ci =. if (vtact==. & ventas==. & otrostrab1==. & otrostrab2==.)
label var ylmotros_ci "Ingreso laboral monetario de otros trabajos" 


******************
***ylnmotros_ci***
******************

gen ylnmotros_ci=.
label var ylnmotros_ci "Ingreso laboral NO monetario de otros trabajos" 


************
***ylm_ci***
************

egen ylm_ci= rsum(ylmpri_ci ylmsec_ci), missing
replace ylm_ci=. if ylmpri_ci==. & ylmsec_ci==.
label var ylm_ci "Ingreso laboral monetario total"  


*************
***ylnm_ci***
*************

egen ylnm_ci=rsum(ylnmpri_ci ylnmsec_ci), missing
replace ylnm_ci=. if ylnmpri_ci==. &  ylnmsec_ci==.
label var ylnm_ci "Ingreso laboral NO monetario total"  


*************
***ynlm_ci***
*************

egen ynlm_ci=rsum(alquiler intereses dividendos jubilacion ayudas remesas1 beca pension indemni herencia), missing
replace ynlm_ci=. if alquiler==. & intereses==. & dividendos==. & jubilacion==. & ayudas==. & remesas1==. & beca==. & pension==. & indemni==. & herencia==.
label var ynlm_ci "Ingreso no laboral monetario"  

**************
***ynlnm_ci***
**************

gen ynlnm_ci=.
label var ynlnm_ci "Ingreso no laboral no monetario" 
     
                                                                                                                    
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
label var nrylmpri_ch "Hogares con algún miembro que no respondió por ingresos"


**************
*** ylm_ch ***
**************

by idh_ch, sort: egen ylm_ch=sum(ylm_ci) if miembros_ci==1, missing
label var ylm_ch "Ingreso laboral monetario del hogar"

***************
*** ylnm_ch ***
***************

by idh_ch, sort: egen ylnm_ch=sum(ylnm_ci) if miembros_ci==1, missing
label var ylnm_ch "Ingreso laboral no monetario del hogar"

*****************************************************************
*identificador de top-code del ingreso de la actividad principal*
*****************************************************************

gen tcylmpri_ci=.
**************************************************
*Identificador de los hogares en donde (top code)*
**************************************************
gen tcylmpri_ch=.

****************
*** ylmnr_ch ***
****************

by idh_ch, sort: egen ylmnr_ch=sum(ylm_ci) if miembros_ci==1, missing
replace ylmnr_ch=. if nrylmpri_ch==1
label var ylmnr_ch "Ingreso laboral monetario del hogar"

***************
*** ynlm_ch ***
***************

by idh_ch, sort: egen ynlm_ch=sum(ynlm_ci) if miembros_ci==1, missing
label var ynlm_ch "Ingreso no laboral monetario del hogar"


**************
***ynlnm_ch***
**************

gen ynlnm_ch=.
label var ynlnm_ch "Ingreso no laboral no monetario del hogar"


********
***NA***
********
gen rentaimp_ch=.
label var rentaimp_ch "Rentas imputadas del hogar"

gen autocons_ci=autocons
label var autocons_ci "Autoconsumo reportado por el individuo"

by idh_ch, sort: egen numper=sum(miembros_ci) if miembros_ci==1, missing
gen autocons_ch= autocons/numper
label var autocons_ch "Autoconsumo reportado por el hogar"



****************
***remesas_ci***
****************
gen remesas_ci=remesas1
label var remesas_ci "Remesas mensuales reportadas por el individuo" 


****************
***remesas_ch***
****************

by idh_ch, sort: egen remesas_ch=sum(remesas_ci) if miembros_ci==1, missing
label var remesas_ch "Remesas mensuales del hogar" 

*****************
***ylhopri_ci ***
*****************

gen ylmhopri_ci=ylmpri_ci/(horaspri_ci*4.3)
label var ylmhopri_ci "Salario monetario de la actividad principal" 


***************
***ylmho_ci ***
***************

gen ylmho_ci=ylm_ci/(horastot_ci*4.3)
label var ylmho_ci "Salario monetario de todas las actividades" 


	****************************
	***VARIABLES DE EDUCACION***
	****************************
	
	/*
	p03a07a Nivel educativo más alto aprobado? 20
1 Ninguno
2 Preparatoria
3 Primaria
4 Básico
5 Diversificado
6 Superior
7 Postgrado
	
  p03a07b Grado educativo más alto aprobado? 21
77 GRADO IGNORADO                                     
       */
	

*************
***aedu_ci*** 
*************

gen aedu_ci=.
replace	 aedu_ci=0  if (p03a07a==1) 

*Primaria
replace aedu_ci=1  if (p03a07a==3 & p03a07b==1)
replace aedu_ci=2  if (p03a07a==3 & p03a07b==2)
replace aedu_ci=3  if (p03a07a==3 & p03a07b==3)
replace aedu_ci=4  if (p03a07a==3 & p03a07b==4)
replace aedu_ci=5  if (p03a07a==3 & p03a07b==5)
replace aedu_ci=6  if (p03a07a==3 & p03a07b==6)


*Secundaria
replace aedu_ci=7  if (p03a07a==4 & p03a07b==1) 
replace aedu_ci=8 if (p03a07a==4 & p03a07b==2) 
replace aedu_ci=9 if (p03a07a==4 & p03a07b==3) 
replace aedu_ci=10 if (p03a07a==5 & p03a07b==1) 
replace aedu_ci=11 if (p03a07a==5 & p03a07b==2) 
replace aedu_ci=12 if (p03a07a==5 & p03a07b==3) 

*Superior
replace aedu_ci=13 if (p03a07a==6 & p03a07b==1)
replace aedu_ci=14 if (p03a07a==6 & p03a07b==2)
replace aedu_ci=15 if (p03a07a==6 & p03a07b==3)
replace aedu_ci=16 if (p03a07a==6 & p03a07b==4)
replace aedu_ci=17 if (p03a07a==6 & (p03a07b==5 | p03a07b==6))

*Postgrado
replace aedu_ci=18 if p03a07a==7 & p03a07b==1 
replace aedu_ci=19 if p03a07a==7 & p03a07b==2 
replace aedu_ci=20 if p03a07a==7 & p03a07b==3 
replace aedu_ci=21 if p03a07a==7 & (p03a07b==4)
replace aedu_ci=22 if p03a07a==7 & (p03a07b>=5 & p03a07b<=6)
replace aedu_ci=.  if p03a07a==. | p03a07b ==77

label var aedu_ci "Anios de educacion aprobados" 


**************
***eduno_ci***
**************

gen byte eduno_ci=0
replace eduno_ci=1 if aedu_ci==0
label variable eduno_ci "Sin educacion"


**************
***edupi_ci***
**************

gen byte edupi_ci=0
replace edupi_ci=1 if (aedu_ci>=1 & aedu_ci<6) 
label variable edupi_ci "Primaria incompleta"


**************
***edupc_ci***
**************

gen byte edupc_ci=0
replace edupc_ci=1 if aedu_ci==6 
label variable edupc_ci "Primaria completa"


**************
***edusi_ci***
**************

gen byte edusi_ci=0
replace edusi_ci=1 if (aedu_ci>=7 & aedu_ci<12) 
label variable edusi_ci "Secundaria incompleta"


**************
***edusc_ci***
**************

gen byte edusc_ci=0
replace edusc_ci=1 if aedu_ci==12 
label variable edusc_ci "Secundaria completa"


**************
***eduui_ci***
**************

gen byte eduui_ci=0
replace eduui_ci=1 if aedu_ci>12 & aedu_ci<16
replace eduui_ci=. if aedu_ci==.
label variable eduui_ci "Universitaria incompleta"

***************
***eduuc_ci****
***************

gen byte eduuc_ci=0
replace eduuc_ci=1 if aedu_ci>=17
replace eduuc_ci=. if aedu_ci==.
label variable eduuc_ci "Universitaria incompleta o mas"


***************
***edus1i_ci***
***************
gen edus1i_ci=0
replace edus1i_ci=1 if (aedu_ci>6 & aedu_ci<9)
label variable edus1i_ci "1er ciclo de la secundaria incompleto"

***************
***edus1c_ci***
***************
gen edus1c_ci=0
replace edus1c_ci=1 if aedu_ci==9
label variable edus1c_ci "1er ciclo de la secundaria completo"

***************
***edus2i_ci***
***************
gen edus2i_ci=0
replace edus2i_ci=1 if aedu_ci>9 & aedu_ci<12
label variable edus2i_ci "2do ciclo de la secundaria incompleto"

***************
***edus2c_ci***
***************
gen edus2c_ci=0
replace edus2c_ci=1 if aedu_ci==12
label variable edus2c_ci "2do ciclo de la secundaria completo"

local var = "eduno edupi edupc edusi edusc edusc eduui eduuc edus1i edus1c edus2i edus2c"
foreach x of local var {
replace `x'_ci=. if aedu_ci==.
}

***************
***edupre_ci***
***************

gen byte edupre_ci= .
label variable edupre_ci "Educacion preescolar"


**************
***eduac_ci***
**************
gen byte eduac_ci=.
label variable eduac_ci "Superior universitario vs superior no universitario"


***************
***asiste_ci***
***************

/*
P03A05 Asiste actualmente a clases en el ciclo escolar año 2004? 18
1 Si
2 No
*/
/*
generat asiste_ci=.
replace asiste_ci=1 if p03a05==1 
replace asiste_ci=0 if p03a05==2
label variable asiste_ci "Asiste actualmente a la escuela"
*/

* Modificaciones Marcela Rubio Septiembre 2014: variable habia sido generada incorrectamente
gen asiste_ci=.
replace asiste_ci=1 if p03a05==1 & p03a04==1
replace asiste_ci=0 if p03a05==2 & p03a04==1
replace asiste_ci=0 if p03a04==2
label var asiste_ci "Personas que actualmente asisten a centros de enseñanza"


**************
***pqnoasis_ci***
**************

/*
P03A06 Causa principal de deserción en el ciclo escolar año 2004? 19
1 Enfermedad
2 Falta de maestro
3 Oficios de la casa
4 Huelga magisterial
5 Falta de dinero
6 Trabajo
7 No le interesa
8 Mal tiempo
9 Embarazo
10 Migración temporal
11 No tiene quien lo lleve
12 Cerró pensum
13 Problemas personales o familiares
14 Por problemas con el maestro.
98 Otra causa ¿cuál?
*/
gen pqnoasis_ci=p03a06
label var pqnoasis_ci "Razones para no asistir a la escuela"
label def pqnoasis_ci 1 "Enfermedad" ///
2 "Falta de maestro" ///
3 "Oficios de la casa" ///
4 "Huelga magisterial" ///
5 "Falta de dinero" ///
6 "Trabajo" ///
7 "No le interesa" ///
8 "Mal tiempo" ///
9 "Embarazo" ///
10 "Migración temporal" ///
11 "No tiene quien lo lleve" ///
12 "Cerró pensum" ///
13 "Problemas personales o familiares" ///
14 "Por problemas con el maestro." ///
98 "Otra causa ¿cuál?"
label val pqnoasis_ci pqnoasis_ci

**************
*pqnoasis1_ci*
**************
**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**

g       pqnoasis1_ci = 1 if pqnoasis_ci==5
replace pqnoasis1_ci= 2 if  pqnoasis_ci==6
replace pqnoasis1_ci= 3 if  pqnoasis_ci==13 | pqnoasis_ci==1
replace pqnoasis1_ci= 4 if  pqnoasis_ci==7
replace pqnoasis1_ci= 5 if  pqnoasis_ci==3 | pqnoasis_ci==9
replace pqnoasis1_ci= 6 if  pqnoasis_ci==12
replace pqnoasis1_ci= 8 if  pqnoasis_ci==11 
replace pqnoasis1_ci= 9 if  pqnoasis_ci==98 | pqnoasis_ci==10 | pqnoasis_ci==8 | pqnoasis_ci==4 | pqnoasis_ci==2 | pqnoasis_ci==14

label define pqnoasis1_ci 1 "Problemas económicos" 2 "Por trabajo" 3 "Problemas familiares o de salud" 4 "Falta de interés" 5	"Quehaceres domésticos/embarazo/cuidado de niños/as" 6 "Terminó sus estudios" 7	"Edad" 8 "Problemas de acceso"  9 "Otros"
label value  pqnoasis1_ci pqnoasis1_ci

***************
***repite_ci***
***************

gen repite_ci=.
label var repite_ci "Ha repetido al menos un grado"


******************
***repiteult_ci***
******************

gen repiteult_ci=.
label var repiteult "Ha repetido el último grado"


***************
***edupub_ci***
***************
gen edupub_ci=.
label var edupub_ci "Asiste a un centro de enseñanza público"

*************
**tecnica_ci*
*************
gen tecnica_ci=.
label var tecnica_ci "=1 formacion terciaria tecnica"


**********************************
**** VARIABLES DE LA VIVIENDA ****
**********************************



/*
P02A05 Qué tipo de servicio de agua dispone regularmente este hogar 32
Measurement Level: Ordinal
Column Width: 8 Alignment: Right
Print Format: F11
Write Format: F11
Value Label
1 Chorro de uso exclusivo
2 Chorro para varios hogares
3 Chorro público fuera de la vivienda
4 Pozo
5 Camión o tonel
6 Río lago o manantial
7 Se la regala el vecino
8 Agua de lluvia
98 Otro ¿cuál?
*/

****************
***aguared_ch***
****************
generate aguared_ch =.
replace aguared_ch = 1 if p02a05<4
replace aguared_ch = 0 if p02a05>=4
la var aguared_ch "Acceso a fuente de agua por red"
	
*****************
*aguafconsumo_ch*
*****************
gen aguafconsumo_ch = 0
replace aguafconsumo_ch = 1 if (p02a05==1 | p02a05==2)
replace aguafconsumo_ch = 2 if p02a05==3
replace aguafconsumo_ch = 5 if p02a05==8
replace aguafconsumo_ch = 6 if p02a05==5
replace aguafconsumo_ch = 8 if p02a05==6
replace aguafconsumo_ch = 10 if (p02a05==9 | p02a05==4 | p02a05==7)


*****************
*aguafuente_ch*
*****************

gen aguafuente_ch = 1 if (p02a05==1 | p02a05==2)
replace aguafuente_ch = 2 if p02a05==3
replace aguafuente_ch = 5 if p02a05==8
replace aguafuente_ch= 6 if p02a05==5
replace aguafuente_ch = 8 if p02a05==6
replace aguafuente_ch= 10 if p02a05==9 | p02a05==4 | p02a05==7

*************
*aguadist_ch*
*************
gen aguadist_ch=0
replace aguadist_ch= 1 if  p02a05 ==1
replace aguadist_ch= 2 if  p02a05 ==2
replace aguadist_ch= 3 if  p02a05==3
replace aguadist_ch= 0 if  p02a05>=4 & p02a05 <=9

**************
*aguadisp1_ch*
**************

gen aguadisp1_ch =9


**************
*aguadisp2_ch*
**************
gen aguadisp2_ch = 9



*************
*aguamala_ch*  Altered
*************
gen aguamala_ch = 2
replace aguamala_ch = 0 if aguafuente_ch<=7
replace aguamala_ch = 1 if aguafuente_ch>7 & aguafuente_ch!=10


*****************
*aguamejorada_ch*  Altered
*****************
gen aguamejorada_ch = 2
replace aguamejorada_ch = 0 if aguafuente_ch>7 & aguafuente_ch!=10
replace aguamejorada_ch = 1 if aguafuente_ch<=7 



*****************
***aguamide_ch***
*****************
gen aguamide_ch =. 


*****************
*bano_ch         *  Altered
*****************
gen bano_ch=.
replace bano_ch=0 if p02a06==5
replace bano_ch=1 if p02a06==1
replace bano_ch=2 if p02a06==2
replace bano_ch=6 if p02a06==3|p02a06==4

***************
***banoex_ch***
***************
generate banoex_ch=9
la var banoex_ch "El servicio sanitario es exclusivo del hogar"


*****************
*banomejorado_ch*  Altered
*****************
gen banomejorado_ch= 2
replace banomejorado_ch =1 if bano_ch<=3 & bano_ch!=0
replace banomejorado_ch =0 if (bano_ch ==0 | bano_ch>=4) & bano_ch!=6


************
*sinbano_ch*
************
gen sinbano_ch = 3
replace sinbano_ch = 0 if p02a06!=5

*label var sinbano_ch "= 0 si tiene baño en la vivienda o dentro del terreno"

*************
*aguatrat_ch*
*************
gen aguatrat_ch = 9





************
***luz_ch***
************
gen luz_ch=.
label var luz_ch  "La principal fuente de iluminación es electricidad"


****************
***luzmide_ch***
****************
gen luzmide_ch=.
label var luzmide_ch "Usan medidor para pagar consumo de electricidad"


****************
***combust_ch***
****************

gen combust_ch=.
label var combust_ch "Principal combustible gas o electricidad" 




/*
P02A06 El tipo de servicio sanitario que tiene este hogar es? 33
Measurement Level: Ordinal
Column Width: 8 Alignment: Right
Print Format: F11
Write Format: F11
Value Label
1 Inodoro conectado a red de drenajes
2 Inodoro conectado a fosa séptica
3 Excusado lavable
4 Letrina o pozo ciego
5 No tiene
*/




*************
***des1_ch***
*************

/*
gen des1_ch=.
label var des1_ch "Tipo de desague según unimproved de MDG"
label def des1_ch 0"No tiene servicio sanitario" 1"Conectado a red general o cámara séptica"
label def des1_ch 2"Letrina o conectado a pozo ciego" 3"Desemboca en río o calle", add
label val des1_ch des1_ch
*/

* Modificaciones Marcela Rubio: variable habia sido generada como missing
gen des1_ch=.
replace des1_ch=0 if p02a06==5
replace des1_ch=1 if p02a06==1 | p02a06==2 | p02a06==3
replace des1_ch=2 if p02a06==4
label def des1_ch 0"No tiene servicio sanitario" 1"Conectado a red general o cámara séptica"
label def des1_ch 2"Letrina o conectado a pozo ciego" 3"Desemboca en río o calle", add
label val des1_ch des1_ch

*************
***des2_ch***
*************

/*
gen des2_ch=.
label var des2_ch "Tipo de desague sin incluir definición MDG"
label def des2_ch 0"No tiene servicio sanitario" 1"Conectado a red general, cámara séptica, pozo o letrina"
label def des2_ch 2"Cualquier otro caso", add
label val des2_ch des2_ch
*/

* Modificaciones Marcela Rubio: variable habia sido generada como missing
gen des2_ch=.
replace des2_ch=0 if p02a06==5
replace des2_ch=1 if p02a06==1 | p02a06==2 | p02a06==3 | p02a06==4
label var des2_ch "Tipo de desague sin incluir definición MDG"
label def des2_ch 0"No tiene servicio sanitario" 1"Conectado a red general, cámara séptica, pozo o letrina"
label def des2_ch 2"Cualquier otro caso", add
label val des2_ch des2_ch



*************
***piso_ch***
*************

/*
P02A09 Material predominante en el piso de la vivienda que ocupa es 36
Measurement Level: Ordinal
Column Width: 8 Alignment: Right
Print Format: F11
Write Format: F11
Value Label
1 Ladrillo cerámico
2 Ladrillo de cemento
3 Ladrillo de barro
4 Torta de cemento
5 Madera
6 Tierra
98 Otro ¿cuál?
*/

gen piso_ch=.
replace piso_ch=0 if p02a09 == 6
replace piso_ch=1 if p02a09 >=1 &  p02a09 <=5
replace piso_ch=2 if p02a09 == 98
label var piso_ch "Materiales de construcción del piso"  
label def piso_ch 0"Piso de tierra" 1"Materiales permanentes" 2 "Otros materiales"
label val piso_ch piso_ch

**************
***pared_ch***
**************

/*
P02A07 Material predominante en las paredes exteriores de la vivien 34
1 Ladrillo
2 Block
3 Concreto
4 Adobe
5 Madera
6 Lámina metálica
7 Bajareque
8 Lepa palo o caña
9 Piedra
98 Otro ¿cuál?
*/
gen pared_ch=.
replace pared_ch=0 if p02a07 >=6 & p02a07 <=8
replace pared_ch=1 if p02a07 >=1 & p02a07 <=5
replace pared_ch=2 if p02a07 ==98
label var pared_ch "Materiales de construcción de las paredes"
label def pared_ch 0"No permanentes" 1"Permanentes" 2 "Otros materiales"
label val pared_ch pared_ch


**************
***techo_ch***
**************

/*

P02A08 Material predominante en el techo de la vivienda que ocupa e 35
1 Concreto
2 Lámina metálica
3 Asbesto cemento
4 Teja
5 Paja palma o similar
98 Otro ¿cuál?
*/
gen techo_ch=.
replace techo_ch= 0 if p02a08 == 5
replace techo_ch= 1 if p02a08 >= 1 & p02a08 <= 4
replace techo_ch= 2 if p02a08 == 98
label var techo_ch "Materiales de construcción del techo"
label def techo_ch 0"No permanentes" 1"Permanentes" 1"Otros materiales"
label val techo_ch techo_ch


**************
***resid_ch***
**************

gen resid_ch =.
label var resid_ch "Método de eliminación de residuos"
label def resid_ch 0"Recolección pública o privada" 1"Quemados o enterrados"
label def resid_ch 2"Tirados a un espacio abierto" 3"Otros", add
label val resid_ch resid_ch

*************
***dorm_ch***
*************

gen dorm_ch=p02a03
label var dorm_ch "Habitaciones para dormir"


****************
***cuartos_ch***
****************

gen cuartos_ch=p02a02
label var cuartos_ch "Habitaciones en el hogar"
 

***************
***cocina_ch***
***************
gen cocina_ch=.
label var cocina_ch "Cuarto separado y exclusivo para cocinar"


**************
***telef_ch***
**************
gen telef_ch=.
label var telef_ch "El hogar tiene servicio telefónico fijo"


***************
***refrig_ch***
***************

gen refrig_ch=.
label var refrig_ch "El hogar posee refrigerador o heladera"


**************
***freez_ch***
**************

gen freez_ch=.
label var freez_ch "El hogar posee congelador"


*************
***auto_ch***
*************

gen auto_ch=.
label var auto_ch "El hogar posee automovil particular"


**************
***compu_ch***
**************

gen compu_ch=.
label var compu_ch "El hogar posee computador"


*****************
***internet_ch***
*****************

gen internet_ch=.
label var internet_ch "El hogar posee conexión a Internet"


************
***cel_ch***
************

gen cel_ch=.
label var cel_ch "El hogar tiene servicio telefonico celular"



**************
***vivi1_ch***
**************

gen vivi1_ch=.
label var vivi1_ch "Tipo de vivienda en la que reside el hogar"
label def vivi1_ch 1"Casa" 2"Departamento" 3"Otros"
label val vivi1_ch vivi1_ch


*************
***vivi2_ch***
*************

gen vivi2_ch=.
label var vivi2_ch "La vivienda es casa o departamento"


*****************
***viviprop_ch***
*****************
/*
P02A01    Forma de tenencia de la vivienda                                   12
              1    Propia totalmente pagada
              2    Propia pagandose a plazos
              3    Heredada o donada
              4    Alquilada?
              5    Cedida o prestada?
             98    Otra forma, ¿cual?
			 			 
*/

gen viviprop_ch=0 if p02a01==4
replace viviprop_ch=1 if p02a01==1
replace viviprop_ch=2 if p02a01==2
replace viviprop_ch=3 if p02a01==3 | p02a01==5
replace viviprop_ch=. if p02a01==. 
label var viviprop_ch "Propiedad de la vivienda"
label def viviprop_ch 0"Alquilada" 1"Propia y totalmente pagada" 2 "Propia y en proceso de pago"
label def viviprop_ch 3"Ocupada (propia de facto)", add
label val viviprop_ch viviprop_ch


****************
***vivitit_ch***
****************
gen vivitit_ch= .
label var vivitit_ch "El hogar posee un título de propiedad"


****************
***vivialq_ch***
****************

gen vivialq_ch = . 
label var vivialq_ch "Alquiler mensual"


*******************
***vivialqimp_ch***
*******************

gen vivialqimp_ch=.
label var vivialqimp_ch "Alquiler mensual imputado"

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

rename p05a03d2 codindustria
rename p05a02d2 codocupa



compress


saveold "`base_out'", replace


log close

