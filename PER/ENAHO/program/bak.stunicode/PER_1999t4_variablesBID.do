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

local PAIS PER
local ENCUESTA ENAHO
local ANO "1999"
local ronda t4 
local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
                        
capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Perú
Encuesta: ENAHO
Round: t4
Autores: Mayra Sáenz E-mail: saenzmayra.a@gmail.com - mayras@iadb.org
Generación nuevas variables LMK: Yessenia Loayza (desloay@hotmail.com)
Última versión: Yessenia Loayza - Email: desloay@hotmail.com
Fecha última modificación: agosto 2013

							SCL/LMK - IADB
****************************************************************************/
****************************************************************************/

use `base_in', clear

***************
***region_c ***
***************
tostring UBIGEO, replace
gen digito ="0"
gen length =length(UBIGEO)
egen aux = concat(digito UBIGEO) if length==5
replace UBIGEO=aux if length==5
drop digito length aux 

gen region_c=real(substr(UBIGEO,1,2))
label define region_c ///
1"Amazonas"	          ///
2"Ancash"	          ///
3"Apurimac"	          ///
4"Arequipa"	          ///
5"Ayacucho"	          ///
6"Cajamarca"	      ///
7"Callao"	          ///
8"Cusco"	          ///
9"Huancavelica"	      ///
10"Huanuco"	          ///
11"Ica"	              ///
12"Junin"	          ///
13"La libertad"	      ///
14"Lambayeque"	      ///
15"Lima"	          ///
16"Loreto"	          ///
17"Madre de Dios"	  ///
18"Moquegua"	      ///
19"Pasco"	          ///
20"Piura"	          ///
21"Puno"	          ///
22"San Martín"	      ///
23"Tacna"	          ///
24"Tumbes"	          ///
25"Ucayali"	
label value region_c region_c
label var region_c "division politico-administrativa, departamento"          

************************
*** region según BID ***
************************
gen region_BID_c=3 
label var region_BID_c "Regiones BID"
label define region_BID_c 1 "Centroamérica_(CID)" 2 "Caribe_(CCB)" 3 "Andinos_(CAN)" 4 "Cono_Sur_(CSC)"
label value region_BID_c region_BID_c

***************
***factor_ch***
***************

gen factor_ch=FACTOR
label variable factor_ch "Factor de expansion del hogar"

/*Nota: La variable "factlab" se crea solo para PERU porque el factor para las variables laborales
 es diferente al factor de las demas variables, como las demograficas */
gen factlab=.
replace factlab=FAC_EMPL

***************
****idh_ch*****
**************

sort CONGLOME VIVIENDA HOGAR
egen idh_ch= group(CONGLOME VIVIENDA HOGAR)
label variable idh_ch "ID del hogar"

*************
****idp_ci****
**************

gen idp_ci=CODPERSO
label variable idp_ci "ID de la persona en el hogar"

**********
***zona***
**********

gen byte zona_c=0 if ESTRATO>=4
replace zona_c=1 if ESTRATO<4

label variable zona_c "Zona del pais"
label define zona_c 1 "Urbana" 0 "Rural"
label value zona_c zona_c

************
****pais****
************

gen str3 pais_c="PER"
label variable pais_c "Pais"

**********
***anio***
**********

gen anio_c=1999
label variable anio_c "Anio de la encuesta"

*********
***mes***
*********
tostring FECENT01, replace 
gen mes_c=real(substr(FECENT01,3,2))
label variable mes_c "Mes de la encuesta"

*****************
***relacion_ci***
*****************

gen relacion_ci=.
replace relacion_ci=1 if P203==1
replace relacion_ci=2 if P203==2
replace relacion_ci=3 if P203==3
replace relacion_ci=4 if P203>=4 & P203<=6
replace relacion_ci=5 if P203==8 | P203==9
replace relacion_ci=6 if P203==7

label variable relacion_ci "Relacion con el jefe del hogar"
label define relacion_ci 1 "Jefe/a" 2 "Esposo/a" 3 "Hijo/a" 4 "Otros parientes" 5 "Otros no parientes"
label define relacion_ci 6 "Empleado/a domestico/a", add

label value relacion_ci relacion_ci

/*En este año no hay empleados domésticos ni pensionistas*/


****************************
***VARIABLES DEMOGRAFICAS***
****************************

***************
***factor_ci***
***************

gen factor_ci=FACTOR
label variable factor_ci "Factor de expansion del individuo"

**********
***sexo***
**********

gen sexo_ci=P207

label define sexo_ci 1 "Hombre" 2 "Mujer"
label value sexo_ci sexo_ci

**********
***edad***
**********

gen edad_ci=P208A
replace edad_ci=. if P208A==99
label variable edad_ci "Edad del individuo"


*****************
***civil_ci***
*****************

gen civil_ci=.
replace civil_ci=1 if P209==6
replace civil_ci=2 if P209==1 | P209==2
replace civil_ci=3 if P209==4 | P209==5
replace civil_ci=4 if P209==3

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
*************
***raza_ci***
*************
gen raza_ci=.
label define raza_ci 1 "Indígena" 2 "Afro-descendiente" 3 "Otros"
label value raza_ci raza_ci 
label value raza_ci raza_ci
label var raza_ci "Raza o etnia del individuo" 
notes raza_ci: En el cuestionario no consta una pregunta relacionada con raza.

gen raza_idioma_ci = .
gen id_ind_ci      = .
gen id_afro_ci     = .


************************************
*** VARIABLES DEL MERCADO LABORAL***
************************************
* Esta sección es para los residentes habituales del hogar mayores a 14 años*/ 

****************
****condocup_ci*
****************
gen condocup_ci=.
replace condocup_ci=1 if OCUPA==1
replace condocup_ci=2 if OCUPA==2
replace condocup_ci=3 if condocup_ci!=1 & condocup_ci!=2
replace condocup_ci=4 if edad_ci<14
label define condocup_ci 1"ocupados" 2"desocupados" 3"inactivos" 4"menor de PET"
label value condocup_ci condocup_ci
label var condocup_ci "Condicion de ocupacion utilizando definicion del pais"

****************
*afiliado_ci****
****************
gen afiliado_ci=0     if condocup_ci==1 | condocup_ci==2 
replace afiliado_ci=1 if (P558A==1) & afiliado_ci==0 
label var afiliado_ci "Afiliado a la Seguridad Social"
****************
*tipopen_ci*****
****************
gen tipopen_ci=.
********************
*** instcot_ci *****
********************
gen instcot_ci=. 
label var instcot_ci "institución a la cual cotiza"

****************
*cotizando_ci***
****************
gen cotizando_ci=0     if condocup_ci==1 | condocup_ci==2 
replace cotizando_ci=1 if ((P524B1>0 & P524B1!=.) | (P538B1>0 & P538B1!=.)) & cotizando_ci==0 /*a ocupados subordinados: empleados u obreros*/
label var cotizando_ci "Cotizante a la Seguridad Social"

gen cotizapri_ci=0     if condocup_ci==1 | condocup_ci==2 
replace cotizapri_ci=1 if (P524B1>0 & P524B1!=.) 
label var cotizapri_ci "Cotizante a la Seguridad Social por su trabajo principal"

gen cotizasec_ci=0     if condocup_ci==1 | condocup_ci==2 
replace cotizasec_ci=1 if (P538B1>0 & P538B1!=.) 
label var cotizasec_ci "Cotizante a la Seguridad Social por su trabajo secundario"

*****************
*tipocontrato_ci*
*****************
gen tipocontrato_ci=. 
label var tipocontrato_ci "Tipo de contrato segun su duracion en act principal"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci

*************
*tamemp_ci***
*************
/*
gen tamemp_ci=P512B
label var tamemp_ci "# empleados en la empresa de la actividad principal"
*/

gen tamemp_ci=1 if P512B>=1 &  P512B<=5 
label var  tamemp_ci "Tamaño de Empresa" 
*Empresas medianas
replace tamemp_ci=2 if P512B>=6 &  P512B<=50
*Empresas grandes
replace tamemp_ci=3 if P512B>=51 &  P512B<=9998
label define tamaño 1"Pequeña" 2"Mediana" 3"Grande"
label values tamemp_ci tamaño
tab tamemp_ci [iw=FACTOR]

****************
**categoinac_ci*
****************
gen categoinac_ci =1 if (P546==6 & condocup_ci==3)
replace categoinac_ci = 2 if  (P546==4 & condocup_ci==3)
replace categoinac_ci = 3 if  (P546==5 & condocup_ci==3)
replace categoinac_ci = 4 if  ((categoinac_ci ~=1 & categoinac_ci ~=2 & categoinac_ci ~=3) & condocup_ci==3)
label var categoinac_ci "Categoría de inactividad"
label define categoinac_ci 1 "jubilados o pensionados" 2 "Estudiantes" 3 "Quehaceres domésticos" 4 "Otros"
label value categoinac_ci categoinac_ci

*************
**  ypen_ci *
*************

generat pjub=P5561C*30  if P5561B==1
replace pjub=P5561C*4.3 if P5561B==2 
replace pjub=P5561C*2   if P5561B==3 
replace pjub=P5561C     if P5561B==4 
replace pjub=P5561C/2   if P5561B==5 
replace pjub=P5561C/3   if P5561B==6
replace pjub=P5561C/6   if P5561B==7 
replace pjub=P5561C/12  if P5561B==8 
replace pjub=.          if P5561C==999999


generat pviudz=P5564C*30  if P5564B==1 
replace pviudz=P5564C*4.3 if P5564B==2 
replace pviudz=P5564C*2   if P5564B==3
replace pviudz=P5564C     if P5564B==4 
replace pviudz=P5564C/2   if P5564B==5 
replace pviudz=P5564C/3   if P5564B==6
replace pviudz=P5564C/6   if P5564B==7 
replace pviudz=P5564C/12  if P5564B==8
replace pviudz=.          if P5564C==999999

egen ypen_ci= rsum(pjub pviudz)
replace ypen_ci=. if pjub==. & pviudz==.
label var ypen_ci "Valor de la pension contributiva"


/*Modificación Mayra Sáenz- Julio 2015: Se reemplazan por variables originales
* 1998-2002: 
gen     ypen_ci = 0
replace ypen_ci = D556T1/3 if pension_ci==1
replace ypen_ci = D556T2/3 if pension_ci==1
label var ypen_ci "Valor de la pension contributiva"*/

*************
**pension_ci*
*************
/*gen pension_ci=0 
replace pension_ci=1 if (P5561A==1 | P5564A==1) /* A todas las per mayores de 13*/
replace pension_ci=. if  P5561A==. & P5564A==.
label var pension_ci "1=Recibe pension contributiva"*/

*MGD 12/29/2015: recibe pension si declara ingreso por pension
gen pension_ci= (ypen_ci>0 & ypen_ci!=.)
label var pension_ci "1=Recibe pension contributiva"

****************
*instpen_ci*****
****************
gen instpen_ci=.
label var instpen_ci "Institucion proveedora de la pension - variable original de cada pais" 

***************
*pensionsub_ci*
***************
gen pensionsub_ci=.
label var pensionsub_ci "1=recibe pension subsidiada / no contributiva"

*****************
**  ypensub_ci  *
*****************
gen ypensub_ci=.
label var ypensub_ci "Valor de la pension subsidiada / no contributiva"

*************
*cesante_ci* 
*************
generat cesante_ci=0 if condocup_ci==2
replace cesante_ci=1 if P552==1 & condocup_ci==2
label var cesante_ci "Desocupado - definicion oficial del pais"

*********
*lp_ci***
*********
gen lp_ci =linea99 
label var lp_ci "Linea de pobreza oficial del pais"

*********
*li_ci***
*********
gen lpe_ci =linpe99
label var lpe_ci "Linea de indigencia oficial del pais"


/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/

*************
**salmm_ci***
*************
gen mesanterior=mes_c-1		

gen salmm_ci= .
replace salmm_ci=345
label var salmm_ci "Salario minimo legal"

*************
***tecnica_ci**
*************
gen tecnica_ci=(P301A==8 | P301A==9 | P304A==4)
label var tecnica_ci "=1 formacion terciaria tecnica"	

************
***emp_ci***
************
gen emp_ci=(condocup_ci==1)

****************
***desemp_ci***
****************
gen desemp_ci=(condocup_ci==2)

*************
***pea_ci***
*************
gen pea_ci=(emp_ci==1 | desemp_ci==1)

****************
***formal_ci ***
****************
gen formal_ci=(cotizando_ci==1)

*****************
***desalent_ci***
*****************

gen desalent_ci=(emp_ci==0 & P545==2 & (P549==1 | P549==2))


*****************
***horaspri_ci***
*****************

gen horaspri_ci=P513T 
replace horaspri_ci=. if P513T==99 | emp_ci~=1


*****************
***horastot_ci***
*****************

gen p518_alt=P518
replace p518_alt=. if P518==99

egen horastot_ci=rsum(horaspri_ci p518_alt)
replace horastot_ci=. if horaspri_ci==. & p518_alt==.
replace horastot_ci=. if emp_ci~=1

drop p518_alt

***************
***subemp_ci***
***************
/*
/*Sobre las horas normalmente trabajadas*/
gen subemp_ci=0
replace subemp_ci=1 if (P519==1 & horastot_ci<=30) & P521==1 
replace subemp_ci=1 if (P519==2 & P520<=30) & P521==1
replace subemp_ci=. if emp_ci==.
*/

* Modificacion con subempleo visible: quiere trabajar mas horas y esta disponible a trabajar mas horas. MGD 06/19/2014
gen subemp_ci=0
replace subemp_ci=1 if horaspri_ci<=30 & P521==1 & emp_ci==1 

*******************
***tiempoparc_ci***
*******************
/*Sobre las horas normalmente trabajadas*/

gen tiempoparc_ci=0
/*replace tiempoparc_ci=1 if (P519==1 & horastot_ci<=30) & P521==2
replace tiempoparc_ci=1 if (P519==2 & P520<=30) & P521==2
replace tiempoparc_ci=. if emp_ci==.*/
* 10/20/2015 MGD: no se usa las horas totales trabajadas sino solo las de la actividad principal.
replace tiempoparc_ci=1 if (horaspri_ci>=1 & horaspri_ci<30) & P521==2 & emp_ci==1
replace tiempoparc_ci=. if emp_ci==0


******************
***categopri_ci***
******************
* 10/20/2015 MGD: se añade la categoria otra clasificacion para sacarlos de los no remunerados

gen categopri_ci=.
replace categopri_ci=0 if P507==7
replace categopri_ci=1 if P507==1
replace categopri_ci=2 if P507==2 
replace categopri_ci=3 if P507==3 | P507==4 | P507==6 
replace categopri_ci=4 if P507==5 /*| P507==7*/

label define categopri_ci 0 "Otra clasificación" 1"Patron" 2"Cuenta propia" 
label define categopri_ci 3"Empleado" 4" No remunerado", add
label value categopri_ci categopri_ci
label variable categopri_ci "Categoria ocupacional trabajo principal"


******************
***categosec_ci***
******************
* 10/20/2015 MGD: se añade la categoria otra clasificacion para sacarlos de los no remunerados

gen categosec_ci=.
replace categosec_ci=0 if P517==7
replace categosec_ci=1 if P517==1
replace categosec_ci=2 if P517==2 
replace categosec_ci=3 if P517==3 | P517==4 | P517==6
replace categosec_ci=4 if P517==5 /*| P517==7*/

label define categosec_ci 0 "Otra clasificación" 1"Patron" 2"Cuenta propia" 
label define categosec_ci 3"Empleado" 4 "No remunerado" , add
label value categosec_ci categosec_ci
label variable categosec_ci "Categoria ocupacional trabajo secundario"


*****************
***nempleos_ci***
*****************

gen nempleos_ci=.
replace nempleos_ci=1 if emp_ci==1
replace nempleos_ci=2 if emp_ci==1 & P514==1

#delimit ;
replace nempleos_ci=2 if emp_ci==1 & P514==2 & (P5151==1 | P5152==1 | P5153==1 |
        P5154==1 | P5155==1 | P5156==1 | P5157==1 | P5158==1 | P5159==1 | P51510==1 |
        P51511==1 | P51512==1);
#delimit cr

/*
*****************
***firmapeq_ci***
*****************

gen firmapeq_ci=.
replace firmapeq_ci=1 if P512B<=5
replace firmapeq_ci=0 if P512B>=6 & P512B<9999
*/

*****************
***spublico_ci***
*****************

gen spublico_ci=(P510==1 | P510==2 | P510==3)
replace spublico_ci=. if emp_ci~=1


**************
***ocupa_ci***
**************

gen ocupa_ci=.
replace ocupa_ci=1 if (P505>=211 & P505<=396) & emp_ci==1
replace ocupa_ci=2 if (P505>=111 & P505<=148) & emp_ci==1
replace ocupa_ci=3 if (P505>=411 & P505<=462) & emp_ci==1
replace ocupa_ci=4 if (P505>=571 & P505<=583) | (P505>=911 & P505<=931) & emp_ci==1
replace ocupa_ci=5 if (P505>=511 & P505<=565) | (P505>=941 & P505<=961) & emp_ci==1
replace ocupa_ci=6 if (P505>=611 & P505<=641) | (P505>=971 & P505<=973) & emp_ci==1
replace ocupa_ci=7 if (P505>=711 & P505<=886) | (P505>=981 & P505<=987) & emp_ci==1
replace ocupa_ci=8 if (P505>=11 & P505<=24) & emp_ci==1


*************
***rama_ci***
*************

gen rama_ci=.
replace rama_ci=1 if (P506>=100 & P506<=500) & emp_ci==1
replace rama_ci=2 if (P506>=1000 & P506<=1429) & emp_ci==1
replace rama_ci=3 if (P506>=1500 & P506<=3720) & emp_ci==1
replace rama_ci=4 if (P506>=4000 & P506<=4100) & emp_ci==1
replace rama_ci=5 if (P506>=4500 & P506<=4550) & emp_ci==1
replace rama_ci=6 if (P506>=5000 & P506<=5520) & emp_ci==1 
replace rama_ci=7 if (P506>=6000 & P506<=6420) & emp_ci==1
replace rama_ci=8 if (P506>=6500 & P506<=7020) & emp_ci==1
replace rama_ci=9 if (P506>=7111 & P506<=9900) & emp_ci==1


****************
***durades_ci***
****************

gen durades_ci=P551/4.3 /*Sin filtros para el calculo: if desemp_ci==1*/
replace durades_ci=. if P551==999



*******************
***antiguedad_ci***
*******************

gen antiguedad_ci=.

*************************************************************************************
*******************************INGRESOS**********************************************
*************************************************************************************

*Mayra SÃ¡enz- Julio 2015: En este quietly se encuentra la generaciÃ³n de ingresos anterior.
/*
quietly {

***************
***ylmpri_ci***
***************

*Para los trabajadores dependientes

gen ypridbd=.

replace ypridbd=P524D1*30 if P523==1 
replace ypridbd=P524D1*4.3 if P523==2 
replace ypridbd=P524D1*2 if P523==3 
replace ypridbd=P524D1 if P523==4 
replace ypridbd=. if P524D1==999999

replace ypridbd=0 if categopri_ci==4 
/* A los trabajadores no remunerados que trabajan menos de 15 horas la encuesta no les
pregunta acerca de sus ingresos y los manda a la sección de desempleo. Como este grupo en
realidad está trabajando, reemplazo su ingreso missing por cero*/

replace ypridbd=. if categopri_ci<=2

*Para los trabajadores independientes
/*En este caso la pregunta no permite distinguir entre ingreso monetario
y en especie, vamos a asumir que es monetario*/

gen yprijbi=.
replace yprijbi=P530A
replace yprijbi=. if P530A==999999
replace yprijbi=. if categopri_ci>2

*Ingreso laboral monetario para todos

egen ylmpri_ci=rsum(yprijbi ypridbd)
replace ylmpri_ci=. if ypridbd==. & yprijbi==.
replace ylmpri_ci=0 if P51112==1
replace ylmpri_ci=. if emp~=1


*******************
*** nrylmpri_ci ***
*******************

gen nrylmpri_ci=(ylmpri_ci==. & emp_ci==1)


******************
*** ylnmpri_ci ***
******************

*Ingreso laboral no monetario de los dependientes

forvalues i =1(1) 6 {

gen especie`i'=.
replace especie`i'=P529`i'B*30  if P529`i'A==1
replace especie`i'=P529`i'B*4.3 if P529`i'A==2
replace especie`i'=P529`i'B*2   if P529`i'A==3
replace especie`i'=P529`i'B     if P529`i'A==4
replace especie`i'=P529`i'B/2   if P529`i'A==5
replace especie`i'=P529`i'B/3   if P529`i'A==6
replace especie`i'=P529`i'B/6   if P529`i'A==7
replace especie`i'=P529`i'B/12  if P529`i'A==8
replace especie`i'=. if P529`i'B==999999
}

egen ylnmprid=rsum(especie1 especie2 especie3 especie4 especie5 especie6)
replace ylnmprid=. if especie1==. &  especie2==. & especie3==. & especie4==. & especie5==. & especie6==.
replace ylnmprid=0 if categopri_ci==4
replace ylnmprid=0 if P528==2
replace ylnmprid=. if categopri_ci<=2 | categopri_ci==.

*Ingreso laboral no monetario de los independientes (autoconsumo)

gen ylnmprii=P536
replace ylnmprii=. if P536==999999
replace ylnmprii=. if categopri_ci>2

*Ingreso laboral no monetario para todos

egen ylnmpri_ci=rsum(ylnmprid ylnmprii)
replace ylnmpri_ci=. if ylnmprid==. & ylnmprii==.
replace ylnmpri_ci=. if emp_ci~=1


***************
***ylmsec_ci***
***************

*Para los trabajadores dependientes

gen ysecbd=.
replace ysecbd=P538D1 
replace ysecbd=. if P538D1==999999
replace ysecbd=0 if categosec_ci==4
replace ysecbd=. if categosec_ci<=2 | categosec_ci==.

*Para los trabajadores independientes
/*En este caso la pregunta no permite distinguir entre ingreso monetario
y en especie, vamos a asumir que es monetario*/

gen ysecjbi=.
replace ysecjbi=P541A
replace ysecjbi=. if P541A==999999
replace ysecjbi=. if categosec_ci>2

*Ingreso laboral monetario para todos

egen ylmsec_ci=rsum(ysecjbi ysecbd)
replace ylmsec_ci=. if ysecjbi==. & ysecbd==.
replace ylmsec_ci=0 if P53712==1
replace ylmsec_ci=. if emp~=1


******************
****ylnmsec_ci****
******************

*Ingreso laboral no monetario de los dependientes

forvalues i =1(1) 6 {

gen especiesec`i'=.
replace especiesec`i'=P540`i'B*30  if P540`i'A==1
replace especiesec`i'=P540`i'B*4.3 if P540`i'A==2
replace especiesec`i'=P540`i'B*2   if P540`i'A==3
replace especiesec`i'=P540`i'B     if P540`i'A==4
replace especiesec`i'=P540`i'B/2   if P540`i'A==5
replace especiesec`i'=P540`i'B/3   if P540`i'A==6
replace especiesec`i'=P540`i'B/6   if P540`i'A==7
replace especiesec`i'=P540`i'B/12  if P540`i'A==8
replace especiesec`i'=. if P540`i'B==999999
}

egen ylnmsecd=rsum(especiesec1 especiesec2 especiesec3 especiesec4 especiesec5 especiesec6)
replace ylnmsecd=. if especiesec1==. &  especiesec2==. & especiesec3==. & especiesec4==. & especiesec5==. & especiesec6==.
replace ylnmsecd=0 if categosec_ci==4
replace ylnmsecd=. if emp_ci~=1

*Ingreso laboral no monetario de los independientes (autoconsumo)

gen ylnmseci=P543
replace ylnmseci=. if P543==999999
replace ylnmseci=. if emp_ci~=1

*Ingreso laboral no monetario para todos

egen ylnmsec_ci=rsum(ylnmsecd ylnmseci)
replace ylnmsec_ci=. if ylnmsecd==. & ylnmseci==.
replace ylnmsec_ci=. if emp_ci==0


************
***ylm_ci***
************

*Sumamos ingresos extraordinarios por trabajo dependiente
gen ingext=P544T/12
replace ingext=. if P544T==999999


egen ylm_ci=rsum(ylmpri_ci ylmsec_ci ingext)
replace ylm_ci=. if ylmpri_ci==. & ylmsec_ci==. & ingext==.


*************
***ylnm_ci***
*************

egen ylnm_ci=rsum(ylnmpri_ci ylnmsec_ci)
replace ylnm_ci=. if ylnmpri_ci==. & ylnmsec_ci==.


*************
***ynlm_ci***
*************

*Transferencias corrientes

forvalues i=1(1) 6 {

gen transl`i'=.
replace transl`i'=P556`i'C*30  if P556`i'B==1
replace transl`i'=P556`i'C*4.3 if P556`i'B==2
replace transl`i'=P556`i'C*2   if P556`i'B==3
replace transl`i'=P556`i'C     if P556`i'B==4
replace transl`i'=P556`i'C/2   if P556`i'B==5
replace transl`i'=P556`i'C/3   if P556`i'B==6
replace transl`i'=P556`i'C/6   if P556`i'B==7
replace transl`i'=P556`i'C/12  if P556`i'B==8
replace transl`i'=. if P556`i'C==999999

}

egen transltot=rsum(transl1 transl2 transl3 transl4 transl5 transl6)
replace transltot=. if transl1==. & transl2==. & transl3==. & transl4==. & transl5==. & transl6==. 


forvalues i=1(1) 6 {

gen transe`i'=.
replace transe`i'=P556`i'E*30  if P556`i'D==1
replace transe`i'=P556`i'E*4.3 if P556`i'D==2
replace transe`i'=P556`i'E*2   if P556`i'D==3
replace transe`i'=P556`i'E     if P556`i'D==4
replace transe`i'=P556`i'E/2   if P556`i'D==5
replace transe`i'=P556`i'E/3   if P556`i'D==6
replace transe`i'=P556`i'E/6   if P556`i'D==7
replace transe`i'=P556`i'E/12  if P556`i'D==8
replace transe`i'=. if P556`i'E==999999

}

egen transetot=rsum(transe1 transe2 transe3 transe4 transe5 transe6)
replace transetot=. if transe1==. & transe2==. & transe3==. & transe4==. & transe5==. & transe6==.

*Rentas de la propiedad

forvalues i=1(1) 8 {

gen rentas`i'=.
replace rentas`i'=P557`i'C*30  if P557`i'B==1
replace rentas`i'=P557`i'C*4.3 if P557`i'B==2
replace rentas`i'=P557`i'C*2   if P557`i'B==3
replace rentas`i'=P557`i'C     if P557`i'B==4
replace rentas`i'=P557`i'C/2   if P557`i'B==5
replace rentas`i'=P557`i'C/3   if P557`i'B==6
replace rentas`i'=P557`i'C/6   if P557`i'B==7
replace rentas`i'=P557`i'C/12  if P557`i'B==8
replace rentas`i'=. if P557`i'C==999999
}

egen rentastot=rsum(rentas1 rentas2 rentas3 rentas4 rentas5 rentas6 rentas7 rentas8)
replace rentastot=. if rentas1==. & rentas2==. & rentas3==. & rentas4==. & rentas5==. & rentas6==. & rentas7==. & rentas8==.


* Otros ingresos extraordinarios

gen otrosexttot=P558T/12
replace otrosexttot=. if P558T==999999


egen ynlm_ci=rsum(transltot transetot rentastot otrosexttot)
replace ynlm_ci=. if transltot==. & transetot==. & rentastot==. & otrosexttot==.

**************
***ynlnm_ci***
**************

gen ynlnm_ci=.


****************
***remesas_ci***
****************

gen remesas_ci=transe5

************************
*** HOUSEHOLD INCOME ***
************************

*******************
*** nrylmpri_ch ***
*******************
*Creating a Flag label for those households where someone has a ylmpri_ci as missing

by idh_ch, sort: egen nrylmpri_ch=sum(nrylmpri_ci) if miembros_ci==1
replace nrylmpri_ch=1 if nrylmpri_ch>0 & nrylmpri_ch<.
replace nrylmpri_ch=. if nrylmpri_ch==.


**************
*** ylm_ch ***
**************

by idh_ch, sort: egen ylm_ch=sum(ylm_ci) if miembros_ci==1


****************
*** ylmnr_ch ***
****************

by idh_ch, sort: egen ylmnr_ch=sum(ylm_ci) if miembros_ci==1
replace ylmnr_ch=. if nrylmpri_ch==1


***************
*** ylnm_ch ***
***************

by idh_ch, sort: egen ylnm_ch=sum(ylnm_ci) if miembros_ci==1

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

gen ylnmotros_ci=.

*******************
*** remesas_ch ***
*******************

by idh_ch, sort: egen remesas_ch=sum(remesas_ci) if miembros_ci==1


***************
*** ynlm_ch ***
***************

by idh_ch, sort: egen ynlm_ch=sum(ynlm_ci) if miembros_ci==1

****************
*** ynlnm_ch ***
****************

gen ynlnm_ch=.

*******************
*** autocons_ci ***
*******************

gen p536_alt=P536
replace p536_alt=. if P536==999999

gen p543_alt=P543
replace p543_alt=. if P543==999999


egen autocons_ci=rsum(p536_alt p543_alt)
replace autocons_ci=. if p536_alt==. & p543_alt==.
replace autocons_ci=. if emp_ci~=1

drop p536_alt p543_alt


*******************
*** autocons_ch ***
*******************

by idh_ch, sort: egen autocons_ch=sum(autocons_ci) if miembros_ci==1

*******************
*** rentaimp_ch ***
*******************

gen rentaimp_ch=P106
replace rentaimp_ch=. if P106==99999


*****************
***ylhopri_ci ***
*****************

gen ylmhopri_ci=ylmpri_ci/(horaspri_ci*4.3)


***************
***ylmho_ci ***
***************

gen ylmho_ci=ylm_ci/(horastot_ci*4.3)

}

*/
*==================================================================================================================================================================*
* Mayra SÃ¡enz- Julio 2015: Se reemplazan los ingresos por los originales del instituto de estadÃ­stica del paÃ­s, de acuerdo a sintaxis elaborada por Marcos Robles.
*==================================================================================================================================================================*
*** Las variables son las mismas para 1998-2000

**************
***ylmpri_ci***
***************

recode I524D1 I530A I538D1 I541A D544T D556T1 D556T2 D557T D558T D529T D536 D540T D543 (.=0) (999999=0)
gen ypridbd = I524D1 /3
gen yprijbi = I530A  /3 
gen ysecbd  = I538D1 /3
gen ysecjbi = I541A  /3
gen ingext  = D544T  /3


egen ylmpri_ci=rsum(ypridbd yprijbi ingext), missing


*******************
*** nrylmpri_ci ***
*******************

gen nrylmpri_ci=.


******************
*** ylnmpri_ci ***
******************

gen ylnmprid = D529T /3
gen ylnmprii = D536  /3
gen ylnmsecd = D540T /3
gen ylnmseci = D543  /3


egen ylnmpri_ci=rsum(ylnmprid ylnmprii), missing



***************
***ylmsec_ci***
***************
egen ylmsec_ci=rsum(ysecbd ysecjbi), missing


******************
****ylnmsec_ci****
******************

egen ylnmsec_ci=rsum(ylnmsecd ylnmseci), missing


************
***ylm_ci***
************
egen ylm_ci = rowtotal(ypridbd yprijbi ysecbd ysecjbi ingext), missing

*************
***ylnm_ci***
*************

egen ylnm_ci = rowtotal(ylnmprid ylnmprii ylnmsecd ylnmseci), missing

*************
***ynlm_ci***
*************

gen transltot = D556T1 /3
gen transetot = D556T2 /3
gen remesas_ci= D556T2 /3 if P5565A==1
recode remesas_ci (.=0)
gen D557T_1 = D557T/3 
gen D558T_1 = D558T/3

egen ynlm_ci  = rowtotal(transltot transetot D557T_1 D558T_1), missing  // d557t es renta de propiedad y d558t ing extraord

**************
***ynlnm_ci***
**************

gen vivialqimp = ia01hd /3
gen ynlnm_ci = (ig06hd+ig08hd+sig24+gru13hd+gru23hd+gru24hd+gru33hd+gru34hd+gru43hd+gru44hd+gru53hd+gru54hd+gru63hd+gru64hd+gru73hd+gru74hd+gru83hd+gru84hd) /(3 * nmiembros_ch)

********************
***Transferencias***
********************

*-Monetarias
* 1998-2000
gen trac_pri = D556T1/3 if P5561A==2 | P5562A==3 | P5563A==6
gen trac_pub = 0


*-No Monetarias
*Se generan a partir de 2001
gen dona_pub =.
gen dona_pri =.

* TOTAL (las privadas incluyen transferencias del exterior)

egen trat_pri = rsum( trac_pri  dona_pri  transetot), missing
egen trat_pub = rsum( trac_pub  dona_pub), missing

****************
*Rentas y otros*
****************
egen rtasot = rsum(D557T_1  D558T_1), missing
label var rtasot "Rentas y otros"


************************
*** HOUSEHOLD INCOME ***
************************

*******************
*** nrylmpri_ch ***
*******************
*Creating a Flag label for those households where someone has a ylmpri_ci as missing

by idh_ch, sort: egen nrylmpri_ch=sum(nrylmpri_ci) if miembros_ci==1
replace nrylmpri_ch=1 if nrylmpri_ch>0 & nrylmpri_ch<.
replace nrylmpri_ch=. if nrylmpri_ch==.


**************
*** ylm_ch ***
**************

by idh_ch, sort: egen ylm_ch=sum(ylm_ci) if miembros_ci==1


****************
*** ylmnr_ch ***
****************

by idh_ch, sort: egen ylmnr_ch=sum(ylm_ci) if miembros_ci==1
replace ylmnr_ch=. if nrylmpri_ch==1


***************
*** ylnm_ch ***
***************

by idh_ch, sort: egen ylnm_ch=sum(ylnm_ci) if miembros_ci==1

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

gen ylnmotros_ci=.

*******************
*** remesas_ch ***
*******************

by idh_ch, sort: egen remesas_ch=sum(remesas_ci) if miembros_ci==1


***************
*** ynlm_ch ***
***************

by idh_ch, sort: egen ynlm_ch=sum(ynlm_ci) if miembros_ci==1

****************
*** ynlnm_ch ***
****************

by idh_ch, sort: egen ynlnm_ch=sum(ynlnm_ci) if miembros_ci==1

*******************
*** autocons_ci ***
*******************

gen p536_alt=P536
replace p536_alt=. if P536==999999

gen p543_alt=P543
replace p543_alt=. if P543==999999


egen autocons_ci=rsum(p536_alt p543_alt)
replace autocons_ci=. if p536_alt==. & p543_alt==.
replace autocons_ci=. if emp_ci~=1

drop p536_alt p543_alt


*******************
*** autocons_ch ***
*******************

by idh_ch, sort: egen autocons_ch=sum(autocons_ci) if miembros_ci==1

*******************
*** rentaimp_ch ***
*******************

*gen rentaimp_ch=P106
*replace rentaimp_ch=. if P106==99999

*ModificaciÃ³n Mayra SÃ¡enz - Julio 2015
gen rentaimp_ch= vivialqimp

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

destring P301B, replace

*Los q responden con antiguo sistema de prima(p301b): habian cinco años de primaria.
gen byte aedu_ci=.
replace aedu_ci=0  if P301A==1 | P301A==2
replace aedu_ci=1  if P301A==3 &P301B==0
replace aedu_ci=2  if P301A==3 &P301B==1 
replace aedu_ci=3  if P301A==3 &P301B==2
replace aedu_ci=4  if P301A==3 &P301B==3
replace aedu_ci=5  if P301A==3 &P301B==4
*Como se responde actual sist prim (P301C): seis años de primaria.
replace aedu_ci=0  if P301A==3 & P301C==0 &P301B==0
replace aedu_ci=1  if P301A==3 & P301C==1 &P301B==0
replace aedu_ci=2  if P301A==3 & P301C==2 &P301B==0
replace aedu_ci=3  if P301A==3 & P301C==3 &P301B==0
replace aedu_ci=4  if P301A==3 & P301C==4 &P301B==0
replace aedu_ci=5  if P301A==3 & P301C==5 &P301B==0
replace aedu_ci=6  if P301A==4 
replace aedu_ci=7  if P301A==5 &P301B==1
replace aedu_ci=8  if P301A==5 &P301B==2
replace aedu_ci=9  if P301A==5 &P301B==3
replace aedu_ci=10 if P301A==5 &P301B==4
replace aedu_ci=11 if P301A==6 
replace aedu_ci=12 if (P301A>=7 & P301A<=10) &P301B==1
replace aedu_ci=13 if (P301A>=7 & P301A<=10) &P301B==2
replace aedu_ci=14 if (P301A>=7 & P301A<=10) &P301B==3
replace aedu_ci=15 if (P301A>=7 & P301A<=10) &P301B==4
replace aedu_ci=16 if (P301A>=7 & P301A<=10) &P301B==5
replace aedu_ci=17 if (P301A>=7 & P301A<=10) &P301B==6
replace aedu_ci=18 if (P301A>=7 & P301A<=10) &P301B==7
replace aedu_ci=19 if P301A==11 &P301B==1
replace aedu_ci=20 if P301A==11 &P301B==2
replace aedu_ci=21 if P301A==11 &P301B==3
replace aedu_ci=22 if P301A==11 &P301B==4
replace aedu_ci=. if P212==. | P212==0


**************
***eduno_ci***
**************

gen byte eduno_ci=(P301A==1 | P301A==2) 
replace eduno_ci=. if P301A==. | P301A==99
replace eduno_ci=. if P212==0
label variable eduno_ci "Cero anios de educacion"

**************
***edupi_ci***
**************

gen byte edupi_ci=(P301A==3)
replace edupi_ci=. if P301A==. | P301A==99
replace edupi_ci=. if P212==0
label variable edupi_ci "Primaria incompleta"

**************
***edupc_ci***
**************

gen byte edupc_ci=(P301A==4)
replace edupc_ci=. if P301A==. | P301A==99
replace edupc_ci=. if P212==0
label variable edupc_ci "Primaria completa"

**************
***edusi_ci***
**************

gen byte edusi_ci=(P301A==5)
replace edusi_ci=. if P301A==. | P301A==99
replace edusi_ci=. if P212==0
label variable edusi_ci "Secundaria incompleta"

**************
***edusc_ci***
**************

gen byte edusc_ci=(P301A==6)
replace edusc_ci=. if P301A==. | P301A==99
replace edusc_ci=. if P212==0
label variable edusc_ci "Secundaria completa"

***************
***edus1i_ci***
***************

gen byte edus1i_ci=(P301A==5 & aedu_ci<=8)
replace edus1i_ci=. if P301A==. | P301A==99
replace edus1i_ci=. if P212==0
label variable edus1i_ci "1er ciclo de la secundaria incompleto"

***************
***edus1c_ci***
***************

gen byte edus1c_ci=(P301A==5 & aedu_ci==9)
replace edus1c_ci=. if P301A==. | P301A==99
replace edus1c_ci=. if P212==0
label variable edus1c_ci "1er ciclo de la eecundaria completo"

***************
***edus2i_ci***
***************

gen byte edus2i_ci=(P301A==5 & aedu_ci==10)
replace edus2i_ci=. if P301A==. | P301A==99
replace edus2i_ci=. if P212==0
label variable edus2i_ci "2do ciclo de la secundaria incompleto"

***************
***edus2c_ci***
***************

gen byte edus2c_ci=(P301A==6)
replace edus2c_ci=. if P301A==. | P301A==99
replace edus2c_ci=. if P212==0
label variable edus2c_ci "2do ciclo de la secundaria completo"

**************
***eduui_ci***
**************

gen byte eduui_ci=(P301A==7 | P301A==9)
replace eduui_ci=. if P301A==. | P301A==99
replace eduui_ci=. if P212==0
label variable eduui_ci "Universitaria incompleta"

***************
***eduuc_ci***
***************

gen byte eduuc_ci=(P301A==8 | P301A==10)
replace eduuc_ci=. if P301A==. | P301A==99
replace eduuc_ci=. if P212==0
label variable eduuc_ci "Universitaria incompleta o mas"


***************
***edupre_ci***
***************

gen byte edupre_ci=(P301A==2)
replace edupre_ci=. if P301A==. | P301A==99
replace edupre_ci=. if P212==0
label variable edupre_ci "Educacion preescolar"


**************
***eduac_ci***
**************
gen byte eduac_ci=.
replace eduac_ci=1 if (P301A==9 | P301A==10)
replace eduac_ci=0 if (P301A==7 | P301A==8)
label variable eduac_ci "Superior universitario vs superior no universitario"

***************
***asiste_ci***
***************

gen asiste_ci=(P303==1)
replace asiste_ci=. if P303==9
replace asiste_ci=. if P212==0
label variable asiste_ci "Asiste actualmente a la escuela"

*****************
***pqnoasis_ci***
*****************

gen pqnoasis_ci=P310

label variable pqnoasis_ci "Razones para no asistir a la escuela"
label define pqnoasis_ci 1 "Estoy trabajando"
label define pqnoasis_ci 2 "No me interesa", add
label define pqnoasis_ci 3 "Por enfermedad", add
label define pqnoasis_ci 4 "Prob. econ", add
label define pqnoasis_ci 5 "Prob.fam", add
label define pqnoasis_ci 6 "Bajas notas", add
label define pqnoasis_ci 7 "Termino", add
label define pqnoasis_ci 8 "No tiene la edad", add
label define pqnoasis_ci 9 "Quehaceres del hogar", add
label define pqnoasis_ci 10 "Otra razón", add
label define pqnoasis_ci 99 "Missing", add
label value pqnoasis_ci pqnoasis_ci

**************
*pqnoasis1_ci*
**************
**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**

g       pqnoasis1_ci = 1 if P310==4
replace pqnoasis1_ci = 2 if P310==1
replace pqnoasis1_ci = 3 if P310==3 | P310==5
replace pqnoasis1_ci = 4 if P310==2
replace pqnoasis1_ci = 5 if P310==9
replace pqnoasis1_ci = 6 if P310==7
replace pqnoasis1_ci = 7 if P310==8
replace pqnoasis1_ci = 9 if P310==6 | P310==10

label define pqnoasis1_ci 1 "Problemas económicos" 2 "Por trabajo" 3 "Problemas familiares o de salud" 4 "Falta de interés" 5	"Quehaceres domésticos/embarazo/cuidado de niños/as" 6 "Terminó sus estudios" 7	"Edad" 8 "Problemas de acceso"  9 "Otros"
label value  pqnoasis1_ci pqnoasis1_ci


***************
***repite_ci***
***************

gen repite_ci=.
gen repiteult_ci=.

***************
***edupub_ci***
***************

gen edupub_ci=(P301D==1)
replace edupub_ci=. if P301D==9
replace edupub_ci=. if P212==0

**********************************
**** VARIABLES DE LA VIVIENDA ****
**********************************

gen aguared_ch=(P110==1 | P110==2)

gen aguadist_ch=1 if P110==1
replace aguadist_ch=2 if P110==2
replace aguadist_ch=3 if P110>=3 & P110<=7

gen aguamala_ch=.
/*NA*/

gen aguamide_ch=.
/*NA*/

gen luz_ch=P1121

gen luzmide_ch=.
/*NA*/

gen combust_ch=(P1131==1 | P1132==2)

gen bano_ch=.
/*NA*/

gen banoex_ch=.
/*NA*/

gen des1_ch=0 if P111==6
replace des1_ch=1 if P111>=1 & P111<=3
replace des1_ch=2 if P111==4
replace des1_ch=3 if P111==5


gen des2_ch=0 if P111==6
replace des2_ch=1 if P111>=1 & P111<=3
replace des2_ch=2 if P111==4

gen piso_ch=0 if P103==6
replace piso_ch=1 if P103>=1 & P103<=5
replace piso_ch=2 if P103==7

gen pared_ch=0 if P102==3 | P102==4 | P102==7
replace pared_ch=1 if P102==1 | P102==2 | P102==5 | P102==6
replace pared_ch=2 if P102==8

gen techo_ch=.
/*NA*/

gen resid_ch=.
/*NA*/

**Daniela Zuluaga- Enero 2018: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**
	
*********************
***aguamejorada_ch***
*********************
g       aguamejorada_ch = 1 if (P110 >=1 & P110 <=3) | P110 ==5
replace aguamejorada_ch = 0 if  P110 ==4 | (P110 >=6 & P110 <=7)
		
*********************
***banomejorado_ch***
*********************
g       banomejorado_ch = 1 if (P111 >=1 & P111 <=4)
replace banomejorado_ch = 0 if (P111 >=5 & P111 <=6)

gen dorm_ch=.
/*NA*/

gen cuartos_ch=P104
replace cuartos_ch=. if P104==99

gen cocina_ch=.
/*NA*/

gen telef_ch=(P1141==1 & P1142==1)

gen refrig_ch=.
gen freez_ch=.
gen auto_ch=.
gen compu_ch=.
gen internet_ch=.
/*NA*/

gen cel_ch=(P1142==1)

gen vivi1_ch=1 if P101==1
replace vivi1_ch=2 if P101==2
replace vivi1_ch=3 if P101>2


gen vivi2_ch=(P101<=2)

gen viviprop_ch=0 if P105A==1
replace viviprop_ch=1 if P105A==2
replace viviprop_ch=2 if P105A==4
replace viviprop_ch=3 if P105A==3 | P105A>5

gen vivitit_ch=.
/*NA*/

gen vivialq_ch=P105B if viviprop_ch==0

/*gen vivialqimp_ch=P106
replace vivialqimp_ch=. if P106==99999*/

*Modificación Mayra Sáenz - Julio 2015
gen vivialqimp_ch= vivialqimp


/*_____________________________________________________________________________________________________*/
* Asignación de etiquetas e inserción de variables externas: tipo de cambio, Indice de Precios al 
* Consumidor (2011=100), líneas de pobreza
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
para anÃ¡lisis de trends (en el marco de estudios sobre el futuro del trabajo)*/
clonevar codocupa = P505 
clonevar codindustria = P506

compress


saveold "`base_out'", version(12) replace


log close

