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
local ANO "2003"
local ronda a 
local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
                        
capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Perú
Encuesta: ENAHO
Round: a
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
gen region_c=real(substr(ubigeo,1,2))
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

gen factor_ch=facpob07
label variable factor_ch "Factor de expansion del hogar"

/*Nota: La variable "factlab" se crea solo para PERU porque el factor para las variables laborales
 es diferente al factor de las demas variables, como las demograficas */
gen factlab=.
replace factlab=fac500
label var factlab "factor de expansion para variables laborales, solo PERU"

***************
****idh_ch*****
**************

sort conglome vivienda hogar 
egen idh_ch= group(conglome vivienda hogar)
label variable idh_ch "ID del hogar"

*************
****idp_ci****
**************

gen idp_ci=codperso
label variable idp_ci "ID de la persona en el hogar"

**********
***zona***
**********
/*
Variable estrato:
Área Urbana:	
1. mayor de 100,000 viviendas
2. de 20,001 a 100,000 viviendas
3. de 10,001 a 20,000 viviendas
4. de 4,001 a 10,000 viviendas
5. de 401 a 4000 viviendas	
		
Area Rural:	 
6. Centros poblados con menos de 400 viviendas habitantes
7. Areas de Empadronamiento Rural con mas de 140 viviendas (AER compuestos)
8. Areas de Empadronamiento Rural con menos de 140 viviendas (AER simples)
*/

gen byte zona_c=0 if estrato>=6
replace zona_c=1 if estrato<6

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

gen anio_c=2003
label variable anio_c "Anio de la encuesta"

*********
***mes***
*********

tostring fecent, replace 
gen mes_c=real(substr(fecent,2,2))
label variable mes_c "Mes de la encuesta"
*****************
***relacion_ci***
*****************

gen relacion_ci=.
replace relacion_ci=1 if p203==1
replace relacion_ci=2 if p203==2
replace relacion_ci=3 if p203==3
replace relacion_ci=4 if p203>=4 & p203<=7
replace relacion_ci=5 if p203==10 | p203==9
replace relacion_ci=6 if p203==8

label variable relacion_ci "Relacion con el jefe del hogar"
label define relacion_ci 1 "Jefe/a" 2 "Esposo/a" 3 "Hijo/a" 4 "Otros parientes" 5 "Otros no parientes"
label define relacion_ci 6 "Empleado/a domestico/a", add

label value relacion_ci relacion_ci
*************
***raza_ci***
*************

gen raza_ci=.
label define raza_ci 1 "Indígena" 2 "Afro-descendiente" 3 "Otros"
label value raza_ci raza_ci 
label value raza_ci raza_ci
label var raza_ci "Raza o etnia del individuo" 

*Modificación Mayra Sáenz 10/20/2015: modificaciones realizadas en base a metodología enviada por SCL/GDI Maria Olga Peña
gen raza_idioma_ci = .
gen id_ind_ci      = .
gen id_afro_ci     = .

****************************
***VARIABLES DEMOGRAFICAS***
****************************

***************
***factor_ci***
***************

gen factor_ci=facpob07
label variable factor_ci "Factor de expansion del individuo"

**********
***sexo***
**********

gen sexo_ci=p207

label define sexo_ci 1 "Hombre" 2 "Mujer"
label value sexo_ci sexo_ci

**********
***edad***
**********

gen edad_ci=p208a
replace edad_ci=. if p208a==99
label variable edad_ci "Edad del individuo"


*****************
***civil_ci***
*****************

gen civil_ci=.
replace civil_ci=1 if p209==6
replace civil_ci=2 if p209==1 | p209==2
replace civil_ci=3 if p209==4 | p209==5
replace civil_ci=4 if p209==3

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
/* Esta sección es para los residentes habituales del hogar mayores a 14 años */ 

****************
****condocup_ci*
****************
gen condocup_ci=.
replace condocup_ci=1 if ocu500==1
replace condocup_ci=2 if ocu500==2
replace condocup_ci=3 if condocup_ci!=1 & condocup_ci!=2
replace condocup_ci=4 if edad_ci<14
label define condocup_ci 1"ocupados" 2"desocupados" 3"inactivos" 4"menor de PET"
label value condocup_ci condocup_ci
label var condocup_ci "Condicion de ocupacion utilizando definicion del pais"

****************
*afiliado_ci****
****************
gen afiliado_ci=0     if condocup_ci==1 | condocup_ci==2 
replace afiliado_ci=1 if (p558a==1) & afiliado_ci==0
label var afiliado_ci "Afiliado a la Seguridad Social"

*Nota: seguridad social comprende solo los que en el futuro me ofrecen una pension.

****************
*tipopen_ci*****
****************
gen tipopen_ci=.

********************
*** instcot_ci *****
********************
gen instcot_ci=. /*La variable */
label var instcot_ci "institución a la cual cotiza"

****************
*cotizando_ci***
****************
gen cotizando_ci=0     if condocup_ci==1 | condocup_ci==2 
replace cotizando_ci=1 if ((p524b1>0 & p524b1!=.) | (p538b1>0 & p538b1!=.)) & cotizando_ci==0 /*a ocupados subordinados: empleados u obreros*/
label var cotizando_ci "Cotizante a la Seguridad Social"

gen cotizapri_ci=0     if condocup_ci==1 | condocup_ci==2 
replace cotizapri_ci=1 if (p524b1>0 & p524b1!=.)  
label var cotizapri_ci "Cotizante a la Seguridad Social por su trabajo principal"

gen cotizasec_ci=0     if condocup_ci==1 | condocup_ci==2 
replace cotizasec_ci=1 if (p538b1>0 & p538b1!=.) 
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
gen tamemp_ci=p512b
label var tamemp_ci "# empleados en la empresa de la actividad principal"
*/
gen tamemp_ci=1 if p512b>=1 &  p512b<=5 
label var  tamemp_ci "Tamaño de Empresa" 
*Empresas medianas
replace tamemp_ci=2 if p512b>=6 &  p512b<=50
*Empresas grandes
replace tamemp_ci=3 if p512b>=51 &  p512b<=9998
label define tamaño 1"Pequeña" 2"Mediana" 3"Grande"
label values tamemp_ci tamaño
tab tamemp_ci [iw=fac500]

****************
**categoinac_ci*
****************
gen categoinac_ci =1 if (p546==6 & condocup_ci==3)
replace categoinac_ci = 2 if  (p546==4 & condocup_ci==3)
replace categoinac_ci = 3 if  (p546==5 & condocup_ci==3)
replace categoinac_ci = 4 if  ((categoinac_ci ~=1 & categoinac_ci ~=2 & categoinac_ci ~=3) & condocup_ci==3)
label var categoinac_ci "Categoría de inactividad"
label define categoinac_ci 1 "jubilados o pensionados" 2 "Estudiantes" 3 "Quehaceres domésticos" 4 "Otros"
label value categoinac_ci categoinac_ci


*************
*ypen_ci*
*************

generat pjub=p5564c*30 if p5564b==1 
replace pjub=p5564c*4.3  if p5564b==2 
replace pjub=p5564c*2  if p5564b==3 
replace pjub=p5564c    if p5564b==4 
replace pjub=p5564c/2  if p5564b==5 
replace pjub=p5564c/3  if p5564b==6 
replace pjub=p5564c/6  if p5564b==7 
replace pjub=p5564c/12 if p5564b==8 
replace pjub=.         if p5564c==999999

generat pviudz=p5565c*30 if p5565b==1 
replace pviudz=p5565c*4.3 if p5565b==2
replace pviudz=p5565c*2  if p5565b==3 
replace pviudz=p5565c    if p5565b==4 
replace pviudz=p5565c/2  if p5565b==5 
replace pviudz=p5565c/3  if p5565b==6 
replace pviudz=p5565c/6  if p5565b==7 
replace pviudz=p5565c/12 if p5565b==8 
replace pviudz=.         if p5565c==999999

egen ypen_ci= rsum(pjub pviudz)
replace ypen_ci=. if pjub==. & pviudz==.
label var ypen_ci "Valor de la pension contributiva"


/*Modificación Mayra Sáenz- Julio 2015: Se reemplazan por variables originales
* 2003: 
gen     ypen_ci = 0
replace ypen_ci = d556t1/8 if pension_ci==1
replace ypen_ci = d556t2/8 if pension_ci==1
label var ypen_ci "Valor de la pension contributiva"*/

*************
**pension_ci*
*************
/*gen pension_ci=0 
replace pension_ci=1 if (p5564a==1 | p5565a==1 ) /* A todas las per mayores de 13*/
replace pension_ci=. if p5564a==. & p5565a==.
label var pension_ci "1=Recibe pension contributiva"*/

*MGD 8/31/2015: recibe pension si declara ingreso por pension
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
replace cesante_ci=1 if p552==1 & condocup_ci==2
label var cesante_ci "Desocupado - definicion oficial del pais"

*********
*lp_ci***
*********
gen lp_ci =linea03   
label var lp_ci "Linea de pobreza oficial del pais"

*********
*li_ci***
*********
gen lpe_ci =linpe03
label var lpe_ci "Linea de indigencia oficial del pais"

/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/

*************
**salmm_ci***
*************

gen salmm_ci= .
replace salmm_ci=410 if mes_c>=1 & mes_c<=9   /*abril-agosto*/
*replace salmm_ci=436 if mes_c==10             /*septiembre*/
replace salmm_ci=460 if mes_c>=10 & mes_c<=12 /*oct-noviembre*/
label var salmm_ci "Salario minimo legal"

***************
***tecnica_ci**
***************
gen tecnica_ci=(p301a==7 | p301a==8 | p304a==4 | p308a==4)
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

*************
***formal_ci***
*************
gen formal_ci=(cotizando_ci==1)

*****************
***desalent_ci***
*****************

gen desalent_ci=(emp_ci==0 & p545==2 & (p549==1 | p549==2))


*****************
***horaspri_ci***
*****************

gen horaspri_ci=p513t 
replace horaspri_ci=. if emp_ci~=1


*****************
***horastot_ci***
*****************

egen horastot_ci=rsum(horaspri_ci p518)
replace horastot_ci=. if horaspri_ci==. & p518==.
replace horastot_ci=. if emp_ci~=1

***************
***subemp_ci***
***************
/*
/*Sobre las horas normalmente trabajadas*/
gen subemp_ci=0
replace subemp_ci=1 if (p519==1 & horastot_ci<=30) & p521==1 
replace subemp_ci=1 if (p519==2 & p520<=30) & p521==1
replace subemp_ci=. if emp_ci==.
*/

* Modificacion con subempleo visible: quiere trabajar mas horas y esta disponible a trabajar mas horas. MGD 06/19/2014
gen subemp_ci=0
replace subemp_ci=1 if horaspri_ci<=30 & p521==1 & p521a==1 & emp_ci==1 

*******************
***tiempoparc_ci***
*******************
/*Sobre las horas normalmente trabajadas*/

gen tiempoparc_ci=0
/*replace tiempoparc_ci=1 if (p519==1 & horastot_ci<=30) & p521==2
replace tiempoparc_ci=1 if (p519==2 & p520<=30) & p521==2
replace tiempoparc_ci=. if emp_ci==.*/
* 10/20/2015 MGD: no se usa las horas totales trabajadas sino solo las de la actividad principal.
replace tiempoparc_ci=1 if (horaspri_ci>=1 & horaspri_ci<30) & p521==2 & emp_ci==1
replace tiempoparc_ci=. if emp_ci==0


******************
***categopri_ci***
******************
* 10/20/2015 MGD: se añade la categoria otra clasificacion para sacarlos de los no remunerados

gen categopri_ci=.
replace categopri_ci=0 if p507==7
replace categopri_ci=1 if p507==1
replace categopri_ci=2 if p507==2 
replace categopri_ci=3 if p507==3 | p507==4 | p507==6 
replace categopri_ci=4 if p507==5 /*| p507==7 */

label define categopri_ci 0 "Otra clasificación" 1"Patron" 2"Cuenta propia" 
label define categopri_ci 3"Empleado" 4" No remunerado", add
label value categopri_ci categopri_ci
label variable categopri_ci "Categoria ocupacional trabajo principal"


******************
***categosec_ci***
******************
* 10/20/2015 MGD: se añade la categoria otra clasificacion para sacarlos de los no remunerados

gen categosec_ci=.
replace categosec_ci=0 if p517==7
replace categosec_ci=1 if p517==1
replace categosec_ci=2 if p517==2 
replace categosec_ci=3 if p517==3 | p517==4 | p517==6
replace categosec_ci=4 if p517==5 /*| p517==7*/

label define categosec_ci 0 "Otra clasificación"  1"Patron" 2"Cuenta propia" 
label define categosec_ci 3"Empleado" 4 "No remunerado" , add
label value categosec_ci categosec_ci
label variable categosec_ci "Categoria ocupacional trabajo secundario"


*****************
***nempleos_ci***
*****************

gen nempleos_ci=.
replace nempleos_ci=1 if emp_ci==1
replace nempleos_ci=2 if emp_ci==1 & p514==1

#delimit ;
replace nempleos_ci=2 if emp_ci==1 & p514==2 & (p5151==1 | p5152==1 | p5153==1 |
        p5154==1 | p5155==1 | p5156==1 | p5157==1 | p5158==1 | p5159==1 | p51510==1 |
        p51511==1 | p51512==1);
#delimit cr

/*
*****************
***firmapeq_ci***
*****************

gen firmapeq_ci=.
replace firmapeq_ci=1 if p512b<=5
replace firmapeq_ci=0 if p512b>=6 & p512b<9999

*/
*****************
***spublico_ci***
*****************

gen spublico_ci=(p510==1 | p510==2 | p510==3)
replace spublico_ci=. if emp_ci~=1


**************
***ocupa_ci***
**************

gen ocupa_ci=.
replace ocupa_ci=1 if (p505>=211 & p505<=396) & emp_ci==1
replace ocupa_ci=2 if (p505>=111 & p505<=148) & emp_ci==1
replace ocupa_ci=3 if (p505>=411 & p505<=462) & emp_ci==1
replace ocupa_ci=4 if (p505>=571 & p505<=583) | (p505>=911 & p505<=931) & emp_ci==1
replace ocupa_ci=5 if (p505>=511 & p505<=565) | (p505>=941 & p505<=961) & emp_ci==1
replace ocupa_ci=6 if (p505>=611 & p505<=641) | (p505>=971 & p505<=973) & emp_ci==1
replace ocupa_ci=7 if (p505>=711 & p505<=886) | (p505>=981 & p505<=987) & emp_ci==1
replace ocupa_ci=8 if (p505>=11 & p505<=24) & emp_ci==1


*************
***rama_ci***
*************

gen rama_ci=.
replace rama_ci=1 if (p506>=100 & p506<=500) & emp_ci==1
replace rama_ci=2 if (p506>=1000 & p506<=1429) & emp_ci==1
replace rama_ci=3 if (p506>=1500 & p506<=3720) & emp_ci==1
replace rama_ci=4 if (p506>=4000 & p506<=4100) & emp_ci==1
replace rama_ci=5 if (p506>=4500 & p506<=4550) & emp_ci==1
replace rama_ci=6 if (p506>=5000 & p506<=5520) & emp_ci==1 
replace rama_ci=7 if (p506>=6000 & p506<=6420) & emp_ci==1
replace rama_ci=8 if (p506>=6500 & p506<=7020) & emp_ci==1
replace rama_ci=9 if (p506>=7111 & p506<=9900) & emp_ci==1



****************
***durades_ci***
****************
/*En meses*/

gen durades_ci=p551/4.3 /* sin filtro: if desemp_ci==1*/



*******************
***antiguedad_ci***
*******************

gen antiguedad_ci=.

*************************************************************************************
*******************************INGRESOS**********************************************
*************************************************************************************

*Mayra Sáenz- Julio 2015: En este quietly se encuentra la generación de ingresos anterior.
/*
quietly {

***************
***ylmpri_ci***
***************

*Para los trabajadores dependientes

gen ypridbd=.
replace ypridbd=p524d1*30 if p523==1 
replace ypridbd=p524d1*4.3 if p523==2 
replace ypridbd=p524d1*2 if p523==3 
replace ypridbd=p524d1 if p523==4 
replace ypridbd=. if p524d1==999999

replace ypridbd=0 if categopri_ci==4 
/* A los trabajadores no remunerados que trabajan menos de 15 horas la encuesta no les
pregunta acerca de sus ingresos y los manda a la sección de desempleo. Como este grupo en
realidad está trabajando, reemplazo su ingreso missing por cero*/

replace ypridbd=. if categopri_ci<=2

*Para los trabajadores independientes 
/*En este caso la pregunta no permite distinguir entre ingreso monetario
y en especie, vamos a asumir que es monetario*/

gen yprijbi=.
replace yprijbi=p530a
replace yprijbi=. if p530a==999999
replace yprijbi=. if categopri_ci>2


*Ingreso laboral monetario para todos

egen ylmpri_ci=rsum(yprijbi ypridbd)
replace ylmpri_ci=. if ypridbd==. & yprijbi==. 
replace ylmpri_ci=0 if p51112==1
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
replace especie`i'=p529`i'b*30  if p529`i'a==1
replace especie`i'=p529`i'b*4.3 if p529`i'a==2
replace especie`i'=p529`i'b*2   if p529`i'a==3
replace especie`i'=p529`i'b     if p529`i'a==4
replace especie`i'=p529`i'b/2   if p529`i'a==5
replace especie`i'=p529`i'b/3   if p529`i'a==6
replace especie`i'=p529`i'b/6   if p529`i'a==7
replace especie`i'=p529`i'b/12  if p529`i'a==8
replace especie`i'=. if p529`i'b==999999
}

egen ylnmprid=rsum(especie1 especie2 especie3 especie4 especie5 especie6)
replace ylnmprid=. if especie1==. &  especie2==. & especie3==. & especie4==. & especie5==. & especie6==.
replace ylnmprid=0 if categopri_ci==4
replace ylnmprid=0 if p528==2
replace ylnmprid=. if categopri_ci<=2 | categopri_ci==.

*Ingreso laboral no monetario de los independientes (autoconsumo)

gen ylnmprii=p536
replace ylnmprii=. if p536==999999
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
replace ysecbd=p538d1
replace ysecbd=. if p538d1==999999
replace ysecbd=0 if categosec_ci==4

replace ysecbd=. if categosec_ci<=2 | categosec_ci==.

*Para los trabajadores independientes
/*En este caso la pregunta no permite distinguir entre ingreso monetario
y en especie, vamos a asumir que es monetario*/

gen ysecjbi=.
replace ysecjbi=p541a
replace ysecjbi=. if p541a==999999
replace ysecjbi=. if categosec_ci>2

*Ingreso laboral monetario para todos

egen ylmsec_ci=rsum(ysecjbi ysecbd)
replace ylmsec_ci=. if ysecjbi==. & ysecbd==.
replace ylmsec_ci=0 if p53712==1
replace ylmsec_ci=. if emp~=1


******************
****ylnmsec_ci****
******************

*Ingreso laboral no monetario de los dependientes

forvalues i =1(1) 6 {

gen especiesec`i'=.
replace especiesec`i'=p540`i'b*30  if p540`i'a==1
replace especiesec`i'=p540`i'b*4.3 if p540`i'a==2
replace especiesec`i'=p540`i'b*2   if p540`i'a==3
replace especiesec`i'=p540`i'b     if p540`i'a==4
replace especiesec`i'=p540`i'b/2   if p540`i'a==5
replace especiesec`i'=p540`i'b/3   if p540`i'a==6
replace especiesec`i'=p540`i'b/6   if p540`i'a==7
replace especiesec`i'=p540`i'b/12  if p540`i'a==8
replace especiesec`i'=. if p540`i'b==999999
}

egen ylnmsecd=rsum(especiesec1 especiesec2 especiesec3 especiesec4 especiesec5 especiesec6)
replace ylnmsecd=. if especiesec1==. &  especiesec2==. & especiesec3==. & especiesec4==. & especiesec5==. & especiesec6==.
replace ylnmsecd=0 if categosec_ci==4
replace ylnmsecd=. if emp_ci~=1

*Ingreso laboral no monetario de los independientes (autoconsumo)

gen ylnmseci=p543
replace ylnmseci=. if p543==999999
replace ylnmseci=. if emp_ci~=1

*Ingreso laboral no monetario para todos

egen ylnmsec_ci=rsum(ylnmsecd ylnmseci)
replace ylnmsec_ci=. if ylnmsecd==. & ylnmseci==.
replace ylnmsec_ci=. if emp_ci==0


************
***ylm_ci***
************

*Sumamos ingresos extraordinarios por trabajo dependiente
gen ingext=p544t/12
replace ingext=. if p544t==999999

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

forvalues i=1(1) 7 {

gen transl`i'=.
replace transl`i'=p556`i'c*30  if p556`i'b==1
replace transl`i'=p556`i'c*4.3 if p556`i'b==2
replace transl`i'=p556`i'c*2   if p556`i'b==3
replace transl`i'=p556`i'c     if p556`i'b==4
replace transl`i'=p556`i'c/2   if p556`i'b==5
replace transl`i'=p556`i'c/3   if p556`i'b==6
replace transl`i'=p556`i'c/6   if p556`i'b==7
replace transl`i'=p556`i'c/12  if p556`i'b==8
replace transl`i'=. if p556`i'c==999999
}

egen transltot=rsum(transl1 transl2 transl3 transl4 transl5 transl6 transl7)
replace transltot=. if transl1==. & transl2==. & transl3==. & transl4==. & transl5==. & transl6==. & transl7==. 


forvalues i=1(1) 7 {

gen transe`i'=.
replace transe`i'=p556`i'e*30  if p556`i'd==1
replace transe`i'=p556`i'e*4.3 if p556`i'd==2
replace transe`i'=p556`i'e*2   if p556`i'd==3
replace transe`i'=p556`i'e     if p556`i'd==4
replace transe`i'=p556`i'e/2   if p556`i'd==5
replace transe`i'=p556`i'e/3   if p556`i'd==6
replace transe`i'=p556`i'e/6   if p556`i'd==7
replace transe`i'=p556`i'e/12  if p556`i'd==8
replace transe`i'=. if p556`i'e==999999
}

egen transetot=rsum(transe1 transe2 transe3 transe4 transe5 transe6 transe7)
replace transetot=. if transe1==. & transe2==. & transe3==. & transe4==. & transe5==. & transe6==. & transe7==. 

/*Rentas de la propiedad **** ENAHO 2003 NO TIENE LAS VARIBALES SOBRE RENTAS E INGRESOS EXTRAORDINARIOS 

forvalues i=1(1) 8 {

gen rentas`i'=.
replace rentas`i'=p557`i'c*30  if p557`i'b==1
replace rentas`i'=p557`i'c*4.3 if p557`i'b==2
replace rentas`i'=p557`i'c*2   if p557`i'b==3
replace rentas`i'=p557`i'c     if p557`i'b==4
replace rentas`i'=p557`i'c/2   if p557`i'b==5
replace rentas`i'=p557`i'c/3   if p557`i'b==6
replace rentas`i'=p557`i'c/6   if p557`i'b==7
replace rentas`i'=p557`i'c/12  if p557`i'b==8
replace rentas`i'=. if p557`i'c==999999
}

egen rentastot=rsum(rentas1 rentas2 rentas3 rentas4 rentas5 rentas6 rentas7 rentas8)
replace rentastot=. if rentas1==. & rentas2==. & rentas3==. & rentas4==. & rentas5==. & rentas6==. & rentas7==. & rentas8==.


* Otros ingresos extraordinarios

gen otrosexttot=p558t/12
replace otrosexttot=. if p558t==999999
*/

egen ynlm_ci=rsum(transltot transetot )
replace ynlm_ci=. if transltot==. & transetot==. 

**************
***ynlnm_ci***
**************

gen ynlnm_ci=.

****************
***remesas_ci***
****************

gen remesas_ci=transe3

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

gen p536_alt=p536
replace p536_alt=. if p536==999999

gen p543_alt=p543
replace p543_alt=. if p543==999999


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

gen rentaimp_ch=p106
replace rentaimp=. if p106==99999


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
* Mayra Sáenz- Julio 2015: Se reemplazan los ingresos por los originales del instituto de estadística del país, de acuerdo a sintaxis elaborada por Marcos Robles.
*==================================================================================================================================================================*


**************
***ylmpri_ci***
***************

recode i524d1 i530a i538d1 i541a d544t d556t1 d556t2 d557t d558t d529t d536 d540t d543 (.=0) (999999=0)
gen ypridbd = i524d1 /8
gen yprijbi = i530a  /8 
gen ysecbd  = i538d1 /8
gen ysecjbi = i541a  /8
gen ingext  = d544t  /8


egen ylmpri_ci=rsum(ypridbd yprijbi ingext), missing


*******************
*** nrylmpri_ci ***
*******************

gen nrylmpri_ci=.


******************
*** ylnmpri_ci ***
******************

gen ylnmprid = d529t /8
gen ylnmprii = d536  /8
gen ylnmsecd = d540t /8
gen ylnmseci = d543  /8

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

gen transltot = d556t1 /8
gen transetot = d556t2 /8
gen remesas_ci= d556t2 /8 if p5563a==1
recode remesas_ci (.=0)
gen d557t_1 = d557t/8 
gen d558t_1 = d558t/8
egen ynlm_ci  = rowtotal(transltot transetot d557t_1 d558t_1), missing  // d557t es renta de propiedad y d558t ing extraord

**************
***ynlnm_ci***
**************

gen vivialqimp = ia01hd /8
gen ynlnm_ci = (ig06hd+ig08hd+sig24+gru13hd1+gru13hd2+gru13hd3+gru23hd1+gru23hd2+gru23hd3+gru24hd+gru33hd1+gru33hd2+gru33hd3+gru34hd+gru43hd1+gru43hd2+gru43hd3+gru44hd+gru53hd1+gru53hd2+gru53hd3+gru54hd+gru63hd1+gru63hd2+gru63hd3+gru64hd+gru73hd1+gru73hd2+gru73hd3+gru74hd+gru83hd1+gru83hd2+gru83hd3+gru84hd) /(8 * nmiembros_ch)

********************
***Transferencias***
********************

*-Monetarias
* 2003
gen trac_pri = d556t1/8 if p5561a==1 | p5562a==1 | p5563a==1 | p5567a==1
*Modificación SGR 2017 06
gen trac_pub = d556t1/8 if p5566a==1  
*gen trac_pub = d556t1/8 if p5561a==6 

*-No Monetarias
* 2003: donaciones publicas y privadas
gen dona_pub = (gru13hd1+gru23hd1+gru33hd1+gru43hd1+gru53hd1+gru63hd1+gru73hd1+gru83hd1) /(8 * mieperho)
gen dona_pri = ynlnm_ci - dona_pub


* TOTAL (las privadas incluyen transferencias del exterior)

egen trat_pri = rsum( trac_pri  dona_pri  transetot), missing
egen trat_pub = rsum( trac_pub  dona_pub), missing

****************
*Rentas y otros*
****************
egen rtasot = rsum(d557t_1  d558t_1), missing
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

gen p536_alt=p536
replace p536_alt=. if p536==999999

gen p543_alt=p543
replace p543_alt=. if p543==999999


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

*gen rentaimp_ch=p106
*replace rentaimp=. if p106==99999

*Modificación Mayra Sáenz - Julio 2015
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

*Los q responden con antiguo sistema de prima(p301b): habian cinco años de primaria.
gen byte aedu_ci=.
replace aedu_ci=0  if p301a==1 | p301a==2
replace aedu_ci=1  if p301a==3 & p301b==0
replace aedu_ci=2  if p301a==3 & p301b==1 
replace aedu_ci=3  if p301a==3 & p301b==2
replace aedu_ci=4  if p301a==3 & p301b==3
replace aedu_ci=5  if p301a==3 & p301b==4
*Como se responde actual sist prim (p301c): seis años de primaria.
replace aedu_ci=0  if p301a==3 & p301c==0 & p301b==0
replace aedu_ci=1  if p301a==3 & p301c==1 & p301b==0
replace aedu_ci=2  if p301a==3 & p301c==2 & p301b==0
replace aedu_ci=3  if p301a==3 & p301c==3 & p301b==0
replace aedu_ci=4  if p301a==3 & p301c==4 & p301b==0
replace aedu_ci=5  if p301a==3 & p301c==5 & p301b==0
replace aedu_ci=6  if p301a==4 
replace aedu_ci=7  if p301a==5 & p301b==1
replace aedu_ci=8  if p301a==5 & p301b==2
replace aedu_ci=9  if p301a==5 & p301b==3
replace aedu_ci=10 if p301a==5 & p301b==4
replace aedu_ci=11 if p301a==6 
replace aedu_ci=12 if (p301a>=7 & p301a<=10) & p301b==1
replace aedu_ci=13 if (p301a>=7 & p301a<=10) & p301b==2
replace aedu_ci=14 if (p301a>=7 & p301a<=10) & p301b==3
replace aedu_ci=15 if (p301a>=7 & p301a<=10) & p301b==4
replace aedu_ci=16 if (p301a>=7 & p301a<=10) & p301b==5
replace aedu_ci=17 if (p301a>=7 & p301a<=10) & p301b==6
replace aedu_ci=18 if (p301a>=7 & p301a<=10) & p301b==7
replace aedu_ci=19 if p301a==11 & p301b==1
replace aedu_ci=20 if p301a==11 & p301b==2
replace aedu_ci=21 if p301a==11 & p301b==3
replace aedu_ci=22 if p301a==11 & p301b==4
replace aedu_ci=. if p212==.

**************
***eduno_ci***
**************

gen byte eduno_ci=(p301a==1 | p301a==2) 
replace eduno_ci=. if p301a==99
replace eduno_ci=. if p212==. | aedu_ci==.
label variable eduno_ci "Cero anios de educacion"

**************
***edupi_ci***
**************

gen byte edupi_ci=(p301a==3)
replace edupi_ci=. if p301a==99
replace edupi_ci=. if p212==. | aedu_ci==.
label variable edupi_ci "Primaria incompleta"

**************
***edupc_ci***
**************

gen byte edupc_ci=(p301a==4)
replace edupc_ci=. if p301a==99
replace edupc_ci=. if p212==. | aedu_ci==.
label variable edupc_ci "Primaria completa"

**************
***edusi_ci***
**************

gen byte edusi_ci=(p301a==5)
replace edusi_ci=. if p301a==99
replace edusi_ci=. if p212==. | aedu_ci==.
label variable edusi_ci "Secundaria incompleta"

**************
***edusc_ci***
**************

gen byte edusc_ci=(p301a==6)
replace edusc_ci=. if p301a==99
replace edusc_ci=. if p212==. | aedu_ci==.
label variable edusc_ci "Secundaria completa"

***************
***edus1i_ci***
***************

gen byte edus1i_ci=(p301a==5 & aedu_ci<=8)
replace edus1i_ci=. if p301a==99
replace edus1i_ci=. if p212==. | aedu_ci==.
label variable edus1i_ci "1er ciclo de la secundaria incompleto"

***************
***edus1c_ci***
***************

gen byte edus1c_ci=(p301a==5 & aedu_ci==9)
replace edus1c_ci=. if p301a==99
replace edus1c_ci=. if p212==. | aedu_ci==.
label variable edus1c_ci "1er ciclo de la secundaria completo"

***************
***edus2i_ci***
***************

gen byte edus2i_ci=(p301a==5 & aedu_ci==10)
replace edus2i_ci=. if p301a==99
replace edus2i_ci=. if p212==. | aedu_ci==.
label variable edus2i_ci "2do ciclo de la secundaria incompleto"

***************
***edus2c_ci***
***************

gen byte edus2c_ci=(p301a==6)
replace edus2c_ci=. if p301a==99
replace edus2c_ci=. if p212==. | aedu_ci==.
label variable edus2c_ci "2do ciclo de la secundaria completo"

**************
***eduui_ci***
**************

gen byte eduui_ci=(p301a==7 | p301a==9)
replace eduui_ci=. if p301a==99
replace eduui_ci=. if p212==. | aedu_ci==.
label variable eduui_ci "Universitaria incompleta"

***************
***eduuc_ci***
***************

gen byte eduuc_ci=(p301a==8 | p301a==10 | p301a==11)
replace eduuc_ci=. if p301a==99
replace eduuc_ci=. if p212==. | aedu_ci==.
label variable eduuc_ci "Universitaria incompleta o mas"


***************
***edupre_ci***
***************

gen byte edupre_ci=(p301a==2)
replace edupre_ci=. if p301a==99
replace edupre_ci=. if p212==. | aedu_ci==.
label variable edupre_ci "Educacion preescolar"


**************
***eduac_ci***
**************
gen byte eduac_ci=.
replace eduac_ci=1 if (p301a==9 | p301a==10)
replace eduac_ci=0 if (p301a==7 | p301a==8)
label variable eduac_ci "Superior universitario vs superior no universitario"

***************
***asiste_ci***
***************
/*Se considera la variable p303 para aquellos entrevistados el primer trimestre
ya que al momento de la encuesta es periodo de vacaciones */
destring mes, replace
g asiste_ci = (p307==1 | (p303==1 & (mes>=1 & mes<=3)))
replace asiste_ci=. if p212==.
label variable asiste_ci "Asiste actualmente a la escuela"

**************
***pqnoasis***
**************


*Modificado Mayra Sáenz - Junio 2016- antes se generaba como missing
gen pqnoasis_ci=p313

label variable pqnoasis_ci "Razones para no asistir a la escuela"
label define pqnoasis_ci 1 "SS militar" 2 "Trabajando"
label define pqnoasis_ci 3 "No centro ens.adult." 4 "No centro ens.pob", add 
label define pqnoasis_ci 5 "No interesa" 6 "Edad insuficiente" , add
label define pqnoasis_ci 7 "Enfermedad" 8 "Prob economicos", add
label define pqnoasis_ci 9 "Prob familiares" 10 "Bajas notas", add
label define pqnoasis_ci 11 "Quehaceres domesticos" 12 "Termino estudios", add
label define pqnoasis_ci 13 "Otros" 99 "Missing", add
label value pqnoasis_ci pqnoasis_ci

**************
*pqnoasis1_ci*
**************
**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**

g       pqnoasis1_ci = 1 if p313==8
replace pqnoasis1_ci = 2 if p313==2
replace pqnoasis1_ci = 3 if p313==7 | p313==9
replace pqnoasis1_ci = 4 if p313==5
replace pqnoasis1_ci = 5 if p313==11
replace pqnoasis1_ci = 6 if p313==12
replace pqnoasis1_ci = 7 if p313==6
replace pqnoasis1_ci = 8 if p313==3 | p313==4
replace pqnoasis1_ci = 9 if p313==1 | p313==10 | p313==13

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

gen edupub_ci=(p301d==1)
replace edupub_ci=. if p212==.



**********************************
**** VARIABLES DE LA VIVIENDA ****
**********************************

gen aguared_ch=(p110==1 | p110==2)

gen aguadist_ch=1 if p110==1
replace aguadist_ch=2 if p110==2
replace aguadist_ch=3 if p110>=3 & p110<=7

gen aguamala_ch=.
/*NA*/

gen aguamide_ch=.
/*NA*/

gen luz_ch=p1121

gen luzmide_ch=.
/*NA*/

* MR: correción sintáxis
/*
gen combust_ch=(p1131==1 | p1132==2)
*/
gen combust_ch=(p1131==1 | p1132==1)

gen bano_ch=.
/*NA*/

gen banoex_ch=.
/*NA*/

gen des1_ch=0 if p111==6
replace des1_ch=1 if p111>=1 & p111<=3
replace des1_ch=2 if p111==4
replace des1_ch=3 if p111==5


gen des2_ch=0 if p111==6
replace des2_ch=1 if p111>=1 & p111<=3
replace des2_ch=2 if p111==4

gen piso_ch=0 if p103==6
replace piso_ch=1 if p103>=1 & p103<=5
replace piso_ch=2 if p103==7

gen pared_ch=0 if p102==3 | p102==4 | p102==7
replace pared_ch=1 if p102==1 | p102==2 | p102==5 | p102==6
replace pared_ch=2 if p102==8

gen techo_ch=0 if p103a>=5 & p103a<=7
replace techo_ch=1 if p103a>=1 & p103a<=4
replace techo_ch=2 if p103a==8

gen resid_ch=.
/*NA*/

**Daniela Zuluaga- Enero 2018: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**
	
*********************
***aguamejorada_ch***
*********************
g       aguamejorada_ch = 1 if (p110 >=1 & p110 <=3) | p110 ==5
replace aguamejorada_ch = 0 if  p110 ==4 | (p110 >=6 & p110 <=7)
		
*********************
***banomejorado_ch***
*********************
g       banomejorado_ch = 1 if (p111 >=1 & p111 <=4)
replace banomejorado_ch = 0 if (p111 >=5 & p111 <=6)

gen dorm_ch=.
/*NA*/

gen cuartos_ch=p104

gen cocina_ch=.
/*NA*/

gen telef_ch=(p1141==1 & p1142==1)


* MR: se incluye base de hogar con durable goods la cual no habia sido incluida y se generan estas variables las cuales estaban como missing
gen refrig_ch= (p6124==1)
gen freez_ch=.
gen auto_ch= (p61215==1)
gen compu_ch=(p61220==1)
/*NA*/

gen internet_ch=(p1144==1)

gen cel_ch=(p1142==1)

gen vivi1_ch=1 if p101==1
replace vivi1_ch=2 if p101==2
replace vivi1_ch=3 if p101>2


gen vivi2_ch=(p101<=2)

gen viviprop_ch=0 if p105a==1
replace viviprop_ch=1 if p105a==2
replace viviprop_ch=2 if p105a==4
replace viviprop_ch=3 if p105a==3 | p105a>5

gen vivitit_ch=.
/*NA*/

gen vivialq_ch=p105b if viviprop_ch==0

/*gen vivialqimp_ch=p106
replace vivialqimp_ch=. if p106==99999*/

*Modificación Mayra Sáenz - Julio 2015
gen vivialqimp_ch= vivialqimp





*****************
*** PERU 2003 ***
*****************

* 22067 Obs => IV Trim. 2003

/* p203 ¿Cuál es la relación de parentesco con el jefe del hogar?
 0 Panel (No Presente el año actual)
 1 Jefe(a)
 2 Esposo(a)
 3 Hijo(a)
 4 Yerno/Nuera
 5 Nieto
 6 Padres/Suegros
 7 Otros parientes
 8 Trabajador Hogar
 9 Pensionista
10 Otros no parientes
*/

* Variables

gen parentco=p203 
gen edad = p208a
gen sexo = p207
*gen factorex=round(f_trimes,1) /* Factor de Expansión Trimestral */

/*
ESTRATO 

Estrato Geográfico
 1 Centros poblados de más de 100,000 viviendas
 2 Centros poblados de 20,001 a 100,000 viviendas
 3 Centros poblados de 10,001 a 20,000 viviendas
 4 Centros poblados de 4,001 a 10,000 viviendas
 5 Centros poblados de 401 a 4,000 viviendas

 6 Centros poblados con menos de 401 viviendas
 7 Area de Empadronamiento Rural Compuesta  AER Compuesto
 8 Area de Empadronamiento Simple  AER Simple
*/

gen area=1 if 	  (estrato>=1 & estrato<=5)
replace area=2 if (estrato>=6 & estrato<=8)

** Years of education. 
/* 
Mayores de 3 años de edad(inclusive)
301. ¿CUAL ES EL ULTIMO AÑO O GRADO DE ESTUDIOS Y NIVEL QUE APROBO?

p301a					p301b	p301c
Nivel educativo que aprobó		Año	Grado
1 Sin nivel				---	---
2 Educación Inicial			---	---
3 Primaria incompleta			0-4	0-5
4 Primaria completa			0,5-6	6
5 Secundaria incomplet			1-4	---
6 Secundaria completa			5,6	---
7 Superior no Universitaria Incompleta	1-4	---	
8 Superior no Universitaria Completa	3-5	---		
9 Superior Universitaria Incompleta	1-5,6	---
10 Superior Universitaria Completa	4-7	---
11 Post -Grado Universitario		1-2	---
99 Missing value
*/

tab p301b p301c if p301a==3
tab p301b if p301a==4
tab p301b if p301a==4 & p307==1

gen transicion=1 if (p301a==3 & p301b==0 & p301c==0)	

gen anoest=0 if 	p301a==1 | p301a==2 

replace anoest=1 if	(p301a==3 & p301c==1 & (p301b==0 | p301b==.)) | transicion==1
replace anoest=2 if	(p301a==3 & p301b==1 & (p301c==0 | p301c==.)) | (p301a==3 & p301c==2 & (p301b==0 | p301b==.))
replace anoest=3 if	(p301a==3 & p301b==2 & (p301c==0 | p301c==.)) | (p301a==3 & p301c==3 & (p301b==0 | p301b==.))
replace anoest=4 if	(p301a==3 & p301b==3 & (p301c==0 | p301c==.)) | (p301a==3 & p301c==4 & (p301b==0 | p301b==.))
replace anoest=5 if	(p301a==3 & p301b==4 & (p301c==0 | p301c==.)) | (p301a==3 & p301c==5 & (p301b==0 | p301b==.))
replace anoest=6 if	(p301a==4 & (p301b>=5 & p301b<=6) & (p301c==0 | p301c==.)) | (p301a==4 & p301c==6  & (p301b==0 | p301b==.))
replace anoest=7 if	(p301a==5 & p301b==1)				
replace anoest=8 if	(p301a==5 & p301b==2)				
replace anoest=9 if	(p301a==5 & p301b==3)				
replace anoest=10 if	(p301a==5 & p301b==4)
replace anoest=11 if	(p301a==6 & (p301b>=5 & p301b<=6))
replace anoest=12 if	(p301a>=7 & p301a<=10) & p301b==1
replace anoest=13 if	(p301a>=7 & p301a<=10) & p301b==2
replace anoest=14 if	(p301a>=7 & p301a<=10) & p301b==3
replace anoest=15 if	(p301a>=7 & p301a<=10) & p301b==4
replace anoest=16 if	(p301a>=7 & p301a<=10) & p301b==5
replace anoest=17 if	(p301a>=7 & p301a<=10) & p301b==6 | (p301a==11) & p301b==1
replace anoest=18 if	(p301a>=7 & p301a<=10) & p301b==7 | (p301a==11) & p301b==2
replace anoest=19 if 	(p301a==11) & p301b==3 /* No obs */
replace anoest=20 if 	(p301a==11) & p301b==4 /* No obs */

** Economic Active Population 
* Included in the database (14 years or more of age)
/*
ocu500
indicador de la	pea  (inei)	Freq.	Percent	Cum.
14 años y más			
0. ---
1. ocupado
2. desocupado abierto	
3. desocupado oculto
4. no pea	
*/

gen peaa=1 if      ocu500==1
replace peaa=2 if (ocu500>=2 & ocu50<=2)
replace peaa=3 if  ocu500==4

gen tasadeso=0 if peaa==1
replace tasadeso=1 if peaa==2

************************
*** MDGs CALCULATION ***
************************
/*
p204
204. ¿ES MIEMBRO DEL HOGAR?
 1. Si
 2. No

Mayores de 3 años de edad
301. ¿CUAL ES EL ULTIMO AÑO O GRADO DE ESTUDIOS Y NIVEL QUE APROBO?

p301a					p301b	p301c	p301d Centro de estudios
Nivel educativo que aprobó		Año	Grado	1. Estatal 2.No Estatal
1 Sin nivel				---	---	1-2
2 Inicial				---	---	1-2
3 Primaria incompleta			0-5	0-5	1-2
4 Primaria completa			5-6	6	1-2 ==> 303
5 Secundaria incomplet			1-4	---	1-2 ==> 303
6 Secundaria completa			5,6	---	1-2 ==> 303
7 Sup. no Univ.Incomp.			1-4	---	1-2 ==> 303	 
8 Sup.no Univ. Comp.			3-5	---	1-2 ==> 303		
9 Sup. Univ. Incompl.			1-6	---	1-2 ==> 303
10 Sup. Univ. Completa			4-7	---	1-2 ==> 303
11 Post -Grado Universitario		1-3	---	1-2 ==> 303
99 Missing value			

p302 
302. ¿Sabe leer y escribir?

p303 
En el 2002, ¿Estuvo matriculado en algún centro o programa de enseñanza regular?
1 Si ==> 304 ==> 305 ==>306
2 No ==> 306
. Missing value

p304a Nivel educativo al que asiste
p304b Año de estudios al que asiste
p304c Grado de estudios al que asiste
p304d Centro de estudios al que asiste
p305 El resultado que obtuvo en el 2002 fue?

p306 
306. Este año, ¿Está matriculado en algún centro o programa de enseñanza regular?
1 Si
2 No ==>
. Missing value

p307 Actualmente, asiste a algún centro de enseñanza regular?
1 Si
2 No
. Missing value

p308a 					p308b	p308c	p308d Centro de estudios
308. Nivel educativo al que asiste	Año	Grado	1. Estatal 2.No Estatal
 1 Educación Inicial			---	---	1-2
 2 Primaria				0	1-6	1-2
 3 Secundaria				1-5	---	1-2
 4 Superior No Universitaria		1-5	---	1-2
 5 Superior Universitaria		1-6	---	1-2
 6 Post-grado Universitario, 		1-2	---	1-2

p310
310. EN LOS ULTIMOS 12 MESES, ¿ASISTIO A ALGUN CENTRO DE
ESTUDIOS NO REGULAR COMO ACADEMIA, CEOS U OTROS?
*/

*** GOAL 2. ACHIEVE UNIVERSAL PRIMARY EDUCATION

** Target 3, Indicator: Net Attendance Ratio in Primary
* ISCED 1

gen NERP=0 if     p204==1 & (edad>=6 & edad<=11) & (p307==1 | p307==2) 
replace NERP=1 if p204==1 & (edad>=6 & edad<=11) & (p307==1) & (p308a==2)

** Target 3, Additional Indicator: Net Attendance Ratio in Secondary
* ISCED 2 & 3

gen NERS=0 if     p204==1 & (edad>=12 & edad<=16) & (p307==1 | p307==2) 
replace NERS=1 if p204==1 & (edad>=12 & edad<=16) & (p307==1) & (p308a==3)

** Target 3, Indicator: Literacy Rate of 15-24 Years Old
* At least 5 years of formal education

gen ALFABET=0 if     p204==1 & (edad>=15 & edad<=24) & (anoest>=0 & anoest<99)
replace ALFABET=1 if p204==1 & (edad>=15 & edad<=24) & (anoest>=5 & anoest<99)

** Target 3, Indicator: Literacy Rate of 15-24 Years Old
* Read & write

gen leeYesc=0 if (p301a>=1 & p301a<=11)
replace leeYesc=1 if (p302==1 | (p301a>=4 & p301a<=11))

gen ALFABET2=0 if     p204==1 & (edad>=15 & edad<=24) & (leeYesc==0 | leeYesc==1)
replace ALFABET2=1 if p204==1 & (edad>=15 & edad<=24) & (leeYesc==1)

*** GOAL 3 PROMOTE GENDER EQUALITY AND EMPOWER WOMEN

gen prim=1 if (p307==1 & p308a==2) & p204==1
gen sec=1 if  (p307==1 & p308a==3) & p204==1
gen ter=1 if  (p307==1 & (p308a==4 | p308a==5)) & p204==1

** Target 4, Indicator: Ratio Girls to boys in primary, secondary and tertiary (%)

** Target 4, Ratio of Girls to Boys in Primary*

gen RPRIMM=1 if (prim==1) & sexo==2 
replace RPRIMM=0 if RPRIMM==. 
gen RPRIMH=1 if (prim==1) & sexo==1 
replace RPRIMH=0 if RPRIMH==.

gen RATIOPRIM=0 if (prim==1) & sexo==2  
replace RATIOPRIM=1 if (prim==1)  & sexo==1   

** Target 4, Ratio of Girls to Boys in Secondary*

gen RSECM=1 if (sec==1) & sexo==2 
replace RSECM=0 if RSECM==.
gen RSECH=1 if (sec==1) & sexo==1 
replace RSECH=0 if RSECH==.

gen RATIOSEC=0 if (sec==1) & sexo==2 
replace RATIOSEC=1 if (sec==1) & sexo==1  

** Target 4, Indicator: Ratio of Girls to Boys in Tertiary*

gen RTERM=1 if (ter==1) & sexo==2 
replace RTERM=0 if RTERM==.
gen RTERH=1 if (ter==1) & sexo==1 
replace RTERH=0 if RTERH==.

gen RATIOTER=0 if (ter==1) & sexo==2 
replace RATIOTER=1 if (ter==1) & sexo==1  

** Target 4, Indicator: Ratio of Girls to Boys in Primary, Secondary and Tertiary*

gen RALLM=1 if (prim==1 | sec==1 | ter==1) & sexo==2 
replace RALLM=0 if RALLM==.
gen RALLH=1 if (prim==1 | sec==1 | ter==1) & sexo==1 
replace RALLH=0 if RALLH==.
gen RATIOALL=0 if (prim==1 | sec==1 | ter==1) & sexo==2  
replace RATIOALL=1 if (prim==1 | sec==1 | ter==1) & sexo==1    

** Target 4, Indicator: Ratio of literate women to men 15-24 year olds*

gen MA=1 if ((anoest>=5 & anoest<99) & (edad>=15 & edad<=24) & (sexo==2)) & p204==1
replace MA=0 if MA==.
gen HA=1 if ((anoest>=5 & anoest<99) & (edad>=15 & edad<=24) & (sexo==1)) & p204==1
replace HA=0 if HA==.
gen RATIOLIT=0 if ((anoest>=5 & anoest<99) & (edad>=15 & edad<=24) & (sexo==2)) & p204==1
replace RATIOLIT=1 if ((anoest>=5 & anoest<99) & (edad>=15 & edad<=24) & (sexo==1)) & p204==1 

** Target 4, Indicator: Ratio of literate women to men 15-24 year olds*
* Read & write
 
gen MARW=1 if ((p302==1 | (p301a>=4 & p301a<=11)) & (edad>=15 & edad<=24) & (sexo==2)) & p204==1
replace MARW=0 if MARW==.
gen HARW=1 if ((p302==1 | (p301a>=4 & p301a<=11)) & (edad>=15 & edad<=24) & (sexo==1)) & p204==1
replace HARW=0 if HARW==.
gen RATIOLITRW=0 if ((p302==1 | (p301a>=4 & p301a<=11)) & (edad>=15 & edad<=24) & (sexo==2)) & p204==1
replace RATIOLITRW=1 if ((p302==1 | (p301a>=4 & p301a<=11)) & (edad>=15 & edad<=24) & (sexo==1)) & p204==1

** Target 4, Indicator: Share of women in wage employment in the non-agricultural sector (%)

/*
p507						p506
507. EN SU CENTRO DE TRABAJO UD. ERA:		¿A QUE SE DEDICA EL NEGOCIO, ORGANISMO
						O EMPRESA EN LA QUE TRABAJO?
1. ¿Empleador o patrono? 
2. ¿Trabajador Independiente?
3. ¿Empleado? 			=>510
4. ¿Obrero?			=>510
5. ¿Trabajador Familiar No
    Remunerado? 		=>511
6. ¿Trabajador del Hogar?	=>511
7. ¿Otro?			=>510
*/

* Without Domestic Service

gen WENAS=0 if     (edad>=15 & edad<=64) & (p507==3 | p507==4) & (p506>=1110 & p506<9900) & peaa==1 & p204==1 
replace WENAS=1 if (edad>=15 & edad<=64) & (p507==3 | p507==4) & (p506>=1110  & p506<9900) & peaa==1 & (sexo==2) & p204==1

** Target 4, Indicator: Share of women in wage employment in the non-agricultural sector (%)
* With domestic servants

gen WENASD=0 if     (edad>=15 & edad<=64) & (p507==3 | p507==4 | p507==6) & (p506>=1110 & p506<9900) & peaa==1 & p204==1
replace WENASD=1 if (edad>=15 & edad<=64) & (p507==3 | p507==4 | p507==6) & (p506>=1110 & p506<9900) & peaa==1 & (sexo==2) & p204==1

*** GOAL 7 ENSURE ENVIROMENTAL SUSTAINABILITY

** Access to Electricity ** Additional Indicator

/*
112. ¿CUAL ES EL TIPO DE ALUMBRADO QUE TIENE SU HOGAR?
(Acepte una o más alternativas)
1. Electricidad				p1121
2. Kerosene (mechero/lamparín)		p1122
3. Petróleo/gas (lámpara)		p1123
4. Vela 				p1124
5. Generador 				p1125
6. Otro 				p1126

7. NO UTILIZA 				p1127
*/

gen ELEC=0 if     p204==1 
replace ELEC=1 if p204==1 & (p1121==1 | p1125==1)

** Target 9, Indicator: Proportion of the population using solidfuels (%)
/*
113. ¿CUAL ES EL COMBUSTIBLE QUE USAN EN EL
HOGAR PARA COCINAR SUS ALIMENTOS?
(Acepte una o más alternativas)
1. Electricidad 	p1131
2. Gas 			p1132
3. Kerosene 		p1133
4. Carbón		p1134
5. Leña 		p1135
6. Otro(Especifique)	p1136
7. NO COCINAN		p1137
*/

gen solidfuels=1 if p1134==1 | p1135==1
replace solidfuels=0 if solidfuels==.

gen otherfuel=1 if p1131==1 | p1132==1 | p1133==1 | p1136==1
replace otherfuel=0 if otherfuel==.

gen SFUELS=0 if     p204==1
replace SFUELS=1 if p204==1 & (solidfuels==1 & otherfuel==0)	/* Exclusive use of solidfuels */

** Target 10, Indicator: Proportion of the population with sustainable access to an improved water source (%)
/*
p110
110. EL ABASTECIMIENTO DE AGUA EN SU HOGAR PROCEDE DE:
 1. ¿Red pública, dentro de la vivienda?
 2. ¿Red pública, fuera de la vivienda pero dentro del edificio?
 3. ¿Pilón de uso público? 
 4. ¿Camión - cisterna u otro similar?
 5. ¿Pozo? 
 6. ¿Río, acequia, manantial o similar ?
 7. ¿Otra?
*/

gen WATER=0 if     p204==1 & (p110>=1 & p110<=7)
replace WATER=1 if p204==1 & ((p110>=1 & p110<=3)|p110==5)

** Target 10, Indicator: Proportion of Population with Access to Improved Sanitation, Urban and Rural (%)
/*
p111
111. EL SERVICIO HIGIENICO QUE TIENE SU HOGAR ESTA CONECTADO A:
 1. ¿Red pública, dentro de la vivienda?
 2. ¿Red pública, fuera de la vivienda pero dentro del edificio?
 3. ¿Pozo séptico?
 4. ¿Pozo ciego o negro/letrina?
 5. ¿Río, acequia o canal ?
 6. NO TIENE
*/
 
gen SANITATION=0 if     p204==1 & (p111>=1 & p111<=6)
replace SANITATION=1 if p204==1 & (p111>=1 & p111<=3)

** Target 11, Indicator: Proportion of the population with access to secure tenure (%)
/*
** Datos de vivienda
101. TIPO DE VIVIENDA:
 1. Casa independiente
 2. Departamento en edificio
 3. Vivienda en quinta 
 4. Vivienda en casa de vecindad (Callejón, solar o corralón) 
 5. Choza o cabaña 
 6. Vivienda improvisada
 7. Local no destinado para habitación humana
 8. Otro

** Datos del hogar
p105a
105. LA VIVIENDA QUE OCUPA SU HOGAR ES:
 1. ¿Alquilada?
 2. ¿Propia, totalmente pagada?
 3. ¿Propia, por invasión?
 4. ¿Propia, comprándola a plazos?
 5. ¿Cedida por el centro de trabajo?
 6. ¿Cedida por otro hogar o institución? 
 7. ¿Otra forma?

** Datos de vivienda
102. EL MATERIAL PREDOMINANTE EN LAS PAREDES
EXTERIORES ES:
 1. ¿Ladrillo o bloque de cemento?
 2. ¿Piedra o sillar con cal o cemento?
 3. ¿Adobe o tapia?
 4. ¿Quincha (caña con barro)?
 5. ¿Piedra con barro?
 6. ¿Madera?
 7. ¿Estera?
 8. ¿Otro material?

** Datos de vivienda
103. EL MATERIAL PREDOMINANTE EN LOS PISOS ES:
 1. ¿Parquet o madera pulida?
 2. ¿Láminas asfálticas, vinílicos o similares? 
 3. ¿Losetas, terrazos o similares?
 4. ¿Madera (entablados)?
 5. ¿Cemento?
 6. ¿Tierra? 
 7. ¿Otro material?

** Datos de vivienda
104. SIN CONTAR BAÑO, COCINA, PASADIZOS NI
GARAJE ¿CUANTAS HABITACIONES EN TOTAL
TIENE LA VIVIENDA?
*/

* Hogar. Número secuencial del hogar 
* 11-55 => 1st Digit => Household number, 
*	=> 2nd Digit => Number of households in the dwelling

gen str1 hog_no=substr(hogar,1,1)
gen str1 numhog=substr(hogar,2,1)

destring hog_no numhog codperso,replace

tab hog_no
tab p101 hog_no
tab hog_no if p101==.

* Dwelling ID
gen id_dwelling_=1 if hog_no==1 & codperso==1
gen id_dwelling=sum(id_dwelling_)

* Household ID
gen id_household_=1 if codperso==1
gen id_household=sum(id_household_)

egen tipoviv=max(p101), by(id_dwelling)
gen tenencia = p105a
egen paredes=max(p102), by(id_dwelling)
egen piso=max(p103),	by(id_dwelling)
egen nrocuart=max(p104),by(id_dwelling)

gen persroom=totmieho/nrocuart 		/*Total de personas en el hogar/nrocuart */
global variable SECTEN
gen SECTEN=1 if     p204==1
replace SECTEN=0 if p204==1 & (((tenencia==5 | tenencia==6 | tenencia==7) | (tipoviv==5 | tipoviv==6 | tipoviv==7 | tipoviv==8)) | ((paredes==4 | paredes==5 | paredes==7 | paredes==8)| (piso==6 | piso==7)) | (persroom>=3) | (SANITATION==0 | WATER==0))

* Dirt floors ** Additional indicator
/*
** Datos de vivienda
103. EL MATERIAL PREDOMINANTE EN LOS PISOS ES:
 1. ¿Parquet o madera pulida?
 2. ¿Láminas asfálticas, vinílicos o similares? 
 3. ¿Losetas, terrazos o similares?
 4. ¿Madera (entablados)?
 5. ¿Cemento?
 6. ¿Tierra? 
 7. ¿Otro material?
*/

gen DIRT=0 if     p204==1 & (piso>=1 & piso<=7)
replace DIRT=1 if p204==1 & (piso==6)

** GOAL 8. DEVELOP A GLOBAL PARTNERSHIP FOR DEVELOPMENT

** Target 16, Indicator: Unemployment Rate of 15 year-olds (%)

gen UNMPLYMENT15=0 if     p204==1 & (edad>=15 & edad<=24) & (tasadeso==0 | tasadeso==1)
replace UNMPLYMENT15=1 if p204==1 & (edad>=15 & edad<=24) & (tasadeso==1)

** Target 18, Indicator: "Telephone lines and cellular subscribers per 100 population"
/*
114. SU HOGAR TIENE :
(Acepte una o más alternativas)
1. ¿Teléfono (fijo)? 		p1141
2. ¿Celular?			p1142
3. ¿Beeper? 			p1143
4. ¿Internet?			p1144
5. NO TIENE			p1145
*/

gen TELEPHONE=0 if     p204==1
replace TELEPHONE=1 if p204==1 & (p1141==1)

/*
** Fixed Lines

noisily display "Telephone Lines"
gen TEL=1 if p204==1
global variable TEL
do "$DefvarsSTOTD"
replace TEL=0 if p204==1
replace TEL=1 if p204==1 & (p1141==1)
do "$DefvarsSTOTN"
global indicador 99 " "
do "$DefvarsS"
noisily do "$TablasS"

** Cel Lines

noisily display "Telephone Lines"
gen CEL=1 if p204==1
global variable CEL
do "$DefvarsSTOTD"
replace CEL=0 if p204==1
replace CEL=1 if p204==1 & (p1142==1)
do "$DefvarsSTOTN"
global indicador 100 " "
do "$DefvarsS"
noisily do "$TablasS"
*/
** Target 18, Indicator: "Personal computers in use per 100 population"
/*
612. EQUIPAMIENTO DEL HOGAR
Su hogar tiene:
20.COMPUTADORA? ...........
*/

gen COMPUTER=.

** Target 18, Indicator: "Internet users per 100 population"
/*
114. SU HOGAR TIENE :
(Acepte una o más alternativas)
1. ¿Teléfono (fijo)? 		p1141
2. ¿Celular?			p1142
3. ¿Beeper? 			p1143
4. ¿Internet?			p1144
5. NO TIENE			p1145

p314 
En el mes anterior, hizo uso del servicio de Interner por cabina pública?
 1 Si
 2 No
 9 Missing value
*/
gen INTUSERS=.
************************************************************************
**** ADDITIONAL SOCIO - ECONOMIC COMMON COUNTRY ASESSMENT INDICATORS ****
************************************************************************

** CCA 19. Proportion of children under 15 who are working
* INCLUDES POPULATION 12 TO 15 YEARS-OLD

* Not available

** CCA 41 Number of Persons per Room*

gen PERSROOM2=persroom if parentco==1  

*** Disconnected Youths
 /*
p549
549. ¿POR QUE NO BUSCO TRABAJO?
1. No hay trabajo 
2. Se cansó de buscar  
3. Por su edad 
4. Falta de experiencia 
5. Sus estudios no le permiten  
6. Los quehaceres del hogar
no le permiten  
7. Razones de Salud  
8. Falta de capital  
9. Otro  
10. Ya encontró trabajo 
11. Si buscó trabajo 
*/

gen DISCONN=0 if     p204==1 & (edad>=15 & edad<=24) 
replace DISCONN=1 if p204==1 & (edad>=15 & edad<=24) & (peaa==. | peaa==3) & p307!=1 & ((p549>=1 & p549<=4) | p549==9)


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
para análisis de trends (en el marco de estudios sobre el futuro del trabajo)*/
clonevar codocupa = p505 
clonevar codindustria = p506


compress


saveold "`base_out'", version(12) replace


log close

