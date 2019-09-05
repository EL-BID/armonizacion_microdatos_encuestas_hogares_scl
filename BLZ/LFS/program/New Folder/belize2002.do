clear

cd "X:\ARM\BLZ\2002"


capture log close
log using "X:\ARM\BLZ\2002\Arm_data\BLZ2002EA_BID.log", replace


clear
set more off
set memory 400m

************************************************************************************************************
*****                                       BELIZE 2002                                                *****
*****                                 LABOR FORCE SURVEY 2002                                          *****
*****                                        19,363 Personas                                           *****
*****                                         4,285 Hogares                                            *****
************************************************************************************************************

use "X:\ARM\BLZ\2002\Orig_data\belize02.dta", clear

destring _all, replace
compress

ren district q01 
ren urbrur q02
ren ednumber q03
* ren sample q04 not available for 2002 *
ren hhnumber q05
ren week q06
ren result q07
*sort qid
sort  q01 q02 q03 q05 q06 q07 /* q04 */

***************************
* Identificador del Hogar *
***************************

egen idh_ch=group(q01 q02 q03 q05 q06 q07) /* qo4 */
label var idh_ch "Identificador Unico del Hogar"

****************************
* Identificador Individual *
****************************

gen idp_ci= personnu /*  personnu for 2002 */
label var idp_ci "Identificador Individual dentro del Hogar"

sort idh_ch idp_ci


*************
* factor_ch *
*************

************************* WEIGHTS **************************************************************************
* Corozal (1)	Orange Walk (2)		Belize (3)	Cayo (4)	Stann Creek (5)		Toledo (6) *
* 12.56		13.47			14.42		14.06		13.48			12.85	   *
************************************************************************************************************

gen factor_ch=.

replace factor_ch=12.56 if q01==1
replace factor_ch=13.47 if q01==2
replace factor_ch=14.42 if q01==3
replace factor_ch=14.06 if q01==4
replace factor_ch=13.48 if q01==5
replace factor_ch=12.85 if q01==6

label var factor_ch "Factor de Expansion del Hogar"

*************
* factor_ci *
*************

gen factor_ci=factor_ch
label var factor_ci "Factor de Expansion del Individuo"

**************
* factor2_ci *
**************

gen factor2_ci=factor_ch*100
label var factor2_ci "Factor de Expansion del Individuo (multiplicado por 100 para tabular)" /* Se debe dividir por 100 la poblacion para tener el numero correcto */

********
* ZONA *
********

gen byte zona_c=1 if q02==1 | q02==2 | q02==3 | q02==4 /* Urbana */
replace zona_c=0 if q02==5 /* Rural */
label variable zona_c "ZONA GEOGRAFICA"
label define zona_c 0 "Rural" 1 "Urbana"
label value zona_c zona_c

**** q01==3 es la ciudad BELIZE *****

******************
* COUNTRY - YEAR *
******************

gen str3 pais_c="BLZ"
label variable pais_c "Nombre del Pais"

**********
* anio_c *
**********

gen anio_c=2002
label variable anio_c "Año de la Encuesta"

**************************
* Periodo de Referencia: * 
**************************

gen byte mes_c=4
label variable mes_c "Mes de la Encuesta: Abril"

********
* SEXO *
********

gen sexo_ci=sex
label var sexo_ci "Sexo del Individuo"
label define sexo_ci 1 "Hombre" 2 "Mujer"  
label value sexo_ci sexo_ci


**************
* PARENTESCO *
**************


gen relacion_ci=.
replace relacion_ci=1 if relate2==1
replace relacion_ci=2 if relate2==2
replace relacion_ci=3 if relate2==3 | relate1==1
replace relacion_ci=4 if relate2==4 | relate2==5 | relate2==6 | relate2==7 | relate1==2 | relate1==3
replace relacion_ci=5 if relate2==8 | relate1==4
replace relacion_ci=. if relate2==9 | relate1==9 /* No sabe */
label var relacion_ci "Parentesco o relacion con el Jefe del Hogar"
label define relacion_ci 1 "Jefe(a)" 2 "Esposo(a) o compañero(a)" 3 "Hijo(a)" 4 "Otro pariente" 5 "Otro NO pariente" 6 "Empleado(a) domestico(a)" 
label value relacion_ci relacion_ci

********
* EDAD *
********

gen edad_ci=age2
replace edad_ci=age1 if age2==.
label var edad_ci "Edad del Individuo"

****************
* nconyuges_ch *
****************

egen nconyuges_ch=sum(relacion_ci==2), by (idh_ch)
label variable nconyuges_ch "Numero de Conyuges"

*************
* nhijos_ch *
*************

egen nhijos_ch=sum(relacion_ci==3 & edad_ci<18), by (idh_ch)
label variable nhijos_ch "Numero de Hijos"

****************
* notropari_ch *
****************

egen notropari_ch=sum((relacion_ci==3 & edad_ci>=18) | (relacion_ci>3 & relacion_ci<5)), by (idh_ch)
label variable notropari_ch "Numero de Otros Parientes "

******************
* notronopari_ch *
******************

egen notronopari_ch=sum(relacion_ci==5), by (idh_ch)
label variable notronopari_ch "Numero de Otros NO Parientes "

**************
* nempdom_ch *
**************

egen nempdom_ch=sum(relacion_ci==6), by (idh_ch)
label variable nempdom_ch "Numero de Empleados Domesticos"

* HOUSEHOLD TYPE (unipersonal, nuclear, ampliado, compuesto, corresidentes)    
* note: These are all defined in terms of relationship to household head

***************
* clasehog_ch *
***************

gen clasehog_ch=.
replace clasehog_ch=1 if nhijos_ch==0 & nconyuges_ch==0 & notropari_ch==0 & notronopari_ch==0 /* unipersonal*/
replace clasehog_ch=2 if nhijos_ch>0 & notropari_ch==0 & notronopari_ch==0 /* nuclear (child with or without spouse but without other relatives)*/
replace clasehog_ch=2 if nconyuges_ch>0 & notropari_ch==0 & notronopari_ch==0 /* nuclear (spouse with or without children but without other relatives)*/
replace clasehog_ch=3 if notropari_ch>0 & notronopari_ch==0 /* ampliado*/
replace clasehog_ch=4 if (nconyuges_ch>0 | nhijos_ch>0 | (notropari_ch>0 & notropari_ch<.)) & (notronopari_ch>0 & notronopari_ch<.) /* compuesto  (some relatives plus non relative)*/
replace clasehog_ch=5 if nhijos_ch==0 & nconyuges_ch==0 & notropari_ch==0 & notronopari_ch>0 & notronopari_ch<./** corresidente*/

label variable clasehog_ch "CLASE HOGAR"
label define clasehog_ch 1 "Unipersonal" 2 "Nuclear" 3 "Ampliado" 4 "Compuesto" 5 "Corresidente"
label value clasehog_ch clasehog_ch


* Checking 'clasehog' (que todas las personas pertenezcan a un hogar con 'clasehog' definido)
assert clasehog==int(clasehog)
assert clasehog>=1 & clasehog<=5
sum clasehog
scalar claseT=r(N)
assert claseT==19363

* HOUSEHOLD COMPOSITION VARIABLES 
/* note: These are unrelated to who is the head
   note: That childh denotes the number of children of the head, while numkids counts the number of all kids in the household */

sort idh_ch

********************************************************************************************
* NUMBER OF PERSONS IN THE HOUSEHOLD (not including domestic employees or other relatives) *
********************************************************************************************

****************
* nmiembros_ch *
****************

egen nmiembros_ch=sum(relacion_ci>0 & relacion_ci<5), by (idh_ch)
label variable nmiembros_ch "Numero de miembros de 7 años o mas de edad en el Hogar"

***************
* nmayor21_ch *
***************

egen nmayor21_ch=sum((relacion_ci>0 & relacion_ci<5) & (edad_ci>=21)), by (idh_ch)
label variable nmayor21_ch "Numero de personas de 21 años o mas dentro del Hogar"

***************
* nmenor21_ch *
***************

egen nmenor21_ch=sum((relacion_ci>0 & relacion_ci<5) & (edad_ci<21)), by (idh_ch)
label variable nmenor21_ch "Numero de personas menores a 21 años dentro del Hogar"

***************
* nmayor65_ch *
***************

egen nmayor65_ch=sum((relacion_ci>0 & relacion_ci<5) & (edad_ci>=65)), by (idh_ch)
label variable nmayor65_ch "Numero de personas de 65 años o mas dentro del Hogar"

**************
* nmenor6_ch *
**************

egen nmenor6_ch=sum((relacion_ci>0 & relacion_ci<5) & (edad_ci<6)), by (idh_ch)
label variable nmenor6_ch "Numero de niños menores a 6 años dentro del Hogar"

**************
* nmenor1_ch *
**************

egen nmenor1_ch=sum((relacion_ci>0 & relacion_ci<5) & (edad_ci<1)),  by (idh_ch)
label variable nmenor1_ch "Numero de niños menores a 1 año dentro del Hogar"

***********************************************************
*** ESTADO CIVIL PARA PERSONAS DE 10 AÑOS O MAS DE EDAD ***
***********************************************************

gen civil_ci=.
label var civil_ci "Estado Civil"
label define civil_ci 1 "Soltero" 2 "Union Formal o Informal" 3 "Divorciado o Separado" 4 "Viudo"
label value civil_ci civil_ci

*** REPORTED HEAD OF HOUSEHOLD

***********
* jefe_ci *
***********

gen jefe_ci=0
replace jefe_ci=1 if relacion_ci==1
label var jefe_ci "Jefe de Hogar Declarado"

*** We want to know if there is only one head in each hh and if there is a hh with no head:
*egen hh=sum(jefe), by (idh_ch) *
* assert hh==1 * revisar *

*****************************************
***   VARIABLES DEL MERCADO LABORAL   ***
*****************************************

**********
* emp_ci *
**********

gen byte emp_ci=.
replace emp_ci=1 if econact==1 | othecon==1 | tempabs==1
replace emp_ci=0 if econact==2 & othecon==2 & tempabs==2
label var emp_ci "Empleado en la semana de referencia"

**************
* desemp1_ci *
**************

* Individuos que no han trabajado la semana pasada y declaran haber estado buscando trabajo
gen byte desemp1_ci=.
label var desemp1_ci "Personas que no tienen trabajo y han buscado trabajo la semana pasada"

**************
* desemp2_ci * 
**************

gen byte desemp2_ci=.
replace desemp2_ci=1 if (looked==2 & reason>=1 & reason<=3)
replace desemp2_ci=0 if (looked==2 & reason>=4 & reason<=17) | emp_ci==1
label var desemp2_ci "desemp1 + personas que no tienen trabajo ni lo buscaron, pero esperan respuesta de una solicitud de empleo, entrevista"

**************
* desemp3_ci *
**************

gen byte desemp3_ci=.
replace desemp3_ci=1 if looked==1 | (looked==2 & reason>=1 & reason<=4)
replace desemp3_ci=0 if (looked==2 & reason>=5 & reason<=17) | emp_ci==1
label var desemp3_ci "desemp2 + personas que no tienen trabajo pero han buscado trabajo durante las 4 semanas anteriores a la semana pasada"

****************************************
* PEA: Poblacion Economicamente Activa *
****************************************

***********
* pea1_ci *
***********

gen byte pea1_ci=.
label var pea1_ci "Poblacion Economicamente Activa utilizando la definicion 'desemp1'"

***********
* pea2_ci *
***********

gen byte pea2_ci=1 if (emp_ci==1 | desemp2_ci==1)
replace pea2_ci=0 if pea2_ci~=1
label var pea2_ci "Poblacion Economicamente Activa utilizando la definicion 'desemp2'"

***********
* pea3_ci *
***********

gen byte pea3_ci=1 if (emp_ci==1 | desemp3_ci==1)
replace pea3_ci=0 if pea3_ci~=1
label var pea3_ci "Poblacion Economicamente Activa utilizando la definicion 'desemp3'"

*****************************
* Trabajadores Desalentados *
*****************************

***************
* desalent_ci *
***************

gen byte desalent_ci=.
replace desalent_ci=0 if pea3_ci==1 | reason<99
replace desalent_ci=1 if reason==5 | reason==6 | reason==8
label var desalent_ci "Trabajadores desalentados, personas que creen que por alguna razon no conseguiran trabajo" 

*** Horas trabajadas en la Actividad Principal

***************
* horaspri_ci *
***************

gen horaspri_ci=uslmain if uslmain<99 /* uslmain for 2002 */
label var horaspri_ci "Horas totales trabajadas la semana pasada en la Actividad Principal"

egen horastot_ci=rsum(uslmain  uslother) if uslmain<99 &  uslother<99
replace horastot_ci=. if uslmain==. &  uslother==.
label var horastot_ci "Horas totales trabajadas la semana pasada en todas las Actividades"

*****************************
* Trabajadores Subempleados *
*****************************

gen subemp_ci=.
replace subemp_ci=1 if horastot_ci<=30 & addwork==1
replace subemp_ci=0 if (horastot_ci>30 & horastot_ci<.) | (horastot_ci<=30 & addwork==2)
label var subemp_ci "Trabajadores subempleados"

*******************************
* Trabajadores a Medio Tiempo *
*******************************

gen byte tiempoparc_ci=.
replace tiempoparc_ci=1 if horastot_ci<=30 & addwork==2
replace tiempoparc_ci=0 if (horastot_ci>30 & horastot_ci<.) | (horastot_ci<=30 & addwork==1)
label var tiempoparc_ci "Trabajadores a medio tiempo"

*************
* Contratos *
*************

gen contrato_ci=.
label var contrato_ci "Personas empleadas que han firmado un contrato de trabajo"

*********************************
* Beneficios (Seguridad Social) *
*********************************

gen segsoc_ci=.
label variable segsoc_ci "Personas que cuentan con seguro social"

*************************
* Numero de ocupaciones *
*************************

gen nempleos_ci=.
replace nempleos_ci=1 if multjobj==2
replace nempleos_ci=2 if multjobj==1
label var nempleos_ci "Numero de empleos"
label define nempleos_ci 1 "un trabajo" 2 "dos o mas trabajos"
label values nempleos_ci nempleos_ci

**********************
* Tamaño de la firma *
**********************

/* valores positivos solo para los patrones en categopri==1 */
gen byte tamfirma_ci=.
replace tamfirma_ci=1 if yearly==2 | yearly==3 | yearly==4
replace tamfirma_ci=0 if yearly==1
label var tamfirma_ci "Trabajadores formales"
label define tamfirma_ci 1 "Mas de 4 trabajadores" 0 "4 o menos trabajadores"
label values tamfirma_ci tamfirma_ci

******************
* Sector Publico *
******************

gen spublico_ci=.
replace spublico_ci=1 if empmain==3 | empmain==4
replace spublico_ci=0 if empmain==1 | empmain==2 | empmain==5 | empmain==6
label var spublico_ci "Personas que trabajan en el sector publico"

************************************************************************************************************
*** VARIABLES DE DEMANDA LABORAL
************************************************************************************************************

*********************************
* OCUPACION (variable p40iscom) *
*********************************

capture drop ocupa_ci 
gen ocupa_ci=.
replace ocupa_ci=1 if occmain>=2000 & occmain<=3999 /* occmain for 2002 */
replace ocupa_ci=2 if occmain>=1000 & occmain<=1999
replace ocupa_ci=3 if occmain>=4000 & occmain<=4999
replace ocupa_ci=4 if occmain>=5200 & occmain<=5999
replace ocupa_ci=5 if occmain>=5000 & occmain<=5199
replace ocupa_ci=6 if occmain>=6000 & occmain<=6999
replace ocupa_ci=7 if occmain>=7000 & occmain<=8999
replace ocupa_ci=8 if occmain>=0 & occmain<=999 
replace ocupa_ci=9 if (occmain>=9000 & occmain<=9996) | occmain==9998

label var ocupa_ci "Ocupacion Laboral en la Actividad Principal"
label define ocupa_ci 1 "PROFESIONALES Y TECNICOS" 2 "GERENTES, DIRECTORES Y FUNCIONARIOS SUPERIORES"  3 "PERSONAL ADMINISTRATIVO Y NIVEL INTERMEDIO" 4 "COMERCIANTES Y VENDEDORES" 5 "TRABAJADORES EN SERVICIOS" 6 "TRABAJADORES AGRICOLAS Y AFINES" 7 "OBREROS NO AGRICOLAS, CONDUCTORES DE MAQUINAS Y VEHICULOS DE TRANSPORTE Y SIMILARES" 8 "FUERZAS ARMADAS" 9 "OTRAS OCUPACIONES NO CLASIFICADAS ANTERIORMENTE"
label values ocupa_ci ocupa_ci

****************************
* RAMA (variable p41isicm) *
****************************
*Yanira Oviedo, julio 2010: en este año la codificación es diferente y no se cuenta con manual para 
*trabajar la información

gen byte rama_ci=.
*replace rama_ci=1 if indmdct>=0 & indmdct<1000
*replace rama_ci=2 if indmdct>=1000 & indmdct<2000
*replace rama_ci=3 if indmdct>=2000 & indmdct<4000
*replace rama_ci=4 if indmdct>=4000 & indmdct<5000
*replace rama_ci=5 if indmdct>=5000 & indmdct<6000
*replace rama_ci=6 if indmdct>=6000 & indmdct<7000
*replace rama_ci=7 if indmdct>=7000 & indmdct<8000
*replace rama_ci=8 if indmdct>=8000 & indmdct<9000
*replace rama_ci=9 if indmdct>=9000 & indmdct<10000

label var rama_ci "Rama Laboral en la Ocupacion Principal"
label define rama_ci 1 "Agricultura, caza, silvicultura y pesca" 2 "Explotación de minas y canteras" 3 "Industrias manufactureras" 4 "Electricidad, gas y agua" 5 "Construcción" 6 "Comercio al por mayor y menor, restaurantes, hoteles" 7 "Transporte y almacenamiento" 8 "Establecimientos financieros, seguros, bienes inmuebles" 9 "Servicios sociales, comunales y personales"
label values rama_ci rama_ci

******************************
*** DURACION DEL DESEMPLEO ***
******************************

gen durades_ci=.
label var durades_ci "Duracion del Desempleo (en meses)"

gen durades1_ci=wowork if wowork<9
label var durades1_ci "Duracion del Desempleo (categorias)"
label define durades1_ci 1 "Menos de 1 mes" 2 "1 a 3 meses" 3 "4 a 6 meses" 4 "7 a 12 meses" 5 "mas de 12 meses"
label values durades1_ci durades1_ci

****************************************
*** Antiguedad, AÑOS (NOT AVAILABLE) ***
****************************************

gen antiguedad_ci=yrsmain if yrsmain<99
label var antiguedad_ci "Antiguedad en la Ocupacion Actual (en años)"

********************************************************************************
* VARIABLES EDUCATIVAS (para todos los miembros del hogar)
********************************************************************************
/*

Tertiary Education

- Belize Teacher Training College BTTC
- Belize College of Agriculture BCA
- BNS
- Sixth Form

*/


***********
* aedu_ci *
***********

gen byte aedu_ci=.
replace aedu_ci= yrcomple
replace aedu_ci= yrscompl if yrcomple==.
replace aedu_ci= (formin - 1) if (yrcomple==. & yrscompl==.)
replace aedu_ci= (classin - 1) if (yrcomple==. & yrscompl==. & formin==. & classin>0)
replace aedu_ci=. if aedu==99
label variable aedu_ci "Años de Educacion"

***************************************
** Categorias educativas excluyentes **
***************************************

gen eduno_ci=.
replace eduno_ci=1 if aedu_ci==0 
replace eduno_ci=0 if aedu_ci!=0
replace eduno_ci=. if aedu_ci==.
label var eduno_ci "1 = personas sin educacion (excluye preescolar)"


/* 8 years - Primary School, http://ambergriscaye.com/pages/edu/education.html */

gen edupre_ci=.
label var edupre_ci "Educacion preescolar"

gen edupi_ci=.
replace edupi_ci=1 if (aedu_ci>0 & aedu_ci<8)
replace edupi_ci=0 if (aedu_ci==0 | aedu_ci>=8)
replace edupi_ci=. if aedu_ci==. 
label var edupi_ci "1 = personas que no han completado el nivel primario"

gen edupc_ci=.
replace edupc_ci=1 if (aedu_ci==8) 
replace edupc_ci=0 if (aedu_ci!=8)
replace edupc_ci=. if aedu_ci==. 
label var edupc_ci "1 = personas que han completado el nivel primario"

/* 4 years - Secondary School */

gen edusi_ci=.
/* replace edusi_ci=1 if ((classin==9) | (yrcomple>8 & yrcomple<=14) | (formin>8 & formin<=14)) & (tertiary>4)
replace edusi_ci=0 if ()
label var edusi_ci "1 = personas que no han completado el nivel secundario"
*/
gen edusc_ci=.
replace edusc_ci=1 if (aedu_ci==12) 
replace edusc_ci=0 if (aedu_ci!=12) 
replace edusc_ci=. if aedu_ci==. 
label var edusc_ci "1 = personas que han completado el nivel secundario"

gen eduui_ci=.
label var eduui_ci "1 = personas que no han completado el nivel universitario o superior"

gen eduuc_ci=.
replace eduuc_ci=1 if tertiary==4
replace eduuc_ci=0 if tertiary!=4
replace eduuc_ci=. if aedu_ci==. 
label var eduuc_ci "1 = personas que han completado el nivel universitario o superior"

gen edus1i_ci=.
*replace edus1i_ci=1 if (aedu_ci>8 & aedu_ci<12) 
*replace edus1i_ci=0 if (aedu_ci<=8 | aedu_ci>=12) 
*replace edus1i_ci=. if aedu_ci==. 
label var edus1i_ci "1 = personas que no han completado el primer ciclo de la educacion secundaria"

gen edus1c_ci=.
*replace edus1c_ci=1 if (aedu_ci==12) 
*replace edus1c_ci=0 if (aedu_ci!=12) 
*replace edus1c_ci=. if aedu_ci==. 
label var edus1c_ci "1 = personas que han completado el primer ciclo de la educacion secundaria"

gen edus2i_ci=.
label var edus2i_ci "1 = personas que no han completado el segundo ciclo de la educacion secundaria"

gen edus2c_ci=.
label var edus2c_ci "1 = personas que han completado el segundo ciclo de la educacion secundaria"

gen eduac_ci=.
label var eduac_ci "Educacion terciaria académica versus educación terciaria no-académica "

gen repite_ci=.
label var repite_ci "=1 si repite o repitio algun grado o año"

gen repiteult_ci=.
label var repiteult_ci "=1 si repite el grado o año que cursa actualmente"

**********
* ASISTE *
**********

gen asiste_ci=.
replace asiste_ci=1 if (attdsch==0) | (attdsch2<2) 
replace asiste_ci=0 if (attdsch==2) | (attdsch2==2)
label var asiste_ci "Personas que actualmente asisten a centros de enseñanza"

*********************
* POR QUE NO ASISTE *
*********************

/* For individuals under 14 years */

gen pqnoasis_ci=.
replace pqnoasis_ci=noattend
label var pqnoasis_ci "Razon por la cual N no esta asistiendo a la escuela "
label define pqnoasis_ci 1 "Demasiado joven" 2 "Problemas Financieros" 3 "Trabajo" 4 "Trabajo en el hogar" 5 "Distancia, transporte" 6 "Enfermedad, discapacidad" 7 "Falta de espacio en la escuela" 8 "Otro"
label values pqnoasis_ci pqnoasis_ci

gen edupub_ci=.
label var edupub_ci "1 = personas que asisten a centros de enseñanza publicos"


**************************************
*** VARIABLES DE INGRESO (MENSUAL) ***
**************************************

* Create a dummy indicating this person's income should NOT be included 
gen miembros_ci=1
replace miembros_ci=0 if  (relacion_ci==0 | relacion_ci==6 | relacion_ci==.)
replace miembros_ci=0 if factor_ci==.
label variable miembros_ci "Variable dummy que indica las personas que son miembros del Hogar"

sort idh_ch

******************************************
*** INGRESOS LABORALES  	       ***
******************************************

***************************
*** OCUPACION PRINCIPAL ***
***************************

****** INGRESOS MONETARIOS ******

****** INGRESO MONETARIO LABORAL ACTIVIDAD PRINCIPAL ******
gen ylmpri_ci=.
label var ylmpri_ci "Ingreso Laboral Monetario de la Actividad Principal"

****** INGRESO LABORAL NO MONETARIO ACTIVIDAD PRINCIPAL ******
gen ylnmpri_ci=.
label var ylnmpri_ci " Ingreso NO monetario Laboral ocupacion principal"

/* CATEGORIA OCUPACIONAL ACTIVIDAD PRINCIPAL
INDEPENDIENTES
ASALARIADOS
TRAB SIN PAGO */

gen categopri_ci=.
replace categopri_ci=1 if empmain==1
replace categopri_ci=2 if empmain==2
replace categopri_ci=3 if empmain==3 | empmain==4 | empmain==5
replace categopri_ci=4 if empmain==6
label var categopri_ci "CATEGORIA OCUPACIONAL ACTIVIDAD PRINCIPAL"
label define categopri_ci 1 "Patron" 2 "Cuenta Propia" 3 "Asalariado" 4 "Trabajador No Remunerado" 
label values categopri_ci categopri_ci

****************************
*** OCUPACION SECUNDARIA ***
****************************

/* CATEGORIA OCUPACIONAL ACTIVIDAD SECUNDARIA
INDEPENDIENTES
ASALARIADOS
TRAB SIN PAGO */


gen categosec_ci=.
replace categosec_ci=1 if empother==1 /* empother for 2002 */
replace categosec_ci=2 if empother==2
replace categosec_ci=3 if empother==3 | empother==4 | empother==5
replace categosec_ci=4 if empother==6
label var categosec_ci "CATEGORIA OCUPACIONAL ACTIVIDAD SECUNDARIA"
label define categosec_ci 1 "Patron" 2 "Cuenta Propia" 3 "Asalariado" 4 "Trabajador No Remunerado" 
label values categosec_ci categosec_ci

*********************************
****** INGRESOS MONETARIOS ******
*********************************

****** INGRESO MONETARIO LABORAL ACTIVIDAD SECUNDARIA ******
gen ylmsec_ci=.

****** INGRESO NO MONETARIO LABORAL ACTIVIDAD SECUNDARIA ******
gen ylnmsec_ci=.

********************************************************************
*** INGRESO MONETARIO TOTAL ( OCUP PRINCIPAL + OCUP SECUNDARIA ) ***
********************************************************************
gen ylm_ci=.
/* 
* gen ylm_ci p50ginc*(365/12) if p51==1 /* diario */
* replace ylm_ci=p50ginc*(30/7) if p51==2 /* semanal */
* replace ylm_ci=p50ginc*(15/7) if p51==3 /* cada dos semanas */
* replace ylm_ci=p50ginc*2 if p51==4 /* dos veces al mes */
* replace ylm_ci=p50ginc if p51==5 /* mensual */
* replace ylm_ci=p50ginc/(365/30) if p51==6 /* anual */
* replace ylm_ci=p50ginc if p51==7 & categopri_ci==4 
*/
label var ylm_ci "Ingreso Laboral Monetario Total (Bruto)"


***********************************************************************
*** INGRESO NO MONETARIO TOTAL ( OCUP PRINCIPAL + OCUP SECUNDARIA ) ***
***********************************************************************
gen ylnm_ci=.
label var ylnm_ci "Ingreso Laboral NO Monetario Total"

**********************************
*** OTRAS FUENTES DE INGRESOS  ***
**********************************

gen ynlm_ci=.
label var ynlm_ci "Ingreso NO Laboral Monetario"

gen autocons_ci=.
label var autocons_ci "Autoconsumo Individual"

gen remesas_ci=.
label var remesas_ci "Remesas Individuales"

gen ynlnm_ci=.
label var ynlnm_ci "Ingreso NO Laboral NO Monetario"

*** FLAGS
*** Dummy Individual si no reporta el ingreso laboral monetario de la actividad principal
gen byte nrylmpri_ci=.
/*
gen byte nrylmpri_ci=0
replace nrylmpri_ci=1 if p51==9 | (p51==7 & p50ginc>0)
*/
label var nrylmpri_ci "Identificador de No Respuesta del Ingreso Monetario de la Actividad Principal"


*** Dummy para el Hogar
/* 
capture drop nrylmpri_ch
sort idh
egen nrylmpri_ch=sum(nrylmpri_ci) if miembro==1, by(idh_ch)
replace nrylmpri_ch=1 if nrylmpri_ch>1 & nrylmpri_ch~=. & miembro==1 
label var nrylmpri_ch "Identificador de Hogares en donde alguno de los miembros No Responde el Ingreso Monetario de la Actividad Principal"
*/

egen ylm_ch=sum(ylm_ci) if miembros_ci==1, by(idh_ch)
label var ylm_ch "Ingreso Laboral Monetario del Hogar (Bruto)"

egen ylmnr_ch=sum(ylm_ci) if miembros_ci==1, by(idh_ch) /* & nrylmpri_ch==0 There are no INCOME VARIABLES for 2002 */
label var ylmnr_ch "Ingreso Laboral Monetario del Hogar, considera 'missing' la No Respuesta"

egen ylnm_ch=sum(ylnm_ci) if miembros_ci==1, by(idh_ch)
label var ylnm_ch "Ingreso Laboral No Monetario del Hogar"

egen ynlm_ch=sum(ynlm_ci) if miembros_ci==1, by(idh_ch)
label var ynlm_ch "Ingreso No Laboral Monetario del Hogar"

egen ynlnm_ch=sum(ynlnm_ci) if miembros_ci==1, by(idh_ch)
label var ynlnm_ch "Ingreso No Laboral No Monetario del Hogar"

egen autocons_ch=sum(autocons_ci) if miembros_ci==1, by(idh_ch)
label var autocons_ch "Autoconsumo del Hogar"

egen remesas_ch=sum(remesas_ci) if miembros_ci==1, by(idh_ch)
label var remesas_ch "Remesas del Hogar"

*********************************************
*** INGRESO HORARIO DE TODOS LOS TRABAJOS ***
*********************************************

/* This is not accurate in the sense that if you have more than one job
you will have wage averaged over several jobs */
gen ylmho_ci=ylm_ci/(horastot_ci*4.3) 
label var ylmho_ci "Salario Horario Monetario de todas las Actividades"

*************************************************
*** INGRESO HORARIO DE LA OCUPACION PRINCIPAL ***
*************************************************

gen ylmhopri_ci=ylmpri_ci/(horaspri_ci*4.3) 
label var ylmhopri_ci "Salario Horario Monetario de la Actividad Principal"

*************************
*** HOUSING VARIABLES ***
*************************

gen aguared_ch=.

gen aguadist_ch=.

gen aguamala_ch=.
gen aguamide_ch=.

gen luz_ch=.
/* Not available for 2002
gen luz_ch=1 if h4==1 | h4==2 | h4==3
replace luz_ch=0 if h4>3 & h4<9
*/

gen luzmide_ch=.

gen combust_ch=.

gen bano_ch=.

gen banoex_ch=.

gen des1_ch=.

gen des2_ch=.

gen piso_ch=.
gen pared_ch=.
gen techo_ch=.

gen resid_ch=.

/* p3_1 not available for 2006

gen resid_ch=0 if p3_1==1
replace resid_ch=1 if p3_1==4 | p3_1==5
replace resid_ch=2 if p3_1==2 | p3_1==3
replace resid_ch=3 if p3_1==6 | p3_1==7
*/

gen dorm_ch=.

gen cuartos_ch=.

gen cocina_ch=.

gen telef_ch=.

gen refrig_ch=.

gen freez_ch=.

gen auto_ch=.

gen compu_ch=.

gen internet_ch=.

gen cel_ch=.

gen vivi1_ch=.

gen vivi2_ch=.

gen viviprop_ch=.

gen vivitit_ch=.

gen vivialq_ch=.

gen vivialqimp_ch=.

save "X:\ARM\BLZ\2002\Arm_data\BLZ2002EA_BID.dta", replace
log close

/*
** CIUDAD PRINCIPAL **
gen citym_ci=0
replace citym_ci=1 if q01==3

gen sector_ci=1 if rama_ci==6
replace sector_ci=2 if rama_ci==9
replace sector_ci=3 if rama_ci==3
replace sector_ci=4 if rama_ci==1
replace sector_ci=5 if rama_ci==2 | rama_ci==4 | rama_ci==5 | rama_ci==7 | rama_ci==8


* 1
gen employees=0
replace employees=1 if (categopri==3 | categopri==4)
* 2
gen employeesm=0 if employees==1
replace employeesm=1 if tamfirma==0 & employeesm==0
* 3
gen employeesnm=0 if employees==1
replace employeesnm=1 if tamfirma==1 & employeesnm==0
* 4
gen firmow=0
replace firmow=1 if (categopri==1 | categopri==2)
* 5
gen selfe=0 if firmow==1
replace selfe=1 if categopri==2 & selfe==0
* 6
gen selfenp=0 if firmow==1
replace selfenp=1 if categopri==2 & ocupa_ci>1 & selfenp==0
* 7
gen patron=0 if firmow==1
replace patron=1 if categopri==1 & patron==0
* 8
gen patronm=0 if patron==1
replace patronm=1 if categopri==1 & tamfirma==0 & patronm==0
* 9
gen patronnm=0 if patron==1
replace patronnm=1 if categopri==1 & tamfirma==1 & patronnm==0
* 10
gen patronnp=0 if firmow==1
replace patronnp=1 if categopri==1 & ocupa_ci>1 & patronnp==0
* 11
gen patronnpm=0 if patronnp==1
replace patronnpm=1 if categopri==1 & ocupa_ci>1 & tamfirma==0 & patronnpm==0
* 12
gen patronnpnm=0 if patronnp==1
replace patronnpnm=1 if categopri==1 & ocupa_ci>1 & tamfirma==1 & patronnpnm==0

***
sort idh_ch
foreach var of varlist employees firmow selfe selfenp patron patronm patronnm patronnp patronnpm patronnpnm  {
egen `var'_ch=sum(`var') if miembro==1, by(idh_ch) 
replace `var'_ch=1 if `var'_ch>1 & `var'_ch~=. & miembro==1 
}


** INGRESO TOTAL DEL HOGAR **
gen yt_ch=ylm_ch if miembros_ci==1
label var yt_ch "Ingreso Total del Hogar (Laboral)"


foreach var of varlist ylm_ch yt_ch {
gen `var'_pc=`var'/nmiembros_ch
}

by idh_ch, sort: gen hogar=1 if _n==1

* Lineas de Pobreza Mensuales Per Capita

gen lineap_idb=76.3831

gen pobrey_idb=.
replace pobrey_idb=1 if yt_ch_pc<=lineap_idb
replace pobrey_idb=0 if yt_ch_pc>lineap_idb & yt_ch_pc<.
label var pobrey_idb "1= Personas pobres segun el Ingreso Total Per Capita Familiar (LP=IDB)"


gen inbus_ci=0
replace inbus_ci=1 if (categopri_ci==1 | categopri_ci==2) & ylm_ci>0 & ylm_ci<.

sort idh_ch
egen inbus_ch=sum(inbus_ci) if miembro==1, by(idh_ch) 
replace inbus_ch=1 if inbus_ch>1 & inbus_ch~=. & miembro==1 

gen ybusiness_ci=ylm_ci if inbus_ci==1
label var ybusiness_ci "Ingreso Total Individual Trabajo Independiente"

egen ybusiness_ch=sum(ybusiness_ci) if inbus_ch==1, by(idh_ch)
label var ybusiness_ch "Ingreso por Trabajo Independiente Monetario del Hogar"

gen ybus_ratio=ybusiness_ch/yt_ch

compress
save "`out'1999\BLZ1999EA_BID_sis.dta", replace

capture log close

log using "D:\Data.idb\CECILIACA\BANANAS\BELIZE\Programs\1999\sisrequest_blz99.log", replace


save "X:\ARM\BLZ\2001\Orig_data\belize01_02.dta", replace

/*


**** TABULADOS ****

version 7.0
preserve
keep if ylm_ci>0 & ylm_ci<. & categopri<.
count if ylm_ci<.
tab categopri_ci
tab categopri_ci [w=factor2_ci]
** TOTAL PAIS **
foreach var of varlist employees firmow selfe selfenp patron patronm patronnm patronnp patronnpm patronnpnm {
tab `var' [w=factor2_ci]
}
** ZONA URBANA **
foreach var of varlist employees firmow selfe selfenp patron patronm patronnm patronnp patronnpm patronnpnm {
tab `var' [w=factor2_ci] if zona_c==1
}
** METROPOLITAN AREA **
foreach var of varlist employees firmow selfe selfenp patron patronm patronnm patronnp patronnpm patronnpnm {
tab `var' [w=factor2_ci] if citym_ci==1
}
** YOUTH (AGE 24 OR LESS) **
foreach var of varlist employees firmow selfe selfenp patron patronm patronnm patronnp patronnpm patronnpnm {
tab `var' [w=factor2_ci] if edad_ci<=24
}
** FEMALE **
foreach var of varlist employees firmow selfe selfenp patron patronm patronnm patronnp patronnpm patronnpnm {
tab `var' [w=factor2_ci] if sexo_ci==2
}
** COMERCIO, SERVICIOS, MANUFACTURAS, AGRICULTURA, OTROS **
forvalues i=1(1) 4 {
foreach var of varlist employees firmow selfe selfenp patron patronm patronnm patronnp patronnpm patronnpnm {
tab `var' [w=factor2_ci] if sector_ci==`i'
}
}

** CHEQUEO LOS OPUESTOS **
** ZONA RURAL **
foreach var of varlist employees firmow selfe selfenp patron patronm patronnm patronnp patronnpm patronnpnm {
tab `var' [w=factor2_ci] if zona_c==0
}
** RESTO DEL PAIS **
foreach var of varlist employees firmow selfe selfenp patron patronm patronnm patronnp patronnpm patronnpnm {
tab `var' [w=factor2_ci] if citym_ci==0
}
** NON-YOUTH (AGE MORE THAN 24) **
foreach var of varlist employees firmow selfe selfenp patron patronm patronnm patronnp patronnpm patronnpnm {
tab `var' [w=factor2_ci] if edad_ci>24
}
** MALE **
foreach var of varlist employees firmow selfe selfenp patron patronm patronnm patronnp patronnpm patronnpnm {
tab `var' [w=factor2_ci] if sexo_ci==1
}
** OTROS SECTORES **
foreach var of varlist employees firmow selfe selfenp patron patronm patronnm patronnp patronnpm patronnpnm {
tab `var' [w=factor2_ci] if sector_ci==5
}

** NUMERO DE OBSERVACIONES **
** TOTAL PAIS **
foreach var of varlist employees firmow selfe selfenp patron patronm patronnm patronnp patronnpm patronnpnm {
tab `var'
}
** ZONA URBANA **
foreach var of varlist employees firmow selfe selfenp patron patronm patronnm patronnp patronnpm patronnpnm {
tab `var' if zona_c==1
}
** METROPOLITAN AREA **
foreach var of varlist employees firmow selfe selfenp patron patronm patronnm patronnp patronnpm patronnpnm {
tab `var' if citym_ci==1
}
** YOUTH (AGE 24 OR LESS) **
foreach var of varlist employees firmow selfe selfenp patron patronm patronnm patronnp patronnpm patronnpnm {
tab `var' if edad_ci<=24
}
** FEMALE **
foreach var of varlist employees firmow selfe selfenp patron patronm patronnm patronnp patronnpm patronnpnm {
tab `var' if sexo_ci==2
}
** COMERCIO, SERVICIOS, MANUFACTURAS, AGRICULTURA, OTROS **
forvalues i=1(1) 4 {
foreach var of varlist employees firmow selfe selfenp patron patronm patronnm patronnp patronnpm patronnpnm {
tab `var' if sector_ci==`i'
}
}

** CHEQUEO LOS OPUESTOS **
** ZONA RURAL **
foreach var of varlist employees firmow selfe selfenp patron patronm patronnm patronnp patronnpm patronnpnm {
tab `var' if zona_c==0
}
** RESTO DEL PAIS **
foreach var of varlist employees firmow selfe selfenp patron patronm patronnm patronnp patronnpm patronnpnm {
tab `var' if citym_ci==0
}
** NON-YOUTH (AGE MORE THAN 24) **
foreach var of varlist employees firmow selfe selfenp patron patronm patronnm patronnp patronnpm patronnpnm {
tab `var' if edad_ci>24
}
** MALE **
foreach var of varlist employees firmow selfe selfenp patron patronm patronnm patronnp patronnpm patronnpnm {
tab `var' if sexo_ci==1
}
** OTROS SECTORES **
foreach var of varlist employees firmow selfe selfenp patron patronm patronnm patronnp patronnpm patronnpnm {
tab `var' if sector_ci==5
}

restore

foreach var of varlist firmow selfe selfenp patron patronm patronnm patronnp patronnpm patronnpnm {
sum ybus_ratio [w=factor2_ci] if `var'==1
}

capture log close

log using "D:\Data.idb\CECILIACA\BANANAS\BELIZE\Programs\1999\sisrequest_blz99_2.log", replace

**** TABULADOS 2 ****
version 7.0
preserve
keep if emp_ci==1 & categopri<.
count if emp_ci==1
tab categopri_ci
tab categopri_ci [w=factor2_ci]

** TOTAL PAIS **
foreach var of varlist employees firmow selfe selfenp patron patronm patronnm patronnp patronnpm patronnpnm {
tab `var' [w=factor2_ci]
}
** ZONA URBANA **
foreach var of varlist employees firmow selfe selfenp patron patronm patronnm patronnp patronnpm patronnpnm {
tab `var' [w=factor2_ci] if zona_c==1
}
** METROPOLITAN AREA **
foreach var of varlist employees firmow selfe selfenp patron patronm patronnm patronnp patronnpm patronnpnm {
tab `var' [w=factor2_ci] if citym_ci==1
}
** YOUTH (AGE 24 OR LESS) **
foreach var of varlist employees firmow selfe selfenp patron patronm patronnm patronnp patronnpm patronnpnm {
tab `var' [w=factor2_ci] if edad_ci<=24
}
** FEMALE **
foreach var of varlist employees firmow selfe selfenp patron patronm patronnm patronnp patronnpm patronnpnm {
tab `var' [w=factor2_ci] if sexo_ci==2
}
** COMERCIO, SERVICIOS, MANUFACTURAS, AGRICULTURA, OTROS **
forvalues i=1(1) 4 {
foreach var of varlist employees firmow selfe selfenp patron patronm patronnm patronnp patronnpm patronnpnm {
tab `var' [w=factor2_ci] if sector_ci==`i'
}
}

restore
capture log close


log using "D:\Data.idb\CECILIACA\BANANAS\BELIZE\Programs\1999\sisrequest_blz99_pov.log", replace

** POBREZA **
* % Personas
foreach var of varlist pobrey_idb {
tab `var' [w=factor2_ci]
}


* % Hogares
foreach var of varlist pobrey_idb {
tab `var' [w=factor2_ci] if hogar==1
}

preserve
keep if ylm_ci>0 & ylm_ci<. & categopri<.
local pobre="pobrey_idb"

foreach p of local pobre {
* CONSUMO
* INGRESO
** TOTAL PAIS **
foreach var of varlist employees firmow selfe selfenp patron patronm patronnm patronnp patronnpm patronnpnm {
tab `var' [w=factor2_ci] if `p'==1
}
** ZONA URBANA **
foreach var of varlist employees firmow selfe selfenp patron patronm patronnm patronnp patronnpm patronnpnm {
tab `var' [w=factor2_ci] if zona_c==1 & `p'==1
}
** METROPOLITAN AREA **
foreach var of varlist employees firmow selfe selfenp patron patronm patronnm patronnp patronnpm patronnpnm {
tab `var' [w=factor2_ci] if citym_ci==1 & `p'==1
}
** YOUTH (AGE 24 OR LESS) **
foreach var of varlist employees firmow selfe selfenp patron patronm patronnm patronnp patronnpm patronnpnm {
tab `var' [w=factor2_ci] if edad_ci<=24 & `p'==1
}
** FEMALE **
foreach var of varlist employees firmow selfe selfenp patron patronm patronnm patronnp patronnpm patronnpnm {
tab `var' [w=factor2_ci] if sexo_ci==2 & `p'==1
}
** COMERCIO, SERVICIOS, MANUFACTURAS, AGRICULTURA, OTROS **
forvalues i=1(1) 4 {
foreach var of varlist employees firmow selfe selfenp patron patronm patronnm patronnp patronnpm patronnpnm {
tab `var' [w=factor2_ci] if sector_ci==`i' & `p'==1
}
}
** NUMERO DE OBSERVACIONES **
** TOTAL PAIS **
foreach var of varlist employees firmow selfe selfenp patron patronm patronnm patronnp patronnpm patronnpnm {
tab `var' if `p'==1
}
** ZONA URBANA **
foreach var of varlist employees firmow selfe selfenp patron patronm patronnm patronnp patronnpm patronnpnm {
tab `var' if zona_c==1 & `p'==1
}
** METROPOLITAN AREA **
foreach var of varlist employees firmow selfe selfenp patron patronm patronnm patronnp patronnpm patronnpnm {
tab `var' if citym_ci==1 & `p'==1
}
** YOUTH (AGE 24 OR LESS) **
foreach var of varlist employees firmow selfe selfenp patron patronm patronnm patronnp patronnpm patronnpnm {
tab `var' if edad_ci<=24 & `p'==1
}
** FEMALE **
foreach var of varlist employees firmow selfe selfenp patron patronm patronnm patronnp patronnpm patronnpnm {
tab `var' if sexo_ci==2 & `p'==1
}
** COMERCIO, SERVICIOS, MANUFACTURAS, AGRICULTURA, OTROS **
forvalues i=1(1) 4 {
foreach var of varlist employees firmow selfe selfenp patron patronm patronnm patronnp patronnpm patronnpnm {
tab `var' if sector_ci==`i' & `p'==1
}
}
}
restore



capture log close

*/
