clear

cd "${surveysFolder}\ARM\BLZ\2001"


capture log close
log using "${surveysFolder}\ARM\BLZ\2001\Arm_data\BLZ2001EA_BID.log", replace


clear
set more off
set memory 400m

************************************************************************************************************
*****                                       BELIZE 2001                                                *****
*****                                 LABOR FORCE SURVEY 2001                                          *****
*****                                      19,603  Personas                                            *****
*****                                        4,285 Hogares                                             *****
************************************************************************************************************

use "${surveysFolder}\ARM\BLZ\2001\Orig_data\belize01.dta", clear

destring _all, replace
compress

ren district q01 
ren urbrur q02
ren ednum q03
* ren sample q04 not available for 2001 *
ren hhnum q05
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

gen idp_ci=p00
label var idp_ci "Identificador Individual dentro del Hogar"

sort idh_ch idp_ci


*************
* factor_ch *
*************

************************* WEIGHTS **************************************************************************
* Corozal (1)	Orange Walk (2)		Belize (3)	Cayo (4)	Stann Creek (5)		Toledo (6) *
* 12.48		12.91			15.24		12.86		13.60			14.25      *
************************************************************************************************************

gen factor_ch=.

replace factor_ch=12.48 if q01==1
replace factor_ch=12.91 if q01==2
replace factor_ch=15.24 if q01==3
replace factor_ch=12.86 if q01==4
replace factor_ch=13.60 if q01==5
replace factor_ch=14.25 if q01==6

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

gen anio_c=2001
label variable anio_c "Año de la Encuesta"

**************************
* Periodo de Referencia: * 
**************************

gen byte mes_c=4
label variable mes_c "Mes de la Encuesta: Abril"

********
* SEXO *
********

gen sexo_ci=p02
label var sexo_ci "Sexo del Individuo"
label define sexo_ci 1 "Hombre" 2 "Mujer"  
label value sexo_ci sexo_ci


**************
* PARENTESCO *
**************

gen relacion_ci=1 if p01==1
replace relacion_ci=2 if p01==2
replace relacion_ci=3 if p01==3
replace relacion_ci=4 if p01==4 | p01==5 | p01==6 | p01==7
replace relacion_ci=5 if p01==8
replace relacion_ci=. if p01==9 /* No sabe */
label var relacion_ci "Parentesco o relacion con el Jefe del Hogar"
label define relacion_ci 1 "Jefe(a)" 2 "Esposo(a) o compañero(a)" 3 "Hijo(a)" 4 "Otro pariente" 5 "Otro NO pariente" 6 "Empleado(a) domestico(a)" 
label value relacion_ci relacion_ci

********
* EDAD *
********

gen edad_ci=p03
label var edad_ci "Edad del Individuo"

****************
* nconyuges_ch *
****************

egen nconyuges_ch=sum(relacion_ci==2), by (idh_ch)
label variable nconyuges_ch "Numero de Conyuges"

*************
* nhijos_ch *
*************

egen nhijos_ch=sum(relacion_ci==3 & edad<18), by (idh_ch)
label variable nhijos_ch "Numero de Hijos"

****************
* notropari_ch *
****************

egen notropari_ch=sum((relacion_ci==3 & edad>=18) | (relacion_ci>3 & relacion_ci<5)), by (idh_ch)
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
assert claseT==19603

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

egen nmayor21_ch=sum((relacion_ci>0 & relacion_ci<5) & (edad>=21)), by (idh_ch)
label variable nmayor21_ch "Numero de personas de 21 años o mas dentro del Hogar"

***************
* nmenor21_ch *
***************

egen nmenor21_ch=sum((relacion_ci>0 & relacion_ci<5) & (edad<21)), by (idh_ch)
label variable nmenor21_ch "Numero de personas menores a 21 años dentro del Hogar"

***************
* nmayor65_ch *
***************

egen nmayor65_ch=sum((relacion_ci>0 & relacion_ci<5) & (edad>=65)), by (idh_ch)
label variable nmayor65_ch "Numero de personas de 65 años o mas dentro del Hogar"

**************
* nmenor6_ch *
**************

egen nmenor6_ch=sum((relacion_ci>0 & relacion_ci<5) & (edad<6)), by (idh_ch)
label variable nmenor6_ch "Numero de niños menores a 6 años dentro del Hogar"

**************
* nmenor1_ch *
**************

egen nmenor1_ch=sum((relacion_ci>0 & relacion_ci<5) & (edad<1)),  by (idh_ch)
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
replace emp_ci=1 if p14==1 | p15==1 | p16==1
replace emp_ci=0 if p14==2 & p15==2 & p16==2
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
replace desemp2_ci=1 if (p18==2 & p20>=1 & p20<=3)
replace desemp2_ci=0 if (p18==2 & p20>=4 & p20<=17) | emp_ci==1
label var desemp2_ci "desemp1 + personas que no tienen trabajo ni lo buscaron, pero esperan respuesta de una solicitud de empleo, entrevista"

**************
* desemp3_ci *
**************

gen byte desemp3_ci=.
replace desemp3_ci=1 if p18==1 | (p18==2 & p20>=1 & p20<=4)
replace desemp3_ci=0 if (p18==2 & p20>=5 & p20<=17) | emp_ci==1
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
replace desalent_ci=0 if pea3_ci==1 | p20<99
replace desalent_ci=1 if p20==5 | p20==6 | p20==8
label var desalent_ci "Trabajadores desalentados, personas que creen que por alguna razon no conseguiran trabajo" 

*** Horas trabajadas en la Actividad Principal

***************
* horaspri_ci *
***************

gen horaspri_ci=p42mainj if p42mainj<99 /* p42mainj for 2001 */
label var horaspri_ci "Horas totales trabajadas la semana pasada en la Actividad Principal"

egen horastot_ci=rsum(p42mainj  p42other) if p42mainj<99 &  p42other<99
replace horastot_ci=. if p42mainj==. &  p42other==.
label var horastot_ci "Horas totales trabajadas la semana pasada en todas las Actividades"

*****************************
* Trabajadores Subempleados *
*****************************

gen subemp_ci=.
replace subemp_ci=1 if horastot_ci<=30 & p48==1
replace subemp_ci=0 if (horastot_ci>30 & horastot_ci<.) | (horastot_ci<=30 & p48==2)
label var subemp_ci "Trabajadores subempleados"

*******************************
* Trabajadores a Medio Tiempo *
*******************************

gen byte tiempoparc_ci=.
replace tiempoparc_ci=1 if horastot_ci<=30 & p48==2
replace tiempoparc_ci=0 if (horastot_ci>30 & horastot_ci<.) | (horastot_ci<=30 & p48==1)
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
replace nempleos_ci=1 if p37==2
replace nempleos_ci=2 if p37==1
label var nempleos_ci "Numero de empleos"
label define nempleos_ci 1 "un trabajo" 2 "dos o mas trabajos"
label values nempleos_ci nempleos_ci

**********************
* Tamaño de la firma *
**********************

/* valores positivos solo para los patrones en categopri==1 */
gen byte tamfirma_ci=.
replace tamfirma_ci=1 if p39yr_ro==2 | p39yr_ro==3 | p39yr_ro==4
replace tamfirma_ci=0 if p39yr_ro==1
label var tamfirma_ci "Trabajadores formales"
label define tamfirma_ci 1 "Mas de 4 trabajadores" 0 "4 o menos trabajadores"
label values tamfirma_ci tamfirma_ci

******************
* Sector Publico *
******************

gen spublico_ci=.
replace spublico_ci=1 if p38main==3 | p38main==4
replace spublico_ci=0 if p38main==1 | p38main==2 | p38main==5 | p38main==6
label var spublico_ci "Personas que trabajan en el sector publico"

************************************************************************************************************
*** VARIABLES DE DEMANDA LABORAL
************************************************************************************************************

*********************************
* OCUPACION (variable p40iscom) *
*********************************

capture drop ocupa_ci 
gen ocupa_ci=.
replace ocupa_ci=1 if p40iscom>=2000 & p40iscom<=3999 /* p40iscom for 2001 */
replace ocupa_ci=2 if p40iscom>=1000 & p40iscom<=1999
replace ocupa_ci=3 if p40iscom>=4000 & p40iscom<=4999
replace ocupa_ci=4 if p40iscom>=5200 & p40iscom<=5999
replace ocupa_ci=5 if p40iscom>=5000 & p40iscom<=5199
replace ocupa_ci=6 if p40iscom>=6000 & p40iscom<=6999
replace ocupa_ci=7 if p40iscom>=7000 & p40iscom<=8999
replace ocupa_ci=8 if p40iscom>=0 & p40iscom<=999 
replace ocupa_ci=9 if (p40iscom>=9000 & p40iscom<=9996) | p40iscom==9998

label var ocupa_ci "Ocupacion Laboral en la Actividad Principal"
label define ocupa_ci 1 "PROFESIONALES Y TECNICOS" 2 "GERENTES, DIRECTORES Y FUNCIONARIOS SUPERIORES"  3 "PERSONAL ADMINISTRATIVO Y NIVEL INTERMEDIO" 4 "COMERCIANTES Y VENDEDORES" 5 "TRABAJADORES EN SERVICIOS" 6 "TRABAJADORES AGRICOLAS Y AFINES" 7 "OBREROS NO AGRICOLAS, CONDUCTORES DE MAQUINAS Y VEHICULOS DE TRANSPORTE Y SIMILARES" 8 "FUERZAS ARMADAS" 9 "OTRAS OCUPACIONES NO CLASIFICADAS ANTERIORMENTE"
label values ocupa_ci ocupa_ci

****************************
* RAMA (variable p41isicm) *
****************************

gen byte rama_ci=.
replace rama_ci=1 if p41isicm>=0 & p41isicm<1000
replace rama_ci=2 if p41isicm>=1000 & p41isicm<2000
replace rama_ci=3 if p41isicm>=2000 & p41isicm<4000
replace rama_ci=4 if p41isicm>=4000 & p41isicm<5000
replace rama_ci=5 if p41isicm>=5000 & p41isicm<6000
replace rama_ci=6 if p41isicm>=6000 & p41isicm<7000
replace rama_ci=7 if p41isicm>=7000 & p41isicm<8000
replace rama_ci=8 if p41isicm>=8000 & p41isicm<9000
replace rama_ci=9 if p41isicm>=9000 & p41isicm<10000

label var rama_ci "Rama Laboral en la Ocupacion Principal"
label define rama_ci 1 "Agricultura, caza, silvicultura y pesca" 2 "Explotación de minas y canteras" 3 "Industrias manufactureras" 4 "Electricidad, gas y agua" 5 "Construcción" 6 "Comercio al por mayor y menor, restaurantes, hoteles" 7 "Transporte y almacenamiento" 8 "Establecimientos financieros, seguros, bienes inmuebles" 9 "Servicios sociales, comunales y personales"
label values rama_ci rama_ci

******************************
*** DURACION DEL DESEMPLEO ***
******************************

gen durades_ci=.
label var durades_ci "Duracion del Desempleo (en meses)"

gen durades1_ci=p34 if p34<9
label var durades1_ci "Duracion del Desempleo (categorias)"
label define durades1_ci 1 "Menos de 1 mes" 2 "1 a 3 meses" 3 "4 a 6 meses" 4 "7 a 12 meses" 5 "mas de 12 meses"
label values durades1_ci durades1_ci

****************************************
*** Antiguedad, AÑOS (NOT AVAILABLE) ***
****************************************

gen antiguedad_ci=p49yrmj if p49yrmj<99
label var antiguedad_ci "Antiguedad en la Ocupacion Actual (en años)"

********************************************************************************
* VARIABLES EDUCATIVAS (para todos los miembros del hogar)
********************************************************************************

* AÑOS DE EDUCACION

* Esta solo la variable 'p14' que se refire al maximo nivel completo

/* NIVEL=p14
1 Pre primaria
2 Primaria
3 Secundaria
4 Terciaria
5 Otra
6 Ninguna */


***********
* aedu_ci *
***********

gen byte aedu_ci=.
label variable aedu_ci "Años de Educacion"

***************************************
** Categorias educativas excluyentes **
***************************************

gen eduno_ci=.
replace eduno_ci=1 if p07==1 & p08==0
replace eduno_ci=0 if (p07>1 & p07<=6) | (p07==1 & p08>0 & p08<=7)
label var eduno_ci "1 = personas sin educacion (excluye preescolar)"


************ REVISAR ******************

gen edupre_ci=.
label var edupre_ci "Educacion preescolar"

gen edupi_ci=.
replace edupi_ci=1 if p07==1 & p08>0 & p08<7
replace edupi_ci=0 if (p07>1 & p07<=6) | (p07==1 & p08==7)
label var edupi_ci "1 = personas que no han completado el nivel primario"

gen edupc_ci=.
replace edupc_ci=1 if (p07==1 & p08==7)
replace edupc_ci=0 if (p07==1 & p08>0 & p08<7) | (p07>1 & p07<=6)
label var edupc_ci "1 = personas que han completado el nivel primario"

/*gen edusi_ci=.
label var edusi_ci "1 = personas que no han completado el nivel secundario"

gen edusc_ci=.
replace edusc_ci=1
replace edusc_ci=0
label var edusc_ci "1 = personas que han completado el nivel secundario"

gen eduui_ci=.
label var eduui_ci "1 = personas que no han completado el nivel universitario o superior"

gen eduuc_ci=.
replace eduuc_ci=1 if
replace eduuc_ci=0 if
label var eduuc_ci "1 = personas que han completado el nivel universitario o superior"*/


gen edus1i_ci=.
label var edus1i_ci "1 = personas que no han completado el primer ciclo de la educacion secundaria"

gen edus1c_ci=.
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
***************************************************************************

**********
* ASISTE *
**********

gen asiste_ci=.
replace asiste_ci=1 if p06==0 | p06==1
replace asiste_ci=0 if p06==2
label var asiste_ci "Personas que actualmente asisten a centros de enseñanza"

* POR QUE NO ASISTE 
gen pqnoasis_ci=.
replace pqnoasis_ci=p08b
label var pqnoasis_ci "Razon principal por la cual ha abandonado o ha dejado de asistir a clases en los ultimos 5 años"
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
*** INGRESOS LABORALES  ***
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
replace categopri_ci=1 if p38main==1
replace categopri_ci=2 if p38main==2
replace categopri_ci=3 if p38main==3 | p38main==4 | p38main==5
replace categopri_ci=4 if p38main==6
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
replace categosec_ci=1 if p38other==1 /* p38other for 2001 */
replace categosec_ci=2 if p38other==2
replace categosec_ci=3 if p38other==3 | p38other==4 | p38other==5
replace categosec_ci=4 if p38other==6
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
gen ylm_ci=p50ginc*(365/12) if p51==1 /* diario */
replace ylm_ci=p50ginc*(30/7) if p51==2 /* semanal */
replace ylm_ci=p50ginc*(15/7) if p51==3 /* cada dos semanas */
replace ylm_ci=p50ginc*2 if p51==4 /* dos veces al mes */
replace ylm_ci=p50ginc if p51==5 /* mensual */
replace ylm_ci=p50ginc/(365/30) if p51==6 /* anual */
replace ylm_ci=p50ginc if p51==7 & categopri_ci==4
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
gen byte nrylmpri_ci=0
replace nrylmpri_ci=1 if p51==9 | (p51==7 & p50ginc>0)
label var nrylmpri_ci "Identificador de No Respuesta del Ingreso Monetario de la Actividad Principal"

*** Dummy para el Hogar
capture drop nrylmpri_ch
sort idh
egen nrylmpri_ch=sum(nrylmpri_ci) if miembro==1, by(idh_ch)
replace nrylmpri_ch=1 if nrylmpri_ch>1 & nrylmpri_ch~=. & miembro==1 
label var nrylmpri_ch "Identificador de Hogares en donde alguno de los miembros No Responde el Ingreso Monetario de la Actividad Principal"



egen ylm_ch=sum(ylm_ci) if miembros_ci==1, by(idh_ch)
label var ylm_ch "Ingreso Laboral Monetario del Hogar (Bruto)"

egen ylmnr_ch=sum(ylm_ci) if miembros_ci==1 & nrylmpri_ch==0, by(idh_ch)
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
/* Not available for 2001
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

*******************
*** BELIZE 2001 ***
*******************

/*
1                   head 
2       spouse / partner
3                  child
4 son / daughter-in- law
5             grandchild 
6  parent /parent-in-law 
7         other relative 
8           non relative 
9                 dk/ ns 
*/

rename p01 parentco
replace parentco = . if parentco == 9

rename p02 sexo
rename p03 edad

/*
  q1.6 presently |
       attending |
          school |
  ---------------+
0 yes- full-time |
1 yes -part time |
2             no |
9          dk/ns |
  ---------------+
*/


rename p06 asiste 
replace asiste = 1 if asiste == 0
replace asiste = . if asiste == 9

/*
    q1.7highest education |
          level completed |
 -------------------------+
1                    none |
2                 primary |
3             high school |
4            bttc/bca/bns |
5sixth form or equivalent |
6              university |
9                   dk/ns |
 -------------------------+
*/

rename p07 ultcurso
replace ultcurso = . if ultcurso == 9

/*
 q1.8 number |
    of years |
   completed |
 ------------+
           0 |
           1 |
           2 |
           3 |
           4 |
           5 |
           6 |
           7 |
          99 |
 ------------+
*/

rename p08 ultgrado 
replace ultgrado = . if  ultgrado == 99

gen cursoasi = ultcurso
gen gradoasi = ultgrado+1 if asiste==1

* categoria ocupacional

gen categ = categopri_ci
/*
    CATEGORIA OCUPACIONAL |
      ACTIVIDAD PRINCIPAL |
 -------------------------+
1                  Patron |
2           Cuenta Propia |
3              Asalariado |
4Trabajador No Remunerado |
*/

rename p40iscom ocup
rename p41isicm rama
gen water   =.

gen combusti=.
gen servsani=.
gen usosani =.
gen nrocuart=.
gen tipoviv =.
gen tenencia=.
gen paredes =.
gen piso    =.
gen condact = pea2_ci

** Area
gen area=.
replace area=1 if zona==1
replace area=2 if zona==0

** Expansion Factor

* Not in database

** Years of education. 
/*

*/


gen peaa=0 if pea2==0
replace peaa=1 if pea2==1

gen TASADESO=0 if peaa==1 & edad>=15
replace TASADESO=1 if peaa==0 & edad>=15


************************
*** MDGs CALCULATION ***
************************

/*
SECTION 5. EDUCATION

asiste
5.1a Is ............ attending school/classes?
 1. Yes
 2. No	==> Go to Q.5.17
 9. DK/NS

cursoasi
5.2a What type of school is ............ attending at present?
 01. Pre-school/Nursery/Kindergarten
 10. Private primary
 11. Government and Assisted Primary
 31. Private Secondary
 32. Government and Assisted Secondary
 34. Technical/vocational
 40. Adult Literacy Classes
 60. University
 61. Tertiary Institution
 98. Other(Specify )
 99. Not stated

gradoasi
5.2b In what class/standard /year/form is ......?
*/

*** GOAL 2. ACHIEVE UNIVERSAL PRIMARY EDUCATION
* ISCED 1
* NIVEL=2 BASICA

gen NERP=1 if (edad>=5 & edad<=10)
replace NERP=0 if (edad>=5 & edad<=10)
replace NERP=1 if (edad>=5 & edad<=10) & asiste==1 & ((cursoasi==2 | cursoasi==1 )  & ((gradoasi>=1 & gradoasi<=7)))

** Target 3, Additional Indicator: Net Attendance Ratio in Secondary
* ISCED 2 & 3

gen NERS=1 if (edad>=11 & edad<=16)
replace NERS=0 if (edad>=11 & edad<=16)
replace NERS=1 if asiste==1 & (edad>=11 & edad<=16) & (((cursoasi==2 | cursoasi==1) & (gradoasi>=1 & gradoasi<=7)) | (cursoasi==3))

** Target 3, Indicator: Literacy Rate of 15-24 Years Old
* At least 5 years of formal education

gen LIT=1 if (edad>=15 & edad<=24)
replace LIT=0 if (edad>=15 & edad<=24) 
replace LIT=1 if (edad>=15 & edad<=24) & ((asiste==1 & ((cursoasi==2 | cursoasi==1) & (gradoasi>=4)) | (cursoasi==3 | cursoasi==4 | cursoasi==5 | cursoasi==6))) 
** Target 4, Indicator: Ratio Girls to boys in primary, secondary and tertiary (%)

** Target 4, Ratio of Girls to Boys in Primary*

gen RPRIMM=1 if sexo==2 & asiste==1 & ((cursoasi==2 | cursoasi==1 )  & ((gradoasi>=1 & gradoasi<=7)))
replace RPRIMM=0 if RPRIMM==. 
gen RPRIMH=1 if sexo==1 & asiste==1 & ((cursoasi==2 | cursoasi==1 )  & ((gradoasi>=1 & gradoasi<=7)))
replace RPRIMH=0 if RPRIMH==.
gen RATIOPRIM=0 if sexo==2 & asiste==1 & ((cursoasi==2 | cursoasi==1 )  & ((gradoasi>=1 & gradoasi<=7)))
replace RATIOPRIM=1 if sexo==1 & asiste==1 & ((cursoasi==0 | cursoasi==1 )  & ((gradoasi>=1 & gradoasi<=7)))

** Target 4, Ratio of Girls to Boys in Secondary*

gen RSECM=1 if sexo==2 & asiste==1  & (((cursoasi==2 | cursoasi==1) & ((gradoasi>=1 & gradoasi<=7))) | (cursoasi==3))
replace RSECM=0 if RSECM==.
gen RSECH=1 if sexo==1 & asiste==1 & (((cursoasi==2 | cursoasi==1) & (gradoasi>=1 & gradoasi<=7)) | (cursoasi==3))
replace RSECH=0 if RSECH==.
gen RATIOSEC=0 if sexo==2 & asiste==1 & (((cursoasi==2 | cursoasi==1) & (gradoasi>=1 & gradoasi<=7)) | (cursoasi==3))
replace RATIOSEC=1 if sexo==1 & asiste==1 & (((cursoasi==2 | cursoasi==1) & (gradoasi>=1 & gradoasi<=7)) | (cursoasi==3))

** Target 4, Indicator: Ratio of Girls to Boys in Tertiary*

gen RTERM=1 if sexo==2 & asiste==1 & (cursoasi==5 | cursoasi==6)
replace RTERM=0 if RTERM==.
gen RTERH=1 if sexo==1 & asiste==1 & (cursoasi==5 | cursoasi==6)
replace RTERH=0 if RTERH==.
gen RATIOTER=0 if sexo==2 & asiste==1 & (cursoasi==5 | cursoasi==6)
replace RATIOTER=1 if sexo==1 & asiste==1 & (cursoasi==5 | cursoasi==6)

** Target 4, Indicator: Ratio of Girls to Boys in Primary, Secondary and Tertiary*

gen RALLM=1 if sexo==2 & asiste==1 & ((((cursoasi==2 | cursoasi==1 )  & ((gradoasi>=1 & gradoasi<=7)))) | (((cursoasi==2 | cursoasi==1)) | (cursoasi==3 | cursoasi==4)) | (cursoasi==5 | cursoasi==6) )
replace RALLM=0 if RALLM==.
gen RALLH=1 if sexo==1 & asiste==1 & ((((cursoasi==2 | cursoasi==1 )  & ((gradoasi>=1 & gradoasi<=7)))) | (((cursoasi==2 | cursoasi==1)) | (cursoasi==3 | cursoasi==4)) | (cursoasi==5 | cursoasi==6) )
replace RALLH=0 if RALLH==.
gen RATIOALL=0 if sexo==2 & asiste==1 & ((((cursoasi==2 | cursoasi==1 )  & ((gradoasi>=1 & gradoasi<=7)))) | (((cursoasi==2 | cursoasi==1)) | (cursoasi==3 | cursoasi==4)) | (cursoasi==5 | cursoasi==6) )
replace RATIOALL=1 if sexo==1 & asiste==1 & ((((cursoasi==2 | cursoasi==1 )  & ((gradoasi>=1 & gradoasi<=7)))) | (((cursoasi==2 | cursoasi==1)) | (cursoasi==3 | cursoasi==4)) | (cursoasi==5 | cursoasi==6) )

** Target 4, Indicator: Ratio of literate women to men 15-24 year olds*
* At least 5 years of formal education

gen MA=1 if (edad>=15 & edad<=24) & ((asiste==1 & ((cursoasi==2 | cursoasi==1) & (gradoasi>=4)) | (cursoasi==3 | cursoasi==4 | cursoasi==5 | cursoasi==6)) & (sexo==2))
replace MA=0 if MA==.
gen HA=1 if (edad>=15 & edad<=24) & ((asiste==1 & ((cursoasi==2 | cursoasi==1) & (gradoasi>=4)) | (cursoasi==3 | cursoasi==4 | cursoasi==5 | cursoasi==6)) & (sexo==1))
replace HA=0 if HA==.
gen RATIOLIT=0 if (edad>=15 & edad<=24) & ((asiste==1 & ((cursoasi==2 | cursoasi==1) & (gradoasi>=4)) | (cursoasi==3 | cursoasi==4 | cursoasi==5 | cursoasi==6)) & (sexo==2))
replace RATIOLIT=1 if (edad>=15 & edad<=24) & ((asiste==1 & ((cursoasi==2 | cursoasi==1) & (gradoasi>=4)) | (cursoasi==3 | cursoasi==4 | cursoasi==5 | cursoasi==6)) & (sexo==1))

** Target 4, Indicator: Share of women in wage employment in the non-agricultural sector (%)
/*
categ (p611)						ocup (p68)
6.11. Did ..... carry on your/his/her own business, 	6.8 What is ......./your occupation?
 or work for a wage or salary or as an unpaid worker 
 in a family business?
 1. Paid employee - Government				rama (p69)
 2. Paid employee - Private				6.9 What type of business/activity 
 3. Unpaid Worker					 is carried on where ...... is working?
 4. Own business - with paid help
 5. Own business - without paid help
 9. Don't know/Not Stated
*/

* Without Domestic Service

gen domestic=0 if (ocup>0 & ocup<9999)
replace domestic=1 if ((ocup>=9130 & ocup<=9139))
gen WENAS=1 if (edad>=15 & edad<=64) & (categ==3) & (rama>=1000 & rama<9999) & domestic==0
replace WENAS=0 if (edad>=15 & edad<=64) & ((categ==3) & (rama>=1000 & rama<9999) & domestic==0)
replace WENAS=1 if (edad>=15 & edad<=64) & ((categ==3) & (rama>=1000 & rama<9999) & sexo==2) & domestic==0

** Target 4, Indicator: Share of women in wage employment in the non-agricultural sector (%)
* With domestic servants

gen WENASD=1 if (edad>=15 & edad<=64) & (categ==3) & (rama>=1000 & rama<9999)
replace WENASD=0 if (edad>=15 & edad<=64) & ((categ==3) & (rama>=1000 & rama<9999))
replace WENASD=1 if (edad>=15 & edad<=64) & ((categ==3) & (rama>=1000 & rama<9999)) & sexo==2

gen ELEC=.
gen SFUELS=.
gen WATER=.
gen SANITATION=.
gen SECTEN=.


** GOAL 8. DEVELOP A GLOBAL PARTNERSHIP FOR DEVELOPMENT

** Target 16, Indicator: Unemployment Rate of 15 year-olds (%)

gen UNMPLYMENT15=1 if TASADESO==0 & (edad>=15 & edad<=24)
replace UNMPLYMENT15=0 if TASADESO==0 & (edad>=15 & edad<=24)
replace UNMPLYMENT15=1 if TASADESO==1 & (edad>=15 & edad<=24)

** Target 18, Indicator: "Telephone lines and celullar subscribers per 100 population"
*Only telephone lines
/*
19. Does this household have any of the following?
 a. Telephone	(h20a)
*/

gen TELEPHONE=.

************************************************************************
**** ADDITIONAL SOCIO - ECONOMIC COMMON COUNTRY ASESSMENT INDICATORS ****
************************************************************************

** CCA 41 Number of Persons per Room*

generate PERSROOM2=.

** Disconnected Youths
/*
6.17 Why did ......... not seek work or do anything to earn income last week?
 01. At school
 02. Housekeeping
 03. Retired
 04. Disabled
 05. Temporary illness
 06. Did not want work
 07. Awaiting results or applications
 08. Knew of no vacancy
 09. Presently employed
 10. Discouraged
 11. Other (Specify )
 99. DK/NS
*/

gen DISCONN=.
**********************************************
*** OTHERS INDICATORS RELATED TO EDUCATION ***
**********************************************

** CAMBIAR

* Grade for age
gen GFA =.
gen GFAP = .
gen GFAS =.

 * NA

* Grade for age primary
 * NA

* Grade for age Secondary
 * NA

* Ever enrolled primary
* NA

* Ever enrolled secondary
* NA




save "${surveysFolder}\ARM\BLZ\2001\Arm_data\BLZ2001EA_BID.dta", replace
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

log using "${surveysFolder}\Data.idb\CECILIACA\BANANAS\BELIZE\Programs\1999\sisrequest_blz99.log", replace


save "${surveysFolder}\ARM\BLZ\2001\Orig_data\belize01_02.dta", replace

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

log using "${surveysFolder}\Data.idb\CECILIACA\BANANAS\BELIZE\Programs\1999\sisrequest_blz99_2.log", replace

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


log using "${surveysFolder}\Data.idb\CECILIACA\BANANAS\BELIZE\Programs\1999\sisrequest_blz99_pov.log", replace

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
