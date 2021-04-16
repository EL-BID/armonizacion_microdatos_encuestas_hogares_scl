

clear
set more off


use  "${surveysFolder}\ARM\BLZ\2007\Orig_data\Core&QualityBelize2007.dta"


* Identificador del Hogar
egen idh_ch=group(district urbrur ctv ednumber hhnumber)
label var idh_ch "Identificador Unico del Hogar"

* Identificador Individual
gen idp_ci=cq10
label var idp_ci "Identificador Individual dentro del Hogar"

sort idh_ch idp_ci

gen factor_ch=weights
label var factor_ch "Factor de Expansion del Hogar"

gen factor_ci=weights
label var factor_ci "Factor de Expansion del Individuo"




* ZONA
gen byte zona_c=1 if urbrur==1 | urbrur==2 | urbrur==3 | urbrur==4 /* Urbana */
replace zona_c=0 if urbrur==5  /* Rural */
label variable zona_c "ZONA GEOGRAFICA"
label define zona_c 0 "Rural" 1 "Urbana"
label value zona_c zona_c



* COUNTRY - YEAR
gen str3 pais_c="BLZ"
label variable pais_c "Nombre del Pais"
gen anio_c=2007
label variable anio_c "Año de la Encuesta"

* Periodo de Referencia: 
gen byte mes_c=10
label variable mes_c "Mes de la Encuesta: Octubre"


* SEXO
gen sexo_ci=cq12

label var sexo_ci "Sexo del Individuo"
label define sexo_ci 1 "Hombre" 2 "Mujer"  
label value sexo_ci sexo_ci

* PARENTESCO

gen relacion_ci=1 if cq11==1
replace relacion_ci=2 if cq11==2
replace relacion_ci=3 if cq11==3
replace relacion_ci=4 if cq11==4 | cq11==5 | cq11==6 | cq11==7
replace relacion_ci=5 if cq11==8
replace relacion_ci=. if cq11==9 /* No sabe */
label var relacion_ci "Parentesco o relacion con el Jefe del Hogar"
label define relacion_ci 1 "Jefe(a)" 2 "Esposo(a) o compañero(a)" 3 "Hijo(a)" 4 "Otro pariente" 5 "Otro NO pariente" 
label value relacion_ci relacion_ci

* EDAD
gen edad_ci=cq13
label var edad_ci "Edad del Individuo"

egen nconyuges_ch=sum(relacion_ci==2), by (idh_ch)
label variable nconyuges_ch "Numero de Conyuges"

egen nhijos_ch=sum(relacion_ci==3 & edad<18), by (idh_ch)
label variable nhijos_ch "Numero de Hijos"
egen notropari_ch=sum((relacion_ci==3 & edad>=18) | (relacion_ci>3 & relacion_ci<5)), by (idh_ch)
label variable notropari_ch "Numero de Otros Parientes "
egen notronopari_ch=sum(relacion_ci==5), by (idh_ch)
label variable notronopari_ch "Numero de Otros NO Parientes "
egen nempdom_ch=sum(relacion_ci==6), by (idh_ch)
label variable nempdom_ch "Numero de Empleados Domesticos"

* HOUSEHOLD TYPE (unipersonal, nuclear, ampliado, compuesto, corresidentes)    
* note: These are all defined in terms of relationship to household head

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



* HOUSEHOLD COMPOSITION VARIABLES 
/* note: These are unrelated to who is the head
   note: That childh denotes the number of children of the head, while numkids counts the number of all kids in the household */

sort idh_ch

* NUMBER OF PERSONS IN THE HOUSEHOLD (not including domestic employees or other relatives)
egen nmiembros_ch=sum(relacion_ci>0 & relacion_ci<5), by (idh_ch)
label variable nmiembros_ch "Numero de miembros de 7 años o mas de edad en el Hogar"

egen nmayor21_ch=sum((relacion_ci>0 & relacion_ci<5) & (edad>=21)), by (idh_ch)
label variable nmayor21_ch "Numero de personas de 21 años o mas dentro del Hogar"

egen nmenor21_ch=sum((relacion_ci>0 & relacion_ci<5) & (edad<21)), by (idh_ch)
label variable nmenor21_ch "Numero de personas menores a 21 años dentro del Hogar"

egen nmayor65_ch=sum((relacion_ci>0 & relacion_ci<5) & (edad>=65)), by (idh_ch)
label variable nmayor65_ch "Numero de personas de 65 años o mas dentro del Hogar"

egen nmenor6_ch=sum((relacion_ci>0 & relacion_ci<5) & (edad<6)), by (idh_ch)
label variable nmenor6_ch "Numero de niños menores a 6 años dentro del Hogar"

egen nmenor1_ch=sum((relacion_ci>0 & relacion_ci<5) & (edad<1)),  by (idh_ch)
label variable nmenor1_ch "Numero de niños menores a 1 año dentro del Hogar"


*** ESTADO CIVIL PARA PERSONAS DE 10 AÑOS O MAS DE EDAD
gen civil_ci=.
label var civil_ci "Estado Civil"
label define civil_ci 1 "Soltero" 2 "Union Formal o Informal" 3 "Divorciado o Separado" 4 "Viudo"
label value civil_ci civil_ci

*** REPORTED HEAD OF HOUSEHOLD
gen jefe_ci=0
replace jefe_ci=1 if relacion_ci==1
label var jefe_ci "Jefe de Hogar Declarado"

/*** We want to know if there is only one head in each hh and if there is a hh with no head:
egen hh=sum(jefe), by (idh_ch)
assert hh==1
*/

*****************************************
***   VARIABLES DEL MERCADO LABORAL   ***
*****************************************

gen byte emp_ci=.
replace emp_ci=1 if cq112==1 | cq113==1 | cq114==1
replace emp_ci=0 if cq112==2 & cq113==2 & cq114==2
label var emp_ci "Empleado en la semana de referencia"

* desemp1
* Individuos que no han trabajado la semana pasada y declaran haber estado buscando trabajo
gen byte desemp1_ci=.
label var desemp1_ci "Personas que no tienen trabajo y han buscado trabajo la semana pasada"

* desemp2
gen byte desemp2_ci=.
label var desemp2_ci "desemp1 + personas que no tienen trabajo ni lo buscaron, pero esperan respuesta de una solicitud de empleo, entrevista"

* desemp3
gen byte desemp3_ci=.
replace desemp3_ci=1 if cq116==1 | (cq116==2 & cq118>=6 & cq118<=9)
replace desemp3_ci=0 if (cq116==2 & cq118>=10 & cq118<=17) | emp_ci==1 | (cq116==2 & cq118>=1 & cq118<=5)
label var desemp3_ci "desemp2 + personas que no tienen trabajo pero han buscado trabajo durante las 4 semanas anteriores a la semana pasada"




* PEA: Poblacion Economicamente Activa
gen byte pea1_ci=.
label var pea1_ci "Poblacion Economicamente Activa utilizando la definicion 'desemp1'"

gen byte pea2_ci=.
label var pea2_ci "Poblacion Economicamente Activa utilizando la definicion 'desemp2'"

gen byte pea3_ci=1 if (emp_ci==1 | desemp3_ci==1)
replace pea3_ci=0 if pea3_ci~=1
label var pea3_ci "Poblacion Economicamente Activa utilizando la definicion 'desemp3'"



* Trabajadores Desalentados
gen byte desalent_ci=.
replace desalent_ci=0 if pea3_ci==1 | cq118<99
replace desalent_ci=1 if cq118==10 | cq118==11 | cq118==13
label var desalent_ci "Trabajadores desalentados, personas que creen que por alguna razon no conseguiran trabajo" 


*** Horas trabajadas en la Actividad Principal
gen horaspri_ci=cq132m if cq132m<99
label var horaspri_ci "Horas totales trabajadas la semana pasada en la Actividad Principal"

gen horastot_ci=cq132t if cq132t<99
label var horastot_ci "Horas totales trabajadas la semana pasada en todas las Actividades"

* Trabajadores Subempleados
gen subemp_ci=.
replace subemp_ci=1 if horastot_ci<=30 & cq138==1
replace subemp_ci=0 if (horastot_ci>30 & horastot_ci<.) | (horastot_ci<=30 & cq138==2)
label var subemp_ci "Trabajadores subempleados"

* Trabajadores a Medio Tiempo
gen byte tiempoparc_ci=.
replace tiempoparc_ci=1 if horastot_ci<=30 & cq138==2
replace tiempoparc_ci=0 if (horastot_ci>30 & horastot_ci<.) | (horastot_ci<=30 & cq138==1)
label var tiempoparc_ci "Trabajadores a medio tiempo"

* Contratos: OJO: ESTA VARIABLE SALE DEL MODULO DE CALIDAD DE VIDA ASI QUE SOLO ESTA PARA UNA PERSONA POR HOGAR
gen contrato_ci=.
replace contrato_ci=1 if q426==1
replace contrato_ci=0 if q426==2
label var contrato_ci "Personas empleadas que han firmado un contrato de trabajo"

* Beneficios (Seguridad Social) OJO: ESTA VARIABLE SALE DEL MODULO DE CALIDAD DE VIDA ASI QUE SOLO ESTA PARA UNA PERSONA POR HOGAR
gen segsoc_ci=.
replace segsoc_ci=1 if q431==1 | q439==1 
replace segsoc_ci=1 if q431==2 | q439==2
label variable segsoc_ci "Personas que cuentan con seguro social"


* Numero de ocupaciones
gen nempleos_ci=.
replace nempleos_ci=1 if cq127==2
replace nempleos_ci=2 if cq127==1
label var nempleos_ci "Numero de empleos"
label define nempleos_ci 1 "un trabajo" 2 "dos o mas trabajos"
label values nempleos_ci nempleos_ci

* Tamaño de la firma
/* valores positivos solo para los patrones en categopri==1 */
gen byte tamfirma_ci=.
replace tamfirma_ci=1 if cq129y==2 | cq129y==3 | cq129y==4
replace tamfirma_ci=0 if cq129y==1
label var tamfirma_ci "Trabajadores formales"
label define tamfirma_ci 1 "Mas de 4 trabajadores" 0 "4 o menos trabajadores"
label values tamfirma_ci tamfirma_ci

* Sector Publico
gen spublico_ci=.
replace spublico_ci=1 if cq128m==3 | cq128m==4
replace spublico_ci=0 if cq128m==1 | cq128m==2 | cq128m==5 | cq128m==6
label var spublico_ci "Personas que trabajan en el sector publico"

************************************************************************************************************
*** VARIABLES DE DEMANDA LABORAL
************************************************************************************************************

* OCUPACION 
capture drop ocupa_ci 
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

label var ocupa_ci "Ocupacion Laboral en la Actividad Principal"
label define ocupa_ci 1 "PROFESIONALES Y TECNICOS" 2 "GERENTES, DIRECTORES Y FUNCIONARIOS SUPERIORES"  3 "PERSONAL ADMINISTRATIVO Y NIVEL INTERMEDIO" 4 "COMERCIANTES Y VENDEDORES" 5 "TRABAJADORES EN SERVICIOS" 6 "TRABAJADORES AGRICOLAS Y AFINES" 7 "OBREROS NO AGRICOLAS, CONDUCTORES DE MAQUINAS Y VEHICULOS DE TRANSPORTE Y SIMILARES" 8 "FUERZAS ARMADAS" 9 "OTRAS OCUPACIONES NO CLASIFICADAS ANTERIORMENTE"
label values ocupa_ci ocupa_ci

* RAMA 
gen byte rama_ci=.
replace rama_ci=1 if cq131misi>=0 & cq131misi<1000
replace rama_ci=2 if cq131misi>=1000 & cq131misi<2000
replace rama_ci=3 if cq131misi>=2000 & cq131misi<4000
replace rama_ci=4 if cq131misi>=4000 & cq131misi<5000
replace rama_ci=5 if cq131misi>=5000 & cq131misi<6000
replace rama_ci=6 if cq131misi>=6000 & cq131misi<7000
replace rama_ci=7 if cq131misi>=7000 & cq131misi<8000
replace rama_ci=8 if cq131misi>=8000 & cq131misi<9000
replace rama_ci=9 if cq131misi>=9000 & cq131misi<10000

label var rama_ci "Rama Laboral en la Ocupacion Principal"
label define rama_ci 1 "Agricultura, caza, silvicultura y pesca" 2 "Explotación de minas y canteras" 3 "Industrias manufactureras" 4 "Electricidad, gas y agua" 5 "Construcción" 6 "Comercio al por mayor y menor, restaurantes, hoteles" 7 "Transporte y almacenamiento" 8 "Establecimientos financieros, seguros, bienes inmuebles" 9 "Servicios sociales, comunales y personales"
label values rama_ci rama_ci


*** DURACION DEL DESEMPLEO
gen durades_ci=.
label var durades_ci "Duracion del Desempleo (en meses)"

gen durades1_ci=cq124 if cq124<9
label var durades1_ci "Duracion del Desempleo (categorias)"
label define durades1_ci 1 "Menos de 1 mes" 2 "1 a 3 meses" 3 "4 a 6 meses" 4 "7 a 12 meses" 5 "mas de 12 meses"
label values durades1_ci durades1_ci

*** Antiguedad, AÑOS (NOT AVAILABLE)
gen antiguedad_ci=.
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



gen byte aedu_ci=.

replace aedu_ci=0 if cq16b==0 | cq16b==1 | cq16b==2
replace aedu_ci=0 if cq16a==0 | cq16a==1 | cq16a==2

replace aedu_ci=1 if cq16b==3 
replace aedu_ci=1 if cq16a==3

replace aedu_ci=2 if cq16b==4 
replace aedu_ci=0 if cq16a==4

replace aedu_ci=3 if cq16b==5 
replace aedu_ci=0 if cq16a==5

replace aedu_ci=4 if cq16b==6 
replace aedu_ci=0 if cq16a==6

replace aedu_ci=5 if cq16b==7
replace aedu_ci=5 if cq16a==7 

replace aedu_ci=6 if cq16b==8 
replace aedu_ci=6 if cq16a==8 

replace aedu_ci=7 if cq16b==9 
replace aedu_ci=7 if cq16a==9 

replace aedu_ci=8 if cq16b==10
replace aedu_ci=8 if cq16a==10 

replace aedu_ci=9 if cq16b==11 
replace aedu_ci=9 if cq16a==11 

replace aedu_ci=10 if cq16b==12
replace aedu_ci=10 if cq16a==12 

replace aedu_ci=11 if cq16b==13 
replace aedu_ci=11 if cq16a==13 

replace aedu_ci=12 if cq16b==14 
replace aedu_ci=12 if cq16a==14 

replace aedu_ci=13 if cq16b==15 
replace aedu_ci=13 if cq16a==15 

replace aedu_ci=14 if cq16b==16 
replace aedu_ci=14 if cq16a==16 

replace aedu_ci=17 if cq16b==17 

label variable aedu_ci "Años de Educacion"




**************
***eduno_ci***
**************

gen byte eduno_ci=(aedu_ci==0) 
replace eduno_ci=. if aedu_ci==.
label variable eduno_ci "Cero anios de educacion"

**************
***edupi_ci***
**************

gen byte edupi_ci=(aedu_ci>=1 & aedu_ci<6)
replace edupi_ci=. if aedu_ci==.
label variable edupi_ci "Primaria incompleta"

**************
***edupc_ci***
**************

gen byte edupc_ci=(aedu_ci==6)
replace edupc_ci=. if aedu_ci==.
label variable edupc_ci "Primaria completa"

**************
***edusi_ci***
**************

gen byte edusi_ci=(aedu_ci>6 & aedu_ci<12)
replace edusi_ci=. if aedu_ci==.
label variable edusi_ci "Secundaria incompleta"

**************
***edusc_ci***
**************


gen byte edusc_ci=(aedu_ci==12)
replace edusc_ci=. if aedu_ci==.
label variable edusc_ci "Secundaria completa"

***************
***edus1i_ci***
***************

gen byte edus1i_ci=(aedu_ci>6 & aedu_ci<9)
replace edus1i_ci=. if aedu_ci==.
label variable edus1i_ci "1er ciclo de la secundaria incompleto"

***************
***edus1c_ci***
***************

gen byte edus1c_ci=(aedu_ci==9)
replace edus1c_ci=. if aedu_ci==.
label variable edus1c_ci "1er ciclo de la secundaria completo"

***************
***edus2i_ci***
***************

gen byte edus2i_ci=(aedu_ci>9 & aedu_ci<12)
replace edus2i_ci=. if aedu_ci==.
label variable edus2i_ci "2do ciclo de la secundaria incompleto"

***************
***edus2c_ci***
***************

gen byte edus2c_ci=(aedu_ci==12)
replace edus2c_ci=. if aedu_ci==.
label variable edus2c_ci "2do ciclo de la secundaria completo"

**************
***eduui_ci***
**************

gen byte eduui_ci=(aedu_ci>12 & aedu_ci<16)
replace eduui_ci=. if aedu_ci==.
label variable eduui_ci "Universitaria incompleta"

***************
***eduuc_ci***
***************

gen byte eduuc_ci=(aedu_ci>=16) 
replace eduuc_ci=. if aedu_ci==.
label variable eduuc_ci "Universitaria completa o mas"



gen eduac_ci=.
label var eduac_ci "Educacion terciaria académica versus educación terciaria no-académica "

gen repite_ci=.
label var repite_ci "=1 si repite o repitio algun grado o año"

gen repiteult_ci=.
label var repiteult_ci "=1 si repite el grado o año que cursa actualmente"

* ASISTE
gen asiste_ci=.
replace asiste_ci=1 if cq16==1 | cq16==2
replace asiste_ci=0 if cq16==3
label var asiste_ci "Personas que actualmente asisten a centros de enseñanza"

* POR QUE NO ASISTE 
gen pqnoasis_ci=.
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
gen ylmpri2_ci=cq142
replace ylmpri2_ci=. if cq142==0 | cq142==26 | cq142==99
label var ylmpri2_ci "Ingreso Laboral Monetario de la Actividad Principal"

****** INGRESO LABORAL NO MONETARIO ACTIVIDAD PRINCIPAL ******
gen ylnmpri_ci=.
label var ylnmpri_ci " Ingreso NO monetario Laboral ocupacion principal"

/* CATEGORIA OCUPACIONAL ACTIVIDAD PRINCIPAL
INDEPENDIENTES
ASALARIADOS
TRAB SIN PAGO */

gen categopri_ci=.
replace categopri_ci=1 if cq128m==1
replace categopri_ci=2 if cq128m==2
replace categopri_ci=3 if cq128m==3 | cq128m==4 | cq128m==5
replace categopri_ci=4 if cq128m==6
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
replace categosec_ci=1 if cq128o==1
replace categosec_ci=2 if cq128o==2
replace categosec_ci=3 if cq128o==3 | cq128o==4 | cq128o==5
replace categosec_ci=4 if cq128o==6
label var categosec_ci "CATEGORIA OCUPACIONAL ACTIVIDAD SECUNDARIA"
label define categosec_ci 1 "Patron" 2 "Cuenta Propia" 3 "Asalariado" 4 "Trabajador No Remunerado" 
label values categosec_ci categosec_ci

****** INGRESOS MONETARIOS ******

****** INGRESO MONETARIO LABORAL ACTIVIDAD SECUNDARIA ******
gen ylmsec_ci=.

****** INGRESO NO MONETARIO LABORAL ACTIVIDAD SECUNDARIA ******
gen ylnmsec_ci=.

********************************************************************
*** INGRESO MONETARIO TOTAL ( OCUP PRINCIPAL + OCUP SECUNDARIA ) ***
********************************************************************
gen ylm_ci=.
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

/*
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

*/

*** INGRESO HORARIO DE TODOS LOS TRABAJOS ***
/* This is not accurate in the sense that if you have more than one job
you will have wage averaged over several jobs */
gen ylmho_ci=.
label var ylmho_ci "Salario Horario Monetario de todas las Actividades"

*** INGRESO HORARIO DE LA OCUPACION PRINCIPAL ***
*gen ylmhopri_ci=ylmpri2_ci/(horaspri_ci*4.3) 
*label var ylmhopri_ci "Salario Horario Monetario de la Actividad Principal"


*** HOUSING VARIABLES ***

gen aguared_ch=.

gen aguadist_ch=.

gen aguamala_ch=.
gen aguamide_ch=.

gen luz_ch=.
replace luz_ch=1 if h4==1 | h4==2 | h4==3
replace luz_ch=0 if h4>3 & h4<9

gen luzmide_ch=.

gen combust_ch=.
replace combust_ch=1 if h5==1 | h5==4
replace combust_ch=0 if h5==2 | h5==3 | h5==5 | h5==6

gen bano_ch=.

gen banoex_ch=.

gen des1_ch=.

gen des2_ch=.

*OJO: ESTA VARIABLE SALE DEL MODULO DE CALIDAD DE VIDA ASI QUE SOLO ESTA PARA UNA PERSONA POR HOGAR
gen piso1=.
replace piso1=1 if q17==2 | q17==3 | q17==4
replace piso1=0 if q17==1
replace piso1=2 if q17==5
by  district urbrur ctv ednumber hhnumber, sort: egen piso_ch=sum(piso1)



*OJO: ESTA VARIABLE SALE DEL MODULO DE CALIDAD DE VIDA ASI QUE SOLO ESTA PARA UNA PERSONA POR HOGAR
gen pared1=.
replace pared1=1 if q15>=1 & q15<=4
replace pared1=0 if q15>=5 & q15<=8
replace pared1=2 if q15==9
by  district urbrur ctv ednumber hhnumber, sort: egen pared_ch=sum(pared1)


*OJO: ESTA VARIABLE SALE DEL MODULO DE CALIDAD DE VIDA ASI QUE SOLO ESTA PARA UNA PERSONA POR HOGAR
gen techo1=.
replace techo1=1 if q16>=1 & q16<=4 
replace techo1=0 if q16>=5 & q16<=6
replace techo1=2 if q16==7
by  district urbrur ctv ednumber hhnumber, sort: egen techo_ch=sum(techo1)


*OJO: ESTA VARIABLE SALE DEL MODULO DE CALIDAD DE VIDA ASI QUE SOLO ESTA PARA UNA PERSONA POR HOGAR
gen resid1=0 if q29==1 | q29==2
replace resid1=1 if q29==4 | q29==5
replace resid1=2 if q29==3 | q29==6
replace resid1=3 if  q29==7
by  district urbrur ctv ednumber hhnumber, sort: egen resid_ch=sum(resid1)


*OJO: ESTA VARIABLE SALE DEL MODULO DE CALIDAD DE VIDA ASI QUE SOLO ESTA PARA UNA PERSONA POR HOGAR
gen dorm1=.
replace dorm1=q26
by  district urbrur ctv ednumber hhnumber, sort: egen dorm_ch=sum(dorm1)

*OJO: ESTA VARIABLE SALE DEL MODULO DE CALIDAD DE VIDA ASI QUE SOLO ESTA PARA UNA PERSONA POR HOGAR
gen cuartos1=.
replace cuartos1=q25
by  district urbrur ctv ednumber hhnumber, sort: egen cuartos_ch=sum(cuartos1)

*OJO: ESTA VARIABLE SALE DEL MODULO DE CALIDAD DE VIDA ASI QUE SOLO ESTA PARA UNA PERSONA POR HOGAR
gen cocina1=.
replace cocina1=1 if q27==1
replace cocina1=0 if q27==2
by  district urbrur ctv ednumber hhnumber, sort: egen cocina_ch=sum(cocina1)


gen telef_ch=.
replace telef_ch=1 if h61==1 
replace telef_ch=0 if h61==2 


gen refrig_ch=.

gen freez_ch=.

gen auto_ch=.

gen compu_ch=.
replace compu_ch=1 if h62==1 
replace compu_ch=0 if h62==2 

*OJO: ESTA VARIABLE SALE DEL MODULO DE CALIDAD DE VIDA ASI QUE SOLO ESTA PARA UNA PERSONA POR HOGAR
gen internet1=.
replace internet1=1 if q210e==1 
replace internet1=0 if q210e==2 
by  district urbrur ctv ednumber hhnumber, sort: egen internet_ch=sum(internet1)

gen cel_ch=.

*OJO: ESTA VARIABLE SALE DEL MODULO DE CALIDAD DE VIDA ASI QUE SOLO ESTA PARA UNA PERSONA POR HOGAR
gen vivi1=.
replace vivi1=1 if q11==1 | q11==2 | q11==4
replace  vivi1=2 if q11==3
replace vivi1=3 if q11==5 | q11==6 | q11==7
by  district urbrur ctv ednumber hhnumber, sort: egen vivi1_ch=sum(vivi1)


*OJO: ESTA VARIABLE SALE DEL MODULO DE CALIDAD DE VIDA ASI QUE SOLO ESTA PARA UNA PERSONA POR HOGAR
gen vivi2=.
replace vivi2=1 if q11==1 | q11==2 | q11==4 | q11==3
replace vivi2=0 if q11==5 | q11==6 | q11==7
by  district urbrur ctv ednumber hhnumber, sort: egen vivi2_ch=sum(vivi2)


gen viviprop_ch=.
replace viviprop_ch=0 if h1>=3 & h1<=5
replace viviprop_ch=1 if h1==1
replace viviprop_ch=2 if h1==2
replace viviprop_ch=3 if h1==6

*OJO: ESTA VARIABLE SALE DEL MODULO DE CALIDAD DE VIDA ASI QUE SOLO ESTA PARA UNA PERSONA POR HOGAR
gen vivitit=.
replace vivitit=1 if q14==1
replace vivitit=0 if q14==2
by  district urbrur ctv ednumber hhnumber, sort: egen vivitit_ch=sum(vivitit)

gen vivialq_ch=.

gen vivialqimp_ch=.
*/

compress

save "${surveysFolder}\ARM\BLZ\2007\Arm_data\BLZ2007EA_BID.dta", replace


