* (Versi�n Stata 12)
clear
set more off
*________________________________________________________________________________________________________________*

 * Activar si es necesario (dejar desactivado para evitar sobreescribir la base y dejar la posibilidad de 
 * utilizar un loop)
 * Los datos se obtienen de las carpetas que se encuentran en el servidor: \\Sdssrv03\surveys
 * Se tiene acceso al servidor �nicamente al interior del BID.
 * El servidor contiene las bases de datos MECOVI.
 *________________________________________________________________________________________________________________*
 


*global ruta = "\\Sdssrv03\surveys"

local PAIS DOM
local ENCUESTA ENFT
local ANO "1997"
local ronda m4 

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
*local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
   



capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
Pa�s: Republica Dominicana
Encuesta: ENFT
Round: m4
Autores: 
�ltima versi�n: Mayra S�enz
Fecha �ltima modificaci�n: 20 de Septiembre de 2013

							SCL/LMK - IADB
****************************************************************************/
/***************************************************************************
Detalle de procesamientos o modificaciones anteriores:

****************************************************************************/


use `base_in', clear

foreach v of varlist _all {
local lowname = lower("`v'")
rename `v' `lowname'
}

		**********************************
		***VARIABLES DEL IDENTIFICACION***
		**********************************
		
	****************
	* region_BID_c *
	****************
	
gen region_BID_c=1

label var region_BID_c "Regiones BID"
label define region_BID_c 1 "Centroam�rica_(CID)" 2 "Caribe_(CCB)" 3 "Andinos_(CAN)" 4 "Cono_Sur_(CSC)"
label value region_BID_c region_BID_c


***************
***factor_ch***
***************

gen factor_ch= factorex 
label variable factor_ch "Factor de expansion del hogar"


***************
****idh_ch*****
***************

gen idh_ch = id_hogar
label variable idh_ch "ID del hogar"


*************
****idp_ci****
**************

gen idp_ci=id_pers 
label variable idp_ci "ID de la persona en el hogar"

**********
***zona***
**********

gen byte zona_c=0 if zona==1
replace zona_c=1 if zona==0

label variable zona_c "Zona del pais"
label define zona_c 1 "Urbana" 0 "Rural"
label value zona_c zona_c


**********
***raza***
**********

gen raza_ci=.
label var raza_ci "Raza o etnia del individuo"  

*Modificaci�n Mayra S�enz 10/20/2015: modificaciones realizadas en base a metodolog�a enviada por SCL/GDI Maria Olga Pe�a

gen raza_idioma_ci = .
gen id_ind_ci      = .
gen id_afro_ci     = .
************
***region***
************

gen region_c=.
label var region_c "Region"  


************
****pais****
************

gen str3 pais_c="DOM"
label variable pais_c "Pais"

**********
***anio***
**********

gen anio_c=1997
label variable anio_c "Anio de la encuesta"

*********
***mes***
*********

gen byte mes_c=4 
label variable mes_c "Mes de la encuesta"


*****************
***relacion_ci***
*****************

gen relacion_ci=1 if parentco==1
replace relacion_ci=2 if parentco==2
replace relacion_ci=3 if parentco==3 
replace relacion_ci=4 if parentco>=4 & parentco<=15
replace relacion_ci=5 if parentco>=17 & parentco<=22

label variable relacion_ci "Relacion con el jefe del hogar"
label define relacion_ci 1 "Jefe/a" 2 "Esposo/a" 3 "Hijo/a" 4 "Otros parientes" 5 "Otros no parientes"
label value relacion_ci relacion_ci
/*
1: Jefe
2: Esposa (o), compa�era (o)
3: Hijo (a)
4: Padre
5: Madre
6: Nieto (a)
7: Hermano (a)
8: Sobrino (a)
9: T�o (a)
10: Primo (a)
11: Suegro (a)
12: Yerno
13: Nuera
14: Cu�ado (a)
15: Otro pariente
17: Sirviente (a) no remunerado
18: Otro trabajador no remunerado
19: Hu�sped
20: Otro miembro del hogar no emparentado con el jefe
21: Cocinera remunerada
22: Sirviente remunerado
*/
	****************************
	***VARIABLES DEMOGRAFICAS***
	****************************

***************
***factor_ci***
***************

gen factor_ci=factor_ch
label variable factor_ci "Factor de expansion del individuo"

**********
***sexo***
**********

gen sexo_ci=sexo
label var sexo_ci "Sexo del individuo" 
label define sexo_ci 1 "Hombre" 2 "Mujer"
label value sexo_ci sexo_ci

**********
***edad***
**********

gen edad_ci=edad
replace edad_ci = . if edad ==99
label variable edad_ci "Edad del individuo"


*****************
***civil_ci***
*****************

gen civil_ci=.
replace civil_ci=1 if estcivil ==1
replace civil_ci=2 if estcivil ==2 | estcivil ==3 
replace civil_ci=3 if estcivil ==5
replace civil_ci=4 if estcivil ==4

label variable civil_ci "Estado civil"
label define civil_ci 1 "Soltero" 2 "Union formal o informal"
label define civil_ci 3 "Divorciado o separado" 4 "Viudo" , add
label value civil_ci civil_ci

/*
1: Soltero
2: Casado
3: Uni�n Libre
4: Viudo
5: Divorciado
*/
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
*NOTA: No dentro de las relaciones de parentezco no es posible identificar a los empleados dom�sticos
gen nempdom_ch=.
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

****************
****condocup_ci*
****************
/*
ACTIVIDA CONDICI�N DE ACTIVIDAD (Pregunta 31)
1: Ocupado
2: Desocupado
3: Estudiante
4: Ama de casa
5: Rentista, Jubilado, Pensionado
6: Infante
7: Otros (ciego, mendigo, paral�tico)
*/

gen condocup_ci=.
replace condocup_ci=1 if activida==1 & edad_ci>=10
replace condocup_ci=2 if activida==2 & edad_ci>=10
recode condocup_ci .=3 if edad_ci>=10
replace condocup_ci=4 if edad<10
label var condocup_ci "Condicion de ocupaci�n de acuerdo a def de cada pais"
label define condocup_ci 1 "Ocupado" 2 "Desocupado" 3 "Inactivo" 4 "Menor que 10" 
label value condocup_ci condocup_ci


************
***emp_ci***
************

gen byte emp_ci=(condocup_ci==1)

****************
***desemp_ci***
****************

gen desemp_ci=(condocup_ci==2)

*************
***pea_ci***
*************
gen pea_ci=0
replace pea_ci=1 if emp_ci==1 |desemp_ci==1

*****************
***desalent_ci***
*****************
/*
0: No aplicable
1: Cre�a que no pod�a encontrar
2: Esperando una respuesta de solicitud de empleo
3: Le est�n buscando empleo
4: Enfermo
5: Otras razones
6: Sin informaci�n
*/

gen desalent_ci=(pqnobus==1)
replace desalent_ci=. if (pqnobus==0 | pqnobus ==.)
label var desalent_ci "Trabajadores desalentados"
*Utilizo para desalentado la opcion 10 y no la 1 como se ha hecho antes

 ***************
 ***subemp_ci***
 ***************    
 
* En 1997 las horas no se desagregan por trabajo principal o secundario.

gen tothoras=hrstot
replace tothoras=. if hrstot==. 
replace tothoras=. if hrstot>70

/*
*En este a�o no se dispone de la variable que indica si desea trabajar m�s horas. 
* Pero en la variable que indica las razones por las cuales trabaj� menos de 35 horas
* existe la categor�a " 6: Pudo encontrar s�lo un trabajo a tiempo parcial" con lo que se 
* puede inferir que s� hubiera querido trabajar m�s horas.
*/

gen subemp_ci= (tothoras>=1 & tothoras<=30) & emp_ci==1 & rzmen35==6
replace subemp_ci =. if emp_ci ==0

label var subemp_ci "Personas en subempleo por horas"

*****************
***horaspri_ci***
*****************
gen horaspri_ci=tothoras  if emp_ci==1 
label var horaspri_ci "Horas trabajadas semanalmente en el trabajo principal"

*****************
***horastot_ci***
*****************

gen horastot_ci=tothoras  if emp_ci==1 
label var horastot_ci "Horas trabajadas semanalmente en todos los empleos"

*******************
***tiempoparc_ci***
*******************
/*
0: No aplicable
1: Per�odo de poca actividad
2: Deficiencias en el material de trabajo
3: Reparaci�n de planta o maquinaria
4: Nuevo trabajo comenzado durante la semana
6: Pudo encontrar s�lo un trabajo a tiempo parcial
8: Disputa laboral
9: Mal tiempo
10: Enfermedad Propia
12: Muy ocupado con quehaceres dom�sticos
13: No quiso un trabajo tiempo completo
14: El tiempo completo del empleo es por bajo de 35 horas
15: Otra (s) raz�n (es) (especifique)
*/

gen tiempoparc_ci=(horastot_ci>=1 & horastot_ci<=30) &  emp_ci==1 & (rzmen35==13)
replace tiempoparc_ci=. if emp_ci==0
label var tiempoparc_c "Personas que trabajan medio tiempo" 


******************
***categopri_ci***
******************
/*
catego
0: No aplicable
1: Patr�n o empleador
2: Cuenta propia
3: Empleado del Estado (P�blico)
4: Empleados privado
5: Familiar remunerado
6: Familiar no remunerado
8: Empleado dom�stico
*/


gen categopri_ci=.
replace categopri_ci=1 if catego==1
replace categopri_ci=2 if catego==2 
replace categopri_ci=3 if catego==3 | catego==4 | catego==8 | catego==5
replace categopri_ci=4 if catego==6
replace categopri_ci=. if emp_ci==0

label define categopri_ci 1"Patron" 2"Cuenta propia" 0"Otro"
label define categopri_ci 3"Empleado" 4" No remunerado" , add
label value categopri_ci categopri_ci
label variable categopri_ci "Categoria ocupacional"

******************
***categosec_ci***
******************

gen categosec_ci=.
label define categosec_ci 1"Patron" 2"Cuenta propia" 0"Otro" 
label define categosec_ci 3"Empleado" 4"No remunerado" , add
label value categosec_ci categosec_ci
label variable categosec_ci "Categoria ocupacional trabajo secundario"


*****************
***nempleos_ci***
*****************

gen nempleos_ci=.
label var nempleos_ci "N�mero de empleos" 
/*
*****************
***firmapeq_ci***
*****************
/*
0: No aplicable
1: De 1 a 4 personas
2: De 5 a 10 personas
3: De 11 a 19 personas
4: De 20 y m�s personas*/

gen firmapeq_ci=.
replace firmapeq_ci=1 if tamaest==1 
replace firmapeq_ci=0 if tamaest>=2 & tamaest!=.
replace firmapeq_ci=. if emp_ci==0
label var firmapeq_ci "Trabajadores informales"
*/
*****************
***spublico_ci***
*****************

gen spublico_ci=(catego==3)
replace spublico_ci=. if emp_ci==0 
label var spublico_ci "Personas que trabajan en el sector p�blico"


**************
***ocupa_ci***
**************
tostring ocupac, replace
gen digito = "0"
egen x = concat(digito ocupac) if length(ocupac)==2
replace ocupac=x if length(ocupac)==2
gen ocupa=real(substr(ocupac,1,2))
 
gen ocupa_ci=.
replace ocupa_ci=1 if (ocupa>=21 & ocupa<=34)  & emp_ci==1
replace ocupa_ci=2 if (ocupa>=11 & ocupa<=13)  & emp_ci==1
replace ocupa_ci=3 if (ocupa>=41 & ocupa<=42)  & emp_ci==1
replace ocupa_ci=4 if (ocupa==52)              & emp_ci==1
replace ocupa_ci=5 if (ocupa==51)              & emp_ci==1
replace ocupa_ci=6 if (ocupa==61)              & emp_ci==1
replace ocupa_ci=7 if (ocupa>=71 & ocupa<=83)  & emp_ci==1
replace ocupa_ci=8 if (ocupa==1)               & emp_ci==1
replace ocupa_ci=9 if (ocupa>=91 & ocupa<=93)  & emp_ci==1
label define ocupa_ci 1"profesional y tecnico" 2"director o funcionario sup" 3"administrativo y nivel intermedio"
label define ocupa_ci  4 "comerciantes y vendedores" 5 "en servicios" 6 "trabajadores agricolas", add
label define ocupa_ci  7 "obreros no agricolas, conductores de maq y ss de transporte", add
label define ocupa_ci  8 "FFAA" 9 "Otras ", add
label value ocupa_ci ocupa_ci
label variable ocupa_ci "Ocupacion laboral"


*************
***rama_ci***
*************
/*
tostring ramac, replace
egen y = concat(digito ramac) if length(ramac)==2
replace ramac=y if length(ramac)==2
gen rama1=real(substr(ramac,1,2))
tab rama1
*/
*Modificaci�n MGD - ABRIL 2014
gen rama_ci=.
replace rama_ci = 1 if (ramac>=11 & ramac<=50) & emp_ci==1
replace rama_ci = 2 if (ramac>=101 & ramac<=142) & emp_ci==1
replace rama_ci = 3 if (ramac>=151 & ramac<=372) & emp_ci==1
replace rama_ci = 4 if (ramac>=401 & ramac<=410) & emp_ci==1
replace rama_ci = 5 if (ramac>=451 & ramac<=455) & emp_ci==1
replace rama_ci = 6 if (ramac>=501 & ramac<=552) & emp_ci==1
replace rama_ci = 7 if (ramac>=601 & ramac<=642) & emp_ci==1
replace rama_ci = 8 if (ramac>=651 & ramac<=702) & emp_ci==1
replace rama_ci = 9 if (ramac>=711 & ramac<=990) & emp_ci==1

label var rama_ci "Rama de actividad"
label def rama_ci 1"Agricultura, caza, silvicultura y pesca" 2"Explotaci�n de minas y canteras" 3"Industrias manufactureras"
label def rama_ci 4"Electricidad, gas y agua" 5"Construcci�n" 6"Comercio, restaurantes y hoteles" 7"Transporte y almacenamiento", add
label def rama_ci 8"Establecimientos financieros, seguros e inmuebles" 9"Servicios sociales y comunales", add
label val rama_ci rama_ci
*drop digito x y ocupa rama1 

****************
***durades_ci***
****************
*Modificaci�n Mayra S�enz - Febrero 2014. Se reemplaza la variable semanas de desocupaci�n por semanas de b�squeda
* de empleo acorde a la definici�n del documento metodol�gico.
gen durades_ci=.
replace durades_ci= (sembusc/4.3) 
replace durades_ci=. if sembusc==99 | sembusc == 0
label variable durades_ci "Duracion del desempleo en meses"

*******************
***antiguedad_ci***
*******************

gen antiguedad_ci= .
label var antiguedad_ci "Antiguedad en la actividad actual en anios"


	
/************************************************************************************************************
* 3. Creaci�n de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/

*********
*lp_ci***
*********

gen lp_ci =.
label var lp_ci "Linea de pobreza oficial del pais"

*********
*lpe_ci***
*********

gen lpe_ci =.
label var lpe_ci "Linea de indigencia oficial del pais"



****************
*cotizando_ci***
****************
gen cotizando_ci=.
label var cotizando_ci "Cotizante a la Seguridad Social"

****************
*afiliado_ci****
****************
gen afiliado_ci=.	

label var afiliado_ci "Afiliado a la Seguridad Social"

****************
*tipopen_ci*****
****************
gen tipopen_ci=.
label var tipopen_ci "Tipo de pension - variable original de cada pais" 

********************
*** instcot_ci *****
********************
gen instcot_ci=.
label var instcot_ci "instituci�n a la cual cotiza"

****************
*instpen_ci*****
****************
gen instpen_ci=.
label var instpen_ci "Institucion proveedora de la pension - variable original de cada pais" 


*****************
*tipocontrato_ci*
*****************

gen tipocontrato_ci=. 
label var tipocontrato_ci "Tipo de contrato segun su duracion"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci

**********
*busca_ci* 
**********
* La pregunta cesante s�lo responden los que s� estuvieron buscando trabajo la semana anterior.
* La pregunta inacdese s�lo responden los que no estuvieron buscando trabajo la semana anterior.
gen busca_ci=0 if inacdese==1|inacdese==2
replace busca_ci=1 if cesante==1|cesante==2
label var busca_ci "Busco trabajo la semana anterior"
label define busca_ci 0 "No" 1 "Si"
label value busca_ci busca_ci


*************
*cesante_ci* 
*************

gen cesante_ci=1 if cesante==2
replace cesante_ci=0 if cesante==1
label var cesante_ci "Desocupado - definicion oficial del pais"	

*************
*tamemp_ci***
*************
/*
0: No aplicable
1: De 1 a 4 personas
2: De 5 a 10 personas
3: De 11 a 19 personas
4: De 20 y m�s personas*/
gen tamemp_ci=1 if tamaest==1
replace tamemp_ci=2 if tamaest==2 | tamaest==3
replace tamemp_ci=3 if tamaest==4
label var tamemp_ci "# empleados en la empresa segun rangos"
label define tamemp_ci 1 "Pequena" 2 "Mediana" 3 "Grande"
label value tamemp_ci tamemp_ci

*************
**pension_ci*
*************

gen pension_ci=1 if ingpoj!=0 & ingpoj!=.
recode pension_ci .=0 
label var pension_ci "1=Recibe pension contributiva"

*************
*ypen_ci*
*************

gen ypen_ci=ingpojm
recode ypen_ci 0=.
label var ypen_ci "Valor de la pension contributiva mensual"
note:  ypen_ci en el formulario se establece que 0 es cuando no aplica.

***************
*pensionsub_ci*
***************

gen pensionsub_ci=.
label var pensionsub_ci "1=recibe pension subsidiada / no contributiva"

*****************
**ypensub_ci*
*****************

gen ypensub_ci=.
label var ypensub_ci "Valor de la pension subsidiada / no contributiva"

**********
**tc_ci***
**********
gen tc_ci=14.02

label var tc_ci "Tipo de cambio LCU/USD"

*************
**salmm_ci***
*************

* DOM 1997
gen salmm_ci= 1555

label var salmm_ci "Salario minimo legal"


*******************
***categoinac_ci*** 
*******************
gen categoinac_ci =1 if ((activida==5) & condocup_ci==3)
replace categoinac_ci = 2 if  (activida==3 & condocup_ci==3)
replace categoinac_ci = 3 if  (activida==4 & condocup_ci==3)
replace categoinac_ci = 4 if  ((categoinac_ci ~=1 & categoinac_ci ~=2 & categoinac_ci ~=3) & condocup_ci==3)
label var categoinac_ci "Categor�a de inactividad"
label define categoinac_ci 1 "jubilados o pensionados" 2 "Estudiantes" 3 "Quehaceres dom�sticos" 4 "Otros" 

*******************
***formal***
*******************
/*
gen formal=1 if cotizando_ci==1

replace formal=1 if afiliado_ci==1 & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1 & pais_c=="BOL"   /* si se usa afiliado, se restringiendo a ocupados solamente*/
replace formal=1 if afiliado_ci==1 & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1 & pais_c=="CRI"
replace formal=1 if afiliado_ci==1 & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1 & pais_c=="GTM" & anio_c>1998
replace formal=1 if afiliado_ci==1 & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1 & pais_c=="PAN"
replace formal=1 if afiliado_ci==1 & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1 & pais_c=="PRY" & anio_c<=2006
replace formal=1 if afiliado_ci==1 & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1 & pais_c=="DOM"
replace formal=1 if afiliado_ci==1 & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1 & pais_c=="MEX" & anio_c>=2008
*/

*No se puede generar ya que no existe la variable cotizando.
gen  formal_ci=.
*replace formal_ci=1 if formal==1 & (condocup_ci==1 | condocup_ci==2)
*replace formal_ci=0 if formal_ci==. & (condocup_ci==1 | condocup_ci==2) 
label var formal_ci "1=afiliado o cotizante / PEA"

************************************************************************
**************************INGRESOS**************************************
************************************************************************
recode  ingmen  ingpojm ingcapm ingfamm ingotrm ingespm ingpojme ingfamme ingotrme (0=.)
gen ymensual=ingmen 	 
gen pension= ingpojm
gen intereses= ingcapm 	
gen remesasnales=ingfamm  
gen otrosing=ingotrm			


/*
*Para ingresos se toman las variables calculadas por el Banco Central de Rep�blica Dominicana.
ingmen Ingreso mensual 
ingpojm ingreso mensual por concepto de pensiones y jubilaciones
ingcapm ingreso mensual por concepto de intereses, alquiler u otras rentas de la propiedad
ingfamm ingreso mensual de familiares del interior
ingespm ingreso mensual por pago en especie
ingotrm ingreso mensual de otros ingresos

ingpojme ingreso mensual por concepto de pensiones y jubilacion del exterior.
ingfamme Ingreso mensual de familiares del exterior.
ingotrme ingreso mensual recibido del exterior por concepto de otros ingresos
*/



*M�dulo de ingresos del exterior
********************************
*Modulo Ingresos del Exterior
gen pension_int=ingpojme	 



*Variable semestral
gen otros_int=ingotrme	



***************
***ylmpri_ci***
***************

egen ylmpri_ci=rsum(ymensual), missing
replace ylmpri_ci=. if ymensual==. 
replace ylmpri_ci=. if emp_ci==0
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
gen ylnmpri_ci=.
label var ylnmpri_ci "Ingreso laboral NO monetario actividad principal"   


***************
***ylmsec_ci***
***************

gen ylmsec_ci=.
label var ylmsec_ci "Ingreso laboral monetario segunda actividad" 


****************
***ylnmsec_ci***
****************

gen ylnmsec_ci=.
label var ylnmsec_ci "Ingreso laboral NO monetario actividad secundaria"


*****************
***ylmotros_ci***
*****************

gen ylmotros_ci=.
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

*****************************************************************
*identificador de top-code del ingreso de la actividad principal*
*****************************************************************

gen tcylmpri_ci=.

*************
***ylnm_ci***
*************

egen ylnm_ci=rsum(ylnmpri_ci ylnmsec_ci), missing
replace ylnm_ci=. if ylnmpri_ci==. &  ylnmsec_ci==.
label var ylnm_ci "Ingreso laboral NO monetario total"  


*************
***ynlm_ci***
*************
destring pension intereses remesasnales otrosing pension_int otros_int ingfamme, replace
egen ynlm_ci=rsum(pension intereses remesasnales otrosing pension_int otros_int ingfamme), missing
replace ynlm_ci=. if pension ==. & intereses==. & remesasnales==. & otrosing==. & pension_int==. & otros_int==. & ingfamme==. 
label var ynlm_ci "Ingreso no laboral monetario"  


**************
***ynlnm_ci***
**************

gen ynlnm_ci=ingespm
replace ynlnm_ci=. if ingespm==.
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
label var nrylmpri_ch "Hogares con alg�n miembro que no respondi� por ingresos"


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
/*Modificaci�n Mayra S�enz-Febrero 2014
gen ynlnm_ch=.
label var ynlnm_ch "Ingreso no laboral no monetario del hogar"
*/
by idh_ch, sort: egen ynlnm_ch=sum(ynlnm_ci) if miembros_ci==1, missing
label var ynlnm_ch "Ingreso no laboral no monetario del hogar"


********
***NA***
********
gen rentaimp_ch=.
label var rentaimp_ch "Rentas imputadas del hogar"

gen autocons_ci=.
label var autocons_ci "Autoconsumo reportado por el individuo"

gen autocons_ch=.
label var autocons_ch "Autoconsumo reportado por el hogar"



****************
***remesas_ci***
****************

*Aqui se toma el valor mensual de las remesas

gen remesas_ci=ingfamme
label var remesas_ci "Remesas mensuales reportadas por el individuo" 

****************
***remesas_ch***
****************

*Aqui se toma el valor mensual de las remesas

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
*Nivel
1: Preprimario
2: Primario
3: Secundario
4: Vocacional
5: Universitario
6: Post Universitario
7: Ninguno

*Anoaprob
ANOAPROB �LTIMO A�O APROBADO (Pregunta 24)
1: Primer grado
2: Segundo grado
3: Tercer grado
4: Cuarto grado
5: Quinto grado
6: Sexto grado
7: S�ptimo grado
8: Octavo grado
9: Ning�n grado

*/
*************
***aedu_ci*** 
*************

gen	 aedu_ci=0  if ((nivel==1) | (nivel==7))|anoaprob==9

*Primaria
replace aedu_ci=1  if (nivel==2 & anoaprob==1)
replace aedu_ci=2  if (nivel==2 & anoaprob==2)
replace aedu_ci=3  if (nivel==2 & anoaprob==3)
replace aedu_ci=4  if (nivel==2 & anoaprob==4)
replace aedu_ci=5  if (nivel==2 & anoaprob==5)
replace aedu_ci=6  if (nivel==2 & anoaprob==6)
replace aedu_ci=7  if (nivel==2 & anoaprob==7)
replace aedu_ci=8  if (nivel==2 & anoaprob==8)

*Secundaria
replace aedu_ci=9  if (nivel==3 & anoaprob==1) 
replace aedu_ci=10 if (nivel==3 & anoaprob==2) 
replace aedu_ci=11 if (nivel==3 & anoaprob==3) 
replace aedu_ci=12 if (nivel==3 & anoaprob==4) 

*T�cnico vocacional
replace aedu_ci=13 if (nivel==4 & anoaprob==1)
replace aedu_ci=14 if (nivel==4 & anoaprob==2)
replace aedu_ci=15 if (nivel==4 & anoaprob==3)
replace aedu_ci=16 if (nivel==4 & anoaprob==4)

*Universitario
replace aedu_ci=13 if (nivel==5 & anoaprob==1)
replace aedu_ci=14 if (nivel==5 & anoaprob==2)
replace aedu_ci=15 if (nivel==5 & anoaprob==3)
replace aedu_ci=16 if (nivel==5 & anoaprob==4)
replace aedu_ci=17 if (nivel==5 & (anoaprob==5 | anoaprob==6))

*Posgrado
replace aedu_ci=18 if nivel==6 & anoaprob==1 
replace aedu_ci=19 if nivel==6 & anoaprob==2 
replace aedu_ci=.  if nivel==.

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
replace edupi_ci=1 if (aedu_ci>=1 & aedu_ci<8) 
label variable edupi_ci "Primaria incompleta"


**************
***edupc_ci***
**************

gen byte edupc_ci=0
replace edupc_ci=1 if aedu_ci==8 
label variable edupc_ci "Primaria completa"


**************
***edusi_ci***
**************

gen byte edusi_ci=0
replace edusi_ci=1 if (aedu_ci>=9 & aedu_ci<12) 
label variable edusi_ci "Secundaria incompleta"


**************
***edusc_ci***
**************

gen byte edusc_ci=0
replace edusc_ci=1 if aedu_ci==12 & nivel==3 
label variable edusc_ci "Secundaria completa"


* Mayra S�enz - Septiembre 2013. Seg�n el documento metodol�gico, para Rep�blica Dominicana
* se reconoce como educaci�n superior completa cuando alcanzan 16 o 17 a�os de educaci�n formal.

/*

*************
**eduui_ci***
*************

drop eduui_ci
gen byte eduui_ci=0 if edad_ci>=4
replace eduui_ci=1 if aedu_ci>=13 & aedu_ci<=16 & nivel==5 
replace eduui_ci=1 if aedu_ci>=13 & aedu_ci<=14 & nivel==4 
label var eduui_ci "universitaria incompleta"

*************
**eduuc_ci***
*************

drop eduuc_ci
gen byte eduuc_ci=0 if edad_ci>=4
replace eduuc_ci=1 if aedu_ci==17 & nivel==5 
replace eduuc_ci=1 if aedu_ci==15 & nivel==4 
label var eduuc_ci "universitaria completa"

*/


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
replace eduuc_ci=1 if aedu_ci>=16
replace eduuc_ci=. if aedu_ci==.
label variable eduuc_ci "Universitaria incompleta o mas"


***************
***edus1i_ci***
***************

gen byte edus1i_ci=.
label variable edus1i_ci "1er ciclo de la secundaria incompleto"

***************
***edus1c_ci***
***************

gen byte edus1c_ci=.
label variable edus1c_ci "1er ciclo de la secundaria completo"

***************
***edus2i_ci***
***************

gen byte edus2i_ci=.
label variable edus2i_ci "2do ciclo de la secundaria incompleto"

***************
***edus2c_ci***
***************

gen byte edus2c_ci=.
label variable edus2c_ci "2do ciclo de la secundaria completo"

local var = "eduno edupi edupc edusi edusc edusc eduui eduuc edus1i edus1c edus2i edus2c"
foreach x of local var {
replace `x'_ci=. if aedu_ci==.
}

***************
***edupre_ci***
***************

gen byte edupre_ci= (nivel==1)
label variable edupre_ci "Educacion preescolar"


**************
***eduac_ci***
**************
gen byte eduac_ci=1 if nivel==5 
replace eduac_ci=0 if nivel==4
label variable eduac_ci "Superior universitario vs superior no universitario"


***************
***asiste_ci***
***************
generat asiste_ci=1 if asiste!=7 
replace asiste_ci=0 if asiste==7 | nivelmat==7
replace asiste_ci=. if asiste==. & nivelmat==.
label variable asiste_ci "Asiste actualmente a la escuela"


**************
***pqnoasis***
**************

gen pqnoasis_ci=pqnoasis 
label var pqnoasis_ci "Razones para no asistir a la escuela"
label def pqnoasis_ci 0  "No aplicable  " ///
1  "Por edad  " ///
2  "Muy caro  " ///
3  "Necesita trabajo  " ///
4  "Muy lejos  " ///
5  "Por enfermedad  " ///
6  "Le fue mal  " ///
7  "Por vacaciones  " ///
8  "No quiere  " ///
9  "Ya tiene suficiente educaci�n  " ///
10  " Por huelga  " ///
11  " Otros  " 

label val pqnoasis_ci pqnoasis_ci

**************
*pqnoasis1_ci*
**************
**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**
g       pqnoasis1_ci = 1 if pqnoasis ==2
replace pqnoasis1_ci = 2 if pqnoasis ==3
replace pqnoasis1_ci = 3 if pqnoasis ==5 
replace pqnoasis1_ci = 4 if pqnoasis ==8
replace pqnoasis1_ci = 6 if pqnoasis ==9
replace pqnoasis1_ci = 7 if pqnoasis ==1 
replace pqnoasis1_ci = 8 if pqnoasis ==4
replace pqnoasis1_ci = 9 if pqnoasis ==6  | pqnoasis ==7 | pqnoasis ==10 | pqnoasis ==11

label define pqnoasis1_ci 1 "Problemas econ�micos" 2 "Por trabajo" 3 "Problemas familiares o de salud" 4 "Falta de inter�s" 5	"Quehaceres dom�sticos/embarazo/cuidado de ni�os/as" 6 "Termin� sus estudios" 7	"Edad" 8 "Problemas de acceso"  9 "Otros"
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
label var repiteult "Ha repetido el �ltimo grado"


***************
***edupub_ci***
***************

gen edupub_ci=0
replace edupub_ci=1 if centro==1
replace edupub_ci=. if centro==. | centro==0  
label var edupub_ci "Asiste a un centro de ense�anza p�blico"


*************
**tecnica_ci*
*************


gen tecnica_ci=.
replace tecnica_ci=1 if nivel==4 
replace tecnica_ci=0 if nivel==5 
label var tecnica_ci "1=formacion terciaria tecnica"


**********************************
**** VARIABLES DE LA VIVIENDA ****
**********************************

/* En el documento de la CEPAL consta lo siguiente:
"Este m�dulo no est� incluido en el archivo, dado que la informaci�n correspondiente no fue suministrada por el pa�s. 
 S�lo se dispone de las variables TIPOVIV, TENENCIA, TOTCUAR y ALQMES"
La variable ALQMES no est�.
*/

****************
***aguared_ch***
****************

gen aguared_ch=.
label var aguared_ch "Acceso a fuente de agua por red"


*****************
***aguadist_ch***
*****************

gen aguadist_ch=.
label var aguadist_ch "Ubicaci�n de la principal fuente de agua"
label def aguadist_ch 1"Dentro de la vivienda" 2"Fuera de la vivienda pero en el terreno"
label def aguadist_ch 3"Fuera de la vivienda y del terreno", add
label val aguadist_ch aguadist_ch


*****************
***aguamala_ch***
*****************

gen aguamala_ch=.
label var aguamala_ch "Agua unimproved seg�n MDG" 


*****************
***aguamide_ch***
*****************

gen aguamide_ch=.
label var aguamide_ch "Usan medidor para pagar consumo de agua"


************
***luz_ch***
************

gen luz_ch=.
label var luz_ch  "La principal fuente de iluminaci�n es electricidad"


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


*************
***bano_ch***
*************

gen bano_ch=.
label var bano_ch "El hogar tiene servicio sanitario"


***************
***banoex_ch***
***************

gen banoex_ch=.
label var banoex_ch "El servicio sanitario es exclusivo del hogar"


*************
***des1_ch***
*************

gen des1_ch=.
label var des1_ch "Tipo de desague seg�n unimproved de MDG"
label def des1_ch 0"No tiene servicio sanitario" 1"Conectado a red general o c�mara s�ptica"
label def des1_ch 2"Letrina o conectado a pozo ciego" 3"Desemboca en r�o o calle", add
label val des1_ch des1_ch


*************
***des2_ch***
*************

gen des2_ch=.
label var des2_ch "Tipo de desague sin incluir definici�n MDG"
label def des2_ch 0"No tiene servicio sanitario" 1"Conectado a red general, c�mara s�ptica, pozo o letrina"
label def des2_ch 2"Cualquier otro caso", add
label val des2_ch des2_ch

*************
***piso_ch***
*************

gen piso_ch=.
label var piso_ch "Materiales de construcci�n del piso"  
label def piso_ch 0"Piso de tierra" 1"Materiales permanentes" 2"otros materiales"
label val piso_ch piso_ch

**************
***pared_ch***
**************

gen pared_ch=. 
label var pared_ch "Materiales de construcci�n de las paredes"
label def pared_ch 0"No permanentes" 1"Permanentes" 2"otros materiales"
label val pared_ch pared_ch

**************
***techo_ch***
**************

gen techo_ch=.
label var techo_ch "Materiales de construcci�n del techo"
label def techo_ch 0"No permanentes" 1"Permanentes" 2"otros materiales"
label val techo_ch techo_ch


**************
***resid_ch***
**************

gen resid_ch =.   
label var resid_ch "M�todo de eliminaci�n de residuos"
label def resid_ch 0"Recolecci�n p�blica o privada" 1"Quemados o enterrados"
label def resid_ch 2"Tirados a un espacio abierto" 3"Otros", add
label val resid_ch resid_ch

**Daniela Zuluaga- Enero 2018: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**
	
 *********************
 ***aguamejorada_ch***
 *********************
g       aguamejorada_ch = .
		
 *********************
 ***banomejorado_ch***
 *********************
g       banomejorado_ch = . 

*************
***dorm_ch***
*************
*Hay hogares que reportan no tener cuartos exclusivamente para dormir y cuentan con un solo espacio. Para estas 
*observaciones se cambia el 0 que tienen por 1. Porque aunque no sea exclusivo tienen un espacio para dormitorio

gen dorm_ch=.
label var dorm_ch "Habitaciones para dormir"


****************
***cuartos_ch***
****************

gen cuartos_ch=totcuar
replace cuartos_ch=. if totcuar==0
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
label var telef_ch "El hogar tiene servicio telef�nico fijo"


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
label var internet_ch "El hogar posee conexi�n a Internet"


************
***cel_ch***
************

gen cel_ch=.
label var cel_ch "El hogar tiene servicio telefonico celular"



**************
***vivi1_ch***
**************
*tipoviv
/*
1: Separada
2: Vecindad
3: Apartamento
4: Otro tipo
*/


gen vivi1_ch=1 if tipoviv==1 | tipoviv==2 
replace vivi1_ch=2 if tipoviv==3
replace vivi1_ch=3 if tipoviv==4 
label var vivi1_ch "Tipo de vivienda en la que reside el hogar"
label def vivi1_ch 1"Casa" 2"Departamento" 3"Otros"
label val vivi1_ch vivi1_ch


*************
***vivi2_ch***
*************

gen vivi2_ch=0
replace vivi2_ch=1 if vivi1_ch==1 | vivi1_ch==2
replace vivi2_ch=. if vivi1_ch==.
label var vivi2_ch "La vivienda es casa o departamento"

*****************
***viviprop_ch***
*****************

/*
TENENCIA TENENCIA DE LA VIVIENDA (Pregunta 11)
0: Sin informaci�n
11: Comprada al contado
12: Comprada a plazo al gobierno
13: Comprada a plazo a particular
14: Construida
15: Heredada
16: Donada por el gobierno
17: Donada por familiares
20: Alquilada
30: Cedida
40: Otras
*/

gen viviprop_ch=0 if tenencia==20
replace viviprop_ch=1 if tenencia==11 | tenencia==14
replace viviprop_ch=2 if tenencia==12 | tenencia==13
replace viviprop_ch=3 if tenencia==15 | tenencia==16 | tenencia==17 | tenencia==30
replace viviprop_ch=. if tenencia==40
label var viviprop_ch "Propiedad de la vivienda"
label def viviprop_ch 0"Alquilada" 1"Propia y totalmente pagada" 2 "Propia y en proceso de pago"
label def viviprop_ch 3"Ocupada (propia de facto)", add
label val viviprop_ch viviprop_ch

****************
***vivitit_ch***
****************

gen vivitit_ch=.
label var vivitit_ch "El hogar posee un t�tulo de propiedad"


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

ren ocup ocup_old



/*_____________________________________________________________________________________________________*/
* Asignaci�n de etiquetas e inserci�n de variables externas: tipo de cambio, Indice de Precios al 
* Consumidor (2011=100), Paridad de Poder Adquisitivo (PPA 2011),  l�neas de pobreza
/*_____________________________________________________________________________________________________*/


do "$ruta\harmonized\_DOCS\\Labels&ExternalVars_Harmonized_DataBank.do"

/*_____________________________________________________________________________________________________*/
* Verificaci�n de que se encuentren todas las variables armonizadas 
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



compress


saveold "`base_out'", replace


log close





	

	




 
 
