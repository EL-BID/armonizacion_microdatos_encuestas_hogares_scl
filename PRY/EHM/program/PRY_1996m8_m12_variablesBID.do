
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

local PAIS PRY
local ENCUESTA EHM
local ANO "1996"
local ronda m8_m12

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\\`PAIS'_`ANO'`ronda'.dta"
*local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
                                                    
capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Paraguay
Encuesta: EIH
Round: Agosto 1997 - Julio 1998
Autores:
Versión 2013: Mayra Sáenz
Última versión: Mayra Sáenz - Email: mayras@iadb.org, saenzmayra.a@gmail.com
Fecha última modificación: 4 de Septiembre de 2013

							SCL/LMK - IADB
****************************************************************************/
/***************************************************************************
Detalle de procesamientos o modificaciones anteriores:

****************************************************************************/
use `base_in', clear
/*
DURADES, CODIGO Y COMENTARIOS ANTERIORES
gen tdurades_ci=mesesbus
replace tdurades_ci=. if tdurades_ci==0
tiene un comportamiento muy estraño
SON unas duraciones pero no son en numeros sino en intervalos

May 19, 2006 (Analia)
The following lines:
gen aguared_ch=(agua==4 | agua==5)
gen aguamala_ch=(agua<=3)
were changed with
gen aguared_ch=(agua<=2)
gen aguamala_ch=(agua==3 | agua==4 | agua==7 | agua==8)

*/


************
* Region_c *
************
*Inclusión Mayra Sáenz - Julio 2013

gen region_c=  .
label var region_c "División política"

***************
***factor_ch***
***************

gen factor_ch=factorex 
label variable factor_ch "Factor de expansion del hogar"


***************
****idh_ch*****
***************

sort id_hogar
gen idh_ch=id_hogar
label variable idh_ch "ID del hogar"

**************
****idp_ci****
**************

bysort idh_ch:gen idp_ci=_n 
label variable idp_ci "ID de la persona en el hogar"

**********
***zona***
**********
/**YL-> la muestra de rural es muy pequena, en realidad parece ser solo urbana.
gen byte zona_c=1
*/

*Modificación Mayra Sáenz - Septiembre 2014
*Se decide trabajar estrictamente con el área urbana, pues la muestra rural no es representativa.
*Por lo tanto se elimina.
drop if area==6
g zona_c=1
label variable zona_c "Zona del pais"
label define zona_c 1 "Urbana" 0 "Rural"
label value zona_c zona_ci

************
****pais****
************

gen str3 pais_c="PRY"
label variable pais_c "Pais"

**********
***anio***
**********

gen anio_c=1996
label variable anio_c "Anio de la encuesta"

*****************
*** region según BID ***
*****************
gen region_BID_c=.
replace region_BID_c=4 if pais=="PRY" 
label var region_BID_c "Regiones BID"
label define region_BID_c 1 "Centroamérica_(CID)" 2 "Caribe_(CCB)" 3 "Andinos_(CAN)" 4 "Cono_Sur_(CSC)"
label value region_BID_c region_BID_c

***************
****muestra****
***************
*1996 es Urbano (U)

gen muestra_AMA=0
replace muestra_AMA=1 if dominio==1
label variable muestra_AMA "Asuncion Metropolitana"
	 	
gen muestra_N=0
label variable muestra_N "Muestra Nacional"
	 	
gen muestra_U=0
replace muestra_U=1 if zona_c==1
label variable muestra_U "Muestra Urbana"
		
		
*********
***mes***
*********

gen mes_c=.
label variable mes_c "Mes de la encuesta"
label define mes_c 6 "Junio" 7 " Julio" 8 "Agosto" 
label define mes_c 9 "Septiembre" 10 "Octubre" 11 "Noviembre", add
label value mes_c mes_c

*****************
***relacion_ci***
*****************

gen paren=parentco
gen relacion_ci=.
replace relacion_ci=1 if paren==1
replace relacion_ci=2 if paren==2 
replace relacion_ci=3 if paren==3
replace relacion_ci=4 if paren==4
replace relacion_ci=5 if paren==5
replace relacion_ci=6 if paren==6


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

gen factor_ci=factorex 
label variable factor_ci "Factor de expansion del individuo"

**********
***sexo***
**********

gen sexo_ci=.
replace sexo_ci=1 if sexo==1
replace sexo_ci=2 if sexo==6

label define sexo_ci 1 "Hombre" 2 "Mujer"
label value sexo_ci sexo_ci

**********
***edad***
**********

gen edad_ci=edad
label variable edad_ci "Edad del individuo"


*****************
***civil_ci***
*****************


gen civil_ci=.
replace civil_ci=1 if estcivil==5
replace civil_ci=2 if estcivil==1 | estcivil==2 
replace civil_ci=3 if estcivil==3
replace civil_ci=4 if estcivil==4 

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

by idh_ch, sort: egen nmayor21_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad>=21)
label variable nmayor21_ch "Numero de familiares mayores a 21 anios"

*****************
***nmenor21_ch***
*****************

by idh_ch, sort: egen nmenor21_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad<21)
label variable nmenor21_ch "Numero de familiares menores a 21 anios"

*****************
***nmayor65_ch***
*****************

by idh_ch, sort: egen nmayor65_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad>=65)
label variable nmayor65_ch "Numero de familiares mayores a 65 anios"

****************
***nmenor6_ch***
****************

by idh_ch, sort: egen nmenor6_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad<6)
label variable nmenor6_ch "Numero de familiares menores a 6 anios"

****************
***nmenor1_ch***
****************

by idh_ch, sort: egen nmenor1_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad<1)
label variable nmenor1_ch "Numero de familiares menores a 1 anio"

****************
***miembros_ci***
****************

gen miembros_ci=(relacion_ci<5)
label variable miembros_ci "Miembro del hogar"


/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
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
*cotizapri_ci***
****************
gen cotizapri_ci=.
label var cotizapri_ci "Cotizante a la Seguridad Social en actividad ppal."

****************
*cotizasec_ci***
****************
gen cotizasec_ci=.
label var cotizasec_ci "Cotizante a la Seguridad Social en actividad sec."

****************
*afiliado_ci****
****************
* MGD 9/20/2016: se lo deja como missing porque se genera un salto en la serie.
gen afiliado_ci=.	
/*replace afiliado_ci=1 if sistjub==1
replace afiliado_ci=0 if (sistjub==6 | sistjub==.)
replace afiliado_ci = 0 if peagru == 2 */
label var afiliado_ci "Afiliado a la Seguridad Social"

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
gen instcot_ci=.
label var instpen_ci "Institucion a la que cotiza - variable original de cada pais" 

****************
****condocup_ci*
****************
/*
* Comprobacion que no se toma en cuenta el desempleo oculto. 
* Tambien la edad minima de la encuesta es 7 anios. MGD 05/27/2014

gen condocup_ci=.
replace condocup_ci=1 if (trabasal==1 | trainoag==1 | trabag==1)
replace condocup_ci=2 if (trabasal==6 | trainoag==6 | trabag==6) & busco7==1 & trahora==1
recode condocup_ci .=3 if edad_ci>=7
recode condocup_ci .=4 if edad_ci<7

label var condocup_ci "Condicion de ocupación de acuerdo a def de cada pais"
label define condocup_ci 1 "Ocupado" 2 "Desocupado" 3 "Inactivo" 4 "Menor de PET" 
label value condocup_ci condocup_ci
*/

* No coincide cuando se calcula con la variable creada de desempleo oculto al condicionar por busqueda de trabajo;
* sin embargo si coincide con calculos externos SEDLAC. MGD 05/28/2014

gen condocup_ci=.
replace condocup_ci=1 if (peades==1 | peades==4 | peades==5) & edad_ci>=7
replace condocup_ci=2 if ((peades==2 & busco7==1) | peades==7) & edad_ci>=7
recode condocup_ci .=3 if (peades==3 | peades==6) | (condocup_ci==. & edad_ci>=7)
recode condocup_ci (.=4) if edad_ci<7 | peades==0

label var condocup_ci "Condicion de ocupación de acuerdo a def de cada pais"
label define condocup_ci 1 "Ocupado" 2 "Desocupado" 3 "Inactivo" 4 "Menor de PET" 
label value condocup_ci condocup_ci


*************
*cesante_ci* 
*************

gen cesante_ci=1 if hatrab==1 &  peagru==2 & edad_ci>=10
replace cesante_ci=0 if hatrab==6 &  peagru==2 & edad_ci>=10
label var cesante_ci "Desocupado - definicion oficial del pais"	

*************
**pension_ci*
*************

gen pension_ci=1 if (yjubpen>0 & yjubpen<. & yjubpen!=999999999) 
recode pension_ci .=0 
label var pension_ci "1=Recibe pension contributiva"

*************
*ypen_ci*
*************
replace yjubpen=. if (yjubpen >= 999999999 & yjubpen!=.)
gen ypen_ci=yjubpen
replace ypen_ci=. if pension_ci==0 
label var ypen_ci "Valor de la pension contributiva"

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
gen tc_ci=.
label var tc_ci "Tipo de cambio LCU/USD"

*************
**salmm_ci***
*************

* PRY 1996
gen salmm_ci= 469157
label var salmm_ci "Salario minimo legal"

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

gen desalent_ci=.
/*no se puede construir*/

***************
***subemp_ci***
***************
*gen subemp_ci=0
egen tothoras=rsum(hrstralu hrstrama hrstrami hrstraju hrstravi hrstrasa hrstrado hrstras), missing
*replace subemp_ci=1 if tothoras<=30 & deseamas==1 & emp_ci==1 

* Modificacion: considerando horas de la actividad principal solamente. MGD 06/19/2014
egen horastotales=rsum(hrstralu hrstrama hrstrami hrstraju hrstravi hrstrasa hrstrado), missing
gen subemp_ci=0
replace subemp_ci=1 if (horastotales>=1 & horastotales<=30) & (emp_ci==1 & deseamas==1)


*****************
***horaspri_ci***
*****************

egen hr_seman=rsum(hrstralu hrstrama hrstrami hrstraju hrstravi hrstrasa hrstrado), missing
gen hr_sem_s=tothoras
gen horaspri_ci=hr_seman if emp_ci==1 

*****************
***horastot_ci***
*****************

gen horastot_ci=hr_sem_s  if emp_ci==1 

*******************
***tiempoparc_ci***
*******************

/*gen tiempoparc_ci=0
replace tiempoparc_ci=1 if tothoras>=30 & deseamas==6 & emp_ci==1 */

*10/21/15 MGD: corrección de sintaxis
gen tiempoparc_ci=0
replace tiempoparc_ci=1 if horaspri_ci>=1 & horaspri_ci<30 & deseamas==6 & emp_ci==1

******************
***categopri_ci***
******************
/*
0: No aplicable
1: Empleado público 2: Empleado privado 3: Obrero público 4: Obrero privado
5: Empleador o patrón
6: Trabajador por cuenta propia
7: Trabajador familiar no remunerado
8: Empleado doméstico
*/

gen categopri_ci=.
replace categopri_ci=1 if categ==5
replace categopri_ci=2 if categ==6 
replace categopri_ci=3 if categ==1 | categ==2 | categ==3 | categ==4 | categ==8
replace categopri_ci=4 if categ==7
replace categopri_ci=. if emp_ci~=1 |categ==0

label define categopri_ci 1"Patron" 2"Cuenta propia" 
label define categopri_ci 3"Empleado" 4" Familiar no remunerado" , add
label value categopri_ci categopri_ci
label variable categopri_ci "Categoria ocupacional trabajo principal"

******************
***categosec_ci***
******************

gen categosec_ci=.
replace categosec_ci=1 if categs==5
replace categosec_ci=2 if categs==6 
replace categosec_ci=3 if categs==1 | categs==2 | categs==3 | categs==4 | categs==8
replace categosec_ci=4 if categs==7
replace categosec_ci=. if emp_ci~=1 | otrotra7~=1

label define categosec_ci 1"Patron" 2"Cuenta propia" 
label define categosec_ci 3"Empleado" 4"Familiar no remunerado" , add
label value categosec_ci categosec_ci
label variable categosec_ci "Categoria ocupacional trabajo secundario"

*****************
*tipocontrato_ci*
*****************
gen tipocontrato_ci=. 
label var tipocontrato_ci "Tipo de contrato segun su duracion"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci


*****************
***nempleos_ci***
*****************

gen nempleos_ci=.


/*
*****************
***firmapeq_ci***
*****************
/*

TAMESP ¿CUÁNTAS PERSONAS APROXIMADAMENTE TRABAJAN EN EL ESTABLECIMIENTO DONDE TRABAJA?
(Pregunta 11)
0: No aplicable
1: Solo
2: 2 a 5 personas
3: 6 a 10 personas
4: 11 a 20 personas
5: 21 a 50 personas
6: Más de 50 personas
7: No sabe el informante
8: Empleado doméstico
9: No responde

*/

gen byte firmapeq_ci=.
replace firmapeq_ci=1 if tamesp ==1 | tamesp==2
replace firmapeq_ci=0 if tamesp>=3 & tamesp<=6
*/

*****************
***spublico_ci***
*****************
* 10/21/2015 MGD: corrección pequeña para que se tomen en cuenta a todos los ocupados.
gen spublico_ci=0 if emp_ci==1
replace spublico_ci=1 if (categ==1 | categ==3) & emp_ci==1

/*gen spublico_ci=.
replace spublico_ci=1 if categ ==1 | categ==3 & emp_ci==1
replace spublico_ci=0 if categ ==2 | categ>=4  & emp_ci==1
replace spublico_ci=. if emp_ci==.*/

**************
***ocupa_ci***
**************
/*
recode ocupr (0=.) (5=6) (6/8=7)(9=5)(10=9), gen (ocupa_ci)
replace ocupa_ci=. if emp_ci!=1
*/
*Modificacion MGD 07/22/2014: se utiliza la varibale desagregada para clasificar la ocupacion del trabajo principal
gen ocupa_ci=.
replace ocupa_ci=1 if (ocuprin>=1  & ocuprin<=676) & emp_ci==1
replace ocupa_ci=2 if (ocuprin>=680  & ocuprin<=835) & emp_ci==1
replace ocupa_ci=3 if (ocuprin>=840  & ocuprin<=983) & emp_ci==1
replace ocupa_ci=4 if (ocuprin>=990  & ocuprin<=1103) & emp_ci==1
replace ocupa_ci=5 if (ocuprin>=2120  & ocuprin<=2305) & emp_ci==1
replace ocupa_ci=6 if (ocuprin>=1110  & ocuprin<=1247) & emp_ci==1
replace ocupa_ci=7 if (ocuprin>=1260 & ocuprin<=2116)  & emp_ci==1
replace ocupa_ci=9 if (ocuprin>=2310 & ocuprin<=2352)  & emp_ci==1


*************
***rama_ci***
*************
g rama_ci=.
replace rama_ci=1 if (rama>=110 & rama<=149) & emp_ci==1
replace rama_ci=2 if (rama>=210 & rama<=290) & emp_ci==1
replace rama_ci=3 if (rama>=310 & rama<=394) & emp_ci==1
replace rama_ci=4 if (rama>=410 & rama<=411) & emp_ci==1
replace rama_ci=5 if (rama>=510 & rama<=511) & emp_ci==1
replace rama_ci=6 if (rama>=610 & rama<=614) & emp_ci==1
replace rama_ci=7 if (rama>=710 & rama<=750) & emp_ci==1
replace rama_ci=8 if (rama>=810 & rama<=836) & emp_ci==1
replace rama_ci=9 if (rama>=910 & rama<=999) & emp_ci==1

/*
gen rama1=string(ramap)
gen rama_ci=real(substr(rama1,1,1))  
replace rama_ci=. if rama_ci==0 | emp_ci~=1
drop rama1
*/

****************
***durades_ci***
****************
recode anosbus (9=.)
recode mesesbus (99=.)
recode sembus (9=.)
gen durades_ci=(anosbus*12)+mesesbus+(sembus/4.3)
replace durades_ci=. if durades_ci==0

*******************
***antiguedad_ci***
*******************

/*gen antiguedad_ci=trano if trano>0
replace antiguedad_ci=. if antiguedad_ci==99*/

* Nota MGD 09/22/2014: se incluyo a meses y semanas.
recode trano trmeses (99=.)
gen antiguedad_ci=trano+(trmeses/12)+(trsem/52) if emp_ci==1

replace antiguedad_ci=. if categopri!=3

*******************
***tamemp_ci***
*******************
*Paraguay Pequeña 1 a 5, Mediana 6 a 50, Grande Más de 50

gen tamemp_ci = 1 if tamestp>=1 & tamestp<=2
replace tamemp_ci = 2 if (tamestp>=3 & tamestp<=5)
replace tamemp_ci = 3 if (tamestp==6)
label define tamemp_ci 1 "Pequeña" 2 "Mediana" 3 "Grande"
label value tamemp_ci tamemp_ci
label var tamemp_ci "Tamaño de empresa"


*******************
***categoinac_ci*** 
*******************
gen categoinac_ci =1 if (pqnotra7==16 & condocup_ci==3)
replace categoinac_ci = 2 if  (pqnotra7==1 & condocup_ci==3)
replace categoinac_ci = 3 if  (pqnotra7==14 & condocup_ci==3)
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


*************************************************************************************
*******************************INGRESOS**********************************************
*************************************************************************************

****************
***ylmpri_ci ***
****************

/*Ingresos netos por remuneracion al trabajo*/
gen ylmpri_ci=yprin if emp_ci==1 
replace ylmpri_ci=. if yprin<=0 | yprin==99999999
replace ylmpri_ci=0 if categopri_ci==4


*****************
***nrylmpri_ci***
*****************

gen nrylmpri_ci=(ylmpri_ci==. & emp_ci==1)


*****************
*** ylnmpri_ci***
*****************

gen ylnmpri_ci=.

***************
***ylmsec_ci***
***************

/*existen unas actividades ocacionales-las pongo en secundario*/
gen ysec1=ysec
replace ysec1=. if ysec==0 | ysec==9999999 /*No aplicable*/

gen ylmsec_ci=ysec1 
replace ylmsec_ci=. if emp_ci~=1
replace ylmsec_ci=. if ysec1==. 

drop ysec1 

*************
*ylmotros_ci*
*************

gen yocasio1=yotrasac
replace yocasio1=. if yotrasac==0 | yotrasac==99999999 | yotrasac==9999999 /*No aplicable*/

gen ylmotros_ci= yocasio1
replace ylmotros_ci=. if emp_ci~=1
replace ylmotros_ci=. if yocasio1==.

drop yocasio1

******************
****ylnmsec_ci****
******************
gen ylnmsec_ci=.

************
***ylm_ci***
************

egen ylm_ci=rsum(ylmpri_ci ylmsec_ci), missing
replace ylm_ci=. if ylmpri_ci==. & ylmsec_ci==.

*****************************************************************
*identificador de top-code del ingreso de la actividad principal*
*****************************************************************

gen tcylmpri_ci=.

*************
***ylnm_ci***
*************

gen ylnm_ci=.

*************
***ynlm_ci***
*************

local var="yalqrent yinteres yayudafa yotrosy yjubpen"
foreach x of local var {
gen `x'1=`x'
replace `x'1=. if `x'==0 | `x'==99999999 | `x'==9999999 /*No aplicable*/
}

egen ynlm_ci=rsum(yalqrent1 yinteres1 yayudafa1 yotrosy1 yjubpen1), missing
replace ynlm_ci=. if yalqrent1==. & yinteres1==. & yayudafa1==. & yotrosy1==. & yjubpen1==.

drop yalqrent1 yinteres1 yayudafa1 yotrosy1 yjubpen1

*********************************************
*Ingreso laboral no monetario otros trabajos*
*********************************************

gen ylnmotros_ci=.

*****************
***remesas_ci***
*****************

gen remesas_ci=.
gen ynlnm_ci=.
************************
*** HOUSEHOLD INCOME ***
************************

/*Dado que el ingreso del hogar no tiene en cuenta el ingreso de las empleadas domésticas
voy a crear una flag que me identifique a las mismas como para que en este caso figure un missing
en el ingreso del hogar, las empleadas domésticas en este caso se identifican con un 9 en la variable parentco*/

******************
*** nrylmpri_ch***
******************
*Creating a Flag label for those households where someone has a ylmpri_ci as missing

by idh_ch, sort: egen nrylmpri_ch=sum(nrylmpri_ci) if miembros_ci==1, missing
replace nrylmpri_ch=1 if nrylmpri_ch>0 & nrylmpri_ch<.
replace nrylmpri_ch=. if nrylmpri_ch==.


*************
*** ylm_ch***
*************

by idh_ch, sort: egen ylm_ch=sum(ylm_ci) if miembros_ci==1, missing

**************************************************
*Identificador de los hogares en donde (top code)*
**************************************************

gen tcylmpri_ch=.

****************
*** ylmnr_ch ***
****************

by idh_ch, sort: egen ylmnr_ch=sum(ylm_ci) if miembros_ci==1, missing
replace ylmnr_ch=. if nrylmpri_ch==1

***************
*** ylnm_ch ***
***************

by idh_ch, sort: egen ylnm_ch=sum(ylnm_ci) if miembros_ci==1, missing

**********************************
*** remesas_ch & remesasnm_ch ***
**********************************

gen remesash=.

by idh_ch, sort: egen remesasi=sum(remesas_ci) if miembros_ci==1, missing
replace remesasi=. if remesasi==0
egen remesas_ch=rsum(remesasi remesash), missing
replace remesas_ch=. if remesasi==. 

gen remesasnm_ch=.


***************
*** ynlm_ch ***
***************

by idh_ch, sort: egen ynlm=sum(ynlm_ci) if miembros_ci==1, missing
egen ynlm_ch=rsum(ynlm remesash), missing
replace ynlm_ch=. if ynlm==. 
drop ynlm

****************
*** ynlnm_ch ***
****************

gen ynlnm_ch=remesasnm_ch

*******************
*** autocons_ci ***
*******************

gen autocons_ci=.


*******************
*** autocons_ch ***
*******************

by idh_ch, sort: egen autocons_ch=sum(autocons_ci) if miembros_ci==1, missing

*******************
*** rentaimp_ch ***
*******************

gen rentaimp_ch=.

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

/*
grado
0 Ninguno 
1 Primero
2 Segundo
3 Tercero
4 Cuarto
5 Quinto
6 Sexto

nivel 
0 Ninguno
1 Preescolar
2 Primaria
3 Secundaria Basica
4 Bachillerato Comercial
5 Bachillerato Tecnico
6 Bachillerato Humanistico
7 Formacion Docente
8 Formacion Politica
9 Universitario

*/

*** people who have missings
gen byte yedc=.
replace yedc=. if nivgrado==99

**<5 and no education
replace yedc=0 if nivgrado==0 

*** preescolar o jardin o pre-primaria
replace yedc=0 if nivgrado==10 

*** primaria 
replace yedc=1 if nivgrado==21
replace yedc=2 if nivgrado==22
replace yedc=3 if nivgrado==23

replace yedc=4 if nivgrado==24
replace yedc=5 if nivgrado==25
replace yedc=6 if nivgrado==26 

*** secundaria 
replace yedc=7 if nivgrado==31 
replace yedc=8 if nivgrado==32
replace yedc=9 if nivgrado==33
replace yedc=10 if nivgrado==44| nivgrado==54| nivgrado==64
replace yedc=11 if nivgrado==45| nivgrado==55| nivgrado==65
replace yedc=12 if nivgrado==46| nivgrado==56| nivgrado==66



*** superior   *** 
replace yedc=13 if nivgrado==71 | nivgrado==81
replace yedc=14 if nivgrado==72 
replace yedc=15 if nivgrado==73 | nivgrado==83
replace yedc=16 if nivgrado==84

*** universitario
replace yedc=13 if nivgrado==91
replace yedc=14 if nivgrado==92
replace yedc=15 if nivgrado==93
replace yedc=16 if nivgrado==94 
replace yedc=17 if nivgrado==95 
replace yedc=18 if nivgrado==96
gen byte aedu_ci=yedc

**************
***eduno_ci***
**************

gen byte eduno_ci=0
replace eduno_ci=1 if aedu_ci==0
replace eduno_ci=. if aedu_ci==.
label variable eduno_ci "Cero anios de educacion"

**************
***edupi_ci***
**************

gen byte edupi_ci=0
replace edupi_ci=1 if aedu_ci>0 & aedu_ci<6
replace edupi_ci=. if aedu_ci==.
label variable edupi_ci "Primaria incompleta"

**************
***edupc_ci***
**************

gen byte edupc_ci=0
replace edupc_ci=1 if aedu_ci==6
replace edupc_ci=. if aedu_ci==.
label variable edupc_ci "Primaria completa"

**************
***edusi_ci***
**************

gen byte edusi_ci=0
replace edusi_ci=1 if aedu_ci>6 & aedu_ci<12
replace edusi_ci=. if aedu_ci==.
label variable edusi_ci "Secundaria incompleta"

**************
***edusc_ci***
**************

gen byte edusc_ci=0
replace edusc_ci=1 if aedu_ci==12
replace edusc_ci=. if aedu_ci==.
label variable edusc_ci "Secundaria completa"

***************
***edus1i_ci***
***************

gen byte edus1i_ci=0
replace edus1i_ci=1 if aedu_ci>6 & aedu_ci<9
replace edus1i_ci=. if aedu_ci==.
label variable edus1i_ci "1er ciclo de la secundaria incompleto"

***************
***edus1c_ci***
***************

gen byte edus1c_ci=0
replace edus1c_ci=1 if aedu_ci==9
replace edus1c_ci=. if aedu_ci==.
label variable edus1c_ci "1er ciclo de la eecundaria completo"

***************
***edus2i_ci***
***************

gen byte edus2i_ci=0
replace edus2i_ci=1 if aedu_ci>9 & aedu_ci<12
replace edus2i_ci=. if aedu_ci==.
label variable edus2i_ci "2do ciclo de la secundaria incompleto"
***************
***edus2c_ci***
***************

gen byte edus2c_ci=0
replace edus2c_ci=1 if aedu_ci==12
replace edus2c_ci=. if aedu_ci==.
label variable edus2c_ci "2do ciclo de la secundaria completo"

**************
***eduui_ci***
**************

gen byte eduui_ci=0
replace eduui_ci=1 if aedu_ci>12 & aedu_ci<17
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
***edupre_ci***
***************

gen byte edupre_ci=.
label variable edupre_ci "Educacion preescolar"


**************
***eduac_ci***
**************
gen byte eduac_ci=.
label variable eduac_ci "Superior universitario vs superior no universitario"

***************
***asiste_ci***
***************

gen asiste_ci=.
replace asiste_ci=1 if asiste==1 
replace asiste_ci=0 if asiste==6
label variable asiste_ci "Asiste actualmente a la escuela"

*****************
***pqnoasis_ci***
*****************

gen pqnoasis_ci=.

**************
*pqnoasis1_ci*
**************
**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**

g       pqnoasis1_ci = .

***************
***repite_ci***
***************

gen repite_ci=.
label variable repite_ci "Esta repitendo el grado o curso"


******************
***repiteult_ci***
******************

gen repiteult_ci=.
label variable repiteult_ci "Esta repitendo ultimo grado o curso"

***************
***edupub_ci***
***************

gen edupub_ci=.

***************
***tecnica_ci**
***************

gen tecnica_ci=.

label var tecnica_ci "1=formacion terciaria tecnica"

********************************************
***Variables de Infraestructura del hogar***
********************************************

***************
**aguared_ch***
***************

gen aguared_ch=(agua<=2)


****************
**aguadist_ch***
****************

gen aguadist_ch=lugabast


****************
**aguamala_ch***
****************

gen aguamala_ch=(agua==3 | agua==4 | agua==7 | agua==8)


****************
**aguamide_ch***
****************

gen aguamide_ch=.

****************
*****luz_ch*****
****************

gen luz_ch=luz
replace luz_ch=0 if luz_ch==2

****************
***luzmide_ch***
****************

gen luzmide_ch=.

****************
***combust_ch***
****************

gen combust_ch=(combusti==3)

****************
****bano_ch*****
****************


gen bano_ch=(servsani<5)



****************
****banoex_ch***
****************

gen banoex_ch=.


****************
****des1_ch*****
****************

gen des1_ch=.
replace des1_ch=0 if servsani==5 
replace des1_ch=1 if servsani==1 | servsani==3
replace des1_ch=2 if servsani==4 | servsani==2



****************
****des2_ch*****
****************

gen des2_ch=des1_ch
replace des2_ch=. if des1_ch==3
 
****************
****piso_ch*****
****************

gen piso_ch=.
replace piso_ch=1 if piso>0
replace piso_ch=0 if piso==1
replace piso_ch=2 if piso==8



****************
****pared_ch****
****************

gen muros1=pared
gen pared_ch=.
replace pared_ch=0 if muros1>=1
replace pared_ch=1 if muros1>=3 & muros1<=5
replace pared_ch=2 if muros1==6
drop muros1

****************
****techo_ch****
****************
gen techo1=techo
gen techo_ch=.
replace techo_ch=0 if techo1>=1 
replace techo_ch=1 if techo1>=2 & techo1<=6
replace techo_ch=2 if techo1==7
drop techo1

****************
****resid_ch****
****************


gen resid_ch=0 if basura==4
replace resid_ch=1 if basura==2 
replace resid_ch=2 if basura==1 | basura==3
replace resid_ch=3 if basura==5 

**Daniela Zuluaga- Enero 2018: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**
	
*********************
***aguamejorada_ch***
*********************
g       aguamejorada_ch = 1 if (agua >=1 & agua <=2) | (agua >=5 & agua <=6)
replace aguamejorada_ch = 0 if (agua >=3 & agua <=4) | (agua >=7 & agua <=8)
		
*********************
***banomejorado_ch***
*********************
g       banomejorado_ch = 1 if (servsani >=1 & servsani <=2)
replace banomejorado_ch = 0 if (servsani >=3 & servsani <=6)


****************
****dorm_ch*****
****************

gen dorm_ch=.
replace dorm_ch=nrodormi 
replace dorm_ch=. if dorm_ch==99

****************
***cuartos_ch***
****************

gen cuartos_ch=nrocuart

****************
***cocina_ch****
****************

gen cocina_ch=cocina
replace cocina_ch=0 if cocina==6


****************
****telef_ch****
****************

gen telef_ch=.


****************
****refrig_ch***
****************

gen refrig1=tiene01
gen refrig_ch=.
replace refrig_ch=1 if refrig1>0
replace refrig_ch=0 if refrig1==0
drop refrig1

****************
****freez_ch****
****************

gen freez_ch=.


****************
****auto_ch****
****************
gen automovil=tiene12
gen auto_ch=.
replace auto_ch=1 if automovil>0
replace auto_ch=0 if automovil==0
drop automovil 

****************
****compu_ch****
****************

gen compu_ch=.
replace compu_ch=1 if tiene19>0
replace compu_ch=0 if tiene19==0

****************
**internet_ch***
****************

gen internet_ch=.

****************
****cel_ch******
****************

gen cel_ch=.

****************
****vivi1_ch****
****************

gen vivi1_ch=.
replace vivi1_ch=1 if tipoviv==1
replace vivi1_ch=2 if tipoviv==2


****************
****vivi2_ch****
****************

gen vivi2_ch=0
replace vivi2_ch=1 if tipoviv==1
replace vivi2_ch=0 if tipoviv==2

*******************
****viviprop_ch****
*******************

gen viviprop_ch=.
replace viviprop_ch=0 if tenencia==2 
replace viviprop_ch=1 if tenencia==1
replace viviprop_ch=3 if tenencia==3 

******************
****vivitit_ch****
******************

gen vivitit_ch=.

*******************
****vivialq_ch****
*******************

gen vivialq_ch=.

*********************
****vivialqimp_ch****
*********************

gen vivialqimp_ch=.

*********
*raza_ci*
*********
/*
gen raza_ci=1 if idioma==1 
replace raza_ci=0 if idioma==2 | idioma==3 | idioma==4 | idioma==5
*/
/*
*Mayra Sáenz- Octubre 2013
gen raza_ci=.
replace raza_ci= 1 if idioma ==1 
replace raza_ci= 3 if idioma ==2 | idioma ==3 | idioma ==4 |raza_ci==.

label define raza_ci 1 "Indígena" 2 "Afro-descendiente" 3 "Otros" 
label value raza_ci raza_ci 
label value raza_ci raza_ci
label var raza_ci "Raza o etnia del individuo" 
*/

*Modificación Mayra Sáenz 10/20/2015: modificaciones realizadas en base a metodología enviada por SCL/GDI Maria Olga Peña
gen raza_idioma_ci=.
replace raza_idioma_ci= 1 if idioma ==1 
replace raza_idioma_ci= 3 if idioma ==2 | idioma ==3 | idioma ==4 |raza_idioma_ci==.

label define raza_idioma_ci 1 "Indígena" 2 "Afro-descendiente" 3 "Otros" 
label value raza_idioma_ci raza_idioma_ci 
label value raza_idioma_ci raza_idioma_ci
label var raza_idioma_ci "Raza o etnia del individuo" 

gen raza_ci=.

gen id_ind_ci = 0
replace id_ind_ci=1 if raza_idioma_ci==1 
label define id_ind_ci 1 "Indígena" 0 "Otros" 
label value id_ind_ci id_ind_ci 
label var id_ind_ci  "Indigena" 

gen id_afro_ci = 0
replace id_afro_ci=1 if raza_idioma_ci==2 
label define id_afro_ci 1 "Afro-descendiente" 0 "Otros" 
label value id_afro_ci id_afro_ci 
label var id_afro_ci "Afro-descendiente"



ren ocup ocup_old

/*_____________________________________________________________________________________________________*/
* Asignación de etiquetas e inserción de variables externas: tipo de cambio, Indice de Precios al 
* Consumidor (2011=100), líneas de pobreza
/*_____________________________________________________________________________________________________*/


do "$gitFolder\armonizacion_microdatos_encuestas_hogares_scl\_DOCS\\Labels&ExternalVars_Harmonized_DataBank.do"

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
rename ocuprin codocupa
rename rama codindustria

compress


saveold "`base_out'", replace


log close





