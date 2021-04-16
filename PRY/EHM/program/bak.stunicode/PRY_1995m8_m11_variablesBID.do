
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
local ANO "1995"
local ronda m8_m11

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
/* No es claro por que se usaba el primer digito.
Ver manual 1995...esa NO es la clasificacion correcta

gen ocupa=string(ocupp)
gen ocupa_ci=real(substr(ocupa,1,1))
replace ocupa_ci=. if ocupa_ci==0 | emp_ci~=1
drop ocupa

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

gen byte zona_c=(area==1)

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

gen anio_c=1995
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
*1995 y 1997-2003 es nacional (N)

gen muestra_AMA=0
replace muestra_AMA=1 if depto==0
label variable muestra_AMA "Asuncion Metropolitana"
	 	
gen muestra_N=1
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
gen afiliado_ci=.	
replace afiliado_ci=1 if asistjub==1
replace afiliado_ci=0 if (asistjub==6 | asistjub==9 | asistjub==.)
replace afiliado_ci = 0 if pea == 2 
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
* En ocupados no se toma en cuenta al subempleo y los categorizan como inactivos (esto se cambia). 
* Tambien la edad minima de la encuesta es 7 anios. MGD 05/27/2014

gen condocup_ci=.
replace condocup_ci=1 if (trabasal==1 | trainoag==1 | trabag==1) & (asalar==1 | indenoag==1 | indeag==1)
replace condocup_ci=2 if ((trabasal==6 | trainoag==6 | trabag==6) & (asalar==6 | indenoag==6 | indeag==6)) & busco7==1 & trahora==1
recode condocup_ci .=3 if edad_ci>=7
recode condocup_ci .=4 if edad_ci<7

label var condocup_ci "Condicion de ocupación de acuerdo a def de cada pais"
label define condocup_ci 1 "Ocupado" 2 "Desocupado" 3 "Inactivo" 4 "Menor de PET" 
label value condocup_ci condocup_ci
*/

* No coincide cuando se calcula con la variable creada de desempleo oculto al condicionar el calculo original por busqueda de trabajo;
* sin embargo si coincide con calculos externos SEDLAC. MGD 05/28/2014

gen condocup_ci=.
replace condocup_ci=1 if (pea==1 | pea==4 | pea==5) & edad_ci>=7
replace condocup_ci=2 if pea==2  & edad_ci>=7
recode condocup_ci .=3 if (pea==3 | pea==6) | (condocup_ci==. & edad_ci>=7)
recode condocup_ci (.=4) if edad_ci<7 | pea==0

label var condocup_ci "Condicion de ocupación de acuerdo a def de cada pais"
label define condocup_ci 1 "Ocupado" 2 "Desocupado" 3 "Inactivo" 4 "Menor de PET" 
label value condocup_ci condocup_ci


*************
*cesante_ci* 
*************

gen cesante_ci=1 if hatrab==1 &  pea==2 & edad_ci>=10
replace cesante_ci=0 if hatrab==6 &  pea==2 & edad_ci>=10
label var cesante_ci "Desocupado - definicion oficial del pais"	

*************
**pension_ci*
*************

gen pension_ci=1 if (yjubpen>0 & yjubpen<. & yjubpen!=9999999) 
recode pension_ci .=0 
label var pension_ci "1=Recibe pension contributiva"

*************
*ypen_ci*
*************
replace yjubpen=. if (yjubpen >= 9999999 & yjubpen!=.)
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

* PRY 1995
gen salmm_ci= 417450
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
/*
gen subemp_ci=0
egen tothoras=rsum(asalhplu asalhpma asalhpmi asalhpju asalhpvi asalhpsa asalhpdo inahplu inahpma inahpmi inahpju inahpvi inahpsa inahpdo asalhslu asalhsma asalhsmi asalhsju asalhsvi asalhssa asalhsdo inahslu inahsma inahsmi inahsju inahsvi inahssa inahsdo iahplu iahpma iahpmi iahpju iahpvi iahpsa iahpdo prochplu prochpma prochpmi prochpju prochpvi prochpsa prochpdo talahplu talahpma talahpmi talahpju talahpvi talahpsa talahpdo hrnor12 hrsem12), missing
replace subemp_ci=1 if tothoras<=30 & deseamas==1 & emp_ci==1 
*/
* Generacion de horas totales considerando solo actividad principal. MGD 06/19/2014
egen tothoras1=rsum(asalhplu asalhpma asalhpmi asalhpju asalhpvi asalhpsa asalhpdo inahplu inahpma inahpmi inahpju inahpvi inahpsa inahpdo  iahplu iahpma iahpmi iahpju iahpvi iahpsa iahpdo prochplu prochpma prochpmi prochpju prochpvi prochpsa prochpdo talahplu talahpma talahpmi talahpju talahpvi talahpsa talahpdo), missing

* Modificacion: considerando horas totales. MGD 06/19/2014
gen subemp_ci=0
replace subemp_ci=1 if (tothoras1>=1 & tothoras1<=30) & (emp_ci==1 & deseamas==1)


*****************
***horaspri_ci***
*****************

gen hr_seman=.
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
1: Empleado público  
2: Empleado privado 
3 Obrero público 
4: Obrero privado
5: Empleador o patrón
6: Trabajador por cuenta propia
7: Trabajador familiar remunerado
8: Trabajador familiar no remunerado
9: Empleado doméstico
10: Trabajador independiente agropecuario
*/
/*
gen categopri_ci=.
replace categopri_ci=1 if categ==5
replace categopri_ci=2 if categ==6 | categ==10
replace categopri_ci=3 if categ==1 | categ==2 | categ==3 | categ==4 | categ==7 | categ==9
replace categopri_ci=4 if categ==8
replace categopri_ci=. if emp_ci~=1 |categ==0

label define categopri_ci 1"Patron" 2"Cuenta propia" 
label define categopri_ci 3"Empleado" 4" Familiar no remunerado" , add
label value categopri_ci categopri_ci
label variable categopri_ci "Categoria ocupacional trabajo principal"
*/

*Mélany Gualavisí - Septiembre 2014
/*En lo de Cuenta propia de Paraguay los saltos que se dan desde 1995 hasta 1990 se deben a lo siguiente:
•	En el 95, se cambia la clasificación agregándose dos categorías extra solo en este año: trabajador agropecuario independiente que se va a cuenta propia y trabajador familiar remunerado que se va a empleados.
•	Del 90 al 94 se vuelve a la clasificación anterior; sin embargo, los saltos del 94 y 93 es porque esas encuestas son solo urbanas.  Para el 90-92 ya son completas, por eso sube el porcentaje.
*/
*Modificado Mayra Sáenz - Septiembre de 2014
*retiro a las dos categorías extra de este año para hacerle comparable con el resto de la serie
gen categopri_ci=.
replace categopri_ci=1 if categ==5
replace categopri_ci=2 if categ==6
replace categopri_ci=3 if categ==1 | categ==2 | categ==3 | categ==4 | categ==9
replace categopri_ci=4 if categ==8
replace categopri_ci=. if emp_ci~=1 |categ==0

label define categopri_ci 1"Patron" 2"Cuenta propia" , add modify
label define categopri_ci 3"Empleado" 4" Familiar no remunerado" , add modify
label value categopri_ci categopri_ci
label variable categopri_ci "Categoria ocupacional trabajo principal"


******************
***categosec_ci***
******************

gen categosec_ci=.


*****************
*tipocontrato_ci*
*****************
/*
CONTRP12 ¿BAJO QUÉ TIPO DE CONTRATO TRABAJA EN ESTA OCUPACIÓN? (Pregunta 10)
0: No aplicable
1: Contrato indefinido (nombrado)
2: Contrato definido
3: Sin contrato
9: No responde
*/

gen tipocontrato_ci=. 
replace tipocontrato_ci=1 if contrp12 ==1 
replace tipocontrato_ci=2 if contrp12 ==2
replace tipocontrato_ci=3 if contrp12 ==3
label var tipocontrato_ci "Tipo de contrato segun su duracion"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci



*****************
***nempleos_ci***
*****************

egen prisec=rsum(asal1a16 asa18a23 inoa1a14 ina16a20 iag1a2 iag3a4 iag5a6), missing
egen iahp= rsum(iahplu iahpma iahpmi iahpju iahpvi iahpsa iahpdo), missing
egen prochp= rsum (prochplu prochpma prochpmi prochpju prochpvi prochpsa prochpdo), missing
egen talhp=rsum(talahplu talahpma talahpmi talahpju talahpvi talahpsa talahpdo), missing
egen horagro=rsum(iahp prochp talhp), missing

gen byte empleos=1 if prisec==1
replace empleos=2 if prisec==3 & (ocupasal>0 & ocusasal==0 & nrocupas==0 & ocupina>0 & ocusina==0 & nrocuina==0 & horagro==0)  
replace empleos=2 if prisec==3 & (ocupasal>0 & ocusasal>0 & nrocupas==0 & ocupina==0 & ocusina==0 & nrocuina==0 & horagro==0)  
replace empleos=2 if prisec==3 & (ocupasal==0 & ocusasal==0 & nrocupas==0 & ocupina>0 & ocusina>0 & nrocuina==0 & horagro==0)  
replace empleos=3 if prisec==3 & (ocupasal>0 & ocusasal>0 & nrocupas==0 & ocupina>0 & ocusina==0 & nrocuina==0 & horagro==0)
replace empleos=3 if prisec==3 & (ocupasal>0 & ocusasal>0 & nrocupas==1 & ocupina==0 & ocusina==0 & nrocuina==0 & horagro==0)  
replace empleos=3 if prisec==3 & (ocupasal>0 & ocusasal>0 & nrocupas==1 & ocupina>0 & ocusina==0 & nrocuina==0 & horagro==0)  
replace empleos=3 if prisec==3 & (ocupasal>0 & ocusasal==0 & nrocupas==0 & ocupina>0 & ocusina>0 & nrocuina==0 & horagro==0)
replace empleos=3 if prisec==3 & (ocupasal>0 & ocusasal==0 & nrocupas==1 & ocupina>0 & ocusina>0 & nrocuina==0 & horagro==0)

replace empleos=4 if prisec==3 & (ocupasal>0 & ocusasal>0 & nrocupas==0 & ocupina>0 & ocusina>0 & nrocuina==0 & horagro==0)  
replace empleos=4 if prisec==3 & (ocupasal>0 & ocusasal>0 & nrocupas==2 & ocupina==0 & ocusina==0 & nrocuina==0 & horagro==0)  
replace empleos=5 if prisec==3 & (ocupasal>0 & ocusasal>0 & nrocupas==1 & ocupina>0 & ocusina>0 & nrocuina==0 & horagro==0)  
replace empleos=5 if prisec==3 & (ocupasal>0 & ocusasal>0 & nrocupas==2 & ocupina>0 & ocusina==0 & nrocuina==0 & horagro==0)  

replace empleos=6 if prisec==3 & (ocupasal>0 & ocusasal>0 & nrocupas==2 & ocupina>0 & ocusina>0 & nrocuina==0 & horagro==0)  


replace empleos=5 if prisec==3 & (ocupasal>0 & ocusasal>0 & nrocupas==0 & ocupina>0 & ocusina>0 & nrocuina==1 & horagro==0)  
replace empleos=3 if prisec==3 & (ocupasal==0 & ocusasal==0 & nrocupas==0 & ocupina>0 & ocusina>0 & nrocuina==1 & horagro==0)  
replace empleos=4 if prisec==3 & (ocupasal==0 & ocusasal==0 & nrocupas==0 & ocupina>0 & ocusina>0 & nrocuina==2 & horagro==0)  

*** empleos for people who have horas agro
** first:  for those who only have recorded from agro
replace empleos=2 if prisec==3 & (ocupasal==0 & ocusasal==0 & nrocupas==0 & ocupina==0 & ocusina==0 & nrocuina==0 & iahp>0 & prochp>0 & talhp==0)  
replace empleos=2 if prisec==3 & (ocupasal==0 & ocusasal==0 & nrocupas==0 & ocupina==0 & ocusina==0 & nrocuina==0 & iahp>0 & prochp==0 & talhp>0) 
replace empleos=2 if prisec==3 & (ocupasal==0 & ocusasal==0 & nrocupas==0 & ocupina==0 & ocusina==0 & nrocuina==0 & iahp==0 & prochp>0 & talhp>0) 
replace empleos=3 if prisec==3 & (ocupasal==0 & ocusasal==0 & nrocupas==0 & ocupina==0 & ocusina==0 & nrocuina==0 & iahp>0 & prochp>0 & talhp>0) 

*second:  those who recorded from agro and ocupina
*replace empleos=3 if prisec==3 & (ocupasal==0 & ocusasal==0 & nrocupas==0 & ocupina>0 & ocusina==0 & nrocuina==0 & iahp>0 & prochp>0 & talhp==0)  

replace empleos=2 if prisec==3 & (ocupasal==0 & ocusasal==0 & nrocupas==0 & ocupina>0 & ocusina==0 & nrocuina==0 & iahp>0 & prochp==0 & talhp==0)
replace empleos=2 if prisec==3 & (ocupasal==0 & ocusasal==0 & nrocupas==0 & ocupina>0 & ocusina==0 & nrocuina==0 & iahp==0 & prochp>0 & talhp==0)
replace empleos=2 if prisec==3 & (ocupasal==0 & ocusasal==0 & nrocupas==0 & ocupina>0 & ocusina==0 & nrocuina==0 & iahp==0 & prochp==0 & talhp>0)

replace empleos=3 if prisec==3 & (ocupasal==0 & ocusasal==0 & nrocupas==0 & ocupina>0 & ocusina>0 & nrocuina==0 & iahp>0 & prochp==0 & talhp==0)  
replace empleos=3 if prisec==3 & (ocupasal==0 & ocusasal==0 & nrocupas==0 & ocupina>0 & ocusina>0 & nrocuina==0 & iahp==0 & prochp>0 & talhp==0) 
replace empleos=3 if prisec==3 & (ocupasal==0 & ocusasal==0 & nrocupas==0 & ocupina>0 & ocusina>0 & nrocuina==0 & iahp==0 & prochp==0 & talhp>0) 

replace empleos=3 if prisec==3 & (ocupasal==0 & ocusasal==0 & nrocupas==0 & ocupina>0 & ocusina==0 & nrocuina==0 & iahp>0 & prochp>0 & talhp==0)  
replace empleos=3 if prisec==3 & (ocupasal==0 & ocusasal==0 & nrocupas==0 & ocupina>0 & ocusina==0 & nrocuina==0 & iahp>0 & prochp==0 & talhp>0) 
replace empleos=3 if prisec==3 & (ocupasal==0 & ocusasal==0 & nrocupas==0 & ocupina>0 & ocusina==0 & nrocuina==0 & iahp==0 & prochp>0 & talhp>0) 
replace empleos=4 if prisec==3 & (ocupasal==0 & ocusasal==0 & nrocupas==0 & ocupina>0 & ocusina==0 & nrocuina==0 & iahp>0 & prochp>0 & talhp>0) 

replace empleos=4 if prisec==3 & (ocupasal==0 & ocusasal==0 & nrocupas==0 & ocupina>0 & ocusina>0 & nrocuina==0 & iahp>0 & prochp>0 & talhp==0)  
replace empleos=4 if prisec==3 & (ocupasal==0 & ocusasal==0 & nrocupas==0 & ocupina>0 & ocusina>0 & nrocuina==0 & iahp>0 & prochp==0 & talhp>0) 
replace empleos=4 if prisec==3 & (ocupasal==0 & ocusasal==0 & nrocupas==0 & ocupina>0 & ocusina>0 & nrocuina==0 & iahp==0 & prochp>0 & talhp>0) 
replace empleos=5 if prisec==3 & (ocupasal==0 & ocusasal==0 & nrocupas==0 & ocupina>0 & ocusina>0 & nrocuina==0 & iahp>0 & prochp>0 & talhp>0) 

*third:  those who recorded from agro and ocupasal
replace empleos=2 if prisec==3 & (ocupasal>0 & ocusasal==0 & nrocupas==0 & ocupina==0 & ocusina==0 & nrocuina==0 & iahp>0 & prochp==0 & talhp==0)
replace empleos=2 if prisec==3 & (ocupasal>0 & ocusasal==0 & nrocupas==0 & ocupina==0 & ocusina==0 & nrocuina==0 & iahp==0 & prochp>0 & talhp==0)
replace empleos=2 if prisec==3 & (ocupasal>0 & ocusasal==0 & nrocupas==0 & ocupina==0 & ocusina==0 & nrocuina==0 & iahp==0 & prochp==0 & talhp>0)

replace empleos=3 if prisec==3 & (ocupasal>0 & ocusasal==0 & nrocupas==0 & ocupina==0 & ocusina==0 & nrocuina==0 & iahp>0 & prochp>0 & talhp==0)  
replace empleos=3 if prisec==3 & (ocupasal>0 & ocusasal==0 & nrocupas==0 & ocupina==0 & ocusina==0 & nrocuina==0 & iahp>0 & prochp==0 & talhp>0) 
replace empleos=3 if prisec==3 & (ocupasal>0 & ocusasal==0 & nrocupas==0 & ocupina==0 & ocusina==0 & nrocuina==0 & iahp==0 & prochp>0 & talhp>0) 
replace empleos=4 if prisec==3 & (ocupasal>0 & ocusasal==0 & nrocupas==0 & ocupina==0 & ocusina==0 & nrocuina==0 & iahp>0 & prochp>0 & talhp>0) 

replace empleos=3 if prisec==3 & (ocupasal>0 & ocusasal>0 & nrocupas==0 & ocupina==0 & ocusina==0 & nrocuina==0 & iahp>0 & prochp==0 & talhp==0) 
replace empleos=3 if prisec==3 & (ocupasal>0 & ocusasal>0 & nrocupas==0 & ocupina==0 & ocusina==0 & nrocuina==0 & iahp==0 & prochp>0 & talhp==0) 
replace empleos=3 if prisec==3 & (ocupasal>0 & ocusasal>0 & nrocupas==0 & ocupina==0 & ocusina==0 & nrocuina==0 & iahp==0 & prochp==0 & talhp>0) 
 
replace empleos=4 if prisec==3 & (ocupasal>0 & ocusasal>0 & nrocupas==0 & ocupina==0 & ocusina==0 & nrocuina==0 & iahp>0 & prochp>0 & talhp==0)  
replace empleos=4 if prisec==3 & (ocupasal>0 & ocusasal>0 & nrocupas==0 & ocupina==0 & ocusina==0 & nrocuina==0 & iahp>0 & prochp==0 & talhp>0) 
replace empleos=4 if prisec==3 & (ocupasal>0 & ocusasal>0 & nrocupas==0 & ocupina==0 & ocusina==0 & nrocuina==0 & iahp==0 & prochp>0 & talhp>0) 
replace empleos=5 if prisec==3 & (ocupasal>0 & ocusasal>0 & nrocupas==0 & ocupina==0 & ocusina==0 & nrocuina==0 & iahp>0 & prochp>0 & talhp>0) 

*fourth:  those who recorded from agro, ocupasal, and ocupina
replace empleos=3 if prisec==3 & (ocupasal>0 & ocusasal==0 & nrocupas==0 & ocupina>0 & ocusina==0 & nrocuina==0 & iahp>0 & prochp==0 & talhp==0)  
replace empleos=3 if prisec==3 & (ocupasal>0 & ocusasal==0 & nrocupas==0 & ocupina>0 & ocusina==0 & nrocuina==0 & iahp==0 & prochp>0 & talhp==0) 
replace empleos=3 if prisec==3 & (ocupasal>0 & ocusasal==0 & nrocupas==0 & ocupina>0 & ocusina==0 & nrocuina==0 & iahp==0 & prochp==0 & talhp>0) 

replace empleos=4 if prisec==3 & (ocupasal>0 & ocusasal==0 & nrocupas==0 & ocupina>0 & ocusina>0 & nrocuina==0 & iahp>0 & prochp==0 & talhp==0)  
replace empleos=4 if prisec==3 & (ocupasal>0 & ocusasal==0 & nrocupas==0 & ocupina>0 & ocusina>0 & nrocuina==0 & iahp==0 & prochp>0 & talhp==0) 
replace empleos=4 if prisec==3 & (ocupasal>0 & ocusasal==0 & nrocupas==0 & ocupina>0 & ocusina>0 & nrocuina==0 & iahp==0 & prochp==0 & talhp>0) 

replace empleos=4 if prisec==3 & (ocupasal>0 & ocusasal==0 & nrocupas==0 & ocupina>0 & ocusina==0 & nrocuina==0 & iahp>0 & prochp>0 & talhp==0)  
replace empleos=4 if prisec==3 & (ocupasal>0 & ocusasal==0 & nrocupas==0 & ocupina>0 & ocusina==0 & nrocuina==0 & iahp>0 & prochp==0 & talhp>0) 
replace empleos=4 if prisec==3 & (ocupasal>0 & ocusasal==0 & nrocupas==0 & ocupina>0 & ocusina==0 & nrocuina==0 & iahp==0 & prochp>0 & talhp>0) 
replace empleos=5 if prisec==3 & (ocupasal>0 & ocusasal==0 & nrocupas==0 & ocupina>0 & ocusina==0 & nrocuina==0 & iahp>0 & prochp>0 & talhp>0) 

replace empleos=5 if prisec==3 & (ocupasal>0 & ocusasal>0 & nrocupas==0 & ocupina>0 & ocusina==0 & nrocuina==0 & iahp>0 & prochp>0 & talhp==0)  
replace empleos=5 if prisec==3 & (ocupasal>0 & ocusasal>0 & nrocupas==0 & ocupina>0 & ocusina==0 & nrocuina==0 & iahp>0 & prochp==0 & talhp>0) 
replace empleos=5 if prisec==3 & (ocupasal>0 & ocusasal>0 & nrocupas==0 & ocupina>0 & ocusina==0 & nrocuina==0 & iahp==0 & prochp>0 & talhp>0) 
replace empleos=6 if prisec==3 & (ocupasal>0 & ocusasal>0 & nrocupas==0 & ocupina>0 & ocusina==0 & nrocuina==0 & iahp>0 & prochp>0 & talhp>0) 

replace empleos=5 if prisec==3 & (ocupasal>0 & ocusasal==0 & nrocupas==0 & ocupina>0 & ocusina>0 & nrocuina==0 & iahp>0 & prochp>0 & talhp==0)  
replace empleos=5 if prisec==3 & (ocupasal>0 & ocusasal==0 & nrocupas==0 & ocupina>0 & ocusina>0 & nrocuina==0 & iahp>0 & prochp==0 & talhp>0) 
replace empleos=5 if prisec==3 & (ocupasal>0 & ocusasal==0 & nrocupas==0 & ocupina>0 & ocusina>0 & nrocuina==0 & iahp==0 & prochp>0 & talhp>0) 
replace empleos=6 if prisec==3 & (ocupasal>0 & ocusasal==0 & nrocupas==0 & ocupina>0 & ocusina>0 & nrocuina==0 & iahp>0 & prochp>0 & talhp>0) 

replace empleos=6 if prisec==3 & (ocupasal>0 & ocusasal>0 & nrocupas==0 & ocupina>0 & ocusina>0 & nrocuina==0 & iahp>0 & prochp>0 & talhp==0)  
replace empleos=6 if prisec==3 & (ocupasal>0 & ocusasal>0 & nrocupas==0 & ocupina>0 & ocusina>0 & nrocuina==0 & iahp>0 & prochp==0 & talhp>0) 
replace empleos=6 if prisec==3 & (ocupasal>0 & ocusasal>0 & nrocupas==0 & ocupina>0 & ocusina>0 & nrocuina==0 & iahp==0 & prochp>0 & talhp>0) 
replace empleos=7 if prisec==3 & (ocupasal>0 & ocusasal>0 & nrocupas==0 & ocupina>0 & ocusina>0 & nrocuina==0 & iahp>0 & prochp>0 & talhp>0) 

** after running this part there are 7 people still left with missings so 
** we run the program especially looking at the nrocupas and nrocuina

replace empleos=4 if prisec==3 & (ocupasal==0 & ocusasal==0 & nrocupas==0 & ocupina>0 & ocusina>0 & nrocuina==1 & iahp>0 & prochp==0 & talhp==0)  

replace empleos=4 if prisec==3 & (ocupasal==0 & ocusasal==0 & nrocupas==0 & ocupina>0 & ocusina>0 & nrocuina==2 & iahp>0 & prochp==0 & talhp==0)  

*** to generate number of empleos for those who did not work last week
replace empleos=1 if tientrab==1 & yprin>0
replace empleos=2 if tientrab==1 & yprin>0 & ysec>0

tab empleos if emp_ci==1, miss  

** change people to have empleos=0 if they are not employed
replace empleos=0 if emp_ci~=1

gen nempleos_ci=0
replace nempleos_ci=1 if empleos==1
replace nempleos_ci=2 if empleos>=2
replace nempleos_ci=. if pea_ci==0
drop empleos horagro prisec iahp prochp talhp
/*
*****************
***firmapeq_ci***
*****************
/*

TAMESP12 ¿CUÁNTAS PERSONAS APROXIMADAMENTE TRABAJAN EN EL ESTABLECIMIENTO DONDE TRABAJA?
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
replace firmapeq_ci=1 if tamesp12 ==1 | tamesp12==2
replace firmapeq_ci=0 if tamesp12>=3 & tamesp12<=6
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

gen ocupa_ci=.
gen ocuprin=ocupp 

replace ocupa_ci=1 if (ocuprin>=1 & ocuprin<=676) & emp_ci==1
replace ocupa_ci=2 if (ocuprin>=680 & ocuprin<=835) & emp_ci==1
replace ocupa_ci=3 if (ocuprin>=840 & ocuprin<=983) & emp_ci==1
replace ocupa_ci=4 if (ocuprin>=990 & ocuprin<=1103) & emp_ci==1
replace ocupa_ci=5 if (ocuprin>=2120 & ocuprin<=2305) & emp_ci==1
replace ocupa_ci=6 if (ocuprin>=1110 & ocuprin<=1247) & emp_ci==1
replace ocupa_ci=7 if (ocuprin>=1260 & ocuprin<=2116) & emp_ci==1
replace ocupa_ci=9 if (ocuprin>=2310 & ocuprin<=2352) & emp_ci==1
replace ocupa_ci=. if emp_ci~=1



*************
***rama_ci***
*************

* Se utiliza la rama de ocupados no la de actividad principal porque hay muchos valores no declarados. MGD 4/21/2014
g rama_ci=.
/*replace rama_ci=1 if (ramap>=110 & ramap<=149) & emp_ci==1
replace rama_ci=2 if (ramap>=210 & ramap<=290) & emp_ci==1
replace rama_ci=3 if (ramap>=310 & ramap<=394) & emp_ci==1
replace rama_ci=4 if (ramap>=410 & ramap<=411) & emp_ci==1
replace rama_ci=5 if (ramap>=510 & ramap<=511) & emp_ci==1
replace rama_ci=6 if (ramap>=610 & ramap<=614) & emp_ci==1
replace rama_ci=7 if (ramap>=710 & ramap<=750) & emp_ci==1
replace rama_ci=8 if (ramap>=810 & ramap<=836) & emp_ci==1
replace rama_ci=9 if (ramap>=910 & ramap<=999) & emp_ci==1*/

/*
gen rama1=string(ramap)
gen rama_ci=real(substr(rama1,1,1))  
replace rama_ci=. if rama_ci==0 | emp_ci~=1
drop rama1
*/
****************
***durades_ci***
****************

gen durades_ci=(anosbus*12)+mesesbus+(sembus/4.3)
replace durades_ci=. if durades_ci==0

*******************
***antiguedad_ci***
*******************
/* 
tomado de homogenizacion anterior
to generate job tenure
note: here there is no overlapping between months and years
first for people who are empleados
*/
gen asnroano1=asnroano
replace asnroano1=. if asnroano1==0 | asnroano1==99
gen byte tenurey=asnroano1 if asal1a16==1

*** months for empleados
gen asnromes1=asnromes
replace asnromes1=. if asnromes1==0
gen byte tenurem= asnromes1 if asal1a16==1 & asnromes1~=.
replace tenurem=. if tenurem==99 | (asnrosem==99 & asal1a16==1)
replace tenurem=0 if (asnrosem>0 & asnrosem<4) & asal1a16==1  
*** less than one month 
replace tenurem=1 if (asnrosem>=4 & asnrosem<8) & asal1a16==1
replace tenurem=2 if (asnrosem>=8 & asnrosem<12) & asal1a16==1

*** people who are indep't
*** years for indep'ts
gen inanrano1=inanrano
replace inanrano1=. if inanrano1==0 | inanrano1==99 
replace tenurey=inanrano1 if inoa1a14==1 & inanrano1~=.

*** months for indep'ts
gen inanrmes1=inanrmes
replace inanrmes1=. if inanrmes1==0 | inanrmes1==99
replace tenurem=inanrmes1 if inoa1a14==1 & inanrmes1~=.
replace tenurem=. if tenurem==99 | (inanrsem==99 & inoa1a14==1)
replace tenurem=0 if (inanrsem>0 & inanrsem<4) & inoa1a14==1 
*** less than one month
replace tenurem=1 if (inanrsem>=4 & inanrsem<8) & inoa1a14==1
replace tenurem=2 if (inanrsem>=8 & inanrsem<12) & inoa1a14==1


**** for those who did not work in the last week
*** years for those who did not work
gen anosp121=anosp12
replace anosp121=. if anosp121==0 | anosp121==99 
replace tenurey=anosp121 if tientrab==1 & anosp121~=.


*** months for those who did not work
gen mesesp121=mesesp12
replace mesesp121=. if mesesp121==0 | mesesp121==99
replace tenurem=mesesp121 if tientrab==1 & mesesp121~=.
replace tenurem=. if tenurem==99 | (semp12==99 & tientrab==1)
replace tenurem=0 if (semp12>0 & semp12<4) & tientrab==1 
*** less than one month
replace tenurem=1 if (semp12>=4 & semp12<8) & tientrab==1
replace tenurem=2 if (semp12>=8 & semp12<12) & tientrab==1

/*note: there are 9 people who will have missings reported for both tenurem
& tenurey even though they declared their first job to be either dep't or indep't no agrícola. For those who are agrícola tenurem & tenurey are not available
*/
*** now create one variable for paraguay 95 using tenurey and tenurem

gen byte tenure=tenurey
replace tenure=0 if tenurem>=0 & tenurem<12 
** less than one year
/*replace tenure=1 if tenurem>=12 & tenurem<24
replace tenure=2 if tenurem>=24 & tenurem<36
replace tenure=3 if tenurem>=36 & tenurem<48
replace tenure=4 if tenurem>=48 & tenurem<60
replace tenure=5 if tenurem>=60 & tenurem<72
replace tenure=6 if tenurem>=72 & tenurem<84
*/
replace tenure=tenurem/12 if tenure==.
drop asnromes1 inanrano1 asnroano1 inanrmes1 anosp121 mesesp121
gen antiguedad_ci=tenure

*2015, 12 MLO: elimino la variable antiguedad, la variable creada se refiere a cuanto tiempo estuvo en esa ocupacion no en la empresa
replace antiguedad_ci=.
*******************
***tamemp_ci***
*******************
*Paraguay Pequeña 1 a 5, Mediana 6 a 50, Grande Más de 50

gen tamemp_ci = 1 if tamesp12>=1 & tamesp12<=2
replace tamemp_ci = 2 if (tamesp12>=3 & tamesp12<=5)
replace tamemp_ci = 3 if (tamesp12==6)
label define tamemp_ci 1 "Pequeña" 2 "Mediana" 3 "Grande"
label value tamemp_ci tamemp_ci
label var tamemp_ci "Tamaño de empresa"

*******************
***categoinac_ci***
*******************
gen categoinac_ci =1 if (pqnotra7==12 & condocup_ci==3)
replace categoinac_ci = 2 if  (pqnotra7==2 & condocup_ci==3)
replace categoinac_ci = 3 if  (pqnotra7==17 & condocup_ci==3)
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
replace ylmpri_ci=. if yprin<=0 | yprin==999999999 
replace ylmpri_ci=. if yprin>9500000 
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
replace ysec1=. if ysec==0 | ysec==99999999 /*No aplicable*/

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
replace ylmotros_ci=. if ylmotros_ci> 6500000 
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
replace `x'1=. if `x'==0 | `x'==99999999 | `x'==9999999  /*No aplicable*/
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
nivel
0 ninguno
1 pre-primario
2 primario
3 secundario
4 superior
5 universitario
6 especial

grado 
0 ninguno
0 pre-primario
1 primero
2 segundo
3 tercero
4 cuarto
5 quinto
6 sexto
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
replace yedc=10 if nivgrado==34
replace yedc=11 if nivgrado==35
replace yedc=12 if nivgrado==36



*** universitario o superior
replace yedc=13 if nivgrado==51 | nivgrado==41
replace yedc=14 if nivgrado==52 | nivgrado==42
replace yedc=15 if nivgrado==53 | nivgrado==43
replace yedc=16 if nivgrado==54 | nivgrado==44 
replace yedc=17 if nivgrado==55 
replace yedc=18 if nivgrado==56
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

* Sólo se pregunta a personas de 5-35 años de edad
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
replace luz_ch=0 if luz_ch==6

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

* Modificaciones Marcela Rubio - Septiembre 2014

/*
gen des1_ch=.
replace des1_ch=0 if servsani==5 
replace des1_ch=1 if servsani==1 | servsani==3
replace des1_ch=2 if servsani==4 | servsani==2
*/

gen des1_ch=.
replace des1_ch=0 if servsani==6
replace des1_ch=1 if servsani==1 | servsani==3
replace des1_ch=2 if servsani==4 | servsani==2


****************
****des2_ch*****
****************

* Modificaciones Marcela Rubio - Septiembre 2014

/*
gen des2_ch=des1_ch
replace des2_ch=. if des1_ch==3
*/

gen des2_ch=.
replace des2_ch=0 if servsani==6
replace des2_ch=1 if servsani==1 | servsani==3 | servsani==4 | servsani==2
replace des2_ch=2 if servsani==5
 
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
replace cuartos_ch=. if nrocuart==99
****************
***cocina_ch****
****************

gen cocina_ch=cocina
replace cocina_ch=0 if cocina==6


****************
****telf_ch****
****************

gen telef_ch=.
replace telef_ch=0 if telefono>=0
replace telef_ch=1 if telefono==11

****************
****refrig_ch***
****************

gen refrig1=heladera
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
gen automovil=coches
gen camioneta=camionet
gen auto_ch=.
replace auto_ch=1 if automovil>0 | camioneta>0
replace auto_ch=0 if automovil==0 | camioneta==0
drop automovil camioneta

****************
****compu_ch****
****************

gen compu_ch=. 

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

gen raza_ci=.

label define raza_idioma_ci 1 "Indígena" 2 "Afro-descendiente" 3 "Otros" 
label value raza_idioma_ci raza_idioma_ci 
label value raza_idioma_ci raza_idioma_ci
label var raza_idioma_ci "Raza o etnia del individuo" 

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



compress


saveold "`base_out'", replace


log close


