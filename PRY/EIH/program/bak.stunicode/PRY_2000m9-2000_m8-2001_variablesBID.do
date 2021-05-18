
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
local ENCUESTA EIH
local ANO "2000"
local ronda m9-2000_m8-2001

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
Round: Septiembre 2000- Agosto 2001
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
MAL CODIFICADO 
gen ocupa=string(ocupp)
gen ocupa_ci=real(substr(ocupa,1,1))
replace ocupa_ci=8 if ocupp==110
replace ocupa_ci=. if ocupa_ci==0 | ocupp==9999 | emp_ci~=1
drop ocupa
DURADES, CODIGO Y COMENTARIOS ANTERIORES
gen tdurades_ci=mesesbus
replace tdurades_ci=. if tdurades_ci==0
tiene un comportamiento muy estraño
son unas duraciones pero no son en numeros sino en intervalos

May 18, 2006 (Analia)
The following lines:
gen aguared_ch=(agua==4 | agua==5)
gen aguamala_ch=(agua<=3)
were changed by:
gen aguared_ch=(agua<=3)
gen aguamala_ch=(agua==4 | agua==5 | (agua>=8 & agua<=11))

*April 2nd, 2007
RAMA: from 2000 code changed (from national classification to ISIC3- 4digits) but program didn't reflect changes
previous code:

gen rama1=string(ramap)
gen rama_ci=real(substr(rama1,1,1))  
replace rama_ci=. if rama_ci==0 | emp_ci~=1
drop rama1

*/


************
* Region_c *
************
*Inclusión Mayra Sáenz - Abril 2014

gen region_c=  depto
label define region_c ///
           0 "Asunción" ///    
           1 "Concepción" ///  
           2 "San pedro" ///   
           3 "Cordillera" ///  
           4 "Guairá" ///      
           5 "Caaguazú" ///    
           6 "Caazapá" ///     
           7 "Itapúa" ///      
           8 "Misiones" ///    
           9 "Paraguarí" ///   
          10 "Alto paraná" /// 
          11 "Central" ///     
          12 "Ñeembucú" ///    
          13 "Amambay" ///     
          14 "Canindeyú" ///   
          15 "Pdte. Hayes" 
label value region_c region_c
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

gen anio_c=2001
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
label define mes_c 1 "Enero" 2 "Febrero" 3 "Marzo" 4 "Abril" 5 "Mayo"
label define mes_c 6 "Junio" 7 " Julio" 8 "Agosto",add
label define mes_c 9 "Septiembre" 10 "Octubre" 11 "Noviembre" 12 "Diciembre", add
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
replace sexo_ci=2 if sexo==2

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
replace civil_ci=3 if estcivil==3 | estcivil==6
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
replace lp_ci= 242632  if dominio==1 | dominio==2   /*area metropolitana*/
replace lp_ci= 144200 if dominio==3 | dominio==5   /*rural*/
replace lp_ci= 170397 if dominio==4  /*resto urbano*/
label var lp_ci "Linea de pobreza oficial del pais"

*********
*lpe_ci***
*********

gen lpe_ci =.
replace lpe_ci= 120068  if dominio==1 | dominio==2   /*area metropolitana*/
replace lpe_ci= 85262 if dominio==3 | dominio==5   /*rural*/
replace lpe_ci= 92142 if dominio==4  /*resto urbano*/
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
replace afiliado_ci=1 if sistjub==1
replace afiliado_ci=0 if (sistjub==6 | sistjub==.)
replace afiliado_ci = 0 if peaa == 2 
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

* Comprobacion que no se toma en cuenta el desempleo oculto. MGD 05/27/2014

gen condocup_ci=.
replace condocup_ci=1 if (trabajo==1 | almenos1==1)
replace condocup_ci=2 if (trabajo==6 | almenos1==6) & busco7==1 & trahora==1
recode condocup_ci .=3 if edad_ci>=10
recode condocup_ci .=4 if edad_ci<10

label var condocup_ci "Condicion de ocupación de acuerdo a def de cada pais"
label define condocup_ci 1 "Ocupado" 2 "Desocupado" 3 "Inactivo" 4 "Menor de PET" 
label value condocup_ci condocup_ci
*/

* No coincide cuando se calcula con la variable creada de desempleo oculto al condicionar por busqueda de trabajo;
* sin embargo si coincide con calculos externos SEDLAC. MGD 05/28/2014

gen condocup_ci=.
replace condocup_ci=1 if (pead==1 | pead==4 | pead==5) & edad_ci>=10
replace condocup_ci=2 if (pead==2 | pead==7)  & edad_ci>=10
recode condocup_ci .=3 if (pead==3 | pead==6) | (condocup_ci==. & edad_ci>=10)
recode condocup_ci (.=4) if edad_ci<10 | pead==0

label var condocup_ci "Condicion de ocupación de acuerdo a def de cada pais"
label define condocup_ci 1 "Ocupado" 2 "Desocupado" 3 "Inactivo" 4 "Menor de PET" 
label value condocup_ci condocup_ci

*************
*cesante_ci* 
*************

gen cesante_ci=1 if hatrab==1 &  peaa==2 & edad_ci>=10
replace cesante_ci=0 if hatrab==6 &  peaa==2 & edad_ci>=10
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

* PRY 2000
gen salmm_ci= 	680162 /*782186 */
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
*/

* Generacion de horas totales solo de la actividad principal. MGD 06/19/2014
egen horastotales=rsum(hrstralu hrstrama hrstrami hrstraju hrstravi hrstrasa hrstrado), missing

* Modificacion: considerando horas totales. MGD 06/19/2014
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

gen categopri_ci=.
replace categopri_ci=1 if categp==5
replace categopri_ci=2 if categp==6 
replace categopri_ci=3 if categp==1 | categp==2 | categp==3 | categp==4 | categp==8
replace categopri_ci=4 if categp==7
replace categopri_ci=. if emp_ci~=1

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

***************
***segsoc_ci***
***************

gen segsoc_ci=.
/*existe si esta asegurado, pero por el trabajo asegus*/

*****************
***nempleos_ci***
*****************

gen nempleos_ci=.
/*
*************
*firmapeq_ci*
*************
capture drop firmapeq_ci
recode tamestp (1/2=1) (3/6=0), gen(firmapeq_ci)
replace firmapeq_ci=. if tamestp>6
*/

*****************
***spublico_ci***
*****************
* 10/21/2015 MGD: corrección pequeña para que se tomen en cuenta a todos los ocupados.
gen spublico_ci=0 if emp_ci==1
replace spublico_ci=1 if (categ==1 | categ==3) & emp_ci==1

/*gen spublico_ci=.
replace spublico_ci=1 if categ ==1 | categ==3 & emp_ci==1
replace spublico_ci=0 if categ ==2 | categ>=4 & emp_ci==1
replace spublico_ci=. if emp_ci==.*/

**************
***ocupa_ci***
**************
/*no habra categoria 5 porque servicios y comerciantes estan en la misma codificacion
OCUPP: 5112 a 5230: Trabajadores de los servicios y vendedores de comercios y mercados
*/	
gen ocupa_ci=.
replace ocupa_ci=1 if (ocup>=2113 & ocup<=3480)  & emp_ci==1
replace ocupa_ci=2 if (ocup>=1110 & ocup<=1236) & emp_ci==1
replace ocupa_ci=3 if (ocup>=4111 & ocup<=4223) & emp_ci==1
replace ocupa_ci=4 if ((ocup>=5210 & ocup<=5230) | (ocup>=9111 & ocup<=9113)) & emp_ci==1
replace ocupa_ci=5 if ((ocup>=5111 & ocup<=5169) | (ocup>=9120 & ocup<=9170)) & emp_ci==1
replace ocupa_ci=6 if ((ocup>=6111 & ocup<=6153) | (ocup>=9211 & ocup<=9212)) & emp_ci==1
replace ocupa_ci=7 if ((ocup>=7111 & ocup<=8340) | (ocup>=9311 & ocup<=9334))& emp_ci==1
replace ocupa_ci=8 if ocup==110 & emp_ci==1
replace ocupa_ci=9 if (ocup>=9339 & ocup<=9999) & emp_ci==1

*************
***rama_ci***
*************

gen rama_ci=.
replace rama_ci=1 if (rama>=111 & rama<=502) & emp_ci==1
replace rama_ci=2 if (rama>=1010 & rama<=1429) & emp_ci==1
replace rama_ci=3 if (rama>=1511 & rama<=3720) & emp_ci==1
replace rama_ci=4 if (rama>=4010 & rama<=4100) & emp_ci==1
replace rama_ci=5 if (rama>=4510 & rama<=4550) & emp_ci==1
replace rama_ci=6 if (rama>=5010 & rama<=5520) & emp_ci==1
replace rama_ci=7 if (rama>=6010 & rama<=6420) & emp_ci==1
replace rama_ci=8 if (rama>=6510 & rama<=7020) & emp_ci==1
replace rama_ci=9 if (rama>=7110 & rama<=9999) & emp_ci==1


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

gen antiguedad_ci=trano+(trmeses/12)+(trsem/52) if emp_ci==1
replace antiguedad_ci=. if trano==99

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
gen categoinac_ci =1 if (pqnotra7==14 & condocup_ci==3)
replace categoinac_ci = 2 if  (pqnotra7==10 & condocup_ci==3)
replace categoinac_ci = 3 if  (pqnotra7==5 & condocup_ci==3)
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

******************
****ylnmsec_ci****
******************
gen ylnmsec_ci=.

*************
*ylmotros_ci*
*************
gen yocasio1=yotrasac
replace yocasio1=. if yocasio==0 | yocasio==99999999 /*No aplicable*/


gen ylmotros_ci= yocasio1
replace ylmotros_ci=. if emp_ci~=1
replace ylmotros_ci=. if yocasio1==.
drop yocasio1

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
*Modificación Mayra Sáenz- Marzo 2014
*Se incluye yayudex en la recodificación.
local var="yalqrent yinteres yayudafa yotrosy yjubpen yayudex"
foreach x of local var {
gen `x'1=`x'
replace `x'1=. if `x'==0 | `x'==99999999 | `x'==9999999 /*No aplicable*/
}

*sumamos ingresos extraordinarios anuales
gen yextrat1=yextrat
replace yextrat1=. if yextrat==999999999
replace yextrat1=yextrat1/12

egen ynlm_ci=rsum(yalqrent1 yinteres1 yayudafa1 yotrosy1 yjubpen1 yextrat1 yayudex1), missing
replace ynlm_ci=. if yalqrent1==. & yinteres1==. & yayudafa1==. & yotrosy1==. & yjubpen1==. & yextrat1==.

drop yalqrent1 yinteres1 yayudafa1 yotrosy1 yjubpen1


*********************************************
*Ingreso laboral no monetario otros trabajos*
*********************************************

gen ylnmotros_ci=.

*****************
***remesas_ci***
*****************

gen remesas_ci=yayudex1
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

gen rentaimp_ch=v23ede

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

*** people who have missings
gen byte yedc=.

replace yedc=. if nivgrado==9999

**<5 and no education
replace yedc=0 if nivgrado==0 | nivgrado==1002

*** preescolar o jardin o pre-primaria
replace yedc=0 if nivgrado==100 

*** primaria 
replace yedc=1 if nivgrado==201 
replace yedc=2 if nivgrado==202
replace yedc=3 if nivgrado==203 

replace yedc=4 if nivgrado==204 
replace yedc=5 if nivgrado==205 
replace yedc=6 if nivgrado==206  

*** secundaria 
replace yedc=7 if nivgrado==301 | nivgrado==207
replace yedc=8 if nivgrado==302 | nivgrado==208 
replace yedc=9 if nivgrado==303 | nivgrado==299
replace yedc=10 if nivgrado==404 | nivgrado==504 | nivgrado==604
replace yedc=11 if nivgrado==405 | nivgrado==505 | nivgrado==605
replace yedc=12 if nivgrado==406 | nivgrado==506 | nivgrado==606

*** superior no universitario  *** 
replace yedc=13 if nivgrado==701 | nivgrado==801 
replace yedc=14 if nivgrado==702 | nivgrado==802 
replace yedc=15 if nivgrado==703 | nivgrado==803 
replace yedc=16 if nivgrado==704 | nivgrado==804

*** universitario
replace yedc=13 if nivgrado==901
replace yedc=14 if nivgrado==902
replace yedc=15 if nivgrado==903
replace yedc=16 if nivgrado==904 
replace yedc=17 if nivgrado==905 
replace yedc=18 if nivgrado==906
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
label variable edus1c_ci "1er ciclo de la secundaria completo"

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
replace asiste_ci=1 if asiste>=1 & asiste<=7 
replace asiste_ci=0 if asiste==8
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

gen aguared_ch=(agua<=3)
replace aguared_ch=. if agua==99


****************
**aguadist_ch***
****************

gen aguadist_ch=lugabast


****************
**aguamala_ch***
****************

gen aguamala_ch=(agua==4 | agua==5 | (agua>=8 & agua<=11))
replace aguamala_ch=. if agua==99


****************
**aguamide_ch***
****************

gen aguamide_ch=.

****************
*****luz_ch*****
****************

* Modificaciones Marcela Rubio - Noviembre 2014
/*
gen luz_ch=luz
replace luz_ch=0 if luz_ch==2
*/

gen luz_ch = (luz==1)
replace luz_ch = . if luz==.

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

* Modificaciones Marcela Rubio - Diciembre 2014

/*
gen piso_ch=.
replace piso_ch=1 if piso>0
replace piso_ch=0 if piso==1 | piso==4
replace piso_ch=2 if piso==8 
opción 4 = lecherada es cemento
*/

gen piso_ch=.
replace piso_ch=0 if piso==1
replace piso_ch=1 if piso>=2 & piso<=7
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
g       aguamejorada_ch = 1 if (agua >=1 & agua <=3) | (agua >=6 & agua <=7)
replace aguamejorada_ch = 0 if (agua >=4 & agua <=5) | (agua >=8 & agua <=9)
		
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
****telf_ch****
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
gen automovil=tiene13+tiene14
gen auto_ch=.
replace auto_ch=1 if automovil>0
replace auto_ch=0 if automovil==0
drop automovil 

****************
****compu_ch****
****************

gen compu_ch=.
replace compu_ch=1 if tiene20>0
replace compu_ch=0 if tiene20==0

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

gen vivialqimp_ch=v23ede


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
bys idh_ch: gen aux=raza_ci if relacion_ci==1
bys idh_ch: egen aux1 = max(aux)
replace raza_ci=aux1 if (raza_ci ==. & relacion_ci ==3)  
replace raza_ci=3 if raza_ci==. 
drop aux aux1
label define raza_ci 1 "Indígena" 2 "Afro-descendiente" 3 "Otros" 
label value raza_ci raza_ci 
label value raza_ci raza_ci
label var raza_ci "Raza o etnia del individuo"
*/

*Modificación Mayra Sáenz 10/20/2015: modificaciones realizadas en base a metodología enviada por SCL/GDI Maria Olga Peña

gen raza_ci=.

gen raza_idioma_ci=.
replace raza_idioma_ci= 1 if idioma ==1 
replace raza_idioma_ci= 3 if idioma ==2 | idioma ==3 | idioma ==4 |raza_idioma_ci==.
bys idh_ch: gen aux=raza_idioma_ci if relacion_ci==1
bys idh_ch: egen aux1 = max(aux)
replace raza_idioma_ci=aux1 if (raza_idioma_ci ==. & relacion_ci ==3)  
replace raza_idioma_ci=3 if raza_idioma_ci==. 
drop aux aux1
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


/*

*********************
*** PARAGUAY 2001 ***
*********************


/*
Parentesco

 1. Jefe/a	
 2. Esposo/a/compañero/a	
 3. Hijo/a	
 4. Otro pariente	
 5. No pariente	
 6. Empleado doméstico	
*/

 gen     incl=1 if (parentco>=1 &  parentco<=5)
 replace incl=0 if  parentco==6

** AREA

 tab area [iw=factorex]

** Gender classification of the population refering to the head of the household.

 sort id_hogar nrolinea

 gen     sexo_d_=1 if parentco==1 & sexo==1
 replace sexo_d_=2 if parentco==1 & sexo==2

 egen sexo_d=max(sexo_d_), by(id_hogar)
 
 tab sexo   [iw=factorex]
 tab sexo_d [iw=factorex]

 tab sexo sexo_d if parentco==1
 
** Years of education. 
* Included in the database

** Economic Active Population 
* Included in the database (10 years or more of age)

 gen	 tasadeso=0 if peaa==1
 replace tasadeso=1 if peaa==2


************************
*** MDGs CALCULATION ***
************************

** For further information on this do file contact Pavel Luengas (pavell@iadb.org)

/*

3: ¿Cuál es el último grado o curso aprobado por ... ?
GRADO
-1: No aplicable (menores de 5 años)
 0: Ninguno
 1: Primero
 2: Segundo
 3: Tercero
 4: Cuarto
 5: Quinto
 6: Sexto
 7: Séptimo
 8: Octavo
 88: Escuela especial
 99: No responde

4: ¿Cuál es el nivel del grado o curso más alto que aprobó...?
NIVEL
-1: No aplicable (menores de 5 años)
 0: Sin instrucción / Especial
 1: Preescolar
 2: Primario
 3: Secundario Básico
 4: Bachiller Comercial
 5: Bachiller Técnico
 6: Bachiller Humanístico
 7: Formación Docente
 8: Formación Militar / Policial
 9: Universitario
 10: Educación Básica Adultos
 99: No responde

7: ¿Asiste ... actualmente a una institución de enseñanza formal?
ASISTE
-1: No aplicable (menores de 5 años)
 0: No aplicable (personas de 36 años de EDAD y más)
 1: Si, preescolar
 2: Si, primario
 3: Si, secundario
 4: Si, superior
 5: Si, universitario
 6: Si, educación de adultos
 7: Especial
 8: No asiste
 9: No responde
*/


 gen	 prim=0 if (asiste==2)
 replace prim=1 if (asiste==2 & ((grado>=0 & grado<=5) | nivel==1))

*** GOAL 2. ACHIEVE UNIVERSAL PRIMARY EDUCATION

** Target 3, Indicator: Net Attendance Ratio in Primary
* ISCED 1

 gen 	 NERP=0 if (edad>=6 & edad<=11) & (asiste>=1 & asiste<=8)
 replace NERP=1 if (edad>=6 & edad<=11) & (prim==1)
	
** Target 3, Additional Indicator: Net Attendance Ratio in Secondary
* ISCED 2 & 3

 gen	 NERS=0 if (edad>=12 & edad<=17) & (asiste>=1 & asiste<=8)
 replace NERS=1 if (edad>=12 & edad<=17) & ((asiste==2 & prim==0) | asiste==3)

** Upper secondary
   /* 
   educ. media científica (bachillerato humanístico científico)             
   educ. media técnica    (bachillerato técnico comercial)
   */

 gen     NERS2=0 if (edad>=15 & edad<=17) & (asiste>=1 & asiste<=8)
 replace NERS2=1 if (edad>=15 & edad<=17) & (asiste==3)


** Target 3, Indicator: Literacy Rate of 15-24 Years Old
* At least 5 years of formal education

 gen	 ALFABET=0 if (edad>=15 & edad<=24) & (anoest>=0 & anoest<99)
 replace ALFABET=1 if (edad>=15 & edad<=24) & (anoest>=5 & anoest<99)

** Target 3, Indicator: Literacy Rate of 15-24 Years Old
* Read & write

 gen	 ALFABET2=0 if (edad>=15 & edad<=24) & (alfabet==1 | alfabet==6)
 replace ALFABET2=1 if (edad>=15 & edad<=24) & (alfabet==1) 


*** GOAL 3 PROMOTE GENDER EQUALITY AND EMPOWER WOMEN

gen sec=1 if ((asiste==2 & prim==0) | asiste==3)
gen ter=1 if (asiste==4 | asiste==5)

** Target 4, Indicator: Ratio Girls to boys in primary, secondary and tertiary (%)

** Target 4, Ratio of Girls to Boys in Primary*

 gen RPRIMM=1 if (prim==1) & sexo==2 
 replace RPRIMM=0 if RPRIMM==. 
 gen RPRIMH=1 if (prim==1) & sexo==1 
 replace RPRIMH=0 if RPRIMH==.

 gen RATIOPRIM=0 if     (prim==1) & sexo==2  
 replace RATIOPRIM=1 if (prim==1)  & sexo==1   
	
** Target 4, Ratio of Girls to Boys in Secondary*

 gen RSECM=1 if (sec==1) & sexo==2 
 replace RSECM=0 if RSECM==.
 gen RSECH=1 if (sec==1) & sexo==1 
 replace RSECH=0 if RSECH==.

 gen RATIOSEC=0     if (sec==1) & sexo==2 
 replace RATIOSEC=1 if (sec==1) & sexo==1  
	
** Target 4, Indicator: Ratio of Girls to Boys in Tertiary*

 gen RTERM=1 if (ter==1) & sexo==2 
 replace RTERM=0 if RTERM==.
 gen RTERH=1 if (ter==1) & sexo==1 
 replace RTERH=0 if RTERH==.

 gen RATIOTER=0     if (ter==1) & sexo==2 
 replace RATIOTER=1 if (ter==1) & sexo==1  



** Target 4, Indicator: Ratio of Girls to Boys in Primary, Secondary and Tertiary*

 noisily display "All"
 gen RALLM=1 if (prim==1 | sec==1 | ter==1) & sexo==2 
 replace RALLM=0 if RALLM==.
 gen RALLH=1 if (prim==1 | sec==1 | ter==1) & sexo==1 
 replace RALLH=0 if RALLH==.

 gen     RATIOALL=0 if (prim==1 | sec==1 | ter==1) & sexo==2  
 replace RATIOALL=1 if (prim==1 | sec==1 | ter==1) & sexo==1    


** Target 4, Indicator: Ratio of literate women to men 15-24 year olds*
* Knows how to read & write


 gen MA2=1 if ((alfabet==1) & (edad>=15 & edad<=24) & (sexo==2)) 
 replace MA2=0 if MA2==.
 gen HA2=1 if ((alfabet==1) & (edad>=15 & edad<=24) & (sexo==1)) 
 replace HA2=0 if HA2==.

 gen     RATIOLIT2=0 if ((alfabet==1) & (edad>=15 & edad<=24) & (sexo==2)) 
 replace RATIOLIT2=1 if ((alfabet==1) & (edad>=15 & edad<=24) & (sexo==1)) 


** Target 4, Indicator: Ratio of literate women to men 15-24 year olds*
* At least 5 years of formal education


 gen MA=1 if ((anoest>=5 & anoest<99) & (edad>=15 & edad<=24) & (sexo==2)) 
 replace MA=0 if MA==.
 gen HA=1 if ((anoest>=5 & anoest<99) & (edad>=15 & edad<=24) & (sexo==1)) 
 replace HA=0 if HA==.

 gen     RATIOLIT=0 if ((anoest>=5 & anoest<99) & (edad>=15 & edad<=24) & (sexo==2)) 
 replace RATIOLIT=1 if ((anoest>=5 & anoest<99) & (edad>=15 & edad<=24) & (sexo==1)) 


** Target 4, Indicator: Share of women in wage employment in the non-agricultural sector (%)
/*
12: ¿En esta ocupación trabaja como:....?
CATEGP CATEGORÍA OCUPACIONAL
0: No aplicable
1: Empleado público
2: Empleado privado
3: Obrero público
4: Obrero privado
5: Empleador o patrón
6: Trabajador por cuenta propia
7: Trabajador familiar no remunerado
8: Empleado doméstico
9: No responde

*/

* Without Domestic Service


 gen	 WENAS=0 if (edad>=15 & edad<=64) & (categp>=1 & categp<=4) & (ramar>=2 & ramar<=9) & (peaa==1)
 replace WENAS=1 if (edad>=15 & edad<=64) & (categp>=1 & categp<=4) & (ramar>=2 & ramar<=9) & (peaa==1) & (sexo==2)


** Target 4, Indicator: Share of women in wage employment in the non-agricultural sector (%)
* With domestic servants


 gen	 WENASD=0 if (edad>=15 & edad<=64) & ((categp>=1 & categp<=4) | (categp==8)) & (ramar>=2 & ramar<=9) & (peaa==1)
 replace WENASD=1 if (edad>=15 & edad<=64) & ((categp>=1 & categp<=4) | (categp==8)) & (ramar>=2 & ramar<=9) & (peaa==1) & (sexo==2)


*Proportion of 1 Year Old Children Immunized Against Measles*
/*
19: ¿ …(NOMBRE)… tiene la vacuna antisarampión?
0: No aplicable (personas de 5 años de edad y más)
1: Si
2: No sabe
6: No
9: No responde
*/


 gen	 MEASLES=0 if (edad>=1 & edad<=4) & (antisa==1 | antisa==6)
 replace MEASLES=1 if (edad>=1 & edad<=4) & (antisa==1)


*Proportion of Births Attended by Skilled Health Personnel*
/*
Pregunta 1: ¿ …(NOMBRE)… nació en un … ?
NACIOEN
0: No aplicable (personas de 5 años de edad y más)
1: Hospital
2: Centro de salud
3: Puesto de salud
4: Clínica o sanatorio privado
5: En casa de partera
6: En su casa
7: Otro
9: No responde
*/


 gen	 SKILLED=0 if (nacioen>=1 & nacioen<=9)
 replace SKILLED=1 if (nacioen>=1 & nacioen<=5) 


*** GOAL 7 ENSURE ENVIROMENTAL SUSTAINABILITY

** Access to Electricity ** Additional Indicator
/*
9. Corriente eléctrica. ¿Dispone de luz eléctrica?
 1. Si
 6. No
*/

* Gender classification of the population refers to the head of the household.


 gen	 ELEC=0 if (luz==1 | luz==6) /* Total population excluding missing information */
 replace ELEC=1 if (luz==1)

 
** Target 9, Indicator: Proportion of the population using solidfuels (%)
/*
15b: Combustible que utiliza habitualmente para
cocinar)
1: Leña
2: Carbón
3: Gas
5: Alcohol
6: Electricidad
7: Otro
8: No cocina
*/

* Gender classification of the population refers to the head of the household.


 gen	 SFUELS=0 if (combusti>=1 & combusti<=8) /* Total population excluding missing information */
 replace SFUELS=1 if (combusti==1 | combusti==2)


** Target 10, Indicator: Proportion of the population with sustainable access to an improved water source (%)

/*
6: ¿De dónde proviene el agua que utiliza?)
1: Corposana 		==>7 ==>8
2: Senasa 		==>7 ==>8
3: Red privada 		==>7 ==>8
4: Arroyo - río 	==>8
5: Ycuá o manantial 	==>8
6: Pozo con bomba	==>8
7: Pozo sin bomba 	==>8
8: Aguatero 		==>7 ==>8
9: Otro 		==>7 ==>8
10: Aljibe con bomba 	==>7 ==>8
11: Aljibe sin bomba 	==>7 ==>8
99: No responde 	==>7 ==>8

8. Lugar de abastecimiento
1: Dentro de la vivienda
2: Dentro de la propiedad
3: Fuera de la propiedad

*/

* Gender classification of the population refers to the head of the household.


 gen	 WATER=0 if (agua>=1 & agua<=11) /* Total population excluding missing information */
 replace WATER=1 if ((agua>=1 & agua<=3) | (agua>=6 & agua<=8) | (agua>=10 & agua<=11))

 	
** Target 10, Indicator: Proportion of Population with Access to Improved Sanitation, Urban and Rural (%)
/*
14: ¿Tiene servicio sanitario?
1: W.C. conectado a red pública
2: W.C. con pozo ciego
3: Excusado tipo municipal
4: Letrina común
5: Otro
6: No tiene

*/

* Gender classification of the population refers to the head of the household.


 gen	 SANITATION=0 if (servsani>=1 & servsani<=6) /* Total population excluding missing information */
 replace SANITATION=1 if (servsani==1 | servsani==2)

 	
** Target 11, Indicator: Proportion of the population with access to secure tenure (%)

/*

1. Tipo de vivienda		18. Durante el último año, ¿Cuál ha sido la situación legal de la vivienda?
						
1. Casa				1. Propia 
2. Rancho 			2. Pagando en cuotas 
3. Dpto. o piso 		3. Propiedad en condominio 
4. Pieza de inquilinato 	4. Arrendatario o inquilino 
5. Vivienda improvisada 	5. Ocupante de hecho 
6. Otro (especificar) 		6. Cedida
				7. Otra

4. PISO				3. PARED
v04				v03
1. Tierra 			1. Estaqueo 
2. Madera			2. Adobe 
3. Ladrillo 			3. Madera
4. Lecherada 			4. Ladrillo 
5. Baldosa 	 		5. Piedra 
6. Cerámica			6. Otro
7. Granito 				
8. Otro		


2a. Número de pieza
(No incluya baño, cocina, cuartos o
piezas destinadas exclusivamente al
comercio o industria)

*/

 gen persroom=totpers/nrocuart

* Indicator components

* 1. Non secure tenure or type of dwelling.

 gen     secten_1=0 if ((tenencia>=1 & tenencia<=7) & (tipoviv>=1 & tipoviv<=6)) /* Total population excluding missing information */
 replace secten_1=1 if ((tenencia>=5 & tenencia<=7) | (tipoviv==2 | tipoviv==5 | tipoviv==6))

* 2. Low quality of the floor or walls materials.

 gen     secten_2=0 if ((pared>=1 & pared<=6) & (piso>=1 & piso<=8)) /* Total population excluding missing information */
 replace secten_2=1 if ((pared==1) 	      | (piso==1)) 

* 3. Crowding (defined as not more than two people sharing the same room)

 gen secten_3=1     if (persroom>2) 

* 4. Lack of basic services

 gen secten_4=1	    if (SANITATION==0 | WATER==0) 

* Gender classification of the population refers to the head of the household.

 gen     SECTEN=1 if (secten_1>=0 & secten_1<=1) & (secten_2>=0 & secten_2<=1) /* Total population excluding missing information */
 replace SECTEN=0 if (secten_1==1 | secten_2==1 | secten_3==1 | secten_4==1)


* Dirt floors ** Additional indicator

* Gender classification of the population refers to the head of the household.


 gen	 DIRT=0 if (piso>=1 & piso<=8) /* Total population excluding missing information */
 replace DIRT=1 if (piso==1)

 	
** GOAL 8. DEVELOP A GLOBAL PARTNERSHIP FOR DEVELOPMENT

** Target 16, Indicator: Unemployment Rate of 15 year-olds (%)

 gen	 UNMPLYMENT15=0 if (edad>=15 & edad<=24) & (tasadeso==0 | tasadeso==1)
 replace UNMPLYMENT15=1 if (edad>=15 & edad<=24) & (tasadeso==1) 

** Target 18, Indicator: "Telephone lines and celullar subscribers per 100 population"
* 27: ¿Tiene el hogar algunos de los siguientes bienes utilizados mayormente por el hogar?


* Gender classification of the population refers to the head of the household.

 gen     TELCEL=0 if (lineaf==1 | lineaf==6) & (celular==1 | celular==6)	/* Total population excluding missing information */
 replace TELCEL=1 if (lineaf==1 | celular==1) 

** FIXED LINES

* Gender classification of the population refers to the head of the household.

 gen     TEL=0 if (lineaf==1 | lineaf==6)	/* Total population excluding missing information */
 replace TEL=1 if (lineaf==1) 


** CEL LINES

* Gender classification of the population refers to the head of the household.

 gen     CEL=0 if (celular==1 | celular==6)	/* Total population excluding missing information */
 replace CEL=1 if (celular==1)

** Target 18, Indicator: "Personal computers in use per 100 population"
* 27: ¿Tiene el hogar algunos de los siguientes bienes utilizados mayormente por el hogar?
* 20 Computadora

* Gender classification of the population refers to the head of the household.

 gen	 COMPUTER=0 if (tiene20>=0 & tiene20<=10)
 replace COMPUTER=1 if (tiene20>=1 & tiene20<=10)
 	
** Target 18, Indicator: "Internet users per 100 population"
* 27: ¿Tiene el hogar algunos de los siguientes bienes utilizados mayormente por el hogar?

* Gender classification of the population refers to the head of the household.

 gen	 INTUSERS=0 if (internet==1 | internet==6)
 replace INTUSERS=1 if (internet==1)

************************************************************************
**** ADDITIONAL SOCIO - ECONOMIC COMMON COUNTRY ASESSMENT INDICATORS ****
************************************************************************

** CCA 19. Proportion of children under 15 who are working
* 12 to 14

 gen	 CHILDREN=0 if (edad>=12 & edad<=14) 
 replace CHILDREN=1 if (edad>=12 & edad<=14) & (peaa==1)

** CCA 41 Number of Persons per Room*

 generate PERSROOM2=persroom if parentco==1

 gen     popinlessthan2=1 if persroom<=2
 replace popinlessthan2=0 if popinlessthan2==.

* Gender classification of the population refers to the head of the household.

 gen     PLT2=0 if persroom<.		/* Total population excluding missing information */
 replace PLT2=1 if (popinlessthan2==1)

******************************************************
**** ADDITIONAL INDICATORS RELATED TO EDUCATION ******
******************************************************

*** Proportion of population below corresponding grade for age

 gen     rezago=0       if (anoest>=0 & anoest<99)  & edad==6 /* This year of age is not included in the calculations */
	 
 replace rezago=1 	if (anoest>=0 & anoest<1 )  & edad==7
 replace rezago=0 	if (anoest>=1 & anoest<99)  & edad==7
 
 replace rezago=1 	if (anoest>=0 & anoest<2 )  & edad==8
 replace rezago=0	if (anoest>=2 & anoest<99)  & edad==8

 replace rezago=1 	if (anoest>=0 & anoest<3 )  & edad==9
 replace rezago=0	if (anoest>=3 & anoest<99)  & edad==9

 replace rezago=1 	if (anoest>=0 & anoest<4 )  & edad==10
 replace rezago=0	if (anoest>=4 & anoest<99)  & edad==10

 replace rezago=1 	if (anoest>=0 & anoest<5 )  & edad==11
 replace rezago=0	if (anoest>=5 & anoest<99)  & edad==11

 replace rezago=1	if (anoest>=0 & anoest<6)   & edad==12
 replace rezago=0	if (anoest>=6 & anoest<99)  & edad==12

 replace rezago=1 	if (anoest>=0 & anoest<7)   & edad==13
 replace rezago=0	if (anoest>=7 & anoest<99)  & edad==13

 replace rezago=1 	if (anoest>=0 & anoest<8)   & edad==14
 replace rezago=0	if (anoest>=8 & anoest<99)  & edad==14

 replace rezago=1 	if (anoest>=0 & anoest<9 )  & edad==15
 replace rezago=0	if (anoest>=9 & anoest<99)  & edad==15

 replace rezago=1 	if (anoest>=0  & anoest<10) & edad==16
 replace rezago=0	if (anoest>=10 & anoest<99) & edad==16

 replace rezago=1 	if (anoest>=0  & anoest<11) & edad==17
 replace rezago=0	if (anoest>=11 & anoest<99) & edad==17

* Primary and Secondary [ISCED 1, 2 & 3]

 gen     REZ=0 if (edad>=7 & edad<=17) & (rezago==1 | rezago==0)
 replace REZ=1 if (edad>=7 & edad<=17) & (rezago==1)
		
* Primary completion rate [15 - 24 years of age]

 gen     PRIMCOMP=0 if (edad>=15 & edad<=24) & (anoest>=0  & anoest<99)
 replace PRIMCOMP=1 if (edad>=15 & edad<=24) & (anoest>=6  & anoest<99)


* Average years of education of the population 15+

 gen     AEDUC_15=anoest if ((edad>=15 & edad<.) & (anoest>=0 & anoest<99))
 gen     AEDUC_15_24=anoest if ((edad>=15 & edad<=24) & (anoest>=0 & anoest<99))
 gen     AEDUC_25=anoest if ((edad>=25 & edad<.) & (anoest>=0 & anoest<99))


* Grade for age

 gen GFA=(anoest/(edad-6)) if (edad>=7 & edad<=17) & (anoest>=0 & anoest<99)

* Grade for age primary

 gen GFAP=(anoest/(edad-6)) if (edad>=7 & edad<=11) & (anoest>=0 & anoest<99)

* Grade for age Secondary

 gen GFAS=(anoest/(edad-6)) if (edad>=12 & edad<=17) & (anoest>=0 & anoest<99)

*/

ren ocup ocup_old

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


