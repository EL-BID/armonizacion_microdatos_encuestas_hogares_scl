
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
local ENCUESTA EPH
local ANO "2004"
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
Encuesta: EPH
Round: Agosto-Noviembre
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
codigo extraño para la creacion de la variable desemp
gen desemp2_ci=(desemp1_ci==1)
replace desemp1_ci=1 if a05==6>?????
ANTIGUEDAD
*b08 es cuanto tiempo de su vida ha trabajado en esta ocupacion 
*b11 es cuanto lleva trabajando en la empresa
* antiguedad_ci ES ANUAL!!!!
gen antiguedad_ci=b08a*12+b08m+b08s/4 if b08a~=9 & b08m~=99 & b08s~=9

April 10, 2006 (Analia)
The following line was added:
replace remesas_ci=. if e01g>=999999999

The following command:
gen des1_ch=.
replace des1_ch=0 if v12==2 
replace des1_ch=1 if v12==1 
replace des1_ch=2 if v12>=3 &  v12<=6 
replace des1_ch=3 if v12==7
was replaced with
replace des1_ch=0 if v10==5 
replace des1_ch=1 if v10==1 | v10==3
replace des1_ch=2 if v10==4 | v10==2
Since v12 is the variable that categorizes bargage recollection!

March, 2008 (Revised) tede
*/



************
* Region_c *
************
*Inclusión Mayra Sáenz - Abril 2014

/*gen region_c=  dpto
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
          15 "Pdte. Hayes" */
		  
*Modificación Mayra Sáenz - Septiembre 2014		  
gen region_c    = 1 if dpto == 0
replace region_c= 2 if dpto == 2		  
replace region_c= 3 if dpto == 5		  
replace region_c= 4 if dpto == 7
replace region_c= 5 if dpto == 10
replace region_c= 6 if dpto == 11
*replace region_c= 7 if dpto == 1 | dpto == 3 | dpto == 4 | dpto == 6 | dpto == 8 | dpto == 9 | (dpto >= 12 & dpto >= 15) 
replace region_c= 7 if region_c == .
label define region_c ///
1 "Asunción" ///
2 "San Pedro" ///
3 "Caaguazú" ///
4 "Itapúa" ///
5 "Alto Paraná" ///
6 "Central" ///
7 "Resto" 
label value region_c region_c

label var region_c "División política"
		  
***************
***factor_ch***
***************

gen factor_ch=fex 
label variable factor_ch "Factor de expansion del hogar"


***************
****idh_ch*****
***************
gen upms=string(upm)
gen nvivis=string(nvivi)
gen nhogas=string(nhoga)
gen idh_ch=upms+nvivis+nhogas
sort idh_ch
label variable idh_ch "ID del hogar"
drop upms nvivis nhogas

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

gen anio_c=2004
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
*1995 y 1997-2004 es nacional (N)

gen muestra_AMA=0
replace muestra_AMA=1 if dpto==0
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

gen paren=p03
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

gen factor_ci=fex 
label variable factor_ci "Factor de expansion del individuo"

**********
***sexo***
**********

gen sexo_ci=.
replace sexo_ci=1 if p06==1
replace sexo_ci=2 if p06==6

label define sexo_ci 1 "Hombre" 2 "Mujer"
label value sexo_ci sexo_ci

**********
***edad***
**********

gen edad_ci=p02
label variable edad_ci "Edad del individuo"


*****************
***civil_ci***
*****************
/*
     estado |
      civil |      Freq.     Percent        Cum.
------------+-----------------------------------
 1casado     |     10,673       24.73       24.73
 2unido      |      4,324       10.02       34.75
 3separado   |        613        1.42       36.17
 4viudo      |      1,070        2.48       38.65
 5soltero    |     26,418       61.21       99.85
 6divorciado |         63        0.15      100.00
------------+-----------------------------------
      Total |     43,161      100.00
*/

gen estcivil=p09
gen civil_ci=.
replace civil_ci=1 if estcivil==5
replace civil_ci=2 if estcivil==1 | estcivil==2 
replace civil_ci=3 if estcivil==3 | estcivil==6 
replace civil_ci=4 if estcivil==4 

label variable civil_ci "Estado civil"
label define civil_ci 1 "Soltero" 2 "Union formal o informal"
label define civil_ci 3 "Divorciado o separado" 4 "Viudo" , add
label value civil_ci civil_ci
drop estcivil

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


************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/

*********
*lp_ci***
*********

gen lp_ci =.
replace lp_ci= 322823  if dominio==1 | dominio==2   /*area metropolitana*/
replace lp_ci= 193349 if dominio==3 | dominio==5   /*rural*/
replace lp_ci= 227552 if dominio==4  /*resto urbano*/
label var lp_ci "Linea de pobreza oficial del pais"

*********
*lpe_ci***
*********

gen lpe_ci =.
replace lpe_ci= 166251  if dominio==1 | dominio==2   /*area metropolitana*/
replace lpe_ci= 118058 if dominio==3 | dominio==5   /*rural*/
replace lpe_ci= 127583 if dominio==4  /*resto urbano*/
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
replace afiliado_ci=1 if b10==1
replace afiliado_ci=0 if (b10==6 | b10==.)
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
gen condocup_ci=.
replace condocup_ci=1 if peaa==1
replace condocup_ci=2 if peaa==2
replace condocup_ci=3 if peaa==3 & edad_ci>=10
replace condocup_ci=. if peaa == 0
replace condocup_ci=4 if edad_ci<10 

label var condocup_ci "Condicion de ocupación de acuerdo a def de cada pais"
label define condocup_ci 1 "Ocupado" 2 "Desocupado" 3 "Inactivo" 4 "Menor de PET" 
label value condocup_ci condocup_ci
*/


* Comprobacion que no se toma en cuenta el desempleo oculto. MGD 05/27/2014
* No coincide con la variable creada pead ya que se condiciona por busqueda de trabajo y disponibilidad para trabajar.
gen condocup_ci=.
replace condocup_ci=1 if (pead==1 | pead==4 | pead==5)
replace condocup_ci=2 if (pead==2 | pead==7) & edad>=10
recode condocup_ci .=3 if (pead==3 | pead==6) | (condocup_ci==. & edad>=10)
recode condocup_ci .=4 if edad_ci<10

label var condocup_ci "Condicion de ocupación de acuerdo a def de cada pais"
label define condocup_ci 1 "Ocupado" 2 "Desocupado" 3 "Inactivo" 4 "Menor de PET" 
label value condocup_ci condocup_ci

*************
*cesante_ci* 
*************

gen cesante_ci=1 if a06==1 &  peaa==2 & edad_ci>=10
replace cesante_ci=0 if a06==6 &  peaa==2 & edad_ci>=10
label var cesante_ci "Desocupado - definicion oficial del pais"	

*************
**pension_ci*
*************

gen pension_ci=1 if (e01hde>0 & e01hde<. & e01hde!=999999999) 
recode pension_ci .=0 
label var pension_ci "1=Recibe pension contributiva"

*************
*ypen_ci*
*************
replace e01hde=. if (e01hde >= 999999999 & e01hde!=.)
gen ypen_ci=e01hde 
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
gen tc_ci=5988.750000
label var tc_ci "Tipo de cambio LCU/USD"

*************
**salmm_ci***
*************

* PRY 2004
gen salmm_ci= 	972413
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
gen subemp_ci=0
egen tothoras=rsum(b03lu b03ma b03mi b03ju b03vi b03sa b03do), missing
replace subemp_ci=1 if tothoras<=30 & d02==1 & emp_ci==1 



*****************
***horaspri_ci***
*****************

egen hr_seman=rsum(b03lu b03ma b03mi b03ju b03vi b03sa b03do), missing
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
replace tiempoparc_ci=1 if tothoras>=30 & d02==1 & emp_ci==1 */

*10/21/15 MGD: corrección de sintaxis
gen tiempoparc_ci=0
replace tiempoparc_ci=1 if horaspri_ci>=1 & horaspri_ci<30 & emp_ci==1 & d02==1

******************
***categopri_ci***
******************
/*
-----------------------
categoria de          |
ocupacion (act.       |
principal)            |
----------------------+
0-na                   
1empleado público      
2empleado privado      
3obrero público        
4obrero privado        
5empleador o patrón    
6cuenta propia         
7famil. no remunerado  
8empleado doméstico    
-----------------------
*/
gen categopri_ci=.
replace categopri_ci=1 if b12==5
replace categopri_ci=2 if b12==6 
replace categopri_ci=3 if b12==1 | b12==2 | b12==3 | b12==4 | b12==8
replace categopri_ci=4 if b12==7
replace categopri_ci=. if emp_ci~=1

label define categopri_ci 1 "Patron" 2 "Cuenta propia" 
label define categopri_ci 3 "Empleado" 4 " Familiar no remunerado" , add
label value categopri_ci categopri_ci
label variable categopri_ci "Categoria ocupacional trabajo principal"

******************
***categosec_ci***
******************

gen categosec_ci=.
replace categosec_ci=1 if c07==5
replace categosec_ci=2 if c07==6 
replace categosec_ci=3 if c07==1 | c07==2 | c07==3 | c07==4| c07==8
replace categosec_ci=4 if c07==7
replace categosec_ci=. if emp_ci~=1 | b22~=1

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
replace nempleos_ci=1 if b22==6 /* b23 for 2006 */
replace nempleos_ci=2 if b22==1
replace nempleos_ci=. if emp_ci!=1
/*
*************
*firmapeq_ci*
*************
capture drop firmapeq_ci
recode b09 (1/2=1) (3/6=0), gen(firmapeq_ci)
replace firmapeq_ci=. if b09>6
*/

*****************
***spublico_ci***
*****************
* 10/21/2015 MGD: corrección pequeña para que se tomen en cuenta a todos los ocupados.
gen spublico_ci=0 if emp_ci==1
replace spublico_ci=1 if (cate==1 | cate==3) & emp_ci==1

/*gen spublico_ci=.
replace spublico_ci=1 if cate ==1 | cate==3 & emp_ci==1
replace spublico_ci=0 if cate ==2 | cate>=4 & emp_ci==1
replace spublico_ci=. if emp_ci==.*/

**************
***ocupa_ci***
**************
/*no habra categoria 5 porque servicios y comerciantes estan en la misma codificacion
OCUP: 5112 a 5230: Trabajadores de los servicios y vendedores de comercios y mercados
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
g rama_ci=.
replace rama_ci=1 if (b02>=111 & b02<=500) & emp_ci==1
replace rama_ci=2 if (b02>=1010 & b02<=1429) & emp_ci==1
replace rama_ci=3 if (b02>=1511 & b02<=3720) & emp_ci==1
replace rama_ci=4 if (b02>=4010 & b02<=4100) & emp_ci==1
replace rama_ci=5 if (b02>=4510 & b02<=4550) & emp_ci==1
replace rama_ci=6 if (b02>=5010 & b02<=5520) & emp_ci==1
replace rama_ci=7 if (b02>=6010 & b02<=6420) & emp_ci==1
replace rama_ci=8 if (b02>=6511 & b02<=7020) & emp_ci==1
replace rama_ci=9 if (b02>=7111 & b02<=9999) & emp_ci==1
/*
gen rama1=string(b02)
gen rama_ci=real(substr(rama1,1,1))  
replace rama_ci=. if rama_ci==0 | emp_ci~=1
drop rama1
*/
****************
***durades_ci***
****************

recode a16a (9=.)
recode a16m (99=.)
recode a16s (9=.)
gen durades_ci=(a16a*12)+a16m+(a16s/4.3 )
replace durades_ci=. if durades_ci==0


*******************
***antiguedad_ci***
*******************
*b08 es cuanto tiempo de su vida ha trabajado en esta ocupacion 
*b11 es cuanto lleva trabajando en la empresa
recode b11a b11m b11s (99=.)
gen antiguedad_ci=.
replace antiguedad_ci=b11a+(b11m/12)+(b11s/52) if emp_ci==1

*************
*tamemp_ci
*************
/*
b09:
           0 na                
           1 solo              
           2 2 a 5 personas    
           3 6 a 10 personas   
           4 11 a 20 personas  
           5 21 a 50 personas  
           6 más de 50 pers.   
           7 empleado doméstico
           9 nr                

*/
*Paraguay Pequeña 1 a 5, Mediana 6 a 50, Grande Más de 50

gen tamemp_ci = 1 if b09>=1 & b09<=2
replace tamemp_ci = 2 if (b09>=3 & b09<=5)
replace tamemp_ci = 3 if (b09==6)

label define tamemp_ci 1 "Pequeña" 2 "Mediana" 3 "Grande"
label value tamemp_ci tamemp_ci
label var tamemp_ci "Tamaño de empresa"

*******************
***categoinac_ci*** 
*******************
gen categoinac_ci =1 if (a05==14 & condocup_ci==3)
replace categoinac_ci = 2 if  (a05==10 & condocup_ci==3)
replace categoinac_ci = 3 if  (a05==5 & condocup_ci==3)
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
gen ylmpri_ci=e01aimde if emp_ci==1 
replace ylmpri_ci=. if e01aimde<=0 | e01aimde>=999999999
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

gen ysec1=e01bimde
replace ysec1=. if ysec==0 | ysec>=999999999 /*No aplicable*/


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


gen yocasio1=e01cimde
replace yocasio1=. if yocasio==0 | yocasio>=999999999 /*No aplicable*/

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
/*
local var="e01d e01e e01f e01g e01h e01i e01j"
foreach x of local var {
gen `x'1=`x'
replace `x'1=. if `x'==0 | `x'>=999999999 /*No aplicable*/
}


egen ynlm_ci=rsum(e01d1 e01e1 e01f1 e01g1 e01h1 e01i1 e01j1)
replace ynlm_ci=. if e01d1==. & e01e1==. & e01f1==. & e01g1==. & e01h1==. & e01i1==. & e01j1==. 

drop e01d1 e01e1 e01f1 e01g1 e01h1 e01i1 e01j1

*/

*Modificación Mayra Sáenz - Septiembre 2013
* Se elimina las remesas del cálculo del ingreso no laboral monetario del individuo (ynlm_ci), porque se està duplicando 
* al momento de generar el ingreso no laboral monetario del hogar (ynlm_ch), en donde se suma ynlm_ci y remesas_ci.
local var="e01dde e01ede e01fde e01hde e01ide e01jde"
foreach x of local var {
gen `x'1=`x'
replace `x'1=. if `x'==0 | `x'>=999999999 /*No aplicable*/
}

* No corre la sintaxis en esta parte: observacion: no existe la variable e01g1) MGD 4/28/2014
/*
egen ynlm_ci=rsum(e01d1 e01e1 e01f1 e01h1 e01i1 e01j1 e01g1), missing
replace ynlm_ci=. if e01d1==. & e01e1==. & e01f1==. & e01h1==. & e01i1==. & e01j1==. 

drop e01d1 e01e1 e01f1 e01h1 e01i1 e01j1
*/
egen ynlm_ci=rsum(e01dde1 e01ede1 e01fde1 e01hde1 e01ide1 e01jde1), missing
replace ynlm_ci=. if e01dde1==. & e01ede1==. & e01fde1==. & e01hde1==. & e01ide1==. & e01jde1==. 

drop e01dde1 e01ede1 e01fde1 e01hde1 e01ide1 e01jde1

*********************************************
*Ingreso laboral no monetario otros trabajos*
*********************************************

gen ylnmotros_ci=.

*****************
***remesas_ci***
*****************

gen remesas_ci=e01gde
replace remesas_ci=. if e01gde>=999999999
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

gen rentaimp_ch=v20ede /*Si tuviera que alquilar esta vivienda cuanto estimaría que le Pag.. por mes*/
replace rentaimp_ch=. if v20ede>=15000000

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

replace yedc=. if ed43==9999

**<5 and no education
replace yedc=0 if ed43==0 | ed43==100

*** preescolar o jardin o pre-primaria
replace yedc=0 if ed43>=200 & ed43<=300

*** primaria 
replace yedc=1 if ed43==301 
replace yedc=2 if ed43==302 
replace yedc=3 if ed43==303 

replace yedc=4 if ed43==304 
replace yedc=5 if ed43==305 
replace yedc=6 if ed43==306 

*** secundaria 
replace yedc=7 if ed43==407 | ed43==501
replace yedc=8 if ed43==408 | ed43==502
replace yedc=9 if ed43==409 | ed43==503
replace yedc=10 if ed43==604 | ed43==704 | ed43==801 | ed43==901 
replace yedc=11 if ed43==605 | ed43==705 | ed43==802 | ed43==902 
replace yedc=12 if ed43==606 | ed43==706

*** superior no universitario  *** 
replace yedc=13 if ed43==1101 | ed43==1201 | ed43==1301
replace yedc=14 if ed43==1102 | ed43==1202 | ed43==1302
replace yedc=15 if ed43==1103 | ed43==1203 | ed43==1303
replace yedc=16 if ed43==1204 | ed43==1304

*** universitario
replace yedc=13 if ed43==1401
replace yedc=14 if ed43==1402
replace yedc=15 if ed43==1403
replace yedc=16 if ed43==1404 
replace yedc=17 if ed43==1405 
replace yedc=18 if ed43==1406
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

replace asiste_ci=1 if ed07>=1 & ed07<=9 /*ed07 for 2004*/
replace asiste_ci=0 if ed07==10
label variable asiste_ci "Asiste actualmente a la escuela"

*****************
***pqnoasis_ci***
*****************

gen pqnoasis_ci=ed10 /*ed10 for 2004*/

**************
*pqnoasis1_ci*
**************
**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**

g       pqnoasis1_ci = 1 if ed10==1 | ed10==4
replace pqnoasis1_ci = 2 if ed10==2
replace pqnoasis1_ci = 3 if ed10==12 | ed10==13 | ed10==14
replace pqnoasis1_ci = 4 if ed10==15
replace pqnoasis1_ci = 5 if ed10==3 
replace pqnoasis1_ci = 6 if ed10==6
replace pqnoasis1_ci = 7 if ed10==5
replace pqnoasis1_ci = 8 if ed10==7  | ed10==8  | ed10==9 | ed10==10 | ed10==11
replace pqnoasis1_ci = 9 if ed10==16 | ed10==17 | ed10==18

label define pqnoasis1_ci 1 "Problemas económicos" 2 "Por trabajo" 3 "Problemas familiares o de salud" 4 "Falta de interés" 5	"Quehaceres domésticos/embarazo/cuidado de niños/as" 6 "Terminó sus estudios" 7	"Edad" 8 "Problemas de acceso"  9 "Otros"
label value  pqnoasis1_ci pqnoasis1_ci


***************
***repite_ci***
***************

*** ed14: Repitió algún grado o curso? ***

gen repite_ci=.
replace repite_ci=1 if ed14==1
replace repite_ci=2 if ed14==6
replace repite_ci=0 if ed14==0
replace repite_ci=9 if ed14==9

label define repite_ci 1"Si" 6"No" 
label define repite_ci 0"NA" 9"NR", add
label variable repite_ci "Esta repitendo el grado o curso"

******************
***repiteult_ci***
******************

*** Está repitiendo actualmente algún grado o curso del año lectivo anterior? ED13 ***

gen repiteult_ci=.
replace repiteult_ci=1 if ed13==1
replace repiteult_ci=2 if ed13==6
replace repiteult_ci=0 if ed13==0
replace repiteult_ci=9 if ed13==9

label define repiteult_ci 1 Si 2 No 
label define repiteult_ci 0 NA 9 NR, add
label variable repiteult_ci "Esta repitendo ultimo grado o curso"


***************
***edupub_ci***
***************

gen edupub_ci=1 if ed08==1 /*ed08 for 2004*/
replace edupub_ci=0 if ed08>=2

***************
***tecnica_ci**
***************

gen tecnica_ci=.
replace tecnica_ci=1 if ed04==11
recode tecnica_ci .=0 
label var tecnica_ci "1=formacion terciaria tecnica"


********************************************
***Variables de Infraestructura del hogar***
********************************************

***************
**aguared_ch***
***************

gen aguared_ch=(v06==1 | v06==5) /* v06==1 | v06==5 for 2004 */


****************
**aguadist_ch***
****************

gen aguadist_ch=1 if v08==2  /* v08 for 2004 */
replace aguadist_ch=2 if v08==1|v08==3
replace aguadist_ch=3 if v08>3

****************
**aguamala_ch***
****************

gen aguamala_ch=(v06~=5 | v06~=1) /* v06~=5 | v06~=1 for 2004 */


****************
**aguamide_ch***
****************

gen aguamide_ch=.

****************
*****luz_ch*****
****************

gen luz_ch=v09 /* v09 for 2004 */
replace luz_ch=0 if luz_ch==6

****************
***luzmide_ch***
****************

gen luzmide_ch=.

****************
***combust_ch***
****************

gen combust_ch=(v15b==4 | v15b==2) /* v15b for 2006 */

****************
****bano_ch*****
****************

gen bano_ch=(v13==1) /* v13 for 2004 */

****************
****banoex_ch***
****************

gen banoex_ch=.

****************
****des1_ch*****
****************

* Modificaciones Marcela Rubio - Noviembre 2014
/*
gen des1_ch=.
replace des1_ch=0 if v14==5 /* v14 for 2004 */
replace des1_ch=1 if v14==1 | v14==3
replace des1_ch=2 if v14==4 | v14==2
*/
gen des1_ch=.
replace des1_ch=0 if v13==6 
replace des1_ch=1 if v14==1 | v14==3
replace des1_ch=2 if v14==4 | v14==2
replace des1_ch=3 if v14==5

****************
****des2_ch*****
****************

* Modificaciones Marcela Rubio - Noviembre 2014

/*
gen des2_ch=des1_ch
replace des2_ch=. if des1_ch==3 /* Sentido??? */
*/

gen des2_ch=.
replace des2_ch=0 if des1_ch==0
replace des2_ch=1 if des1_ch==1 | des1_ch==2
replace des2_ch=2 if des1_ch==3 
 
****************
****piso_ch*****
****************


gen piso_ch=.
replace piso_ch=0 if v04==1
replace piso_ch=1 if v04>=2 & v04<=8
replace piso_ch=2 if v04==9



****************
****pared_ch****
****************

gen muros1=v03
gen pared_ch=.
replace pared_ch=0 if muros1>=1
replace pared_ch=1 if muros1>=3 & muros1<=5
replace pared_ch=2 if muros1==6
drop muros1

****************
****techo_ch****
****************
gen techo1=v05
gen techo_ch=.
replace techo_ch=0 if techo1>=6 & techo1<=8
replace techo_ch=1 if techo1>=1 & techo1<=5
replace techo_ch=2 if techo1==9
drop techo1

****************
****resid_ch****
****************

gen basura=v16 /* v16 for 2004 */
gen resid_ch=0 if basura==2
replace resid_ch=1 if basura==1 
replace resid_ch=2 if basura>=3 & basura<=7 /* >=3 & <=7 for 2004 */
replace resid_ch=3 if basura==8 /* v16==8 for 2004 */
drop basura

**Daniela Zuluaga- Enero 2018: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**
	
*********************
***aguamejorada_ch***
*********************
g       aguamejorada_ch = 1 if (v06 >=1 & v06 <=5) 
replace aguamejorada_ch = 0 if (v06 >=6 & v06 <=8) 
		
*********************
***banomejorado_ch***
*********************
g       banomejorado_ch = 1 if (v14 >=1 & v14 <=2)
replace banomejorado_ch = 0 if (v14 >=3 & v14 <=5) | v13==6

****************
****dorm_ch*****
****************

gen dorm_ch=.
replace dorm_ch=v02b
replace dorm_ch=. if dorm_ch==99

****************
***cuartos_ch***
****************

gen cuartos_ch=v02a

****************
***cocina_ch****
****************

gen cocina_ch=v15a /* v15a for 2004 */
replace cocina_ch=0 if cocina==6 /* chequear */


****************
****telef_ch****
****************

gen telef_ch=v11l /* v11l for 2004 */

****************
****refrig_ch***
****************

gen refrig1=v2202 /* v2202 for 2004 */
gen refrig_ch=.
replace refrig_ch=1 if refrig1==2
drop refrig1

****************
****freez_ch****
****************

gen freez_ch=.


****************
****auto_ch*****
****************
gen automovil=v2301 /* v2301 for 2004 */
gen auto_ch=.
replace auto_ch=1 if automovil==1
replace auto_ch=0 if automovil==0
drop automovil 

****************
****compu_ch****
****************

gen compu_ch=v2211 /* v2211 for 2004 */
replace compu_ch=1 if v2211==11

****************
**internet_ch***
****************

gen internet_ch=v2212 /* v2212 for 2004 */
replace internet_ch=1 if v2212==12

****************
****cel_ch******
****************

gen cel_ch=v11c /* v11c for 2004 */
replace cel_ch=0 if cel_ch==6

****************
****vivi1_ch****
****************

gen vivi1_ch=.
replace vivi1_ch=3 if v01>0
replace vivi1_ch=1 if v01==1
replace vivi1_ch=2 if v01==3


****************
****vivi2_ch****
****************

gen vivi2_ch=0
replace vivi2_ch=0 if v01>0
replace vivi2_ch=1 if v01==1 | v01==3


*******************
****viviprop_ch****
*******************

gen viviprop_ch=.
replace viviprop_ch=0 if v17==3 | v17==4 /* v17 for 2004 */ 
replace viviprop_ch=1 if v17==1
replace viviprop_ch=2 if v17==2
replace viviprop_ch=3 if v17==5 | v17==6 

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

gen vivialqimp_ch=v20ede /* v20ede for 2004 */
replace vivialqimp_ch=. if v20ede>=15000000 


*******************************************************
***           VARIABLES DE DIVERSIDAD               ***
*******************************************************				
* Maria Antonella Pereira & Nathalia Maya - Julio 2021	

	***************
	***afroind_ci***
	***************
gen afroind_ci=. 

	***************
	***afroind_ch***
	***************
gen afroind_ch=. 

	*******************
	***afroind_ano_c***
	*******************
gen afroind_ano_c=.		

	*******************
	***dis_ci***
	*******************
gen dis_ci=. 

	*******************
	***dis_ch***
	*******************
gen dis_ch=. 

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
afroind_ci afroind_ch afroind_ano_c dis_ci dis_ch relacion_ci civil_ci jefe_ci nconyuges_ch nhijos_ch notropari_ch notronopari_ch nempdom_ch ///
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
rename b01 codocupa
rename rama codindustria

compress


saveold "`base_out'", replace


log close

