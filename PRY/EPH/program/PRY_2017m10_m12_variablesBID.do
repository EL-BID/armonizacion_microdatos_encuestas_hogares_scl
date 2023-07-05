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
local ANO "2017"
local ronda m10_m12

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
*local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\\`PAIS'_`ANO'`ronda'.dta"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
                                                    
capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Paraguay
Encuesta: EPH
Round: Octubre-Diciembre
Autores: Marcela G. Rubio

Última versión: Alvaro Altamirano - Email: alvaroalt@iadb.org


							SCL/LMK - IADB
****************************************************************************/
/***************************************************************************
Detalle de procesamientos o modificaciones anteriores:

****************************************************************************/
use `base_in', clear

************
* Region_c *
************
/*gen region_c=  dpto 

label define region_c  ///
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
          12 "ñeembucú" ///    
          13 "Amambay" ///     
          14 "Canindeyú" ///   
          15 "Pdte. hayes" /// 
          16 "Boquerón" ///
          17 "Alto paraguay"
label value region_c region_c
label var region_c "División política, departamento"*/

*Modificación Mayra Sáenz - Septiembre 2014		  
gen region_c    = 1 if dpto == 0
replace region_c= 2 if dpto == 2		  
replace region_c= 3 if dpto == 5		  
replace region_c= 4 if dpto == 7
replace region_c= 5 if dpto == 10
replace region_c= 6 if dpto == 11
*replace region_c= 7 if dpto == 1 | dpto == 3 | dpto == 4 | dpto == 6 | dpto == 8 | dpto == 9 | (dpto >= 12 & dpto >= 15) 
replace region_c= 7 if dpto == 20
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
destring idh_ch, replace
sort idh_ch
label variable idh_ch "ID del hogar"
drop upms nvivis nhogas

**************
****idp_ci****
**************

*cap bysort idh_ch:gen idp_ci=_n 
gen idp_ci=l02
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

gen anio_c=2017
label variable anio_c "Anio de la encuesta"

*****************
*** region según BID ***
*****************
gen region_BID_c=.
replace region_BID_c=4 if pais=="PRY" 
label var region_BID_c "Regiones BID"
label define region_BID_c 1 "Centroamérica_(CID)" 2 "Caribe_(CCB)" 3 "Andinos_(CAN)" 4 "Cono_Sur_(CSC)"
label value region_BID_c region_BID_c

******************************
*	mes_c
******************************

*MGR: Base no trae variable de fecha de visitas
gen mes_c= .
label variable mes_c "Mes de la encuesta"

*****************
***relacion_ci***
*****************

gen relacion_ci=.
replace relacion_ci=1 if p03==1
replace relacion_ci=2 if p03==2 
replace relacion_ci=3 if p03==3 | p03==4
replace relacion_ci=4 if p03==5 | p03==6 | p03==7 | p03==8 | p03==9
replace relacion_ci=5 if p03==10 
replace relacion_ci=6 if p03>=11

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

***************
***upm_ci***
***************

clonevar upm_ci=upm
label variable upm_ci "Unidad Primaria de Muestreo"

***************
***estrato_ci***
***************

egen estrato_ci=group(dpto area)
label variable estrato_ci "Estrato"

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
       civil |  
 ------------+
 1casado     | 
 2unido      | 
 3separado   | 
 4viudo      | 
 5soltero    | 
 6divorciado | 
 ------------+
       Total |  
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

by idh_ch, sort: egen nmayor65_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad_ci>=65 )
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
label variable miembros_ci "Miembros del hogar"


*******************************************************
***           VARIABLES DE DIVERSIDAD               ***
*******************************************************				
* Maria Antonella Pereira & Nathalia Maya - Marzo 2021	

			
	***************
   *** afroind_ci ***
	***************
gen afroind_ci=. 

	***************
   *** afroind_ch ***
	***************
gen afroind_ch=. 

	*******************
   *** afroind_ano_c ***
	*******************
gen afroind_ano_c=.		

	*******************
	*** dis_ci ***
	*******************
gen dis_ci=. 

	*******************
	*** dis_ch ***
	*******************
gen dis_ch=. 



************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/
	
*********
*lp_ci***
*********
gen lp_ci = linpobto 
label var lp_ci "Linea de pobreza oficial del pais"

*********
*lpe_ci***
*********

gen lpe_ci = linpobex 
label var lp_ci "Linea de pobreza extrema del pais"

****************
*cotizando_ci***
****************
gen cotizando_ci=.
replace cotizando_ci=1 if b10==1 | c07==1
replace cotizando_ci=0 if (b10==6 | b10==.) & (c07==6 | c07==.)
replace cotizando_ci = 0 if peaa == 2 
label var cotizando_ci "Cotizante a la Seguridad Social"

****************
*cotizapri_ci***
****************
gen cotizapri_ci=.
replace cotizapri_ci=1 if b10==1 
replace cotizapri_ci=0 if (b10==6 | b10==.) 
replace cotizapri_ci = 0 if peaa == 2 
label var cotizapri_ci "Cotizante a la Seguridad Social en actividad ppal."

****************
*cotizasec_ci***
****************
gen cotizasec_ci=.
replace cotizasec_ci=1 if  c07==1
replace cotizasec_ci=0 if (c07==6 | c07==.)
replace cotizasec_ci = 0 if peaa == 2 
label var cotizasec_ci "Cotizante a la Seguridad Social en actividad sec."

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

****************
*instpen_ci*****
****************
gen instpen_ci=.
label var instpen_ci "Institucion proveedora de la pension - variable original de cada pais" 

****************
*instcot_ci*****
****************

clonevar instcot_ci=b11
label var instpen_ci "Institucion a la que cotiza - variable original de cada pais" 


****************
****condocup_ci*
****************

* Comprobacion que no se toma en cuenta el desempleo oculto. MGD 05/27/2014
* No coincide con la variable creada pead ya que se condiciona por busqueda de trabajo y disponibilidad para trabajar.
gen condocup_ci=.
replace condocup_ci=1 if (pead==1 | pead==4 | pead==5)
replace condocup_ci=2 if (pead==2 | pead==6) & edad_ci >=10
recode condocup_ci .=3 if (pead==3) | (condocup_ci==. & edad_ci >=10) 
recode condocup_ci .=4 if edad_ci<10

label var condocup_ci "Condicion de ocupación de acuerdo a def de cada pais"
label define condocup_ci 1 "Ocupado" 2 "Desocupado" 3 "Inactivo" 4 "Menor de PET" 
label value condocup_ci condocup_ci


*************
*cesante_ci* 
*************

gen cesante_ci=1 if a12==1 &  peaa==2 & edad_ci>=10
replace cesante_ci=0 if a12==6 &  peaa==2 & edad_ci>=10
label var cesante_ci "Desocupado - definicion oficial del pais"	

*************
**pension_ci*
*************
*DZ Jul 2017: Cambio de categorias respecto al anio anterior*
gen pension_ci=1 if (e01j>0 & e01j<. & e01j!=99999999999) | (e01h>0 & e01h<. & e01h!=99999999999)
recode pension_ci .=0 
label var pension_ci "1=Recibe pension contributiva"

*************
*ypen_ci*
*************
*DZ Jul 2017: Cambio de categorias respecto al anio anterior*
replace e01j=. if (e01j >= 99999999999 & e01j!=.)
replace e01h=. if (e01h >= 99999999999 & e01h!=.)
egen ypen_ci=rowtotal(e01j e01h)
replace ypen_ci=. if pension_ci==0
label var ypen_ci "Valor de la pension contributiva"

***************
*pensionsub_ci*
***************
* Programa Adulto Mayor
gen pensionsub_ci=.
replace pensionsub_ci=1 if (e01k>0 & e01k<. & e01k!=99999999999)
recode pensionsub_ci .=0
label var pensionsub_ci "1=recibe pension subsidiada / no contributiva"

*****************
**ypensub_ci*
*****************
replace e01k=. if (e01k >= 99999999999)
gen ypensub_ci=e01k
*replace ypensub_ci=0 if pensionsub_ci==0
replace ypensub_ci=. if pensionsub_ci==0
label var ypensub_ci "Valor de la pension subsidiada / no contributiva"

*************
**salmm_ci***
*************

* Vigente a partir del 1ero de julio de 2017** 
gen salmm_ci= 	2041123
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

gen desalent_ci=. /*Se intenta construir como en el 2009 pero no existe la variable a09*/
*gen desalent_ci=(a09==2)

***************
***subemp_ci***
***************
gen holune=int(b03lu) if b03lu<99
gen homart=int(b03ma) if b03ma<99
gen homier=int(b03mi) if b03mi<99
gen hojuev=int(b03ju) if b03ju<99
gen hovier=int(b03vi) if b03vi<99
gen hosab=int(b03sa)  if b03sa<99
gen hodom=int(b03do)  if b03do<99

gen minlune=b03lu-holune
gen minmart=b03ma-homart
gen minmier=b03mi-homier
gen minjuev=b03ju-hojuev
gen minvier=b03vi-hovier
gen minsab=b03sa-hosab
gen mindom=b03do-hodom

egen horas=rsum(holune homart homier hojuev hovier hosab hodom), missing
egen min = rsum(minlune minmart minmier minjuev minvier minsab mindom), missing
replace min=min*100
gen horas2 =min/60
egen tothoras=rsum(horas horas2), missing


gen subemp_ci=0
replace subemp_ci=1 if (tothoras>=1 & tothoras<=30) & (emp_ci==1 & d01==1)
drop min* 

*******************
***tiempoparc_ci***
*******************

* 10/21/2015 MGD: se considera solo la restricción de si quiere trabajar más.  No la de disponibilidad
gen tiempoparc_ci=0
replace tiempoparc_ci=1 if (tothoras>=1 & tothoras<30) & emp_ci==1 /*& d01==6*/  & d03==6

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

******************
***categopri_ci***
******************
/*
           categoria de |
        ocupacion (act. |
             principal) |
  ----------------------+
0 na                      |     11,257       53.47       53.47
1 empleado/obrero público |        807        3.83       57.30
2 empleado/obrero privado |      3,037       14.43       71.73
3 empleador o patrón      |        467        2.22       73.95
4 cuenta propia           |      3,678       17.47       91.42
5 famil. no remunerado    |      1,054        5.01       96.42
6 empleado doméstico      |        750        3.56       99.99
9 nr                      |          3        0.01      100.00
  ----------------------+
                  Total |

*/

	gen categopri_ci=.
	replace categopri_ci=1 if cate_pea==3 
	replace categopri_ci=2 if cate_pea==4 
	replace categopri_ci=3 if cate_pea==1 | cate_pea==2 | cate_pea==6 
	replace categopri_ci=4 if cate_pea==5 
	replace categopri_ci=. if emp_ci~=1

	label define categopri_ci 1 "Patron" 2 "Cuenta propia" 
	label define categopri_ci 3 "Empleado" 4 " Familiar no remunerado" , add
	label value categopri_ci categopri_ci
	label variable categopri_ci "Categoria ocupacional"


******************
***categosec_ci***
******************

gen categosec_ci=.
replace categosec_ci=1 if c09==3
replace categosec_ci=2 if c09==4 
replace categosec_ci=3 if c09==1 | c09==2 | c09==6
replace categosec_ci=4 if c09==5 
replace categosec_ci=. if emp_ci~=1 

label define categosec_ci 1"Patron" 2 "Cuenta propia" 
label define categosec_ci 3"Empleado" 4"Familiar no remunerado" , add
label value categosec_ci categosec_ci
label variable categosec_ci "Categoria ocupacional trabajo secundario"

*****************
*tipocontrato_ci*
*****************

*MGR: correción a sintáxis
/*
gen tipocontrato_ci=. /* Solo asalariados*/
replace tipocontrato_ci=1 if b26==1 & categopri_ci==3
replace tipocontrato_ci=2 if (b26==2 | b26==4) & categopri_ci==3
replace tipocontrato_ci=3 if (b26==3 | tipocontrato_ci==.) & categopri_ci==3
label var tipocontrato_ci "Tipo de contrato segun su duracion"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci
*/

gen tipocontrato_ci=. /* Solo asalariados*/
replace tipocontrato_ci=1 if b26==1 & categopri_ci==3
replace tipocontrato_ci=2 if (b26==2 | b26==3) & categopri_ci==3
replace tipocontrato_ci=3 if (b26==4 | tipocontrato_ci==.) & categopri_ci==3
label var tipocontrato_ci "Tipo de contrato segun su duracion"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci


*****************
***nempleos_ci***
*****************

gen nempleos_ci=0
replace nempleos_ci=1 if b31==6 
replace nempleos_ci=2 if b31==1
replace nempleos_ci=. if pea_ci==0

*****************
***spublico_ci***
*****************

* 10/21/2015 MGD: corrección pequeña para que se tomen en cuenta a todos los ocupados.
gen spublico_ci=0 if emp_ci==1
replace spublico_ci=1 if cate_pea==1 & emp_ci==1
/*replace spublico_ci=0 if cate_pea!=1 & cate_pea!=9
replace spublico_ci=. if emp_ci~=1*/

***ocupa_ci***
**************
/*no habra categoria 5 porque servicios y comerciantes estan en la misma codificacion
OCUP: 5112 a 5230: Trabajadores de los servicios y vendedores de comercios y mercados
Ultima actualización Alvaro AM, con inclusión de CIUO a 4 dígitos provista por instituto de estadística*/
gen ocupa_ci=.
replace ocupa_ci=1 if (b01c>=2113 & b01c<=3480)  & emp_ci==1
replace ocupa_ci=2 if (b01c>=1110 & b01c<=1236) & emp_ci==1
replace ocupa_ci=3 if (b01c>=4111 & b01c<=4223) & emp_ci==1
replace ocupa_ci=4 if ((b01c>=5210 & b01c<=5230) | (b01c>=9111 & b01c<=9113)) & emp_ci==1
replace ocupa_ci=5 if ((b01c>=5111 & b01c<=5169) | (b01c>=9120 & b01c<=9170)) & emp_ci==1
replace ocupa_ci=6 if ((b01c>=6111 & b01c<=6153) | (b01c>=9211 & b01c<=9212)) & emp_ci==1
replace ocupa_ci=7 if ((b01c>=7111 & b01c<=8340) | (b01c>=9311 & b01c<=9339))& emp_ci==1
replace ocupa_ci=8 if b01c==110 & emp_ci==1
replace ocupa_ci=9 if (b01c>9339 & b01c<=9999) & emp_ci==1

*************
***rama_ci***
*************
g rama_ci=.
replace rama_ci=1 if (b02c>=111 & b02c<=500) & emp_ci==1
replace rama_ci=2 if (b02c>=1010 & b02c<=1429) & emp_ci==1
replace rama_ci=3 if (b02c>=1511 & b02c<=3720) & emp_ci==1
replace rama_ci=4 if (b02c>=4010 & b02c<=4100) & emp_ci==1
replace rama_ci=5 if (b02c>=4510 & b02c<=4550) & emp_ci==1
replace rama_ci=6 if (b02c>=5010 & b02c<=5520) & emp_ci==1
replace rama_ci=7 if (b02c>=6010 & b02c<=6420) & emp_ci==1
replace rama_ci=8 if (b02c>=6511 & b02c<=7020) & emp_ci==1
replace rama_ci=9 if (b02c>=7111 & b02c<=9999) & emp_ci==1


****************
***durades_ci***
****************


gen dura=(a11a*12)+a11m+(a11s/4.3 )

gen durades_ci=int(dura)
replace durades_ci=. if durades_ci==0

*******************
***antiguedad_ci***
*******************

*2014,05 modificacion MLO
gen atenure=b09a if b09a!=99
gen stenure=b09s/52.17 if b09s!=9
gen mtenure=b09m/12 if b09m!=99
egen antiguedad_ci=rowtotal(atenure stenure mtenure), m

*************
*tamemp_ci
*************
/*
na               
solo               
2 a 5 personas      
6 a 10 personas     
11 a 20 personas    
21 a 50 personas    
51 a 100 personas   
101 a 500 personas  
más de 500 personas 
empleado doméstico 
no sabe             
nr         
*/
*Paraguay Pequeña 1 a 5, Mediana 6 a 50, Grande Más de 50

gen tamemp_ci = 1 if b08>=1 & b08<=2
replace tamemp_ci = 2 if (b08>=3 & b08<=6)
replace tamemp_ci = 3 if (b08>=7 & b08<=9)

label define tamemp_ci 1 "Pequeña" 2 "Mediana" 3 "Grande"
label value tamemp_ci tamemp_ci
label var tamemp_ci "Tamaño de empresa"

gen tamemp_ci1 = 1 if b08>1 & b08<=2
replace tamemp_ci1 = 2 if (b08>=3 & b08<=5)
replace tamemp_ci1 = 3 if (b08>=6 & b08<=8)

*******************
***categoinac_ci*** 
*******************

*MGR: modificación sintáxis ya que variable a09 no está disponible

/*
gen categoinac_ci =1 if ((a09==14 | a09==15) & condocup_ci==3)
replace categoinac_ci = 2 if  (a09==7 & condocup_ci==3)
replace categoinac_ci = 3 if  (a09==6 & condocup_ci==3)
replace categoinac_ci = 4 if  ((categoinac_ci ~=1 & categoinac_ci ~=2 & categoinac_ci ~=3) & condocup_ci==3)
label var categoinac_ci "Categoría de inactividad"
label define categoinac_ci 1 "jubilados o pensionados" 2 "Estudiantes" 3 "Quehaceres domésticos" 4 "Otros" 
*/
gen categoinac_ci =1 if ((ra06ya09==7) & condocup_ci==3)
replace categoinac_ci = 2 if  (ra06ya09==1 & condocup_ci==3)
replace categoinac_ci = 3 if  (ra06ya09==2 & condocup_ci==3)
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

* Formalidad sin restringir a PEA
g formal_1 = 0 if condocup_ci>=1 & condocup_ci<=3
replace formal_1=1 if cotizando_ci==1
replace formal_1=1 if afiliado_ci==1 & (cotizando_ci!=1 | cotizando_ci!=0) & pais_c=="PRY" & anio_c<=2006

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

*MGR: modifico sintáxis para incorporar alquiler, comida y vestido en especie

*Bebidas y comidas en especie
gen comida=.
replace comida=b20g*30 if b20u==2 & b20g!=99999999999 & b20g!=.
replace comida=b20g*4.3 if b20u==3 & b20g!=99999999999 & b20g!=.
replace comida=b20g*2 if b20u==4 & b20g!=99999999999 & b20g!=.
replace comida=b20g if b20u==5 & b20g!=99999999999 & b20g!=.
replace comida=b20g/12 if b20u==6 & b20g!=99999999999 & b20g!=.

*Uniformees
gen unifor=.
replace unifor=b25/12 
replace unifor =. if b25==99999999999

*alquiler
gen alquiler=.
replace alquiler=b23 if b21==1 & b23!=99999999999

egen ylnmpri_ci=rsum(comida unifor alquiler), missing
replace ylnmpri_ci=. if comida==. & unifor==. & alquiler==. 
replace ylnmpri_ci=. if emp_ci~=1
label var ylnmpri_ci "Ingreso laboral NO monetario actividad principal"   

***************
***ylmsec_ci***
***************
/*existen unas actividades ocasionales-las pongo en secundario*/
gen ysec1=e01bimde
replace ysec1=. if e01bimde==0 | e01bimde>=999999999 /*No aplicable*/

gen ylmsec_ci=ysec1
replace ylmsec_ci=. if emp_ci~=1
replace ylmsec_ci=. if ysec1==. 

drop ysec1 

*************
*ylmotros_ci*
*************
gen yocasio1=e01cimde 
replace yocasio1=. if e01cimde ==0 | e01cimde >=999999999 /*No aplicable*/

gen ylmotros_ci= yocasio1
replace ylmsec_ci=. if emp_ci~=1
replace ylmsec_ci=. if yocasio1==.
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
destring e02tde, force replace
local var="e01dde e01ede e01fde e01gde e01hde e01ide e01jde e01kde e01lde  e02tde"

foreach x of local var {
gen `x'1=`x'
replace `x'1=. if `x'==0 | `x'>=999999999 /*No aplicable*/
}


egen ynlm_ci=rsum(e01dde1 e01ede1 e01fde1 e01gde1 e01hde1 e01ide1 e01jde1 e01kde1 e01lde1 e02tde1), missing
replace ynlm_ci=. if e01dde1==. & e01ede1==. & e01fde1==. & e01gde1==. & e01hde1==. & e01ide1==. & e01jde1==. & e01kde1==. & e02tde1==.

drop e01dde1 e01ede1 e01fde1 e01gde1 e01hde1 e01ide1 e01jde1 e01kde1 e02tde1

*********************************************
*Ingreso laboral no monetario otros trabajos*
*********************************************

gen ylnmotros_ci=.

*****************
***remesas_ci***
*****************

gen remesas_ci= e02tde
gen ynlnm_ci=.

************************
*** HOUSEHOLD INCOME ***
************************

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
destring v19, force replace
replace v19 = . if v19>=99999999999
gen rentaimp_ch=v19
replace rentaimp_ch=. if v19>=99999999999 /*Si tuviera que alquilar esta vivienda cuanto estimaría que le Pag.. por mes*/
sort v19
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
*Mod. 7/2022 Agustina Thailinger y Javier Valverde SCL/EDU

/*
Ninguno                                                  0
Educ. Especial                                           1
Educ. Inicial                                            2
Educ. Escolar Básica 1º al 6º (Primaria)                 3
Educ. Escolar Básica 7º al 9º                            4
Secundaria - Ciclo Básico                                5
Bachillerato Humanístico /Científico                     6
Bachillerato Técnico /Comercial                          7
Bachillerato a Distancia                                 8
Educ. Media Científica                                   9
Educ. Media Técnica                                      10
Educ. Media Abierta                                      11
Educ. Básica Bilingüe para personas Jóvenes y Adultas    12
Educ. Media a Distancia para Jóvenes y Adultos           13
Educ. Básica Alternativa de Jóvenes y Adultos            14
Educ. Media Alternativa de Jóvenes y Adultos             15
Educ. Media para personas Jóvenes y Adultas              16
Formación Profesional no Bachillerato de la Media        17
Programas de Alfabetización                              18
Grado Especial/Programas Especiales                      19
Técnica Superior                                         20
Formación Docente                                        21
Profesionalización Docente                               22
Form. Militar/Policial                                   23
Superior Universitario                                   24
*/

capture drop nivgra
gen nivgra=ed0504
tostring nivgra, gen(nivgra_str)
gen aedu_temp=substr(nivgra_str,-1,1)
destring aedu_temp, replace
replace aedu_temp=. if nivgra==8888 | nivgra==9999 // na, nr
replace aedu_temp=0 if (nivgra>=1200 & nivgra<=1299) | (nivgra>=1300 & nivgra<=1399) | (nivgra>=1400 & nivgra<=1499) | (nivgra>=1500 & nivgra<=1599) | (nivgra>=1600 & nivgra<=1699) | (nivgra>=1800 & nivgra<=1899)  // educación para adultos
replace aedu_temp=. if (nivgra>=100 & nivgra<=199) | (nivgra>=1900 & nivgra<=1999)  // educación especial

gen aedu_ci=aedu_temp
replace aedu_ci=0 if nivgra==0 // sin instruccion
replace aedu_ci=0 if nivgra>=200 & nivgra<=299 // inicial prejardin, inicial jardin, preescolar
replace aedu_ci=aedu_temp if nivgra>=300 & nivgra<=399 // escolar basica: 1 a 6
replace aedu_ci=aedu_temp if nivgra>=400 & nivgra<=499 // escolar basica: 7 a 9
replace aedu_ci=aedu_temp+9 if nivgra>=900 & nivgra<=999 // media cientifica
replace aedu_ci=aedu_temp+9 if nivgra>=1000 & nivgra<=1099 // media tecnica
replace aedu_ci=aedu_temp+9 if nivgra>=1100 & nivgra<=1199 // media abierta
replace aedu_ci=aedu_temp+9 if nivgra>=500 & nivgra<=599 // ciclo básico de sencudaria antiguo
replace aedu_ci=aedu_temp+6 if nivgra>=600 & nivgra<=699 // educación secundaria y bachillerato
replace aedu_ci=aedu_temp+6 if nivgra>=700 & nivgra<=799 // educación secundaria y bachillerato
replace aedu_ci=aedu_temp+6 if nivgra>=800 & nivgra<=899 // educación secundaria y bachillerato
replace aedu_ci=aedu_temp+6 if nivgra>=1700 & nivgra<=1799 // programa de formación profesional
replace aedu_ci=aedu_temp+12 if nivgra>=2000 & nivgra<=2099 // tecnica superior
replace aedu_ci=aedu_temp+12 if nivgra>=2100 & nivgra<=2199 // formación docente
replace aedu_ci=aedu_temp+12 if nivgra>=2200 & nivgra<=2299 // profesionalización docente
replace aedu_ci=aedu_temp+12 if nivgra>=2300 & nivgra<=2399 // militar/policial
replace aedu_ci=aedu_temp+12 if nivgra>=2400 & nivgra<=2499 // superior universitario

replace aedu_ci=aedu_temp+12+5+2 if ed06c==8 // doctorado
replace aedu_ci=aedu_temp+12+5 if ed06c==9 // maestria
replace aedu_ci=aedu_temp+12+5 if ed06c==10 // especialización

**************
***eduno_ci***
**************
gen byte eduno_ci=(aedu_ci==0)
replace eduno_ci=. if aedu_ci==.
label variable eduno_ci "Cero anios de educacion"

**************
***edupi_ci***
**************
gen byte edupi_ci=(aedu_ci>0 & aedu_ci<6)
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
***eduuc_ci****
***************
gen byte eduuc_ci=(aedu_ci>=16)
replace eduuc_ci=. if aedu_ci==.
label variable eduuc_ci "Universitaria completa o mas"

***************
***edupre_ci***
***************
gen byte edupre_ci=. 
label variable edupre_ci "Educacion preescolar"

***************
***asis_pre***
***************
gen byte asispre_ci=(ed08==1)
label variable asispre_ci "Asistencia a Educacion preescolar" 

**************
***eduac_ci***
**************
gen byte eduac_ci=. 
replace eduac_ci=1 if nivgra>=2400 & nivgra<=2499 | nivgra>=2200 & nivgra<=2299 // profesionalización docente
replace eduac_ci=0 if nivgra>=2000 & nivgra<=2099 // tecnica superior
replace eduac_ci=0 if nivgra>=2100 & nivgra<=2199 // formacion docente
replace eduac_ci=0 if nivgra>=2300 & nivgra<=2399 // formacion militar
label variable eduac_ci "Superior universitario vs superior no universitario"
		
***************
***asiste_ci***
***************
gen asiste_ci=.
replace asiste_ci=1 if ed08>=1 & ed08<=18
replace asiste_ci=0 if ed08==19
label variable asiste_ci "Asiste actualmente a la escuela"

*****************
***pqnoasis_ci***
*****************
gen pqnoasis_ci=ed10
replace pqnoasis_ci=. if ed10==99
label define pqnoasis_ci 1 "Sin recursos en el hogar" 2 "Necesidad de trabajar" 3 "Muy costosos los materiales y matrícula" 4 "No tiene edad adecuada" 5 "Considera que terminó los estudios" 6 "No existe institución cercana" 7 "Institución cercana muy mala" 8 "El centro educativo cerró"  9 "El docente no asiste con regularidad" 10 "Institución no ofrece escolaridad completa" 11 "Requiere educación especial" 12 "Por enfermedad/accidente" 13 "Realiza labores en el hogar" 14 "Motivos familiares" 15 "No quiere estudiar" 16 "Asiste a una enseñanza vocacional o formación profesional" 17 "Servicio militar" 18 "Otra razón" 
label value pqnoasis_ci pqnoasis_ci

******************
***pqnoasis1_ci***
******************
**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**
g pqnoasis1_ci=1 if ed10==1 | ed10==3
replace pqnoasis1_ci=2 if ed10==2
replace pqnoasis1_ci=3 if ed10==11 | ed10==12 | ed10==14
replace pqnoasis1_ci=4 if ed10==15
replace pqnoasis1_ci=5 if ed10==13
replace pqnoasis1_ci=6 if ed10==5
replace pqnoasis1_ci=7 if ed10==4
replace pqnoasis1_ci=8 if ed10==6  | ed10==7  | ed10==8  | ed10==9 | ed10==10 
replace pqnoasis1_ci=9 if ed10==16 | ed10==17 | ed10==18

label define pqnoasis1_ci 1 "Problemas económicos" 2 "Por trabajo" 3 "Problemas familiares o de salud" 4 "Falta de interés" 5 "Quehaceres domésticos/embarazo/cuidado de niños/as" 6 "Terminó sus estudios" 7	"Edad" 8 "Problemas de acceso"  9 "Otros"
label value  pqnoasis1_ci pqnoasis1_ci
		
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
gen edupub_ci=1 if ed09==1 
replace edupub_ci=0 if (ed09>=2 & ed09<=3)
replace edupub_ci=. if ed09==.

drop nivgra aedu_temp

********************************************
***Variables de Infraestructura del hogar***
********************************************

****************
***aguared_ch***
****************
generate aguared_ch =.
replace aguared_ch = 1 if (v06==1 | v06==2 | v06==3| v06==4)
replace aguared_ch = 0 if v06>4
la var aguared_ch "Acceso a fuente de agua por red"
	
*****************
*aguafconsumo_ch*
*****************
gen aguafconsumo_ch = 0
replace aguafconsumo_ch = 1 if (v08==4 | v08==1 | v08==2 |v08==3) & v09<=2
replace aguafconsumo_ch = 2 if (v08==4 | v08==1 | v08==2 |v08==3) & v09==3
replace aguafconsumo_ch = 3 if v08==11
replace aguafconsumo_ch= 4 if (v08==5 | v08==6)
replace aguafconsumo_ch = 5 if v08==10
replace aguafconsumo_ch = 6 if v08==12
replace aguafconsumo_ch = 7 if v08==8 | ((v08==1 | v08==2 |v08==3| v08==4 |v08==5|v08==6|v08==8|v08==10|v08==11|v08==12) & v09==5)
replace aguafconsumo_ch = 8 if v08==13
replace aguafconsumo_ch = 9 if v08==9 | v08==7
replace aguafconsumo_ch = 10 if v08==14 


*****************
*aguafuente_ch*
*****************

gen aguafuente_ch = 1 if (v06==4 | v06==1 | v06==2 |v06==3) & v07a<=2
replace aguafuente_ch = 2 if (v06==4 | v06==1 | v06==2 |v06==3) & v07a==3
replace aguafuente_ch= 4 if (v06==5 | v06==6)
replace aguafuente_ch = 5 if v06==10
replace aguafuente_ch = 6 if v06==11
replace aguafuente_ch = 7 if (v06==1 | v06==2 |v06==3 |v06==4 |v06==5|v06==10|v06==11) & v07a ==5
replace aguafuente_ch = 8 if v06==9
replace aguafuente_ch = 10 if (v06==12 | v06==8 |v06==7|(v06==.& jefe_ci!=.))


*************
*aguadist_ch*
*************
gen aguadist_ch=0
replace aguadist_ch= 1 if v07a==2
replace aguadist_ch= 2 if v07a==1 | v07a ==4 
replace aguadist_ch= 3 if v07a==3 

**************
*aguadisp1_ch*
**************
destring v07, replace
gen aguadisp1_ch =0
replace aguadisp1_ch =1 if v07==1

**************
*aguadisp2_ch*
**************
gen aguadisp2_ch = 9



*************
*aguamala_ch*  Altered
*************
gen aguamala_ch = 2
replace aguamala_ch = 0 if aguafuente_ch<=7
replace aguamala_ch = 1 if aguafuente_ch>7 & aguafuente_ch!=10


*****************
*aguamejorada_ch*  Altered
*****************
gen aguamejorada_ch = 2
replace aguamejorada_ch = 0 if aguafuente_ch>7 & aguafuente_ch!=10
replace aguamejorada_ch = 1 if aguafuente_ch<=7 



*****************
***aguamide_ch***
*****************
gen aguamide_ch =.
label var aguamide_ch "Usan medidor para pagar consumo de agua"


*****************
*bano_ch         *  Altered
*****************
destring v13, replace
gen bano_ch=.
replace bano_ch=0 if v12==6
replace bano_ch=1 if v13==1
replace bano_ch=2 if v13==2
replace bano_ch=3 if (v13==5 | v13==6)
replace bano_ch=4 if v13==4
replace bano_ch=5 if v13==7
replace bano_ch=6 if (v13==8 | v13==3) |(v13==. & jefe_ci!=.)

***************
***banoex_ch***
***************
generate banoex_ch=9
la var banoex_ch "El servicio sanitario es exclusivo del hogar"


*****************
*banomejorado_ch*  Altered
*****************
gen banomejorado_ch= 2
replace banomejorado_ch =1 if bano_ch<=3 & bano_ch!=0
replace banomejorado_ch =0 if (bano_ch ==0 | bano_ch>=4) & bano_ch!=6

************
*sinbano_ch*
************
gen sinbano_ch = 3
replace sinbano_ch = 0 if v12==1

*label var sinbano_ch "= 0 si tiene baño en la vivienda o dentro del terreno"

*************
*aguatrat_ch*
*************
gen aguatrat_ch = 9
*label var aguatrat_ch "= 9 la encuesta no pregunta de si se trata el agua antes de consumirla"




****************
*****luz_ch*****
****************
*ren v10 luz_ch
gen luz_ch = (v10==1)

****************
***luzmide_ch***
****************

gen luzmide_ch=.

****************
***combust_ch***
****************

gen combust_ch=(v14b==4 | v14b==2)

 



****************
****des1_ch*****
****************
destring v13, force replace
gen des1_ch=.
replace des1_ch=0 if v12==6
replace des1_ch=1 if v13==1 | v13==2
replace des1_ch=2 if v13==5 |v13==6 |v13==7 | v13==3
replace des1_ch=3 if v13==4 


****************
****des2_ch*****
****************

*MGR: agrego categoría resto
/*
gen des2_ch=.
replace des2_ch=1 if des1_ch==1| des1_ch==2
replace des2_ch=0 if des1_ch==0
*/
gen des2_ch=.
replace des2_ch=0 if des1_ch==0
replace des2_ch=1 if des1_ch==1| des1_ch==2
replace des2_ch=1 if des1_ch==3
 
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
*gen muros1=v03
gen pared_ch=.
replace pared_ch=0 if v03==7
replace pared_ch=1 if v03>=1 & v03<=5
replace pared_ch=2 if v03==6 | v03==9

****************
****techo_ch****
****************

/*
gen techo_ch=.
replace techo_ch=0 if v05==8
replace techo_ch=1 if v05==1| (v05>=3 & v05<=6)
replace techo_ch=2 if v05==2 | v05==7 | v05==9
*/
gen techo_ch=.
replace techo_ch=0 if v05==8 | v05==2 
replace techo_ch=1 if v05==1 | (v05>=3 & v05<=6)
replace techo_ch=2 if v05==7 | v05==9

****************
****resid_ch****
****************
*La etiqueta de la categoría 4 está mal en la base.
*MGR:La etiqueta de la categoría 4 no está mal, es la correcta de acuerdo a cuestionario: Tira en el arroyo. Corrijo sintáxis
/*
gen basura=v15
gen resid_ch=0 if basura==2 | basura==3
replace resid_ch=1 if basura==1 | basura==4
replace resid_ch=2 if basura>=5 & basura<=8
drop basura
*/

gen resid_ch=0 if v15==2 | v15==3
replace resid_ch=1 if v15==1 
replace resid_ch=2 if v15>=4 & v15<=8


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
replace cuartos_ch=. if v02a==99

****************
***cocina_ch****
****************

gen cocina_ch=(v14a ==1)



****************
****telef_ch****
****************

gen telef_ch=(v11a ==1)

 
****************
****refrig_ch***
****************

gen refrig_ch=.
replace refrig_ch= (v2403==1)

****************
****freez_ch****
****************

gen freez_ch=.


****************
****auto_ch*****
****************
gen automovil=v2413
gen auto_ch=.
replace auto_ch=1 if automovil==1
replace auto_ch=0 if automovil==6
drop automovil 

****************
****compu_ch****
****************

*MGR: encuesta agrega opción si hogar cuenta con tableta
*gen compu_ch=(v23a==1)
gen compu_ch=(v23a1==1 | v23a2==1)

****************
**internet_ch***
****************

gen internet_ch=(v23b==1)


****************
****cel_ch******
****************

gen cel_ch=(v11b ==1)

****************
****vivi1_ch****
****************

*MGR: corrección error en sintáxis
/*
gen vivi1_ch=.
replace vivi1_ch=3 if v01>0
replace vivi1_ch=1 if v01==1
replace vivi1_ch=2 if v01==3
*/
gen vivi1_ch=.
replace vivi1_ch=1 if v01==1
replace vivi1_ch=2 if v01==2
replace vivi1_ch=3 if v01>=3 & v01<9 

****************
****vivi2_ch****
****************

*MGR: corrección error en sintáxis
/*
gen vivi2_ch=0
replace vivi2_ch=0 if v01>0
replace vivi2_ch=1 if v01==1 | v01==3
*/
gen vivi2_ch = (v01==1 | v01==2)
replace vivi2_ch=. if v01==.

*******************
****viviprop_ch****
*******************

gen viviprop_ch=.
replace viviprop_ch=0 if v16==4
replace viviprop_ch=1 if v16==1 | v16==3
replace viviprop_ch=2 if v16==2
replace viviprop_ch=3 if v16==5 | v16==6 | v16==7

******************
****vivitit_ch****
******************

gen vivitit_ch=.

*******************
****vivialq_ch****
*******************
destring v18, force replace
gen vivialq_ch=v18
replace vivialq_ch=. if v18==99999999999

*********************
****vivialqimp_ch****
*********************
gen vivialqimp_ch=v19
replace vivialqimp_ch=. if v19==99999999999



	*******************
	*** benefdes_ci ***
	*******************

	g benefdes_ci=.
	label var benefdes_ci "=1 si tiene seguro de desempleo"

	*******************
	*** ybenefdes_ci***
	*******************
	g ybenefdes_ci=.
	label var ybenefdes_ci "Monto de seguro de desempleo"

	
******************************
*** VARIABLES DE MIGRACION ***
******************************

* Variables incluidas por SCL/MIG Fernando Morales

	*******************
	*** migrante_ci ***
	*******************
	
	gen migrante_ci=(p10a>19) if p10a!=99 & p10a!=.
	label var migrante_ci "=1 si es migrante"
	
	**********************
	*** migantiguo5_ci ***
	**********************
	
	gen migantiguo5_ci=(migrante_ci==1 & p11a<20) if migrante_ci!=. & p11a!=. & p11a!=99 & !inrange(edad_ci,0,4)
	label var migantiguo5_ci "=1 si es migrante antiguo (5 anos o mas)"
		
	**********************
	*** migrantelac_ci ***
	**********************
	
	gen migrantelac_ci=(migrante_ci==1 & inlist(p10a,20,21,22,23,24,28,31,32)) if migrante_ci!=.
	label var migrantelac_ci "=1 si es migrante proveniente de un pais LAC"
	
	**********************
	*** migrantiguo5_ci ***
	**********************
	
	gen migrantiguo5_ci=(migrante_ci==1 & p11a<20) if migrante_ci!=. & p11a!=. & p11a!=99 & !inrange(edad_ci,0,4)
	replace migrantiguo5_ci= 0 if migrantiguo5_ci != 1 & migrante_ci == 1
	replace migrantiguo5_ci= . if migrante_ci == 0
	label var migrantiguo5_ci "=1 si es migrante antiguo (5 anos o mas)"
		
	**********************
	*** miglac_ci ***
	**********************
	
	gen miglac_ci=(migrante_ci==1 & inlist(p10a,20,21,22,23,24,28,31,32)) if migrante_ci!=.
	replace miglac_ci = 0 if miglac_ci != 1 & migrante_ci == 1
	replace miglac_ci = . if migrante_ci == 0
	label var miglac_ci "=1 si es migrante proveniente de un pais LAC"

******************************
* Variables SPH - PMTC y PNC *
******************************

* PTMC:  Ingresos del estado monetario tekoporã deflactados 
* PNC: 	 Ingresos del estado monetario adulto mayor deflactados

* Ingreso del hogar
egen ingreso_total = rowtotal(ylm_ci ylnm_ci ynlm_ci ynlnm_ci), missing
bys idh_ch: egen y_hog = sum(ingreso_total)

* Personas que reciben transferencias monetarias condicionadas
gen transf = e01ide
bys idh_ch: egen ing_ptmc=sum(transf)
gen ptmc_ci  = (ing_ptmc>0 & ing_ptmc!=.)
bys idh_ch: egen ptmc_ch=max(ptmc_ci)

* Adultos mayores 
gen mayor64_ci=(edad>64 & edad!=.)
bys idh_ch: egen ing_pension  = sum(e01kde)
gen pnc_ci  = (ing_pension>0 & ing_pension!=.)

*ingreso neto del hogar
gen y_pc     = y_hog / nmiembros_ch 
gen y_pc_net = (y_hog - ing_ptmc -ing_pension) / nmiembros_ch

lab def ptmc_ch 1 "Beneficiario PTMC" 0 "No beneficiario PTMC"
lab val ptmc_ch ptmc_ch

lab def pnc_ci 1 "Beneficiario PNC" 0 "No beneficiario PNC"
lab val pnc_ci pnc_ci
	
/*_____________________________________________________________________________________________________*/
* Asignación de etiquetas e inserción de variables externas: tipo de cambio, Indice de Precios al 
* Consumidor (2011=100), líneas de pobreza
/*_____________________________________________________________________________________________________*/


do "$gitFolder\armonizacion_microdatos_encuestas_hogares_scl\_DOCS\\Labels&ExternalVars_Harmonized_DataBank.do"

*_____________________________________________________________________________________________________*

*  Pobres extremos, pobres moderados, vulnerables y no pobres 
* con base en ingreso neto (Sin transferencias)
* y líneas de pobreza internacionales
gen     grupo_int = 1 if (y_pc_net<lp31_ci)
replace grupo_int = 2 if (y_pc_net>=lp31_ci & y_pc_net<(lp31_ci*1.6))
replace grupo_int = 3 if (y_pc_net>=(lp31_ci*1.6) & y_pc_net<(lp31_ci*4))
replace grupo_int = 4 if (y_pc_net>=(lp31_ci*4) & y_pc_net<.)

tab grupo_int, gen(gpo_ingneto)

* Crear interacción entre recibirla la PTMC y el gpo de ingreso
gen ptmc_ingneto1 = 0
replace ptmc_ingneto1 = 1 if ptmc_ch == 1 & gpo_ingneto1 == 1

gen ptmc_ingneto2 = 0
replace ptmc_ingneto2 = 1 if ptmc_ch == 1 & gpo_ingneto2 == 1

gen ptmc_ingneto3 = 0
replace ptmc_ingneto3 = 1 if ptmc_ch == 1 & gpo_ingneto3 == 1

gen ptmc_ingneto4 = 0
replace ptmc_ingneto4 = 1 if ptmc_ch == 1 & gpo_ingneto4 == 1

lab def grupo_int 1 "Pobre extremo" 2 "Pobre moderado" 3 "Vulnerable" 4 "No pobre"
lab val grupo_int grupo_int



/*_____________________________________________________________________________________________________*/
* Verificación de que se encuentren todas las variables armonizadas 
/*_____________________________________________________________________________________________________*/

order region_BID_c region_c pais_c anio_c mes_c zona_c factor_ch	idh_ch	idp_ci	factor_ci upm_ci estrato_ci sexo_ci edad_ci ///
afroind_ci afroind_ch afroind_ano_c dis_ci dis_ch relacion_ci civil_ci jefe_ci nconyuges_ch nhijos_ch notropari_ch notronopari_ch nempdom_ch ///
clasehog_ch nmiembros_ch miembros_ci nmayor21_ch nmenor21_ch nmayor65_ch nmenor6_ch	nmenor1_ch	condocup_ci ///
categoinac_ci nempleos_ci emp_ci antiguedad_ci	desemp_ci cesante_ci durades_ci	pea_ci desalent_ci subemp_ci ///
tiempoparc_ci categopri_ci categosec_ci rama_ci spublico_ci tamemp_ci cotizando_ci instcot_ci	afiliado_ci ///
formal_ci tipocontrato_ci ocupa_ci horaspri_ci horastot_ci	pensionsub_ci pension_ci tipopen_ci instpen_ci	ylmpri_ci nrylmpri_ci ///
tcylmpri_ci ylnmpri_ci ylmsec_ci ylnmsec_ci	ylmotros_ci	ylnmotros_ci ylm_ci	ylnm_ci	ynlm_ci	ynlnm_ci ylm_ch	ylnm_ch	ylmnr_ch  ///
ynlm_ch	ynlnm_ch ylmhopri_ci ylmho_ci rentaimp_ch autocons_ci autocons_ch nrylmpri_ch tcylmpri_ch remesas_ci remesas_ch	ypen_ci	ypensub_ci ///
salmm_ci tc_c ipc_c lp19_c lp31_c lp5_c lp_ci lpe_ci aedu_ci eduno_ci edupi_ci edupc_ci	edusi_ci edusc_ci eduui_ci eduuc_ci	edus1i_ci ///
edus1c_ci edus2i_ci edus2c_ci edupre_ci eduac_ci asiste_ci pqnoasis_ci pqnoasis1_ci	repite_ci repiteult_ci edupub_ci ///
aguared_ch aguafconsumo_ch aguafuente_ch aguadist_ch aguadisp1_ch aguadisp2_ch aguamala_ch aguamejorada_ch aguamide_ch bano_ch banoex_ch banomejorado_ch sinbano_ch aguatrat_ch luz_ch luzmide_ch combust_ch des1_ch des2_ch piso_ch ///
pared_ch techo_ch resid_ch dorm_ch cuartos_ch cocina_ch telef_ch refrig_ch freez_ch auto_ch compu_ch internet_ch cel_ch ///
vivi1_ch vivi2_ch viviprop_ch vivitit_ch vivialq_ch	vivialqimp_ch migrante_ci migantiguo5_ci migrantelac_ci, first


/*Homologar nombre del identificador de ocupaciones (isco, ciuo, etc.) y de industrias y dejarlo en base armonizada 
para análisis de trends (en el marco de estudios sobre el futuro del trabajo)*/
rename b01c codocupa
rename b02c codindustria


compress


saveold "`base_out'", version(12) replace


log close

