
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
local ANO "2015"
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

Última versión: Marcela G. Rubio - Email: mrubio@iadb.org, marcelarubio28@gmail.com
Fecha última modificación: 5 de mayo de 2015

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

gen anio_c=2015
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

gen pension_ci=1 if (e01g>0 & e01g<. & e01g!=99999999999) | (e01i>0 & e01i<. & e01i!=99999999999)
recode pension_ci .=0 
label var pension_ci "1=Recibe pension contributiva"

*************
*ypen_ci*
*************
replace e01g=. if (e01g >= 99999999999 & e01g!=.)
replace e01i=. if (e01i >= 99999999999 & e01i!=.)
egen ypen_ci=rowtotal(e01g e01i)
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

* Se mantiene desde 2014
gen salmm_ci= 	1824055
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
replace tamemp_ci = 3 if (b08>=6 & b08<=8)

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
/*
	0	sin instrucción        
	101	1º educación especial  
	102	2º educación especial  
	103	3º educación especial  
	104	4º educación especial  
	105	5º educación especial  
	106	6º educación especial  
	107	7º educación especial  
	108	8º educación especial  
	109	9º educación especial  
	110	prejardín educacion especial   
	111	jardín educación especial      
	112	preescolar educación especial  
	210	prejardín     
	211	jardín        
	212	preescolar    
	301	1er. grado    
	302	2do. grado    
	303	3er. grado    
	304	4to. grado    
	305	5to. grado    
	306	6to. grado    
	407	7mo. grado    
	408	8vo. grado    
	409	9no. grado    
	501	1º básico     
	502	2º básico     
	503	3º básico     
	604	4º curso humanístico-científico  
	605	5º curso humanístico-científico  
	606	6º curso humanístico-científico  
	704	4º curso técnico-comercial  
	705	5º curso técnico-comercial  
	706	6º curso técnico-comercial  
	803	3º bachillerato a distancia 
	901	1º ed. media científica 
	902	2º ed. media científica 
	903	3º ed. media científica 
	1001	1º ed. media técnica 
	1002	2º ed. media técnica 
	1003	3º ed. media técnica 
	1101	1er. ciclo ed. básica bil. de jóvenes y adultos  
	1102	2do. ciclo ed. básica bil. de jóvenes y adultos  
	1103	3er. ciclo ed. básica bil. de jóvenes y adultos  
	1104	4to. ciclo ed. básica bil. de jóvenes y adultos  
	1201	1er. sem. ed. media a dist. para jóvenes y adultos 
	1202	2do. sem. ed. media a dist. para jóvenes y adultos 
	1203	3er. sem. ed. media a dist. para jóvenes y adultos 
	1204	4to. sem. ed. media a dist. para jóvenes y adultos 
	1301	1er. sem. ed. básica altern. de jóvenes y adultos  
	1302	2do. sem. ed. básica altern. de jóvenes y adultos  
	1303	3er. sem. ed. básica altern. de jóvenes y adultos  
	1401	1er. sem. ed. media altern. de jóvenes y adultos  
	1402	2do. sem. ed. media altern. de jóvenes y adultos  
	1403	3er. sem. ed. media altern. de jóvenes y adultos  
	1404	4to. sem. ed. media altern. de jóvenes y adultos  
	1501	1º formación profesional no bachillerao de la media
	1502	2º formación profesional no bachillerao de la media
	1503	3º formación profesional no bachillerao de la media
	1601	programas de alfabetización  
	1700	grado especial  
	1801	1º técnica superior  
	1802	2º técnica superior  
	1803	3º técnica superior  
	1804	4º técnica superior  
	1901	1º form. docente     
	1902	2º form. docente     
	1903	3º form. docente     
	1904	4º form. docente     
	2001	1er. sem. profesionalización docente 
	2002	2do. sem. profesionalización docente 
	2003	3er. sem. profesionalización docente 
	2004	4to. sem. profesionalización docente 
	2005	5to. sem. profesionalización docente 
	2006	6to. sem. profesionalización docente 
	2101	1º form. militar-policial 
	2102	2º form. militar-policial 
	2103	3º form. militar-policial 
	2104	4º form. militar-policial 
	2201	1º universitario 
	2202	2º universitario 
	2203	3º universitario 
	2204	4º universitario 
	2205	5º universitario 
	2206	6º universitario 
	8888	na 
	9999	nr nivel y grado 
*/

/*
*Modificación Mayra Sáenz -02/01/2016: Se incorpora la nueva sintaxis para la generación de las variables
asiste_ci y aedu_ci elaborada por Iván Bornacelly SCL/EDU.
*/

capture drop nivgra
gen nivgra=ed0504
tostring nivgra, gen(nivgra_str)
gen aedu_temp=substr(nivgra_str,-1,1)
destring aedu_temp, replace
replace aedu_temp=. if nivgra==9999 // No sabe no responde
	*replace aedu_temp=. if nivgra>=1100 & nivgra<=1900 // Educación para adultos
	*replace aedu_temp=. if nivgra>=101 & nivgra<=112 // Educación Especial

	gen aedu_ci=aedu_temp
	*replace aedu_ci=0 if nivgra>=210 & nivgra<=212 // Educación Inicial o Prescolar
	replace aedu_ci=0 if nivgra>=110 & nivgra<=212 // Educación Inicial o Prescolar (regular y especial)
	replace aedu_ci=aedu_temp+6 if nivgra>=501 & nivgra<=503 // Ciclo básico de sencudaria antiguo
	*replace aedu_ci=aedu_temp+6 if nivgra>=604 & nivgra<=803 // Educación Secundaria y Bachillerato
	replace aedu_ci=aedu_temp+6 if nivgra>=604 & nivgra<=706 // Educación Secundaria y Bachillerato
	*replace aedu_ci=aedu_temp+9 if nivgra>=900 & nivgra<=1003 // Educación Media 
	replace aedu_ci=aedu_temp+9 if nivgra>=801 & nivgra<=1103 // Educación Media (se incorporó la Educación Media Abierta)
	replace aedu_ci=aedu_temp+12 if nivgra>=2000 & nivgra<=2406 // Educación superior (técnica, tecnológica, universitaria)
	replace aedu_ci=aedu_temp+12+5 if ed06c==8 // Doctorado
	replace aedu_ci=aedu_temp+12+2 if ed06c==9 // Maestria
	replace aedu_ci=aedu_temp+12+1 if ed06c==10 // Especialización

	*Añadir educación para adultos
	replace aedu_ci=3 if nivgra==1201 | nivgra==1401 //Educación básica ciclo 1
	replace aedu_ci=5 if nivgra==1202 | nivgra==1402 //Educación básica ciclo 2
	replace aedu_ci=7 if nivgra==1203 | nivgra==1403 //Educación básica ciclo 3
	replace aedu_ci=9 if nivgra==1204 //Educación básica ciclo 4
	replace aedu_ci=9+aedu_temp if nivgra>=1301 & nivgra<=1304 //Educación media a distancia y alternativa
	replace aedu_ci=9+aedu_temp if nivgra>=1501 & nivgra<=1604 //Educación media a distancia y alternativa

	*Añadir programa de formación profesional (no bachillerato de la media)
	replace aedu_ci=aedu_temp+6 if nivgra>=1701 & nivgra<=1703

	*Añadiendo la enseñanza especial (ya queda añadida con aedu_temp)
	*Añadiendo programa de alfabetización 
 	replace aedu_ci=0 if nivgra==1801
	
	*Añadiendo grado especial 0 (ya queda añadida con aedu_temp)
	*Añadir los que nunca asistieron a una institución educativa formal
	replace aedu_ci=0 if ed03==6
	


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
replace edupre_ci=1 if nivgra>=100 & nivgra!=.
replace edupre_ci=0 if nivgra==0

***************
***asis_pre***
***************

gen byte asispre_ci=. /* Por que no se construyo esta variable si tenemos la informacion??? */
label variable edupre_ci "Asistencia a Educacion preescolar" 

**************
***eduac_ci***
**************
gen byte eduac_ci=. 
label variable eduac_ci "Superior universitario vs superior no universitario"


***************
***asiste_ci***
***************

/*
*Modificación Mayra Sáenz -02/01/2016: Se incorpora la nueva sintaxis para la generación de las variables
asiste_ci y aedu_ci elaborada por Iván Bornacelly SCL/EDU.

Iván Bornacelly: La variable ed09 indica quienes de los que asisten están en una institución público,
privada o privada subvencionada. Los valores que son missing no necesariamente indican que no asiste
a alguna institución. Los valores missing también pueden ser por que hay población menor a 5 años de 
edad a la cuál no se le hace la pregunta y se está incluyendo dentro del conteo. Por lo tanto el código
debería ser el siguiente:
*/
/*
gen asiste_ci=.
replace asiste_ci=1 if ed08>=1 & ed08<=18
replace asiste_ci=0 if ed08==19
*Los que no saben o no responden quedan como missing
label variable asiste_ci "Asiste actualmente a la escuela"
*/

/*Modificación Mayra Sáenz -01/23/2017: Se incluye a los que no asisten o no asistieron a una institución de enseñanza educativa ed03==6 porque la ed08 sólo responden los que 
                                        responden que sí asisten o asistieron a una institución */
***************
***asiste_ci***
***************
gen asiste_ci=.
replace asiste_ci=1 if ed08>=1 & ed08<=18
replace asiste_ci=0 if ed08==19 | ed03==6
label variable asiste_ci "Asiste actualmente a la escuela"



*****************
***pqnoasis_ci***
*****************

gen pqnoasis_ci = ed10
replace pqnoasis_ci=. if ed10==99

**************
*pqnoasis1_ci*
**************
**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**

g       pqnoasis1_ci = 1 if ed10 ==1 | ed10 ==3
replace pqnoasis1_ci = 2 if ed10 ==2
replace pqnoasis1_ci = 3 if ed10 ==11 | ed10 ==12 | ed10 ==14
replace pqnoasis1_ci = 4 if ed10 ==15
replace pqnoasis1_ci = 5 if ed10 ==13
replace pqnoasis1_ci = 6 if ed10 ==5
replace pqnoasis1_ci = 7 if ed10 ==4
replace pqnoasis1_ci = 8 if ed10 ==6  | ed10 ==7  | ed10 ==8  | ed10 ==9 | ed10 ==10 
replace pqnoasis1_ci = 9 if ed10 ==16 | ed10 ==17 | ed10 ==18

label define pqnoasis1_ci 1 "Problemas económicos" 2 "Por trabajo" 3 "Problemas familiares o de salud" 4 "Falta de interés" 5	"Quehaceres domésticos/embarazo/cuidado de niños/as" 6 "Terminó sus estudios" 7	"Edad" 8 "Problemas de acceso"  9 "Otros"
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
replace edupub_ci=0 if ed09>=2 & ed09<=3

***************
***tecnica_ci**
***************

gen tecnica_ci=.
replace tecnica_ci=1 if ed0504>=1801 & ed0504<=1804
recode tecnica_ci .=0 
label var tecnica_ci "1=formacion terciaria tecnica"


********************************************
***Variables de Infraestructura del hogar***
********************************************

***************
**aguared_ch***
***************

gen aguared_ch=(v06==1 | v06==2 | v06==3| v06==4)  


****************
**aguadist_ch***
****************

* MGR: modifico sintáxis para incluir a los que la fuente de agua está fuera de la vivienda y terreno
/*
gen aguadist_ch=. 

	replace aguadist_ch=1 if v09==2
	replace aguadist_ch=2 if v09==1| v09==4
	
	label var aguadist_ch "Ubicación de la principal fuente de agua"
	label def aguadist_ch 1"Dentro de la vivienda" 2"Fuera de la vivienda pero en el terreno"
	label def aguadist_ch 3"Fuera de la vivienda y del terreno", add
	label val aguadist_ch aguadist_ch
*/


gen aguadist_ch=. 
replace aguadist_ch=1 if v09==2
replace aguadist_ch=2 if v09==1| v09==4
replace aguadist_ch=3 if aguadist~=1 & aguadist~=2 & aguadist==. 
label var aguadist_ch "Ubicación de la principal fuente de agua"
label def aguadist_ch 1"Dentro de la vivienda" 2"Fuera de la vivienda pero en el terreno"
label def aguadist_ch 3"Fuera de la vivienda y del terreno", add
label val aguadist_ch aguadist_ch


****************
**aguamala_ch***
****************

*MGR: corrección a sintáxis para sacar a los que reciben agua por medio de aguatero o agua embotellada las cuales no son aguamala
*cuya fuente de agua es diferente de red pública, privada o de pozo
/*
gen aguamala_ch=0 
replace aguamala_ch=(v08>=8 & v08 <=13)
label var aguamala_ch "Agua unimproved según MDG" 
*/
gen aguamala_ch=0 
replace aguamala_ch=(v08>=8 & v08 <=10) | v08==13
label var aguamala_ch "Agua unimproved según MDG" 

****************
**aguamide_ch***
****************

gen aguamide_ch=.


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
****bano_ch*****
****************
* Si existe la pregunta tiene baño (v12) pero se obtiene los mismos resultados con la v13 
* que pregunta del tipo de desague sanitario.
gen bano_ch=(v12==1)


****************
****banoex_ch***
****************

gen banoex_ch=. 

****************
****des1_ch*****
****************

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

**Daniela Zuluaga- Enero 2018: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**
	
*********************
***aguamejorada_ch***
*********************
g       aguamejorada_ch = 1 if (v06 >=1 & v06 <=7) | v06 == 10
replace aguamejorada_ch = 0 if (v06 >=8 & v06 <=9) | v06 == 11 | v06 == 12
		
*********************
***banomejorado_ch***
*********************
g       banomejorado_ch = 1 if (v13 >=1 & v13 <=3) | (v13 >=5 & v13 <=6) 
replace banomejorado_ch = 0 if  v13 ==4 | (v13 >=7 & v13 <=8) | v12==6

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
*recode v17 (99999999999=.)
gen vivialq_ch=v18
replace vivialq_ch=. if v18==99999999999

*********************
****vivialqimp_ch****
*********************
*recode v19 (99999999999=.)
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
	
	gen migrante_ci=.
	label var migrante_ci "=1 si es migrante"
	
	**********************
	*** migantiguo5_ci ***
	**********************
	
	gen migantiguo5_ci=.
	label var migantiguo5_ci "=1 si es migrante antiguo (5 anos o mas)"
		
	**********************
	*** migrantelac_ci ***
	**********************
	
	gen migrantelac_ci=.
	label var migrantelac_ci "=1 si es migrante proveniente de un pais LAC"
	
	
/*_____________________________________________________________________________________________________*/
* Asignación de etiquetas e inserción de variables externas: tipo de cambio, Indice de Precios al 
* Consumidor (2011=100), líneas de pobreza
/*_____________________________________________________________________________________________________*/


do "$gitFolder\armonizacion_microdatos_encuestas_hogares_scl\_DOCS\\Labels&ExternalVars_Harmonized_DataBank.do"

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
edus1c_ci edus2i_ci edus2c_ci edupre_ci eduac_ci asiste_ci pqnoasis_ci pqnoasis1_ci	repite_ci repiteult_ci edupub_ci tecnica_ci ///
aguared_ch aguadist_ch aguamala_ch aguamide_ch luz_ch luzmide_ch combust_ch	bano_ch banoex_ch des1_ch des2_ch piso_ch aguamejorada_ch banomejorado_ch  ///
pared_ch techo_ch resid_ch dorm_ch cuartos_ch cocina_ch telef_ch refrig_ch freez_ch auto_ch compu_ch internet_ch cel_ch ///
vivi1_ch vivi2_ch viviprop_ch vivitit_ch vivialq_ch	vivialqimp_ch migrante_ci migantiguo5_ci migrantelac_ci, first

/*Homologar nombre del identificador de ocupaciones (isco, ciuo, etc.) y de industrias y dejarlo en base armonizada 
para análisis de trends (en el marco de estudios sobre el futuro del trabajo)*/
rename b01c codocupa
rename b02c codindustria


compress


saveold "`base_out'", version(12) replace


log close

