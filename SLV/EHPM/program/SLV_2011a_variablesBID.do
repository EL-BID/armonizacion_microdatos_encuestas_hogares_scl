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

local PAIS SLV
local ENCUESTA EHPM
local ANO "2011"
local ronda a 

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
   



capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: El Salvador
Encuesta: EHPM
Round: a
Autores: Mayo 2013 - Yessenia Loayza
2013 - incoporacion de Variables LMK por Yessenia Loayza (desloay@hotmail.com)
Última versión: Maria Laura Oliveri - Email: mloliveri@iadb.org, lauraoliveri@yahoo.com
Modificación 2014: Mayra Sáenz - Email: mayras@iadb.org - saenzmayra.a@gmail.com
Fecha última modificación: 23 de Octubre de 2013

			  
							SCL/LMK - IADB
****************************************************************************/
/***************************************************************************
Detalle de procesamientos o modificaciones anteriores:
****************************************************************************/

clear all
set more off
use "`base_in'", clear

foreach v of varlist _all {
      capture rename `v' `=lower("`v'")'
   }

		**********************************
		***VARIABLES DEL IDENTIFICACION***
		**********************************
		
	****************
	* region_BID_c *
	****************
	
gen region_BID_c=1

label var region_BID_c "Regiones BID"
label define region_BID_c 1 "Centroamérica_(CID)" 2 "Caribe_(CCB)" 3 "Andinos_(CAN)" 4 "Cono_Sur_(CSC)"
label value region_BID_c region_BID_c

***********
* Region_c *
************
*Inclusión Mayra Sáenz - Julio 2013

gen region_c=  depto 

label define region_c  ///
          1 "Ahuachapán" ///
           2 "Santa Ana" ///
           3 "Sonsonate" ///
           4 "Chalatenango" ///
           5 "La Libertad" ///
           6 "San Salvador" ///
           7 "Cuscatlán" ///
           8 "La Paz" ///
           9 "Cabañas" ///
          10 "San Vicente" ///
          11 "Usulután" ///
          12 "San Miguel" ///
          13 "Morazán" ///
          14 "La Unión" 
		    
label value region_c region_c
label var region_c "División política, departamento"
***************
***factor_ch***
***************
gen factor_ch=fac00 /*todos los factores 00-04 son los mismos*/
label variable factor_ch "Factor de expansion del hogar"

***************
****idh_ch*****
***************
sort lote tipo folio viv 
egen idh_ch = concat(lote tipo folio viv)
label variable idh_ch "ID del hogar"

*************
****idp_ci****
**************
gen idp_ci=r101
label variable idp_ci "ID de la persona en el hogar"

**********
***zona***
**********
gen zona_c=area
label variable zona_c "Zona del pais"
label define zona_c 1 "Urbana" 0 "Rural"
label value zona_c zona_c

************
****pais****
************
gen str3 pais_c="SLV"
label variable pais_c "Pais"

**********
***anio***
**********
gen anio_c=2011
label variable anio_c "Anio de la encuesta"

***********
***mes_c***
***********
gen mes_c=mes
label variable mes_c "Mes de la encuesta"



		****************************
		***VARIABLES DEMOGRAFICAS***
		****************************

*****************
***relacion_ci***
*****************
gen relacion_ci=1     if r103==1
replace relacion_ci=2 if r103==2
replace relacion_ci=3 if r103==3
replace relacion_ci=4 if r103>=4 & r103<=9
replace relacion_ci=5 if r103==11
replace relacion_ci=6 if r103==10
label variable relacion_ci "Relacion con el jefe del hogar"
label define relacion_ci 1 "Jefe/a" 2 "Esposo/a" 3 "Hijo/a" 4 "Otros parientes" 5 "Otros no parientes"
label define relacion_ci 6 "Empleado/a domestico/a", add
label value relacion_ci relacion_ci

***************
***factor_ci***
***************
gen factor_ci=fac01
label variable factor_ci "Factor de expansion del individuo"

**********
***sexo***
**********
gen sexo_ci=r104
label var sexo_ci "Sexo del individuo" 
label define sexo_ci 1 "Hombre" 2 "Mujer"
label value sexo_ci sexo_ci

**********
***edad***
**********
gen edad_ci=r106
label variable edad_ci "Edad del individuo"

**************
***civil_ci***
**************
gen civil_ci=.
replace civil_ci=1 if r107==6
replace civil_ci=2 if r107==1 | r107==2 
replace civil_ci=3 if r107==4 | r107==5
replace civil_ci=4 if r107==3
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
**** nuclear   
replace clasehog_ch=2 if (nhijos_ch>0| nconyuges_ch>0) & (notropari_ch==0 & notronopari_ch==0)
**** ampliado
replace clasehog_ch=3 if notropari_ch>0 & notronopari_ch==0
**** compuesto  
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
gen miembros_ci=(relacion_ci<=4)
label variable miembros_ci "Miembro del hogar"

******************************************************************************
*	VARIABLES DE DIVERSIDAD
******************************************************************************
**María Antonella Pereira & Nathalia Maya - Marzo 2021 
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

		************************************
		*** VARIABLES DEL MERCADO LABORAL***
		************************************

***************
****condocup_ci*
****************
/*
gen condocup_ci=.
replace condocup_ci=1 if r403==1 | r404<=9 | r405b==1 | (r406>=1 & r406<=11)
replace condocup_ci=2 if r407==1 & (r408>=1 & r408<=8)
replace condocup_ci=3 if condocup_ci!=1 & condocup_ci!=2
replace condocup_ci=4 if edad_ci<10
label define condocup_ci 1"ocupados" 2"desocupados" 3"inactivos" 4"menor de PET"
label value condocup_ci condocup_ci
label var condocup_ci "Condicion de ocupacion utilizando definicion del pais"
*/

* Se considera el limite inferior de la encuesta que es de 5 anios y mas. MGD 06/09/2014
gen condocup_ci=.
replace condocup_ci=1 if r403==1 | r404<=9 | (r405==1 & r406<=11) | (r405b==1 & r406b<=2) 
replace condocup_ci=2 if condocup_ci!=1 & (r407==1 | r408<=8) & r409a==1
replace condocup_ci=3 if (condocup_ci!=1 & condocup_ci!=2) & edad_ci>=5
replace condocup_ci=4 if edad_ci<5
label define condocup_ci 1"ocupados" 2"desocupados" 3"inactivos" 4"menor de PET"
label value condocup_ci condocup_ci
label var condocup_ci "Condicion de ocupacion utilizando definicion del pais"

************
***emp_ci***
************
gen emp_ci=(condocup_ci==1)

****************
***desemp_ci***
****************
gen desemp_ci=(condocup_ci==2)

*************
***pea_ci***
*************
gen pea_ci=(emp_ci==1 | desemp_ci==1)

/*
************
***emp_ci***
************
gen emp_ci=(r403==1 | r404<=9 | r405b==1 | (r406>=1 & r406<=11))
label var emp_ci "Ocupado"

 
****************
***desemp1_ci***
****************
gen desemp1_ci=(emp_ci==0 &  r407==1) /*periodo de referencia es el mes */
replace desemp1_ci=. if r403==.
la var desemp1_ci "Personas sin trabajo que buscaron en el periodo de referencia"

****************
***desemp2_ci*** 
****************
gen desemp2_ci=(desemp1_ci==1 | (r409>=4 & r409<=7))
replace desemp2_ci=. if r403==.
la var desemp2_ci "des1 + no trabajaron ni buscaron en la ult semana pero esperan respuesta de solicit"

****************
***desemp3_ci***
****************
gen desemp3_ci=. /*el periodo de referencia  es un mes, no hay otra preg*/
label var desemp3_ci "desemp2_ci + personas que buscaron antes del periodo de referencia"

*************
***pea1_ci***
*************
gen pea1_ci=0
replace pea1_ci=1 if emp_ci==1 |desemp1_ci==1
label var pea1_ci "Población Económicamente Activa con desemp1_ci"

*************
***pea2_ci***
*************
gen pea2_ci=0
replace pea1_ci=1 if emp_ci==1 |desemp2_ci==1
label var pea2_ci "Población Económicamente Activa con desemp2_ci"

*************
***pea3_ci***
*************
gen pea3_ci=.
label var pea3_ci "Población Económicamente Activa con desemp3_ci"
*/
*****************
***desalent_ci***
*****************
gen desalent_ci=(r409==3)
replace desalent_ci=. if r409==.
label var desalent_ci "Trabajadores desalentados"

*****************
***horaspri_ci***
*****************
egen horaspri_ci= rsum(r411a r411d) if emp_ci==1
label var horaspri_ci "Horas totales trabajadas en la actividad principal (semana)"

*****************
***horastot_ci***
*****************
egen horastot_ci=rsum(horaspri_ci r433) if emp_ci==1
label var horastot_ci "Horas totales trabajadas en todas las actividades (semana)"

***************
***subemp_ci***
***************
gen subemp_ci=0
replace subemp_ci=1 if horaspri_ci<=30  & emp_ci==1 & r413==2 | r413==3
label var subemp_ci "Personas en subempleo por horas"

*******************
***tiempoparc_ci***
*******************
gen tiempoparc_ci=(emp_ci==1 & r413==1 & horastot_ci<30)
replace tiempoparc_ci=. if emp_ci==0
label var tiempoparc_c "Personas que trabajan medio tiempo"

******************
***categopri_ci***
******************
gen categopri_ci=.
replace categopri_ci=1 if r418==1
replace categopri_ci=2 if r418==2 | r418==3 | r418==4 
replace categopri_ci=3 if r418==6 | r418==7 | r418==9  | r418==8 
replace categopri_ci=4 if r418==5 
replace categopri_ci=0 if r418==10 
replace categopri_ci=. if emp_ci==0
label define categopri_ci 0 "Otros" 1"Patron" 2"Cuenta propia" 0"Otro"
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
/*
*****************
***contrato_ci***
*****************
gen contrato_ci=(emp_ci==1 & r419==1)
replace contrato_ci=. if emp_ci!=1 | r419==.| r419==3
label var contrato_ci "Ocupados que tienen contrato firmado de trabajo"

***************
***segsoc_ci***
***************
gen segsoc_ci=(r422==1 | r422==2) if emp_ci==1
label var segsoc_ci "Personas que tienen seguridad social en salud por su trabajo"*/

*****************
***nempleos_ci***
*****************
gen nempleos_ci=.
replace nempleos_ci=1 if emp_ci==1 & r432==2
replace nempleos_ci=2 if emp_ci==1 & r432==1
replace nempleos_ci=. if emp_ci==0
label var nempleos_ci "Número de empleos" 
/*
*****************
***firmapeq_ci***
*****************
gen firmapeq_ci=0
replace firmapeq_ci=1 if r421<=5
replace firmapeq_ci=0 if r421>5 & r421!=. 
replace firmapeq_ci=. if emp_ci==0
label var firmapeq_ci "Trabajadores informales"*/

*****************
***spublico_ci***
*****************
gen spublico_ci=(r420==2 & emp_ci==1) 
replace spublico_ci=. if emp_ci==0 
label var spublico_ci "Personas que trabajan en el sector público"

**************
***ocupa_ci***
**************
*modificacion MLO 2014,01
gen ocupa_ci=.

replace ocupa_ci=1 if (r414>=2111 & r414<=3480) 
replace ocupa_ci=2 if (r414>=1110 & r414<=1319) 
replace ocupa_ci=3 if (r414>=4110 & r414<=4223)
replace ocupa_ci=4 if (r414>=9110 & r414<=9113) | (r414>=5210 & r414<=5230) 
replace ocupa_ci=5 if (r414>=5111 & r414<=5169) | (r414>=9120 & r414<=9162) 
replace ocupa_ci=6 if (r414>=6110 & r414<=6210) | (r414>=9210 & r414<=9220)
replace ocupa_ci=7 if (r414>=7111 & r414<=8340) | (r414>=9311 & r414<=9333) 
replace ocupa_ci=8 if r414==110 
replace ocupa_ci=. if emp_ci!=1 
*GRANDES GRUPOS OCUPACIONALES ACORDE A "CLASIFICACION INTERNACIONAL UNIFORME DE OCUPACIONES, 1988 (CIUO-88)" QUE SUSTITUYE A CIUO-68


label var ocupa_ci "Ocupacion laboral"
label def ocupa_ci 1"profesional y tecnico" 2"director o funcionario sup" 3"administrativo y nivel intermedio"
label def ocupa_ci  4 "comerciantes y vendedores" 5 "en servicios" 6 "trabajadores agricolas", add
label def ocupa_ci  7 "obreros no agricolas, conductores de maq y ss de transporte", add
label def ocupa_ci  8 "FFAA" 9 "Otras ", add
label val ocupa_ci ocupa_ci

*************
***rama_ci***
*************
*No tenemos la descripción de los códigos
* MLO = supongo que se mantiene CIIU rev 3
* MGD: se utiliza  CIIU REV. 4
g rama_ci=. 
replace rama_ci=1 if (r416>=100 & r416<=322) & emp_ci==1 
replace rama_ci=2 if (r416>=510 & r416<=990) & emp_ci==1 
replace rama_ci=3 if (r416>=1010 & r416<=3320) & emp_ci==1 
replace rama_ci=4 if (r416>=3510 & r416<=3900) & emp_ci==1 
replace rama_ci=5 if (r416>=4100 & r416<=4390) & emp_ci==1 
replace rama_ci=6 if ((r416>=4510 & r416<=4799) | (r416>=5510 & r416<=5630))& emp_ci==1 
replace rama_ci=7 if ((r416>=4911 & r416<=5320) | (r416>=6110 & r416<=6190)) & emp_ci==1 
replace rama_ci=8 if (r416>=6411 & r416<=8299) & emp_ci==1 
replace rama_ci=9 if ((r416>=5811 & r416<=6022) | (r416>=6201 & r416<=6399) | (r416>=8411 & r416<=9900)) & emp_ci==1 

label var rama_ci "Rama de actividad"
label def rama_ci 1"Agricultura, caza, silvicultura y pesca" 2"Explotación de minas y canteras" 3"Industrias manufactureras"
label def rama_ci 4"Electricidad, gas y agua" 5"Construcción" 6"Comercio, restaurantes y hoteles" 7"Transporte y almacenamiento", add
label def rama_ci 8"Establecimientos financieros, seguros e inmuebles" 9"Servicios sociales y comunales", add
label val rama_ci rama_ci

****************
***durades_ci***
****************
gen durades_ci= .
label variable durades_ci "Duracion del desempleo en meses"
 
*******************
***antiguedad_ci***
*******************
gen antiguedad_ci=.
label var antiguedad_ci "Antiguedad en la actividad actual en anios"


			**************
			***INGRESOS***
			**************

***************
***ylmpri_ci***
***************
*Para asalariados
gen yprid=.
replace yprid=r424*30 	if r423==1
replace yprid=r424*4.3 	if r423==2
replace yprid=r424*2 	if r423==3
replace yprid=r424 		if r423==4 | r423==5
gen hrsextrasd=		r42501a*r42501b/12 
gen vacacionesd=	r42502a*r42502b/12 
gen aguinaldod=		r42503a*r42503b/12 
gen bonificacionesd=r42504a*r42504b/12 
gen propina=r42511a*r42511b/12 
egen yprijbd=rsum(yprid hrsextrasd vacacionesd aguinaldod bonificacionesd propina), missing
drop yprid-propina

*Para trabajadores independientes
gen cost = r429*-1
egen ingrneto= rsum(r428 cost), missing
replace ingrneto=0 if ingrneto<0
gen yprijbi=. 
replace yprijbi=ingrneto*30 	if r427==1
replace yprijbi=ingrneto*4.3 	if r427==2
replace yprijbi=ingrneto*2 		if r427==3
replace yprijbi=ingrneto 		if r427==4 | r427==9
replace yprijbi=ingrneto/2 		if r427==5
replace yprijbi=ingrneto/3 		if r427==6
replace yprijbi=ingrneto/6 		if r427==7
replace yprijbi=ingrneto/12 	if r427==8

egen ylmpri_ci=rsum(yprijbi yprijbd), missing
label var ylmpri_ci "Ingreso laboral monetario actividad principal"
drop yprijbi yprijbd

******************
****nrylmpri_ci***
******************
gen nrylmpri_ci=(ylmpri_ci==. & emp_ci==1)
replace nrylmpri_ci=. if emp_ci!=1
la var nrylmpri_ci "Identificador de No respuesta del ingreso de la actividad principal"

****************
***ylnmpri_ci***
****************

g food1=r42505a*r42505b/12 
g ropa1=r42506a*r42506b/12 
g merca1=r42507a*r42507b/12 
g vivi1=r42508a*r42508b/12 
g trans1=r42509a*r42509b/12 
g segur1=r42510a*r42510b/12 
g otross1=r42512a*r42512b/12 

egen ylnmpri_ci=rsum(food1 ropa1 merca1 vivi1 trans1 segur1 otross1), missing
replace ylnmpri_ci=. if emp_ci!=1
label var ylnmpri_ci "Ingreso laboral NO monetario actividad principal"  
drop food1-otross1

***************
***ylmsec_ci***
***************

gen hrsextrasd1     =r43501a*r43501b/12 
gen vacacionesd1    =r43502a*r43502b/12 
gen aguinaldod1     =r43503a*r43503b/12 
gen bonificacionesd1=r43504a*r43504b/12 
gen propina1        =r43511a*r43511b/12 
egen yprijbd1 =rsum(hrsextrasd1 vacacionesd1 aguinaldod1 bonificacionesd1 propina1), missing

egen ylmsec_ci=rsum(r434 yprijbd1), missing
replace ylmsec_ci=. if emp_ci!=1
label var ylmsec_ci "Ingreso laboral monetario segunda actividad" 
drop  hrsextrasd1-yprijbd1

****************
***ylnmsec_ci***
****************
g food2=r43505a*r43505b/12 
g ropa2=r43506a*r43506b/12 
g merca2=r43507a*r43507b/12 
g vivi2=r43508a*r43508b/12 
g trans2=r43509a*r43509b/12 
g segur2=r43510a*r43510b/12 
g otross2=r43512a*r43512b/12 

egen ylnmsec_ci=rsum(food2 ropa2 merca2 vivi2 trans2 segur2 otross2), missing
replace ylnmsec_ci=. if emp_ci!=1 
label var ylnmsec_ci "Ingreso laboral no monetario actividad secundaria"
drop food2-otross2

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
egen ylm_ci= rsum(ylmpri_ci ylmsec_ci ylmotros_ci), missing
replace ylm_ci=. if ylmpri_ci==. &  ylmsec_ci==. & ylmotros_ci==. 
label var ylm_ci "Ingreso laboral monetario total"  

*************
***ylnm_ci***
*************
egen ylnm_ci=rsum(ylnmpri_ci ylnmsec_ci ylnmotros_ci), missing
replace ylnm_ci=. if ylnmpri_ci==. &  ylnmsec_ci==. & ylnmotros_ci==.
label var ylnm_ci "Ingreso laboral NO monetario total"  

*************
***ynlm_ci***
*************
gen remesas =r44401a*r44401b/12
gen ayuda   =r44402a*r44402b/12
gen cuotalim=r44403a*r44403b/12
gen alqui   =r44404a*r44404b/12
gen alqneg  =r44405a*r44405b/12
gen alqterr =r44406a*r44406b/12
gen jubil   =r44407a*r44407b/12
gen deveh   =r44408a*r44408b/12
gen pension =r44409a*r44409b/12
gen ahorros =r44410a*r44410b/12
gen otros   =r44411a*r44411b/12

gen utilidades   =r44501/12
gen dividendos   =r44502/12
gen intereses    =r44503/12
gen herencias    =r44504/12
gen indemnizacion=r44505/12
gen ayudagob     =r44506/12
gen acteventual  =r44507/12
gen arrendamiento=r44508/12
gen remesaevent1 =r44509/12
gen remesaevent2 =r44510/12
gen aguinaldo    =r44511/12
gen otrosy       =r44512/12

egen ynlm_ci=rsum(remesas-otrosy), missing
egen miss=rowmiss(remesas-otrosy)
replace ynlm_ci=. if miss==23
label var ynlm_ci "Ingreso no laboral monetario" 
drop ayuda-otrosy miss 

*******************
*** nrylmpri_ch ***
*******************
by idh_ch, sort: egen nrylmpri_ch=sum(nrylmpri_ci) if miembros_ci==1
replace nrylmpri_ch=1 if nrylmpri_ch>0 & nrylmpri_ch!=.
replace nrylmpri_ch=. if nrylmpri_ch==.
label var nrylmpri_ch "Hogares con algún miembro que no respondió por ingresos"

*************
***ylnm_ci***
*************
gen ynlnm_ci=.
label var ynlnm_ci "Ingreso no laboral no monetario" 

		************************
		*** INGRESO DEL HOGAR***
		************************

**************
*** ylm_ch ***
**************
by idh_ch, sort: egen ylm_ch=sum(ylm_ci) if miembros_ci==1
label var ylm_ch "Ingreso laboral monetario del hogar"

***************
*** ylnm_ch ***
***************
by idh_ch, sort: egen ylnm_ch=sum(ylnm_ci) if miembros_ci==1
label var ylnm_ch "Ingreso laboral no monetario del hogar"

****************
*** ylmnr_ch ***
****************
by idh_ch, sort: egen ylmnr_ch=sum(ylm_ci) if miembros_ci==1
replace ylmnr_ch=. if nrylmpri_ch==1
label var ylmnr_ch "Ingreso laboral monetario del hogar"

***************
*** ynlm_ch ***
***************
by idh_ch, sort: egen ynlm_ch=sum(ynlm_ci) if miembros_ci==1
label var ynlm_ch "Ingreso no laboral monetario del hogar"

**************
***ynlnm_ch***
**************
gen ynlnm_ch=.
label var ynlnm_ch "Ingreso no laboral no monetario del hogar"

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

*****************
***rentaimp_ch***
*****************
gen rentaimp_ch=.
label var rentaimp_ch "Rentas imputadas del hogar"

*****************
***autocons_ci***
*****************
gen autocons_ci=r431
label var autocons_ci "Autoconsumo reportado por el individuo"

*****************
***autocons_ch***
*****************
by idh_ch, sort: egen autocons_ch=sum(autocons_ci) if miembros_ci==1
label var autocons_ch "Autoconsumo reportado por el hogar"

****************
***remesas_ci***
****************
gen remesas_ci=remesas
label var remesas_ci "Remesas mensuales reportadas por el individuo" 
drop remesas
****************
***remesas_ch***
****************
by idh_ch, sort: egen remesas_ch=sum(remesas_ci) if miembros_ci==1
label var remesas_ch "Remesas mensuales del hogar" 


			****************************
			***VARIABLES DE EDUCACION***
			****************************

*************
***aedu_ci***
*************
gen aedu_ci=aproba1
label var aedu_ci "Anios de educacion aprobados" 

**************
***eduno_ci***
**************
gen eduno_ci=0
replace eduno_ci=1 if aedu_ci==0
label var eduno_ci "Sin educacion"

**************
***edupi_ci***
**************
gen edupi_ci=0
replace edupi_ci=1 if (aedu_ci>=1 & aedu_ci<6) 
label var edupi_ci "Primaria incompleta"

**************
***edupc_ci***
**************
gen edupc_ci=0
replace edupc_ci=1 if aedu_ci==6 
replace edupc_ci=. if aedu_ci==.
label var edupc_ci "Primaria completa"

**************
***edusi_ci***
**************
gen edusi_ci=0
replace edusi_ci=1 if (aedu_ci>6 & aedu_ci<12) 
label var edusi_ci "Secundaria incompleta"

**************
***edusc_ci***
**************
gen edusc_ci=(aedu_ci==12)
replace edusc_ci=. if aedu_ci==.
label var edusc_ci "Secundaria Completa"

**************
***eduui_ci***
**************
gen eduui_ci=(aedu_ci>12 & aedu_ci<17) 
label var eduui_ci "Universitaria o Terciaria Incompleta"

**************
***eduuc_ci***
**************
gen eduuc_ci=(aedu_ci>=17)
label var eduuc_ci "Universitaria o Terciaria Completa"

***************
***edus1i_ci***
***************
gen  edus1i_ci=0
replace edus1i_ci=1 if (aedu_ci>=6 & aedu_ci<9)
label var edus1i_ci "1er ciclo de la secundaria incompleto"

***************
***edus1c_ci***
***************
gen byte edus1c_ci=0
replace edus1c_ci=1 if aedu_ci==9
label var edus1c_ci "1er ciclo de la secundaria completo"

***************
***edus2i_ci***
***************
gen edus2i_ci=(aedu_ci>9 & aedu_ci<12)
label var edus2i_ci "2do ciclo de Educacion Secundaria Incompleto"

***************
***edus2c_ci***
***************
gen edus2c_ci=(aedu_ci==12)
label var edus2c_ci "2do ciclo de Educacion Secundaria Completo"

local var = "eduno edupi edupc edusi edusc edusc eduui eduuc edus1i edus1c edus2i edus2c"
foreach x of local var {
replace `x'_ci=. if aedu_ci==.
}

***************
***edupre_ci***
***************
gen edupre_ci=(r204==0 |r204==1) 
label var edupre_ci "Educacion preescolar"
****************
***asispre_ci***
****************
*Agregada por Iván Bornacelly - 01/23/2017
	g asispre_ci=.
	replace asispre_ci=1 if r203==1 & r204==1
	recode asispre_ci (.=0)
	la var asispre_ci "Asiste a educacion prescolar"		
**************
***eduac_ci***
**************
gen eduac_ci=.
label variable eduac_ci "Superior universitario vs superior no universitario"

***************
***asiste_ci***
***************
gen asiste_ci=(r203==1)
label variable asiste_ci "Asiste actualmente a la escuela"

**************
***pqnoasis_ci***
**************
gen pqnoasis_ci=r220 
label var pqnoasis_ci "Razones para no asistir a la escuela"

**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**
	
**************
*pqnoasis1_ci*
**************
g       pqnoasis1_ci = 1 if r220 ==3
replace pqnoasis1_ci = 2 if r220 ==1
replace pqnoasis1_ci = 3 if r220 ==4 | r220 ==5 | r220 ==6
replace pqnoasis1_ci = 4 if r220 ==10
replace pqnoasis1_ci = 5 if r220 ==2 | r220 ==12 | r220 ==15 | r220 ==16
replace pqnoasis1_ci = 6 if r220 ==8
replace pqnoasis1_ci = 7 if r220 ==7 
replace pqnoasis1_ci = 8 if r220 ==9  | r220 ==13 | r220 ==14
replace pqnoasis1_ci = 9 if r220 ==11 | r220 ==17

label define pqnoasis1_ci 1 "Problemas económicos" 2 "Por trabajo" 3 "Problemas familiares o de salud" 4 "Falta de interés" 5	"Quehaceres domésticos/embarazo/cuidado de niños/as" 6 "Terminó sus estudios" 7	"Edad" 8 "Problemas de acceso"  9 "Otros"
label value  pqnoasis1_ci pqnoasis1_ci

***************
***repite_ci***
***************
gen repite_ci=.
label var repite_ci "Ha repetido al menos un grado"

******************
***repiteult_ci***
******************
gen repiteult_ci= (r207a==1 | r218a ==1)
label var repiteult "Ha repetido el último grado"

***************
***edupub_ci***
***************
gen edupub_ci=.
replace edupub_ci=1 if r210a==1 
replace edupub_ci=0 if r210a==2 | r210a==3
label var edupub_ci "Personas que asisten a centros de ensenanza publicos"


		**********************************
		**** VARIABLES DE LA VIVIENDA ****
		**********************************


****************
***aguared_ch***
****************
gen aguared_ch=.
replace aguared_ch=(r312<5)
label var aguared_ch "Acceso a fuente de agua por red"

*****************
***aguadist_ch***
*****************
gen aguadist_ch=1		if r312==1 |r312==2
replace aguadist_ch=2	if r312==3 |r312==4
replace aguadist_ch=3	if r312==4.1
label var aguadist_ch "Ubicación de la principal fuente de agua"
label def aguadist_ch 1"Adentro de la casa" 2"Afuera de la casa pero dentro del terreno" 
label def aguadist_ch 3"Afuera de la casa y afuera del terreno", add
label val aguadist_ch aguadist_ch

*****************
***aguamala_ch***
*****************
gen aguamala_ch=.
label var aguamala_ch "Agua unimproved según MDG" 

*****************
***aguamide_ch***
*****************
gen aguamide_ch=.
label var aguamide_ch "Usan medidor para pagar consumo de agua"

************
***luz_ch***
************
gen luz_ch=0
replace luz_ch=1 if r311==1 | r311==2 | r311==6
label var luz_ch  "La principal fuente de iluminación es electricidad"

****************
***luzmide_ch***
****************
gen luzmide_ch=.
label var luzmide_ch "Usan medidor para pagar consumo de electricidad"

****************
***combust_ch***
****************
gen combust_ch=0
replace combust_ch=1 if  r326==1 | r326==2 | r326==3
label var combust_ch "Principal combustible gas o electricidad" 

*************
***bano_ch***
*************
gen bano_ch=0
replace bano_ch=1 if r317a>=1 &  r317a<=3
replace bano_ch=. if r317a==.
label var bano_ch "El hogar tiene servicio sanitario"

***************
***banoex_ch***
***************
gen banoex_ch=0
replace banoex_ch=1 if r321==2
replace banoex_ch=. if bano_ch==0
label var banoex_ch "El servicio sanitario es exclusivo del hogar"

*************
***des1_ch***
*************
gen des1_ch=.
replace des1_ch=0 if bano_ch==0
replace des1_ch=1 if r319>=1 & r319<=4
replace des1_ch=2 if r319>=5 & r319<=9
label var des1_ch "Tipo de desague según unimproved de MDG"
label def des1_ch 0"No tiene servicio sanitario" 1"Conectado a red general o cámara séptica"
label def des1_ch 2"Letrina o conectado a pozo ciego" 3"Desemboca en río o calle", add
label val des1_ch des1_ch

*************
***des2_ch***
*************
gen des2_ch=.
replace des2_ch=0 if bano_ch==0
replace des2_ch=1 if r319>=1 & r319<=9
label var des2_ch "Tipo de desague sin incluir definición MDG"
label def des2_ch 0"No tiene servicio sanitario" 1"Conectado a red general, cámara séptica, pozo o letrina"
label def des2_ch 2"Cualquier otro caso", add
label val des2_ch des2_ch

*************
***piso_ch***
*************

* Modificaciones Marcela Rubio Septiembre 2014
/*
gen piso_ch=1 		if r304==5
replace piso_ch=0 	if r304>=1 & r304<=4
replace piso_ch=2 	if r304==6
replace piso_ch=. 	if r304==.
label var piso_ch "Materiales de construcción del piso"  
label def piso_ch 0"Piso de tierra" 1"Materiales permanentes" 2"Otros"
label val piso_ch piso_ch
*/

gen piso_ch= . 
replace piso_ch = 0 if r304==5
replace piso_ch = 1 if r304>=1 & r304<=4 
replace piso_ch = 2 if r304==6
label var piso_ch "Materiales de construcción del piso"  
label def piso_ch 0"Piso de tierra" 1"Materiales permanentes" 2"Otros"
label val piso_ch piso_ch

**************
***pared_ch***
**************
gen pared_ch=0 		if r303==2 | r303==3 |r303==5 |r303==6 |r303==7 
replace pared_ch=1 	if r303==1 | r303==4
replace pared_ch=2 	if r303==8
replace pared_ch=. 	if r303==.
label var pared_ch "Materiales de construcción de las paredes"
label def pared_ch 0"No permanentes" 1"Permanentes" 2"otros"
label val pared_ch pared_ch

**************
***techo_ch***
**************
gen techo_ch=.
replace techo_ch=1 	if r302>=1 & r302<=4
replace techo_ch=0 	if r302>=5 & r302<=6
replace techo_ch=2 	if r302==7
label var techo_ch "Materiales de construcción del techo"
label def techo_ch 0"No permanentes" 1"Permanentes" 2"otros"
label val techo_ch techo_ch

**************
***resid_ch***
**************
gen resid_ch =0    if r329==1 | r329==2
replace resid_ch=1 if r329==4 | r329==5
replace resid_ch=2 if r329==6
replace resid_ch=3 if r329==3
replace resid_ch=. if r329==.
label var resid_ch "Método de eliminación de residuos"
label def resid_ch 0"Recolección pública o privada" 1"Quemados o enterrados"
label def resid_ch 2"Tirados a un espacio abierto" 3"Otros", add
label val resid_ch resid_ch

**Daniela Zuluaga- Enero 2018: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**
	
*********************
***aguamejorada_ch***
*********************
g       aguamejorada_ch = 1 if  (r312 >=1 & r312 <=4) |  (r312 >4 & r312 <5) | (r313 >=4 & r313 <=5) | r313 == 8 | r313 == 10
replace aguamejorada_ch = 0 if ((r312 >=5 & r312 <=6) & (r313 != 4 | r313 != 5 | r313 != 8 | r313 != 10) ) | (r313 >=1 & r313 <=3) | (r313 >=6 & r313 <=7) | r313 == 9 | (r313 >=11 & r313 <=13)		
								
*********************
***banomejorado_ch***
*********************
g       banomejorado_ch = 1 if  ((r319>=1 & r319 <=2) | r319 == 5 | r319 == 7 | r319 == 9) & r321 ==2
replace banomejorado_ch = 0 if (((r319>=1 & r319 <=2) | r319 == 5 | r319 == 7 | r319 == 9) & r321 ==1) | (r319>=3 & r319 <=4) | r319 == 6   | r319 == 8 | r319 == 10 | r317a ==4 | r318 ==2

*************
***dorm_ch***
*************
gen dorm_ch=r306
label var dorm_ch "Habitaciones para dormir"

****************
***cuartos_ch***
****************
gen cuartos_ch=r305
label var cuartos_ch "Habitaciones en el hogar"

***************
***cocina_ch***
***************
gen cocina_ch=.
label var cocina_ch "Cuarto separado y exclusivo para cocinar"

**************
***telef_ch***
**************
gen telef_ch=0
replace telef_ch=1 if r3281a==1
replace telef_ch=. if r3281a==.
label var telef_ch "El hogar tiene servicio telefónico fijo"

***************
***refrig_ch***
***************
gen refrig_ch=0
replace refrig_ch=1 if  r33005a==1
replace refrig_ch=. if  r33005a==.
label var refrig_ch "El hogar posee refrigerador o heladera"

**************
***freez_ch***
**************
gen freez_ch=.
label var freez_ch "El hogar posee congelador"

*************
***auto_ch***
*************
gen auto_ch=0
replace auto_ch=1 if r33012a==1
replace auto_ch=. if r33012a==.
label var auto_ch "El hogar posee automovil particular"

**************
***compu_ch***
**************
gen compu_ch=0
replace compu_ch=1 if r33009a==1
replace compu_ch=. if r33009a==.
label var compu_ch "El hogar posee computador"

*****************
***internet_ch***
*****************
gen internet_ch=0
replace internet_ch=1 if r3283a==1
replace internet_ch=. if r3283a==.
label var internet_ch "El hogar posee conexión a Internet"

************
***cel_ch***
************
gen cel_ch=0
replace cel_ch=1 if r3282a==1 
replace cel_ch=. if r3282a==.
label var cel_ch "El hogar tiene servicio telefonico celular"

**************
***vivi1_ch***
**************
gen vivi1_ch=1     if r301==1 | r301==1.1
replace vivi1_ch=2 if r301==2
replace vivi1_ch=3 if r301>=3 & r301<=9
replace vivi1_ch=. if r301==.
label var vivi1_ch "Tipo de vivienda en la que reside el hogar"
label def vivi1_ch 1"Casa" 2"Departamento" 3"Otros"
label val vivi1_ch vivi1_ch

**************
***vivi2_ch***
**************
gen vivi2_ch=0
replace vivi2_ch=1 if r301>=1 & r301<=2
replace vivi2_ch=. if r301==.
label var vivi2_ch "La vivienda es casa o departamento"

*****************
***viviprop_ch***
*****************
gen viviprop_ch=0 		if r308==1
replace viviprop_ch=1 	if r308==3 
replace viviprop_ch=2 	if r308==2 |r308==4 | r308==5
replace viviprop_ch=3 	if r308==6 |r308==7 | r308==8
replace viviprop_ch=. 	if r308==.
label var viviprop_ch "Propiedad de la vivienda"
label def viviprop_ch 0"Alquilada" 1"Propia y totalmente pagada" 2"Propia y en proceso de pago"
label def viviprop_ch 3"Ocupada (propia de facto)", add
label val viviprop_ch viviprop_ch

****************
***vivitit_ch***
****************
gen vivitit_ch=.
label var vivitit_ch "El hogar posee un título de propiedad"

****************
***vivialq_ch***
****************
gen vivialq_ch= r308c  
label var vivialq_ch "Alquiler mensual"

*******************
***vivialqimp_ch***
*******************
gen vivialqimp_ch=r310a 
label var vivialqimp_ch "Alquiler mensual imputado"


****************
*afiliado_ci****
****************
gen afiliado_ci=(r108a>=1 & r108a<=2) /*todas personas*/	
label var afiliado_ci "Afiliado a la Seguridad Social"
*Nota: seguridad social comprende solo los que en el futuro me ofrecen una pension.

****************
*cotizando_ci***
****************
gen cotizando_ci=0     if condocup_ci==1 | condocup_ci==2 
replace cotizando_ci=1 if (r422==1) & cotizando_ci==0 /*solo a emplead@s y asalariad@s, difiere con los otros paises*/
label var cotizando_ci "Cotizante a la Seguridad Social"

********************
*** instcot_ci *****
********************
gen instcot_ci=r108a
label var instcot_ci "institución a la cual cotiza"

*****************
*tipocontrato_ci*
*****************
/*
gen tipocontrato_ci=.
replace tipocontrato_ci=1 if r419==1 & emp_ci==1
replace tipocontrato_ci=2 if (r419==1 & r419a>0 & r419a<=12) & emp_ci==1
replace tipocontrato_ci=3 if r419==2 & emp_ci==1
replace tipocontrato_ci =. if r419==3 | emp_ci==0 
label var tipocontrato_ci "Tipo de contrato segun su duracion en act principal"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci
*/

* Incorporando variable de temporalidad en asalariados y firma de contrato. MGD 06/16/2014
gen tipocontrato_ci=.
replace tipocontrato_ci=1 if (r419==1 & r418==6) & categopri_ci==3
replace tipocontrato_ci=2 if (r419==1 & r418==7) & categopri_ci==3 
replace tipocontrato_ci=3 if (r419==2 | tipocontrato_ci==.) & categopri_ci==3
label var tipocontrato_ci "Tipo de contrato segun su duracion en act principal"
label define tipocontrato_ci 0 "Con contrato" 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci


*************
*tamemp_ci***
*************
gen tamemp_ci=1 if r421>=1 & r421<=5
replace tamemp_ci=2 if r421>=6 & r421<=50
replace tamemp_ci=3 if r421>50 & r421!=.
label var tamemp_ci "# empleados en la empresa segun rangos"
	label define tamemp_ci 1 "Pequena" 2 "Mediana" 3 "Grande" 
	label value tamemp_ci tamemp1_ci

*************
**pension_ci*
*************
gen pension_ci=0 
replace pension_ci=1 if (r44407a>0 & r44407a !=.) /* A todas las per mayores de cinco*/
label var pension_ci "1=Recibe pension contributiva"

*************
*ypen_ci*
*************
gen ypen_ci=r44407a*r44407b/12 if pension_ci==1
label var ypen_ci "Valor de la pension contributiva"

****************
*instpen_ci*****
****************
gen instpen_ci=.
label var instpen_ci "Institucion proveedora de la pension - variable original de cada pais" 

***************
*pensionsub_ci*
***************
gen pensionsub_ci=1 if r325a5==1 & edad_ci>=65
recode pensionsub_ci .=0
label var pensionsub_ci "1=recibe pension subsidiada / no contributiva"

*****************
**  ypensub_ci  *
*****************
* no se puede determinar el monto de la pension basica universal
gen ypensub_ci=.
label var ypensub_ci "Valor de la pension subsidiada / no contributiva"

*************
*cesante_ci* 
*************
generat cesante_ci=0 if condocup_ci==2
replace cesante_ci=1 if r410==1 & condocup_ci==2
label var cesante_ci "Desocupado - definicion oficial del pais"

*********
*lp_ci***
*********
gen lp_ci =.
label var lp_ci "Linea de pobreza oficial del pais"

*********
*lpe_ci***
*********
gen lpe_ci =.
*zona urbana
replace lpe_ci=178.90	if mes_c==1 & zona_c==1
replace lpe_ci=	178.90	if mes_c==2 & zona_c==1
replace lpe_ci=177.63	if mes_c==3 & zona_c==1
replace lpe_ci=181.99	if mes_c==4 & zona_c==1
replace lpe_ci=186.21	if mes_c==5 & zona_c==1
replace lpe_ci=187.61	if mes_c==6 & zona_c==1
replace lpe_ci=189.77	if mes_c==7 & zona_c==1
replace lpe_ci=188.50	if mes_c==8 & zona_c==1
replace lpe_ci=183.02	if mes_c==9 & zona_c==1
replace lpe_ci=181.61	if mes_c==10 & zona_c==1
replace lpe_ci=182.50	if mes_c==11 & zona_c==1
replace lpe_ci=179.42   if mes_c==12 & zona_c==1

*zona rural
replace lpe_ci=132.75	if mes_c==1 & zona_c==0
replace lpe_ci=136.70	if mes_c==2 & zona_c==0
replace lpe_ci=137.59	if mes_c==3 & zona_c==0
replace lpe_ci=144.00	if mes_c==4 & zona_c==0
replace lpe_ci=149.42	if mes_c==5 & zona_c==0
replace lpe_ci=151.61	if mes_c==6 & zona_c==0
replace lpe_ci=152.19	if mes_c==7 & zona_c==0
replace lpe_ci=151.61	if mes_c==8 & zona_c==0
replace lpe_ci=148.39	if mes_c==9 & zona_c==0
replace lpe_ci=145.75	if mes_c==10 & zona_c==0
replace lpe_ci=141.98	if mes_c==11 & zona_c==0
replace lpe_ci=140.37   if mes_c==12 & zona_c==0
label var lpe_ci "Linea de indigencia oficial del pais"


/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/

*************
**salmm_ci***
*************
*gen salmm_ci= 216.1 /*fuente: ILO*/

*2015 MGD: salarios segun actividad
g salmm_ci=.
replace salmm_ci=104 if rama_ci==1
replace salmm_ci=190 if rama_ci==3
replace salmm_ci=209 if rama_ci==6
replace salmm_ci=167 if salmm_ci==.
label var salmm_ci "Salario minimo legal"

*************
***tecnica_ci**
*************
gen tecnica_ci=(r204==5 | r217a==5)
label var tecnica_ci "=1 formacion terciaria tecnica"

*****************
**categoinac_ci**
*****************	
gen categoinac_ci=.	
replace categoinac_ci=1 if r409==13
replace categoinac_ci=2 if r409==8 | r409==15
replace categoinac_ci=3 if r409==12
recode categoinac_ci .=4 if condocup_ci==3 
label var categoinac_ci "Condición de inactividad"
	label define categoinac_ci 1 "jubilado/pensionado" 2 "estudiante" 3 "quehaceres_domesticos" 4 "otros_inactivos" 
	label value categoinac_ci categoinac_ci
	
*****************
***formal_ci*****
*****************
gen byte formal_ci=1 if cotizando_ci==1 & (condocup_ci==1 | condocup_ci==2)
recode formal_ci .=0 if (condocup_ci==1 | condocup_ci==2)
label var formal_ci "1=afiliado o cotizante / PEA"

*variables que faltan generar

gen tcylmpri_ci =.
gen tcylmpri_ch =.
gen tipopen_ci=.

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




compress


saveold "`base_out'", replace


log close


