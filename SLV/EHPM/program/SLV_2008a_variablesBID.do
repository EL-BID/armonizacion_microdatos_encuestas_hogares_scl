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
local ANO "2008"
local ronda a 

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
   



capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: El Salvador
Encuesta: EHPM
Round: a
Autores: 2009 por Melisa Morales para Suzanne Duryea
2013 - incoporacion de Variables LMK por Yessenia Loayza (desloay@hotmail.com)
Última versión: Maria Laura Oliveri - Email: mloliveri@iadb.org, lauraoliveri@yahoo.com
Modificación 2014: Mayra Sáenz - Email: mayras@iadb.org - saenzmayra.a@gmail.com
Fecha última modificación: 23 de Octubre de 2013

			  
							SCL/LMK - IADB
****************************************************************************/
/***************************************************************************
Detalle de procesamientos o modificaciones anteriores:
*Yanira Oviedo: se redicciona el archivo de datos de origen para obtener una base final menos pesada, se cambian los
*nombres de las variables a minúscula y se generan algunas variables que hacían falta
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

******************************************************************************
*	HOUSEHOLD VARIABLES
******************************************************************************
************
* Region_c *
************
*Inclusión Mayra Sáenz - Abril 2014
gen region_c= r004

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
***ine01***
***************
gen ine01= r004
label define ine01  ///
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
label value ine01 ine01


******************************
*	factor_ch
******************************
g factor_ch= fac00 
label var factor_ch "Household Expansion Factor"
******************************
*	idh_ch
******************************
sort lote lote tipo folio viv 
egen idh_ch= group(lote tipo folio viv)
label var idh_ch "ID del hogar"
******************************
*	idp_c
******************************
g idp_ci= numorden

******************************
*	zona_c
******************************
g byte zona_c=(area==1) 
label var zona_c "Zona del pais"
label define zona_c 1 "Urbana" 0 "Rural"
label value zona_c zona_c
******************************
*	pais_c
******************************
g str3 pais_c="SLV"
label var pais_c "El Salvador"
******************************
*	anio_c
******************************
g anio_c=2008
label var anio_c "Year of the survey"
******************************
*	mes_c
******************************
g mes_c=r016m 
label var mes_c "Mes de la entrevista"
label define mes_c 3 "March" 5 "May" 8 "August" 10 "October"
label value mes_c mes_c

******************************
*	relacion_ci
******************************
g relacion_ci= .
replace relacion_ci=1 if r103==1
replace relacion_ci=2 if r103==2
replace relacion_ci=3 if r103==3
replace relacion_ci=4 if r103>=4 & r103<=9
replace relacion_ci=5 if r103==11
replace relacion_ci=6 if r103==10
label var relacion_ci "Relacion con el jefe del hogar"
label define relacion_ci 1 "Jefe/a" 2 "Esposo/a" 3 "Hijo/a" 4 "Otros parientes" 5 "Otros no parientes"
label define relacion_ci 6 "Empleado/a domestico/a", add
label value relacion_ci relacion_ci


******************************************************************************
*	DEMOGRAPHIC VARIABLES
******************************************************************************

******************************
*	factor_ci
******************************
g factor_ci= fac01
label var factor_ci "Individual Expansion Factor"
******************************
*	sexo_ci
******************************
g sexo_ci=r104
label define sexo_ci 1 "Hombre" 2 "Mujer"
label value sexo_ci sexo_ci
******************************
*	edad_ci
******************************
g edad_ci= r106                    
******************************
*	civil_ci
******************************
g civil_ci= .
replace civil_ci=1 if r107==6
replace civil_ci=2 if r107==1 | r107==2
replace civil_ci=3 if r107==4 | r107==5
replace civil_ci=4 if r107==3
label var civil_ci "Estado civil"
label define civil_ci 1 "Soltero" 2 "Union formal o informal"
label define civil_ci 3 "Divorciado o separado" 4 "Viudo" , add
label value civil_ci civil_ci

******************************
*	jefe_ci
******************************
g jefe_ci=(relacion_ci==1)
label variable jefe_ci "Jefe de hogar"
***************************************************************************
*	nconyuges_ch & nhijos_ch & notropari_ch & notronopari_ch & nempdom_ch
****************************************************************************
by idh_ch, sort: egen nconyuges_ch=sum(relacion_ci==2)
by idh_ch, sort: egen nhijos_ch=sum(relacion_ci==3)
by idh_ch, sort: egen notropari_ch=sum(relacion_ci==4)
by idh_ch, sort: egen notronopari_ch=sum(relacion_ci==5)
by idh_ch, sort: egen nempdom_ch=sum(relacion_ci==6)
label var nconyuges_ch "Numero de conyuges"
label var nhijos_ch "Numero de hijos"
label var notropari_ch "Numero de otros familiares"
label var notronopari_ch "Numero de no familiares"
label var nempdom_ch "Numero de empleados domesticos"
******************************
*	clasehog_ch
******************************
g byte clasehog_ch=0
replace clasehog_ch=1 if nhijos_ch==0 & nconyuges_ch==0 & notropari_ch==0 & notronopari_ch==0
replace clasehog_ch=2 if (nhijos_ch>0| nconyuges_ch>0) & (notropari_ch==0 & notronopari_ch==0)
replace clasehog_ch=3 if notropari_ch>0 & notronopari_ch==0
replace clasehog_ch=4 if ((nconyuges_ch>0 | nhijos_ch>0 | notropari_ch>0) & (notronopari_ch>0))
replace clasehog_ch=5 if nhijos_ch==0 & nconyuges_ch==0 & notropari_ch==0 & notronopari_ch>0
label var clasehog_ch "Tipo de hogar"
label define clasehog_ch 1 " Unipersonal" 2 "Nuclear" 3 "Ampliado" 
label define clasehog_ch 4 "Compuesto" 5 " Corresidente", add
label value clasehog_ch clasehog_ch

/*al parecer es estandar esta definicion. Me parece que hogar unipersonal (y los otros) deberian considerar la 
condicion " & jefe_ci==1" . Preguntar a Suzanne porque no la incluyen*/

***************************************************************************************
*	nmiembros_ch & nmayor21_ch & nmenor21_ch & nmayor65_ch & nmenor6_ch & nmenor1_ch  
***************************************************************************************

by idh_ch, sort: egen nmiembros_ch=sum(relacion_ci>=1 & relacion_ci<=4)
label var nmiembros_ch "Numero de familiares en el hogar"

by idh_ch, sort: egen nmayor21_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad>=21)
label var nmayor21_ch "Numero de familiares mayores a 21 anios"

by idh_ch, sort: egen nmenor21_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad<21)
label var nmenor21_ch "Numero de familiares menores a 21 anios"

by idh_ch, sort: egen nmayor65_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad>=65)
label var nmayor65_ch "Numero de familiares mayores a 65 anios"

by idh_ch, sort: egen nmenor6_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad<6)
label var nmenor6_ch "Numero de familiares menores a 6 anios"

by idh_ch, sort: egen nmenor1_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad<1)
label var nmenor1_ch "Numero de familiares menores a 1 anio"

******************************
*	miembros_ci
******************************
*2014, 01 modificado segun documento metodologico by MLO
g miembros_ci=(relacion_ci<5)
*g miembros_ci=(relacion_ci<6)
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

******************************************************************************
*	LABOR MARKET
******************************************************************************

****************
****condocup_ci*
****************
/*
generat r405=1
replace r405=2 if r40501==2 & r40502==2 & r40503==2  & r40504==2  & r40505==2 & r40506==2 & r40507==2  & r40508==2  & r40509==2  
replace r405=. if r40501==. & r40502==. & r40503==.  & r40504==.  & r40505==. & r40506==. & r40507==.  & r40508==.  & r40509==.  

gen condocup_ci=.
replace condocup_ci=1 if r403==1 | r404==1 | r405==1
replace condocup_ci=2 if r406==1 & (r408>=1 & r408<=8)
replace condocup_ci=3 if condocup_ci!=1 & condocup_ci!=2
replace condocup_ci=4 if edad_ci<10
label define condocup_ci 1"ocupados" 2"desocupados" 3"inactivos" 4"menor de PET"
label value condocup_ci condocup_ci
label var condocup_ci "Condicion de ocupacion utilizando definicion del pais"
drop r405
*/
* Se considera el limite inferior de la encuesta que es de 5 anios y mas. MGD 06/09/2014
gen condocup_ci=.
replace condocup_ci=1 if r403==1 | r404==1 | (r40501==1 | r40502==1 | r40503==1 | r40504==1  | r40505==1 | r40506==1 | r40507==1  | r40508==1  | r40509==1) 
replace condocup_ci=2 if condocup_ci!=1 & (r406==1 | (r407==17 | r407==18) | r408<=8 | r409<=4)
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
******************************
*	emp_ci
******************************
g emp_ci=(r403==1 | (r403==2 & r404==1))
replace emp_ci=. if r403==. 
label var emp_ci "Empleado"
label define emp_ci 1"Si" 0 "No"
label value emp_ci emp_ci

******************************
*	desemp1_ci	& desemp2_ci & desemp3_ci 
******************************
g desemp1_ci=(emp_ci==0 &  r406==1)
replace desemp1_ci=. if r403==.
label var desemp1_ci "Personas sin trabajo que buscaron en el periodo de referencia"
 
g desemp2_ci=(desemp1_ci==1 | (emp_ci==0 & r406==2 & r407==18))
replace desemp2_ci=. if r403==.
label var desemp2_ci "des1 + no trabajaron ni buscaron en la ult semana pero esperan respuesta de solicit"
 
g desemp3_ci=.
*NA

*A los desocupados no les preguntan si buscaron trabajo antes de la semana de referencia

******************************
*	pea1_ci, pea2_ci, pea3_ci
******************************
g pea1_ci=(emp_ci==1 | desemp1_ci==1)
g pea2_ci=(emp_ci==1 | desemp2_ci==1)
g pea3_ci=.
*NA*/
******************************
*	desalent_ci
******************************
g desalent_ci=(emp_ci==0 & r406==2 & r407==3)
replace desalent_ci=. if emp_ci!=0
label var desalent_ci "Personas que creen que por alguna razon no conseguiran trabajo"


******************************
*	horaspri_ci & horastot_ci
******************************
egen horaspri_ci= rsum(r411a r411d) if emp_ci==1
label var horaspri_ci "Hs totales (semanales) trabajadas en act. principal"

egen horastot_ci=rsum(horaspri_ci r433) if emp_ci==1
label var horastot_ci "Hs totales (semanales)trabajadas en toda actividad"

******************************
*	subemp_ci
******************************
*NA
/*no se pregunta en la encuesta si se quiere trabajar mas.La encuesta pregunta porque motivo trabaja menos de 40 horas*/
*Alternativa MGD 06/21/2014: aunque no se pregunta si quiere trabajr ams horas se puede inferir con la pregunta de que trabaja
*menor de 40 horas porque solo consiguio empleo parcial o falta de trabajo.
g subemp_ci=0
replace subemp_ci=1 if  emp_ci==1 & horaspri_ci<=30 & (r413== 3 | r413==2)

******************************
*	tiempoparc_ci
******************************
g tiempoparc_ci=(emp_ci==1 & r413==1 & horastot_ci<30)
replace tiempoparc_ci=. if emp_ci!=1
label var tiempoparc_ci "Trabajan menos de 30 hs semanales y no quieren trabajar mas"

******************************
*	categopri_ci
******************************
g categopri_ci=.
replace categopri_ci=1 if r418==1
replace categopri_ci=2 if r418==2 | r418==3 | r418==4 
replace categopri_ci=3 if r418==6 | r418==7 | r418==8 | r418==9 
replace categopri_ci=4 if r418==5
replace categopri_ci=0 if r418==10
replace categopri_ci=. if emp_ci!=1
label define categopri_ci 0"Otros" 1"Patron" 2"Cuenta propia" 3"Empleado" 4" Familiar no remunerado"
label value categopri_ci categopri_ci
label variable categopri_ci "Categoria ocupacional trabajo principal"
******************************
*	categosec_ci
******************************
* g categosec_ci=.
*NA

/*******************************
*	contrato_ci
******************************
/*g contrato_ci=(emp_ci==1 & r419==1)
replace contrato_ci=. if emp_ci!=1 |r419==.|r419==3
label var contrato_ci "Persona empleada que firmo contrato de trabajo"
entiendo que no podria generarse - ver survey
No incluye a todos los empleados*/

*r419==3 No sabe No responde.*/
/*gen contrato_ci=.
******************************
*	segsoc_ci
******************************
g segsoc_ci=(r422==1 | r422==2) if emp_ci==1*/
******************************
*	nempleos_ci
******************************
g nempleos_ci=.
replace nempleos_ci=1 if r432==2
replace nempleos_ci=2 if r432==1
replace nempleos_ci=. if emp_ci!=1

/******************************
*	firmapeq_ci
******************************
g firmapeq_ci=(r421<=5) if emp_c==1
label var firmapeq_ci "1=5 o menos trabajadores"*/
******************************
*	spublico_ci
******************************
g spublico_ci=(r420==2 & emp_ci==1)
replace spublico_ci=. if categopri_ci==1 | categopri_ci==2 | categopri_ci==4 | r418==9 | emp_ci!=1
label var spublico_ci "Trabaja en sector publico"


******************************************************************************
*		LABOR DEMAND 
******************************************************************************

******************************
*	ocupa_ci 
******************************
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


/*
1 Profesionales y técnicos.
2 Directores y funcionarios superiores. 
3 Personal administrativo y nivel intermedio. 
4 Comerciantes y vendedores. 
5 Trabajadores en servicios. 
6 Trabajadores agrícolas y afines. 
7 Obreros no agrícolas, conductores de maquinas y vehículos de transporte y similares. 
8 Fuerzas Armadas. 
9 Otras ocupaciones no clasificadas en las anteriores*/

******************************
*	rama_ci
******************************
g rama_ci=. 
replace rama_ci=1 if (r416>=100 & r416<=500)  & emp_ci==1 
replace rama_ci=2 if (r416>=1000 & r416<=1429)  & emp_ci==1 
replace rama_ci=3 if (r416>=1500 & r416<=3720)  & emp_ci==1 
replace rama_ci=4 if (r416>=4000 & r416<=4100)  & emp_ci==1 
replace rama_ci=5 if (r416>=4500 & r416<=4550)  & emp_ci==1 
replace rama_ci=6 if (r416>=5000 & r416<=5520)  & emp_ci==1 
replace rama_ci=7 if (r416>=6000 & r416<=6420)  & emp_ci==1 
replace rama_ci=8 if (r416>=6500 & r416<=7499)  & emp_ci==1 
replace rama_ci=9 if (r416>=7511 & r416<=9900)  & emp_ci==1 

/*
1 Agricultura, caza, silvicultura y pesca
2 Explotación de minas y canteras
3 Industria manufacturera
4 Electricidad, gas y agua.
5 Construcción.
6 Comercio al por mayor y menor, restaurantes, hoteles.
7 Transporte y almacenamiento.
8 Establecimientos financieros, seguros, bienes inmuebles.
9 Servicios sociales, comunales y personales.*/

******************************
*	ylmpri_ci 
******************************

*For dependent workers
g yprid=.
replace yprid=r424*30 if r423==1
replace yprid=r424*4.3 if r423==2
replace yprid=r424*2 if r423==3
replace yprid=r424 if r423==4 | r423==5

g hrsextrasd=r42501a*r42501b/12 
g vacacionesd=r42502a*r42502b/12 
g aguinaldod=r42503a*r42503b/12 
g bonificacionesd=r42504a*r42504b/12 
egen yprijbd=rsum(yprid hrsextrasd vacacionesd aguinaldod bonificacionesd), missing

*For independent workers
g ingrneto=r428-r429 
replace ingrneto=0 if ingrneto<0

g yprijbi=. 
replace yprijbi=ingrneto*30 if r427==1
replace yprijbi=ingrneto*4.3 if r427==2
replace yprijbi=ingrneto*2 if r427==3
replace yprijbi=ingrneto if r427==4 | r427==9
replace yprijbi=ingrneto/2 if r427==5
replace yprijbi=ingrneto/12 if r427==8

egen ylmpri_ci=rsum(yprijbi yprid), missing
codebook ylmpri_ci if emp_ci==0
label var ylmpri_ci "Ingreso laboral monetario actividad principal"

******************************
*	nrylmpri_ci 
******************************
g nrylmpri_ci=(ylmpri_ci==. & emp_ci==1)
replace nrylmpri_ci=. if emp_ci!=1
label var nrylmpri_ci "Identificador de No respuesta del ingreso de la actividad principal"

******************************
*	ylnmpri_ci 
******************************
g food1=r42505a*r42505b/12 if emp_ci==1
g ropa1=r42506a*r42506b/12 if emp_ci==1
g merca1=r42507a*r42507b/12 if emp_ci==1
g vivi1=r42508a*r42508b/12 if emp_ci==1
g trans1=r42509a*r42509b/12 if emp_ci==1
g segur1=r42510a*r42510b/12 if emp_ci==1
g otross1=r42512a*r42512b/12 if emp_ci==1 

egen ylnmpri_ci=rsum(food1 ropa1 merca1 vivi1 trans1 segur1 otross1), missing
replace ylnmpri_ci=. if emp_ci!=1

******************************
*	ylmsec_ci & ylmsec1_ci
******************************
g ylmsec_ci=r434 if emp_ci==1

g hrsextras=r43501a*r43501b/12 if emp_ci==1
g vacaciones=r43502a*r43502b/12 if emp_ci==1
g aguinaldo=r43503a*r43503b/12 if emp_ci==1
g bonificaciones=r43504a*r43504b/12 if emp_ci==1

egen ylmsec1_ci=rsum(ylmsec_ci hrsextras vacaciones aguinaldo bonificaciones), missing
replace ylmsec1_ci=. if emp_ci!=1 | r432!=1

******************************
*	ylnmsec_ci
******************************
g food2=r43505a*r43505b/12 if emp_ci==1
g ropa2=r43506a*r43506b/12 if emp_ci==1 
g merca2=r43507a*r43507b/12 if emp_ci==1 
g vivi2=r43508a*r43508b/12 if emp_ci==1 
g trans2=r43509a*r43509b/12 if emp_ci==1
g segur2=r43510a*r43510b/12 if emp_ci==1 
g otross2=r43512a*r43512b/12 if emp_ci==1

egen ylnmsec_ci=rsum(food2 ropa2 merca2 vivi2 trans2 segur2 otross2), missing
label var ylnmsec_ci "Ingreso laboral no monetario actividad secundaria"
replace ylnmsec_ci=. if emp_ci!=1 | r432!=1
******************************
*	ylm_ci 
******************************
egen ylm_ci=rsum(ylmpri_ci ylmsec_ci) if emp_ci==1, missing
label var ylm_ci "Ingreso laboral monetario total"
******************************
*	ylnm_ci
******************************
egen ylnm_ci=rsum(ylnmpri_ci ylnmsec_ci) if emp_ci==1, missing
label var ylnm_ci "Ingreso laboral no monetario total"
******************************
*	ynlm_ci
******************************
g remesas=r44401a*r44401b/12
g ayuda=r44402a*r44402b/12
g cuotalim=r44403a*r44403b/12
g alqui=r44404a*r44404b/12
g alqneg=r44405a*r44405b/12
g jubil=r44407a*r44407b/12
g deveh=r44408a*r44408b/12
g pension=r44409a*r44409b/12
g otros=r44410a*r44410b/12

g utilidades=r44501/12
g dividendos=r44502/12
g intereses=r44503/12
g herencias=r44504/12
g indemnizacion=r44505/12
g ayudagob=r44506/12
g acteventual=r44507/12
g arrendamiento=r44508/12
g remesaevent1=r44509/12
g remesaevent2=r44510/12
g otrosy=r44512/12

egen ynlm_ci=rsum(remesas ayuda cuotalim alqui alqneg jubil deveh otros utilidades dividendos intereses herencias indemnizacion ayudagob acteventual arrendamiento remesaevent1 remesaevent2 otrosy), missing
label var ynlm_ci "Ingreso no laboral monetario" 

gen ynlnm_ci=.
gen ynlnm_ch=.

******************************
*	nrylmpri_ch 
******************************
by idh_ch, sort: egen nrylmpri_ch=sum(nrylmpri_ci) if miembros_ci==1
label var nrylmpri_ch "Identificador de los hogares en donde alguno de los miembros NS/NR el ingreso de la actividad principal"
replace nrylmpri_ch=1 if nrylmpri_ch>0 & nrylmpri_ch<.
******************************
*	ylm_ch 
******************************
by idh_ch, sort: egen ylm_ch=sum(ylm_ci) if miembros_ci==1
label var ylm_ch "Ingreso laboral monetario del hogar-ignora no respuesta"

******************************
*	ylmnr_ch 
******************************
by idh_ch, sort: egen ylmnr_ch=sum(ylm_ci) if miembros_ci==1
label var ylmnr_ch "Ingreso laboral monetario del hogar - considera la no respuesta"
replace ylmnr_ch=. if nrylmpri_ch==1
******************************
*	ylnm_ch 
******************************
by idh_ch, sort: egen ylnm_ch=sum(ylnm_ci) if miembros_ci==1
label var ylnm_ch "Ingreso laboral no monetario del hogar" 
******************************
*	ynlm_ch 
******************************
by idh_ch, sort: egen ynlm_ch=sum(ynlm_ci) if miembros_ci==1
label var ynlm_ch "Ingreso no laboral monetario del Hogar"
******************************
*	ynlnm_ch 
******************************
/*g ynlnm_ch=.
label var ynlnm_ch "Ingreso no laboral no monetario del Hogar"*/
******************************
*	ylmhopri_ci 
******************************
g ylmhopri_ci=ylmpri_ci/(horaspri_ci*4.3) 
label var ylmhopri_ci "Salario monetario de la actividad principal"
******************************
*	ylmho_ci 
******************************
g ylmho_ci=ylm_ci/(horastot_ci*4.3) 
label var ylmho_ci "Salario monetario de todas las actividades"
******************************
*	autocons_ci 
******************************
g autocons_ci=r431
label var autocons_ci "Autoconsumo reportado por el individuo"
******************************
*	autocons_ch 
******************************
by idh_ch, sort: egen autocons_ch=sum(autocons_ci) if miembros_ci==1
label var autocons_ch "Autoconsumo del Hogar"
******************************
*	rentaimp_ch 
******************************

******************************
*	remesas_ci & remesas_ch
******************************
*Hay problemas con los ceros y los missing en la variable remesaevent1
*egen remesas_ci=rsum(remesas remesaevent1)


gen temp=remesaevent1
recode temp (0=.)
egen remesas_ci=rsum(remesas temp), missing
replace remesas_ci=. if remesas==. & temp==.
label var remesas_ci "Remesas reportadas por el individuo"
drop temp

by idh_ch, sort: gen remesas_ch=sum(remesas_ci) if miembros_ci==1
label var remesas_ch "Remesas del hogar"

******************************
*	durades_ci
******************************
g durades_ci=r409/4.3 if /*desemp_ci==1 &*/ r409>0  

******************************
*	antiguedad_ci
******************************
g antiguedad_ci=.
*NA


			****************************
			***VARIABLES DE EDUCACION***
			****************************

*************
***aedu_ci***
*************

* MGD 12/9/2016: si está disponible aproba1
g aedu_ci = aproba1
label var aedu_ci "Anios de educacion aprobados" 

**************
***eduno_ci***
**************
gen eduno_ci = (aedu_ci == 0)
replace eduno_ci = . if aedu_ci == .
label var eduno_ci "Sin educacion"

**************
***edupi_ci***
**************
gen edupi_ci = (aedu_ci >= 1 & aedu_ci < 6)
replace edupi_ci = . if aedu_ci == . 
label var edupi_ci "Primaria incompleta"

**************
***edupc_ci***
**************
gen edupc_ci = (aedu_ci == 6)
replace edupc_ci = . if aedu_ci == .
label var edupc_ci "Primaria completa"

**************
***edusi_ci***
**************
gen edusi_ci = (aedu_ci > 6 & aedu_ci <= 10) 
replace edusi_ci = . if aedu_ci == .  
label var edusi_ci "Secundaria incompleta"

**************
***edusc_ci***
**************
gen edusc_ci = (aedu_ci == 11)
replace edusc_ci = . if aedu_ci == .
label var edusc_ci "Secundaria Completa"

**************
***eduui_ci***
**************
gen eduui_ci = (aedu_ci == 12 | aedu_ci >= 12 & r219 == 2) // 12 anios de estudio o mas y título de bachiller
replace eduui_ci = 1 if aedu_ci >= 13 & inlist(r219, 1, .) // mas de 12 anios de estudio, perdido.
replace eduui_ci = . if aedu_ci == .
label var eduui_ci "Universitaria o Terciaria Incompleta"

**************
***eduuc_ci***
**************
gen eduuc_ci=(aedu_ci > 12 & (r219 >= 3 & r219 <= 11)) // mas de 12 anios de estudio pero con titulo terciario; incluye profesorado
replace eduuc_ci=. if aedu_ci==.
label var eduuc_ci "Universitaria o Terciaria Completa"

***************
***edus1i_ci***
***************
gen  edus1i_ci = (aedu_ci > 6 & aedu_ci < 9)
replace edus1i_ci = . if aedu_ci == .
label var edus1i_ci "1er ciclo de la secundaria incompleto"

***************
***edus1c_ci***
***************
gen byte edus1c_ci = (aedu_ci == 9)
replace edus1c_ci = . if aedu_ci == .
label var edus1c_ci "1er ciclo de la secundaria completo"

***************
***edus2i_ci***
***************
gen edus2i_ci=(aedu_ci == 10)
replace edus2i_ci = . if aedu_ci==.
label var edus2i_ci "2do ciclo de Educacion Secundaria Incompleto"

***************
***edus2c_ci***
***************
gen edus2c_ci = (aedu_ci == 11)
replace edus2c_ci = . if aedu_ci == .
label var edus2c_ci "2do ciclo de Educacion Secundaria Completo"

***************
***edupre_ci***
***************
gen edupre_ci = (r209 == 1)
label var edupre_ci "Educacion preescolar"

****************
***asispre_ci***
****************
*Agregada por Iván Bornacelly - 01/23/2017
g asispre_ci = (r203 == 1 & r204 == 1) // no consideramos menores de 3 años (r201a)
la var asispre_ci "Asiste a educacion prescolar"
	
	
**************
***eduac_ci***
**************
gen eduac_ci = .
replace eduac_ci = 1 if r217a == 4
replace eduac_ci = 0 if r217a == 5
label variable eduac_ci "Superior universitario vs superior no universitario"

***************
***asiste_ci***
***************
gen asiste_ci = (r203 == 1)
replace asiste_ci = . if r203 == .
label variable asiste_ci "Asiste actualmente a la escuela"

*****************
***pqnoasis_ci***
*****************
gen pqnoasis_ci= r220 
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
gen repiteult_ci= r207a == 1
label var repiteult "Ha repetido el último grado"

***************
***edupub_ci***
***************
gen edupub_ci=.
replace edupub_ci=1 if r210a==1 
replace edupub_ci=0 if r210a==2 | r210a==3
label var edupub_ci "Personas que asisten a centros de ensenanza publicos"


******************************************************************************
*	INFRAESTRUCTURE VARIABLES 
******************************************************************************

****************
***aguared_ch***
****************
generate aguared_ch =.
replace aguared_ch = 1 if r312==1 | r312==2
replace aguared_ch = 0 if r312> 2
la var aguared_ch "Acceso a fuente de agua por red"
	
*****************
*aguafconsumo_ch*
*****************
gen aguafconsumo_ch = 0
replace aguafconsumo_ch= 3 if r315 ==3  

*****************
*aguafuente_ch*
*****************

gen aguafuente_ch = 1 if r312==1 |  r312==2
replace aguafuente_ch = 2 if r313==2
replace aguafuente_ch= 4 if (r313==5 | (r313>5 & r313<5.2))
replace aguafuente_ch = 5 if r313==10
replace aguafuente_ch= 6 if r313==3
replace aguafuente_ch= 7 if r313==8 | r313==1 | r313==11
replace aguafuente_ch = 8 if r313==7 | r313 ==9
replace aguafuente_ch= 9 if  r313==6 |(r313>6 & r313 <6.2)
replace aguafuente_ch= 10 if r313==13 | r313==12 |r313==4 | (r313>4 & r313<4.2)

*************
*aguadist_ch*
*************
gen aguadist_ch=0
replace aguadist_ch= 1 if  (r312==1 | r313==1 | r313==3 | r313==4| r313==5 | r313==6)
replace aguadist_ch= 2 if  r312==2
replace aguadist_ch=3 if r313==2 |  (r313>4.0 &r313<4.2)| (r313>5.0 &r313<5.2)| (r313>6.0 &r313<6.2)| r313==12 | r313==13


**************
*aguadisp1_ch*
**************
gen aguadisp1_ch =9



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
gen aguamide_ch = .


*****************
*bano_ch         *  Altered
*****************

gen bano_ch=.
replace bano_ch=1 if (r319==1 | r319==3)
replace bano_ch=2 if (r319==2 | r319==4)
replace bano_ch=3 if (r319==7| r319 ==8)
replace bano_ch=4 if (r318 !=5 & (r324a == 3 |r324a == 4))
replace bano_ch=6 if (r319==5 | r319==6)
replace bano_ch=0 if r318==4 |r318==5

***************
***banoex_ch***
***************
generate banoex_ch=1 if r321==2
replace banoex_ch=0 if r321!=2
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
replace sinbano_ch = 0 if r318!=5
replace sinbano_ch = 2 if r318==5 & (r324a==3|r324a==4) 

*label var sinbano_ch "= 0 si tiene baño en la vivienda o dentro del terreno"

*************
*aguatrat_ch*
*************
gen aguatrat_ch=0
replace aguatrat_ch=1 if r315 ==1
*label var aguatrat_ch "= 9 la encuesta no pregunta de si se trata el agua antes de consumirla"


******************************
*	luz_ch
******************************
g luz_ch=(r311==1 | r311==2)
******************************
*	luzmide_ch
******************************
g luzmide_ch=.
*NA
******************************
*	combust_ch
******************************
g combust_ch=(r326==1|r326==3)


******************************
*	des1_ch
******************************
g des1_ch=.
replace des1_ch=0 if bano_ch==0
replace des1_ch=1 if bano_ch==1 & (r325a==1 | r325a==3) 
replace des1_ch=2 if bano_ch==1 &  r325a==2
replace des1_ch=3 if bano_ch==1 & (r325a==4 |r325a==5)
******************************
*	des2_ch
******************************
g des2_ch=.
replace des2_ch=0 if bano_ch==0
replace des2_ch=1 if (des1_ch==1 | des1_ch==2)
replace des2_ch=2 if des1_ch==3 | (bano_ch==1 &  r325a==6)
******************************
*	piso_ch
******************************
g piso_ch=.
replace piso_ch=0 if r304==5
replace piso_ch=1 if r304>=1 & r304<=4
replace piso_ch=2 if r304==6
******************************
*	pared_ch
******************************
g pared_ch=.
replace pared_ch=0 if r303==2 | r303==3 | r303==5| r303==6 | r303==7 |r303==8
replace pared_ch=1 if r303==1 | r303==4 
replace pared_ch=2 if r303==9
******************************
*	techo_ch
******************************
g techo_ch=.
replace techo_ch=0 if r302==5 | r302==6 |r302==7
replace techo_ch=1 if r302>=1 & r302<=4
replace techo_ch=2 if r302==8
******************************
*	resid_ch
******************************
g resid_ch=.
replace resid_ch=0 if r329==1 | r329==2
replace resid_ch=1 if r329==4 | r329==5
replace resid_ch=2 if r329==6
replace resid_ch=3 if r329==3 | r329==7

******************************
*	dorm_ch
******************************
g dorm_ch=r306
******************************
*	cuartos_ch
******************************
/*g cuartos_ch=r305

Estrictamente no se podria generar porque cuartos_ch es "Cantidad de habitaciones en el hogar" (no necesariamente para dormir). 
La pregunta r305 no incluye cocina, bano o garage*/


* MGR Jul, 2015: genero variable con r305 ya que toda la serie se genera de esta manera con excepción de 2006-2009
gen cuartos_ch=r305

******************************
*	cocina_ch
******************************
*g cocina_ch=
*NA
******************************
*	telef_ch
******************************
g telef_ch=(r3281a==1)
******************************
*	refrig_ch
******************************
gen refrig_ch=(r33005a==1)
******************************
*	freez_ch
******************************
g freez_ch=.
*NA
******************************
*	auto_ch
******************************
g auto_ch=(r33012a==1)
******************************
*	compu_ch
******************************
g compu_ch=(r33009a==1)
******************************
*	internet_ch 
******************************
g internet_ch=(r3283a==1)

notes: la variable internet_ch en realidad indica Internet y/o correo electrónico
******************************
*	cel_ch
******************************
gen cel_ch=(r3282a==1)
******************************
*	vivi1_ch
******************************
gen vivi1_ch=.
replace vivi1_ch=1 if r301==1
replace vivi1_ch=2 if r301==2  
replace vivi1_ch=3 if r301>=3 
******************************
*	vivi2_ch
******************************
gen vivi2_ch=(r301>=1 & r301<=2)
******************************
*	viviprop_ch
******************************
gen viviprop_ch=.
replace viviprop_ch=0 if r308a==1
replace viviprop_ch=1 if r308a==3 & r308a<=5
replace viviprop_ch=2 if r308a==2
replace viviprop_ch=3 if r308a>=6 & r308a<=9
label var viviprop_ch "Propiedad de la vivienda" 
******************************
*	vivitit_ch
******************************
g vivitit_ch=.
*NA
******************************
*	vivialq_ch
******************************
g vivialq_ch=r308c if r308a==1
label var vivialq_ch "Alquiler mensual"

notes: vivialq_ch, pago en dolares.

******************************
*	vivialqimp_ch
******************************
*g vivialqimp_ch=.


****************
*afiliado_ci****
****************
gen afiliado_ci=.
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
gen instcot_ci=.
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
gen pensionsub_ci=.
label var pensionsub_ci "1=recibe pension subsidiada / no contributiva"

*****************
**  ypensub_ci  *
*****************
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
replace lpe_ci= 158.23	if mes_c==1 & zona_c==1
replace lpe_ci= 160.05	if mes_c==2 & zona_c==1
replace lpe_ci= 160.18	if mes_c==3 & zona_c==1
replace lpe_ci= 161.21	if mes_c==4 & zona_c==1
replace lpe_ci= 167.01	if mes_c==5 & zona_c==1
replace lpe_ci= 173.28	if mes_c==6 & zona_c==1
replace lpe_ci= 180.21	if mes_c==7 & zona_c==1
replace lpe_ci= 180.21	if mes_c==8 & zona_c==1
replace lpe_ci= 178.11	if mes_c==9 & zona_c==1
replace lpe_ci= 177.60	if mes_c==10 & zona_c==1
replace lpe_ci= 178.77	if mes_c==11 & zona_c==1
replace lpe_ci= 179.42  if mes_c==12 & zona_c==1

*zona rural
replace lpe_ci=120.55	if mes_c==1 & zona_c==0
replace lpe_ci=120.86	if mes_c==2 & zona_c==0
replace lpe_ci=122.23	if mes_c==3 & zona_c==0
replace lpe_ci=123.57	if mes_c==4 & zona_c==0
replace lpe_ci=126.14	if mes_c==5 & zona_c==0
replace lpe_ci=130.18	if mes_c==6 & zona_c==0
replace lpe_ci=135.02	if mes_c==7 & zona_c==0
replace lpe_ci=134.57	if mes_c==8 & zona_c==0
replace lpe_ci=132.45	if mes_c==9 & zona_c==0
replace lpe_ci=130.94	if mes_c==10 & zona_c==0
replace lpe_ci=129.29	if mes_c==11 & zona_c==0
replace lpe_ci=128.67   if mes_c==12 & zona_c==0
label var lpe_ci "Linea de indigencia oficial del pais"


/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/

*************
**salmm_ci***
*************
*gen salmm_ci= 159.40 /*fuente: ILO*/

*2015 MGD: salarios segun actividad
g salmm_ci=.
replace salmm_ci=91 if rama_ci==1
replace salmm_ci=171 if rama_ci==3
replace salmm_ci=184 if rama_ci==6
replace salmm_ci=149 if salmm_ci==.
label var salmm_ci "Salario minimo legal"


*************
***tecnica_ci**
*************
gen tecnica_ci=(r204==5 | nivaprob==5)
label var tecnica_ci "=1 formacion terciaria tecnica"

*****************
**categoinac_ci**
*****************	
gen categoinac_ci=.	
replace categoinac_ci=1 if r407==13
replace categoinac_ci=2 if r407==8 | r407==15
replace categoinac_ci=3 if r407==12
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
gen ylmotros_ci=.
gen ylnmotros_ci=.
gen rentaimp_ch=.
gen categosec_ci=.
gen aguadist_ch=.
*gen cuartos_ch=.
gen cocina_ch=.
gen vivialqimp_ch=.

******************************
*** VARIABLES DE MIGRACION ***
******************************

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

	**********************
	*** migrantiguo5_ci **
	**********************
	gen migrantiguo5_ci=.
	label var migrantiguo5_ci "=1 si es migrante antiguo (5 anos o mas)"
		
	**********************
	*** miglac_ci ***
	**********************
	gen miglac_ci=.
	label var miglac_ci "=1 si es migrante proveniente de un pais LAC"
	

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
edus1c_ci edus2i_ci edus2c_ci edupre_ci eduac_ci asiste_ci pqnoasis_ci pqnoasis1_ci	repite_ci repiteult_ci edupub_ci ///
aguared_ch aguafconsumo_ch aguafuente_ch aguadist_ch aguadisp1_ch aguadisp2_ch aguamala_ch aguamejorada_ch aguamide_ch bano_ch banoex_ch banomejorado_ch sinbano_ch aguatrat_ch luz_ch luzmide_ch combust_ch des1_ch des2_ch piso_ch  ///
pared_ch techo_ch resid_ch dorm_ch cuartos_ch cocina_ch telef_ch refrig_ch freez_ch auto_ch compu_ch internet_ch cel_ch ///
vivi1_ch vivi2_ch viviprop_ch vivitit_ch vivialq_ch	vivialqimp_ch , first




compress


saveold "`base_out'", replace


log close




















