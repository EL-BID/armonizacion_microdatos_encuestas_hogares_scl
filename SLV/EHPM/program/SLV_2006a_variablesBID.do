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
local ANO "2006"
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
Autores: 2008 -do file prepared by Melisa Morales for Suzanne Duryea 
Melisa Morales sugiere chequearlo.
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
gen factor_ch=fac01 
label var factor_ch "Household Expansion Factor"
******************************
*	idh_ch
******************************
sort lote tipo folio viv 
egen idh_ch=group(lote tipo folio viv)
label var idh_ch "ID del hogar"

******************************
*	idp_ci
******************************
gen idp_ci=numorden
******************************
*	zona_c
******************************
gen byte zona_c=(area==1) 
label var zona_c "Area of the country"
label define zona_c 1 "Urbana" 0 "Rural"
label value zona_c zona_c
******************************
*	pais_c
******************************
gen str3 pais_c="SLV"
label var pais_c "El Salvador"
******************************
*	anio_c
******************************
gen anio_c=2006
label var anio_c "Year of the survey"
******************************
*	mes_c
******************************
gen mes_c= r016m
label variable mes_c "Month of the survey"
label define mes_c 1 "Jan" 2 "Feb" 3 "Mar" 4 "Apr" 5 "May" 6 "Jun" 7 "Jul", add
label define mes_c 8 "Aug" 9 "Sep" 10 "Oct" 11 "Nov" 12 "Dec", add
label value mes_c mes_c
******************************
*	relacion_ci
******************************
gen relacion_ci=.
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
gen factor_ci=fac01
label var factor_ci "Individual Expansion Factor"
******************************
*	sexo_ci
******************************
gen sexo_ci=r104
label define sexo_ci 1 "Man" 2 "Woman"
label value sexo_ci sexo_ci
******************************
*	edad_ci
******************************
gen edad_ci= r106
******************************
*	civil_ci
******************************
gen civil_ci=.
replace civil_ci=1 if r108==6
replace civil_ci=2 if r108==1 | r108==2
replace civil_ci=3 if r108==4 | r108==5
replace civil_ci=4 if r108==3
label var civil_ci "Estado civil"
label define civil_ci 1 "Soltero" 2 "Union formal o informal" 3 "Divorciado o separado" 4 "Viudo"
label value civil_ci civil_ci
******************************
*	jefe_ci
******************************
gen jefe_ci=(relacion_ci==1)
label var jefe_ci "Jefe de hogar"
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
gen byte clasehog_ch=.
replace clasehog_ch=1 if nhijos_ch==0 & nconyuges_ch==0 & notropari_ch==0 & notronopari_ch==0
replace clasehog_ch=2 if (nhijos_ch>0| nconyuges_ch>0) & (notropari_ch==0 & notronopari_ch==0)
replace clasehog_ch=3 if notropari_ch>0 & notronopari_ch==0
replace clasehog_ch=4 if ((nconyuges_ch>0 | nhijos_ch>0 | notropari_ch>0) & (notronopari_ch>0))
replace clasehog_ch=5 if nhijos_ch==0 & nconyuges_ch==0 & notropari_ch==0 & notronopari_ch>0
label var clasehog_ch "Tipo de hogar"
label define clasehog_ch 1 "Unipersonal" 2 "Nuclear" 3 "Ampliado" 
label define clasehog_ch 4 "Compuesto" 5 "Corresidente", add
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

by idh_ch, sort: egen nmayor65_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad_ci>=65)
label var nmayor65_ch "Numero de familiares mayores a 65 anios"

by idh_ch, sort: egen nmenor6_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad_ci<6)
label var nmenor6_ch "Numero de familiares menores a 6 anios"

by idh_ch, sort: egen nmenor1_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad_ci<1)
label var nmenor1_ch "Numero de familiares menores a 1 anio"

******************************
*	miembros_ci
******************************
*2014, 01 modificado segun documento metodologico by MLO
g miembros_ci=(relacion_ci<5)
*g miembros_ci=(relacion_ci<6)
label var miembros_ci "Miembro del hogar"

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
gen emp_ci=(r403==1 | (r403==2 & r404==1))
replace emp_ci=. if r403==. 
label var emp_ci "Empleado"
label define emp_ci 1"Si" 0 "No"
label value emp_ci emp_ci
******************************
*	desemp1_ci	& desemp2_ci & desemp3_ci 
******************************
gen desemp1_ci=(emp_ci==0 &  r406==1)
replace desemp1_ci=. if r403==. 
label var desemp1_ci "Personas sin trabajo que buscaron en el periodo de referencia"
 
gen desemp2_ci=(desemp1_ci==1 | (emp_ci==0 & r406==2 & r407==18))
replace desemp2_ci=. if r403==.
label var desemp2_ci "des1 + no trabajaron ni buscaron en la ult semana pero esperan respuesta de solicit"
 
gen desemp3_ci=.
*NA
label var desemp3_ci "des2 + no tienen trabajo pero buscaron antes de la semana pasada"

*A los desocupados no les preguntan si buscaron trabajo antes de la semana de referencia
******************************
*	pea1_ci, pea2_ci, pea3_ci
******************************
gen pea1_ci=(emp_ci==1 | desemp1_ci==1)
gen pea2_ci=(emp_ci==1 | desemp2_ci==1)
gen pea3_ci=.
*NA*/
******************************
*	desalent_ci
******************************
gen desalent_ci=(emp_ci==0 &  r406==2 & r407==3)
replace desalent_ci=. if emp_ci!=0
label var desalent_ci "Personas que creen que por alguna razon no conseguiran trabajo"


******************************
*	horaspri_ci & horastot_ci
******************************
egen horaspri_ci= rsum(r411a r411d) if emp_ci==1
label var horaspri_ci "Hs totales (semanales) trabajadas en act. principal"

egen horastot_ci=rsum(horaspri_ci r432) if emp_ci==1
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
gen tiempoparc_ci=(emp_ci==1 & r413==1 & horastot_ci<30)
replace tiempoparc_ci=. if emp_ci!=1
label var tiempoparc_ci "Trabajan menos de 30 hs semanales y no quieren trabajar mas"
******************************
*	categopri_ci
******************************
gen categopri_ci=.
replace categopri_ci=1 if r417==1
replace categopri_ci=2 if r417>=2 & r417<=4 
replace categopri_ci=3 if r417>=6 & r417<=9
replace categopri_ci=4 if r417==5
replace categopri_ci=0 if r417==10
replace categopri_ci=. if emp_ci!=1
label var categopri_ci "Categoria ocupacional trabajo principal"
label define categopri_ci 0"Otros" 1"Patron" 2"Cuenta propia" 3"Empleado" 4"Familiar no remunerado"
label value categopri_ci categopri_ci
*puse 'cooperativista' en cuentapropista y 'servicio domestico' en empleado 
******************************
*	categosec_ci
******************************
/*gen categosec_ci=. 
replace categosec_ci=1 if r439==1
replace categosec_ci=2 if r439>=2  & r439<=4 
replace categosec_ci=3 if r439>=6 & r439<=10
replace categosec_ci=4 if r439==5
label var categosec_ci "Categoria ocupacional trabajo secundario"
label define categosec_ci 1"Patron" 2"Cuenta propia" 3"Empleado" 4"Familiar no remunerado"
label value categosec_ci categosec_ci*/
/*
******************************
*	contrato_ci
******************************
/*gen contrato_ci=(emp_ci==1 & r418==1)
replace contrato_ci=. if emp_ci!=1 |r418==.|r418==3
label var contrato_ci "Persona empleada que firmo contrato de trabajo"

entiendo que no podria generarse - ver survey*/

*Incluye unicamente a asalariados permanente y temporal, aprendices y servicio domestico (los de preg r417) 
gen contrato_ci=.
******************************
*	segsoc_ci
******************************
gen segsoc_ci=(r421==1 | r421==2) if emp_ci==1*/
******************************
*	nempleos_ci
******************************
gen nempleos_ci=. 
replace nempleos_ci=1 if r431==2
replace nempleos_ci=2 if r431==1
replace nempleos_ci=. if emp_ci!=1
/*
******************************
*	firmapeq_ci
******************************
gen firmapeq_ci=(r420<=5)
replace firmapeq_ci=. if emp_ci!=1
label var firmapeq_ci "1=5 o menos trabajadores"*/
******************************
*	spublico_ci
******************************
gen spublico_ci=(r419==2 & emp_ci==1)
replace spublico_ci=. if categopri_ci==1 | categopri_ci==2 | categopri_ci==4 | r417==9 | emp_ci!=1
label var spublico_ci "Trabaja en sector publico"



******************************************************************************
*		LABOR DEMAND
******************************************************************************

******************************
*	ocupa_ci
******************************
gen ocupa_ci=. 
replace ocupa_ci=1 if (r414>=2111 & r414<=3699) 
replace ocupa_ci=2 if (r414>=1110 & r414<=1319) 
replace ocupa_ci=3 if (r414>=4110 & r414<=4223)
replace ocupa_ci=4 if (r414>=9110 & r414<=9113) | (r414>=5210 & r414<=5230) 
replace ocupa_ci=5 if (r414>=5111 & r414<=5169) | (r414>=9120 & r414<=9162) 
replace ocupa_ci=6 if (r414>=6110 & r414<=6210) | (r414>=9210 & r414<=9220)
replace ocupa_ci=7 if (r414>=7111 & r414<=8340) | (r414>=9311 & r414<=9333) 
replace ocupa_ci=8 if r414==110  |  r414==111
replace ocupa_ci=. if emp_ci!=1 

******************************
*	rama_ci
******************************
gen rama_ci=. 
replace rama_ci=1 if (r416>=100 & r416<=500) & emp_ci==1 
replace rama_ci=2 if (r416>=1000 & r416<=1429) & emp_ci==1 
replace rama_ci=3 if (r416>=1500 & r416<=3720) & emp_ci==1 
replace rama_ci=4 if (r416>=4000 & r416<=4100) & emp_ci==1 
replace rama_ci=5 if (r416>=4500 & r416<=4550) & emp_ci==1 
replace rama_ci=6 if (r416>=5000 & r416<=5520) & emp_ci==1 
replace rama_ci=7 if (r416>=6000 & r416<=6420) & emp_ci==1 
replace rama_ci=8 if (r416>=6500 & r416<=7499) & emp_ci==1 
replace rama_ci=9 if (r416>=7500 & r416<=9503) & emp_ci==1 


/*a qué se |
  dedica la |
    empresa |      Freq.     Percent        Cum.
------------+-----------------------------------
        111 |      4,117       15.95       15.95
        112 |         56        0.22       16.17
        113 |        521        2.02       18.19
        121 |        398        1.54       19.73
        122 |         71        0.28       20.01
        130 |        196        0.76       20.76
        140 |         22        0.09       20.85
        200 |          7        0.03       20.88
        500 |        324        1.26       22.13
       1010 |          2        0.01       22.14
       1110 |          1        0.00       22.14
       1410 |          7        0.03       22.17
       1422 |          8        0.03       22.20
       1429 |          2        0.01       22.21
       1511 |         31        0.12       22.33

hasta 9500

SURVEY ARM, Rama laboral actividad principal
1 Agricultura, caza, silvicultura y pesca
2 Explotación de minas y canteras
3 Industrias manufactureras
4 Electricidad, gas y agua.
5 Construcción.
6 Comercio al por mayor y menor, restaurantes, hoteles.
7 Transporte y almacenamiento.
8 Establecimientos financieros, seguros, bienes inmuebles.
9 Servicios sociales, comunales y personales*/

******************************
*	ylmpri_ci & ylmpri1_ci
******************************

*For dependent workers

gen yprid=0 if emp_ci==1
replace yprid=r423*30 if r422==1
replace yprid=r423*4.3 if r422==2
replace yprid=r423*2 if r422==3
replace yprid=r423 if r422==4 | r422==5
replace yprid=. if r422==.
replace yprid=999999 if r423==999999.99

gen hrsextrasd=r42401a*r42401b/12 if emp_ci==1
replace hrsextrasd=999999 if r42401a==999999.99 

gen vacacionesd=r42402a*r42402b/12 if emp_ci==1
replace vacacionesd=999999 if r42402a==999999.99
 
gen aguinaldod=r42403a*r42403b/12 if emp_ci==1
replace aguinaldod=999999 if r42403a==999999.99 

gen bonificacionesd=r42404a*r42404b/12 if emp_ci==1
replace bonificacionesd=999999 if r42404a==999999.99 

egen yprijbd=rsum(yprid hrsextrasd vacacionesd aguinaldod bonificacionesd), missing
replace yprijbd=999999 if yprid==999999 | hrsextrasd==999999 | vacacionesd==999999 | aguinaldod==999999 | bonificacionesd==999999 
replace yprijbd=yprid if yprid>0 & yprid~=999999 & (hrsextrasd==999999 | vacacionesd==999999 | aguinaldod==999999 | bonificacionesd==999999)
replace yprijbd=. if emp_ci!=1

*For independent workers

gen ingrneto=r427-r428 if emp_ci==1
replace ingrneto=0 if ingrneto<0

gen yprijbi=0 if emp_ci==1
replace yprijbi=ingrneto*30 if r426==1
replace yprijbi=ingrneto*4.3 if r426==2
replace yprijbi=ingrneto*2 if r426==3
replace yprijbi=ingrneto if r426==4 | r426==9
replace yprijbi=ingrneto/2 if r426==5
replace yprijbi=ingrneto/3 if r426==6
replace yprijbi=ingrneto/6 if r426==7
replace yprijbi=ingrneto/12 if r426==8
replace yprijbi=999999 if ingrneto==999999 
replace yprijbi=. if categopri_ci>2 | r426==. 

egen ylmpri_ci=rsum(yprijbi yprid), missing
label var ylmpri_ci "Ingreso laboral monetario actividad principal"
replace ylmpri_ci=. if yprijbi==999999 | yprid==999999
replace ylmpri_ci=. if emp!=1

egen ylmpri1_ci=rsum(yprijbi yprijbd), missing
replace ylmpri1_ci=. if yprijbi==999999 | yprijbd==999999
replace ylmpri1_ci=. if emp!=1
******************************
*	nrylmpri_ci & nrylmpri1_ci
******************************
gen nrylmpri_ci=(ylmpri_ci==. & emp_ci==1)
replace nrylmpri_ci=. if emp_ci!=1
label var nrylmpri_ci "Identificador de No respuesta del ingreso de la actividad principal"
gen nrylmpri1_ci=(ylmpri1_ci==. & emp_ci==1)
replace nrylmpri1_ci=. if emp_ci!=1
******************************
*	ylnmpri_ci & ylnmpri1_ci
******************************
gen food1=r42405a*r42405b/12 if emp_ci==1
replace food1=. if r42405a==999999.99 

gen ropa1=r42406a*r42406b/12 if emp_ci==1
replace ropa1=. if r42406a==999999.99 

gen merca1=r42407a*r42407b/12 if emp_ci==1
replace merca1=. if r42407a==999999.99 

gen vivi1=r42408a*r42408b/12 if emp_ci==1
replace vivi1=. if r42408a==999999.99 

gen trans1=r42409a*r42409b/12 if emp_ci==1
replace trans1=. if r42409a==999999.99 

gen segur1=r42410a*r42410b/12 if emp_ci==1
replace segur1=. if r42410a==999999.99 

gen otross1=r42411a*r42411b/12 if emp_ci==1 
replace otross1=. if r42411a==999999.99 

egen ylnmpri_ci=rsum(food1 ropa1 merca1 vivi1 trans1 segur1 otross1), missing
replace ylnmpri_ci=. if emp_ci!=1

egen ylnmpri1_ci=rsum(ylnmpri_ci r430), missing
replace ylnmpri1_ci=ylnmpri_ci if r430==999999.99
replace ylnmpri1_ci=. if emp_ci!=1
******************************
*	ylmsec_ci
******************************
gen ylmsec_ci=r433 if emp_ci==1
replace ylmsec_ci=. if r433==999999.99

gen hrsextras=r43401a*r43401b/12 if emp_ci==1

gen vacaciones=r43402a*r43402b/12 if emp_ci==1
replace vacaciones=. if r43402a==999999.99 

gen aguinaldo=r43403a*r43403b/12 if emp_ci==1
replace aguinaldo=. if r43403a==999999.99 

gen bonificaciones=r43404a*r43404b/12 if emp_ci==1

egen ylmsec1_ci=rsum(ylmsec_ci hrsextras vacaciones aguinaldo bonificaciones), missing
replace ylmsec1_ci=. if emp_ci!=1 | r431!=1

******************************
*	ylnmsec_ci
******************************
gen food2=r43405a*r43405b/12 if emp_ci==1
gen ropa2=r43406a*r43406b/12 if emp_ci==1 
gen merca2=r43407a*r43407b/12 if emp_ci==1 
gen vivi2=r43408a*r43408b/12 if emp_ci==1 
gen trans2=r43409a*r43409b/12 if emp_ci==1
gen segur2=r43410a*r43410b/12 if emp_ci==1 
gen otross2=r43411a*r43411b/12 if emp_ci==1

egen ylnmsec_ci=rsum(food2 ropa2 merca2 vivi2 trans2 segur2 otross2), missing
label var ylnmsec_ci "Ingreso laboral no monetario actividad secundaria"
replace ylnmsec_ci=. if emp_ci!=1 | r431!=1

******************************
*	ylm_ci & ylm1_ci
******************************
egen ylm_ci=rsum(ylmpri_ci ylmsec_ci) if emp_ci==1, missing
label var ylm_ci "Ingreso laboral monetario total"

egen ylm1_ci=rsum(ylmpri1_ci ylmsec1_ci) if emp_ci==1, missing

******************************
*	ylnm_ci & ylnm1_ci
******************************
egen ylnm_ci=rsum(ylnmpri_ci ylnmsec_ci) if emp_ci==1, missing
label var ylnm_ci "Ingreso laboral no monetario total"

egen ylnm1_ci=rsum(ylnmpri1_ci ylnmsec_ci) if emp_ci==1, missing

******************************
*	ynlm_ci
******************************
gen remesas=r44301a*r44301b/12
replace remesas=. if r44301a==999999.99 

gen ayuda=r44302a*r44302b/12
replace ayuda=. if r44302a==999999.99 

gen cuotalim=r44303a*r44303b/12
replace cuotalim=. if r44303a==999999.99

gen alqui=r44304a*r44304b/12
replace alqui=. if r44304a==999999.99 

gen alqneg=r44305a*r44305b/12
replace alqneg=. if r44305a==999999.99 

gen jubil=r44307a*r44307b/12
replace jubil=. if r44307a==999999.99 

gen deveh=r44308a*r44308b/12
replace deveh=. if r44308a==999999.99 

gen otros=r44309a*r44309b/12
replace otros=. if r44309a==999999.99 

gen utilidades=r44401/12
replace utilidades=. if r44401==999999.99

gen dividendos=r44402/12
replace dividendos=. if r44402==999999.99

gen intereses=r44403/12
replace intereses=. if r44403==999999.99

gen herencias=r44404/12
replace herencias=. if r44404==999999.99

gen indemnizacion=r44405/12
replace indemnizacion=. if r44405==999999.99

gen ayudagob=r44406/12

gen acteventual=r44407/12
replace acteventual=. if r44407==999999.99

gen arrendamiento=r44408/12
replace arrendamiento=. if r44408==999999.99

gen otrosy=r44409/12
replace otrosy=. if r44409==999999.99

egen ynlm_ci=rsum(remesas ayuda cuotalim alqui alqneg jubil deveh otros utilidades dividendos intereses herencias indemnizacion ayudagob acteventual arrendamiento otrosy), missing
label var ynlm_ci "Ingreso no laboral monetario"

gen ynlnm_ci=.
gen ynlnm_ch=.

******************************
*	nrylmpri_ch
******************************
by idh_ch, sort: egen nrylmpri_ch=sum(nrylmpri_ci) if miembros_ci==1
label var nrylmpri_ch "Identificador de los hogares en donde alguno de los miembros NS/NR el ingreso de la actividad principal"
replace nrylmpri_ch=1 if nrylmpri_ch>0 & nrylmpri_ch<.

by idh_ch, sort: egen nrylmpri1_ch=sum(nrylmpri1_ci) if miembros_ci==1
replace nrylmpri1_ch=1 if nrylmpri1_ch>0 & nrylmpri1_ch<.
******************************
*	ylm_ch & ylm1_ch 
******************************
by idh_ch, sort: egen ylm_ch=sum(ylm_ci) if miembros_ci==1
label var ylm_ch "Ingreso laboral monetario del hogar-ignora no respuesta"
by idh_ch, sort: egen ylm1_ch=sum(ylm1_ci) if miembros_ci==1
******************************
*	ylmnr_ch & ylmnr1_ch 
******************************
by idh_ch, sort: egen ylmnr_ch=sum(ylm_ci) if miembros_ci==1
label var ylmnr_ch "Ingreso laboral monetario del hogar - considera la no respuesta"
replace ylmnr_ch=. if nrylmpri_ch==1

by idh_ch, sort: egen ylmnr1_ch=sum(ylm1_ci) if miembros_ci==1
replace ylmnr1_ch=. if nrylmpri1_ch==1

******************************
*	ylnm_ch & ylnm1_ch 
******************************
by idh_ch, sort: egen ylnm_ch=sum(ylnm_ci) if miembros_ci==1
label var ylnm_ch "Ingreso laboral no monetario del hogar" 
by idh_ch, sort: egen ylnm1_ch=sum(ylnm1_ci) if miembros_ci==1
******************************
*	ynlm_ch 
******************************
by idh_ch, sort: egen ynlm_ch=sum(ynlm_ci) if miembros_ci==1
label var ynlm_ch "Ingreso no laboral monetario del Hogar"
******************************
*	ynlnm_ch 
******************************
*gen ynlnm_ch=.
*label var ynlnm_ch "Ingreso no laboral no monetario del Hogar"
******************************
*	ylmhopri_ci & ylmhopri1_ci
******************************
gen ylmhopri_ci=ylmpri_ci/(horaspri_ci*4.3) 
label var ylmhopri_ci "Salario monetario de la actividad principal"

gen ylmhopri1_ci=ylmpri1_ci/(horaspri_ci*4.3)

******************************
*	ylmho_ci & ylm1ho_ci
******************************
gen ylmho_ci=ylm_ci/(horastot_ci*4.3) 
label var ylmho_ci "Salario monetario de todas las actividades"
gen ylmho1_ci=ylm1_ci/(horastot_ci*4.3)
******************************
*	autocons_ci 
******************************
gen autocons_ci=r430
replace autocons_ci=. if r430==999999.99
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
gen remesas_ci=remesas
label var remesas_ci "Remesas reportadas por el individuo"

by idh_ch, sort: gen remesas_ch=sum(remesas_ci) if miembros_ci==1
label var remesas_ch "Remesas del hogar"
******************************
*	durades_ci
******************************
gen durades_ci=.
replace durades_ci= r409/4.3 if /*desemp_ci==1 & */r409>0
******************************
*	antiguedad_ci
******************************
gen antiguedad_ci=.
*NA

			****************************
			***VARIABLES DE EDUCACION***
			****************************
			
/* 
Nota: Al no poder discriminar este anio entre bachillerato tecnico o general 
se adopta el secundario completo a los 11 anios de educación. 
*/

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
* hay 7 personas en esta base que declaran  no haber obtenido titulo con mas de 12 anios de educación, se toman como universitario incompleto.
gen eduui_ci = (aedu_ci == 12 | aedu_ci >= 12 & r220 == 2) // 12 anios de estudio o mas y título de bachiller
replace eduui_ci = 1 if aedu_ci >= 13 & inlist(r220, 1, .) // mas de 12 anios de estudio, perdido, no obtuvo.
replace eduui_ci = . if aedu_ci == .
label var eduui_ci "Universitaria o Terciaria Incompleta"

**************
***eduuc_ci***
**************
gen eduuc_ci=(aedu_ci > 12 & (r220 >= 3 & r220 <= 11)) // mas de 12 anios de estudio pero con titulo terciario; incluye profesorado
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
replace eduac_ci = 1 if r219a == 4
replace eduac_ci = 0 if r219a == 5
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
gen pqnoasis_ci=r221
label var pqnoasis_ci "Razones para no asistir a la escuela"
label define pqnoasis_ci 1 "Necesita trabajar" 2 " Causas del hogar"
label define pqnoasis_ci 3 "Muy caro" 4 " Enfermedad o discapacidad", add 
label define pqnoasis_ci 5 "Los padres no quieren" 6 "Por la edad" , add
label define pqnoasis_ci 7 "Finalizo sus estudios" 8 "No existe escuela cercana o cupo", add
label define pqnoasis_ci 9 "No le interesa no trae para estudiar" 10 "Repite mucho", add
label define pqnoasis_ci 11 "Quehaceres domesticos" 12 "Centro de ensenanza inhabilitado por terremotos", add
label define pqnoasis_ci 13 "Otros", add
label value pqnoasis_ci pqnoasis_ci

**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**
	
**************
*pqnoasis1_ci*
**************
g       pqnoasis1_ci = 1 if r221 ==3
replace pqnoasis1_ci = 2 if r221 ==1
replace pqnoasis1_ci = 3 if r221 ==4 | r221 ==5
replace pqnoasis1_ci = 4 if r221 ==9
replace pqnoasis1_ci = 5 if r221 ==2 | r221 ==11
replace pqnoasis1_ci = 6 if r221 ==7
replace pqnoasis1_ci = 7 if r221 ==6 
replace pqnoasis1_ci = 8 if r221 ==8  | r221 ==12
replace pqnoasis1_ci = 9 if r221 ==10 | r221 ==13

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
gen repiteult_ci= (r207a == 1)
replace repiteult_ci = . if  r207a == .
label var repiteult "Ha repetido el último grado"

***************
***edupub_ci***
***************
gen edupub_ci=.
replace edupub_ci=1 if r210a == 1 
replace edupub_ci=0 if r210a == 2 | r210a == 3
label var edupub_ci "Personas que asisten a centros de ensenanza publicos"



******************************************************************************
*	INFRAESTRUCTURE VARIABLES 
******************************************************************************
****************
***aguared_ch***
****************
generate aguared_ch =.
replace aguared_ch = 1 if r31301==1 | r31301==2
replace aguared_ch = 0 if r31301> 2
la var aguared_ch "Acceso a fuente de agua por red"
	
*****************
*aguafconsumo_ch*
*****************
gen aguafconsumo_ch = 0
replace aguafconsumo_ch = 1 if (r31301==1 |  r31301==2) & r314==1
replace aguafconsumo_ch = 2 if (r313a1==2) & r314==1
replace aguafconsumo_ch = 5 if (r313a1==7) & r314==1
replace aguafconsumo_ch= 6 if (r313a1==4) & r314==1
replace aguafconsumo_ch= 7 if (r313a1==1) & r314==1
replace aguafconsumo_ch = 8 if (r313a1==6) & r314==1
replace aguafconsumo_ch= 10 if (r313a1==8 | r313a1==3 |r313a1==5) & r314==1
*****************
*aguafuente_ch*
*****************

gen aguafuente_ch = 1 if r31301==1 |  r31301==2
replace aguafuente_ch = 2 if r313a1==2
replace aguafuente_ch = 5 if r313a1==7
replace aguafuente_ch= 6 if r313a1==4
replace aguafuente_ch= 7 if r313a1==1
replace aguafuente_ch = 8 if r313a1==6
replace aguafuente_ch= 10 if r313a1==8 | r313a1==3 |r313a1==5

*************
*aguadist_ch*
*************
gen aguadist_ch=0
replace aguadist_ch= 1 if  (r31301==1 | r313a1==1 | r313a1==4 | r313a1==5)
replace aguadist_ch= 2 if  r31301==2
replace aguadist_ch=3 if r313a1==2 | r313a1==3 | r313a1==8


**************
*aguadisp1_ch*
**************
gen aguadisp1_ch =9



**************
*aguadisp2_ch*
**************
gen aguadisp2_ch = 1 if r313a2<=11 
*replace aguadisp2_ch = 2 if  r313a2>=12
replace aguadisp2_ch = 3 if r313a2 ==24



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
replace bano_ch=1 if (r317==1 | r317==4)
replace bano_ch=2 if (r317==2 | r317==5)
replace bano_ch=3 if ( r317==7)
replace bano_ch=6 if (r317==3 | r317==6 )
replace bano_ch=0 if r317==8 

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
replace sinbano_ch = 0 if r317==8
replace sinbano_ch = 1 if r318==2
replace sinbano_ch = 2 if r318a==5 & r318==2
*label var sinbano_ch "= 0 si tiene baño en la vivienda o dentro del terreno"

*************
*aguatrat_ch*
*************
gen aguatrat_ch=0
replace aguatrat_ch=1 if r315 <4
*label var aguatrat_ch "= 9 la encuesta no pregunta de si se trata el agua antes de consumirla"


/*gen aguadist_ch=.
replace aguadist_ch=1 if r313==1
replace aguadist_ch=2 if r313==2
replace aguadist_ch=3 if r313>=2 & r313<=9

aguadist_ch:ubicacion de la principal fuente de agua
1 dentro de la casa
2 fuera de la casa pero outside del terreno ( o a menos de 100m de distancia)
3 dentro de la casa pero outside del terreno

Survey:

313. ¿TIENE ESTA VIVIENDA SERVICIO DE AGUA POR CAÑERIA?
01. ¿Dentro de la vivienda?
02. ¿Fuera de la vivienda pero dentro de la propiedad
03. No tiene
04. Tiene pero no le cae ( mas de un mes)

Si reponde 1 o 2, preguntar cuantas horas al dia...............>

313A. ¿COMO SE ABASTECE DE AGUA ESTA VIVIENDA?.................>r313a1  
01. ¿Cañería del vecino?
02. ¿Pila o chorro público?
03. ¿Chorro común?
04. ¿Camión, carreta o pipa?
05. ¿Pozo (privado o común)?
06. ¿Ojo de agua, río o quebrada?
07. ¿Colecta agua lluvia?
08. ¿Otros medios?
		(Especifique)......................................>r313a1ot 
Si reponde de 1 a 3, preguntar cuantas horas al día............>r313a2 */


******************************
*	luz_ch
******************************
gen luz_ch=(r312==1 | r312==2)
******************************
*	luzmide_ch
******************************
gen luzmide_ch=.
*NA
******************************
*	combust_ch
******************************
gen combust_ch=(r312>=1 & r312<=3)


******************************
*	des1_ch
******************************
gen des1_ch=.
replace des1_ch=0 if r317==8
replace des1_ch=1 if bano_ch==1 & (r318a==1 | r318a==3) 
replace des1_ch=2 if bano_ch==1 &  r318a==2
replace des1_ch=3 if bano_ch==1 & (r318a==4 |r318a==5)

/*Tipo de desagüe incluyendo la
definición de "Unimproved" del MDG
0 No corresponde: El hogar no tiene servicio higiénico.
1 El desagüe está conectado a la red general o a una cámara séptica
2 El desagüe está conectado a un pozo ciego o es una letrina.
3 El desagüe se comunica con la superficie: desemboca en un río o en la calle.

r318a: Como se deshacen de las aguas grises*/

******************************
*	des2_ch
******************************
gen des2_ch=.
replace des2_ch=0 if r317==8
replace des2_ch=1 if (des1_ch==1 | des1_ch==2)
replace des2_ch=2 if des1_ch==3 | (bano_ch==1 &  r318a==6)
******************************
*	piso_ch
******************************
gen piso_ch=.
replace piso_ch=0 if r304==4
replace piso_ch=1 if r304>=1 & r304<=3
replace piso_ch=2 if r304==5
******************************
*	pared_ch
******************************
gen pared_ch=.
replace pared_ch=0 if r303==2 | r303==3 | r303==6 | r303==7
replace pared_ch=1 if r303==1 | r303==4 | r303==5
replace pared_ch=2 if r303==8
******************************
*	techo_ch
******************************
gen techo_ch=.
replace techo_ch=0 if r302==5 | r302==6
replace techo_ch=1 if r302>=1 & r302<=4
replace techo_ch=2 if r302==7
******************************
*	resid_ch
******************************
gen resid_ch=.
replace resid_ch=0 if r324==1 | r324==2
replace resid_ch=1 if r324==4 | r324==5
replace resid_ch=2 if r324==6
replace resid_ch=3 if r324==3 | r324==7


******************************
*	dorm_ch
******************************
gen dorm_ch=r306
******************************
*	cuartos_ch
******************************
/*gen cuartos_ch=r305

Estrictamente hablando no se puede generar porque cuartos_ch es "Cantidad de habitaciones en el hogar" (no necesariamente para dormir). 
La pregunta r305 no incluye cocina, bano o garage*/

* MGR Jul, 2015: genero variable con r305 ya que toda la serie se genera de esta manera con excepción de 2006-2009
gen cuartos_ch=r305

******************************
*	cocina_ch
******************************
gen cocina_ch=(r321==1)
replace cocina_ch=. if r321==.
label var cocina_ch "Si existe un cuarto separado y exclusivo para cocinar"
******************************
*	telef_ch
******************************
gen telef_ch=(r3231a==1)
******************************
*	refrig_ch
******************************
gen refrig_ch=(r32505a==1)
******************************
*	freez_ch
******************************
gen freez_ch=.
*NA
******************************
*	auto_ch
******************************
gen auto_ch=(r32512a==1)
******************************
*	compu_ch
******************************
gen compu_ch=(r32509a==1)
******************************
*	internet_ch
******************************
gen internet_ch=(r3233a==1)

notes: la variable internet_ch en realidad indica Internet y/o correo electrónico

/*323. ¿TIENE UD. EN USO?
1. Teléfono Fijo
2. Teléfono Celular
3. Internet y/o correo electrónico
4. Cable*/

******************************
*	cel_ch
******************************
gen cel_ch=(r3232a==1)
******************************
*	vivi1_ch
******************************
gen vivi1_ch=.
replace vivi1_ch=1 if r301==1
replace vivi1_ch=2 if r301==2  
replace vivi1_ch=3 if r301>=3 & r301<10
******************************
*	vivi2_ch
******************************
gen vivi2_ch=(r301>=1 & r301<=2)
******************************
*	viviprop_ch
******************************
gen viviprop_ch=.
replace viviprop_ch=0 if r308a==1
replace viviprop_ch=1 if r308a==3
replace viviprop_ch=2 if r308a==2
replace viviprop_ch=3 if r308a>=6 & r308a<=9
label var viviprop_ch "Propiedad de la vivienda" 

/*ARM
0 Alquilada
1 Propia y totalmente pagada
2 Propia y en proceso de pago
3 Ocupada (propia de facto)

Survey: 308. ¿CUAL ES LA FORMA DE TENENCIA DE ESTA VIVIENDA
1. Inquilino
2. Propietario de la vivienda pero la está pagando a plazos"
3. Propietario
4. Propietario de la vivienda en terreno público
5. Propietario de la vivienda en terreno privado
6. Colono
7. Guardián de la vivienda
8. Ocupante gratuito
9. otros*/
******************************
*	vivitit_ch
******************************
gen vivitit_ch=.
*NA
******************************
*	vivialq_ch
******************************
gen vivialq_ch=r308c if r308a==1
label var vivialq_ch "Alquiler mensual"

notes: vivialq_ch, pago en dolares.

******************************
*	vivialqimp_ch
******************************
*gen vivialqimp_ch=.


****************
*afiliado_ci****
****************
gen afiliado_ci= (r109==1 | r109==2)
label var afiliado_ci "Afiliado a la Seguridad Social"
*Nota: seguridad social comprende solo los que en el futuro me ofrecen una pension.

****************
*cotizando_ci***
****************
gen cotizando_ci=0     if condocup_ci==1 | condocup_ci==2 
replace cotizando_ci=1 if (r421==1) & cotizando_ci==0 /*solo a emplead@s y asalariad@s, difiere con los otros paises*/
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
replace tipocontrato_ci=1 if r418==1 & emp_ci==1
replace tipocontrato_ci=2 if (r418==1 & r418a>0 & r418a<=99) & emp_ci==1
replace tipocontrato_ci=3 if r418==2 & emp_ci==1
replace tipocontrato_ci =. if r418==3 | emp_ci==0 
label var tipocontrato_ci "Tipo de contrato segun su duracion en act principal"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci
*/
* Incorporando variable de temporalidad en asalariados y firma de contrato. MGD 06/16/2014
gen tipocontrato_ci=.
replace tipocontrato_ci=1 if (r418==1 & r417==6) & categopri_ci==3
replace tipocontrato_ci=2 if (r418==1 & r417==7) & categopri_ci==3 
replace tipocontrato_ci=3 if (r418==2 | tipocontrato_ci==.) & categopri_ci==3
label var tipocontrato_ci "Tipo de contrato segun su duracion en act principal"
label define tipocontrato_ci 0 "Con contrato" 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci

*************
*tamemp_ci***
*************
gen tamemp_ci=1 if r420>=1 & r420<=5
replace tamemp_ci=2 if r420>=6 & r420<=50
replace tamemp_ci=3 if r420>50 & r420!=.
label var tamemp_ci "# empleados en la empresa segun rangos"
	label define tamemp_ci 1 "Pequena" 2 "Mediana" 3 "Grande" 
	label value tamemp_ci tamemp1_ci

*************
**pension_ci*
*************
gen pension_ci=0 
replace pension_ci=1 if (r44307a>0 & r44307a!=.) /* A todas las per mayores de cinco*/
label var pension_ci "1=Recibe pension contributiva"

*************
*ypen_ci*
*************
* MGD 12/22/2015: se recodifica a missings
recode r44307a (999999.99=.)
gen ypen_ci=r44307a*r44307b/12 if pension_ci==1
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
replace lpe_ci= 136.50	if mes_c==1 & zona_c==1
replace lpe_ci= 135.60  if mes_c==2 & zona_c==1
replace lpe_ci=	135.60	if mes_c==3 & zona_c==1
replace lpe_ci= 136.50	if mes_c==4 & zona_c==1
replace lpe_ci= 136.50	if mes_c==5 & zona_c==1
replace lpe_ci= 138.90	if mes_c==6 & zona_c==1
replace lpe_ci= 140.10	if mes_c==7 & zona_c==1
replace lpe_ci= 137.70	if mes_c==8 & zona_c==1
replace lpe_ci= 138.90	if mes_c==9 & zona_c==1
replace lpe_ci= 137.70	if mes_c==10 & zona_c==1
replace lpe_ci= 140.10	if mes_c==11 & zona_c==1
replace lpe_ci= 143.70  if mes_c==12 & zona_c==1

*zona rural
replace lpe_ci=98.70	if mes_c==1 & zona_c==0
replace lpe_ci=99.90	if mes_c==2 & zona_c==0
replace lpe_ci=97.20	if mes_c==3 & zona_c==0
replace lpe_ci=98.70	if mes_c==4 & zona_c==0
replace lpe_ci=98.70	if mes_c==5 & zona_c==0
replace lpe_ci=98.70	if mes_c==6 & zona_c==0
replace lpe_ci=98.70	if mes_c==7 & zona_c==0
replace lpe_ci=98.70	if mes_c==8 & zona_c==0
replace lpe_ci=99.90	if mes_c==9 & zona_c==0
replace lpe_ci=99.90	if mes_c==10 & zona_c==0
replace lpe_ci=99.90	if mes_c==11 & zona_c==0
replace lpe_ci=101.10   if mes_c==12 & zona_c==0
label var lpe_ci "Linea de indigencia oficial del pais"


/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/

*************
**salmm_ci***
*************
*gen salmm_ci= 138.66 /*fuente: ILO*/

*2015 MGD: salarios segun actividad
g salmm_ci=.
replace salmm_ci=87 if rama_ci==1
replace salmm_ci=164 if rama_ci==3
replace salmm_ci=174 if rama_ci==6
replace salmm_ci=142 if salmm_ci==.
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

























