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
 


*global ruta = "${surveysFolder}"

local PAIS SLV
local ENCUESTA EHPM
local ANO "2009"
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
Autores: *por Melisa Morales para Suzanne Duryea
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
*Inclusión Mayra Sáenz - Julio 2013

gen region_c=  r004

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
la var factor_ch "Household Expansion Factor"
******************************
*	idh_ch
******************************
so lote tipo folio viv
egen idh_ch= group(lote tipo folio viv)
la var idh_ch "Household ID"
******************************
*	idp_c
******************************
g idp_ci= r101
la var idp_ci "Individual ID"
******************************
*	zona_c
******************************
recode area (0=0 Rural) (1=1 Urban) ,g(zona_c)
******************************
*	pais_c & anio_c
******************************
g str3 pais_c="SLV"
la var pais_c "El Salvador"
g anio_c=edicion
la var anio_c "Year"
******************************
*	mes_c
******************************
g mes_c=r016m 
la var mes_c "Mes de la entrevista"
la de mes_c 1"Jan" 2"Feb" 3 "Mar" 4"Apr" 5"May" 6"Jun" 7"Jul" 8"Aug" 9"Sep" 10"Oct" 11"Nov" 12"Dec"
la val mes_c mes_c
******************************
*	relacion_ci
******************************
recode r103 (4/9=4 "Other Relative") (11=5 "Non relative") (10=6 Maid), g(relacion_ci)
la de relacion_ci 1 "Head" 2 "Spouse/Partner" 3 "Son/Daughter", add


******************************************************************************
*	DEMOGRAPHIC VARIABLES
******************************************************************************

******************************
*	factor_ci
******************************
g factor_ci= fac01
la var factor_ci "Individual Expansion Factor"
******************************
*	sexo_ci
******************************
g sexo_ci=r104
la de sexo_ci 1 "Hombre" 2 "Mujer"
la val sexo_ci sexo_ci
******************************
*	edad_ci
******************************
g edad_ci= r106                    
******************************
*	civil_ci
******************************
recode r107 (6=1 Single) (1 2=2 "Formal or informal union") (4 5=3 "Divorced or separated") (3=4 Widowed) (else=.), g (civil_ci)
******************************
*	jefe_ci
******************************
g jefe_ci=(relacion_ci==1)
la var jefe_ci "Household Head"
***************************************************************************
*	nconyuges_ch & nhijos_ch & notropari_ch & notronopari_ch & nempdom_ch
****************************************************************************
bys idh_ch: egen nconyuges_ch=sum(relacion_ci==2)
bys idh_ch: egen nhijos_ch=sum(relacion_ci==3)
bys idh_ch: egen notropari_ch=sum(relacion_ci==4)
bys idh_ch: egen notronopari_ch=sum(relacion_ci==5)
bys idh_ch: egen nempdom_ch=sum(relacion_ci==6)
la var nconyuges_ch "Numero de conyuges"
la var nhijos_ch "Numero de hijos"
la var notropari_ch "Numero de otros familiares"
la var notronopari_ch "Numero de no familiares"
la var nempdom_ch "Numero de empleados domesticos"
******************************
*	clasehog_ch
******************************
g clasehog_ch=.
replace clasehog_ch=1 if nhijos_ch==0 & nconyuges_ch==0 & notropari_ch==0 & notronopari_ch==0
replace clasehog_ch=2 if (nhijos_ch>0| nconyuges_ch>0) & (notropari_ch==0 & notronopari_ch==0)
replace clasehog_ch=3 if notropari_ch>0 & notronopari_ch==0
replace clasehog_ch=4 if ((nconyuges_ch>0 | nhijos_ch>0 | notropari_ch>0) & (notronopari_ch>0))
replace clasehog_ch=5 if nhijos_ch==0 & nconyuges_ch==0 & notropari_ch==0 & notronopari_ch>0
la var clasehog_ch "Tipo de hogar"
la de clasehog_ch 1 "Unipersonal" 2 "Nuclear" 3 "Ampliado" 4 "Compuesto" 5 "Corresidente"
la val clasehog_ch clasehog_ch
/*al parecer es estandar esta definicion. Me parece que hogar unipersonal (y los otros) deberian considerar la 
condicion " & jefe_ci==1" . Preguntar a Suzanne porque no la incluyen*/

***************************************************************************************
*	nmiembros_ch & nmayor21_ch & nmenor21_ch & nmayor65_ch & nmenor6_ch & nmenor1_ch  
***************************************************************************************
bys idh_ch: egen nmiembros_ch=sum(relacion_ci>=1 & relacion_ci<=4)
la var nmiembros_ch "Numero de familiares en el hogar"

bys idh_ch: egen nmayor21_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad>=21)
la var nmayor21_ch "Numero de familiares mayores a 21 anios"

bys idh_ch: egen nmenor21_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad<21)
la var nmenor21_ch "Numero de familiares menores a 21 anios"

bys idh_ch: egen nmayor65_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad_ci>=65)
la var nmayor65_ch "Numero de familiares mayores a 65 anios"

bys idh_ch: egen nmenor6_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad_ci<6)
la var nmenor6_ch "Numero de familiares menores a 6 anios"

bys idh_ch: egen nmenor1_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad_ci<1)
la var nmenor1_ch "Numero de familiares menores a 1 anio"

******************************
*	miembros_ci
******************************
*2014, 01 modificado segun documento metodologico by MLO
*g miembros_ci=(relacion_ci<6)
g miembros_ci=(relacion_ci<5)
la var miembros_ci "Household member"

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


/*******************************
*	emp_ci
******************************
g emp_ci=(r403==1 | (r403==2 & r404==1))
replace emp_ci=. if r403==. 
la var emp_ci "Empleado"
la def emp_ci 1"Si" 0 "No"
la val emp_ci emp_ci

*********************************************
*	desemp1_ci	& desemp2_ci & desemp3_ci  
*********************************************
g desemp1_ci=(emp_ci==0 &  r406==1)
replace desemp1_ci=. if r403==.
la var desemp1_ci "Personas sin trabajo que buscaron en el periodo de referencia"
 
g desemp2_ci=(desemp1_ci==1 | (emp_ci==0 & r406==2 & r407==18))
replace desemp2_ci=. if r403==.
la var desemp2_ci "des1 + no trabajaron ni buscaron en la ult semana pero esperan respuesta de solicit"
 
g desemp3_ci=. 


******************************
*	pea1_ci, pea2_ci, pea3_ci
******************************
g pea1_ci=(emp_ci==1 | desemp1_ci==1)
g pea2_ci=(emp_ci==1 | desemp2_ci==1)
g pea3_ci=.*/

******************************
*	desalent_ci
******************************
g desalent_ci=(emp_ci==0 & r406==2 & r407==3)
replace desalent_ci=. if emp_ci!=0
la var desalent_ci "Discouraged/Lost hope"
   

******************************
*	horaspri_ci & horastot_ci
******************************
egen horaspri_ci= rsum(r411a r411d) if emp_ci==1
la var horaspri_ci "Hs totales (semanales) trabajadas en act. principal"

egen horastot_ci=rsum(horaspri_ci r433) if emp_ci==1
la var horastot_ci "Hs totales (semanales)trabajadas en toda actividad"

******************************
*	subemp_ci
******************************
*Alternativa MGD 06/21/2014: aunque no se pregunta si quiere trabajr ams horas se puede inferir con la pregunta de que trabaja
*menor de 40 horas porque solo consiguio empleo parcial o falta de trabajo.

g subemp_ci=0
replace subemp_ci=1 if  emp_ci==1 & horaspri_ci<=30 & (r413== 3 | r413==2)

******************************
*	tiempoparc_ci
******************************
g tiempoparc_ci=(emp_ci==1 & r413==1 & horastot_ci<30)
replace tiempoparc_ci=. if emp_ci!=1
la var tiempoparc_ci "Trabajan menos de 30 hs semanales y no quieren trabajar mas"

******************************
*	categopri_ci
******************************
recode r418  (10=0 "Others")(1=1 "Own business") (2 3 4=2 "Self-employed") (6/9=3 Employee) (5=4 "Unpaid family worker") (else=.),g ( categopri_ci )
replace categopri_ci=. if emp_ci!=1

******************************
*	categosec_ci
******************************
g categosec_ci=.  	

/*
******************************
*	contrato_ci
******************************
g contrato_ci=(emp_ci==1 & r419==1)
replace contrato_ci=. if emp_ci!=1|r419==.|r419==3

******************************
*	segsoc_ci
******************************
g segsoc_ci=(r422==1 | r422==2) if emp_ci==1*/

******************************
*	nempleos_ci
******************************
recode r432 (2=1) (1=2) (else=.), g(nempleos_ci)
replace nempleos_ci=. if emp_ci!=1

/******************************
*	firmapeq_ci
******************************
g firmapeq_ci=(r421<=5) if emp_c==1
la var firmapeq_ci "1=5 o menos trabajadores"*/
******************************
*	spublico_ci
******************************
g spublico_ci=(r420==2 & emp_ci==1)
replace spublico_ci=. if categopri_ci==1 | categopri_ci==2 | categopri_ci==4 | r418==9 | emp_ci!=1
la var spublico_ci "Trabaja en sector publico"


******************************************************************************
*		LABOR DEMAND 
******************************************************************************

******************************
*	ocupa_ci 
******************************
*modificacion MLO 2014,01
g ocupa_ci=.
destring r414, replace
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
Se genera con R414 pero no tenemos que' significa R414

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
destring r416, replace
* MLO = supongo que se mantiene CIIU rev 3
replace rama_ci=1 if (r416>=100 & r416<=322) & emp_ci==1 
replace rama_ci=2 if (r416>=510 & r416<=990) & emp_ci==1 
replace rama_ci=3 if (r416>=1010 & r416<=3320) & emp_ci==1 
replace rama_ci=4 if (r416>=3510 & r416<=3900) & emp_ci==1 
replace rama_ci=5 if (r416>=4100 & r416<=4390) & emp_ci==1 
replace rama_ci=6 if ((r416>=4510 & r416<=4799) | (r416>=5510 & r416<=5630))& emp_ci==1 
replace rama_ci=7 if ((r416>=4911 & r416<=5320) | (r416>=6110 & r416<=6190)) & emp_ci==1 
replace rama_ci=8 if (r416>=6411 & r416<=8299) & emp_ci==1 
replace rama_ci=9 if ((r416>=5811 & r416<=6022) | (r416>=6201 & r416<=6399) | (r416>=8411 & r416<=9900)) & emp_ci==1 

/*Se genera con R416 pero no tenemos que' significa R416

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
la var ylmpri_ci "Ingreso laboral monetario actividad principal"

******************************
*	nrylmpri_ci 
******************************
g nrylmpri_ci=(ylmpri_ci==. & emp_ci==1)
replace nrylmpri_ci=. if emp_ci!=1
la var nrylmpri_ci "Identificador de No respuesta del ingreso de la actividad principal"

******************************
*	ylnmpri_ci 
******************************
if emp_ci==1{
g food1=r42505a*r42505b/12 
g ropa1=r42506a*r42506b/12 
g merca1=r42507a*r42507b/12 
g vivi1=r42508a*r42508b/12 
g trans1=r42509a*r42509b/12 
g segur1=r42510a*r42510b/12 
g otross1=r42512a*r42512b/12 
}
egen ylnmpri_ci=rsum(food1 ropa1 merca1 vivi1 trans1 segur1 otross1), missing
replace ylnmpri_ci=. if emp_ci!=1

******************************
*	ylmsec_ci & ylmsec1_ci
******************************
if emp_ci==1{
g ylmsec_ci=r434 
g hrsextras=r43501a*r43501b/12
g vacaciones=r43502a*r43502b/12
g aguinaldo=r43503a*r43503b/12 
g bonificaciones=r43504a*r43504b/12
}
egen ylmsec1_ci=rsum(ylmsec_ci hrsextras vacaciones aguinaldo bonificaciones), missing
replace ylmsec1_ci=. if emp_ci!=1 | r432!=1

******************************
*	ylnmsec_ci
******************************
if emp_ci==1{
g food2=r43505a*r43505b/12 
g ropa2=r43506a*r43506b/12 
g merca2=r43507a*r43507b/12 
g vivi2=r43508a*r43508b/12 
g trans2=r43509a*r43509b/12 
g segur2=r43510a*r43510b/12 
g otross2=r43512a*r43512b/12 
}
egen ylnmsec_ci=rsum(food2 ropa2 merca2 vivi2 trans2 segur2 otross2), missing
la var ylnmsec_ci "Ingreso laboral no monetario actividad secundaria"
replace ylnmsec_ci=. if emp_ci!=1 | r432!=1
******************************
*	ylm_ci 
******************************
egen ylm_ci=rsum(ylmpri_ci ylmsec_ci) if emp_ci==1, missing
la var ylm_ci "Ingreso laboral monetario total"
******************************
*	ylnm_ci
******************************
egen ylnm_ci=rsum(ylnmpri_ci ylnmsec_ci) if emp_ci==1, missing
la var ylnm_ci "Ingreso laboral no monetario total"
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
la var ynlm_ci "Ingreso no laboral monetario" 

gen ynlnm_ci=.
gen ynlnm_ch=.

******************************
*	nrylmpri_ch 
******************************
bys idh_ch: egen nrylmpri_ch=sum(nrylmpri_ci) if miembros_ci==1
la var nrylmpri_ch "Identificador de los hogares en donde alguno de los miembros NS/NR el ingreso de la actividad principal"
replace nrylmpri_ch=1 if nrylmpri_ch>0 & nrylmpri_ch<.
******************************
*	ylm_ch 
******************************
bys idh_ch: egen ylm_ch=sum(ylm_ci) if miembros_ci==1
la var ylm_ch "Ingreso laboral monetario del hogar-ignora no respuesta"

******************************
*	ylmnr_ch 
******************************
bys idh_ch: egen ylmnr_ch=sum(ylm_ci) if miembros_ci==1
la var ylmnr_ch "Ingreso laboral monetario del hogar - considera la no respuesta"
replace ylmnr_ch=. if nrylmpri_ch==1
******************************
*	ylnm_ch 
******************************
bys idh_ch: egen ylnm_ch=sum(ylnm_ci) if miembros_ci==1
la var ylnm_ch "Ingreso laboral no monetario del hogar" 
******************************
*	ynlm_ch 
******************************
bys idh_ch: egen ynlm_ch=sum(ynlm_ci) if miembros_ci==1
la var ynlm_ch "Ingreso no laboral monetario del Hogar"
******************************
*	ynlnm_ch 
******************************
*g ynlnm_ch=. /*NA: la var ynlnm_ch "Ingreso no laboral no monetario del Hogar"*/
******************************
*	ylmhopri_ci 
******************************
g ylmhopri_ci=ylmpri_ci/(horaspri_ci*4.3) 
la var ylmhopri_ci "Salario monetario de la actividad principal"
******************************
*	ylmho_ci 
******************************
g ylmho_ci=ylm_ci/(horastot_ci*4.3) 
la var ylmho_ci "Salario monetario de todas las actividades"
******************************
*	autocons_ci 
******************************
g autocons_ci=r431
la var autocons_ci "Autoconsumo reportado por el individuo"
******************************
*	autocons_ch 
******************************
bys idh_ch: egen autocons_ch=sum(autocons_ci) if miembros_ci==1
la var autocons_ch "Autoconsumo del Hogar"

******************************
*	remesas_ci & remesas_ch
******************************
g remesas_ci=.
/*g temp=remesaevent1
recode temp (0=.)
egen remesas_ci=rsum(remesas temp)
drop temp

replace remesas_ci=. if remesas==. & temp==.
*/


compare  r44401a r7041a

*Modificación Mayra Sáenz - Septiembre 2014
replace remesas_ci  = remesas

by idh_ch, sort: gen remesas_ch=sum(remesas_ci) if miembros_ci==1
label var remesas_ch "Remesas del hogar"

/*
egen remesas_ci=rsum(remesas remesaevent1(
la var remesas_ci "Cash remittances from abroad"

bys idh_ch: egen remesas_ch=sum(remesas_ci)if miembros_ci==1 
la var remesas_ch "Household Cash remittances from abroad"*/

*g remesas_ch=.

******************************
*	durades_ci
******************************
g durades_ci=r409/4.3 if r409>0  

******************************
*	antiguedad_ci
******************************
g antiguedad_ci=. 

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
replace eduui_ci = 1 if aedu_ci >= 13 & inlist(r219, 1,  .) // mas de 12 anios de estudio, perdido.
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


******************************
*	aguared_ch
******************************


/*
recode r312 (1 2=1 Yes)(else=0 No),g(aguared_ch)
*/

* Modificación Marcela Rubio Septiembre 2014: cambio en la sintaxis 
gen aguared_ch=.
replace aguared_ch=(r312<5)
label var aguared_ch "Acceso a fuente de agua por red"


******************************
*	aguadist_ch
******************************
g aguadist_ch=.
/*the last line cannot be codified (includes those households with no piped water

aguadist_ch: Ubicación de la principal fuente de agua
1 Adentro de la casa 
2 Afuera de la casa pero adentro del terreno (o a menos de 100mts de distancia)
3 Afuera de la casa y afuera del terreno (o a más de 100mts de distancia)*/

******************************
*	aguamala_ch
******************************
g aguamala_ch=(r315==1)

/*Se asume que si le hacen algun tratamiento al agua, es porque se duda de que sea buena*/
******************************
*	aguamide_ch
******************************
g aguamide_ch=.
*NA
******************************
*	luz_ch
******************************
recode r311 (1 2=1 Yes) (else=0 No), g(luz_ch) 
la var luz_ch "Electricity is the main lighting type"
******************************
*	luzmide_ch
******************************
g luzmide_ch=. 
******************************
*	combust_ch
******************************
recode r326 (3 1=1 Yes) (else=0 No), g(combust_ch)
la var combust_ch "Main fuel:Gas or electricity"

******************************
*	bano_ch 
******************************

recode r319 (1/8=1 Yes) (else=0 No), g(bano_ch)

******************************
*	banoex_ch
******************************

recode r321 (2=1 Yes) (else=0 No), g(banoex_ch)

******************************
*	des1_ch
******************************

* MGR Jul, 2015: corrección en sintáxis
/*
recode r325a (3 1=1) (2=2) (4=3) (else=.) if bano_ch==1,g (des1_ch)
replace des1_ch=0 if bano_ch==0
*/

g des1_ch=.
replace des1_ch=0 if bano_ch==0
replace des1_ch=1 if bano_ch==1 & (r319>=1 & r319<=4)
replace des1_ch=2 if bano_ch==1 & (r319>=5 & r319<=8)

******************************
*	des2_ch
******************************

* MGR Jul, 2015: corrección en sintáxis
/*
recode r325a (1/3=1) (else=2) if bano_ch==1,g (des2_ch)
replace des2_ch=0 if bano_ch==0
*/

g des2_ch=.
replace des2_ch=0 if bano_ch==0
replace des2_ch=1 if (des1_ch==1 | des1_ch==2)

******************************
*	piso_ch
******************************
recode r304 (5=0 "Dirt floor")(1/4=1 "Permanent materials")(6=2 "Other materials"),g (piso_ch)

******************************
*	pared_ch
******************************
recode r303 (2 3 5/8=0 "Non-permanent materials")(1 4 =1 "Permanent materials")(9=2 "Other materials"),g (pared_ch)

******************************
*	techo_ch
******************************
recode r302 (5/7=0 "Non-permanent materials")(1/4 =1 "Permanent materials")(8=2 "Other materials"),g (techo_ch)

******************************
*	resid_ch
******************************
recode r329 (1 2=0)(4 5=1)(6=2)(3 7=3),g (resid_ch)

**Daniela Zuluaga- Enero 2018: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**
	
*********************
***aguamejorada_ch***
*********************
g       aguamejorada_ch = 1 if  (r312 >=1 & r312 <=4) | (r313 >=4 & r313 <=5) | r313 == 8 | r313 == 10
replace aguamejorada_ch = 0 if ((r312 >=5 & r312 <=6) & (r313 != 4 | r313 != 5 | r313 != 8 | r313 != 10) ) | (r313 >=1 & r313 <=3) | (r313 >=6 & r313 <=7) | r313 == 9 | (r313 >=11 & r313 <=14)
				
*********************
***banomejorado_ch***
*********************
g       banomejorado_ch = 1 if  ((r319>=1 & r319 <=2) | r319 == 5 | r319 == 7) & r321 ==2
replace banomejorado_ch = 0 if (((r319>=1 & r319 <=2) | r319 == 5 | r319 == 7) & r321 ==1) | (r319>=3 & r319 <=4) | r319 == 6   | r319 == 8 | r317a ==4 | r318 ==2
	

******************************
*	dorm_ch
******************************
g dorm_ch=r310

******************************
*	cuartos_ch
******************************
*g cuartos_ch=.
/*g cuartos_ch=R305
Estrictamente no se podria generar porque cuartos_ch es "Cantidad de habitaciones en el hogar" (no necesariamente para dormir). 
La pregunta R305 no incluye cocina, banio o garage*/

* MGR Jul, 2015: genero variable con r305 ya que toda la serie se genera de esta manera con excepción de 2006-2009
g cuartos_ch=r305

******************************
*	cocina_ch
******************************
g cocina_ch=. 

******************************
*	telef_ch
******************************
g telef_ch=(r3281a==1)

******************************
*	refrig_ch
******************************
g refrig_ch=(r33005a==1)

******************************
*	freez_ch
******************************
g freez_ch=. /*NA*/

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

******************************
*	cel_ch
******************************
g cel_ch=(r3282a==1)

******************************
*	vivi1_ch
******************************
recode r301 (1=1 House) (2=2 "Apartment") (4/8=3 Other) ,g ( vivi1_ch)

******************************
*	vivi2_ch
******************************
g vivi2_ch=(r301>=1 & r301<=2)

******************************
*	viviprop_ch
******************************
recode r308a (1=0) (3/5=1) (6/9=3 Other) ,g ( viviprop_ch)

******************************
*	vivitit_ch
******************************
g vivitit_ch=.
*NA
******************************
*	vivialq_ch
******************************
g vivialq_ch=r308c if r308a==1
la var vivialq_ch "Alquiler mensual"

******************************
*	vivialqimp_ch
******************************
g vivialqimp_ch=.

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
*li_ci***
*********
gen lpe_ci =.
*zona urbana
replace lpe_ci= 174.24	if mes_c==1 & zona_c==1
replace lpe_ci= 172.05	if mes_c==2 & zona_c==1
replace lpe_ci=170.09	if mes_c==3 & zona_c==1
replace lpe_ci=167.62	if mes_c==4 & zona_c==1
replace lpe_ci=169.06	if mes_c==5 & zona_c==1
replace lpe_ci=168.93	if mes_c==6 & zona_c==1
replace lpe_ci=168.00	if mes_c==7 & zona_c==1
replace lpe_ci=166.97	if mes_c==8 & zona_c==1
replace lpe_ci=166.18	if mes_c==9 & zona_c==1
replace lpe_ci=165.29	if mes_c==10 & zona_c==1
replace lpe_ci=164.13	if mes_c==11 & zona_c==1
replace lpe_ci=163.34   if mes_c==12 & zona_c==1

*zona rural
replace lpe_ci=124.83	if mes_c==1 & zona_c==0
replace lpe_ci=124.70	if mes_c==2 & zona_c==0
replace lpe_ci=123.94	if mes_c==3 & zona_c==0
replace lpe_ci=122.61	if mes_c==4 & zona_c==0
replace lpe_ci=124.25	if mes_c==5 & zona_c==0
replace lpe_ci=123.22	if mes_c==6 & zona_c==0
replace lpe_ci=120.51	if mes_c==7 & zona_c==0
replace lpe_ci=118.73	if mes_c==8 & zona_c==0
replace lpe_ci=116.37	if mes_c==9 & zona_c==0
replace lpe_ci=117.57	if mes_c==10 & zona_c==0
replace lpe_ci=116.67	if mes_c==11 & zona_c==0
replace lpe_ci=117.57   if mes_c==12 & zona_c==0
label var lpe_ci "Linea de indigencia oficial del pais"


/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/

*************
**salmm_ci***
*************
*gen salmm_ci= 176 /*fuente: ILO*/

*2015 MGD: salarios segun actividad
g salmm_ci=.
replace salmm_ci=103 if rama_ci==1
replace salmm_ci=188 if rama_ci==3
replace salmm_ci=208 if rama_ci==6
replace salmm_ci=166 if salmm_ci==.
label var salmm_ci "Salario minimo legal"

*************
***tecnica_ci**
*************
gen tecnica_ci=(r204==5)
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
aguared_ch aguadist_ch aguamala_ch aguamide_ch luz_ch luzmide_ch combust_ch	bano_ch banoex_ch des1_ch des2_ch piso_ch aguamejorada_ch banomejorado_ch  ///
pared_ch techo_ch resid_ch dorm_ch cuartos_ch cocina_ch telef_ch refrig_ch freez_ch auto_ch compu_ch internet_ch cel_ch ///
vivi1_ch vivi2_ch viviprop_ch vivitit_ch vivialq_ch	vivialqimp_ch , first




compress


saveold "`base_out'", replace


log close


