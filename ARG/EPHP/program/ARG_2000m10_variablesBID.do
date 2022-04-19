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

local PAIS ARG
local ENCUESTA EPHP
local ANO "2000"
local ronda m10

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
   



capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Argentina
Encuesta: EPHP
Round: m10
Autores:
Última versión: Maria Laura Oliveri - Email: mloliveri@iadb.org, lauraoliveri@yahoo.com
Fecha última modificación: 31 de Octubre de 2013

			  
							SCL/LMK - IADB
****************************************************************************/
/***************************************************************************
Detalle de procesamientos o modificaciones anteriores:
****************************************************************************/

clear all
set more off
use "`base_in'", clear


		**********************************
		***VARIABLES DEL IDENTIFICACION***
		**********************************
		
	****************
	* region_BID_c *
	****************
	
gen region_BID_c=4

label var region_BID_c "Regiones BID"
label define region_BID_c 1 "Centroamérica_(CID)" 2 "Caribe_(CCB)" 3 "Andinos_(CAN)" 4 "Cono_Sur_(CSC)"
label value region_BID_c region_BID_c


	****************
	* region_c *
	****************
	
gen region_c=.
replace region_c=1  if (aglomerado>=2 & aglomerado<=3) | (aglomerado>=33 & aglomerado<=34) | (aglomerado==38) /*Buenos Aires */
replace region_c=2  if aglomerado==22                          /*Catamarca*/
replace region_c=3  if aglomerado==8                           /*Chaco*/
replace region_c=4  if aglomerado==9 | aglomerado==91          /*Chubut*/
replace region_c=5  if aglomerado==32                          /*Ciudad de Buenos Aires*/
replace region_c=6  if aglomerado==13 | aglomerado==36         /*Córdova*/
replace region_c=7  if aglomerado==12                          /*Corrientes*/
replace region_c=8  if aglomerado==6 | aglomerado==14          /*Entre Ríos*/
replace region_c=9  if aglomerado==15                          /*Formosa*/
replace region_c=10 if aglomerado==19                          /*Jujuy*/
replace region_c=11 if aglomerado==30                          /*La pampa*/
replace region_c=12 if aglomerado==25                          /*La Rioja*/
replace region_c=13 if aglomerado==10                          /*Mendoza*/
replace region_c=14 if aglomerado==7                           /*Misiones*/
replace region_c=15 if aglomerado==17                          /*Neuquen*/
replace region_c=16 if aglomerado==93                          /*Río Negro*/ 
replace region_c=17 if aglomerado==23                          /*Salta*/
replace region_c=18 if aglomerado==27                          /*San Juan*/ 
replace region_c=19 if aglomerado==26                          /*San Luis*/
replace region_c=20 if aglomerado==20                          /*Santa Cruz*/
replace region_c=21 if aglomerado>=4 & aglomerado<=5           /*Santa Fe*/
replace region_c=22 if aglomerado==18                          /*Santiago de Estero*/
replace region_c=23 if aglomerado==31                          /*Tierra del Fuego*/
replace region_c=24 if aglomerado==29                          /*Tucuman*/

	label define region_c     ///
	1"Buenos Aires"           ///	
	2"Catamarca"              ///
	3"Chaco"                  /// 
	4"Chubut"                 ///
	5"Ciudad de Buenos Aires" ///
	6"Córdoba"                ///
	7"Corrientes"             ///
	8"Entre Ríos"             ///
	9"Formosa"                ///
	10"Jujuy"                 ///
	11"La Pampa"              ///
	12"La Rioja"              ///
	13"Mendoza"               ///
	14"Misiones"              ///
	15"Neuquon"               ///
	16"Río Negro"             ///
	17"Salta"                 ///
	18"San Juan"              ///
	19"San Luis"              ///
	20"Santa Cruz"            ///
	21"Santa Fe"              ///
	22"Santiago del Estero"   ///
	23"Tierra del Fuego"      ///
	24"Tucumán"               
   label value region_c region_c
   label var region_c "division politico-administrativa, provincia"

/***********************************************************************
Variables de Hogar
************************************************************************/

/***********
La variable aglomerado se llama aglomera en algunas encuestas y aglomerado en otras.
Lo mismo ocurre con componen/componente. Voy a corregir eso.
************/

gen a=aglomera
drop aglomera
rename a aglomera

gen b=componen
drop componen
rename b componen

/***********
Factor de expansion del hogar (factor_ch)
************/

gen factor_ch=pondera

/***********
anio
************/
/*
capture destring ano, replace
replace ano=1992 if ano==92
replace ano=1993 if ano==93
replace ano=1994 if ano==94
replace ano=1995 if ano==95
replace ano=1996 if ano==96
replace ano=1997 if ano==97
replace ano=1998 if ano==98
replace ano=1999 if ano==99
replace ano=2000 if ano==00
replace ano=2001 if ano==01
replace ano=2002 if ano==02

rename ano anio_c
*/
gen anio_c=2000
/***********
idh_ch (idhogar)
************/

gen str2 ciudad=string(aglomera)
gen str13 idh_ch=codusu+ciudad
drop ciudad


/***********
idp_ci
Para reconocer a un individuo se deben usar las variables idp_ci AND idh_ch
************/

gen idp_ci=componen

/***********
zona
La encuesta se realiza solo en areas urbanas
************/

gen zona_c=1

/***********
pais
************/

gen str3 pais_c="ARG"

/***********
mes
************/

capture destring onda, replace
gen mes_c=.
replace mes_c=5 if onda==1
replace mes_c=10 if onda==3
drop if mes_c==5

/***********
relacion_ci
************/

gen relacion_ci=1 if h08==1
replace relacion_ci=2 if h08==2
replace relacion_ci=3 if h08==3
replace relacion_ci=4 if h08==4 | h08==5 | h08==6 | h08==7 | h08==8 | h08==9
replace relacion_ci=5 if h08==10
replace relacion_ci=6 if h08==11

label define parent 1 "Jefe" 2 "Conyuge" 3 "Hijo" 4 "Otros Parientes" 5 "Otros no Parientes" 6 "Servicio Domestico"

label values relacion_ci relacion_ci

/***********
edad_ci
************/

capture gen edad_ci=h12
replace edad_ci=0 if edad_ci==-1
replace edad_ci=98 if edad_ci>=98

sort idh_ch 
gen byte jefe_ci=(relacion_ci==1)
egen byte nconyuges_ch=sum(relacion_ci==2), by(idh_ch)
egen byte nhijos_ch=sum(relacion_ci==3), by(idh_ch)
egen byte notropari_ch=sum(relacion_ci==4),by(idh_ch)
egen byte notronopari_ch=sum(relacion_ci==5), by(idh_ch)
egen byte nempdom_ch=sum(relacion_ci==6), by(idh_ch)
gen byte clasehog_ch=0
**** unipersonal
replace clasehog_ch=1 if nhijos_ch==0 & nconyuges_ch==0 & notropari_ch==0 & notronopari_ch==0
**** nuclear (child with or without spouse but without other relatives)
replace clasehog_ch=2 if nhijos_ch>0 & notropari_ch==0 & notronopari_ch==0
**** nuclear (spouse with or without children but without other relatives)
replace clasehog_ch=2 if nhijos_ch==0 & nconyuges_ch>0 & notropari_ch==0 & notronopari_ch==0
**** ampliado
replace clasehog_ch=3 if notropari_ch>0 & notronopari_ch==0
**** compuesto (some relatives plus non relative)
replace clasehog_ch=4 if ((nconyuges_ch>0 | nhijos_ch>0 | notropari_ch>0) & (notronopari_ch>0))
**** corresidente
replace clasehog_ch=5 if nhijos_ch==0 & nconyuges_ch==0 & notropari_ch==0 & notronopari_ch>0
sort idh_ch
*** number of persons in household (not including domestic employees or their
*** relatives or guests, but including corresidentes)
*** note: there was a small change to include otros parientes in nmiembros_ch
*2014, 01 Modificacion MLO segun docuemnto metodologico
/*
egen byte nmiembros_ch=sum(relacion_ci>0 & relacion_ci<=5) if relacion_ci~=6, by (idh_ch)
egen byte nmayor21_ch=sum((relacion_ci>0 & relacion_ci<=5) & (edad_ci>=21 & edad_ci<=98)), by(idh_ch)
egen byte nmenor21_ch=sum((relacion_ci>0 & relacion_ci<=5) & (edad_ci<21)), by(idh_ch)
egen byte nmayor65_ch=sum((relacion_ci>0 & relacion_ci<=5) & (edad_ci>=65)), by(idh_ch)
egen byte nmenor6_ch=sum((relacion_ci>0 & relacion_ci<=5) & (edad_ci<6)), by(idh_ch)
egen byte nmenor1_ch=sum((relacion_ci>0 & relacion_ci<=5) & (edad_ci<1)), by(idh_ch)
*/
egen byte nmiembros_ch=sum(relacion_ci>0 & relacion_ci<5), by (idh_ch)
egen byte nmayor21_ch=sum((relacion_ci>0 & relacion_ci<5) & (edad_ci>=21 & edad_ci<=98)), by(idh_ch)
egen byte nmenor21_ch=sum((relacion_ci>0 & relacion_ci<5) & (edad_ci<21)), by(idh_ch)
egen byte nmayor65_ch=sum((relacion_ci>0 & relacion_ci<5) & (edad_ci>=65)), by(idh_ch)
egen byte nmenor6_ch=sum((relacion_ci>0 & relacion_ci<5) & (edad_ci<6)), by(idh_ch)
egen byte nmenor1_ch=sum((relacion_ci>0 & relacion_ci<5) & (edad_ci<1)), by(idh_ch)
/***********
miembros_ci
************/

*gen miembros_ci=(relacion_ci>=1 & relacion_ci<=5)
gen miembros_ci=(relacion_ci>=1 & relacion_ci<5)
/********************************************************************** 
Variables Demograficas
***********************************************************************/ 

/***********
factor de expansion individual
************/

gen factor_ci=pondera

/***********
sexo_ci
************/

capture gen sexo_ci=h13
drop if sexo_ci>2

/***********
Estado Civil
************/

gen civil_ci=h14
replace civil_ci=2 if civil_ci==3
replace civil_ci=3 if civil_ci==4
replace civil_ci=4 if civil_ci==5
replace civil_ci=. if h14==9


/*********************************************************************** 
Variables de Demando Laboral
************************************************************************/

/***********
Ocupa_ci: Cada aglomerado va cambiando el numero de digitos que usa para armar su variable de educacion de acuerdo al 
siguiente cuadro:


	1992	1993	1994	1995	1996	1997	1998
1	2	3	3	3	3	3	3
2		2	2		3	3	3
3				2	2	2	3
4	2	2	2	3	3	3	3
5		2	3	3	3	3	3
6	2	2	3	3	3	3	3
7					2	3	3
8			2	2	2	3	3
9	2	2	2	2	2	2	3
10	2	2	3	3	3	3	3
12			2	2	2	3	3
13	2	2	3	3	3	3	3
14					3	3	3
15					2	3	3
17	2	3	3	3	3		3
18			2	2	2	3	3
19	2	2	2	2	2	3	3
20		2	2	2	2	2	3
22		2		2	2	3	3
23	2	2	2	2	2	3	3
25			2		2	3	3
26	2	2	2	2	2	2	3
27	2		2	2	2	2	3
29	2	2	3	3	3	3	3
30		3	3	3	3	3	3
31	2	2	2	2	2	2	3
32						3	3
33						3	3
34					3	3	3
36					3	3	3

Por lo tanto, se va a proceder a agregar la variable de ocupacion una vez que esten armadas todas las bases anuales-nacionales
por separado, al final del do.file
**************/

/***********
rama_ci
It was difficult to construct the variable Rama. The problem is that for the bananas 
we are using the ISIC Rev 2, but Argentinean surveys use ISIC Rev 3, so there are more 
categories than the ones that appear in the bananas. I added "Administracion Publica y 
Defensa, planes de seguridad social y de afiliación" (p18=751 a to 759), "Enseñanza" 
(801 to 809), "Actividades de Servicios Sociales y de salud" (851 to 859), 
"Otras actividades comunitarias, sociales y de salud" (900 to 930) and "Hogars privados 
con servicio domestico" to Rama==9 (=Servicios Sociales, comunales y personales). 
Finally, I haven't added "Organizaciones y organos extraterritoriales" (990) and 
"Actividades no bien especificadas" (997 to 999) to any category
************/  
/*
capture drop rama_ci
gen rama_ci = .
		
replace rama_ci = 1 if p18>=11 & p18<=50
replace rama_ci = 2 if p18>=101 & p18<=142
replace rama_ci = 3 if p18>=150 & p18<=372
replace rama_ci = 4 if p18>=401 & p18<=419
replace rama_ci = 5 if p18>=451 & p18<=459
replace rama_ci = 6 if p18>=500 & p18<=552
replace rama_ci = 7 if p18>=601 & p18<=650
replace rama_ci = 8 if p18>=651 & p18<=749
replace rama_ci = 9 if p18>=751 & p18<=930
*/
/*********** 
Horas
Horas del trabajo principal. Esta variable puede crearse solo para el periodo 1995-2001, pero no para antes de 1995
************/


gen horaspri_ci=p15p
replace horaspri_ci=. if p15p>=900
replace horaspri_ci=. if horaspri_ci<0
 

/*********** 
Horas totales trabajadas en todas las actividades
Esta variable se llamaba p15 en el periodo 1992-1994, y paso a llamarse p15t a partir de 1995
************/


gen horastot_ci=p15t
replace horastot_ci=. if p15t>900



replace horastot_ci=. if horastot_ci<0

  	
label var horaspri_ci "Horas trabajadas en la actividad principal"
		
label var horastot_ci "Horas trabajadas en todas las actividades"




/************
ramault_ci
*************/

gen ramault_ci=.
		
replace ramault_ci = 1 if p39>=11 & p39<=50
replace ramault_ci = 2 if p39>=101 & p39<=142
replace ramault_ci = 3 if p39>=150 & p39<=372
replace ramault_ci = 4 if p39>=401 & p39<=419
replace ramault_ci = 5 if p39>=451 & p39<=459
replace ramault_ci = 6 if p39>=500 & p39<=552
replace ramault_ci = 7 if p39>=601 & p39<=650
replace ramault_ci = 8 if p39>=651 & p39<=749
replace ramault_ci = 9 if p39>=751 & p39<=930

/************
ramault2_ci
*************/

gen ramaul2_ci= .
		
/**********
Duracion del desempleo (medido en meses)
***********/

replace p32d=0 if p32d<0
replace p32=0 if p32<0
replace p32d=p32d/30
egen durades_ci=rsum(p32 p32d), missing
		
label var durades_ci "duracion del desempleo (mensual)"	

g durades1_ci=.

/*********************************************************************** 
Variables del mercado laboral
************************************************************************/
****************
****condocup_ci*
****************

gen condocup_ci=.
replace condocup_ci=1 if estado==1
replace condocup_ci=2 if estado==2
replace condocup_ci=3 if estado==3 & edad_ci>=10
replace condocup_ci=. if estado == 0
replace condocup_ci=4 if estado == 4 | edad_ci<10
label var condocup_ci "Condicion de ocupación de acuerdo a def de cada pais"
label define condocup_ci 1 "Ocupado" 2 "Desocupado" 3 "Inactivo" 4 "Menor de PET" 
label value condocup_ci condocup_ci


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

/**********
Antiguedad (en anios)
***********/

replace p22m=. if p22m==-1 | p22m==99 | p22m<0
replace p22=. if p22==99 | p22<0
replace p22m=p22m/12
egen antiguedad_ci=rsum(p22 p22m) if emp_ci==1, missing
		
label var antiguedad_ci "antiguedad laboral (anios)"	

/*
/**********
emp_ci
No considere empleados a aquellos que fueron suspendidos o no trabajaron por falta de trabajo
(cuenta propistas)
***********/

gen emp_ci=((p01==1) | (p01==2 & p04==1 & (p05==3|p05==4|p05==5)))

/**********
desemp1_ci 
***********/

gen desemp1_ci=(p01==2 & p07==1)

/**********
desemp2_ci
***********/

gen desemp2_ci=(desemp1_ci==1 | (p01==2 & p07==2))

/**********
desemp3_ci 
***********/

gen desemp3_ci=(desemp2_ci | (p01==2 & p07==2 & p37==2))

/**********
pea1_ci 
***********/

gen pea1_ci=(emp_ci==1 | desemp1_ci==1)

/**********
pea2_ci
***********/

gen pea2_ci=(emp_ci==1 | desemp2_ci==1)

/**********
pea3_ci 
***********/

gen pea3_ci=(emp_ci==1 | desemp3_ci==1)
*/
/**********
desalent_ci
***********/

gen desalent_ci=(pea_ci~=1 & (p01==2 & p07==2) & p08==4) 

/**********
subemp_ci
***********/

gen subemp_ci=(horaspri_ci>=1 & horaspri_ci<=30 & p16==1 & emp_ci==1)

/**********
tiempoparc_ci
***********/

gen tiempoparc_ci=(horaspri_ci>=1 & horaspri_ci<=30 & p16==2 & emp_ci==1)

/**********
categopri_ci 
***********/

gen categopri_ci=p17 if emp_ci==1
replace categopri_ci=. if categopri_ci<1 | categopri_ci>4

/**********
categosec_ci 
***********/

gen categosec_ci=.

/**********
contrato_ci
If the person is employed and has signed a contract
There's no way to know if a worker has signed a contract or no
***********/

*gen contrato_ci=.

/**********
segsoc_ci 
It's difficult to define this variable in a proper way.  We don't consider the 
people that declare to have aguinaldo (4), vacaciones (8), Vacaciones y Aguinaldo (12), 
Indemnizacion (32), Indemnizacion y Aguinaldo (36), Indemnización y Vacaciones (40) and 
Indemnización, vacaciones y aguinaldo (44)
***********/

*gen segsoc_ci=(p23~=. & p23~=-2 & p23~=4 & p23~=8 & p23~=12 & p23~=32 & p23~=36 & p23~=40 & p23~=44 & p23~=64 & p23~=99 & p23~=0 & emp_ci==1) 
*replace segsoc_ci=. if emp_ci~=1

/**********
nempleos_ci
***********/

gen nempleos_ci=1 if p12==1
replace nempleos_ci=2 if p12>=2 & p12<=8

/**********
tamfirma_ci 
***********/

gen tamfirma_ci=(emp_ci==1 & (p19>=3 & p19<=8))
replace tamfirma_ci=. if p19==9

/**********
firmapeq_ci 
***********/
*capture drop firmapeq_ci
*gen firmapeq_ci=1 if tamfirma_ci==0 & emp_ci==1
*replace  firmapeq_ci=0 if tamfirma_ci==1 & emp_ci==1


/**********
spublico_ci 
***********/

gen spublico_ci=(p18b==1 & emp_ci==1)
replace spublico_ci=. if p18b==9 | p18b==.

/******************************************
Variables de Ingreso
Cuestion a tener en cuenta: hay casos en que individuos que declaran ser desempleados,
aparecen con ingresos laborales positivos. Esto puede deberse a que la persona estuvo
trabajando hasta poco tiempo antes de ser entrevistado y en el momento de la entrevista
haya estado desempleado, o a un error. En este programa no se va a hacer nada respecto a 
estos casos, pero solo se lo remarca.
*******************************************/


/*************
Variables generadas
ylmpri_ci nrylmpri_ci ylm_ci ynlm_ci nrylmpri_ch ylm_ch ylmnr_ch ynlm_ch ylmhopri_ci ylmho_ci 

Variables generadas para chequeo
ylm_ci_chequeo nrylm_ci  

**************/


/*************
Para 1995 a 2002
**************/


label var p21 "ingreso proveniente de la ocupacion principal"
label var p47_1 "ingreso asalariado"
label var p47_2 "ingreso por bonificación no habituales (asalariados)"
label var p47_3 "ingreso como cuenta propista"
label var p47_4 "como ganancia de patron"
label var p48_1 "jubilacion o pension"
label var p48_2 "alquileres, rentas o intereses"
label var p48_3 "utilidades, beneficios o dividendos"
label var p48_4 "seguro de desempleo"
label var p48_5 "indemnizacion por despido"
label var p48_6 "beca de estudio"
label var p48_7 "cuota de alimentos"
label var p48_8 "aportes de personas que no viven en el hogar"
label var p48_9 "otros"

		
/******
Now I will define some variables to know who is not declaring incomes. 
These dummies take a 0 value when the person is not reporting incomes
*******/

gen nodeclap21=((p21~=. & p21~=9) & p21~=-9)

forvalues n=1(1)4 {
gen nodeclap47`n'=((p47_`n'~=-9) & (p21~=9 | p21~=0))
}

forvalues p=1(1)9 {
gen nodeclap48`p'=(p48_`p'~=-9)
}
	
/**********		
Now I will sum the incomes, so I will replace the missing observations by 0. 
The variables created above will tell you which individuals that have a 0 in the 
income variables are the ones that didn't report incomes
***********/		
		
replace p21=0 if nodeclap21==0

forvalues n=1(1)4 {
replace p47_`n'=0 if nodeclap47`n'==0

gen p47`n'mis=(p47_`n'==.)

replace p47_`n'=0 if p47`n'mis==1
}


forvalues p=1(1)9 {
replace p48_`p'=0 if nodeclap48`p'==0

gen p48`p'mis=(p48_`p'==.)

replace p48_`p'=0 if p48`p'mis==1
}

/***********
ylmpri_ci
Voy a utilizar la variable p21 para generar esta variable (Cuanto gana en esa ocupacion).
Hay veces que p21 es positiva pero la fuente es de ingreso no laboral (ya que los valores
provienen de p48_1-9 y no de p47_1-4, que son las que se asocian a ingresos laborales, 
por eso al generar la variable digo que ylmpri_ci tome el valor de p21 siempre y cuando 
alguna p47_1-4 tome valores distintos de cero.
Otro problema es que p21 no esta necesariamente medida en terminos mensuales (en la gran
mayoria de los casos, al corroborar el valor de p21 con las p47 se puede ver que esta en 
terminos mensuales, pero no en todos los casos). Para tener en cuenta este punto voy a 
considerar que si p21 toma menor valor que alguna p47_1-4 y la persona declara tener una
sola ocupacion, el valor real es el de la variable p47 (el que sin dudas es mensual).
************/

gen ylmpri_ci=p21 if ((p47_1>0 & p47_1<.) | (p47_2>0 & p47_2<.) | (p47_3>0 & p47_3<.) | (p47_4>0 & p47_4<.))
replace ylmpri_ci=p47_1+p47_2 if (p21<p47_1+p47_2 & p47_3==0 & p47_4==0) & p12==1
replace ylmpri_ci=p47_3 if (p21<p47_3 & p47_1==0 & p47_2==0 & p47_4==0) & p12==1
replace ylmpri_ci=p47_4 if (p21<p47_4 & p47_1==0 & p47_2==0 & p47_3==0) & p12==1

replace ylmpri_ci=p47_1+p47_2 if (p21>p47_1+p47_2 & p47_3==0 & p47_4==0) & p12==1
replace ylmpri_ci=p47_3 if (p21>p47_3 & p47_1==0 & p47_2==0 & p47_4==0) & p12==1
replace ylmpri_ci=p47_4 if (p21>p47_4 & p47_1==0 & p47_2==0 & p47_3==0) & p12==1

/***********
ylnmpri_ci
************/

gen ylnmpri_ci=.

/***********
ylmsec_ci
************/

gen ylmsec_ci=.

/***********
ylnmsec_ci
************/

gen ylnmsec_ci=.

/***********
ylmotros_ci
************/

gen ylmotros_ci=.

/***********
ylnmotros_ci
************/

gen ylnmotros_ci=.

/***********
nrylmpri_ci
************/

gen byte nrylmpri_ci=0
replace nrylmpri_ci=1 if nodeclap21==0

/***********
ylm_ci
Para crear el ingreso laboral, sumo los ingresos provenientes de trabajar como
asalariado, cuenta propista y patron.
Si bien hay veces que p21 (ingreso de la ocupacion principal), es mayor que la suma 
de las p471-3, a pesar de que la suma de las p47 no solo tiene en cuenta la ocupacion 
principal sino tambien secundarias, puede deberse a que en esos casos el individuo en 
cuestion cobre en la ocupacion principal cada mas de 30 dias. Supongo que lo que ocurre
es esto.
************/

gen ylm_ci=p47_1+p47_2+p47_3+p47_4
replace ylm_ci=. if p47_1==. & p47_2==. & p47_3==. & p47_4==. 

/***********
ylnm_ci
************/

gen ylnm_ci=.

/************
ynlm_ci
*************/


*** Cambio realizado el 08/30/05 ya que algunas de las respuestas en p48* contenian un 2, que representa una no respuesta, y no debe ser sumado como parte de ynlm_ci
gen ynlm_ci=p48_1+p48_2+p48_3+p48_4+p48_5+p48_6+p48_7+p48_8+p48_9
replace ynlm_ci=. if (p48_1==.|p48_1==2) & (p48_2==.|p48_2==2) & (p48_3==.|p48_3==2) & (p48_4==.|p48_4==2) & (p48_5==.|p48_5==2) & (p48_6==.|p48_6==2) & (p48_7==.|p48_7==2) & (p48_8==.|p48_8==2) & (p48_9==.| p48_9==2)


/***********
ynlnm_ci
************/

gen ynlnm_ci=.

/************
nrylmpri_ch
*************/

by idh_ch, sort: egen nrylmpri_ch=sum(nrylmpri_ci) if miembros_ci==1, missing
replace nrylmpri_ch=1 if nrylmpri_ch>1 & nrylmpri_ch<.

/************
ylm_ch
*************/

by idh_ch, sort: egen ylm_ch=sum(ylm_ci) if miembros_ci==1, missing

/***********
ylnm_ch
************/

gen ylnm_ch=.

/************
ylmnr_ch
*************/

by idh_ch, sort: egen ylmnr_ch=sum(ylm_ci) if miembros_ci==1, missing
replace ylmnr_ch=. if nrylmpri_ch==1

/************
ynlm_ch
*************/

by idh_ch, sort: egen ynlm_ch=sum(ynlm_ci) if miembros_ci==1, missing

/***********
ynlnm_ch
************/

gen ynlnm_ch=.

/***********
rentaimp_ch
************/

gen rentaimp_ch=.

/***********
autocons_ci
************/

gen autocons_ci=.

/***********
autocons_ch
************/

gen autocons_ch=.

/***********
remesas_ci
************/

gen remesas_ci=.

/***********
remesas_ch
************/

gen remesas_ch=.

/***********
ylmhopri_ci
************/

gen ylmhopri_ci=ylmpri_ci/(horaspri_ci*4.2)

/***********
ylmho_ci
************/

gen ylmho_ci=ylm_ci/(horastot_ci*4.2)
replace ylmho_ci=. if ylmho_ci<=0

replace p21=. if nodeclap21==0

forvalues n=1(1)4 {
replace p47_`n'=. if nodeclap47`n'==0 | p47`n'mis==1
}

forvalues p=1(1)9 {
replace p48_`p'=. if nodeclap48`p'==0 | p48`p'mis==1
}



replace ylmpri_ci=. if ylmpri_ci<0
replace ylm_ci=. if ylm_ci<0
replace ynlm_ci=. if ynlm_ci<0
replace ylm_ci=. if ylm_ci<0
replace ylmnr_ch=. if ylmnr_ch<0
replace ynlm_ch=. if ynlm_ch<0

/******************************************************************************** 
Education 
*********************************************************************************/


capture replace p56="." if p56=="-" | p56=="c" | p56=="\" | p56=="`" | p56=="/" | p56=="C" | p56=="?" | p56=="m"
capture destring p56, replace


/**********
aedu_ci (Anios de educacion)
Disponible a partir de 1995
***********/

capture destring p58b, replace 

gen aedu_ci=.
replace aedu_ci=0 if p55==3
replace aedu_ci=0 if p56==0
replace aedu_ci=0 if p56==1 & p58==2 & p58b==0
forvalues v=1(1)7 {
replace aedu_ci=`v' if p56==1 & p58==2 & p58b==`v'
}
replace aedu_ci=7 if p56==1 & p58==1
replace aedu_ci=7 if (p56>=2 & p56<=6) & p58==2 & p58b==0
replace aedu_ci=8 if (p56>=2 & p56<=6) & p58==2 & p58b==1
replace aedu_ci=9 if (p56>=2 & p56<=6) & p58==2 & p58b==2
replace aedu_ci=10 if (p56>=2 & p56<=6) & p58==2 & p58b==3
replace aedu_ci=11 if (p56>=2 & p56<=6) & p58==2 & p58b==4
replace aedu_ci=12 if (p56>=2 & p56<=6) & p58==2 & p58b==5
replace aedu_ci=13 if (p56>=2 & p56<=6) & p58==2 & p58b==6
replace aedu_ci=12 if (p56>=2 & p56<=6) & p58==1
replace aedu_ci=12 if (p56>=2 & p56<=6) & p58==1 & p58b==5
replace aedu_ci=13 if (p56>=2 & p56<=6) & p58==1 & p58b==6
replace aedu_ci=12 if (p56>=7 & p56<=8) & p58==2 & p58b==0
replace aedu_ci=13 if (p56>=7 & p56<=8) & p58==2 & p58b==1
replace aedu_ci=14 if (p56>=7 & p56<=8) & p58==2 & p58b==2
replace aedu_ci=15 if (p56>=7 & p56<=8) & p58==2 & p58b==3
replace aedu_ci=16 if (p56>=7 & p56<=8) & p58==2 & p58b==4
replace aedu_ci=17 if (p56>=7 & p56<=8) & p58==2 & p58b>=5 & p58b<=9
replace aedu_ci=15 if p56==7 & p58==1
replace aedu_ci=17 if p56==8 & p58==1


/**********
eduno_ci 
***********/

gen eduno_ci=(p55==3 | p56==0)

/**********
edupi_ci (Primaria Incompleta)
***********/

gen edupi_ci=(p56==1 & p58==2)

/**********
edupc_ci (Primaria Completa)
***********/

gen edupc_ci=(p56==1 & p58==1)

/**********
edusi_ci (Secundaria Incompleta)
***********/

gen edusi_ci=(p56>=2 & p56<=6 & p58==2)

/**********
edusc_ci (Secundaria Completa)
***********/

gen edusc_ci=(p56>=2 & p56<=6 & p58==1)

/**********
eduui_ci (Universitaria Incompleta)
***********/

gen eduui_ci=(p56>=7 & p56<=8 & p58==2)

/**********
eduuc_ci (Universitaria Completa)
***********/

gen eduuc_ci=(p56>=7 & p56<=8 & p58==1)

/**********
edus1i_ci 
***********/

gen edus1i_ci=.

/**********
edus1c_ci 
***********/

gen edus1c_ci=.

/**********
edus2i_ci 
***********/

gen edus2i_ci=.

/**********
edus2c_ci 
***********/

gen edus2c_ci=.

/**********
edupre_ci (Educacion Preescolar)
Si bien en el año 1992 esta el rubro (nivel==10), no se distingue de aquellos que nunca fueron al colegio,
por lo que es conveniente no crearla
***********/

gen edupre_ci=.

/**********
eduac_ci 
***********/

gen eduac_ci=.
replace eduac_ci=1 if p56==8 & (eduuc_ci==1 | eduui_ci==1)
replace eduac_ci=0 if (eduuc_ci==1 | eduui_ci==1) & p56~=8



/**********
asiste_ci (Variable Dummy de asistencia escolar)
***********/

gen asiste_ci=(p55==1)

/**********
Pqnoasis (Razones para no asistir a la escuela)
No disponible
***********/

gen pqnoasis_ci=.

**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci**
	
**************
*pqnoasis1_ci*
**************
	
gen pqnoasis1_ci=. 

/**********
repite_ci
***********/

gen repite_ci=.

/**********
edupub_ci
***********/

gen edupub_ci=.


label var  aedu_ci "Anios de Educacion"

label var  eduno_ci "Sin Educacion"
		
label var  edupi_ci "Primaria Incompleta"
		
label var  edupc_ci "Primaria Completa"
		
label var  edusi_ci "Secundaria Incompleta"
		
label var  edusc_ci "Secundaria Completa"
		
label var  eduui_ci "Universitaria o Terciaria Incompleta"
		
label var  eduuc_ci  "Universitaria o Terciaria Completa"
		

/***************************************************
Variables generadas que indican cual es la mayor cantidad de regiones con las que se
pueden contar de acuerdo al año inicial que se desea tomar. A partir de 1997, todas las
encuestas cuentan con la misma cantidad de regiones. Se debe tener en cuenta que estas 
variables son utiles para contar con la mayor cantidad de regiones si uno esta interesado
en tener series de tiempo o trabajar con al menos dos años (para lo cual se necesita ser 
consistente en las regiones a incorporar en los diferentes periodos. 
En caso que se este interesado en un analisis de corte transversal, en los cuales un año 
es suficiente, lo indicado es trabajar con todas las regiones existentes para el año en cuestion,
por lo que no se requiere la utilizacion de estas variables.
****************************************************/

gen byte muestra_92=(aglomera==1 | aglomera==2 | aglomera==5 | aglomera==6 | aglomera==9 | aglomera==17 | aglomera==19 | aglomera==20 | aglomera==23 | aglomera==26 | aglomera==30 | aglomera==31 | aglomera==32 | aglomera==33)
                       
gen byte muestra_93=((muestra_92==1 | aglomera==22 | aglomera==29) & (anio>=1993 & anio<=2002))                       
                       
gen byte muestra_94=((muestra_93==1 | aglomera==12 | aglomera==8 | aglomera==18 | aglomera==27) & (anio>=1994 & anio<=2002))                       
  
gen byte muestra_95=((muestra_94==1 | aglomera==14 | aglomera==15 | aglomera==34 | aglomera==7 | aglomera==36) & (anio>=1995 & anio<=2002))                       

gen byte muestra_96=((muestra_95==1 | aglomera==03 | aglomera==25 | aglomera==10) & (anio>=1996 & anio<=2002))
                       
gen byte muestra_97=((muestra_96==1 | aglomera==13 | aglomera==04) & (anio>=1997 & anio<=2002))

gen byte muestra_98=((muestra_97==1) & (anio>=1998 & anio<=2002))

gen byte muestra_99=((muestra_97==1) & (anio>=1999 & anio<=2002))

gen byte muestra_00=((muestra_97==1) & (anio>=2000 & anio<=2002))
                      
gen byte muestra_01=((muestra_97==1) & (anio>=2001 & anio<=2002))

gen byte muestra_02=((muestra_97==1 | aglomera==91 | aglomera==38 | aglomera==93) & (anio>=2002 & anio<=2002))




**************************************************************************************************************************
/*********************************************************************** 

               BASES HOGARES ARGENTINA, ONDA DE OCTUBRE
               
************************************************************************/
/*March 29, 2006. Modificacion hecha por Analia
The following code:
if `i'>=104 & `i'<=132 {
gen vivi1_ch=1 if tipoviv==1
replace vivi1_ch=2 if tipoviv==2
replace vivi1_ch=3 if tipoviv>2 & tipoviv<=8
gen vivi2_ch=(vivi1_ch==1 | vivi1_ch==2)
gen cuartos_ch=habitacion
gen aguared_ch=(agua==1)
gen bano_ch=(bano==1)
gen viviprop_ch=0 if tenencia==3
replace viviprop_ch=1 if tenencia==1 | tenencia==2
replace viviprop_ch=3 if tenencia==4 | tenencia==5
replace viviprop_ch=4 if tenencia==8
gen pared_ch=0 if material==4 | material==5
replace pared_ch=1 if material<4
replace pared_ch=2 if material==8
gen luz_ch=.
gen banoex_ch=.
gen des1_ch=.
gen des2_ch=.
}
has been dropped.
The problem was that in 1997 for certain provinces (Resistencia, Posadas, Mendoza, Santiago, Mar del Plata, Catamarca, 
Tierra del Fuego, Tucuman, Bahia Blanca, Formosa, Corrientes, Concordia y Salta) the databases did not contain the original
variables (in the form of p0j), but only aggregated variables by CEPAL (such as the ones displayed in the code above). In 
order to construct some armonized variables the original were needed, although this wasn't obvious at first sight. Upon 
recognition of the error, updated databases for these "aglomerados" replaced the previous ones.
*/

/******************
Variables de Hogar
*******************/
* MGR Jul, 2015: variables de módulo vivienda tenían mismo nombre que variables de módulo de personas por lo que se sobreescribía sobre ellas
* Renombro variables de vivienda y hago nuevamente el merge. Armonizo con las variables correctas.

/*Daniela Zuluaga- Enero 2018, 
 Se generan alas variables de hogar a partir de estas variables originales- renombradas y mejorando sintaxis*/

gen vivi1_ch=1 if p01_hog==1
replace vivi1_ch=2 if p01_hog==2
replace vivi1_ch=3 if p01_hog>2 & p01_hog<=8

gen vivi2_ch=(vivi1_ch==1 | vivi1_ch==2)

replace vivi2_ch=. if vivi1_ch==.

gen cuartos_ch=p03_hog
replace cuartos_ch=. if p03_hog==98 | p03_hog==99

gen aguared_ch=.
replace aguared_ch=1 if p04_hog==1
replace aguared_ch=0 if  p04_hog==2

gen luz_ch=.
replace luz_ch=1 if p05_hog==1
replace luz_ch=0 if p05_hog==2

gen bano_ch=.
replace bano_ch=1 if p06a_hog==1 | p06a_hog==2
replace bano_ch=0 if p06a_hog==3

gen banoex_ch=.
replace banoex_ch=1 if p06d_hog==1
replace banoex_ch=0 if p06d_hog==2


gen des1_ch=0 if bano_ch==0
replace des1_ch=1 if p06c_hog==1 | p06c_hog==2
replace des1_ch=2 if p06c_hog==3
replace des1_ch=. if p06c_hog==9


gen des2_ch=0 if bano_ch==0
replace des2_ch=1 if p06c_hog==1 | p06c_hog==2 | p06c_hog==3
replace des2_ch=. if p06c_hog==9


gen viviprop_ch=.
replace viviprop_ch=0 if p07_hog==3
replace viviprop_ch=1 if p07_hog==1 | p07_hog==2
replace viviprop_ch=3 if p07_hog==4 | p07_hog==5 | p07_hog==8

gen pared_ch=.
replace pared_ch=0 if p08_hog==4 | p08_hog==5
replace pared_ch=1 if p08_hog<4
replace pared_ch=2 if p08_hog==8

gen aguadist_ch=.
gen aguamala_ch=.
gen aguamide_ch=.
gen luzmide_ch=.
gen combust_ch=.
gen piso_ch=.
gen techo_ch=.
gen resid_ch=.
gen dorm_ch=.
gen cocina_ch=.
gen telef_ch=.
gen refrig_ch=.
gen freez_ch=.
gen auto_ch=.
gen compu_ch=.
gen internet_ch=.
gen cel_ch=.
gen vivitit_ch=.
gen vivialq_ch=.
gen vivialqimp_ch=.

	**Daniela Zuluaga- Enero 2018: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**
	
	*********************
    ***aguamejorada_ch***
    *********************
	gen  aguamejorada_ch = 1 if p04_hog == 1 
	replace  aguamejorada_ch = 0 if p04_hog == 2
		
	*********************
    ***banomejorado_ch***
    *********************
	gen banomejorado_ch = 1 if (p06a_hog == 1 & (p06b_hog == 1 | p06b_hog == 2)  & (p06c_hog==1| p06c_hog==2 | p06c_hog==3) & p06d_hog == 1)
	replace  banomejorado_ch = 0 if (p06a_hog == 1 & (p06b_hog == 1 | p06b_hog == 2)  & (p06c_hog==1| p06c_hog==2 | p06c_hog==3) & p06d_hog == 2) | (p06a_hog == 1 & p06b_hog == 3 & (p06d_hog == 1 | p06d_hog == 2)) | p06a_hog == 2
	


/****************************************************
Variables generadas que indican cual es la mayor cantidad de regiones con las que se
pueden contar de acuerdo al año inicial que se desea tomar. A partir de 1997, todas las
encuestas cuentan con la misma cantidad de regiones. Se debe tener en cuenta que estas 
variables son utiles para contar con la mayor cantidad de regiones si uno esta interesado
en tener series de tiempo o trabajar con al menos dos años (para lo cual se necesita ser 
consistente en las regiones a incorporar en los diferentes periodos. 
En caso que se este interesado en un analisis de corte transversal, en los cuales un año 
es suficiente, lo indicado es trabajar con todas las regiones existentes para el año en cuestion,
por lo que no se requiere la utilizacion de estas variables.
****************************************************/
/*
capture destring p20, replace
gen ocupa = .
replace ocupa = 1 if (p20>=130 & p20<150) | (p20>=230 & p20<240)| (p20 >=430 & p20<470) | (p20 >=940 & p20<950)| (p20>=410 & p20<420) | (p20>=210 & p20<220) | (p20>=110 & p20<120)								
replace ocupa = 2 if p20 ==11 | p20 ==21 | p20 ==31 | p20 ==41 | p20 ==51		
replace ocupa = 3 if (p20>=120 & p20<130) | (p20>=320 & p20<330) | (p20>=220 & p20<230) | (p20>=360 & p20<370) | (p20>=420 & p20<430) | (p20>=520 & p20<530) | (p20>=620 & p20<630) | (p20>=660 & p20<670) | (p20>=720 & p20<730) | (p20>=820 & p20<830) | (p20>=860 & p20<870) | (p20>=920 & p20<930) | (p20>=970 & p20<980)													
replace ocupa = 4 if (p20>=310 & p20<320) | (p20>=330 & p20<350) 		 									 						
replace ocupa = 5 if (p20>=300 & p20<310) | (p20>=350 & p20<360) | (p20>=370 & p20<380) | (p20>=390 & p20<400) | (p20>=470 & p20<500) | (p20>=510 & p20<520) | (p20>=530 & p20<600) | (p20>=850 & p20<860) | (p20>=870 & p20<880) | (p20>=930 & p20<940) | (p20>=950 & p20<970) | (p20>=980 & p20<990)  						
replace ocupa = 6 if (p20>=610 & p20<620) | (p20>=630 & p20<660) | (p20>=670 & p20<680) | (p20>=730 & p20<740)																									
replace ocupa = 7 if (p20>=380 & p20<390) | (p20>=500 & p20<510) | (p20>=680 & p20<690) | (p20>=740 & p20<750) | (p20>=760 & p20<770) | (p20>=780 & p20<790) | (p20>=840 & p20<850)															
replace ocupa = 8 if (p20>=400 & p20<410)			 																	
replace ocupa = 9 if (p20>=710 & p20<720) | (p20>=810 & p20<820) | (p20>=910 & p20<920) | (p20>=750 & p20<760) | (p20>=770 & p20<780) | (p20>=830 & p20<840)																											
*/

* Modificacion MGD 06/26/2014: segun clasificacion en el manual de la encuesta y considerando la clasificacion del 3er digito para porfesionales y tecnicos.
* Comparable con la serie posterior.
gen ocup4=substr(p20,3,1)
destring ocup4 p20, replace

gen ocupa_ci = .
replace ocupa_ci = 1 if (((p20>=111 & p20<=149) | (p20>=211 & p20<=239)) & (ocup4==1 | ocup4==2)) & emp_ci==1
replace ocupa_ci = 2 if (p20 ==11 | p20 ==21 | p20 ==31 | p20 ==41 | p20 ==51) & emp_ci==1		
replace ocupa_ci = 3 if (((p20>=111 & p20<=149) | (p20>=211 & p20<=239)) & (ocup4==3 | ocup4==4)) & emp_ci==1
replace ocupa_ci = 4 if (p20>=302 & p20<=394) & emp_ci==1		 									 						
replace ocupa_ci = 5 if (p20>=411 & p20<=594) & emp_ci==1					
replace ocupa_ci = 6 if (p20>=611 & p20<=683) & emp_ci==1 																									
replace ocupa_ci = 7 if (p20>=711 & p20<=997) & emp_ci==1 	
replace ocupa_ci = 8 if (p20>=400 & p20<410)			 																													
replace ocupa_ci = 9 if ((p20>=998 & p20<=999) | ocup4>=7 | p20==198) & emp_ci==1  & ocupa_ci!=2																											
replace ocupa_ci =. if ocup4==. & ocupa_ci!=2

label var     ocupa_ci "ocupation in primary job"
label define  ocupa_ci 1 "Profesionales y técnicos", add
label define  ocupa_ci 2 "Directores y funcionarios superiores", add
label define  ocupa_ci 3 "Personal administrativo y nivel intermedio", add
label define  ocupa_ci 4 "Comerciantes y vendedores", add
label define  ocupa_ci 5 "Trabajadores en servicios", add
label define  ocupa_ci 6 "Trabajadores agrícolas y afines", add
label define  ocupa_ci 7 "Obreros no agrícolas, conductores de maquinas y vehículos de   transporte y similares", add
label define  ocupa_ci 8 "Fuerzas Armadas", add
label define  ocupa_ci 9 "Otras ocupaciones no clasificadas en las anteriores", add

/************************************************************************************************************
* 2. Recategorización de variables/ corrección de errores de las bases armonizadas
************************************************************************************************************/
gen semestre_c=.
	
	****************************
	***VARIABLES DE EDUCACION***
	****************************

*************
***aedu_ci*** 
*************

replace eduno_ci=1	if aedu_ci==0 
replace aedu_ci=3 	if aedu_ci==. & edupi_ci==1
replace aedu_ci=7 	if aedu_ci==. & edupc_ci==1
replace aedu_ci=9 	if aedu_ci==. & edusi_ci==1
replace aedu_ci=12 	if aedu_ci==. & edusc_ci==1
replace aedu_ci=14 	if aedu_ci==. & eduui_ci==1
replace aedu_ci=18 	if aedu_ci==. & eduuc_ci==1

			
*******************************************************
***           VARIABLES DE DIVERSIDAD               ***
*******************************************************				
* Maria Antonella Pereira & Nathalia Maya - Marzo 2021	

			
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
/*

*************
*subemp_ci*** 
*************
tab subemp_ci
replace subemp_ci=1 if p15t<30 & p16==1
recode subemp_ci .=0 if emp_ci==1
tab subemp_ci*/

*************
***rama_ci*** 
*************

gen rama_ci = .
		
replace rama_ci = 1 if (p18>=10 & p18<=50) & emp_ci==1
replace rama_ci = 2 if (p18>=100 & p18<=149) & emp_ci==1
replace rama_ci = 3 if (p18>=150 & p18<=390) & emp_ci==1
replace rama_ci = 4 if (p18>=400 & p18<=420) & emp_ci==1
replace rama_ci = 5 if (p18>=450 & p18<=459) & emp_ci==1
replace rama_ci = 6 if (p18>=500 & p18<=559) & emp_ci==1
replace rama_ci = 7 if (p18>=600 & p18<=649) & emp_ci==1
replace rama_ci = 8 if (p18>=650 & p18<=749) & emp_ci==1
replace rama_ci = 9 if (p18>=750 & p18<=998) & emp_ci==1
/*label var rama_ci "Rama de actividad"
label def rama_ci 1"Agricultura, caza, silvicultura y pesca" 2"Explotación de minas y canteras" 3"Industrias manufactureras"
label def rama_ci 4"Electricidad, gas y agua" 5"Construcción" 6"Comercio, restaurantes y hoteles" 7"Transporte y almacenamiento", add
label def rama_ci 8"Establecimientos financieros, seguros e inmuebles" 9"Servicios sociales y comunales", add
label val rama_ci rama_ci */


/************************************************************************************************************
* Líneas de pobreza oficiales
************************************************************************************************************/

*********
*lp_ci***
*********
capture drop lp_ci
gen lp_ci =154.95

label var lp_ci "Linea de pobreza oficial del pais"

*********
*lpe_ci***
*********

gen lpe_ci =64.30

label var lpe_ci "Linea de indigencia oficial del pais"


****************
*cotizando_ci***
****************
gen cotizando_ci=.
replace cotizando_ci = 1 if benef2==11 | benef2==12 | benef2==14
replace cotizando_ci = 0 if benef2 == 13 | benef2 == 15
replace cotizando_ci = 0 if benef2 == 16 | benef2 == 17
recode cotizando_ci .=0 if (estado==1 & (categopri_ci==1 | categopri_ci==2))| (estado==1 & (categosec_ci==1 | categosec_ci==2))  /*independiente que no cotiza en primera/segunda ocupacion*/ 
replace cotizando_ci =0 if estado==2										/* desocupados no cotizan*/
label var cotizando_ci "Cotizante a la Seguridad Social"
label define cotizando_ci 0"No cotiza" 1"Cotiza a la SS" 
label value cotizando_ci cotizando_ci

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
 

*****************
*tipocontrato_ci*
*****************

gen tipocontrato_ci=.
replace tipocontrato_ci=1 if p24==1
replace tipocontrato_ci=2 if p24>=2 & p24<=4
label var tipocontrato_ci "Tipo de contrato segun su duracion"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci



*************
*cesante_ci* 
*************

gen cesante_ci=1 if p37==1
recode cesante_ci .=0 if pea_ci==1 & desemp_ci==0
* No todos los desempleados respondieron si han trabajado antes
label var cesante_ci "Desocupado - definicion oficial del pais"	


*************
*tamemp_ci
*************

*gen tamemp_ci=p19
*este variable se mide a forma diferente que anos en la encuesta continua

*************
*tamemp_ci
*************
gen tamemp_ci=.
replace tamemp_ci=1 if p19 == 1 | p19 == 2
replace tamemp_ci=2 if (p19 >= 3 & p19 <=5) | p19b == 1
replace tamemp_ci=3 if (p19 >= 6 & p19 < 9) | p19b == 2 

label var tamemp_ci "# empleados en la empresa segun rangos"
label define tamemp_ci 1 "Pequena" 2 "Mediana" 3 "Grande"
label value tamemp_ci tamemp_ci

*************
**pension_ci*
*************
recode fuente (5 10 14 17 19 21=1 "Si recibio") (else=0 "No recibio") , gen(pension_ci) 
replace pension_ci = 1 if p48_1>0 
recode pension_ci .=0
label var pension_ci "1=Recibe pension contributiva"

*************
**ypen_ci*
*************

gen ypen_ci=p48_1
label var ypen_ci "Valor de la pension contributiva"

***************
*pensionsub_ci*
***************

gen byte pensionsub_ci=.
label var pensionsub_ci "1=recibe pension subsidiada / no contributiva"

*****************
**ypensub_ci*
*****************

gen byte ypensub_ci=.
label var ypensub_ci "Valor de la pension subsidiada / no contributiva"

*************
**salmm_ci***
*************

* ARG 2000 - Oct
gen salmm_ci= 	200
label var salmm_ci "Salario minimo legal"

*************
***tecnica_ci**
*************
gen tecnica_ci=.
label var tecnica_ci "=1 formacion terciaria tecnica"



***************
***formal_ci***
***************

gen byte formal_ci=1 if cotizando_ci==1 & (condocup_ci==1 | condocup_ci==2)
recode formal_ci .=0 if (condocup_ci==1 | condocup_ci==2)
label var formal_ci "1=afiliado o cotizante / PEA"



******************
***categoinac_ci**
******************
gen categoinac_ci=.
replace categoinac_ci=1 if p11==1
replace categoinac_ci=2 if p11==3
replace categoinac_ci=4 if p11==4
recode categoinac_ci .= 4 if condocup_ci==3

label var categoinac_ci "Condición de inactividad"
	label define categoinac_ci 1 "jubilado/pensionado" 2 "estudiante" 3 "quehaceres_domesticos" 4 "otros_inactivos" 
	label value categoinac_ci categoinac_ci
	
	

*variables que faltan generar
*faltan las variables de LMK
gen tcylmpri_ci =.
gen tcylmpri_ch =.
gen repiteult_ci=.
gen instcot_ci=.



/*_____________________________________________________________________________________________________*/
* Asignación de etiquetas e inserción de variables externas: tipo de cambio, Indice de Precios al 
* Consumidor (2011=100), Paridad de Poder Adquisitivo (PPA 2011),  líneas de pobreza
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
aguared_ch aguadist_ch aguamala_ch aguamide_ch luz_ch luzmide_ch combust_ch	bano_ch banoex_ch des1_ch des2_ch piso_ch aguamejorada_ch banomejorado_ch ///
pared_ch techo_ch resid_ch dorm_ch cuartos_ch cocina_ch telef_ch refrig_ch freez_ch auto_ch compu_ch internet_ch cel_ch ///
vivi1_ch vivi2_ch viviprop_ch vivitit_ch vivialq_ch	vivialqimp_ch , first

/*Homologar nombre del identificador de ocupaciones (isco, ciuo, etc.) y dejarlo en base armonizada 
para análisis de trends (en el marco de estudios sobre el futuro del trabajo)*/
rename p20 codocupa
rename p18 codindustria

compress


saveold "`base_out'", replace


log close
