clear
set mem 400m
set more off

capture log close

use "${surveysFolder}\ARM\URU\ECH\2007\Van_data\ury07.dta", clear


/***************************************************************************************************************************
 							armonización 2007
****************************************************************************************************************************/

/************************************************************************/
/*				VARIABLES DEL HOGAR			*/
/************************************************************************/
gen idh_ch=correlat
gen idp_ci=nper
gen factor_ch=pesoan
gen zona_c=1 /*La encuesta es solo urbana!*/
gen str3 pais_c="URY"
gen anio_c=2007

destring dpto, replace
gen region_c = dpto
gen mes_c=.

/*
e32
1	Jefe
2	Esposo o compañero
3	Hijo de ambos
4	Hijo sólo del jefe
5	Hijo sólo del cónyuge
6	Yerno o nuera
7	Padre
8	Suegro
9	Hermano
10	Cuñado
11	Nieto
12	Otro pariente
13	Otro no pariente
14	Servicio doméstico o familiar del mismo
*/

gen relacion_ci=e32
replace relacion_ci=3 if e32==4 | e32==5
replace relacion_ci=4 if e32>=6 & e32<=14
replace relacion_ci=5 if e32==13
replace relacion_ci=6 if e32==14
label define relacion_ci 1 "Jefe" 2 "Conyuge" 3 "Hijo" 4 "Otros Parientes" 5 "Otros no Parientes" 6 "Servicio Domestico"
label values relacion_ci relacion_ci

/************************************************************************/
/*			VARIABLES DE INFRAESTRUCTURA DEL HOGAR		*/
/************************************************************************/	

/*
d11	
1	Red general
2	Canilla pública
3	Pozo surgente no protegido
4	Pozo surgente protegido
5	Aljibe
6	Arroyo, río
7	Otro
*/

gen aguared_ch=(d11==1)
replace aguared_ch =. if d11==.

gen aguadist_ch=.

gen aguamala_ch=(d11==5|d11==6) 
replace aguamala_ch =. if d11==.

gen aguamide_ch=.

gen luz_ch=.

gen luzmide_ch=.

/*
d21	1	Energía eléctrica (U.T.E)
	2	Energía eléctrica (grupo electrógeno)
	3	Gas por cañería
	4	Supergás
	5	Queroseno
	6	Leña
	7	Ninguna

*/

gen combust_ch=(d21>=1 & d21<=6)
replace combust_ch =. if d10==.


/*
d15	1	Sí. Con cisterna
	2	Sí. Sin cisterna
	3	No
*/

gen bano_ch=(d15!=3)
replace bano_ch = . if d15==.

/*
d17	1	De uso exclusivo del hogar
	2	Compartido con otro hogar
*/


gen banoex_ch=.
replace banoex_ch=1 if d17==1
replace banoex_ch=0 if d17==2

gen des1_ch=.

gen des2_ch=.

gen piso_ch=.

gen pared_ch=.

gen techo_ch=.

gen resid_ch=.

/*
d10	N°	Número de habitaciones para dormir
*/

gen dorm_ch=d10
replace dorm_ch=. if d10==9
sum dorm*
/*
d9	N°	Número de habitaciones residenciales
*/

gen cuartos_ch=d9
replace cuartos_ch=. if d9==99
sum cuartos*

/*
d20	1	SI, privado de este hogar
	2	SI, compartido con otros hogares
	3	NO hay
*/

gen cocina_ch=.
replace cocina_ch = 1 if d20==1 | d20==2 
replace cocina_ch = 1 if d20==0

/*
d21_3		1 = Si / 2 = No		Refrigerador (con o sin frezer)
d21_4		1 = Si / 2 = No		Freezer (solo)
d21_5_1		1 = Si / 2 = No		TV color
d21_6		1 = Si / 2 = No		Radio
d21_14_1 	1 = Si / 2 = No		Microcomputador (incluye laptop)
d21_15		1 = Si / 2 = No		Conexión a internet
d21_16_1 	1 = Si / 2 = No		Teléfono
d21_17_1 	1 = Si / 2 = No		Celular
d21_18_1 	1 = Si / 2 = No		Automóvil o camioneta
*/

gen refrig_ch=.
replace refrig_ch= 1 if d22_3 ==1
replace refrig_ch= 0 if d22_3 ==2

gen freezer_ch=.
replace freezer_ch= 1 if d22_4 ==1
replace freezer_ch= 0 if d22_4 ==2

gen auto_ch=.
replace auto_ch= 1 if d22_18_1 ==1
replace auto_ch= 0 if d22_18_1 ==2

gen telef_ch=.
replace telef_ch= 1 if d22_16_1 ==1
replace telef_ch= 0 if d22_16_1 ==2

gen compu_ch=.
replace compu_ch= 1 if d22_14_1 ==1
replace compu_ch= 0 if d22_14_1 ==2

gen internet_ch=.
replace internet_ch= 1 if d22_15_1 ==1 | d22_15_2 ==1
replace internet_ch= 0 if d22_15_1 ==2

gen cel_ch=.
replace cel_ch= 1 if d22_17_1 ==1
replace cel_ch= 0 if d22_17_1 ==2

/*
TENENCIA DE LA VIVIENDA		d8_1	1	Propietario de la vivienda y el terreno y la está pagando
					2	Propietario de la vivienda y el terreno y ya la pagó
					3	Propietario solamente de la vivienda y la está pagando
					4	Propietario solamente de la vivienda y ya la pagó
					5	Inquilino o arrendatario de la vivienda
					6	Ocupante con relación de dependencia
					7	Ocupante gratuito
					8	Ocupante sin permiso del propietario
				d8_2	$	Monto de la cuota de compra
				d8_3	$	Monto de la cuota de alquiler
*/

gen viv1_ch=.
gen viv2_ch=.

gen viviprop_ch=0 if d8_1==5
replace viviprop_ch=2 if d8_1==1 | d8_1==3
replace viviprop_ch=1 if d8_1==2 | d8_1==4
replace viviprop_ch=3 if d8_1==6 | d8_1==7 | d8_1==8 
gen vivialq_ch=d8_3
gen vivialqimp_ch=.


/************************************************************************/
/*				VARIABLES DEMOGRAFICAS			*/
/************************************************************************/


gen factor_ci=pesoano

/*
e27	1	Hombre
	2	Mujer
*/

gen sexo_ci=e27

/*
e28	Años	Años cumplidos
*/

gen edad_ci=e28
replace edad_ci=. if e28==99

/*
ESTADO CIVIL ACTUAL	e40	1	Divorciado/a
				2	Casado/a (incluye separado y aún no se divorció)
				3	Viudo/a
				4	Soltero/a
				5	Separado/a de unión libre
*/

gen civil_ci=1 		if e40==4
replace civil_ci=2 	if e40==2
replace civil_ci=3 	if e40==1 | e40==5
replace civil_ci=4 	if e40==3

/*
RELACION DE PARENTESCO	e32	1	Jefe
*/

gen jefe_ci=(e32==1)

sort idh_ch
by idh_ch: egen byte nconyuges_ch=sum(relacion_ci==2)
by idh_ch: egen byte nhijos_ch=sum(relacion_ci==3)
by idh_ch: egen byte notropari_ch=sum(relacion_ci==4)
by idh_ch: egen byte notronopari_ch=sum(relacion_ci==5)
by idh_ch: egen byte nempdom_ch=sum(relacion_ci==6)

gen byte clasehog_ch=0
replace clasehog_ch=1 if nhijos_ch==0 & nconyuges_ch==0 & notropari_ch==0 & notronopari_ch==0 /*Unipersonal*/
replace clasehog_ch=2 if nhijos_ch>0 & notropari_ch==0 & notronopari_ch==0 /*Nuclear (child with or without spouse but without other relatives)*/
replace clasehog_ch=2 if nhijos_ch==0 & nconyuges_ch>0 & notropari_ch==0 & notronopari_ch==0 /*Nuclear (spouse with or without children but without other relatives)*/
replace clasehog_ch=3 if notropari_ch>0 & notronopari_ch==0 /*Ampliado*/
replace clasehog_ch=4 if ((nconyuges_ch>0 | nhijos_ch>0 | notropari_ch>0) & (notronopari_ch>0))/*Compuesto (some relatives plus non relative)*/
replace clasehog_ch=5 if nhijos_ch==0 & nconyuges_ch==0 & notropari_ch==0 & notronopari_ch>0 /*Corresidente*/

sort idh_ch
by idh_ch: egen byte nmiembros_ch=sum(relacion_ci>0 & relacion_ci<=5) if relacion_ci~=6
by idh_ch: egen byte nmayor21_ch=sum((relacion_ci>0 & relacion_ci<=5) & (edad_ci>=21 & edad_ci<=98))
by idh_ch: egen byte nmenor21_ch=sum((relacion_ci>0 & relacion_ci<=5) & (edad_ci<21))
by idh_ch: egen byte nmayor65_ch=sum((relacion_ci>0 & relacion_ci<=5) & (edad_ci>=65))
by idh_ch: egen byte nmenor6_ch=sum((relacion_ci>0 & relacion_ci<=5) & (edad_ci<6))
by idh_ch: egen byte nmenor1_ch=sum((relacion_ci>0 & relacion_ci<=5) & (edad_ci<1))




/******************************/
/*VARIABLES DE DEMANDA LABORAL*/
/******************************/

/*
CONDICION DE ACTIVIDAD ECONOMICA	pobpcoac	1	Menor de 14 años
							2	Ocupados
							3	Desocupados por primera vez
							4	Desocupados propiamente dichos
							5	Desocupados en seguro de paro
							6	Inactivo, realiza quehaceres del hogar
							7	Inactivo, estudiante
							8	Inactivo, rentista
							9	Inactivo, pensionista
							10	Inactivo, jubilado
							11	Inactivo, otro
*/

gen emp_ci=(pobpcoac==2)

/*
TAREAS QUE PROPORCIONAN MAYORES INGRESOS	f67_1		Descripción
						f67-2	Cód.	Código
*/

***************************** REVISAR EL NUMERO DE DIGITOS


replace f73_2="." if f73_2=="X211"

destring f73_2, replace

gen ocupa_ci=.
replace ocupa_ci=1 if (f73_2>=210 & f73_2<=320 | f73_2>=2110 & f73_2<=3470) & emp_ci==1
replace ocupa_ci=2 if (f73_2>=130 & f73_2<=190 | f73_2>=1110 & f73_2<=1310) & emp_ci==1
replace ocupa_ci=3 if (f73_2>=4110 & f73_2<=4220 | f73_2>=410 & f73_2<=430) & emp_ci==1
replace ocupa_ci=4 if (f73_2>=5110 & f73_2<=5210) & emp_ci==1
replace ocupa_ci=5 if (f73_2==5220 | f73_2==5230) & emp_ci==1 /*Aunque no esta desagregado en la base, esta es la desagregación a tres digitos de la CIUO-88*/
replace ocupa_ci=6 if (f73_2>=6110 & f73_2<=6210) & emp_ci==1
replace ocupa_ci=7 if (f73_2>=7110 & f73_2<=8340) & emp_ci==1 /*Incluye artesanos y operarios en hilanderias*/
replace ocupa_ci=8 if (f73_2==110 | f73_2==120) & emp_ci==1
replace ocupa_ci=9 if f73_2>9000 & emp_ci==1

/*
A QUE SE DEDICA EL ESTABLECIMIENTO DONDE REALIZA LAS TAREAS	f68_1		Descripción
								f74_2	Cód.	Código
*/

replace f74_2="." if f74_2=="." | f74_2=="+512" | f74_2=="<" | f74_2=="X211" | f74_2=="15 4"

destring f74_2, replace

gen rama_ci=.
replace rama_ci=1 if f74_2>000 & f74_2<=500 & emp_ci==1
replace rama_ci=2 if f74_2>=1000 & f74_2<=1400 & emp_ci==1
replace rama_ci=3 if f74_2>=1500 & f74_2<=3600 & emp_ci==1
replace rama_ci=4 if (f74_2==4000 | f74_2==4100) & emp_ci==1
replace rama_ci=5 if f74_2==4500 & emp_ci==1
replace rama_ci=6 if f74_2>=5000 & f74_2<=5500 & emp_ci==1
replace rama_ci=7 if f74_2>=6000 & f74_2<=6400 & emp_ci==1
replace rama_ci=8 if f74_2>=6500 & f74_2<=7400 & emp_ci==1
replace rama_ci=9 if f74_2>=7500 & f74_2<=9900 & emp_ci==1

*********************************************************************

/*
HORAS TRABAJADAS POR SEMANA	f88	Nº	Número de horas trabajadas por semana

CUANTAS HORAS TRABAJA EN OTRAS OCUPACIONES	f101	Nº	Número de horas que trabaja
*/

*********************************** REVISAR LAS VARIABLES DE INGRESO****************************
gen horaspri_ci=f88
replace horaspri_ci=. if f88==99 | emp_ci==0
replace f101=. if f101==99

egen horastot_ci=rsum(horaspri_ci f101)
replace horastot_ci=. if f101==. & horaspri_ci==.



egen ylmpri_ci=rsum(g129_1 g129_2 g129_3 g129_4 g129_5 g129_6 g129_7 g129_8  g147 g148 g150 g151 g152) 

egen ylnmpri_ci=rsum(g130_4 g131_2 g132_3 g134_2 g135_2 g137_2 g149_2 g149_3 g149_4 g149_5 g149_6 g149_7) 

egen ylmsec_ci=rsum(g138_1 g138_2 g138_3 g138_4 g138_5 g138_6 g138_7)

egen ylnmsec_ci=rsum(g138_8 g139_4 g140_2 g141_3 g143_2 g144_2 g146_2)

gen ylmotros_ci=.

egen ylm_ci=rsum(ylmpri_ci ylmsec_ci) 
replace ylm_ci=. if ylmpri_ci==. & ylmsec_ci==.

egen ylnm_ci=rsum(ylnmpri_ci ylnmsec_ci) 
replace ylnm_ci=. if ylnmpri_ci==. & ylnmsec_ci==.

egen ynlm_ci = rsum(g153_1 g153_2 g153_3 g153_4 g153_5 g153_6 g153_7 g153_8 g153_9 g153_10 g153_11 g153_12 g153_13 g153_14 g153_15 g153_16 g153_17 g153_18 g153_19 g153_20 g153_21 g153_22 g153_23 g153_24 g153_25 g153_26 g155_4 g156_2 g156_3)

gen ynlnm_ci=.

sort idh_ch
* by idh_ch: egen nrylmpri_ch=max(nrylmpri_ci) if relacion_ci!=6
by idh_ch: egen ylm_ch=sum(ylm_ci)if relacion_ci!=6
by idh_ch: egen ylnm_ch=sum(ylnm_ci)if relacion_ci!=6
* gen ylmnr_ch=. if nrylmpri_ch==1
by idh_ch: egen ynlm_ch=sum(ynlm_ci) if relacion_ci!=6
gen ynlnm_ch=.
gen ylmhopri_ci=ylmpri_ci/(horaspri_ci*4.2)

replace ylmhopri_ci=. if ylmhopri_ci<=0
gen ylmho_ci=ylm_ci/(horastot_ci*4.2)
replace ylmho_ci=. if ylmho_ci<=0
gen rentaimp_ch=.
gen autocons_ch=.
gen autocons_ci=.

/*
egen remesas_ci=rsum(g5_2 g5_3 g5_4 g5_5 g5_6 g5_7 g5_8 g5_9 g5_10 g5_11)
replace remesas_ci=. if g5_2==. & g5_3==. & g5_4==. & g5_5==. & g5_6==. & g5_7==. & g5_8==. & g5_9==. & g5_10==. & g5_11==.

sort idh_ch
by idh_ch: egen remesas_ch=sum(remesas_ci)if relacion_ci!=6
*/

gen durades_ci=f116/4 if (pobpcoac==3|pobpcoac==4|pobpcoac==5) /*De los que estan desempleados cuanto hace (meses) que buscan*/
replace durades_ci=. if f116==99
gen antiguedad_ci=.


/******************************************************************************************/
/*					VARIABLES DEL MERCADO LABORAL			  */
/******************************************************************************************/

gen desemp1_ci=((pobpcoac==3|pobpcoac==4|pobpcoac==5) & f110==1)

gen desemp2_ci=(desemp1_ci==1 | f110==2 & f111==2 | f111==3)

gen desemp3_ci=(desemp2_ci==1 | (f112==1 | f116>=4))

gen pea1_ci=(emp_ci==1 | desemp1_ci==1)

gen pea2_ci=(emp_ci==1 | desemp2_ci==1)

gen pea3_ci=(emp_ci==1 | desemp3_ci==1)

gen desalent_ci=.
gen subemp_ci=(horastot_ci>=1 & horastot_ci<=30 & (f105>=1 & f105<=3))
replace subemp_ci=. if emp_ci==0

gen tiempoparc_ci=(horastot_ci>=1 & horastot_ci<=30 & f105==4)
replace tiempoparc_ci=. if emp_ci==0

/*
CATEGORÍA DE LA OCUPACIÓN	f75	1	Asalariado privado
					2	Asalariado público
					3	Miembro de cooperativa de producción
					4	Patrón
					5	Cuenta propia sin local o inversión
					6	Cuenta propia con local o inversión
					7	Miembro del hogar no remunerado
					8	Programa público de empleo
*/

gen categopri_ci=1 	if f75==4
replace categopri_ci=2 	if f75==5 | f75==6
replace categopri_ci=3 	if f75==1 | f75==2 | f75==3
replace categopri_ci=4 	if f75==7 
replace categopri_ci=. 	if emp_ci!=1

/*
CATEGORÍA DE LA OCUPACIÓN	f95	1	Asalariado privado
					2	Asalariado público
					3	Miembro de cooperativa de producción
					4	Patrón
					5	Cuenta propia sin local o inversión
					6	Cuenta propia con local o inversión
					7	Miembro del hogar no remunerado
*/


gen categosec_ci=1 if f95==4
replace categosec_ci=2 if f95==5 | f95==6
replace categosec_ci=3 if f95==1 | f95==2 | f95==3
replace categosec_ci=4 if f95==7 

gen contrato_ci=.

gen segsoc_ci=.

/*
TRABAJOS QUE TIENE	f72	Nº	Número de trabajos que tiene
*/

gen nempleos_ci=1 if f72==1
replace nempleos_ci=2 if f72>1

/*
TAMAÑO  DE LA EMPRESA 	f80	1	Una persona
				2	2 a 4 personas
				3	5 a 9 personas
				4	10 a 49  personas
				5	50 o más personas
*/


gen firmapeq_ci=.
replace firmapeq_ci=1 if emp_ci==1 & f80==1 | f80==2
replace firmapeq_ci=0 if emp_ci==1 & f80>2

/*
CATEGORÍA DE LA OCUPACIÓN	f75/f85		2	Asalariado público
*/

gen spublico_ci=(emp_ci==1 & f75==2)
replace spublico= 1 if f95==2
replace spublico =. if emp_ci==.


/******************************************************************************************/
/*						VARIABLES EDUCATIVAS			  */
/******************************************************************************************/


/*
NIVEL Y AÑO MAS ALTO ALCANZADO	
			e54_1_1	Año	Años cursados en Primaria
			e54_1_2	1 = Si / 2 = No	Finalizo o no el nivel
			e54_2_1	Año	Años cursados en Secundaria
			e54_2_2	1 = Si / 2 = No	Finalizo o no el nivel
			e54_3_1	Año	Años cursados en Enseñanza Técnica
			e54_3_2	1 = Si / 2 = No	Finalizo o no el nivel
			e54_3_3	1	Exigencia de enseñanza secundaria completa para realizar curso de UTU
				2	Exigencia de enseñanza secundaria primer ciclo para realizar curso de UTU
				3	Exigencia de enseñanza primaria completa para realizar curso de UTU
				4	Ninguna exigencia
			e52-4_1	Año	Magisterio o Profesorado
			e52-4_2	1 = Si / 2 = No	Finalizo o no el nivel
			e54_5-1	Año	Universidad o similar
			e54_5_2	1 = Si / 2 = No	Finalizo o no el nivel
			e54_6_1	Año	Terciario no universitario
			e54_6_2	1 = Si / 2 = No	Finalizo o no el nivel
			e52-7-1	Año	Posgrado (maestría o doctorado)
			e54_7_2	1 = Si / 2 = No	Finalizo o no el nivel
*/

tab e54_1_1	
tab e54_2_1	
tab e54_3_1	
tab e54_4_1	
tab e54_5_1
tab e54_6_1
tab e54_7_1	

gen dpri = 1 if (e54_1_1>0)
gen dsec = 1 if (e54_2_1>0)
gen dtec = 1 if (e54_3_1>0)
gen dmag = 1 if (e54_4_1>0)
gen duni = 1 if (e54_5_1>0)
gen dter = 1 if (e54_6_1>0)
gen dmae = 1 if (e54_7_1>0)

gen aedu_ci = .
replace aedu_ci = e54_1_1 		if dpri==1
replace aedu_ci = e54_2_1 + 6		if dsec==1
replace aedu_ci = e54_3_1 + 6		if dtec==1
replace aedu_ci = e54_4_1 + 6		if dmag==1
replace aedu_ci = e54_5_1 + 12		if duni==1
replace aedu_ci = e54_6_1 + 12		if dter==1
replace aedu_ci = e54_7_1 + 18		if dmae==1

replace aedu_ci = 0 if edad_ci>7 & aedu_ci == . 
replace aedu_ci = . if e53==0

**************
***eduno_ci***
**************

gen byte eduno_ci=(aedu_ci==0) 
replace eduno_ci=. if aedu_ci==.
label variable eduno_ci "Cero anios de educacion"

**************
***edupi_ci***
**************

gen byte edupi_ci=(aedu_ci>=1 & aedu_ci<6)
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
***eduuc_ci***
***************

gen byte eduuc_ci=(aedu_ci>=16) 
replace eduuc_ci=. if aedu_ci==.
label variable eduuc_ci "Universitaria completa o mas"

***************
***edupre_ci***
***************

gen edupre_ci=(e52_1>0)
replace edupre_ci=. if aedu_ci==.
label variable edupre_ci "Educacion preescolar"


gen eduac_ci=.

/*
ASISTENCIA ACTUAL A ESTABLECIMIENTO DE ENSEÑANZA	e50	1 = Si / 2 = No	
*/

gen asiste_ci=.
replace asiste_ci = 1 if (e50==1)
replace asiste_ci = 0 if (e50==2)


gen pqnoasist_ci=.

gen repite_ci=.

/*
ESTABLECIMIENTO PÚBLICO O PRIVADO	e51	1	Público
						2	Privado
*/

gen edupub_ci=.
replace edupub_ci = 1 if (e51==1)
replace edupub_ci = 0 if (e51==2)

label var  aedu_ci "Anios de Educacion"

save "${surveysFolder}\ARM\URU\ECH\\2007\Arm_data\URU2007EA_BID.dta", replace




	
