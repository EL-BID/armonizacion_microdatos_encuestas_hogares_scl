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

local PAIS CHL
local ENCUESTA CASEN
local ANO "1998"
local ronda m11_m12 

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
   



capture log close
log using "`log_file'", replace 

log off
/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Chile
Encuesta: CASEN
Round: Noviembre- Diciembre
Autores: 
Modificación 2014: Mayra Sáenz - Email: mayras@iadb.org - saenzmayra.a@gmail.com
Versión 2007: Victoria, Maria Fernanda Prada (MFP)
Última versión: María Laura Oliveri (MLO) - Email: mloliveri@iadb.org, lauraoliveri@yahoo.com
Fecha última modificación: 26 de Agosto de 2013

							SCL/LMK - IADB
****************************************************************************/
/***************************************************************************
Detalle de procesamientos o modificaciones anteriores:

****************************************************************************/


use `base_in', clear

		**********************************
		***VARIABLES DEL IDENTIFICACION***
		**********************************
		
	****************
	* region_BID_c *
	****************
	
gen region_BID_c=4


/***** revision July 29,2005  Suzanne

removed condition (& edad_ci<18) froom the following two lines:

by idh_ch: egen byte nhijos_ch=sum((relacion_ci==3) & edad_ci<18)
by idh_ch: egen byte notropari_ch=sum((relacion_ci==4) & edad_ci>=18)

******* revision June 8 2006 MFP
removed desemp1 and desemp2 because the reference period of 2 months implies that those variables
can't be created.
Now desemp3== old definition of desemp1

previous code:

gen desemp1_ci=(o1==2 & o2==2 & o3==1) *El periodo de referencia de la encuesta es de dos meses!
gen desemp2_ci=(desemp1_ci | (o1==2 & o2==2 & o3==2 & (o7==7)))
gen desemp3_ci=(desemp2_ci | (o4>8 & o4<=300))

***/

/*** revision October 16 2006 (Victoria)
The code for the education dummies was changed in order to make it
comparable with the following years and also to make the returns
to education coherent. 
Old code can be seen in the "VARIABLES EDUCATIVAS" sector

Also two new conditions were added to the creation of aedu_ci
*/

/*** revision October 23 2006 (Victoria)
Change the code for ynlm_ci that double counted some variables.
Old code can be seen in the "VARIABLES DE DEMANDA LABORAL" section
*/

/*** revision January 24 2007 (Ma Fda)
Change the code for firmapeq. It was created as tamfirma(1=more than 5 employees)
This change implied the respective change in sociometro's program (DONE!)
*/

/**** revision August 2007 (Victoria) ***

With the unification Sociometro/Equis we decided to add two new varibales: howner and floor.
This variables were already created for Atlas

gen howner=(viviprop_ch==1 | viviprop==2);
replace howner=. if viviprop_ch==.;
gen floor=(piso_ch==1);
replace floor=. if piso_ch==.;

Also, the orginal data was replaced with the new Mecovi versions
*****/

/******************
VARIABLES DEL HOGAR
*******************/
gen factor_ch=expr
egen idh_ch=group(r p comu z seg f)
gen idp_ci=o
gen zona_c=z
replace zona_c=0 if z==2
gen pais_c="CHL"
gen anio_c=1998
gen mes_c=.
gen relacion_ci=pco1
replace relacion_ci=4 if pco1>=4 & pco1<=10
replace relacion_ci=5 if pco1==11
replace relacion_ci=6 if pco1==12

/*************************************
VARIABLES DE INFRAESTRUCTURA DEL HOGAR
**************************************/
gen aguared_ch=(v13==1 | v13==2 | v13==3)
gen aguadist_ch=v14
gen aguamala_ch=(v13==5|v13==6)
gen aguamide_ch=(v13==1 |v13==2)
gen luz_ch=(v16<=5)
gen luzmide_ch=(v16==1 | v16==2)
replace luzmide_ch=. if luz_ch==0
gen combust_ch=.
gen bano_ch=((v11!=0 & v33==.) | v33>0 & v33<=3)
gen banoex_ch=(v33==. | v33<v11)
replace banoex_ch=. if bano_ch==0 
gen des1_ch=0 if bano_ch==0 | v15==7
replace des1_ch=1 if v15==1 | v15==2
replace des1_ch=2 if v15==3 | v15==4
replace des1_ch=3 if v15==5 | v15==6
gen des2_ch=des1_ch
replace des2_ch=. if des1_ch==3
gen piso_ch=0 if v19==5
replace piso_ch=1 if v19<5
gen pared_ch=0 if v17>=4 & v17<=7
replace pared_ch=1 if v17<4
replace pared_ch=2 if v17==8
gen techo_ch=0 if v21>=5
replace techo_ch=1 if v21<5
gen resid_ch=.

 **Daniela Zuluaga- Enero 2018: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**
	
 *********************
 ***aguamejorada_ch***
 *********************
g       aguamejorada_ch = 1 if (v13 >=1 & v13 <=4)
replace aguamejorada_ch = 0 if (v13 >=5 & v13 <=6) | v14 == 3

 *********************
 ***banomejorado_ch***
 *********************
g       banomejorado_ch = 1 if  (v15 >=1 & v15 <=4) 
replace banomejorado_ch = 0 if  (v15 >=5 & v15 <=7)

gen dorm_ch=v6 
egen piezaviv=rsum(v6 v7 v8 v9 v10 v11 v12), missing
replace piezaviv=. if  v6==. & v7==. & v8==. & v9==. & v10==. & v11==. & v12==.
egen piezahog=rsum(v28 v29 v30 v31 v32 v33 v34), missing
replace piezahog=. if v28==. & v29==. & v30==. & v31==. & v32==. & v33==. & v34==.
gen cuartos_ch=piezaviv 
gen cocina_ch=(v10!=0)
sort idh_ch
by idh_ch: egen telef_ch=sum(p4==1)
by idh_ch: egen refrig_ch=sum(p3==1)
gen freez_ch=.
by idh_ch: egen auto_ch=sum(p9==1)
gen compu_ch=.
gen internet_ch=.
gen cel_ch=.
gen vivi1_ch=1 if v23==1 | v23==2
replace vivi1_ch=2 if v23==3
replace vivi1_ch=3 if v23>3
gen vivi2_ch=(vivi1_ch==1 | vivi1_ch==2)
gen viviprop_ch=0 if v24==5
replace viviprop_ch=1 if v24==1 | v24==3
replace viviprop_ch=2 if v24==2 | v24==4
replace viviprop_ch=3 if v24==9
replace viviprop_ch=4 if v24>5 & v24<=8
gen vivitit_ch=.
gen vivialq_ch=.
gen vivialqimp_ch= yaimhaj

/* new variables August 2007 */

gen howner=(viviprop_ch==1 | viviprop==2)
replace howner=. if viviprop_ch==.
gen floor=(piso_ch==1)
replace floor=. if piso_ch==.



/*********************
VARIABLES DEMOGRAFICAS
*********************/
gen factor_ci=expr
gen sexo_ci=sexo
gen edad_ci=edad


gen civil_ci=1 if ecivil==7
replace civil_ci=2 if ecivil==1 | ecivil==2
replace civil_ci=3 if ecivil==3 | ecivil==4 | ecivil==5
replace civil_ci=4 if ecivil==6


gen jefe_ci=(relacion_ci==1)
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
by idh_ch:egen byte nmiembros_ch=sum(relacion_ci>0 & relacion_ci<5) if relacion_ci~=6
by idh_ch:egen byte nmayor21_ch=sum((relacion_ci>0 & relacion_ci<5) & (edad_ci>=21 & edad_ci<=98))
by idh_ch:egen byte nmenor21_ch=sum((relacion_ci>0 & relacion_ci<5) & (edad_ci<21))
by idh_ch:egen byte nmayor65_ch=sum((relacion_ci>0 & relacion_ci<5) & (edad_ci>=65))
by idh_ch:egen byte nmenor6_ch=sum((relacion_ci>0 & relacion_ci<5) & (edad_ci<6))
by idh_ch:egen byte nmenor1_ch=sum((relacion_ci>0 & relacion_ci<5) & (edad_ci<1))

gen miembros_ci=(relacion_ci<5)
label variable miembros_ci "Miembro del hogar"

/***************************
VARIABLES DE DEMANDA LABORAL
****************************/


****************
****condocup_ci*
****************

gen condocup_ci=.
replace condocup_ci=1 if (o1==1 | o2==1)
replace condocup_ci=2 if ((o1==2 | o2==2) & (o3==1))
recode condocup_ci (.=3) if edad_ci>=12 
replace condocup_ci=4 if edad<12
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

*gen emp_ci=(o1==1 | (o1==2 & o2==1))


gen ocupa_ci=.
replace ocupa_ci=1 if (o6>=2100 & o6<=3480) & emp_ci==1
replace ocupa_ci=2 if (o6>=1100 & o6<=1319) & emp_ci==1
replace ocupa_ci=3 if (o6>=4100 & o6<=4223) & emp_ci==1
replace ocupa_ci=4 if ((o6>=9100 & o6<=9113) | (o6>=5200 & o6<=5230)) & emp_ci==1
replace ocupa_ci=5 if ((o6>=5100 & o6<=5169) | (o6>=9100 & o6<=9162)) & emp_ci==1
replace ocupa_ci=6 if ((o6>=6100 & o6<=6210) | (o6>=9200 & o6<=9220)) & emp_ci==1
replace ocupa_ci=7 if ((o6>=7100 & o6<=8340) | (o6>=9300 & o6<=9333))  & emp_ci==1
replace ocupa_ci=8 if o6==110 & emp_ci==1
replace ocupa_ci=9 if o6==9999 & emp_ci==1

label variable ocupa_ci "Ocupacion laboral"
label define ocupa_ci 1"profesional y tecnico" 2"director o funcionario sup" 3"administrativo y nivel intermedio"
label define ocupa_ci  4 "comerciantes y vendedores" 5 "en servicios" 6 "trabajadores agricolas", add
label define ocupa_ci  7 "obreros no agricolas, conductores de maq y ss de transporte", add
label define ocupa_ci  8 "FFAA" 9 "Otras", add
label value ocupa_ci ocupa_ci


****************
***  rama_ci ***
****************
/*
gen rama_ci=substr(string(o7),1,1)
destring rama_ci, replace
replace rama_ci=. if emp_ci==0 | rama_ci<=0
*/

gen rama_ci=.
replace rama_ci=1 if (o7>=1110 & o7<=1400)   & emp_ci==1
replace rama_ci=2 if (o7>=2000 & o7<=2990) & emp_ci==1
replace rama_ci=3 if (o7>=3000 & o7<=3990) & emp_ci==1
replace rama_ci=4 if (o7>=4000 & o7<=4990) & emp_ci==1
replace rama_ci=5 if o7==5000 & emp_ci==1
replace rama_ci=6 if (o7>=6000 & o7<=6990) & emp_ci==1
replace rama_ci=7 if (o7>=7000 & o7<=7990) & emp_ci==1
replace rama_ci=8 if (o7>=8000 & o7<=8400) & emp_ci==1
replace rama_ci=9 if (o7>=9000  & o7<=9900) & emp_ci==1



gen horaspri_ci=o11
replace horaspri_ci=. if emp_ci==0 | o11==99

gen horastot_ci=horaspri_ci

gen ylmpri_ci=yopraj
replace ylmpri_ci=. if  emp_ci==0

gen ylnmpri_ci=.

gen ylmsec_ci=.
gen ylnmsec_ci=.
gen ylnmotros_ci=.
gen nrylmpri_ci=(emp_ci==1 & ylmpri_ci==.)
replace nrylmpri_ci=. if emp_ci==0

gen ylm_ci= ytrabaj 
/*La variable ytrabaj contiene:SALARIOS + bonificaciones, REMUNERACIONES EN ESPECIE, RETIROS DE
PRODUCTOS O MERCADERIAS, INGRESO POR OTROS TRABAJOS. Dado que es imposibles desagregarla, dejamos ylmpri_ci lo mas limpio 
posible y sumamos todas estas cosas al final, aun cuando una parte del mismo debiera ir a ylnm_ci. A partir del 92 ya se 
pueden realizar estas distinciones.*/
replace ylm_ci=. if ytrabaj==.

gen ylnm_ci=.

/*
OLD CODE:
egen ynlm_ci=rsum(yjubaj yautaj ymoneaj ypasaj ysufaj ycesaj yfamaj yosuaj ysubaj)
replace ynlm_ci=. if yjubaj==. & yautaj==. & ymoneaj==. & ypasaj==. & ysufaj==. & ycesaj==. & yfamaj==. & yosuaj==. & ysubaj==. 
*/

***NEW
gen yautaj1=yautaj
replace yautaj1=0 if yautaj==.
gen ytrabaj1=ytrabaj
replace ytrabaj1=0 if ytrabaj==.
gen ysubaj1=ysubaj
replace ysubaj1=0 if ysubaj==.

gen ynlm_ci = yautaj1 - ytrabaj1 + ysubaj1
replace ynlm_ci=. if yautaj==. & ytrabaj==. & ysubaj==. 
****


gen ynlnm_ci=.

sort idh_ch 
by idh_ch: egen nrylmpri_ch=max(nrylmpri_ci) if miembros_ci==1
by idh_ch: egen ylm_ch=sum(ylm_ci)if miembros_ci==1, missing
by idh_ch: egen ylnm_ch=sum(ylnm_ci)if miembros_ci==1, missing
gen ylmnr_ch=ylm_ch
replace ylmnr_ch=. if nrylmpri_ch==1
by idh_ch: egen ynlm_ch=sum(ynlm_ci)if miembros_ci==1, missing
gen ynlnm_ch=.
gen ylmhopri_ci=ylmpri_ci/(horaspri_ci*4.2)
replace ylmhopri_ci=. if ylmhopri_ci<=0
gen ylmho_ci=ylm_ci/(horastot_ci*4.3)
label var ylmho_ci "Salario monetario de todas las actividades" 

gen rentaimp_ch=yaimhaj
gen autocons_ch=.
gen remesas_ci=.
gen remesas_ch=.
gen durades_ci=.
gen antiguedad_ci=.

/****************************
VARIABLES DEL MERCADO LABORAL
*****************************/
/*gen desemp1_ci=.
/*El periodo de referencia de la encuesta es de dos meses!*/
gen desemp2_ci=.
gen desemp3_ci=(o1==2 & o2==2 & o3==1) 

gen pea1_ci=.
gen pea2_ci=.
gen pea3_ci=(emp_ci==1 | desemp3_ci==1)*/

gen desalent_ci=(o1==2 & o2==2 & o3==2 & o5==8)

gen subemp_ci=.
gen tiempoparc_ci=.
gen categopri_ci=.
replace categopri_ci=1 if o8==1
replace categopri_ci=2 if o8==2
replace categopri_ci=3 if o8>=3 & o8<=5
replace categopri_ci=4 if o8==6
replace categopri_ci=. if emp_ci==0
gen categosec_ci=.
/*
gen contrato_ci=(o9>=1 & o9<=3)
replace contrato_ci=. if emp_ci==0
gen segsoc_ci=(o15<=6)
replace segsoc_ci=. if emp_ci==0 /*Esta variable es solo para los empleados!!!: La pregunta 25 es para todas las personas, 
tengan empleo o no*/
*/
gen nempleos_ci=1 if o25==2
replace nempleos_ci=2 if o25==1
/*
gen firmapeq_ci=0 if o10==1 | o10==2
replace firmapeq_ci=1 if o10==3 | o10==4 | o10==5 | o10==6
replace firmapeq_ci=. if o10==7 | emp_ci==0
*cambio en 01/24/07*
ren firmapeq_ci tamfirma_ci
gen firmapeq_ci=1 if tamfirma_ci==0
replace firmapeq_ci=0 if tamfirma_ci==1
replace firmapeq_ci=. if emp_ci==0
drop tamfirma_ci*/

gen spublico_ci=.

/*******************
VARIABLES EDUCATIVAS
*******************/
*************
***aedu_ci*** 
************* 

gen aedu_ci=.
replace aedu_ci=0 if e6==0 | e6==1 | e6==16
replace aedu_ci=. if e6==4
*We assume that 'e6==4', Diferential Education, will be equivalent to missing
replace aedu_ci=e5 if e6==2 | e6==3
replace aedu_ci=0 if (e5>=8 & e6==2) | (e5==9 & e6==3)
replace aedu_ci=. if e5==10 & e6==2

*NEW: 16 Oct 2006 (Victoria)
replace aedu_ci=6 if (e5>=6 & e6==2) 
replace aedu_ci=8 if (e5>=8 & e6==3) 
*

replace aedu_ci=8+e5 if e6==6 | e6==8 /*nuevo"científica, humanista, media técnica prof*/
replace aedu_ci=6+e5 if e6==5 | e6==7  /*antiguo: humanidades o técnica*/ 
/*En Chile hubo un cambio en el sistema educativo, por el cual se paso de una primaria
(preparatoria) de 6 años, a una de 8. Por lo tanto, para todos los que contesten haber participado del sistema antiguo, se 
les imputan 6 años de educacion primaria en vez de 8 ==> Con la secundaria no hay problema, porque en el sistema antiguo se
compenzaba y duraba 6 años.*/
replace aedu_ci=12 if (e5>=5 & (e6>=5 & e6<=8))|(e5>=4 & (e6==5 | e6==7))
replace aedu_ci=8 if (e6>=5 & e6<=8) & (e5==9 | e5==10)
replace aedu_ci=12+e5 if e6>=9 & e6<=14 /* técnica superior completo o incompleto, profesional completo o incompleto */
replace aedu_ci=12 if (e6>=9 & e6<=14) & (e5==9 | e5==10)
replace aedu_ci=17+e5 if e6==15 /*posgrado*/
replace aedu_ci=. if e6==99 | e6==.
/*
OLD CODE:
gen eduno_ci=(aedu_ci==0)

gen edupi_ci=((e6==2 & (e5>=1 & e5<6)) |(e6==3 & (e5>=1 & e5<8)))
gen edupc_ci=((e6==3 & e5==8) |(e6==2 & e5==6))

gen edus1i_ci=(((e6==5 | e6==7) & (e5>=1 & e5<4)) | ((e6==6 | e6==8) & (e5>=1 & e5<2)))
gen edus1c_ci=(((e6==5 | e6==7) & e5==4) | ((e6==6 | e6==8) & e5==2))
gen edus2i_ci=(((e6==5 | e6==7) & (e5>4 & e5<6)) | ((e6==6 | e6==8) & (e5>2 & e5<4)) | ((e6>=5 & e6<=8) & (e5==9 | e5==10)))
gen edus2c_ci=((e6==5 | e6==7) & (e5==6)) | ((e6==6 | e6==8) & (e5==4 | e5==5))

gen edusi_ci=(edus1i_ci==1 | edus1c_ci==1 | edus2i_ci==1)
gen edusc_ci=edus2c_ci
gen eduui_ci=(e6==9 | e6==11 | e6==13)
gen eduuc_ci=(e6==10 | e6==12 | e6==14 | e6==15)
*/

***************
***asiste_ci***
***************

gen asiste_ci=(e2==1)
label variable asiste_ci "Asiste actualmente a la escuela"

*****************
***pqnoasis_ci***
*****************
*Modificado Mayra Sáenz Junio, 2016: antes se generaba como missing
gen pqnoasis_ci= e3

**************
*pqnoasis1_ci*
**************
**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**

g       pqnoasis1_ci = 1 if e3 ==4
replace pqnoasis1_ci = 2 if e3 ==5 | e3 ==17
replace pqnoasis1_ci = 3 if e3 ==7 | e3 ==14 | e3 ==15
replace pqnoasis1_ci = 4 if e3 ==9
replace pqnoasis1_ci = 5 if e3 ==6  | e3 ==8
replace pqnoasis1_ci = 7 if e3 ==10 | e3 ==11
replace pqnoasis1_ci = 8 if e3 ==1  | e3 ==2  | e3 ==3
replace pqnoasis1_ci = 9 if e3 ==12 | e3 ==13 | e3 ==16 | e3 ==18 | e3 ==19

label define pqnoasis1_ci 1 "Problemas económicos" 2 "Por trabajo" 3 "Problemas familiares o de salud" 4 "Falta de interés" 5	"Quehaceres domésticos/embarazo/cuidado de niños/as" 6 "Terminó sus estudios" 7	"Edad" 8 "Problemas de acceso"  9 "Otros"
label value  pqnoasis1_ci pqnoasis1_ci

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
replace edusc_ci=1  if aedu_ci==13 & e6==8 // hay casos de estudiantes tecnicos secundarios con 13 anios de educacion. no corresponde a terciario, asi que los dejo aca.
replace edusc_ci=. if aedu_ci==.
label variable edusc_ci "Secundaria completa"

***************
***edus1i_ci***
***************

gen byte edus1i_ci=0
replace edus1i_ci=1 if aedu_ci>6 & aedu_ci<8 
replace edus1i_ci=. if aedu_ci==.
label variable edus1i_ci "1er ciclo de la secundaria incompleto"

***************
***edus1c_ci***
***************

gen byte edus1c_ci=0
replace edus1c_ci=1 if aedu_ci==8
replace edus1c_ci=. if aedu_ci==.
label variable edus1c_ci "1er ciclo de la secundaria completo"

***************
***edus2i_ci***
***************

gen byte edus2i_ci=0
replace edus2i_ci=1 if aedu_ci>8 & aedu_ci<12
replace edus2i_ci=. if aedu_ci==.
label variable edus2i_ci "2do ciclo de la secundaria incompleto"

***************
***edus2c_ci***
***************

gen byte edus2c_ci=0
replace edus2c_ci=1 if aedu_ci==12
replace edusc_ci=1  if aedu_ci==13 & e6==8 // hay casos de estudiantes tecnicos secundarios con 13 anios de educacion. no corresponde a terciario, asi que los dejo aca.
replace edus2c_ci=. if aedu_ci==.
label variable edus2c_ci "2do ciclo de la secundaria completo"

**************
***eduui_ci***
**************

gen byte eduui_ci=(aedu_ci>12 & e6==9)  | (aedu_ci>12 & e6==11)  // mas de 12 anios y tecnico superior completo o profesional incompleto 
replace eduui_ci=0 if aedu_ci==13 & e6==8 // hay casos de estudiantes tecnicos secundarios con 13 anios de educacion. no corresponde a terciario, asi que aquí ponemos 0.
replace eduui_ci=. if aedu_ci==.
label variable eduui_ci "Universitaria incompleta"

***************
***eduuc_ci****
***************

gen byte eduuc_ci=0
replace eduuc_ci=1 if aedu_ci>12 & (e6==10 | e6==12 | e6==14 | e6==15)
replace eduuc_ci=. if aedu_ci==.
label variable eduuc_ci "Universitaria incompleta o mas"


***************
***edupre_ci***
***************

gen edupre_ci=(e6==1)
replace edupre_ci=. if e6 == . | e6 == 99
label variable edupre_ci "Educacion preescolar"


**************
***eduac_ci***
**************
gen eduac_ci=.
replace eduac_ci=0 if e6>=9 & e6<=12
replace eduac_ci=1 if e6==13 & e6<=15
label variable eduac_ci "Superior universitario vs superior no universitario"




foreach var of varlist edu* {
replace `var'=. if  aedu_ci==.
}


gen ban_aedu=aedu_ci
replace ban_aedu=aedu_ci-1 if asiste_ci==1 & aedu!=0 & aedu!=.
gen repite_ci=.
gen repiteult_ci=.

gen edupub_ci=.


/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/


*********
*lp_ci***
*********

gen lp_ci =.
replace lp_ci= 37889  if zona_c==1  /*urbana*/
replace lp_ci= 25546  if zona_c==0	/*rural*/
label var lp_ci "Linea de pobreza oficial del pais"

*********
*lpe_ci***
*********

gen lpe_ci =.
replace lpe_ci= 18944     if zona_c==1  /*urbana*/
replace lpe_ci= 14598     if zona_c==0	/*rural*/
label var lpe_ci "Linea de indigencia oficial del pais"

****************
*cotizando_ci***
****************
gen cotizando_ci=.
replace cotizando_ci=1 if (o15 >= 1 & o15 <= 6)
recode cotizando_ci .=0 if o21==1 | o21==2
label var cotizando_ci "Cotizante a la Seguridad Social"

****************
*afiliado_ci****
****************
gen afiliado_ci=.	
replace afiliado_ci=1 if (o27 >= 1 & o27 <= 6)
recode afiliado_ci .=0 
label var afiliado_ci "Afiliado a la Seguridad Social"


****************
*tipopen_ci*****
****************

gen tipopen_ci=.

* no esta la variable
label define  t 1 "Jubilacion" 2 "Pension invalidez" 3 "Pension viudez" 12 " Jub y inv" 13 "Jub y viud" 23 "Viud e inv"  123 "Todas"
label value tipopen_ci t

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
replace instcot_ci=o15 if o15<=6
label var instcot_ci "Institucion a la que cotiza - variable original de cada pais" 


*****************
*tipocontrato_ci*
*****************
/*
gen tipocontrato_ci=.
replace tipocontrato_ci=1 if o9==1 & categopri_ci==3
replace tipocontrato_ci=2 if o9==2 & categopri_ci==3
replace tipocontrato_ci=3 if o9==4 & categopri_ci==3
label var tipocontrato_ci "Tipo de contrato segun su duracion"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci
*/
	
* Corregido por la variable de firmo o no firmo y no por el tipo de trabajo MGD 06/16/2014	
gen tipocontrato_ci=.
replace tipocontrato_ci=1 if o9==1 & categopri_ci==3
replace tipocontrato_ci=2 if (o9==2 | o9==3) & categopri_ci==3
replace tipocontrato_ci=3 if (o9>=4 | tipocontrato_ci==.) & categopri_ci==3
label var tipocontrato_ci "Tipo de contrato segun su duracion"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci
	
*************
*cesante_ci* 
*************
gen cesante_ci=1 if o4==1
replace cesante_ci=0 if o4==2
label var cesante_ci "Desocupado - definicion oficial del pais"	

**************
***tamemp_ci**
**************

gen tamemp_ci=1 if o10==1 | o10==2 
replace tamemp_ci=2 if o10==3 | o10==4
replace tamemp_ci=3 if o10==5 | o10==5

label var tamemp_ci "# empleados en la empresa segun rangos"
label define tamemp_ci 1 "Pequena" 2 "Mediana" 3 "Grande"
label value tamemp_ci tamemp_ci


*************
**pension_ci*
*************
*egen auxpen=rsum(yjubaj)
*MLO: estas variables vienen en el modulo de ignresos complementario ajustado por CEPAL
egen auxpen=rsum(yjubaj yinvaj ymonaj yvitaj yorfaj yotpaj), missing
gen pension_ci=1 if auxpen>0 & auxpen!=.
recode pension_ci .=0 
label var pension_ci "1=Recibe pension contributiva"

*************
**ypen_ci*
*************

gen ypen_ci=auxpen
replace ypen_ci=. if auxpen<0
drop auxpen
label var ypen_ci "Valor de la pension contributiva"

***************
*pensionsub_ci*
***************
egen auxpens=rsum(ypa1aj ypa2aj ypa3aj), missing
gen pensionsub_ci=1 if auxpens>0 & auxpens!=.
recode pensionsub_ci .=0 
label var pensionsub_ci "1=recibe pension subsidiada / no contributiva"

*****************
**ypensub_ci*
*****************
destring auxpens, replace
gen ypensub_ci=auxpens
replace ypensub_ci=. if auxpens<0
drop auxpens
label var ypensub_ci "Valor de la pension subsidiada / no contributiva"

**********
**tc_ci***
**********
gen tc_ci=271.65

label var tc_ci "Tipo de cambio LCU/USD"

*************
**salmm_ci***
*************
* CHL 1998
gen salmm_ci= 	80500

label var salmm_ci "Salario minimo legal"

*************
***tecnica_ci**
*************

gen tecnica_ci=.
replace tecnica_ci=1 if e6==9 | e6==10
recode tecnica_ci .=0 
label var tecnica_ci "1=formacion terciaria tecnica"


****************
**categoinac_ci*
****************

gen categoinac_ci=1 if o5==5
replace categoinac_ci=2 if o5==4
replace categoinac_ci=3 if o5==1
replace categoinac_ci=4 if o5==2 | o5==3 | o5==6 | o5==7 | o5==8 | o5==9 | o5==10


label var categoinac_ci "Condición de inactividad"
	label define categoinac_ci 1 "jubilado/pensionado" 2 "estudiante" 3 "quehaceres_domesticos" 4 "otros_inactivos" 
	label value categoinac_ci categoinac_ci
	
***************
***formal_ci***
***************

gen byte formal_ci=1 if cotizando_ci==1 & (condocup_ci==1 | condocup_ci==2)
recode formal_ci .=0 if (condocup_ci==1 | condocup_ci==2)
label var formal_ci "1=afiliado o cotizante / PEA"

*************************
*** VARIABLES DE RAZA ***
*************************

* MGR Oct. 2015: modificaciones realizadas en base a metodología enviada por SCL/GDI Maria Olga Peña

gen raza_idioma_ci = . 
gen id_ind_ci = .
gen id_afro_ci = .
gen raza_ci=.

* variables que faltan crear
gen ylmotros_ci=.
gen tcylmpri_ci =.
gen tcylmpri_ch =.
gen autocons_ci=.

gen region_c=.

******************************
*** VARIABLES DE GDI *********
******************************
	***************
	***afroind_ci***
	***************
* no hay variable 
gen afroind_ci=. 
	
	*******************
	***afroind_ano_c***
	*******************
gen afroind_ano_c=2000

	
	/***************************
     * DISCAPACIDAD
    ***************************/
	
gen dis_ci=. 
lab def dis_ci  1 "Con Discapacidad" 0 "Sin Discapacidad"
lab val dis_ci dis_ci
label var dis_ci "Personas con discapacidad"

	*******************
	***dis_ch***
	*******************
gen dis_ch=. 

	**************************
	** REGIONES **************
	**************************

   gen ine01=.   
   replace ine01=1 if  r==1				/*Arica y Parinacota*/
   replace ine01=2 if  r==2				/*Antofagasta*/
   replace ine01=3 if  r==3				/*Atacama*/
   replace ine01=4 if  r==4				/*Coquimbo*/
   replace ine01=5 if  r==5		    	/*Valparaíso*/
   replace ine01=6 if  r==6				/*O'Higgins*/
   replace ine01=7 if  r==7				/*Maule*/
   replace ine01=8 if  r==8				/*Bío Bío*/
   replace ine01=9 if  r==9				/*La Araucanía*/
   replace ine01=10 if r==10			/*Los Lagos*/
   replace ine01=11 if r==11			/*Aysén*/
   replace ine01=12 if r==12			/*Magallanes y Antártica Chilena*/
   replace ine01=13 if r==13			/*Metropolitana Santiago*/

	label define ine01 1"Arica y Parinacota" 2"Antofagasta" 3"Atacama" 4"Coquimbo" 5"Valparaíso" 6"O'Higgins" 7"Maule" 8"Bío Bío" 9"La Araucanía" 10"Los Lagos" 11"Aysén" 12"Magallanes y Antártica Chilena" 13"Metropolitana Santiago"
	label value ine01 ine01
	label var ine01 " Primera division politico-administrativa, región"
	
	**************************
	** PROVINCIAS ************
	**************************
gen ine02 = . 

	*******************
	*** SALUD  ***
	*******************
*******************
*** cobsalud_ci ***
*******************

gen cobsalud_ci=.
replace cobsalud_ci=1 if ((s1>=0 & s1<7) | s1==8) 
replace cobsalud_ci=0 if s1==7

label var cobsalud_ci "Tiene cobertura de salud"
label define cobsalud_ci 0 "No" 1 "Si" 
label value cobsalud_ci cobsalud_ci

************************
*** tipocobsalud_ci  ***
************************

gen tipocobsalud_ci=1 if s1>=0 & s1<=5
replace tipocobsalud_ci=2 if s1==6
replace tipocobsalud_ci=3 if s1==8
replace tipocobsalud_ci=0 if cobsalud==0
replace tipocobsalud_ci=. if s1==9

label var tipocobsalud_ci "Tipo cobertura de salud"
lab def tipocobsalud_ci 0"Sin cobertura" 1"Publico" 2"Privado" 3"otro" 
lab val tipocobsalud_ci tipocobsalud_ci


*********************
*** probsalud_ci  ***
*********************
* Nota: se pregunta si tuvieron problemas de salud en últimos 30 días.
 
gen probsalud_ci=1 if  s16==1 
replace probsalud_ci=0 if s16==2
replace probsalud_ci=. if s16==.

label var probsalud_ci "Tuvo algún problema de salud en los ultimos días"
lab def probsalud_ci 0 "No" 1 "Si"
lab val probsalud_ci probsalud_ci

*********************
*** distancia_ci  ***
*********************
gen distancia_ci=.

label var distancia_ci "Dificultad de acceso a salud por distancia"
lab def distancia_ci 0 "No" 1 "Si"
lab val distancia_ci distancia_ci

*****************
*** costo_ci  ***
*****************
* reporta que no tuvo consulta por costo
gen costo_ci=.
replace costo_ci=0 if s22!=3 
replace costo_ci=1 if s22==3 
replace costo_ci=. if s22==9

label var costo_ci "Dificultad de acceso a salud por costo"
lab def costo_ci 0 "No" 1 "Si"
lab val costo_ci costo_ci

********************
*** atencion_ci  ***
********************
gen atencion_ci=.
replace atencion_ci=0 if s22!=6
replace atencion_ci=1 if s22==6
replace atencion_ci=. if s22==9

label var atencion_ci "Dificultad de acceso a salud por problemas de atencion"
lab def atencion_ci 0 "No" 1 "Si"
lab val atencion_ci atencion_ci
	
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
	*** migrantiguo5_ci ***
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
* Consumidor (2011=100), Paridad de Poder Adquisitivo (PPA 2011),  líneas de pobreza
/*_____________________________________________________________________________________________________*/


do "$gitFolder\armonizacion_microdatos_encuestas_hogares_scl\_DOCS\\Labels&ExternalVars_Harmonized_DataBank.do"

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


/*Homologar nombre del identificador de ocupaciones (isco, ciuo, etc.) y de industrias y dejarlo en base armonizada 
para análisis de trends (en el marco de estudios sobre el futuro del trabajo) */
rename o7 codindustria
rename o6 codocupa

compress

saveold "`base_out'", replace


log close

