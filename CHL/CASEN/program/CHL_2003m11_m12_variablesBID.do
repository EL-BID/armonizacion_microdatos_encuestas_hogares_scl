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
local ANO "2003"
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
Versión 2007: Victoria
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
Also ylm_ci changed in order to make it comparable with previous years.
Old code can be seen in the "VARIABLES DE DEMANDA LABORAL" section
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
gen factor_ch=expr /*Esta es la expansion que se usa en todos los años anteriores. La provincial recien aparece en el 2000*/
egen idh_ch=group( r z seg f)
gen idp_ci=o
gen zona_c=z
replace zona_c=0 if z==2
gen pais_c="CHL"
gen anio_c=2003
gen mes_c=11
gen relacion_ci=pco1
replace relacion_ci=4 if pco1>=4 & pco1<=10
replace relacion_ci=5 if pco1==11
replace relacion_ci=6 if pco1==12

/*************************************
VARIABLES DE INFRAESTRUCTURA DEL HOGAR
**************************************/
gen aguared_ch=(v4==1 | v4==2 | v4==3)
replace aguared_ch=. if v4==.
gen aguadist_ch=v5
replace aguadist_ch=. if v5==9
gen aguamala_ch=(v4>=5 & v4<=6)
replace aguamala_ch=. if v4==9
gen aguamide_ch=(v4==1 |v4==2)
replace aguamide_ch=. if aguared_ch==.
gen luz_ch=(v7<=6)
gen luzmide_ch=(v7==1 | v7==2)
replace luzmide_ch=. if luz_ch==0
gen combust_ch=.
gen bano_ch=((v3g!=0 & v16g==.) | v16g>0 & v16g<=3)
gen banoex_ch=(v16g==. | v16g<v3g)
replace banoex_ch=. if bano_ch==0 
gen des1_ch=0 if bano_ch==0 | v6==7
replace des1_ch=. if v6==9
replace des1_ch=1 if v6==1 | v6==2
replace des1_ch=2 if v6==3 | v6==4
replace des1_ch=3 if v6==5 | v6==6
gen des2_ch=des1_ch
replace des2_ch=. if des1_ch==3
gen piso_ch=0 if v9a==5
replace piso_ch=1 if v9a<5
gen pared_ch=0 if v8a>=4 & v8a<=7
replace pared_ch=1 if v8a<4
replace pared_ch=2 if v8a==8
replace pared_ch=. if v8a==9
gen techo_ch=0 if v10a>=4 & v10a<=6
replace techo_ch=1 if v10a<4
replace techo_ch=. if v10a==9
gen resid_ch=.

 **Daniela Zuluaga- Enero 2018: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**
	
 *********************
 ***aguamejorada_ch***
 *********************
g       aguamejorada_ch = 1 if (v4 >=1 & v4 <=4)
replace aguamejorada_ch = 0 if (v4 >=5 & v4 <=6) | v5==3

 *********************
 ***banomejorado_ch***
 *********************
g       banomejorado_ch = 1 if  (v6 >=1 & v6 <=4) 
replace banomejorado_ch = 0 if  (v6 >=5 & v6 <=7)


gen dorm_ch=v3a 
egen piezaviv=rsum(v3a v3b v3c v3d v3e v3f v3g), missing
replace piezaviv=. if v3a==. & v3b==. & v3c==. & v3d==. & v3e==. & v3f==. & v3g==. 
egen piezahog=rsum(v16a v16b v16c v16d v16e v16f v16g), missing
replace piezahog=. if v16a==. & v16b==. & v16c==. & v16d==. & v16e==. & v16f==. & v16g==. 
gen cuartos_ch=piezaviv 
gen cocina_ch=(v3f!=0)
sort idh_ch
by idh_ch: egen telef_ch=sum(r10c==1)
replace telef_ch=1 if telef_ch>=1
by idh_ch: egen refrig_ch=sum(r10b==1)
replace refrig_ch=1 if refrig_ch>=1
gen freez_ch=.
gen auto_ch=.
bys idh_ch: egen compu_ch=sum(r10f==1)
replace compu_ch=1 if compu_ch>=1
by idh_ch: egen internet_ch=sum(r10g==1 | r10h==1)
replace internet_ch=1 if internet_ch>=1
by idh_ch: egen cel_ch=sum(r11==1 | r11==2)
replace cel_ch=1 if cel_ch>=1
gen vivi1_ch=1 if v11==1 | v11==2
replace vivi1_ch=2 if v11==3
replace vivi1_ch=3 if v11>3
gen vivi2_ch=(vivi1_ch==1 | vivi1_ch==2)
gen viviprop_ch=0 if v12==5 | v12==6
replace viviprop_ch=1 if v12==1 | v12==3
replace viviprop_ch=2 if v12==2 | v12==4
replace viviprop_ch=3 if v12==10
replace viviprop_ch=4 if v12>6 & v12<=9
gen vivitit_ch=.
gen vivialq_ch=.
gen vivialqimp_ch=yaimhaj

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
replace civil_ci=. if ecivil==9
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


	***************
	***upm_ci***
	***************
gen upm_ci=. 

	***************
	***estrato_ci**
	***************

clonevar estrato_ci=estrato
label variable estrato_ci "Estrato"


          ******************************
          *** VARIABLES DE DIVERSIDAD **
          ******************************
*Nathalia Maya & Antonella Pereira
*Julio 2021	

	***************
	***afroind_ci***
	***************
**Pregunta: Pueblos indígenas, pertenece usted o es descendiente de alguno de ellos? (r25) (Aimara 1; Rapa-Nui 2; Quechua 3; Mapuche 4; Atacameño 5; Coya 6; Kawashkar 7; Yagán 8; No pertenece a ningún pueblo indígena 0; sin dato 9)
gen afroind_ci=. 
replace afroind_ci=1 if (r25 >=1 & r25 <=8 )
replace afroind_ci=3 if r25==0
replace afroind_ci=. if r25==9

	***************
	***afroind_ch***
	***************
gen afroind_jefe= afroind_ci if relacion_ci==1
egen afroind_ch  = min(afroind_jefe), by(idh_ch) 
drop afroind_jefe

	*******************
	***afroind_ano_c***
	*******************
gen afroind_ano_c=2000

	*******************
	***dis_ci***
	*******************
gen dis_ci=. 

	*******************
	***dis_ch***
	*******************
gen dis_ch=. 



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
replace ocupa_ci=1 if (o7>=2100 & o7<=3480) & emp_ci==1
replace ocupa_ci=2 if (o7>=1100 & o7<=1319) & emp_ci==1
replace ocupa_ci=3 if (o7>=4100 & o7<=4223) & emp_ci==1
replace ocupa_ci=4 if ((o7>=9100 & o7<=9113) | (o7>=5200 & o7<=5230)) & emp_ci==1
replace ocupa_ci=5 if ((o7>=5100 & o7<=5169) | (o7>=9100 & o7<=9162)) & emp_ci==1
replace ocupa_ci=6 if ((o7>=6100 & o7<=6210) | (o7>=9200 & o7<=9220)) & emp_ci==1
replace ocupa_ci=7 if ((o7>=7100 & o7<=8340) | (o7>=9300 & o7<=9333))  & emp_ci==1
replace ocupa_ci=8 if o7==110 & emp_ci==1
replace ocupa_ci=9 if o7==9999 & emp_ci==1


label variable ocupa_ci "Ocupacion laboral"
label define ocupa_ci 1"profesional y tecnico" 2"director o funcionario sup" 3"administrativo y nivel intermedio"
label define ocupa_ci  4 "comerciantes y vendedores" 5 "en servicios" 6 "trabajadores agricolas", add
label define ocupa_ci  7 "obreros no agricolas, conductores de maq y ss de transporte", add
label define ocupa_ci  8 "FFAA" 9 "Otras", add
label value ocupa_ci ocupa_ci

*************
** rama_ci***
*************
/*
gen rama_ci=substr(string(o8),1,1)
destring rama_ci, replace
replace rama_ci=. if emp_ci==0 | rama_ci<=0
*/
gen rama_ci=.
replace rama_ci=1 if (o8>=1000 & o8<=1499) & emp_ci==1
replace rama_ci=2 if (o8>=2000 & o8<=2999) & emp_ci==1
replace rama_ci=3 if (o8>=3000 & o8<=3999) & emp_ci==1
replace rama_ci=4 if (o8>=4000 & o8<=4999) & emp_ci==1
replace rama_ci=5 if (o8>=5000 & o8<=5999) & emp_ci==1
replace rama_ci=6 if (o8>=6000 & o8<=6999) & emp_ci==1
replace rama_ci=7 if (o8>=7000 & o8<=7999) & emp_ci==1
replace rama_ci=8 if (o8>=8000 & o8<=8999) & emp_ci==1
replace rama_ci=9 if (o8>=9000 & o8<=9990) & emp_ci==1


/*************************************************************************************************************
horaspri_ci
-----------
==> Pareciera ser que la gente esta contestando horas mensuales:
. su o19_hrs

    Variable |       Obs        Mean    Std. Dev.       Min        Max
-------------+--------------------------------------------------------
     o19_hrs |     86077    181.4701    66.04026          1        540


Por lo tanto, para crear las horas en el trabajo principal, dividimos las horas mensauales por 22!:

. gen hora=o19_hrs/22
. su hora

    Variable |       Obs        Mean    Std. Dev.       Min        Max
-------------+--------------------------------------------------------
        hora |     86077    8.248643     3.00183   .0454545   24.54545


==> La otra opcion, es dividirlos por la cantidad de dias que declaran haber trabajado para ganar ese sueldo:

. gen hora=o19_hrs/o19_dia
(171063 missing values generated)

. su hora

    Variable |       Obs        Mean    Std. Dev.       Min        Max
-------------+--------------------------------------------------------
        hora |     86014    8.082385    2.140593   .1428571         98

*************************************************************************************************************/

gen horaspri_ci=o19_hrs/4 /*Da mas bajo que en los otros años, porque la creación de esta variable es muy mala...*/

gen horastot_ci=horaspri_ci


gen ylmpri_ci=yopraj 
replace ylmpri_ci=. if  emp_ci==0

/*********
ylmpri1_ci
*********/

/*
gen aux1=.
replace aux1=yextaj*22 if (o20_t1==1 & o20_p1==1) | (o20_t2==1 & o20_p2==1)
replace aux1=yextaj*4 if o20_t1==1 & o20_p1==2 | (o20_t2==1 & o20_p2==2)
replace aux1=yextaj*2 if o20_t1==1 & o20_p1==3 | (o20_t2==1 & o20_p2==3)
replace aux1=yextaj if o20_t1==1 & o20_p1==4 | (o20_t2==1 & o20_p2==4)
replace aux1=yextaj/2 if o20_t1==1 & o20_p1==5 | (o20_t2==1 & o20_p2==5)
replace aux1=yextaj/3 if o20_t1==1 & o20_p1==6 | (o20_t2==1 & o20_p2==6)
replace aux1=yextaj/4 if o20_t1==1 & o20_p1==7 | (o20_t2==1 & o20_p2==7)
replace aux1=yextaj/6 if o20_t1==1 & o20_p1==8 | (o20_t2==1 & o20_p2==8)
replace aux1=yextaj/12 if o20_t1==1 & o20_p1==9 | (o20_t2==1 & o20_p2==9)
replace aux1=. if yextaj==. | yextaj==9999999

egen ylmpri1_ci=rsum(ylmpri_ci aux1) 
*Este tipo de ingreso tiene salario mas bonificaciones
replace ylmpri1_ci=. if (ylmpri_ci==. & aux1==.) | emp_ci==0
drop aux1 
*/

*********
*ylmpri2_ci: Este además, incluye bonificaciones y ganancias anuales
*********
/*
gen aux1=yextaj/12 if o20_t1!=1 & o20_t2!=1 & o21_t==1 
gen aux2=yvpaaj/12 if o21_t==2
egen ylmpri2_ci=rsum(ylmpri1_ci aux1 aux2)
replace ylmpri2_ci=. if ylmpri1_ci==. & aux1==. & aux2==.
drop aux1 aux2
*/

/*********
ylnmpri_ci
**********/

/*
gen aux1=.
replace aux1=yespaj*22 if (o20_t1==2 & o20_p1==1) | (o20_t2==2 & o20_p2==1)
replace aux1=yespaj*4 if (o20_t1==2 & o20_p1==2) | (o20_t2==2 & o20_p2==2)
replace aux1=yespaj*2 if (o20_t1==2 & o20_p1==3) | (o20_t2==2 & o20_p2==3)
replace aux1=yespaj if (o20_t1==2 & o20_p1==4) | (o20_t2==2 & o20_p2==4)
replace aux1=yespaj/2 if (o20_t1==2 & o20_p1==5) | (o20_t2==2 & o20_p2==5)
replace aux1=yespaj/3 if (o20_t1==2 & o20_p1==6) | (o20_t2==2 & o20_p2==6)
replace aux1=yespaj/4 if (o20_t1==2 & o20_p1==7) | (o20_t2==2 & o20_p2==7)
replace aux1=yespaj/6 if (o20_t1==2 & o20_p1==8) | (o20_t2==2 & o20_p2==8)
replace aux1=yespaj/12 if (o20_t1==2  & o20_p1==9) | (o20_t2==2 & o20_p2==9)
replace aux1=. if yespaj==. | yespaj==9999999

gen autoc=.
replace autoc=yac1aj*22 if (o20_t1==3 & o20_p1==1) | (o20_t2==3 & o20_p2==1)
replace autoc=yac1aj*4 if (o20_t1==3 & o20_p1==2) | (o20_t2==3 & o20_p2==2)
replace autoc=yac1aj*2 if (o20_t1==3  & o20_p1==3) | (o20_t2==3 & o20_p2==3)
replace autoc=yac1aj if (o20_t1==3  & o20_p1==4) | (o20_t2==3 & o20_p2==4)
replace autoc=yac1aj/2 if (o20_t1==3  & o20_p1==5) | (o20_t2==3 & o20_p2==5)
replace autoc=yac1aj/3 if (o20_t1==3  & o20_p1==6) | (o20_t2==3 & o20_p2==6)
replace autoc=yac1aj/4 if (o20_t1==3  & o20_p1==7) | (o20_t2==3 & o20_p2==7)
replace autoc=yac1aj/6 if (o20_t1==3  & o20_p1==8) | (o20_t2==3 & o20_p2==8)
replace autoc=yac1aj/12 if (o20_t1==3  & o20_p1==9) | (o20_t2==3 & o20_p2==9)
replace autoc=. if yac1aj==. | yac1aj==9999999

gen aux3=.
replace aux3=yccsaj*22 if (o20_t1==4 & o20_p1==1) | (o20_t2==4 & o20_p2==1)
replace aux3=yccsaj*4 if (o20_t1==4 & o20_p1==2) | (o20_t2==4 & o20_p2==2)
replace aux3=yccsaj*2 if (o20_t1==4  & o20_p1==3) | (o20_t2==4 & o20_p2==3)
replace aux3=yccsaj if (o20_t1==4  & o20_p1==4) | (o20_t2==4 & o20_p2==4)
replace aux3=yccsaj/2 if (o20_t1==4  & o20_p1==5) | (o20_t2==4 & o20_p2==5)
replace aux3=yccsaj/3 if (o20_t1==4  & o20_p1==6) | (o20_t2==4 & o20_p2==6)
replace aux3=yccsaj/4 if (o20_t1==4  & o20_p1==7) | (o20_t2==4 & o20_p2==7)
replace aux3=yccsaj/6 if (o20_t1==4  & o20_p1==8) | (o20_t2==4 & o20_p2==8)
replace aux3=yccsaj/12 if (o20_t1==4  & o20_p1==9) | (o20_t2==4 & o20_p2==9)
replace aux3=. if yccsaj==. | yccsaj==9999999

egen ylnmpri_ci=rsum(aux1 autoc aux3)
replace ylnmpri_ci=. if (aux1==. & autoc==. & aux3==.) | emp_ci==0
drop aux1 aux3
*/

gen ylnmpri_ci=.


gen ylmsec_ci=.
gen ylnmsec_ci=.
gen ylnmotros_ci=.
gen nrylmpri_ci=(emp_ci==1 & ylmpri_ci==.)
replace nrylmpri_ci=. if emp_ci==0
/*
gen nrylmpri1_ci=(emp_ci==1 & ylmpri1_ci==.)
replace nrylmpri1_ci=. if emp_ci==0
gen nrylmpri2_ci=(emp_ci==1 & ylmpri2_ci==.)
replace nrylmpri2_ci=. if emp_ci==0
*/

/*****
ylm_ci
******
La cuarta categoria de tipo de ingreso es "Ingreso por otros trabajos realizados" e incluye trabajos secundarios, 
esporadicos u ocasionales. Por lo tanto, la sumamos directamente a ylm_ci*/

/*
OLD CODE:
egen ylm_ci=rsum(ylmpri_ci ytrsaj ytroaj)
replace ylm_ci=. if (ylmpri_ci==. & ytrsaj==. & ytroaj==.) | emp_ci==0
*/

***NEW***

gen ylm_ci= yopraj 
replace ylmpri_ci=. if  emp_ci==0


/*
egen ylm1_ci=rsum(ylmpri1_ci ytrsaj ytroaj)
replace ylm1_ci=. if (ylmpri1_ci==. & ytrsaj==. & ytroaj==.) | emp_ci==0
egen ylm2_ci=rsum(ylmpri2_ci ytrsaj ytroaj)
replace ylm2_ci=. if (ylmpri2_ci==. & ytrsaj==. & ytroaj==.) | emp_ci==0
*/

*gen ylnm_ci=ylnmpri_ci
gen ylnm_ci=.

/*
OLD CODE:
egen ynlm_ci=rsum(yre1aj ymesaj yfajaj yamaaj ydesaj yah1aj yah2aj ydonaj yrutaj yre2aj yre3aj yoasaj yonaaj yfamaj yjubaj yinvaj ymonaj yorfaj yotpaj ypa1aj ypa2aj ypa3aj ysu1aj ysu2aj ysu3aj ysu4aj ysu5aj yce1aj yce2aj yce3aj yaguaj yas1aj yas2aj yas3aj yas4aj)
replace ynlm_ci=. if yre1aj==. & ymesaj==. & yfajaj==. & yamaaj==. &  ydesaj==. & yah1aj==. & yah2aj==. & ydonaj==. & yrutaj==. & yre2aj==. & yre3aj==. & yoasaj==. & yonaaj==. & yfamaj==. & yjubaj==. & yinvaj==. & ymonaj==. & yorfaj==. & yotpaj==. & ypa1aj==. & ypa2aj==. & ypa3aj==. & ysu1aj==. & ysu2aj==. & ysu3aj==. & ysu4aj==. & ysu5aj==. & yce1aj==. & yce2aj==. & yce3aj==. & yaguaj==. & yas1aj==. & yas2aj==. & yas3aj==. & yas4aj==.
*/

***NEW
gen yautaj1=yautaj
replace yautaj1=0 if yautaj==.

gen ysubaj1=ysubaj
replace ysubaj1=0 if ysubaj==.

* 2014, 02 agregada linea por MLO porque no se estaba sumando cuando ylm_ci era missing
gen negylm=-ylm_ci
replace negylm=0 if ylm_ci==.

egen ynlm_ci = rsum(yautaj1 negylm ysubaj1), missing
replace ynlm_ci=. if yautaj==. & ylm_ci==. & ysubaj==. 


/*
gen ynlm_ci = yautaj1 - ylm_ci + ysubaj1
replace ynlm_ci=. if yautaj==. & ylm_ci==. & ysubaj==. */
****

gen ynlnm_ci=.

sort idh_ch 
by idh_ch: egen nrylmpri_ch=max(nrylmpri_ci) if miembros_ci==1
*by idh_ch: egen nrylmpri1_ch=max(nrylmpri1_ci) if miembros_ci==1, missing
*by idh_ch: egen nrylmpri2_ch=max(nrylmpri2_ci) if miembros_ci==1, missing
by idh_ch: egen ylm_ch=sum(ylm_ci)if miembros_ci==1, missing
*
by idh_ch: egen ylmpri_ch=sum(ylmpri_ci)if miembros_ci==1, missing
*
*by idh_ch: egen ylm1_ch=sum(ylm1_ci) if miembros_ci==1, missing
*by idh_ch: egen ylm2_ch=sum(ylm2_ci) if miembros_ci==1, missing
by idh_ch: egen ylnm_ch=sum(ylnm_ci)if miembros_ci==1, missing
gen ylmnr_ch=ylm_ch
replace ylmnr_ch=. if nrylmpri_ch==1
*
gen ylmprinr_ch=ylmpri_ch
replace ylmprinr_ch=. if nrylmpri_ch==1
*
/*
gen ylmnr1_ch=ylm1_ch
replace ylmnr1_ch=. if nrylmpri1_ch==1
gen ylmnr2_ch=ylm2_ch
replace ylmnr2_ch=. if nrylmpri2_ch==1
*/

by idh_ch: egen ynlm_ch=sum(ynlm_ci)if miembros_ci==1, missing
gen ynlnm_ch=.
gen ylmhopri_ci=ylmpri_ci/(horaspri_ci*4.2)
replace ylmhopri_ci=. if ylmhopri_ci<=0

/*
gen ylmhopri1_ci=ylmpri1_ci/(horaspri_ci*4.2)
replace ylmhopri1_ci=. if ylmhopri1_ci<=0
gen ylmhopri2_ci=ylmpri2_ci/(horaspri_ci*4.2)
replace ylmhopri2_ci=. if ylmhopri2_ci<=0
*/

gen ylmho_ci=ylm_ci/(horastot_ci*4.3)
label var ylmho_ci "Salario monetario de todas las actividades" 

gen rentaimp_ch=yaimhaj
*egen autocons_ci=rsum(autoc yac2aj)
*replace autocons_ci=. if autoc==. & yac2aj==.
*sort idh_ch
*by idh_ch: egen autocons_ch=sum(autocons_ci)
gen remesas_ci=.
gen remesas_ch=.

gen durades_ci=o4/4.3
replace durades_ci=. if o4==999 /*| activ!=2*/
label var durades_ci "Duración del desempleo"

g year=anio_c
g mes=12
g date1=mdy(o16_mes, 01, o16_anio)
g date2=mdy(mes, 31 , year)
replace date2=. if date1==. 
format 	date1 %td
format 	date2 %td	

g tiempotrab=date2-date1
g antiguedad_ci=tiempotrab/365

/*gen antiguedad_ci=(2003-o16_año)+((11-o16_mes)/12)+1 if o16_mes<=11
replace antiguedad_ci=(2003-o16_año)+1 if o16_mes==12 | o16_mes==99/*Hay una cita en una de las bananas originales en donde dicen que 
las entrevistas fueron realizadas casi finalizando el ciclo lectivo (y, como consecuencia, se consideraba que ese año se 
sumaba a aedu). Por lo tanto, podemos suponer que las entrevistas se realizaron en noviembre a los efectos de calcular le 
tenure.*/
*/
/****************************
VARIABLES DEL MERCADO LABORAL
*****************************/
/*gen desemp1_ci=.
/*El periodo de referencia de la encuesta es de dos meses!*/
gen desemp2_ci=.
gen desemp3_ci=(o1==2 & o2==2 & o3==1)

gen pea1_ci=.
gen pea2_ci=.
gen pea3_ci=(emp_ci==1 | desemp3_ci==1)
*/
*gen desalent_ci=(o1==2 & o2==2 & o3==2 & o7==8)
gen desalent_ci=(o1==2 & o2==2 & o3==2 & o6==8)

gen subemp_ci=.
gen tiempoparc_ci=.
gen categopri_ci=.
replace categopri_ci=1 if o9==1
replace categopri_ci=2 if o9==2
replace categopri_ci=3 if o9>=3 & o9<=7
replace categopri_ci=4 if o9==8
replace categopri_ci=. if emp_ci==0
gen categosec_ci=.

/*
gen contrato_ci=(o11>=1 & o11<=2)
replace contrato_ci=. if emp_ci==0
gen segsoc_ci=(o28<=5)
replace segsoc_ci=. if emp_ci==0 | o28==9 /*Esta variable es solo para los empleados!!!: La pregunta 25 es para todas las personas, 
tengan empleo o no*/
*/
gen nempleos_ci=1 if o22==2
replace nempleos_ci=2 if o22==1
/*
gen firmapeq_ci=1 if o14=="A" | o14=="B"
replace firmapeq_ci=0 if o14=="C" | o14=="D" | o14=="E" | o14=="F"
replace firmapeq_ci=. if o14=="X" | emp_ci==0*/

* Mod MLO incorporacion 2015/10
gen spublico_ci=(o9==3 | o9==4 | o9==9)
replace spublico_ci=. if emp_ci!=1
label var spublico_ci "Personas que trabajan en el sector público"
/*******************
VARIABLES EDUCATIVAS
*******************/
gen byte aedu_ci=.
replace aedu_ci=0 if e7t==0 | e7t==1 | e7t==16 
replace aedu_ci=e7c if e7t==2 | e7t==3 /*El máximo es 6 u 8 dependiendo si es el sistema viejo (preparatoria) o el nuevo (basica)*/
replace aedu_ci=. if e7t==4
*We assume that 'e7t==4', Diferential Education, will be equivalent to missing

*NEW: 16 Oct 2006 (Victoria)
replace aedu_ci=6 if (e7c>=6 & e7t==2) 
replace aedu_ci=8 if (e7c>=8 & e7t==3) 
*

/*
 table e7c e7t, c(mean edad)
--------------------------------------------------------------------------------------------------
          |                                          tipo                                         
    curso |          humanidades  educación media cien  técnica, comercial,   educación media técn
----------+---------------------------------------------------------------------------------------
        0 |                                                                                       
        1 |             63.51868               26.9809              62.61818              20.29556
        2 |             63.62524               29.3199              63.13158              23.61858
        3 |              65.1869              28.73852              61.97458              24.17076
        4 |             64.29607              32.52289              61.84691              28.57235
        5 |             65.15193                                    62.48571              35.31684
        6 |             65.25408                                    64.89024                      
--------------------------------------------------------------------------------------------------
Esta bastante claro que Humanidades y Tecnica, Comercial, etc... eran parte del sistema viejo (6 años de primaria) y que las otras dos son 
parte del sistema nuevo (8 años de primaria)
*/
replace aedu_ci=e7c+6 if e7t==5 | e7t==7
replace aedu_ci=e7c+8 if e7t==6 |e7t==8
replace aedu_ci=e7c+12 if e7t>=9 & e7t<=14
replace aedu_ci=e7c+17 if e7t==15
replace aedu_ci=. if e7t==99

** Generating attend. Dummy variable for school attendance
gen byte asiste_ci=(e2==1) 
label variable asiste_ci "Dummy variable for school attendance"

* We substract one year of education for those who are attending school at the moment that the survey took place
gen ban_aedu=aedu_ci
replace ban_aedu=aedu_ci-1 if aedu_ci!=0 & asiste_ci==1

/*
OLD CODE:
gen eduno_ci=(e7t==0 | e7t==1 | e7t==16)
gen edupi_ci=((e7t==2 & e7c>0 & e7c<6) | (e7t==3 & e7c>0 & e7c<8))
gen edupc_ci=((e7t==2 & e7c==6) | (e7t==3 & e7c==8))
gen edus1i_ci=(((e7t==5 | e7t==7) & (e7c>=1 & e7c<4)) | ((e7t==6 | e7t==8) & (e7c>=1 & e7c<2)))
gen edus1c_ci=(((e7t==5 | e7t==7) & e7c==4) | ((e7t==6 | e7t==8) & e7c==2))
gen edus2i_ci=((e7t==5 | e7t==7) & (e7c>4 & e7c<6)) | ((e7t==6 | e7t==8) & (e7c>2 & e7c<4))
gen edus2c_ci=((e7t==5 | e7t==7) & (e7c==6)) | ((e7t==6 | e7t==8) & (e7c==4 | e7c==5))

gen eduui_ci=(e7t==9 | e7t==11 | e7t==13) 
gen eduuc_ci=(e7t==10 | e7t==12 | e7t==14 | e7t==15)
gen edusi_ci=(edus1i_ci==1 | edus1c_ci==1 | edus2i_ci==1)
gen edusc_ci=edus2c_ci
*/


*****************
***pqnoasis_ci***
*****************
*Modificado Mayra Sáenz Junio, 2016: antes se generaba como missing
gen pqnoasis_ci= e3

**************
*pqnoasis1_ci*
**************
**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**

g       pqnoasis1_ci = 1 if e3 ==5  | e3 ==6
replace pqnoasis1_ci = 2 if e3 ==7  | e3 ==8 | e3 ==9
replace pqnoasis1_ci = 3 if e3 ==11 | e3 ==17 | e3 ==18
replace pqnoasis1_ci = 4 if e3 ==14
replace pqnoasis1_ci = 5 if e3 ==10 | e3 ==12 | e3 ==13
replace pqnoasis1_ci = 6 if e3 ==20
replace pqnoasis1_ci = 7 if e3 ==1 
replace pqnoasis1_ci = 8 if e3 ==3  | e3 ==4
replace pqnoasis1_ci = 9 if e3 ==2  | e3 ==15 | e3 ==16 | e3 ==19 | e3 ==21

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
replace edupi_ci=1 if aedu_ci>0 & aedu_ci<8
replace edupi_ci=. if aedu_ci==.
label variable edupi_ci "Primaria incompleta"

**************
***edupc_ci***
**************

gen byte edupc_ci=0
replace edupc_ci=1 if aedu_ci==8
replace edupc_ci=. if aedu_ci==.
label variable edupc_ci "Primaria completa"

**************
***edusi_ci***
**************

gen byte edusi_ci=0
replace edusi_ci=1 if aedu_ci>8 & aedu_ci<12
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
replace edus1i_ci=1 if aedu_ci==9
replace edus1i_ci=. if aedu_ci==.
label variable edus1i_ci "1er ciclo de la secundaria incompleto"

***************
***edus1c_ci***
***************

gen byte edus1c_ci=0
replace edus1c_ci=1 if aedu_ci==10
replace edus1c_ci=. if aedu_ci==.
label variable edus1c_ci "1er ciclo de la secundaria completo"

***************
***edus2i_ci***
***************

gen byte edus2i_ci=0
replace edus2i_ci=1 if aedu_ci==11
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

gen edupre_ci=(e7t==1)
label variable edupre_ci "Educacion preescolar"


**************
***eduac_ci***
**************
gen eduac_ci=.
replace eduac_ci=0 if e7t>=9 & e7t<=12
replace eduac_ci=1 if e7t>=13 & e7t<=15
label variable eduac_ci "Superior universitario vs superior no universitario"


foreach var of varlist edu* {
replace `var'=. if  aedu_ci==.
}

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
replace lp_ci= 43712   if zona_c==1  /*urbana*/
replace lp_ci= 29473   if zona_c==0	/*rural*/
label var lp_ci "Linea de pobreza oficial del pais"

*********
*lpe_ci***
*********

gen lpe_ci =.
replace lpe_ci= 21856   if zona_c==1  /*urbana*/
replace lpe_ci= 16842   if zona_c==0	/*rural*/
label var lpe_ci "Linea de indigencia oficial del pais"

****************
*cotizando_ci***
****************
gen cotizando_ci=.
replace cotizando_ci=1 if (o28 >= 1 & o28 <= 5)
recode cotizando_ci .=0 if (activ==1 | activ==2)
label var cotizando_ci "Cotizante a la Seguridad Social"

****************
*afiliado_ci****
****************
gen afiliado_ci=.	
replace afiliado_ci=1 if (o28 >= 1 & o28 <= 6)
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
replace instcot_ci=o28 if o28<=5
label var instcot_ci "Institucion a la que cotiza - variable original de cada pais" 


*****************
*tipocontrato_ci*
*****************
/*
gen tipocontrato_ci=.
replace tipocontrato_ci=1 if o12a==1 & categopri_ci==3
replace tipocontrato_ci=2 if o12a==2 & categopri_ci==3
replace tipocontrato_ci=3 if o11 ==3 & categopri_ci==3
label var tipocontrato_ci "Tipo de contrato segun su duracion"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci
*/
 * Corregido por la variable de firmo o no firmo e inclusion de todas las categorias temporales MGD 06/16/2014	
gen tipocontrato_ci=.
replace tipocontrato_ci=1 if ((o11==1 | o11==2) & o12a==1) & categopri_ci==3
replace tipocontrato_ci=2 if ((o11==1 | o11==2) & (o12a>=2 & o12a<=5)) & categopri_ci==3
replace tipocontrato_ci=3 if (o11>=3 | tipocontrato_ci==.) & categopri_ci==3
label var tipocontrato_ci "Tipo de contrato segun su duracion"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci
	
*************
*cesante_ci* 
*************
gen cesante_ci=1 if o5==1
replace cesante_ci=0 if o5==2
label var cesante_ci "Desocupado - definicion oficial del pais"	


**************
***tamemp_ci**
**************
gen tamemp_ci=1 if o14=="A" | o14=="B"
replace tamemp_ci=2 if o14=="C" | o14=="D"
replace tamemp_ci=3 if o14=="E" | o14=="F"

label var tamemp_ci "# empleados en la empresa segun rangos"
label define tamemp_ci 1 "Pequena" 2 "Mediana" 3 "Grande"
label value tamemp_ci tamemp_ci


*************
**pension_ci*
*************
*egen auxpen=rsum(yjubaj), missing
*MLO: estas variables vienen en el modulo de ignresos complementario ajustado por CEPAL
*yvitaj
egen auxpen=rsum(yjubaj yinvaj ymonaj yorfaj yotpaj), m
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
egen auxpens=rsum(ypasaj yasaj), missing
gen pensionsub_ci=1 if auxpens>0 & auxpens!=.
recode pensionsub_ci .=0 
label var pensionsub_ci "1=recibe pension subsidiada / no contributiva"

*****************
**ypensub_ci*
*****************
destring auxpens, replace

gen  ypensub_ci=auxpens
replace ypensub_ci=. if auxpens<0
drop auxpens
label var ypensub_ci "Valor de la pension subsidiada / no contributiva"

**********
**tc_ci***
**********
gen tc_ci=530.9547619
label var tc_ci "Tipo de cambio LCU/USD"

*************
**salmm_ci***
*************
* CHL 2003
gen salmm_ci= 	115648
label var salmm_ci "Salario minimo legal"

*************
***tecnica_ci**
*************

gen tecnica_ci=.
replace tecnica_ci=1 if e7t==9 | e7t==10
recode tecnica_ci .=0 
label var tecnica_ci "1=formacion terciaria tecnica"



****************
**categoinac_ci*
****************


gen categoinac_ci=1 if o6==5
replace categoinac_ci=2 if o6==4
replace categoinac_ci=3 if o6==1
replace categoinac_ci=4 if o6==2 | o6==3 | o6==6 | o6==7 | o6==8 | o6==9 | o6==10


label var categoinac_ci "Condición de inactividad"
	label define categoinac_ci 1 "jubilado/pensionado" 2 "estudiante" 3 "quehaceres_domesticos" 4 "otros_inactivos" 
	label value categoinac_ci categoinac_ci
	

***************
***formal_ci***
***************

gen byte formal_ci=1 if cotizando_ci==1 & (condocup_ci==1 | condocup_ci==2)
recode formal_ci .=0 if (condocup_ci==1 | condocup_ci==2)
label var formal_ci "1=afiliado o cotizante / PEA"

* variables que faltan crear
gen ylmotros_ci=.
gen tcylmpri_ci =.
gen tcylmpri_ch =.
gen autocons_ci=.
gen autocons_ch=.
gen region_c=.

******************************
*** VARIABLES DE GDI *********
******************************
	
	
	/***************************
     * DISCAPACIDAD
    ***************************/
	
gen dis_ci==. 
lab def dis_ci 1 1 "Con Discapacidad" 0 "Sin Discapacidad"
lab val dis_ci dis_ci
label var dis_ci "Personas con discapacidad"
		
		
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
	
   gen geolev1=.   
   replace geolev1=15201 if  r==1		/*Arica, Parinacota y Tarapacá*/
   replace geolev1=15202 if  r==2		/*Antofagasta*/
   replace geolev1=15203 if  r==3		/*Atacama*/
   replace geolev1=15204 if  r==4		/*Coquimbo*/
   replace geolev1=15205 if  r==5		/*Valparaíso*/
   replace geolev1=15206 if  r==6		/*O'Higgins*/
   replace geolev1=15207 if  r==7		/*Maule*/
   replace geolev1=15208 if  r==8		/*Ñuble y Bío Bío*/
   replace geolev1=15209 if  r==9		/*La Araucanía*/
   replace geolev1=15210 if r==10		/*Los Lagos y Los Ríos*/
   replace geolev1=15211 if r==11		/*Aysén*/
   replace geolev1=15212 if r==12		/*Magallanes y Antártica Chilena*/
   replace geolev1=15213 if r==13		/*Metropolitana Santiago*/

	label define geolev1 15201"Arica, Parinacota y Tarapacá" 15202"Antofagasta" 15203"Atacama" 15204"Coquimbo" 15205"Valparaíso" 15206"O'Higgins" 15207"Maule" 15208"Ñuble y Bío Bío" 15209"La Araucanía" 15210"Los Lagos y Los Ríos" 15211"Aysén" 15212"Magallanes y Antártica Chilena" 15213"Metropolitana Santiago"
	label value geolev1 geolev1
	label var geolev1 " Primera division politico-administrativa, región"
	
	
	**************************
	** PROVINCIAS ************
	**************************
		
   gen ine02=.   
   replace ine02=11 if p==11			/*Arica*/
   replace ine02=12 if p==12			/*Parinacota*/
   replace ine02=13 if p==13			/*Iquique*/
   replace ine02=21 if p==21			/*Tocopilla*/
   replace ine02=22 if p==22		    /*El Loa*/
   replace ine02=23 if p==23			/*Antofagasta*/
   replace ine02=31 if p==31			/*Chañaral*/
   replace ine02=32 if p==32			/*Copiapó*/
   replace ine02=33 if p==33			/*Huasco*/
   replace ine02=41 if p==41			/*Elqui*/
   replace ine02=42 if p==42			/*Limarí*/
   replace ine02=43 if p==43			/*Choapa*/
   replace ine02=51 if p==51			/*Petorca*/
   replace ine02=52 if p==52			/*Los Andes*/
   replace ine02=53 if p==53	    	/*San Felipe de Aconcagua*/
   replace ine02=54 if p==54			/*Quillota*/
   replace ine02=55 if p==55			/*Valparaíso*/
   replace ine02=56 if p==56			/*San Antonio*/
   replace ine02=61 if p==61			/*Cachapoal*/
   replace ine02=62 if p==62			/*Colchagua*/
   replace ine02=63 if p==63			/*Cardenal Caro*/
   replace ine02=71 if p==71			/*Curico*/
   replace ine02=72 if p==72			/*Talca*/
   replace ine02=73 if p==73			/*Linares*/
   replace ine02=74 if p==74	    	/*Cauquenes*/
   replace ine02=81 if p==81			/*Ñuble*/
   replace ine02=82 if p==82			/*Bio Bío*/
   replace ine02=83 if p==83			/*Concepción*/
   replace ine02=84 if p==84			/*Arauco*/
   replace ine02=91 if p==91			/*Malleco*/
   replace ine02=92 if p==92			/*Cautín*/
   replace ine02=101 if p==101			/*Valdivia*/
   replace ine02=102 if p==102			/*Osorno*/
   replace ine02=103 if p==103			/*Llanquihue*/
   replace ine02=104 if p==104			/*Chiloé*/
   replace ine02=105 if p==105			/*Palena*/
   replace ine02=111 if p==111			/*Cohaique*/
   replace ine02=112 if p==112	    	/*Aisén*/
   replace ine02=113 if p==113			/*General Carrera*/
   replace ine02=114 if p==114			/*Capitán Prat*/
   replace ine02=121 if p==121			/*Última Esperanza*/
   replace ine02=122 if p==122			/*Magallanes*/
   replace ine02=123 if p==123			/*Tierra del Fuego*/
   replace ine02=131 if p==131			/*Santiago*/
   replace ine02=132 if p==132			/*Chacabuco*/
   replace ine02=133 if p==133			/*Cordillera*/
   replace ine02=134 if p==134			/*Maipo*/
   replace ine02=135 if p==135			/*Melipilla*/
   replace ine02=136 if p==136			/*Talagante*/

	label define ine02 11"Arica" 12"Parinacota" 13"Iquique" 21"Tocopilla" 22"El Loa" 23"Antofagasta" 31"Chañaral" 32"Copiapó" 33"Huasco" 41"Elqui" 42"Limarí" 43"Choapa" 51"Petorca" 52"Los Andes" 53"San Felipe de Aconcagua" 54"Quillota" 55"Valparaíso" 56"San Antonio" 61"Cachapoal" 62"Colchagua" 63"Cardenal Caro" 71"Curico" 72"Talca" 73"Linares" 74"Cauquenes" 81"Ñuble" 82"Bio Bío" 83"Concepción" 84"Arauco" 91"Malleco" 92"Cautín" 101"Valdivia" 102"Osorno" 103"Llanquihue" 104"Chiloé" 105"Palena" 111"Cohaique" 112"Aisén" 113"General Carrera" 114"Capitán Prat" 121"Última Esperanza" 122"Magallanes" 123"Tierra del Fuego" 131"Santiago" 132"Chacabuco" 133"Cordillera" 134"Maipo" 135"Melipilla" 136"Talagante"
	label value ine02 ine02
	label var ine02 " Segunda division politico-administrativa, Provincia"


/*_____________________________________________________________________________________________________*/
* Asignación de etiquetas e inserción de variables externas: tipo de cambio, Indice de Precios al 
* Consumidor (2011=100), Paridad de Poder Adquisitivo (PPA 2011),  líneas de pobreza
/*_____________________________________________________________________________________________________*/


do "$gitFolder\armonizacion_microdatos_encuestas_hogares_scl\_DOCS\\Labels&ExternalVars_Harmonized_DataBank.do"

/*_____________________________________________________________________________________________________*/
* Verificación de que se encuentren todas las variables armonizadas 
/*_____________________________________________________________________________________________________*/

order region_BID_c region_c pais_c anio_c mes_c zona_c factor_ch	idh_ch	idp_ci	factor_ci sexo_ci edad_ci ///
afroind_ci afroind_ch afroind_ano_c dis_ci dis_ch  relacion_ci civil_ci jefe_ci nconyuges_ch nhijos_ch notropari_ch notronopari_ch nempdom_ch ///
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



rename o8 codindustria
rename o7 codocupa
compress


saveold "`base_out'", replace


log close

