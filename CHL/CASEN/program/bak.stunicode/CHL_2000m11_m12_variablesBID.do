* (Versión Stata 12)
clear
set more off
*________________________________________________________________________________________________________________*

 * Activar si es necesario (dejar desactivado para evitar sobreescribir la base y dejar la posibilidad de 
 * utilizar un loop)
 * Los datos se obtienen de las carpetas que se encuentran en el servidor: \\Sdssrv03\surveys
 * Se tiene acceso al servidor únicamente al interior del BID.
 * El servidor contiene las bases de datos MECOVI.
 *________________________________________________________________________________________________________________*
 


*global ruta = "\\Sdssrv03\surveys"

local PAIS CHL
local ENCUESTA CASEN
local ANO "2000"
local ronda m11_m12 

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
   



capture log close
log using "`log_file'", replace 

/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Chile
Encuesta: CASEN
Round: Noviembre - Diciembre
Autores: 
Versión 2007: Victoria
Versión 2012: Yanira Oviedo (YO), Yessenia Loaysa (YL)
Modificación 2014: Mayra Sáenz - Email: mayras@iadb.org - saenzmayra.a@gmail.com
Última versión: María Laura Oliveri (MLO) - Email: mloliveri@iadb.org, lauraoliveri@yahoo.com
Fecha última modificación: 26 de Marzo de 2013

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
ren segmento seg
ren folio f
egen idh_ch=group(r p c z seg f)
gen idp_ci=o
gen zona_c=z
replace zona_c=0 if z==2
gen pais_c="CHL"
gen anio_c=2000
gen mes_c=11
gen relacion_ci=pco1
replace relacion_ci=4 if pco1>=4 & pco1<=10
replace relacion_ci=5 if pco1==11
replace relacion_ci=6 if pco1==12

/*************************************
VARIABLES DE INFRAESTRUCTURA DEL HOGAR
**************************************/
gen aguared_ch=(v11==1 | v11==2 | v11==3)
gen aguadist_ch=v12
gen aguamala_ch=(v11==5|v11==6)
gen aguamide_ch=(v11==1 |v11==2)
gen luz_ch=(v15<=5)
gen luzmide_ch=(v15==1 | v15==2)
replace luzmide_ch=. if luz_ch==0
gen combust_ch=.
gen bano_ch=((v9!=0 & v32==.) | v32>0 & v32<=3)
gen banoex_ch=(v32==. | v32<v9)
replace banoex_ch=. if bano_ch==0 
gen des1_ch=0 if bano_ch==0 | v14==7
replace des1_ch=1 if v14==1 | v14==2
replace des1_ch=2 if v14==3 | v14==4
replace des1_ch=3 if v14==5 | v14==6
gen des2_ch=des1_ch
replace des2_ch=. if des1_ch==3
gen piso_ch=0 if v18==5
replace piso_ch=1 if v18<5
gen pared_ch=0 if v16>=4 & v16<=7
replace pared_ch=1 if v16<4
replace pared_ch=2 if v16==8
gen techo_ch=0 if v20>=5
replace techo_ch=1 if v20<5
gen resid_ch=.

 **Daniela Zuluaga- Enero 2018: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**
	
 *********************
 ***aguamejorada_ch***
 *********************
g       aguamejorada_ch = 1 if (v11 >=1 & v11 <=4)
replace aguamejorada_ch = 0 if (v11 >=5 & v11 <=6) | v12 == 3 

 *********************
 ***banomejorado_ch***
 *********************
g       banomejorado_ch = 1 if  (v14 >=1 & v14 <=4) 
replace banomejorado_ch = 0 if  (v14 >=5 & v14 <=7)

gen dorm_ch=v4 
egen piezaviv=rsum(v4 v5 v6 v7 v8 v9 v10), missing
replace piezaviv=. if v4==. & v5==. & v6==. & v7==. & v8==. & v9==. & v10==. 
egen piezahog=rsum(v27 v28 v29 v30 v31 v32 v33), missing
replace piezahog=. if v27==. & v28==. & v29==. & v30==. & v31==. & v32==. & v33==. 
gen cuartos_ch=piezaviv 
gen cocina_ch=(v8!=0)
sort idh_ch
by idh_ch: egen telef_ch=sum(p3==1)
replace telef_ch=1 if telef_ch>=1
by idh_ch: egen refrig_ch=sum(p2==1)
replace refrig_ch=1 if refrig_ch>=1
gen freez_ch=.
gen auto_ch=.
by idh_ch: egen compu_ch=sum(p6==1)
replace compu_ch=1 if compu_ch>=1
by idh_ch: egen internet_ch=sum(p7==1)
replace compu_ch=1 if compu_ch==1

/*****
cel_ch
*****/
gen aux1=p8
replace aux1=0 if p8==2 | p8==.
by idh_ch:egen aux2=sum(aux1)
gen cel_ch=(aux2>0)
drop aux1 aux2

gen vivi1_ch=1 if v22==1 | v22==2
replace vivi1_ch=2 if v22==3
replace vivi1_ch=3 if v22>3
gen vivi2_ch=(vivi1_ch==1 | vivi1_ch==2)
gen viviprop_ch=0 if v23==5 | v23==6
replace viviprop_ch=1 if v23==1 | v23==3
replace viviprop_ch=2 if v23==2 | v23==4
replace viviprop_ch=3 if v23==10
replace viviprop_ch=4 if v23>6 & v23<=9
recode v24 (99999999=.)
gen vivitit_ch=.
gen vivialq_ch=v24 if viviprop_ch==0 /*Cuanto paga y cuanto pagaria estan en la misma pregunta*/
gen vivialqimp_ch=v24 if viviprop_ch!=0


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

*************************
*** VARIABLES DE RAZA ***
*************************

* MGR Oct. 2015: modificaciones realizadas en base a metodología enviada por SCL/GDI Maria Olga Peña

/*
ETNIA ¿En Chile, la ley reconoce ocho pueblos originarios o indígenas, ¿pertenece usted a alguno
de ellos? (Preg. 7)
etnia:
           0 no pertenece a ninguno 
           1 si, aymará
           2 si, rapa-nui
           3 si, quechua
           4 si, mapuche
           5 si, atacameño
           6 si, coya
           7 si, kawaskar
           8 si, yagán 
*/

gen raza_ci=.
replace raza_ci= 1 if  (etnia >=1 & etnia <=8 )
replace raza_ci= 3 if (etnia==0)& raza_ci==.
label define raza_ci 1 "Indígena" 2 "Afro-descendiente" 3 "Otros"
label value raza_ci raza_ci 
label value raza_ci raza_ci
label var raza_ci "Raza o etnia del individuo" 

gen raza_idioma_ci=.

gen id_ind_ci = 0
replace id_ind_ci=1 if raza_ci==1
label define id_ind_ci 1 "Indígena" 0 "Otros" 
label value id_ind_ci id_ind_ci 
label var id_ind_ci  "Indigena" 

gen id_afro_ci = 0
replace id_afro_ci=1 if raza_ci==2
label define id_afro_ci 1 "Afro-descendiente" 0 "Otros" 
label value id_afro_ci id_afro_ci 
label var id_afro_ci "Afro-descendiente" 


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
* Utiliza CIUO-88 (MGD 6/16/17)
gen ocupa_ci=.
replace ocupa_ci=1 if (o8>=2100 & o8<=3480) & emp_ci==1
replace ocupa_ci=2 if (o8>=1100 & o8<=1319) & emp_ci==1
replace ocupa_ci=3 if (o8>=4100 & o8<=4223) & emp_ci==1
replace ocupa_ci=4 if ((o8>=9100 & o8<=9113) | (o8>=5200 & o8<=5230)) & emp_ci==1
replace ocupa_ci=5 if ((o8>=5100 & o8<=5169) | (o8>=9100 & o8<=9162)) & emp_ci==1
replace ocupa_ci=6 if ((o8>=6100 & o8<=6210) | (o8>=9200 & o8<=9220)) & emp_ci==1
replace ocupa_ci=7 if ((o8>=7100 & o8<=8340) | (o8>=9300 & o8<=9333))  & emp_ci==1
replace ocupa_ci=8 if o8==110 & emp_ci==1
replace ocupa_ci=9 if o8==9999 & emp_ci==1


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
gen rama_ci=substr(string(o9),1,1)
destring rama_ci, replace
replace rama_ci=. if emp_ci==0 | rama_ci<=0
*/

gen rama_ci=.
replace rama_ci=1 if (o9>=1110 & o9<=1400)   & emp_ci==1
replace rama_ci=2 if (o9>=2000 & o9<=2990) & emp_ci==1
replace rama_ci=3 if (o9>=3000 & o9<=3990) & emp_ci==1
replace rama_ci=4 if (o9>=4000 & o9<=4990) & emp_ci==1
replace rama_ci=5 if o9==5000 & emp_ci==1
replace rama_ci=6 if (o9>=6000 & o9<=6990) & emp_ci==1
replace rama_ci=7 if (o9>=7000 & o9<=7990) & emp_ci==1
replace rama_ci=8 if (o9>=8000 & o9<=8400) & emp_ci==1
replace rama_ci=9 if (o9>=9000  & o9<=9900) & emp_ci==1


gen horaspri_ci=o14
replace horaspri_ci=. if emp_ci==0 | o14==999

gen horastot_ci=horaspri_ci

gen ylmpri_ci=yopraj
replace ylmpri_ci=. if yopraj==. | emp_ci==0


/*********
ylmpri1_ci (No estan los ingresos ajustados para estos "extras"del ingreso. Solo esta yopraj.
*********/
/*
gen aux1=.
replace aux1=o22*22 if o20==1 & o23==1
replace aux1=o22*4 if o20==1 & o23==2
replace aux1=o22*2 if o20==1 & o23==3
replace aux1=o22 if o20==1 & o23==4
replace aux1=o22/2 if o20==1 & o23==5
replace aux1=o22/3 if o20==1 & o23==6
replace aux1=o22/4 if o20==1 & o23==7
replace aux1=o22/6 if o20==1 & o23==8
replace aux1=o22/12 if o20==1 & o23==9
replace aux1=. if o22==. | o22==9999999

gen aux2=.
replace aux2=o25*22 if o24==1 & o26==1
replace aux2=o25*4 if o24==1 & o26==2
replace aux2=o25*2 if o24==1 & o26==3
replace aux2=o25 if o24==1 & o26==4
replace aux2=o25/2 if o24==1 & o26==5
replace aux2=o25/3 if o24==1 & o26==6
replace aux2=o25/4 if o24==1 & o26==7
replace aux2=o25/6 if o24==1 & o26==8
replace aux2=o25/12 if o24==1 & o26==9
replace aux2=. if o25==. | o25==9999999

egen ylmpri1_ci=rsum(ylmpri_ci aux1 aux2) 
*Este tipo de ingreso tiene salario mas bonificaciones*
replace ylmpri1_ci=. if (ylmpri_ci==. & aux1==. & aux2==.) | emp_ci==0
drop aux1 aux2
*/

/*********
ylmpri2_ci: Este además, incluye bonificaciones y ganancias anuales
*********/
/*
gen aux1=o28/12 
*o28 es anual
egen ylmpri2_ci=rsum(ylmpri1_ci aux1)
replace ylmpri2_ci=. if ylmpri1_ci==. & aux1==.
drop aux1
*/

/*********
ylnmpri_ci
**********/

/*
gen aux1=.
replace aux1=o22*22 if (o20==2 | o20==3| o20==4) & o23==1
replace aux1=o22*4  if (o20==2 | o20==3| o20==4) & o23==2
replace aux1=o22*2  if (o20==2 | o20==3| o20==4) & o23==3
replace aux1=o22    if (o20==2 | o20==3| o20==4) & o23==4
replace aux1=o22/2  if (o20==2 | o20==3| o20==4) & o23==5
replace aux1=o22/3  if (o20==2 | o20==3| o20==4) & o23==6
replace aux1=o22/4  if (o20==2 | o20==3| o20==4) & o23==7
replace aux1=o22/6  if (o20==2 | o20==3| o20==4) & o23==8
replace aux1=o22/12 if (o20==2 | o20==3| o20==4) & o23==9
replace aux1=. if o22==. | o22==9999999

gen aux2=.
replace aux2=o25*22 if (o24==2 | o24==3 | o24==4) & o26==1
replace aux2=o25*4  if (o24==2 | o24==3 | o24==4) & o26==2
replace aux2=o25*2  if (o24==2 | o24==3 | o24==4) & o26==3
replace aux2=o25    if (o24==2 | o24==3 | o24==4) & o26==4
replace aux2=o25/2  if (o24==2 | o24==3 | o24==4) & o26==5
replace aux2=o25/3  if (o24==2 | o24==3 | o24==4) & o26==6
replace aux2=o25/4  if (o24==2 | o24==3 | o24==4) & o26==7
replace aux2=o25/6  if (o24==2 | o24==3 | o24==4) & o26==8
replace aux2=o25/12 if (o24==2 | o24==3 | o24==4) & o26==9
replace aux2=. if o25==. | o25==9999999

egen ylnmpri_ci=rsum(aux1 aux2)
replace ylnmpri_ci=. if (aux1==. & aux2==.) | emp_ci==0
drop aux1 aux2
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

*egen ylm_ci=rsum(ytrabaj)
gen ylm_ci=ytrabaj
replace ylm_ci=. if (ytrabaj==.) | emp_ci==0

/*
egen ylm1_ci=rsum(ylmpri1_ci o30)
replace ylm1_ci=. if (ylmpri1_ci==. & o30==.) | emp_ci==0
egen ylm2_ci=rsum(ylmpri2_ci o30)
replace ylm2_ci=. if (ylmpri2_ci==. & o30==.) | emp_ci==0
*/

*gen ylnm_ci=ylnmpri_ci
gen ylnm_ci=.
gen ynlnm_ci=.	

/*
OLD CODE:
egen ynlm_ci=rsum(yjubaj yautaj ymoneaj ypasaj ysufaj ycesaj yfamaj yosuaj ysubaj)
replace ynlm_ci=. if yjubaj==. & yautaj==. & ymoneaj==. & ypasaj==. & ysufaj==. & ycesaj==. & yfamaj==. & yosuaj==. & ysubaj==. 
gen ynlnm_ci=.
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

sort idh_ch 
by idh_ch: egen nrylmpri_ch=max(nrylmpri_ci) if miembros_ci==1
*by idh_ch: egen nrylmpri1_ch=max(nrylmpri1_ci) if miembros_ci==1, missing
*by idh_ch: egen nrylmpri2_ch=max(nrylmpri2_ci) if miembros_ci==1, missing
by idh_ch: egen ylm_ch=sum(ylm_ci)if miembros_ci==1, missing
*by idh_ch: egen ylm1_ch=sum(ylm1_ci) if miembros_ci==1, missing
*by idh_ch: egen ylm2_ch=sum(ylm2_ci) if miembros_ci==1, missing
by idh_ch: egen ylnm_ch=sum(ylnm_ci)if miembros_ci==1, missing
gen ylmnr_ch=ylm_ch
replace ylmnr_ch=. if nrylmpri_ch==1

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
gen autocons_ch=.
gen remesas_ci=.
gen remesas_ch=.

gen durades_ci=o4/4.3
replace durades_ci=. if o4==999 /*| activ!=2*/
label var durades_ci "Duración del desempleo"

g year=anio_c
g mes=12
g date1=mdy(o16m, 01, o16a)
g date2=mdy(mes, 31 , year)
replace date2=. if date1==. 
format 	date1 %td
format 	date2 %td	

g tiempotrab=date2-date1
g antiguedad_ci=tiempotrab/365
/*gen antiguedad_ci=(2000-o16a)+((11-o16m)/12)+1 if o16m<=11
replace antiguedad_ci=(2000-o16a)+1 if o16m==12 | o16m==99/*Hay una cita en una de las bananas originales en donde dicen que 
las entrevistas fueron realizadas casi finalizando el ciclo lectivo (y, como consecuencia, se consideraba que ese año se 
sumaba a aedu). Por lo tanto, podemos suponer que las entrevistas se realizaron en noviembre a los efectos de calcular le 
tenure.*/
replace antiguedad_ci=. if o16a==9999 | o16m==99*/

/****************************
VARIABLES DEL MERCADO LABORAL
*****************************/
/*gen desemp1_ci=. 
gen desemp2_ci=.
gen desemp3_ci=(o1==2 & o2==2 & o3==1)
/*El periodo de referencia de la encuesta es de dos meses!*/
gen pea1_ci=.
gen pea2_ci=.
gen pea3_ci=(emp_ci==1 | desemp3_ci==1)*/

gen desalent_ci=(o1==2 & o2==2 & o3==2 & o7==8)

gen subemp_ci=.
gen tiempoparc_ci=.
gen categopri_ci=.
replace categopri_ci=1 if o10==1
replace categopri_ci=2 if o10==2
replace categopri_ci=3 if o10>=3 & o10<=7
replace categopri_ci=4 if o10==8
replace categopri_ci=. if emp_ci==0
gen categosec_ci=.

/*
gen contrato_ci=(o11>=1 & o11<=3)
replace contrato_ci=. if emp_ci==0
gen segsoc_ci=(o17<=7)
replace segsoc_ci=. if emp_ci==0 /*Esta variable es solo para los empleados!!!: La pregunta 25 es para todas las personas, 
tengan empleo o no*/
*/
gen nempleos_ci=1 if o29==2
replace nempleos_ci=2 if o29==1

/*
gen firmapeq_ci=0 if o13=="A" | o13=="B"
replace firmapeq_ci=1 if o13=="C" | o13=="D" | o13=="E" | o13=="F"
replace firmapeq_ci=. if o13=="X" | emp_ci==0
*cambio en 01/24/07*
ren firmapeq_ci tamfirma_ci
gen firmapeq_ci=1 if tamfirma_ci==0
replace firmapeq_ci=0 if tamfirma_ci==1
replace firmapeq_ci=. if emp_ci==0
drop tamfirma_ci
*/

* Mod MLO incorporacion 2015/10
gen spublico_ci=(o10==3 | o10==4 | o10==9)
replace spublico_ci=. if emp_ci!=1
label var spublico_ci "Personas que trabajan en el sector público"

/*******************
VARIABLES EDUCATIVAS
*******************/
gen byte aedu_ci=.
replace aedu_ci=0 if e9==0 | e9==1 | e9==16 
replace aedu_ci=e8 if e9==2 | e9==3 
replace aedu_ci=. if e9==4 
*We assume that 'e9==4', Diferential Education, will be equivalent to missing

*NEW: 16 Oct 2006 (Victoria)
replace aedu_ci=6 if (e8>=6 & e9==2) 
replace aedu_ci=8 if (e8>=8 & e9==3) 
*

replace aedu_ci=e8+6 if e9==5 | e9==7
replace aedu_ci=e8+8 if e9==6 | e9==8
replace aedu_ci=e8+12 if e9>=9 & e9<15 
replace aedu_ci=e8+17 if e9==15 /*See the original variable wich is not correct for this category*/
replace aedu_ci=. if e9==99

** Generating attend. Dummy variable for school attendance
gen byte attend=(e3==1) /*There are no missing values in the original dataset*/
replace attend=0 if e8==. | e9==16
label variable attend "Dummy variable for school attendance"

* We substract one year of education for those who are attending school at the moment that the survey took place
gen ban_aedu=aedu_ci-1 if aedu_ci!=0 & attend==1

/*
OLD CODE:
gen eduno_ci=(e9==0 | e9==16 | (e9==1 & e8==0))
gen edupi_ci=((e9==2 & e8>0 & e8<6) | (e9==3 & e8>0 & e8<8))
gen edupc_ci=((e9==2 & e8==6) | (e9==3 & e8==8))
gen edus1i_ci=(((e9==5 | e9==7) & (e8>=1 & e8<4)) | ((e9==6 | e9==8) & (e8>=1 & e8<2)))
gen edus1c_ci=(((e9==5 | e9==7) & e8==4) | ((e9==6 | e9==8) & e8==2))
gen edus2i_ci=((e9==5 | e9==7) & (e8>4 & e8<6)) | ((e9==6 | e9==8) & (e8>2 & e8<4))
gen edus2c_ci=((e9==5 | e9==7) & (e8==6)) | ((e9==6 | e9==8) & (e8==4 | e8==5))

gen eduui_ci=(e9==9 | e9==11 | e9==13) 
gen eduuc_ci=(e9==10 | e9==12 | e9==14 | e9==15)
gen edusi_ci=(edus1i_ci==1 | edus1c_ci==1 | edus2i_ci==1)
gen edusc_ci=edus2c_ci
*/


*****************
***pqnoasis_ci***
*****************
*Modificado Mayra Sáenz Junio, 2016: antes se generaba como missing
gen pqnoasis_ci= e5

**************
*pqnoasis1_ci*
**************
**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**

g       pqnoasis1_ci = 1 if e5 ==6
replace pqnoasis1_ci = 2 if e5 ==7
replace pqnoasis1_ci = 3 if e5 ==9 | e5 ==15 | e5 ==16
replace pqnoasis1_ci = 4 if e5 ==11
replace pqnoasis1_ci = 5 if e5 ==8 | e5 ==10
replace pqnoasis1_ci = 7 if e5 ==1 | e5 ==12 
replace pqnoasis1_ci = 8 if e5 ==2  | e5 ==3  | e5 ==4
replace pqnoasis1_ci = 9 if e5 ==5 | e5 ==13 | e5 ==14 | e5 ==17 | e5 ==18 | e5 ==19 | e5 ==20

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

gen edupre_ci=(e9==1)
label variable edupre_ci "Educacion preescolar"


**************
***eduac_ci***
**************
gen eduac_ci=.
replace eduac_ci=0 if e9>=9 & e9<=12
replace eduac_ci=1 if e9==13 | e9==14
label variable eduac_ci "Superior universitario vs superior no universitario"

***************
***asiste_ci***
***************

gen asiste_ci=(e3==1)
label variable asiste_ci "Asiste actualmente a la escuela"



foreach var of varlist edu* {
replace `var'=. if  aedu_ci==.
}


gen repite_ci=.
gen repiteult_ci=.

gen edupub_ci=.

***********************
*** CHILE 2000	    ***
***********************

 
 tab pco1
 tab nucleo
 tab pco1 nucleo
 tab pco1 nucleo if nucleo==0
 tab pco2 if pco1==12

 rename expr factor /* Expansión Regional */
 rename z area
 rename e1 alfabet
 rename e3 asiste
 rename e9 nivel
 rename e8 ultgrado
 rename v23 tenencia
 rename v11 agua
 rename v12 lugabast
 rename v14 servsani
 rename v16 pared
 rename v18 piso
 rename v22 tipoviv
 rename o10 categ
 rename o13 tamest

 gen     incl=1 if (pco1>=1 &  pco1<=12)
 replace incl=0 if  pco1==12 | (pco1==11 & nucleo==0)

** AREA

 tab area [w=factor]

** Gender classification of the population refering to the head of the household.

 sort r p c area seg f o 

* Household ID

 gen x=1 if pco1==1 	
 gen id_hogar=sum(x)
 drop x

 gen     sexo_d_=1 if pco1==1 & sexo==1
 replace sexo_d_=2 if pco1==1 & sexo==2

 egen sexo_d=max(sexo_d_), by(id_hogar)
 
 tab sexo    [w=factor]
 tab sexo_d [w=factor]

 tab sexo sexo_d if pco1==1

** Years of education. 

* ESC => "Escolaridad": Years of education for the population with 15 years or more of age.


 tab nivel ultgrado if asiste==1 & (nivel==6 | nivel==8) & edad==17
 tab nivel ultgrado if asiste==1 & (nivel==6 | nivel==8) & edad==17 & esc==12

 gen     anoest=0  if (nivel==4 | nivel==16 | nivel==1)
 replace anoest=1  if (nivel==2 | nivel==3) & ultgrado==1
 replace anoest=2  if (nivel==2 | nivel==3) & ultgrado==2
 replace anoest=3  if (nivel==2 | nivel==3) & ultgrado==3
 replace anoest=4  if (nivel==2 | nivel==3) & ultgrado==4
 replace anoest=5  if (nivel==2 | nivel==3) & ultgrado==5
 replace anoest=6  if (nivel==2 | nivel==3) & ultgrado==6
 replace anoest=7  if ((nivel==3) & ultgrado==7) | ((nivel==5 | nivel==7) & ultgrado==1)
 replace anoest=8  if ((nivel==3) & ultgrado==8) | ((nivel==5 | nivel==7) & ultgrado==2)
 replace anoest=9  if ((nivel==5 | nivel==7) & ultgrado==3) | ((nivel==6 | nivel==8) & ultgrado==1)
 replace anoest=10 if ((nivel==5 | nivel==7) & ultgrado==4) | ((nivel==6 | nivel==8) & ultgrado==2)
 replace anoest=11 if ((nivel==5 | nivel==7) & ultgrado==5) | ((nivel==6 | nivel==8) & ultgrado==3)
 replace anoest=12 if ((nivel==5 | nivel==7) & ultgrado==6) | ((nivel==6 | nivel==8) & ultgrado==4)
 replace anoest=13 if (nivel==8 & ultgrado==5) | ((nivel>=9 & nivel<=14) & ultgrado==1) 
 replace anoest=14 if ((nivel>=9 & nivel<=14) & ultgrado==2)
 replace anoest=15 if ((nivel>=9 & nivel<=14) & ultgrado==3) 
 replace anoest=16 if ((nivel>=10 & nivel<=14) & ultgrado==4)
 replace anoest=17 if ((nivel>=12 & nivel<=14) & ultgrado==5)  | (nivel==15 & ultgrado==5)
 replace anoest=18 if ((nivel>=13 & nivel<=15) & ultgrado==6)
 replace anoest=19 if ((nivel>=13 & nivel<=15) & ultgrado==7)
 replace anoest=20 if nivel==15 & ultgrado==8
 replace anoest=21 if nivel==15 & ultgrado==9

 tab anoest esc,missing /* Esc= Escolaridad */
 tab anoest esc if edad>=15, missing


** Economic Active Population 

* For the population with 15 years or more of age.
* 1. Ocupado	2. Desocupado

 gen     activi=1 if (o1==1 | o2==1) & edad>=15
 replace activi=2 if o3==1           & edad>=15
 replace activi=3 if activ==.        & edad>=15

 tab activi [w=factor]
 tab activi [w=factor] if incl==1 
 
 rename activi peaa

 gen     tasadeso=0 if peaa==1 
 replace tasadeso=1 if peaa==2 
************************
*** MDGs CALCULATION ***
************************

*** GOAL 2. ACHIEVE UNIVERSAL PRIMARY EDUCATION
* ISCED 1

 gen     NERP=0 if (edad>=6 & edad<=11) & (asiste==1 | asiste==2)
 replace NERP=1 if (edad>=6 & edad<=11) & (asiste==1) & (nivel==1 | (nivel==3 & (ultgrado>=1 & ultgrado<=5)))

** Target 3, Additional Indicator: Net Attendance Ratio in Secondary
* ISCED 2 & 3

 gen     NERS=0 if (edad>=12 & edad<=17) & (asiste==1 | asiste==2)
 replace NERS=1 if (edad>=12 & edad<=17) & (asiste==1) & ((nivel==3 & (ultgrado>=6 & ultgrado<=8)) | ((nivel==6 | nivel==8) & (ultgrado>=1 & ultgrado<=3)))

** Upper secondary
* Tasa de Neta de Matrícula en la Enseñanza Media

 gen     NERS2=0 if (edad>=14 & edad<=17) & (asiste==1 | asiste==2)
 replace NERS2=1 if (edad>=14 & edad<=17) & (asiste==1) & ((nivel==3 & ultgrado==8) | ((nivel==6 | nivel==8) & (ultgrado>=1 & ultgrado<=3)))

** Target 3, Indicator: Literacy Rate of 15-24 Years Old
* At least 5 years of formal education

 gen     ALFABET=0 if (edad>=15 & edad<=24) & (anoest>=0 & anoest<99) 
 replace ALFABET=1 if (edad>=15 & edad<=24) & (anoest>=5 & anoest<99)

** Target 3, Indicator: Literacy Rate of 15-24 Years Old
* Knows how to read & write

 gen     ALFABET2=0 if (edad>=15 & edad<=24) & (alfabet==1 | alfabet==2)
 replace ALFABET2=1 if (edad>=15 & edad<=24) & (alfabet==1) 

*** GOAL 3 PROMOTE GENDER EQUALITY AND EMPOWER WOMEN

 gen prim=1 if asiste==1 & (nivel==1 | (nivel==3 & (ultgrado>=1 & ultgrado<=5)))
 gen sec=1 if  asiste==1 & (((nivel==3) & (ultgrado>=6 & ultgrado<=8)) | ((nivel==6 | nivel==8) & (ultgrado>=1 & ultgrado<=3)))
 gen ter=1 if  asiste==1 & ((nivel==13 | nivel==9 | nivel==11) |  ((nivel==6 | nivel==8) & ultgrad>=4))

** Target 4, Indicator: Ratio Girls to boys in primary, secondary and tertiary (%)

** Target 4, Ratio of Girls to Boys in Primary*

 gen RPRIMM=1 if (prim==1) & sexo==2 
 replace RPRIMM=0 if RPRIMM==. 
 gen RPRIMH=1 if (prim==1) & sexo==1 
 replace RPRIMH=0 if RPRIMH==.

 gen RATIOPRIM=0 if     (prim==1) & sexo==2  
 replace RATIOPRIM=1 if (prim==1)  & sexo==1   
	
** Target 4, Ratio of Girls to Boys in Secondary*

 gen RSECM=1 if (sec==1) & sexo==2 
 replace RSECM=0 if RSECM==.
 gen RSECH=1 if (sec==1) & sexo==1 
 replace RSECH=0 if RSECH==.

 gen RATIOSEC=0     if (sec==1) & sexo==2 
 replace RATIOSEC=1 if (sec==1) & sexo==1  
	
** Target 4, Indicator: Ratio of Girls to Boys in Tertiary*

 gen RTERM=1 if (ter==1) & sexo==2 
 replace RTERM=0 if RTERM==.
 gen RTERH=1 if (ter==1) & sexo==1 
 replace RTERH=0 if RTERH==.

 gen RATIOTER=0     if (ter==1) & sexo==2 
 replace RATIOTER=1 if (ter==1) & sexo==1  

** Target 4, Indicator: Ratio of Girls to Boys in Primary, Secondary and Tertiary*

 gen RALLM=1 if (prim==1 | sec==1 | ter==1) & sexo==2 
 replace RALLM=0 if RALLM==.
 gen RALLH=1 if (prim==1 | sec==1 | ter==1) & sexo==1 
 replace RALLH=0 if RALLH==.

 gen RATIOALL=0 if     (prim==1 | sec==1 | ter==1) & sexo==2  
 replace RATIOALL=1 if (prim==1 | sec==1 | ter==1) & sexo==1    

** Target 4, Indicator: Ratio of literate women to men 15-24 year olds*
* Knows how to read & write

 gen MA2=1 if ((alfabet==1) & (edad>=15 & edad<=24) & (sexo==2)) 
 replace MA2=0 if MA2==.
 gen HA2=1 if ((alfabet==1) & (edad>=15 & edad<=24) & (sexo==1)) 
 replace HA2=0 if HA2==.

 gen RATIOLIT2=0     if ((alfabet==1) & (edad>=15 & edad<=24) & (sexo==2)) 
 replace RATIOLIT2=1 if ((alfabet==1) & (edad>=15 & edad<=24) & (sexo==1)) 

** Target 4, Indicator: Ratio of literate women to men 15-24 year olds*
* At least 5 years of formal education

 gen MA=1 if ((anoest>=5 & anoest<99) & (edad>=15 & edad<=24) & (sexo==2)) 
 replace MA=0 if MA==.
 gen HA=1 if ((anoest>=5 & anoest<99) & (edad>=15 & edad<=24) & (sexo==1)) 
 replace HA=0 if HA==.

 gen RATIOLIT=0 if     ((anoest>=5 & anoest<99) & (edad>=15 & edad<=24) & (sexo==2)) 
 replace RATIOLIT=1 if ((anoest>=5 & anoest<99) & (edad>=15 & edad<=24) & (sexo==1)) 

** Target 4, Indicator: Share of women in wage employment in the non-agricultural sector (%)

* Without domestic Service
* INCL==1 ==> Excludes nucleo 0

 gen     WENAS=0 if incl==1 & ((edad>=15 & edad<=64) & ((categ>=3 & categ<=5) | categ==9) & (rama>=2 & rama<=9))
 replace WENAS=1 if incl==1 & ((edad>=15 & edad<=64) & ((categ>=3 & categ<=5) | categ==9) & (rama>=2 & rama<=9) & (sexo==2))

* RURAL AREAS ARE NOT PRESENTED FOR THIS INDICATOR
 
** Target 4, Indicator: Share of women in wage employment in the non-agricultural sector (%)
* With domestic servants
* INCL==1 ==> Excludes nucleo 0

 gen     WENASD=0 if incl==1 & ((edad>=15 & edad<=64) & ((categ>=3 & categ<=7) | categ==9) & (rama>=2 & rama<=9)) 
 replace WENASD=1 if incl==1 & ((edad>=15 & edad<=64) & ((categ>=3 & categ<=7) | categ==9) & (rama>=2 & rama<=9) & (sexo==2))

*** GOAL 7 ENSURE ENVIROMENTAL SUSTAINABILITY

** Access to Electricity ** Additional Indicator

* Gender classification of the population refers to the head of the household.

 gen     ELEC=0 if (v15>=1 & v15<=6) /* Total population excluding missing information */
 replace ELEC=1 if (v15>=1 & v15<=5)

** Target 9, Indicator: Proportion of the population using solidfuels (%)

* NA

** Target 10, Indicator: Proportion of the population with sustainable access to an improved water source (%)

* Gender classification of the population refers to the head of the household.

 gen     WATER=0 if (agua>=1 & agua<=6) /* Total population excluding missing information */
 replace WATER=1 if (agua>=1 & agua<=4) 

** Target 10, Indicator: Proportion of Population with Access to Improved Sanitation, Urban and Rural (%)

* Gender classification of the population refers to the head of the household.

 gen     SANITATION=0 if (servsani>=1 & servsani<=7) /* Total population excluding missing information */
 replace SANITATION=1 if (servsani>=1 & servsani<=2)

** Target 11, Indicator: Proportion of the population with access to secure tenure (%)

* PERSONS PER ROOM

 egen nrocuart_hog1=rsum(v4 v5 v6 v7), missing
 egen nrocuart_hogrest=rsum(v27 v28 v29 v30), missing
 recode nrocuart_hogrest (0=.)

 gen nrocuart=nrocuart_hog1 if v25==1
 replace nrocuart=nrocuart_hogrest if (v25>=2 & v25<=9)

 gen persroom=numper/nrocuart

* Indicator components

* 1. Non secure tenure or type of dwelling.

 gen     secten_1=0 if ((tenencia>=1 & tenencia<=10) & (tipoviv>=1 & tipoviv<=8)) /* Total population excluding missing information */
 replace secten_1=1 if ((tenencia>=7 & tenencia<=10) | (tipoviv>=6 & tipoviv<=8))

* 2. Low quality of the floor or walls materials.

 gen     secten_2=0 if ((pared>=1 & pared<=8) & (piso>=1 & piso<=5)) /* Total population excluding missing information */
 replace secten_2=1 if ((pared==5 | pared==7 | pared==8) | (piso==5)) 

* 3. Crowding (defined as not more than two people sharing the same room)

 gen secten_3=1     if (persroom>2) 

* 4. Lack of basic services

 gen secten_4=1	    if (SANITATION==0 | WATER==0)

* Gender classification of the population refers to the head of the household.

 gen     SECTEN=1 if (secten_1>=0 & secten_1<=1) & (secten_2>=0 & secten_2<=1) /* Total population excluding missing information */
 replace SECTEN=0 if (secten_1==1 | secten_2==1 | secten_3==1 | secten_4==1)

* Dirt floors ** Additional indicator
* 9.a Material predominante en el piso de la vivienda

* Gender classification of the population refers to the head of the household.

 gen     DIRT=0 if (piso>=1 & piso<=5) /* Total population excluding missing information */
 replace DIRT=1 if (piso==5)

** GOAL 8. DEVELOP A GLOBAL PARTNERSHIP FOR DEVELOPMENT

** Target 16, Indicator: Unemployment Rate of 15 year-olds (%)
* INCL==1 ==> Excludes nucleo 0

 gen     UNMPLYMENT15=0 if incl==1 & (edad>=15 & edad<=24) & (tasadeso==0 | tasadeso==1) 
 replace UNMPLYMENT15=1 if incl==1 & (edad>=15 & edad<=24) & (tasadeso==1) 

* Telephone Lines and Cellular Subscribers 

* Fixed Line
* Household head

 gen     tel=1 if p3==1 & pco1==1
 replace tel=0 if tel==. & p3!=9 & pco1==1

 egen telefono=max(tel), by(id_hogar)

* Cellular
* Any household member with cellular service

 gen     cel=1 if (p8==1) 
 replace cel=0 if cel==. & p8!=9 

 egen celular=max(cel), by(id_hogar)
 
* Gender classification of the population refers to the head of the household.

 gen     TELCEL=0 if (telefono>=0 & telefono<=1) & (celular>=0 & celular<=1) /* Total population excluding missing information */
 replace TELCEL=1 if (telefono==1 | celular==1) 


** FIXED LINES

* Gender classification of the population refers to the head of the household.

 gen     TEL=0 if (telefono>=0 & telefono<=1) /* Total population excluding missing information */
 replace TEL=1 if (telefono==1) 

** CEL LINES

* Gender classification of the population refers to the head of the household.

 gen     CEL=0 if (celular>=0 & celular<=1) /* Total population excluding missing information */
 replace CEL=1 if (celular==1)

** Target 18, Indicator: "Personal computers in use per 100 population"

* Computers
* Household head

 gen     comp=1 if p6==1 & pco1==1
 replace comp=0 if comp==. & p6!=9 & pco1==1

 egen computador=max(comp), by(id_hogar)

* Gender classification of the population refers to the head of the household.

 gen     COMPUTER=0 if (computador>=0 & computador<=1) /* Total population excluding missing information */
 replace COMPUTER=1 if (computador==1)

* Target 18, Indicator: "Internet users per 100 population"

** Internet access 
* Household head

* Conexión a internet  

 gen     inte=1 if p7==1 & pco1==1
 replace inte=0 if inte==. & p7!=9 & pco1==1

 egen internet=max(inte), by(id_hogar)

 gen     INTUSERS=0 if (internet>=0 & internet<=1) /* Total population excluding missing information */
 replace INTUSERS=1 if (internet==1)

************************************************************************
**** ADDITIONAL SOCIO - ECONOMIC COMMON COUNTRY ASESSMENT INDICATORS ****
************************************************************************

** CCA 19. Proportion of children under 15 who are working
* INCLUDES POPULATION 12 TO 15 YEARS-OLD
* INCL==1 ==> Excludes nucleo 0

 gen     CHILDREN=0 if incl==1 & (edad>=12 & edad<=14) 
 replace CHILDREN=1 if incl==1 & ((edad>=12 & edad<=14) & (o1==1 | o2==1))

** CCA 41 Number of Persons per Room*

 generate PERSROOM2=persroom if pco1==1


 gen     popinlessthan2=1 if persroom<=2
 replace popinlessthan2=0 if popinlessthan2==.

* Gender classification of the population refers to the head of the household.

 gen     PLT2=0 if persroom<.	/* Total population excluding missing information */
 replace PLT2=1 if (popinlessthan2==1)

gen     DISCONN=0 if (edad>=15 & edad<=24)
replace DISCONN=1 if (edad>=15 & edad<=24) & (o7>=8 & o7<=10)


*** Proportion of population below corresponding grade for age

 gen     rezago=0       if (anoest>=0 & anoest<99)  & edad==6 /* This year of age is not included in the calculations */
	 
 replace rezago=1 	if (anoest>=0 & anoest<1 )  & edad==7
 replace rezago=0 	if (anoest>=1 & anoest<99)  & edad==7
 
 replace rezago=1 	if (anoest>=0 & anoest<2 )  & edad==8
 replace rezago=0	if (anoest>=2 & anoest<99)  & edad==8

 replace rezago=1 	if (anoest>=0 & anoest<3 )  & edad==9
 replace rezago=0	if (anoest>=3 & anoest<99)  & edad==9

 replace rezago=1 	if (anoest>=0 & anoest<4 )  & edad==10
 replace rezago=0	if (anoest>=4 & anoest<99)  & edad==10

 replace rezago=1 	if (anoest>=0 & anoest<5 )  & edad==11
 replace rezago=0	if (anoest>=5 & anoest<99)  & edad==11

 replace rezago=1	if (anoest>=0 & anoest<6)   & edad==12
 replace rezago=0	if (anoest>=6 & anoest<99)  & edad==12

 replace rezago=1 	if (anoest>=0 & anoest<7)   & edad==13
 replace rezago=0	if (anoest>=7 & anoest<99)  & edad==13

 replace rezago=1 	if (anoest>=0 & anoest<8)   & edad==14
 replace rezago=0	if (anoest>=8 & anoest<99)  & edad==14

 replace rezago=1 	if (anoest>=0 & anoest<9 )  & edad==15
 replace rezago=0	if (anoest>=9 & anoest<99)  & edad==15

 replace rezago=1 	if (anoest>=0  & anoest<10) & edad==16
 replace rezago=0	if (anoest>=10 & anoest<99) & edad==16

 replace rezago=1 	if (anoest>=0  & anoest<11) & edad==17
 replace rezago=0	if (anoest>=11 & anoest<99) & edad==17

* Primary and Secondary [ISCED 1, 2 & 3]

 gen     REZ=0 if (edad>=7 & edad<=17) & (rezago==1 | rezago==0)
 replace REZ=1 if (edad>=7 & edad<=17) & (rezago==1)

* Primary completion rate [15 - 24 years of age]

 gen     PRIMCOMP=0 if (edad>=15 & edad<=24) & (anoest>=0  & anoest<99)
 replace PRIMCOMP=1 if (edad>=15 & edad<=24) & (anoest>=6  & anoest<99)

* Average years of education of the population 15+

 gen     AEDUC_15=anoest if ((edad>=15 & edad<.) & (anoest>=0 & anoest<99))


 gen     AEDUC_15_24=anoest if ((edad>=15 & edad<=24) & (anoest>=0 & anoest<99))

 gen     AEDUC_25=anoest if ((edad>=25 & edad<.) & (anoest>=0 & anoest<99))

 gen GFA=(anoest/(edad-6)) if (edad>=7 & edad<=17) & (anoest>=0 & anoest<99)

* Grade for age primary

 gen GFAP=(anoest/(edad-6)) if (edad>=7 & edad<=11) & (anoest>=0 & anoest<99)

* Grade for age Secondary

 gen GFAS=(anoest/(edad-6)) if (edad>=12 & edad<=17) & (anoest>=0 & anoest<99)



/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/

*********
*lp_ci***
*********

gen lp_ci =.
replace lp_ci= 40562     if zona_c==1  /*urbana*/
replace lp_ci= 27328     if zona_c==0	/*rural*/
label var lp_ci "Linea de pobreza oficial del pais"

*********
*lpe_ci***
*********

gen lpe_ci =.
replace lpe_ci= 20281     if zona_c==1  /*urbana*/
replace lpe_ci= 15616     if zona_c==0	/*rural*/
label var lpe_ci "Linea de indigencia oficial del pais"

****************
*cotizando_ci***
****************
gen cotizando_ci=.
replace cotizando_ci=1 if (o17 >= 1 & o17 <= 5)
recode cotizando_ci .=0 if (condocup_ci==1 | condocup_ci==2)
label var cotizando_ci "Cotizante a la Seguridad Social"

****************
*afiliado_ci****
****************
gen afiliado_ci=.	
replace afiliado_ci=1 if (o17 >= 1 & o17<= 6)
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
replace instcot_ci=o17 if o17<=5
label var instcot_ci "Institucion a la que cotiza - variable original de cada pais" 


*****************
*tipocontrato_ci*
*****************
/*
gen tipocontrato_ci=.
replace tipocontrato_ci=1 if o15==1 & categopri_ci==3
replace tipocontrato_ci=2 if o15==2 & categopri_ci==3
replace tipocontrato_ci=3 if o11 ==3 & categopri_ci==3
label var tipocontrato_ci "Tipo de contrato segun su duracion"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci
*/	
* Corregido por la variable de firmo o no firmo y no por el tipo de trabajo MGD 06/16/2014	
gen tipocontrato_ci=.
replace tipocontrato_ci=1 if o11==1 & categopri_ci==3
replace tipocontrato_ci=2 if (o11==2 | o11==3) & categopri_ci==3
replace tipocontrato_ci=3 if (o11>=4 | tipocontrato_ci==.) & categopri_ci==3
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

gen tamemp_ci=1 if tamest=="A" | tamest=="B" 
replace tamemp_ci=2 if tamest=="C" | tamest=="D"
replace tamemp_ci=3 if tamest=="E" | tamest=="F"

label var tamemp_ci "# empleados en la empresa segun rangos"
label define tamemp_ci 1 "Pequena" 2 "Mediana" 3 "Grande"
label value tamemp_ci tamemp_ci

*************
**pension_ci*
*************
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
*egen auxpens=rsum(ypasaj)
egen auxpens=rsum(ypa1aj ypa2aj ypa3aj), missing
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



*************
**salmm_ci***
*************
* CHL 2000
gen salmm_ci= 	100000
label var salmm_ci "Salario minimo legal"

*************
***tecnica_ci**
*************

gen tecnica_ci=.
replace tecnica_ci=1 if nivel==9 | nivel==10
recode tecnica_ci .=0 
label var tecnica_ci "1=formacion terciaria tecnica"


**************
**categoinac_ci*
****************


gen categoinac_ci=1 if o7==5
replace categoinac_ci=2 if o7==4
replace categoinac_ci=3 if o7==1
replace categoinac_ci=4 if o7==2 | o7==3 | o7==6 | o7==7 | o7==8 | o7==9 | o7==10


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
gen region_c=.

*YL -> elimino var comp para que no genere problemas al SOCIOMETERO (esta var no es necesaria)
drop comp


/*_____________________________________________________________________________________________________*/
* Asignación de etiquetas e inserción de variables externas: tipo de cambio, Indice de Precios al 
* Consumidor (2011=100), Paridad de Poder Adquisitivo (PPA 2011),  líneas de pobreza
/*_____________________________________________________________________________________________________*/


do "$ruta\harmonized\_DOCS\\Labels&ExternalVars_Harmonized_DataBank.do"

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




compress


saveold "`base_out'", replace


log close




