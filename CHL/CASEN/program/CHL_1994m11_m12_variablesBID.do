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
 


global ruta = "\\Sdssrv03\surveys"

local PAIS CHL
local ENCUESTA CASEN
local ANO "1994"
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

***** revision July 29,2005  Suzanne
*removed condition (& edad_ci<18) froom the following two lines:

*by idh_ch: egen byte nhijos_ch=sum((relacion_ci==3) & edad_ci<18)
*by idh_ch: egen byte notropari_ch=sum((relacion_ci==4) & edad_ci>=18)

******* revision June 8 2006 MFP
*removed desemp1 and desemp2 because the reference period of 2 months implies that those variables
*can't be created.
*Now desemp3== old definition of desemp1

*previous code:
*gen desemp1_ci=(o1==2 & o2==2 & o3==1) *El periodo de referencia de la encuesta es de dos meses*
*gen desemp2_ci=(desemp1_ci |(o1==2 & o2==2 & o3==2 & (o4==6 | o4==7)))
*gen desemp3_ci=.
***

/*** revision October 16 2006 (Victoria)
The code for the education dummies was changed in order to make it
comparable with the following years and also to make the returns
to education coherent. 
Old code can be seen in the "VARIABLES EDUCATIVAS" sector
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

****************************************************************************/


use `base_in', clear

		**********************************
		***VARIABLES DEL IDENTIFICACION***
		**********************************
		
	****************
	* region_BID_c *
	****************
	
gen region_BID_c=4


******************
*VARIABLES DEL HOGAR
*******************
gen factor_ch=expr

egen idh_ch=group(r p c z seg f)
*rename ymonaj ymoneaj

gen idp_ci=o
gen zona_c=z
replace zona_c=0 if z==2
gen pais_c="CHL"
gen anio_c=1994
gen mes_c=.

*rename pc01 pco1

gen relacion_ci=pco1
replace relacion_ci=4 if pco1>=4 & pco1<=8
replace relacion_ci=5 if pco1==9


/*************************************
VARIABLES DE INFRAESTRUCTURA DEL HOGAR
**************************************/

* Modificaciones Marcela Rubio Septiembre 2014: variable aguared_ch habia sido generada como missing
gen aguared_ch=(v14<=2)

gen aguadist_ch=.
gen aguamala_ch=.
gen aguamide_ch=.
gen luz_ch=.
gen luzmide_ch=.
gen combust_ch=.

* Modificaciones Marcela Rubio Septiembre 2014: variable bano_ch habia sido generada como missing
gen bano_ch= (v9!=0)

gen banoex_ch=.

* Modificaciones Marcela Rubio Septiembre 2014: variables des1_ch y des2_ch habian sido generadas como missing
gen des1_ch=.
replace des1_ch = 0 if bano_ch==0 | v15==7
replace des1_ch = 1 if v15== 1 | v15==2
replace des1_ch = 2 if v15== 3 | v15==4
replace des1_ch = 3 if v15== 5 | v15==6

gen des2_ch=.
replace des2_ch = 0 if bano_ch==0 | v15==7
replace des2_ch = 1 if v15== 1 | v15==2 | v15== 3 | v15==4
replace des2_ch = 2 if v15== 5 | v15==6

gen piso_ch=.
gen pared_ch=.
gen techo_ch=.
gen resid_ch=.

 **Daniela Zuluaga- Enero 2018: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**
	
 *********************
 ***aguamejorada_ch***
 *********************
g       aguamejorada_ch = 1 if (v13 >=1 & v13 <=4)
replace aguamejorada_ch = 0 if  v14 == 3

 *********************
 ***banomejorado_ch***
 *********************
g       banomejorado_ch = 1 if  (v15 >=1 & v15 <=4) 
replace banomejorado_ch = 0 if  (v15 >=5 & v15 <=7) 
		
gen dorm_ch=v26
* Modificaciones Marcela Rubio Septiembre 2014
*gen cuartos_ch=v29
egen piezaviv=rsum(v4 v5 v6 v8 v9 v10), missing
replace piezaviv=. if v4==. & v5==. & v6==. & v8==. & v9==. & v10==.
gen cuartos_ch=piezaviv  
gen cocina_ch=.
gen refrig_ch=.
gen freezer_ch=.
gen auto_ch=.
gen telef_ch=.
gen compu_ch=.
gen internet_ch=.
gen cel_ch=.
gen vivi1_ch=.
gen vivi2_ch=.
gen viviprop_ch=.
gen vivitit_ch=.
gen vivialq_ch=.

*sort idh_ch
*by idh_ch: egen yaimhaj=max(yaimaj)

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
gen civil_ci=1 if ecivil==6
replace civil_ci=2 if ecivil==1 | ecivil==2
replace civil_ci=3 if ecivil==3 | ecivil==4
replace civil_ci=4 if ecivil==5
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
*gen emp_ci=(o1==1 | (o1==2 & o2==1))


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


gen ocupa_ci=.
replace ocupa_ci=1 if (o5>=210 & o5<=348) & emp_ci==1
replace ocupa_ci=2 if (o5>=110 & o5<=131) & emp_ci==1
replace ocupa_ci=3 if (o5>=410 & o5<=422) & emp_ci==1
replace ocupa_ci=4 if ((o5>=910 & o5<=911) | (o5>=520 & o5<=523)) & emp_ci==1
replace ocupa_ci=5 if ((o5>=510 & o5<=521) | (o5>=910 & o5<=916)) & emp_ci==1
replace ocupa_ci=6 if ((o5>=610 & o5<=621) | (o5>=920 & o5<=922)) & emp_ci==1
replace ocupa_ci=7 if ((o5>=710 & o5<=834) | (o5>=930 & o5<=933))  & emp_ci==1
replace ocupa_ci=8 if o5==11 & emp_ci==1
replace ocupa_ci=9 if o5==999 & emp_ci==1

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
gen rama_ci=substr(string(o6),1,1)
destring rama_ci, replace
replace rama_ci=. if emp_ci==0 | rama_ci<=0
*/
gen rama_ci=.
replace rama_ci=1 if (o6>=100 & o6<=130)   & emp_ci==1
replace rama_ci=2 if (o6>=210 & o6<=290) & emp_ci==1
replace rama_ci=3 if (o6>=300 & o6<=390) & emp_ci==1
replace rama_ci=4 if (o6>=400 & o6<=490) & emp_ci==1
replace rama_ci=5 if o6==500 & emp_ci==1
replace rama_ci=6 if (o6>=600 & o6<=690) & emp_ci==1
replace rama_ci=7 if (o6>=700 & o6<=790) & emp_ci==1
replace rama_ci=8 if (o6>=800 & o6<=833) & emp_ci==1
replace rama_ci=9 if (o6>=900  & o6<=990) & emp_ci==1


gen horaspri_ci=o14
replace horaspri_ci=. if emp_ci==0 | o14==99


gen horastot_ci=horaspri_ci


gen ylmpri_ci=yopraj 
replace ylmpri_ci=. if  emp_ci==0


gen ylnmpri_ci=.
gen ylmsec_ci=.
gen ylnmsec_ci=.
gen ylnmotros_ci=.
gen nrylmpri_ci=(emp_ci==1 & ylmpri_ci==.)
replace nrylmpri_ci=. if emp_ci==0


gen ylm_ci=ytrabaj
/*La variable ytrabaj contiene: SALARIOS + bonificaciones, REMUNERACIONES EN ESPECIE, RETIROS DE
PRODUCTOS O MERCADERIAS, INGRESO POR OTROS TRABAJOS. Dado que es imposibles desagregarla, dejamos ylmpri_ci lo mas limpio 
posible y sumamos todas estas cosas al final, aun cuando una parte del mismo debiera ir a ylnm_ci. A partir del 92 ya se 
pueden realizar estas distinciones.*/
replace ylm_ci=. if ytrabaj==. | emp_ci==0


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
replace ynlm_ci=. if ynlm_ci<0
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
gen desemp2_ci=.
gen desemp3_ci=(o1==2 & o2==2 & o3==1) /*El periodo de referencia de la encuesta es de dos meses*/

gen pea1_ci=.
gen pea2_ci=.
gen pea3_ci=(emp_ci==1 | desemp3_ci==1)*/

gen desalent_ci=(o1==2 & o2==2 & o3==2 & o4==8)

gen subemp_ci=.
gen tiempoparc_ci=.
gen categopri_ci=.
replace categopri_ci=1 if o7==1
replace categopri_ci=2 if o7==2
replace categopri_ci=3 if o7>=3 & o7<=5
replace categopri_ci=4 if o7==6
replace categopri_ci=. if emp_ci==0
gen categosec_ci=.
/*
gen contrato_ci=(o8==1) /*Crear esta variable, para este año en particular, es un poco problemático, porque no tenemos cuestionario. Entonces, 
uno tendería a replicar los mismos patrones que en años anterioes (o8>=1 & o8<=3). Sin embargo cuando miramos el siguiente tabulado:
----------------------
       o8 |      Freq.
----------+-----------
        1 |     30,675
        2 |      9,966
        8 |      3,224
        9 |         37
----------------------
nos damos cuenta que esa suposición esta mal. Un educated guess nos diría que 1=si, 2=no y 8, 9=NS/NC. Por lo tanto, esta sera la clasificacion
que seguiremos. Aun mas, al hacer esto, las tasas de contrato se vuelven consistentes con el resto de los años*/
replace contrato_ci=. if emp_ci==0

gen segsoc_ci=(o11<5)
replace segsoc_ci=. if emp_ci==0 | o11==.

gen firmapeq_ci=0 if o9=="A" | o9=="B"
replace firmapeq_ci=1 if o9=="C" | o9=="D" | o9=="E" | o9=="F"
replace firmapeq_ci=. if o9=="X" | emp_ci==0
*cambio en 01/24/07*
ren firmapeq_ci tamfirma_ci
gen firmapeq_ci=1 if tamfirma_ci==0
replace firmapeq_ci=0 if tamfirma_ci==1
replace firmapeq_ci=. if emp_ci==0

drop tamfirma_ci*/

gen nempleos_ci=.



gen spublico_ci=.

/*******************
VARIABLES EDUCATIVAS
*******************/

gen byte aedu_ci=.
replace aedu_ci=. if e9==99 			       
replace aedu_ci=0 if e9==1 | e9==15   	   
replace aedu_ci=min(e8,8) if e9>=2 & e9<=3   
replace aedu_ci=. if e9==4                       

*** media;
/*** we have to remember that for media there was a split (6 for the old system or 8 years for the new system) 
by looking at the data this appears to have happened in 1954.  Those who are 42 and are just entering the media
are doing so with 6 (instead of 8) years of schooling

EDAD 		e8		e9		aedu_ci
<42		  1		 5,7		 +8
 42		  1		 5,7		 +6
 43		 1-2		 5,7		 +6
 44		 1-3		 5,7		 +6
 45		 1-4		 5,7		 +6
 >45 		 1-4		 5,7		 +6
**********/

**media humanistica incompleta
replace aedu_ci=min(e8+8, 12) if e9==5 
replace aedu_ci=min(e8+6, 12) if e9==5 & e8==1 & edad==42
replace aedu_ci=min(e8+6, 12) if e9==5 & (e8>=1 & e8<=2) & edad==43
replace aedu_ci=min(e8+6, 12) if e9==5 & (e8>=1 & e8<=3) & edad==44
replace aedu_ci=min(e8+6, 12) if e9==5 & (e8>=1 & e8<=4) & edad==45
replace aedu_ci=min(e8+6, 12) if e9==5 & edad>45

**media humanistica completa
replace aedu_ci=min(e8+8, 12) if e9==6 

**media tecnica Prof incompleta
replace aedu_ci=min(e8+8, 13) if e9==7 
replace aedu_ci=min(e8+6, 13) if e9==7 & e8==1 & edad==42
replace aedu_ci=min(e8+6, 13) if e9==7 & (e8>=1 & e8<=2) & edad==43
replace aedu_ci=min(e8+6, 13) if e9==7 & (e8>=1 & e8<=3) & edad==44
replace aedu_ci=min(e8+6, 13) if e9==7 & (e8>=1 & e8<=4) & edad==45
replace aedu_ci=min(e8+6, 13) if e9==7 & edad>45

**media tecnica Prof completa
replace aedu_ci=min(e8+8, 13) if e9==8 

replace aedu_ci=e8+12 if e9==9           
replace aedu_ci=e8+12 if e9==10          
replace aedu_ci=e8+12 if e9==11          
replace aedu_ci=e8+12 if e9==12          
replace aedu_ci=e8+12 if e9==13          
replace aedu_ci= e8+17 if e9==14        

/*
OLD CODE:
gen eduno_ci=(aedu_ci==0)
gen edupi_ci=(e9==2)
gen edupc_ci=(e9==3)
gen eduui_ci=(e9==9 | e9==11)
gen eduuc_ci=(e9==10 | e9==12 | e9==13 | e9==14)

gen edus1i_ci=((e9==5 & e8==1) | (e9==7 & e8==1))
gen edus1c_ci=((e9==5 & e8==2) | (e9==7 & e8==2))
gen edus2i_ci=((e9==5 & (e8==3 | e8<=5)) | (e9==7 & (e8>=3 | e8<=5)))
gen edus2c_ci=((e9==6 & (e8>=3 & e8<=6)) | (e9==8 & (e8>=4 | e8<=6)))
gen edusi_ci=(edus1i_ci==1 | edus1c_ci==1 | edus2i_ci==1)
gen edusc_ci=(edus2c_ci==1)
*/

*****************
***pqnoasis_ci***
*****************
*Modificado Mayra Sáenz Junio, 2016: antes se generaba como missing
gen pqnoasis_ci= e7

**************
*pqnoasis1_ci*
**************
**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**

g       pqnoasis1_ci = 1 if e7 ==4
replace pqnoasis1_ci = 2 if e7 ==5 | e7 ==16
replace pqnoasis1_ci = 3 if e7 ==7 | e7 ==13 | e7 ==14
replace pqnoasis1_ci = 4 if e7 ==9
replace pqnoasis1_ci = 5 if e7 ==6 | e7 ==8
replace pqnoasis1_ci = 7 if e7 ==10 
replace pqnoasis1_ci = 8 if e7 ==1  | e7 ==2  | e7 ==3
replace pqnoasis1_ci = 9 if e7 ==11 | e7 ==12 | e7 ==15 | e7 ==17 | e7 ==18

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
replace eduac_ci=0 if e9==11 | e9==12 | e9==13 | e9==14
replace eduac_ci=1 if e9==9 | e9==10
label variable eduac_ci "Superior universitario vs superior no universitario"


foreach var of varlist edu* {
replace `var'=. if  aedu_ci==.
}

gen repite_ci=.
gen repiteult_ci=.

gen edupub_ci=.



/*
save "X:\ARM\CHL\1994\Van_data\CHL1994.dta", replace
clear

infile using "X:\ARM\CHL\1994\Programs\Van_prog\asiste.dct"
compress
egen idh_ch=group(r p c z s f)
*egen idh_ch=group(r p c z seg f)
gen idpersox=o
recode edad 99=.
sort idh sexo edad
drop r p c z s f o
*drop r p c z seg f o
compress
save "X:\ARM\CHL\1994\Van_data\asiste.dta", replace
clear

use "X:\ARM\CHL\1994\Van_data\CHL1994.dta"
sort idh_ch sexo edad
/* we sort by sexo and edad because there was an error in the previous banana,
the parentco variable had only 1 digit, so the person 10, 11 or 12, it was considered  0,1 or 2. Now, the problem is fixed. '
We have also now included here the nucleo variable and the parentnu variable*/

merge idh_ch sexo edad using X:\ARM\CHL\1994\Van_data\asiste.dta
drop idp_ci
rename idpersox idp_ci
sort idh_ch idp_ci
*drop _merge
*/
** attend
gen byte asiste_ci=0
replace asiste_ci=1 if e3==1

** subtract a year of schooling for those who are currently enrolled
gen ban_aedu=aedu_ci
replace ban_aedu=aedu_ci-1 if asiste_ci==1 & aedu_ci!=. & aedu_ci!=0



/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/
*********
*lp_ci***
*********

gen lp_ci =.
replace lp_ci= 30100       if zona_c==1  /*urbana*/
replace lp_ci= 20295       if zona_c==0	/*rural*/
label var lp_ci "Linea de pobreza oficial del pais"

*********
*lpe_ci***
*********

gen lpe_ci =.
replace lpe_ci= 15050     if zona_c==1  /*urbana*/
replace lpe_ci= 11597     if zona_c==0	/*rural*/
label var lpe_ci "Linea de indigencia oficial del pais"

****************
*cotizando_ci***
****************
gen cotizando_ci=.
replace cotizando_ci=1 if (o11>= 1 & o11 <= 4)
recode cotizando_ci .=0 if o21==1 | o21==2
label var cotizando_ci "Cotizante a la Seguridad Social"

****************
*afiliado_ci****
****************
gen afiliado_ci=.	
replace afiliado_ci=1 if (o19 >= 1 & o19 <= 4)
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
replace instcot_ci=o11 if o11<=6
label var instcot_ci "Institucion a la que cotiza - variable original de cada pais" 


*****************
*tipocontrato_ci*
*****************
/*
gen tipocontrato_ci=.
replace tipocontrato_ci=1 if o8==1 & categopri_ci==3
replace tipocontrato_ci=2 if o8==2 & categopri_ci==3
replace tipocontrato_ci=3 if o8==4 & categopri_ci==3
label var tipocontrato_ci "Tipo de contrato segun su duracion"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci
*/	
* Corregido por la variable de firmo o no firmo y por el tipo de trabajo MGD 06/16/2014	
gen tipocontrato_ci=.
replace tipocontrato_ci=1 if (o8==1 & o12 ==1) & categopri_ci==3
replace tipocontrato_ci=2 if (o8==1 & (o12>=2 & o12<=6)) & categopri_ci==3
replace tipocontrato_ci=3 if (o8==2 | tipocontrato_ci==.) & categopri_ci==3
label var tipocontrato_ci "Tipo de contrato segun su duracion"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci
	
*************
*cesante_ci* 
*************
gen cesante_ci=1 if o16==1 & condocup_ci==2
replace cesante_ci=0 if o16==2 & condocup_ci==2
label var cesante_ci "Desocupado - definicion oficial del pais"	


**************
***tamemp_ci**
**************


gen tamemp_ci=1 if o9=="A" | o9=="B" | o9=="C"
replace tamemp_ci=2 if o9=="D" | o9=="E"
replace tamemp_ci=3 if o9=="F" | o9=="G"

label var tamemp_ci "# empleados en la empresa segun rangos"
label define tamemp_ci 1 "Pequena" 2 "Mediana" 3 "Grande"
label value tamemp_ci tamemp_ci

*************
**pension_ci*
*************
*egen auxpen=rsum(yjubaj)
*MLO: estas variables vienen en el modulo de ignresos complementario ajustado por CEPAL
egen auxpen=rsum(yjubaj yinvaj ymonaj), missing
gen pension_ci=1 if auxpen>0 & auxpen!=.
recode pension_ci .=0 
label var pension_ci "1=Recibe pension contributiva"

*************
**ypen_ci*
*************

gen ypen_ci=auxpen
replace ypen_ci=. if auxpen<0

label var ypen_ci "Valor de la pension contributiva"

***************
*pensionsub_ci*
***************

gen pensionsub_ci=0 
replace pensionsub_ci=1 if ypasaj>0 & ypasaj!=.
label var pensionsub_ci "1=recibe pension subsidiada / no contributiva"

*****************
**ypensub_ci*
*****************
destring ypasaj, replace
gen ypensub_ci=ypasaj
replace ypensub_ci=. if ypasaj<0

label var ypensub_ci "Valor de la pension subsidiada / no contributiva"

**********
**tc_ci***
**********
gen tc_ci=404.09


label var tc_ci "Tipo de cambio LCU/USD"

*************
**salmm_ci***
*************
* CHL 1994
gen salmm_ci= 	52150.00


label var salmm_ci "Salario minimo legal"

*************
***tecnica_ci**
*************

gen tecnica_ci=.
replace tecnica_ci =1 if e9==11 | e9==12
recode tecnica_ci .=0

label var tecnica_ci "1=formacion terciaria tecnica"


****************
**categoinac_ci*
****************


gen categoinac_ci=1 if o4==5
replace categoinac_ci=2 if o4==4
replace categoinac_ci=3 if o4==1
replace categoinac_ci=4 if o4==2 | o4==3 | o4==6 | o4==7 | o4==8 | o4==9

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
gen freez_ch=.
gen region_c=.

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

/*Homologar nombre del identificador de ocupaciones (isco, ciuo, etc.) y de industrias y dejarlo en base armonizada 
para análisis de trends (en el marco de estudios sobre el futuro del trabajo) */
rename o6 codindustria
rename o5 codocupa

compress



saveold "`base_out'", replace


log close

