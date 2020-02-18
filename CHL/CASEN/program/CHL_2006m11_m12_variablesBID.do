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
local ANO "2006"
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
Round: Noviembre- Diciembre
Autores: 
Modificación 2014: Mayra Sáenz - Email: mayras@iadb.org - saenzmayra.a@gmail.com
Versión 2009: Melisa Morales (MM)
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


/*(Melisa- mmorales June 1st 2009): 'asiste_ci' must be changed
gen byte asiste_ci=(e4==1) 
repalce asiste_ci=. if e4==.*/

*****************************************************

*************
* factor_ch *
*************
gen factor_ch=expr /*Esta es la expansion que se usa en todos los años anteriores. La provincial recien aparece en el 2000*/

*************
* idh_ch    *
*************

egen idh_ch=group(r z seg f) /* no tenemos folio */


*************
* idp_ch    *
*************

gen idp_ci=o

*************
* zona_c    *
*************

gen zona_c=z
replace zona_c=0 if z==2

*************
* pais_c    *
*************

gen pais_c="CHL"

*************
* anio_c    *
*************

gen anio_c=2006

*************
* mes_ch    *
*************

gen mes_c=11


***************
* relacion_ci *
***************
* Yanira Oviedo, Junio 2010: Esta pregunta tiene diferentes opciones de respuesta.  Se guarda la programación
* original y se propone una nueva

*gen relacion_ci=pco1
*replace relacion_ci=4 if pco1>=4 & pco1<=10
*replace relacion_ci=5 if pco1==11
*replace relacion_ci=6 if pco1==12

gen relacion_ci=1 if pco1==1
replace relacion_ci=2 if pco1==2
replace relacion_ci=3 if pco1==3 | pco1==4 | pco1==5
replace relacion_ci=4 if pco1>=6 & pco1<=12
replace relacion_ci=5 if pco1==13
replace relacion_ci=6 if pco1==14

label var relacion_ci "Relación de parentesco con el jefe"
label def relacion_ci 1"Jefe" 2"Conyuge" 3"Hijo/a" 4"Otros parientes" 5"Otros no parientes" 6"Servicio doméstico"
label val relacion_ci relacion_ci	


******************************************
* VARIABLES DE INFRAESTRUCTURA DEL HOGAR *
******************************************

***************
* aguared_ch  *
***************

gen aguared_ch=(v4==1 | v4==2 | v4==3)
replace aguared_ch=. if v4==.

***************
* aguadist_ch *
***************

gen aguadist_ch=v5
replace aguadist_ch=. if v5==9

***************
* aguamala_ch *
***************

gen aguamala_ch=(v4>=5 & v4<=6)
replace aguamala_ch=. if v4==9

***************
* aguamide_ch *
***************

gen aguamide_ch=(v4==1 |v4==2)
replace aguamide_ch=. if aguared_ch==.

***************
* luz_ch      *
***************

gen luz_ch=(v7a<=6)

***************
* luzmide_ch  *
***************

gen luzmide_ch=(v7a==1 | v7a==2)
replace luzmide_ch=. if luz_ch==0

***************
* combust_ch  *
***************

gen combust_ch=.

***************
* bano_ch     *
***************

gen bano_ch=((v3g!=0 & v16g==.) | v16g>0 & v16g<=3)

***************
* banoex_ch   *
***************

gen banoex_ch=(v16g==. | v16g<v3g)
replace banoex_ch=. if bano_ch==0 

***************
* des1_ch     *
***************

gen des1_ch=0 if bano_ch==0 | v6==7
replace des1_ch=. if v6==9
replace des1_ch=1 if v6==1 | v6==2
replace des1_ch=2 if v6==3 | v6==4
replace des1_ch=3 if v6==5 | v6==6

***************
* des2_ch     *
***************

gen des2_ch=des1_ch
replace des2_ch=. if des1_ch==3

***************
* piso_ch     *
***************

gen piso_ch=0 if v9a==5
replace piso_ch=1 if v9a<5

***************
* pared_ch    *
***************

gen pared_ch=0 if v8a>=4 & v8a<=7
replace pared_ch=1 if v8a<4
replace pared_ch=2 if v8a==8
replace pared_ch=. if v8a==9

***************
* techo_ch    *
***************

gen techo_ch=0 if v10a>=4 & v10a<=6
replace techo_ch=1 if v10a<4
replace techo_ch=. if v10a==9

***************
* resid_ch    *
***************

gen resid_ch=.

 **Daniela Zuluaga- Enero 2018: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**
	
 *********************
 ***aguamejorada_ch***
 *********************
g       aguamejorada_ch = 1 if (v4 >=1 & v4 <=4)
replace aguamejorada_ch = 0 if (v4 >=5 & v4 <=6) | v5 ==3

 *********************
 ***banomejorado_ch***
 *********************
g       banomejorado_ch = 1 if  (v6 >=1 & v6 <=4) 
replace banomejorado_ch = 0 if  (v6 >=5 & v6 <=7)

***************
* dorm_ch     *
***************

gen dorm_ch=v3a 

***************
* cuartos_ch  *
***************

egen piezaviv=rsum(v3a v3b v3c v3d v3e v3f v3g), missing
replace piezaviv=. if v3a==. & v3b==. & v3c==. & v3d==. & v3e==. & v3f==. & v3g==. 
egen piezahog=rsum(v16a v16b v16c v16d v16e v16f v16g), missing
replace piezahog=. if v16a==. & v16b==. & v16c==. & v16d==. & v16e==. & v16f==. & v16g==. 
gen cuartos_ch=piezaviv 

***************
* cuartos_ch  *
***************

gen cocina_ch=(v3f!=0)

sort idh_ch

***************
* telef_ch    *
***************

by idh_ch: egen telef_ch=sum(r10d==1)
replace telef_ch=1 if telef_ch>=1

***************
* refrig_ch   *
***************

by idh_ch: egen refrig_ch=sum(r10b==1)
replace refrig_ch=1 if refrig_ch>=1

***************
* freez_ch    *
***************

gen freez_ch=.

***************
* auto_ch     * 
***************
*Yanira Oviedo, Junio 2010: El programa no está cumpliendo con su finalidad. Se propone una instrucción nueva.
*by idh_ch: egen auto_ch=sum(r9a>=1) /* for 2006  */
*replace auto_ch=1 if auto_ch>=1

gen auto_ch=0
replace auto_ch=1 if r9a>=1 & r9a<9
replace auto_ch=. if r9a==. | r9a==9

***************
* compu_ch    * 
***************

by idh_ch: egen compu_ch=sum(r11==1)
replace compu_ch=1 if compu_ch>=1

***************
* compu_ch    * 
***************

by idh_ch: egen internet_ch=sum(r12a==1 | r12a==2 | r12a==3)
replace internet_ch=1 if internet_ch>=1

***************
* compu_ch    * 
***************

by idh_ch: egen cel_ch=sum(r13==1 | r13==2)
replace cel_ch=1 if cel_ch>=1

***************
* vivi1_ch    * 
*************** 

gen vivi1_ch=1 if v11==1 | v11==2
replace vivi1_ch=2 if v11==3
replace vivi1_ch=3 if v11>3

***************
* vivi2_ch    * 
*************** 

gen vivi2_ch=(vivi1_ch==1 | vivi1_ch==2)

***************
* viviprop_ch * 
*************** 

gen viviprop_ch=0 if v12==5 | v12==6
replace viviprop_ch=1 if v12==1 | v12==3
replace viviprop_ch=2 if v12==2 | v12==4
replace viviprop_ch=3 if v12==10
replace viviprop_ch=4 if v12>6 & v12<=9

***************
* vivitit_ch  * 
***************

gen vivitit_ch=.

***************
* vivialq_ch  * 
***************

gen vivialq_ch=. /* ??? */

***************
*vivialqimp_ch* 
***************

gen vivialqimp_ch=yaimhaj

/* new variables */

***************
* howner      * 
***************

gen howner=(viviprop_ch==1 | viviprop==2)
replace howner=. if viviprop_ch==.

***************
* floor       * 
***************

gen floor=(piso_ch==1)
replace floor=. if piso_ch==.


**************************
* VARIABLES DEMOGRAFICAS *
**************************

***************
* factor_ci   * 
***************

gen factor_ci=expr

***************
* sexo_ci     * 
***************

gen sexo_ci=sexo

***************
* edad_ci     * 
***************

gen edad_ci=edad

***************
* civil_ci    * 
***************

gen civil_ci=1 if ecivil==7
replace civil_ci=2 if ecivil==1 | ecivil==2
replace civil_ci=3 if ecivil==3 | ecivil==4 | ecivil==5
replace civil_ci=4 if ecivil==6
replace civil_ci=. if ecivil==9

***************
* jefe_ci     * 
***************

gen jefe_ci=(relacion_ci==1)

****************
* nconyuges_ch * 
****************

sort idh_ch
by idh_ch: egen byte nconyuges_ch=sum(relacion_ci==2) 

****************
* nhijos_ch    * 
****************

by idh_ch: egen byte nhijos_ch=sum(relacion_ci==3)

****************
* notropari_ch * 
****************

by idh_ch: egen byte notropari_ch=sum(relacion_ci==4)

******************
* notronopari_ch * 
******************

by idh_ch: egen byte notronopari_ch=sum(relacion_ci==5)

****************
* nempdom_ch   * 
****************

by idh_ch: egen byte nempdom_ch=sum(relacion_ci==6)

****************
* clasehog_ch  * 
****************

gen byte clasehog_ch=0
replace clasehog_ch=1 if nhijos_ch==0 & nconyuges_ch==0 & notropari_ch==0 & notronopari_ch==0 /*Unipersonal*/
replace clasehog_ch=2 if nhijos_ch>0 & notropari_ch==0 & notronopari_ch==0 /*Nuclear (child with or without spouse but without other relatives)*/
replace clasehog_ch=2 if nhijos_ch==0 & nconyuges_ch>0 & notropari_ch==0 & notronopari_ch==0 /*Nuclear (spouse with or without children but without other relatives)*/
replace clasehog_ch=3 if notropari_ch>0 & notronopari_ch==0 /*Ampliado*/
replace clasehog_ch=4 if ((nconyuges_ch>0 | nhijos_ch>0 | notropari_ch>0) & (notronopari_ch>0))/*Compuesto (some relatives plus non relative)*/
replace clasehog_ch=5 if nhijos_ch==0 & nconyuges_ch==0 & notropari_ch==0 & notronopari_ch>0 /*Corresidente*/

****************
* nmiembros_ch * 
****************

sort idh_ch
by idh_ch:egen byte nmiembros_ch=sum(relacion_ci>0 & relacion_ci<5) if relacion_ci~=6

****************
* nmayor21_ch  * 
****************

by idh_ch:egen byte nmayor21_ch=sum((relacion_ci>0 & relacion_ci<5) & (edad_ci>=21 & edad_ci<=98))

****************
* nmenor21_ch  * 
****************

by idh_ch:egen byte nmenor21_ch=sum((relacion_ci>0 & relacion_ci<5) & (edad_ci<21))

****************
* nmayor65_ch  * 
****************

by idh_ch:egen byte nmayor65_ch=sum((relacion_ci>0 & relacion_ci<5) & (edad_ci>=65))

****************
* nmwnor6_ch   * 
****************

by idh_ch:egen byte nmenor6_ch=sum((relacion_ci>0 & relacion_ci<5) & (edad_ci<6))

****************
* nmenor1_ch   * 
****************

by idh_ch:egen byte nmenor1_ch=sum((relacion_ci>0 & relacion_ci<5) & (edad_ci<1))

****************
* miembros_ci   * 
****************

gen miembros_ci=(relacion_ci<5)
label variable miembros_ci "Miembro del hogar"

*************************
*** VARIABLES DE RAZA ***
*************************

* MGR Oct. 2015: modificaciones realizadas en base a metodología enviada por SCL/GDI Maria Olga Peña

/*
ETNIA ¿En Chile, la ley reconoce ocho pueblos originarios o indígenas, ¿pertenece usted a alguno
de ellos? (Preg. 7)
t4:
           1 aymara
           2 rapa nui 
           3 quechua 
           4 mapuche 
           5 atacameño
           6 coya
           7 kawaskar
           8 yagan
           9 diaguita
          90 no pertenece a ningún pueblo indígena
          99 sin dato

*/

gen raza_ci=.
replace raza_ci= 1 if  (t4 >=1 & t4 <=9 )
replace raza_ci= 3 if (t4==90 | t4==99) & raza_ci==.
label define raza_ci 1 "Indígena" 2 "Afro-descendiente" 3 "Otros"
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


********************************
* VARIABLES DE DEMANDA LABORAL *
********************************

****************
****condocup_ci*
****************

gen condocup_ci=.
replace condocup_ci=1 if (o1==1 | o2==1 | o3==1)
replace condocup_ci=2 if ((o1==2 | o2==2 | o3==2) & (o4==1))
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
/*
****************
* emp_ci       * 
****************

gen emp_ci=(o1==1 | (o1==2 & o2==1))
*/
****************
* ocupa_ci     * 
****************
* Utiliza CIUO-88 (MGD 6/16/17)
gen ocupa_ci=.
replace ocupa_ci=1 if (c_o11>=2100 & c_o11<=3480) & emp_ci==1
replace ocupa_ci=2 if (c_o11>=1100 & c_o11<=1319) & emp_ci==1
replace ocupa_ci=3 if (c_o11>=4100 & c_o11<=4223) & emp_ci==1
replace ocupa_ci=4 if ((c_o11>=9100 & c_o11<=9113) | (c_o11>=5200 & c_o11<=5230)) & emp_ci==1
replace ocupa_ci=5 if ((c_o11>=5100 & c_o11<=5169) | (c_o11>=9100 & c_o11<=9162)) & emp_ci==1
replace ocupa_ci=6 if ((c_o11>=6100 & c_o11<=6210) | (c_o11>=9200 & c_o11<=9220)) & emp_ci==1
replace ocupa_ci=7 if ((c_o11>=7100 & c_o11<=8340) | (c_o11>=9300 & c_o11<=9333))  & emp_ci==1
replace ocupa_ci=8 if c_o11==110 & emp_ci==1
replace ocupa_ci=9 if c_o11==9999

label variable ocupa_ci "Ocupacion laboral"
label define ocupa_ci 1"profesional y tecnico" 2"director o funcionario sup" 3"administrativo y nivel intermedio"
label define ocupa_ci  4 "comerciantes y vendedores" 5 "en servicios" 6 "trabajadores agricolas", add
label define ocupa_ci  7 "obreros no agricolas, conductores de maq y ss de transporte", add
label define ocupa_ci  8 "FFAA" 9 "Otras", add
label value ocupa_ci ocupa_ci



****************
*   rama_ci    * 
****************
/*
gen rama_ci=substr(string(c_o12),1,1)
destring rama_ci, replace
replace rama_ci=. if emp_ci==0 | rama_ci<=0
*/
gen rama_ci=.
replace rama_ci=1 if (c_o12>=1000 & c_o12<=1499) & emp_ci==1
replace rama_ci=2 if (c_o12>=2000 & c_o12<=2999) & emp_ci==1
replace rama_ci=3 if (c_o12>=3000 & c_o12<=3999) & emp_ci==1
replace rama_ci=4 if (c_o12>=4000 & c_o12<=4999) & emp_ci==1
replace rama_ci=5 if (c_o12>=5000 & c_o12<=5999) & emp_ci==1
replace rama_ci=6 if (c_o12>=6000 & c_o12<=6999) & emp_ci==1
replace rama_ci=7 if (c_o12>=7000 & c_o12<=7999) & emp_ci==1
replace rama_ci=8 if (c_o12>=8000 & c_o12<=8999) & emp_ci==1
replace rama_ci=9 if (c_o12>=9000 & c_o12<=9990) & emp_ci==1

****************
* horaspri_ci  * 
****************

gen horaspri_ci=o15 /* horas semanales */

****************
* horastot_ci  * 
****************

gen horastot_ci=horaspri_ci

******************************************************************************
******************************************************************************
******************************************************************************

****************
* ylmpri_ci    * 
****************

gen ylmpri_ci=yopraj 
replace ylmpri_ci=. if emp_ci==0

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


****************
* ylnmpri_ci   * 
**************** 

gen ylnmpri_ci=.

****************
* ylmsec_ci    * 
**************** 

gen ylmsec_ci=.

****************
* ylnmsec_ci   * 
**************** 

gen ylnmsec_ci=.

****************
* ylnmotros_ci * 
**************** 

gen ylnmotros_ci=.


****************
* nrylmpri_ci  * 
**************** 

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

***NEW


****************
* ylm_ci       * 
**************** 

gen ylm_ci= ytrabaj /* As defined for Sociometro purposes */
replace ylm_ci=. if ytrabaj==. | emp_ci==0



***
/*
egen ylm1_ci=rsum(ylmpri1_ci ytrsaj ytroaj)
replace ylm1_ci=. if (ylmpri1_ci==. & ytrsaj==. & ytroaj==.) | emp_ci==0
egen ylm2_ci=rsum(ylmpri2_ci ytrsaj ytroaj)
replace ylm2_ci=. if (ylmpri2_ci==. & ytrsaj==. & ytroaj==.) | emp_ci==0
*/

*gen ylnm_ci=ylnmpri_ci

****************
* ylnm_ci      * 
**************** 

gen ylnm_ci=.

/*
OLD CODE:
egen ynlm_ci=rsum(yre1aj ymesaj yfajaj yamaaj ydesaj yah1aj yah2aj ydonaj yrutaj yre2aj yre3aj yoasaj yonaaj yfamaj yjubaj yinvaj ymonaj yorfaj yotpaj ypa1aj ypa2aj ypa3aj ysu1aj ysu2aj ysu3aj ysu4aj ysu5aj yce1aj yce2aj yce3aj yaguaj yas1aj yas2aj yas3aj yas4aj)
replace ynlm_ci=. if yre1aj==. & ymesaj==. & yfajaj==. & yamaaj==. &  ydesaj==. & yah1aj==. & yah2aj==. & ydonaj==. & yrutaj==. & yre2aj==. & yre3aj==. & yoasaj==. & yonaaj==. & yfamaj==. & yjubaj==. & yinvaj==. & ymonaj==. & yorfaj==. & yotpaj==. & ypa1aj==. & ypa2aj==. & ypa3aj==. & ysu1aj==. & ysu2aj==. & ysu3aj==. & ysu4aj==. & ysu5aj==. & yce1aj==. & yce2aj==. & yce3aj==. & yaguaj==. & yas1aj==. & yas2aj==. & yas3aj==. & yas4aj==.
*/

***NEW

****************
* ynlm_ci      * 
**************** 


gen yautaj1=yautaj
replace yautaj1=0 if yautaj==.

gen ysubaj1=ysubaj
replace ysubaj1=0 if ysubaj==.

* 2014, 01 Agregado MLO, no se estaba restando el ingreso laboral correctamente cueando la variable era missing
gen negylm=-ylm_ci
replace negylm=0 if ylm_ci==.

egen ynlm_ci = rsum(yautaj1 negylm ysubaj1), missing
replace ynlm_ci=. if yautaj==. & ylm_ci==. & ysubaj==. 

*gen ynlm_ci = yautaj1 - ylm_ci + ysubaj1
*replace ynlm_ci=. if yautaj==. & ylm_ci==. & ysubaj==. 


****************
* ynlnm_ci     * 
**************** 

gen ynlnm_ci=.

****************
* nrylmpri_ch  * 
**************** 
 
sort idh_ch 
by idh_ch: egen nrylmpri_ch=max(nrylmpri_ci) if miembros_ci==1
*by idh_ch: egen nrylmpri1_ch=max(nrylmpri1_ci) if miembros_ci==1, missing
*by idh_ch: egen nrylmpri2_ch=max(nrylmpri2_ci) if miembros_ci==1, missing

****************
* ylm_ch       * 
**************** 

by idh_ch: egen ylm_ch=sum(ylm_ci)if miembros_ci==1, missing
*

****************
* ylmpri_ch    * 
**************** 

by idh_ch: egen ylmpri_ch=sum(ylmpri_ci)if miembros_ci==1, missing
*
*by idh_ch: egen ylm1_ch=sum(ylm1_ci) if miembros_ci==1, missing
*by idh_ch: egen ylm2_ch=sum(ylm2_ci) if miembros_ci==1, missing

****************
* ylnm_ch      * 
**************** 

by idh_ch: egen ylnm_ch=sum(ylnm_ci)if miembros_ci==1, missing
gen ylmnr_ch=ylm_ch
replace ylmnr_ch=. if nrylmpri_ch==1


****************
* ylmprinr_ch  * 
**************** 

gen ylmprinr_ch=ylmpri_ch
replace ylmprinr_ch=. if nrylmpri_ch==1

*
/*
gen ylmnr1_ch=ylm1_ch
replace ylmnr1_ch=. if nrylmpri1_ch==1
gen ylmnr2_ch=ylm2_ch
replace ylmnr2_ch=. if nrylmpri2_ch==1
*/

****************
* ynlm_ch      * 
**************** 

by idh_ch: egen ynlm_ch=sum(ynlm_ci)if miembros_ci==1, missing

****************
* ynlnm_ch     * 
**************** 

gen ynlnm_ch=.

****************
* ylmhopri_ci  * 
**************** 

gen ylmhopri_ci=ylmpri_ci/(horaspri_ci*4.2)
replace ylmhopri_ci=. if ylmhopri_ci<=0

/*
gen ylmhopri1_ci=ylmpri1_ci/(horaspri_ci*4.2)
replace ylmhopri1_ci=. if ylmhopri1_ci<=0
gen ylmhopri2_ci=ylmpri2_ci/(horaspri_ci*4.2)
replace ylmhopri2_ci=. if ylmhopri2_ci<=0
*/

****************
* ylmho_ci     * 
**************** 

gen ylmho_ci=.

****************
* rentaimp_ch  * 
**************** 

gen rentaimp_ch=yaimhaj
*egen autocons_ci=rsum(autoc yac2aj)
*replace autocons_ci=. if autoc==. & yac2aj==.
*sort idh_ch
*by idh_ch: egen autocons_ch=sum(autocons_ci)

****************
* remesas_ci   * 
**************** 

gen remesas_ci=.

****************
* remesas_ch   * 
**************** 

gen remesas_ch=.

****************
* durades_ci   * 
**************** 

gen durades_ci=o7/4.3
replace durades_ci=. if o7==999 /*| activ!=2*/
label var durades_ci "Duración del desempleo"

****************
* antiguedad_ci* 
**************** 

*gen antiguedad_ci=(2003-o17) /* 017 for 2006 */

*Yanira Oviedo, Junio 2010: se estaba construyendo con referencia al 2003, esto se cambia y se eliminan los ns/nr
gen antiguedad_ci=(2006-o17)+1
g diff = antiguedad_ci-edad_ci
replace antiguedad_ci=edad_ci if diff==1
replace antiguedad_ci=. if diff>1 | o17==9999
drop diff

/*Hay una cita en una de las bananas originales en donde dicen que 
las entrevistas fueron realizadas casi finalizando el ciclo lectivo (y, como consecuencia, se consideraba que ese año se 
sumaba a aedu). Por lo tanto, podemos suponer que las entrevistas se realizaron en noviembre a los efectos de calcular le 
tenure.*/



*********************************
* VARIABLES DEL MERCADO LABORAL *
*********************************
/*
****************
* desemp1_ci   * 
**************** 

gen desemp1_ci=.

/*El periodo de referencia de la encuesta es de dos meses!*/

****************
* desemp2_ci   * 
**************** 

gen desemp2_ci=.

****************
* desemp3_ci   * 
**************** 

gen desemp3_ci=(o1==2 & o2==2 & o4==1) /* o4 for 2006 */

****************
* pea1_ci      * 
**************** 

gen pea1_ci=.

****************
* pea2_ci      * 
**************** 

gen pea2_ci=.

****************
* pea3_ci      * 
**************** 

gen pea3_ci=(emp_ci==1 | desemp3_ci==1)
*/
****************
* desalent_ci  * 
**************** 

*gen desalent_ci=(o1==2 & o2==2 & o3==2 & o7==8)
gen desalent_ci=(o1==2 & o2==2 & o4==2 & o6==14) /* o4 & o6== 14, o6 refers only for the last 4 weeks period! */

****************
* subemp_ci    * 
**************** 

gen subemp_ci=.

****************
*tiempoparc_ci * 
**************** 

/* o13a could work! */

gen tiempoparc_ci=.
replace tiempoparc_ci=1 if o23==2


****************
*categopri_ci  * 
**************** 

gen categopri_ci=.
replace categopri_ci=1 if o19==1
replace categopri_ci=2 if o19==2
replace categopri_ci=3 if o19>=3 & o19<=7
replace categopri_ci=4 if o19==8
replace categopri_ci=. if emp_ci==0
gen categosec_ci=.
/*
****************
* contrato_ci  * 
**************** 

gen contrato_ci=(o20>=1 & o20<=2)
replace contrato_ci=. if emp_ci==0

****************
* segsoc_ci    * 
**************** 

gen segsoc_ci=(o29<=5) /* o29 for 2006 */
replace segsoc_ci=. if emp_ci==0 | o29==9 /*Esta variable es solo para los empleados!!!: La pregunta 25 es para todas las personas, 
tengan empleo o no*/
*/
****************
* nempleos_ci  * 
**************** 

gen nempleos_ci=1 if o26==2 /* o26 for 2006 */
replace nempleos_ci=2 if o26==1
/*
****************
* firmapeq_ci  * 
**************** 

gen firmapeq_ci=1 if o13=="A" | o13=="B"
replace firmapeq_ci=0 if o13=="C" | o13=="D" | o13=="E" | o13=="F"
replace firmapeq_ci=. if o13=="X" | emp_ci==0*/

****************
* spublico_ci  * 
**************** 

/* Numero de empleados en el Servicio Publico??? o Empresas en el Servicio Publico??? */

* Mod MLO: incorporacion variable 10/2015
gen spublico_ci=(o19==3 | o19==4 | o19==9)
replace spublico_ci=. if emp_ci!=1

************************
* VARIABLES EDUCATIVAS *
************************

****************
* aedu_ci      * 
**************** 

*Yanira Julio 2010:
recode e8t (99=.)
recode e8c (99=.)
*


gen byte aedu_ci=.
*replace aedu_ci=0 if e8t==0 | e8t==1 | e8t==16 
replace aedu_ci=0 if e8t==1 | e8t==16 
replace aedu_ci=e8c if e8t==2 | e8t==3 /*El máximo es 6 u 8 dependiendo si es el sistema viejo (preparatoria) o el nuevo (basica)*/
replace aedu_ci=. if e8t==4 // Educación Especial
*We assume that 'e8t==4', Diferential Education, will be equivalent to missing

*NEW: 16 Oct 2006 (Victoria)
replace aedu_ci=6 if (e8c>=6 & e8t==2) 
replace aedu_ci=8 if (e8c>=8 & e8t==3) 

/*
 table e7c e8t, c(mean edad)
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
replace aedu_ci=e8c+6 if e8t==5 | e8t==7
replace aedu_ci=e8c+8 if e8t==6 | e8t==8
replace aedu_ci=e8c+12 if e8t>=9 & e8t<=14
*Mod. 8/2015 Ivan Bonacelli
*replace aedu_ci=e8c+17 if e8t==15
replace aedu_ci=e8c+12 if e8t==15
replace aedu_ci=. if e8t==99

** Generating attend. Dummy variable for school attendance
*Yanira Oviedo, Junio 2010: anteriormente se había tomado la variable e2 se corrige a e4
gen byte asiste_ci=(e4==1) 
label variable asiste_ci "Dummy variable for school attendance"

/*(Melisa- mmorales June 1st 2009): deberia ser
gen byte asiste_ci=(e4==1) 
replace asiste_ci=. if e4==.*/

* We substract one year of education for those who are attending school at the moment that the survey took place
*Cambio Ivan Bornacelly 10/06/2017
*gen ban_aedu=aedu_ci
*replace ban_aedu=aedu_ci-1 if aedu_ci!=0 & asiste_ci==1


/*
OLD CODE:
gen eduno_ci=(e8t==0 | e8t==1 | e7t==16)
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
*Modificado Mayra Sáenz Junio, 2016: antes se generaba como missing, la e6 es para personas de 7 a 40 años
* y la variable e5 es para niños de 0 a 6 años.


gen pqnoasis_ci= e6

**************
*pqnoasis1_ci*
**************
**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**

g       pqnoasis1_ci = 1 if e6 ==3
replace pqnoasis1_ci = 2 if e6 ==4
replace pqnoasis1_ci = 3 if e6 ==6 | e6 ==10 | e6 ==13 | e6 ==14
replace pqnoasis1_ci = 4 if e6 ==9
replace pqnoasis1_ci = 5 if e6 ==5 | e6 ==7 | e6 ==8
replace pqnoasis1_ci = 6 if e6 ==17
replace pqnoasis1_ci = 7 if e6 ==18 
replace pqnoasis1_ci = 8 if e6 ==1  | e6 ==2  
replace pqnoasis1_ci = 9 if e6 ==11 | e6 ==12 | e6 ==15 | e6 ==16 | e6 ==19 | e6 ==20

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

gen edupre_ci=(e8t==1)
label variable edupre_ci "Educacion preescolar"

***************
***asispre_ci***
***************
*Creación de la variable asistencia a preescolar por Iván Bornacelly - 01/12/17
	g asispre_ci=.
	replace asispre_ci=1 if e4==1 & e8t==1 & edad>=4
	recode asispre_ci (.=0)
	la var asispre_ci "Asiste a educacion prescolar"

**************
***eduac_ci***
**************
gen eduac_ci=.
replace eduac_ci=0 if e8t>=9 & e8t<=12
replace eduac_ci=1 if e8t>=13 & e8t<=15
label variable eduac_ci "Superior universitario vs superior no universitario"


foreach var of varlist edu* {
replace `var'=. if  aedu_ci==.
}

**************
**repite_ci***
**************

gen repite_ci=.

**************
*repiteult_ci*
**************
gen repiteult_ci=.

**************
*edupub_ci   *
**************

gen edupub_ci=.

***********************
*** CHILE 2003	    ***
***********************
 
 tab pco1
 tab nucleo
 tab pco1 nucleo
 tab pco1 nucleo if nucleo==0
 tab pco2 if pco1==12
 
 rename expr factor /* Expansión Regional */
 rename z area
 rename e1 alfabet
 * rename e2 asiste
 rename e8t nivel
 rename e8c ultgrado
 rename v12 tenencia
 rename v4 agua
 rename v5 lugabast
 rename v6 servsani
 rename v8a pared
 rename v9a piso
 rename v11 tipoviv
 rename o19 categ
* rename o13 tamest

 gen     incl=1 if (pco1>=1 &  pco1<=12)
 replace incl=0 if  pco1==12
 
** AREA

 tab area [w=factor]

** Gender classification of the population refering to the head of the household.

 sort seg f o

* Household ID

 gen x=1 if pco1==1 	
 gen id_hogar=sum(x)
 drop x

 gen     sexo_d_=1 if pco1==1 & sexo==1
 replace sexo_d_=2 if pco1==1 & sexo==2

 egen sexo_d=max(sexo_d_), by(id_hogar)
 
 tab sexo   [w=factor]
 tab sexo_d [w=factor]

 tab sexo sexo_d if pco1==1

** Years of education. 

* ESC => "Escolaridad": Years of education for the population with 15 years or more of age.

 tab nivel ultgrado if asiste==1 & (nivel==6 | nivel==8) & edad==17
 tab nivel ultgrado if asiste==1 & (nivel==6 | nivel==8) & edad==17 & esc==12

 gen 	 anoest=0  if  (nivel==4 | nivel==16 | nivel==1)
 replace anoest=1  if  (nivel==2 | nivel==3) & ultgrado==1
 replace anoest=2  if  (nivel==2 | nivel==3) & ultgrado==2
 replace anoest=3  if  (nivel==2 | nivel==3) & ultgrado==3
 replace anoest=4  if  (nivel==2 | nivel==3) & ultgrado==4
 replace anoest=5  if  (nivel==2 | nivel==3) & ultgrado==5
 replace anoest=6  if  (nivel==2 | nivel==3) & ultgrado==6
 replace anoest=7  if  (nivel==3 & ultgrado==7) | ((nivel==5 | nivel==7) & ultgrado==1)
 replace anoest=8  if  (nivel==3 & ultgrado==8) | ((nivel==5 | nivel==7) & ultgrado==2)
 replace anoest=9  if ((nivel==5 | nivel==7) & ultgrado==3) | ((nivel==6 | nivel==8) & ultgrado==1)
 replace anoest=10 if ((nivel==5 | nivel==7) & ultgrado==4) | ((nivel==6 | nivel==8) & ultgrado==2)
 replace anoest=11 if ((nivel==5 | nivel==7) & ultgrado==5) | ((nivel==6 | nivel==8) & ultgrado==3)
 replace anoest=12 if ((nivel==5 | nivel==7) & ultgrado==6) | ((nivel==6 | nivel==8) & ultgrado==4)
 replace anoest=13 if  (nivel==8 & ultgrado==5) | ((nivel>=9 & nivel<=11) & ultgrado==1) | (nivel==12 & ultgrado==2) | (nivel==13 & ultgrado==1)
 replace anoest=14 if ((nivel>=9 & nivel<=13) & ultgrado==2)
 replace anoest=15 if ((nivel>=9 & nivel<=13) & ultgrado==3) | (nivel==14 & ultgrado==3)
 replace anoest=16 if ((nivel>=10 & nivel<=14) & ultgrado==4)
 replace anoest=17 if ((nivel>=12 & nivel<=14) & ultgrado==5) | (nivel==15 & ultgrado==5)
 replace anoest=18 if ((nivel>=13 & nivel<=15) & ultgrado==6)
 replace anoest=19 if ((nivel>=13 & nivel<=15) & ultgrado==7)
 replace anoest=20 if   nivel==15 & ultgrado==8
 replace anoest=21 if   nivel==15 & ultgrado==9
 replace anoest=22 if   nivel==15 & ultgrado==10
 replace anoest=23 if   nivel==15 & ultgrado==11
 
 tab anoest esc,missing /* Esc= Escolaridad */
 tab anoest esc if edad>=15, missing

** Economic Active Population 

* Activ => Condición de actividad

* 1. Ocupado	2. Desocupado

 rename activ activ_orig
 
 gen     activ=1 if (o1==1 | o3==1) & edad>=15
 replace activ=2 if (o4==1)         & edad>=15
 replace activ=3 if activ==.        & edad>=15

 tab activ activ_orig, missing

 tab activ [w=factor]
 tab activ [w=factor] if incl==1 
 
 rename activ peaa
 
 gen     tasadeso=0 if peaa==1 
 replace tasadeso=1 if peaa==2 

************************
*** MDGs CALCULATION ***
************************

** For further information on this do file contact Pavel Luengas (pavell@iadb.org)


* INCL==1 ==> Excludes "S. Doméstico Puertas Adentro"

*** GOAL 2. ACHIEVE UNIVERSAL PRIMARY EDUCATION
* ISCED 1

 gen     NERP=0 if (edad>=6 & edad<=11) & (asiste==1 | asiste==2)
 replace NERP=1 if (edad>=6 & edad<=11) & (asiste==1) & (nivel==1 | (nivel==3 & (ultgrado>=1 & ultgrado<=5)))

** Target 3, Additional Indicator: Net Attendance Ratio in Secondary

 gen     NERS=0 if (edad>=12 & edad<=17) & (asiste==1 | asiste==2)
 replace NERS=1 if (edad>=12 & edad<=17) & (asiste==1) & ((nivel==3 & (ultgrado>=6 & ultgrado<=8)) | ((nivel==6 | nivel==8) & (ultgrado>=1 & ultgrado<=3)))

/*(Melisa June 3, 2009)

La tasa NERS esta incorporando nivel==3, grados 6 a 8 (segundo ciclo de primaria) lo que contradice a 
edupc_ci "Primaria completa" que da 8 anios de educacion.

Chile:  1er ciclo primaria (grados 1 a 4), 2nd ciclo primaria (grados 5 a 8), edades ideales 6-12 years old

Para mi la tasa NERS no deberia tener la condicion (nivel==3 & (ultgrado>=6 & ultgrado<=8), y debe incorporar nivel==7*/



** Upper secondary
* Tasa neta de asistencia en la Enseñanza Media

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

 gen prim=1 if  asiste==1 & (nivel==1 | (nivel==3 & (ultgrado>=1 & ultgrado<=5)))
 gen sec=1 if   asiste==1 & (((nivel==3) & (ultgrado>=6 & ultgrado<=8)) | ((nivel==6 | nivel==8) & (ultgrado>=1 & ultgrado<=3)))
 gen ter=1 if   asiste==1 & ((nivel==13 | nivel==9 | nivel==11) |  ((nivel==6 | nivel==8) & ultgrad>=4))

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

* Without domestic Service
* INCL==1 ==> Excludes nucleo 0

 gen     WENAS=0 if incl==1 & ((edad>=15 & edad<=64) & ((categ>=3 & categ<=5) | categ==9) & (rama>=2 & rama<=9)) 
 replace WENAS=1 if incl==1 & ((edad>=15 & edad<=64) & ((categ>=3 & categ<=5) | categ==9) & (rama>=2 & rama<=9) & (sexo==2))

** Target 4, Indicator: Share of women in wage employment in the non-agricultural sector (%)
* With domestic servants
* INCL==1 ==> Excludes nucleo 0

 gen     WENASD=0 if incl==1 & ((edad>=15 & edad<=64) & ((categ>=3 & categ<=7) | categ==9) & (rama>=2 & rama<=9)) 
 replace WENASD=1 if incl==1 & ((edad>=15 & edad<=64) & ((categ>=3 & categ<=7) | categ==9) & (rama>=2 & rama<=9) & (sexo==2))

*** GOAL 7 ENSURE ENVIROMENTAL SUSTAINABILITY
** Access to Electricity ** Additional Indicator

* Gender classification of the population refers to the head of the household.

 gen     ELEC=0 if (v7a>=1 & v7a<=7)	/* Total population excluding missing information */
 replace ELEC=1 if (v7a>=1 & v7a<=6)

** Target 9, Indicator: Proportion of the population using solidfuels (%)

* NA

** Target 10, Indicator: Proportion of the population with sustainable access to an improved water source (%)
* Gender classification of the population refers to the head of the household.

 gen     WATER=0 if (agua>=1 & agua<=6)  /* Total population excluding missing information */	
 replace WATER=1 if (agua>=1 & agua<=4) 

** Target 10, Indicator: Proportion of Population with Access to Improved Sanitation, Urban and Rural (%)
* Gender classification of the population refers to the head of the household.

 gen     SANITATION=0 if (servsani>=1 & servsani<=7)  /* Total population excluding missing information */
 replace SANITATION=1 if (servsani>=1 & servsani<=2)

** Target 11, Indicator: Proportion of the population with access to secure tenure (%)
* PERSONS PER ROOM

 egen nrocuart_hog1=rsum(v3a v3b v3c v3d v3e), missing
 egen nrocuart_hogrest=rsum(v16a v16b v16c v16d v16e), missing
 recode nrocuart_hogrest (0=.)

 gen nrocuart=nrocuart_hog1 if v14==1
 replace nrocuart=nrocuart_hogrest if (v14>=2 & v14<=9)

 gen persroom=numper/nrocuart
 
* Indicator components

* 1. Non secure tenure or type of dwelling.

 gen     secten_1=0 if ((tenencia>=1 & tenencia<=10) & (tipoviv>=1 & tipoviv<=9)) /* Total population excluding missing information */
 replace secten_1=1 if ((tenencia>=7 & tenencia<=10) | (tipoviv>=6 & tipoviv<=9))

* 2. Low quality of the floor or walls materials.

 gen     secten_2=0 if ((pared>=1 & pared<=8) & (piso>=1 & piso<=5)) /* Total population excluding missing information */
 replace secten_2=1 if ((pared>=6 & pared<=8) | (piso==5)) 

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

 gen     DIRT=0 if (piso>=1 & piso<=5)	/* Total population excluding missing information */
 replace DIRT=1 if (piso==5)

** GOAL 8. DEVELOP A GLOBAL PARTNERSHIP FOR DEVELOPMENT

** Target 16, Indicator: Unemployment Rate of 15 year-olds (%)
* INCL==1 ==> Excludes nucleo 0

 gen     UNMPLYMENT15=0 if incl==1 & (edad>=15 & edad<=24) & (tasadeso==0 | tasadeso==1) 
 replace UNMPLYMENT15=1 if incl==1 & (edad>=15 & edad<=24) & (tasadeso==1) 
	
** Target 18, Indicator: "Telephone lines and celullar subscribers per 100 population"
* Telephone Lines and Cellular Subscribers 

* Fixed Line
* Household head

 gen     tel=1 if r10d==1 & pco1==1
 replace tel=0 if tel==. & r10d!=9 & pco1==1

 egen telefono=max(tel), by(id_hogar)

* Cellular
* Any household member with cellular service

 gen     cel=1 if (r13==1 | r13==2) 
 replace cel=0 if cel==. & r13!=9 
 
 egen celular=max(cel), by(id_hogar)

* Gender classification of the population refers to the head of the household.

 gen     TELCEL=0 if (telefono>=0 & telefono<=1) & (celular>=0 & celular<=1)	/* Total population excluding missing information */
 replace TELCEL=1 if (telefono==1 | celular==1) 


** FIXED LINES

* Gender classification of the population refers to the head of the household.

 gen     TEL=0 if (telefono>=0 & telefono<=1)	/* Total population excluding missing information */
 replace TEL=1 if (telefono==1) 

** CEL LINES

* Gender classification of the population refers to the head of the household.

 gen     CEL=0 if (celular>=0 & celular<=1)	/* Total population excluding missing information */
 replace CEL=1 if (celular==1)

** Target 18, Indicator: "Personal computers in use per 100 population"

* Computers
* Household head

 gen     comp=1 if r11==1 & pco1==1
 replace comp=0 if comp==. & r11!=9 & pco1==1

 egen computador=max(comp), by(id_hogar)

* Gender classification of the population refers to the head of the household.

 gen     COMPUTER=0 if (computador>=0 & computador<=1)	/* Total population excluding missing information */
 replace COMPUTER=1 if (computador==1)

* Target 18, Indicator: "Internet users per 100 population"

** Internet access 
* Household head

* Conexión a internet conmutada 

 gen     inte_1=1 if r12a==1 & pco1==1
 replace inte_1=0 if inte_1==. & r12a!=9 & pco1==1

* Conexión a internet banda ancha

 gen     inte_2=1 if r12a==2 & pco1==1
 replace inte_2=0 if inte_2==. & r12a!=9 & pco1==1

 egen internet_1=max(inte_1), by(id_hogar)
 egen internet_2=max(inte_2), by(id_hogar)

 gen     INTUSERS=0 if (internet_1>=0 & internet_1<=1) & (internet_2>=0 & internet_2<=1) /* Total population excluding missing information */
 replace INTUSERS=1 if (internet_1==1 | internet_2==1)

************************************************************************
**** ADDITIONAL SOCIO - ECONOMIC COMMON COUNTRY ASESSMENT INDICATORS ****
************************************************************************

** CCA 19. Proportion of children under 15 who are working
* INCLUDES POPULATION 12 TO 15 YEARS-OLD
* INCL==1 ==> Excludes nucleo 0

 gen     CHILDREN=0 if incl==1 & (edad>=12 & edad<=14) 
 replace CHILDREN=1 if incl==1 & ((edad>=12 & edad<=14) & (o1==1 | o3==1))

** CCA 41 Number of Persons per Room*

 generate PERSROOM2=persroom if pco1==1

 gen     popinlessthan2=1 if persroom<=2
 replace popinlessthan2=0 if popinlessthan2==.

* Gender classification of the population refers to the head of the household.

 gen     PLT2=0 if persroom<.		/* Total population excluding missing information */
 replace PLT2=1 if (popinlessthan2==1)

** Disconnected Youths

 gen     DISCONN=0 if (edad>=15 & edad<=24)
 replace DISCONN=1 if (edad>=15 & edad<=24)  & (o6>=8 & o6<=10)

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

* Grade for age

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
replace lp_ci= 47099   if zona_c==1  /*urbana*/
replace lp_ci= 31756   if zona_c==0	/*rural*/
label var lp_ci "Linea de pobreza oficial del pais"

*********
*lpe_ci***
*********

gen lpe_ci =.
replace lpe_ci= 23549   if zona_c==1  /*urbana*/
replace lpe_ci= 18146   if zona_c==0	/*rural*/
label var lpe_ci "Linea de indigencia oficial del pais"

****************
*cotizando_ci***
****************
gen cotizando_ci=.
replace cotizando_ci=1 if (o29 >= 1 & o29 <= 5) 
recode cotizando_ci .=0 if (activ==1 | activ==2)
label var cotizando_ci "Cotizante a la Seguridad Social"

****************
*afiliado_ci****
****************
gen afiliado_ci=.	
replace afiliado_ci=1 if (o29 >= 1 & o29 <= 6)
recode afiliado_ci .=0 
label var afiliado_ci "Afiliado a la Seguridad Social"


****************
*tipopen_ci*****
****************

gen tipopen_ci=.

replace tipopen_ci = 1 if (yjubaj > 0 & yjubaj != .)
replace tipopen_ci = 1 if (yvitaj > 0 & yvitaj != .)
replace tipopen_ci = 2 if (yinvaj > 0 & yinvaj != .)
replace tipopen_ci = 3 if (ymonaj > 0 & ymonaj != .)

replace tipopen_ci = 12 if (yjubaj > 0 & yjubaj != .) | (yinvaj > 0 & yinvaj != .)
replace tipopen_ci = 13 if (yinvaj > 0 & yinvaj != .) | (ymonaj > 0 & ymonaj != .)
replace tipopen_ci = 23 if (ymonaj > 0 & ymonaj != .) | (yinvaj > 0 & yinvaj != .)

replace tipopen_ci = 123 if (yjubaj > 0 & yjubaj != .) | (yinvaj > 0 & yinvaj != .) | (ymonaj > 0 & ymonaj != .)

label define  t 1 "Jubilacion" 2 "Pension invalidez" 3 "Pension viudez" 12 " Jub y inv" 13 "Jub y viud" 23 "Viud e inv"  123 "Todas"
label value tipopen_ci t

label var tipopen_ci "Tipo de pension - variable original de cada pais" 

****************
*instpen_ci*****
****************
gen instpen_ci=.
* se clasifican por tipo de pension, no se puede armar una sola variable ver y20*
label var instpen_ci "Institucion proveedora de la pension - variable original de cada pais" 


****************
*instcot_ci*****
****************
gen instcot_ci=.
replace instcot_ci=o29 if o29<=5
label var instcot_ci "Institucion a la que cotiza - variable original de cada pais" 

*****************
*tipocontrato_ci*
*****************
/*
gen tipocontrato_ci=.
replace tipocontrato_ci=1 if o21==1 & categopri_ci==3
replace tipocontrato_ci=2 if (o21==2 | o21==3) & categopri_ci==3
replace tipocontrato_ci=3 if o20==3 & categopri_ci==3
label var tipocontrato_ci "Tipo de contrato segun su duracion"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci
*/	
 * Corregido por la variable de firmo o no firmo MGD 06/16/2014	
gen tipocontrato_ci=.
replace tipocontrato_ci=1 if ((o20==1 | o20==2) & o21==1) & categopri_ci==3
replace tipocontrato_ci=2 if ((o20==1 | o20==2) & (o21>=2 & o21<=5)) & categopri_ci==3
replace tipocontrato_ci=3 if (o20>=3 | tipocontrato_ci==.) & categopri_ci==3
label var tipocontrato_ci "Tipo de contrato segun su duracion"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci

*************
*cesante_ci* 
*************
gen cesante_ci=1 if o8==1
replace cesante_ci=0 if o8==2
label var cesante_ci "Desocupado - definicion oficial del pais"	


**************
***tamemp_ci**
**************

gen tamemp_ci=1 if o13=="A" | o13=="B" 
replace tamemp_ci=2 if o13=="C" | o13=="D"
replace tamemp_ci=3 if o13=="E" | o13=="F"


label var tamemp_ci "# empleados en la empresa segun rangos"
label define tamemp_ci 1 "Pequena" 2 "Mediana" 3 "Grande"
label value tamemp_ci tamemp_ci

*************
**pension_ci*
*************
*MLO agregue la condicion auxpen!=.
egen auxpen=rsum(yjubaj yvitaj yinvaj ymonaj yorfaj), missing
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
egen auxpens=rsum(ypasaj ybspsaj), missing
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
* CHL 2006
gen salmm_ci= 	135000
label var salmm_ci "Salario minimo legal"

*************
*tecnica_ci**
*************


gen tecnica_ci=.
replace tecnica_ci=1 if nivel==9 | nivel==10
recode tecnica_ci .=0 
label var tecnica_ci "1=formacion terciaria tecnica"


**************
**categoinac_ci*
****************


gen categoinac_ci=1 if o6==17
replace categoinac_ci=2 if o6==16
replace categoinac_ci=3 if o6==6
replace categoinac_ci=4 if o6==1 | o6==2 | o6==3 | o6==4 | o6==5 | o6==7 | o6==8 | o6==9 | o6==10| o6==11| o6==12 | o6==13 | o6==14 | o6==15 | o6==18 | o6==19 | o6==20


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
gen tcylmpri_ci =.
gen tcylmpri_ch =.
gen region_c=.
gen ylmotros_ci=.
gen autocons_ci=.
gen autocons_ch=.


*YL -> elimino var comp para que no genere problemas al SOCIOMETERO (esta var no es necesaria)
drop comp

/***************************
* DISCAPACIDAD
***************************/
*Daniela Zuluaga Feb 2020:
*Con base a elaboración Mariana Pinzón y M.Antonela Pereira

gen dis_ci = 0

recode dis_ci nonmiss=. if t1a>=. & t1b>=. & t1c>=.
recode dis_ci nonmiss=. if inlist(9, t1a, t1b, t1c)
foreach i in a b c {
forvalues j=1/6 {
replace dis_ci=1 if t1`i'==`j'
}
}
lab def dis_ci 1 "Con Discapacidad" 0 "Sin Discapacidad"
lab val dis_ci dis_ci


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

rename c_o12 codindustria
rename c_o11 codocupa


compress


saveold "`base_out'", replace


log close

