* (Versi??tata 12)
clear
set more off
*________________________________________________________________________________________________________________*

 * Activar si es necesario (dejar desactivado para evitar sobreescribir la base y dejar la posibilidad de 
 * utilizar un loop)
 * Los datos se obtienen de las carpetas que se encuentran en el servidor: \\Sdssrv03\surveys
 * Se tiene acceso al servidor ??amente al interior del BID.
 * El servidor contiene las bases de datos MECOVI.
 *________________________________________________________________________________________________________________*
 

global ruta = "\\Sdssrv03\surveys"

local PAIS JAM
local ENCUESTA SLC
local ANO "2006"
local ronda m5 

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
*local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\\`PAIS'_`ANO'`ronda'.dta"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"


capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
Pa�s: Jamaica
Encuesta: SLC
Round: a
Autores: Mayra S�enz E-mail: saenzmayra.a@gmail.com - mayras@iadb.org
Versiones anteriores: Yessenia Loayza
Fecha �ltima modificaci�n: 17 de noviembre de 2016

							SCL/LMK - IADB
****************************************************************************/
****************************************************************************/



use `base_in', clear


***********
*factor_ch*
***********

ren finw factor_ch

********
*idh_ch*
********

*destring serial, gen(idh_ch)
g idh_ch = serial
*******
*idp_c*
*******

*destring ind, gen(idp_c)

g idp_c = ind
********
*zona_c*
********

*destring area, gen(zona_c)
g zona_c = area
replace zona_c=1 if zona_c==2

replace zona_c=0 if zona_c==3

********
*pais_c*
********

gen pais_c="JAM"

********
*anio_c*
********

gen anio_c=2006

*******
*mes_c*
*******

gen mes_c=.

*************
*relacion_ci*
*************

/*

relat

1  head 							 1936 	  29.5%  
2  spouse/partner of head  			 746  	  11.4%  
3  child of head/spouse  			 2437 	  37.1%  
4  spouse/partner of child 			 41  	  0.6%  
5  grand child of head/spouse 		 817 	  12.4%  
6  parent of head/spouse 			 74  	  1.1%  
7  other relative of head/spouse 	 446 	  6.8%  
8  domestic employee  				 5   	  0.1%  
9  other non relative  				 61 	  0.9%  


*/

*replace relat="" if relat=="N"
*destring relat, replace


gen relacion_ci=1 if relat==1
replace relacion_ci=2 if relat==2
replace relacion_ci=3 if relat==3
replace relacion_ci=4 if relat>=4 & relat<=7
replace relacion_ci=5 if relat==9
replace relacion_ci=6 if relat==8


***********
*factor_ci*
***********

gen factor_ci=factor_ch

*********
*sexo_ci*
*********

*destring sex, gen(sexo_ci)
g sexo_ci = sex

*********
*edad_ci*
*********

*destring age, gen(edad_ci)
g edad_ci = age

**********
*civil_ci*
**********

gen civil_ci=.

/*

marital

1  Married 			 1098  	 25.6%  
2  Never married  	 2874  	 67.1%  
3  Divorced  		 54 	 1.3%  
4  Separated  		 36  	 0.8%  
5  Widowed  		 222     5.2%  
Sysmiss   			 2282  
*/
*replace marital="" if marital=="N"
*destring marital, replace

replace civil_ci=1 if marital==2
replace civil_ci=2 if marital==1
replace civil_ci=3 if marital==3 | marital==4
replace civil_ci=4 if marital==5

*********
*jefe_ci*
*********

gen jefe_ci=(relacion_ci==1)


by idh_ch, sort: egen nconyuges_ch=sum(relacion_ci==2)
label variable nconyuges_ch "Numero de conyuges"

***************
***nhijos_ch***
***************

by idh_ch, sort: egen nhijos_ch=sum(relacion_ci==3)
label variable nhijos_ch "Numero de hijos"

******************
***notropari_ch***
******************

by idh_ch, sort: egen notropari_ch=sum(relacion_ci==4)
label variable notropari_ch "Numero de otros familiares"

********************
***notronopari_ch***
********************

by idh_ch, sort: egen notronopari_ch=sum(relacion_ci==5)
label variable notronopari_ch "Numero de no familiares"


****************
***nempdom_ch***
****************

by idh_ch, sort: egen nempdom_ch=sum(relacion_ci==6)
label variable nempdom_ch "Numero de empleados domesticos"

*****************
***clasehog_ch***
*****************

gen byte clasehog_ch=0
**** unipersonal
replace clasehog_ch=1 if nhijos_ch==0 & nconyuges_ch==0 & notropari_ch==0 & notronopari_ch==0
**** nuclear   (child with or without spouse but without other relatives)
replace clasehog_ch=2 if (nhijos_ch>0| nconyuges_ch>0) & (notropari_ch==0 & notronopari_ch==0)
**** ampliado
replace clasehog_ch=3 if notropari_ch>0 & notronopari_ch==0
**** compuesto  (some relatives plus non relative)
replace clasehog_ch=4 if ((nconyuges_ch>0 | nhijos_ch>0 | notropari_ch>0) & (notronopari_ch>0))
**** corresidente
replace clasehog_ch=5 if nhijos_ch==0 & nconyuges_ch==0 & notropari_ch==0 & notronopari_ch>0

label variable clasehog_ch "Tipo de hogar"
label define clasehog_ch 1 " Unipersonal" 2 "Nuclear" 3 "Ampliado" 
label define clasehog_ch 4 "Compuesto" 5 " Corresidente", add
label value clasehog_ch clasehog_ch

******************
***nmiembros_ch***
******************

by idh_ch, sort: egen nmiembros_ch=sum(relacion_ci>=1 & relacion_ci<=4)
label variable nmiembros_ch "Numero de familiares en el hogar"



*****************
***nmayor21_ch***
*****************

by idh_ch, sort: egen nmayor21_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad>=21)
label variable nmayor21_ch "Numero de familiares mayores a 21 anios"

*****************
***nmenor21_ch***
*****************

by idh_ch, sort: egen nmenor21_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad<21)
label variable nmenor21_ch "Numero de familiares menores a 21 anios"

*****************
***nmayor65_ch***
*****************

by idh_ch, sort: egen nmayor65_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad>=65)
label variable nmayor65_ch "Numero de familiares mayores a 65 anios"

****************
***nmenor6_ch***
****************

by idh_ch, sort: egen nmenor6_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad<6)
label variable nmenor6_ch "Numero de familiares menores a 6 anios"

****************
***nmenor1_ch***
****************

by idh_ch, sort: egen nmenor1_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad<1)
label variable nmenor1_ch "Numero de familiares menores a 1 anio"

****************
***miembro_ci***
****************

gen miembro_ci=(relacion_ci<6)
label variable miembro_ci "Miembro del hogar"

**********
*ocupa_ci*
**********
tostring r1_02, replace
gen ocupa_ci=substr(r1_02,1,1) 
replace ocupa_ci="" if ocupa_ci=="N"
destring ocupa_ci, replace
replace ocupa_ci=. if relacion_ci!=1  //La ocupacion solo se le pregunta al jefe del hogar

*********
*rama_ci*
*********

tostring r1_03, replace
gen rama_ci=substr(r1_03,1,1) 
replace rama_ci="" if rama_ci=="N"
destring rama_ci, replace
replace rama_ci=. if relacion_ci!=1  //La ocupacion solo se le pregunta al jefe del hogar

/*Se saltan lasvariables hasta las remesas, todas las anteriores estan reportadas en LFS*/

************
*remesas_ci*
************

*Ya estan*

************
*remesas_ch*
************

by idh_ch, sort: egen remesas_ch=sum(remesas_ci) if miembro_ci==1

***********
*asiste_ci*
***********


gen asiste_ci=1 if b01>=1 & b01<=18
replace asiste_ci=0 if b01==17

*********
*aedu_ci*
*********
gen aedu_ci=.
/*
gen aedu_ci=b22 if b22!="NN" & asiste_ci==0
destring aedu_ci, replace

replace b21="." if b21=="NN"

destring b21, replace

replace b04="." if b04=="NN" 
destring b04, replace

replace aedu_ci=b04 if asiste_ci==1
*/

**************
***eduno_ci***
**************

gen byte eduno_ci=.
/*gen byte eduno_ci=0
replace eduno_ci=1 if aedu_ci==0
replace eduno_ci=. if aedu_ci==.
label variable eduno_ci "Cero anios de educacion"*/

**************
***edupi_ci***
**************
gen byte edupi_ci=.
/*
gen byte edupi_ci=0
replace edupi_ci=1 if aedu_ci>0 & aedu_ci<6
replace edupi_ci=. if aedu_ci==.
label variable edupi_ci "Primaria incompleta"
*/
**************
***edupc_ci***
**************
gen byte edupc_ci=.
/*

gen byte edupc_ci=0
gen byte edupc_ci=0
replace edupc_ci=1 if aedu_ci==6
replace edupc_ci=. if aedu_ci==.
label variable edupc_ci "Primaria completa"*/

**************
***edusi_ci***
**************
gen byte edusi_ci=.
/*
gen byte edusi_ci=0
replace edusi_ci=1 if aedu_ci>6 & aedu_ci<12
replace edusi_ci=. if aedu_ci==.
label variable edusi_ci "Secundaria incompleta"*/

**************
***edusc_ci***
**************
gen byte edusc_ci=.
/*
gen byte edusc_ci=0
replace edusc_ci=1 if aedu_ci==12
replace edusc_ci=. if aedu_ci==.
label variable edusc_ci "Secundaria completa"*/

***************
***edus1i_ci***
***************
gen byte edus1i_ci=.
/*
gen byte edus1i_ci=0
replace edus1i_ci=1 if aedu_ci>6 & aedu_ci<9
replace edus1i_ci=. if aedu_ci==.
label variable edus1i_ci "1er ciclo de la secundaria incompleto"*/

***************
***edus1c_ci***
***************
gen byte edus1c_ci=.
/*
gen byte edus1c_ci=0
replace edus1c_ci=1 if aedu_ci==9
replace edus1c_ci=. if aedu_ci==.
label variable edus1c_ci "1er ciclo de la secundaria completo"*/

***************
***edus2i_ci***
***************
gen byte edus2i_ci=.
/*
gen byte edus2i_ci=0
replace edus2i_ci=1 if aedu_ci>9 & aedu_ci<12
replace edus2i_ci=. if aedu_ci==.
label variable edus2i_ci "2do ciclo de la secundaria incompleto"*/
***************
***edus2c_ci***
***************
gen byte edus2c_ci=.
/*
gen byte edus2c_ci=0
replace edus2c_ci=1 if aedu_ci==12
replace edus2c_ci=. if aedu_ci==.
label variable edus2c_ci "2do ciclo de la secundaria completo"*/

**************
***eduui_ci***
**************
gen byte eduui_ci=.
/*
gen byte eduui_ci=0
replace eduui_ci=1 if aedu_ci>12 & aedu_ci<17
replace eduui_ci=. if aedu_ci==.
label variable eduui_ci "Universitaria incompleta"*/

***************
***eduuc_ci****
***************
gen byte eduuc_ci=.
/*
gen byte eduuc_ci=0
replace eduuc_ci=1 if aedu_ci>=17
replace eduuc_ci=. if aedu_ci==.
label variable eduuc_ci "Universitaria incompleta o mas"*/


***************
***edupre_ci***
***************

gen byte edupre_ci=. 


**************
***eduac_ci***
**************
gen byte eduac_ci=. 
label variable eduac_ci "Superior universitario vs superior no universitario"



**********
*pqnoasis*
**********
*replace B23="" if B23=="N"
*destring B23, gen(pqnoasis)
g pqnoasis_ci =.
label define pqnoasis_ci 1 "Reached terminal grade"  2 "Money Problems" 3 "Pregnancy" 4 "Expelled" 5 "No interest in School" 6 "Family Problems" 7 "Other"
label values pqnoasis_ci pqnoasis_ci

**************
*pqnoasis1_ci*
**************
**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**

g       pqnoasis1_ci = 1 if pqnoasis_ci==2
replace pqnoasis1_ci = 3 if pqnoasis_ci==6
replace pqnoasis1_ci = 4 if pqnoasis_ci==5
replace pqnoasis1_ci = 5 if pqnoasis_ci==3
replace pqnoasis1_ci = 6 if pqnoasis_ci==1
replace pqnoasis1_ci = 9 if pqnoasis_ci==4 | pqnoasis_ci==7


label define pqnoasis1_ci 1 "Problemas econ�micos" 2 "Por trabajo" 3 "Problemas familiares o de salud" 4 "Falta de inter�s" 5	"Quehaceres dom�sticos/embarazo/cuidado de ni�os/as" 6 "Termin� sus estudios" 7	"Edad" 8 "Problemas de acceso"  9 "Otros"
label value  pqnoasis1_ci pqnoasis1_ci

***********
*repite_ci*
***********

gen repite_ci=.

**************
*repiteult_ci*
**************

gen repiteult_ci=.

***********
*edupub_ci*
***********
g  edupub_ci=.
/*
replace B03="" if B03=="N"
destring B03, gen(edupub_ci)
replace edupub_ci=0 if edupub_ci==2*/

************
*aguared_ch*
************

/*
I18
Indoor tap/Pipe				1
outside private pipe/tap	2
public standpipe			3
well						4
river, lake spring pond		5
rainswater tank				6
rainwater tank npid			7
trucked water pid			8
trucked water npid			9
trucked water private pid	10
truck water private npid	11
bottled water				12

*/

/*replace I18="" if I18=="NN"

destring I18, replace

gen aguared_ch=1 if I18==1 | I18==2 | I18==3
replace aguared_ch=0 if I18>=4 & I18<=12*/

g aguared_ch      =1 if i09 ==1
replace aguared_ch=0 if i09==2

*************
*aguadist_ch*
*************

*replace I24p="" if I24p=="N"

*destring I24 I24p, replace

gen aguadist_ch=.
/*
replace aguadist_ch=1 if I24==0
replace aguadist_ch=2 if I24<100 & I24p==2
replace aguadist_ch=2 if I24<0.1 & I24p==1
replace aguadist_ch=3 if I24>100 & I24!=. & I24p==2
replace aguadist_ch=3 if I24>0.1 & I24!=. & I24p==1*/

*************
*aguamide_ch*
*************

gen aguamide_ch=.

********
*luz_ch*
********

gen luz_ch=.
replace luz_ch=1 if i12==1
replace luz_ch=0 if i12>1 & i12<=4

************
*luzmide_ch*
************

gen luzmide_ch=.

************
*combust_ch*
************

gen combust_ch=.

*********
*bano_ch*
*********

gen bano_ch=.
*replace bano_ch=0 if I05=="N"
*replace I05="" if I05=="N"
*destring I05, replace


***********
*banoex_ch*
***********
g banoex_ch =.

/*ren I05 banoex_ch 
replace banoex_ch=0 if banoex_ch==2*/

*********
*des1_ch*
*********

gen des1_ch=.

*********
*des2_ch*
*********

gen des2_ch=.

*********
*piso_ch*
*********

gen piso_ch=.

**********
*pared_ch*
**********

gen pared_ch=.
/*replace I02="" if I02=="N"
destring I02, replace

replace pared_ch=0 if I02==1
replace pared_ch=1 if I02>1 & I02<=6
replace pared_ch=2 if I02==7 */

**********
*techo_ch*
**********

gen techo_ch=.

**********
*resid_ch*
**********

*replace I31="" if I31=="N"
*destring I31, replace

gen resid_ch=.
/*replace resid_ch=0 if I31==1
replace resid_ch=1 if I31==3 | I31==4
replace resid_ch=2 if I31==5 | I31==6
replace resid_ch=3 if I31==2  | I31==7*/

**Daniela Zuluaga- Enero 2018: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**
	
*********************
***aguamejorada_ch***
*********************
g       aguamejorada_ch = .
		
*********************
***banomejorado_ch***
*********************
g       banomejorado_ch = .


*********
*dorm_ch*
*********

g dorm_ch=.

*replace I03="" if I03=="NN"

*destring I03, replace

*ren I03 dorm_ch

************
*cuartos_ch*
************

gen cuartos_ch=.

***********
*cocina_ch*
***********

gen cocina_ch=.

**********
*telef_ch*
**********

gen telef_ch=.
*replace I281="" if I281=="N"
*destring I281, replace

*replace telef_ch=1 if I281==1
*replace telef_ch=0 if I281==2

***********
*refrig_ch*
***********

gen refrig_ch=.

*replace refrig_ch=1 if J604Y=="X"
*replace refrig_ch=0 if J604N=="X"

************
*freezer_ch*
************

gen freezer_ch=.

*********
*auto_ch*
*********

gen auto_ch=.

 *replace auto_ch=1 if J617Y=="X"
 *replace auto_ch=0 if J617N=="X"

**********
*compu_ch*
**********
gen compu_ch=.
 *replace compu_ch=1 if  J618Y =="X"
 *replace compu_ch=0 if J618N=="X"

 
 
*************
*internet_ch*
*************

gen internet_ch=.
 *replace internet_ch=1 if  I33=="1"
 *replace internet_ch=0 if I33=="2"

********
*cel_ch*
********


gen cel_ch=.

*replace cel_ch=1 if I282=="1"
*replace cel_ch=0 if I282=="2"

**********
*vivi1_ch*
**********

gen vivi1_ch=.
*replace I01="" if I01=="N"
*destring I01, replace
*replace vivi1_ch=1 if I01==1  |I01==2  |I01==3  |I01==5  
*replace vivi1_ch=2 if I01==4  |I01==7
*replace vivi1_ch=3 if I01==6 |I01==8

**********
*vivi2_ch*
**********

gen vivi2_ch=.

*replace vivi2_ch=1 if vivi1_ch==1 |vivi1_ch==2
*replace vivi2_ch=2 if vivi1_ch==3

*************
*viviprop_ch*
*************

gen viviprop_ch=.
*destring I07, replace


/*No se sabe si es propia totalmente pagada o propia pagada parcialmente*/

*replace viviprop_ch=0 if I07>=2 & I07<=5
*replace viviprop_ch=3 if I07==6

************
*vivitit_ch*
************

gen vivitit_ch=.

************
*vivialq_ch*
************

*replace I10p="" if I10p=="N"
*destring I10p, replace

gen vivialq_ch=.


*replace vivialq_ch=I10 if I10p==4
*replace vivialq_ch=I10*4.3 if I10p==3
*replace vivialq_ch=I10/12 if I10p==5

***************
*vivialqimp_ch*
***************

gen vivialqimp_ch=.

cap ren idp_c idp_ci



g region_c =.
g region_BID_c = .

g raza_idioma_ci =.
g id_ind_ci  =.
g id_afro_ci=. 
g raza_ci =.
g condocup_ci=.
g categoinac_ci=.
g desemp_ci =.
g cesante_ci =.
g pea_ci=.
g tamemp_ci =.
g cotizando_ci=.
g instcot_ci =.
g afiliado_ci=.
g formal_ci=.
g nempleos_ci=.
g emp_ci =.
g tipocontrato_ci=.
g pensionsub_ci=.
g pension_ci=.
g tipopen_ci=.
g instpen_ci=.
g  tcylmpri_ci=.
g tcylmpri_ch=.
g ypen_ci=.
g ypensub_ci=.
g salmm_ci=.
g lp_ci=.
g lpe_ci=.
g  tecnica_ci=.
g rentaimp_ch =.
g aguamala_ch =.
g freez_ch=.
g  antiguedad_ci=.
g durades_ci =.
g desalent_ci =.
g subemp_ci =.
g tiempoparc_ci =.
g categopri_ci=.
g categosec_ci=.
g spublico_ci=.
g  horaspri_ci=.
g horastot_ci=.	
g ylmpri_ci=.
g nrylmpri_ci =.
g ynlm_ch	=.
g ynlnm_ch =.
g ylmhopri_ci =.
g ylmho_ci =.
g autocons_ci =.
g autocons_ch =.
g nrylmpri_ch =.
g ylnmpri_ci=.
g ylmsec_ci=.
g ylnmsec_ci =.
g ylmotros_ci=.
g ylnmotros_ci=.
g ylm_ci =.
g ylnm_ci =.
g ynlm_ci=.
g ynlnm_ci =.
g ylm_ch=.
g  ylnm_ch=.
g ylmnr_ch=.
g miembros_ci=.

destring sexo_ci, replace
  /*_____________________________________________________________________________________________________*/
* Asignaci�n de etiquetas e inserci�n de variables externas: tipo de cambio, Indice de Precios al 
* Consumidor (2011=100), l�neas de pobreza
/*_____________________________________________________________________________________________________*/


do "$ruta\harmonized\_DOCS\\Labels&ExternalVars_Harmonized_DataBank.do"

/*_____________________________________________________________________________________________________*/
* Verificaci�n de que se encuentren todas las variables armonizadas 
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


