clear all
set mem 500m
set more off
cd "X:\ARM\JAM\2005\JSLC\Orig_data"
use slc2005.dta

***********
*factor_ch*
***********

ren finw factor_ch

********
*idh_ch*
********

destring serial, gen(idh_ch)

*******
*idp_c*
*******

destring ind, gen(idp_c)

********
*zona_c*
********

destring area, gen(zona_c)

replace zona_c=1 if zona_c==2

replace zona_c=0 if zona_c==3

********
*pais_c*
********

gen pais_c="JAM"

********
*anio_c*
********

gen anio_c=2005

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

replace relat="" if relat=="N"
destring relat, replace

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

destring sex, gen(sexo_ci)

*********
*edad_ci*
*********

destring age, gen(edad_ci)

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
replace marital="" if marital=="N"
destring marital, replace

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
***miembros_ci***
****************

gen miembros_ci=(relacion_ci<6)
label variable miembros_ci "Miembro del hogar"


**********
*ocupa_ci*
**********

gen ocupa_ci=substr(R1_02,1,1) 
replace ocupa_ci="" if ocupa_ci=="N"
destring ocupa_ci, replace
replace ocupa_ci=. if relacion_ci!=1  //La ocupacion solo se le pregunta al jefe del hogar

*********
*rama_ci*
*********


gen rama_ci=substr(R1_03,1,1) 
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

by idh_ch, sort: egen remesas_ch=sum(remesas_ci) if miembros_ci==1

*********
*aedu_ci*
*********
/*
gen aedu_ci=B22 if B22!="NN"
destring aedu_ci, replace

replace B21="" if B21=="NN"

destring B21, replace


replace aedu_ci=0 if B21==1 |B21==2
replace aedu_ci=7 if B21==5
replace aedu_ci=18 if B21==14
replace aedu_ci=11 if B21==11
replace aedu_ci=16 if B21==15 | B21==16| B21==12

*/

gen aedu_ci=.

**************
***eduno_ci***
**************

gen byte eduno_ci=.

**************
***edupi_ci***
**************

gen byte edupi_ci=.

**************
***edupc_ci***
**************

gen byte edupc_ci=.

**************
***edusi_ci***
**************

gen byte edusi_ci=.

**************
***edusc_ci***
**************

gen byte edusc_ci=.

***************
***edus1i_ci***
***************

gen byte edus1i_ci=.

***************
***edus1c_ci***
***************

gen byte edus1c_ci=.

***************
***edus2i_ci***
***************

gen byte edus2i_ci=.
***************
***edus2c_ci***
***************

gen byte edus2c_ci=.

**************
***eduui_ci***
**************

gen byte eduui_ci=.

***************
***eduuc_ci****
***************

gen byte eduuc_ci=.


***************
***edupre_ci***
***************

gen byte edupre_ci=. 


**************
***eduac_ci***
**************
gen byte eduac_ci=. 
label variable eduac_ci "Superior universitario vs superior no universitario"

***********
*asiste_ci*
***********

replace B01="" if B01=="NN"
destring B01, replace

gen asiste_ci=1 if B01>=1 & B01<=16
replace asiste_ci=0 if B01==17

**********
*pqnoasis*
**********

gen pqnoasis_ci=.

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

gen edupub_ci=.


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
replace I09="" if I09=="N"

destring I09, replace

gen aguared_ch=1 if I09==1 
replace aguared_ch=0 if I09==2

*************
*aguadist_ch*
*************

gen aguadist_ch=.

*************
*aguamide_ch*
*************

gen aguamide_ch=.

********
*luz_ch*
********

gen luz_ch=.

replace I12="" if I12=="N"
destring I12, replace

replace luz_ch=1 if I12==1
replace luz_ch=0 if I12>1 & I12<=4

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



***********
*banoex_ch*
***********


gen banoex_ch =.


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


**********
*techo_ch*
**********

gen techo_ch=.

**********
*resid_ch*
**********



gen resid_ch=.


*********
*dorm_ch*
*********



gen dorm_ch=.

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
replace I151="" if I151=="N"
destring I151, replace

replace telef_ch=1 if I151==1
replace telef_ch=0 if I151==2

***********
*refrig_ch*
***********

gen refrig_ch=.


************
*freezer_ch*
************

gen freezer_ch=.

*********
*auto_ch*
*********

gen auto_ch=.



**********
*compu_ch*
**********
gen compu_ch=.

 
 
*************
*internet_ch*
*************

gen internet_ch=.


********
*cel_ch*
********


gen cel_ch=.

replace cel_ch=1 if  I152=="1"
replace cel_ch=0 if  I152=="2"

**********
*vivi1_ch*
**********

gen vivi1_ch=.


**********
*vivi2_ch*
**********

gen vivi2_ch=.



*************
*viviprop_ch*
*************

gen viviprop_ch=.





************
*vivitit_ch*
************

gen vivitit_ch=.

************
*vivialq_ch*
************

replace I032="" if I032=="N"
destring I032, replace

gen vivialq_ch=.


replace vivialq_ch=I031 if I032==4
replace vivialq_ch=I031*4.3 if I032==3
replace vivialq_ch=I031/12 if I032==5

***************
*vivialqimp_ch*
***************

gen vivialqimp_ch=.

cap ren idp_c idp_ci


save "X:\ARM\JAM\2005\JSLC\Arm_data\JAM2005EA_BID.dta", replace



save "X:\ARM\JAM\2005\JAM2005EA_BID.dta", replace


