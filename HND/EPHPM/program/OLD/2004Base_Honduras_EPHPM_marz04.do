***********************************************************************************************************
*****                            HONDURAS 2004 - MARZO                                                *****
*****                EPHPM 2004 (Encuesta Permanente de Hogares de Propositos Multiples)              ***** 
*****                                  	       personas                                               ***** 
*****                                          hogares                                                *****
***********************************************************************************************************

*** Revised March, 2008 (by tede) ***

clear
cd X:\ARM\HON\2004\Marzo\Arm_data

capture log close
log using HON2004EA_BID.log, replace 

set more off

use "X:\ARM\HON\2004\Marzo\Orig_data\hnd04-my.dta", clear

************
****pais****
************

gen pais_c="HON"

**********
***anio***
**********

gen anio_c=2004

*********
***mes***
*********

gen mes_c=3
label define mes_c 3 "Marzo" 5 "Mayo" 9 "Septiembre"
label value mes_c mes_c

***************
****idh_ch*****
***************

replace numhog=1 if numhog==.
egen idh_ch=group(hogar depto domi ur numhog) 

***************
**factor_ch****
***************

gen factor_ch=factor

***************
****factor_ci**
***************

gen factor_ci=factor_ch

**********
***zona***
**********

gen zona_c=1 if domi==1 | domi==2 | domi==3 | domi==4
replace zona_c=0 if domi==5
label define zona_c 0 "Rural" 1 "Urbana" 
label value zona_c zona_c

******************
*** miembros_ch***
******************

gen miembros_ci=1 if rela_j>=1 & rela_j<=8
replace miembros_ci=0 if rela_j==9

gen uno=1 if miembros_ci==1
egen nmiembros_ch=sum(uno), by(idh_ch)
replace nmiembros_ch=. if miembros_ci!=1
label var nmiembros_ch "Numero de miembros del Hogar"
drop uno

******************
***categopri_ci***
******************

gen categopri_ci=1 if p29a==7 /* p29a for March 2004 */
replace categopri_ci=2 if p29a==4 | p29a==5 | p29a==6
replace categopri_ci=3 if p29a==1 | p29a==2 | p29a==3
replace categopri_ci=4 if p29a==12 | p29a==13
label var categopri_ci "Categoria ocupacional actividad principal"
label define categopri_ci 1 "Patron" 2 "Cuenta Propia" 3 "Empleado" 4 "Trabajador no remunerado"
label value categopri_ci categopri_ci

******************
***categosec_ci***
******************

gen categosec_ci=1 if p41a==7 /* p41a for March 2004 */
replace categosec_ci=2 if p41a==4 | p41a==5 | p41a==6
replace categosec_ci=3 if p41a==1 | p41a==2 | p41a==3
replace categosec_ci=4 if p36==12 | p36==13
label var categosec_ci "Categoria ocupacional actividad secundaria"
label define categosec_ci 1 "Patron" 2 "Cuenta Propia" 3 "Empleado" 4 "Trabajador no remunerado"
label value categosec_ci categosec_ci

************
***emp_ci***
************

gen emp_ci=.
replace emp_ci=1 if p12==1 | p13==1 /* p12 & p13 for 2004 */
replace emp_ci=0 if p12==2 & p13==2 
label var emp_ci "Empleado en la semana de referencia"

****************
***ylmpri_ci ***
****************

gen ylmpri_ci=.
replace ylmpri_ci=p33 if p33<99999 & edad>4 & p31==1 /* p33 & p31 for 2004 */
replace ylmpri_ci=p33*p32 if p33<99999 & edad>4 & (p31==2 | p31==3 | p31==4) /* p32 for 2004 */
replace ylmpri_ci=p35 if p35<999999 & edad>4 & p35>=0 /* p35 for 2004 */
replace ylmpri_ci=0 if p33==0 & p35==0 & edad>4 & (p12==1 | p13==1)
replace ylmpri_ci=0 if (p29a==12 | p29a==13) & edad>4 & (p12==1 | p13==1)
label var ylmpri_ci "Ingreso Laboral Monetario de la Actividad Principal"

****************
***ylmsec_ci ***
****************

gen ylmsec_ci=.
replace ylmsec_ci=p45 if p45<99999 & edad>4 & p43==1 /* p45 & p43 for 2004 */
replace ylmsec_ci=p45*p39 if p45<99999 & edad>4 & (p43==2 | p43==3 | p43==4)
replace ylmsec_ci=p47 if p47<99999 & edad>4 & p47>=0 /* p47 for 2004 */
replace ylmsec_ci=0 if p45==0 & p47==0 & edad>4 & p37==1 /* p37 for 2004 */
replace ylmsec_ci=0 if (p41a==12 | p41a==13) & edad>4 & p37==1 /* p41a for 2004 */
label var ylmsec_ci "Ingreso Laboral Monetario de la Actividad Secundaria"

************
***ylm_ci***
************

egen ylm_ci=rsum(ylmpri_ci ylmsec_ci)
replace ylm_ci=. if ylmpri_ci==. & ylmsec_ci==.
label var ylm_ci "Ingreso Laboral Monetario Total"

*****************
***ylnmpri_ci ***
*****************

gen yalim2=p34a if p34a<99999 & p34a>=0 /* p34 for 2004 */
gen yropa2=p34b if p34b<99999 & p34b>=0
gen yhabita2=p34c if p34c<99999 & p34c>=0
gen ytrans2=p34d if p34d<99999 & p34d>=0
gen yotro2=p34e if p34e<99999 & p34e>=0
gen yprodu2=p36 if p36<99999 & p36>=0 /* p36 for 2004 */

egen ylnmpri_ci=rsum(yalim2 yropa2 yhabita2 ytrans2 yotro2 yprodu2)
replace ylnmpri_ci=. if yalim2==. & yropa2==. & yhabita2==. & ytrans2==. & yotro2==. & yprodu2==.
replace ylnmpri_ci=0 if ((p34a==. & p34b==. & p34c==. & p34d==. & p34e==. & categopri==3) | (p36==. & (categopri==1 | categopri==2))) & edad>4 & (p12==1 | p13==1)
label var ylnmpri_ci "Ingreso Laboral No Monetario de la Actividad Principal"

*****************
***ylnmsec_ci ***
*****************

gen yalim3=p46a if p46a<99999 & p46a>=0 /* p46 for 2004 */
gen yropa3=p46b if p46b<99999 & p46b>=0
gen yhabita3=p46c if p46c<99999 & p46c>=0
gen ytrans3=p46d if p46d<99999 & p46d>=0
gen yotro3=p46e if p46e<99999 & p46e>=0
gen yprodu3=p48 if p48<99999 & p48>=0 /* p48 for 2004 */

egen ylnmsec_ci=rsum(yalim3 yropa3 yhabita3 ytrans3 yotro3 yprodu3)
replace ylnmsec_ci=. if yalim3==. & yropa3==. & yhabita3==. & ytrans3==. & yotro3==. & yprodu3==.
replace ylnmsec_ci=0 if ((p46a==. & p46b==. & p46c==. & p46d==. & p46e==. & categosec==3) | (p48==. & (categosec==1 | categosec==2))) & edad>4 & p32==1
label var ylnmsec_ci "Ingreso Laboral No Monetario de la Actividad Secundaria"

*****************
*** ylnm_ci *****
*****************

egen ylnm_ci=rsum(ylnmpri_ci ylnmsec_ci)
replace ylnm_ci=. if ylnmpri_ci==. & ylnmsec_ci==.
label var ylnm_ci "Ingreso Laboral No Monetario Total"

***********************
*** autoconsumop_ci ***
***********************

gen autoconsumop_ci=yprodu2 
replace autoconsumop_ci=0 if p36==. & edad>4 & (categopri==1 | categopri==2) & (p12==1 | p13==1) /* p36 for 2004 */
label var autoconsumop_ci "Autoconsumo Individual Actividad Principal(Trabajadores Independientes)"

***********************
*** autoconsumos_ci ***
***********************

gen autoconsumos_ci=yprodu2 
replace autoconsumos_ci=0 if p48==. & edad>4 & (categosec==1 | categosec==2) & p37==1 
label var autoconsumos_ci "Autoconsumo Individual Actividad Secundaria(Trabajadores Independientes)"

***********************
*** autoconsumo_ci ***
***********************

egen autoconsumo_ci=rsum(autoconsumop_ci autoconsumos_ci)
replace autoconsumo_ci=. if autoconsumop_ci==. & autoconsumos_ci==.
label var autoconsumo_ci "Autoconsumo Individual (Trabajadores Independientes)"

*************
***ypenju2***
*************

gen ypenju2=p5201c03/3 if p5201c03>=0 /* p5201c03 for 2004 */ 
replace ypenju2=p5201c02 if p5201c02>=0 & (p5201c03==. | p5201c03==0) /* p5201c02 for 2004 */

***********
***yjub2***
***********

gen yjub2=p5203c03/3 if  p5203c03>=0 /*  p5203c03 for 2004 */
replace  yjub2=p5203c02 if p5203c02>=0 & (p5203c03==. | p5203c03==0) /* p5203c02 for 2004 */

***************
***yalquile2***
***************

gen yalquile2=p5205c03/3 if  p5205c03>=0 /*  p5205c03 for 2004 */
replace yalquile2= p5205c02 if  p5205c02>=0 & (p5205c03==. | p5205c03==0) /*  p5205c02 for 2004 */

***************
***ysubsi2*****
***************

gen ysubsi2=p5207c03/3 if p5207c03>=0 /* p5207c03 for 2004 */
replace ysubsi2= p5207c02 if  p5205c02>=0 & (p5207c03==. | p5207c03==0) /*  p5207c02 for 2004 */

***************
***ybonos2*****
***************

gen ybonos2=p5217c03/3 if p5217c03>=0 /*  p5217c03 for 2004 */
replace ybonos2=p5217c02 if  p5217c02>=0 & (p5217c03==. | p5217c03==0) /* p5217c02 for 2004 */

****************
***yremesa2*****
****************

gen yremesa2=p5209c03/3 if p5209c03>=0 /* p5209c03 for 2004 */
replace yremesa2=p5209c02 if p5209c02>=0 & (p5209c03==. | p5209c03==0) /* p5209c02 for 2004 */ 
/* Tipo de cambio lempiras por dolares (Banco Central de Honduras) a enero 2004: 17.98 
                                                                     febrero 2004: 18.06 
								     marzo 2004: 18.14 
								     promedio 3 meses: 18.24 */

****************
***yremesad2****
****************

gen yremesad2=p5210c03/3*18.24 if p5210c03>=0 /* p5210c03 for 2004 */
replace yremesad2=p5210c02*18.24 if p5210c02>=0 & (p5210c03==. | p5210c03==0) /* p5210c02 for 2004 */

****************
***yayuda2******
****************

gen yayuda2=p5214c03/3 if p5214c03>=0 /* p5214c03 for 2004 */
replace yayuda2=p5214c02 if p5214c02>=0 & (p5214c03==. | p5214c03==0) /* p5214c02 for 2004*/ 

****************
***yayupar2*****
****************

gen yayupar2=p5216c03/3 if p5216c03>=0 /* p5216c03 for 2004 */ 
replace yayupar2=p5216c02 if p5216c02>=0 & (p5216c03==. | p5216c03==0) /* p5216c02 for 2004 */

****************
***yotros2******
****************

gen yotros2=p5219c03/3 if p5219c03>=0 /*  p5219c03 for 2004 */ 
replace yotros2= p5219c02 if p5219c02>=0 & (p5219c03==. | p5219c02==0) /*  p5219c02 for 2004 */

****************
***interes2*****
****************

gen interes2=p5208c03/3 if p5208c03>=0 /* p5208c03 for 2004 */
replace interes2=p5208c02 if p5208c02>=0 & (p5208c03==. | p5208c03==0) /* p5208c02 for 2004 */

****************
***prestlab2****
**************** 

gen prestlab2=p5218c03/3 if p5218c03>=0 /* p5218c03 for 2004 */ 
replace prestlab2=p5218c02 if p5218c02>=0 & (p5218c03==. | p5218c03==0) /* p5218c02 for 2004 */

****************
***yremerasde***
**************** 

gen yremesade=(p5212c03/3)*18.24 if p5212c03>=0 /* p5212c03 for 2004 */ 
replace yremesade=p5212c02*18.24 if p5212c02>=0 & (p5212c03==. | p5212c03==0) /* p5212c02 for 2004 */

****************
***yremerase****
**************** 

gen yremesae=p5211c03/3 if p5211c03>=0 /* p5211c03 for 2004 */
replace yremesae=p5211c02 if p5211c02>=0 & (p5211c03==. | p5211c03==0) /* p5211c02 for 2004 */

****************
***yayudae******
**************** 

gen yayudae=p5215c03/3 if p5215c03>=0 /* p5215c03 for 2004 */
replace yayudae=p5215c02 if p5215c02>=0 & (p5215c03==. | p5215c03==0) /* p5215c02 for 2004 */

/* No estan las ayudas de particulares en especie:*/

/* No estan las herencias */

*Hay pension por divorcio: p5213c03 & p5213c02 for 2004 */ 

****************
***ypdiv********
**************** 

gen ypdiv=p5213c03/3 if p5213c03>=0
replace ypdiv=p5213c02 if p5213c02>=0 & (p5213c03==. | p5213c03==0)

sum ypdiv if ypdiv>=0

***********************
*** remesasm_ci     ***
***********************

egen remesasm_ci=rsum(yremesa2 yremesad2)
replace remesasm_ci=. if yremesa2==. & yremesad2==. 
replace remesasm_ci=0 if p5209c03==0 & p5209c02==0 & p5210c03==0 & p5210c02==0 
label var remesasm_ci "Remesas Individuales (monetario)"

***********************
*** remesas_ci     ***
***********************

egen remesas_ci=rsum(yremesa2 yremesad2 yremesade yremesae)
replace remesas_ci=. if yremesa2==. & yremesad2==. & yremesade==. & yremesae==.
replace remesas_ci=0 if p5209c03==0 & p5209c02==0 & p5210c03==0 & p5210c02==0 & p5212c03==0 & p5212c02==0 & p5211c03==0 & p5211c02==0 
label var remesas_ci "Remesas Individuales (monetario + especies)"

***********************
*** ynlm_ci         ***
***********************

egen ynlm_ci=rsum(ypenju2 yjub2 ysubsi2 yalquile2 ybonos2 yremesa2 yremesad2 yayuda2 yayupar2 yotros2)
replace ynlm_ci=0 if p5201c03==. & p5201c02==. & p5203c03==. & p5203c02==. & p5205c03==. & p5205c02==. & p5207c03==. & p5207c02==. & p5217c03==. & p5217c02==. & p5209c03==. & p5209c02==. & p5210c03==. & p5210c02==. & p5214c03==. & p5214c02==. & p5216c03==. & p5216c02==. & p5219c03==. & p5219c02==.
replace ynlm_ci=. if ypenju2==. & ysubsi2==. & yalquile2==. & ybonos2==. & yremesa2==. & yayuda2==. & yayupar2==. & yotros2==. & yjub2==. & yremesad2==.
label var ynlm_ci "Ingreso No Laboral Monetario"

***********************
*** ynlm2_ci        ***
***********************

egen ynlm2_ci=rsum(ypenju2 yjub2 ysubsi2 yalquile2 ybonos2 yremesa2 yremesad2 yayuda2 yayupar2 yotros2 interes2 prestlab2)
replace ynlm2_ci=. if ypenju2==. & ysubsi2==. & yalquile2==. & ybonos2==. & yremesa2==. & yayuda2==. & yayupar2==. & yotros2==. & interes2==. & prestlab2==. & yjub2==. & yremesad2==.
replace ynlm2_ci=0 if p5201c03==. & p5201c02==. & p5203c03==. & p5203c02==. & p5205c03==. & p5205c02==. & p5207c03==. & p5207c02==. & p5217c03==. & p5217c02==. & p5209c03==. & p5209c02==. & p5210c03==. & p5210c02==. & p5214c03==. & p5214c02==. & p5216c03==. & p5216c02==. & p5219c03==. & p5219c02==. & p5208c02==. & p5208c02==. & p5218c03==. & p5218c02==.   
label var ynlm2_ci "Ingreso No Laboral Monetario 2"

***********************
*** ynlm4_ci        ***
***********************

egen ynlm4_ci=rsum(ypenju2 yjub2 ysubsi2 yalquile2 ybonos2 yremesa2 yremesad2 yayuda2 yayupar2 yotros2 interes2 prestlab2 ypdiv)
replace ynlm4_ci=. if ypenju2==. & ysubsi2==. & yalquile2==. & ybonos2==. & yremesa2==. & yayuda2==. & yayupar2==. & yotros2==. & interes2==. & prestlab2==. & yjub2==. & yremesad2==. & ypdiv
replace ynlm4_ci=0 if p5201c03==. & p5201c02==. & p5203c03==. & p5203c02==. & p5205c03==. & p5205c02==. & p5207c03==. & p5207c02==. & p5217c03==. & p5217c02==. & p5209c03==. & p5209c02==. & p5210c03==. & p5210c02==. & p5214c03==. & p5214c02==. & p5216c03==. & p5216c02==. & p5219c03==. & p5219c02==. & p5208c02==. & p5208c02==. & p5218c03==. & p5218c02==.  & p5213c03==. & p5213c02==. 
label var ynlm4_ci "Ingreso No Laboral Monetario 4"

***********************
*** ynlnm_ci        ***
***********************

egen ynlnm_ci=rsum(yremesade yremesae yayudae) 
replace ynlnm_ci=. if yremesade==. & yremesae==. & yayudae==. 
replace ynlnm_ci=0 if p5212c03==. & p5212c02==. & p5211c03==. & p5211c02==. & p5215c03==. & p5215c02==.
label var ynlnm_ci "Ingreso No Laboral No Monetario"

***********************
*** ynl_ci          ***
***********************

egen ynl_ci=rsum(ypenju2 yjub2 ysubsi2 yalquile2 ybonos2 yremesa2 yremesad2 yayuda2 yayupar2 yotros2 interes2 prestlab2 yremesade yremesae yayudae ypdiv)
replace ynl_ci=. if ypenju2==. & ysubsi2==. & yalquile2==. & ybonos2==. & yremesa2==. & yayuda2==. & yayupar2==. & yotros2==. & interes2==. & prestlab2==. & yjub2==. & yremesad2==. & yremesade==. & yremesae==. & yayudae==. & ypdiv==.
replace ynl_ci=0 if p5201c03==. & p5201c02==. & p5203c03==. & p5203c02==. & p5205c03==. & p5205c02==. & p5207c03==. & p5207c02==. & p5217c03==. & p5217c02==. & p5209c03==. & p5209c02==. & p5210c03==. & p5210c02==. & p5214c03==. & p5214c02==. & p5216c03==. & p5216c02==. & p5219c03==. & p5219c02==. & p5208c02==. & p5208c02==. & p5218c03==. & p5218c02==.  & p5213c03==. & p5213c02==. & p5212c03==. & p5212c02==. & p5211c03==. & p5211c02==. & p5215c03==. & p5215c02==.
label var ynl_ci "Ingreso No Laboral (Monetario + No Monetario)"

***********************
*** nrylmpri_ci     ***
*********************** 

gen nrylmpri_ci=0 
replace nrylmpri_ci=1 if p33==99999
replace nrylmpri_ci=1 if p35==999999
label var nrylmpri_ci "Identificador de No Respuesta del Ingreso Monetario de la Actividad Principal"

***********************
*** ylmnr_ci        ***
***********************

egen ylmnr_ci=rsum(ylmpri_ci ylmsec_ci) if nrylmpri_ci==0
replace ylmnr_ci=. if ylmpri_ci==. 
label var ylmnr_ci "Ingreso Laboral Monetario Total, considera 'missing' la No Respuesta "

***********************
*** yl_ci           ***
***********************

egen yl_ci=rsum(ylmnr_ci ylnm_ci)
replace yl_ci=. if ylmnr_ci==. & ylnm_ci==.
label var yl_ci "Ingreso Laboral Individual (Monetario + No Monetario)"

***********************
*** nrylmpri_ch     ***
***********************

egen nrylmpri_ch=sum(nrylmpri_ci) if miembros_ci==1, by(idh_ch)
replace nrylmpri_ch=1 if nrylmpri_ch>1 & nrylmpri_ch<.
label var nrylmpri_ch "Identificador de Hogares en donde alguno de los miembros No Responde el Ingreso Monetario de la Actividad Principal"

***********************
*** ylm_ch          ***
***********************

egen ylm_ch=sum(ylm_ci) if miembros_ci==1, by(idh_ch)
label var ylm_ch "Ingreso Laboral Monetario del Hogar"

***********************
*** ylmnr_ch        ***
***********************

egen ylmnr_ch=sum(ylm_ci) if miembros_ci==1 & nrylmpri_ch==0, by(idh_ch)
label var ylmnr_ch "Ingreso Laboral Monetario del Hogar, considera 'missing' la No Respuesta"

***********************
*** ylnm_ch         ***
***********************

egen ylnm_ch=sum(ylnm_ci) if miembros_ci==1, by(idh_ch)
label var ylnm_ch "Ingreso Laboral No Monetario del Hogar"

***********************
*** ynlm_ch         ***
***********************

egen ynlm_ch=sum(ynlm_ci) if miembros_ci==1, by(idh_ch)
label var ynlm_ch "Ingreso No Laboral Monetario del Hogar"

***********************
*** ynlm2_ch        ***
***********************

egen ynlm2_ch=sum(ynlm2_ci) if miembros_ci==1, by(idh_ch)
label var ynlm2_ch "Ingreso No Laboral Monetario 2 del Hogar"

***********************
*** ynlm4_ch        ***
***********************

egen ynlm4_ch=sum(ynlm4_ci) if miembros_ci==1, by(idh_ch)
label var ynlm4_ch "Ingreso No Laboral Monetario 4 del Hogar"

***********************
*** ynlnm_ch        ***
***********************

egen ynlnm_ch=sum(ynlnm_ci) if miembros_ci==1, by(idh_ch)
label var ynlnm_ch "Ingreso No Laboral No Monetario del Hogar"

***********************
*** ynl_ch          ***
***********************

egen ynl_ch=sum(ynl_ci) if miembros_ci==1, by(idh_ch)
label var ynl_ch "Ingreso No Laboral del Hogar (monetario + no monetario)"

***********************
*** autoconsumo_ch  ***
***********************

egen autoconsumo_ch=sum(autoconsumo_ci) if miembros_ci==1, by(idh_ch)
label var autoconsumo_ch "Autoconsumo del Hogar"

***********************
*** remesasm_ch     ***
***********************

egen remesasm_ch=sum(remesasm_ci) if miembros_ci==1, by(idh_ch)
label var remesasm_ch "Remesas del Hogar (monetario)"
egen remesas_ch=sum(remesas_ci) if miembros_ci==1, by(idh_ch)
label var remesas_ch "Remesas del Hogar (monetario + especies)"

***********************
*** rama_ci         ***
***********************

gen rama_ci=rama
replace rama_ci=. if rama==10 | rama==11 | emp_ci==0

drop yalim2 yropa2 yhabita2 ytrans2 yotro2 yprodu2 ypenju2 ysubsi2 yalquile2 ybonos2 yayuda2 yayupar2 yotros2 interes2 prestlab2 yalim3 yropa3 yhabita3 ytrans3 yotro3 yprodu3 yjub2 yremesade yremesae yayudae ypdiv autoconsumop_ci autoconsumos_ci

save "X:\ARM\HON\2004\Marzo\Arm_data\HON2004_1EA_BID.dta", replace

log close

clear
