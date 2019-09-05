
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

local PAIS HND
local ENCUESTA EPHPM
local ANO "2004"
local ronda m3

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
   
capture log close
log using "`log_file'", replace 

log off
/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Honduras
Encuesta: EPHPM
Round: m5
Autores: Revised March, 2008 (by tede) 
Última versión: María Laura Oliveri (MLO) - Email: mloliveri@iadb.org, lauraoliveri@yahoo.com
Fecha última modificación: 9 de Septiembre de 2013

							SCL/LMK - IADB
****************************************************************************/
/***************************************************************************
Detalle de procesamientos o modificaciones anteriores:
*****                            HONDURAS 2004 - MARZO                                                *****
*****                EPHPM 2004 (Encuesta Permanente de Hogares de Propositos Multiples)              ***** 
*****                                  	       personas                                               ***** 
*****                                          hogares                                                *****

****************************************************************************/

clear all
set more off
use "`base_in'", clear

foreach v of varlist _all {
      capture rename `v' `=lower("`v'")'
   }

		**********************************
		***VARIABLES DEL IDENTIFICACION***
		**********************************
		
	****************
	* region_BID_c *
	****************
	
gen region_BID_c=1

label var region_BID_c "Regiones BID"
label define region_BID_c 1 "Centroamérica_(CID)" 2 "Caribe_(CCB)" 3 "Andinos_(CAN)" 4 "Cono_Sur_(CSC)"
label value region_BID_c region_BID_c

************
****pais****
************

gen pais_c="HND"

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

*********
*edad_ci*
*********

gen edad_ci=edad

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
/*
************
***emp_ci***
************

gen emp_ci=.
replace emp_ci=1 if p12==1 | p13==1 /* p12 & p13 for 2004 */
replace emp_ci=0 if p12==2 & p13==2 
label var emp_ci "Empleado en la semana de referencia"
*/

****************
****condocup_ci*
****************

gen condocup_ci=.
replace condocup_ci=condact
replace condocup_ci=4 if condact == 4 | edad_ci<10
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


***************
*subemp_ci    *	
***************
gen subemp_ci=.
*falta generar
label var subemp_ci "Trabajadores subempleados"

*************
*firmapeq_ci*
*************
gen firmapeq_ci=.
*falta generar

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

/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/
*********
*lp25_ci
*********
gen lp25_ci =.

label var lp25_ci "Linea de pobreza de uds1.25 por dia en moneda local"

*********
*lp4_ci*
*********
gen lp4_ci = .

label var lp4_ci "Linea de pobreza de uds4 por dia en moneda local"

*********
*lp_ci***
*********

gen lp_ci =.
label var lp_ci "Linea de pobreza oficial del pais"

*********
*lpe_ci***
*********

gen lpe_ci =.
label var lpe_ci "Linea de indigencia oficial del pais"

****************
*cotizando_ci***
****************
gen cotizando_ci=.

label var cotizando_ci "Cotizante a la Seguridad Social"

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
label var tipocontrato_ci "Tipo de contrato segun su duracion"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci


*************
*cesante_ci* 
*************

gen cesante_ci=.
*falta crear
label var cesante_ci "Desocupado - definicion oficial del pais"	

*************
*tamemp_ci
*************

gen tamemp_ci=.
*falta crear
label var tamemp_ci "# empleados en la empresa"

*************
**pension_ci*
*************
gen yjub= .
gen ypensi=  .
egen aux1=rowtotal(yjub ypensi), missing


gen pension_ci =.
replace pension_ci=1 if aux1!=. & aux1!=0
recode pension_ci .=0
drop aux1
label var pension_ci "1=Recibe pension contributiva"

*************
*ypen_ci*
*************

egen ypen_ci=rowtotal(yjub ypensi), missing
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

**********
**tc_ci***
**********
gen tc_ci= .

label var tc_ci "Tipo de cambio LCU/USD"

*************
**salmm_ci***
*************

* HON 2004
gen salmm_ci= 	.

label var salmm_ci "Salario minimo legal"

*************
*tecnica_ci**
*************

gen tecnica_ci=.
*falta crear
label var tecnica_ci "1=formacion terciaria tecnica"

gen semestre_c=.
gen idp_c=.
gen relacion_ci=rela_j
gen sexo_ci= sexo
gen civil_ci=.
gen jefe_ci=.
gen nconyuges_ch=.
gen nhijos_ch=.
gen notropari_ch=.
gen notronopari_ch=.
gen nempdom_ch=.
gen clasehog_ch=.
gen nmayor21_ch=.
gen  nmenor21_ch=.
gen  nmayor65_ch=.
gen nmenor6_ch=.
gen nmenor1_ch=.
gen ocupa_ci=.
gen horaspri_ci=.
gen horastot_ci=.
gen  ylmotros_ci=.
gen ylnmotros_ci=.
gen tcylmpri_ci=.
gen tcylmpri_ch=.
gen ylmhopri_ci=.
gen ylmho_ci =.
gen rentaimp_ch=.
gen autocons_ci=.
gen autocons_ch=.
gen durades_ci=.
gen antiguedad_ci =.
gen desalent_ci=.
gen tiempoparc_ci =.
gen  nempleos_ci=.
gen spublico_ci=.
gen aedu_ci =.
gen  eduno_ci =.
gen edupi_ci=.
gen	edupc_ci=.
gen	edusi_ci=.
gen	edusc_ci=.
gen	eduui_ci=.
gen	eduuc_ci=.
gen	edus1i_ci=.
gen	edus1c_ci=.
gen	edus2i_ci =.
gen edus2c_ci=.
gen	edupre_ci=.
gen	eduac_ci=.
gen asiste_ci=.
gen pqnoasis=.
gen	repite_ci=.
gen	repiteult_ci=.
gen	edupub_ci_ci=.
gen aguared_ch=.
gen aguadist_ch=.
gen	aguamala_ch=.
gen	aguamide_ch=.
gen luz_ch=.
gen	luzmide_ch=.
gen	combust_ch=.
gen	bano_ch=.
gen	banoex_ch=.
gen des1_ch=.
gen	des2_ch=.
gen	piso_ch=.
gen	pared_ch=.
gen	techo_ch=.
gen	resid_ch=.
gen	dorm_ch=.
gen	cuartos_ch=.
gen	cocina_ch=.
gen	telef_ch=.
gen refrig_ch=.
gen	freez_ch=.
gen	auto_ch=.
gen	compu_ch=.
gen	internet_ch=.	
gen cel_ch=.
gen	vivi1_ch=.
gen	vivi2_ch=.
gen	viviprop_ch=.
gen vivitit_ch=.
gen	vivialq_ch=.
gen	vivialqimp_ch=.
gen region_c=.
gen raza_ci=.
gen instcot_ci=.

**Verificación de que se encuentren todas las variables del SOCIOMETRO y las nuevas de mercado laboral
qui sum factor_ch	idh_ch	idp_c	zona_c	pais_c	anio_c	mes_c	relacion_ci	factor_ci	sexo_ci	edad_ci	civil_ci	///
jefe_ci	nconyuges_ch	nhijos_ch	notropari_ch	notronopari_ch	nempdom_ch	clasehog_ch	nmiembros_ch	///
miembros_ci	nmayor21_ch	nmenor21_ch	nmayor65_ch	nmenor6_ch	nmenor1_ch	ocupa_ci	rama_ci	horaspri_ci	///
horastot_ci	ylmpri_ci	ylnmpri_ci	ylmsec_ci	ylnmsec_ci	ylmotros_ci	ylnmotros_ci	nrylmpri_ci	tcylmpri_ci ///
ylm_ci	ylnm_ci	ynlm_ci	ynlnm_ci	nrylmpri_ch	tcylmpri_ch	ylm_ch	ylnm_ch	ylmnr_ch	ynlm_ch	ynlnm_ch	///
ylmhopri_ci	ylmho_ci	rentaimp_ch	autocons_ci	autocons_ch	remesas_ci	remesas_ch	durades_ci	antiguedad_ci ///
emp_ci	desemp_ci	pea_ci	 desalent_ci	subemp_ci	tiempoparc_ci ///
categopri_ci	categosec_ci	nempleos_ci	firmapeq_ci	spublico_ci	aedu_ci	eduno_ci ///
edupi_ci	edupc_ci	edusi_ci	edusc_ci	eduui_ci	eduuc_ci	edus1i_ci	edus1c_ci	edus2i_ci ///
edus2c_ci	edupre_ci	eduac_ci	asiste_ci	pqnoasis	repite_ci	repiteult_ci	edupub_ci	///
aguared_ch	aguadist_ch	aguamala_ch	aguamide_ch	luz_ch	luzmide_ch	combust_ch	bano_ch	banoex_ch	///
des1_ch	des2_ch	piso_ch	pared_ch	techo_ch	resid_ch	dorm_ch	cuartos_ch	cocina_ch	telef_ch ///
refrig_ch	freez_ch	auto_ch	compu_ch	internet_ch	cel_ch	vivi1_ch	vivi2_ch	viviprop_ch	///
vivitit_ch	vivialq_ch	vivialqimp_ch region_BID_c region_c raza_ci        lp25_ci	       lp4_ci	 ///
lp_ci	       lpe_ci	       cotizando_ci	             afiliado_ci	///
tipopen_ci	   instpen_ci	   instcot_ci	   instpen_ci	   tipocontrato_ci 	   condocup_ci 	   cesante_ci ///
tamemp_ci 	   pension_ci 	   ypen_ci 	   pensionsub_ci 	   ypensub_ci 	   salmm_ci	   tecnica_ci	


qui destring $var, replace
 

* Activar solo si es necesario
*keep *_ci  *_c  idh_ch 
qui compress



saveold "`base_out'", replace


log close


