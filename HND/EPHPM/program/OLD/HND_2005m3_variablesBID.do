
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
 


*global ruta = "${surveysFolder}"

local PAIS HND
local ENCUESTA EPHPM
local ANO "2005"
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
*****                            HONDURAS 2005 - MARZO                                                *****
*****                EPHPM 2005 (Encuesta Permanente de Hogares de Propositos Multiples)              ***** 
*****                                  	       personas                                             ***** 
*****                                          hogares                                                *****
*** Revised March, 2008 (by tede) ***

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

gen edad_ci=edad
************
****pais****
************

gen pais_c="HND"

**********
***anio***
**********

gen anio_c=2005

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
***nmiembros_ch***
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

gen categopri_ci=1 if p43==7 /* p43 for March 2005 */
replace categopri_ci=2 if p43==4 | p43==5 | p43==6
replace categopri_ci=3 if p43==1 | p43==2 | p43==3
replace categopri_ci=4 if p43==12 | p43==13
label var categopri_ci "Categoria ocupacional actividad principal"
label define categopri_ci 1 "Patron" 2 "Cuenta Propia" 3 "Empleado" 4 "Trabajador no remunerado"
label value categopri_ci categopri_ci

******************
***categosec_ci***
******************

gen categosec_ci=1 if p66==7 /* p66 for March 2005 */
replace categosec_ci=2 if p66==4 | p66==5 | p66==6
replace categosec_ci=3 if p66==1 | p66==2 | p66==3
replace categosec_ci=4 if p66==12 | p66==13
label var categosec_ci "Categoria ocupacional actividad secundaria"
label define categosec_ci 1 "Patron" 2 "Cuenta Propia" 3 "Empleado" 4 "Trabajador no remunerado"
label value categosec_ci categosec_ci
/*
************
***emp_ci***
************

gen emp_ci=.
replace emp_ci=1 if p17==1 | p18==1 /* p17 & p18 for 2005 */
replace emp_ci=0 if p17==2 & p18==2 
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
****************
***ylmpri_ci ***
****************

gen ylmpri_ci=.
replace ylmpri_ci=p49 if p49<99999 & edad_ci>4 & p47==1 /* p49 & p47 for 2005 */
replace ylmpri_ci=p49*p48 if p49<99999 & edad_ci>4 & (p47==2 | p47==3 | p47==4) /* p48 for 2005 */
replace ylmpri_ci=p58 if p58<999999 & edad_ci>4 & p58>=0 /* p58 for 2005 */
replace ylmpri_ci=0 if p49==0 & p58==0 & edad_ci>4 & (p17==1 | p18==1)
replace ylmpri_ci=0 if (p43==12 | p43==13) & edad_ci>4 & (p17==1 | p18==1)
label var ylmpri_ci "Ingreso Laboral Monetario de la Actividad Principal"

****************
***ylmsec_ci ***
****************

gen ylmsec_ci=.
replace ylmsec_ci=p72 if p72<99999 & edad_ci>4 & p70==1 /* p72 & p70 for 2005 */
replace ylmsec_ci=p72*p71 if p72<99999 & edad_ci>4 & (p70==2 | p70==3 | p70==4)
replace ylmsec_ci=p81 if p81<99999 & edad_ci>4 & p81>=0 /* p81 for 2005 */ 

replace ylmsec_ci=0 if p72==0 & p81==0 & edad_ci>4 & p61==1 /* p61 for 2005 */
replace ylmsec_ci=0 if (p66==12 | p66==13) & edad_ci>4 & p61==1 /* p66 for 2005 */
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

gen yalim2=p50l1 if p50l1<99999 & p50l1>=0 /* p50 for 2005 */
gen yropa2=p50l2 if p50l2<99999 & p50l2>=0
gen yhabita2=p50l3 if p50l3<99999 & p50l3>=0
gen ytrans2=p50l4 if p50l4<99999 & p50l4>=0
gen yotro2=p50l5 if p50l5<99999 & p50l5>=0
gen yprodu2=p59 if p59<99999 & p59>=0 /* p59 for 2005 */

egen ylnmpri_ci=rsum(yalim2 yropa2 yhabita2 ytrans2 yotro2 yprodu2)
replace ylnmpri_ci=. if yalim2==. & yropa2==. & yhabita2==. & ytrans2==. & yotro2==. & yprodu2==.
replace ylnmpri_ci=0 if ((p50l1==. & p50l2==. & p50l3==. & p50l4==. & p50l5==. & categopri==3) | (p59==. & (categopri==1 | categopri==2))) & edad_ci>4 & (p17==1 | p18==1)
label var ylnmpri_ci "Ingreso Laboral No Monetario de la Actividad Principal"

*****************
***ylnmsec_ci ***
*****************

gen yalim3=p73l1 if p73l1<99999 & p73l1>=0 /* p73 for 2005 */
gen yropa3=p73l2 if p73l2<99999 & p73l2>=0
gen yhabita3=p73l3 if p73l3<99999 & p73l3>=0
gen ytrans3=p73l4 if p73l4<99999 & p73l4>=0
gen yotro3=p73l5 if p73l5<99999 & p73l5>=0
gen yprodu3=p82 if p82<99999 & p82>=0 /* p82 for 2005 */

egen ylnmsec_ci=rsum(yalim3 yropa3 yhabita3 ytrans3 yotro3 yprodu3)
replace ylnmsec_ci=. if yalim3==. & yropa3==. & yhabita3==. & ytrans3==. & yotro3==. & yprodu3==.
replace ylnmsec_ci=0 if ((p73l1==. & p73l2==. & p73l3==. & p73l4==. & p73l5==. & categosec==3) | (p82==. & (categosec==1 | categosec==2))) & edad_ci>4 & p82==1
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
replace autoconsumop_ci=0 if p59==. & edad_ci>4 & (categopri==1 | categopri==2) & (p17==1 | p18==1) /* p59 for 2005 */
label var autoconsumop_ci "Autoconsumo Individual Actividad Principal(Trabajadores Independientes)"

***********************
*** autoconsumos_ci ***
***********************

gen autoconsumos_ci=yprodu2 
replace autoconsumos_ci=0 if p82==. & edad_ci>4 & (categosec==1 | categosec==2) & p61==1 
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

gen ypenju2=p87l01c5/3 if p87l01c5>=0 /* p87l01c5 for 2004 */ 
replace ypenju2=p87l01c1 if p87l01c1>=0 & (p87l01c5==. | p87l01c5==0) /* p87l01c1 March 2005 */

***********
***yjub2***
***********

gen yjub2=p87l02c5/3 if  p87l02c5>=0 /*  p87l02c5 March 2005 */
replace yjub2=p87l02c1 if p87l02c1>=0 & (p87l02c5==. | p87l02c5==0) /* p87l02c1 March 2005 */

***************
***yalquile2***
***************

gen yalquile2=p87l03c5/3 if  p87l03c5>=0 /*  p87l03c5 March 2005 */
replace yalquile2= p87l03c1 if  p87l03c1>=0 & (p87l03c5==. | p87l03c5==0) /*  p87l03c1 March 2005 */

***************
***ysubsi2*****
***************

gen ysubsi2=p87l04c5/3 if p87l04c5>=0 /* p87l04c5 March 2005 */
replace ysubsi2= p87l04c1 if  p87l04c1>=0 & (p87l04c5==. | p87l04c5==0) /*  p87l04c1 March 2005 */

***************
***ybonos2*****
***************

gen ybonos2=p87l10c5/3 if p87l10c5>=0 /*  p87l10c5 March 2005 */
replace ybonos2=p87l10c1 if  p87l10c1>=0 & (p87l10c5==. | p87l10c5==0) /* p87l10c1 March 2005 */

****************
***yremesa2*****
****************

gen yremesa2=p87l06c5/3 if p87l06c5>=0 /* p87l06c5 March 2005 */
replace yremesa2=p87l06c2 if p87l06c2>=0 & (p87l06c5==. | p87l06c5==0) /* p87l06c2 March 2005 */ 

/* Tipo de cambio lempiras por dolares (Banco Central de Honduras) a enero 2005: 18.88 
                                                                     febrero 2005: 18.95 
								     marzo 2005: 19.00 
								     promedio 3 meses: 18.94 */

****************
***yremesad2****
****************

gen yremesad2=(p87l06c6/3)*18.94 if p87l06c6>=0 /* p87l06c6 for March 2005 */
replace yremesad2=p87l06c2*18.94 if p87l06c2>=0 & (p87l06c6==. | p87l06c6==0) /* p87l06c2 for March 2005 */

****************
***yayuda2******
****************

gen yayuda2=p87l08c5/3 if p87l08c5>=0 /* p87l08c5 for March 2005 */
replace yayuda2=p87l08c1 if p87l08c1>=0 & (p87l08c5==. | p87l08c5==0) /* p87l08c1 for March 2005*/ 

****************
***yayupar2*****
****************

gen yayupar2=p87l09c5/3 if p87l09c5>=0 /* p87l09c5 for March 2005 */ 
replace yayupar2=p87l09c1 if p87l09c1>=0 & (p87l09c5==. | p87l09c5==0) /* p87l09c1 for March 2005 */

****************
***yotros2******
****************

gen yotros2=p87l12c5/3 if p87l12c5>=0 /*  p87l12c5 for March 2005 */ 
replace yotros2= p87l12c1 if p87l12c1>=0 & (p87l12c5==. | p87l12c5==0) /*  p87l12c1 for March 2005 */

****************
***interes2*****
****************

gen interes2=p87l05c5/3 if p87l05c5>=0 /* p87l05c5 for March 2005 */
replace interes2=p87l05c1 if p87l05c1>=0 & (p87l05c5==. | p87l05c5==0) /* p87l05c1 for March 2005 */

****************
***prestlab2****
**************** 

gen prestlab2=p87l11c5/3 if p87l11c5>=0 /* p87l11c5 for March 2005 */ 
replace prestlab2=p87l11c1 if p87l11c1>=0 & (p87l11c5==. | p87l11c5==0) /* p87l11c1 for March 2005 */

****************
***yremerasde***
**************** 

gen yremesade=(p87l06c8/3)*18.94 if p87l06c8>=0 /* p87l06c8 for March 2005 */ 
replace yremesade=p87l06c4*18.94 if p87l06c4>=0 & (p87l06c8==. | p87l06c8==0) /* p87l06c4 for March 2005 */

****************
***yremerase****
**************** 

gen yremesae=p87l06c7/3 if p87l06c7>=0 /* p87l06c7 for March 2005 */
replace yremesae=p87l06c3 if p87l06c3>=0 & (p87l06c7==. | p87l06c7==0) /* p87l06c3 for March 2005 */

****************
***yayudae******
**************** 

gen yayudae=p87l08c7/3 if p87l08c7>=0 /* p87l08c7 for March 2005 */
replace yayudae=p87l08c3 if p87l08c3>=0 & (p87l08c7==. | p87l08c7==0) /* p87l08c3 for March 2005 */

/* No estan las ayudas de particulares en especie:*/

/* No estan las herencias */

*Hay pension por divorcio: p5213c03 & p5213c02 for March 2005 */ 

****************
***ypdiv********
**************** 

gen ypdiv=p87l07c5/3 if p87l07c5>=0
replace ypdiv=p87l07c1 if p87l07c1>=0 & (p87l07c5==. | p87l07c5==0)

sum ypdiv if ypdiv>=0

***********************
*** remesasm_ci     ***
***********************

egen remesasm_ci=rsum(yremesa2 yremesad2)
replace remesasm_ci=. if yremesa2==. & yremesad2==. 
replace remesasm_ci=0 if p87l06c5==0 & p87l06c2==0 & p87l06c6==0 & p87l06c1==0 
label var remesasm_ci "Remesas Individuales (monetario)"

***********************
*** remesas_ci     ***
***********************

egen remesas_ci=rsum(yremesa2 yremesad2 yremesade yremesae)
replace remesas_ci=. if yremesa2==. & yremesad2==. & yremesade==. & yremesae==.
replace remesas_ci=0 if p87l06c5==0 & p87l06c2==0 & p87l06c6==0 & p87l06c1==0 & p87l06c8==0 & p87l06c4==0 & p87l06c7==0 & p87l06c3==0 
label var remesas_ci "Remesas Individuales (monetario + especies)"

***********************
*** ynlm_ci         ***
***********************

egen ynlm_ci=rsum(ypenju2 yjub2 ysubsi2 yalquile2 ybonos2 yremesa2 yremesad2 yayuda2 yayupar2 yotros2)
replace ynlm_ci=0 if p87l01c5==. & p87l01c1==. & p87l02c5==. & p87l02c1==. & p87l03c5==. & p87l03c1==. & p87l04c5==. & p87l04c1==. & p87l10c5==. & p87l10c1==. & p87l06c5==. & p87l06c2==. & p87l06c6==. & p87l06c2==. & p87l08c5==. & p87l08c1==. & p87l09c5==. & p87l09c1==. & p87l12c5==. & p87l12c1==.
replace ynlm_ci=. if ypenju2==. & ysubsi2==. & yalquile2==. & ybonos2==. & yremesa2==. & yayuda2==. & yayupar2==. & yotros2==. & yjub2==. & yremesad2==.
label var ynlm_ci "Ingreso No Laboral Monetario"

***********************
*** ynlm2_ci        ***
***********************

egen ynlm2_ci=rsum(ypenju2 yjub2 ysubsi2 yalquile2 ybonos2 yremesa2 yremesad2 yayuda2 yayupar2 yotros2 interes2 prestlab2)
replace ynlm2_ci=. if ypenju2==. & ysubsi2==. & yalquile2==. & ybonos2==. & yremesa2==. & yayuda2==. & yayupar2==. & yotros2==. & interes2==. & prestlab2==. & yjub2==. & yremesad2==.
replace ynlm2_ci=0 if p87l01c5==. & p87l01c1==. & p87l02c5==. & p87l02c1==. & p87l03c5==. & p87l03c1==. & p87l04c5==. & p87l04c1==. & p87l10c5==. & p87l10c1==. & p87l06c5==. & p87l06c2==. & p87l06c6==. & p87l06c2==. & p87l08c5==. & p87l08c1==. & p87l09c5==. & p87l09c1==. & p87l12c5==. & p87l12c1==. & p87l05c5==. & p87l05c1==. & p87l11c5==. & p87l11c1==.   
label var ynlm2_ci "Ingreso No Laboral Monetario 2"

***********************
*** ynlm4_ci        ***
***********************

egen ynlm4_ci=rsum(ypenju2 yjub2 ysubsi2 yalquile2 ybonos2 yremesa2 yremesad2 yayuda2 yayupar2 yotros2 interes2 prestlab2 ypdiv)
replace ynlm4_ci=. if ypenju2==. & ysubsi2==. & yalquile2==. & ybonos2==. & yremesa2==. & yayuda2==. & yayupar2==. & yotros2==. & interes2==. & prestlab2==. & yjub2==. & yremesad2==. & ypdiv
replace ynlm4_ci=0 if p87l01c5==. & p87l01c1==. & p87l02c5==. & p87l02c1==. & p87l03c5==. & p87l03c1==. & p87l04c5==. & p87l04c1==. & p87l10c5==. & p87l10c1==. & p87l06c5==. & p87l06c2==. & p87l06c6==. & p87l06c2==. & p87l08c5==. & p87l08c1==. & p87l09c5==. & p87l09c1==. & p87l12c5==. & p87l12c1==. & p87l05c5==. & p87l05c1==. & p87l11c5==. & p87l11c1==.  & p87l07c5==. & p87l07c1==. 
label var ynlm4_ci "Ingreso No Laboral Monetario 4"

***********************
*** ynlnm_ci        ***
***********************

egen ynlnm_ci=rsum(yremesade yremesae yayudae) 
replace ynlnm_ci=. if yremesade==. & yremesae==. & yayudae==. 
replace ynlnm_ci=0 if p87l06c8==. & p87l06c4==. & p87l06c7==. & p87l06c3==. & p87l08c7==. & p87l08c3==.
label var ynlnm_ci "Ingreso No Laboral No Monetario"

***********************
*** ynl_ci          ***
***********************

egen ynl_ci=rsum(ypenju2 yjub2 ysubsi2 yalquile2 ybonos2 yremesa2 yremesad2 yayuda2 yayupar2 yotros2 interes2 prestlab2 yremesade yremesae yayudae ypdiv)
replace ynl_ci=. if ypenju2==. & ysubsi2==. & yalquile2==. & ybonos2==. & yremesa2==. & yayuda2==. & yayupar2==. & yotros2==. & interes2==. & prestlab2==. & yjub2==. & yremesad2==. & yremesade==. & yremesae==. & yayudae==. & ypdiv==.
replace ynl_ci=0 if p87l01c5==. & p87l01c1==. & p87l02c5==. & p87l02c1==. & p87l03c5==. & p87l03c1==. & p87l04c5==. & p87l04c1==. & p87l10c5==. & p87l10c1==. & p87l06c5==. & p87l06c2==. & p87l06c6==. & p87l06c2==. & p87l08c5==. & p87l08c1==. & p87l09c5==. & p87l09c1==. & p87l12c5==. & p87l12c1==. & p87l05c5==. & p87l05c1==. & p87l11c5==. & p87l11c1==.  & p87l07c5==. & p87l07c1==. & p87l06c8==. & p87l06c4==. & p87l06c7==. & p87l06c3==. & p87l08c7==. & p87l08c3==.
label var ynl_ci "Ingreso No Laboral (Monetario + No Monetario)"


***********************
*** nrylmpri_ci     ***
*********************** 

gen nrylmpri_ci=0 
replace nrylmpri_ci=1 if p49==99999
replace nrylmpri_ci=1 if p58==999999
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

*** ocupacion principal ***

gen rama_ci=ramaop
replace rama_ci=. if ramaop==10 | ramaop==11 | emp_ci==0

*drop yalim2 yropa2 yhabita2 ytrans2 yotro2 yprodu2 ypenju2 ysubsi2 yalquile2 ybonos2 yayuda2 yayupar2 yotros2 interes2 prestlab2 yalim3 yropa3 yhabita3 ytrans3 yotro3 yprodu3 yjub2 yremesade yremesae yayudae ypdiv autoconsumop_ci autoconsumos_ci

/* ACTIVAR
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

*/
qui destring $var, replace
 

* Activar solo si es necesario
*keep *_ci  *_c  idh_ch 
qui compress



saveold "`base_out'", replace


log close


