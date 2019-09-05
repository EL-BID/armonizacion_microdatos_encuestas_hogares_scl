

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

local PAIS HND
local ENCUESTA EPHPM
local ANO "2006"
local ronda m5

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
****************************************************************************/
clear all
set more off
use "`base_in'", clear

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
* Region_c *
************
*Inclusión Mayra Sáenz - Julio 2013

gen region_c=  .
label var region_c "División política"

************
****pais****
************

gen pais_c="HND"

**********
***anio***
**********

gen anio_c=2006

*********
***mes***
*********

gen mes_c=5
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

gen categosec_ci=1 if p73==7 /* p73 for March 2005 */
replace categosec_ci=2 if p73==4 | p73==5 | p73==6
replace categosec_ci=3 if p73==1 | p73==2 | p73==3
replace categosec_ci=4 if p73==12 | p73==13
label var categosec_ci "Categoria ocupacional actividad secundaria"
label define categosec_ci 1 "Patron" 2 "Cuenta Propia" 3 "Empleado" 4 "Trabajador no remunerado"
label value categosec_ci categosec_ci

************
***emp_ci***
************

gen emp_ci=.
replace emp_ci=1 if p16==1 | p17==1 /* p16 & p17 for 2005 */
replace emp_ci=0 if p16==2 & p17==2 
label var emp_ci "Empleado en la semana de referencia"

****************
***ylmpri_ci ***
****************

gen ylmpri_ci=.
replace ylmpri_ci=p55 if p55<99999 & edad>4 & p53==1 /* p55 & p53 for May 2006 */
replace ylmpri_ci=p55*p54 if p55<99999 & edad>4 & (p53==2 | p53==3 | p53==4) /* p54 for May 2006 */
replace ylmpri_ci=p64 if p64<999999 & edad>4 & p64>=0 /* p64 for May 2006 */
replace ylmpri_ci=0 if p55==0 & p64==0 & edad>4 & (p16==1 | p17==1)
replace ylmpri_ci=0 if (p43==12 | p43==13) & edad>4 & (p16==1 | p17==1)
label var ylmpri_ci "Ingreso Laboral Monetario de la Actividad Principal"

****************
***ylmsec_ci ***
****************

gen ylmsec_ci=.
replace ylmsec_ci=p85 if p85<99999 & edad>4 & p83==1 /* p85 & p83 for May 2006 */
replace ylmsec_ci=p85*p84 if p85<99999 & edad>4 & (p83==2 | p83==3 | p83==4)
replace ylmsec_ci=p94 if p94<99999 & edad>4 & p94>=0 /* p94 for May 2006 */ 

replace ylmsec_ci=0 if p85==0 & p94==0 & edad>4 & p61==1 /* p61 for May 2006 */
replace ylmsec_ci=0 if (p73==12 | p73==13) & edad>4 & p67==1 /* p73 for May 2006 */
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

gen yalim2=p56l1 if p56l1<99999 & p56l1>=0 /* p56 for May 2006 */
gen yropa2=p56l2 if p56l2<99999 & p56l2>=0
gen yhabita2=p56l3 if p56l3<99999 & p56l3>=0
gen ytrans2=p56l4 if p56l4<99999 & p56l4>=0
gen yotro2=p56l5 if p56l5<99999 & p56l5>=0
gen yprodu2=p64 if p64<99999 & p64>=0 /* p64 for May 2006 */

egen ylnmpri_ci=rsum(yalim2 yropa2 yhabita2 ytrans2 yotro2 yprodu2)
replace ylnmpri_ci=. if yalim2==. & yropa2==. & yhabita2==. & ytrans2==. & yotro2==. & yprodu2==.
replace ylnmpri_ci=0 if ((p56l1==. & p56l2==. & p56l3==. & p56l4==. & p56l5==. & categopri==3) | (p64==. & (categopri==1 | categopri==2))) & edad>4 & (p16==1 | p17==1)
label var ylnmpri_ci "Ingreso Laboral No Monetario de la Actividad Principal"

*****************
***ylnmsec_ci ***
*****************

gen yalim3=p86l1 if p86l1<99999 & p86l1>=0 /* p86 for May 2006 */
gen yropa3=p86l2 if p86l2<99999 & p86l2>=0
gen yhabita3=p86l3 if p86l3<99999 & p86l3>=0
gen ytrans3=p86l4 if p86l4<99999 & p86l4>=0
gen yotro3=p86l5 if p86l5<99999 & p86l5>=0
gen yprodu3=p95 if p95<99999 & p95>=0 /* p95 for May 2006 */

egen ylnmsec_ci=rsum(yalim3 yropa3 yhabita3 ytrans3 yotro3 yprodu3)
replace ylnmsec_ci=. if yalim3==. & yropa3==. & yhabita3==. & ytrans3==. & yotro3==. & yprodu3==.
replace ylnmsec_ci=0 if ((p86l1==. & p86l2==. & p86l3==. & p86l4==. & p86l5==. & categosec==3) | (p95==. & (categosec==1 | categosec==2))) & edad>4 & p67==1
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
replace autoconsumop_ci=0 if p64==. & edad>4 & (categopri==1 | categopri==2) & (p16==1 | p17==1) /* p64 for May 2006 */
label var autoconsumop_ci "Autoconsumo Individual Actividad Principal(Trabajadores Independientes)"

***********************
*** autoconsumos_ci ***
***********************

gen autoconsumos_ci=yprodu2 
replace autoconsumos_ci=0 if p95==. & edad>4 & (categosec==1 | categosec==2) & p67==1 
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

gen ypenju2=p00l01c5/3 if p00l01c5>=0 /* p00l01c5 for May 2006 */ 
replace ypenju2=p00l01c1 if p00l01c1>=0 & (p00l01c5==. | p00l01c5==0) /* p00l01c1 May 2006 */

***********
***yjub2***
***********

gen yjub2=p00l02c5/3 if  p00l02c5>=0 /*  p00l02c5 May 2006 */
replace yjub2=p00l02c1 if p00l02c1>=0 & (p00l02c5==. | p00l02c5==0) /* p00l02c1 May 2006 */

***************
***yalquile2***
***************

gen yalquile2=p00l03c5/3 if  p00l03c5>=0 /*  p00l03c5 May 2006 */
replace yalquile2= p00l03c1 if  p00l03c1>=0 & (p00l03c5==. | p00l03c5==0) /*  p00l03c1 May 2006 */

***************
***ysubsi2*****
***************

gen ysubsi2=p00l04c5/3 if p00l04c5>=0 /* p00l04c5 May 2006 */
replace ysubsi2= p00l04c1 if  p00l04c1>=0 & (p00l04c5==. | p00l04c5==0) /*  p00l04c1 May 2006 */

***************
***ybonos2*****
***************

gen ybonos2=p00l10c5/3 if p00l10c5>=0 /*  p00l10c5 May 2006 */
replace ybonos2=p00l10c1 if  p00l10c1>=0 & (p00l10c5==. | p00l10c5==0) /* p00l10c1 May 2006 */

****************
***yremesa2*****
****************

gen yremesa2=p00l06c5/3 if p00l06c5>=0 /* p00l06c5 May 2006 */
replace yremesa2=p00l06c2 if p00l06c2>=0 & (p00l06c5==. | p00l06c5==0) /* p00l06c2 May 2006 */ 

/* Tipo de cambio lempiras por dolares (Banco Central de Honduras) a 
                                                                     marzo 2006: 19.03 
								     abril 2006: 19.03
								     mayo 2006:  19.03
								     promedio 3 meses: 19.03 */

****************
***yremesad2****
****************

gen yremesad2=(p00l06c6/3)*19.03 if p00l06c6>=0 /* p00l06c6 for May 2006 */
replace yremesad2=p00l06c2*19.03 if p00l06c2>=0 & (p00l06c6==. | p00l06c6==0) /* p00l06c2 for May 2006 */

****************
***yayuda2******
****************

gen yayuda2=p00l08c5/3 if p00l08c5>=0 /* p00l08c5 for May 2006 */
replace yayuda2=p00l08c1 if p00l08c1>=0 & (p00l08c5==. | p00l08c5==0) /* p00l08c1 for May 2006*/ 

****************
***yayupar2*****
****************

gen yayupar2=p00l09c5/3 if p00l09c5>=0 /* p00l09c5 for May 2006 */ 
replace yayupar2=p00l09c1 if p00l09c1>=0 & (p00l09c5==. | p00l09c5==0) /* p00l09c1 for May 2006 */

****************
***yotros2******
****************

gen yotros2=p00l12c5/3 if p00l12c5>=0 /*  p00l12c5 for May 2006 */ 
replace yotros2= p00l12c1 if p00l12c1>=0 & (p00l12c5==. | p00l12c5==0) /*  p00l12c1 for May 2006 */

****************
***interes2*****
****************

gen interes2=p00l05c5/3 if p00l05c5>=0 /* p00l05c5 for May 2006 */
replace interes2=p00l05c1 if p00l05c1>=0 & (p00l05c5==. | p00l05c5==0) /* p00l05c1 for May 2006 */

****************
***prestlab2****
**************** 

gen prestlab2=p00l11c5/3 if p00l11c5>=0 /* p00l11c5 for May 2006 */ 
replace prestlab2=p00l11c1 if p00l11c1>=0 & (p00l11c5==. | p00l11c5==0) /* p00l11c1 for May 2006 */

****************
***yremerasde***
**************** 

gen yremesade=(p00l06c8/3)*19.03 if p00l06c8>=0 /* p00l06c8 for May 2006 */ 
replace yremesade=p00l06c4*19.03 if p00l06c4>=0 & (p00l06c8==. | p00l06c8==0) /* p00l06c4 for May 2006 */

****************
***yremerase****
**************** 

gen yremesae=p00l06c7/3 if p00l06c7>=0 /* p00l06c7 for May 2006 */
replace yremesae=p00l06c3 if p00l06c3>=0 & (p00l06c7==. | p00l06c7==0) /* p00l06c3 for May 2006 */

****************
***yayudae******
**************** 

gen yayudae=p00l08c7/3 if p00l08c7>=0 /* p00l08c7 for May 2006 */
replace yayudae=p00l08c3 if p00l08c3>=0 & (p00l08c7==. | p00l08c7==0) /* p00l08c3 for May 2006 */

/* No estan las ayudas de particulares en especie:*/

/* No estan las herencias */

*Hay pension por divorcio: p5213c03 & p5213c02 for May 2006 */ 

****************
***ypdiv********
**************** 

gen ypdiv=p00l07c5/3 if p00l07c5>=0
replace ypdiv=p00l07c1 if p00l07c1>=0 & (p00l07c5==. | p00l07c5==0)

sum ypdiv if ypdiv>=0

***********************
*** remesasm_ci     ***
***********************

egen remesasm_ci=rsum(yremesa2 yremesad2)
replace remesasm_ci=. if yremesa2==. & yremesad2==. 
replace remesasm_ci=0 if p00l06c5==0 & p00l06c2==0 & p00l06c6==0 & p00l06c1==0 
label var remesasm_ci "Remesas Individuales (monetario)"

***********************
*** remesas_ci     ***
***********************

egen remesas_ci=rsum(yremesa2 yremesad2 yremesade yremesae)
replace remesas_ci=. if yremesa2==. & yremesad2==. & yremesade==. & yremesae==.
replace remesas_ci=0 if p00l06c5==0 & p00l06c2==0 & p00l06c6==0 & p00l06c1==0 & p00l06c8==0 & p00l06c4==0 & p00l06c7==0 & p00l06c3==0 
label var remesas_ci "Remesas Individuales (monetario + especies)"

***********************
*** ynlm_ci         ***
***********************

egen ynlm_ci=rsum(ypenju2 yjub2 ysubsi2 yalquile2 ybonos2 yremesa2 yremesad2 yayuda2 yayupar2 yotros2)
replace ynlm_ci=0 if p00l01c5==. & p00l01c1==. & p00l02c5==. & p00l02c1==. & p00l03c5==. & p00l03c1==. & p00l04c5==. & p00l04c1==. & p00l10c5==. & p00l10c1==. & p00l06c5==. & p00l06c2==. & p00l06c6==. & p00l06c2==. & p00l08c5==. & p00l08c1==. & p00l09c5==. & p00l09c1==. & p00l12c5==. & p00l12c1==.
replace ynlm_ci=. if ypenju2==. & ysubsi2==. & yalquile2==. & ybonos2==. & yremesa2==. & yayuda2==. & yayupar2==. & yotros2==. & yjub2==. & yremesad2==.
label var ynlm_ci "Ingreso No Laboral Monetario"

***********************
*** ynlm2_ci        ***
***********************

egen ynlm2_ci=rsum(ypenju2 yjub2 ysubsi2 yalquile2 ybonos2 yremesa2 yremesad2 yayuda2 yayupar2 yotros2 interes2 prestlab2)
replace ynlm2_ci=. if ypenju2==. & ysubsi2==. & yalquile2==. & ybonos2==. & yremesa2==. & yayuda2==. & yayupar2==. & yotros2==. & interes2==. & prestlab2==. & yjub2==. & yremesad2==.
replace ynlm2_ci=0 if p00l01c5==. & p00l01c1==. & p00l02c5==. & p00l02c1==. & p00l03c5==. & p00l03c1==. & p00l04c5==. & p00l04c1==. & p00l10c5==. & p00l10c1==. & p00l06c5==. & p00l06c2==. & p00l06c6==. & p00l06c2==. & p00l08c5==. & p00l08c1==. & p00l09c5==. & p00l09c1==. & p00l12c5==. & p00l12c1==. & p00l05c5==. & p00l05c1==. & p00l11c5==. & p00l11c1==.   
label var ynlm2_ci "Ingreso No Laboral Monetario 2"

***********************
*** ynlm4_ci        ***
***********************

egen ynlm4_ci=rsum(ypenju2 yjub2 ysubsi2 yalquile2 ybonos2 yremesa2 yremesad2 yayuda2 yayupar2 yotros2 interes2 prestlab2 ypdiv)
replace ynlm4_ci=. if ypenju2==. & ysubsi2==. & yalquile2==. & ybonos2==. & yremesa2==. & yayuda2==. & yayupar2==. & yotros2==. & interes2==. & prestlab2==. & yjub2==. & yremesad2==. & ypdiv
replace ynlm4_ci=0 if p00l01c5==. & p00l01c1==. & p00l02c5==. & p00l02c1==. & p00l03c5==. & p00l03c1==. & p00l04c5==. & p00l04c1==. & p00l10c5==. & p00l10c1==. & p00l06c5==. & p00l06c2==. & p00l06c6==. & p00l06c2==. & p00l08c5==. & p00l08c1==. & p00l09c5==. & p00l09c1==. & p00l12c5==. & p00l12c1==. & p00l05c5==. & p00l05c1==. & p00l11c5==. & p00l11c1==.  & p00l07c5==. & p00l07c1==. 
label var ynlm4_ci "Ingreso No Laboral Monetario 4"

***********************
*** ynlnm_ci        ***
***********************

egen ynlnm_ci=rsum(yremesade yremesae yayudae) 
replace ynlnm_ci=. if yremesade==. & yremesae==. & yayudae==. 
replace ynlnm_ci=0 if p00l06c8==. & p00l06c4==. & p00l06c7==. & p00l06c3==. & p00l08c7==. & p00l08c3==.
label var ynlnm_ci "Ingreso No Laboral No Monetario"

***********************
*** ynl_ci          ***
***********************

egen ynl_ci=rsum(ypenju2 yjub2 ysubsi2 yalquile2 ybonos2 yremesa2 yremesad2 yayuda2 yayupar2 yotros2 interes2 prestlab2 yremesade yremesae yayudae ypdiv)
replace ynl_ci=. if ypenju2==. & ysubsi2==. & yalquile2==. & ybonos2==. & yremesa2==. & yayuda2==. & yayupar2==. & yotros2==. & interes2==. & prestlab2==. & yjub2==. & yremesad2==. & yremesade==. & yremesae==. & yayudae==. & ypdiv==.
replace ynl_ci=0 if p00l01c5==. & p00l01c1==. & p00l02c5==. & p00l02c1==. & p00l03c5==. & p00l03c1==. & p00l04c5==. & p00l04c1==. & p00l10c5==. & p00l10c1==. & p00l06c5==. & p00l06c2==. & p00l06c6==. & p00l06c2==. & p00l08c5==. & p00l08c1==. & p00l09c5==. & p00l09c1==. & p00l12c5==. & p00l12c1==. & p00l05c5==. & p00l05c1==. & p00l11c5==. & p00l11c1==.  & p00l07c5==. & p00l07c1==. & p00l06c8==. & p00l06c4==. & p00l06c7==. & p00l06c3==. & p00l08c7==. & p00l08c3==.
label var ynl_ci "Ingreso No Laboral (Monetario + No Monetario)"


***********************
*** nrylmpri_ci     ***
*********************** 

gen nrylmpri_ci=0 
replace nrylmpri_ci=1 if p55==99999
replace nrylmpri_ci=1 if p64==999999
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

*YL: genero las siguientes variables con missing para correr sociometro. Estas variables deben ser creadas correctamente.
gen antiguedad_ci=.
gen ocupa_ci=.
gen ylmotros_ci=.
gen ylnmotros_ci=.
gen tcylmpri_ci=.
gen tcylmpri_ch=.
gen rentaimp_ch=.
gen autocons_ci=.
gen autocons_ch=.
gen desalent_ci=.
gen tiempoparc_ci=.
gen spublico_ci=.
gen raza_ci=.
gen instcot_ci=.
gen idp_c=.
gen relacion_ci=.
gen sexo_ci=sexo
gen edad_ci=edad
gen civil_ci=.
gen jefe_ci=.
gen nconyuges_ch=.
gen nhijos_ch=.
gen  notropari_ch=.
gen notronopari_ch=.
gen nempdom_ch =.
gen clasehog_ch=.
gen nmayor21_ch=.
gen nmenor21_ch =.
gen nmayor65_ch =.
/*
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
