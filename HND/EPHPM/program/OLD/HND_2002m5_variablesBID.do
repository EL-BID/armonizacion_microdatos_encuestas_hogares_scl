

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
local ANO "2002"
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
Round: m9
Autores: Revised March, 2008 (by tede) 
Última versión: María Laura Oliveri (MLO) - Email: mloliveri@iadb.org, lauraoliveri@yahoo.com
Fecha última modificación: 9 de Septiembre de 2013

							SCL/LMK - IADB
****************************************************************************/
/***************************************************************************
Detalle de procesamientos o modificaciones anteriores:
*******************************************************************************************************
*****                                  HONDURAS 2002 - MAYO                                           *
*****                EPHPM 2002 (Encuesta Permanente de Hogares de Propositos Multiples)              *
*****                                  104.907 personas                                               *
*****                                   21.189 hogares                                                *
*******************************************************************************************************
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
* Region_c *
************
*Inclusión Mayra Sáenz - Julio 2013

gen region_c=  .
label var region_c "División política"

gen edad_ci=edad

compress

gen pais_c="HND"

gen anio_c=2002

gen mes_c=5

label define mes_c 3 "Marzo" 5 "Mayo" 9 "Septiembre"

label value mes_c mes_c

gen zona_c=1 if domi==1 | domi==2 | domi==3 | domi==4


replace zona_c=0 if domi==5
label define zona_c 0 "Rural" 1 "Urbana" 
label value zona_c zona_c

egen idh_ch=group(hogar depto domi estrato numhog)

gen factor_ch=factor

gen factor_ci=factor_ch

 
 /* Queda un hogar sin jefe: idh=2241 */
gen miembros_ci=1 if rela_j>=1 & rela_j<=7

replace miembros_ci=0 if rela_j==8
gen uno=1 if miembros_ci==1

egen nmiembros_ch=sum(uno), by(idh_ch)

replace nmiembros_ch=. if miembros_ci!=1

label var nmiembros_ch "Numero de miembros del Hogar"

drop uno

gen categopri_ci=1 if p34==7
replace categopri_ci=2 if p34==4 | p34==5 | p34==6

replace categopri_ci=3 if p34==1 | p34==2 | p34==3

replace categopri_ci=4 if p34==8 | p34==9

label var categopri_ci "Categoria ocupacional actividad principal"

label define categopri_ci 1 "Patron" 2 "Cuenta Propia" 3 "Empleado" 4 "Trabajador no remunerado"

label value categopri_ci categopri_ci

gen categosec_ci=1 if p40==7

replace categosec_ci=2 if p40==4 | p40==5 | p40==6

replace categosec_ci=3 if p40==1 | p40==2 | p40==3

replace categosec_ci=4 if p40==8

label var categosec_ci "Categoria ocupacional actividad secundaria"

label define categosec_ci 1 "Patron" 2 "Cuenta Propia" 3 "Empleado" 4 "Trabajador no remunerado"
label value categosec_ci categosec_ci
 
/*gen emp_ci=.

replace emp_ci=1 if p13==1 | p14==1

replace emp_ci=0 if p13==2 & p14==2

label var emp_ci "Empleado en la semana de referencia"*/

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


gen ylmpri_ci=.

replace ylmpri_ci=p36 if p36<99999 & edad_ci>4 

replace ylmpri_ci=p38 if p38<999999 & edad_ci>4 & p38>=0

replace ylmpri_ci=0 if p36==0 & p38==0 & edad_ci>4 & (p13==1 | p14==1)

replace ylmpri_ci=0 if (p34==8 | p34==9) & edad_ci>4 & (p13==1 | p14==1)

label var ylmpri_ci "Ingreso Laboral Monetario de la Actividad Principal"

gen ylmsec_ci=.

replace ylmsec_ci=p42 if p42<99999 & edad_ci>4 

replace ylmsec_ci=p44 if p44<99999 & edad_ci>4 & p44>0

replace ylmsec_ci=0 if p42==0 & p44==0 & edad_ci>4 & p17a==1

replace ylmsec_ci=0 if (p40==8 | p40==9) & edad_ci>4 & p17a==1

label var ylmsec_ci "Ingreso Laboral Monetario de la Actividad Secundaria"

egen ylm_ci=rsum(ylmpri_ci ylmsec_ci)

replace ylm_ci=. if ylmpri_ci==. & ylmsec_ci==.

label var ylm_ci "Ingreso Laboral Monetario Total"

gen yalim2=p37a if p37a<99999 & p37a>=0

gen yropa2=p37b if p37b<99999 & p37b>=0

gen yhabita2=p37c if p37c<99999 & p37c>=0

gen ytrans2=p37d if p37d<99999 & p37d>=0

gen yotro2=p37e if p37e<99999 & p37e>=0

gen yprodu2=p39 if p39<99999 & p39>=0

egen ylnmpri_ci=rsum(yalim2 yropa2 yhabita2 ytrans2 yotro2 yprodu2)

replace ylnmpri_ci=. if yalim2==. & yropa2==. & yhabita2==. & ytrans2==. & yotro2==. & yprodu2==.

replace ylnmpri_ci=0 if ((categopri==3 & p37a==. & p37b==. & p37c==. & p37d==. & p37e==.) | (p39==. & (categopri==1 | categopri==2))) & edad_ci>4 & (p13==1 | p14==1)

label var ylnmpri_ci "Ingreso Laboral No Monetario de la Actividad Principal"

gen yalim3=p43a if p43a<99999 & p43a>=0

gen yropa3=p43b if p43b<99999 & p43b>=0

gen yhabita3=p43c if p43c<99999 & p43c>=0

gen ytrans3=p43d if p43d<99999 & p43d>=0

gen yotro3=p43e if p43e<99999 & p43e>=0

gen yprodu3=p45 if p45<99999 & p45>=0

 
egen ylnmsec_ci=rsum(yalim3 yropa3 yhabita3 ytrans3 yotro3 yprodu3)

replace ylnmsec_ci=. if yalim3==. & yropa3==. & yhabita3==. & ytrans3==. & yotro3==. & yprodu3==.

replace ylnmsec_ci=0 if ((categosec==3 & p43a==. & p43b==. & p43c==. & p43d==. & p43e==.) | (p45==. & (categosec==1 | categosec==2))) & edad_ci>4 & p17a==1


label var ylnmsec_ci "Ingreso Laboral No Monetario de la Actividad Secundaria" 


egen ylnm_ci=rsum(ylnmpri_ci ylnmsec_ci)

replace ylnm_ci=. if ylnmpri_ci==. & ylnmsec_ci==.

label var ylnm_ci "Ingreso Laboral No Monetario Total"

gen autoconsumop_ci=yprodu2 

replace autoconsumop_ci=0 if p39==. & (p13==1 | p14==1) & edad_ci>4 & (categopri==1 | categopri==2) & (p13==1 | p14==1)

label var autoconsumop_ci "Autoconsumo Individual Actividad Principal(Trabajadores Independientes)"

 
gen autoconsumos_ci=yprodu2 

replace autoconsumos_ci=0 if p45==. & p17a==1 & edad_ci>4 & (categosec==1 | categosec==2) & p17a==1

label var autoconsumos_ci "Autoconsumo Individual Actividad Secundaria(Trabajadores Independientes)"

egen autoconsumo_ci=rsum(autoconsumop_ci autoconsumos_ci)

replace autoconsumo_ci=. if autoconsumop_ci==. & autoconsumos_ci==.

label var autoconsumo_ci "Autoconsumo Individual (Trabajadores Independientes)"

gen ypenju2=v46au3m/3 if v46au3m>=0 & v46au3m<999999

replace ypenju2=v46aum if v46aum>=0 & v46aum<999999 & (v46au3m==. | v46au3m==0)

gen yalquile2=v46bu3m/3 if v46bu3m>=0 & v46bu3m<999999

replace yalquile2=v46bum if v46bum>=0 & v46bum<999999 & (v46bu3m==. | v46bu3m==0)

gen ysubsi2=v46cu3m/3 if v46cu3m>=0 & v46cu3m<999999

replace ysubsi2=v46cum if v46cum>=0 & v46cum<999999 & (v46cu3m==. | v46cu3m==0)

gen ybonos2=v46hu3m/3 if v46hu3m>=0 & v46hu3m<999999

replace ybonos2=v46hum if v46hum>=0 & v46hum<999999 & (v46hu3m==. | v46hu3m==0)

gen yremesa2=v46eu3m/3 if v46eu3m>=0 & v46eu3m<999999

replace yremesa2=v46eum if v46eum>=0 & v46eum<999999 & (v46eu3m==. | v46eu3m==0)

gen yayuda2=v46fu3m/3 if v46fu3m>=0 & v46fu3m<999999

replace yayuda2=v46fum if v46fum>=0 & v46fum<999999 & (v46fu3m==. | v46fu3m==0)

gen yayupar2=v46gu3m/3 if v46gu3m>=0 & v46gu3m<999999

replace yayupar2=v46gum if v46gum>=0 & v46gum<999999 & (v46gu3m==. | v46gu3m==0)

gen yotros2=v46ku3m/3 if v46ku3m>=0 & v46ku3m<999999

replace yotros2=v46kum if v46kum>=0 & v46kum<999999 & (v46ku3m==. | v46ku3m==0)

gen interes2=v46du3m/3 if v46du3m>=0 & v46du3m<999999

replace interes2=v46dum if v46dum>=0 & v46dum<999999 & (v46du3m==. | v46du3m==0)

gen prestlab2=v46iu3m/3 if v46iu3m>=0 & v46iu3m<999999

replace prestlab2=v46ium if v46ium>=0 & v46ium<999999 & (v46iu3m==. | v46iu3m==0)

gen herencia2=v46ju3m/3 if v46ju3m>=0 & v46ju3m<999999

replace herencia2=v46jum if v46jum>=0 & v46jum<999999 & (v46ju3m==. | v46ju3m==0)
gen remesas_ci=yremesa2

replace remesas_ci=0 if v46eu3m==0 & v46eum==0
label var remesas_ci "Remesas Individuales"

egen ynlm_ci=rsum(ypenju2 ysubsi2 yalquile2 ybonos2 yremesa2 yayuda2 yayupar2 yotros2)

replace ynlm_ci=0 if v46au3m==. & v46bu3m==. & v46cu3m==. & v46eu3m==. & v46fu3m==. & v46gu3m==. & v46hu3m==. & v46ku3m==. & v46aum==. & v46bum==. & v46cum==. & v46eum==. & v46fum==. & v46gum==. & v46hum==. & v46kum==.

replace ynlm_ci=. if ypenju2==. & ysubsi2==. & yalquile2==. & ybonos2==. & yremesa2==. & yayuda2==. & yayupar2==. & yotros2==. 


label var ynlm_ci "Ingreso No Laboral Monetario"

 
egen ynlm2_ci=rsum(ypenju2 ysubsi2 yalquile2 ybonos2 yremesa2 yayuda2 yayupar2 yotros2 interes2 prestlab2)

replace ynlm2_ci=. if ypenju2==. & ysubsi2==. & yalquile2==. & ybonos2==. & yremesa2==. & yayuda2==. & yayupar2==. & yotros2==. & interes2==. & prestlab2==. 


replace ynlm2_ci=0 if v46au3m==. & v46bu3m==. & v46cu3m==. & v46eu3m==. & v46fu3m==. & v46gu3m==. & v46hu3m==. & v46ku3m==. & v46du3m==. & v46iu3m==. & v46aum==. & v46bum==. & v46cum==. & v46eum==. & v46fum==. & v46gum==. & v46hum==. & v46kum==. & v46dum==. & v46ium==. 

label var ynlm2_ci "Ingreso No Laboral Monetario 2"


egen ynlm3_ci=rsum(ypenju2 ysubsi2 yalquile2 ybonos2 yremesa2 yayuda2 yayupar2 yotros2 interes2 prestlab2 herencia2)

replace ynlm3_ci=. if ypenju2==. & ysubsi2==. & yalquile2==. & ybonos2==. & yremesa2==. & yayuda2==. &  yayupar2==. & yotros2==. & interes2==. & prestlab2==. & herencia2==.


replace ynlm3_ci=0 if v46au3m==. & v46bu3m==. & v46cu3m==. & v46eu3m==. & v46fu3m==. & v46gu3m==. & v46hu3m==. & v46ku3m==. & v46du3m==. & v46iu3m==. & v46ju3m==. & v46aum==. & v46bum==. & v46cum==. & v46eum==. & v46fum==. & v46gum==. & v46hum==. & v46kum==. & v46dum==. & v46ium==. & v46jum==.

label var ynlm3_ci "Ingreso No Laboral Monetario 3"


gen nrylmpri_ci=0 

replace nrylmpri_ci=1 if p36==99999

replace nrylmpri_ci=1 if p38==999999

label var nrylmpri_ci "Identificador de No Respuesta del Ingreso Monetario de la Actividad Principal"


egen ylmnr_ci=rsum(ylmpri_ci ylmsec_ci) if nrylmpri_ci==0

replace ylmnr_ci=. if ylmpri_ci==. 

label var ylmnr_ci "Ingreso Laboral Monetario Total, considera 'missing' la No Respuesta "

 
egen yl_ci=rsum(ylmnr_ci ylnm_ci)

replace yl_ci=. if ylmnr_ci==. & ylnm_ci==.

label var yl_ci "Ingreso Laboral Individual (Monetario + No Monetario)"


egen nrylmpri_ch=sum(nrylmpri_ci) if miembros_ci==1, by(idh_ch)

replace nrylmpri_ch=1 if nrylmpri_ch>1 & nrylmpri_ch<.

label var nrylmpri_ch "Identificador de Hogares en donde alguno de los miembros No Responde el Ingreso Monetario de la Actividad Principal"
 
egen ylm_ch=sum(ylm_ci) if miembros_ci==1, by(idh_ch)

label var ylm_ch "Ingreso Laboral Monetario del Hogar"

egen ylmnr_ch=sum(ylm_ci) if miembros_ci==1 & nrylmpri_ch==0, by(idh_ch)

label var ylmnr_ch "Ingreso Laboral Monetario del Hogar, considera 'missing' la No Respuesta"

 
egen ylnm_ch=sum(ylnm_ci) if miembros_ci==1, by(idh_ch)

label var ylnm_ch "Ingreso Laboral No Monetario del Hogar"

egen ynlm_ch=sum(ynlm_ci) if miembros_ci==1, by(idh_ch)

label var ynlm_ch "Ingreso No Laboral Monetario del Hogar"

egen ynlm2_ch=sum(ynlm2_ci) if miembros_ci==1, by(idh_ch)

label var ynlm2_ch "Ingreso No Laboral Monetario 2 del Hogar"


egen ynlm3_ch=sum(ynlm3_ci) if miembros_ci==1, by(idh_ch)

label var ynlm3_ch "Ingreso No Laboral Monetario 3 del Hogar"

 
egen autoconsumo_ch=sum(autoconsumo_ci) if miembros_ci==1, by(idh_ch)

label var autoconsumo_ch "Autoconsumo del Hogar"

egen remesas_ch=sum(remesas_ci) if miembros_ci==1, by(idh_ch)

label var remesas_ch "Remesas del Hogar"

gen rama_ci=rama
replace rama_ci=. if rama==10 | emp_ci==0

*drop yalim2 yropa2 yhabita2 ytrans2 yotro2 yprodu2 ypenju2 ysubsi2 yalquile2 ybonos2 yremesa2 yayuda2 yayupar2 yotros2 interes2 herencia2 prestlab2 yalim3 yropa3 yhabita3 ytrans3 yotro3 yprodu3 autoconsumop> _ci autoconsumos_ci
*YL: genero las siguientes variables con missing para correr sociometro. Estas variables deben ser creadas correctamente.
gen idp_c=.
gen relacion_ci=.
gen sexo_ci=sexo
gen civil_ci=.
gen jefe_ci=.
gen nconyuges_ch=.
gen  nhijos_ch=.
gen  notropari_ch=.
gen notronopari_ch=.
gen nempdom_ch=.
gen clasehog_ch=.
gen nmayor21_ch=.
gen nmenor21_ch =.
gen nmayor65_ch=.
gen  nmenor6_ch=.
gen nmenor1_ch=.
gen ocupa_ci=.
gen horaspri_ci=.
gen horastot_ci =.
gen ylmotros_ci =.
gen ylnmotros_ci=.
gen tcylmpri_ci=.
gen tcylmpri_ch =.
gen rentaimp_ch=.
gen autocons_ci=.
gen autocons_ch=.
gen tiempoparc_ci=.
gen firmapeq_ci=.
gen raza_ci=.
gen instcot_ci=.
gen  ynlnm_ci=.
gen ynlnm_ch=.
gen ylmhopri_ci=.
gen ylmho_ci=.
gen durades_ci=.
gen antiguedad_ci =.
gen  desalent_ci =.
gen subemp_ci=.
gen nempleos_ci=.
gen spublico_ci=.
gen aedu_ci=.
gen eduno_ci=.
/* activar
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


