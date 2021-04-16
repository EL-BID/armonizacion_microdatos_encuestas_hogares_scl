
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
local ANO "1989"
local ronda m9

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

 Modificación 4 de Octubre de 2013: Mayra Sáenz
Email: mayras@iadb.org, saenzmayra.a@gmail.com


							SCL/LMK - IADB
****************************************************************************/
/***************************************************************************
Detalle de procesamientos o modificaciones anteriores:
 opened on:  16 Aug 2006, 12:38:18
*******************************************************************************************************
*****                                       HONDURAS 1989                                              
*****                EPHPM 1989 (Encuesta Permanente de Hogares de Propositos Multiples)               
*****                                      46.672 personas                                             
*****                                        8.727 hogares                                             
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

gen region_c=  depto
label define region_c 1 "Atlántida" 2 "Colón" 3 "Comayagua" ///
4 "Copán" 5 "Cortés" 6 "Choluteca" 7 "El Paraíso" 8 "Francisco de Morazán" ///
10 "Intibuca" 12 "La Paz" 13 "Lempira" 14 "Ocotepeque" 15 "Olancho" 16 "Santa Bárbara" ///
17 "Valle" 18 "Yoro"
label var region_c "División política"


***************
***factor_ch***
***************
gen factor_ch=factorex


***************
****idh_ch*****
*************** 
egen idh_ch=group(region depto dominio estrato replica segmento vivienda hogar nhogares)

*************
****idp_ci****
**************
gen idp_ci=

                
**********
***zona***
**********
gen zona_c=1 if dominio==1 | dominio==2 | dominio==3

replace  zona_c=0 if dominio==4

label define zona_c 0 "Rural" 1 "Urbana" 

label value zona_c zona_c


**********
***raza***
**********
gen raza_ci= .
label define raza_ci 1 "Indígena" 2 "Afro-descendiente" 3 "Otros"
label value raza_ci raza_ci 
label value raza_ci raza_ci
label var raza_ci "Raza o etnia del individuo" 

************
****pais****
************
gen pais_c="HND"

**********
***anio***
**********
gen anio_c=1989

*********
***mes***
*********
gen mes_c=9
label define mes_c 3 "Marzo" 5 "Mayo" 9 "Septiembre"

label value mes_c mes_c

*****************
***relacion_ci***
*****************

gen relacion_ci=.
replace relacion_ci=1 if parentco==1
replace relacion_ci=2 if parentco==2
replace relacion_ci=3 if parentco==3 
replace relacion_ci=4 if parentco==4 | parentco==5 
replace relacion_ci=5 if parentco==6
replace relacion_ci=6 if parentco==7
label var relacion_ci "Relacion con el Jefe de Hogar"
label define relacion 1 "Jefe de Hogar" 2 "Conyuge" 3 "Hijos" 4 "Otros Parientes" 5 "Otros no Parientes" 6 "Servicio Domestico"
label value relacion relacion

	****************************
	***VARIABLES DEMOGRAFICAS***
	****************************

***************
***factor_ci***
***************
gen factor_ci=factor_ch

**********
***sexo***
**********
gen sexo_ci=sexo
label var sexo "Sexo del Individuo"
label define sexo_ci 1 "Masculino" 2 "Femenino"
label value sexo_ci sexo_ci

**********
***edad***
**********
gen edad_ci=edad

*****************
***civil_ci***
*****************
gen civil_ci=.
*************
***jefe_ci***
*************

gen jefe_ci=(relacion_ci==1)
label variable jefe_ci "Jefe de hogar"


******************
***nconyuges_ch***
******************

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

by idh_ch, sort: egen nmayor21_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad_ci>=21)
label variable nmayor21_ch "Numero de familiares mayores a 21 anios"

*****************
***nmenor21_ch***
*****************

by idh_ch, sort: egen nmenor21_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad_ci<21)
label variable nmenor21_ch "Numero de familiares menores a 21 anios"

*****************
***nmayor65_ch***
*****************

by idh_ch, sort: egen nmayor65_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad_ci>=65)
label variable nmayor65_ch "Numero de familiares mayores a 65 anios"

****************
***nmenor6_ch***
****************

by idh_ch, sort: egen nmenor6_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad_ci<6)
label variable nmenor6_ch "Numero de familiares menores a 6 anios"

****************
***nmenor1_ch***
****************

by idh_ch, sort: egen nmenor1_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad_ci<1)
label variable nmenor1_ch "Numero de familiares menores a 1 anio"

****************
***miembros_ci***
****************

gen miembros_ci=(relacion_ci<6)
label variable miembros_ci "Miembro del hogar"

************************************
*** VARIABLES DEL MERCADO LABORAL***
************************************


****************
****condocup_ci*
****************










gen miembros_ci=1 if parentco>=1 & parentco<=6

replace  miembros_ci=0 if parentco==7

gen uno=1 if miembros_ci==1

egen nmiembros_ch=sum(uno), by(idh_ch)

replace  nmiembros_ch=. if miembros_ci!=1

label var nmiembros_ch "Numero de miembros del Hogar"

drop uno

/*gen emp_ci=.

replace  emp_ci=1 if trabajo==1 | realitrb==1

replace  emp_ci=0 if trabajo==6 & realitrb==6

label var emp_ci "Empleado en la semana de referencia"*/

****************
****condocup_ci*
****************

recode condact (1=1) (2 3=2) (4/9=3), gen(condocup_ci)
replace condocup_ci=4 if edad_ci<10
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

replace  ylmpri_ci=yocupp if yocupp<99999 & edad>9 & yocupp>0

replace  ylmpri_ci=ganocupp if ganocupp<99999 & edad>9 & ganocupp>0

replace  ylmpri_ci=0 if yocupp==0 & ganocupp==0 & edad>9 & (trabajo==1 | realitrb==1)

replace  ylmpri_ci=0 if categ==8 & edad>9 & (trabajo==1 | realitrb==1)

label var ylmpri_ci "Ingreso Laboral Monetario de la Actividad Principal"

gen ylmotros_ci=.

replace  ylmotros_ci=yocups if yocups<99999 & edad>9 & yocups>0

replace  ylmotros_ci=ganocups if ganocups<99999 & edad>9 & ganocups>0

replace  ylmotros_ci=0 if yocups==0 & ganocups==0 & edad>9 & otrotrab==1

label var ylmotros_ci "Ingreso Laboral Monetario de Otras Ocupaciones distintas a la principal"

egen ylm_ci=rsum(ylmpri_ci ylmotros_ci)

replace  ylm_ci=. if ylmpri_ci==. & ylmotros_ci==.

label var ylm_ci "Ingreso Laboral Monetario Total"

gen nrylmpri_ci=0 

replace  nrylmpri_ci=1 if yocupp==99999

replace  nrylmpri_ci=1 if ganocupp==99999

label var nrylmpri_ci "Identificador de No Respuesta del Ingreso Monetario de la Actividad Principal"

egen ylmnr_ci=rsum(ylmpri_ci ylmotros_ci) if nrylmpri_ci==0

replace  ylmnr_ci=. if ylmpri_ci==. 

label var ylmnr_ci "Ingreso Laboral Monetario Total, considera 'missing' la No Respuesta "

egen nrylmpri_ch=sum(nrylmpri_ci) if miembros_ci==1, by(idh_ch)

replace  nrylmpri_ch=1 if nrylmpri_ch>1 & nrylmpri_ch<.

label var nrylmpri_ch "Identificador de Hogares en donde alguno de los miembros No Responde el Ingreso Monetario de la Actividad Principal"

egen ylm_ch=sum(ylm_ci) if miembros_ci==1, by(idh_ch)

label var ylm_ch "Ingreso Laboral Monetario del Hogar"

egen ylmnr_ch=sum(ylm_ci) if miembros_ci==1 & nrylmpri_ch==0, by(idh_ch)

label var ylmnr_ch "Ingreso Laboral Monetario del Hogar, considera 'missing' la No Respuesta"

gen categopri_ci=1 if categ==7

replace  categopri_ci=2 if categ==4 | categ==5 | categ==6

replace  categopri_ci=3 if categ==1 | categ==2 | categ==3

replace  categopri_ci=4 if categ==8

label var categopri_ci "Categoria ocupacional actividad principal"

label define categopri_ci 1 "Patron" 2 "Cuenta Propia" 3 "Empleado" 4 "Trabajador no remunerado"

label value categopri_ci categopri_ci

gen rama_ci=ramaocr

replace  rama_ci=. if ramaocr<1 | emp_ci==0



/*
gen desemp1_ci=.
replace desemp1_ci=1 if buscosem==1 
replace desemp1_ci=0 if buscosem==6 | emp==1
label var desemp1_ci "Personas que no tienen trabajo y han buscado trabajo la semana pasada"

gen desemp2_ci=.
replace desemp2_ci=0 if desemp1==0
replace desemp2_ci=1 if desemp1_ci==1 | pqnobus==3 | pqnobus==4
label var desemp2_ci "desemp1 + personas que no tienen trabajo ni lo buscaron, pero esperan respuesta de una solicitud de empleo, entrevista o temporada agricola"

gen desemp3_ci=.
replace desemp3_ci=0 if desemp2==0 
replace desemp3_ci=1 if desemp2==1 | buscomes==1 
label var desemp3_ci "desemp2 + personas que no tienen trabajo pero han buscado trabajo durante las 4 semanas anteriores a la semana pasada"

gen byte pea1_ci=.
replace pea1=1 if emp==1 | desemp1==1
replace pea1=0 if emp==0 & desemp1==0
label variable pea1_ci "Poblacion Economicamente Activa utilizando la definicion 'desemp1'"

gen byte pea2_ci=.
replace pea2=1 if emp==1 | desemp2==1
replace pea2=0 if emp==0 & desemp2==0
label variable pea2_ci "Poblacion Economicamente Activa utilizando la definicion 'desemp2'"

gen byte pea3_ci=.
replace pea3=1 if emp==1 | desemp3==1
replace pea3=0 if emp==0 & desemp3==0
label variable pea3_ci "Poblacion Economicamente Activa utilizando la definicion 'desemp3'"
*/
gen asiste_ci=.
replace asiste_ci=1 if asiste==1
replace asiste_ci=0 if asiste==6
label var asiste_ci "Personas que actualmente asisten a centros de enseñanza"
drop asiste

/*gen segsoc_ci=.
label var segsoc "Personas que cuentan con seguro social"*/



gen durades_ci=.
replace durades_ci=tpobusc if tpobusc<99 & tpobusc>0
replace durades=0.5 if tpobusc==1 & tpobusc==0
label var durades "Duracion del Desempleo (en meses)"

gen nempleos_ci=.
label var nempleos "Numero de empleos"

/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/


**********
**tc_ci***
**********
gen tc_ci=2
label var tc_ci "Tipo de cambio LCU/USD"

*************
**salmm_ci***
*************
* HON 1989
gen salmm_ci= 	180.30
label var salmm_ci "Salario minimo legal"

****************
*afiliado_ci****
****************
gen afiliado_ci=.
label var afiliado_ci "Afiliado a la Seguridad Social"

****************
*cotizando_ci***
****************
gen cotizando_ci=.
label var cotizando_ci "Cotizante a la Seguridad Social"

*****************
*tipocontrato_ci*
*****************
gen tipocontrato_ci=.
label var tipocontrato_ci "Tipo de contrato segun su duracion"

*************
*tamemp_ci
*************
recode tamaest (1=1) (2=2) (nonmissing=.), gen(tamemp_ci)
label var tamemp_ci "# empleados en la empresa"
label define tamemp_ci  1 "Menos de 10" 2 "10 o mas"


*************
*cesante_ci* 
*************
gen cesante_ci=trabant if trabant==1 & condocup_ci==2
label var cesante_ci "Desocupado -definicion oficial del pais- que ha trabajado antes"	

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

*************
**ypen_ci*
*************
gen ypen_ci=.
label var ypen_ci "Valor de la pension contributiva"


*************
**pension_ci*
*************

gen pension_ci=.
label var pension_ci "1=Recibe pension contributiva"

*****************
**ypensub_ci*
*****************

gen ypensub_ci=.
label var ypensub_ci "Valor de la pension subsidiada / no contributiva"

***************
*pensionsub_ci*
***************

gen pensionsub_ci=.
label var pensionsub_ci "1=recibe pension subsidiada / no contributiva"


*************
***tecnica_ci**
*************
gen tecnica_ci=.
replace tecnica_ci=nivel==6|nivel==7
recode tecnica_ci .=0 
label var tecnica_ci "=1 formacion terciaria tecnica"

*Poverty

*********
*lp25_ci
*********
gen lp25_ci = .

label var lp25_ci "Linea de pobreza de uds1.25 por dia en moneda local"

*********
*lp4_ci*
*********
gen lp4_ci = .

label var lp4_ci "Linea de pobreza de uds4 por dia en moneda local"


*********
*lp_ci***
*********
capture drop lp_ci
gen lp_ci =.
label var lp_ci "Linea de pobreza oficial del pais"

*********
*lpe_ci***
*********

gen lpe_ci =.
label var lpe_ci "Linea de indigencia oficial del pais"

**********************************
**** VARIABLES DE LA VIVIENDA ****
**********************************
gen aguared_ch=.

gen aguadist_ch=.

replace  aguadist_ch=1 if aguab==6

replace  aguadist_ch=2 if aguab==7

replace  aguadist_ch=3 if aguab==5

gen aguamala_ch=.

replace  aguamala_ch=1 if aguaa==4

replace  aguamala_ch=0 if aguaa>=1 & aguaa<=3

gen aguamide_ch=.

gen luz_ch=.

replace  luz_ch=1 if luz==1 | luz==2 | luz==3

replace  luz_ch=0 if luz==4 

gen luzmide_ch=.

gen combust_ch=.

gen bano_ch=.

replace  bano_ch=1 if servsana==1 | servsana==2

replace  bano_ch=0 if servsana==3

gen banoex_ch=.

replace  banoex=1 if usoserv==7

replace  banoex=0 if usoserv==8

gen des1_ch=.
 
gen des2_ch=.

replace  des2_ch=1 if servsanb==4 | servsanb==5 

replace  des2_ch=2 if servsanb==6 

replace  des2_ch=0 if servsanb==0

gen piso_ch=.

gen pared_ch=.

replace  pared_ch=0 if paredes==4 | paredes==5

replace  pared_ch=1 if paredes>=1 & paredes<=3

*replace pared_ch=2 if paredes==6

gen techo_ch=.

gen resid_ch=.

gen dorm_ch=.

replace  dorm_ch=nrodorm if nrodorm>=0 & nrodorm<99

gen cuartos_ch=.

replace  cuartos_ch=nrohab if nrohab>=0 & nrohab<99

gen cocina_ch=.

gen telef_ch=.

gen refrig_ch=.

gen freez_ch=.

gen auto_ch=.

gen compu_ch=.

gen internet_ch=.

gen cel_ch=.

gen vivi1_ch=.

gen vivi2_ch=.

replace  vivi2_ch=1 if tipoviv==1

replace  vivi2_ch=0 if tipoviv>=2 & tipoviv<=6

gen viviprop_ch=.

replace  viviprop_ch=0 if tenencia==3

replace  viviprop_ch=1 if tenencia==1

replace  viviprop_ch=2 if tenencia==2

replace  viviprop_ch=3 if tenencia==4

gen vivitit_ch=.

gen vivialq_ch=.

replace  vivialq_ch=pagomens if pagomens>0 & pagomens<9999

gen vivialqimp_ch=.

* falta agregar las siguientes variables:
gen horaspri_ci=.
gen aedu_ci=.

gen ylnm_ci=.
gen ynlm_ci=.
gen ynlnm_ci=.
gen antiguedad_ci=.
gen ocupa_ci=.
gen firmapeq_ci=.
*gen contrato_ci=.
gen subemp_ci=.

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
lp_ci	       lpe_ci	       cotizando_ci	         afiliado_ci	///
tipopen_ci	   instpen_ci	   instcot_ci	   instpen_ci	   tipocontrato_ci 	   condocup_ci 	   cesante_ci ///
tamemp_ci 	   pension_ci 	   ypen_ci 	   pensionsub_ci 	   ypensub_ci 	   salmm_ci	   tecnica_ci	///
tamemp_ci categoinac_ci formal_ci

*/

qui destring $var, replace
 

* Activar solo si es necesario
*keep *_ci  *_c  idh_ch 
qui compress



saveold "`base_out'", replace


log close



