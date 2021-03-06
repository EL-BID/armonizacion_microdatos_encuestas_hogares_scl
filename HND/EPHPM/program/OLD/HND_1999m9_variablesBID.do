use "${surveysFolder}\ARM\HON\1999\Orig_data\hnd99.dta", clear

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
local ANO "1999"
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

							SCL/LMK - IADB
****************************************************************************/
/***************************************************************************
Detalle de procesamientos o modificaciones anteriores:
 opened on:  16 Aug 2006, 12:40:08
*******************************************************************************************************
*****                                   HONDURAS 1999                                                 *
*****                EPHPM 1999 (Encuesta Permanente de Hogares de Propositos Multiples)              *
*****                                  33.772 personas                                                *
*****                                   6.611 hogares                                                 *
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

compress

gen pais_c="HON"
 
*** HOUSING ***
gen aguared_ch=.
 
gen aguadist_ch=.

replace aguadist_ch=1 if aguab==6

replace aguadist_ch=2 if aguab==7 

replace aguadist_ch=3 if aguab==8
 
gen aguamala_ch=.

replace aguamala_ch=1 if aguaa>=4 & aguaa<=5

replace aguamala_ch=0 if aguaa>=1 & aguaa<=3

gen aguamide_ch=.
 
gen luz_ch=.

replace luz_ch=1 if luz==1 | luz==2 | luz==3

replace luz_ch=0 if luz==4 
 
gen luzmide_ch=.

gen combust_ch=.
 
gen bano_ch=.

replace bano_ch=1 if servsana==1 | servsana==2

replace bano_ch=0 if servsana==3

gen banoex_ch=.

replace banoex=1 if usoserv==7

replace banoex=0 if usoserv==8

gen des1_ch=.

gen des2_ch=.

replace des2_ch=1 if servsanb==4 | servsanb==5 

replace des2_ch=2 if servsanb==6 

replace des2_ch=0 if servsanb==0

gen piso_ch=.

replace piso_ch=0 if piso==5

replace piso_ch=1 if piso>=1 & piso<=4 

*replace piso_ch=2 if piso==
 
gen pared_ch=.

replace pared_ch=0 if paredes==4 | paredes==5

replace pared_ch=1 if paredes>=1 & paredes<=3

*replace pared_ch=2 if paredes==6
gen techo_ch=.

gen resid_ch=.
 
gen dorm_ch=.

replace dorm_ch=nrodorm if nrodorm>=0 & nrodorm<99

gen cuartos_ch=.

replace cuartos_ch=nrohab if nrohab>=0 & nrohab<99

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

replace vivi2_ch=1 if tipoviv==1

replace vivi2_ch=0 if tipoviv>=2 & tipoviv<=6

gen viviprop_ch=.

replace viviprop_ch=0 if tenencia==3

replace viviprop_ch=1 if tenencia==1

replace viviprop_ch=2 if tenencia==2

replace viviprop_ch=3 if tenencia==4 | tenencia==5 | tenencia==6

gen vivitit_ch=.
 
gen vivialq_ch=.

replace vivialq_ch=pagomens if pagomens>0 & pagomens<9999

gen vivialqimp_ch=.

gen anio_c=1999

gen mes_c=9

label define mes_c 3 "Marzo" 5 "Mayo" 9 "Septiembre"

label value mes_c mes_c

egen idh_ch=group(region depto municip dominio replica segmento vivienda hogar) 

gen factor_ch=factorex

gen factor_ci=factor_ch

gen zona_c=1 if dominio==1 | dominio==2 | dominio==3 | dominio==4

replace zona_c=0 if dominio==5

label define zona_c 0 "Rural" 1 "Urbana" 

label value zona_c zona_c

gen miembros_ci=1 if parentco>=1 & parentco<=7

replace miembros_ci=0 if parentco==8

gen uno=1 if miembros_ci==1

egen nmiembros_ch=sum(uno), by(idh_ch)

replace nmiembros_ch=. if miembros_ci!=1

label var nmiembros_ch "Numero de miembros del Hogar"

drop uno
gen sexo_ci=sexo

label var sexo "Sexo del Individuo"

label define sexo_ci 1 "Masculino" 2 "Femenino"

label value sexo_ci sexo_ci

gen edad_ci=edad if edad<99

label var edad_ci "Edad del Individuo"

drop edad edadj

gen civil_ci=.

replace civil_ci=1 if estcivil==5

replace civil_ci=2 if estcivil==1 | estcivil==6

replace civil_ci=3 if estcivil==3 | estcivil==4

replace civil_ci=4 if estcivil==2

label var civil_ci "Estado Civil"

label define civil_ci 1 "Soltero" 2 "Union Formal o Informal" 3 "Divorciado o Separado" 4 "Viudo"

label value civil_ci civil_ci

gen jefe_ci=0

replace jefe_ci=1 if parentco==1

label var jefe_ci "Jefe de Hogar Declarado"

gen relacion_ci=.

replace relacion_ci=1 if parentco==1

replace relacion_ci=2 if parentco==2

replace relacion_ci=3 if parentco==3 

replace relacion_ci=4 if parentco==4 | parentco==5 | parentco==6

replace relacion_ci=5 if parentco==7

replace relacion_ci=6 if parentco==8

label var relacion_ci "Relacion con el Jefe de Hogar"

label define relacion 1 "Jefe de Hogar" 2 "Conyuge" 3 "Hijos" 4 "Otros Parientes" 5 "Otros no Parientes" 6 "Servicio Domestico"
label value relacion relacion

egen byte nconyuges_ch=sum(parentco==2), by (idh)

label variable nconyuges "Numero de Conyuges"

egen byte nhijos_ch=sum(parentco==3), by (idh)

label variable nhijos_ch "Numero de Hijos menores de 18"

egen byte notropari_ch=sum(parentco==4 | parentco==5 | parentco==6),by (idh)

label variable notropari_ch "Numero de Otros Parientes "

egen byte notronopari_ch=sum(parentco==7), by(idh)

label variable notronopari_ch "Numero de Otros NO Parientes "

egen byte nempdom_ch=sum(parentco==8), by(idh)

label variable nempdom_ch "Numero de Empleados Domesticos"

gen clasehog_ch=.

replace clasehog_ch=1 if nhijos_ch==0 & nconyuges_ch==0 & notropari_ch==0 & notronopari_ch==0 /* uniper sonal*/

replace clasehog_ch=2 if nhijos_ch>0 & notropari_ch==0 & notronopari_ch==0 /* nuclear (child with or without spouse but without other relatives)*/

 replace clasehog_ch=2 if nconyuges_ch>0 & notropari_ch==0 & notronopari_ch==0 /* nuclear (spouse with or without children but without other relatives)*/

replace clasehog_ch=3 if notropari_ch>0 & notronopari_ch==0 /* ampliado*/

replace clasehog_ch=4 if (nconyuges_ch>0 | nhijos_ch>0 | (notropari_ch>0 & notropari_ch<.)) & (notronopari_ch>0 & notronopari_ch<.) /* compuesto  (some relatives plus non relative)*/


replace clasehog_ch=5 if nhijos_ch==0 & nconyuges_ch==0 & notropari_ch==0 & notronopari_ch>0 & notronopari_ch<. /** corresidente*/

label variable clasehog_ch "CLASE HOGAR"

label define clasehog_ch 1 "Unipersonal" 2 "Nuclear" 3 "Ampliado" 4 "Compuesto" 5 "Corresidente"

label value clasehog_ch clasehog_ch

egen nmayor21_ch=sum((parentco>0 & parentco<=7) & (edad>=21)), by (idh)

label variable nmayor21_ch "Numero de personas de 21 años o mas dentro del Hogar"

egen nmenor21_ch=sum((parentco>0 & parentco<=7) & (edad<21)), by (idh)

label variable nmenor21_ch "Numero de personas menores a 21 años dentro del Hogar"

egen nmayor65_ch=sum((parentco>0 & parentco<=7) & (edad>=65)), by (idh)

label variable nmayor65_ch "Numero de personas de 65 años o mas dentro del Hogar"

egen nmenor6_ch=sum((parentco>0 & parentco<=7) & (edad<6)), by (idh)

label variable nmenor6_ch "Numero de niños menores a 6 años dentro del Hogar"

egen nmenor1_ch=sum((parentco>0 & parentco<=7) & (edad<1)),  by (idh)

label variable nmenor1_ch "Numero de niños menores a 1 año dentro del Hogar"

gen idp_ci=ordenper

label var idp_ci "Identificador Individual dentro del Hogar"

gen asiste_ci=.

replace asiste_ci=1 if asiste==1

replace asiste_ci=0 if asiste==6

label var asiste_ci "Personas que actualmente asisten a centros de enseñanza"

drop asiste

gen pqnoasis_ci=.

label var pqnoasis_ci "Razones para no asistir a centros de enseñanza"

gen repiteult_ci=.

label var repiteult_ci "Personas que han repetido el ultimo grado"

gen repite_ci=.

label var repite_ci "Personas que han repetido al menos un año o grado"

* Años de educacion aprobados **
replace grado=. if grado==9

gen aedu_ci=.

replace aedu_ci=0 if (nivel>=1 & nivel<=3) | (nivel==4 & grado==0)

replace aedu_ci=grado if nivel==4 & grado>=1

replace aedu_ci=grado+6 if (nivel==5 | nivel==6) & grado>=0

replace aedu_ci=grado+12 if (nivel==7 | nivel==8) & grado>=0

*replace aedu_ci=grado+16 if nivel==9
label var aedu_ci "Años de educacion aprobados"
 
gen eduno_ci=.

replace eduno=1 if (nivel==1 & edad>=5) 

replace eduno=0 if (nivel>3 & nivel<9 & edad>=5)     

label var eduno_ci "1 = personas sin educacion (excluye preescolar)"

gen edupi_ci=.

replace edupi=1 if (nivel==4 & grado<6 & grado>=0) 

replace edupi=0 if ((nivel>=5 & nivel<9) | (nivel==4 & grado>=6)) | (eduno==1)

label var edupi_ci "1 = personas que no han completado el nivel primario"

gen edupc_ci=.

replace edupc=1 if (nivel==4 & grado==6) 

replace edupc=0 if (edupi==1 | eduno==1) | (nivel==4 & grado>6) | (nivel>4 & grado<9 & nivel<9) 

replace edupi=1 if nivel==4 & (grado==0 | grado==.)

label var edupc_ci "1 = personas que han completado el nivel primario"

gen edusi_ci=.

replace edusi=1 if (nivel==4 & grado>6 & grado<.) | (nivel==5 & grado<6 & grado>=0) | (nivel==6 & grado <3 & grado>=0)

replace edusi=0 if (edupi==1 | eduno==1 | edupc==1) | (nivel==5 & grado==6 & grado<.) | (nivel==6 & grado==3 & grado<.) | (nivel>=7 & nivel<9) 

label var edusi_ci "1 = personas que no han completado el nivel secundario"

gen edusc_ci=.

replace edusc=1 if (nivel==5 & grado==6 & grado<.) | (nivel==6 & grado==3 & grado<.) 

replace edusc=0 if (edupi==1 | eduno==1 | edupc==1 | edusi==1) | (nivel>6 & nivel<9) 

label var edusc_ci "1 = personas que han completado el nivel secundario"

gen eduui_ci=.

replace eduui=1 if (nivel==7 & grado<3 & grado>=0) | (nivel==8 & grado<5 & grado>=0) 

replace eduui=0 if (edupi==1 | eduno==1 | edupc==1 | edusi==1 | edusc==1) | (nivel==7 & grado>=3) | (nivel==8 & grado>=5) 

label var eduui_ci "1 = personas que no han completado el nivel universitario"

gen eduuc_ci=.

replace eduuc=1 if (nivel==7 & grado>=3 & grado<.) | (nivel==8 & grado>=5 & grado<.)

replace eduuc=0 if (edupi==1 | eduno==1 | edupc==1 | edusi==1 | edusc==1 | eduui==1) 

label var eduuc_ci "1 = personas que han completado el nivel universitario"


replace edupi=1 if nivel==4 & grado==.

replace edupc=0 if nivel==4 & grado==.

replace edusi=1 if (nivel==5 | nivel==6) & grado==.

replace edusc=0 if (nivel==5 | nivel==6) & grado==.

replace eduui=1 if (nivel==7 | nivel==8) & grado==.

replace eduuc=0 if (nivel==7 | nivel==8) & grado==.

gen edus1i_ci=.

replace edus1i=0 if edusi==1 | edusc==1 

replace edus1i=1 if edusi==1 & (nivel==5 & grado<3 & grado>=0) | (nivel==6 & grado<3 & grado>=0)

label var edus1i_ci "1 = personas que no han completado el primer ciclo de la educacion secundaria"

gen edus1c_ci=.

replace edus1c=0 if edusi==1 | edusc==1 

replace edus1c=1 if edusi==1 & (nivel==5 & grado==3) | (nivel==6 & grado==3)

label var edus1c_ci "1 = personas que han completado el primer ciclo de la educacion secundaria"

gen edus2i_ci=.

replace edus2i=0 if edusi==1 | edusc==1 

replace edus2i=1 if edusi==1 & (nivel==5 & grado>3 & grado<6) 

label var edus2i_ci "1 = personas que no han completado el segundo ciclo de la educacion secundaria"

gen edus2c_ci=.

replace edus2c=0 if edusi==1 | edusc==1 

replace edus2c=1 if edusi==1 & (nivel==5 & grado==6) 

label var edus2c_ci "1 = personas que han completado el segundo ciclo de la educacion secundaria"

gen edupre_ci=.

replace edupre=0 if eduno==1 | edupi==1 | edupc==1 | edusi==1 | edusc==1 | eduui==1 | eduuc==1

replace edupre=1 if nivel==2 

label var edupre_ci "Educacion preescolar"

gen eduac_ci=.

replace eduac=0 if eduui==1 | eduuc==1

replace eduac=1 if nivel==8 

label var eduac_ci "Educacion universitaria vs educacion terciaria"

gen edupub_ci=.

label var edupub_ci "1 = personas que asisten a centros de enseñanza publicos"

gen categopri_ci=1 if categ==7

replace categopri_ci=2 if categ==4 | categ==5 | categ==6

replace categopri_ci=3 if categ==1 | categ==2 | categ==3

replace categopri_ci=4 if categ==8

label var categopri_ci "Categoria ocupacional actividad principal"

label define categopri_ci 1 "Patron" 2 "Cuenta Propia" 3 "Empleado" 4 "Trabajador no remunerado"

label value categopri_ci categopri_ci

/*gen emp_ci=.

replace emp_ci=1 if trabajo==1 | realitrb==1

replace emp_ci=0 if trabajo==6 & realitrb==6

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

replace ylmpri_ci=yocupp if yocupp<99999 & edad>9 & yocupp>0

replace ylmpri_ci=ganocupp if ganocupp<99999 & edad>9 & ganocupp>0

replace ylmpri_ci=0 if yocupp==0 & ganocupp==0 & edad>9 & (trabajo==1 | realitr==1)

replace ylmpri_ci=0 if categ==8 & edad>9 & (trabajo==1 | realitr==1)

label var ylmpri_ci "Ingreso Laboral Monetario de la Actividad Principal"

gen ylmsec_ci=.

replace ylmsec_ci=yocups if yocups<99999 & edad>9 & yocups>0

replace ylmsec_ci=ganocups if ganocups<99999 & edad>9 & ganocups>0

replace ylmsec_ci=0 if yocups==0 & ganocups==0 & edad>9 & otrotrab==1

label var ylmsec_ci "Ingreso Laboral Monetario de la Actividad Secundaria"

egen ylm_ci=rsum(ylmpri_ci ylmsec_ci)

replace ylm_ci=. if ylmpri_ci==. & ylmsec_ci==.

label var ylm_ci "Ingreso Laboral Monetario Total"

gen horaspri_ci= hrsocupp if  hrsocupp<99 &  hrsocupp>0

label var horaspri "Horas totales trabajadas la semana pasada en la Actividad Principal"

gen horassec_ci= hrsocups if  hrsocups<99 &  hrsocups>0

egen horastot=rsum(horaspri horassec)

replace horastot=. if horaspri==. & horassec==.

label var horassec "Horas totales trabajadas la semana pasada en todas las Actividades"

drop horassec


gen ylmhopri_ci=ylmpri_ci/(4.3*horaspri)

label var ylmhopri_ci "Salario Horario Monetario de la Actividad Principal"

gen ylmho_ci=ylm_ci/(4.3*horastot)

label var ylmho_ci "Salario Horario Monetario de todas las Actividades"


gen durades_ci=.

replace durades_ci=tpobusc if tpobusc<99 & tpobusc>0

replace durades=0.5 if ctpobusc==1 & tpobusc==0

label var durades "Duracion del Desempleo (en meses)"

gen antiguedad=anostrab if emp_ci==1 & anostrab<99 & anostrab>0

replace antiguedad=0.5 if tpotraba==1

label var antiguedad "Antiguedad en la Ocupacion Actual (en anios)"
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

label variable pea3_ci "Poblacion Economicamente Activa utilizando la definicion 'desemp3'"*/

gen desalent_ci=.

replace desalent=0 if pea==1

replace desalent=1 if pqnobus==5

label var desalent_ci "Trabajadores desalentados, personas que creen que por alguna razon no conseguiran trabajo" 
 
gen subemp_ci=.

replace subemp=0 if emp_ci==0 | emp_ci==1

replace subemp=1 if horastot<30 & destrb==1

label var subemp "Trabajadores subempleados"

gen tiempoparc=.

replace tiempoparc=0 if emp_ci==0 | emp_ci==1

replace tiempoparc=1 if horastot<30 & destrb==6

label var tiempoparc "Trabajadores a medio tiempo"

*gen contrato_ci=.

*label var contrato "Peronas empleadas que han firmado un contrato de trabajo"

*gen segsoc_ci=.

*label var segsoc "Personas que cuentan con seguro social"

gen nempleos_ci=1 if emp_ci==1

replace nempleos=2 if emp_ci==1 & otrotrab==1

replace nempleos=0 if emp_ci==0

label var nempleos "Numero de empleos"

gen tamfirma_ci=.

replace tamfirma=1 if tamaest==2

replace tamfirma=0 if tamaest==1

label var tamfirma "Trabajadores formales: 1 = + de 10 empleados"


gen spublico_ci=.

label var spublico "Personas que trabajan en el sector publico"

gen yalim2= yalim if yalim<99999 & yalim>=0

gen yropa2= yropa if yropa<99999 & yropa>=0

gen yhabita2= yhabita if yhabita<99999 & yhabita>=0

gen ytrans2= ytrans if ytrans<99999 & ytrans>=0

gen yotro2= yotro if yotro<99999 & yotro>=0

gen yprodu2= yprodu if yprodu<99999 & yprodu>=0

egen ylnm_ci=rsum(yalim2 yropa2 yhabita2 ytrans2 yotro2 yprodu2)

replace ylnm_ci=. if yalim2==. & yropa2==. & yhabita2==. & ytrans2==. & yotro2==. & yprodu2==.

replace ylnm_ci=0 if (ralim==6 & rropa==6 & rhabita==6 & rtrans==6 & rotro==6) | retprodu==6

label var ylnm_ci "Ingreso Laboral No Monetario Total"

gen autoconsumo_ci=yprodu2 

replace autoconsumo_ci=0 if yprodu==0 & edad>9 & (categopri==1 | categopri==2)

label var autoconsumo_ci "Autoconsumo Individual (Trabajadores Independientes)"

gen ypenju2=ypenju if ypenju>0 & ypenju<99999

gen ysubsi2=ysubsi if ysubsi>0 & ysubsi<99999

gen yalquile2=yalquile if yalquile>0 & yalquile<99999

gen ybonos2=ybonos if ybonos>0 & ybonos<99999

gen yremesa2=yremesa if yremesa>0 & yremesa<99999

gen yayuda2=yayuda if yayuda>0 & yayuda<99999

gen yayupar2=yayupar if yayupar>0 & yayupar<99999

gen yotros2=yotros if yotros>0 & yotros<99999

gen remesas_ci=yremesa2

replace remesas_ci=0 if yremesa==0

label var remesas_ci "Remesas Individuales"

egen ynlm_ci=rsum(ypenju2 ysubsi2 yalquile2 ybonos2 yremesa2 yayuda2 yayupar2 yotros2)

replace ynlm_ci=. if ypenju2==. & ysubsi2==. & yalquile2==. & ybonos2==. & yremesa2==. & yayuda2==. & yayupar2==. & yotros2==.

replace ynlm_ci=0 if ypenju==0 & ysubsi==0 & yalquile==0 & ybonos==0 & yremesa==0 & yayuda==0 & yayupar==0 & yotros==0 

label var ynlm_ci "Ingreso No Laboral Monetario"


gen nrylmpri_ci=0 

replace nrylmpri_ci=1 if yocupp==99999

replace nrylmpri_ci=1 if ganocupp==99999

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

egen autoconsumo_ch=sum(autoconsumo_ci) if miembros_ci==1, by(idh_ch)

label var autoconsumo_ch "Autoconsumo del Hogar"


egen remesas_ch=sum(remesas_ci) if miembros_ci==1, by(idh_ch)

label var remesas_ch "Remesas del Hogar"

gen rama_ci=ramaocr

replace rama_ci=. if ramaocr<1 | emp_ci==0

*drop yalim2 yropa2 yhabita2 ytrans2 yotro2 yprodu2 ypenju2 ysubsi2 yalquile2 ybonos2 yremesa2 yayuda2 yayupar2 yotros2

gen firmapeq_ci=.
replace firmapeq_ci= 1 if perstrab>1 & perstrab<=5
recode firmapeq_ci .=0 if tamaest==1 | tamaest==2


tab firmapeq_ci [w=int(factor_ci)] 
***************
*subemp_ci    *	
***************

drop subemp_ci

gen subemp_ci=.
replace subemp_ci=1 if horastot>=1 & horastot<30 & destrbms==1
recode subemp_ci .=0 if  condact==1
tab subemp_ci [w=int(factor_ci)]

/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/


**********
**tc_ci***
**********
gen tc_ci=14.66
label var tc_ci "Tipo de cambio LCU/USD"

*************
**salmm_ci***
*************
* HON 1999
gen salmm_ci= 	1221.58
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
gen ypen_ci=yjubcor if yjubcor!=0
label var ypen_ci "Valor de la pension contributiva"


*************
**pension_ci*
*************

gen pension_ci=ypen_ci!=.
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
gen lp25_ci = 442.2549

label var lp25_ci "Linea de pobreza de uds1.25 por dia en moneda local"

*********
*lp4_ci*
*********
gen lp4_ci = 707.6078

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

*falta armar las siguientes variables:
gen ynlnm_ci=.
gen antiguedad_ci=.
gen ocupa_ci=.
gen horastot_ci=.
gen ylmotros_ci =.
gen ylnmotros_ci=.
gen tcylmpri_ci=.
gen tcylmpri_ch =.
gen rentaimp_ch=.
gen autocons_ci=.
gen autocons_ch=.
gen tiempoparc_ci=.
gen raza_ci=.
gen instcot_ci=.
gen ynlnm_ch=.
gen ylnmpri_ci =.
gen ylnmsec_ci =.
gen categosec_ci=.

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



 
