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
 
global ruta = "${surveysFolder}"

local PAIS SUR
local ENCUESTA SLC
local ANO "2017"
local ronda m10_m9 


local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
   
capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Surinam
Encuesta: SLC
Round: m10_m9
Creado por: Alvaro Altamirano  (alvaroalt@iadb.org)
Fecha: Junio 2018

							SCL/LMK - IADB
****************************************************************************/
/***************************************************************************
Detalle de procesamientos o modificaciones anteriores:
MLO cambio del limite de edad de condocup_ci a 5+

****************************************************************************/

use `base_in', clear

		*************************
		***VARIABLES DEL HOGAR***
		*************************
		
************
* Region_BID *
************
gen region_BID_c=.
replace region_BID_c=2 
label var region_BID_c "Regiones BID"
label define region_BID_c 1 "Centroamérica_(CID)" 2 "Caribe_(CCB)" 3 "Andinos_(CAN)" 4 "Cono_Sur_(CSC)"
label value region_BID_c region_BID_c

	***************
	***region_c ***
	***************
	clonevar region_c=domain
	label var region_c "division politico-administrativa, provincia"
   
	***************
	***factor_ch***
	***************
	gen factor_ch=weight
	label variable factor_ch "Factor de expansion del hogar"
	
	*************
	****idh_ch***
	*************
	gen idh_ch = hhid
	label variable idh_ch "ID del hogar"
	
	*************
	****idh_ci***
	*************
	gen idp_ci=memberid
	label variable idp_ci "ID de la persona en el hogar"
	
	*************
	****zona_c***
	*************
	*No existe variable de area
	gen zona_c=.
	label value zona_c zona_c
	/*
	replace zona_c=0 	if area==2
	label variable zona_c "Zona del pais"
	label define zona_c 1 "Urbana" 0 "Rural"
		*/
	*************
	****pais_c***
	*************
	gen str3 pais_c="SUR"
	label variable pais_c "Pais"

	************
	***anio_c***
	************
	gen anio_c=2017
	label variable anio_c "Anio de la encuesta" 
		
	***********
	***mes_c***
	***********
	gen mes_c=09
	label var mes_c "Mes de la encuesta" 
	
	*****************
	***relacion_ci***
	*****************
	*la li q1_02
	gen relacion_ci=1     if q1_02==1
	replace relacion_ci=2 if q1_02==2
	replace relacion_ci=3 if q1_02==3
	replace relacion_ci=4 if q1_02>=4 & q1_02<=8
	replace relacion_ci=5 if q1_02==10
	replace relacion_ci=6 if q1_02==9
	label variable relacion_ci "Relacion con el jefe del hogar"
	label define relacion_ci 1 "Jefe/a" 2 "Esposo/a" 3 "Hijo/a" 4 "Otros parientes" 5 "Otros no parientes"
	label define relacion_ci 6 "Empleado/a domestico/a", add
	label value relacion_ci relacion_ci

			****************************
			***VARIABLES DEMOGRAFICAS***
			****************************
	
	***************
	***factor_ci***
	***************
	gen factor_ci=weight
	label variable factor_ci "Factor de expansion del individuo"
	
	***************
	***upm_ci***
	***************

	clonevar upm_ci=psu
	label variable upm_ci "Unidad Primaria de Muestreo"

	***************
	***estrato_ci***
	***************

	clonevar estrato_ci=stratum
	label variable estrato_ci "Estrato"

	*************
	***sexo_ci***
	*************
	gen sexo_ci=q1_03
	label var sexo_ci "Sexo del individuo" 
	label def sexo_ci 1"Masculino" 2"Femenino" 
	label val sexo_ci sexo_ci

	**********
	***edad***
	**********
	
	*tab q1_04
	gen edad_ci=q1_04 if q1_04<99
	label variable edad_ci "Edad del individuo"

*************************
*** VARIABLES DE RAZA ***
*************************

gen raza_ci=1  if q1_07 == 4
replace raza_ci=2 if q1_07 == 1 | q1_07 == 3
replace raza_ci=3 if q1_07 == 2
replace raza_ci=4 if q1_07 == 5
replace raza_ci=5 if raza_ci==. |  q1_07>=6
 
label define raza_ci 1 "Indígena" 2 "Afro-descendiente" 3 "Indo-Surinamese" 4 "Javanes" 5 "Otros"
label value raza_ci raza_ci 
label var raza_ci "Raza o etnia del individuo"

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

gen raza_idioma_ci=.
	
	**************
	***civil_ci***
	**************
	gen civil_ci=1 		if q1_09==1
	replace civil_ci=2	if q1_09==2 | q1_09==3
	replace civil_ci=3	if q1_09==4 | q1_09==5
	replace civil_ci=4	if q1_09==6
	label var civil_ci "Estado civil" 
	label def civil_ci 1"Soltero"  2 "Union formal o informal" 3"Divorciado o separado" 4"Viudo" 
	label val civil_ci civil_ci
		
	*************
	***jefe_ci***
	*************
	gen jefe_ci=(relacion_ci==1)
	label var jefe_ci "Jefe de hogar"
	label def jefe_ci 1"Si" 0"No"
	label val jefe_ci jefe_ci

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
	replace clasehog_ch=3 if ((clasehog_ch ==2 & notropari_ch>0) & notronopari_ch==0) |(notropari_ch>0 & notronopari_ch==0) 
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
	by idh_ch, sort: egen nmiembros_ch=sum(relacion_ci>=1 & relacion_ci<5)
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

	*****************
	***miembros_ci***
	*****************
	gen miembros_ci=(relacion_ci<5)
	label variable miembros_ci "Miembro del hogar"


			***********************************
			***VARIABLES DEL MERCADO LABORAL***
			***********************************

	****************
	****condocup_ci*
	****************
	*All persons 15 yrs and older
	generat condocup_ci=.
	replace condocup_ci=1 if q9_06==1 | q9_07==1 | q9_08==1 
	replace condocup_ci=2 if (q9_06==2 | q9_07==2 | q9_08==2) & q9_29==1
	replace condocup_ci=3 if condocup_ci!=1 & condocup_ci!=2
	replace condocup_ci=4 if edad_ci<15
	label define condocup_ci 1 "ocupados" 2 "desocupados" 3 "inactivos" 4 "menor <15"
	label value condocup_ci condocup_ci
	label var condocup_ci "Condicion de ocupacion utilizando definicion del pais"

	****************
	*afiliado_ci****
	****************
	*Which benefits do you receive from your employer in this job?, Pension or retirement fund
	gen afiliado_ci=(q9_16a==1 | q9_36b==1 | q9_36c==1 | q9_36d==1 | q9_36e==1) /*todas personas*/	
	replace afiliado_ci=. if q9_16a==. & q9_36b==. & q9_36c==. & q9_36d==. & q9_36e==.
	label var afiliado_ci "Afiliado a la Seguridad Social"
	
	****************
	*cotizando_ci***
	****************
	gen cotizando_ci=0     if condocup_ci==1 | condocup_ci==2 
	replace cotizando_ci=1 if afiliado_ci==1 
	label var cotizando_ci "Cotizante a la Seguridad Social"

	****************
	*instpen_ci*****
	****************
	gen instpen_ci=.
	label var instpen_ci "Institucion proveedora de la pension - variable original de cada pais" 
	gen tipopen_ci=.
	
	********************
	*** instcot_ci *****
	********************
	gen instcot_ci=.
	label var instcot_ci "institución a la cual cotiza"

	*************
	*tamemp_ci
	*************
	*Pequeña 1 a 5 Mediana 6 a 20 Grande Más de 20
	gen tamemp_ci=.
	replace tamemp_ci=1 if q9_14>=1 & q9_14<=5
	replace tamemp_ci=2 if q9_14>=6 & q9_14<=20
	replace tamemp_ci=3 if q9_14>20 & q9_14!=.
	label var tamemp_ci "# empleados en la empresa segun rangos"
	label define tamemp_ci 1 "Pequeña" 2 "Mediana" 3 "Grande"
	label value tamemp_ci tamemp_ci
	
	*************
	**pension_ci*
	*************
	gen pension_ci=0
	replace pension_ci=1 if q10_08>0 | q10_09>0 | q10_10>0 | q10_11>0 | q10_12>0
	replace pension_ci=. if q10_08==. & q10_09==. & q10_10==. & q10_11==. & q10_12==.
	label var pension_ci "1=Recibe pension contributiva"

	*************
	*ypen_ci*****
	*************
	egen ypen_ci=rowtotal(q10_08 q10_09 q10_10 q10_11 q10_12) if pension_ci==1
	replace ypen_ci=. if ypen_ci==999999 
	label var ypen_ci "Valor de la pension contributiva"

	***************
	*pensionsub_ci*
	***************
	gen pensionsub_ci=0 
	replace pensionsub_ci=(q10_07>0) if q10_07!=.
	label var pensionsub_ci "1=recibe pension subsidiada / no contributiva"

	*****************
	**  ypensub_ci  *
	*****************
	gen ypensub_ci=q10_07 if pensionsub_ci==1 & q10_07!=.
	replace ypensub_ci=. if ypensub_ci==999999
	label var ypensub_ci "Valor de la pension subsidiada / no contributiva"

	*************
	*cesante_ci* 
	*************
	cap clonevar trabant = q9_34
	generat cesante_ci=0 if condocup_ci==2 & trabant!=.
	replace cesante_ci=1 if (trabant!=1 & trabant!=.) & condocup_ci==2
	label var cesante_ci "Desocupado - definicion oficial del pais"

	*********
	*lp_ci***
	*********
	
	gen lp_ci =.
	label var lp_ci "Linea de pobreza oficial del pais"

	***********
	*lpe_ci ***
	***********
	
	gen lpe_ci = .
	label var lpe_ci "Linea de indigencia oficial del pais"

	
/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/

	*************
	**salmm_ci***
	*************
	*https://www.state.gov/documents/organization/265828.pdf
	gen salmm_ci= 835 
	label var salmm_ci "Salario minimo legal"

	************
	***emp_ci***
	************
	gen byte emp_ci=(condocup_ci==1)
	label var emp_ci "Ocupado (empleado)"

	****************
	***desemp_ci***
	****************
	gen desemp_ci=(condocup_ci==2)
	label var desemp_ci "Desempleado que buscó empleo en el periodo de referencia"
  
	*************
	***pea_ci***
	*************
	gen pea_ci=0
	replace pea_ci=1 if emp_ci==1 | desemp_ci==1
	label var pea_ci "Población Económicamente Activa"

	*****************
	***desalent_ci***
	*****************
	cap clonevar bustrama=q9_29
	cap clonevar motnobus = q9_30
	gen desalent_ci=(motnobus==6 | motnobus==3)
	label var desalent_ci "Trabajadores desalentados"
	
	*****************
	***horaspri_ci***
	*****************
	*considering all your jobs
	gen horaspri_ci=q9_23h
	replace horaspri_ci=. if q9_23h==999
	replace horaspri_ci=. if emp_ci==0
	label var horaspri_ci "Horas trabajadas semanalmente en el trabajo principal"

	*****************
	***horastot_ci***
	*****************
	gen horastot_ci=horaspri_ci if emp_ci==1 
	label var horastot_ci "Horas trabajadas semanalmente en todos los empleos"
	
	***************
	***subemp_ci***
	***************
	*Encuesta solo pregunta horas para TODAS las actividades simultaneamente, no se puede distinguir entre principal o secundaria
	gen subemp_ci=0
	replace subemp_ci=1 if q9_26==1 & q9_25==1 & horastot_ci<=30 & emp_ci==1
	label var subemp_ci "Personas en subempleo por horas"
	
	*******************
	***tiempoparc_ci***
	*******************
	*No se puede desagregar entre horas de trabajo principal o secundario, por lo que se tiene que usar horastot
	gen tiempoparc_ci=((horastot_ci>=1 & horastot_ci<30) & q9_25==2 & emp_ci==1)
	replace tiempoparc_ci=. if emp_ci==0
	label var tiempoparc_c "Personas que trabajan medio tiempo" 

	******************
	***categopri_ci***
	******************
	gen categopri_ci=.
	replace categopri_ci=1 if q9_10==1
	replace categopri_ci=2 if q9_10==2  
	replace categopri_ci=3 if (q9_10>=4 & q9_10<=6)
	replace categopri_ci=4 if q9_10==3
	replace categopri_ci=. if emp_ci==0
	replace categopri_ci=0 if q9_10==7
	label define categopri_ci 1"Patron" 2"Cuenta propia" 0"Otro"
	label define categopri_ci 3"Empleado" 4" No remunerado" , add
	label value categopri_ci categopri_ci
	label variable categopri_ci "Categoria ocupacional"

	******************
	***categosec_ci***
	******************
	gen categosec_ci=.
	label variable categosec_ci "Categoria ocupacional en la segunda actividad"

	*No hay información sobre ocupación secundaria
	/*
	replace categosec_ci=1 if p54==5
	replace categosec_ci=2 if p54==6
	replace categosec_ci=3 if (p54>=1 & p54<=4) | p54==10
	replace categosec_ci=4 if (p54>=7 & p54<=9)
	label define categosec_ci 1"Patron" 2"Cuenta propia" 0"Otro"
	label define categosec_ci 3"Empleado" 4" No remunerado" , add
	label value categosec_ci categosec_ci
*/
	*****************
	*tipocontrato_ci*
	*****************
	gen tipocontrato_ci=. /* Solo disponible para asalariados*/
	replace tipocontrato_ci=1 if q9_15==1 & categopri_ci==3
	replace tipocontrato_ci=2 if q9_15==2 & categopri_ci==3
	replace tipocontrato_ci=3 if q9_15==3 & categopri_ci==3
	label var tipocontrato_ci "Tipo de contrato segun su duracion en act principal"
	label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
	label value tipocontrato_ci tipocontrato_ci

	*****************
	***nempleos_ci***
	*****************
	gen nempleos_ci=q9_09
	replace nempleos_ci=. if emp_ci!=1
	label var nempleos_ci "Número de empleos" 
	label define nempleos_ci 1 "Un empleo" 2 "Mas de un empleo"
	label value nempleos_ci nempleos_ci
	
	*****************
	***spublico_ci***
	*****************
	gen spublico_ci=(q9_10==4 & emp_ci==1)
	replace spublico_ci=. if emp_ci==.
	label var spublico_ci "Personas que trabajan en el sector público"

	**************
	***ocupa_ci***
	**************
	*Utiliza CIUO-08.
	generat ocupa_ci=.
	replace ocupa_ci=1 if (q9_19>=2111 & q9_19<=3522) & emp_ci==1
	replace ocupa_ci=2 if (q9_19>=1111 & q9_19<=1439) & emp_ci==1
	replace ocupa_ci=3 if (q9_19>=4110 & q9_19<=4419) & emp_ci==1
	replace ocupa_ci=4 if ((q9_19>=5211 & q9_19<=5249) | (q9_19>=9510 & q9_19<=9520)) & emp_ci==1
	replace ocupa_ci=5 if ((q9_19>=5110 & q9_19<=5169) | (q9_19>=5311 & q9_19<=5419) | (q9_19>=9111 & q9_19<=9129) | (q9_19>=9610 & q9_19<=9624))  & emp_ci==1
	replace ocupa_ci=6 if ((q9_19>=6110 & q9_19<=6340) | (q9_19>=9210 & q9_19<=9216)) & emp_ci==1
	replace ocupa_ci=7 if ((q9_19>=7111 & q9_19<=8350) | (q9_19>=9310 & q9_19<=9412))  & emp_ci==1
	replace ocupa_ci=8 if (q9_19>=110 & q9_19<=310) & emp_ci==1
	replace ocupa_ci=9 if q9_19>=9629 & q9_19!=. & emp_ci==1
	label define ocupa_ci 1"profesional y tecnico" 2"director o funcionario sup" 3"administrativo y nivel intermedio"
	label define ocupa_ci  4 "comerciantes y vendedores" 5 "en servicios" 6 "trabajadores agricolas", add
	label define ocupa_ci  7 "obreros no agricolas, conductores de maq y ss de transporte", add
	label define ocupa_ci  8 "FFAA" 9 "Otras ", add
	label value ocupa_ci ocupa_ci
	label variable ocupa_ci "Ocupacion laboral" 
	
	*************
	***rama_ci***
	*************
	gen rama_ci=.
	replace rama_ci = 1 if (q9_21>=111 & q9_21<=322) & emp_ci==1
	replace rama_ci = 2 if (q9_21>=510 & q9_21<=990) & emp_ci==1
	replace rama_ci = 3 if (q9_21>=1010 & q9_21<=3320) & emp_ci==1
	replace rama_ci = 4 if (q9_21>=3510 & q9_21<=3900) & emp_ci==1
	replace rama_ci = 5 if (q9_21>=4100 & q9_21<=4390) & emp_ci==1
	replace rama_ci = 6 if ((q9_21>=4510 & q9_21<=4799) | (q9_21>=5510 & q9_21<=5630)) & emp_ci==1
	replace rama_ci = 7 if ((q9_21>=4911 & q9_21<=5320) | (q9_21>=6110 & q9_21<=6190)) & emp_ci==1
	replace rama_ci = 8 if (q9_21>=6411 & q9_21<=8299) & emp_ci==1
	replace rama_ci = 9 if ((q9_21>=5811 & q9_21<=6020) | (q9_21>=6201 & q9_21<=6399) | (q9_21>=8410 & q9_21<=9900)) & emp_ci==1
	label var rama_ci "Rama de actividad"
	label def rama_ci 1"Agricultura, caza, silvicultura y pesca" 2"Explotación de minas y canteras" 3"Industrias manufactureras"
	label def rama_ci 4"Electricidad, gas y agua" 5"Construcción" 6"Comercio, restaurantes y hoteles" 7"Transporte y almacenamiento", add
	label def rama_ci 8"Establecimientos financieros, seguros e inmuebles" 9"Servicios sociales y comunales", add
	label val rama_ci rama_ci

	****************
	***durades_ci***
	****************
	/*      Never worked |      1,093       47.11       47.11
 1 to 3 months ago |        181        7.80       54.91
 4 to 6 months ago |         52        2.24       57.16
7 to 12 months ago |         70        3.02       60.17
Over 12 months ago |        924       39.83      100.00
Se hace con promedios por no tener número exacto*/
	gen durades_ci=. if q9_34==.
	replace durades_ci= . if q9_34==1
	replace durades_ci=(1+3)/2 if q9_34==2
	replace durades_ci=(4+6)/2 if q9_34==3
	replace durades_ci=(7+12)/2 if q9_34==4
	replace durades_ci=(12+12)/2 if q9_34==5	
	label variable durades_ci "Duracion del desempleo en meses"
		
	***************
	*antiguedad_ci*
	***************
	/*AJAM, igual que con durades_ci, se estima un promedio
	
	Years working in |
        main job |      Freq.     Percent        Cum.
-----------------+-----------------------------------
Less than 1 year |        316       11.97       11.97
    1 to 4 years |        677       25.63       37.60
    5 to 9 years |        486       18.40       56.00
10 years or over |      1,162       44.00      100.00
-----------------+-----------------------------------
           Total |      2,641      100.00
	*/
	gen antiguedad_ci=.
	replace antiguedad_ci=0 if q9_22==1
	replace antiguedad_ci=(1+4)/2 if q9_22==2
	replace antiguedad_ci=(5+9)/2 if q9_22==3
	replace antiguedad_ci=(10+10)/2 if q9_22==4
	label var antiguedad_ci "antiguedad laboral (anios) - aproximacion"	

*******************
***categoinac_ci***
*******************
gen categoinac_ci =1 if (q9_28==5 & condocup_ci==3)
replace categoinac_ci = 2 if  (q9_28==3 & condocup_ci==3)
replace categoinac_ci = 3 if  (q9_28==4 & condocup_ci==3)
replace categoinac_ci = 4 if  ((categoinac_ci ~=1 & categoinac_ci ~=2 & categoinac_ci ~=3) & condocup_ci==3)
label var categoinac_ci "Categoría de inactividad"
label define categoinac_ci 1 "jubilados o pensionados" 2 "Estudiantes" 3 "Quehaceres domésticos" 4 "Otros"

*******************
***formal***
*******************
gen formal_ci=(cotizando_ci==1)
label var formal_ci "1=afiliado o cotizante / PEA"



			**************************
			***VARIABLES DE INGRESO***
			**************************

    ***************
	***ylmpri_ci***
	***************
	*Net monthly income and child support from employer, Includes overtime, tips, bonuses.
	egen ylmpri_ci = rsum(q10_02b q10_15) , m
	replace ylmpri_ci =. if q10_02b==. & q10_15==.
	replace ylmpri_ci =. if ylmpri_ci<0
	label var ylmpri_ci "Ingreso laboral monetario actividad principal" 

	*****************
	***nrylmpri_ci***
	*****************
	gen nrylmpri_ci=(ylmpri_ci==. & emp_ci==1)
	label var nrylmpri_ci "Id no respuesta ingreso de la actividad principal"  

	****************
	***ylnmpri_ci***
	****************
	gen ylnmpri_ci=.
	label var ylnmpri_ci "Ingreso laboral NO monetario actividad principal"   

	***************
	***ylmsec_ci***
	***************
	gen ylmsec_ci=q10_04b if q10_04b!=.
	label var ylmsec_ci "Ingreso laboral monetario segunda actividad" 

	****************
	***ylnmsec_ci***
	****************
	gen ylnmsec_ci=.
	label var ylnmsec_ci "Ingreso laboral NO monetario actividad secundaria"
	
**********************************************************************************************
***TCYLMPRI_CH : Identificador de los hogares en donde alguno de los miembros reporta como
*** top-code el ingreso de la actividad principal. .
***********************************************************************************************
gen tcylmpri_ch = .
label var tcylmpri_ch "Id hogar donde algún miembro reporta como top-code el ingr de activ. principal"

***********************************************************************************************
***TCYLMPRI_CI : Identificador de top-code del ingreso de la actividad principal.
***********************************************************************************************
gen tcylmpri_ci = .
label var tcylmpri_ci "Identificador de top-code del ingreso de la actividad principal"

	*****************
	***ylmotros_ci***
	*****************

	gen ylmotros_ci= .
	label var ylmotros_ci "Ingreso laboral monetario de otros trabajos" 

	******************
	***ylnmotros_ci***
	******************
	gen ylnmotros_ci=.
	label var ylnmotros_ci "Ingreso laboral NO monetario de otros trabajos" 

	************
	***ylm_ci***
	************
	egen ylm_ci= rsum(ylmpri_ci ylmsec_ci),m
	replace ylm_ci=. if ylmpri_ci==. &  ylmsec_ci==.
	label var ylm_ci "Ingreso laboral monetario total"  

	*************
	***ylnm_ci***
	*************
	egen ylnm_ci=rsum(ylnmpri_ci ylnmsec_ci), m
	replace ylnm_ci=. if ylnmpri_ci==. &  ylnmsec_ci==.
	label var ylnm_ci "Ingreso laboral NO monetario total"  

	*************
	***ynlm_ci***
	*************
	*ingresos de los ultimos doce meses
	egen ynlm_aux=rsum(q10_18 q10_19 q10_20 q10_21 q10_22 q10_23), miss
	replace ynlm_aux=ynlm_aux/12
	*ingresos de los ultimos 6 meses
	egen ynlm_aux2=rsum(q10_14 q10_15 q10_16 q10_17a), miss
	replace ynlm_aux2=ynlm_aux2/6
	*remesas en dolares y euros, durante los ultimos 6 meses
	g remesasaux=(q10_17b*8.71)+ (q10_17c*4.47)
	replace remesasaux=remesasaux/6	
	replace remesasaux=. if q10_17b==. & q10_17c==.
	egen ynlm_ci=rsum(ynlm_aux remesas q10_07 q10_08 q10_09 q10_10 q10_11 q10_12 q10_13), m
	replace ynlm_ci = . if ynlm_aux==. & remesas==. & q10_07==. & q10_08==. & q10_09==. & q10_10==. & q10_11==. & q10_13==. 
	label var ynlm_ci "Ingreso no laboral monetario"  
	
	**************
	***ynlnm_ci***
	**************
	gen ynlnm_ci=.
	label var ynlnm_ci "Ingreso no laboral no monetario" 

	****************
	***remesas_ci***
	****************
	gen remesas_ci=remesasaux
	label var remesas_ci "Remesas mensuales reportadas por el individuo" 

		************************
		***INGRESOS DEL HOGAR***
		************************

	*****************
	***nrylmpri_ch***
	*****************
	by idh_ch, sort: egen nrylmpri_ch=sum(nrylmpri_ci) if miembros_ci==1
	replace nrylmpri_ch=1 if nrylmpri_ch>0 & nrylmpri_ch<.
	replace nrylmpri_ch=. if nrylmpri_ch==.
	label var nrylmpri_ch "Hogares con algún miembro que no respondió por ingresos"

	************
	***ylm_ch***
	************
	by idh_ch, sort: egen ylm_ch=sum(ylm_ci) if miembros_ci==1
	label var ylm_ch "Ingreso laboral monetario del hogar"

	*************
	***ylnm_ch***
	*************
	by idh_ch, sort: egen ylnm_ch=sum(ylnm_ci) if miembros_ci==1
	label var ylnm_ch "Ingreso laboral no monetario del hogar"

	**************
	***ylmnr_ch***
	**************
	by idh_ch, sort: egen ylmnr_ch=sum(ylm_ci) if miembros_ci==1
	replace ylmnr_ch=. if nrylmpri_ch==1
	label var ylmnr_ch "Ingreso laboral monetario del hogar"

	*************
	***ynlm_ch***
	*************
	by idh_ch, sort: egen ynlm_ch=sum(ynlm_ci) if miembros_ci==1
	label var ynlm_ch "Ingreso no laboral monetario del hogar"

	**************
	***ynlnm_ch***
	**************
	gen ynlnm_ch=.
	label var ynlnm_ch "Ingreso no laboral no monetario del hogar"

	*****************
	***ylmhopri_ci***
	*****************
	gen ylmhopri_ci=ylmpri_ci/(4.3*horaspri_ci)
	replace ylmhopri_ci=. if ylmhopri_ci<=0
	label var ylmhopri_ci "Salario monetario de la actividad principal" 

	**************
	***ylmho_ci***
	**************
	gen ylmho_ci=ylm_ci/(horastot_ci*4.3)
	label var ylmho_ci "Salario monetario de todas las actividades" 

	*****************
	***rentaimp_ch***
	*****************
	gen rentaimp_ch=.
	label var rentaimp_ch "Rentas imputadas del hogar"

	*****************
	***autocons_ci***
	*****************
	gen autocons_ci=.
	label var autocons_ci "Autoconsumo reportado por el individuo"
	
	*****************
	***autocons_ch***
	*****************	
	gen autocons_ch=.
	label var autocons_ch "Autoconsumo reportado por el hogar"

	****************
	***remesas_ch***
	****************
	by idh_ch, sort: egen remesas_ch=sum(remesas_ci) if miembros_ci==1
	label var remesas_ch "Remesas mensuales del hogar"	

			****************************
			***VARIABLES DE EDUCACION***
			****************************
	
	/*
	*************
	***aedu_ci***
	*************
	*Hay una variable anterior, q3_04, pero no pregunta cuál fue el último nivel/anio aprobado, 
	* además de contener cursos en siglas que no pude rescatar del holandés (online).
	*As benchmark del average schooling resultante, http://hdr.undp.org/sites/default/files/Country-Profiles/SUR.pdf
	cap clonevar nivinst = q3_20
	cap clonevar anoinst = q3_23
	replace anoinst= 0 if (nivinst == 1)
	gen aedu_ci = .
	replace aedu_ci = 0          if nivinst==1
	replace aedu_ci = anoinst          if nivinst==2
	replace aedu_ci = anoinst+6  if nivinst==3 | nivinst==4
	replace aedu_ci = anoinst+12  if nivinst==5
	replace aedu_ci = anoinst+17 if nivinst==6
	replace aedu_ci =. if (nivinst==. | anoinst==. | anoinst==99)
	label var aedu_ci "Anios de educacion aprobados"*/
	
	*** modificado por Angela Lopez 04/11/2019
	
	*************
	***aedu_ci***
	*************
	
	cap clonevar asis_niv_inst = q3_04
	cap clonevar asis_ano_inst = q3_08
	cap clonevar noasis_niv_inst = q3_20
	cap clonevar noasis_ano_inst = q3_23
	cap clonevar finalizo = q3_22
	
	cap gen aedu_ci = .
	replace aedu_ci =0 if q3_01 == 2 /* nunca asistió a la escuela */
		
	* PARA LOS QUE ASISTEN 
	

	
		replace aedu_ci = 0                       if asis_niv_inst == 1 & q3_02 == 1  /* prescolar*/
		
		replace aedu_ci = asis_ano_inst-1         if asis_niv_inst == 2 & q3_02 == 1  /* primaria hasta 6 anios */
				
		replace aedu_ci = asis_ano_inst-1+6       if asis_niv_inst == 3 & q3_02 == 1  /* secundaria baja, hasta 4 anios */
		replace aedu_ci = asis_ano_inst-1+6+4     if asis_niv_inst == 4 & q3_02 == 1  /* secundaria alta, hasta 2 anios */
		replace aedu_ci = asis_ano_inst-1+6+4+2   if asis_niv_inst == 5 & q3_02 == 1  /* Universitaria, hasta 5 anios con maestria 4 normal */
		replace aedu_ci = asis_ano_inst-1+6+4+2+4 if asis_niv_inst == 6 & q3_02 == 1  /* maestria doctorado, hasta 5 anios */
	
	
	* PARA LOS QUE NO ASISTEN PERO ALGUNA VEZ ASISTIERON
	

	
		replace aedu_ci = 0                       if noasis_niv_inst == 1 & q3_02 == 2 & q3_01 == 1 /* prescolar-ninguno*/
		
		replace aedu_ci = noasis_ano_inst         if noasis_niv_inst == 2 & finalizo ==2 & q3_02 == 2 & q3_01 == 1 /* primaria si no finalizó pongo el grado que finalizó */
		replace aedu_ci = 6				          if noasis_niv_inst == 2 & finalizo ==1 & q3_02 == 2 & q3_01 == 1 /* primaria si finalizó asumo 6 anios */

		replace aedu_ci = noasis_ano_inst+6       if noasis_niv_inst == 3 & finalizo ==2 & q3_02 == 2 & q3_01 == 1 /* secundaria baja, hasta 4 anios */
		replace aedu_ci = 6+4                     if noasis_niv_inst == 3 & finalizo ==1 & q3_02 == 2 & q3_01 == 1 /* secundaria baja, hasta 4 anios */
		
		replace aedu_ci = noasis_ano_inst+6+4     if noasis_niv_inst == 4 & finalizo ==2 & q3_02 == 2 & q3_01 == 1 /* secundaria alta, hasta 3 anios */
		replace aedu_ci = 6+4+2                   if noasis_niv_inst == 4 & finalizo ==1 & q3_02 == 2 & q3_01 == 1 /* secundaria alta, hasta 3 anios */

		replace aedu_ci = noasis_ano_inst+6+4+2   if noasis_niv_inst == 5 & finalizo ==2  & q3_02 == 2 & q3_01 == 1 /* Universitaria, hasta 5 anios con maestria 4 normal */
		replace aedu_ci = 4+6+4+2                 if noasis_niv_inst == 5 & finalizo ==1  & q3_02 == 2 & q3_01 == 1
		
		replace aedu_ci = noasis_ano_inst+6+4+2+4 if noasis_niv_inst == 6 & finalizo ==2  & q3_02 == 2 & q3_01 == 1/* maestria doctorado, hasta 5 anios */
		replace aedu_ci = 3+6+4+2+4 if noasis_niv_inst == 6 & finalizo ==2 & q3_02 == 2 & q3_01 == 1/* como no se sabe si es maestría o doctorado asumimos 3 +*/
		
	    replace aedu_ci = 0 if aedu_ci ==-1

	**************
	***eduno_ci***
	**************
	gen eduno_ci=0
	replace eduno_ci=1 if aedu_ci==0
	label variable eduno_ci "Sin educacion"

	**************
	***edupi_ci***
	**************
	gen edupi_ci=0
	replace edupi_ci=1 if (aedu_ci>=1 & aedu_ci<6) 
	label variable edupi_ci "Primaria incompleta"

	**************
	***edupc_ci***
	**************
	gen edupc_ci=0
	replace edupc_ci=1 if aedu_ci==6 
	label variable edupc_ci "Primaria completa"

	**************
	***edusi_ci***
	**************
	gen edusi_ci=0
	replace edusi_ci=1 if (aedu_ci>=7 & aedu_ci<12) 
	label variable edusi_ci "Secundaria incompleta"

	**************
	***edusc_ci***
	**************
	gen edusc_ci=0
	replace edusc_ci=1 if aedu_ci==12 
	label variable edusc_ci "Secundaria completa"

	**************
	***eduui_ci***
	**************
	gen eduui_ci=(aedu_ci>12 & aedu_ci<17)
	label variable eduui_ci "Superior incompleto"

	***************
	***eduuc_ci***
	***************
	gen byte eduuc_ci= (aedu_ci>=17)
	label variable eduuc_ci "Superior completo"

	***************
	***edus1i_ci***
	***************
	gen edus1i_ci=0
	replace edus1i_ci=1 if (aedu_ci>6 & aedu_ci<9)
	label variable edus1i_ci "1er ciclo de la secundaria incompleto"

	***************
	***edus1c_ci***
	***************
	gen edus1c_ci=0
	replace edus1c_ci=1 if aedu_ci==9
	label variable edus1c_ci "1er ciclo de la secundaria completo"

	***************
	***edus2i_ci***
	***************
	gen edus2i_ci=0
	replace edus2i_ci=1 if aedu_ci>9 & aedu_ci<12
	label variable edus2i_ci "2do ciclo de la secundaria incompleto"

	***************
	***edus2c_ci***
	***************
	gen edus2c_ci=0
	replace edus2c_ci=1 if aedu_ci==12
	label variable edus2c_ci "2do ciclo de la secundaria completo"

	local var = "eduno edupi edupc edusi edusc edusc eduui eduuc edus1i edus1c edus2i edus2c"
	foreach x of local var {
	replace `x'_ci=. if aedu_ci==.
	}

	***************
	***edupre_ci***
	***************
	gen edupre_ci=.
	label variable edupre_ci "Educacion preescolar"
	
	***************
	***asispre_ci**
	***************
	g asispre_ci=.
	replace asispre_ci=1 if q3_04==1
	recode asispre_ci (.=0)
	la var asispre_ci "Asiste a educacion prescolar"
	
	**************
	***eduac_ci***
	**************
	gen eduac_ci=.
	label variable eduac_ci "Superior universitario vs superior no universitario"

	***************
	***asiste_ci***
	***************
	gen asiste_ci=(q3_02==1)
	replace asiste_ci=. if q3_01==.
	label variable asiste_ci "Asiste actualmente a la escuela"

	**************
	***pqnoasis_ci***
	**************
	gen pqnoasis_ci=.
	label val pqnoasis_ci pqnoasis_ci
	/*Variable se refiere a por qué no asistió durante ocasiones no-regulares.
	label var pqnoasis_ci "Razones para no asistir a la escuela"
	label def pqnoasis_ci 1"edad" 2"terminó sus estudios" 3"falta recursos económicos" 4"fracaso escolar"
	label def pqnoasis_ci 5"por trabajo" 6"por asistir a nivelación SENESCYT" 7"enfermedad o discapacidad" 8"quehaceres del hogar", add
	label def pqnoasis_ci 9"familia no permite" 10"no hay establecimientos educativos" 11"no está interesado", add
	label def pqnoasis_ci 12"por embarazo" 13"por falta de cupo" 14"Temor a los compañeros" 15"Cuidar a los hijos" 16"Otra razón" , add
	
	**************
    *pqnoasis1_ci*
    **************
	*/
    g       pqnoasis1_ci =.
	label value  pqnoasis1_ci pqnoasis1_ci
	/*Idem al comentario anterior
	**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**
    
    replace pqnoasis1_ci = 2 if p09==5
    replace pqnoasis1_ci = 3 if p09==7  | p09==9
    replace pqnoasis1_ci = 4 if p09==11
    replace pqnoasis1_ci = 5 if p09==8  | p09==12 | p09==15
    replace pqnoasis1_ci = 6 if p09==2
    replace pqnoasis1_ci = 7 if p09==1 
    replace pqnoasis1_ci = 8 if p09==10
    replace pqnoasis1_ci = 9 if p09==4  | p09==6 | p09==13 | p09==14 | p09==16

    label define pqnoasis1_ci 1 "Problemas económicos" 2 "Por trabajo" 3 "Problemas familiares o de salud" 4 "Falta de interés" 5	"Quehaceres domésticos/embarazo/cuidado de niños/as" 6 "Terminó sus estudios" 7	"Edad" 8 "Problemas de acceso"  9 "Otros"
    
	*/
	***************
	***repite_ci***
	***************
	gen repite_ci=.
	label var repite_ci "Ha repetido al menos un grado"

	******************
	***repiteult_ci***
	******************
	gen repiteult_ci=.
	label var repiteult "Ha repetido el último grado"

	***************
	***edupub_ci***
	***************
	gen edupub_ci=(q3_03==1) if q3_03!=.
	label var edupub_ci "Asiste a un centro de ensenanza público"

	****************
	***tecnica_ci **
	****************
	gen tecnica_ci=.
	label var tecnica_ci "=1 formacion terciaria tecnica"

	
	
	
	**********************************
	**** VARIABLES DE LA VIVIENDA ****
	**********************************
	
	****************
	***aguared_ch***
	****************
	generat aguared_ch=(q13_15==1 |q13_15==2 |q13_15==3)
	replace aguared_ch=. if q13_15==.
	label var aguared_ch "Acceso a fuente de agua por red"

	*****************
	***aguadist_ch***
	*****************
	gen aguadist_ch=.
	replace aguadist_ch=1 if q13_15==1
	replace aguadist_ch=2 if q13_15==2
	replace aguadist_ch=3 if q13_15==3
	replace aguadist_ch=. if q13_15==.
	label var aguadist_ch "Ubicación de la principal fuente de agua"
	label def aguadist_ch 1"Dentro de la vivienda" 2"Fuera de la vivienda pero en el terreno"
	label def aguadist_ch 3"Fuera de la vivienda y del terreno", add
	label val aguadist_ch aguadist_ch

	*****************
	***aguamala_ch***
	*****************
	gen aguamala_ch=(q13_15==6)
	replace aguamala_ch=. if q13_15==.
	label var aguamala_ch "Agua unimproved según MDG" 
	
	*****************
	***aguamide_ch***
	*****************
	gen aguamide_ch=.
	label var aguamide_ch "Usan medidor para pagar consumo de agua"

	************
	***luz_ch***
	************
	gen luz_ch=(q13_17>=1 & q13_17<=3)
	label var luz_ch  "La principal fuente de iluminación es electricidad"
	
	****************
	***luzmide_ch***
	****************
	gen luzmide_ch=.
	label var luzmide_ch "Usan medidor para pagar consumo de electricidad"

	****************
	***combust_ch***
	****************
	gen combust_ch=0
	replace combust_ch=1 if  q13_13>=1 & q13_13<=4 
	label var combust_ch "Principal combustible gas o electricidad"  
	
	*************
	***bano_ch***
	*************
	gen bano_ch=1
	replace bano_ch=0 if q13_14==1 | q13_14==2
	replace bano_ch=. if q13_14==.
	label var bano_ch "El hogar tiene servicio sanitario"
	
	***************
	***banoex_ch***
	***************
	gen banoex_ch=.
	label var banoex_ch "El servicio sanitario es exclusivo del hogar"

	*************
	***des1_ch***
	*************
	gen des1_ch=.
	replace des1_ch=0 if bano_ch==5
	replace des1_ch=1 if q13_14==1 | q13_14==2
	replace des1_ch=2 if q13_14==3
	replace des1_ch=3 if q13_14==4 
	replace des1_ch=. if q13_14==6
	label var des1_ch "Tipo de desague según unimproved de MDG"
	label def des1_ch 0"No tiene servicio sanitario" 1"Conectado a red general o cámara séptica"
	label def des1_ch 2"Letrina o conectado a pozo ciego" 3"Desemboca en río o calle", add
	label val des1_ch des1_ch
	
	*************
	***des2_ch***
	*************
	gen des2_ch=.
	replace des2_ch=0 if bano_ch==0
	replace des2_ch=1 if q13_14==1 | q13_14==2
	label var des2_ch "Tipo de desague sin incluir definición MDG"
	label def des2_ch 0"No tiene servicio sanitario" 1"Conectado a red general, cámara séptica, pozo o letrina"
	*label def des2_ch 2"Cualquier otro caso", add
	label val des2_ch des2_ch
		
	*************
	***piso_ch***
	*************
	gen piso_ch=.
	replace piso_ch=1	if q13_10==1 |q13_10==2| q13_10==3 | q13_10==4 | q13_10==5
	replace piso_ch=2 	if q13_10==6 	
	replace piso_ch=. 	if q13_10==.
	label var piso_ch "Materiales de construcción del piso"  
	label def piso_ch 0"Piso de tierra" 1"Materiales permanentes" 2"Otros materiales"
	label val piso_ch piso_ch
		
	**************
	***pared_ch***
	**************
	gen pared_ch=0 		if q13_08==9 | q13_08==8
	replace pared_ch=1	if q13_08>=1 & q13_08<=7
	label var pared_ch "Materiales de construcción de las paredes"
	label def pared_ch 0"No permanentes" 1"Permanentes"
	label val pared_ch pared_ch
	
	**************
	***techo_ch***
	**************
	gen techo_ch=0 		if  q13_11==6 |q13_11==7
	replace techo_ch=1	if  q13_11>=1 & q13_11<=5
	label var techo_ch "Materiales de construcción del techo"
	label def techo_ch 0"No permanentes" 1"Permanentes"
	label val techo_ch techo_ch
	
	**************
	***resid_ch***
	**************
	gen resid_ch =.
	label val resid_ch resid_ch
	/*No existe variable de residuos
	replace resid_ch=1 if q13_17==4
	replace resid_ch=2 if q13_17==3
	replace resid_ch=3 if q13_17==5
	replace resid_ch=. if q13_17==.
	label var resid_ch "Método de eliminación de residuos"
	label def resid_ch 0"Recolección pública o privada" 1"Quemados o enterrados"
	label def resid_ch 2"Tirados a un espacio abierto" 3"Otros", add
	*/
	
    *********************
    ***aguamejorada_ch***
    *********************
	g       aguamejorada_ch = 1 if (q13_15 >=1 & q13_15 <=3) | q13_15 ==5  | q13_15 ==7
	replace aguamejorada_ch = 0 if  q13_15 ==4  | q13_15 ==6
    *********************
    ***banomejorado_ch***
    *********************
	g       banomejorado_ch = 1 if (q13_14>=1 & q13_14 <=2)
	replace banomejorado_ch = 0 if  q13_14>=3 
		
	*************
	***dorm_ch***
	*************
	*Dado que hay hogares que reportan 0 habitaciones exclusivas para dormir, pues la vivienda está constituída por
	*un sólo ambiente, a estos hogares se les imputa 1 habitación. A los hogares que dicen no tener cuartos exclusivos 
	*para dormir, pero que viven en viviendas de 2 o más habitaciones se les asigna missing

	gen dorm_ch=q13_20
	replace dorm_ch=1 if q13_20==0 & q13_18==1
	replace dorm_ch=. if q13_20==0 & q13_18>1
	label var dorm_ch "Habitaciones para dormir"
	
	****************
	***cuartos_ch***
	****************
	gen cuartos_ch=q13_18 if q13_18<99
	label var cuartos_ch "Habitaciones en el hogar"
		
	***************
	***cocina_ch***
	***************
	gen cocina_ch=.
	label var cocina_ch "Cuarto separado y exclusivo para cocinar"

	**************
	***telef_ch***
	**************
	gen telef_ch=0
	replace telef_ch=1 if q13_23f>=1
	replace telef_ch=. if q13_23f==.
	label var telef_ch "El hogar tiene servicio telefónico fijo"
	
	***************
	***refrig_ch***
	***************
	gen refrig_ch=0
	replace refrig_ch=1 if  q16_01>=1
	replace refrig_ch=. if  q16_01==.
	label var refrig_ch "El hogar posee refrigerador o heladera"
		
	**************
	***freez_ch***
	**************
	gen freez_ch=0
	replace freez_ch=1 if  q16_03>=1
	replace freez_ch=. if  q16_03==.
	label var freez_ch "El hogar posee congelador"

	*************
	***auto_ch***
	*************
	gen auto_ch=0
	replace auto_ch=1 if q13_23i>=1
	replace auto_ch=. if q13_23i==.
	label var auto_ch "El hogar posee automovil particular"

	**************
	***compu_ch***
	**************
	gen compu_ch=0
	replace compu_ch=1 if q16_17a>=1 | q16_17b>=1 
	replace compu_ch=. if q16_17a==. & q16_17b==.
	label var compu_ch "El hogar posee computador"
	
	*****************
	***internet_ch***
	*****************
	gen internet_ch=. 
	label var internet_ch "El hogar posee conexión a Internet"

	************
	***cel_ch***
	************
	gen cel_ch=0
	replace cel_ch=1 if q13_23g>=1 
	replace cel_ch=. if q13_23g==. 
	label var cel_ch "El hogar tiene servicio telefonico celular"
	
	**************
	***vivi1_ch***
	**************
	gen vivi1_ch=1 		if q13_01==1
	replace vivi1_ch=2 	if q13_01==2
	replace vivi1_ch=3 	if q13_01>=3 & q13_01<=5
	replace vivi1_ch=. 	if q13_01==.
	label var vivi1_ch "Tipo de vivienda en la que reside el hogar"
	label def vivi1_ch 1"Casa" 2"Departamento" 3"Otros"
	label val vivi1_ch vivi1_ch
		
	**************
	***vivi2_ch***
	**************
	gen vivi2_ch=0
	replace vivi2_ch=1 if q13_01==1 | q13_01==2
	replace vivi2_ch=. if q13_01==.
	label var vivi2_ch "La vivienda es casa o departamento"
	
	*****************
	***viviprop_ch***
	*****************
	gen viviprop_ch=.
	replace viviprop_ch = 0 if q13_02==4 | q13_02==5
	replace viviprop_ch = 1 if q13_02==2 | q13_02==3
	replace viviprop_ch = 2 if q13_02==1
	replace viviprop_ch = 3 if q13_02>=7 & q13_02<.
	label var viviprop_ch "Propiedad de la vivienda"
	label def viviprop_ch 0"Alquilada" 1"Propia y totalmente pagada" 2"Propia y en proceso de pago"
	label def viviprop_ch 3"Ocupada (propia de facto)", add
	label val viviprop_ch viviprop_ch
	
	****************
	***vivitit_ch***
	****************
	gen vivitit_ch=.
	label var vivitit_ch "El hogar posee un título de propiedad"

	****************
	***vivialq_ch***
	****************
	gen vivialq_ch=.
	label var vivialq_ch "Alquiler mensual"

	*******************
	***vivialqimp_ch***
	*******************
	gen vivialqimp_ch=.
	label var vivialqimp_ch "Alquiler mensual imputado"
	
	*******************
	*** benefdes_ci ***
	*******************

	g benefdes_ci=.
	label var benefdes_ci "=1 si tiene seguro de desempleo"

	*******************
	*** ybenefdes_ci***
	*******************
	g ybenefdes_ci=.
	label var ybenefdes_ci "Monto de seguro de desempleo"


******************************
*** VARIABLES DE MIGRACION ***
******************************

* Variables incluidas por SCL/MIG Fernando Morales

	*******************
	*** migrante_ci ***
	*******************
	
	gen migrante_ci=(q2_01!=1) if q2_01!=.
	label var migrante_ci "=1 si es migrante"
	
	**********************
	*** migantiguo5_ci ***
	**********************
	
	gen migantiguo5_ci=(migrante_ci==1 & q2_07>4) if migrante_ci!=. & !inrange(edad_ci,0,4)
	replace migantiguo5_ci=. if (q2_07==. & q2_03==1)
	label var migantiguo5_ci "=1 si es migrante antiguo (5 anos o mas)"
		
	**********************
	*** migrantelac_ci ***
	**********************
	
	gen migrantelac_ci=(migrante_ci==1 & (inlist(q2_01,3,4) | inlist(q2_02,"Argentinie","Haiti","Haitie","Colombia","JAMAICA","Jamaica") | inlist(q2_02,"dominicaanse republi","Trinidad","VENEZUELA","haiti","venezuela"))) if migrante_ci!=. 
	replace migrantelac_ci=. if (q2_01==7 & mi(q2_02))
	label var migrantelac_ci "=1 si es migrante proveniente de un pais LAC"

	
/*_____________________________________________________________________________________________________*/
* Asignación de etiquetas e inserción de variables externas: tipo de cambio, Indice de Precios al 
* Consumidor (2011=100), Paridad de Poder Adquisitivo (PPA 2011),  líneas de pobreza
/*_____________________________________________________________________________________________________*/


do "$ruta\harmonized\_DOCS\\Labels&ExternalVars_Harmonized_DataBank.do"

/*_____________________________________________________________________________________________________*/
* Verificación de que se encuentren todas las variables armonizadas 
/*_____________________________________________________________________________________________________*/

order region_BID_c region_c pais_c anio_c mes_c zona_c factor_ch	idh_ch	idp_ci	factor_ci upm_ci estrato_ci sexo_ci edad_ci ///
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
vivi1_ch vivi2_ch viviprop_ch vivitit_ch vivialq_ch	vivialqimp_ch migrante_ci migantiguo5_ci migrantelac_ci, first

/*Homologar nombre del identificador de ocupaciones (isco, ciuo, etc.) y de industrias y dejarlo en base armonizada 
para análisis de trends (en el marco de estudios sobre el futuro del trabajo)*/
rename q9_19 codocupa
rename q9_21 codindustria

compress

saveold "`base_out'", version(12) replace


log close


