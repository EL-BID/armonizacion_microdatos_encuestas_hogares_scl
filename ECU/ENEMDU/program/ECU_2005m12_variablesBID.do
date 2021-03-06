
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

local PAIS ECU
local ENCUESTA ENEMDU
local ANO "2005"
local ronda m12 


local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
*local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\\`PAIS'_`ANO'`ronda'.dta"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
   


capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Ecuador
Encuesta: ENEMDU
Round: m12
Autores: Yesenia Loayza (yessenial@iadb.org / desloay@hotmail.com)
Última versión: Mayra Sáenz E-mail: mayras@iadb.org / saenzmayra.a@gmail.com
Fecha última modificación: 24 de Septiembre de 2013

							SCL/LMK - IADB
****************************************************************************/
/***************************************************************************
Detalle de procesamientos o modificaciones anteriores:
*2014, 01 MLO cambio del limite de edad de condocup_ci a 5+
****************************************************************************/


use `base_in', clear

		*************************
		***VARIABLES DEL HOGAR***
		*************************
  ************
* Region_BID *
************
gen region_BID_c=.
replace region_BID_c=3 
label var region_BID_c "Regiones BID"
label define region_BID_c 1 "Centroamérica_(CID)" 2 "Caribe_(CCB)" 3 "Andinos_(CAN)" 4 "Cono_Sur_(CSC)"
label value region_BID_c region_BID_c

*** Region_c *********
**********************
destring ciudad, replace
gen region_c=int(ciudad/10000)
	recode  region_c (14/16=89) (19/22=89)
	label define region_c ///
	1 "Azuay" ///
	2 "Bolívar" ///
	3 "Cañar" ///
	4 "Carchi" /// 
	5 "Cotopaxi" ///
	6 "Chimborazo" ///
	7 "El Oro" ///
	8 "Esmeraldas" ///
	9 "Guayas" ///
	10 "Imbabura" ///
	11 "Loja" ///
	12 "Los Ríos" ///
	13 "Manabí" ///
	17 "Pichincha" ///
	18 "Tungurahua" ///
	89 "Amazonia" 
   label value region_c region_c

	***************
	***factor_ch***
	***************
	gen factor_ch=fexp
	label variable factor_ch "Factor de expansion del hogar"
	
	*************
	****idh_ch***
	*************
	sort ciudad zona sector panelm vivienda hogar
	egen idh_ch = group(ciudad zona sector panelm vivienda hogar)
	label variable idh_ch "ID del hogar"
	
	*************
	****idh_ci***
	*************
	gen idp_ci=persona
	label variable idp_ci "ID de la persona en el hogar"
	
	*************
	****zona_c***
	*************
	gen zona_c=1 		if area==1
	replace zona_c=0 	if area==2
	label variable zona_c "Zona del pais"
	label define zona_c 1 "Urbana" 0 "Rural"
	label value zona_c zona_c

	*************
	****pais_c***
	*************
	gen str3 pais_c="ECU"
	label variable pais_c "Pais"

	************
	***anio_c***
	************
	
	gen anio_c=2005
	label variable anio_c "Anio de la encuesta" 
	
	***********
	***mes_c***
	***********
	gen mes_c=12
	label var mes_c "Mes de la encuesta" 
	
	*****************
	***relacion_ci***
	*****************
	
	generat relacion_ci=1 if reljefe==1
	replace relacion_ci=2 if reljefe==2
	replace relacion_ci=3 if reljefe==3
	replace relacion_ci=4 if reljefe>=4 & reljefe<=7
	replace relacion_ci=5 if reljefe==9
	replace relacion_ci=6 if reljefe==8
	label variable relacion_ci "Relacion con el jefe del hogar"
	label define relacion_ci 1 "Jefe/a" 2 "Esposo/a" 3 "Hijo/a" ///
	4 "Otros parientes" 5 "Otros no parientes" 6 "Empleado/a domestico/a"
	label value relacion_ci relacion_ci


			****************************
			***VARIABLES DEMOGRAFICAS***
			****************************
	
	***************
	***factor_ci***
	***************
	gen factor_ci=fexp
	label variable factor_ci "Factor de expansion del individuo"
	
	*************
	***sexo_ci***
	*************
	gen sexo_ci=sexo
	label var sexo_ci "Sexo del individuo" 
	label def sexo_ci 1"Masculino" 2"Femenino" 
	label val sexo_ci sexo_ci

	**********
	***edad***
	**********
	gen edad_ci=edad if edad<99
	label variable edad_ci "Edad del individuo"
	
	**************
	***civil_ci***
	**************
	gen civil_ci=.
	replace civil_ci= 1 if pe4a==6
	replace civil_ci= 2 if pe4a==1 | pe4a==5
	replace civil_ci= 3 if pe4a==2 | pe4a==3
	replace civil_ci= 4 if pe4a==4
	label var civil_ci "Estado civil" 
	label def civil_ci 1"Soltero"  2"Union formal o informal" 3"Divorciado o separado" 4"Viudo" 
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

	
	
	
         ******************************
         *** VARIABLES DE DIVERSIDAD **
         ******************************
*Nathalia Maya & Antonella Pereira
*Feb 2021	

		***************
	***afroind_ci***
	***************										
**Pregunta- pe13 (1 indígena, 2 blanco, 3 mestizo, 4 negro, 5 mulato, 6 otro) 
gen afroind_ci=. 
replace afroind_ci=1 if pe13 == 1
replace afroind_ci=2 if pe13 == 4 | pe13 == 5
replace afroind_ci=3 if pe13 == 2 | pe13 == 3 | pe13 ==6 
replace afroind_ci=. if pe13==. 
replace afroind_ci=9 if pe13==. & edad_ci<5

	***************
	***afroind_ch***
	***************
gen afroind_jefe= afroind_ci if relacion_ci==1
egen afroind_ch  = min(afroind_jefe), by(idh_ch) 
drop afroind_jefe

	*******************
	***afroind_ano_c***
	*******************
gen afroind_ano_c=2004

	*******************
	***dis_ci***
	*******************
gen dis_ci=. 

	*******************
	***dis_ch***
	*******************
gen dis_ch=. 
	
			***********************************
			***VARIABLES DEL MERCADO LABORAL***
			***********************************
	****************
	****condocup_ci*
	****************
/*2014, 01 MLO cambio del limite de edad de condocup_ci a 5+
gen gesbuscatra = (pe30a<=7 | pe30b<=7)
gen condocup_ci=.
replace condocup_ci=1 if condact>=0 & condact<=4
replace condocup_ci=2 if trabajo==2 & actayuda==11 & aunotra==2 & gesbuscatra==1 /*genero desempleo abierto*/
replace condocup_ci=3 if condocup_ci!=1 & condocup_ci!=2
*replace condocup_ci=4 if edad_ci<10
replace condocup_ci=4 if edad<5
*/
	*MGD-> la variable condact considera a menores solamente de 0 a 5;
	* sin embargo, se genera con las variables originales para corregir el desempleo oculto. 06/04/2014
	generat condocup_ci=.
	replace condocup_ci=1 if trabajo==1 | actayuda<11 | aunotra==1 
	replace condocup_ci=2 if (trabajo==2 | actayuda==11 | aunotra==2 ) & (bustrasa==1 | bustrama==1)
	replace condocup_ci=3 if condocup_ci!=1 & condocup_ci!=2
	replace condocup_ci=4 if edad_ci<5
	label define condocup_ci 1 "ocupados" 2 "desocupados" 3 "inactivos" 4 "menor de PET"
	label value condocup_ci condocup_ci
	label var condocup_ci "Condicion de ocupacion utilizando definicion del pais"
	

****************
*afiliado_ci****
****************
gen afiliado_ci=(iess>=2 & iess<=4) /*todas personas*/	
replace afiliado_ci=. if iess==.
label var afiliado_ci "Afiliado a la Seguridad Social"
*Nota: seguridad social comprende solo los que en el futuro me ofrecen una pension.

****************
*cotizando_ci***
****************
gen cotizando_ci=0     if condocup_ci==1 | condocup_ci==2 
replace cotizando_ci=1 if (pe42f==1) & cotizando_ci==0 /*Emplead@s y asalariad@s, ocupados y desoc*/
label var cotizando_ci "Cotizante a la Seguridad Social"
*Nota: solo seguro social publico, con el cual tenga derecho a pensiones en el futuro.


****************
*instpen_ci*****
****************
gen instpen_ci=.
label var instpen_ci "Institucion proveedora de la pension - variable original de cada pais" 
gen tipopen_ci=.
********************
*** instcot_ci *****
********************
gen instcot_ci=iess /* a todas las personas. Año 2000 10 años y mas de edad*/
label var instcot_ci "institución a la cual cotiza"

*************
*tamemp_ci***
*************
/*
gen tamemp_ci=npertra
replace tamemp_ci=. if (npertra>=99 & npertra!=.)
replace tamemp_ci=100 if pertrabn==2
label define tamemp_ci 100 "100 y más empleados"
label value tamemp_ci tamemp_ci 
label var tamemp_ci "# empleados en la empresa de la actividad principal"*/
*************
*tamemp_ci
*************
*Ecuador Pequeña 1 a 5 Mediana 6 a 50 Grande Más de 50
*1 = menos de 100
*2 = más de 100

gen tamemp_ci=.
replace tamemp_ci=1 if pertrabn==1 & (npertra>=1 & npertra<=5)
replace tamemp_ci=2 if npertra>=6 & npertra<=50
replace tamemp_ci=3 if pertrabn==2 | (npertra>50 & npertra!=.) 
*2014, 01 modificacion MLO
*replace tamemp_ci=. if (npertra>=99)
label var tamemp_ci "# empleados en la empresa segun rangos"
label define tamemp_ci 1 "Pequeña" 2 "Mediana" 3 "Grande"
label value tamemp_ci tamemp_ci

*************
**pension_ci*
*************
gen pension_ci=0 
replace pension_ci=1 if (pe69a==1) /* A todas las per mayores de cinco*/
replace pension_ci=. if pe69a==.
label var pension_ci "1=Recibe pension contributiva"

*************
*ypen_ci*
*************
gen ypen_ci=pe69b if pension_ci==1
replace ypen_ci=. if pe69b==999999 
label var ypen_ci "Valor de la pension contributiva"

***************
*pensionsub_ci*
***************
gen pensionsub_ci=0 
replace pensionsub_ci=1 if pe72==1
label var pensionsub_ci "1=recibe pension subsidiada / no contributiva"

*****************
**  ypensub_ci  *
*****************
gen ypensub_ci=pe73 if pensionsub_ci==1
replace ypensub_ci=. if ypensub_ci==999999
label var ypensub_ci "Valor de la pension subsidiada / no contributiva"

*************
*cesante_ci* 
*************
generat cesante_ci=0 if condocup_ci==2
replace cesante_ci=1 if trabant==1 & condocup_ci==2
label var cesante_ci "Desocupado - definicion oficial del pais"

*********
*lp_ci***
*********
gen lp_ci =55.506195
label var lp_ci "Linea de pobreza oficial del pais"

***********
*lpe_ci ***
***********
gen lpe_ci = 31.280207
label var lpe_ci "Linea de indigencia oficial del pais"


	/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/


*************
**salmm_ci***
*************
gen salmm_ci= 150
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
replace pea_ci=1 if emp_ci==1 |desemp_ci==1
label var pea_ci "Población Económicamente Activa"


	*****************
	***desalent_ci***
	*****************
	gen desalent_ci=(motnobus==2 | motnobus==3)
	label var desalent_ci "Trabajadores desalentados"
	
	
	*****************
	***horaspri_ci***
	*****************
	gen horaspri_ci=hortrahp  
	replace horaspri_ci=. if emp_ci==0 | hortrahp==999
	label var horaspri_ci "Horas trabajadas semanalmente en el trabajo principal"

	*****************
	***horastot_ci***
	*****************
	for var hortrahp: recode X (999=.) 
	for var hortrahs hortraho: recode X (99=.) (999=.) 
	egen horastot_ci=rsum(hortrahp hortrahs hortraho) if emp_ci==1 
	replace horastot_ci=. if hortrahp==. & hortrahs==. & hortraho==.
	replace horastot_ci=. if emp_ci==0
	label var horastot_ci "Horas trabajadas semanalmente en todos los empleos"
	
	***************
	***subemp_ci***
	***************
	*Modificacion MGD 06/18/2014 solo horas de actividad principal.
	gen subemp_ci=0
	replace subemp_ci=1 if (hormas==1) & horaspri_ci<=30 & emp_ci==1
	label var subemp_ci "Personas en subempleo por horas"

	*******************
	***tiempoparc_ci***
	*******************
	
	* MGR: Modifico serie en base a correcciones Laura Castrillo: se debe utilizar horaspri en lugar de horastot como había sido generada antes
	gen tiempoparc_ci=((horaspri_ci>=1 & horaspri_ci<30) & hormas==2 & emp_ci==1)
	replace tiempoparc_ci=. if emp_ci==0
	label var tiempoparc_c "Personas que trabajan medio tiempo" 

	******************
	***categopri_ci***
	******************
	
	gen categopri_ci=.
	replace categopri_ci=1 if catetrab==6
	replace categopri_ci=2 if catetrab==7 
	replace categopri_ci=3 if (catetrab>=1 & catetrab<=5) | catetrab==9
	replace categopri_ci=4 if catetrab==8
	replace categopri_ci=. if emp_ci==0
	label define categopri_ci 1"Patron" 2"Cuenta propia" 
	label define categopri_ci 3"Empleado" 4" No remunerado" , add
	label value categopri_ci categopri_ci
	label variable categopri_ci "Categoria ocupacional"

	******************
	***categosec_ci***
	******************
	gen categosec_ci=.
	replace categosec_ci=1 if cates==6
	replace categosec_ci=2 if cates==7	
	replace categosec_ci=3 if (cates>=1 & cates<=5) | cates==9
	replace categosec_ci=4 if cates==8	
	replace categosec_ci=. if emp_ci==0
	label define categosec_ci 1"Patron" 2"Cuenta propia" 0"Otro"
	label define categosec_ci 3"Empleado" 4" No remunerado" , add
	label value categosec_ci categosec_ci
	label variable categosec_ci "Categoria ocupacional en la segunda actividad"

*****************
*tipocontrato_ci*
*****************
generat tipocontrato_ci=. /* Solo disponible para asalariados*/
replace tipocontrato_ci=1 if (estabil>=1 & estabil<=3) & categopri_ci==3
replace tipocontrato_ci=2 if (estabil==4 | estabil==5) & categopri_ci==3
label var tipocontrato_ci "Tipo de contrato segun su duracion en act principal"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci

	*****************
	***nempleos_ci***
	*****************
	generat nempleos_ci=1 if numtrab==1
	replace nempleos_ci=2 if numtrab==2
	replace nempleos_ci=. if emp_ci!=1
	label var nempleos_ci "Número de empleos" 
	label define nempleos_ci 1 "Un empleo" 2 "Mas de un empleo"
	label value nempleos_ci nempleos_ci
/*
	*****************
	***firmapeq_ci***
	*****************
	gen firmapeq_ci=.
	replace firmapeq_ci=1 if (npertra>=1 & npertra<=5)
	replace firmapeq_ci=0 if (npertra>=6 & npertra<99) | (pertrabn==2)
	label var firmapeq_ci "Trabajadores informales"
	label def firmapeq_ci 1"5 o menos trabajadores" 0"Mas de 5 trabajadores"
	label val firmapeq_ci firmapeq_ci
*/		
	*****************
	***spublico_ci***
	*****************
	gen spublico_ci=(catetrab==1 & emp_ci==1)
	replace spublico_ci=. if emp_ci==.
	label var spublico_ci "Personas que trabajan en el sector público"

	**************
	***ocupa_ci***
	**************
rename grupo p41
	* Modificacion MGD 07/29/2014: se utiliza CIUO-88.
	generat ocupa_ci=.
	replace ocupa_ci=1 if (p41>=2110 & p41<=3480) & emp_ci==1
	replace ocupa_ci=2 if (p41>=1110 & p41<=1319) & emp_ci==1
	replace ocupa_ci=3 if (p41>=4110 & p41<=4223) & emp_ci==1
	replace ocupa_ci=4 if ((p41>=5200 & p41<=5259) | (p41>=9100 & p41<=9113)) & emp_ci==1
	replace ocupa_ci=5 if ((p41>=5110 & p41<=5179) | (p41>=9120 & p41<=9162))  & emp_ci==1
	replace ocupa_ci=6 if ((p41>=6110 & p41<=6210) | (p41>=9210 & p41<=9213)) & emp_ci==1
	replace ocupa_ci=7 if ((p41>=7110 & p41<=8350) | (p41>=9300 & p41<=9333)) & emp_ci==1
	replace ocupa_ci=8 if (p41>=110 & p41<=310) & emp_ci==1
	replace ocupa_ci=9 if (p41==9999) & emp_ci==1
	label define ocupa_ci 1"profesional y tecnico" 2"director o funcionario sup" 3"administrativo y nivel intermedio"
	label define ocupa_ci  4 "comerciantes y vendedores" 5 "en servicios" 6 "trabajadores agricolas", add
	label define ocupa_ci  7 "obreros no agricolas, conductores de maq y ss de transporte", add
	label define ocupa_ci  8 "FFAA" 9 "Otras ", add
	label value ocupa_ci ocupa_ci
	label variable ocupa_ci "Ocupacion laboral" 
	
	
	*************
	***rama_ci***
	*************
	*Nota: se utiliza CIUU revision3 
	* Ultima actualizacion MGD 4/16/2014
	gen rama_ci=.
	replace rama_ci = 1 if (rama>=111  & rama<=502) & emp_ci==1
	replace rama_ci = 2 if (rama>=1010 & rama<=1429) & emp_ci==1
	replace rama_ci = 3 if (rama>=1511 & rama<=3720) & emp_ci==1
	replace rama_ci = 4 if (rama>=4010 & rama<=4100) & emp_ci==1
	replace rama_ci = 5 if (rama>=4510 & rama<=4550) & emp_ci==1
	replace rama_ci = 6 if (rama>=5010 & rama<=5520) & emp_ci==1
	replace rama_ci = 7 if (rama>=6010 & rama<=6420) & emp_ci==1
	replace rama_ci = 8 if (rama>=6511 & rama<=7499) & emp_ci==1
	replace rama_ci = 9 if (rama>=7500 & rama<=9900) & emp_ci==1
	label var rama_ci "Rama de actividad"
	label def rama_ci 1"Agricultura, caza, silvicultura y pesca" 2"Explotación de minas y canteras" 3"Industrias manufactureras"
	label def rama_ci 4"Electricidad, gas y agua" 5"Construcción" 6"Comercio, restaurantes y hoteles" 7"Transporte y almacenamiento", add
	label def rama_ci 8"Establecimientos financieros, seguros e inmuebles" 9"Servicios sociales y comunales", add
	label val rama_ci rama_ci
	
	****************
	***durades_ci***
	****************

	gen durades_ci=tiembus/4.33 if tiembus!=999
	label variable durades_ci "Duracion del desempleo en meses"
	

	***************
	*antiguedad_ci*
	***************
	/*
	gen antiguedad_ci=.
	label var antiguedad_ci "antiguedad laboral (anios) - aproximacion"	
	*/
	*Modificación Mayra Sáenz - Agosto 2013
	gen antiguedad_ci=pe44
	label var antiguedad_ci "antiguedad laboral (años)"	

*******************
***categoinac_ci***
*******************
gen categoinac_ci =1 if (condina==2 & condocup_ci==3)
replace categoinac_ci = 2 if  ( condina==3 & condocup_ci==3)
replace categoinac_ci = 3 if  ( condina==4 & condocup_ci==3)
replace categoinac_ci = 4 if  ((categoinac_ci ~=1 & categoinac_ci ~=2 & categoinac_ci ~=3) & condocup_ci==3)
label var categoinac_ci "Categoría de inactividad"
label define categoinac_ci 1 "jubilados o pensionados" 2 "Estudiantes" 3 "Quehaceres domésticos" 4 "Otros"

*******************
***formal***
*******************
/*gen formal=1 if cotizando_ci==1

replace formal=1 if afiliado_ci==1 & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1 & pais_c=="BOL"   /* si se usa afiliado, se restringiendo a ocupados solamente*/
replace formal=1 if afiliado_ci==1 & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1 & pais_c=="CRI"
replace formal=1 if afiliado_ci==1 & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1 & pais_c=="GTM" & anio_c>1998
replace formal=1 if afiliado_ci==1 & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1 & pais_c=="PAN"
replace formal=1 if afiliado_ci==1 & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1 & pais_c=="PRY" & anio_c<=2006
replace formal=1 if afiliado_ci==1 & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1 & pais_c=="DOM"
replace formal=1 if afiliado_ci==1 & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1 & pais_c=="MEX" & anio_c>=2008

gen byte formal_ci=.
replace formal_ci=1 if formal==1 & (condocup_ci==1 | condocup_ci==2)
replace formal_ci=0 if formal_ci==. & (condocup_ci==1 | condocup_ci==2) 
label var formal_ci "1=afiliado o cotizante / PEA"*/

* MGD 04/2016; se cambia por afiliado ya que usa la pregunta que abarca a todos los ocupados.
g formal_ci=(afiliado_ci==1)
*gen formal_ci=(cotizando_ci==1)
label var formal_ci "1=afiliado o cotizante / PEA"

			**************************
			***VARIABLES DE INGRESO***
			**************************


	foreach var of varlist pe61 pe62b  pe63  pe64 ///
	pe65b pe66 pe67b pe68b pe69b pe70b pe71b pe73 {

	replace `var'=. if `var'==999991
	replace `var'=. if `var'==999999
	replace `var'=. if `var'==999
	replace `var'=. if `var'==9999
	replace `var'=. if `var'==99999
	replace `var'=. if `var'==999999
	replace `var'=. if `var'==9999999
	replace `var'=. if `var'==99999999
	replace `var'=. if `var'==999999999
	replace `var'=. if `var'==39999999
	replace `var'=. if `var'==89999999
	replace `var'=. if `var'==89999999
	replace `var'=. if `var'==10000010
	replace `var'=. if `var'==10333333
	replace `var'=. if `var'==999992
	replace `var'=. if `var'==999991
	}

	recode pe61 (200120=. ) ( 150120=.)  (245140=.)
	recode pe66 (120130=.)

	***************
	***ylmpri_ci***
	***************
	
	egen ylmpri_ci = rsum(pe61 pe62b  pe63  pe64) 
	replace ylmpri_ci =. if pe61==. & pe62b==. & pe63==. & pe64==. 
	label var ylmpri_ci "Ingreso laboral monetario actividad principal" 

	*****************
	***nrylmpri_ci***
	*****************
	gen nrylmpri_ci=(ylmpri_ci==. & emp_ci==1)
	label var nrylmpri_ci "Id no respuesta ingreso de la actividad principal"  

	****************
	***ylnmpri_ci***
	****************
	gen ylnmpri_ci=pe65b
	label var ylnmpri_ci "Ingreso laboral NO monetario actividad principal"   


	***************
	***ylmsec_ci***
	***************
	gen ylmsec_ci=  pe66 
	label var ylmsec_ci "Ingreso laboral monetario segunda actividad" 

	****************
	***ylnmsec_ci***
	****************
	gen ylnmsec_ci=pe67b
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
	egen ylm_ci= rsum(ylmpri_ci ylmsec_ci)
	replace ylm_ci=. if ylmpri_ci==. &  ylmsec_ci==.
	label var ylm_ci "Ingreso laboral monetario total"  

	*************
	***ylnm_ci***
	*************
	egen ylnm_ci=rsum(ylnmpri_ci ylnmsec_ci)
	replace ylnm_ci=. if ylnmpri_ci==. &  ylnmsec_ci==.
	label var ylnm_ci "Ingreso laboral NO monetario total"  

	*************
	***ynlm_ci***
	*************
	egen ynlm_ci=rsum(pe68b pe69b pe70b pe71b pe73)
	replace ynlm_ci = . if pe68b==. & pe69b==. & pe70b==. & pe71b==. & pe73==. 
	label var ynlm_ci "Ingreso no laboral monetario"  


	**************
	***ynlnm_ci***
	**************
	gen ynlnm_ci=.
	label var ynlnm_ci "Ingreso no laboral no monetario" 

	****************
	***remesas_ci***
	****************
	gen remesas_ci=pe71b
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

	*************
	***aedu_ci***
	*************
	
	/*replace nivinst=. if (nivinst>=11)
	replace anoinst=. if (anoinst>=20)
	replace anoinst= 0 if (nivinst == 1)

	* 26/08/2015: Cambios educación I.B-no se cuenta como años de educación la educación para adultos
	gen aedu_ci = .
	replace aedu_ci = 0          if nivinst==1
	/*replace aedu_ci = 0          if nivinst==2 & (anoinst==0)
	replace aedu_ci = 2          if nivinst==2 & (anoinst==1)
	replace aedu_ci = 4          if nivinst==2 & (anoinst==2)
	replace aedu_ci = 6          if nivinst==2 & (anoinst==3)*/
	replace aedu_ci = 0          if nivinst==3
	replace aedu_ci = anoinst    if nivinst==4
	replace aedu_ci = anoinst    if nivinst==5
	replace aedu_ci = anoinst+6  if nivinst==6
	replace aedu_ci = anoinst+9  if nivinst==7
	replace aedu_ci = anoinst+12 if nivinst==8 | nivinst==9
	replace aedu_ci = anoinst+17 if nivinst==10
	replace aedu_ci =. if (nivinst==. | anoinst==.)
	label var aedu_ci "Anios de educacion aprobados" 
	label value aedu_ci aedu_ci 
	*/
	
	*************
	***aedu_ci***
	*************
	
	replace nivinst=. if (nivinst>=11)
	replace anoinst=. if (anoinst>=20)
	replace anoinst= 0 if (nivinst == 1)

	* 09/29/2015: Cambios educación por Iván Bornacelly SCL/EDU-no se cuenta como años de educación la educación para adultos
	gen aedu_ci = .
	replace aedu_ci = 0          if nivinst==1
	/*replace aedu_ci = 0          if nivinst==2 & (anoinst==0)
	replace aedu_ci = 2          if nivinst==2 & (anoinst==1)
	replace aedu_ci = 4          if nivinst==2 & (anoinst==2)
	replace aedu_ci = 6          if nivinst==2 & (anoinst==3)*/
	replace aedu_ci = 0          if nivinst==3
	replace aedu_ci = anoinst-1    if nivinst==4 // Se le resta 1 dado que el nivel primaria y educación básica tienen incluido el primera año de educación inicial
	replace aedu_ci = anoinst-1    if nivinst==5
	* siguiente linea Mod Ivan B./EDU 2015, 12
	replace aedu_ci=0 if aedu_ci==-1
	replace aedu_ci = anoinst+6  if nivinst==6
	replace aedu_ci = anoinst+9  if nivinst==7
	replace aedu_ci = anoinst+12 if nivinst==8 | nivinst==9
	replace aedu_ci = anoinst+17 if nivinst==10
	replace aedu_ci =. if (nivinst==. | anoinst==.)
	label var aedu_ci "Anios de educacion aprobados" 
	label value aedu_ci aedu_ci


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

	*************
	***eduui_ci***
	**************
	/*
	*Para la educación superior no es posible saber cuantos años dura el ciclo
	gen eduui_ci=.
	label variable eduui_ci "Superior incompleto"
	*/
	*Modificación Mayra Sáenz - Agosto 2013
	gen eduui_ci=(aedu_ci>12 & aedu_ci<17)
	label variable eduui_ci "Superior incompleto"

	***************
	***eduuc_ci***
	***************
	/*
	*Para la educación superior no es posible saber cuantos años dura el ciclo
	gen byte eduuc_ci=.
	label variable eduuc_ci "Superior completo"
	*/
	*Modificación Mayra Sáenz - Agosto 2013
	
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
	***asispre_ci***
	***************	
	*Variable agregada por Iván Bornacelly - 01/16/2017
	g asispre_ci=.
	la var asispre_ci "Asiste a educacion prescolar"
	
	**************
	***eduac_ci***
	**************
	gen eduac_ci=.
	label variable eduac_ci "Superior universitario vs superior no universitario"

	***************
	***asiste_ci***
	***************
	gen asiste_ci=(asistea==1)
	replace asiste_ci=. if  asistea==.
	label variable asiste_ci "Asiste actualmente a la escuela"

	**************
	***pqnoasis_ci***
	**************
	gen pqnoasis_ci=pe08 
	label var pqnoasis_ci "Razones para no asistir a la escuela"
	label def pqnoasis_ci 1"edad" 2"termino sus estudios" 3"falta recursos económicos" 4"fracaso escolar"
	label def pqnoasis_ci 5"por trabajo" 6"temor maestros" 7"enfermedad o discapacidad" 8"quehaceres del hogar", add
	label def pqnoasis_ci 9"familia no permite" 10"no hay establecimientos educativos" 11"no está interesado", add
	label def pqnoasis_ci 12"por embarazo" 13"otra razón", add
	label val pqnoasis_ci pqnoasis_ci
	
		
	**************
    *pqnoasis1_ci*
    **************
    **Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**
    g       pqnoasis1_ci = 1 if pe08==3
    replace pqnoasis1_ci = 2 if pe08==5
    replace pqnoasis1_ci = 3 if pe08==7  | pe08==9
    replace pqnoasis1_ci = 4 if pe08==11
    replace pqnoasis1_ci = 5 if pe08==8  | pe08==12
    replace pqnoasis1_ci = 6 if pe08==2
    replace pqnoasis1_ci = 7 if pe08==1 
    replace pqnoasis1_ci = 8 if pe08==10
    replace pqnoasis1_ci = 9 if pe08==4  | pe08==6 | pe08==13

    label define pqnoasis1_ci 1 "Problemas económicos" 2 "Por trabajo" 3 "Problemas familiares o de salud" 4 "Falta de interés" 5	"Quehaceres domésticos/embarazo/cuidado de niños/as" 6 "Terminó sus estudios" 7	"Edad" 8 "Problemas de acceso"  9 "Otros"
    label value  pqnoasis1_ci pqnoasis1_ci

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
	gen edupub_ci=.
	label var edupub_ci "Asiste a un centro de ensenanza público"

*************
***tecnica_ci**
*************
gen tecnica_ci=(nivinst==8)
label var tecnica_ci "=1 formacion terciaria tecnica"

		**********************************
		**** VARIABLES DE LA VIVIENDA ****
		**********************************
	
	****************
	***aguared_ch***
	****************
	
	generat aguared_ch= (vi10==1 | vi10==2 | vi10==3 | vi10==4)
	replace aguared_ch=. if vi10==.
	label var aguared_ch "Acceso a fuente de agua por red"	
	
	*****************
	***aguadist_ch***
	*****************
	gen aguadist_ch=.
	replace aguadist_ch=1 if vi11==1
	replace aguadist_ch=2 if vi11==2
	replace aguadist_ch=3 if vi11==3 | vi11==4
	label var aguadist_ch "Ubicación de la principal fuente de agua"
	label def aguadist_ch 1"Dentro de la vivienda" 2"Fuera de la vivienda pero en el terreno"
	label def aguadist_ch 3"Fuera de la vivienda y del terreno", add
	label val aguadist_ch aguadist_ch

	*****************
	***aguamala_ch***
	*****************
	gen aguamala_ch=(vi10==7 | vi10==8)
	replace aguamala_ch=. if vi08==.
	label var aguamala_ch "Agua unimproved según MDG" 
	
	
	*****************
	***aguamide_ch***
	*****************
	gen aguamide_ch=.
	label var aguamide_ch "Usan medidor para pagar consumo de agua"

	************
	***luz_ch***
	************
	/*
	gen luz_ch=(vi14==1)
	label var luz_ch  "La principal fuente de iluminación es electricidad"
	
	*/
	*Modificación Mayra Sáenz - Agosto 2013
	gen luz_ch=(vi14==1|vi14==2)
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
	replace combust_ch=1 if vi07==1 | vi07==3 
	replace combust_ch=. if vi07==.
	label var combust_ch "Principal combustible gas o electricidad" 
	
	*************
	***bano_ch***
	*************
	gen bano_ch=1
	replace bano_ch=0 if vi08==5 
	label var bano_ch "El hogar tiene servicio sanitario"
	
	***************
	***banoex_ch***
	***************
	gen banoex_ch=(vi09==1)
	replace banoex_ch=. if vi09==.
	label var banoex_ch "El servicio sanitario es exclusivo del hogar"

	*************
	***des1_ch***
	*************
	gen des1_ch=.
	replace des1_ch=0 if vi08==5
	replace des1_ch=1 if vi08==1 | vi08==2
	replace des1_ch=2 if vi08==3 | vi08==4
	label var des1_ch "Tipo de desague según unimproved de MDG"
	label def des1_ch 0"No tiene servicio sanitario" 1"Conectado a red general o cámara séptica"
	label def des1_ch 2"Letrina o conectado a pozo ciego" 3"Desemboca en río o calle", add
	label val des1_ch des1_ch
	
	*************
	***des2_ch***
	*************
	
	gen des2_ch=.
	replace des2_ch=0 if des1_ch==0
	replace des2_ch=1 if des1_ch==1 | des1_ch==2
	label var des2_ch "Tipo de desague sin incluir definición MDG"
	label def des2_ch 0"No tiene servicio sanitario" 1"Conectado a red general, cámara séptica, pozo o letrina"
	label def des2_ch 2"Cualquier otro caso", add
	label val des2_ch des2_ch
	
	*************
	***piso_ch***
	*************

	* Modificaciones Marcela Rubio Septiembre 2014
	
	/*
	gen piso_ch=.
	replace piso_ch=0 	if vi04==6
	replace piso_ch=1 	if vi04==1 | vi04==2 | vi04==3 	
	replace piso_ch=2 	if vi04==4 | vi04==5 | vi04==6
	label var piso_ch "Materiales de construcción del piso"  
	label def piso_ch 0"Piso de tierra" 1"Materiales permanentes" 2"Otros materiales"
	label val piso_ch piso_ch
	*/
	
	gen piso_ch=.
	replace piso_ch=0 	if vi04==6
	replace piso_ch=1 	if vi04==1 | vi04==2 | vi04==3 	
	replace piso_ch=2 	if vi04==4 | vi04==5 | vi04==7
	label var piso_ch "Materiales de construcción del piso"  
	label def piso_ch 0"Piso de tierra" 1"Materiales permanentes" 2"Otros materiales"
	label val piso_ch piso_ch
	
	
	**************
	***pared_ch***
	**************
	
	gen pared_ch=0 		if vi03==4 |vi03==5 |vi03==6
	replace pared_ch=1	if vi03>=1 & vi03<=3
	label var pared_ch "Materiales de construcción de las paredes"	
	
	**************
	***techo_ch***
	**************
	
	gen techo_ch=.
	label var techo_ch "Materiales de construcción del techo"	
	
	**************
	***resid_ch***
	**************
	
	generat resid_ch=0 if vi15==1 | vi15==2
	replace resid_ch=1 if vi15==4
	replace resid_ch=2 if vi15==3
	replace resid_ch=3 if vi15==5
	replace resid_ch=. if vi15==.
	label var resid_ch "Método de eliminación de residuos"
	label def resid_ch 0"Recolección pública o privada" 1"Quemados o enterrados"
	label def resid_ch 2"Tirados a un espacio abierto" 3"Otros", add
	label val resid_ch resid_ch
	
	**Daniela Zuluaga- Enero 2018: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**
	
    *********************
    ***aguamejorada_ch***
    *********************
	g       aguamejorada_ch = 1 if (vi10 >=1 & vi10 <=4) | vi10 ==6  | vi10 ==8
	replace aguamejorada_ch = 0 if  vi10 ==5 | vi10 ==7 | vi10 ==9
    *********************
    ***banomejorado_ch***
    *********************
	g       banomejorado_ch = 1 if (vi08 >=1 & vi08 <=4) & vi09 == 1
	replace banomejorado_ch = 0 if ((vi08 >=1 & vi08 <=4) & vi09 == 2) | vi08 ==5 
		 
	*************
	***dorm_ch***
	*************
	/*A los hogares que reportan 0 habitaciones exclusivas para dormir, pues la vivienda está constituída por
	un sólo ambiente,se les imputa 1 habitación. A los hogares que dicen no tener cuartos exclusivos 
	para dormir, pero que viven en vivienda de 2, 3 y 4 habitaciones se les asigna missing*/
/*
	gen dorm_ch=vi06
	replace dorm_ch=1 if vi06==0 & vi05==1
	replace dorm_ch=. if vi06==0 & vi05>1
	label var dorm_ch "Habitaciones para dormir"
*/	
	**Modificación Mayra Sáenz - Agosto 2013
		
	gen dorm_ch=vi06 if vi06!=99
	replace dorm_ch=1 if vi06==0 & vi05==1
	replace dorm_ch=. if vi06==0 & (vi05>1 & vi05!=99)
	label var dorm_ch "Habitaciones para dormir"	
		
		
	****************
	***cuartos_ch***
	****************
	/*
	gen cuartos_ch=vi05 
	label var cuartos_ch "Habitaciones en el hogar"
	*/	
	**Modificación Mayra Sáenz - Agosto 2013
	gen cuartos_ch=vi05 if vi05!=99
	label var cuartos_ch "Habitaciones en el hogar"

	***************
	***cocina_ch***
	***************
	gen cocina_ch=.
	label var cocina_ch "Cuarto separado y exclusivo para cocinar"

	**************
	***telef_ch***
	**************

	gen telef_ch=.
	replace telef_ch=1 if vi13==1
	replace telef_ch=0 if vi13==2
	label var telef_ch "El hogar tiene servicio telefónico fijo"	
		
	***************
	***refrig_ch***
	***************
	
	gen refrig_ch=0
	replace refrig_ch=1 if  vi1602a==1
	replace refrig_ch=. if  vi1602a==.
	label var refrig_ch "El hogar posee refrigerador o heladera"
			
	**************
	***freez_ch***
	**************
	gen freez_ch=.
	label var freez_ch "El hogar posee congelador"

	*************
	***auto_ch***
	*************
		
	gen auto_ch=0
	replace auto_ch=1 if vi1607a==1
	replace auto_ch=. if vi1607a==.
	label var auto_ch "El hogar posee automovil particular"
	
	**************
	***compu_ch***
	**************

	gen compu_ch=.
	label var compu_ch "El hogar posee computador"
	
	*****************
	***internet_ch***
	*****************
	
	gen internet_ch=.
	label var internet_ch "El hogar posee conexión a Internet"
	
	************
	***cel_ch***
	************
		
	gen cel_ch=.
	label var cel_ch "El hogar tiene servicio telefonico celular"
		
	**************
	***vivi1_ch***
	**************
	
	generat vivi1_ch=1 if vi01==1
	replace vivi1_ch=2 if vi01==2
	replace vivi1_ch=3 if vi01==3 | vi01==4 | vi01==5 | vi01==6 | vi01==7
	replace vivi1_ch=. if vi01==.
	label var vivi1_ch "Tipo de vivienda en la que reside el hogar"
	label def vivi1_ch 1"Casa" 2"Departamento" 3"Otros"
	label val vivi1_ch vivi1_ch
		
	**************
	***vivi2_ch***
	**************
	
	gen vivi2_ch=(vivi1_ch==1 |  vivi1_ch==2)
	replace vivi2_ch=. if vivi1_ch==.
	label var vivi2_ch "La vivienda es casa o departamento"
		
	*****************
	***viviprop_ch***
	*****************
	
	gen viviprop_ch=.
	replace viviprop_ch=0 if vi02==1
	replace viviprop_ch=1 if vi02==2
	replace viviprop_ch=2 if vi02==3
	replace viviprop_ch=3 if vi02==4 | vi01==5
		
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

	******************************
	*** VARIABLES DE MIGRACION ***
	******************************

	* Variables incluidas por SCL/MIG Fernando Morales

	*******************
	*** migrante_ci ***
	*******************
	
	gen migrante_ci=.
	label var migrante_ci "=1 si es migrante"
	
	**********************
	*** migantiguo5_ci ***
	**********************
	
	gen migantiguo5_ci=.
	label var migantiguo5_ci "=1 si es migrante antiguo (5 anos o mas)"
		
	**********************
	*** migrantelac_ci ***
	**********************
	
	gen migrantelac_ci=.
	label var migrantelac_ci "=1 si es migrante proveniente de un pais LAC"
	
	
	**********************
	*** migrantiguo5_ci ***
	**********************
	
	gen migrantiguo5_ci=.
	label var migrantiguo5_ci "=1 si es migrante antiguo (5 anos o mas)"
		
	**********************
	*** miglac_ci ***
	**********************
	
	gen miglac_ci=.
	label var miglac_ci "=1 si es migrante proveniente de un pais LAC"
	
	
/*_____________________________________________________________________________________________________*/
* Asignación de etiquetas e inserción de variables externas: tipo de cambio, Indice de Precios al 
* Consumidor (2011=100), Paridad de Poder Adquisitivo (PPA 2011),  líneas de pobreza
/*_____________________________________________________________________________________________________*/


do "$gitFolder\armonizacion_microdatos_encuestas_hogares_scl\_DOCS\\Labels&ExternalVars_Harmonized_DataBank.do"

/*_____________________________________________________________________________________________________*/
* Verificación de que se encuentren todas las variables armonizadas 
/*_____________________________________________________________________________________________________*/

order region_BID_c region_c pais_c anio_c mes_c zona_c factor_ch	idh_ch	idp_ci	factor_ci sexo_ci edad_ci ///
afroind_ci afroind_ch afroind_ano_c dis_ci dis_ch relacion_ci civil_ci jefe_ci nconyuges_ch nhijos_ch notropari_ch notronopari_ch nempdom_ch ///
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

/*Homologar nombre del identificador de ocupaciones (isco, ciuo, etc.) y de industrias y dejarlo en base armonizada 
para análisis de trends (en el marco de estudios sobre el futuro del trabajo)*/
rename p41 codocupa
rename rama codindustria

compress


saveold "`base_out'", version(12) replace


log close


