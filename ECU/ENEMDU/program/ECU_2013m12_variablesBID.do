
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
local ANO "2013"
local ronda m12 


local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
   
capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Ecuador
Encuesta: ENEMDU
Round: m12
Autores: Yessenia Loayza (yessenial@iadb.org / desloay@hotmail.com)
Última versión: Yessenia Loayza E-mail: yessenial@iadb.org / desloay@hotmail.com
Fecha última modificación: Febrero 2014

							SCL/LMK - IADB
****************************************************************************/
/***************************************************************************
Detalle de procesamientos o modificaciones anteriores:
2014, 01 MLO cambio del limite de edad de condocup_ci a 5+

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

	***************
	***region_c ***
	***************
	gen region_c=int(ciudad/10000)
	gen canton =int(ciudad/100)
	recode  region_c (14/16=89) (19/22=89)
	replace region_c =23 if canton==1706
	replace region_c =24 if (canton==917 | canton==915 | canton==926)
	
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
	23 "Santo Domingo de los Tsáchilas" ///
    24 "Santa Elena" ///
	89 "Amazonia"  ///
	90 "zonas no delimitadas" 
   label value region_c region_c
   drop canton
   label var region_c "division politico-administrativa, provincia"
   
	***************
	***factor_ch***
	***************
	gen factor_ch=fexp
	label variable factor_ch "Factor de expansion del hogar"
	
	*************
	****idh_ch***
	*************
	sort area ciudad zona sector panelm vivienda hogar
	* Modificación Marcela Rubio
	*egen idh_ch = group( area ciudad zona sector panelm vivienda hogar)
	egen idh_ch = group(ciudad zona sector panelm vivienda hogar)
	label variable idh_ch "ID del hogar"
	
	*************
	****idh_ci***
	*************
	gen idp_ci=p01
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

	gen anio_c=2013
	label variable anio_c "Anio de la encuesta" 
		
	***********
	***mes_c***
	***********
	gen mes_c=12
	label var mes_c "Mes de la encuesta" 
	
	*****************
	***relacion_ci***
	*****************
	*la li p04
	gen relacion_ci=1     if p04==1
	replace relacion_ci=2 if p04==2
	replace relacion_ci=3 if p04==3
	replace relacion_ci=4 if p04>=4 & p04<=7
	replace relacion_ci=5 if p04==9
	replace relacion_ci=6 if p04==8
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
	gen factor_ci=fexp
	label variable factor_ci "Factor de expansion del individuo"
	
	***************
	***upm_ci***
	***************

	clonevar upm_ci=sector
	label variable upm_ci "Unidad Primaria de Muestreo"

	***************
	***estrato_ci***
	***************

	gen estrato_ci=.
	label variable estrato_ci "Estrato"

	*************
	***sexo_ci***
	*************
	*la li p02
	gen sexo_ci=p02
	label var sexo_ci "Sexo del individuo" 
	label def sexo_ci 1"Masculino" 2"Femenino" 
	label val sexo_ci sexo_ci

	**********
	***edad***
	**********
	
	*tab p03
	gen edad_ci=p03 if p03<98
	label variable edad_ci "Edad del individuo"
	
	**************
	***civil_ci***
	**************
	*la li p06
	gen civil_ci=1 		if p06==6
	replace civil_ci=2	if p06==1 | p06==5
	replace civil_ci=3	if p06==2 | p06==3
	replace civil_ci=4	if p06==4
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
**Pregunta: p15 (1 indígena, 2 afroecuatoriano, 3 negro, 4 mulato, 5 montubio, 6 mestizo, 7 blanco, 8 otro) (adiciona categorías afroecuatoriano y montubio)
gen afroind_ci=. 
replace afroind_ci=1  if p15 == 1
replace afroind_ci=2 if p15 == 2 | p15 == 3 | p15 == 4
replace afroind_ci=3 if p15 == 5 | p15 == 6| p15 ==7 | p15 == 8
replace afroind_ci=. if p15==. 
replace afroind_ci=9 if p15==. & edad_ci<5

	***************
	***afroind_ch***
	***************
gen afroind_jefe= afroind_ci if relacion_ci==1
egen afroind_ch  = min(afroind_jefe), by(idh_ch) 
drop afroind_jefe

	*******************
	***afroind_ano_c***
	*******************
gen afroind_ano_c=2010

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
	gen condocup_ci=.
	replace condocup_ci=1 if condact>=0 & condact <=3
	replace condocup_ci=2 if condact==5
	replace condocup_ci=3 if condocup_ci!=1 & condocup_ci!=2
	*replace condocup_ci=4 if edad_ci<10
	replace condocup_ci=4 if edad<5
	*/
	
	*YL-> al cambiar la categoria 4 a <5 toca generar nuevamente la variable condocup caso contrario el grupo 6-9 se van a inactivos
	generat condocup_ci=.
	replace condocup_ci=1 if p20==1 | p21<12 | p22==1 
	replace condocup_ci=2 if (p20==2 | p21==12 | p22==2) & p32<11
	replace condocup_ci=3 if condocup_ci!=1 & condocup_ci!=2
	replace condocup_ci=4 if edad_ci<5
	label define condocup_ci 1 "ocupados" 2 "desocupados" 3 "inactivos" 4 "menor de PET"
	label value condocup_ci condocup_ci
	label var condocup_ci "Condicion de ocupacion utilizando definicion del pais"

	
	****************
	*afiliado_ci****
	****************
	cap clonevar iess = p05a 
	gen afiliado_ci=(iess>=1 & iess<=4) /*todas personas*/	
	replace afiliado_ci=. if iess==.
	label var afiliado_ci "Afiliado a la Seguridad Social"
	*Nota: seguridad social comprende solo los que en el futuro me ofrecen una pension.

	****************
	*cotizando_ci***
	****************
	gen cotizando_ci=0     if condocup_ci==1 | condocup_ci==2 
	replace cotizando_ci=1 if (p44f==1) & cotizando_ci==0 /*solo a emplead@s y asalariad@s, difiere con los otros paises*/
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
	gen instcot_ci=iess /* a todas las personas*/
	label var instcot_ci "institución a la cual cotiza"

	*************
	*tamemp_ci***
	*************
	/*
	gen tamemp_ci=p47b
	replace tamemp_ci=. if (p47b>=99 & p47b!=.)
	replace tamemp_ci=100 if p47a==2
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
	replace tamemp_ci=1 if p47a==1 & (p47b>=1 & p47b<=5)
	replace tamemp_ci=2 if p47b>=6 & p47b<=50
	replace tamemp_ci=3 if (p47a==2) | (p47b>50 & p47b!=.)
	label var tamemp_ci "# empleados en la empresa segun rangos"
	label define tamemp_ci 1 "Pequeña" 2 "Mediana" 3 "Grande"
	label value tamemp_ci tamemp_ci
	
	*************
	**pension_ci*
	*************
	gen pension_ci=0 
	replace pension_ci=1 if (p72a==1) /* A todas las per mayores de cinco*/
	replace pension_ci=. if p72a==.
	label var pension_ci "1=Recibe pension contributiva"

	*************
	*ypen_ci*
	*************
	gen ypen_ci=p72b if pension_ci==1
	replace ypen_ci=. if ypen_ci==999999 
	label var ypen_ci "Valor de la pension contributiva"

	***************
	*pensionsub_ci*
	***************
	gen pensionsub_ci=0 
	replace pensionsub_ci=1 if p75==1
	label var pensionsub_ci "1=recibe pension subsidiada / no contributiva"

	*****************
	**  ypensub_ci  *
	*****************
	gen ypensub_ci=p76 if pensionsub_ci==1
	replace ypensub_ci=. if ypensub_ci==999999
	label var ypensub_ci "Valor de la pension subsidiada / no contributiva"

	*************
	*cesante_ci* 
	*************
	cap clonevar trabant = p37
	generat cesante_ci=0 if condocup_ci==2
	replace cesante_ci=1 if trabant==1 & condocup_ci==2
	label var cesante_ci "Desocupado - definicion oficial del pais"

	*********
	*lp_ci***
	*********
	gen lp_ci =78.10468 
	label var lp_ci "Linea de pobreza oficial del pais"

	***********
	*lpe_ci ***
	***********
	gen lpe_ci =44.01546
	label var lpe_ci "Linea de indigencia oficial del pais"

	*************
	**salmm_ci***
	*************
	gen salmm_ci= 318
	label var salmm_ci "Salario minimo legal"

	/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/


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
	cap clonevar bustrama=p32
	cap clonevar motnobus = p34
	gen desalent_ci=(motnobus==6 | motnobus==7)
	label var desalent_ci "Trabajadores desalentados"
	
	*****************
	***horaspri_ci***
	*****************
	*tab p51a
	gen horaspri_ci=p51a
	replace horaspri_ci=. if p51a==999
	replace horaspri_ci=. if emp_ci==0
	label var horaspri_ci "Horas trabajadas semanalmente en el trabajo principal"

	*****************
	***horastot_ci***
	*****************
	for var p51a p51b p51c: recode X (999=.)
	egen horastot_ci=rsum(p51a p51b p51c) if emp_ci==1 
	replace horastot_ci=. if p51a==. & p51b==. & p51c==.
	replace horastot_ci=. if emp_ci==0
	label var horastot_ci "Horas trabajadas semanalmente en todos los empleos"
	
	***************
	***subemp_ci***
	***************
	/*
	*la li p27
	gen subemp_ci=0
	replace subemp_ci=1 if (p27>=1 & p27<=3) & horastot_ci<=30 & emp_ci==1
	replace subemp_ci =. if emp_ci ==.
	label var subemp_ci "Personas en subempleo por horas"
	*/
	*Modificacion MGD 06/18/2014 solo horas de actividad principal y considerando dos alternativas en subempleo visible.
	gen subemp_ci=0
	replace subemp_ci=1 if (p27>=1 & p27<=3) & p28==1 & horaspri_ci<=30 & emp_ci==1
	label var subemp_ci "Personas en subempleo por horas"
	
	*******************
	***tiempoparc_ci***
	*******************
	* MGR: Modifico serie en base a correcciones Laura Castrillo: se debe utilizar horaspri en lugar de horastot como había sido generada antes
	gen tiempoparc_ci=((horaspri_ci>=1 & horaspri_ci<30) & p27==4 & emp_ci==1)
	replace tiempoparc_ci=. if emp_ci==0
	label var tiempoparc_c "Personas que trabajan medio tiempo" 

	******************
	***categopri_ci***
	******************
	gen categopri_ci=.
	replace categopri_ci=1 if p42==5
	replace categopri_ci=2 if p42==6 
	replace categopri_ci=3 if (p42>=1 & p42<=4) | p42==10
	replace categopri_ci=4 if (p42>=7 & p42<=9)
	replace categopri_ci=. if emp_ci==0
	label define categopri_ci 1"Patron" 2"Cuenta propia" 0"Otro"
	label define categopri_ci 3"Empleado" 4" No remunerado" , add
	label value categopri_ci categopri_ci
	label variable categopri_ci "Categoria ocupacional"

	g catetrab=p42
	******************
	***categosec_ci***
	******************
	*la li p54
	gen categosec_ci=.
	replace categosec_ci=1 if p54==5
	replace categosec_ci=2 if p54==6	
	replace categosec_ci=3 if (p54>=1 & p54<=4) | p54==10
	replace categosec_ci=4 if (p54>=7 & p54<=9)
	label define categosec_ci 1"Patron" 2"Cuenta propia" 0"Otro"
	label define categosec_ci 3"Empleado" 4" No remunerado" , add
	label value categosec_ci categosec_ci
	label variable categosec_ci "Categoria ocupacional en la segunda actividad"

	*****************
	*tipocontrato_ci*
	*****************
	cap clonevar estabil=p43 
	gen tipocontrato_ci=. /* Solo disponible para asalariados*/
	replace tipocontrato_ci=1 if (estabil==1 | estabil==2) & categopri_ci==3
	replace tipocontrato_ci=2 if (estabil==3)              & categopri_ci==3
	replace tipocontrato_ci=3 if (estabil>=4 & estabil<=6) & categopri_ci==3
	label var tipocontrato_ci "Tipo de contrato segun su duracion en act principal"
	label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
	label value tipocontrato_ci tipocontrato_ci

	*****************
	***nempleos_ci***
	*****************
	*la li p50
	gen nempleos_ci=p50
	replace nempleos_ci=. if emp_ci!=1
	label var nempleos_ci "Número de empleos" 
	label define nempleos_ci 1 "Un empleo" 2 "Mas de un empleo"
	label value nempleos_ci nempleos_ci
/*
	*****************
	***firmapeq_ci***
	*****************
	*tab p47b
	gen firmapeq_ci=.
	replace firmapeq_ci=1 if p47b>=1 & p47b<=5 
	replace firmapeq_ci=0 if (p47b>5 & p47b<99) | p47a==2
	replace firmapeq_ci=. if emp_ci==0 
	label var firmapeq_ci "Trabajadores informales"
	label def firmapeq_ci 1"5 o menos trabajadores" 0"Mas de 5 trabajadores"
	label val firmapeq_ci firmapeq_ci
	*/	
	*****************
	***spublico_ci***
	*****************
	gen spublico_ci=(p42==1 & emp_ci==1)
	replace spublico_ci=. if emp_ci==.
	label var spublico_ci "Personas que trabajan en el sector público"

	**************
	***ocupa_ci***
	**************
	* Modificacion MGD 07/29/2014: se utiliza CIUO-08.
	generat ocupa_ci=.
	replace ocupa_ci=1 if (p41>=2111 & p41<=3522) & emp_ci==1
	replace ocupa_ci=2 if (p41>=1111 & p41<=1439) & emp_ci==1
	replace ocupa_ci=3 if (p41>=4110 & p41<=4419) & emp_ci==1
	replace ocupa_ci=4 if ((p41>=5211 & p41<=5249) | (p41>=9510 & p41<=9520)) & emp_ci==1
	replace ocupa_ci=5 if ((p41>=5110 & p41<=5169) | (p41>=5311 & p41<=5419) | (p41>=9111 & p41<=9129) | (p41>=9610 & p41<=9624))  & emp_ci==1
	replace ocupa_ci=6 if ((p41>=6110 & p41<=6340) | (p41>=9210 & p41<=9216)) & emp_ci==1
	replace ocupa_ci=7 if ((p41>=7111 & p41<=8350) | (p41>=9310 & p41<=9412))  & emp_ci==1
	replace ocupa_ci=8 if (p41>=110 & p41<=310) & emp_ci==1
	replace ocupa_ci=9 if p41>=9629 & p41!=. & emp_ci==1
	label define ocupa_ci 1"profesional y tecnico" 2"director o funcionario sup" 3"administrativo y nivel intermedio"
	label define ocupa_ci  4 "comerciantes y vendedores" 5 "en servicios" 6 "trabajadores agricolas", add
	label define ocupa_ci  7 "obreros no agricolas, conductores de maq y ss de transporte", add
	label define ocupa_ci  8 "FFAA" 9 "Otras ", add
	label value ocupa_ci ocupa_ci
	label variable ocupa_ci "Ocupacion laboral" 
	
	
	*************
	***rama_ci***
	*************
	/*
	gen rama_ci=.
	replace rama_ci = 1 if rama1==1
	replace rama_ci = 2 if rama1==2
	replace rama_ci = 3 if rama1==3
	replace rama_ci = 4 if rama1>=4 & rama1<=5
	replace rama_ci = 5 if rama1==6
	replace rama_ci = 6 if rama1==7 | rama1==9
	replace rama_ci = 7 if rama1==8
	replace rama_ci = 8 if rama1>=11 & rama1<=12
	replace rama_ci = 9 if rama1>=13 & rama1<=21
	label var rama_ci "Rama de actividad"
	label def rama_ci 1"Agricultura, caza, silvicultura y pesca" 2"Explotación de minas y canteras" 3"Industrias manufactureras"
	label def rama_ci 4"Electricidad, gas y agua" 5"Construcción" 6"Comercio, restaurantes y hoteles" 7"Transporte y almacenamiento", add
	label def rama_ci 8"Establecimientos financieros, seguros e inmuebles" 9"Servicios sociales y comunales", add
	label val rama_ci rama_ci
	*/
	* Clasificacion segun variable desagregada p40
	
	gen rama_ci=.
	replace rama_ci = 1 if (p40>=111 & p40<=322) & emp_ci==1
	replace rama_ci = 2 if (p40>=510 & p40<=990) & emp_ci==1
	replace rama_ci = 3 if (p40>=1010 & p40<=3320) & emp_ci==1
	replace rama_ci = 4 if (p40>=3510 & p40<=3900) & emp_ci==1
	replace rama_ci = 5 if (p40>=4100 & p40<=4390) & emp_ci==1
	replace rama_ci = 6 if ((p40>=4510 & p40<=4799) | (p40>=5510 & p40<=5630)) & emp_ci==1
	replace rama_ci = 7 if ((p40>=4911 & p40<=5320) | (p40>=6110 & p40<=6190)) & emp_ci==1
	replace rama_ci = 8 if (p40>=6411 & p40<=8299) & emp_ci==1
	replace rama_ci = 9 if ((p40>=5811 & p40<=6020) | (p40>=6201 & p40<=6399) | (p40>=8410 & p40<=9900)) & emp_ci==1
	label var rama_ci "Rama de actividad"
	label def rama_ci 1"Agricultura, caza, silvicultura y pesca" 2"Explotación de minas y canteras" 3"Industrias manufactureras"
	label def rama_ci 4"Electricidad, gas y agua" 5"Construcción" 6"Comercio, restaurantes y hoteles" 7"Transporte y almacenamiento", add
	label def rama_ci 8"Establecimientos financieros, seguros e inmuebles" 9"Servicios sociales y comunales", add
	label val rama_ci rama_ci

	
	****************
	***durades_ci***
	****************
	gen durades_ci=p33/4.33
	label variable durades_ci "Duracion del desempleo en meses"
		
	***************
	*antiguedad_ci*
	***************
	* MLO: no se puede distinguir menos de 1 año (indicados como  00)
	gen antiguedad_ci=p45
	label var antiguedad_ci "antiguedad laboral (anios) - aproximacion"	

*******************
***categoinac_ci***
*******************
gen categoinac_ci =1 if (p36==2 & condocup_ci==3)
replace categoinac_ci = 2 if  ( p36==3 & condocup_ci==3)
replace categoinac_ci = 3 if  ( p36==4 & condocup_ci==3)
replace categoinac_ci = 4 if  ((categoinac_ci ~=1 & categoinac_ci ~=2 & categoinac_ci ~=3) & condocup_ci==3)
label var categoinac_ci "Categoría de inactividad"
label define categoinac_ci 1 "jubilados o pensionados" 2 "Estudiantes" 3 "Quehaceres domésticos" 4 "Otros"

*******************
***formal***
*******************
* MGD 04/2016; se cambia por afiliado ya que usa la pregunta que abarca a todos los ocupados.
g formal_ci=(afiliado_ci==1)
*gen formal_ci=(cotizando_ci==1)
label var formal_ci "1=afiliado o cotizante / PEA"


			**************************
			***VARIABLES DE INGRESO***
			**************************

	foreach var of varlist p63 p64b p65 p66 p67 ///
	p68b p69 p70b p71b p72b p73b p74b p76 {

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

    ***************
	***ylmpri_ci***
	***************
	gen p65b = p65*-1
	egen ylmpri_ci = rsum(p63 p64b p65b p66 p67) 
	replace ylmpri_ci =. if p63==. & p64b==. & p65b==. & p66==. & p67==.
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
	gen ylnmpri_ci=p68b
	label var ylnmpri_ci "Ingreso laboral NO monetario actividad principal"   


	***************
	***ylmsec_ci***
	***************
	gen ylmsec_ci=p69 
	label var ylmsec_ci "Ingreso laboral monetario segunda actividad" 

	****************
	***ylnmsec_ci***
	****************
	gen ylnmsec_ci=p70b
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
	egen ynlm_ci=rsum(p71b p72b p73b p74b p76)
	replace ynlm_ci = . if p71b==. & p72b==. & p73b==. & p74b==. & p76==. 
	label var ynlm_ci "Ingreso no laboral monetario"  


	**************
	***ynlnm_ci***
	**************
	gen ynlnm_ci=.
	label var ynlnm_ci "Ingreso no laboral no monetario" 

	****************
	***remesas_ci***
	****************
	gen remesas_ci=p74b
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
	
	cap clonevar nivinst = p10a /* Nivel de instruccion */
	cap clonevar anoinst = p10b /* Anios aprobados */
	
	// Categorias nivel (formulario) - se explican los dos sistemas y equivalencias. 
	label define P10a 1 "Ninguno" 2 "Centro de alfabetización" 3 "Jardín de Infantes" 4 "Primaria" 5 "Educación Básica" 6 "Secundaria" 7 "Educación Media / Bachillerato" 8 "Superior no Universitario" 9 "Superior Universitario" 10 "Post - grado"
	label values p10a P10a
	label values nivinst P10a
	
 	gen aedu_ci = . 
	replace aedu_ci = 0 if nivinst == 1 | nivinst == 2 | nivinst == 3 // Ninguno, jardin o centro alf.
	replace aedu_ci = anoinst if nivinst == 4 // Anios primaria. 
	replace aedu_ci = anoinst - 1 if nivinst == 5 // Anios 2-7 de Educacion basica (equivalentes 1 a 6 primaria anterior por eso se resta 1).  
	replace aedu_ci = 0 if nivinst == 5 & aedu_ci == -1 // para que no queden en -1 los de 0 anios aprobados.
	replace aedu_ci = anoinst + 6 if nivinst == 6 // Secundaria.
	replace aedu_ci = anoinst + 9 if nivinst == 7 // bachillerato.
	replace aedu_ci = anoinst + 12 if nivinst == 8 | nivinst == 9 // Superior.
	replace aedu_ci = anoinst + 16 if nivinst == 10 // Posgrado.

	label var aedu_ci "Anios de educacion aprobados"


	**************
	***eduno_ci***
	**************
	gen eduno_ci = (aedu_ci == 0)
	replace eduno_ci = . if aedu_ci == .
	label variable eduno_ci "Sin educacion"


	**************
	***edupi_ci***
	**************
 	gen edupi_ci = (aedu_ci >= 1 & aedu_ci < 6) 
	replace edupi_ci = . if aedu_ci == .
	label variable edupi_ci "Primaria incompleta"

	**************
	***edupc_ci***
	**************
	gen edupc_ci = (aedu_ci == 6) 
	replace edupc_ci = . if aedu_ci == .  
	label variable edupc_ci "Primaria completa"

	**************
	***edusi_ci***
	**************
 	gen edusi_ci = (aedu_ci >= 7 & aedu_ci < 12) 
	replace edusi_ci = . if aedu_ci == . 
	label variable edusi_ci "Secundaria incompleta"

	**************
	***edusc_ci***
	**************
	gen edusc_ci= (aedu_ci == 12)
	replace edusc_ci = . if aedu_ci == . 
	label variable edusc_ci "Secundaria completa"

	**************
	***eduui_ci***
	**************
	gen eduui_ci = (p12a == 2 & nivinst == 8) | (p12a == 2 & nivinst == 9)
	replace eduui_ci = . if aedu_ci == . 
	label variable eduui_ci "Superior incompleto"

	***************
	***eduuc_ci***
	***************
 	gen byte eduuc_ci = (p12a == 1 & nivinst == 8) | (p12a == 1 & nivinst == 9) | (nivinst == 10)
	replace eduuc_ci = . if aedu_ci == . 
	label variable eduuc_ci "Superior completo"


	***************
	***edus1i_ci***
	***************
	gen edus1i_ci = (aedu_ci > 6 & aedu_ci < 9)
	replace edus1i_ci = . if aedu_ci == . 
	label variable edus1i_ci "1er ciclo de la secundaria incompleto"

	***************
	***edus1c_ci***
	***************
 	gen edus1c_ci = (aedu_ci == 9)
	replace edus1c_ci = . if aedu_ci == . 
	label variable edus1c_ci "1er ciclo de la secundaria completo"

	***************
	***edus2i_ci***
	***************
	gen edus2i_ci = (aedu_ci > 9 & aedu_ci < 12)
	replace edus2i_ci = . if aedu_ci == . 
	label variable edus2i_ci "2do ciclo de la secundaria incompleto"

	***************
	***edus2c_ci***
	***************
	gen edus2c_ci = (aedu_ci == 12)
	replace edus2c_ci = . if aedu_ci == . 
	label variable edus2c_ci "2do ciclo de la secundaria completo"


	***************
	***edupre_ci***
	***************
	gen edupre_ci=.
	label variable edupre_ci "Educacion preescolar"


	***************
	***asispre_ci**
	***************
	g asispre_ci=.
	la var asispre_ci "Asiste a educacion prescolar"
	
	**************
	***eduac_ci***
	**************
	gen eduac_ci=.
	replace eduac_ci = 1 if p10a == 9 | p10a == 10 
	replace eduac_ci = 0 if p10a == 8
	label variable eduac_ci "Superior universitario vs superior no universitario"

	***************
	***asiste_ci***
	***************
	gen asiste_ci=(p07==1)
	replace asiste_ci = . if p07 == .
	label variable asiste_ci "Asiste actualmente a la escuela"

	**************
	*pqnoasis_ci**
	**************
	
    recode p09 (14=17)	// JV: No tiene opción "Falta de recursos tecnológicos". Recoded accordingly
	
	gen pqnoasis_ci=p09
	label var pqnoasis_ci "Razones para no asistir a la escuela"

	label def pqnoasis_ci 1"edad" 2"terminó sus estudios" 3"falta recursos económicos" 4"fracaso escolar" 5"por trabajo" 6"temor a los maestros" 7"enfermedad o discapacidad" 8"quehaceres del hogar" 9"familia no permite" 10"no hay establecimientos educativos" 11"no está interesado" 12"por embarazo" 13"por falta de cupo" 14"Temor a los compañeros" 15"Cuidar a los hijos" 16"Falta de recursos tecnologicos" 17"Otra razón" 
	label val pqnoasis_ci pqnoasis_ci
		
	**************
    *pqnoasis1_ci*
    **************
	
    g pqnoasis1_ci = 1 if p09 == 3
    replace pqnoasis1_ci = 2 if p09 == 5
    replace pqnoasis1_ci = 3 if p09 == 7  | p09 == 9
    replace pqnoasis1_ci = 4 if p09 == 11
    replace pqnoasis1_ci = 5 if p09 == 8  | p09 == 12 | p09 == 15
    replace pqnoasis1_ci = 6 if p09 == 2
    replace pqnoasis1_ci = 7 if p09 == 1 
    replace pqnoasis1_ci = 8 if p09 == 10 | p09 == 13
    replace pqnoasis1_ci = 9 if p09 == 4 | p09 == 6 | p09 == 14 | p09 == 16 | p09 == 17

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

	drop nivinst anoinst 

	**********************************
	**** VARIABLES DE LA VIVIENDA ****
	**********************************
	
****************
***aguared_ch***
****************
gen aguared_ch =0
replace aguared_ch=1 if vi10==1
replace aguared_ch=. if vi10==.
label var aguared_ch "Acceso a fuente de agua por red"


*****************
*aguafconsumo_ch*
*****************
gen aguafconsumo_ch = 0

*****************
*aguafuente_ch*
*****************
gen aguafuente_ch = 0
replace aguafuente_ch = 1 if vi10==1
replace aguafuente_ch = 2 if vi10==2
replace aguafuente_ch = 6 if vi10==4
replace aguafuente_ch = 8 if vi10==6
replace aguafuente_ch = 10 if (vi10==3|vi10==5| vi10==7)


*************
*aguadist_ch*
*************
gen aguadist_ch=0
replace aguadist_ch=1 if vi10a==1
replace aguadist_ch=2 if vi10a==2
replace aguadist_ch=3 if vi10a==3

**************
*aguadisp1_ch*
**************
gen aguadisp1_ch = 9 


**************
*aguadisp2_ch*
**************
gen aguadisp2_ch = 9



*************
*aguamala_ch*  Altered
*************
gen aguamala_ch = 2
replace aguamala_ch = 0 if aguafuente_ch<=7 
replace aguamala_ch = 1 if aguafuente_ch>7 & aguafuente_ch!=10

*****************
*aguamejorada_ch*  Altered
*****************
gen aguamejorada_ch = 2
replace aguamejorada_ch = 0 if aguafuente_ch>7 & aguafuente_ch!=10
replace aguamejorada_ch = 1 if aguafuente_ch<=7 
*label var aguamejorada_ch "= 1 si la fuente de agua es mejorada"

*****************
***aguamide_ch***
*****************
gen aguamide_ch=.
label var aguamide_ch "Usan medidor para pagar consumo de agua"

*****************
*bano_ch         *  Altered
*****************
gen bano_ch=6
replace bano_ch=0 if vi09==5
replace bano_ch=1 if vi09==1
replace bano_ch=2 if vi09==2
replace bano_ch=3 if vi09==3 
replace bano_ch=6 if vi09==4

***************
***banoex_ch***
***************
gen banoex_ch=.
label var banoex_ch "El servicio sanitario es exclusivo del hogar"

*****************
*banomejorado_ch*  Altered
*****************
gen banomejorado_ch= 2
replace banomejorado_ch =1 if bano_ch<=3 & bano_ch!=0
replace banomejorado_ch =0 if (bano_ch ==0 | bano_ch>=4) & bano_ch!=6



************
*sinbano_ch*
************
gen sinbano_ch = 3
replace sinbano_ch = 0 if vi09!=5
*label var sinbano_ch "= 0 si tiene baño en la vivienda o dentro del terreno"

*************
*aguatrat_ch*
*************
gen aguatrat_ch =9


	************
	***luz_ch***
	************
	gen luz_ch=(vi12==1 | vi12==2)
	label var luz_ch  "La principal fuente de iluminación es electricidad"
	
	****************
	***luzmide_ch***
	****************
	gen luzmide_ch=.
	label var luzmide_ch "Usan medidor para pagar consumo de electricidad"

	****************
	***combust_ch***
	****************
	gen combust_ch=0 if vi08>=1 & vi08<=4
	replace combust_ch=1 if  vi08==1 | vi08==3 
	label var combust_ch "Principal combustible gas o electricidad" 
	


	*************
	***des1_ch***
	*************

	gen des1_ch=.
	replace des1_ch=0 if bano_ch==0
	replace des1_ch=1 if vi09==1 | vi09==2
	replace des1_ch=2 if vi09==3 | vi09==4
	label var des1_ch "Tipo de desague según unimproved de MDG"
	label def des1_ch 0"No tiene servicio sanitario" 1"Conectado a red general o cámara séptica"
	label def des1_ch 2"Letrina o conectado a pozo ciego" 3"Desemboca en río o calle", add
	label val des1_ch des1_ch
	
	
	*************
	***des2_ch***
	*************
	gen des2_ch=.
	replace des2_ch=0 if bano_ch==0
	replace des2_ch=1 if vi09==1 | vi09==2 | vi09==3 | vi09==4
	label var des2_ch "Tipo de desague sin incluir definición MDG"
	label def des2_ch 0"No tiene servicio sanitario" 1"Conectado a red general, cámara séptica, pozo o letrina"
	label def des2_ch 2"Cualquier otro caso", add
	label val des2_ch des2_ch
	
	
	*************
	***piso_ch***
	*************
	gen piso_ch=1 		if vi04a==1 |vi04a==2| vi04a==3 | vi04a==4 
	replace piso_ch=0 	if vi04a==7
	replace piso_ch=2 	if vi04a==5| vi04a==6 | vi04a==8 	
	replace piso_ch=. 	if vi04a==.
	label var piso_ch "Materiales de construcción del piso"  
	label def piso_ch 0"Piso de tierra" 1"Materiales permanentes" 2"Otros materiales"
	label val piso_ch piso_ch
	
	
	**************
	***pared_ch***
	**************
	gen pared_ch=0 		if vi05a==5 |vi05a==6 |vi05a==7
	replace pared_ch=1	if vi05a>=1 & vi05a<=4
	label var pared_ch "Materiales de construcción de las paredes"
	label def pared_ch 0"No permanentes" 1"Permanentes"
	label val pared_ch pared_ch
	
	
	**************
	***techo_ch***
	**************
	gen techo_ch=0 		if  vi03a==5 |vi03a==6
	replace techo_ch=1	if  vi03a>=1 & vi03a<=4
	label var techo_ch "Materiales de construcción del techo"
	label def techo_ch 0"No permanentes" 1"Permanentes"
	label val techo_ch techo_ch
	
	**************
	***resid_ch***
	**************
	gen resid_ch =0    if vi13==1 | vi13==2
	replace resid_ch=1 if vi13==4
	replace resid_ch=2 if vi13==3
	replace resid_ch=3 if vi13==5
	replace resid_ch=. if vi13==.
	label var resid_ch "Método de eliminación de residuos"
	label def resid_ch 0"Recolección pública o privada" 1"Quemados o enterrados"
	label def resid_ch 2"Tirados a un espacio abierto" 3"Otros", add
	label val resid_ch resid_ch
	

	
	*************
	***dorm_ch***
	*************
	*Dado que hay hogares que reportan 0 habitaciones exclusivas para dormir, pues la vivienda está constituída por
	*un sólo ambiente, a estos hogares se les imputa 1 habitación. A los hogares que dicen no tener cuartos exclusivos 
	*para dormir, pero que viven en vivienda de 2, 3 y 4 habitaciones se les asigna missing

	gen dorm_ch=vi07
	replace dorm_ch=1 if vi07==0 & vi06==1
	replace dorm_ch=. if vi07==0 & vi06>1
	label var dorm_ch "Habitaciones para dormir"
	
	****************
	***cuartos_ch***
	****************
	gen cuartos_ch=vi06 if vi06<99
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
	replace telef_ch=1 if eq1501==1
	replace telef_ch=. if eq1501==.
	label var telef_ch "El hogar tiene servicio telefónico fijo"
	
	***************
	***refrig_ch***
	***************
	gen refrig_ch=0
	replace refrig_ch=1 if  eq0101==1
	replace refrig_ch=. if  eq0101==.
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
	replace auto_ch=1 if eq1401==1
	replace auto_ch=. if eq1401==.
	label var auto_ch "El hogar posee automovil particular"

	
	**************
	***compu_ch***
	**************
	gen compu_ch=0
	replace compu_ch=1 if eq0401 ==1 | eq1801==1 
	replace compu_ch=. if eq0401==. & eq1801==.
	label var compu_ch "El hogar posee computador"
	
	
	*****************
	***internet_ch***
	*****************
	gen internet_ch=0
	replace internet_ch=1 if vi21==1 
	replace internet_ch=. if vi21==. 
	label var internet_ch "El hogar posee conexión a Internet"
	

	************
	***cel_ch***
	************
	gen cel_ch=0
	replace cel_ch=1 if eq1701==1 
	replace cel_ch=. if eq1701==. 
	label var cel_ch "El hogar tiene servicio telefonico celular"
	
		
	**************
	***vivi1_ch***
	**************
	gen vivi1_ch=1 		if vi02==1
	replace vivi1_ch=2 	if vi02==2
	replace vivi1_ch=3 	if vi02==3 | vi02==4 | vi02==5 | vi02==6 | vi02==7
	replace vivi1_ch=. 	if vi02==.
	label var vivi1_ch "Tipo de vivienda en la que reside el hogar"
	label def vivi1_ch 1"Casa" 2"Departamento" 3"Otros"
	label val vivi1_ch vivi1_ch
	
	
	**************
	***vivi2_ch***
	**************
	gen vivi2_ch=0
	replace vivi2_ch=1 if vi02==1 | vi02==2
	replace vivi2_ch=. if vi02==.
	label var vivi2_ch "La vivienda es casa o departamento"
		
	*****************
	***viviprop_ch***
	*****************
	gen viviprop_ch=.
	replace viviprop_ch = 0 if vi14==1 | vi14==2
	replace viviprop_ch = 1 if vi14==4	
	replace viviprop_ch = 2 if vi14==3
	replace viviprop_ch = 3 if vi14==5
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
	
	gen migrante_ci=(p15aa==3) if p15aa!=.
	label var migrante_ci "=1 si es migrante"
	
	**********************
	*** migantiguo5_ci ***
	**********************
	
	gen migantiguo5_ci=.
	label var migantiguo5_ci "=1 si es migrante antiguo (5 anos o mas)"
	/* Encuesta pregunta sobre tiempo que lleva viviendo en esa ciudad */
		
	**********************
	*** migrantelac_ci ***
	**********************
	
	gen migrantelac_ci=(inlist(p15ab,32,44,52,68,76,84,152,170,188,214,222,320,328,332,340,388,484,558,591,600,604,740,780,858,862) & migrante_ci==1) if migrante_ci!=.
	label var migrantelac_ci "=1 si es migrante proveniente de un pais LAC"
	/* Fuente https://aplicaciones2.ecuadorencifras.gob.ec/SIN/descargas/cu.pdf */
	
	**********************
	*** migrantiguo5_ci ***
	**********************
	 
	gen migrantiguo5_ci=.
	label var migrantiguo5_ci "=1 si es migrante antiguo (5 anos o mas)"
		
	**********************
	*** miglac_ci ***
	**********************
	
	gen miglac_ci=(inlist(p15ab,32,44,52,68,76,84,152,170,188,214,222,320,328,332,340,388,484,558,591,600,604,740,780,858,862) & migrante_ci==1) if migrante_ci!=.
	replace miglac_ci = 0 if !inlist(p15ab,32,44,52,68,76,84,152,170,188,214,222,320,328,332,340,388,484,558,591,600,604,740,780,858,862) & migrante_ci==1
	replace miglac_ci = . if migrante_ci==0
	label var miglac_ci "=1 si es migrante proveniente de un pais LAC"
	
	
******************************
* Variables SPH - PMTC y PNC *
******************************

* PTMC: p75 Bono de desarrollo humano (incluye vejez)
* PNC: p76 Monto que recibió por el bono de desarrollo humano

* Ingreso del hogar
egen ingreso_total = rowtotal(ylm_ci ylnm_ci ynlm_ci ynlnm_ci), missing
bys idh_ch: egen y_hog = sum(ingreso_total)

* Ingreso por transferencias
bys idh_ch: egen ing_ptmc = sum(p76)
replace ing_ptmc=. if y_hog==.

* Ingreso per capita neto (sin lo recibido por las transferencias)
gen y_pc_net = (y_hog - ing_ptmc) / nmiembros_ch

* Beneficiarios
gen percibe_ptmc_ci = (p75==1) // si recibe el bono de desarrollo humano 
bys idh_ch: egen ptmc_ch=max(percibe_ptmc_ci) 
replace ptmc_ch = (percibe==1 | (ing_ptmc>0 & ing_ptmc!=.)) 

* Adultos mayores 
gen mayor64_ci=(edad>64 & edad!=.)
	

lab def ptmc_ch 1 "Beneficiario PTMC" 0 "No beneficiario PTMC"
lab val ptmc_ch ptmc_ch

/*_____________________________________________________________________________________________________*/
* Asignación de etiquetas e inserción de variables externas: tipo de cambio, Indice de Precios al 
* Consumidor (2011=100), Paridad de Poder Adquisitivo (PPA 2011),  líneas de pobreza
/*_____________________________________________________________________________________________________*/


do "$gitFolder\armonizacion_microdatos_encuestas_hogares_scl\_DOCS\\Labels&ExternalVars_Harmonized_DataBank.do"

*_____________________________________________________________________________________________________*

*  Pobres extremos, pobres moderados, vulnerables y no pobres 
* con base en ingreso neto (Sin transferencias)
* y líneas de pobreza internacionales
gen     grupo_int = 1 if (y_pc_net<lp31_ci)
replace grupo_int = 2 if (y_pc_net>=lp31_ci & y_pc_net<(lp31_ci*1.6))
replace grupo_int = 3 if (y_pc_net>=(lp31_ci*1.6) & y_pc_net<(lp31_ci*4))
replace grupo_int = 4 if (y_pc_net>=(lp31_ci*4) & y_pc_net<.)

tab grupo_int, gen(gpo_ingneto)

* Crear interacción entre recibirla la PTMC y el gpo de ingreso
gen ptmc_ingneto1 = 0
replace ptmc_ingneto1 = 1 if ptmc_ch == 1 & gpo_ingneto1 == 1

gen ptmc_ingneto2 = 0
replace ptmc_ingneto2 = 1 if ptmc_ch == 1 & gpo_ingneto2 == 1

gen ptmc_ingneto3 = 0
replace ptmc_ingneto3 = 1 if ptmc_ch == 1 & gpo_ingneto3 == 1

gen ptmc_ingneto4 = 0
replace ptmc_ingneto4 = 1 if ptmc_ch == 1 & gpo_ingneto4 == 1

lab def grupo_int 1 "Pobre extremo" 2 "Pobre moderado" 3 "Vulnerable" 4 "No pobre"
lab val grupo_int grupo_int

/*_____________________________________________________________________________________________________*/
* Verificación de que se encuentren todas las variables armonizadas 
/*_____________________________________________________________________________________________________*/

order region_BID_c region_c pais_c anio_c mes_c zona_c factor_ch	idh_ch	idp_ci	factor_ci upm_ci estrato_ci sexo_ci edad_ci ///
afroind_ci afroind_ch afroind_ano_c dis_ci dis_ch relacion_ci civil_ci jefe_ci nconyuges_ch nhijos_ch notropari_ch notronopari_ch nempdom_ch ///
clasehog_ch nmiembros_ch miembros_ci nmayor21_ch nmenor21_ch nmayor65_ch nmenor6_ch	nmenor1_ch	condocup_ci ///
categoinac_ci nempleos_ci emp_ci antiguedad_ci	desemp_ci cesante_ci durades_ci	pea_ci desalent_ci subemp_ci ///
tiempoparc_ci categopri_ci categosec_ci rama_ci spublico_ci tamemp_ci cotizando_ci instcot_ci	afiliado_ci ///
formal_ci tipocontrato_ci ocupa_ci horaspri_ci horastot_ci	pensionsub_ci pension_ci tipopen_ci instpen_ci	ylmpri_ci nrylmpri_ci ///
tcylmpri_ci ylnmpri_ci ylmsec_ci ylnmsec_ci	ylmotros_ci	ylnmotros_ci ylm_ci	ylnm_ci	ynlm_ci	ynlnm_ci ylm_ch	ylnm_ch	ylmnr_ch  ///
ynlm_ch	ynlnm_ch ylmhopri_ci ylmho_ci rentaimp_ch autocons_ci autocons_ch nrylmpri_ch tcylmpri_ch remesas_ci remesas_ch	ypen_ci	ypensub_ci ///
salmm_ci tc_c ipc_c lp19_c lp31_c lp5_c lp_ci lpe_ci aedu_ci eduno_ci edupi_ci edupc_ci	edusi_ci edusc_ci eduui_ci eduuc_ci	edus1i_ci ///
edus1c_ci edus2i_ci edus2c_ci edupre_ci eduac_ci asiste_ci pqnoasis_ci pqnoasis1_ci	repite_ci repiteult_ci edupub_ci ///
aguared_ch aguafconsumo_ch aguafuente_ch aguadist_ch aguadisp1_ch aguadisp2_ch aguamala_ch aguamejorada_ch aguamide_ch bano_ch banoex_ch banomejorado_ch sinbano_ch aguatrat_ch luz_ch luzmide_ch combust_ch des1_ch des2_ch piso_ch  ///
pared_ch techo_ch resid_ch dorm_ch cuartos_ch cocina_ch telef_ch refrig_ch freez_ch auto_ch compu_ch internet_ch cel_ch ///
vivi1_ch vivi2_ch viviprop_ch vivitit_ch vivialq_ch	vivialqimp_ch migrante_ci migantiguo5_ci migrantelac_ci, first

/*Homologar nombre del identificador de ocupaciones (isco, ciuo, etc.) y de industrias y dejarlo en base armonizada 
para análisis de trends (en el marco de estudios sobre el futuro del trabajo)*/
rename p41 codocupa
rename p40 codindustria

compress

saveold "`base_out'", version(12) replace


log close





	
