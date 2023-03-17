* (Versión Stata 13)
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

local PAIS BRA
local ENCUESTA PNAD
local ANO "2013"
local ronda m9 
local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
                        
capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Brasil
Encuesta: PNAD
Round: m9
Autores: Marcela Rubio mrubio@iadb.org | marcelarubio28@gmail.com
 Daniela Zuluaga E-mail: danielazu@iadb.org, da.zuluaga@hotmail.com - Octubre de 2017
Última modificación: Cesar Lins - Marzo 2021

							
****************************************************************************/
****************************************************************************/


use `base_in', clear



								********************************
								**** ARMONIZACIÓN PNAD 2013 **** 
								********************************
				*************************
				***VARIABLES DEL HOGAR***
				*************************
*****************
*** region_ci ***
*****************
*generacion "region_c" proyecto maps America.		

gen region_c = uf
destring region_c, replace
label define region_c ///
11 "Rondônia" ///
12 "Acre" ///
13 "Amazonas" ///
14 "Roraima" ///
15 "Pará" ///
16 "Amapá" ///
17 "Tocantins" ///
21 "Maranhão" ///
22 "Piauí" ///
23 "Ceará" ///
24 "Rio Grande do Norte" ///
25 "Paraíba" ///
26 "Pernambuco" ///
27 "Alagoas" ///
28 "Sergipe" ///
29 "Bahia" ///
31 "Minas Gerais" ///
32 "Espírito Santo" ///
33 "Rio de Janeiro" ///
35 "São Paulo" ///
41 "Paraná" ///
42 "Santa Catarina" ///
43 "Rio Grande do Sul" ///
50 "Mato Grosso do Sul" ///
51 "Mato Grosso" ///
52 "Goiás" ///
53 "Distrito Federal"
label value region_c region_c
label var region_c "division politico-administrativa"

************************
*** region según BID ***
************************
gen region_BID_c=4 
label var region_BID_c "Regiones BID"
label define region_BID_c 1 "Centroamérica_(CID)" 2 "Caribe_(CCB)" 3 "Andinos_(CAN)" 4 "Cono_Sur_(CSC)"
label value region_BID_c region_BID_c
   
   
***************
***factor_ch***
***************
gen factor_ch=v4732
label variable factor_ch "Factor de expansión del hogar"

***************
****idh_ch*****
***************
sort uf v0102 v0103 v0403
egen idh_ch=group(uf v0102 v0103 v0403)
label variable idh_ch "ID del hogar"

*************
****idp_ci****
**************
gen idp_ci=v0301
label variable idp_ci "ID de la persona en el hogar"

**********
***zona***
**********
gen zona_c=1 if v4728>=1 & v4728<=3
replace zona_c=0 if v4728>=4 & v4728<=8
label variable zona_c "Zona del pais"
label define zona_c 1 "Urbana" 0 "Rural"
label value zona_c zona_c

************
****pais****
************
gen str3 pais_c="BRA"
label variable pais_c "País"

**********
***anio***
**********
gen anio_c=2013
label variable anio_c "Anio de la encuesta"

*********
***mes***
*********
gen mes_c=9
label variable mes_c "mes de la encuesta"

*****************
***relacion_ci***
*****************
gen relacion_ci=v0402
replace relacion_ci=5 if v0402==5| v0402==6| v0402==8
replace relacion_ci=6 if v0402==7
label var relacion_ci "Relación de parentesco con el  jefe de hogar"
label define relacion_ci 1 "Jefe" 2 "Conyuge" 3 "Hijo" 4 "Otros Parientes" 5 "Otros no Parientes" 6 "Servicio Domestico"
label values relacion_ci relacion_ci

				****************************
				***VARIABLES DEMOGRÁFICAS***
				****************************
				
***************
***factor_ci***
***************
gen factor_ci=v4729
label variable factor_ci "Factor de expansión de personas"

	***************
	***upm_ci***
	***************
gen upm_ci=upa
	***************
	***estrato_ci***
	***************
gen estrato_ci=v4602

**********
***sexo***
**********
gen sexo_ci=1 if v0302==2
replace sexo_ci=2 if v0302==4
label var sexo_ci "Sexo del individuo" 
label define sexo_ci 1 "Hombre" 2 "Mujer"
label value sexo_ci sexo_ci

**********
***edad***
**********
gen edad_ci=v8005
label variable edad_ci "Edad del individuo"


**************
***civil_ci***
**************

* MGR Nov 2015: correccion en sintaxis no se incluian a los que indican vivir en compañia de conyugue

gen civil_ci=.
replace civil_ci=1 if v4011==0
replace civil_ci=2 if v4111==1 | v4011==1
replace civil_ci=3 if v4011==3 |v4011==5
replace civil_ci=4 if v4011==7
label variable civil_ci "Estado civil"
label define civil_ci 1 "Soltero" 2 "Union formal o informal"
label define civil_ci 3 "Divorciado o separado" 4 "Viudo" , add
label value civil_ci civil_ci

/*
gen civil_ci=.
replace civil_ci=1 if v4011==0
replace civil_ci=2 if v4011==1
replace civil_ci=3 if v4011==3 | v4011==5
replace civil_ci=4 if v4011==7
label variable civil_ci "Estado civil"
label define civil_ci 1 "Soltero" 2 "Union formal o informal"
label define civil_ci 3 "Divorciado o separado" 4 "Viudo" , add
label value civil_ci civil_ci
*/

*************
***jefe_ci***
*************
gen jefe_ci=(v0402==1)
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
by idh_ch, sort: egen nempdom_ch=sum(relacion_ci==6)
label variable nempdom_ch "Numero de empleados domesticos"


*****************
***clasehog_ch***
*****************
gen byte clasehog_ch=0
replace  clasehog_ch=1   if nhijos_ch==0 & nconyuges_ch==0 & notropari_ch==0 & notronopari_ch==0   /*Unipersonal*/
replace  clasehog_ch=2   if nhijos_ch>0 & notropari_ch==0 & notronopari_ch==0                      /*Nuclear (child with or without spouse but without other relatives)*/
replace  clasehog_ch=2   if nhijos_ch==0 & nconyuges_ch>0 & notropari_ch==0 & notronopari_ch==0    /*Nuclear (spouse with or without children but without other relatives)*/
replace  clasehog_ch=3   if notropari_ch>0 & notronopari_ch==0                                     /*Ampliado*/
replace  clasehog_ch=4   if ((nconyuges_ch>0 | nhijos_ch>0 | notropari_ch>0) & (notronopari_ch>0)) /*Compuesto (some relatives plus non relative)*/
replace  clasehog_ch=5   if nhijos_ch==0 & nconyuges_ch==0 & notropari_ch==0 & notronopari_ch>0    /*Corresidente*/
label variable clasehog_ch "Tipo de hogar"
label define clasehog_ch 1 " Unipersonal" 2 "Nuclear" 3 "Ampliado" 
label define clasehog_ch 4 "Compuesto" 5 " Corresidente", add
label value clasehog_ch clasehog_ch

****************
***miembros_ci***
****************
gen miembros_ci=(relacion_ci<5)
label variable miembros_ci "Miembro del hogar"

******************
***nmiembros_ch***
******************
by idh_ch, sort: egen nmiembros_ch=sum(relacion_ci>=1 & relacion_ci<=4) if miembros_ci==1
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

*******************************************************
***           VARIABLES DE DIVERSIDAD               ***
*******************************************************				
* Maria Antonella Pereira & Nathalia Maya - Marzo 2021	
												
	***************
	***afroind_ci***
	***************
**Pregunta: COR OU RACA? (v0404) (BRANCA 2, PRETA 4, AMARELA 6, PARDA 8, INDIGENA 0, IGNORADA 9) 

gen afroind_ci=. 
replace afroind_ci=1  if v0404==0
replace afroind_ci=2 if v0404 == 4 | v0404 == 8 
replace afroind_ci=3 if v0404 == 2 | v0404 == 6 
replace afroind_ci=. if v0404==9


	***************
	***afroind_ch***
	***************
gen afroind_jefe= afroind_ci if relacion_ci==1
egen afroind_ch  = min(afroind_jefe), by(idh_ch) 

drop afroind_jefe 

	*******************
	***afroind_ano_c***
	*******************
gen afroind_ano_c=1990


	*******************
	***dis_ci***
	*******************
gen dis_ci=. 

	*******************
	***dis_ch***
	*******************
gen dis_ch=. 



		************************************
		*** VARIABLES DEL MERCADO LABORAL***
		************************************
****************
****condocup_ci*
****************
gen condocup_ci=.
replace condocup_ci=1 if (v9001==1 | v9002==2 | v9003==1 | v9004==2)
replace condocup_ci=2 if  v9004==4 & (v9115==1 & (v9119>=1 & v9119<=8)) /*tomaron alguna providencia en la semana de referencia*/
replace condocup_ci=3 if  condocup_ci!=1 & condocup_ci!=2
replace condocup_ci=4 if edad_ci<10
label define condocup_ci 1"ocupados" 2"desocupados" 3"inactivos" 4"menor 10 años"
label value condocup_ci condocup_ci
label var condocup_ci "Condicion de ocupacion utilizando definicion del pais"

/*
Definiciones:
* População ocupada: Aquelas pessoas que, num determinado período de referência,
trabalharam ou tinham trabalho mas não trabalharam (por exemplo, pessoas em férias).

* População Desocupada: aquelas pessoas que não tinham trababalho, num determinado 
período de referência, mas estavam dispostas a trabalhar, e que, para isso, tomaram
alguma providência efetiva (consultando pessoas, jornais, etc.).

População Não Economicamente Ativa: pessoas não classificadas como ocupadas ou 
desocupadas

PET: >=10 años de edad
*/

****************
*afiliado_ci****
****************

gen afiliado_ci=.
label var afiliado_ci "Afiliado a la Seguridad Social"

****************
*cotizando_ci***
****************
gen cotizando_ci=0     if condocup_ci==1 | condocup_ci==2 
replace cotizando_ci=1 if (v9059==1 | v9099==1 | v9103==1 | v9120==2) & cotizando_ci==0
label var cotizando_ci "Cotizante a la Seguridad Social"

gen cotizapri_ci=0     if condocup_ci==1 | condocup_ci==2 
replace cotizapri_ci=1 if (v9059==1) & cotizando_ci==0 
label var cotizapri_ci "Cotizante a la Seguridad Social por su trabajo principal"

gen cotizasec_ci=0     if condocup_ci==1 | condocup_ci==2 
replace cotizasec_ci=1 if (v9099==1) & cotizando_ci==0 
label var cotizasec_ci "Cotizante a la Seguridad Social por su trabajo secundario"

gen cotizaotros_ci=0     if condocup_ci==1 | condocup_ci==2 
replace cotizaotros_ci=1 if (v9103==1 | v9120==2) & cotizando_ci==0 
label var cotizaotros_ci "Cotizante a la Seguridad Social por otro trabajos o por aporte privado"

****************
*instpen_ci*****
****************
gen instpen_ci=.
label var instpen_ci "Institucion proveedora de la pension - variable original de cada pais" 

********************
*** instcot_ci *****
********************
gen instcot_ci=.
label var instcot_ci "institución a la cual cotiza"

*****************
*tipocontrato_ci*
*****************
gen tipocontrato_ci=. /*solo se pregunta si tiene o no contrato*/
label var tipocontrato_ci "Tipo de contrato segun su duracion en act principal"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci

*************
**pension_ci*
*************

sum v1252 v1255 v1258 v1261
foreach var of varlist v1252 v1255 v1258 v1261 {
replace `var'=. if `var'>=999999
}

gen pension_ci=0 
replace pension_ci=1 if (v1252>0 & v1252!=.) | (v1255>0 & v1255!=.) | (v1258>0 & v1258!=.) | (v1261>0 & v1261!=.) /*A todas las per mayores de diez años*/
label var pension_ci "1=Recibe pension contributiva"
 
*************
*ypen_ci*
*************

egen ypen_ci=rsum (v1252 v1255 v1258 v1261)
replace ypen_ci=. if ypen_ci<=0
label var ypen_ci "Valor de la pension contributiva"

***************
*pensionsub_ci*
***************
/*DZ Septiembre 2017- Creacion de la variable  pension subsidiada*
http://dds.cepal.org/bdps/programa/?id=43
segun la fuente, el monto bpc para adultos mayores fue de 678 reales. Se encuentran beneficiarios con dicho monto*/
gen pensionsub_ci=(v1273==678)
label var pensionsub_ci "1=recibe pension subsidiada / no contributiva"


*****************
**  ypensub_ci  *
*****************
/*DZ Septiembre 2017- Creacion de la variable valor de la pension subsidiada*
http://dds.cepal.org/bdps/programa/?id=43
segun la fuente, el monto bpc para adultos mayores fue de 678 reales. Se encuentran beneficiarios con dicho monto*/
gen ypensub_ci=v1273 if v1273==678
label var ypensub_ci "Valor de la pension subsidiada / no contributiva"



*************
*cesante_ci* 
*************
generat cesante_ci=0 if condocup_ci==2
replace cesante_ci=1 if (v9067==1 | v9106==2) & condocup_ci==2
label var cesante_ci "Desocupado - definicion oficial del pais"

*****************
*region /area ***
*****************
gen region=.	
replace region=1	if region_c>=11 & region_c<=17
replace region=2	if region_c>=21 & region_c<=29
replace region=3	if region_c>=31 & region_c<=35
replace region=4	if region_c>=41 & region_c<=43
replace region=5	if region_c>=50 & region_c<=53
label define region 1"norte" 2"Nordeste" 3"Sudeste/leste" 4"sul" 5"Centro_Oeste"
label value region region
label var region "distribución regional del país"

gen area=.
replace area=1 if zona_c==1
replace area=2 if zona_c==0
replace area=3 if v4727==1
*label define area 1"urbana" 2"rural" 3"metropolitana" 
*label value area area
*label var area "area del pais"

*********
*lp_ci***
*********

*MR: se agregan líneas de pobreza y pobreza extrema nacionales

gen lp_ci=.			
replace lp_ci= 281.41 	if region_c==33	& area==3                /*Rio de janeiro-metropolitano*/	
replace lp_ci= 238.77 	if region_c==33	& area==1	             /*Rio de janeiro-urbano*/
replace lp_ci= 214.89 	if region_c==33	& area==2	             /*Rio de janeiro-rural*/
replace lp_ci= 283.11 	if region_c==35	& area==3	             /*Sao Paulo-metropolitano*/
replace lp_ci= 250.71 	if region_c==35	& area==1	             /*Sao paulo-urbano*/
replace lp_ci= 204.66 	if region_c==35	& area==2	             /*Sao paulo-rural*/
replace lp_ci= 313.81 	if region==4	& area==3	& region_c==43 /*Porto alegre: sur-metropolitana-rio grande de sul*/
replace lp_ci= 259.24 	if region==4	& area==3	& region_c==41 /*curitiba:     sur-metropolitana-paraná*/
replace lp_ci= 247.30 	if region==4	& area==1            /*sur-urbana*/	
replace lp_ci= 225.13 	if region==4	& area==2            /*sur-rural */	
replace lp_ci= 223.42  	if region==2	& area==3	& region_c==23 /*Fortaleza:    noreste-metropolitana-ceará*/
replace lp_ci= 293.35 	if region==2	& area==3	& region_c==26 /*recife:       noreste-metropolitana-pernambuco*/
replace lp_ci= 276.29 	if region==2	& area==3	& region_c==29 /*salvador:     noreste-metropolitana-bahia*/
replace lp_ci= 252.41 	if region==2	& area==1            /*noreste-urbana*/	
replace lp_ci= 225.13 	if region==2	& area==2            /*noreste-rural*/	
replace lp_ci= 220.01 	if region==3	& area==3	& region_c==31 /*belo horizonte:sureste-metropolitana-minas gerais*/
replace lp_ci= 197.84 	if region==3	& area==1            /*sudeste-urbano*/	
replace lp_ci= 168.84 	if region==3	& area==2            /*sudeste-rural*/	
replace lp_ci= 250.71 	if region==1	& area==3	& region_c==15 /*belem: noreste-metropolitana-pará*/
replace lp_ci= 259.24 	if region==1	& area==1            /*norte-urbano*/	
replace lp_ci= 226.83 	if region==1	& area==2            /*norte-rural */	
replace lp_ci= 243.89 	if region_c==53	& area==3	             /*Distrito federal-metropolitana*/
replace lp_ci= 209.78 	if region==5	& area==1            /*centro oeste-urbano*/	
replace lp_ci= 184.19 	if region==5	& area==2            /*centro oeste-rural */	
label var lp_ci "Linea de pobreza oficial del pais"

***********
*lpe_ci ***
***********
gen lpe_ci=.

replace lpe_ci= 140.70 	if region_c==33	& area==3		    /*Rio de janeiro-metropolitano*/
replace lpe_ci= 119.38 	if region_c==33	& area==1		    /*Rio de janeiro-urbano*/
replace lpe_ci= 107.45 	if region_c==33	& area==2		    /*Rio de janeiro-rural*/
replace lpe_ci= 141.56 	if region_c==35	& area==3		    /*Sao Paulo-metropolitano*/
replace lpe_ci= 125.35 	if region_c==35	& area==1		    /*Sao paulo-urbano*/
replace lpe_ci= 102.33 	if region_c==35	& area==2		    /*Sao paulo-rural*/
replace lpe_ci= 156.91 	if region==4	& area==3	& region_c==43	/*Porto alegre: sur-metropolitana-rio grande de sul*/
replace lpe_ci= 129.62 	if region==4	& area==3	& region_c==41	/*curitiba:     sur-metropolitana-paraná*/
replace lpe_ci= 123.65 	if region==4	& area==1		/*sur-urbana*/
replace lpe_ci= 112.56 	if region==4	& area==2		/*sur-rural */
replace lpe_ci= 111.71 	if region==2	& area==3	& region_c==23	/*Fortaleza:    noreste-metropolitana-ceará*/
replace lpe_ci= 146.67 	if region==2	& area==3	& region_c==26	/*recife:       noreste-metropolitana-pernambuco*/
replace lpe_ci= 138.15 	if region==2	& area==3	& region_c==29	/*salvador:     noreste-metropolitana-bahia*/
replace lpe_ci= 126.21 	if region==2	& area==1		/*noreste-urbana*/
replace lpe_ci= 112.56 	if region==2	& area==2		/*noreste-rural*/
replace lpe_ci= 110.00 	if region==3	& area==3	& region_c==31	/*belo horizonte:sureste-metropolitana-minas gerais*/
replace lpe_ci= 98.92 	if region==3	& area==1		/*sudeste-urbano*/
replace lpe_ci= 84.42 	if region==3	& area==2		/*sudeste-rural*/
replace lpe_ci= 125.35 	if region==1	& area==3	& region_c==15	/*belem: noreste-metropolitana-pará*/
replace lpe_ci= 129.62 	if region==1	& area==1		/*norte-urbano*/
replace lpe_ci= 113.42 	if region==1	& area==2		/*norte-rural */
replace lpe_ci= 121.94 	if region_c==53	& area==3		    /*Distrito federal-metropolitana*/				
replace lpe_ci= 104.89 	if region==5	& area==1		/*centro oeste-urbano*/
replace lpe_ci= 92.10 	if region==5	& area==2		/*centro oeste-rural */
label var lpe_ci "Linea de indigencia oficial del pais"


*************
**salmm_ci***
*************
gen salmm_ci=678 
label var salmm_ci "Salario minimo legal"
* MR: Salario minimo aumentó de 2012 a 2013

*************
***tecnica_ci**
*************
gen tecnica_ci=. /*No se puede identificar educación técnica superior*/
label var tecnica_ci "=1 formacion terciaria tecnica"	

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
***formal_ci ***
****************
gen formal_ci=(cotizando_ci==1)

*****************
***desalent_ci***
*****************
gen desalent_ci=.
label var desalent_ci "Trabajadores desalentados"

*****************
***horaspri_ci***
*****************
*2014, 01 revision MLO

gen horaspri_ci=v9058
replace horaspri_ci= v0713 if (edad_ci>=5 & edad_ci<=9)
replace horaspri_ci=. if (v0713<0 & edad_ci>=5 & edad_ci<=9 ) | emp_ci==0
label var horaspri_ci "Horas trabajadas semanalmente en el trabajo principal"
*Nota: se incluye las horas de trabajo de los menroes. No afecta al sociometro xq filtra por edades.

*****************
***horastot_ci***
*****************

/*yl: creacion de este indicador, construir hacia atras*/
egen horastot_ci=rsum(horaspri_ci v9101 v9105)
replace horastot_ci = . if emp_ci==0 /*Necesitamos que sólo se fije en los empleados "adultos"*/
replace horastot_ci=. if (horaspri_ci==. & v9101==. & v9105==.) | horastot_ci>150
label var horastot_ci "Horas trabajadas semanalmente en todos los empleos"

***************
***subemp_ci***
***************
gen subemp_ci=. /*discutir si hay la prob de construirla con v9115*/
label var subemp_ci "Personas en subempleo por horas"

*******************
***tiempoparc_ci***
*******************
gen tiempoparc_ci=. /*discutir si hay la prob de construirla con v9115*/
label var tiempoparc_c "Personas que trabajan medio tiempo" 

******************
***categopri_ci***
******************
generate aux08 =.
replace  aux08 = 1 if v9008>=1 & v9008<=4
replace  aux08 = 3 if v9008>=5 & v9008<=7
replace  aux08 = 4 if v9008>=8 & v9008<=10
replace  aux08 = 5 if v9008==11
replace  aux08 = 6 if v9008==12
replace  aux08 = 7 if v9008==13

gen categopri_ci=.
replace categopri_ci=1 if  v9029==4 | aux08==4 | v0708==4 | v0711==4
replace categopri_ci=2 if  v9029==3 | aux08==3 | v0708==3 | v0711==3
replace categopri_ci=3 if (v9029>=1 & v9029<=2) | aux08==1 | (v0708>=1 & v0708<=2) | (v0711>=1 & v0711<=2)
replace categopri_ci=4 if (v9029>=5 & v9029<=8) | (aux08>=5 & aux08<=8) | (v0708>=5 & v0708<=8) | (v0711>=5 & v0711<=8)
replace categopri_ci=. if emp_ci!=1
label define categopri_ci 1"Patron" 2"Cuenta propia" 0"Otro"
label define categopri_ci 3"Empleado" 4" No remunerado" , add
label value categopri_ci categopri_ci
label variable categopri_ci "Categoria ocupacional"
drop aux08 
 
******************
***categosec_ci***
******************
generat categosec_ci=1 if v9092==4
replace categosec_ci=2 if v9092==3
replace categosec_ci=3 if v9092==1
replace categosec_ci=4 if v9092==5 |v9092==6
replace categosec_ci=. if emp_ci!=1 
label define categosec_ci 1"Patron" 2"Cuenta propia" 0"Otro" 
label define categosec_ci 3"Empleado" 4"No remunerado" , add
label value categosec_ci categosec_ci
label variable categosec_ci "Categoria ocupacional trabajo secundario"

*****************
***nempleos_ci***
*****************


generat nempleos_ci=1 if v9005==1
replace nempleos_ci=2 if (v9005>1 & v9005!=.)
label var nempleos_ci "Número de empleos" 


*****************
***spublico_ci***
*****************
gen spublico_ci=(v9032==4)
replace spublico_ci=. if v9032==9
label var spublico_ci "Personas que trabajan en el sector público"

**************
***ocupa_ci***
**************
gen ocupa_ci=.
replace ocupa_ci=1 if v4810==2 | v4810==3 & emp_ci==1
replace ocupa_ci=2 if v4810==1 & emp_ci==1
replace ocupa_ci=3 if v4810==4 & emp_ci==1
replace ocupa_ci=4 if v4810==6 & emp_ci==1
replace ocupa_ci=5 if v4810==5 & emp_ci==1
replace ocupa_ci=6 if v4810==7 & emp_ci==1
replace ocupa_ci=7 if v4810==8 & emp_ci==1
replace ocupa_ci=8 if v4810==9 & emp_ci==1 /* modifico esta linea, revisar hacia atras */
replace ocupa_ci=9 if v4810==10 & emp_ci==1
label variable ocupa_ci "Ocupacion laboral"
label define ocupa_ci 1"profesional y tecnico" 2"director o funcionario sup" 3"administrativo y nivel intermedio"
label define ocupa_ci  4 "comerciantes y vendedores" 5 "en servicios" 6 "trabajadores agricolas", add
label define ocupa_ci  7 "obreros no agricolas, conductores de maq y ss de transporte", add
label define ocupa_ci  8 "FFAA" 9 "Otras ", add
label value ocupa_ci ocupa_ci

	

*************
***rama_ci***
*************
gen rama_ci=.
replace rama_ci=1 if v9907>1101 & v9907<5002
replace rama_ci=2 if v9907>=10000 & v9907<=14004 
replace rama_ci=3 if v9907>=15010 & v9907<=37000 
replace rama_ci=4 if v9907>=40010 & v9907<=41000 
replace rama_ci=5 if v9907>=45005 & v9907<=45999 
replace rama_ci=6 if v9907>=50010 & v9907<=55030
replace rama_ci=7 if v9907>=60010 & v9907<=64020
replace rama_ci=8 if v9907>=65000 & v9907<=70002
replace rama_ci=9 if v9907>=71010 & v9907<=99000
replace rama_ci=. if emp_ci==0
label var rama_ci "Rama de actividad"
label def rama_ci 1"Agricultura, caza, silvicultura y pesca" 2"Explotación de minas y canteras" 3"Industrias manufactureras"
label def rama_ci 4"Electricidad, gas y agua" 5"Construcción" 6"Comercio, restaurantes y hoteles" 7"Transporte y almacenamiento", add
label def rama_ci 8"Establecimientos financieros, seguros e inmuebles" 9"Servicios sociales y comunales", add
label val rama_ci rama_ci

****************
***durades_ci***
****************

gen durades_ci=.
*MLO 03,2014
* anterir definicion se refería a peridodo sin empleo, no a periodo en busqueda de empleo que es lo que intenta medir esta variable.
* no se puede construir la variable en numero de meses, se construye variable alternativa
* no se puede distinguir entre quienes buscan por mas de 305 dias.
/*
V9115	115	Tomou alguma providência para conseguir trabalho na semana de referência (1-si 2-no)
		
V9116	116	Tomou alguma providência para conseguir trabalho no período de captação de 23 dias (2 si 4 no)
		
V9117	117	Tomou alguma providência para conseguir trabalho no período de captação de 30 dias (1-si 2-no)
		
V9118	118	Tomou alguma providência para conseguir trabalho no período de captação de 305 dias (2 si 4 no)
	*/	
		
gen durades1_ci=1 if v9115==1
replace durades1_ci=2 if v9116==2 & durades1_ci!=1
replace durades1_ci=3 if v9117==1 & durades1_ci!=1 & durades1_ci!=2
replace durades1_ci=4 if v9118==2 & durades1_ci!=1 & durades1_ci!=2 & durades1_ci!=3
label var durades1_ci "Duracion de desempleo alternativa"
label def durades1_ci 1"1 semana" 2"8 a 23 dias" 3"24 a 30 dias" 4"31 a 305 dias"
label val durades1_ci durades1_ci
label variable durades_ci "Duracion del desempleo en meses"

*MGD:3/04/2015 durades variable continua 
gen aux1=v1091/12
egen durades_2=rsum(aux1 v1092) if condocup_ci==2 & (v9115==1 | v9116==2 | v9117==1 | v9118==2)
recode durades_2 (0=0.23) if condocup_ci==2 & (v9115==1 | v9116==2 | v9117==1 | v9118==2)


*******************
***antiguedad_ci***
*******************
gen aux2=v9612/12
egen antiguedad_ci=rsum(v9611 aux2) if emp_ci==1
replace antiguedad_ci=. if v9611==. & v9612==. 
drop aux*
label var antiguedad_ci "Antiguedad en la actividad actual en anios"



**************
***INGRESOS***
**************

***************
***ylmpri_ci***
***************
gen ylmpri_ci=v9532 
replace ylmpri_ci=v7122 if (edad_ci>=5 & edad_ci<=9) 
replace ylmpri_ci=. if v9532<0 | v9532>=999999 | v4814!=1
replace ylmpri_ci=. if  (edad_ci>=5 & edad_ci<=9) & (v7122<=0 | v7122>=999999 | emp_ci==0)
label var ylmpri_ci "Ingreso laboral monetario actividad principal" 

*****************
***nrylmpri_ci***
*****************
gen nrylmpri_ci=(ylmpri_ci==. & v4814==1)
replace nrylmpri_ci=. if v4814==2
label var nrylmpri_ci "Id no respuesta ingreso de la actividad principal"  

****************
***ylnmpri_ci***
****************
gen ylnmpri_ci=v9535 if edad_ci>=10
replace ylnmpri_ci=v7125 if edad_ci>=5 & edad_ci<=9
replace ylnmpri_ci=. if v9535<0 | v9535>=999999 | v4814!=1
replace ylnmpri_ci=. if (edad_ci>=5 & edad_ci<=9) & (v7125<0 | v7125>=999999 | emp_ci==0)
label var ylnmpri_ci "Ingreso laboral NO monetario actividad principal"  

***************
***ylmsec_ci***  
***************
gen ylmsec_ci=v9982 if edad_ci>=10
replace ylmsec_ci=. if v9982<0 | v9982>=999999 | v4814!=1
label var ylmsec_ci "Ingreso laboral monetario segunda actividad" 

****************
***ylnmsec_ci***
****************
gen ylnmsec_ci=v9985 if edad_ci>=10
replace ylnmsec_ci=. if v9985<0 | v9985>5500 | v4814!=1
label var ylnmsec_ci "Ingreso laboral NO monetario actividad secundaria"

*****************
***ylmotros_ci***
*****************
gen ylmotros_ci=v1022 if edad_ci>=10 
replace ylmotros_ci=. if v1022<0 | v1022>=999999 | v4814!=1
label var ylmotros_ci "Ingreso laboral monetario de otros trabajos" 

******************
***ylnmotros_ci***
******************
gen ylnmotros_ci=v1025 if edad_ci>=10
replace ylnmotros_ci=. if v1025<0 | v1025>=20000 | v4814!=1
label var ylnmotros_ci "Ingreso laboral NO monetario de otros trabajos" 

************
***ylm_ci***
************
egen ylm_ci=rsum(ylmpri_ci ylmsec_ci ylmotros_ci)
replace ylm_ci=. if ylmpri_ci==. & ylmsec_ci==. & ylmotros_ci==.
label var ylm_ci "Ingreso laboral monetario total"  

*************
***ylnm_ci***
*************
egen ylnm_ci=rsum(ylnmpri_ci ylnmsec_ci ylnmotros_ci)
replace ylnm_ci=. if ylnmpri_ci==. & ylnmsec_ci==. & ylnmotros_ci==.
label var ylnm_ci "Ingreso laboral NO monetario total"  

*************
***ynlm_ci***
*************
foreach var of varlist v1252 v1255 v1258 v1261 v1264 v1267 v1270 v1273 { 
replace `var'=. if `var'>=999999 | `var'<0
}

egen ynlm_ci=rsum(v1252 v1255 v1258 v1261 v1264 v1267 v1270 v1273) if edad_ci>=10
replace ynlm_ci=. if (v1252==. &  v1255==. &  v1258==. &  v1261==. &  v1264==. &  v1267==. & v1270==. & v1273==.) | ynlm_ci<0
label var ynlm_ci "Ingreso no laboral monetario"  

**************
***ylnm_ci***
**************
gen ynlnm_ci=.
label var ynlnm_ci "Ingreso no laboral no monetario" 

**************
*** ylm_ch ***
**************
by idh_ch, sort: egen ylm_ch=sum(ylm_ci) if miembros_ci==1
label var ylm_ch "Ingreso laboral monetario del hogar"


***************
*** ylnm_ch ***
***************
by idh_ch, sort: egen ylnm_ch=sum(ylnm_ci) if miembros_ci==1
label var ylnm_ch "Ingreso laboral no monetario del hogar"

****************
*** ylmnr_ch ***
****************
sort idh_ch 
by idh_ch: egen nrylmpri_ch=max(nrylmpri_ci) if miembros_ci==1
by idh_ch, sort: egen ylmnr_ch=sum(ylm_ci) if miembros_ci==1
replace ylmnr_ch=. if nrylmpri_ch==1
label var ylmnr_ch "Ingreso laboral monetario del hogar"

***************
*** ynlm_ch ***
***************
by idh_ch, sort: egen ynlm_ch=sum(ynlm_ci) if miembros_ci==1
label var ynlm_ch "Ingreso no laboral monetario del hogar"

**************
***ynlnm_ch***
**************
gen ynlnm_ch=.
label var ynlnm_ch "Ingreso no laboral no monetario del hogar"

*****************
***ylhopri_ci ***
*****************
*2015, 03 modificacion MLO
*gen ylmhopri_ci=ylmpri_ci/(horaspri_ci*4.2)
gen ylmhopri_ci=ylmpri_ci/(horaspri_ci*4.3)
replace ylmhopri_ci=. if ylmhopri_ci<=0
label var ylmhopri_ci "Salario monetario de la actividad principal" 

***************
***ylmho_ci ***
***************
gen ylmho_ci=.
label var ylmho_ci "Salario monetario de todas las actividades" 

********
***NA***
********
gen rentaimp_ch=.
label var rentaimp_ch "Rentas imputadas del hogar"

gen autocons_ci=.
label var autocons_ci "Autoconsumo reportado por el individuo"

gen autocons_ch=.
label var autocons_ch "Autoconsumo reportado por el hogar"

****************
***remesas_ci***
****************
gen remesas_ci=.
label var remesas_ci "Remesas mensuales reportadas por el individuo" 

****************
***remesas_ch***
****************
gen remesas_ch=.
label var remesas_ch "Remesas mensuales del hogar" 


					****************************
					***	VARIABLES EDUCATIVAS ***
					****************************
					
/* 
Notas construcción aedu_ci: 

En todos los casos Alfabetização de jóvens e adultos, Creche, 
Classe de alfabetização - CA, Maternal, jardim de infáncia, etc... imputan 0 
años de educación.

En todos los casos, para aquellos que asisten se le resta 1 al grado declarado 
para el computo de aedu_ci ya que el asistente no completó dicho año. 

Hay dos sistemas: antiguo y actual. El actual, Esino fundamental, abarca 9 
años de duración y siguen 3 de Esino medio. Para el antiguo, se computan 4 años 
obligatorios de Elementar, 4 años de Medio 1er ciclo y se completa con 
4 años de Medio 2do ciclo (científico, classico, etc..)

Bajo esta implementación de aedu_ci condicionar o no por v6030 o v6070 según 
corresponda arroja exáctamente los mismos resultados de 2007 a 2015.

Para los que declaran cursos no seriados (o en general todos aquellos que no 
declaran grado pero si nivel), en el caso de los asistentes, al no contar con 
información sobre los años de escolaridad aprobados se imputa por metodología 
el número máximo de años del nivel anterior. Para los no asistentes, el 
procedimiento es análogo salvo para aquellos en los que pueda discriminarse 
la finalización del curso en cuyo caso se asignan los años que correspondan.

La Educación Pre-Vestibular hace referencia a cursos de nivelación cortos 
(menores a un año) que son requisito de admisión para las universidades 
o servicio público. En esos casos, se imputan los 12 años de educación 
por secundario completo.

En la encuesta, para el nivel de postgrado (Maestría o Doctorado) no se pregunta 
el grado al que asisten, por lo que se imputan los años requeridos para 
acceder a ese nivel. En caso de haber finalizado dicha instancia, se imputan 
como aprobados el promedio de duración entre maestría y doctorado al no poder 
discriminar que individuo pertenece a cada nivel.
*/
		
**************
**asiste_ci***
**************
gen asiste_ci = (v0602 == 2)
label var asiste_ci "Personas que actualmente asisten a un centro de enseñanza"

***************
***edupub_ci***
***************
gen edupub_ci = (v6002 == 2)
replace edupub_ci=. if v6002 == .
label var  edupub_ci "Personas que asisten a centros de enseñanza públicos"

*************
***aedu_ci***
*************
gen nivel_asiste = v6003
gen grado_asiste = v0605
replace grado_asiste = 9 if grado_asiste == 0 // Novena serie se codifica como 0.
gen nivel_no_asiste = v6007
gen grado_no_asiste = v0610
replace grado_no_asiste = 9 if grado_no_asiste == 0
gen finalizo = v0611

gen aedu_ci =. 
label var aedu_ci "Anios de educacion"

* PARA LOS QUE ASISTEN:
***********************
* Creche, Alfabetizacion, Classe de alfabetização - CA, maternal etc.
replace aedu_ci = 0 if inlist(nivel_asiste, 6, 7, 8, 9) 

* Esino fundamental y medio 
replace aedu_ci = grado_asiste - 1 if inlist(nivel_asiste, 1, 3) // Esino fundamental
replace aedu_ci = grado_asiste + 9 - 1  if inlist(nivel_asiste, 2, 4) // Esino medio

* Superior
replace aedu_ci = grado_asiste + 12 - 1 if nivel_asiste == 5 // Universitario

* Imputación para los que declaran nivel pero no grado (incluye no seriados)
replace aedu_ci = 0 if (inlist(nivel_asiste, 1, 3) & grado_asiste == .) // Esino fundamental 
replace aedu_ci = 9 if (inlist(nivel_asiste, 2, 4) & grado_asiste == .) // Esino medio
replace aedu_ci = 12 if nivel_asiste == 10 // Pre-Vestibular 
replace aedu_ci = 12 if (nivel_asiste == 5 & grado_asiste == .) // Universitario
replace aedu_ci = 12 + 4 if nivel_asiste == 11 // Maestría o Doctorado

* PARA LOS QUE NO ASISTEN:
**************************

* Creche, Alfabetización, Classe de alfabetização - CA, maternal etc.
replace aedu_ci = 0 if inlist(nivel_no_asiste, 10, 11, 12, 13)

* Esino fundamental y medio 

* Sistema antiguo 
replace aedu_ci = grado_no_asiste if nivel_no_asiste == 1 // Elementar (primario) - 4 anios
replace aedu_ci = grado_no_asiste + 4 if nivel_no_asiste == 2 // Medio 1er ciclo (ginasal) - 4 anios
replace aedu_ci = grado_no_asiste + 8 if nivel_no_asiste == 3 // Medio 2do ciclo (científico, clasico etc.)

* Sistema actual
replace aedu_ci = grado_no_asiste if inlist(nivel_no_asiste, 4, 6) // Esino fundamental
replace aedu_ci = grado_no_asiste + 9 if inlist(nivel_no_asiste, 5, 7) // Esino medio 

* Superior
replace aedu_ci = grado_no_asiste + 12 if nivel_no_asiste == 8 // Universitario

* Imputación para los que declaran nivel pero no grado

* No finalizado
replace aedu_ci = 0 if (inlist(nivel_no_asiste, 1, 4, 6)  & grado_no_asiste == . & inlist(finalizo, 3, .)) // Elementar, Esino fundamental
replace aedu_ci = 4 if (nivel_no_asiste == 2 & grado_no_asiste == . & inlist(finalizo, 3, .)) // Medio 1 
replace aedu_ci = 8 if (nivel_no_asiste == 3 & grado_no_asiste == . & inlist(finalizo, 3, .)) // Medio 2 
replace aedu_ci = 9 if (inlist(nivel_no_asiste, 5, 7) & grado_no_asiste == . & inlist(finalizo, 3, .)) // Esino medio
replace aedu_ci = 12 if (nivel_no_asiste == 8 & grado_no_asiste == . & inlist(finalizo, 3, .)) // Universitario
replace aedu_ci = 12 + 4 if (nivel_no_asiste == 9 & grado_no_asiste == . & inlist(finalizo, 3, .)) // Maestría o Doctorado

* Finalizado 
replace aedu_ci = 4 if (nivel_no_asiste == 1  & grado_no_asiste == . & finalizo == 1) // Elementar
replace aedu_ci = 8 if (nivel_no_asiste == 2 & grado_no_asiste == . & finalizo == 1) // Medio 1 
replace aedu_ci = 9 if (inlist(nivel_no_asiste, 4, 6) & grado_no_asiste == . & finalizo == 1) // Esino fundamental
replace aedu_ci = 12 if (nivel_no_asiste == 3 & grado_no_asiste == . & finalizo == 1) // Medio 2 
replace aedu_ci = 12 if (inlist(nivel_no_asiste, 5, 7) & grado_no_asiste == . & finalizo == 1) // Esino medio
replace aedu_ci = 12 + 4 if (nivel_no_asiste == 8 & grado_no_asiste == . & finalizo == 1) // Universitario
replace aedu_ci = 12 + 4 + 2 if (nivel_no_asiste == 9 & grado_no_asiste == . & finalizo == 1) // Maestría o Doctorado

		
**************
***eduno_ci***
**************
gen byte eduno_ci = (aedu_ci == 0)
replace eduno_ci = . if aedu_ci == . 
label variable eduno_ci "Cero anios de educacion"

**************
***edupi_ci***
**************
gen byte edupi_ci = (aedu_ci > 0 & aedu_ci < 5)
replace edupi_ci = . if aedu_ci == .
label variable edupi_ci "Primaria incompleta"

**************
***edupc_ci***
**************
gen byte edupc_ci = (aedu_ci == 5)
replace edupc_ci = . if aedu_ci == .
label variable edupc_ci "Primaria completa"

**************
***edusi_ci***
**************
gen byte edusi_ci = (aedu_ci > 5 & aedu_ci < 12) 
replace edusi_ci = . if aedu_ci == .
label variable edusi_ci "Secundaria incompleta"

**************
***edusc_ci***
**************
gen byte edusc_ci = (aedu_ci == 12) 
replace edusc_ci = . if aedu_ci == .
label variable edusc_ci "Secundaria completa"

**************
***eduui_ci***
**************
* Entre 13 y 14 anios o 15 que no declaran nivel finalizado.
gen byte eduui_ci = (aedu_ci >= 13 & aedu_ci <= 14) | (aedu_ci == 15 & finalizo != 1) 
replace eduui_ci=. if aedu_ci==.
label variable eduui_ci "Universitaria incompleta"

**************
***eduuc_ci***
**************
/* Aquellos con 15 anios que completaron nivel 
o cualqueira con mas de 15 anios de educ.*/
gen byte eduuc_ci = (aedu_ci == 15 & finalizo == 1 | aedu_ci > 15) 
replace eduuc_ci = . if aedu_ci == .
label variable eduuc_ci "Universitaria completa o mas"

***************
***edus1i_ci***
***************
gen edus1i_ci = (aedu_ci > 5 & aedu_ci < 9)
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
gen byte edus2i_ci = (aedu_ci > 9 & aedu_ci < 12)
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
* No se declara la finalización en ese nivel.
gen byte edupre_ci=.
label variable edupre_ci "Educacion preescolar"

***************
***asispre_ci**
***************
g asispre_ci = (v6003 == 9) 
la var asispre_ci "Asiste a educacion prescolar"	
	
**************
***eduac_ci***
**************
* No puede discriminarse educación superior no universitaria. 
gen byte eduac_ci=.
label variable eduac_ci "Superior universitario vs superior no universitario"

******************
***pqnoasis_ci***
******************
gen pqnoasis_ci = .
label var pqnoasis_ci "Razones para no asistir a la escuela"

**************
*pqnoasis1_ci*
**************
**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**
gen pqnoasis1_ci = .

***************
***repite_ci***
***************
gen repite_ci=.
label var repite_ci "Personas que han repetido al menos un año o grado"

******************
***repiteult_ci***
******************
gen repiteult_ci=.
label var repiteult_ci "Personas que han repetido el último año o grado"

drop nivel_asiste grado_asiste grado_no_asiste nivel_no_asiste finalizo


		**********************************
		**** VARIABLES DE LA VIVIENDA ****
		**********************************
		
****************
***aguared_ch***
****************
gen aguared_ch=(v0212==2 | v0213==1)
label var aguared_ch "Acceso a fuente de agua por red"

*****************
***aguadist_ch***
*****************
gen aguadist_ch=1 if v0211==1 |v0213==1
replace aguadist_ch=2 if v0214==2
replace aguadist_ch=3 if v0214==4
replace aguadist_ch=. if v0214==9 
label var aguadist_ch "Ubicación de la principal fuente de agua"
label def aguadist_ch 1"Adentro de la casa" 2"Afuera de la casa pero dentro del terreno" 3"Afuera de la casa y del terreno" 
label val aguadist_ch aguadist_ch  

*****************
***aguamala_ch***
*****************
gen aguamala_ch=(v0212==6) 
label var aguamala_ch "Agua unimproved según MDG"

*****************
***aguamide_ch***
*****************
gen aguamide_ch=.
label var aguamide_ch "Usan medidor para pagar consumo de agua"

************
***luz_ch***
************
gen luz_ch=(v0219==1)
label var luz_ch  "La principal fuente de iluminación es electricidad"

****************
***luzmide_ch***
****************
gen luzmide_ch=.
label var luzmide_ch "Usan medidor para pagar consumo de electricidad"

****************
***combust_ch***
****************
gen combust_ch=(v0223==1|v0223==2|v0223==5)
label var combust_ch "Principal combustible gas o electricidad" 

*************
***bano_ch***
*************
gen bano_ch=(v0215==1)
label var bano_ch "El hogar tiene servicio sanitario"

***************
***banoex_ch***
***************
gen banoex_ch=(v0216==2)
replace banoex_ch=. if bano_ch==0 | bano_ch==.|v0216==9
label var banoex_ch "El servicio sanitario es exclusivo del hogar"

*************
***des1_ch***
*************
gen des1_ch=1 if v0217>=1 & v0217<=3
replace des1_ch=2 if v0217==4
replace des1_ch=3 if v0217>=5 & v0217<=7
replace des1_ch=0 if bano_ch==0
replace des1_ch=. if v0217==9
label var des1_ch "Tipo de desague según unimproved de MDG"
label def des1_ch 0"No tiene servicio sanitario" 1"Conectado a red general o cámara séptica"
label def des1_ch 2"Letrina o conectado a pozo ciego" 3"Desemboca en río o calle", add
label val des1_ch des1_ch


*************
***des2_ch***
*************
*El indicador debería ser una reclasificación de des1_ch, por ello se cambia aquí: 
gen des2_ch=0 if des1_ch==0
replace des2_ch=1 if des1_ch==1 | des1_ch==2 
replace des2_ch=2 if des1_ch==3
label var des2_ch "Tipo de desague sin incluir definición MDG"
label def des2_ch 0"No tiene servicio sanitario" 1"Conectado a red general, cámara séptica, pozo o letrina"
label def des2_ch 2"Cualquier otro caso", add
label val des2_ch des2_ch

*************
***piso_ch***
*************
gen piso_ch=.
label var piso_ch "Materiales de construcción del piso" 

**************
***pared_ch***
**************
* Se cambia la construcción de la variable incluyendo: tapia sin revestir y de paja 
/*
gen pared_ch=0
replace pared_ch=1 if v0203==1 | v0203==2 |v0203==4
replace pared_ch=2 if v0203==6 | v0203==3 |v0203==5
replace pared_ch=. if v0203==9
label var pared_ch "Materiales de construcción de las paredes"
label def pared_ch 0"No permanentes" 1"Permanentes" 2"Otros materiales:otros"
label val pared_ch pared_ch
*/
* MGR Jul, 2015: se modifica sintáxis para incluir opción 5 (paja) como material impermanente
gen pared_ch=0 if v0203==5 
replace pared_ch=1 if v0203==1 | v0203==2 |v0203==4
replace pared_ch=2 if v0203==6 | v0203==3 
replace pared_ch=. if v0203==9
label var pared_ch "Materiales de construcción de las paredes"
label def pared_ch 0"No permanentes" 1"Permanentes" 2"Otros materiales:otros"
label val pared_ch pared_ch

**************
***techo_ch***
**************
/*
*No se incluían los techos de paja
gen techo_ch=0
replace techo_ch=1 if v0204<=5
replace techo_ch=2 if v0204==7 |v0204==6
replace techo_ch=. if v0204==9
label var techo_ch "Materiales de construcción del techo"
*/
* MGR Jul, 2015: se modifica sintáxis para incluir opción 6 (paja) como material impermanente
gen techo_ch=0 if v0204==6
replace techo_ch=1 if v0204<=5
replace techo_ch=2 if v0204==7
replace techo_ch=. if v0204==9
label var techo_ch "Materiales de construcción del techo"

**************
***resid_ch***
**************
gen resid_ch=0 if v0218==1 | v0218==2
replace resid_ch=1 if v0218==3
replace resid_ch=2 if v0218==4 | v0218==5
replace resid_ch=3 if v0218==6
replace resid_ch=. if v0218==9
label var resid_ch "Método de eliminación de residuos"
label def resid_ch 0"Recolección pública o privada" 1"Quemados o enterrados"
label def resid_ch 2"Tirados a un espacio abierto" 3"Otros", add
label val resid_ch resid_ch

**Daniela Zuluaga- Enero 2018: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**
	
*********************
***aguamejorada_ch***
*********************
gen aguamejorada_ch = 1 if v0212 == 2 | v0212 ==4
replace aguamejorada_ch = 0 if v0212 == 6
				
*********************
***banomejorado_ch***
*********************
gen banomejorado_ch = 1 if (v0215 == 1 & (v0217 >= 1 & v0217 <=3) & v0216 == 2 )
replace banomejorado_ch = 0 if (v0215 == 1 & (v0217 >= 1 & v0217 <=3) & v0216 == 4) | v0215 == 3 | (v0215 == 1 & (v0217 >= 4 & v0217<=7))

*************
***dorm_ch***
*************
gen dorm_ch=v0206
replace dorm_ch=. if v0206==99 |v0206==-1
label var dorm_ch "Habitaciones para dormir"

****************
***cuartos_ch***
****************
gen cuartos_ch=v0205
replace cuartos_ch=. if v0205==99 | v0205==-1
label var cuartos_ch "Habitaciones en el hogar"

***************
***cocina_ch***
***************
gen cocina_ch=.
label var cocina_ch "Cuarto separado y exclusivo para cocinar"

**************
***telef_ch***
**************
gen telef_ch=(v2020==2)
replace telef_ch=. if v2020==9
label var telef_ch "El hogar tiene servicio telefónico fijo"

***************
***refrig_ch***
***************
gen refrig_ch=(v0228==2 |v0228==4)
replace refrig_ch=. if v0228==9
label var refrig_ch "El hogar posee refrigerador o heladera"

**************
***freez_ch***
**************
gen freez_ch=(v0229==1)
replace freez_ch=. if v0229==9
label var freez_ch "El hogar posee congelador"

*************
***auto_ch***
*************
gen auto_ch=(v2032==2 | v2032==6)
replace auto_ch=. if v0229==9
label var auto_ch "El hogar posee automovil particular"

**************
***compu_ch***
**************
gen compu_ch=(v0231==1)
label var compu_ch "El hogar posee computador"

*****************
***internet_ch***
*****************
gen internet_ch=(v0232==2)
label var internet_ch "El hogar posee conexión a Interne

************
***cel_ch***
************
gen cel_ch=(v0220==2)
label var cel_ch "El hogar tiene servicio telefonico celular"

**************
***vivi1_ch***
**************
gen viv1_ch=1 if v0202==2
replace viv1_ch=2 if v0202==4
replace viv1_ch=3 if v0202==6
label var viv1_ch "Tipo de vivienda en la que reside el hogar"
label def viv1_ch 1"Casa" 2"Departamento" 3"Otros"
label val viv1_ch viv1_ch

**************
***vivi2_ch***
**************
gen viv2_ch=(viv1_ch==1 | viv1_ch==2)
replace viv2_ch=. if viv1_ch==.
label var viv2_ch "La vivienda es casa o departamento"

*****************
***viviprop_ch***
*****************
gen viviprop_ch=0 if v0207==3
replace viviprop_ch=1 if v0207==1
replace viviprop_ch=2 if v0207==2
replace viviprop_ch=3 if v0207>=4 /*corrigo =3 no =4, revisar en anios anteriores */
replace viviprop_ch=. if v0207==9 | v0207==.
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
gen vivialq_ch=v0208
replace vivialq_ch=. if vivialq_ch>=999999999 | vivialq_ch<0
label var vivialq_ch "Alquiler mensual"

*******************
***vivialqimp_ch***
*******************
gen vivialqimp_ch=.
label var vivialqimp_ch "Alquiler mensual imputado"

*******************
***tamemp_ci*******
*******************

gen tamemp_ci=1 if v9019==1 | v9019==3 | v9019==5 |v9017==1 | v9017==3 | v9017==5 | v9040==2 | v9040==4 | v9048==2 | v9048==4 | v9048==6 
replace tamemp_ci=2 if v9019==7 | v9017==7 | v9040==6 | v9048==8
replace tamemp_ci=3 if v9019==8 | v9017==8 | v9040==8 | v9048==0

* rev MLO, 2015, 03
* se incorporan cuenta propia y trabajadores agricolas
recode tamemp_ci . =1 if v9049==3
replace tamemp_ci=1 if v9014==2 |  v9014==4 |  v9014==6
replace tamemp_ci=1 if v9049==3 | v9050==6 | v9050==4 | v9050==2 | v9052==2 | v9052==4 | v9052==6
replace tamemp_ci=2 if v9014==8 | v9052==8
replace tamemp_ci=3 if v9014==0 | v9050==8 | v9052==0 

label var  tamemp_ci "Tamaño de Empresa" 

label define tamaño 1"Pequeña" 2"Mediana" 3"Grande"
label values tamemp_ci tamaño


******************
***categoinac_ci**
******************
gen categoinac_ci=.
replace categoinac_ci=1 if (v9122==2 | v9123==1) & condocup_ci==3
replace categoinac_ci=2 if v0602==2 & condocup_ci==3
replace categoinac_ci=3 if v9121==1 & condocup_ci==3
recode categoinac_ci .=4 if condocup_ci==3
label var  categoinac_ci "Condición de Inactividad" 
label define inactivo 1"Pensionado" 2"Estudiante" 3"Hogar" 4"Otros"
label values categoinac_ci inactivo

*******************
***  benefdes_ci***
*******************

g benefdes_ci=0 if desemp_ci==1
replace benefdes_ci=1 if  v9084==2 & desemp_ci==1
label var benefdes_ci "=1 si tiene seguro de desempleo"

*******************
*** ybenefdes_ci***
*******************
g ybenefdes_ci=.
label var ybenefdes_ci "Monto de seguro de desempleo"


*variables que faltan generar
gen tcylmpri_ci=.
gen tcylmpri_ch=.

gen vivi1_ch =.
gen vivi2_ch =.
gen tipopen_ci=.

******************************
*** VARIABLES DE MIGRACION ***
******************************

* Variables incluidas por SCL/MIG Fernando Morales

	*******************
	*** migrante_ci ***
	*******************
	
	gen migrante_ci=(v5030==98) 
	label var migrante_ci "=1 si es migrante"
	
	**********************
	*** migantiguo5_ci ***
	**********************
	
	gen migantiguo5_ci=(migrante_ci==1 & (v0507==1 | (v5080!=. & v5080!=98) | (migrante_ci==1 & v5065==6) | (migrante_ci==1 & v5063==4))) if migrante_ci!=. & !inrange(edad_ci,0,4)	
	label var migantiguo5_ci "=1 si es migrante antiguo (5 anos o mas)"
		
	**********************
	*** migrantelac_ci ***
	**********************
	
	gen migrantelac_ci=.
	label var migrantelac_ci "=1 si es migrante proveniente de un pais LAC"
	
	**********************
	*** migrantiguo5_ci ***
	**********************
	
	gen migrantiguo5_ci=(migrante_ci==1 & (v0507==1 | (v5080!=. & v5080!=98) | (migrante_ci==1 & v5065==6) | (migrante_ci==1 & v5063==4))) if migrante_ci!=. & !inrange(edad_ci,0,4)	
	replace migrantiguo5_ci = 0 if migantiguo5_ci != 1 & migrante_ci==1
	replace migrantiguo5_ci = . if migrante_ci==0
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
aguared_ch aguadist_ch aguamala_ch aguamide_ch luz_ch luzmide_ch combust_ch	bano_ch banoex_ch des1_ch des2_ch piso_ch aguamejorada_ch banomejorado_ch  ///
pared_ch techo_ch resid_ch dorm_ch cuartos_ch cocina_ch telef_ch refrig_ch freez_ch auto_ch compu_ch internet_ch cel_ch ///
vivi1_ch vivi2_ch viviprop_ch vivitit_ch vivialq_ch	vivialqimp_ch , first

/*Homologar nombre del identificador de ocupaciones (isco, ciuo, etc.) y de industrias y dejarlo en base armonizada 
para análisis de trends (en el marco de estudios sobre el futuro del trabajo)*/
rename v9906 codocupa
rename v9907 codindustria

compress

saveold "`base_out'", version(12) replace


log close

