* (Versi�n Stata 13)
clear
set more off
*________________________________________________________________________________________________________________*

 * Activar si es necesario (dejar desactivado para evitar sobreescribir la base y dejar la posibilidad de 
 * utilizar un loop)
 * Los datos se obtienen de las carpetas que se encuentran en el servidor: \\Sdssrv03\surveys
 * Se tiene acceso al servidor �nicamente al interior del BID.
 * El servidor contiene las bases de datos MECOVI.
 *________________________________________________________________________________________________________________*
 

global ruta = "\\Sdssrv03\surveys"

local PAIS BRA
local ENCUESTA PNAD
local ANO "2012"
local ronda m9 
local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
                        
capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
Pa�s: Brasil
Encuesta: PNAD
Round: m9
Autores: Yessenia Loayza
Modificaci�n 2014: Mayra S�enz - Email: mayras@iadb.org - saenzmayra.a@gmail.com
�ltima versi�n: Yessenia Loayza - Email: desloay@hotmail.com
�ltima modificaci�n: Daniela Zuluaga E-mail: danielazu@iadb.org, da.zuluaga@hotmail.com
Fecha �ltima modificaci�n: Octubre de 2017

							SCL/LMK - IADB
****************************************************************************/
****************************************************************************/


use `base_in', clear



								********************************
								**** ARMONIZACI�N PNAD 2011 **** 
								********************************
				*************************
				***VARIABLES DEL HOGAR***
				*************************
*****************
*** region_ci ***
*****************
*YL: generacion "region_c" proyecto maps America.		
*Modificaci�n Mayra S�enz - 12/10/2014  uf string, label define add modify
destring uf, replace 

gen region_c = uf
label define region_c ///
11 "Rond�nia" ///
12 "Acre" ///
13 "Amazonas" ///
14 "Roraima" ///
15 "Par�" ///
16 "Amap�" ///
17 "Tocantins" ///
21 "Maranh�o" ///
22 "Piau�" ///
23 "Cear�" ///
24 "Rio Grande do Norte" ///
25 "Para�ba" ///
26 "Pernambuco" ///
27 "Alagoas" ///
28 "Sergipe" ///
29 "Bahia" ///
31 "Minas Gerais" ///
32 "Esp�rito Santo" ///
33 "Rio de Janeiro" ///
35 "S�o Paulo" ///
41 "Paran�" ///
42 "Santa Catarina" ///
43 "Rio Grande do Sul" ///
50 "Mato Grosso do Sul" ///
51 "Mato Grosso" ///
52 "Goi�s" ///
53 "Distrito Federal", add modify
label value region_c region_c
label var region_c "division politico-administrativa"

************************
*** region seg�n BID ***
************************
gen region_BID_c=4 
label var region_BID_c "Regiones BID"
label define region_BID_c 1 "Centroam�rica_(CID)" 2 "Caribe_(CCB)" 3 "Andinos_(CAN)" 4 "Cono_Sur_(CSC)"
label value region_BID_c region_BID_c
   
   
***************
***factor_ch***
***************
gen factor_ch=v4732
label variable factor_ch "Factor de expansi�n del hogar"

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
label variable pais_c "Pa�s"

**********
***anio***
**********
gen anio_c=2012
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
label var relacion_ci "Relaci�n de parentesco con el  jefe de hogar"
label define relacion_ci 1 "Jefe" 2 "Conyuge" 3 "Hijo" 4 "Otros Parientes" 5 "Otros no Parientes" 6 "Servicio Domestico"
label values relacion_ci relacion_ci

				****************************
				***VARIABLES DEMOGR�FICAS***
				****************************
				
***************
***factor_ci***
***************
gen factor_ci=v4729
label variable factor_ci "Factor de expansi�n de personas"

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
replace edad_ci=. if edad_ci==999
label variable edad_ci "Edad del individuo"


**************
***civil_ci***
**************

* MGR Nov 2015: correccion en sintaxis no se incluian a los que indican vivir en compa�ia de conyugue

gen civil_ci=.
replace civil_ci=1 if v4011==0
replace civil_ci=2 if v4111==1 
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
replace civil_ci=3 if v4011==3 |v4011==5
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

*************************
*** VARIABLES DE RAZA ***
*************************

* MGR Oct. 2015: modificaciones realizadas en base a metodolog�a enviada por SCL/GDI Maria Olga Pe�a


/*COR OU RACA v0404
2 BRANCA
4 PRETA
6 AMARELA
8 PARDA
0 INDIGENA
9 IGNORADA*/

gen raza_ci=.
replace raza_ci= 1 if  (v0404 ==0)
replace raza_ci= 2 if  (v0404 ==4 | v0404 ==8)
replace raza_ci= 3 if (v0404==2 | v0404==6 | v0404== 9) & raza_ci==.
label define raza_ci 1 "Ind�gena" 2 "Afro-descendiente" 3 "Otros"
label value raza_ci raza_ci 
label value raza_ci raza_ci
label var raza_ci "Raza o etnia del individuo" 

gen raza_idioma_ci=.

gen id_ind_ci = 0
replace id_ind_ci=1 if raza_ci==1
label define id_ind_ci 1 "Ind�gena" 0 "Otros" 
label value id_ind_ci id_ind_ci 
label var id_ind_ci  "Indigena" 

gen id_afro_ci = 0
replace id_afro_ci=1 if raza_ci==2
label define id_afro_ci 1 "Afro-descendiente" 0 "Otros" 
label value id_afro_ci id_afro_ci 
label var id_afro_ci "Afro-descendiente" 


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
label define condocup_ci 1"ocupados" 2"desocupados" 3"inactivos" 4"menor 10 a�os"
label value condocup_ci condocup_ci
label var condocup_ci "Condicion de ocupacion utilizando definicion del pais"

/*
Definiciones:
* Popula��o ocupada: Aquelas pessoas que, num determinado per�odo de refer�ncia,
trabalharam ou tinham trabalho mas n�o trabalharam (por exemplo, pessoas em f�rias).

* Popula��o Desocupada: aquelas pessoas que n�o tinham trababalho, num determinado 
per�odo de refer�ncia, mas estavam dispostas a trabalhar, e que, para isso, tomaram
alguma provid�ncia efetiva (consultando pessoas, jornais, etc.).

Popula��o N�o Economicamente Ativa: pessoas n�o classificadas como ocupadas ou 
desocupadas

PET: >=10 a�os de edad
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
label var instcot_ci "instituci�n a la cual cotiza"

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
*2014, 01 Revision MLO
replace `var'=. if `var'>=999999
*replace `var'=. if `var'==999999995904
}

gen pension_ci=0 
replace pension_ci=1 if (v1252>0 & v1252!=.) | (v1255>0 & v1255!=.) | (v1258>0 & v1258!=.) | (v1261>0 & v1261!=.) /*A todas las per mayores de diez a�os*/
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
segun la fuente, el monto bpc para adultos mayores fue de 622 reales. Se encuentran beneficiarios con dicho monto*/
gen pensionsub_ci=(v1273==622)
label var pensionsub_ci "1=recibe pension subsidiada / no contributiva"


*****************
**  ypensub_ci  *
*****************
/*DZ Septiembre 2017- Creacion de la variable valor de la pension subsidiada*
http://dds.cepal.org/bdps/programa/?id=43
segun la fuente, el monto bpc para adultos mayores fue de 622 reales. Se encuentran beneficiarios con dicho monto*/
gen ypensub_ci=v1273 if v1273==622
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
label var region "distribuci�n regional del pa�s"

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

*MR: se agregan l�neas de pobreza y pobreza extrema nacionales

gen lp_ci=.			
replace lp_ci=  266.39  	if region_c==33	& area==3                /*Rio de janeiro-metropolitano*/	
replace lp_ci=  226.03  	if region_c==33	& area==1	             /*Rio de janeiro-urbano*/
replace lp_ci=  203.43  	if region_c==33	& area==2	             /*Rio de janeiro-rural*/
replace lp_ci=  268.01  	if region_c==35	& area==3	             /*Sao Paulo-metropolitano*/
replace lp_ci=  237.33  	if region_c==35	& area==1	             /*Sao paulo-urbano*/
replace lp_ci=  193.74  	if region_c==35	& area==2	             /*Sao paulo-rural*/
replace lp_ci=  297.07  	if region==4	& area==3	& region_c==43 /*Porto alegre: sur-metropolitana-rio grande de sul*/
replace lp_ci=  245.40  	if region==4	& area==3	& region_c==41 /*curitiba:     sur-metropolitana-paran�*/
replace lp_ci=  234.10  	if region==4	& area==1            /*sur-urbana*/	
replace lp_ci=  213.11  	if region==4	& area==2            /*sur-rural */	
replace lp_ci=  211.50   	if region==2	& area==3	& region_c==23 /*Fortaleza:    noreste-metropolitana-cear�*/
replace lp_ci=  277.69  	if region==2	& area==3	& region_c==26 /*recife:       noreste-metropolitana-pernambuco*/
replace lp_ci=  261.55  	if region==2	& area==3	& region_c==29 /*salvador:     noreste-metropolitana-bahia*/
replace lp_ci=  238.95  	if region==2	& area==1            /*noreste-urbana*/	
replace lp_ci=  213.11  	if region==2	& area==2            /*noreste-rural*/	
replace lp_ci=  208.27  	if region==3	& area==3	& region_c==31 /*belo horizonte:sureste-metropolitana-minas gerais*/
replace lp_ci=  187.28  	if region==3	& area==1            /*sudeste-urbano*/	
replace lp_ci=  159.84  	if region==3	& area==2            /*sudeste-rural*/	
replace lp_ci=  237.33  	if region==1	& area==3	& region_c==15 /*belem: noreste-metropolitana-par�*/
replace lp_ci=  245.40  	if region==1	& area==1            /*norte-urbano*/	
replace lp_ci=  214.73  	if region==1	& area==2            /*norte-rural */	
replace lp_ci=  230.87  	if region_c==53	& area==3	             /*Distrito federal-metropolitana*/
replace lp_ci=  198.58  	if region==5	& area==1            /*centro oeste-urbano*/	
replace lp_ci=  174.37  	if region==5	& area==2            /*centro oeste-rural */	
label var lp_ci "Linea de pobreza oficial del pais"

***********
*lpe_ci ***
***********
gen lpe_ci=.
replace lpe_ci=  133.20  	if region_c==33	& area==3		    /*Rio de janeiro-metropolitano*/
replace lpe_ci=  113.01  	if region_c==33	& area==1		    /*Rio de janeiro-urbano*/
replace lpe_ci=  101.71  	if region_c==33	& area==2		    /*Rio de janeiro-rural*/
replace lpe_ci=  134.00  	if region_c==35	& area==3		    /*Sao Paulo-metropolitano*/
replace lpe_ci=  118.67  	if region_c==35	& area==1		    /*Sao paulo-urbano*/
replace lpe_ci=  96.87  	if region_c==35	& area==2		    /*Sao paulo-rural*/
replace lpe_ci=  148.53  	if region==4	& area==3	& region_c==43	/*Porto alegre: sur-metropolitana-rio grande de sul*/
replace lpe_ci=  122.70  	if region==4	& area==3	& region_c==41	/*curitiba:     sur-metropolitana-paran�*/
replace lpe_ci=  117.05  	if region==4	& area==1		/*sur-urbana*/
replace lpe_ci=  106.56  	if region==4	& area==2		/*sur-rural */
replace lpe_ci=  105.75  	if region==2	& area==3	& region_c==23	/*Fortaleza:    noreste-metropolitana-cear�*/
replace lpe_ci=  138.85  	if region==2	& area==3	& region_c==26	/*recife:       noreste-metropolitana-pernambuco*/
replace lpe_ci=  130.77  	if region==2	& area==3	& region_c==29	/*salvador:     noreste-metropolitana-bahia*/
replace lpe_ci=  119.47  	if region==2	& area==1		/*noreste-urbana*/
replace lpe_ci=  106.56  	if region==2	& area==2		/*noreste-rural*/
replace lpe_ci=  104.14  	if region==3	& area==3	& region_c==31	/*belo horizonte:sureste-metropolitana-minas gerais*/
replace lpe_ci=  93.64  	if region==3	& area==1		/*sudeste-urbano*/
replace lpe_ci=  79.92  	if region==3	& area==2		/*sudeste-rural*/
replace lpe_ci=  118.67  	if region==1	& area==3	& region_c==15	/*belem: noreste-metropolitana-par�*/
replace lpe_ci=  122.70  	if region==1	& area==1		/*norte-urbano*/
replace lpe_ci=  107.36  	if region==1	& area==2		/*norte-rural */
replace lpe_ci=  115.44  	if region_c==53	& area==3		    /*Distrito federal-metropolitana*/				
replace lpe_ci=  99.29  	if region==5	& area==1		/*centro oeste-urbano*/
replace lpe_ci=  87.18  	if region==5	& area==2		/*centro oeste-rural */
label var lpe_ci "Linea de indigencia oficial del pais"



*************
**salmm_ci***
*************
gen salmm_ci=622
label var salmm_ci "Salario minimo legal"

*************
***tecnica_ci**
*************
gen tecnica_ci=. /*No se puede identificar educaci�n t�cnica superior*/
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
replace v9058 = . if v9058 == -1 | v9058 == 99

gen horaspri_ci=v9058
replace horaspri_ci= v0713 if (edad_ci>=5 & edad_ci<=9)
replace horaspri_ci=. if v9058<0 |  (v0713<0 & edad_ci>=5 & edad_ci<=9 ) |  emp_ci==0

*2014,01 revision MLO
replace horaspri_ci=. if horaspri_ci<0 | horaspri_ci>150

label var horaspri_ci "Horas trabajadas semanalmente en el trabajo principal"
*Nota: se incluye las horas de trabajo de los menroes. No afecta al sociometro xq filtra por edades.

*****************
***horastot_ci***
*****************
/* 2014, 01 revision MLO
v9058	N�mero de horas trabalhadas por semana nesse trabalho

00 a 98
99 -Ignorado
-1 - N�o aplic�vel

v9101	N�mero de horas trabalhadas por semana nesse emprego secund�rio 
	
99 - Ignorado
-1  - N�o aplic�vel

v9105	N�mero de horas trabalhadas por semana nesse(s) outro(s) trabalho(s)

(excluindo-se o principal e o secund�rio)	
99 - Ignorado
-1  - N�o aplic�vel
*/


replace v9101 = . if v9101 == -1 | v9101 == 99
replace v9105 = . if v9105 == -1 | v9105 == 99

/*yl: creacion de este indicador, construir hacia atras*/
egen horastot_ci=rsum(horaspri_ci v9101 v9105)
replace horastot_ci = . if emp_ci==0 /*Necesitamos que s�lo se fije en los empleados "adultos"*/
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
label var nempleos_ci "N�mero de empleos" 

*****************
***firmapeq_ci***
*****************
/*
gen firmapeq_ci=1     if v9008==1 & v9040<=4 					                                                               /*v9008=Empleado permanente en el Agro*/
replace firmapeq_ci=0 if v9008==1 & (v9040==6 | v9040==8)                                                                  /*v9008=Empleado permanente en el Agro*/
replace firmapeq_ci=1 if (v9008>=2 & v9008<=4) & ((v9013==1 & v9014<=6) | v9013==3)                                        /*v9008= Algun tipo de empleado en el Agro*/
replace firmapeq_ci=0 if (v9008>=2 & v9008<=4) & (v9013==1 & (v9014==8 | v9014==0))                                        /*v9008= Algun tipo de empleado en el Agro*/
replace firmapeq_ci=1 if v9008==5 & ((v9049==1 & v9050<=6) | v9049==3)                                                     /*v9008=Cuenta propia en Servicios Auxiliares*/ 
replace firmapeq_ci=0 if v9008==5 & v9049==1 & v9050==8                                                                    /*v9008=Cuenta propia en Servicios Auxiliares*/ 
replace firmapeq_ci=1 if v9008==6 | v9008==7                                                                               /*Cuenta Propia en Agro o en otra actividad*/
replace firmapeq_ci=0 if (v9008==8 | v9029==4) & ((v9048==0 | v9048==8) | ((v9048==2 | v9048==4) & v9049==1 & v9050>=6))   /*Empleador en los servicios auxiliares Agricolas o Empleador NO Agro*/
replace firmapeq_ci=1 if (v9008==8 | v9029==4) & ((v9048<=6 & v9049==3) | ((v9048==2 | v9048==4) & v9049==1 & v9050<=4))   /*Empleador en los servicios auxiliares Agricolas o Empleador NO Agro*/
replace firmapeq_ci=1 if (v9008==9 | v9008==10) & ((v9016==2 & v9017<=5 & v9018==4) | (v9016==2 & v9017<=3 & v9018==2 ///
& v9019<=3) | (v9016==4 & v9018==2 & v9019<=5) | (v9016==4 & v9018==4))                                                    /*Empleador en Agro u otras actividades*/
replace firmapeq_ci=0 if (v9008==9 | v9008==10) & ((v9016==2 & (v9017==7 | v9017==8)) | (v9016==4 & v9018==2 & v9019>5))  /*Empleador en Agro u otras actividades*/
replace firmapeq_ci=1 if (v9008>=11 & v9008<=13) | (v9029>=5 & v9029<=7)                                                   /*Trabajador No remunerado*/
replace firmapeq_ci=1 if v9029==1 & (v9032==2 & v9040<=4)                                                                  /*Empleado NO Agricola*/
replace firmapeq_ci=0 if v9029==1 & (v9032==2 & (v9040==6 | v9040==8))                                                     /*Empleado NO Agricola*/
/*Los empleados NO Agricolas que trabajan en el sector PUBLICO o que son empleados domesticos no tienen tama�o de firma!*/
replace firmapeq_ci=1 if v9029==3 & (v9049==3 | (v9049==1 | v9050<=6))                                                     /*Cuenta Propia NO Agricola*/
replace firmapeq_ci=0 if v9029==3 & (v9049==1 | (v9050==8 | v9050==0))                                                     /*Cuenta Propia NO Agricola*/
label var firmapeq_ci "Trabajadores informales"
 */
/* Algunas observaciones a esta variable:
   > En esta linea corrigo: replace firmapeq_ci=0 if (v9008==9 | v9008==10) & ((v9016==2 & (v9017==7 | v9017==8)) | (v9016==4 & v9018==2 & v9019>5))
     es >5 no >=5. ver en los otros anios. 
   > Considero que en la linea: replace firmapeq_ci=1 if (v9008>=11 & v9008<=13) | (v9029>=5 & v9029<=7)
     la parte (v9029>=5 & v9029<=7) se debe eliminar puesto que los empleados no agricolas no pertenecen a una firma.
*/

*****************
***spublico_ci***
*****************
gen spublico_ci=(v9032==4)
replace spublico_ci=. if v9032==9
label var spublico_ci "Personas que trabajan en el sector p�blico"

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
label def rama_ci 1"Agricultura, caza, silvicultura y pesca" 2"Explotaci�n de minas y canteras" 3"Industrias manufactureras"
label def rama_ci 4"Electricidad, gas y agua" 5"Construcci�n" 6"Comercio, restaurantes y hoteles" 7"Transporte y almacenamiento", add
label def rama_ci 8"Establecimientos financieros, seguros e inmuebles" 9"Servicios sociales y comunales", add
label val rama_ci rama_ci

****************
***durades_ci***
****************
/*
replace v1091=. if v1091==99 | v1091<0
replace v1092=. if v1092==99 | v1092<0
gen aux1=v1091/12
egen durades_ci=rsum(aux1 v1092) if  v4814!=1 & edad_ci>=10
replace durades_ci=. if (v1091==. & v1092==.) */
gen durades_ci=.
*MLO 03,2014
* anterir definicion se refer�a a peridodo sin empleo, no a periodo en busqueda de empleo que es lo que intenta medir esta variable.
* no se puede construir la variable en numero de meses, se construye variable alternativa
* no se puede distinguir entre quienes buscan por mas de 305 dias.
/*
V9115	115	Tomou alguma provid�ncia para conseguir trabalho na semana de refer�ncia (1-si 2-no)
		
V9116	116	Tomou alguma provid�ncia para conseguir trabalho no per�odo de capta��o de 23 dias (2 si 4 no)
		
V9117	117	Tomou alguma provid�ncia para conseguir trabalho no per�odo de capta��o de 30 dias (1-si 2-no)
		
V9118	118	Tomou alguma provid�ncia para conseguir trabalho no per�odo de capta��o de 305 dias (2 si 4 no)
	*/	
		
gen durades1_ci=1 if v9115==1
replace durades1_ci=2 if v9116==2 & durades1_ci!=1
replace durades1_ci=3 if v9117==1 & durades1_ci!=1 & durades1_ci!=2
replace durades1_ci=4 if v9118==2 & durades1_ci!=1 & durades1_ci!=2 & durades1_ci!=3
label var durades1_ci "Duracion de desempleo alternativa"
label def durades1_ci 1"1 semana" 2"8 a 23 dias" 3"24 a 30 dias" 4"31 a 305 dias"
label val durades1_ci durades1_ci

*MGD:3/04/2015 durades variable continua 
gen aux1=v1091/12
egen durades_2=rsum(aux1 v1092) if condocup_ci==2 & (v9115==1 | v9116==2 | v9117==1 | v9118==2)
recode durades_2 (0=0.23) if condocup_ci==2 & (v9115==1 | v9116==2 | v9117==1 | v9118==2)

*******************
***antiguedad_ci***
*******************
replace v9611=. if v9611==99 | v9611<0
replace v9612=. if v9612==99 | v9612<0
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
*2014, 01 revision MLO
*replace ylmpri_ci=. if v9532<0 | v9532>=999999995904 | v4814!=1
replace ylmpri_ci=. if  (edad_ci>=5 & edad_ci<=9) & (v7122<=0 | v7122>=999999 | emp_ci==0)
*2014,01 revision MLO
*replace ylmpri_ci=. if  (edad_ci>=5 & edad_ci<=9) & (v7122<=0 | v7122>=999999995904 | emp_ci==0)
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
*2014, 01 revision MLO
replace ylnmpri_ci=. if v9535<0 | v9535>=999999 | v4814!=1
*replace ylnmpri_ci=. if v9535<0 | v9535>=999999995904 | v4814!=1
replace ylnmpri_ci=. if (edad_ci>=5 & edad_ci<=9) & (v7125<0 | v7125>=999999 | emp_ci==0)
label var ylnmpri_ci "Ingreso laboral NO monetario actividad principal"  

***************
***ylmsec_ci***  
***************
gen ylmsec_ci=v9982 if edad_ci>=10
*2014, 01 revision MLO
replace ylmsec_ci=. if v9982<0 | v9982>=999999 | v4814!=1
*replace ylmsec_ci=. if v9982<0 | v9982>=999999995904 | v4814!=1
label var ylmsec_ci "Ingreso laboral monetario segunda actividad" 

****************
***ylnmsec_ci***
****************
gen ylnmsec_ci=v9985 if edad_ci>=10
replace ylnmsec_ci=. if v9985<0 | v9985>=999999 | v4814!=1
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
replace ylnmotros_ci=. if v1025<0 | v1025>=999999 | v4814!=1
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
**************
**asiste_ci***
**************
gen asiste_ci=(v0602==2)
label var asiste_ci "Personas que actualmente asisten a un centro de ense�anza"

***************
***edupub_ci***
***************
gen edupub_ci=(v6002==2)
label var  edupub_ci "Personas que asisten a centros de ense�anza p�blicos"

*************
***aedu_ci***
*************
/*Modificado Mayra S�enz 12/10/2014
*gen aedu_ci=.
* Si se genera con . se generan alrededor de 10% de hogares con jefe de hogar con missing en educaci�n.
gen aedu_ci=0
label var aedu_ci "Anios de educacion"

*PARA LOS QUE NO ASISTEN
*************************
*Maternal, jardim de inf�ncia etc., creche o alfabetizaci�n de adultos
replace aedu_ci=0 if (v6007==10| v6007==11 | v6007==12 | v6007==13) & asiste_ci==0
	*Sistema antiguo
*Elementar (prim�rio) - se asume que el m�ximo es 4 - Anteriormente se permit�a 6 pero no 5
replace aedu_ci=0  if v6007==1 & v0610==. & v0611!=1 & asiste_ci==0
replace aedu_ci=min(v0610,4) if v6007==1 & v0610>=1 & v0610<=6 & asiste_ci==0
*Medio 1 ciclo (ginasial, etc) - se asume que el m�ximo es 8
replace aedu_ci=min(v0610+4,8) if v6007==2 & v0610>=1 & v0610<=5 & asiste_ci==0
replace aedu_ci=4  if v6007==2 & v0610==. & v0611!=1 & asiste_ci==0
*Medio 2 ciclo (cientifico, clasico, etc, etc) - se asume que el m�ximo es 11, pero
*bajo la l�gica anterior deber�an se 12, ya que se permite hasta 4 a�os adicionales en este nivel
*Aunque solo es necesario tener 11 a�os de educaci�n para completar la secundaria
replace aedu_ci=min(v0610+8,12) if v6007==3 & v0610>=1 & v0610<=4 & asiste_ci==0
replace aedu_ci=8  if v6007==3 & v0610==. & v0611!=1 & asiste_ci==0
	*Sistema nuevo
*Primeiro grau - Bajo este sistema la primaria llega hasta el grado 8
replace aedu_ci=min(v0610,8) if v6007==4 & v0610>=1 & v0610<=8 & asiste_ci==0
replace aedu_ci=0  if v6007==4 & v0610==. & v0611!=1 & asiste_ci==0
*Segundo grau - Secundaria son 4 a�os m�s
replace aedu_ci=min(v0610+8,12) if v6007==5 & v0610>=1 & v0610<=4 & asiste_ci==0
replace aedu_ci=8 if v6007==5 & v0610==. & v0611!=1 & asiste_ci==0
	*Educa��o de jovens e adultos ou supletivo do ensino fundamental 
*1� grau - Bajo este sistema la primaria llega hasta el grado 8
replace aedu_ci=min(v0610,8) if v6007==6 & v0610>=1 & v0610<=8 & asiste_ci==0
replace aedu_ci=0  if v6007==6 & v0610==. & v0611!=1 & asiste_ci==0
*2� grau - Secundaria son 4 a�os m�s
replace aedu_ci=min(v0610+8,12) if v6007==7 & v0610>=1 & v0610<=4 & asiste_ci==0
replace aedu_ci=8 if v6007==7 & v0610==. & v0611!=1 & asiste_ci==0
*Superior
replace aedu_ci=min(v0610+11,17) if v6007==8 & v0610>=1 & v0610<=8 & asiste_ci==0
replace aedu_ci=11 if v6007==8 & v0610==. & v0611!=1 & asiste_ci==0
*Maestria o doctorado  
*Para este ciclo no se pregunta el �ltimo a�o aprobado. Por lo tanto se supone que si termin� el ciclo 
*el individuo cuenta con 19 a�os de educaci�n (2 a�os m�s de educaci�n), si el individuo no termin� se le agrega 
*1 a�o m�s de eduaci�n para quedar con 18 ya que si el �ltimo ciclo m�s alto alcanzado es postgrado, el individuo 
*por lo menos tuvo que cursar 1 a�o en ese nivel
replace aedu_ci=18 if v6007==9 & v0611==3 & asiste_ci==0
replace aedu_ci=19 if v6007==9 & v0611==1 & asiste_ci==0


*PARA LOS QUE ASISTEN
**********************
*Pre-escolar, creche o alfabetizaci�n de adultos
replace aedu_ci=0 if (v6003==6| v6003==7 | v6003==8 |v6003==9) & asiste_ci==1
*Regular de 1� grau/ Supletivo de 1� grau   (se asume que el m�ximo es 8) 
replace aedu_ci=0  if (v6003==1 | v6003==3) & v0605==. & asiste_ci==1
replace aedu_ci=min(v0605-1,7) if (v6003==1 | v6003==3) & v0605>=1 & v0605<=8 & asiste_ci==1
*Regular de 2� grau/ Supletivo de 2� grau   (se asume que el m�ximo es 4, pero con 3 basta para completar el ciclo)
replace aedu_ci=min(v0605+8-1,11) if (v6003==2 | v6003==4) & v0605>=1 & v0605<=4 & asiste_ci==1
replace aedu_ci=8  if (v6003==2 | v6003==4) & v0605==. & asiste_ci==1
*Pre-vestibular
replace aedu_ci=11  if v6003==10 & asiste_ci==1
*Superior
replace aedu_ci=min(v0605+11-1,17) if v6003==5 & v0605>=1 & v0605<=8 & asiste_ci==1
replace aedu_ci=12 if v6003==5 & v0605==. & asiste_ci==1
*Maestria o doctorado  
*Si el �ltimo ciclo m�s alto alcanzado es postgrado, el individuo por lo menos tuvo que cursar 1 a�o en ese nivel
replace aedu_ci=18 if v6003==11  & asiste_ci==1
*Se deja s�lo la informaci�n de las personas con 5 a�os o m�s
replace aedu_ci=. if edad_ci<5


*Modificaci�n Mayra S�enz - Agosto 2015: Se incluyen las variables con los cambios sugeridos por 
*Iv�n Bornacelly de SCL/EDU : Consideramos que esto no es una argumento fuerte para asignarle 0 a�os de educaci�n a 64,548 observaciones (2005) � Aproximadamente 15% de la muestra. Adem�s que no tienen informaci�n en ninguna de las otras variables de educaci�n.


*************
***aedu_ci***
*************
*Modificado Mayra S�enz 12/10/2014
*gen aedu_ci=.
* Si se genera con . se generan alrededor de 10% de hogares con jefe de hogar con missing en educaci�n.
gen aedu_ci=3
label var aedu_ci "Anios de educacion"


*PARA LOS QUE NO ASISTEN
*************************
*Creche o alfabetizaci�n de adultos
replace aedu_ci=. if v6007==10 | v6007==11|v6007==12 

*Maternal, jardim de inf�ncia etc., 
replace aedu_ci=0 if v6007==13 & asiste_ci==0

	*Sistema antiguo
*Elementar (prim�rio) � Son obligatorios 4 a�os. Pueden llegar a ser hasta 6 
replace aedu_ci=0  if v6007==1 & v0610==. & v0611!=1 & asiste_ci==0
replace aedu_ci=v0610 if v6007==1 & v0610>=1 & v0610<=6 & asiste_ci==0
*Medio 1 ciclo (ginasial, etc) - se asume que quienes llegan a ese nivel por lo menos hicieron cuatro a�os del anterior.
 replace aedu_ci=v0610+4 if v6007==2 & v0610>=1 & v0610<=5 & asiste_ci==0
replace aedu_ci=4  if v6007==2 & v0610==. & v0611!=1 & asiste_ci==0
*Medio 2 ciclo (cientifico, clasico, etc, etc) � En este nivel son obligatorios 4 a�os tambi�n. No es importante el nivel m�ximo que se indique. 
replace aedu_ci=v0610+8 if v6007==3 & v0610>=1 & v0610<=4 & asiste_ci==0
replace aedu_ci=8  if v6007==3 & v0610==. & v0611!=1 & asiste_ci==0

	*Sistema nuevo
*Primeiro grau - Bajo este sistema la primaria comprende 8 a�os obligatorios
replace aedu_ci=v0610 if v6007==4 & v0610>=1 & v0610<=9 & asiste_ci==0
replace aedu_ci=0  if v6007==4 & v0610==. & v0611!=1 & asiste_ci==0
*Segundo grau - Secundaria son 4 a�os m�s
replace aedu_ci=v0610+8 if v6007==5 & v0610>=1 & v0610<=4 & asiste_ci==0
replace aedu_ci=8 if v6007==5 & v0610==. & v0611!=1 & asiste_ci==0


	*Educa��o de jovens e adultos ou supletivo do ensino fundamental 
*1� grau - Bajo este sistema la primaria comprende 8 a�os obligatorios
replace aedu_ci=v0610 if v6007==6 & v0610>=1 & v0610<=9 & asiste_ci==0
replace aedu_ci=0  if v6007==6 & v0610==. & v0611!=1 & asiste_ci==0
*2� grau - Secundaria son 4 a�os m�s
replace aedu_ci=v0610+8 if v6007==7 & v0610>=1 & v0610<=4 & asiste_ci==0
replace aedu_ci=8 if v6007==7 & v0610==. & v0611!=1 & asiste_ci==0


*Superior
replace aedu_ci=v0610+11 if v6007==8 & v0610>=1 & v0610<=8 & asiste_ci==0
replace aedu_ci=12 if v6007==8 & v0610==. & v0611!=1 & asiste_ci==0

/**Maestria o doctorado  
Para este ciclo no se pregunta el �ltimo a�o aprobado. Por lo tanto se supone que si termin� el ciclo 
el individuo cuenta con 19 a�os de educaci�n (2 a�os m�s de educaci�n), si el individuo no termin� se le agrega 
1 a�o m�s de eduaci�n para quedar con 18 ya que si el �ltimo ciclo m�s alto alcanzado es postgrado, el individuo*/
*No se puede identificar si la persona tiene maestr�a o doctorado por separado. Se asume que el nivel *educativo m�s alto posible logrado en Maestr�a que dura en promedio dos a�os. 
*por lo menos tuvo que cursar 1 a�o en ese nivel
replace aedu_ci=18 if v6007==9 & v0611==3 & asiste_ci==0
replace aedu_ci=19 if v6007==9 & v0611==1 & asiste_ci==0


*PARA LOS QUE ASISTEN
**********************
*Creche o alfabetizaci�n para adultos
replace aedu_ci=. if (v6003==6 | v6003==7 | v6003==8)

*Pre-escolar
replace aedu_ci=0 if  v6003==9 & asiste_ci==1

*Regular de 1� grau/ Supletivo de 1� grau   
replace aedu_ci=0  if (v6003==1 | v6003==3) & v0605==. & asiste_ci==1
* Este �-1� es por que est� asistiendo?

replace aedu_ci=(v0605-1) if (v6003==1 | v6003==3) & v0605>=1 & v0605<=8 & asiste_ci==1
*Regular de 2� grau/ Supletivo de 2� grau   (se asume que el m�ximo es 4, pero con 3 basta para completar el ciclo)
* D�nde encuentran que con 3 a�os es suficientes para completar el ciclo.

replace aedu_ci=v0605+8-1 if (v6003==2 | v6003==4) & v0605>=1 & v0605<=4 & asiste_ci==1
replace aedu_ci=8  if (v6003==2 | v6003==4) & v0605==. & asiste_ci==1

*Pre-vestibular
replace aedu_ci=11  if v6003==10 & asiste_ci==1

*Superior
replace aedu_ci=v0605+11 if v6003==5 & v0605>=1 & v0605<=8 & asiste_ci==1
replace aedu_ci=12 if v6003==5 & v0605==. & asiste_ci==1

*Maestria o doctorado  
*Si el �ltimo ciclo m�s alto alcanzado es postgrado, el individuo por lo menos tuvo que cursar 1 a�o en ese nivel
replace aedu_ci=18 if v6003==11  & asiste_ci==1

*Se deja s�lo la informaci�n de las personas con 5 a�os o m�s
replace aedu_ci=. if edad_ci<5
*/

*Modificado por Iv�n Bornacelly - 03/07/2017

*************
***aedu_ci***
*************
gen nivel_asist=v6003
gen grado_asist=v0605
gen nivel_no_asist=v6007
gen grado_no_asist=v0610
gen finalizo=v0611
gen Ensino_8_9=.
gen finalizo_1=v0609
gen seria_asist=v0604
gen seria_no_asist=v0608
gen dur_fund_asist=v6030
gen dur_fund_no_asist=v6070

gen aedu_ci=.
label var aedu_ci "Anios de educacion"

*PARA LOS QUE ASISTEN:*
**********************
*Creche & Pre-escolar
replace aedu_ci=0 if nivel_asist==7 | nivel_asist==8 | nivel_asist==9 // Estudiantes de Prescolar y jard�n no se les asigna a�os de educaci�n. Ac� se incluyen los que est�n en Clase de Alfabetizaci�n.

*Primaria / B�sica - Nuevo sistema (Regular de ensino Fundamental grado 1)
*Se le resta 1 por que est� asistiendo al grado que reporta, por lo tanto no se debe considerar dentro de los a�os de educaci�n aprobados.
replace aedu_ci=grado_asist-1 if nivel_asist==1 & dur_fund_asist==1 // 8 a�os.
replace aedu_ci=grado_asist if nivel_asist==1 & dur_fund_asist==3 // 9 a�os.

*Secundaria / Ensino Fundamental 2do Ciclo - Sistema Nuevo (Regular de ensino Fundamental grado 2)
replace aedu_ci=grado_asist+8-1 if nivel_asist==2

*Primaria / B�sica - Supletivo
*Seriado
replace aedu_ci=grado_asist-1 if nivel_asist==3 & seria_asist==2 
*No Seriado
replace aedu_ci=1 if nivel_asist==3 & seria_asist==4

*Secundaria  Ensino Fundamental 2do Ciclo - Supletivo
*Seriado
replace aedu_ci=grado_asist+8-1 if nivel_asist==4 & seria_asist==2
*NO Seriado
replace aedu_ci=8 if nivel_asist==4 & seria_asist==4

*Superior (Preuniversitario y Superior)
replace aedu_ci=12 if nivel_asist==10 // Preuniversitario
replace aedu_ci=grado_asist+12-1 if nivel_asist==5 // Universitario - No incluye Postgrados

*Maestrias/Doctorado
replace aedu_ci=12+5 if nivel_asist==11

*Quitando a quienes no se cuentan:
replace aedu_ci=. if nivel_asist==6 // Educaci�n para adultos. 

*Reemplazando por missing los que tienen como respuesta: Indenterminado (9)
replace aedu_ci=. if v0605==9

*PARA LOS QUE NO ASISTEN:*
**************************
*Creche & Pre-escolar
replace aedu_ci=0 if nivel_no_asist==11 | nivel_no_asist==12 | nivel_no_asist==13 // Estudiantes de Prescolar y jard�n no se les asigna a�os de educaci�n.

*Primaria / Elemental
*Termino 1er A�o
replace aedu_ci=grado_no_asist if nivel_no_asist==1 & finalizo_1==1
*No termin� 1er a�o
replace aedu_ci=0 if nivel_no_asist==1 & finalizo_1==3

*Medio 1 // Se asume que son 4 a�os obligatorios. Pueden llegar a ser 6.
*Seriado -> Termino 1er A�o 
replace aedu_ci=grado_no_asist+4 if nivel_no_asist==2 & seria_no_asist==2 & finalizo_1==1 
*Seriado -> No termin� 1er A�o
replace aedu_ci=4 if nivel_no_asist==2 & seria_no_asist==2 & finalizo_1==3 
*No Seriado -> Si concluy�
replace aedu_ci=8 if nivel_no_asist==2 & seria_no_asist==4 & finalizo==1 
*No Seriado -> No concluy�
replace aedu_ci=4 if nivel_no_asist==2 & seria_no_asist==4 & finalizo==3 

*Medio 2 // Se asume que son 4 a�os obligatorios (Adicionales a los anteriores). Pueden llegar a ser 4
*Seriado -> Termin� 1er A�o
replace aedu_ci=grado_no_asist+4+4 if nivel_no_asist==3 & seria_no_asist==2 & finalizo_1==1 
*Seriado -> No termin� 1er A�o
replace aedu_ci=8 if nivel_no_asist==3 & seria_no_asist==2 & finalizo_1==3
*No Seriado -> Si concluy�
replace aedu_ci=12 if nivel_no_asist==3 & seria_no_asist==4 & finalizo==1
*No Seriado -> No concluy�
replace aedu_ci=8 if nivel_no_asist==3 & seria_no_asist==4 & finalizo==1

*Ensino Fundamental
*Eliminando indeterminados (9)
replace grado_no_asist=. if grado_no_asist==9 & nivel_no_asist==4
*Termin� 1er a�o -> 8 a�os
replace aedu_ci=grado_no_asist if nivel_no_asist==4 & finalizo_1==1 & dur_fund_no_asist==1
*Termin� 1er a�o -> 9 a�os // Se debe sumar 1 a�o m�s por que el nuevo sistema educaci�n empieza los cursos a partir del grado cero.
replace aedu_ci=grado_no_asist+1 if nivel_no_asist==4 & finalizo_1==1 & dur_fund_no_asist==3
*No termin� 1er A�o
replace aedu_ci=0 if nivel_no_asist==4 & finalizo_1==3

*Ensino Medio // Se suman 8 a�os de Ensino Fundamental
*Eliminando indeterminados (9)
replace grado_no_asist=. if grado_no_asist==9 & nivel_no_asist==5
*Termin� 1er a�o
replace aedu_ci=grado_no_asist+8 if nivel_no_asist==5 & finalizo_1==1
*No termin� 1er A�o
replace aedu_ci=8 if nivel_no_asist==5 & finalizo_1==3

*Ensino Fundamental Supletivo
*Seriado -> Termin� 1er a�o
replace aedu_ci=grado_no_asist if nivel_no_asist==6 & seria_no_asist==2 & finalizo_1==1
*Seriado -> No termin� 1er a�o
replace aedu_ci=0 if nivel_no_asist==6 & seria_no_asist==2 & finalizo_1==3
*No Seriado -> concluy�
replace aedu_ci=8 if nivel_asist==6 & seria_no_asist==4 & finalizo==1
*No Seriado -> No concluy�
replace aedu_ci=0 if nivel_asist==6 & seria_no_asist==4 & finalizo==3

*Secundaria  Ensino Fundamental 2do Ciclo - Supletivo
*Seriado -> Termin� 1er a�o
replace aedu_ci=grado_no_asist+8 if nivel_no_asist==7 & seria_no_asist==2 & finalizo_1==1
*Seriado -> No termin� 1er a�o
replace aedu_ci=8 if nivel_no_asist==7 & seria_no_asist==2 & finalizo_1==3
*No Seriado -> concluy�
replace aedu_ci=12 if nivel_asist==7 & seria_no_asist==4 & finalizo==1
*No Seriado -> No concluy�
replace aedu_ci=8 if nivel_asist==7 & seria_no_asist==4 & finalizo==3

*Superior
*Termino 1er A�o
replace aedu_ci=grado_no_asist+12 if nivel_no_asist==8 & finalizo_1==1
*No Termino 1er A�o
replace aedu_ci=12 if nivel_no_asist==8 & finalizo_1==3

*Maestrado ou dooutorado
*Concluy�
replace aedu_ci=17+2 if nivel_no_asist==9 & finalizo==1
*No Concluy�
replace aedu_ci=17+1 if nivel_no_asist==9 & finalizo==3

*Quitando a quienes no se cuentan:
replace aedu_ci=. if nivel_no_asist==10 // Educaci�n para adultos. 

*Reemplazando por missing los que tienen como respuesta: Indenterminado (9)
replace aedu_ci=. if v0610==9

**************
***eduno_ci***
**************
gen byte eduno_ci=0
replace eduno_ci=1 if aedu_ci==0
replace eduno_ci=. if aedu_ci==.
label variable eduno_ci "Cero anios de educacion"

**************
***edupi_ci***
**************
gen byte edupi_ci=0
replace edupi_ci=1 if aedu_ci>0 & aedu_ci<6
replace edupi_ci=. if aedu_ci==.
label variable edupi_ci "Primaria incompleta"

**************
***edupc_ci***
**************
gen byte edupc_ci=0
replace edupc_ci=1 if aedu_ci==6
replace edupc_ci=. if aedu_ci==.
label variable edupc_ci "Primaria completa"

**************
***edusi_ci***
**************
gen byte edusi_ci=0
replace edusi_ci=1 if aedu_ci>6 & aedu_ci<11
replace edusi_ci=. if aedu_ci==.
label variable edusi_ci "Secundaria incompleta"

**************
***edusc_ci***
**************
gen byte edusc_ci=0
replace edusc_ci=1 if aedu_ci==11
replace edusc_ci=. if aedu_ci==.
label variable edusc_ci "Secundaria completa"

**************
***eduui_ci***
**************
gen byte eduui_ci=0
replace eduui_ci=1 if aedu_ci>11 & aedu_ci<17
replace eduui_ci=. if aedu_ci==.
label variable eduui_ci "Universitaria incompleta"

**************
***eduuc_ci***
**************
gen byte eduuc_ci=0
replace eduuc_ci=1 if aedu_ci>=17
replace eduuc_ci=. if aedu_ci==.
label variable eduuc_ci "Universitaria completa o mas"

***************
***edus1i_ci***
***************
*La secundaria s�lo dura 4 a�os. No puede divirse en ciclos
gen edus1i_ci=.
label variable edus1i_ci "1er ciclo de la secundaria incompleto" 

***************
***edus2i_ci***
***************
gen byte edus2i_ci=.
label variable edus2i_ci "2do ciclo de la secundaria incompleto" 

***************
***edus2c_ci***
***************
gen edus2c_ci=.
label variable edus2c_ci "2do ciclo de la secundaria completo" 

***************
***edupre_ci***
***************
gen byte edupre_ci=.
label variable edupre_ci "Educacion preescolar"

***************
***asispre_ci***
***************
*Creaci�n de la variable asistencia a preescolar por Iv�n Bornacelly - 01/12/17
	g asispre_ci=.
	replace asispre_ci=1 if (v6003==7 | v6003==8 | v6003==9) & v8005>=4
	recode asispre_ci (.=0)
	la var asispre_ci "Asiste a educacion prescolar"	
	
**************
***eduac_ci***
**************
gen byte eduac_ci=.
label variable eduac_ci "Superior universitario vs superior no universitario"

foreach var of varlist edu* {
replace `var'=. if aedu_ci==.
}

******************
***pqnoasis_ci***
******************
gen pqnoasis_ci=.
label var pqnoasis_ci "Razones para no asistir a la escuela"

**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**
	
**************
*pqnoasis1_ci*
**************
gen pqnoasis1_ci = .

***************
***repite_ci***
***************
gen repite_ci=.
label var repite_ci "Personas que han repetido al menos un a�o o grado"

local varlist = "NERP NERS NERS2 LIT LIT2 RATIOPRIM RATIOSEC RATIOTER RATIOALL RATIOLIT2 RATIOLIT WENAS WENASD ELEC SFUELS WATER SANITATION SECTEN UNMPLYMENT15 TELCEL TEL CEL COMPUTER INTUSERS CHILDREN PERSROOM2 PLT2 DISCONN REZ PRIMCOMP AEDUC_15 AEDUC_15_24" 
foreach var of local varlist {
gen `var' = .
}
local varlist2 = "AEDUC_25 GFA GFAP GFAS"
foreach var of local varlist2 {
gen `var' = .
}

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
label var aguadist_ch "Ubicaci�n de la principal fuente de agua"
label def aguadist_ch 1"Adentro de la casa" 2"Afuera de la casa pero dentro del terreno" 3"Afuera de la casa y del terreno" 
label val aguadist_ch aguadist_ch  

*****************
***aguamala_ch***
*****************
gen aguamala_ch=(v0212==6) 
label var aguamala_ch "Agua unimproved seg�n MDG"

*****************
***aguamide_ch***
*****************
gen aguamide_ch=.
label var aguamide_ch "Usan medidor para pagar consumo de agua"

************
***luz_ch***
************
gen luz_ch=(v0219==1)
replace luz_ch=. if v0219==9
label var luz_ch  "La principal fuente de iluminaci�n es electricidad"

****************
***luzmide_ch***
****************
gen luzmide_ch=.
label var luzmide_ch "Usan medidor para pagar consumo de electricidad"

****************
***combust_ch***
****************
gen combust_ch=(v0223==1|v0223==2|v0223==5)
replace combust_ch=. if v0223==9
label var combust_ch "Principal combustible gas o electricidad" 

*************
***bano_ch***
*************
gen bano_ch=(v0215==1)
replace bano_ch=. if v0215==9
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
label var des1_ch "Tipo de desague seg�n unimproved de MDG"
label def des1_ch 0"No tiene servicio sanitario" 1"Conectado a red general o c�mara s�ptica"
label def des1_ch 2"Letrina o conectado a pozo ciego" 3"Desemboca en r�o o calle", add
label val des1_ch des1_ch


*************
***des2_ch***
*************
*El indicador deber�a ser una reclasificaci�n de des1_ch, por ello se cambia aqu�: 
gen des2_ch=0 if des1_ch==0
replace des2_ch=1 if des1_ch==1 | des1_ch==2 
replace des2_ch=2 if des1_ch==3
label var des2_ch "Tipo de desague sin incluir definici�n MDG"
label def des2_ch 0"No tiene servicio sanitario" 1"Conectado a red general, c�mara s�ptica, pozo o letrina"
label def des2_ch 2"Cualquier otro caso", add
label val des2_ch des2_ch

*************
***piso_ch***
*************
gen piso_ch=.
label var piso_ch "Materiales de construcci�n del piso" 

**************
***pared_ch***
**************
/*
* Se cambia la construcci�n de la variable incluyendo: tapia sin revestir y de paja 
gen pared_ch=0
replace pared_ch=1 if v0203==1 | v0203==2 |v0203==4
replace pared_ch=2 if v0203==6 | v0203==3 |v0203==5
replace pared_ch=. if v0203==9
label var pared_ch "Materiales de construcci�n de las paredes"
label def pared_ch 0"No permanentes" 1"Permanentes"
label val pared_ch pared_ch
*/

* MGR Jul, 2015: se modifica sint�xis para incluir opci�n 5 (paja) como material impermanente
gen pared_ch=0 if v0203==5 
replace pared_ch=1 if v0203==1 | v0203==2 |v0203==4
replace pared_ch=2 if v0203==6 | v0203==3 
replace pared_ch=. if v0203==9
label var pared_ch "Materiales de construcci�n de las paredes"
label def pared_ch 0"No permanentes" 1"Permanentes" 2"Otros materiales:otros"
label val pared_ch pared_ch

**************
***techo_ch***
**************
/*
*No se inclu�an los techos de paja
gen techo_ch=0
replace techo_ch=1 if v0204<=5
replace techo_ch=2 if v0204==7 |v0204==6
replace techo_ch=. if v0204==9
label var techo_ch "Materiales de construcci�n del techo"
*/
* MGR Jul, 2015: se modifica sint�xis para incluir opci�n 6 (paja) como material impermanente
gen techo_ch=0 if v0204==6
replace techo_ch=1 if v0204<=5
replace techo_ch=2 if v0204==7
replace techo_ch=. if v0204==9
label var techo_ch "Materiales de construcci�n del techo"

**************
***resid_ch***
**************
gen resid_ch=0 if v0218==1 | v0218==2
replace resid_ch=1 if v0218==3
replace resid_ch=2 if v0218==4 | v0218==5
replace resid_ch=3 if v0218==6
replace resid_ch=. if v0218==9
label var resid_ch "M�todo de eliminaci�n de residuos"
label def resid_ch 0"Recolecci�n p�blica o privada" 1"Quemados o enterrados"
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
label var telef_ch "El hogar tiene servicio telef�nico fijo"

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
label var internet_ch "El hogar posee conexi�n a Interne

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
label var vivitit_ch "El hogar posee un t�tulo de propiedad"

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
* Nota MGD 01/2015: Faltaba categorizar el numero de trabajadores de los cuenta propia (V9052)
gen tamemp_ci=1 if v9019==1 | v9019==3 | v9019==5 |v9017==1 | v9017==3 | v9017==5 | v9040==2 | v9040==4 | v9048==2 | v9048==4 | v9048==6 | v9052==2 | v9052==4 | v9052==6
replace tamemp_ci=2 if v9019==7 | v9017==7 | v9040==6 | v9048==8 | v9052==8
replace tamemp_ci=3 if v9019==8 | v9017==8 | v9040==8 | v9048==0 | v9052==0

* rev MLO, 2015, 03
* se incorporan cuenta propia y trabajadores agricolas
recode tamemp_ci . =1 if v9049==3

replace tamemp_ci=1 if v9014==2 |  v9014==4 |  v9014==6
replace tamemp_ci=1 if v9049==3 | v9050==6 | v9050==4 | v9050==2 | v9052==2 | v9052==4 | v9052==6
replace tamemp_ci=2 if v9014==8 | v9052==8
replace tamemp_ci=3 if v9014==0 | v9050==8 | v9052==0 

label var  tamemp_ci "Tama�o de Empresa" 

label define tama�o 1"Peque�a" 2"Mediana" 3"Grande"
label values tamemp_ci tama�o
 g tamemp_o=.

******************
***categoinac_ci**
******************
gen categoinac_ci=.
replace categoinac_ci=1 if (v9122==2 | v9123==1) & condocup_ci==3
replace categoinac_ci=2 if v0602==2 & condocup_ci==3
replace categoinac_ci=3 if v9121==1 & condocup_ci==3
recode categoinac_ci .=4 if condocup_ci==3
*replace categoinac_ci=4 if o7==1 | o7==2 | o7==3 | o7==4 | o7==5 | o7==6 | o7==7 | o7==8 | o7==9| o7== 13| o7==14 | o7==15 | o7==16 | o7==17
label var  categoinac_ci "Condici�n de Inactividad" 
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
gen edus1c_ci=.
gen repiteult_ci=.
gen vivi1_ch =.
gen vivi2_ch =.
gen tipopen_ci=.

/*_____________________________________________________________________________________________________*/
* Asignaci�n de etiquetas e inserci�n de variables externas: tipo de cambio, Indice de Precios al 
* Consumidor (2011=100), Paridad de Poder Adquisitivo (PPA 2011),  l�neas de pobreza
/*_____________________________________________________________________________________________________*/


do "$ruta\harmonized\_DOCS\\Labels&ExternalVars_Harmonized_DataBank.do"

/*_____________________________________________________________________________________________________*/
* Verificaci�n de que se encuentren todas las variables armonizadas 
/*_____________________________________________________________________________________________________*/

order region_BID_c region_c pais_c anio_c mes_c zona_c factor_ch	idh_ch	idp_ci	factor_ci sexo_ci edad_ci ///
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
vivi1_ch vivi2_ch viviprop_ch vivitit_ch vivialq_ch	vivialqimp_ch , first




compress


saveold "`base_out'", replace


log close



