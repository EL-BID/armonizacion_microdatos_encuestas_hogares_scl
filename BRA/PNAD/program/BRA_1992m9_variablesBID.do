* (Versión Stata 13)
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

local PAIS BRA
local ENCUESTA PNAD
local ANO "1992"
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
Autores: 
Autor 2010: Yanira Oviedo
Generación nuevas variables LMK: Yessenia Loayza (desloay@hotmail.com)
Modificación 2014: Mayra Sáenz - Email: mayras@iadb.org - saenzmayra.a@gmail.com
Última versión: Yessenia Loayza - Email: desloay@hotmail.com | yessenial@iadb.org
Fecha última modificación: octubre 2013

							SCL/LMK - IADB
****************************************************************************/
****************************************************************************/
use `base_in', clear

*Solo hogares permenantes
keep if V0201==1

*****************
*** region_ci ***
*****************
*YL: generacion "region_c" proyecto maps America.	
rename UF uf
gen region_c = uf
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

************************
*** region según BID ***
************************
gen region_BID_c=4 
label var region_BID_c "Regiones BID"
label define region_BID_c 1 "Centroamérica_(CID)" 2 "Caribe_(CCB)" 3 "Andinos_(CAN)" 4 "Cono_Sur_(CSC)"
label value region_BID_c region_BID_c

/********************************/
/*    VARIABLES DEL HOGAR	*/
/********************************/
*YL: Se genera el id a nivel de familia no de unidade domiciliar
sort uf CONTROL V0103 V0403
egen idh_ch=group(uf CONTROL V0103 V0403)
sort idh_ch
by idh_ch: gen idp_ci=_n
gen factor_ch=V4611
gen zona_c=1 if V4105>=1 & V4105<=3
replace zona_c=0 if V4105>=4 & V4105<=8
gen str3 pais_c="BRA"
gen anio_c=1992
gen mes_c=.
gen relacion_ci=V0402
replace relacion_ci=5 if V0402==5|V0402==6|V0402==8
replace relacion_ci=6 if V0402==7
label define relacion_ci 1 "Jefe" 2 "Conyuge" 3 "Hijo" 4 "Otros Parientes" 5 "Otros no Parientes" 6 "Servicio Domestico"
label values relacion_ci relacion_ci

/************************************************************************/
/*			VARIABLES DE INFRAESTRUCTURA DEL HOGAR		*/
/************************************************************************/	
gen aguared_ch=(V0212==2 | V0213==1)
gen aguadist_ch=1 if V0211==1 |V0213==1
replace aguadist_ch=2 if V0214==2
replace aguadist_ch=3 if V0214==4
replace aguadist_ch=0 if V0214==9 
gen aguamala_ch=(V0212==6) /*"Otra"*/	
gen aguamide_ch=.
gen luz_ch=(V0219==1)
replace luz_ch=. if V0219==9
gen luzmide_ch=.
gen combust_ch=(V0223==1|V0223==2|V0223==5)
replace combust_ch=. if V0223==9
gen bano_ch=(V0215==1)
replace bano_ch=. if V0215==9
gen banoex_ch=(V0216==2)
replace banoex_ch=. if bano_ch==0 | bano_ch==.|V0216==9
gen des1_ch=1 if V0217>=1 & V0217<=3
replace des1_ch=2 if V0217==4
replace des1_ch=3 if V0217>=5
replace des1_ch=0 if bano_ch==0
replace des1_ch=. if V0217==9
gen des2_ch=des1_ch
replace des2_ch=. if des1_ch==3
gen piso_ch=.
gen pared_ch=0
replace pared_ch=1 if V0203==1 | V0203==2 |V0203==4
replace pared_ch=2 if V0203==6
replace pared_ch=. if V0203==9
gen techo_ch=0
replace techo_ch=1 if V0204<=5
replace techo_ch=2 if V0204==7
replace techo_ch=. if V0204==9
gen resid_ch=0 if V0218==1 | V0218==2
replace resid_ch=1 if V0218==3
replace resid_ch=2 if V0218==4 | V0218==5
replace resid_ch=3 if V0218==6
replace resid_ch=. if V0218==9

**Daniela Zuluaga- Enero 2018: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**
	
*********************
***aguamejorada_ch***
*********************
gen aguamejorada_ch = 1 if V0212 == 2 | V0212 == 4
replace aguamejorada_ch = 0 if V0212 == 6
				
*********************
***banomejorado_ch***
*********************
gen banomejorado_ch = 1 if (V0215 == 1 & (V0217 >= 1 & V0217 <=3) & V0216 == 2 )
replace banomejorado_ch = 0 if (V0215 == 1 & (V0217 >= 1 & V0217 <=3) & V0216 == 4) | V0215 == 3 | (V0215 == 1 & (V0217 >= 4 & V0217 <=7))


gen dorm_ch=V0206
replace dorm_ch=. if V0206==99 |V0206==-1
gen cuartos_ch=V0205
replace cuartos_ch=. if V0205==99 | V0205==-1
gen cocina_ch=.
gen refrig_ch=(V0228==2 |V0228==4)
replace refrig_ch=. if V0228==9
gen freez_ch=(V0229==1)
replace freez_ch=. if V0229==9
gen auto_ch=.
gen telef_ch=(V0220==2)
replace telef_ch=. if V0220==9
capture gen compu_ch=(V0231==1)
capture gen internet_ch=(V0232==2)
gen cel_ch=.
gen viv1_ch=1 if V0202==2
replace viv1_ch=2 if V0202==4
replace viv1_ch=3 if V0202==6
gen viv2_ch=(viv1_ch==1 | viv1_ch==2)
replace viv2_ch=. if viv1_ch==.
gen viviprop_ch=0 if V0207==3
replace viviprop_ch=1 if V0207==1
replace viviprop_ch=2 if V0207==2
replace viviprop_ch=4 if V0207>=4
replace viviprop_ch=. if V0207==9
gen vivialq_ch=V0208
*2014,01 modificacion MLO
*replace vivialq_ch=. if vivialq_ch>=999999999 | vivialq_ch<0
replace vivialq_ch=. if vivialq_ch>=99999999 | vivialq_ch<0
gen vivialqimp_ch=.

/************************************************************************/
/*				VARIABLES DEMOGRAFICAS			*/
/************************************************************************/
****************
***miembros_ci***
****************
gen miembros_ci=(relacion_ci<5)
label variable miembros_ci "Miembro del hogar"

*************************
*** VARIABLES DE RAZA ***
*************************

* MGR Oct. 2015: modificaciones realizadas en base a metodología enviada por SCL/GDI Maria Olga Peña

/*COR OU RACA V0404
2 BRANCA
4 PRETA
6 AMARELA
8 PARDA
0 INDIGENA
9 IGNORADA*/

gen raza_ci=.
replace raza_ci= 1 if  (V0404 ==0)
replace raza_ci= 2 if  (V0404 ==4 | V0404 ==8)
replace raza_ci= 3 if (V0404==2 | V0404==6 | V0404== 9)& raza_ci==.
label define raza_ci 1 "Indígena" 2 "Afro-descendiente" 3 "Otros"
label value raza_ci raza_ci 
label value raza_ci raza_ci
label var raza_ci "Raza o etnia del individuo" 

gen raza_idioma_ci=.

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


gen factor_ci=V4611 /*AUN CUANDO HAY UN FACTOR DE PERSONAS ES IDENTICO AL DE HOGARES, EXCEPTO PARA EL '93 EN DONDE SE REGISTRAN VALORES NEGATIVOS! PARA HOMOGENEIZAR,A TODOS LES PONEMOS EL FACTOR DE EXPANSION DEL HOGAR*/
gen sexo_ci=1 if V0302==2
replace sexo_ci=2 if V0302==4
gen edad_ci=V8005
replace edad_ci=. if edad_ci==999
capture gen civil_ci=1 if V1001==3 & V1003==3 /*EN ALGUNOS AÑOS NO ESTA EL MODULO DE NUPCIALIDAD!*/
capture replace civil_ci=2 if V1001==1
capture replace civil_ci=3 if V1004==2
capture replace civil_ci=4 if V1004==4
gen jefe_ci=(V0402==1)
sort idh_ch
by idh_ch: egen byte nconyuges_ch=sum(relacion_ci==2) 
by idh_ch: egen byte nhijos_ch=sum(relacion_ci==3)
by idh_ch: egen byte notropari_ch=sum(relacion_ci==4)
by idh_ch: egen byte notronopari_ch=sum(relacion_ci==5)
by idh_ch: egen byte nempdom_ch=sum(relacion_ci==6)
gen byte clasehog_ch=0
replace clasehog_ch=1 if nhijos_ch==0 & nconyuges_ch==0 & notropari_ch==0 & notronopari_ch==0 /*Unipersonal*/
replace clasehog_ch=2 if nhijos_ch>0 & notropari_ch==0 & notronopari_ch==0 /*Nuclear (child with or without spouse but without other relatives)*/
replace clasehog_ch=2 if nhijos_ch==0 & nconyuges_ch>0 & notropari_ch==0 & notronopari_ch==0 /*Nuclear (spouse with or without children but without other relatives)*/
replace clasehog_ch=3 if notropari_ch>0 & notronopari_ch==0 /*Ampliado*/
replace clasehog_ch=4 if ((nconyuges_ch>0 | nhijos_ch>0 | notropari_ch>0) & (notronopari_ch>0))/*Compuesto (some relatives plus non relative)*/
replace clasehog_ch=5 if nhijos_ch==0 & nconyuges_ch==0 & notropari_ch==0 & notronopari_ch>0 /*Corresidente*/
sort idh_ch
by idh_ch:egen byte nmiembros_ch=sum(relacion_ci>0 & relacion_ci<5) if miembros_ci==1
by idh_ch:egen byte nmayor21_ch=sum((relacion_ci>0 & relacion_ci<5) & (edad_ci>=21 & edad_ci<=98))
by idh_ch:egen byte nmenor21_ch=sum((relacion_ci>0 & relacion_ci<5) & (edad_ci<21))
by idh_ch:egen byte nmayor65_ch=sum((relacion_ci>0 & relacion_ci<5) & (edad_ci>=65))
by idh_ch:egen byte nmenor6_ch=sum((relacion_ci>0 & relacion_ci<5) & (edad_ci<6))
by idh_ch:egen byte nmenor1_ch=sum((relacion_ci>0 & relacion_ci<5) & (edad_ci<1))

/******************************************************************************/
/*				VARIABLES DE DEMANDA LABORAL		      */
/******************************************************************************/

/************************************************************************************************************************/


****************
****condocup_ci*
****************
gen condocup_ci=.
replace condocup_ci=1 if (V9001==1 | V9002==2 | V9003==1 | V9004==2)
replace condocup_ci=2 if  V9004==4 & (V9115==1 & (V9119>=1 & V9119<=8)) /*tomaron alguna providencia en la semana de referencia*/
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
replace cotizando_ci=1 if (V9059==1 | V9099==1 | V9103==1 | V9120==2) & cotizando_ci==0 /*solo a emplead@s y asalariad@s, difiere con los otros paises*/
label var cotizando_ci "Cotizante a la Seguridad Social"

gen cotizapri_ci=0     if condocup_ci==1 | condocup_ci==2 
replace cotizapri_ci=1 if (V9059==1) & cotizando_ci==0 
label var cotizapri_ci "Cotizante a la Seguridad Social por su trabajo principal"

gen cotizasec_ci=0     if condocup_ci==1 | condocup_ci==2 
replace cotizasec_ci=1 if (V9099==1) & cotizando_ci==0 
label var cotizasec_ci "Cotizante a la Seguridad Social por su trabajo secundario"

gen cotizaotros_ci=0     if condocup_ci==1 | condocup_ci==2 
replace cotizaotros_ci=1 if (V9103==1 | V9120==2) & cotizando_ci==0 
label var cotizaotros_ci "Cotizante a la Seguridad Social por otro trabajos o por aporte privado"

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
*sum v1252 v1255 v1258 v1261
* 2014, 01 revision MLO
foreach var of varlist V1252 V1255 V1258 V1261{ 
replace `var'=. if `var'>=999999 | `var'==-1
}

gen pension_ci=0 
replace pension_ci=1 if (V1251==1) | (V1254==2) | (V1257==3) | (V1260==4) /*A todas las per mayores de diez años*/

*replace pension_ci=1 if (V1252>0 & V1252!=.) | (V1255>0 & V1255!=.) | (V1258>0 & V1258!=.) | (V1261>0 & V1261!=.) /*A todas las per mayores de diez años*/
label var pension_ci "1=Recibe pension contributiva"
 
*************
*ypen_ci*
*************
sum V1252 V1255 V1258 V1261 
egen ypen_ci=rsum (V1252 V1255 V1258 V1261)
replace ypen_ci=. if ypen_ci<=0
label var ypen_ci "Valor de la pension contributiva"

****************
*instpen_ci*****
****************
gen instpen_ci=.
label var instpen_ci "Institucion proveedora de la pension - variable original de cada pais" 

***************
*pensionsub_ci*
***************
gen pensionsub_ci=.
label var pensionsub_ci "1=recibe pension subsidiada / no contributiva"

*****************
**  ypensub_ci  *
*****************
gen ypensub_ci=.
label var ypensub_ci "Valor de la pension subsidiada / no contributiva"

*************
*cesante_ci* 
*************
generat cesante_ci=0 if condocup_ci==2
replace cesante_ci=1 if (V9067==1 | V9106==2) & condocup_ci==2
label var cesante_ci "Desocupado - definicion oficial del pais"

*****************
*region /area ***
*****************

gen region=.	
replace region=1	if uf>=11 & uf<=17
replace region=2	if uf>=21 & uf<=29
replace region=3	if uf>=31 & uf<=35
replace region=4	if uf>=41 & uf<=43
replace region=5	if uf>=50 & uf<=53
label define region 1"norte" 2"Nordeste" 3"Sudeste/leste" 4"sul" 5"Centro_Oeste"
label value region region
label var region "distribución regional del país"

gen area=.
replace area=1 if zona_c==1
replace area=2 if zona_c==0
replace area=3 if V4107==1
label define area 1"urbana" 2"rural" 3"metropolitana" 
label value area area
label var area "area del pais"

*********
*lp_ci***
*********
gen lp_ci =.
replace lp_ci=321149.074390846	if region==4	& area==1		/*sur-urbana*/
replace lp_ci=292356.3986822	if region==4	& area==2		/*sur-rural */
replace lp_ci=327793.537907895	if region==2	& area==1		/*noreste-urbana*/
replace lp_ci=292356.3986822	if region==2	& area==2		/*noreste-rural*/
replace lp_ci=256919.259484591	if region==3	& area==1		/*sudeste-urbano*/
replace lp_ci=219267.299039736	if region==3	& area==2		/*sudeste-rural*/
replace lp_ci=336652.822784534	if region==1	& area==1		/*norte-urbano*/
replace lp_ci=294571.22004179	if region==1	& area==2		/*norte-rural */
replace lp_ci=272423.007906365	if region==5	& area==1		/*centro oeste-urbano*/
replace lp_ci=239200.689871743	if region==5	& area==2		/*centro oeste-rural */
replace lp_ci=365445.498493181	if uf==33	& area==3		/*Rio de janeiro-metropolitano*/
replace lp_ci=310074.968435478	if uf==33	& area==1		/*Rio de janeiro-urbano*/
replace lp_ci=279067.471507672	if uf==33	& area==2		/*Rio de janeiro-rural*/
replace lp_ci=367660.31957191	if uf==35	& area==3		/*Sao Paulo-metropolitano*/
replace lp_ci=325578.716829166	if uf==35	& area==1		/*Sao paulo-urbano*/
replace lp_ci=265778.544305058	if uf==35	& area==2		 /*Sao paulo-rural*/
replace lp_ci=316719.431952527	if uf==53	& area==3		/*Distrito federal-metropolitana*/
replace lp_ci=407527.101235924	if region==4	& area==3	& uf==43	/*Porto alegre: sur-metropolitana-rio grande de sul*/
replace lp_ci=336652.822784534	if region==4	& area==3	& uf==41	/*curitiba:     sur-metropolitana-paraná*/
replace lp_ci=290141.577603471	if region==2	& area==3	& uf==23	/*Fortaleza:    noreste-metropolitana-ceará*/
replace lp_ci=380949.246886868	if region==2	& area==3	& uf==26	/*recife:       noreste-metropolitana-pernambuco*/
replace lp_ci=358801.034695271	if region==2	& area==3	& uf==29	/*salvador:     noreste-metropolitana-bahia*/
replace lp_ci=285711.935165151	if region==3	& area==3	& uf==31	/*belo horizonte:sureste-metropolitana-minas gerais*/
replace lp_ci=325578.716829166	if region==1	& area==3	& uf==15	/*belem: noreste-metropolitana-pará*/
label var lp_ci "Linea de pobreza oficial del pais"

***********
*lpe_ci ***
***********
gen lpe_ci =.
replace lpe_ci=160574.537195423	if region==4	& area==1		/*sur-urbana*/
replace lpe_ci=146178.1993411	if region==4	& area==2		/*sur-rural */
replace lpe_ci=163896.768953948	if region==2	& area==1		/*noreste-urbana*/
replace lpe_ci=146178.1993411	if region==2	& area==2		/*noreste-rural*/
replace lpe_ci=128459.629742295	if region==3	& area==1		/*sudeste-urbano*/
replace lpe_ci=109633.649519868	if region==3	& area==2		/*sudeste-rural*/
replace lpe_ci=168326.411392267	if region==1	& area==1		/*norte-urbano*/
replace lpe_ci=147285.610020895	if region==1	& area==2		/*norte-rural */
replace lpe_ci=136211.503953182	if region==5	& area==1		/*centro oeste-urbano*/
replace lpe_ci=119600.344935872	if region==5	& area==2		/*centro oeste-rural */
replace lpe_ci=182722.74924659	if uf==33	& area==3		/*Rio de janeiro-metropolitano*/
replace lpe_ci=155037.484217739	if uf==33	& area==1		/*Rio de janeiro-urbano*/
replace lpe_ci=139533.735753836	if uf==33	& area==2		/*Rio de janeiro-rural*/
replace lpe_ci=183830.159785955	if uf==35	& area==3		/*Sao Paulo-metropolitano*/
replace lpe_ci=162789.358414583	if uf==35	& area==1		/*Sao paulo-urbano*/
replace lpe_ci=132889.272152529	if uf==35	& area==2		 /*Sao paulo-rural*/
replace lpe_ci=158359.715976263	if uf==53	& area==3		/*Distrito federal-metropolitana*/
replace lpe_ci=203763.550617962	if region==4	& area==3	& uf==43	/*Porto alegre: sur-metropolitana-rio grande de sul*/
replace lpe_ci=168326.411392267	if region==4	& area==3	& uf==41	/*curitiba:     sur-metropolitana-paraná*/
replace lpe_ci=145070.788801735	if region==2	& area==3	& uf==23	/*Fortaleza:    noreste-metropolitana-ceará*/
replace lpe_ci=190474.623443434	if region==2	& area==3	& uf==26	/*recife:       noreste-metropolitana-pernambuco*/
replace lpe_ci=179400.517347635	if region==2	& area==3	& uf==29	/*salvador:     noreste-metropolitana-bahia*/
replace lpe_ci=142855.967582576	if region==3	& area==3	& uf==31	/*belo horizonte:sureste-metropolitana-minas gerais*/
replace lpe_ci=162789.358414583	if region==1	& area==3	& uf==15	/*belem: noreste-metropolitana-pará*/
label var lpe_ci "Linea de indigencia oficial del pais"

*************
**salmm_ci***
*************
gen salmm_ci=522186.94 /* cruzeiros*/
label var salmm_ci "Salario minimo legal"

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

/*************************************************************************************************************************/
gen ocupa_ci=.
replace ocupa_ci=1 if V4710==1 & emp_ci==1
replace ocupa_ci=3 if V4710==2 & emp_ci==1
replace ocupa_ci=4 if V4710==5 & emp_ci==1
replace ocupa_ci=5 if V4710==7 & emp_ci==1
replace ocupa_ci=6 if V4710==3 & emp_ci==1
replace ocupa_ci=7 if (V4710==4 | V4710==6) & emp_ci==1 
replace ocupa_ci=9 if V4710==8 & emp_ci==1

gen rama_ci=.
replace rama_ci=1 if V9907>0 & V9907<50
replace rama_ci=2 if V9907>=50 & V9907<=59 
replace rama_ci=3 if V9907>=100 & V9907<=300 
replace rama_ci=4 if V9907>=351 & V9907<=353 
replace rama_ci=5 if V9907==340 
replace rama_ci=6 if (V9907>=410 & V9907<=419) | (V9907>=420 & V9907<=424)|(V9907>=511 & V9907<=512)
replace rama_ci=7 if (V9907>=471 & V9907<=477) | (V9907>=481 & V9907<=482)|V9907==583
replace rama_ci=8 if (V9907>=451 & V9907<=453) | (V9907>=461 & V9907<=464)
replace rama_ci=9 if (V9907>=610 & V9907<=619) | (V9907>=621 & V9907<=624)|(V9907>=631 & V9907<=632) | (V9907>=711 & V9907<=717) | (V9907>=721 & V9907<=727) | (V9907==801)| (V9907>=521 & V9907<=582) | (V9907>=584 & V9907<=610) | V9907==354
replace rama_ci=. if emp_ci==0
label define rama_ci 1 "Agricultura, Caza, Civicultura y Pesca" 2 "Explotación de minas y Canteras" 3 "Industrias Manufactureras" 4 "Electricidad, Gas y Agua" 5 "Construcción" 6 "Comercio al por mayor y menor, Restaurantes y Hoteles" 7 "Transporte y Almacenamiento" 8 "Establecimientos Financieros, Seguros y Bienes Inmuebles" 9 "Servicios Sociales, Comunales y personales" 
label values rama_ci rama_ci


/*EN TODOS LOS AÑOS, EXCEPTO EN EL '96, '97 Y '01 SE DESCRIBEN LAS CONDICIONES LABORALES DE LOS NIÑOS DE ENTRE 5 Y 9 AÑOS.
ESTO QUIERE DECIR QUE HAY QUE TENER EN CUENTA QUE EN ESOS AÑOS LOS INGRESOS DE ESTOS NIÑOS VAN A VALER 0, CUANDO EN OTROS 
AÑOS TIENEN UN VALOR POSITIVO. PARA MANTENER LA COMPARABILIDAD ACROSS TIME, SOLO SE DEBEN AGARRAR LOS INGRESOS DE LOS 
MAYORES DE 10 AÑOS, A MENOS QUE SE VAYAN A EXCLUIR LOS AÑOS 1996 Y 1997! ==>> CREAMOS DOS VARIABLES DE INGRESO, UNA QUE LOS 
TIENE EN CUENTA (ej: ylmprik_ci) Y OTRA QUE NO (ylmpri_ci original)*/ 

gen horaspri_ci=V9058
replace horaspri_ci=. if horaspri_ci==99 |horaspri_ci==-1 | V4714!=1 /*Necesitamos que sólo se fije en los empleados "adultos"*/
gen horasprik_ci=horaspri_ci
capture replace horasprik_ci=V0713 if edad_ci>=5 & edad_ci<=9
replace horasprik_ci=. if edad_ci>=5 & edad_ci<=9 & (horasprik_ci==99 | horasprik_ci==-1| emp_ci==0)

*2014,01 revision MLO
replace horaspri_ci=. if horaspri_ci<0 | horaspri_ci>150

*****************
***horastot_ci***
*****************
*gen horastot_ci=.
*2014, 01 incorporacio MLO
replace V9058 = . if V9058 == -1 | V9058 == 99
replace V9101 = . if V9101 == -1 | V9101 == 99
replace V9105 = . if V9105 == -1 | V9105 == 99

egen horastot_ci = rsum(V9058 V9101 V9105) 
replace horastot_ci = . if  (horaspri_ci==. & V9101==. & V9105==.) | V4714!=1 /*Necesitamos que sólo se fije en los empleados "adultos"*/

replace horastot_ci = . if horastot_ci < 0
replace horastot_ci = . if horastot_ci > 150

gen ylmpri_ci=V9532 
*2014, 01 Modificacion MLO
*replace ylmpri_ci=. if V9532==-1 | V9532>=999999999999 | V4714!=1 
replace ylmpri_ci=. if V9532==-1 | V9532>=99999999 | V4714!=1 

gen ylmprik_ci=V9532
replace ylmprik_ci=. if V9532==-1 | V9532>=99999999 | emp_ci==0 
*2014, 01 Modificacion MLO
*replace ylmprik_ci=. if V9532==-1 | V9532>=999999999999 | emp_ci==0
capture replace ylmprik_ci=V7122 if edad_ci>=5 & edad_ci<=9
*2014, 01 Modificacion MLO
*capture replace ylmprik_ci=. if  edad_ci>=5 & edad_ci<=9 & (V7122==-1 | V7122>=999999999999 |emp_ci==0)
capture replace ylmprik_ci=. if  edad_ci>=5 & edad_ci<=9 & (V7122==-1 | V7122>=99999999 |emp_ci==0)
gen ylnmpri_ci=V9535 if edad_ci>=10
*2014, 01 Modificacion MLO
*replace ylnmpri_ci=. if V9535==-1 | V9535>=999999999999 | V4714!=1
replace ylnmpri_ci=. if V9535==-1 | V9535>=99999999 | V4714!=1

gen ylnmprik_ci=V9535
*2014, 01 Modificacion MLO
*replace ylnmprik_ci=. if V9535==-1 | V9535>=999999999999 | emp_ci==0
replace ylnmprik_ci=. if V9535==-1 | V9535>=99999999 | emp_ci==0
capture replace ylnmprik_ci=V7125 if edad_ci>=5 & edad_ci<=9
*2014, 01 Modificacion MLO
*capture replace ylnmprik_ci=. if edad_ci>=5 & edad_ci<=9 & (V7125==-1 | V7125>=999999999999 | emp_ci==0)
capture replace ylnmprik_ci=. if edad_ci>=5 & edad_ci<=9 & (V7125==-1 | V7125>=99999999 | emp_ci==0)

/*TODAS LAS VARIABLES "SECUNDARIAS": ylmsec_ci, ylnmsec_ci, ylmotros_ci, ylnmotros_ci Y durades_ci ESTAN CREADAS SÓLO PARA 
LOS MAYORES DE 10 AÑOS. POR LO TANTO LAS VARIABLES AGREGADAS CON SUFIJO k EN REALIDAD SÓLO SE REFIEREN A LA ACTIVIDAD 
PRINCIPAL DE LOS NIÑOS*/

gen ylmsec_ci=V9982 if edad_ci>=10
*2014, 01 modificacion MLO
*replace ylmsec_ci=. if V9982==-1 | V9982>=999999999999 | V4714!=1
replace ylmsec_ci=. if V9982==-1 | V9982>=99999999 | V4714!=1

gen ylnmsec_ci=V9985 if edad_ci>=10
*replace ylnmsec_ci=. if V9985==-1 | V9985>=999999999999 | V4714!=1
replace ylnmsec_ci=. if V9985==-1 | V9985>=99999999 | V4714!=1

gen ylmotros_ci=V1022 if edad_ci>=10

*replace ylmotros_ci=. if V1022==-1 | V1022>=999999999999 | V4714!=1
replace ylmotros_ci=. if V1022==-1 | V1022>=99999999 | V4714!=1

gen ylnmotros_ci=V1025 if edad_ci>=10
*replace ylnmotros_ci=. if V1025==-1 | V1025>=999999999999 | V4714!=1
replace ylnmotros_ci=. if V1025==-1 | V1025>=99999999 | V4714!=1

gen nrylmpri_ci=(ylmpri_ci==. & V4714==1)
replace nrylmpri_ci=. if V4714==2

gen nrylmprik_ci=(ylmprik_ci==. & emp_ci==1)
replace nrylmprik_ci=. if emp_ci==0

egen ylm_ci=rsum(ylmpri_ci ylmsec_ci ylmotros_ci)
replace ylm_ci=. if ylmpri_ci==. & ylmsec_ci==. & ylmotros_ci==.

egen ylmk_ci=rsum(ylmprik_ci ylmsec_ci ylmotros_ci)
replace ylmk_ci=. if ylmprik_ci==. & ylmsec_ci==. & ylmotros_ci==.

egen ylnm_ci=rsum(ylnmpri_ci ylnmsec_ci ylnmotros_ci)
replace ylnm_ci=. if ylnmpri_ci==. & ylnmsec_ci==. & ylnmotros_ci==.

egen ylnmk_ci=rsum(ylnmprik_ci ylnmsec_ci ylnmotros_ci)
replace ylnmk_ci=. if ylnmprik_ci==. & ylnmsec_ci==. & ylnmotros_ci==.


foreach var of varlist V1252 V1255 V1258 V1261 V1264 V1267 V1270 V1273{ 
replace `var'=. if `var'>=999999 | `var'==-1
}
egen ynlm_ci=rsum(V1252 V1255 V1258 V1261 V1264 V1267 V1270 V1273) if edad_ci>=10
replace ynlm_ci=. if (V1252==. &  V1255==. &  V1258==. &  V1261==. &  V1264==. &  V1267==. & V1270==. & V1273==.) | ynlm_ci<0

gen ynlnm_ci=.
sort idh_ch 
by idh_ch: egen nrylmpri_ch=max(nrylmpri_ci) if miembros_ci==1
by idh_ch: egen nrylmprik_ch=max(nrylmprik_ci) if miembros_ci==1

by idh_ch: egen ylm_ch=sum(ylm_ci) if miembros_ci==1
by idh_ch: egen ylmk_ch=sum(ylmk_ci) if miembros_ci==1
by idh_ch: egen ylnm_ch=sum(ylnm_ci)if miembros_ci==1
by idh_ch: egen ylnmk_ch=sum(ylnmk_ci) if miembros_ci==1

gen ylmnr_ch=ylm_ch
replace ylmnr_ch=. if nrylmpri_ch==1
gen ylmnrk_ch=ylmk_ch
replace ylmnrk_ch=. if nrylmprik_ch==1

by idh_ch: egen ynlm_ch=sum(ynlm_ci)if miembros_ci==1
gen ynlnm_ch=.
*2015, 03 modificacion MLO
*gen ylmhopri_ci=ylmpri_ci/(horaspri_ci*4.2)
gen ylmhopri_ci=ylmpri_ci/(horaspri_ci*4.3)
gen ylmhoprik_ci=ylmprik_ci/(horasprik_ci*4.3)
replace ylmhopri_ci=. if ylmhopri_ci<=0
replace ylmhoprik_ci=. if ylmhoprik_ci<=0

gen rentaimp_ch=.
gen autocons_ch=.
gen autocons_ci=.
gen remesas_ci=.
sort idh_ch
gen remesas_ch=.

replace V1091=. if V1091==99 | V1091==-1
replace V1092=. if V1092==99 | V1092==-1

*Yanira Oviedo, Junio 2010: Se estaba multiplicando por 12, pero al ser un valor anual, debería dividirse 
*gen aux1=V1091*12
/*
gen aux1=V1091/12
egen durades_ci=rsum(aux1 V1092) if  V4714!=1 & edad_ci>=10
replace durades_ci=. if (V1091==. & V1092==.) */
*MLO 03,2014
gen durades_ci=.

replace V9611=. if V9611==99 | V9611==-1
replace V9612=. if V9612==99 | V9612==-1
gen aux2=V9612/12
egen antiguedad_ci=rsum(V9611 aux2) if emp_ci>=1
replace antiguedad_ci=. if V9611==. & V9612==. 

drop aux*


/******************************************************************************************/
/*					VARIABLES DEL MERCADO LABORAL			  			*/
/******************************************************************************************/

gen desalent_ci=.
gen subemp_ci=.
gen tiempoparc_ci=.

gen categopri_ci=1 if V9029==4 | (V9008>=8 & V9008<=10)
replace categopri_ci=1 if V0708==4 | V0711==4
replace categopri_ci=2 if V0708==3 | V9029==3 |V0711==3 |(V9008>=5 & V9008<=7)
replace categopri_ci=3 if V0708==1 | V0708==2 |V9029==1 |V9029==2 |V0711==1 |V0711==2 | (V9008>=1 & V9008<=4)
replace categopri_ci=4 if (V0708>=5 & V0708<=8) |(V9029>=5 & V9029<=8) |(V0711>=5 & V0711<=8) | (V9008>=11 & V9008<=13)
replace categopri_ci=. if emp_ci!=1

gen categosec_ci=1 if V9092==4
replace categosec_ci=2 if V9092==3
replace categosec_ci=3 if V9092==1
replace categosec_ci=4 if V9092==5 |V9092==6
replace categosec_ci=. if emp_ci!=1 
gen nempleos_ci=1 if V9005==1
replace nempleos_ci=2 if V9005>1 & V9005!=.
/*
gen firmapeq=1 if V9008==1 & V9040<=4 /*V9008=Empleado permanente en el Agro*/
replace firmapeq=0 if V9008==1 & (V9040==6 | V9040==8) /*V9008=Empleado permanente en el Agro*/
replace firmapeq=1 if (V9008>=2 & V9008<=4) & ((V9013==1 & V9014<=6) | V9013==3) /*V9008= Algun tipo de empleado en el Agro*/
replace firmapeq=0 if (V9008>=2 & V9008<=4) & (V9013==1 & (V9014==8 | V9014==0)) /*V9008= Algun tipo de empleado en el Agro*/
replace firmapeq=1 if V9008==5 & ((V9049==1 & V9050<=6) | V9049==3) /*V9008=Cuenta propia en Servicios Auxiliares*/ 
replace firmapeq=0 if V9008==5 & V9049==1 & V9050==8 /*V9008=Cuenta propia en Servicios Auxiliares*/ 
replace firmapeq=1 if V9008==6 | V9008==7 /*Cuenta Propia en Agro o en otra actividad*/
replace firmapeq=0 if (V9008==8 | V9029==4) & ((V9048==0 | V9048==8) | ((V9048==2 | V9048==4) & V9049==1 & V9050>=6)) /*Empleador en los servicios auxiliares Agricolas o Empleador NO Agro*/
replace firmapeq=1 if (V9008==8 | V9029==4) & ((V9048<=6 & V9049==3) | ((V9048==2 | V9048==4) & V9049==1 & V9050<=4)) /*Empleador en los servicios auxiliares Agricolas o Empleador NO Agro*/
replace firmapeq=1 if (V9008==9 | V9008==10) & ((V9016==2 & V9017<=5 & V9018==4) | (V9016==2 & V9017<=3 & V9018==2 & V9019<=3) | (V9016==4 & V9018==2 & V9019<=5) | (V9016==4 & V9018==4)) /*Empleador en Agro u otras actividades*/
replace firmapeq=0 if (V9008==9 | V9008==10) & ((V9016==2 & (V9017==7 | V9017==8)) | (V9016==4 & V9018==2 & V9019>=5)) /*Empleador en Agro u otras actividades*/
replace firmapeq=1 if (V9008>=11 & V9008<=13) | (V9029>=5 & V9029<=7) /*Trabajador No remunerado*/
replace firmapeq=1 if V9029==1 & (V9032==2 & V9040<=4)  /*Empleado NO Agricola*/
replace firmapeq=0 if V9029==1 & (V9032==2 & (V9040==6 | V9040==8)) /*Empleado NO Agricola*/
/*Los empleados NO Agricolas que trabajan en el sector PUBLICO o que son empleados domesticos no tienen tamaño de firma!*/
replace firmapeq=1 if V9029==3 & (V9049==3 | (V9049==1 | V9050<=6))/*Cuenta Propia NO Agricola*/
replace firmapeq=0 if V9029==3 & (V9049==1 | (V9050==8 | V9050==0))/*Cuenta Propia NO Agricola*/
/*Que pasa con los trabajadores no remunerados? Se incluyen en tamaño de firma?*/

ren firmapeq firmapeq_ci
*cambio introducido el 06/13/05*
*/

gen spublico_ci=(V9032==4)
replace spublico_ci=. if V9032==9





					****************************
					***	VARIABLES EDUCATIVAS ***
					****************************

*------------------------------------------------------------------------------------------------------------------
*YANIRA, Ag 2010: SE HACE UNA CORRECIÓN SOBRE LAS VARIABLES DE EDUCACIÓN. PUES LA VARIABLE DE INSUMO PARA CONSTRUIR 
*AÑOS DE EDUCACIÓN NO SE TUVO EN CUENTA UN CAMBIO EN LAS OPCIONES DE LAS VARIABLES INSUMO. LO CUAL GENERÓ UN ERROR
*------------------------------------------------------------------------------------------------------------------


**************
**asiste_ci***
**************

gen asiste_ci=(V0602==2)
label var asiste_ci "Personas que actualmente asisten a un centro de enseñanza"


*************
***aedu_ci***
*************

gen aedu_ci=.
label var aedu_ci "Anios de educacion"


*PARA LOS QUE NO ASISTEN
*************************

*Pre-escolar, creche o alfabetización de adultos
replace aedu_ci=0 if V0607==9 & asiste_ci==0

	*Sistema antiguo
*Elementar (primário) - se asume que el máximo es 4 - Anteriormente se permitía 6 pero no 5
replace aedu_ci=0  if V0607==1 & V0610==. & V0611!=1 & asiste_ci==0
replace aedu_ci=min(V0610,4) if V0607==1 & V0610>=1 & V0610<=6 & asiste_ci==0
*Medio 1 ciclo (ginasial, etc) - se asume que el máximo es 8
replace aedu_ci=min(V0610+4,8) if V0607==2 & V0610>=1 & V0610<=5 & asiste_ci==0
replace aedu_ci=4  if V0607==2 & V0610==. & V0611!=1 & asiste_ci==0
*Medio 2 ciclo (cientifico, clasico, etc, etc) - se asume que el máximo es 11, pero
*bajo la lógica anterior deberían se 12, ya que se permite hasta 4 años adicionales en este nivel
*Aunque solo es necesario tener 11 años de educación para completar la secundaria
replace aedu_ci=min(V0610+8,12) if V0607==3 & V0610>=1 & V0610<=4 & asiste_ci==0
replace aedu_ci=8  if V0607==3 & V0610==. & V0611!=1 & asiste_ci==0


	*Sistema nuevo
*Primeiro grau - Bajo este sistema la primaria llega hasta el grado 8
replace aedu_ci=min(V0610,8) if V0607==4 & V0610>=1 & V0610<=8 & asiste_ci==0
replace aedu_ci=0  if V0607==4 & V0610==. & V0611!=1 & asiste_ci==0
*Segundo grau - Secundaria son 4 años más
replace aedu_ci=min(V0610+8,12) if V0607==5 & V0610>=1 & V0610<=4 & asiste_ci==0
replace aedu_ci=8 if V0607==5 & V0610==. & V0611!=1 & asiste_ci==0

*Superior
replace aedu_ci=min(V0610+11,17) if V0607==6 & V0610>=1 & V0610<=8 & asiste_ci==0
replace aedu_ci=11 if V0607==6 & V0610==. & V0611!=1 & asiste_ci==0

*Maestria o doctorado  
*Para este ciclo no se pregunta el último año aprobado. Por lo tanto se supone que si terminó el ciclo 
*el individuo cuenta con 19 años de educación (2 años más de educación), si el individuo no terminó se le agrega 
*1 año más de eduación para quedar con 18 ya que si el último ciclo más alto alcanzado es postgrado, el individuo 
*por lo menos tuvo que cursar 1 año en ese nivel
replace aedu_ci=18 if V0607==7 & V0611==3 & asiste_ci==0
replace aedu_ci=19 if V0607==7 & V0611==1 & asiste_ci==0




*PARA LOS QUE ASISTEN
**********************

*Pre-escolar, creche o alfabetización de adultos
replace aedu_ci=0 if (V0603==6| V0603==7) & asiste_ci==1

*Regular de 1º grau/ Supletivo de 1º grau   (se asume que el máximo es 8) 
replace aedu_ci=0  if (V0603==1 | V0603==3) & V0605==. & asiste_ci==1
replace aedu_ci=min(V0605-1,7) if (V0603==1 | V0603==3) & V0605>=1 & V0605<=8 & asiste_ci==1
*Regular de 2º grau/ Supletivo de 2º grau   (se asume que el máximo es 4, pero con 3 basta para completar el ciclo)
replace aedu_ci=min(V0605+8-1,11) if (V0603==2 | V0603==4) & V0605>=1 & V0605<=4 & asiste_ci==1
replace aedu_ci=8  if (V0603==2 | V0603==4) & V0605==. & asiste_ci==1

*Pre-vestibular
replace aedu_ci=11  if V0603==8 & asiste_ci==1

*Superior
replace aedu_ci=min(V0605+11-1,17) if V0603==5 & V0605>=1 & V0605<=8 & asiste_ci==1
replace aedu_ci=11 if V0603==5 & V0605==. & asiste_ci==1

*Maestria o doctorado  
*Si el último ciclo más alto alcanzado es postgrado, el individuo por lo menos tuvo que cursar 1 año en ese nivel
replace aedu_ci=18 if V0603==9  & asiste_ci==1

*Se deja sólo la información de las personas con 5 años o más
replace aedu_ci=. if edad_ci<5



** zero if never attended school and invalid anwsers for all other school questions
	replace aedu_ci=0 if (V0606==4) & (V0602==4) & (V0607<1 | V0607>7) & (V0610==0 | V0610==9)

** zero if 
	replace aedu_ci=0 if (V0602==4) & (V0606==4) & (V0603<1 | V0603==6 | V0603==7) & (V0605==0 | V0605==9)

** zero if never attended and it is not even attending now
	replace aedu_ci=0 if V0607==. & V0603==.

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
replace edupi_ci=1 if aedu_ci>0 & aedu_ci<8
replace edupi_ci=. if aedu_ci==.
label variable edupi_ci "Primaria incompleta"

**************
***edupc_ci***
**************
gen byte edupc_ci=0
replace edupc_ci=1 if aedu_ci==8
replace edupc_ci=. if aedu_ci==.
label variable edupc_ci "Primaria completa"

**************
***edusi_ci***
**************
gen byte edusi_ci=0
replace edusi_ci=1 if aedu_ci>8 & aedu_ci<11
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
*La secundaria sólo dura 4 años. No puede divirse en ciclos
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

**************
***eduac_ci***
**************
gen byte eduac_ci=.
label variable eduac_ci "Superior universitario vs superior no universitario"


foreach var of varlist edu* {
replace `var'=. if aedu_ci==.
}

******************
***pqnoasist_ci***
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
label var repite_ci "Personas que han repetido al menos un año o grado"

***************
***edupub_ci***
***************
gen edupub_ci=.
label var  edupub_ci "Personas que asisten a centros de enseñanza públicos"



*******************
***tamemp_ci*******
*******************
gen tamemp_ci=1 if V9019==1 | V9019==3 | V9019==5 |V9017==1 | V9017==3 | V9017==5 | V9040==2 | V9040==4 | V9048==2 | V9048==4 | V9048==6 
replace tamemp_ci=2 if V9019==7 | V9017==7 | V9040==6 | V9048==8
replace tamemp_ci=3 if V9019==8 | V9017==8 | V9040==8 | V9048==0
label var  tamemp_ci "Tamaño de Empresa" 
label define tamaño 1"Pequeña" 2"Mediana" 3"Grande"
label values tamemp_ci tamaño

******************
***categoinac_ci**
******************
gen categoinac_ci=1 if (V9122==2 | V9123==1) & condocup_ci==3
replace categoinac_ci=2 if V0602==2 & condocup_ci==3
replace categoinac_ci=3 if V9121==1 & condocup_ci==3
recode categoinac_ci .=4 if condocup_ci==3
label var  categoinac_ci "Condición de Inactividad" 
label define inactivo 1"Pensionado" 2"Estudiante" 3"Hogar" 4"Otros"
label values categoinac_ci inactivo


*variables que faltan generar
gen tcylmpri_ci=.
gen tcylmpri_ch=.
gen edus1c_ci=.
gen repiteult_ci=.
gen vivi1_ch =.
gen vivi2_ch =.
gen tipopen_ci=.
gen ylmho_ci=. 
gen vivitit_ch=.
gen compu_ch=.
gen internet_ch=.

**Cambio de moneda - Modificación Mayra Sáenz Septiembre 2014

sum ylmpri_ci nrylmpri_ci tcylmpri_ci ylnmpri_ci ylmsec_ci ylnmsec_ci ylmotros_ci ylnmotros_ci ylm_ci ylnm_ci ynlm_ci ynlnm_ci ylm_ch ylnm_ch ylmnr_ch ynlm_ch ynlnm_ch ylmhopri_ci ylmho_ci rentaimp_ch autocons_ci autocons_ch nrylmpri_ch tcylmpri_ch remesas_ci remesas_ch ypen_ci ypensub_ci salmm_ci lp_ci lpe_ci vivialq_ch vivialqimp_ch

local varing "ylmpri_ci nrylmpri_ci tcylmpri_ci ylnmpri_ci ylmsec_ci ylnmsec_ci ylmotros_ci ylnmotros_ci ylm_ci ylnm_ci ynlm_ci ynlnm_ci ylm_ch ylnm_ch ylmnr_ch ynlm_ch ynlnm_ch ylmhopri_ci ylmho_ci rentaimp_ch autocons_ci autocons_ch nrylmpri_ch tcylmpri_ch remesas_ci remesas_ch ypen_ci ypensub_ci salmm_ci lp_ci lpe_ci vivialq_ch vivialqimp_ch"
foreach e of local varing {
replace `e' = `e'/2750000 
}

sum ylmpri_ci nrylmpri_ci tcylmpri_ci ylnmpri_ci ylmsec_ci ylnmsec_ci ylmotros_ci ylnmotros_ci ylm_ci ylnm_ci ynlm_ci ynlnm_ci ylm_ch ylnm_ch ylmnr_ch ynlm_ch ynlnm_ch ylmhopri_ci ylmho_ci rentaimp_ch autocons_ci autocons_ch nrylmpri_ch tcylmpri_ch remesas_ci remesas_ch ypen_ci ypensub_ci salmm_ci lp_ci lpe_ci vivialq_ch vivialqimp_ch


/*_____________________________________________________________________________________________________*/
* Asignación de etiquetas e inserción de variables externas: tipo de cambio, Indice de Precios al 
* Consumidor (2011=100), Paridad de Poder Adquisitivo (PPA 2011),  líneas de pobreza
/*_____________________________________________________________________________________________________*/


do "$ruta\harmonized\_DOCS\\Labels&ExternalVars_Harmonized_DataBank.do"

/*_____________________________________________________________________________________________________*/
* Verificación de que se encuentren todas las variables armonizadas 
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

/*Homologar nombre del identificador de ocupaciones (isco, ciuo, etc.) y de industrias y dejarlo en base armonizada 
para análisis de trends (en el marco de estudios sobre el futuro del trabajo)*/
rename V9906 codocupa
rename V9907 codindustria

compress

saveold "`base_out'", version(12) replace


log close

