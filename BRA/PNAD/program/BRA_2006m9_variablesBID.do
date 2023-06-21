* (versión Stata 13)
clear
set more off
*________________________________________________________________________________________________________________*

 * Activar si es necesario (dejar desactivado para evitar sobreescribir la base y dejar la posibilidad de 
 * utilizar un loop)
 * Los datos se obtienen de las carpetas que se encuentran en el servidor: ${surveysFolder}
 * Se tiene acceso al servidor únicamente al interior del BID.
 * El servidor contiene las bases de datos MECOvI.
 *________________________________________________________________________________________________________________*
 

global ruta = "${surveysFolder}"

local PAIS BRA
local ENCUESTA PNAD
local ANO "2006"
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
Generación nuevas variables LMK: Yessenia Loayza (desloay@hotmail.com)
Modificación 2014: Mayra Sáenz - Email: mayras@iadb.org - saenzmayra.a@gmail.com
versión 2010:  Yanira Oviedo
 Yessenia Loayza - Email: desloay@hotmail.com | yessenial@iadb.org
 Daniela Zuluaga E-mail: danielazu@iadb.org, da.zuluaga@hotmail.com - Octubre de 2017
Última modificación: Cesar Lins - Marzo 2021


							SCL/LMK - IADB
****************************************************************************/
****************************************************************************/
*Nota: Bases de datos con nuevos pesos (descargadas el 30 septiembre 2013)
use `base_in', clear

*****************
*** region_ci ***
*****************
*YL: generacion "region_c" proyecto maps America.
destring uf, replace		
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
/*    vARIABLES DEL HOGAR	*/
/********************************/

***************
****idh_ch*****
***************
sort uf v0102 v0103 v0403
egen idh_ch=group(uf v0102 v0103 v0403)
label variable idh_ch "ID del hogar"

gen idp_ci=v0301
gen factor_ch=v4729
gen zona_c=1 if v4728>=1 & v4728<=3
replace zona_c=0 if v4728>=4 & v4728<=8
gen str3 pais_c="BRA"
gen anio_c=2006
gen mes_c=9
gen relacion_ci=v0402
replace relacion_ci=5 if v0402==5|v0402==6|v0402==8
replace relacion_ci=6 if v0402==7
label define relacion_ci 1 "Jefe" 2 "Conyuge" 3 "Hijo" 4 "Otros Parientes" 5 "Otros no Parientes" 6 "Servicio Domestico"
label values relacion_ci relacion_ci

/************************************************************************/
/*			vARIABLES DE INFRAESTRUCTURA DEL HOGAR		*/
/************************************************************************/	

****************
***aguared_ch***
****************
gen aguared_ch=(v0212==2 | v0213==1)
label var aguared_ch "Acceso a fuente de agua por red"


*****************
*aguafconsumo_ch*
*****************
gen aguafconsumo_ch =0


*****************
*aguafuente_ch*
*****************
gen aguafuente_ch =.
replace aguafuente_ch = 1 if v0212 == 2 | v0213 == 1
replace aguafuente_ch = 10 if (v0212 == 4 |v0212 == 6|v0212 == 9)


*************
*aguadist_ch*
*************
gen aguadist_ch=.
replace aguadist_ch= 1 if v0211==1
replace aguadist_ch= 2 if (v0213==1|v0214==2)
replace aguadist_ch = 3 if (v0213 ==3 & v0214 ==4)


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

*****************
***aguamide_ch***
*****************
gen aguamide_ch=.


*****************
*bano_ch         *  Altered
*****************
gen bano_ch=.

replace bano_ch=1 if (v0217==1|v0217==2)
replace bano_ch=2 if v0217==3
replace bano_ch=6 if (v0217==4 | v0217==7)
replace bano_ch=4 if (v0217==5|v0217==6)
replace bano_ch=0 if v0215 == 3

***************
***banoex_ch***
***************
gen banoex_ch=(v0216==2)
replace banoex_ch=. if bano_ch==0 | bano_ch==.
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
replace sinbano_ch =  0 if v0215==1

*************
*aguatrat_ch*
*************
gen aguatrat_ch =9
replace aguatrat_ch = 1 if v0224==2
replace aguatrat_ch = 0 if v0224==4

		

gen luz_ch=(v0219==1)
replace luz_ch=. if v0219==9

gen luzmide_ch=.

gen combust_ch=(v0223==1|v0223==2|v0223==5)
replace combust_ch=. if v0223==9


gen des1_ch=1 if v0217>=1 & v0217<=3
replace des1_ch=2 if v0217==4
replace des1_ch=3 if v0217>=5
replace des1_ch=0 if bano_ch==0
replace des1_ch=. if v0217==9

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

gen piso_ch=.


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

gen resid_ch=0 if v0218==1 | v0218==2
replace resid_ch=1 if v0218==3
replace resid_ch=2 if v0218==4 | v0218==5
replace resid_ch=3 if v0218==6
replace resid_ch=. if v0218==9



gen dorm_ch=v0206
replace dorm_ch=. if v0206==99 |v0206==-1

gen cuartos_ch=v0205
replace cuartos_ch=. if v0205==99 | v0205==-1

gen cocina_ch=.

gen refrig_ch=(v0228==2 |v0228==4)
replace refrig_ch=. if v0228==9

gen freez_ch=(v0229==1)
replace freez_ch=. if v0229==9

gen auto_ch=.

gen telef_ch=(v2020==2)
replace telef_ch=. if v2020==9
capture gen compu_ch=(v0231==1)
capture gen internet_ch=(v0232==2)

gen cel_ch=(v0220==2)

gen viv1_ch=1 if v0202==2
replace viv1_ch=2 if v0202==4
replace viv1_ch=3 if v0202==6

gen viv2_ch=(viv1_ch==1 | viv1_ch==2)
replace viv2_ch=. if viv1_ch==.

gen viviprop_ch=0 if v0207==3
replace viviprop_ch=1 if v0207==1
replace viviprop_ch=2 if v0207==2
replace viviprop_ch=4 if v0207>=4
replace viviprop_ch=. if v0207==9

gen vivialq_ch=v0208
replace vivialq_ch=. if vivialq_ch>=999999999 | vivialq_ch<0

gen vivialqimp_ch=.



/************************************************************************/
/*				vARIABLES DEMOGRAFICAS			*/
/************************************************************************/
****************
***miembros_ci***
****************
gen miembros_ci=(relacion_ci<5)
label variable miembros_ci "Miembro del hogar"



gen factor_ci=v4729 /*AUN CUANDO HAY UN FACTOR DE PERSONAS ES IDENTICO AL DE HOGARES, EXCEPTO PARA EL '93 EN DONDE SE REGISTRAN vALORES NEGATIvOS! PARA HOMOGENEIZAR,A TODOS LES PONEMOS EL FACTOR DE EXPANSION DEL HOGAR*//*Yanira O: El 
factor de personas está incompleto desde la base original, por ello se usa el de hogares que es muy similar*/

gen sexo_ci=1 if v0302==2
replace sexo_ci=2 if v0302==4

gen edad_ci=v8005
replace edad_ci=. if edad_ci==999

gen civil_ci=.
capture replace civil_ci=1 if v1001==3 & v1003==3 /*EN ALGUNOS AÑOS NO ESTA EL MODULO DE NUPCIALIDAD!*/
capture replace civil_ci=2 if v1001==1
capture replace civil_ci=3 if v1004==2
capture replace civil_ci=4 if v1004==4

gen jefe_ci=(v0402==1)
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


/******************************************************************************/
/*				vARIABLES DE DEMANDA LABORAL		      */
/******************************************************************************/

/************************************************************************************************************************/

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
replace cotizando_ci=1 if (v9059==1 | v9099==1 | v9103==1 | v9120==2) & cotizando_ci==0 /*solo a emplead@s y asalariad@s, difiere con los otros paises*/
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
foreach var of varlist v1252 v1255 v1258 v1261{ 
replace `var'=. if `var'>=999999 | `var'==-1
}

gen pension_ci=0 
replace pension_ci=1 if (v1252>0 & v1252!=.) | (v1255>0 & v1255!=.) | (v1258>0 & v1258!=.) | (v1261>0 & v1261!=.) /*A todas las per mayores de diez años*/
label var pension_ci "1=Recibe pension contributiva"
 
*************
*ypen_ci*
*************
*sum v1252 v1255 v1258 v1261
egen ypen_ci=rsum (v1252 v1255 v1258 v1261)
replace ypen_ci=. if ypen_ci<=0
label var ypen_ci "valor de la pension contributiva"

****************
*instpen_ci*****
****************
gen instpen_ci=.
label var instpen_ci "Institucion proveedora de la pension - variable original de cada pais" 

***************
*pensionsub_ci*
***************
/*DZ Octubre 2017- Creacion de la variable  pension subsidiada*
http://dds.cepal.org/bdps/programa/?id=43
segun la fuente, el monto bpc para adultos mayores fue de 350 reales. Se encuentran beneficiarios con dicho monto*/
gen pensionsub_ci=(v1273==350)
label var pensionsub_ci "1=recibe pension subsidiada / no contributiva"


*****************
**  ypensub_ci  *
*****************
/*DZ Octubre 2017- Creacion de la variable valor de la pension subsidiada*
http://dds.cepal.org/bdps/programa/?id=43
segun la fuente, el monto bpc para adultos mayores fue de 350 reales. Se encuentran beneficiarios con dicho monto*/
gen ypensub_ci=v1273 if v1273==350
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
replace area=3 if v4727==1


*********
*lp_ci***
*********
gen lp_ci=.				
replace lp_ci=168.066285319749	if region==4	& area==1		/*sur-urbana*/
replace lp_ci=152.998273493942	if region==4	& area==2		/*sur-rural */
replace lp_ci=171.543518761481	if region==2	& area==1		/*noreste-urbana*/
replace lp_ci=152.998273493942	if region==2	& area==2		/*noreste-rural*/
replace lp_ci=134.453028241101	if region==3	& area==1		/*sudeste-urbano*/
replace lp_ci=114.748705135155	if region==3	& area==2		/*sudeste-rural*/
replace lp_ci=176.179830115111	if region==1	& area==1		/*norte-urbano*/
replace lp_ci=154.157351405841	if region==1	& area==2		/*norte-rural */
replace lp_ci=142.566573051161	if region==5	& area==1		/*centro oeste-urbano*/
replace lp_ci=125.180405607332	if region==5	& area==2		/*centro oeste-rural */
replace lp_ci=191.247841940919	if uf==33	& area==3		/*Rio de janeiro-metropolitano*/
replace lp_ci=162.270896201203	if uf==33	& area==1		/*Rio de janeiro-urbano*/
replace lp_ci=146.043806536988	if uf==33	& area==2		/*Rio de janeiro-rural*/
replace lp_ci=192.406919705835	if uf==35	& area==3		/*Sao Paulo-metropolitano*/
replace lp_ci=170.384440996564	if uf==35	& area==1		/*Sao paulo-urbano*/
replace lp_ci=139.089339565335	if uf==35	& area==2		 /*Sao paulo-rural*/
replace lp_ci=165.748129642934	if uf==53	& area==3		/*Distrito federal-metropolitana*/
replace lp_ci=213.270320650189	if region==4	& area==3	& uf==43	/*Porto alegre: sur-metropolitana-rio grande de sul*/
replace lp_ci=176.179830115111	if region==4	& area==3	& uf==41	/*curitiba:     sur-metropolitana-paraná*/
replace lp_ci=151.839195729026	if region==2	& area==3	& uf==23	/*Fortaleza:    noreste-metropolitana-ceará*/
replace lp_ci=199.361386736281	if region==2	& area==3	& uf==26	/*recife:       noreste-metropolitana-pernambuco*/
replace lp_ci=187.770608352205	if region==2	& area==3	& uf==29	/*salvador:     noreste-metropolitana-bahia*/
replace lp_ci=149.52104005221	if region==3	& area==3	& uf==31	/*belo horizonte:sureste-metropolitana-minas gerais*/
replace lp_ci=170.384440996564	if region==1	& area==3	& uf==15	/*belem: noreste-metropolitana-pará*/
label var lp_ci "Linea de pobreza oficial del pais"

***********
*lpe_ci ***
***********
gen lpe_ci=.				
replace lpe_ci=84.0331426598747	if region==4	& area==1		/*sur-urbana*/
replace lpe_ci=76.499136746971	if region==4	& area==2		/*sur-rural */
replace lpe_ci=85.7717593807404	if region==2	& area==1		/*noreste-urbana*/
replace lpe_ci=76.499136746971	if region==2	& area==2		/*noreste-rural*/
replace lpe_ci=67.2265141205506	if region==3	& area==1		/*sudeste-urbano*/
replace lpe_ci=57.3743525675773	if region==3	& area==2		/*sudeste-rural*/
replace lpe_ci=88.0899150575556	if region==1	& area==1		/*norte-urbano*/
replace lpe_ci=77.0786757029203	if region==1	& area==2		/*norte-rural */
replace lpe_ci=71.2832865255807	if region==5	& area==1		/*centro oeste-urbano*/
replace lpe_ci=62.5902028036659	if region==5	& area==2		/*centro oeste-rural */
replace lpe_ci=95.6239209704593	if uf==33	& area==3		/*Rio de janeiro-metropolitano*/
replace lpe_ci=81.1354481006013	if uf==33	& area==1		/*Rio de janeiro-urbano*/
replace lpe_ci=73.0219032684938	if uf==33	& area==2		/*Rio de janeiro-rural*/
replace lpe_ci=96.2034598529175	if uf==35	& area==3		/*Sao Paulo-metropolitano*/
replace lpe_ci=85.1922204982822	if uf==35	& area==1		/*Sao paulo-urbano*/
replace lpe_ci=69.5446697826675	if uf==35	& area==2		 /*Sao paulo-rural*/
replace lpe_ci=82.8740648214671	if uf==53	& area==3		/*Distrito federal-metropolitana*/
replace lpe_ci=106.635160325095	if region==4	& area==3	& uf==43	/*Porto alegre: sur-metropolitana-rio grande de sul*/
replace lpe_ci=88.0899150575556	if region==4	& area==3	& uf==41	/*curitiba:     sur-metropolitana-paraná*/
replace lpe_ci=75.9195978645128	if region==2	& area==3	& uf==23	/*Fortaleza:    noreste-metropolitana-ceará*/
replace lpe_ci=99.6806933681403	if region==2	& area==3	& uf==26	/*recife:       noreste-metropolitana-pernambuco*/
replace lpe_ci=93.8853041761023	if region==2	& area==3	& uf==29	/*salvador:     noreste-metropolitana-bahia*/
replace lpe_ci=74.7605200261052	if region==3	& area==3	& uf==31	/*belo horizonte:sureste-metropolitana-minas gerais*/
replace lpe_ci=85.1922204982822	if region==1	& area==3	& uf==15	/*belem: noreste-metropolitana-pará*/
label var lpe_ci "Linea de indigencia oficial del pais"
drop area


*************
**salmm_ci***
*************
gen salmm_ci=350
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
replace ocupa_ci=1 if v4810==2 | v4810==3 & emp_ci==1
replace ocupa_ci=2 if v4810==1 & emp_ci==1
replace ocupa_ci=3 if v4810==4 & emp_ci==1
replace ocupa_ci=4 if v4810==6 & emp_ci==1
replace ocupa_ci=5 if v4810==5 & emp_ci==1
replace ocupa_ci=6 if v4810==7 & emp_ci==1
replace ocupa_ci=7 if v4810==8 & emp_ci==1
replace ocupa_ci=8 if v4810==9 & emp_ci==1 
replace ocupa_ci=9 if v4810==10 & emp_ci==1

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
label define rama_ci 1 "Agricultura, Caza, Civicultura y Pesca" 2 "Explotación de minas y Canteras" 3 "Industrias Manufactureras" 4 "Electricidad, Gas y Agua" 5 "Construcción" 6 "Comercio al por mayor y menor, Restaurantes y Hoteles" 7 "Transporte y Almacenamiento" 8 "Establecimientos Financieros, Seguros y Bienes Inmuebles" 9 "Servicios Sociales, Comunales y personales" 
label values rama_ci rama_ci


/*EN TODOS LOS AÑOS, EXCEPTO EN EL '96, '97 Y '01 SE DESCRIBEN LAS CONDICIONES LABORALES DE LOS NIÑOS DE ENTRE 5 Y 9 AÑOS.
ESTO QUIERE DECIR QUE HAY QUE TENER EN CUENTA QUE EN ESOS AÑOS LOS INGRESOS DE ESTOS NIÑOS vAN A vALER 0, CUANDO EN OTROS 
AÑOS TIENEN UN vALOR POSITIvO. PARA MANTENER LA COMPARABILIDAD ACROSS TIME, SOLO SE DEBEN AGARRAR LOS INGRESOS DE LOS 
MAYORES DE 10 AÑOS, A MENOS QUE SE vAYAN A EXCLUIR LOS AÑOS 1996 Y 1997! ==>> CREAMOS DOS vARIABLES DE INGRESO, UNA QUE LOS 
TIENE EN CUENTA (ej: ylmprik_ci) Y OTRA QUE NO (ylmpri_ci original)*/ 

*****************
***horaspri_ci***
*****************

gen horaspri_ci=v9058
replace horaspri_ci=. if horaspri_ci==99 |horaspri_ci==-1 | v4714!=1 /*Necesitamos que sólo se fije en los empleados "adultos"*/
gen horasprik_ci=horaspri_ci
capture replace horasprik_ci=v0713 if edad_ci>=5 & edad_ci<=9
replace horasprik_ci=. if edad_ci>=5 & edad_ci<=9 & (horasprik_ci==99 | horasprik_ci==-1| emp_ci==0)

*2014,01 revision MLO
replace horaspri_ci=. if horaspri_ci<0 | horaspri_ci>150

*****************
***horastot_ci***
*****************
*gen horastot_ci=.
*2014, 01 incorporacion MLO
replace v9058 = . if v9058 == -1 | v9058 == 99
replace v9101 = . if v9101 == -1 | v9101 == 99
replace v9105 = . if v9105 == -1 | v9105 == 99

egen horastot_ci = rsum(v9058 v9101 v9105) 
replace horastot_ci = . if  (horaspri_ci==. & v9101==. & v9105==.) | v4714!=1 /*Necesitamos que sólo se fije en los empleados "adultos"*/
replace horastot_ci = . if horastot_ci < 0
replace horastot_ci = . if horastot_ci > 150




gen ylmpri_ci=v9532 
replace ylmpri_ci=. if v9532==-1 | v9532>=999999 | v4714!=1 

gen ylmprik_ci=v9532
replace ylmprik_ci=. if v9532==-1 | v9532>=999999 | emp_ci==0 
capture replace ylmprik_ci=v7122 if edad_ci>=5 & edad_ci<=9
capture replace ylmprik_ci=. if  edad_ci>=5 & edad_ci<=9 & (v7122==-1 | v7122>=999999 |emp_ci==0)

gen ylnmpri_ci=v9535 if edad_ci>=10
replace ylnmpri_ci=. if v9535==-1 | v9535>=999999 | v4714!=1

gen ylnmprik_ci=v9535
replace ylnmprik_ci=. if v9535==-1 | v9535>=999999 | emp_ci==0
capture replace ylnmprik_ci=v7125 if edad_ci>=5 & edad_ci<=9
capture replace ylnmprik_ci=. if edad_ci>=5 & edad_ci<=9 & (v7125==-1 | v7125>=999999 | emp_ci==0)

/*TODAS LAS vARIABLES "SECUNDARIAS": ylmsec_ci, ylnmsec_ci, ylmotros_ci, ylnmotros_ci Y durades_ci ESTAN CREADAS SÓLO PARA 
LOS MAYORES DE 10 AÑOS. POR LO TANTO LAS vARIABLES AGREGADAS CON SUFIJO k EN REALIDAD SÓLO SE REFIEREN A LA ACTIvIDAD 
PRINCIPAL DE LOS NIÑOS*/

gen ylmsec_ci=v9982 if edad_ci>=10
replace ylmsec_ci=. if v9982==-1 | v9982>=999999 | v4714!=1

gen ylnmsec_ci=v9985 if edad_ci>=10
replace ylnmsec_ci=. if v9985==-1 | v9985>=999999 | v4714!=1

gen ylmotros_ci=v1022 if edad_ci>=10
replace ylmotros_ci=. if v1022==-1 | v1022>=999999 | v4714!=1

gen ylnmotros_ci=v1025 if edad_ci>=10
replace ylnmotros_ci=. if v1025==-1 | v1025>=999999 | v4714!=1

gen nrylmpri_ci=(ylmpri_ci==. & v4714==1)
replace nrylmpri_ci=. if v4714==2

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


foreach var of varlist v1252 v1255 v1258 v1261 v1264 v1267 v1270 v1273{ 
replace `var'=. if `var'>=999999 | `var'==-1
}

egen ynlm_ci=rsum(v1252 v1255 v1258 v1261 v1264 v1267 v1270 v1273) if edad_ci>=10
replace ynlm_ci=. if (v1252==. &  v1255==. &  v1258==. &  v1261==. &  v1264==. &  v1267==. & v1270==. & v1273==.) | ynlm_ci<0

gen ynlnm_ci=.
sort idh_ch 
by idh_ch: egen nrylmpri_ch=max(nrylmpri_ci) if miembros_ci==1
by idh_ch: egen nrylmprik_ch=max(nrylmprik_ci) if miembros_ci==1

by idh_ch: egen ylm_ch=sum(ylm_ci)if miembros_ci==1
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

replace v1091=. if v1091==99 | v1091==-1
replace v1092=. if v1092==99 | v1092==-1

*Yanira Oviedo, Junio 2010: Se estaba multiplicando por 12, pero al ser un valor anual, debería dividirse 
/*
gen aux1=v1091/12
egen durades_ci=rsum(aux1 v1092) if  v4714!=1 & edad_ci>=10
replace durades_ci=. if (v1091==. & v1092==.) */
gen durades_ci=.
*MLO 03,2014

replace v9611=. if v9611==99 | v9611==-1
replace v9612=. if v9612==99 | v9612==-1
gen aux2=v9612/12
egen antiguedad_ci=rsum(v9611 aux2) if edad_ci>=10
replace antiguedad_ci=. if v9611==. & v9612==. 

drop aux*

/******************************************************************************************/
/*					vARIABLES DEL MERCADO LABORAL			  			*/
/******************************************************************************************/

gen desalent_ci=.
gen subemp_ci=.
gen tiempoparc_ci=.

gen categopri_ci=1 if v9029==4 | (v9008>=8 & v9008<=10)
replace categopri_ci=1 if v0708==4 | v0711==4
replace categopri_ci=2 if v0708==3 | v9029==3 |v0711==3 |(v9008>=5 & v9008<=7)
replace categopri_ci=3 if v0708==1 | v0708==2 |v9029==1 |v9029==2 |v0711==1 |v0711==2 | (v9008>=1 & v9008<=4)
replace categopri_ci=4 if (v0708>=5 & v0708<=8) |(v9029>=5 & v9029<=8) |(v0711>=5 & v0711<=8) | (v9008>=11 & v9008<=13)
replace categopri_ci=. if emp_ci!=1

gen categosec_ci=1 if v9092==4
replace categosec_ci=2 if v9092==3
replace categosec_ci=3 if v9092==1
replace categosec_ci=4 if v9092==5 |v9092==6
replace categosec_ci=. if emp_ci!=1 
gen nempleos_ci=1 if v9005==1
replace nempleos_ci=2 if v9005>1 & v9005!=.
/*
gen firmapeq=1 if v9008==1 & v9040<=4 /*v9008=Empleado permanente en el Agro*/
replace firmapeq=0 if v9008==1 & (v9040==6 | v9040==8) /*v9008=Empleado permanente en el Agro*/
replace firmapeq=1 if (v9008>=2 & v9008<=4) & ((v9013==1 & v9014<=6) | v9013==3) /*v9008= Algun tipo de empleado en el Agro*/
replace firmapeq=0 if (v9008>=2 & v9008<=4) & (v9013==1 & (v9014==8 | v9014==0)) /*v9008= Algun tipo de empleado en el Agro*/
replace firmapeq=1 if v9008==5 & ((v9049==1 & v9050<=6) | v9049==3) /*v9008=Cuenta propia en Servicios Auxiliares*/ 
replace firmapeq=0 if v9008==5 & v9049==1 & v9050==8 /*v9008=Cuenta propia en Servicios Auxiliares*/ 
replace firmapeq=1 if v9008==6 | v9008==7 /*Cuenta Propia en Agro o en otra actividad*/
replace firmapeq=0 if (v9008==8 | v9029==4) & ((v9048==0 | v9048==8) | ((v9048==2 | v9048==4) & v9049==1 & v9050>=6)) /*Empleador en los servicios auxiliares Agricolas o Empleador NO Agro*/
replace firmapeq=1 if (v9008==8 | v9029==4) & ((v9048<=6 & v9049==3) | ((v9048==2 | v9048==4) & v9049==1 & v9050<=4)) /*Empleador en los servicios auxiliares Agricolas o Empleador NO Agro*/
replace firmapeq=1 if (v9008==9 | v9008==10) & ((v9016==2 & v9017<=5 & v9018==4) | (v9016==2 & v9017<=3 & v9018==2 & v9019<=3) | (v9016==4 & v9018==2 & v9019<=5) | (v9016==4 & v9018==4)) /*Empleador en Agro u otras actividades*/
replace firmapeq=0 if (v9008==9 | v9008==10) & ((v9016==2 & (v9017==7 | v9017==8)) | (v9016==4 & v9018==2 & v9019>=5)) /*Empleador en Agro u otras actividades*/
replace firmapeq=1 if (v9008>=11 & v9008<=13) | (v9029>=5 & v9029<=7) /*Trabajador No remunerado*/
replace firmapeq=1 if v9029==1 & (v9032==2 & v9040<=4)  /*Empleado NO Agricola*/
replace firmapeq=0 if v9029==1 & (v9032==2 & (v9040==6 | v9040==8)) /*Empleado NO Agricola*/
/*Los empleados NO Agricolas que trabajan en el sector PUBLICO o que son empleados domesticos no tienen tamaño de firma!*/
replace firmapeq=1 if v9029==3 & (v9049==3 | (v9049==1 | v9050<=6))/*Cuenta Propia NO Agricola*/
replace firmapeq=0 if v9029==3 & (v9049==1 | (v9050==8 | v9050==0))/*Cuenta Propia NO Agricola*/
/*Que pasa con los trabajadores no remunerados? Se incluyen en tamaño de firma?*/

ren firmapeq firmapeq_ci
*cambio introducido el 06/13/05*
*/

gen spublico_ci=(v9032==4)
replace spublico_ci=. if v9032==9



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

En 2006 hay dos sistemas:  Esino fundamental ou 1er  grau, abarca 8 años de 
duración y siguen 4 de Esino fundamental ou 2do grau. El otro, se computan 4 años 
obligatorios de Elementar, 4 años de Medio 1er ciclo y se completa con 4 años 
de Medio 2do ciclo (científico, classico, etc..)

Para los que declaran cursos no seriados, en el caso de los asistentes, al 
no contar con información sobre los años de escolaridad aprobados (ya que 
declaran nivel pero no grado) se imputa por metodología el número máximo de años
del nivel anterior. Para los no asistentes, el procedimiento es análogo salvo 
para aquellos en los que pueda discriminarse la finalización del curso en cuyo 
caso se asignan los años que correspondan.

La Educación Pre-Vestibular hace referencia a cursos de nivelación cortos 
(menores a un año) que son requisito de admisión para las universidades 
o servicio público. En esos casos, se imputan los 12 años de educación 
por secundario completo.

En la encuesta, para el nivel superior (Maestría o Doctorado) no se pregunta el
grado al que asisten, por lo que se imputan los años requeridos para 
acceder a ese nivel. En caso de haber finalizado dicha instancia, se imputan 
como aprobados el promedio de duración entre maestría y doctorado al no poder 
discriminar que individuo pertenece a cada nivel.
*/

* Valores sem declaração van a perdidos:
replace v6002 = . if v6002 == 9
replace v0605 = . if v0605 == 9
replace v0606 = . if v0606 == 9
replace v0610 = . if v0610 == 9
replace v0611 = . if v0611 == 9 

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
gen nivel_asiste = v0603
gen grado_asiste = v0605
gen nivel_no_asiste = v0607
gen grado_no_asiste = v0610
gen finalizo = v0611

gen aedu_ci =. 
label var aedu_ci "Anios de educacion"

* PARA LOS QUE ASISTEN:
***********************
* Alfabetização de adultos, Creche, Pré-escolar
replace aedu_ci = 0 if inlist(nivel_asiste, 6, 7, 8) 

* Esino fundamental 1 y 2 
replace aedu_ci = grado_asiste - 1 if inlist(nivel_asiste, 1, 3) // Ensino fundamental ou 1º grau
replace aedu_ci = grado_asiste + 8 - 1  if inlist(nivel_asiste, 2, 4) // Ensino fundamental ou 2º grau

* Superior
replace aedu_ci = grado_asiste + 12 - 1 if nivel_asiste == 5 // Universitario

* Imputación para los que declaran nivel pero no grado (incluye no seriados)
replace aedu_ci = 0 if (inlist(nivel_asiste, 1, 3) & grado_asiste == .) // Ensino fundamental ou 1º grau
replace aedu_ci = 8 if (inlist(nivel_asiste, 2, 4) & grado_asiste == .) // Ensino fundamental ou 2º grau
replace aedu_ci = 12 if nivel_asiste == 9 // Pre-Vestibular 
replace aedu_ci = 12 if (nivel_asiste == 5 & grado_asiste == .) // Universitario
replace aedu_ci = 12 + 4 if nivel_asiste == 10 // Maestría o Doctorado


* PARA LOS QUE NO ASISTEN:
**************************

* Alfabetização de adultos, Creche, Pré-escolar
replace aedu_ci = 0 if inlist(nivel_no_asiste, 8, 9, 10)

* Esino fundamental 

* Sistema antiguo 
replace aedu_ci = grado_no_asiste if nivel_no_asiste == 1 // Elementar (primario) - 4 anios
replace aedu_ci = grado_no_asiste + 4 if nivel_no_asiste == 2 // Medio 1er ciclo (ginasal) - 4 anios
replace aedu_ci = grado_no_asiste + 8 if nivel_no_asiste == 3 // Medio 2do ciclo (científico, clasico etc.)

* Sistema actual
replace aedu_ci = grado_no_asiste if nivel_no_asiste == 4 // Ensino fundamental ou 1º grau
replace aedu_ci = grado_no_asiste + 8 if nivel_no_asiste == 5 // Ensino fundamental ou 2º grau

* Superior
replace aedu_ci = grado_no_asiste + 12 if nivel_no_asiste == 6 // Universitario

* Imputación para los que declaran nivel pero no grado

* No finalizado
replace aedu_ci = 0 if (inlist(nivel_no_asiste, 1, 4)  & grado_no_asiste == . & inlist(finalizo, 3, .)) // Elementar, Esino fundamental ou 1º grau
replace aedu_ci = 4 if (nivel_no_asiste == 2 & grado_no_asiste == . & inlist(finalizo, 3, .)) // Medio 1 
replace aedu_ci = 8 if (nivel_no_asiste == 3 & grado_no_asiste == . & inlist(finalizo, 3, .)) // Medio 2 
replace aedu_ci = 8 if (nivel_no_asiste == 5 & grado_no_asiste == . & inlist(finalizo, 3, .)) // Ensino fundamental ou 2º grau
replace aedu_ci = 12 if (nivel_no_asiste == 6 & grado_no_asiste == . & inlist(finalizo, 3, .)) // Universitario
replace aedu_ci = 12 + 4 if (nivel_no_asiste == 7 & grado_no_asiste == . & inlist(finalizo, 3, .)) // Maestría o Doctorado

* Finalizado 
replace aedu_ci = 4 if (nivel_no_asiste == 1  & grado_no_asiste == . & finalizo == 1) // Elementar
replace aedu_ci = 8 if (nivel_no_asiste == 2 & grado_no_asiste == . & finalizo == 1) // Medio 1 
replace aedu_ci = 8 if (nivel_no_asiste == 4 & grado_no_asiste == . & finalizo == 1) // Esino fundamental ou 1º grau
replace aedu_ci = 12 if (nivel_no_asiste == 3 & grado_no_asiste == . & finalizo == 1) // Medio 2 
replace aedu_ci = 12 if (nivel_no_asiste == 5 & grado_no_asiste == . & finalizo == 1) // Ensino fundamental ou 2º grau
replace aedu_ci = 12 + 4 if (nivel_no_asiste == 6 & grado_no_asiste == . & finalizo == 1) // Universitario
replace aedu_ci = 12 + 4 + 2 if (nivel_no_asiste == 7 & grado_no_asiste == . & finalizo == 1) // Maestría o Doctorado

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
g asispre_ci = (v0603 == 8) 
la var asispre_ci "Asiste a educacion prescolar"	
	
**************
***eduac_ci***
**************
* No puede discriminarse educación superior no unviersitaria.
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

				
********************************************************************************
********************************************************************************

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
gen categoinac_ci=1 if (v9122==2 | v9123==1) & condocup_ci==3
replace categoinac_ci=2 if v0602==2 & condocup_ci==3
replace categoinac_ci=3 if v9121==1 & condocup_ci==3
recode categoinac_ci .=4 if condocup_ci==3
label var  categoinac_ci "Condición de Inactividad" 
label define inactivo 1"Pensionado" 2"Estudiante" 3"Hogar" 4"Otros"
label values categoinac_ci inactivo


*variables que faltan generar
gen tcylmpri_ci=.
gen tcylmpri_ch=.



gen vivi1_ch =.
gen vivi2_ch =.
gen tipopen_ci=.
gen ylmho_ci=. 
gen vivitit_ch=.

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

order region_BID_c region_c pais_c anio_c mes_c zona_c factor_ch	idh_ch	idp_ci	factor_ci sexo_ci edad_ci ///
afroind_ci afroind_ch afroind_ano_c dis_ci dis_ch relacion_ci civil_ci jefe_ci nconyuges_ch nhijos_ch notropari_ch notronopari_ch nempdom_ch ///
clasehog_ch nmiembros_ch miembros_ci nmayor21_ch nmenor21_ch nmayor65_ch nmenor6_ch	nmenor1_ch	condocup_ci ///
categoinac_ci nempleos_ci emp_ci antiguedad_ci	desemp_ci cesante_ci durades_ci	pea_ci desalent_ci subemp_ci ///
tiempoparc_ci categopri_ci categosec_ci rama_ci spublico_ci tamemp_ci cotizando_ci instcot_ci	afiliado_ci ///
formal_ci tipocontrato_ci ocupa_ci horaspri_ci horastot_ci	pensionsub_ci pension_ci tipopen_ci instpen_ci	ylmpri_ci nrylmpri_ci ///
tcylmpri_ci ylnmpri_ci ylmsec_ci ylnmsec_ci	ylmotros_ci	ylnmotros_ci ylm_ci	ylnm_ci	ynlm_ci	ynlnm_ci ylm_ch	ylnm_ch	ylmnr_ch  ///
ynlm_ch	ynlnm_ch ylmhopri_ci ylmho_ci rentaimp_ch autocons_ci autocons_ch nrylmpri_ch tcylmpri_ch remesas_ci remesas_ch	ypen_ci	ypensub_ci ///
salmm_ci tc_c ipc_c lp19_c lp31_c lp5_c lp_ci lpe_ci aedu_ci eduno_ci edupi_ci edupc_ci	edusi_ci edusc_ci eduui_ci eduuc_ci	edus1i_ci ///
edus1c_ci edus2i_ci edus2c_ci edupre_ci eduac_ci asiste_ci pqnoasis_ci pqnoasis1_ci	repite_ci repiteult_ci edupub_ci ///
aguared_ch aguafconsumo_ch aguafuente_ch aguadist_ch aguadisp1_ch aguadisp2_ch aguamala_ch aguamejorada_ch aguamide_ch bano_ch banoex_ch banomejorado_ch sinbano_ch aguatrat_ch luz_ch luzmide_ch combust_ch des1_ch des2_ch piso_ch ///
pared_ch techo_ch resid_ch dorm_ch cuartos_ch cocina_ch telef_ch refrig_ch freez_ch auto_ch compu_ch internet_ch cel_ch ///
vivi1_ch vivi2_ch viviprop_ch vivitit_ch vivialq_ch	vivialqimp_ch , first

/*Homologar nombre del identificador de ocupaciones (isco, ciuo, etc.) y de industrias y dejarlo en base armonizada 
para análisis de trends (en el marco de estudios sobre el futuro del trabajo)*/
rename v9906 codocupa
rename v9907 codindustria

compress

saveold "`base_out'", version(12) replace


log close














