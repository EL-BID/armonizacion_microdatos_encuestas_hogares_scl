* (Versi??tata 12)
clear
set more off
*________________________________________________________________________________________________________________*

 * Activar si es necesario (dejar desactivado para evitar sobreescribir la base y dejar la posibilidad de 
 * utilizar un loop)
 * Los datos se obtienen de las carpetas que se encuentran en el servidor: ${surveysFolder}
 * Se tiene acceso al servidor ??amente al interior del BID.
 * El servidor contiene las bases de datos MECOVI.
 *________________________________________________________________________________________________________________*
 

global ruta = "${surveysFolder}"

local PAIS JAM
local ENCUESTA SLC
local ANO "2012"
local ronda m5 

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
*local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\\`PAIS'_`ANO'`ronda'.dta"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"


capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Jamaica
Encuesta: SLC
Round: a
Autores: Mayra Sáenz E-mail: saenzmayra.a@gmail.com - mayras@iadb.org
Versiones anteriores: Yessenia Loayza
Fecha última modificación: 17 de noviembre de 2016

							SCL/LMK - IADB
****************************************************************************/
****************************************************************************/



use `base_in', clear



/*clear all 
set memory 200m
set more off
use "${surveysFolder}\survey\JAM\SLC\2012\m5\data_merge\JAM_2012m5.dta", clear */


*MS: Este do file requiere revisarse, sólo se encuentran armonizadas ciertas variables.

//////////////////////////////////////////////////////////////////////////
         											   
 *       			JAMAICA 2010
                             												   
///////////////////////////////////////////////////////////////////////// */ 


**********
* pais_c *
**********

gen str pais_c="JAM"

**********
* anio_c *
**********

gen anio_c=2012

*********
* mes_c *
*********

gen mes_c= int_mth
label var mes_c 
destring mes_c, replace 
label define mes_c 6"JUN" 7"JUL" 8"AUG" 9"SEP" 10"OCT" 11"NOV"
label values mes_c mes_c

**********
* zona_c *
**********

destring area, replace
gen zona_c=.
replace zona_c=1 if area==1 | area==2 
replace zona_c=0 if area==3
label define zona_c 0 "Rural" 1 "Urbana"
label value zona_c zona_c

**********
* idh_ch *
**********

egen idh_ch=group(serial)
label var idh_ch "Identificador Unico del Hogar"

**********
* idp_ci *
**********

gen idp_ci=ind
label var idp_ci "Identificador Individual dentro del Hogar"

***************
* relacion_ci *
***************

/*
 RELAT                   
 1  head
 2  spouse
 3  child
 4  child's spouse
 5  grandchild
 6  parent hd/sp
 7  other relative
 8  helper
 9  other */

destring relat, replace
gen relacion_ci=.
replace relacion_ci=1 if relat==1
replace relacion_ci=2 if relat==2
replace relacion_ci=3 if relat==3
replace relacion_ci=4 if relat>=4 & relat<=7 /* Otros familiares */
replace relacion_ci=5 if relat==9
replace relacion_ci=6 if relat==8 /*Es el sevicio domestico, Incluye a familiares del Serv. Domestico*/

label var relacion_ci "Relacion con el Jefe de Hogar"
label define relacion_ci 1 "Jefe de Hogar" 2 "Conyuge/Pareja" 3 "Hijo(a)/Hijastro(a)" 4 "Otros Parientes" 5 "Otros No parientes" 6 "Servicio Domestico"
label value relacion_ci relacion_ci

/* It is not a “weighting factor” but a “non response factor”. 
It means that when the weight command is applied, the proportions found are right but absolute numbers cannot be derived. */

***************
* factor2_ci  *
***************

gen factor2_ci=finwght*100000000 
label var factor2_ci "Factor de Expansion del Individuo (no suma la poblacion)"

*************
* factor_ch *
*************

gen factor_ch=finwght
label var factor_ch "Factor de expansion del Hogar"

** Population in Jamaica in 2008 = 2,804,332 **
* Se reajustan los ponderadores:
*Modificacion Mayra Sáens Mayo 2014 Population 2010= 2703600

sum finwght
scalar pob=r(sum)
gen pop=finwght*(2703600/pob)

/*
THIS 1000000 CAUSES PROBLEMS WHEN WEIGHTING SO IT WILL BE DROPPED 
sum pop
ret list
gen factor_ci=pop*1000000  Se debe dividir la poblacion por 1000000 
drop pop
label var factor_ci "Factor de Expansion del Individuo (Se debe dividir la poblacion por 1000000)"
*/

*************
* factor_ci * 
*************

sum pop
ret list
gen factor_ci=pop 
drop pop
label var factor_ci "Factor de Expansion del Individuo"

***********
* sexo_ci *
***********
destring sex, replace
gen sexo_ci=1     if sex==1
replace sexo_ci=2 if sex==2
label var sexo_ci "Sexo del Individuo"
label define sexo_ci 1 "Masculino" 2 "Femenino"
label value sexo_ci sexo_ci

***********
* edad_ci *
***********

** Generating Edad
destring ageyrs, replace

gen edad_ci=ageyrs 
label var edad_ci "Edad del Individuo"

************
* civil_ci *
************
/*
MARRIED........1 
NEVER MARRIED..2
DIVORCED.......3
SEPARATED......4
WIDOWED........5
*/

destring marital, replace
gen byte civil_ci=.
replace civil_ci=1 if  marital==2
replace civil_ci=2 if  marital==1
replace civil_ci=3 if  marital==3 | marital==4
replace civil_ci=4 if  marital==5
label var civil_ci "Estado Civil"
label define civil_ci 1 "Soltero" 2 "Union Formal o Informal" 3 "Divorciado o Separado" 4 "Viudo"
label value civil_ci civil_ci


***********
* jefe_ci *
***********

g jefe_ci=(relat==1)
label var jefe_ci "Jefe de Hogar Declarado"

sort idh

****************
* nconyuges_ch *
****************
egen byte nconyuges_ch=sum(relacion_ci==2), by (idh)
label variable nconyuges_ch "Numero de Conyuges"

*************
* nhijos_ch *
*************
egen byte nhijos_ch=sum(relacion_ci==3), by (idh)
label variable nhijos_ch "Numero de Hijos"

****************
* notropari_ch *
****************
egen byte notropari_ch=sum(relacion_ci==4),by (idh)
label variable notropari_ch "Numero de Otros Parientes "

******************
* notronopari_ch *
******************
egen byte notronopari_ch=sum(relacion_ci==5), by (idh)
label variable notronopari_ch "Numero de Otros NO Parientes "

**************
* nempdom_ch *
**************
egen byte nempdom_ch=sum(relacion_ci==6), by (idh)
label variable nempdom_ch "Numero de Empleados Domesticos"

****************
* clasehog_ch *
****************
g clasehog_ch=.
replace clasehog_ch=1 if nhijos_ch==0 & nconyuges_ch==0 & notropari_ch==0 & notronopari_ch==0 /* unipersonal*/
replace clasehog_ch=2 if nhijos_ch>0 & notropari_ch==0 & notronopari_ch==0 /* nuclear (child with or without spouse but without other relatives)*/
replace clasehog_ch=2 if nconyuges_ch>0 & notropari_ch==0 & notronopari_ch==0 /* nuclear (spouse with or without children but without other relatives)*/
replace clasehog_ch=3 if notropari_ch>0 & notronopari_ch==0 /* ampliado*/
replace clasehog_ch=4 if (nconyuges_ch>0 | nhijos_ch>0 | (notropari_ch>0 & notropari_ch<.)) & (notronopari_ch>0 & notronopari_ch<.) /* compuesto  (some relatives plus non relative)*/
replace clasehog_ch=5 if nhijos_ch==0 & nconyuges_ch==0 & notropari_ch==0 & notronopari_ch>0 & notronopari_ch<./** corresidente*/
label variable clasehog_ch "CLASE HOGAR"
label define clasehog_ch 1 "Unipersonal" 2 "Nuclear" 3 "Ampliado" 4 "Compuesto" 5 "Corresidente"
label value clasehog_ch clasehog_ch

***************
* miembros_ch *
***************
sort idh idp
egen nmiembros_ch=sum(relacion_ci>0 & relacion_ci<=5), by (idh)
replace nmiembros_ch=. if relacion_ci==.
label variable nmiembros_ch "Numero de miembros del Hogar"

***************
* miembros_ci *
***************
g miembros_ci=.
replace miembros_ci=1 if relacion_ci>=1 & relacion_ci<=5
replace miembros_ci=0 if relacion_ci==6 /*Empleados domesticos y sus familiares */
label variable miembros_ci "Variable dummy que indica las personas que son miembros del Hogar"

***************
* nmayor21_ch *
***************
egen nmayor21_ch=sum((relacion_ci>0 & relacion_ci<=5) & (edad_ci>=21)), by (idh)
label variable nmayor21_ch "Numero de personas de 21 años o mas dentro del Hogar"

***************
* nmenor21_ch *
***************
egen nmenor21_ch=sum((relacion_ci>0 & relacion_ci<=5) & (edad_ci<21)), by (idh)
label variable nmenor21_ch "Numero de personas menores a 21 años dentro del Hogar"

***************
* nmayor65_ch *
***************
egen nmayor65_ch=sum((relacion_ci>0 & relacion_ci<=5) & (edad_ci>=65)), by (idh)
label variable nmayor65_ch "Numero de personas de 65 años o mas dentro del Hogar"

**************
* nmenor6_ch *
**************
egen nmenor6_ch=sum((relacion_ci>0 & relacion_ci<=5) & (edad_ci<6)), by (idh)
label variable nmenor6_ch "Numero de niños menores a 6 años dentro del Hogar"

**************
* nmenor1_ch *
**************
egen nmenor1_ch=sum((relacion_ci>0 & relacion_ci<=5) & (edad_ci<1)),  by (idh)
label variable nmenor1_ch "Numero de niños menores a 1 año dentro del Hogar"



******************************************************************************
*		LABOR DEMAND
******************************************************************************

******************************
*	ocupa_ci
******************************



******************************
*	rama_ci
******************************


******************************
*	ylmpri_ci 
******************************

******************************
*	ylmsec_ci
******************************
*g ylmsec_ci=.	
*NA
******************************
*	ylnmsec_ci
******************************
*g ylnmsec_ci=.	
*NA
*label var ylnmsec_ci "Ingreso laboral no monetario actividad secundaria"

******************************
*	ylm_ci 
******************************

******************************
*	ylnm_ci 
******************************
*g ylnm_ci=.
*label var ylnm_ci "Ingreso laboral no monetario total"
*NA

******************************
*	ynlm_ci
******************************
*g ynlm_ci=.
*NA
*label var ynlm_ci "Ingreso no laboral monetario" 

******************************
*	ylm_ch  
******************************


******************************
*	ylmnr_ch  
******************************

******************************
*	ylnm_ch 
******************************
*by idh_ch, sort: egen ylnm_ch=sum(ylnm_ci) if miembros_ci==1
*label var ylnm_ch "Ingreso laboral no monetario del hogar" 
*NA
******************************
*	ynlm_ch 
******************************
*by idh_ch, sort: egen ynlm_ch=sum(ynlm_ci) if miembros_ci==1
*label var ynlm_ch "Ingreso no laboral monetario del Hogar"
*NA
******************************
*	ynlnm_ch 
******************************
/*g ynlnm_ch=.
label var ynlnm_ch "Ingreso no laboral no monetario del Hogar"*/
******************************
*	ylmhopri_ci 
*****************************


******************************
*	ylmho_ci 
******************************


******************************
*	autocons_ci & autocons_ch 
******************************
*g autocons_ci=.
*label var autocons_ci "Autoconsumo Individual"
*NA

*egen autocons_ch=.
*label var autocons_ch "Autoconsumo del Hogar"
*NA
******************************
*	remesas_ci & remesas_ch
******************************
*g remesas_ci=k02703/(365/30)
*label var remesas_ci "Remesas Individuales"
*NA

**************
* durades_ci *
**************

*****************
* antiguedad_ci *
*****************
*Ted generated antiguedad_ci from SLC survey that I'll keep it. 


/*REVISAR DESDE ESTA SECCIóN


******************************************************************************
*	EDUCATION
******************************************************************************

/* b1
NURSERY/DAYCARE............01 (EOV)
BASIC/INFANT/KINDERGARTEN..02 
PRIMARY....................03 
PREPARATORY................04 
ALL-AGE 1-6................05 
ALL-AGE 7-9................06 
PRIM & JH 1-6..............07 
PRIM & JH 7-9..............08 
JUN HIGH 7-9...............09 
SECONDARY HIGH.............10 
TECHNICAL..................11 
VOC-AGRI...................12 
UNIVERSITY.................13 (EOV)
OTHER TERT PUB.............14 (EOV)
OTHER TERT PRI.............15 (EOV)
ADULT LIT .................16 (EOV)
ADULT ED/NIGHT SCH.........17 (EOV)
SPECIAL SCH................18 (EOV)
NONE.......................19 (EOV)
*/

*replace b1 = "." if b1 =="NN"
destring b1, replace 

gen NIVEL=0 if b1==1 | b1==2 | b1==4 
replace NIVEL=1 if b1==3 | b1==5 | b1==7 /* PRIMARIO */
replace NIVEL=2 if b1==6 | b1==8 | b1==9  /* 1 CICLO SECUNDARIO */
replace NIVEL=3 if b1==10 | b1==11 /* 2 CICLO SECUNDARIO */
replace NIVEL=4 if b1>=13 & b1<=15 /* TERCIARIO / UNIVERSITARO */

/* b21
BASIC/INFANT/KINDER...01 
PRIMARY...............02 
PREPARATORY...........03 
ALL AGE (1-6).........04 
ALL AGE (7-9).........05 
PRIM/J HIGH (1-6).....06 
PRIM/J HIGH (7-9).....07 
J HIGH (7-9)..........08 
NEW SECONDARY.........09 
COMPREHENSIVE.........10 
SECONDARY HIGH........11 
TECHNICAL.............12 
VOCT/AGRIC............13 
UNIVERSITY............14 (EOV)
OTHER TERT.(PUBLIC)...15 (EOV)
OTHER TERT.(PRVT).....16 (EOV)
ADULT LIT.CLASSES.....17 (EOV)
ADULT EDUC/NIGHT......18 (EOV)
SPECIAL SCHOOL........19 (EOV)
NONE..................20 (EOV)
*/

replace b21 = "." if b21 =="NN"
destring b21, replace 

gen NIVEL2=0 if b21==1 | b21==20 | b21==3 
replace NIVEL2=1 if b21==2 | b21==4 | b21==6 /* PRIMARIO */
replace NIVEL2=2 if b21==7 | b21==8 | b21==9 /* 1 CICLO SECUNDARIO */
replace NIVEL2=3 if b21==11 | b21==12  /* 2 CICLO SECUNDARIO */
replace NIVEL2=4 if b21>=14 & b21<=16 /* TERCIARIO / UNIVERSITARO */

replace b4= "." if b4=="NN"
destring b4, replace 

gen GRADO=b4 if NIVEL>0 & NIVEL<=3 /* son los grados para los que asisten actualmente a primaria o secundaria */ 
/* y los que asisten a universitario ????*/

replace b22= "." if b22=="NN"
destring b22, replace 

gen GRADO2=b22 if NIVEL2>0 & NIVEL2<=3 /* son los grados de primaria o secundaria para los que no asisten actualmente */

*/
*************
* asiste_ci *
*************

gen byte asiste_ci=.
replace asiste_ci=1 if b1>=1 & b1<19
replace asiste_ci=0 if b1==19
label var asiste "Personas que actualmente asisten a centros de enseñanza"


/*
***********
* aedu_ci *
***********

gen byte aedu_ci=.
replace aedu_ci=0 if NIVEL==0 | NIVEL2==0
replace aedu_ci=GRADO-1 if NIVEL==1 | NIVEL==2 | NIVEL==3
replace aedu_ci=GRADO2 if NIVEL2==1 | NIVEL2==2 | NIVEL2==3

replace aedu_ci=GRADO-1 if aedu>=ageyrs & GRADO<. & GRADO2<. 
replace aedu_ci=0 if b1==19 & b21==20
label variable aedu_ci "Años de Educacion (no incluye terciaria o universitaria)"
replace aedu_ci=. if aedu_ci<0


************
* eduno_ci *
************

gen eduno_ci=.
replace eduno_ci=1 if b1==19 & b21==20
replace eduno_ci=0 if (NIVEL>=1 & NIVEL<=4) | (NIVEL2>=1 & NIVEL2<=4)
label var eduno_ci "1 = personas sin educacion (excluye preescolar)"

*************
* edupre_ci *
*************

gen edupre_ci=.
replace edupre_ci=1 if NIVEL==0 | NIVEL2==0
replace edupre_ci=0 if (NIVEL>=1 & NIVEL<=4) | (NIVEL2>=1 & NIVEL2<=4)
label var edupre_ci "Educacion preescolar"

************
* edupi_ci *
************

gen edupi_ci=.
replace edupi_ci=1 if aedu_ci>0 & aedu_ci<6
replace edupi_ci=0 if aedu_ci==0 | (aedu>=6 & aedu!=.) | NIVEL==4 | NIVEL2==4
label var edupi_ci "1 = personas que no han completado el nivel primario"

************
* edupc_ci *
************

gen edupc_ci=.
replace edupc_ci=1 if aedu_ci==6
replace edupc_ci=0 if (aedu_ci>=0 & aedu_ci<6)  | (aedu_ci>6 & aedu!=.) | NIVEL==4 | NIVEL2==4
label var edupc_ci "1 = personas que han completado el nivel primario"

************
* edusi_ci *
************

gen edusi_ci=.
replace edusi_ci=0 if (aedu_ci>=0 & aedu_ci<=6) | (aedu_ci>=11 & aedu_ci!=.) | NIVEL==4 | NIVEL2==4
replace edusi_ci=1 if ((aedu_ci>6 & aedu_ci<11) & asiste==0) | (NIVEL==2 | NIVEL==3)
label var edusi_ci "1 = personas que no han completado el nivel secundario"

************
* edusc_ci *
************

gen edusc_ci=.
replace edusc_ci=1 if (GRADO2==11 | GRADO2==12)
replace edusc_ci=0 if (aedu_ci>=0 & aedu<11) | (aedu>12 & aedu!=.) | NIVEL==4 | NIVEL2==4 | edusi_ci==1
label var edusc_ci "1 = personas que han completado el nivel secundario"

************
* eduui_ci *
************

g b24=b24_exam
destring b24, replace

gen eduui_ci=.
replace eduui_ci=1 if NIVEL==4 | (NIVEL2==4 & b24<7)
replace eduui_ci=0 if (aedu>=0 & aedu<=12) | (NIVEL2==4 & (b24==7 | b24==8))
label var eduui_ci "1 = personas que no han completado el nivel universitario o superior"

************
* eduuc_ci *
************

gen eduuc_ci=.
replace eduuc_ci=1 if NIVEL2==4 & (b24==7 | b24==8)
replace eduuc_ci=0 if (aedu>=0 & aedu<=12) | NIVEL==4 | (NIVEL2==4 & b24<7)
label var eduuc_ci "1 = personas que han completado el nivel universitario o superior"

*************
* edus1i_ci *
*************

gen edus1i_ci=.
replace edus1i_ci=0 if NIVEL==2 | NIVEL2==2 | NIVEL==3 | NIVEL2==3
replace edus1i_ci=1 if edusi==1 & (NIVEL==2 | (NIVEL2==2 & GRADO2<9))
label var edus1i_ci "1 = personas que no han completado el primer ciclo de la educacion secundaria"

*************
* edus1c_ci *
*************

gen edus1c_ci=.
replace edus1c_ci=0 if NIVEL==2 | NIVEL2==2 | NIVEL==3 | NIVEL2==3 
replace edus1c_ci=1 if edusi==1 & (NIVEL2==2 & GRADO2==9)
label var edus1c_ci "1 = personas que han completado el primer ciclo de la educacion secundaria"

*************
* edus2i_ci *
*************

gen edus2i_ci=.
replace edus2i_ci=0 if NIVEL==2 | NIVEL2==2 | NIVEL==3 | NIVEL2==3 
replace edus2i_ci=1 if edusi==1 & (NIVEL==3 | (NIVEL2==3 & GRADO2<11))
label var edus2i_ci "1 = personas que no han completado el segundo ciclo de la educacion secundaria"

*************
* edus2c_ci *
*************

gen edus2c_ci=.
replace edus2c_ci=0 if edusi==1 
replace edus2c_ci=1 if (NIVEL2==3 & (GRADO2==11 | GRADO2==12)) | edusc_ci==1
label var edus2c_ci "1 = personas que han completado el segundo ciclo de la educacion secundaria"

************
* eduac_ci *
************

gen eduac_ci=.
replace eduac_ci=0 if b1==14 | b1==15 | b21==15 | b21==16
replace eduac_ci=1 if b1==13 | b21==14
label var eduac_ci "Educacion terciaria académica versus educación terciaria no-académica "

*************
* repite_ci *
*************

gen repite_ci=.
label var repite_ci "Personas que han repetido al menos un año o grado"

****************
* repiteult_ci *
****************

gen repiteult_ci=.
label var repiteult_ci "Personas que han repetido el ultimo grado"

*************
* edupub_ci *
*************

destring b3, replace

gen edupub_ci=.
replace edupub_ci=1 if b3==1
replace edupub_ci=0 if b3==2
label var edupub_ci "1 = personas que asisten a centros de enseñanza publicos"

***************
* pqnoasis_ci *
***************

/*
ILLNESS................01 
TRUANCY................02 
WORKING OUTSIDE........03 
NEEDED AT HOME.........04 
MARKET DAY.............05 
TRANSPORT PROBLEM......06 
TRANSPORT COSTS........07 
SCHOOL CLOSED..........08 
SHOES/UNIFORM MISSING..09 
RAIN...................10 
MONEY PROBLEMS.........11 
RAN ERRAND.............12 
NOT SAFE AT SCHOOL.....13 
NOT SAFE IN COMMUNITY..14 
VIOLENCE...............15 
OTHER..................16 
*/

destring b10_r1, replace

g pqnoasis_ci=.
replace pqnoasis_ci=b10_r1
label var pqnoasis_ci "Razones para no asistir a centros de enseñanza"
label define pqnoasis_ci 1 "Enfermedad" 2 "Haraganeria" 3 "Trabaja fuera del hogar" 4 "Necesita estar en el hogar" 5 "Dia de mercado" 6 "Problema de Transporte" ///
7 "Costos del transporte" 8 "Escuela cerrada" 9 "Carece de zapatos o uniforme, sucios, o mojados" 10 "Lluvia" ///
11 "Problemas monetarios" 12 "Tiene que realizar alguna diligencia" 13 "No esta seguro en la escuela" 14 "No esta seguro en la comunidad" ///
15 "Violencia" 16 "Otro" 
label values pqnoasis_ci pqnoasis_ci
*/

**************
*pqnoasis1_ci*
**************
**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**

g       pqnoasis1_ci = .

gen luz_ch=.
replace luz_ch=1 if electric>=0
replace luz_ch=0 if electric==0


gen cuartos_ch=.
replace cuartos_ch=i3 if i3>=0 & i3<=12

gen piso_ch=. // sÃ³lo hay la pregunta de pared.

gen region_c =.

************
* pared_ch *
************

destring i2, replace

/*
WOOD...1 
STONE..2 
BRICK..3 
CONCRETE NOG..4 
BLOCK & STEEL..5 
WATTLE/ADOBE..6 
OTHER..7 
*/

gen pared_ch=.
replace pared_ch=1 if i2>=1 & i2<=5
replace pared_ch=0 if i2>=6 & i2<=7


gen techo_ch=.

/*
*** HOUSING ***

**************
* aguared_ch *
**************

/*
INDOOR........01 
OUTSIDE.......02 
PBL.STDPIPE...03 (Q241)
WELL..........04 (Q241)
RIVER.........05 (Q241)
RAIN,PID......06 (Q25)
RAIN,NPID.....07 (Q25)
TRUCKED NWC.PID...08 (Q25)
TRUCKED NWC.NPID..09 (Q25)
TRUCKED PRIVATE PID...10 (Q21)
TRUCKED PRIVATE NPID..11 (Q21)
BOTTLED WATER....12 (Q25)
OTHER.........13 (Q23)
*/

*replace i20 = "." if i20 == "NN"
destring i20, replace

gen aguared_ch=.
replace aguared_ch=1 if i20==1
replace aguared_ch=0 if i20>=2 & i20<=13

***************
* aguadist_ch *
***************

destring i26_1, replace 
destring i26_2, replace 
gen distance = .
replace distance = i26_1*1000 if i26_2 ==1
replace distance = i26_1 if i26_2 ==2

gen aguadist_ch=.
replace aguadist_ch = 1 if distance >100
replace aguadist_ch = 0 if distance <=100

***************
* aguamala_ch *
***************

gen aguamala_ch=.
replace aguamala_ch = 1 if i20 >= 4 & i20 <= 7
replace aguamala_ch = 0 if (i20 >= 1 & i20 <= 3) | (i20 >= 8 & i20 <= 13)

***************
* aguamide_ch *
***************

gen aguamide_ch=.

**********
* luz_ch *
**********

destring i27, replace
/*
ELECTRICITY..1 
KEROSENE.....2 (Q281)
OTHER........3 (Q281)
NONE.........4 (Q281)
*/

gen luz_ch=.
replace luz_ch=1 if i27==1
replace luz_ch=0 if i27>=2 & i27<=4

**************
* luzmide_ch *
**************

gen luzmide_ch=.

**************
* combust_ch *
**************

gen combust_ch=.

***********
* bano_ch *
***********

destring i5, replace

/*
LINKED....1 
NOT LINK..2 
PIT.......3 
OTHER.....4 
NONE......5 
*/

gen bano_ch=.
replace bano_ch=1 if i5==1 | i5==2 | i5==3
replace bano_ch=0 if i5==4 | i5==5

*************
* banoex_ch *
*************

gen banoex_ch=.

***********
* des1_ch *
***********

gen des1_ch=.
replace des1_ch = 0 if i5 == 4
replace des1_ch = 1 if i5 == 1
replace des1_ch = 2 if i5 == 3
replace des1_ch = 3 if i5 == 2 | i5 == 5

***********
* des2_ch *
***********

gen des2_ch=.
replace des2_ch=1 if des1_ch==1 | des1_ch==3 
replace des2_ch=2 if des1_ch==2 | des1_ch==4
replace des2_ch=0 if des1_ch==5

***********
* piso_ch *
***********

gen piso_ch=.

************
* pared_ch *
************

destring i2, replace

/*
WOOD...1 
STONE..2 
BRICK..3 
CONCRETE NOG..4 
BLOCK & STEEL..5 
WATTLE/ADOBE..6 
OTHER..7 
*/

gen pared_ch=.
replace pared_ch=1 if i2>=1 & i2<=5
replace pared_ch=0 if i2>=6 & i2<=7

************
* techo_ch *
************

gen techo_ch=.

************
* resid_ch *
************

destring i36, replace

/*      1   REGULAR PUBLIC COLLECTION
        2   IRREGULAR PUBLIC COLLECTION
        3   PRIVATE COLLECTION
        4   BURN
        5   BURY
        6   DUMP IN SEA, RIVER,GULLY
        7   DUMP IN OWN YARD
        8   DUMP I MUNICIPAL SITE
        9   OTHER DUMPING
        10  OTHER SPECIFY
        97  Missing
        98  Dont Know
        99  Not Stated*/
		
gen resid_ch=.
replace resid_ch=0 if i36==1 | i36==2 | i36==3
replace resid_ch=1 if i36==4 | i36==5
replace resid_ch=2 if i36==6 | i36==7 | i36==8 | i36==9
replace resid_ch=3 if i36==10

***********
* dorm_ch *
***********

gen dorm_ch=.

**************
* cuartos_ch *
**************

destring i3, replace

gen cuartos_ch=.
replace cuartos_ch= i3
replace cuartos_ch=. if i3==97

*************
* cocina_ch *
*************

destring i8, replace

gen cocina_ch=.
replace cocina_ch = 1 if i8 == 1 | i8 == 2
replace cocina_ch = 0 if i8 == 3

************
* telef_ch *
************

destring i30_1, replace

gen telef_ch=.
replace telef_ch=1 if i30_1==1
replace telef_ch=0 if i30_1==2

*************
* refrig_ch *
*************

gen refrig_ch=.

************
* freez_ch *
************

gen freez_ch=.

***********
* auto_ch *
***********

gen auto_ch=.

************
* compu_ch *
************

destring i33, replace
   
/*      1   YES_LAPTOP
        2   YES_DESKTOP
        3   YES_BOTH
        4   NO*/
		
gen compu_ch=.
replace compu_ch=1 if i33==1 | i33==2 | i33==3
replace compu_ch=0 if i33==4


***************
* internet_ch *
***************

 /*     I34                                          
        1   YES
        2   NO
        3   Dont Know
        97  Missing
        99  Not Stated */
		
destring i34, replace

gen internet_ch=.
replace internet_ch=1 if i34==1 | i34==2 | i34==3 | i34==4
replace internet_ch=0 if i34==5

**********
* cel_ch *
**********
destring i30_2 i30_3, replace

gen cel_ch=.
replace cel_ch=1 if i30_2==1 | i30_3==1
replace cel_ch=0 if i30_2==2 & i30_3==2

************
* vivi1_ch *
************

*replace i1 = "." if i1 == "N"
destring i1, replace

/*
SEParate HOUSE..1 
SEMI-DETached...2 
PART of a HOUSe..3 
APARTMENT BUILDING..4 
TOWN HOUSE..5 
IMPROVISED HOUSING UNIT...6 
PART OF COMMERCIAL BUILDING....7 
OTHER......8 
*/

gen vivi1_ch=.
replace vivi1_ch=1 if i1==1 | i1==2 | i1==5 
replace vivi1_ch=2 if i1==4
replace vivi1_ch=3 if i1==3 | (i1>=6 & i1<=8)

************
* vivi2_ch *
************

gen vivi2_ch=.
replace vivi2_ch=1 if vivi1_ch==1 | vivi1_ch==2
replace vivi2_ch=0 if vivi1_ch==3

***************
* viviprop_ch *
***************

*replace i9 = "." if i9 == "N"
destring i9, replace

/*
OWNED.........1 
LEASED........2 (Q09)
PRIVATE RENT..3 (Q09)
GOVT RENT.....4 (Q12)
RENT FREE.....5 (Q12)
SQUATTED......6 (Q12)
*/

gen viviprop_ch=.
replace viviprop_ch=0 if i9==3 | i9==4
replace viviprop_ch=1 if i9==1
replace viviprop_ch=2 if i9==2
replace viviprop_ch=3 if i9==5 | i9==6 | i9==7

**************
* vivitit_ch *
**************

gen vivitit_ch=.

**************
* vivialq_ch *
**************

*replace i12_2 = "." if i12_2 == "N"
destring i12_2, replace

/* i12_2
NONE...0 
WEEK...3 
MONTH..4 
YEAR...5
*/

gen vivialq_ch=.
replace vivialq_ch=i12_1*4.3 if i12_2==3
replace vivialq_ch=i12_1	   if i12_2==4
replace vivialq_ch=i12_1/12  if i12_2==5




*****************
* vivialqimp_ch *
*****************
gen vivialqimp_ch=. 
drop NIVEL GRADO NIVEL2 GRADO2  
*/

*saveold "${surveysFolder}\harmonized\JAM\SLC\data_arm\JAM2012EA_BID.dta", replace 


g region_BID_c = .

g raza_idioma_ci =.
g id_ind_ci  =.
g id_afro_ci=. 
g raza_ci =.
g condocup_ci=.
g categoinac_ci=.
g desemp_ci =.
g cesante_ci =.
g pea_ci=.
g tamemp_ci =.
g cotizando_ci=.
g instcot_ci =.
g afiliado_ci=.
g formal_ci=.
g nempleos_ci=.
g emp_ci =.
g tipocontrato_ci=.
g pensionsub_ci=.
g pension_ci=.
g tipopen_ci=.
g instpen_ci=.
g  tcylmpri_ci=.
g tcylmpri_ch=.
g ypen_ci=.
g ypensub_ci=.
g salmm_ci=.
g lp_ci=.
g lpe_ci=.
g  tecnica_ci=.
g rentaimp_ch =.
g  antiguedad_ci=.
g durades_ci =.
g desalent_ci =.
g subemp_ci =.
g tiempoparc_ci =.
g categopri_ci=.
g categosec_ci=.
g spublico_ci=.
g  horaspri_ci=.
g horastot_ci=.	
g ylmpri_ci=.
g nrylmpri_ci =.
g ynlm_ch	=.
g ynlnm_ch =.
g ylmhopri_ci =.
g ylmho_ci =.
g autocons_ci =.
g autocons_ch =.
g nrylmpri_ch =.
g ylnmpri_ci=.
g ylmsec_ci=.
g ylnmsec_ci =.
g ylmotros_ci=.
g ylnmotros_ci=.
g ylm_ci =.
g ylnm_ci =.
g ynlm_ci=.
g ynlnm_ci =.
g ylm_ch=.
g  ylnm_ch=.
g ylmnr_ch=.
g rama_ci=.
g ocupa_ci=.
g remesas_ci=.
g remesas_ch=.
g aedu_ci=.
g eduno_ci=. 
g edupi_ci=. 
g edupc_ci=.	
g edusi_ci=. 
g edusc_ci=. 
g eduui_ci=. 
g eduuc_ci=.	
g edus1i_ci=.
g edus1c_ci=. 
g edus2i_ci=. 
g edus2c_ci=. 
g edupre_ci=. 
g eduac_ci=. 
g pqnoasis_ci=.	
g repite_ci=. 
g repiteult_ci=. 
g edupub_ci =.
g aguared_ch =.
g aguadist_ch=. 
g aguamala_ch =.
g aguamide_ch =.
g luzmide_ch =.
g combust_ch=.	
g bano_ch =.
g banoex_ch =.
g des1_ch =.
g des2_ch =.
g resid_ch =. 

**Daniela Zuluaga- Enero 2018: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**
	
*********************
***aguamejorada_ch***
*********************
g       aguamejorada_ch = 1 if  i22 ==1 | (i20 >=3 & i20 <=7)
replace aguamejorada_ch = 0 if  i22 ==2 | (i20 >=8 & i20 <=13)
		
*********************
***banomejorado_ch***
*********************
g       banomejorado_ch = 1 if  i5 ==1
replace banomejorado_ch = 0 if (i5>=2 & i5 <=5) | i4 ==3
		
g dorm_ch =. 
g cocina_ch =. 
g telef_ch  =.
g refrig_ch =. 
g freez_ch  =.
g auto_ch  =.
g compu_ch  =.
g internet_ch =. 
g cel_ch  =.
g vivi1_ch =. 
g vivi2_ch =.
g viviprop_ch =.
g vivitit_ch  =.
g vivialq_ch  =.
g vivialqimp_ch=.


destring sexo_ci, replace
  /*_____________________________________________________________________________________________________*/
* Asignación de etiquetas e inserción de variables externas: tipo de cambio, Indice de Precios al 
* Consumidor (2011=100), líneas de pobreza
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



compress


saveold "`base_out'", replace


log close
