clear
set memory 200m
set more off
local in "D:\DATA.IDB\" 
local out="X:\ARM\JAM\2007\"
log using "`out'Documents\JAM07.log", replace
use "jam07B"

/*January 2009
//////////////////////////////////////////////////////////////////////////
         											   
        			JAMAICA 2007
                             												   
/////////////////////////////////////////////////////////////////////////

do file prepared by Melisa Morales for Suzanne Duryea
Melisa Morales sugiere chequearlo
SLC variables by Ted Enamorado*/ 


**********
* pais_c *
**********

gen str pais_c="JAM"

**********
* anio_c *
**********

gen anio_c=2007

*********
* mes_c *
*********

gen mes_c=5 
label var mes_c "Mes de la Encuesta: Mayo de 2002"
label define mes_c 5 "MAY"
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
HEAD.....................1 
SPOUSE / PARTNER.........2 
CHILD OF HEAD / SPOUSE...3 
SPOUSE OF CHILD..........4 
GRANDCHILD...............5 
PARENT OF HEAD / SPOUSE..6 
OTHER RELATIVE...........7 
HELPER / DOMESTIC........8 
OTHER NON-RELATIVE.......9 
*/

destring relatn, replace
gen relacion_ci=.
replace relacion_ci=1 if relatn==1
replace relacion_ci=2 if relatn==2
replace relacion_ci=3 if relatn==3
replace relacion_ci=4 if relatn>=4 & relatn<=7 /* Otros familiares */
replace relacion_ci=5 if relatn==9
replace relacion_ci=6 if relatn==8 /*Es el sevicio domestico, Incluye a familiares del Serv. Domestico*/

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

** POBLACION DA JAMAICA AL 2007 = 2,714,000 **
* Se reajustan los ponderadores:
sum finwght
scalar pob=r(sum)
gen pop=finwght*(2714000 /pob)

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
destring age, replace

gen edad_ci=age
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
g jefe_ci=(relatn==1)
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
*	LABOR MARKET
******************************************************************************

******************************
*	emp_ci
******************************
g emp_ci=(q21==1)
replace emp_ci=. if q21==.
replace emp_ci=1 if q21a==1|q21b==1|q22==1|q23==1
label var emp_ci "Empleado"
label define emp_ci 1"Si" 0 "No"
label value emp_ci emp_ci
******************************
*	desemp1_ci	& desemp2_ci & desemp3_ci 
******************************
g desemp1_ci=(emp_ci==0 & q21a==3)
replace desemp1_ci=. if emp_ci==.
label var desemp1_ci "Personas sin trabajo que buscaron en el periodo de referencia"

g desemp2_ci=(desemp1_ci==1|(emp_ci==0 & q21a==4 & q25==2))
replace desemp2_ci=. if emp_ci==.
label var desemp2_ci "des1 + no trabajaron ni buscaron en la ult semana pero esperan respuesta de solicit"
 
*g desemp3_ci=
*label var desemp3_ci "desemp2 + personas que no tienen trabajo pero han buscado trabajo durante las 4 semanas anteriores a la semana pasada"
*NA. Estrictamente, no se puede generar
******************************
*	pea1_ci, pea2_ci, pea3_ci
******************************
g pea1_ci=(emp_ci==1 | desemp1_ci==1)
g pea2_ci=(emp_ci==1 | desemp2_ci==1)
*g pea3_ci=.
*NA
******************************
*	desalent_ci
******************************
g desalent_ci=(emp_ci==0 & q44==4)
replace desalent_ci=. if emp_ci!=0
label var desalent_ci "Personas que creen que por alguna razon no conseguiran trabajo"
******************************
*	subemp_ci
******************************
*g subemp_ci=.
*NA
******************************
*	horaspri_ci & horastot_ci
******************************
g byte horaspri_ci=q35
label var horaspri_ci "Hs totales (semanales) trabajadas en act. principal"

g byte horastot_ci=horaspri_ci
label var horastot_ci "Hs totales (semanales)trabajadas en toda actividad"
******************************
*	tiempoparc_ci
******************************
g tiempoparc_ci=(emp_ci==1 & q34==3 & horastot_ci<30)
replace tiempoparc_ci=. if emp_ci!=1
label var tiempoparc_ci "Trabajan menos de 30 hs semanales y no quieren trabajar mas"
******************************
*	categopri_ci
******************************
g categopri_ci=.
replace categopri_ci=1 if q323==5
replace categopri_ci=2 if q323==6
replace categopri_ci=3 if q323>=1 & q323<=3
replace categopri_ci=4 if q323==4
label define categopri_ci 1"Patron" 2"Cuenta propia" 3"Empleado" 4" Familiar no remunerado"
label value categopri_ci categopri_ci
label variable categopri_ci "Categoria ocupacional trabajo principal"
******************************
*	categosec_ci
******************************
*g categosec_ci=.
*NA
******************************
*	contrato_ci
******************************
*g contrato_ci=.
*NA
******************************
*	segsoc_ci
******************************
*g segsoc_ci=.
*NA
******************************
*	nempleos_ci
******************************
g nempleos_ci=q37
notes: nempleos_ci answers how many income earnings activities people carried out during week ending.
******************************
*	firmapeq_ci
******************************
*g firmapeq_ci=.
*NA
******************************
*	spublico_ci
******************************
g spublico_ci=(q323==1 |q323==2)
replace spublico_ci=. if q323==9|q323==.
label var spublico_ci "Trabaja en sector publico"



******************************************************************************
*		LABOR DEMAND
******************************************************************************

******************************
*	ocupa_ci
******************************
g ocupa_ci=.
replace ocupa_ci=1 if q38m>=111 & q38m<=3999
replace ocupa_ci=2 if q38m>=1000 & q38m<=1999
replace ocupa_ci=3 if q38m>=4000 & q38m<=4999
replace ocupa_ci=4 if q38m>=7000 & q38m<=7999
replace ocupa_ci=5 if q38m>=5000 & q38m<=5999
replace ocupa_ci=6 if q38m>=6000 & q38m<=6999
replace ocupa_ci=7 if q38m>=8000 & q38m<=8999
******************************
*	rama_ci
******************************
g  rama_ci=.
replace rama_ci=1 if q39m>100 & q39m<999
replace rama_ci=2 if q39m>=1000 & q39m<=1999
replace rama_ci=3 if q39m>=2000 & q39m<4000
replace rama_ci=4 if q39m>=4000 & q39m<5000
replace rama_ci=5 if q39m>=5000 & q39m<6000
replace rama_ci=6 if q39m>=6000 & q39m<7000
replace rama_ci=7 if q39m>=7000 & q39m<8000
replace rama_ci=8 if q39m>=8000 & q39m<9000
replace rama_ci=9 if q39m>=9000 & q39m<9998
label var rama_ci "Rama Laboral en la Ocupacion Principal"
label define rama_ci 1 "Agricultura, caza, silvicultura y pesca" 2 "Explotación de minas y canteras" 3 "Industrias manufactureras" 4 "Electricidad, gas y agua" 5 "Construcción" 6 "Comercio al por mayor y menor, restaurantes, hoteles" 7 "Transporte y almacenamiento" 8 "Establecimientos financieros, seguros, bienes inmuebles" 9 "Servicios sociales, comunales y personales"
label values rama_ci rama_ci
******************************
*	ylmpri_ci 
******************************
g ylmpri_ci=q325a/12
label var ylmpri_ci "Ingreso laboral monetario actividad principal"
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
g ylm_ci=ylmpri_ci
label var ylm_ci "Ingreso laboral monetario total"
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
by idh_ch, sort: egen ylm_ch=sum(ylm_ci) if miembros_ci==1
label var ylm_ch "Ingreso laboral monetario del hogar-ignora no respuesta"
******************************
*	ylmnr_ch  
******************************
by idh_ch, sort: egen ylmnr_ch=sum(ylm_ci) if miembros_ci==1
label var ylmnr_ch "Ingreso laboral monetario del hogar - considera la no respuesta"

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
******************************
g ylmhopri_ci=ylmpri_ci/(horaspri_ci*4.3) 
label var ylmhopri_ci "Salario monetario de la actividad principal"
******************************
*	ylmho_ci 
******************************
g ylmho_ci=ylm_ci/(horastot_ci*4.3) 
label var ylmho_ci "Salario monetario de todas las actividades"
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
recode q41 (99=.) (2=1) (3=2) (4=3) (5=4) (6=5) (7=6), g(durades_ci)
label define durades_ci 1 "Menos de 1 mes" 2 "1-3 meses" 3 "3-6 meses" 4"6-9 meses" 5 "9-12 meses" 6 "12-24 meses" 7 "24 meses y +" 
label values durades_ci durades_ci
*****************
* antiguedad_ci *
*****************
*Ted generated antiguedad_ci from SLC survey that I'll keep it. 
recode q311(9=.) (2=1) (3=2) (4=3) (5=4), g(antiguedad_ci2)
label define antiguedad_ci2 1 "Menos de medio anio" 2"0.5-0.75 anios" 3 "0.75-1 anio" 4 "1-2 anios" 5 "2-5 anios" 
label values antiguedad_ci2 antiguedad_ci2




******************************************************************************
*	EDUCATION
******************************************************************************

/* b01
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

*replace b01 = "." if b01 =="NN"
destring b01, replace 

gen NIVEL=0 if b01==1 | b01==2 | b01==4 
replace NIVEL=1 if b01==3 | b01==5 | b01==7 /* PRIMARIO */
replace NIVEL=2 if b01==6 | b01==8 | b01==9  /* 1 CICLO SECUNDARIO */
replace NIVEL=3 if b01==10 | b01==11 /* 2 CICLO SECUNDARIO */
replace NIVEL=4 if b01>=13 & b01<=15 /* TERCIARIO / UNIVERSITARO */

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

destring b21, replace 

gen NIVEL2=0 if b21==1 | b21==20 | b21==3 
replace NIVEL2=1 if b21==2 | b21==4 | b21==6 /* PRIMARIO */
replace NIVEL2=2 if b21==7 | b21==8 | b21==9 /* 1 CICLO SECUNDARIO */
replace NIVEL2=3 if b21==11 | b21==12  /* 2 CICLO SECUNDARIO */
replace NIVEL2=4 if b21>=14 & b21<=16 /* TERCIARIO / UNIVERSITARO */

destring b04, replace 

gen GRADO=b04 if NIVEL>0 & NIVEL<=3 /* son los grados para los que asisten actualmente a primaria o secundaria */ 
/* y los que asisten a universitario ????*/

destring b22, replace 
gen GRADO2=b22 if NIVEL2>0 & NIVEL2<=3 /* son los grados de primaria o secundaria para los que no asisten actualmente */

*************
* asiste_ci *
*************

gen byte asiste_ci=.
replace asiste_ci=1 if B01>=1 & B01<19
replace asiste_ci=0 if B01==19
label var asiste "Personas que actualmente asisten a centros de enseñanza"

***********
* aedu_ci *
***********

gen byte aedu_ci=.
replace aedu_ci=0 if NIVEL==0 | NIVEL2==0
replace aedu_ci=GRADO-1 if NIVEL==1 | NIVEL==2 | NIVEL==3
replace aedu_ci=GRADO2 if NIVEL2==1 | NIVEL2==2 | NIVEL2==3

replace aedu_ci=GRADO-1 if aedu>=age & GRADO<. & GRADO2<. 
replace aedu_ci=0 if b01==19 & b21==20
label variable aedu_ci "Años de Educacion (no incluye terciaria o universitaria)"
replace aedu_ci=. if aedu_ci<0
************
* eduno_ci *
************

gen eduno_ci=.
replace eduno_ci=1 if b01==19 & b21==20
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
replace eduac_ci=0 if b01==14 | b01==15 | b21==15 | b21==16
replace eduac_ci=1 if b01==13 | b21==14
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

destring b03, replace

gen edupub_ci=.
replace edupub_ci=1 if b03==1
replace edupub_ci=0 if b03==2
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

destring b10r1, replace

g pqnoasis_ci=.
replace pqnoasis_ci=b10r1
label var pqnoasis_ci "Razones para no asistir a centros de enseñanza"
label define pqnoasis_ci 1 "Enfermedad" 2 "Haraganeria" 3 "Trabaja fuera del hogar" 4 "Necesita estar en el hogar" 5 "Dia de mercado" 6 "Problema de Transporte" ///
7 "Costos del transporte" 8 "Escuela cerrada" 9 "Carece de zapatos o uniforme, sucios, o mojados" 10 "Lluvia" ///
11 "Problemas monetarios" 12 "Tiene que realizar alguna diligencia" 13 "No esta seguro en la escuela" 14 "No esta seguro en la comunidad" ///
15 "Violencia" 16 "Otro" 
label values pqnoasis_ci pqnoasis_ci


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

*replace i18 = "." if i18 == "NN"
destring i18, replace

gen aguared_ch=.
replace aguared_ch=1 if i18==1
replace aguared_ch=0 if i18>=2 & i18<=13

***************
* aguadist_ch *
***************

gen distance = .
replace distance = i24*1000 if i24p ==1
replace distance = i24 	if i24p ==2

gen aguadist_ch=.
replace aguadist_ch = 1 if distance >100
replace aguadist_ch = 0 if distance <=100

***************
* aguamala_ch *
***************

gen aguamala_ch=.
replace aguamala_ch = 1 if i18 >= 4 & i18 <= 7
replace aguamala_ch = 0 if (i18 >= 1 & i18 <= 3) | (i18 >= 8 & i18 <= 13)

***************
* aguamide_ch *
***************

destring i20, replace

gen aguamide_ch=.
replace aguamide_ch = 1 if i20 == 1 | i20== 2
replace aguamide_ch = 0 if i20 == 3

**********
* luz_ch *
**********

*replace i25 = "." if i25 == "N"
destring i25, replace
/*
ELECTRICITY..1 
KEROSENE.....2 (Q281)
OTHER........3 (Q281)
NONE.........4 (Q281)
*/

gen luz_ch=.
replace luz_ch=1 if i25==1
replace luz_ch=0 if i25>=2 & i25<=4

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

*replace i04 = "." if i04 == "N"
destring i04, replace

/*
LINKED....1 
NOT LINK..2 
PIT.......3 
OTHER.....4 
NONE......5 
*/

gen bano_ch=.
replace bano_ch=1 if i04==1 | i04==2 | i04==3
replace bano_ch=0 if i04==4 | i04==5

*************
* banoex_ch *
*************

gen banoex_ch=.

***********
* des1_ch *
***********

gen des1_ch=.
replace des1_ch = 0 if i04 == 4
replace des1_ch = 1 if i04 == 1
replace des1_ch = 2 if i04 == 3
replace des1_ch = 3 if i04 == 2 | i04 == 5

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

*replace i02 = "." if i02 == "N"
destring i02, replace

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
replace pared_ch=1 if i02>=1 & i02<=5
replace pared_ch=0 if i02>=6 & i02<=7

************
* techo_ch *
************

gen techo_ch=.

************
* resid_ch *
************

*replace i31 = "." if i31 == "N"
destring i31, replace

/*
GARBAGE TRUCK..1 
PLACE IN SKIP..2 
BURN...........3 
BURY...........4 
IN EMPTY LOT...5 
IN GULLY.......6 
OTHER..........7 
*/

gen resid_ch=.
replace resid_ch=0 if i31==1 | i31==2
replace resid_ch=1 if i31==3 | i31==4
replace resid_ch=2 if i31==5 | i31==6
replace resid_ch=3 if i31==7

***********
* dorm_ch *
***********

gen dorm_ch=.

**************
* cuartos_ch *
**************

*replace i03 = "." if i03 == "NN"
destring i03, replace

gen cuartos_ch=.
replace cuartos_ch= i03

*************
* cocina_ch *
*************

*replace i06 = "." if i06 == "N"
destring i06, replace

gen cocina_ch=.
replace cocina_ch = 1 if i06 == 1 | i06 == 2
replace cocina_ch = 0 if i06 == 3

************
* telef_ch *
************

*replace i281 = "." if i281 == "N"
destring i281, replace

gen telef_ch=.
replace telef_ch=1 if i281==1
replace telef_ch=0 if i281==2

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

*replace i32 = "." if i32 == "N"
destring i32, replace

gen compu_ch=.
replace compu_ch=1 if i32==1
replace compu_ch=0 if i32==2

***************
* internet_ch *
***************

*replace i33 = "." if i33 == "N"
destring i33, replace

gen internet_ch=.
replace internet_ch=1 if i33==1 
replace internet_ch=0 if i33==2

**********
* cel_ch *
**********

*replace i282 = "." if i282 == "N"
destring i282, replace

gen cel_ch=.
replace cel_ch=1 if i282==1
replace cel_ch=0 if i282==2

************
* vivi1_ch *
************

*replace i01 = "." if i01 == "N"
destring i01, replace

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
replace vivi1_ch=1 if i01==1 | i01==2 | i01==5 
replace vivi1_ch=2 if i01==4
replace vivi1_ch=3 if i01==3 | (i01>=6 & i01<=8)

************
* vivi2_ch *
************

gen vivi2_ch=.
replace vivi2_ch=1 if vivi1_ch==1 | vivi1_ch==2
replace vivi2_ch=0 if vivi1_ch==3

***************
* viviprop_ch *
***************

*replace i07 = "." if i07 == "N"
destring i07, replace

/*
OWNED.........1 
LEASED........2 (Q09)
PRIVATE RENT..3 (Q09)
GOVT RENT.....4 (Q12)
RENT FREE.....5 (Q12)
SQUATTED......6 (Q12)
*/

gen viviprop_ch=.
replace viviprop_ch=0 if i07==3 | i07==4
replace viviprop_ch=1 if i07==1
replace viviprop_ch=2 if i07==2
replace viviprop_ch=3 if i07==5 | i07==6 | i07==7

**************
* vivitit_ch *
**************

gen vivitit_ch=.

**************
* vivialq_ch *
**************

*replace i10p = "." if i10p == "N"
destring i10p, replace

/* i10p
NONE...0 
WEEK...3 
MONTH..4 
YEAR...5
*/

gen vivialq_ch=.
replace vivialq_ch=i10*4.3 if i10p==3
replace vivialq_ch=i10	   if i10p==4
replace vivialq_ch=i10/12  if i10p==5
*****************
* vivialqimp_ch *
*****************
gen vivialqimp_ch=. 
drop NIVEL GRADO NIVEL2 GRADO2  
save "`out'Arm_data\JAM2007EA_BID_complete.dta", replace
log close
