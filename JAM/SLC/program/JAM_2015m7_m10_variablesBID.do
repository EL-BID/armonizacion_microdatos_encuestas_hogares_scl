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
local ANO "2015"
local ronda m7_m10 

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
Autores: Daniela Zuluaga E-mail: danielazu@iadb.org - da.zuluaga@hotmail.com
Versiones anteriores: Mayra Sáenz E-mail: saenzmayra.a@gmail.com - mayras@iadb.org
Fecha última modificación: Enero de 2018

							SCL/LMK - IADB
****************************************************************************/
****************************************************************************/



use `base_in', clear



//////////////////////////////////////////////////////////////////////////
         											   
 *       			JAMAICA 2015
                             												   
///////////////////////////////////////////////////////////////////////// */ 


****************
* region_BID_c *
****************

gen region_BID_c=2

************
* region_c *
************

gen region_c=.

**********
* pais_c *
**********

gen str pais_c="JAM"

**********
* anio_c *
**********

gen anio_c=2015

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
*Modificacion Daniela Zuluaga- Enero 2018 Population 2015= 2872000

sum finwght
scalar pob=r(sum)
gen pop=finwght*(2872000/pob)

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

***************
* condocup_ci *
***************

gen condocup_ci=.
replace condocup=1 if (m1==1 | m2==1 | m2==2) 
replace condocup=2 if (m1==2 & m2==3)
replace condocup=3 if (m1==2 & (m2>=4 & m2<=7))
replace condocup=4 if edad_ci<14

*****************
* categoinac_ci *
*****************

gen categoinac_ci=.

***************
* nempleos_ci *
***************

gen nempleos_ci=.

**********
* emp_ci *
**********

gen emp_ci=(condocup_ci==1)
replace emp_ci=. if condocup_ci==.

****************
* antiguedad_ci*
****************

gen antiguedad_ci=.

*************
* desemp_ci *
*************

gen desemp_ci=(condocup_ci==2)
replace desemp_ci=. if condocup_ci==.

***************
* cesante_ci  *
***************

gen cesante_ci=.

***************
* durades_ci  *
***************

gen durades_ci=.

***********
* pea_ci  *
***********

gen pea_ci=(emp_ci==1 | desemp_ci==1)

****************
* desalent_ci  *
****************

gen desalent_ci=.

***************
* subemp_ci   *
***************

gen subemp_ci=.

*****************
* tiempoparc_ci *
*****************

gen tiempoparc_ci=.

****************
* categopri_ci *
****************

gen categopri_ci=.

****************
* categosec_ci *
****************

gen categosec_ci=.

************
* rama_ci  *
************

gen rama_ci=.

****************
* spublico_ci  *
****************

gen spublico_ci=(m10==1 | m10==2)
replace spublico=. if (m10==9)

***************
* tamemp_ci  *
***************

gen tamemp_ci=.

****************
* cotizando_ci *
****************

gen cotizando_ci=.

****************
* instcot_ci  *
****************

gen instcot_ci=.

****************
* afiliado_ci  *
****************

gen afiliado_ci	=.

****************
* formal_ci  *
****************

gen formal_ci=.

******************
* tipocontrato_ci*
******************

gen tipocontrato_ci=.

************
* ocupa_ci *
************

gen ocupa_ci=.

****************
* horaspri_ci  *
****************

gen horaspri_ci =.

****************
* horastot_ci  *
****************

gen horastot_ci = m7
replace horastot_ci= . if (m7==99 | m7==98 | m7==97)

*****************
* pensionsub_ci *
*****************

gen pensionsub_ci =.

****************
* pension_ci   *
****************

gen pension_ci=.

****************
* ypen_ci   *
****************

gen ypen_ci=.

****************
* ypensub_ci   *
****************

gen ypensub_ci=.

****************
* instpen_ci   *
****************

gen instpen_ci =.

****************
* tipopen_ci   *
****************

gen tipopen_ci =.

******************************************************************************
*		INCOME
******************************************************************************

*********
*lp_ci***
*********
gen lp_ci=. 
*********
*lpe_ci***
*********
gen lpe_ci=. 

*********
*salmm_ci***
*********
gen salmm_ci=6200*4.3
label var salmm_ci "Salario Minimo en dolares de Jamaica"

**************
*	ylmpri_ci 
**************

gen ylmpri_ci=.

**************
*	ylnmpri_ci 
**************

gen ylnmpri_ci=.

***************
*	ylmsec_ci
***************

gen ylmsec_ci=.	

***************
*	ylnmsec_ci
***************

gen ylnmsec_ci=.	

***************
* ylmotros_ci
***************

gen ylmotros_ci=.

***************
* ylnmotros_ci 
***************

gen ylnmotros_ci =.

************
*	ylm_ci 
***********

gen ylm_ci =.

************
*	ylnm_ci 
************

gen ylnm_ci=.

*************
*	ynlm_ci
*************

gen ynlm_ci=.

*************
*	ynlnm_ci
*************

gen ynlnm_ci=.

************
*	ylm_ch 
***********

gen ylm_ch =.

************
*	ylnm_ch 
************

gen ylnm_ch=.

************
*	ylnmr_ch 
************

gen ylnmr_ch=.

*************
*	ynlm_ch
*************

gen ynlm_ch=.

*************
*	ynlnm_ch
*************

gen ynlnm_ch=.

****************
*	ylmhopri_ci 
****************

gen ylmhopri_ci=.

*************
*	ylmho_ci 
*************

gen ylmho_ci=.

*************
*nrylmpri_ci
*************

gen nrylmpri_ci=.

*************
*nrylmpri_ch
*************

gen nrylmpri_ch=.

****************
*** ylmnr_ch ***
****************

gen ylmnr_ch =.

*************
*autocons_ci  
*************

gen autocons_ci =.

*************
*autocons_ch  
*************

gen autocons_ch =.

*************
* remesas_ci 
*************

gen remesas_ci=.

*************
* remesas_ch 
*************

gen remesas_ch=.

****************
*	rentaimp_ch  
****************

gen rentaimp_ch =i11 


******************************************************************************
*	EDUCATION
******************************************************************************

/*Se modificó la creación de las variables de educación utilizando como insumo principal
la variable b4 y no b1 para crear aedu_ci*/

replace b1 =. if (b1==96 | b1==97 | b1==98)
replace b22= . if (b22==97 | b22==98 | b22==99)
replace b4=. if b4==97

*************
* asiste_ci *
*************

gen asiste_ci= (b1>=1 & b1<19)
replace asiste_ci=0 if b1==.
label var asiste "Personas que actualmente asisten a centros de enseñanza"


***********
* aedu_ci *
***********

gen aedu_ci=.
replace aedu_ci=0 if b4==0
replace aedu_ci=b4-1 if aedu_ci!=0
replace aedu_ci=b21 if b4==.
label variable aedu_ci "Años de Educacion (no incluye terciaria o universitaria)"


************
* eduno_ci *
************

gen eduno_ci = (aedu_ci == 0)
replace eduno_ci=. if aedu_ci==.
label var eduno_ci "1 = personas sin educacion (excluye preescolar)"

*************
* edupre_ci *
*************

gen edupre_ci= (aedu_ci==1 & b20==1)
replace edupre_ci=. if aedu_ci==. & b20==.
label var edupre_ci "Educacion preescolar"

************
* edupi_ci *
************

gen edupi_ci=.
replace edupi_ci=1 if aedu_ci>0 & aedu_ci<6
replace edupi_ci=0 if aedu_ci==0 | (aedu_ci>=6 & aedu_ci!=.)
label var edupi_ci "1 = personas que no han completado el nivel primario"

************
* edupc_ci *
************

gen edupc_ci=.
replace edupc_ci=1 if aedu_ci==6
replace edupc_ci=0 if (aedu_ci>=0 & aedu_ci<6)  | (aedu_ci>6 & aedu!=.)
label var edupc_ci "1 = personas que han completado el nivel primario"


************
* edusi_ci *
************

gen edusi_ci= (aedu_ci>6 & aedu_ci<11) 
replace edusi_ci=. if aedu_ci==.
label var edusi_ci "1 = personas que no han completado el nivel secundario"

************
* edusc_ci *
************

gen edusc_ci= (aedu_ci>=11 & aedu_ci<=13) 
label var edusc_ci "1 = personas que han completado el nivel secundario"

************
* eduui_ci *
************

/*Se hace una aproximación, y se genera la variable para las personas que afirman que este año están atendiendo a la universidad
o educación terciaria*/
 
gen eduui_ci=(b1==12 | b1==13 | b1==14) 
replace eduui_ci=. if b1==.
label var eduui_ci "1 = personas que no han completado el nivel universitario o superior"

************
* eduuc_ci *
************

**Se hace una aproximación con los diplomas/ certificaciones obtenidas**

gen eduuc_ci= (b24_exam==9 | b24_exam==10 | b24_exam==11) 
replace eduuc_ci=. if b24_exam==.
label var eduuc_ci "1 = personas que han completado el nivel universitario o superior"


***************
***edus1i_ci***
***************
gen edus1i_ci = (aedu_ci >= 6 & aedu_ci < 9)
replace edus1i_ci =. if aedu_ci==.
la var edus1i_ci "1er ciclo de la secundaria incompleto"

***************
***edus1c_ci***
***************
gen edus1c_ci = aedu_ci == 9
replace edus1c_ci =. if aedu_ci==.
la var edus1c_ci "1er ciclo de la secundaria completo"

***************
***edus2i_ci***
***************
gen  edus2i_ci = aedu_ci == 10 
replace edus2i_ci =. if aedu_ci==.
la var edus2i_ci "2do ciclo de la secundaria incompleto"

***************
***edus2c_ci***
***************
gen edus2c_ci = (aedu_ci >= 11 & aedu_ci <= 13) 
replace edus2c_ci =. if aedu_ci==.
la var edus2c_ci "2do ciclo de la secundaria completo"


************
* eduac_ci *
************

**Se hace una aproximación con los diplomas/ certificaciones obtenidas**

gen eduac_ci=.
replace eduac_ci=0 if (b1==13 | b1==14 | b20==13 | b20==14)
replace eduac_ci=1 if b1==12 | b20==12
label var eduac_ci "Educacion terciaria académica versus educación terciaria no-académica "

**************
* tecnica_ci *
**************

gen tecnica_ci=(b1==10 | b20==10)
replace tecnica_ci=. if (b1==. & b20==.)

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

g pqnoasis_ci=.
replace pqnoasis_ci=b10_r1
label var pqnoasis_ci "Razones para no asistir a centros de enseñanza"
label define pqnoasis_ci 1 "Enfermedad" 2 "Haraganeria" 3 "Trabaja fuera del hogar" 4 "Necesita estar en el hogar" 5 "Dia de mercado" 6 "Problema de Transporte" ///
7 "Costos del transporte" 8 "Escuela cerrada" 9 "Carece de zapatos o uniforme, sucios, o mojados" 10 "Lluvia" ///
11 "Problemas monetarios" 12 "Tiene que realizar alguna diligencia" 13 "No esta seguro en la escuela" 14 "No esta seguro en la comunidad" ///
15 "Violencia" 16 "Otro" 
label values pqnoasis_ci pqnoasis_ci


**************
*pqnoasis1_ci*
**************
**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**

g       pqnoasis1_ci = 1 if b10_r1==7 | b10_r1==9 | b10_r1==11
replace pqnoasis1_ci = 2 if b10_r1==3
replace pqnoasis1_ci = 3 if b10_r1==1
replace pqnoasis1_ci = 4 if b10_r1==2
replace pqnoasis1_ci = 5 if b10_r1==5
replace pqnoasis1_ci = 8 if b10_r1==6 | b10_r1==8
replace pqnoasis1_ci = 9 if b10_r1==10 | (b10_r1>=12 & b10_r1<=16)


label define pqnoasis1_ci 1 "Problemas económicos" 2 "Por trabajo" 3 "Problemas familiares o de salud" 4 "Falta de interés" 5	"Quehaceres domésticos/embarazo/cuidado de niños/as" 6 "Terminó sus estudios" 7	"Edad" 8 "Problemas de acceso"  9 "Otros"
label value  pqnoasis1_ci pqnoasis1_ci


******************************************************************************
*	HOUSING
******************************************************************************

**************
* aguared_ch *
**************

/*
INDOOR........01 
OUTSIDE.......02 
PBL.STDPIPE...03 
WELL..........04 
RIVER.........05 
RAIN,PID......06 
RAIN,NPID.....07 
TRUCKED NWC.PID...08 
TRUCKED NWC.NPID..09 
TRUCKED PRIVATE PID...10 
TRUCKED PRIVATE NPID..11 
BOTTLED WATER....12 
OTHER.........13 
*/

gen aguared_ch=.
replace aguared_ch=1 if i27==1
replace aguared_ch=0 if i27>=2 & i27<=13

***************
* aguadist_ch *
***************

gen distance = .
replace distance = i35_1*1000 if i35_2 ==1
replace distance = i35_1 if i35_2 ==2

gen aguadist_ch=.
replace aguadist_ch = 1 if distance >100
replace aguadist_ch = 0 if distance <=100

***************
* aguamala_ch *
***************

gen aguamala_ch=.
replace aguamala_ch = 1 if i27 >= 4 & i27 <= 7
replace aguamala_ch = 0 if (i27 >= 1 & i27 <= 3) | (i27 >= 8 & i27 <= 13)

***************
* aguamide_ch *
***************

gen aguamide_ch=.

**********
* luz_ch *
**********

/*
ELECTRICITY..1 
KEROSENE.....2 
OTHER........3 
NONE.........4 
*/

gen luz_ch=(i36==1)
replace luz_ch=. if i36==.

**************
* luzmide_ch *
**************

gen luzmide_ch=.

**************
* combust_ch *
**************

gen combust_ch=(i49==1 | i49==2)
replace combust_ch=. if i49==.

***********
* bano_ch *
***********

gen bano_ch = (i4==1 | i4==2)
replace bano_ch=. if i4==.

*************
* banoex_ch *
*************

gen banoex_ch = (i6==1)
replace banoex_ch=. if banoex_ch==.

***********
* des1_ch *
***********

gen des1_ch=.
replace des1_ch = 0 if i5 == 5
replace des1_ch = 1 if i5 == 1
replace des1_ch = 2 if (i5 == 3 | i5==2)	
replace des1_ch = 3 if i5 == 4

**Nota: Se asume que la opción 4 (otros) correspondería a desemboca en calle o río.**

***********
* des2_ch *
***********

gen des2_ch =.
replace des2_ch=0 if i5==5
replace des2_ch=1 if (i5==1| i5==3) 
replace des2_ch=2 if (i5==4 | i5==2)


***********
* piso_ch *
***********

gen piso_ch=.

************
* pared_ch *
************

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
replace pared_ch=0 if i2>=6 & i2<7
replace pared_ch=2 if i2==7

************
* techo_ch *
************

gen techo_ch=.

************
* resid_ch *
************


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
	

g resid_ch =.
replace resid_ch=0 if (i47==1 | i47==2 | i47==3)
replace resid_ch=1 if (i47==4 | i47==5)
replace resid_ch=2 if (i47==6 | i47==7 | i47==8 | i47==9)

**Daniela Zuluaga- Enero 2018: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**
	
*********************
***aguamejorada_ch***
*********************
g       aguamejorada_ch = 1 if  i27 ==1 | (i23 >=3 & i23 <=7)
replace aguamejorada_ch = 0 if  i27 ==2 | (i23 >=8 & i23 <=13)
		
*********************
***banomejorado_ch***
*********************
g       banomejorado_ch = 1 if  i5 ==1
replace banomejorado_ch = 0 if (i5>=2 & i5 <=5) | i4 ==3

***********
* dorm_ch *
***********

gen dorm_ch= i3
replace dorm_ch =. if dorm_ch==97

**************
* cuartos_ch *
**************

gen cuartos_ch= .

*************
* cocina_ch *
*************

gen cocina_ch = (i7==1 | i7==2)
replace cocina_ch=. if i7==.

************
* telef_ch *
************

gen telef_ch  =(i40_1==1)
replace telef_ch=. if i40_1==.

*************
* refrig_ch *
*************

gen refrig_ch=.

************
* freez_ch *
************

gen freez_ch=(j604==1)
replace freez_ch=. if j604==.

***********
* auto_ch *
***********

gen auto_ch=.

************
* compu_ch *
************

g compu_ch  = (i42_1==1 | i42_2==1 | i42_3==1 | i42_4==1)
replace compu_ch=. if (i42_1==. & i42_2==. & i42_3==. & i42_4==.)


***************
* internet_ch *
***************

gen internet_ch =(i43==1)
replace internet_ch=. if i43==.

**********
* cel_ch *
**********

gen cel_ch  =(i40_2==1 | i40_3==1)

************
* vivi1_ch *
************

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

gen vivi1_ch =. 
replace vivi1_ch=1 if (i1==1 | i1==2 | i1==5)
replace vivi1_ch=2 if i1==4
replace vivi1_ch=3 if (i1==3 | i1==6 | i1==7 | i1==8)

************
* vivi2_ch *
************

gen vivi2_ch =(vivi1_ch==1 | vivi1_ch==2)
replace vivi2_ch=. if vivi1_ch==.

***************
* viviprop_ch *
***************

/*
OWNED.........1 
LEASED........2 
PRIVATE RENT..3 
GOVT RENT.....4 
RENT FREE.....5 
SQUATTED......6 
*/


gen viviprop_ch =.
replace viviprop_ch=0 if (i10==3 | i10==4)
replace viviprop_ch=1 if i10==1
replace viviprop_ch=2 if i10==2
replace viviprop_ch=3 if (i10==5 | i10==6)

**************
* vivitit_ch *
**************

gen vivitit_ch=.

**************
* vivialq_ch *
**************

/* i14_2
NONE...0 
WEEK...3 
MONTH..4 
YEAR...5
*/

gen vivialq_ch=.
replace vivialq_ch=i14_1*4.3 if i14_2==3
replace vivialq_ch=i14_1 if i14_2==4
replace vivialq_ch=i14_1/12  if i14_2==5
replace vivialq_ch=. if i14_1==99


*****************
* vivialqimp_ch *
*****************

gen vivialqimp_ch=i11 


**Otras Variables**

g raza_idioma_ci =.
g id_ind_ci  =.
g id_afro_ci=. 
g raza_ci =.
g  tcylmpri_ci =.
g tcylmpri_ch=.

  /*_____________________________________________________________________________________________________*/
* Asignación de etiquetas e inserción de variables externas: tipo de cambio, Indice de Precios al 
* Consumidor (2011=100), líneas de pobreza
/*_____________________________________________________________________________________________________*/


do "$gitFolder\armonizacion_microdatos_encuestas_hogares_scl\_DOCS\\Labels&ExternalVars_Harmonized_DataBank.do"

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
