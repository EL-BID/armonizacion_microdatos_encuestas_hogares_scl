* (Version stata 12)
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
local ANO "2018"
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
Autores: 
Versiones anteriores: 
Fecha última modificación: 

							SCL/LMK - IADB
****************************************************************************/
****************************************************************************/



use `base_in', clear


//////////////////////////////////////////////////////////////////////////
         											   
 *       			JAMAICA 2018
                             												   
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

gen anio_c=2018

*********
* mes_c *
*********

gen mes_c= mths_inhh
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
*Modificacion María Reyes Retana- Enero 2018 Population 2018= 2935000

sum finwght
scalar pob=r(sum)
gen pop=finwght*(2935000/pob)

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

destring marital_stat, replace
gen byte civil_ci=.
replace civil_ci=1 if  marital_stat==2
replace civil_ci=2 if  marital_stat==1
replace civil_ci=3 if  marital_stat==3 | marital_stat==4
replace civil_ci=4 if  marital_stat==5
label var civil_ci "Estado Civil"
label define civil_ci 1 "Soltero" 2 "Union Formal o Informal" 3 "Divorciado o Separado" 4 "Viudo"
label value civil_ci civil_ci


***********
* jefe_ci *
***********

gen jefe_ci=(relat==1)
label var jefe_ci "Jefe de Hogar Declarado"

***************************************************************************
*	nconyuges_ch & nhijos_ch & notropari_ch & notronopari_ch & nempdom_ch
****************************************************************************
by idh_ch, sort: egen nconyuges_ch=sum(relacion_ci==2)
by idh_ch, sort: egen nhijos_ch=sum(relacion_ci==3)
by idh_ch, sort: egen notropari_ch=sum(relacion_ci==4)
by idh_ch, sort: egen notronopari_ch=sum(relacion_ci==5)
by idh_ch, sort: egen nempdom_ch=sum(relacion_ci==6)
label var nconyuges_ch "Numero de conyuges"
label var nhijos_ch "Numero de hijos"
label var notropari_ch "Numero de otros familiares"
label var notronopari_ch "Numero de no familiares"
label var nempdom_ch "Numero de empleados domesticos"

****************
* clasehog_ch *
****************
gen clasehog_ch=.
replace clasehog_ch=1 if nhijos_ch==0 & nconyuges_ch==0 & notropari_ch==0 & notronopari_ch==0 
replace clasehog_ch=2 if (nhijos_ch>0| nconyuges_ch>0) & (notropari_ch==0 & notronopari_ch==0) 
replace clasehog_ch=3 if notropari_ch>0 & notronopari_ch==0 
replace clasehog_ch=4 if ((nconyuges_ch>0 | nhijos_ch>0 | notropari_ch>0) & (notronopari_ch>0)) 
replace clasehog_ch=5 if nhijos_ch==0 & nconyuges_ch==0 & notropari_ch==0 & notronopari_ch>0
label variable clasehog_ch "Tipo de hogar"
label define clasehog_ch 1 "Unipersonal" 2 "Nuclear" 3 "Ampliado" 4 "Compuesto" 5 "Corresidente"
label value clasehog_ch clasehog_ch

***************************************************************************************
*	nmiembros_ch & nmayor21_ch & nmenor21_ch & nmayor65_ch & nmenor6_ch & nmenor1_ch  
***************************************************************************************
by idh_ch, sort: egen nmiembros_ch=sum(relacion_ci>=1 & relacion_ci<=4)
label var nmiembros_ch "Numero de familiares en el hogar"

by idh_ch, sort: egen nmayor21_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad>=21)
label var nmayor21_ch "Numero de familiares mayores a 21 anios"

by idh_ch, sort: egen nmenor21_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad<21)
label var nmenor21_ch "Numero de familiares menores a 21 anios"

by idh_ch, sort: egen nmayor65_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad_ci>=65)
label var nmayor65_ch "Numero de familiares mayores a 65 anios"

by idh_ch, sort: egen nmenor6_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad_ci<6)
label var nmenor6_ch "Numero de familiares menores a 6 anios"

by idh_ch, sort: egen nmenor1_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad_ci<1)
label var nmenor1_ch "Numero de familiares menores a 1 anio"

******************************
*	miembros_ci
******************************
gen miembros_ci=(relacion_ci<5)
label var miembros_ci "Miembro del hogar"


******************************************************************************
*	VARIABLES DE DIVERSIDAD
******************************************************************************

    ***************
	***afroind_ci***
	***************
gen afroind_ci=. 


	***************
	***afroind_ch***
	***************

gen afroind_ch=.

    *******************
	***afroind_ano_c***
	*******************
gen afroind_ano_c=.

    *************
	***dis_ci***
	**************
	
gen dis_ci= .


	*************
	***dis_ch***
	**************
egen dis_ch = sum(dis_ci), by(idh_ch) 
replace dis_ch=1 if dis_ch>=1 & dis_ch!=. 

******************************************************************************
*		LABOR MARKET
******************************************************************************

***************
* condocup_ci *
***************

gen condocup_ci=.
replace condocup=1 if (m1==1 | m2==1 | m2==2) 
replace condocup=2 if (m1==2 & m2==3)
replace condocup=3 if (m1==2 & (m2>=4 & m2<=7))
replace condocup=4 if edad_ci<14
label define condocup_ci 1"ocupados" 2"desocupados" 3"inactivos" 4"menor que 14"
label value condocup_ci condocup_ci
label var condocup_ci "Condicion de ocupacion utilizando definicion del pais"

*****************
* categoinac_ci *
*****************

gen categoinac_ci=.
replace categoinac_ci=2 if m2==5


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
replace categopri_ci=1 if m11==5
replace categopri_ci=2 if m11 ==6 
replace categopri_ci=3 if (m11==1 | m11 ==2 | m11==3)
replace categopri_ci=4 if m11==4
replace categopri_ci=0 if m11==9 & condocup_ci==1

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

gen spublico_ci=(m11==1 | m11==2)
replace spublico=. if (m11==9 | condocup_ci>1)

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

gen horastot_ci = m8
replace horastot_ci= . if (m7==99 | m7==98 | m7==97)

*****************
* pensionsub_ci *
*****************

gen pensionsub_ci =.

****************
* pension_ci   *
****************
gen pension_ci=.
replace pension_ci=1 if (d9_a==1 | d9_b==1 |d9_c==1 | d9_d==1)
replace pension_ci=0 if (d9_a==2 & d9_b==2 & d9_c==2 & d9_d==2)
*no se distingue entre contributiva y no contributiva
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

gen edusc_ci= (aedu_ci==11) 
label var edusc_ci "1 = personas que han completado el nivel secundario"

************
* eduui_ci *
************

/*Se hace una aproximación, y se genera la variable para las personas que afirman que este año están atendiendo a la universidad
o educación terciaria*/
 
gen eduui_ci=(aedu_ci>11 & aedu_ci<14) 
replace eduui_ci=. if b1==.
label var eduui_ci "1 = personas que no han completado el nivel universitario o superior"

************
* eduuc_ci *
************

**Se hace una aproximación con los diplomas/ certificaciones obtenidas**

gen eduuc_ci= (b24_exam==9 | b24_exam==10 | b24_exam==11 | aedu_ci>=14) 
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

gen pqnoasis_ci=.
gen pqnoasis1_ci= .

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
replace aguared_ch=1 if i27==1  |i27==2
replace aguared_ch=0 if i27>2 & i27<=13

***************
* aguadist_ch *
***************

gen distance = .
replace distance = i35_1*1000 if i35_2 ==1
replace distance = i35_1 if i35_2 ==2

gen aguadist_ch=.
replace aguadist_ch = 1 if i27==1
replace aguadist_ch = 2 if distance <=100
replace aguadist_ch = 3 if distance >100

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

gen combust_ch=(i50==1 | i50==2)
replace combust_ch=. if i50==.

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
replace des1_ch = 0 if i5 == 6
replace des1_ch = 1 if i5 == 1
replace des1_ch = 2 if (i5 == 3 | i5==2 | i5==4)	
replace des1_ch = 3 if i5 == 5

**Nota: Se asume que la opción 5 (otros) correspondería a desemboca en calle o río.**

***********
* des2_ch *
***********

gen des2_ch =.
replace des2_ch=0 if i5==6
replace des2_ch=1 if (i5==1  |i5 == 3 | i5==2 | i5==4)
replace des2_ch=2 if (i5==5)


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
	

gen resid_ch =.
replace resid_ch=0 if (i48==1 | i48==2 | i48==3)
replace resid_ch=1 if (i48==4 | i48==5)
replace resid_ch=2 if (i48==6 | i48==7 | i48==8 | i48==9)

	
*********************
***aguamejorada_ch***
*********************
gen aguamejorada_ch = 1 if  i27 ==1 | (i27 >=1 & i27 <=4) | (i27 >=8 & i23 <=12)
replace aguamejorada_ch = 0 if  i27 ==2 | (i23 >=4 & i23 <=7)
		
*********************
***banomejorado_ch***
*********************
gen banomejorado_ch = 1 if  i5 ==1
replace banomejorado_ch = 0 if (i5>=2 & i5 <=6) | i4 ==3

***********
* dorm_ch *
***********

gen dorm_ch=.

**************
* cuartos_ch *
**************

gen cuartos_ch= i3
replace cuartos_ch =. if cuartos_ch==.

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

gen refrig_ch=(j604==1)
replace refrig_ch=. if j604==.
*es refri o freezer
************
* freez_ch *
************

gen freez_ch=(j604==1)
replace freez_ch=. if j604==.
*es refri o freezer

***********
* auto_ch *
***********

gen auto_ch=(j615==1)
replace auto_ch=. if j615==.

************
* compu_ch *
************

gen compu_ch  = (i43a==1 | i43b==1 | i43c==1 | i43d==1)
replace compu_ch=. if (i43a==0 & i43b==0 & i43c==0 & i43d==0)


***************
* internet_ch *
***************

gen internet_ch =(i44==1)
replace internet_ch=. if i44==.

**********
* cel_ch *
**********
gen cel_ch  =(i40_2==1 | i40_3==1)
replace telef_ch=. if (i40_2==. & i40_3==.)

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
edus1c_ci edus2i_ci edus2c_ci edupre_ci eduac_ci asiste_ci repite_ci repiteult_ci edupub_ci tecnica_ci ///
aguared_ch aguadist_ch aguamala_ch aguamide_ch luz_ch luzmide_ch combust_ch	bano_ch banoex_ch des1_ch des2_ch piso_ch aguamejorada_ch banomejorado_ch  ///
pared_ch techo_ch resid_ch dorm_ch cuartos_ch cocina_ch telef_ch refrig_ch freez_ch auto_ch compu_ch internet_ch cel_ch ///
vivi1_ch vivi2_ch viviprop_ch vivitit_ch vivialq_ch	vivialqimp_ch , first


compress


saveold "`base_out'", replace


log close
