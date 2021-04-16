set more off

***********************************************************************************************************
*****                                      JAMAICA 2007                                               *****
*****                            SLC 2007 (SURVEY OF LIVING CONDITIONS)                               *****
*****                                         MAYO                                                    *****
***********************************************************************************************************

use "${surveysFolder}\ARM\JAM\2007\Orig_data\jam07_jslc.dta", clear 

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


/** VARIABLE AREA 
  Area code |      Freq.     Percent        Cum.
------------+-----------------------------------
          1 |      2,456       30.01       30.01
          2 |      1,768       21.60       51.61
          3 |      3,960       48.39      100.00
------------+-----------------------------------
*/

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


replace marital = "." if marital == "N"

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

gen jefe_ci=(relatn==1)
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

gen clasehog_ch=.
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

gen miembros_ci=.
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



*******************
* Demanda Laboral *
*******************

************
* ocupa_ci *
************

** con la variable l01 de JSLC **

replace L01 ="." if L01 == "NNNN"
destring L01, replace

gen l01 = L01

gen ocupa_ci=.
replace ocupa_ci=1 if l01>=2000 & l01<=3999
replace ocupa_ci=2 if l01>=1000 & l01<=1999
replace ocupa_ci=3 if l01>=4000 & l01<4999
replace ocupa_ci=4 if l01>=5200 & l01<5999
replace ocupa_ci=5 if l01>=5000 & l01<=5199
replace ocupa_ci=6 if l01>=6000 & l01<=6999
replace ocupa_ci=7 if l01>=7000 & l01<=8999
replace ocupa_ci=8 if l01>0 & l01<=999 
replace ocupa_ci=9 if l01>=9000 & l01<9999

label var ocupa_ci "Ocupacion Laboral en la Actividad Principal"
label define ocupa_ci 1 "PROFESIONALES Y TECNICOS" 2 "GERENTES, DIRECTORES Y FUNCIONARIOS SUPERIORES"  3 "PERSONAL ADMINISTRATIVO Y NIVEL INTERMEDIO" 4 "COMERCIANTES Y VENDEDORES" 5 "TRABAJADORES EN SERVICIOS" 6 "TRABAJADORES AGRICOLAS Y AFINES" 7 "OBREROS NO AGRICOLAS, CONDUCTORES DE MAQUINAS Y VEHICULOS DE TRANSPORTE Y SIMILARES" 8 "FUERZAS ARMADAS" 9 "OTRAS OCUPACIONES NO CLASIFICADAS ANTERIORMENTE"
label values ocupa_ci ocupa2_ci

/*
** con la variable Q38M de LFS **
capture drop ocupa_ci 
gen ocupa_ci=.
replace ocupa_ci=1 if Q38M>=2000 & Q38M<=3999
replace ocupa_ci=2 if Q38M>=1000 & Q38M<=1999
replace ocupa_ci=3 if Q38M>=4000 & Q38M<=4999
replace ocupa_ci=4 if Q38M>=5200 & Q38M<=5999
replace ocupa_ci=5 if Q38M>=5000 & Q38M<=5199
replace ocupa_ci=6 if Q38M>=6000 & Q38M<=6999
replace ocupa_ci=7 if Q38M>=7000 & Q38M<=8999
replace ocupa_ci=8 if Q38M>0 & Q38M<=999 
replace ocupa_ci=9 if (Q38M>=9000 & Q38M<=9996) | Q38M==9998

label var ocupa_ci "Ocupacion Laboral en la Actividad Principal"
label define ocupa_ci 1 "PROFESIONALES Y TECNICOS" 2 "GERENTES, DIRECTORES Y FUNCIONARIOS SUPERIORES"  3 "PERSONAL ADMINISTRATIVO Y NIVEL INTERMEDIO" 4 "COMERCIANTES Y VENDEDORES" 5 "TRABAJADORES EN SERVICIOS" 6 "TRABAJADORES AGRICOLAS Y AFINES" 7 "OBREROS NO AGRICOLAS, CONDUCTORES DE MAQUINAS Y VEHICULOS DE TRANSPORTE Y SIMILARES" 8 "FUERZAS ARMADAS" 9 "OTRAS OCUPACIONES NO CLASIFICADAS ANTERIORMENTE"
label values ocupa_ci ocupa_ci
*/

***********
* rama_ci *
***********

** con la variable L03 de JSLC **

replace L03 ="." if L03 == "NNNN"
destring L03, replace

gen byte rama_ci=.
replace rama_ci=1 if L03>0 & L03<1000
replace rama_ci=2 if L03>=1000 & L03<2000
replace rama_ci=3 if L03>=2000 & L03<4000
replace rama_ci=4 if L03>=4000 & L03<5000
replace rama_ci=5 if L03>=5000 & L03<6000
replace rama_ci=6 if L03>=6000 & L03<7000
replace rama_ci=7 if L03>=7000 & L03<8000
replace rama_ci=8 if L03>=8000 & L03<9000
replace rama_ci=9 if L03>=9000 & L03<10000

label var rama_ci "Rama Laboral en la Ocupacion Principal"
label define rama_ci 1 "Agricultura, caza, silvicultura y pesca" 2 "Explotación de minas y canteras" 3 "Industrias manufactureras" 4 "Electricidad, gas y agua" 5 "Construcción" 6 "Comercio al por mayor y menor, restaurantes, hoteles" 7 "Transporte y almacenamiento" 8 "Establecimientos financieros, seguros, bienes inmuebles" 9 "Servicios sociales, comunales y personales"
label values rama_ci rama_ci

***************
* horaspri_ci *
***************

capture drop horaspri_ci horastot_ci
gen byte horaspri_ci=.
label var horaspri_ci "Horas totales trabajadas la semana pasada en la Actividad Principal"

***************
* horastot_ci *
***************

gen byte horastot_ci=.
label var horastot_ci "Horas totales trabajadas la semana pasada en todas las Actividades"

************
************
* INGRESOS *
************
************

*************
* ylmpri_ci *
*************

gen ylmpri_ci=.
label var ylmpri_ci "Ingreso Laboral Monetario de la Actividad Principal"

*************
* ylmsec_ci *
*************

gen ylmsec_ci=.	
label var ylmsec_ci "Ingreso Laboral Monetario de la Actividad Secundaria"

***************
* ylmotros_ci *
***************

gen ylmotros_ci=.
label var ylmotros_ci "Ingreso Laboral Monetario Otros Trabajos"

**************
* ylnmpri_ci *
**************

gen ylnmpri_ci=.
label var ylnmpri_ci "Ingreso Laboral NO Monetario de la Actividad Principal"

**************
* ylnmsec_ci *
**************

gen ylnmsec_ci=.	
label var ylnmsec_ci "Ingreso Laboral NO Monetario de la Actividad Secundaria"

****************
* ylnmotros_ci *
****************

gen ylnmotros_ci=.
label var ylnmotros_ci "Ingreso Laboral NO Monetario Otros Trabajos"

**********
* ylm_ci *
**********

gen ylm_ci=. /* rsum(ylmpri_ci ylmsec_ci) */
replace ylm_ci=. if ylmpri_ci==. & ylmsec_ci==.
label var ylm_ci "Ingreso Laboral Monetario Total (Labour Force Survey)"

***********
* ylnm_ci *
***********

gen ylnm_ci=.
label var ylnm_ci "Ingreso Laboral NO Monetario Total"


********************************************************************************************************
/*
egen ynlm=rsum(k02701 k02702 k02703 k02704 k02705 k02706 k02707 k02708 k02709 k02710)
gen ynlm_ci=ynlm/(365/30)
label var ynlm_ci "Ingreso NO Laboral Monetario, inc especies (Jamaica Survey of Living Conditions)"
capture drop ynlm

gen ynlnm_ci=.
label var ynlnm_ci "Ingreso NO Laboral NO Monetario"

capture drop nrylmpri_ci
gen nrylmpri_ci=.
replace nrylmpri_ci=0 if (Q325E>=0)
replace nrylmpri_ci=1 if (Q325A==9) | (Q325E==. & Q325O<.)
label var nrylmpri_ci "Identificador de No Respuesta del Ingreso Monetario de la Actividad Principal"

gen autocons_ci=.
label var autocons_ci "Autoconsumo Individual"

gen remesas_ci=k02703/(365/30)
label var remesas_ci "Remesas Individuales"

capture drop nrylmpri_ch
sort idh
egen nrylmpri_ch=sum(nrylmpri_ci) if miembro==1, by(idh) 
replace nrylmpri_ch=1 if nrylmpri_ch>1 & nrylmpri_ch~=. & miembro==1 
label var nrylmpri_ch "Identificador de Hogares en donde alguno de los miembros No Responde el Ingreso Monetario de la Actividad Principal"

egen ylm_ch=sum(ylm_ci) if miembros_ci==1, by(idh_ch)
label var ylm_ch "Ingreso Laboral Monetario del Hogar"

egen ylmnr_ch=sum(ylm_ci) if miembros_ci==1 & nrylmpri_ch==0, by(idh_ch)
label var ylmnr_ch "Ingreso Laboral Monetario del Hogar, considera 'missing' la No Respuesta"

egen ylnm_ch=sum(ylnm_ci) if miembros_ci==1, by(idh_ch)
label var ylnm_ch "Ingreso Laboral No Monetario del Hogar"

egen ynlm_ch=sum(ynlm_ci) if miembros_ci==1, by(idh_ch)
label var ynlm_ch "Ingreso No Laboral Monetario del Hogar, inc especies (Jamaica Survey of Living Conditions)"

egen ynlnm_ch=sum(ynlnm_ci) if miembros_ci==1, by(idh_ch)
label var ynlnm_ch "Ingreso No Laboral No Monetario del Hogar"

egen autocons_ch=rsum(f07*) if miembros_ci==1
label var autocons_ch "Autoconsumo del Hogar"

egen remesas_ch=sum(remesas_ci) if miembros_ci==1, by(idh_ch)
label var remesas_ch "Remesas del Hogar (monetario)"


replace ylm_ch =. if miembros_ci==0
replace ylmnr_ch =. if miembros_ci==0
replace ylnm_ch =. if miembros_ci==0
replace ynlnm_ch =. if miembros_ci==0
replace autocons_ch =. if miembros_ci==0
replace remesas_ch =. if miembros_ci==0
replace ynlm_ch =. if miembros_ci==0

gen ylmhopri_ci=.
replace ylmhopri_ci=ylmpri_ci/(horaspri*(30/7))
label var ylmhopri_ci "Salario Horario Monetario de la Actividad Principal"

gen ylmho_ci=.
replace ylmho_ci=ylm_ci/(horastot*(30/7))
label var ylmho_ci "Salario Horario Monetario de todas las Actividades"
*/
*************************************************************************************************

*****************
* antiguedad_ci *
*****************

gen L07YR2 = L07YR
gen L07MT2 = L07MT

replace L07YR2 = "." if L07YR=="NN" 
replace L07MT2 = "." if L07MT=="NN" 

destring L07YR2 L07MT2, replace
gen L07MT2a = L07MT2/12
egen antiguedad_ci= rsum(L07YR2 L07MT2a)
replace antiguedad_ci = . if L07YR == "" & L07MT == "" 
replace antiguedad_ci = . if L07YR == "NN" & L07MT == "NN" 


label var antiguedad_ci "Antiguedad en la Ocupacion Actual (en anios)"

**************
* durades_ci *
**************

gen durades_ci=.
label var durades_ci "Duracion del Desempleo (en meses)"

****************
* categopri_ci *
****************

/*
CENTRAL GOV'T EMPLOYEE.......1 
OTHER GOV'T AGENCY EMPLOYEE..2 
PRIVATE SECTOR EMPLOYEE......3 
UNPAID FAMILY WORKER.........4 
EMPLOYER.....................5 
OWN ACCOUNT WORKER...........6 
NOT STATED...................7 
*/

replace L02 = "." if L02 == "N"
destring L02, replace

gen categopri_ci=.
replace categopri_ci=1 if L02==5 
replace categopri_ci=2 if L02==6 
replace categopri_ci=3 if L02==1 | L02==2 | L02==3 
replace categopri_ci=4 if L02==4

label var categopri_ci "CATEGORIA OCUPACIONAL ACTIVIDAD PRINCIPAL"
label define categopri_ci 1 "Patron" 2 "Cuenta Propia" 3 "Asalariado" 4 "Trabajador No Remunerado" 
label values categopri_ci categopri_ci

****************
* categosec_ci *
****************

gen categosec_ci=.
label var categosec_ci "CATEGORIA OCUPACIONAL ACTIVIDAD SECUNDARIA"

***************
* contrato_ci *
***************

gen contrato_ci=.
label var contrato "Personas empleadas que han firmado un contrato de trabajo"

*************
* segsoc_ci *
*************

gen segsoc_ci=.
label variable segsoc_ci "Personas que cuentan con seguro social"

***************
* nempleos_ci *
***************

gen nempleos_ci=.
label var nempleos_ci "Numero de empleos"
label define nempleos_ci 1 "un trabajo" 2 "dos trabajos"
label values nempleos_ci nempleos_ci

***************
* tamfirma_ci *
***************

gen byte tamfirma_ci=.
label var tamfirma "Trabajadores formales"
label define tamfirma_ci 1 "5 o mas trabajadores" 0 "Menos de 5 trabajadores"
label values tamfirma_ci tamfirma_ci

***************
* spublico_ci *
***************

gen spublico_ci=.
replace spublico_ci=1 if (L02==1 | L02==2) 
replace spublico_ci=0 if (L02>2 & L02<=6) 
label var spublico_ci "Personas que trabajan en el sector publico"

*************
* EDUCACION *
*************

/* B01
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

replace B01 = "." if B01 =="NN"
destring B01, replace 

gen NIVEL=0 if B01==1 | B01==2 | B01==4 
replace NIVEL=1 if B01==3 | B01==5 | B01==7 /* PRIMARIO */
replace NIVEL=2 if B01==6 | B01==8 | B01==9  /* 1 CICLO SECUNDARIO */
replace NIVEL=3 if B01==10 | B01==11 /* 2 CICLO SECUNDARIO */
replace NIVEL=4 if B01>=13 & B01<=15 /* TERCIARIO / UNIVERSITARO */

/* B21
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

replace B21 = "." if B21 =="NN"
destring B21, replace 

gen NIVEL2=0 if B21==1 | B21==20 | B21==3 
replace NIVEL2=1 if B21==2 | B21==4 | B21==6 /* PRIMARIO */
replace NIVEL2=2 if B21==7 | B21==8 | B21==9 /* 1 CICLO SECUNDARIO */
replace NIVEL2=3 if B21==11 | B21==12  /* 2 CICLO SECUNDARIO */
replace NIVEL2=4 if B21>=14 & B21<=16 /* TERCIARIO / UNIVERSITARO */

replace B04 = "." if B04 =="NN"
destring B04, replace 

gen GRADO=B04 if NIVEL>0 & NIVEL<=3 /* son los grados para los que asisten actualmente a primaria o secundaria */ 
/* y los que asisten a universitario ????*/

replace B22 = "." if B22 =="NN"
destring B22, replace 

gen GRADO2=B22 if NIVEL2>0 & NIVEL2<=3 /* son los grados de primaria o secundaria para los que no asisten actualmente */

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
replace aedu_ci=0 if B01==19 & B21==20
label variable aedu_ci "Años de Educacion (no incluye terciaria o universitaria)"

************
* eduno_ci *
************

gen eduno_ci=.
replace eduno_ci=1 if B01==19 & B21==20
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

replace B24 = "." if B24 == "NN"
destring B24, replace

gen eduui_ci=.
replace eduui_ci=1 if NIVEL==4 | (NIVEL2==4 & B24<7)
replace eduui_ci=0 if (aedu>=0 & aedu<=12) | (NIVEL2==4 & (B24==7 | B24==8))
label var eduui_ci "1 = personas que no han completado el nivel universitario o superior"

************
* eduuc_ci *
************

gen eduuc_ci=.
replace eduuc_ci=1 if NIVEL2==4 & (B24==7 | B24==8)
replace eduuc_ci=0 if (aedu>=0 & aedu<=12) | NIVEL==4 | (NIVEL2==4 & B24<7)
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
replace eduac_ci=0 if B01==14 | B01==15 | B21==15 | B21==16
replace eduac_ci=1 if B01==13 | B21==14
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

replace B03 = "." if B03== "N"
destring B03, replace

gen edupub_ci=.
replace edupub_ci=1 if B03==1
replace edupub_ci=0 if B03==2
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

replace B10R1 = "." if B10R1== "NN"
destring B10R1, replace

gen byte pqnoasis_ci=.
replace pqnoasis_ci=B10R1
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

replace I18 = "." if I18 == "NN"
destring I18, replace

gen aguared_ch=.
replace aguared_ch=1 if I18==1
replace aguared_ch=0 if I18>=2 & I18<=13

***************
* aguadist_ch *
***************

gen distance = .
replace distance = I24*1000 if I24p =="1"
replace distance = I24 	if I24p =="2"

gen aguadist_ch=.
replace aguadist_ch = 1 if distance >100
replace aguadist_ch = 0 if distance <=100

***************
* aguamala_ch *
***************

gen aguamala_ch=.
replace aguamala_ch = 1 if I18 >= 4 & I18 <= 7
replace aguamala_ch = 0 if (I18 >= 1 & I18 <= 3) | (I18 >= 8 & I18 <= 13)

***************
* aguamide_ch *
***************

replace I20 = "." if I20 == "N"
destring I20, replace

gen aguamide_ch=.
replace aguamide_ch = 1 if I20 == 1 | I20== 2
replace aguamide_ch = 0 if I20 == 3

**********
* luz_ch *
**********

replace I25 = "." if I25 == "N"
destring I25, replace
/*
ELECTRICITY..1 
KEROSENE.....2 (Q281)
OTHER........3 (Q281)
NONE.........4 (Q281)
*/

gen luz_ch=.
replace luz_ch=1 if I25==1
replace luz_ch=0 if I25>=2 & I25<=4

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

replace I04 = "." if I04 == "N"
destring I04, replace

/*
LINKED....1 
NOT LINK..2 
PIT.......3 
OTHER.....4 
NONE......5 
*/

gen bano_ch=.
replace bano_ch=1 if I04==1 | I04==2 | I04==3
replace bano_ch=0 if I04==4 | I04==5

*************
* banoex_ch *
*************

gen banoex_ch=.

***********
* des1_ch *
***********

gen des1_ch=.
replace des1_ch = 0 if I04 == 4
replace des1_ch = 1 if I04 == 1
replace des1_ch = 2 if I04 == 3
replace des1_ch = 3 if I04 == 2 | I04 == 5

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

replace I02 = "." if I02 == "N"
destring I02, replace

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
replace pared_ch=1 if I02>=1 & I02<=5
replace pared_ch=0 if I02>=6 & I02<=7

************
* techo_ch *
************

gen techo_ch=.

************
* resid_ch *
************

replace I31 = "." if I31 == "N"
destring I31, replace

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
replace resid_ch=0 if I31==1 | I31==2
replace resid_ch=1 if I31==3 | I31==4
replace resid_ch=2 if I31==5 | I31==6
replace resid_ch=3 if I31==7

***********
* dorm_ch *
***********

gen dorm_ch=.

**************
* cuartos_ch *
**************

replace I03 = "." if I03 == "NN"
destring I03, replace

gen cuartos_ch=.
replace cuartos_ch= I03

*************
* cocina_ch *
*************

replace I06 = "." if I06 == "N"
destring I06, replace

gen cocina_ch=.
replace cocina_ch = 1 if I06 == 1 | I06 == 2
replace cocina_ch = 0 if I06 == 3

************
* telef_ch *
************

replace I281 = "." if I281 == "N"
destring I281, replace

gen telef_ch=.
replace telef_ch=1 if I281==1
replace telef_ch=0 if I281==2

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

replace I32 = "." if I32 == "N"
destring I32, replace

gen compu_ch=.
replace compu_ch=1 if I32==1
replace compu_ch=0 if I32==2

***************
* internet_ch *
***************

replace I33 = "." if I33 == "N"
destring I33, replace

gen internet_ch=.
replace internet_ch=1 if I33==1 
replace internet_ch=0 if I33==2

**********
* cel_ch *
**********

replace I282 = "." if I282 == "N"
destring I282, replace

gen cel_ch=.
replace cel_ch=1 if I282==1
replace cel_ch=0 if I282==2

************
* vivi1_ch *
************

replace I01 = "." if I01 == "N"
destring I01, replace

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
replace vivi1_ch=1 if I01==1 | I01==2 | I01==5 
replace vivi1_ch=2 if I01==4
replace vivi1_ch=3 if I01==3 | (I01>=6 & I01<=8)

************
* vivi2_ch *
************

gen vivi2_ch=.
replace vivi2_ch=1 if vivi1_ch==1 | vivi1_ch==2
replace vivi2_ch=0 if vivi1_ch==3

***************
* viviprop_ch *
***************

replace I07 = "." if I07 == "N"
destring I07, replace

/*
OWNED.........1 
LEASED........2 (Q09)
PRIVATE RENT..3 (Q09)
GOVT RENT.....4 (Q12)
RENT FREE.....5 (Q12)
SQUATTED......6 (Q12)
*/

gen viviprop_ch=.
replace viviprop_ch=0 if I07==3 | I07==4
replace viviprop_ch=1 if I07==1
replace viviprop_ch=2 if I07==2
replace viviprop_ch=3 if I07==5 | I07==6 | I07==7

**************
* vivitit_ch *
**************

gen vivitit_ch=.

**************
* vivialq_ch *
**************

replace I10p = "." if I10p == "N"
destring I10p, replace

/* I10p
NONE...0 
WEEK...3 
MONTH..4 
YEAR...5
*/

gen vivialq_ch=.
replace vivialq_ch=I10*4.3 if I10p==3
replace vivialq_ch=I10	   if I10p==4
replace vivialq_ch=I10/12  if I10p==5


*****************
* vivialqimp_ch *
*****************

gen vivialqimp_ch=. 

drop NIVEL GRADO NIVEL2 GRADO2  l01 L07YR2 L07MT2 L07MT2a

save "${surveysFolder}\ARM\JAM\2007\Arm_data\JAM2007EA_BID.dta", replace

-------


/*  Muestra los individuos que fueron 'matcheados' en la Encuesta de Living Conditions con la de Labour Force */
gen match=0
replace match=1 if sex==SEX & abs(age-AGE)<=1
tab match if muestra==1

/* match	Freq.	Percent	Cum.
			
0	1,321	7.33	7.33
1	16,700	92.67	100.00
			
Total	18,021	100.00 */

gen match2=0
replace match2=1 if relatn==REL
tab match2 if muestra==1

/* match2	Freq.	Percent	Cum.
				
	0	746	4.14	4.14
	1	17,275	95.86	100.00
				
	Total	18,021	100.00 */


capture log close

********************
*** Jamaica 2002 ***
********************

* Variables

 destring sex, replace
 gen sexo = sex
 
 destring age, replace 
 gen edad = age
 
 destring i17, replace
 gen water = i17
 
 destring i04, replace
 gen servsani = i04
 
 destring i05, replace
 gen usosani = i05
 
 destring i03, replace 
 gen nrocuart = i03
 
 destring i01, replace 
 gen tipoviv = i01
 
 destring i02, replace 
 gen paredes = i02
 
 destring i07, replace 
 gen tenenviv = i07
 
 destring b01 b04 b21 b22, replace
 gen pers = hhsize1
 
 destring i24 i27 i27p relatn hhm,replace ignore ("N")
 
 gen cursoasi = b01
 gen gradoasi = b04
 gen ultcurso = b21 
 gen ultgrado = b22
 
/*
hhm
Household Member
 1. Still a member
 2. No longer a member
 3. New member
 
relatn
Relationship Codes
 1 Head
 2 Spouse/partner
 3 Child of head or of spouse
 4 Spouse of child
 5 Grandchild
 6 Parent of head/spouse
 7 Other relative
 8 Helper/domestic
 9 Other not relative
*/

 gen	 incl=1 if (hhm>=1 & hhm<=3)
 replace incl=0 if (hhm==2)

** AREA

 destring area, replace
 gen area_ORIG = area

 generate area_1=.
 replace  area_1=1 if (area_ORIG==1 | area_ORIG==2 | area_ORIG==4 | area_ORIG==5)
 replace  area_1=2 if (area_ORIG==3)

** Gender classification of the population refering to the head of the household.

 sort serial ind

 gen	 sexo_d_=1 if relatn==1 & sexo==1 & incl==1 
 replace sexo_d_=2 if relatn==1 & sexo==2 & incl==1 
 egen sexo_d=max(sexo_d_), by(serial)

 sort serial ind

** Years of education. 

/*
cursoasi(b01)
1. What type of school is ... attending this academic year?

  1. Nursery/Daycare/Basic infant/Kinder
  2. Primary
  3. All age school (1-6)
  4. All age school (7-9)
  5. Primary/Junior High (1-6)
  6. Primary/Junior High (7-9)
  7. Junior High(7-9)
  8. Secondary High
  9. Technical
 10. Vocat/Agric
 11. University
 12. Other Tertiary (Public)
 13. Other Tertiary (Private)
 14. Adult Literacy classes
 15. Adult Education Night
 16. Special School
 17. None
 
gradoasi (b04)
4. What grade is... in at school this year
 Primary 1-6
 7, 8, 9, 10, 11, 12, 13
 
 Grade ___

ultcurso (b21)

21. What type of school did ... last attend?

  1. Basic infant
  2. Primary
  3. All age school (1-6)
  4. All age school (7-9)
  5. Primary/Junior High (1-6)
  6. Primary/Junior High (7-9)
  7. Junior High(7-9)
  8. New Secondary
  9. Comprehensive
 10. Secondary High
 11. Technical
 12. Vocat/Agric
 13. University
 14. Other Tertiary (Public)
 15. Other Tertiary (Private)
 16. Adult Literacy classes
 17. Adult Education Night
 18. Special School
 19. None

ultgrado (b22) 

22. What was the last grade completed at that school?

Years___
*/

 gen	 anoest=.
 replace anoest=0  if (cursoasi==2 & gradoasi==1) | (cursoasi==3 & gradoasi==1) | (cursoasi==4 & gradoasi==1) | (cursoasi==5 & gradoasi==1) | (cursoasi==6 & gradoasi==1) | cursoasi==1 | cursoasi==14 | cursoasi==16 | ultgrado==1 | ultcurso==19 | ultcurso==16 | ultcurso==17 | ultcurso==18
 replace anoest=1  if (cursoasi==2 & gradoasi==2) | (cursoasi==3 & gradoasi==2) | (cursoasi==4 & gradoasi==2) | (cursoasi==5 & gradoasi==2) | (cursoasi==6 & gradoasi==2) | (ultcurso==2 & ultgrado==1) | (ultcurso==3 & ultgrado==1) | (ultcurso==4 & ultgrado==1) | (ultcurso==5 & ultgrado==1) | (ultcurso==6 & ultgrado==1)
 replace anoest=2  if (cursoasi==2 & gradoasi==3) | (cursoasi==3 & gradoasi==3) | (cursoasi==4 & gradoasi==3) | (cursoasi==5 & gradoasi==3) | (cursoasi==6 & gradoasi==3) | (ultcurso==2 & ultgrado==2) | (ultcurso==3 & ultgrado==2) | (ultcurso==4 & ultgrado==2) | (ultcurso==5 & ultgrado==2) | (ultcurso==6 & ultgrado==2)
 replace anoest=3  if (cursoasi==2 & gradoasi==4) | (cursoasi==3 & gradoasi==4) | (cursoasi==4 & gradoasi==4) | (cursoasi==5 & gradoasi==4) | (cursoasi==6 & gradoasi==4) | (ultcurso==2 & ultgrado==3) | (ultcurso==3 & ultgrado==3) | (ultcurso==4 & ultgrado==3) | (ultcurso==5 & ultgrado==3) | (ultcurso==6 & ultgrado==3)
 replace anoest=4  if (cursoasi==2 & gradoasi==5) | (cursoasi==3 & gradoasi==5) | (cursoasi==4 & gradoasi==5) | (cursoasi==5 & gradoasi==5) | (cursoasi==6 & gradoasi==5) | (ultcurso==2 & ultgrado==4) | (ultcurso==3 & ultgrado==4) | (ultcurso==4 & ultgrado==4) | (ultcurso==5 & ultgrado==4) | (ultcurso==6 & ultgrado==4)
 replace anoest=5  if (cursoasi==2 & (gradoasi>=6 & gradoasi<=9)) | (cursoasi==3 & (gradoasi>=6 & gradoasi<=9)) | (cursoasi==4 & gradoasi==6) | (cursoasi==5 & gradoasi==6) | (cursoasi==6 & gradoasi==6) | (ultcurso==2 & ultgrado==5) | (ultcurso==3 & ultgrado==5) | (ultcurso==4 & ultgrado==5) | (ultcurso==5 & ultgrado==5) | (ultcurso==6 & ultgrado==5)
 replace anoest=6  if (cursoasi==4 & gradoasi==7) | (cursoasi==5 & gradoasi==7) | (cursoasi==6 & gradoasi==7) | (cursoasi==7 & gradoasi==7) | (cursoasi==8 & (gradoasi==7 | gradoasi==1)) | (cursoasi==9 & gradoasi==7) | cursoasi==10 | (ultcurso==2 & (ultgrado>=6 & ultgrado<=9)) | (ultcurso==3 & (ultgrado>=6 & ultgrado<=9)) | (ultcurso==5 & ultgrado==6) | (ultcurso==4 & ultgrado==6) | (ultcurso==6 & ultgrado==6) | ultcurso==12
 replace anoest=7  if (cursoasi==4 & gradoasi==8) | (cursoasi==5 & gradoasi==8) | (cursoasi==6 & gradoasi==8) | (cursoasi==7 & gradoasi==8) | (cursoasi==8 & (gradoasi==8 | gradoasi==2)) | (cursoasi==9 & gradoasi==8) | (ultcurso==4 & ultgrado==7) | (ultcurso==5 & ultgrado==7) | (ultcurso==6 & ultgrado==7) | (ultcurso==7 & ultgrado==7) | (ultcurso==8 & (ultgrado==7|ultgrado==1)) | (ultcurso==9 & (ultgrado==7|ultgrado==1)) | (ultcurso==10 & (ultgrado==7|ultgrado==1)) | (ultcurso==11 & (ultgrado==7|ultgrado==1))   
 replace anoest=8  if (cursoasi==4 & gradoasi==9) | (cursoasi==5 & gradoasi==9) | (cursoasi==6 & gradoasi==9) | (cursoasi==7 & gradoasi==9) | (cursoasi==8 & (gradoasi==9 | gradoasi==3)) | (cursoasi==9 & gradoasi==9) | (ultcurso==4 & ultgrado==8) | (ultcurso==5 & ultgrado==8) | (ultcurso==6 & ultgrado==8) | (ultcurso==7 & ultgrado==8) | (ultcurso==8 & (ultgrado==8|ultgrado==2)) | (ultcurso==9 & (ultgrado==8|ultgrado==2)) | (ultcurso==10 & (ultgrado==8|ultgrado==2)) | (ultcurso==11 & (ultgrado==8|ultgrado==2))
 replace anoest=9  if (cursoasi==8 & (gradoasi==10 | gradoasi==4)) | (cursoasi==9 & gradoasi==10) | (ultcurso==4 & (ultgrado>=9 & ultgrado<=12)) | (ultcurso==5 & ultgrado==9) | (ultcurso==6 & ultgrado==9) | (ultcurso==7 & ultgrado==9) | (ultcurso==8 & (ultgrado==9|ultgrado==3)) | (ultcurso==9 & (ultgrado==9|ultgrado==3)) | (ultcurso==10 & (ultgrado==9|ultgrado==3))
 replace anoest=10 if (cursoasi==8 & (gradoasi==11 | gradoasi==5)) | (cursoasi==9 & gradoasi==11) | (ultcurso==8 & (ultgrado==10|ultgrado==4)) | (ultcurso==9 & (ultgrado==10|ultgrado==4)) | (ultcurso==10 & (ultgrado==10|ultgrado==4)) | (ultcurso==11 & (ultgrado==10|ultgrado==4))
 replace anoest=11 if (cursoasi==8 & (gradoasi==12 | gradoasi==6)) | (cursoasi==9 & gradoasi==12) | (ultcurso==8 & (ultgrado==11|ultgrado==5)) | (ultcurso==9 & (ultgrado==11|ultgrado==5)) | (ultcurso==10 & (ultgrado==11|ultgrado==5)) | (ultcurso==11 & (ultgrado==11|ultgrado==5))
* 12 => 12 years or more
 replace anoest=12 if (ultcurso==8 & (ultgrado==12 | ultgrado==6)) | (ultcurso==9 & (ultgrado==12|ultgrado==6)) | (ultcurso==10 & (ultgrado==12|ultgrado==6)) | (cursoasi==11 | cursoasi==12 | cursoasi==13) | ultcurso==13 | ultcurso==14 | ultcurso==15
 replace anoest=99 if gradoasi==0  


************************
*** MDGs CALCULATION ***
************************


/*
cursoasi(b01)
1. What type of school is ... attending this academic year?

  1. Nursery/Daycare/Basic infant/Kinder
  2. Primary
  3. All age school (1-6)
  4. All age school (7-9)
  5. Primary/Junior High (1-6)
  6. Primary/Junior High (7-9)
  7. Junior High(7-9)
  8. Secondary High
  9. Technical
 10. Vocat/Agric
 11. University
 12. Other Tertiary (Public)
 13. Other Tertiary (Private)
 14. Adult Literacy classes
 15. Adult Education Night
 16. Special School
 17. None
 
gradoasi (b04)
4. What grade is... in at school this year
 Primary 1-6
 7, 8, 9, 10, 11, 12, 13
 
 Grade ___
*/

*** GOAL 2. ACHIEVE UNIVERSAL PRIMARY EDUCATION

** Target 3, Indicator: Net Attendance Ratio in Primary
* ISCED 1

 gen	 NERP=0 if incl==1 & (edad>=6 & edad<=11) & (cursoasi>=1 & cursoasi<=17)
 replace NERP=1 if incl==1 & (edad>=6 & edad<=11) & (gradoasi>=1 & gradoasi<=6) 
	
** Target 3, Additional Indicator: Net Attendance Ratio in Secondary
* ISCED 2 & 3

 gen	 NERS=0 if incl==1 & (edad>=12 & edad<=16) & (cursoasi>=1 & cursoasi<=17)
 replace NERS=1 if incl==1 & (edad>=12 & edad<=16) & (gradoasi>=7 & gradoasi<=11) 

** Target 3, Indicator: Literacy Rate of 15-24 Years Old
* At least 5 years of formal education

 gen	 analfabet=1 if ((cursoasi==2 | cursoasi==3 | cursoasi==5) & (gradoasi>=0 & gradoasi<=5)) | (cursoasi==14) | ((ultcurso==2 | ultcurso==3 | ultcurso==5) & (ultgrado>=0 & ultgrado<=4)) | (ultcurso==1 | ultcurso==19) | (cursoasi==17 & ultcurso==.)
 replace analfabet=0 if analfabet==.

 gen	 LIT=1 if incl==1 & (edad>=15 & edad<=24) 
 replace LIT=0 if incl==1 & (edad>=15 & edad<=24) & analfabet==1

*Literacy Rate of 15-24 Years Old*
* Read & write

* Not available

*** GOAL 3 PROMOTE GENDER EQUALITY AND EMPOWER WOMEN

 gen prim=1 if  incl==1 & (gradoasi>=1  & gradoasi<=6 ) 
 gen sec=1 if   incl==1 & (gradoasi>=7  & gradoasi<=11) 
 gen ter=1 if   incl==1 & (cursoasi>=11 & cursoasi<=13)

** Target 4, Indicator: Ratio Girls to boys in primary, secondary and tertiary (%)

** Target 4, Ratio of Girls to Boys in Primary*

 gen RPRIMM=1 if (prim==1) & sexo==2 
 replace RPRIMM=0 if RPRIMM==. 
 gen RPRIMH=1 if (prim==1) & sexo==1 
 replace RPRIMH=0 if RPRIMH==.

 gen RATIOPRIM=0 if     (prim==1) & sexo==2  
 replace RATIOPRIM=1 if (prim==1)  & sexo==1   
	
** Target 4, Ratio of Girls to Boys in Secondary*

 gen RSECM=1 if (sec==1) & sexo==2 
 replace RSECM=0 if RSECM==.
 gen RSECH=1 if (sec==1) & sexo==1 
 replace RSECH=0 if RSECH==.

 gen RATIOSEC=0     if (sec==1) & sexo==2 
 replace RATIOSEC=1 if (sec==1) & sexo==1  
	
** Target 4, Indicator: Ratio of Girls to Boys in Tertiary*

 gen RTERM=1 if (ter==1) & sexo==2 
 replace RTERM=0 if RTERM==.
 gen RTERH=1 if (ter==1) & sexo==1 
 replace RTERH=0 if RTERH==.

 gen RATIOTER=0     if (ter==1) & sexo==2 
 replace RATIOTER=1 if (ter==1) & sexo==1  


** Target 4, Indicator: Ratio of Girls to Boys in Primary, Secondary and Tertiary*

 gen RALLM=1 if (prim==1 | sec==1 | ter==1) & sexo==2 
 replace RALLM=0 if RALLM==.
 gen RALLH=1 if (prim==1 | sec==1 | ter==1) & sexo==1 
 replace RALLH=0 if RALLH==.

 gen     RATIOALL=0 if (prim==1 | sec==1 | ter==1) & sexo==2  
 replace RATIOALL=1 if (prim==1 | sec==1 | ter==1) & sexo==1    


** Target 4, Indicator: Ratio of literate women to men 15-24 year olds*
* At least 5 years of formal education

 gen MA=1 if incl==1 & ((analfabet==0) & (edad>=15 & edad<=24) & (sexo==2)) 
 replace MA=0 if MA==.
 gen HA=1 if incl==1 & ((analfabet==0) & (edad>=15 & edad<=24) & (sexo==1)) 
 replace HA=0 if HA==.

 gen RATIOLIT=0 if     incl==1 & ((analfabet==0) & (edad>=15 & edad<=24) & (sexo==2)) 
 replace RATIOLIT=1 if incl==1 & ((analfabet==0) & (edad>=15 & edad<=24) & (sexo==1))  

*** GOAL 7 ENSURE ENVIROMENTAL SUSTAINABILITY

** Access to Electricity ** Additional Indicator

/*
i24
MAIN SOURCE OF LIGHTING
 1. Electricity
 2. Kerosene
 3. Other
 4. None
*/

* Gender classification of the population refers to the head of the household.

 gen	 ELEC=0 if incl==1 & (i24>=1 & i24<=4)  /* Total population excluding missing information */
 replace ELEC=1 if incl==1 & (i24==1)

** Target 10, Indicator: Proportion of the population with sustainable access to an improved water source (%)
/*
17. What is the main source of drinking water for your household?
 1. Indoor tap/pipe		==> 18
 2. Outside private tap/pipe	==> 18
 3. Public standpipe		==> 22
 4. Well			==> 22
 5. River, Lake, Spring, Pond	==> 22
 6. Rainwater (tank)		==> 22
 7. Trucked water (nwc)		==> 22
 8. Bottled water 		==> 22
 9. Other			==> 22

18. Have you had a water lock-off in the last 20 days?

19. Have you a group or individual meter?

20. How much was the latest water bill for this household?

21. How many months were covered by this bill?

22. Is this supply source used by your household only, or it is shared with others? 

23. How far from this dwelling is this supply source?
	Distance (miles or yards)
*/

* Gender classification of the population refers to the head of the household.

 gen	 WATER=0 if incl==1 & (water>=1 & water<=11)   /* Total population excluding missing information */
 replace WATER=1 if incl==1 & ((water>=1 & water<=4) | water==6)

** Target 10, Indicator: Proportion of Population with Access to Improved Sanitation, Urban and Rural (%)

/*
servsani (i04)
4. What kind of toilet facilities are used by your household?
 1. W.C. linked to sewer
 2. W.C. not linked
 3. Pit
 4. Other
 5. None

usosani (i05)
5. Are the toilet facilities used only by your household, or do other household
use the facilities?
*/

* Gender classification of the population refers to the head of the household.

 gen	 SANITATION=0 if incl==1 & (servsani>=1 & servsani<=5)  /* Total population excluding missing information */
 replace SANITATION=1 if incl==1 & (servsani>=1 & servsani<=2) 

** Target 11, Indicator: Proportion of the population with access to secure tenure (%)
* Rooms excluding kitchen and bathroom

 gen persroom=(pers/nrocuart) if (pers>0 & pers<99) | (nrocuart>0 & nrocuart<99)

* Indicator components

* 1. Non secure tenure or type of dwelling.

 gen	 secten_1=0 if (tipoviv>=1 & tipoviv<=8) & (tenenviv>=1 & tenenviv<=7)   /* Total population excluding missing information */
 replace secten_1=1 if (tipoviv>=6 & tipoviv<=8) | (tenenviv>=5 & tenenviv<=7)

* 2. Low quality of the floor or walls materials.

 gen	 secten_2=0 if (paredes>=1 & paredes<=7)   /* Total population excluding missing information */
 replace secten_2=1 if (paredes==7) 

* 3. Crowding (defined as not more than two people sharing the same room)

 gen secten_3=1     if (persroom>2) 

* 4. Lack of basic services

gen secten_4=1	   if (SANITATION==0 | WATER==0)

* Gender classification of the population refers to the head of the household.

 gen	 SECTEN=1 if incl==1 & (secten_1>=0 & secten_1<=1) & (secten_2>=0 & secten_2<=1) /* Total population excluding missing information */
 replace SECTEN=0 if incl==1 & (secten_1==1 | secten_2==1 | secten_3==1 | secten_4==1)
	
* Dirt floors

* NA

** Target 18, Indicator: "Telephone lines and celullar subscribers per 100 population"

* Gender classification of the population refers to the head of the household.

 gen	 TELCEL=0 if incl==1 & (i27>=1 & i27<=2) /* Total population excluding missing information */
 replace TELCEL=1 if incl==1 & (i27==1 | i27p==1)

* Gender classification of the population refers to the head of the household.

 gen	 TEL=0 if incl==1 & (i27>=1 & i27<=2) /* Total population excluding missing information */
 replace TEL=1 if incl==1 & (i27==1)

** Target 18, Indicator: "Personal computers in use per 100 population"

 gen	 COMPUTER=0 if incl==1	/* Total population excluding missing information */
 replace COMPUTER=1 if incl==1 & j618y=="X"
 	
** CCA 41 Number of Persons per Room*

 generate PERSROOM2=persroom if relatn==1

 gen	 popinlessthan2=1 if persroom<=2
 replace popinlessthan2=0 if popinlessthan2==.

* Gender classification of the population refers to the head of the household.

 gen     PLT2=0 if incl==1 & persroom<.		/* Total population excluding missing information */
 replace PLT2=1 if incl==1 & (popinlessthan2==1)

* Primary completion rate [15 - 24 years of age]

 gen	 primINC=1 if ((cursoasi==2 | cursoasi==3 | cursoasi==5) & (gradoasi>=0 & gradoasi<=6)) | (cursoasi==14) | ((ultcurso==2 | ultcurso==3 | ultcurso==5) & (ultgrado>=0 & ultgrado<=5)) | (ultcurso==1 | ultcurso==19) | (cursoasi==17 & ultcurso==.)
 replace primINC=0 if primINC==.

 gen	 PRIMCOMP=0 if incl==1 & (edad>=15 & edad<=24) & (primINC==0 | primINC==1)
 replace PRIMCOMP=1 if incl==1 & (edad>=15 & edad<=24) & (primINC==0)
	
 
 gen categ = Q323
 gen ramap = Q39M
 gen ocupp = Q38M
 destring categ ramap ocupp, replace
 gen tamest = Q324
 destring tamest ocupp ramap, replace
 destring age, replace
 
 destring Q21 Q22 Q25 Q23 REL, replace

/*
rel
Relationship Codes
 1 Head
 2 Spouse/partner
 3 Child of head or of spouse
 4 Spouse of child
 5 Grandchild
 6 Parent of head/spouse
 7 Other relative
 8 Helper/domestic
 9 Other not relative
*/


** Economic Active Population 

 gen	 peaa=0
 replace peaa=1 if Q21==1 | (Q21==2 & Q22==2)
 replace peaa=3 if Q21>=4 | (Q25>=3 & Q25<=9)
 replace peaa=2 if (Q21==3 & Q23==2) | (Q25>=1 & Q25<=2) 

 gen	 tasadeso=0 if peaa==1
 replace tasadeso=1 if peaa==2

****************************************

** Target 4, Indicator: Share of women in wage employment in the non-agricultural sector (%)
* Without Domestic Service

 gen	 domestic=1 if (ocupp>=9130 & ocupp<=9139)
 replace domestic=0 if domestic==.

 gen	 WENAS=0 if  (edad>=15 & edad<=64) & ((categ>=1 & categ<=3) & ramap>=1000 & (domestic~=1))
 replace WENAS=1 if  (edad>=15 & edad<=64) & ((categ>=1 & categ<=3) & ramap>=1000 & (domestic~=1)) & sexo==2


** Target 4, Indicator: Share of women in wage employment in the non-agricultural sector (%)
* With Domestic Service

 gen 	 WENASD=0 if  (edad>=15 & edad<=64) & ((categ>=1 & categ<=3) & ramap>=1000 )
 replace WENASD=1 if  (edad>=15 & edad<=64) & ((categ>=1 & categ<=3) & ramap>=1000 ) & sexo==2

** GOAL 8. DEVELOP A GLOBAL PARTNERSHIP FOR DEVELOPMENT

** Target 16, Indicator: Unemployment Rate of 15 year-olds (%)

 gen	 UNMPLYMENT15=0 if  (tasadeso==0 | tasadeso==1) & (edad>=15 & edad<=24)
 replace UNMPLYMENT15=1 if  (tasadeso==1) 	        & (edad>=15 & edad<=24)
 
 
save "${surveysFolder}\ARM\JAM\2002\Arm_data\JAM2002EA_BID.dta", replace
