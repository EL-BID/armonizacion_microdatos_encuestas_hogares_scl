/*(Melisa,mmorales, May 09: 
I believe that the variable 'asiste_ci' should be changed 
-children younger than 6: survey asks enrollment not attendance => if s5p1 were used, the var should have been restricted the age*/




**********************************************************

 

//////////////////////////////////////////////////////////////////////////
*        			PANAMA ENV 2008                             
/////////////////////////////////////////////////////////////////////////*/

clear
*capture log close
*log using "${surveysFolder}\ARM\PAN\ENV_2008\Programs\pan_env_2008.log", replace


set more off

*Modificación Mayra Sáenz 10/27/2015

use "${surveysFolder}\survey\PAN\ENV\2008\m8_m11\data_merge\pan08_env.dta", clear

******************************************************************************
*	HOUSEHOLD and DEMOGRAPHIC VARIABLES
******************************************************************************

************
* Region_c *
************
*Inclusión Mayra Sáenz - Julio 2013

gen region_c=  .
label var region_c "División política"

******************************
*	factor_ci
******************************

gen factor_ci=factor
label var factor_ci "Factor de expansion del individuo"

******************************
*	idh_ch
******************************

gen idh_ch=llavehog
label var idh_ch "ID del hogar"


******************************
*	idp_ci
******************************

* numero de persona
bys llavehog: gen nper = [_n]

egen idp_ci=group(llavehog nper) 
label var idp_ci "ID de la persona en el hogar"



/*
           p-P2.Relación de |
                 parentesco |      Freq.     Percent        Cum.
         -------------------+-----------------------------------
1                      Jefe |      7,045       25.94       25.94
2                   Conyuge |      4,509       16.60       42.54
3                   Hijo(a) |     11,195       41.22       83.75
4             Yerno / Nuera |        477        1.76       85.51
5                  Nieto(a) |      2,290        8.43       93.94
6             Padre / Madre |        230        0.85       94.79
7                 Suegro(a) |         95        0.35       95.14
8                Hermano(a) |        309        1.14       96.27
9                 Cuñado(a) |        134        0.49       96.77
10            Otro pariente |        700        2.58       99.34
11 Empleado(a) doméstico(a) |         23        0.08       99.43
12    Pensionista / Huésped |          6        0.02       99.45
13         Otro no pariente |        149        0.55      100.00
               -------------+-----------------------------------
*/


******************************
*	relacion_ci
******************************
gen relacion_ci=.
replace relacion_ci=1 if p2==1
replace relacion_ci=2 if p2==2
replace relacion_ci=3 if p2==3
replace relacion_ci=4 if p2>=4 & p2<=10 
replace relacion_ci=5 if p2==13
replace relacion_ci=6 if p2==11 | p2==12
label var relacion_ci "Relacion con el jefe del hogar"
label define relacion_ci 1 "Jefe/a" 2 "Esposo/a" 3 "Hijo/a" 4 "Otros parientes" 5 "Otros no parientes" 6 "Empleado/a domestico/a"
label value relacion_ci relacion_ci


******************************
*	factor_ch
******************************
gen factorhog=(factor_ci*miembros) if relacion_ci==1
by idh_ch, sort: egen factor_ch=sum(factorhog)
/*o con comando bys*/
label var factor_ch "Factor de expansion del hogar"
drop factorhog

******************************
*	zona_c
******************************
destring area, replace

gen zona_c=0 if area==2 | area==3 /* Rural incluye: Areas Rurales e Indigenas */
replace zona_c=1 if area==1
label var zona_c "Zona del pais"
label define zona_c 1 "Urban" 0 "Rural"
label value zona_c zona_c

******************************
*	pais_c
******************************
gen str3 pais_c="PAN"
label var pais_c "Pais"
******************************
*	anio_c
******************************
gen anio_c=2008
label var anio_c "Year of the survey"
******************************
*	mes_c
******************************
gen mes_c=mes
label var mes_c "Month of the survey"
label define mes_c 1 "Jan" 2 "Feb" 3 "Mar" 4 "Apr" 5 "May" 6 "Jun" 7 "Jul" 8 "Aug" 9 "Sep" 10 "Oct" 11 "Nov" 12 "Dec"
label value mes_c mes_c


******************************
*	sexo_ci
******************************
gen sexo_ci=p3
label define sexo_ci 1 "Hombre" 2 "Mujer"
label value sexo_ci sexo_ci

******************************
*	edad_ci
******************************
gen edad_ci=p4
label var edad_ci "Edad del individuo"

******************************
*	civil_ci
******************************

/*
s3p7. Estado conyugal	
1 Unido(a)
2 Casado(a)
3 Separado(a) de matrimonio
4 Separado(a) de unión
5 Divorciado(a)	
6 Viudo(a)	
7 Soltero(a)	
*/			

gen civil_ci=.
replace civil_ci=1 if s3p7==7 
replace civil_ci=2 if s3p7==1 | s3p7==2
replace civil_ci=3 if s3p7==3 | s3p7==4 | s3p7==5
replace civil_ci=4 if s3p7==6
label var civil_ci "Estado civil"
label define civil_ci 1 "Soltero" 2 "Union formal o informal" 3 "Divorciado o separado" 4 "Viudo"
label value civil_ci civil_ci


******************************
*	jefe_ci
******************************
gen jefe_ci=(relacion_ci==1)
label var jefe_ci "Jefe de hogar"

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

******************************
*	clasehog_ch
******************************
gen clasehog_ch=.
replace clasehog_ch=1 if nhijos_ch==0 & nconyuges_ch==0 & notropari_ch==0 & notronopari_ch==0 
replace clasehog_ch=2 if nhijos_ch>0 & notropari_ch==0 & notronopari_ch==0 
replace clasehog_ch=2 if nconyuges_ch>0 & notropari_ch==0 & notronopari_ch==0 
replace clasehog_ch=3 if ((clasehog_ch ==2 & notropari_ch>0) & notronopari_ch==0) |(notropari_ch>0 & notronopari_ch==0)   
replace clasehog_ch=4 if (nconyuges_ch>0 | nhijos_ch>0 | (notropari_ch>0 & notropari_ch<.)) & (notronopari_ch>0 & notronopari_ch<.)
replace clasehog_ch=5 if nhijos_ch==0 & nconyuges_ch==0 & notropari_ch==0 & notronopari_ch>0 & notronopari_ch<.
label var clasehog_ch "Tipo de hogar"
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

by idh_ch, sort: egen nmayor65_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad>=65)
label var nmayor65_ch "Numero de familiares mayores a 65 anios"

by idh_ch, sort: egen nmenor6_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad<6)
label var nmenor6_ch "Numero de familiares menores a 6 anios"

by idh_ch, sort: egen nmenor1_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad<1)
label var nmenor1_ch "Numero de familiares menores a 1 anio"

******************************
*	miembros_ci
******************************
gen miembros_ci=(relacion_ci<5)
label var miembros_ci "Miembro del hogar"

******************************************************************************
*	LABOR MARKET
******************************************************************************

******************************
*	emp_ci
******************************

/*
s7-p1.Códig |
  o trabajó |
  la semana |
     pasada |      Freq.     Percent        Cum.
------------+-----------------------------------
1        Sí |     10,789       50.38       50.38
2        No |     10,626       49.62      100.00
------------+-----------------------------------
      Total |     21,415      100.00
*/


gen emp_ci=(s7p1b==1)

******************************
*	desemp1_ci	
******************************
gen desemp1_ci=(emp_ci==0 & s7p4==1)
replace desemp1_ci = . if emp_ci==.
label var desemp1_ci "Personas que no tienen trabajo y han buscado trabajo en el periodo de referencia"

******************************
*	desemp2_ci
******************************
gen desemp2_ci=(emp_ci==0 & (s7p4==1 | s7p6==1 | s7p6==2))
replace desemp2_ci = . if emp_ci==.
label var desemp2_ci "des1 + no trabajaron ni buscaron en la ultima semana pero esperan respuesta de solicit"

******************************
*	desemp3_ci
******************************
gen desemp3_ci=(emp_ci==0 & (s7p4==1 | s7p6==1 | s7p6==2 | s7p5==1))
replace desemp3_ci = . if emp_ci==.
label var desemp3_ci "des2 + no tienen trabajo pero buscaron antes de la semana pasada"

******************************
*	pea1_ci, pea2_ci, pea3_ci
******************************
gen pea1_ci=(emp_ci==1 | desemp1_ci==1)
gen pea2_ci=(emp_ci==1 | desemp2_ci==1)
gen pea3_ci=(emp_ci==1 | desemp3_ci==1)

******************************
*	horaspri_ci
******************************
gen horaspri_ci=s7p20 if s7p20>0 & s7p20<99
replace horaspri_ci=. if emp_ci==0
label var horaspri_ci "Hs totales (semanales) trabajadas en act. principal"

******************************
*	horastot_ci
******************************
egen horastot_ci=rsum(s7p20 s7p43) if s7p20>0 & s7p20<99
replace horastot_ci=horaspri_ci if s7p43==99 | s7p20==0
replace horastot_ci=. if (s7p20==0 & s7p43==0) | (s7p20==99 | s7p43==99)|emp_ci==0
label var horastot_ci "Hs totales (semanales)trabajadas en toda actividad"

******************************
*	desalent_ci
******************************
gen desalent_ci=(emp_ci==0 & s7p6==6)

******************************
*	subemp_ci
******************************
gen subemp_ci=. /* No se pregunta si se desea trabajar mas horas */
* (emp_ci==1 & p50==1 & horastot_ci<=30)
replace subemp_ci=. if emp_ci==0

******************************
*	tiempoparc_ci
******************************
gen tiempoparc_ci= .
* (p50==2 & horastot_ci<30)
replace tiempoparc_ci=. if emp_ci==0
label var tiempoparc_ci "Trabajan menos de 30 hs semanales y no quieren trabajar mas"


******************************************************************************
*		LABOR DEMAND
******************************************************************************

******************************
*	ocupa_ci
******************************
/*
s7-p14.Código de ocupación del primer trabajo (CIUO–88)
11 Miembros del poder ejecutivo y legislat	
12 Directores de empresas	
13 Gerentes de empresas	
21 Profesionales de las ciencias físicas,	
22 Profesionales de las ciencias biológica	
23 Profesionales de la enseñanza	
24 Otros profesionales científicos e intel	
31 Técnicos y profesionales de nivel medio	
32 Técnicos y profesionales de nivel medio	
33 Maestros e instructores de nivel medio	
34 Otros técnicos y profesionales de nivel	
41 Oficinistas	
42 Empleados en trato directo con el publi	
43 Jefes y supervisores en actividades adm	
51 Trabajadores de los servicios personale	
52 Modelos, vendedores, demostradores y oc	
61 Trabajadores de explotaciones agropecua	
62 Capataces de explotaciones agropecuaria	
71 Trabajadores de las industrias extracti	
72 Trabajadores de la industria metalúrgic	
73 Mecánicos de precisión, ceramistas y ar	
74 Trabajadores de la industria de la alim	
75 Jefes y encargados en las industrias ex	
81 Operadores de instalaciones fijas y afi	
82 Operadores de maquinas y montadores	
83 Conductores de vehículos y operadores d	
91 Vendedores ambulantes y trabajadores de	
92 Obreros y jornaleros de la minería, la	
*/			

gen labor = s7p14
gen ocupa_ci=.
replace ocupa_ci=1 if labor>=20 & labor<=39
replace ocupa_ci=2 if labor>=10 & labor<=19
replace ocupa_ci=3 if labor>=40 & labor<=49
replace ocupa_ci=4 if labor>=52 & labor<=59
replace ocupa_ci=5 if labor>=50 & labor<=51
replace ocupa_ci=6 if labor>=60 & labor<=69
replace ocupa_ci=7 if labor>=70 & labor<=89
replace ocupa_ci=8 if labor>=0 & labor<=9
replace ocupa_ci=9 if (labor>=90 & labor<=99) | labor==99
drop labor 

label var ocupa_ci "Ocupacion Laboral en la Actividad Principal"
label define ocupa_ci 1 "PROFESIONALES Y TECNICOS" 2 "GERENTES, DIRECTORES Y FUNCIONARIOS SUPERIORES"  3 "PERSONAL ADMINISTRATIVO Y NIVEL INTERMEDIO" 4 "COMERCIANTES Y VENDEDORES" 5 "TRABAJADORES EN SERVICIOS" 6 "TRABAJADORES AGRICOLAS Y AFINES" 7 "OBREROS NO AGRICOLAS, CONDUCTORES DE MAQUINAS Y VEHICULOS DE TRANSPORTE Y SIMILARES" 8 "FUERZAS ARMADAS" 9 "OTRAS OCUPACIONES NO CLASIFICADAS ANTERIORMENTE"
label values ocupa_ci ocupa_ci

******************************
*	rama_ci
******************************

/*
1	Agricultura, ganadería, caza y activida
2	Silvicultura, Extracción de madera y ac
5	Pesca, acuicultura, explotación de cria
13	Extracción de minerales metalíferos
14	Explotación de otras minas y canteras
15	Elaboración de productos alimenticios y
17	Fabricación de productos textiles
18	Fabricación de prendas de vestir; adobo
19	Curtido y adobo de cueros; fabricación
20	Producción de madera y fabricación de p
21	Fabricación de papel y productos de pap
22	Actividades de edición e impresión y de
23	Fabricación de coque, productos de la r
24	Fabricación de sustancias y productos q
25	Fabricación de productos de caucho y de
26	Fabricación de otros productos minerale
27	Fabricación de metales comunes
28	Fabricación de productos elaborados de
29	Fabricación de maquinaria y equipo, n.c
31	Fabricación de maquinaria y aparatos el
32	Fabricación de equipo y aparatos de rad
33	Fabricación de instrumentos médicos, óp
34	Fabricación de vehículos automotores; r
35	Fabricación de otros tipos de equipo de
36	Fabricación de muebles; industrias manu
37	Reciclamiento
40	Suministro de electricidad, gas y agua
41	Captación, depuración y distribución de
45	Construcción
51	Comercio al por mayor y en comisión
52	Comercio al por menor; reparación de ef
53	Comercio al por mayor en zonas francas
55	Hoteles y Restaurantes
60	Transporte por vía terrestre; transport
61	Transporte por vía acuática
62	Transporte por vía aérea
63	Actividades de transporte complementari
64	Correo y telecomunicaciones
65	Intermediación Financiera, excepto la f
66	Financiamiento de planes de seguro de p
67	Actividades auxiliares de la intermedia
70	Actividades inmobiliarias y asesoramien
71	Alquiler de maquinaria y equipo sin ope
72	Informática y actividades conexas
73	Investigación  y desarrollo
74	Otras actividades empresariales
75	Administración pública y defensa; plane
80	Enseñanza
85	Actividades de servicios sociales y de
90	Eliminación de desperdicios y de aguas
91	Actividades de asociaciones, n.c.p.
92	Actividades de esparcimiento y activida
93	Otras actividades de servicio
95	Hogares privados con servicio doméstico
98	Organizaciones y organos extraterritori
*/

gen rama = s7p16
gen rama_ci=0 if emp_ci==1
replace rama_ci=1 if rama>=1 & rama<=5 
replace rama_ci=2 if rama>=13 & rama<=14
replace rama_ci=3 if rama>=15 & rama<=37 
replace rama_ci=4 if rama>=40 & rama<=41 
replace rama_ci=5 if rama>=45 & rama<=45 
replace rama_ci=6 if rama>=51 & rama<=55 
replace rama_ci=7 if rama>=60 & rama<=64 
replace rama_ci=8 if rama>=65 & rama<=74 
replace rama_ci=9 if rama>=75 & rama<=99
drop rama

label var rama_ci "Rama actividad principal"
label define rama_ci 1 "Agricultura, caza, silvicultura y pesca" 2 "Explotación de minas y canteras" 3 "Industrias manufactureras" 4 "Electricidad, gas y agua" 5 "Construcción" 6 "Comercio al por mayor y menor, restaurantes, hoteles" 7 "Transporte y almacenamiento" 8 "Establecimientos financieros, seguros, bienes inmuebles" 9 "Servicios sociales, comunales y personales"
label values rama_ci rama_ci


******************************
*	categopri_ci
******************************




gen categopri_ci=0 if emp_ci==1 & emp_ci==1
replace categopri_ci=1 if s7p23==8 & emp_ci==1
replace categopri_ci=2 if (s7p23==7 | s7p23==9) & emp_ci==1
replace categopri_ci=3 if  (s7p23>=1 & s7p23<=6)  & emp_ci==1

replace categopri_ci=4 if (s7p23==10 | s7p23==11) & emp_ci==1
*MLO= inclui condicion que sea ocupado
label var categopri_ci "Categoria ocupacional en la actividad principal"
label define categopri_ci 1 "Patron" 2 "Cuenta Propia" 3 "Empleado" 4 "Trabajador no remunerado"
label value categopri_ci categopri_ci

******************************
*	categosec_ci
******************************
gen categosec_ci=0 if emp_ci==1
replace categosec_ci=1 if s7p45==8
replace categosec_ci=2 if s7p45==7 | s7p45==9
replace categosec_ci=3 if s7p45>=1 & s7p45<=6 

label define categosec_ci 1 "Patron" 2 "Cuenta Propia" 3 "Empleado" 4 "Familiar no remunerado" 
label value categosec_ci categosec_ci
label var categosec_ci "Categoria ocupacional en la actividad secundaria"

******************************
*	contrato_ci
******************************
gen contrato_ci=0 if s7p25==2
replace contrato_ci=1 if s7p25==1 
label var contrato_ci "Persona empleada que firmo contrato de trabajo"

******************************
*	segsoc_ci
******************************
gen segsoc_ci=.
replace segsoc_ci = 1 if s7p22a == 1 | s7p44a ==1
replace segsoc_ci = 0 if s7p22a == 2 & s7p44a !=1


******************************
*	nempleos_ci
******************************
gen nempleos_ci=0 if emp_ci==1
replace nempleos_ci=1 if s7p38==2 | s7p51==2
replace nempleos_ci=2 if s7p38==1 | s7p51==1

******************************
*	firmapeq_ci
******************************
gen firmapeq_ci=0 if emp_ci==1
replace firmapeq_ci=1 if s7p21<=3

label var firmapeq_ci "1=5 o menos trabajadores"
******************************
*	spublico_ci
******************************
gen spublico_ci=0 if emp_ci==1
replace spublico_ci=1 if s7p23==1
/* no se incluye empleados del canal de Panama */

label var spublico_ci "Trabaja en sector publico"

******************************
*	durades_ci
******************************
* Revisar, no se si esta en dias o meses

gen durades_ci=0 if emp_ci==0
replace durades_ci=s7p7
label var durades_ci "Duracion del desempleo"
******************************
*	antiguedad_ci
******************************
gen m=s7p18 
gen a=s7p17 

gen antiguedad_ci=.
replace antiguedad_ci=s7p17
replace antiguedad_ci=m/12 if a==0
drop a m

****************************************************

******************************
*	ylmpri_ci & ylmpri1_ci
******************************

/* frecuencia ingreso independientes (yindep):
0	0
1	Día
2	Semana
3	Quincena
4	Mes
5	Trimestre
6	Semestre
7	Cosecha
8	Año
9	Otra
99	99
*/

gen yindep = .
replace yindep = s7p24_a*30 if s7p24_b==1
replace yindep = s7p24_a*4.3 if s7p24_b==2
replace yindep = s7p24_a*2 if s7p24_b==3
replace yindep = s7p24_a if s7p24_b==4
replace yindep = s7p24_a/3 if s7p24_b==5
replace yindep = s7p24_a/6 if s7p24_b==6 | s7p24_b==7
* suponemos 2 cosechas en el anio
replace yindep = s7p24_a/12 if s7p24_b==8
replace yindep = . if s7p24_b == 0 | s7p24_b== 9 | s7p24_b==99
replace yindep = . if yindep>30000 /*sacamos los OUTLIERS */


gen ysalario = .
replace ysalario = s7p31
replace ysalario = . if ysalario== 0
replace ysalario = . if ysalario>11500 /*sacamos los OUTLIERS */

gen yaguinaldo = .
replace yaguinaldo = s7p29b/12 

gen ypropina = .
replace ypropina = s7p30b

egen ylmpri_ci=rsum(yindep ysalario ypropina)
replace ylmpri_ci=. if emp_ci==0
replace ylmpri_ci=. if  yindep==. & ysalario==. & yaguinaldo==. & ypropina==.
label var ylmpri_ci "Ingreso laboral monetario act. principal (mes)"

egen ylmpri1_ci=rsum(ylmpri_ci yaguinaldo)
replace ylmpri1_ci=. if ylmpri_ci==. & yaguin==.
replace ylmpri_ci=. if  yindep==. & ysalario==. & yaguinaldo==. & ypropina==.
replace ylmpri1_ci=. if emp_ci==0

******************************
*	nrylmpri_ci & nrylmpri1_ci
******************************
gen nrylmpri_ci=. 
label var nrylmpri_ci "Identificador de NR de ingreso"

******************************
*	ylmsec_ci
******************************

gen yindep2 = .
replace yindep2 = s7p46a*30 if s7p46b==1
replace yindep2 = s7p46a*4.3 if s7p46b==2
replace yindep2 = s7p46a*2 if s7p46b==3
replace yindep2 = s7p46a if s7p46b==4
replace yindep2 = s7p46a/3 if s7p46b==5
replace yindep2 = s7p46a/6 if s7p46b==6 | s7p46b==7
* suponemos 2 cosechas en el anio
replace yindep2 = s7p46a/12 if s7p46b==8
replace yindep2 = . if s7p46b == 0 | s7p46b== 9 | s7p46b==99
replace yindep2 = . if yindep2>30000 /*sacamos los OUTLIERS */


gen ysalario2 = .
replace ysalario2 = s7p48
replace ysalario2 = . if ysalario2== 0

gen yaguinaldo2 = .
replace yaguinaldo2 = s7p47b/12 

gen ypropina2 = .
replace ypropina2 = s7p49b

egen ylmsec_ci=rsum(yindep2 ysalario2 yaguinaldo2 ypropina2)
replace ylmsec_ci=. if emp_ci==0
replace ylmsec_ci=. if  yindep2==. & ysalario2==. & yaguinaldo2==. & ypropina2==.
label var ylmsec_ci "Ingreso laboral monetario act secundaria (mes)"

**** otras actividades

gen yindep3 = .
replace yindep3 = s7p57a*30 if s7p57b==1
replace yindep3 = s7p57a*4.3 if s7p57b==2
replace yindep3 = s7p57a*2 if s7p57b==3
replace yindep3 = s7p57a if s7p57b==4
replace yindep3 = s7p57a/3 if s7p57b==5
replace yindep3 = s7p57a/6 if s7p57b==6 | s7p57b==7
* suponemos 2 cosechas en el anio
replace yindep3 = s7p57a/12 if s7p57b==8
replace yindep3 = . if s7p57b == 0 | s7p57b== 9 | s7p57b==99

gen ysalario3 = .
replace ysalario3 = s7p59
replace ysalario3 = . if ysalario3== 0
replace ysalario3 = . if ysalario3>4000

gen yaguinaldo3 = .
replace yaguinaldo3 = s7p58b/12 

gen ypropina3 = .
replace ypropina3 = s7p60b
replace ypropina3 = . if ypropina2>9999

egen ylmotr_ci=rsum(yindep3 ysalario3 yaguinaldo3 ypropina3)
replace ylmotr_ci=. if emp_ci==0
replace ylmotr_ci=. if  yindep3==. & ysalario3==. & yaguinaldo3==. & ypropina3==.
label var ylmotr_ci "Ingreso laboral monetario act secundaria (mes)"



******************************
*	ylm_ci & ylm1_ci
******************************
egen ylm_ci=rsum(ylmpri_ci ylmsec_ci ylmotr_ci)
replace ylm_ci=. if ylmpri_ci==. & ylmsec_ci==. & ylmotr_ci==.
label var ylm_ci "Ingreso laboral monetario total"
replace ylm_ci=. if emp_ci==0


egen ylm1_ci= rsum(ylmpri1_ci ylmsec_ci ylmotr_ci)
replace ylm1_ci=. if ylmpri1_ci==. & ylmsec_ci==. & ylmotr_ci==.
replace ylm1_ci=. if emp_ci==0

******************************
*	ylnm_ci
******************************

gen alim1 = s7p32b
gen alim2 = s7p50b
gen alim3 = s7p61b

gen vivi = s7p33b
replace vivi = . if vivi > 9999

gen vesti = s7p34b
replace vesti = . if vesti > 9999

gen transpo = s7p35b
replace transpo = . if transpo> 9999

egen ylnmpri_ci= rsum(alim1 alim2 alim3 vivi vesti transpo)
replace ylnmpri_ci=. if emp_ci==0
replace ylnmpri_ci=. if (alim1 == . & alim2==. & alim3==. & vivi ==. & transpo ==. & vesti == .)

gen ylnm_ci=ylnmpri_ci
label var ylnm_ci "Ingreso laboral no monetario total"

******************************
*	ynlm_ci
******************************
gen pensionali = s7p62b
gen pensionorf = s7p63b

gen jub	       = s7p64b
replace jub = . if jub> 9999

egen ayuda     =  rsum(s7p67a s7p67b s7p67c s7p67d s7p67e s7p67f)
replace ayuda = . if (s7p67a==. & s7p67b==. & s7p67c==. & s7p67d==. & s7p67e==. & s7p67f==.)
replace ayuda = . if ayuda> 9999


egen ynlm_ci=rsum(pensionali pensionorf jub ayuda)
replace ynlm_ci = . if (pensionali==. & pensionorf==. & jub==. & ayuda==.)
label var ynlm_ci "Ingreso no laboral monetario(mes)"

gen ynlnm_ci = .

******************************
*	nrylmpri_ch
******************************
gen nrylmpri_ch=.
label var nrylmpri_ch "Identificador de hogares donde miembro NS/NR ingreso"

******************************
*	ylm_ch & ylm1_ch 
******************************
by idh_ch, sort: egen ylm_ch=sum(ylm_ci) if miembros_ci==1
by idh_ch, sort: egen ylm1_ch=sum(ylm1_ci) if miembros_ci==1
label var ylm_ch "Ingreso laboral monetario del Hogar-ignora NR"

****************************
*    ylmnr_ch & ylmnr1_ch  
****************************
by idh_ch, sort: egen ylmnr_ch=sum(ylm_ci) if miembros_ci==1 & nrylmpri_ch==0

by idh_ch, sort: egen ylmnr1_ch=sum(ylm1_ci) if miembros_ci==1 & nrylmpri_ch==0
*replace ylmnr1_ch=. if nrylmpri1_ch==1

label var ylmnr_ch "Ing laboral monetario del Hogar"

******************************
*	ylnm_ch  
******************************
by idh_ch, sort: egen ylnm_ch=sum(ylnm_ci) if miembros_ci==1
label var ylnm_ch "Ing laboral no monetario del Hogar - ignora NR" 

******************************
*	remesas_ci & remesas_ch 
******************************
gen remesas_ci=.
gen remesas_ch=.

******************************
*	ynlm_ch 
******************************
by idh_ch, sort: egen ynlm_ch=sum(ynlm_ci) if miembros_ci
label var ynlm_ch "Ingreso no laboral monetario del Hogar" 

******************************
*	autocons_ci 
******************************
gen autocons_ci=.
label var autocons_ci "Autoconsumo reportado por el individuo"

******************************
*	autocons_ch 
******************************
gen autocons_ch=.

******************************
*	rentaimp_ch 
******************************
gen rentaimp_ch=.

******************************
*	ylmhopri_ci & ylmhopri1_ci
******************************
gen ylmhopri_ci=ylmpri_ci/(horaspri_ci*4.3)
label var ylmhopri_ci "Salario monetario de la actividad principal"

gen ylmhopri1_ci=ylmpri1_ci/(horaspri_ci*4.3)

******************************
*	ylmho_ci & ylm1ho_ci
******************************
gen ylmho_ci=ylm_ci/(horastot_ci*4.3)
label var ylmho_ci "Salario monetario de todas las actividades"

gen ylmho1_ci=ylm1_ci/(horastot_ci*4.3)



******************************************************************************
*	VARIABLES OF HOUSEHOLD INFRAESTRUCTURE 
******************************************************************************

/*
1	Acueducto público
2	Acueducto de la comunidad
3	Acueducto particular
4	Pozo sanitario
5	Pozo brocal no protegido
6	vertiente, quebrada, lluvia
7	Otro
*/

gen aguared_ch=.
replace aguared_ch = 1 if s1b19>=1 & s1b19<=3
replace aguared_ch = 0 if s1b19>=4

/*
1	Dentro de la vivienda
2	Solo en el patio de la vivienda
3	Dentro de la vivienda y el patio
4	Fuera de la vivienda y del patio
*/

gen aguadist_ch=.
replace aguadist_ch = 1 if s1b22==1 | s1b22==3
replace aguadist_ch = 2 if s1b22==2
replace aguadist_ch = 3 if s1b22==4

gen aguamala_ch=.
replace aguamala_ch = 0 if s1b19>=1 & s1b19<=3
replace aguamala_ch = 1 if s1b19>=4 & s1b19<=6

gen aguamide_ch=.

/*
1	Eléctricidad de ELEKTRA O EDEMET EDECHI
2	Eléctrico de la Comunidad
3	Eléctricidad del Municipio
4	Electricidad propia
5	Electricidad de particulares
6	Querosin o diesel, gas
7	Otro
*/

gen luz_ch=.
replace luz_ch = 1 if s1b35>=1 & s1b35<=5
replace luz_ch = 0 if s1b35>=6

gen luzmide_ch=. /* revisar */
replace luzmide_ch = 1 if s1b36b > 1 & s1b36b < 9915 
replace luzmide_ch = 0 if s1b36b == 9998 | s1b36b == 0


/*
1	Gas
2	Leña
3	Electricidad
4	No cocina
5	Otro
*/

gen combust_ch=.
replace combust_ch = 1 if s1b38 ==1 | s1b38== 3
replace combust_ch = 0 if s1b38 == 2 | s1b38 >= 4

/*
1	Conectado a alcantarillado sanitario
2	Conectado a tanque séptico
3	De hueco o letrina
4	No tiene
*/

gen bano_ch=.
replace bano_ch = 1 if s1b29 <= 3
replace bano_ch = 0 if s1b29 == 4

gen banoex_ch=.
replace banoex_ch = 1 if s1b32 == 1
replace banoex_ch = 0 if s1b32 == 2 | s1b32 == 3

gen des1_ch=.
replace des1_ch = 1 if s1b29 == 1 | s1b29 == 2 
replace des1_ch = 2 if s1b29 == 3 
replace des1_ch = 0 if s1b29 == 4 

gen des2_ch=.
replace des1_ch = 1 if s1b29 == 1 | s1b29 == 2 | s1b29 == 3  
replace des1_ch = 0 if s1b29 == 4 

/*
1	Concreto / cemento
2	Mosaico, ladrillo, granito, mármol
3	Madera
4	Tierra / arena
5	Otros materiales
*/

gen piso_ch=.
replace piso_ch = 1 if s1a4>=1 & s1a4<= 3
replace piso_ch = 2 if s1a4==5
replace piso_ch = 0 if s1a4==4

/*
1	Bloque, ladrillo, ...
2	Madera
3	Quincha / adobe
4	Metal
5	Caña, paja, penca, palos
6	Sin paredes
7	Otros materiales
*/

gen pared_ch=.
replace pared_ch = 1 if s1a2 >= 1 & s1a2 <= 4
replace pared_ch = 2 if s1a2 == 7
replace pared_ch = 0 if s1a2 == 5 | s1a2 == 6 


/*
1	Concreto / cemento
2	Teja
3	Fibra-cemento
4	Metal
5	Madera
6	Paja o penca
7	Otros materiales
*/

gen techo_ch=.
replace techo_ch = 1 if s1a3 >= 1 & s1a3 <=5
replace techo_ch = 0 if s1a3 == 6 
replace techo_ch = 2 if s1a3 == 7

/*
1	Servicio de vehículos o carro del Munic
2	Servicio de vehículos particulares
3	La botan a otros lotes
4	La botan / tiran dentro del patio
5	La botan / tiran al río / quebrada / ma
6	La queman
7	La entierran
8	Otro
*/

gen resid_ch=. 
replace resid_ch = 0 if s1b33 == 1 | s1b33 == 2
replace resid_ch = 1 if s1b33 == 6 | s1b33 == 7
replace resid_ch = 2 if s1b33 == 3 | s1b33 == 4 | s1b33 == 5
replace resid_ch = 3 if s1b33 == 8

**Daniela Zuluaga- Enero 2018: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**
	
 *********************
 ***aguamejorada_ch***
 *********************
g       aguamejorada_ch = 1 if (s1b19 >=1 & s1b19 <=4)
replace aguamejorada_ch = 0 if (s1b19 >=5 & s1b19 <=7)
		
 *********************
 ***banomejorado_ch***
 *********************
g       banomejorado_ch = 1 if ((s1b29  >=1 &  s1b29 <=2) & s1b32 == 1)
replace banomejorado_ch = 0 if ((s1b29  >=1 &  s1b29 <=2) & (s1b32  >=2 &  s1b32 <=3)) | (s1b29  >=3 &  s1b29 <=4)

gen dorm_ch=.
gen cuartos_ch=.
gen cocina_ch=.

gen telef_ch=.
replace telef_ch = 1 if  s1b40aa == 1
replace telef_ch = 0 if  s1b40aa == 2

gen refrig_ch=.
gen freez_ch=.
gen auto_ch=.
gen compu_ch=.

gen internet_ch=.
replace internet_ch = 1 if s1b40da == 1
replace internet_ch = 0 if s1b40da == 2

gen cel_ch=.
replace internet_ch = 1 if s1b40ba == 1
replace internet_ch = 0 if s1b40ba == 2

/*
1	Casa Individual
2	Choza o Rancho
3	Apartamento
4	Cuarto en Casa de Vecindad
5	Improvisada
6	Otro
*/

gen vivi1_ch=.
replace vivi1_ch = 1 if s1a1 == 1
replace vivi1_ch = 2 if s1a1 == 3
replace vivi1_ch = 3 if s1a1 == 6 | s1a1 == 2 | s1a1 == 4 | s1a1 == 5

gen vivi2_ch=.
replace vivi2_ch = 1 if s1a1 == 1 | s1a1 == 3
replace vivi2_ch = 0 if s1a1 == 2 | s1a1 >= 4

/*
1	Propia totalmente pagada
2	Propia hipotecada
3	Alquilada
4	Cedida o prestada
5	Ocupantes de hecho
*/

gen viviprop_ch=.
replace viviprop_ch = 0 if s1a5 == 3
replace viviprop_ch = 1 if s1a5 == 1
replace viviprop_ch = 2 if s1a5 == 2
replace viviprop_ch = 3 if s1a5 >= 4

/*
1	Ninguno o no tienen
2	Derecho posesorio
3	Recibo o factura
4	Escritura sin registrar
5	Escritura en trámite
6	Escritura o título registrado
*/

gen vivitit_ch=.
replace vivitit_ch = 1 if s1a8>=4
replace vivitit_ch = 0 if s1a8<=3

gen vivialq_ch=.
replace vivialq_ch = s1a5a if s1a5 == 3
replace vivialq_ch = . if s1a5a==99999.99 

gen vivialqimp_ch=.
replace vivialqimp_ch = s1a6
replace vivialqimp_ch = . if s1a6==99999.99 | s1a6==9999.99 

******************************************************************************
*	EDUCATION
******************************************************************************

/*
Corregida!
*/

******************************
*	aedu_ci
******************************
gen nivel = s5p45a
gen grado = s5p45b

gen aedu_ci=0 if nivel==0 | nivel==1 
replace aedu_ci=grado if nivel==2 
replace aedu_ci=grado+6 if nivel==3 | nivel==4
replace aedu_ci=grado+12 if nivel==6 | nivel==7
replace aedu_ci=grado+17 if nivel==8 
replace aedu_ci=grado+9 if nivel==5 

******************************
*	eduno_ci
******************************
gen eduno_ci=(aedu_ci==0)
replace eduno_ci=. if aedu_ci==.
label var eduno_ci "Personas sin educacion"

******************************
*	edupi_ci
******************************
gen edupi_ci=(aedu_ci>=1 & aedu_ci<6)
replace edupi_ci=. if aedu_ci==.
label var edupi_ci "Personas que no han completado Primaria"

******************************
*	edupc_ci
******************************
gen edupc_ci=(aedu_ci==6)
replace edupc_ci=. if aedu_ci==.
label var edupc_ci "Primaria Completa"

******************************
*	edusi_ci
******************************
gen edusi_ci=(aedu_ci>6 & aedu_ci<12)
replace edupc_ci=. if aedu_ci==.
label var edusi_ci "Secundaria Incompleta"

******************************
*	edusc_ci
******************************
gen edusc_ci=(aedu_ci==12)
replace edusc_ci=. if aedu_ci==.
label var edusc_ci "Secundaria Completa"

******************************
*	edus1i_ci
******************************
gen edus1i_ci=(aedu_ci>6 & aedu_ci<9)
replace edus1i_ci=. if aedu_ci==.
label var edus1i_ci "1er ciclo de Educacion Secundaria Incompleto"

******************************
*	edus1c_ci
******************************
gen edus1c_ci=(aedu_ci==9)
replace edus1c_ci=. if aedu_ci==.
label var edus1c_ci "1er ciclo de Educacion Secundaria Completo"

******************************
*	edus2i_ci
******************************
gen edus2i_ci=(aedu_ci>9 & aedu_ci<12)
replace edus2i_ci=. if aedu_ci==.
label var edus2i_ci "2do ciclo de Educacion Secundaria Incompleto"

******************************
*	edus2c_ci
******************************
gen edus2c_ci=(aedu_ci==12)
replace edus2c_ci=. if aedu_ci==.
label var edus2c_ci "2do ciclo de Educacion Secundaria Completo"
*pongo primaria y secundaria, como equivalente a basica y media

******************************
*	eduui_ci
******************************
gen eduui_ci=(aedu_ci>12 & aedu_ci<17) 
replace eduui_ci=. if aedu_ci==.
label var eduui_ci "Universitaria o Terciaria Incompleta"

******************************
*	eduuc_ci
******************************
gen eduuc_ci=(aedu_ci>=17)
replace eduuc_ci=. if aedu_ci==.
label var eduuc_ci "Universitaria o Terciaria Completa"

******************************
*	edupre_ci
******************************
gen edupre_ci=.
replace edupre_ci =1 if nivel==1 | nivel==2 | s5p17 == 1
replace edupre_ci = 0 if s5p17 == 2
label var edupre_ci "Recibe o Recibio Educacion preescolar"

******************************
*	eduac_ci
******************************
gen eduac_ci=.
replace eduac_ci=0 if nivel==8
replace eduac_ci=1 if nivel==7
label var eduac_ci "Educ terciaria academica vs Educ terciaria no academica"

******************************
*	asiste_ci
******************************
gen asiste_ci=(s5p19==1)
replace asiste_ci=. if s5p19==.
label var asiste "Personas que actualmente asisten o estan matriculados a centros de enseñanza"

/*(Melisa,mmorales, May 09: 
I believe that the variable 'asiste_ci' should be changed 
-children younger than 6: survey asks enrollment not attendance => if s5p1 were used, the var should have been restricted the age*/






******************************
*	pqnoasis_ci
******************************
/*
1	Edad
2	Falta de dinero
3	Trabajo
4	Labores domésticas
5	Terminó estudios
6	No le interesa
7	Enfermedad
8	Distancia / transporte
9	Embarazo
10	Falta de cupo
11	Requiere plantel especial
12	Tenía que repetir
13	No hay centro educativo
14	Otro
*/

gen pqnoasis_ci=s5p20 if s5p20>0

**************
*pqnoasis1_ci*
**************
**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**

g       pqnoasis1_ci = 1 if s5p20==2
replace pqnoasis1_ci = 2 if s5p20==3
replace pqnoasis1_ci = 3 if s5p20==7 
replace pqnoasis1_ci = 4 if s5p20==6
replace pqnoasis1_ci = 5 if s5p20==4 | s5p20==9
replace pqnoasis1_ci = 6 if s5p20==5 
replace pqnoasis1_ci = 7 if s5p20==1 
replace pqnoasis1_ci = 8 if s5p20==8 | s5p20==13
replace pqnoasis1_ci = 9 if s5p20==10  | s5p20==11 | s5p20==12  | s5p20==14

label define pqnoasis1_ci 1 "Problemas económicos" 2 "Por trabajo" 3 "Problemas familiares o de salud" 4 "Falta de interés" 5	"Quehaceres domésticos/embarazo/cuidado de niños/as" 6 "Terminó sus estudios" 7	"Edad" 8 "Problemas de acceso"  9 "Otros"
label value  pqnoasis1_ci pqnoasis1_ci

******************************
*	repiteult_ci  & repite_ci
******************************
gen repiteult_ci=.

gen repite_ci=.
*NA
drop nivel grado

sum  yindep -  ynlm_ci

******************
* RAZA
*****************
*Modificación Mayra Sáenz 10/20/2015: modificaciones realizadas en base a metodología enviada por SCL/GDI Maria Olga Peña

encode s5p13_o, gen(s5p13_2)

gen raza_ci = .

gen raza_idioma_ci=.
replace raza_idioma_ci= 1 if  s5p13 ==2 | s5p13 ==3 | s5p13 ==4 | s5p13 ==5 | s5p13 ==6 ///
| s5p13 ==7 | s5p13 ==8 
replace raza_idioma_ci= 1 if s5p14 ==2 | s5p14 ==3 | s5p14 ==4 | s5p14 ==5 | s5p14 ==6 ///
| s5p14 ==7 | s5p14 ==8 
by idh_ch, sort: gen aux=raza_idioma_ci if p2_paren==1
by idh_ch, sort: egen aux1 = max(aux)
replace raza_idioma_ci=aux1 if (raza_idioma_ci ==. & (p2_paren ==3 | p2_paren==5))  
replace raza_idioma_ci=3 if raza_idioma_ci==. 
drop aux aux1
label define raza_idioma_ci 1 "Indígena" 2 "Afro-descendiente" 3 "Otros" 
label value raza_idioma_ci raza_idioma_ci 
label value raza_idioma_ci raza_idioma_ci
label var raza_idioma_ci "Raza o etnia del individuo" 

gen id_ind_ci = 0
replace id_ind_ci=1 if raza_idioma_ci==1
label define id_ind_ci 1 "Indígena" 0 "Otros" 
label value id_ind_ci id_ind_ci 
label var id_ind_ci "Indigena" 

gen id_afro_ci = 0
label define id_afro_ci 1 "Afro-descendiente" 0 "Otros" 
label value id_afro_ci id_afro_ci 
label var id_afro_ci "Afro-descendiente" 

g region_BID_c=.
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
g tipocontrato_ci=.
g pensionsub_ci=.
g pension_ci=.
g tipopen_ci=.
g instpen_ci=.
g  tcylmpri_ci=.
g  ylnmsec_ci =.
g ylmotros_ci=.
g ylnmotros_ci=.
g ynlnm_ch=.
g tcylmpri_ch=.
g ypen_ci=.
g ypensub_ci=.
g salmm_ci=.
g lp_ci=.
g lpe_ci=.
g edupub_ci=.
g  tecnica_ci=.

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



saveold "${surveysFolder}\harmonized\PAN\ENV\data_arm\PAN2008m8_m11_BID.dta", replace


