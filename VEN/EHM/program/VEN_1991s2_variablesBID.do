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

local PAIS VEN
local ENCUESTA EHM
local ANO "1991"
local ronda s2 
local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
                        
capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: 
Encuesta: EHM
Round: s2
Autores:Mayra Sáenz - saenzmayra.a@gmail.com - mayras@iadb.org - Diciembre 2013 
Versión 2006: Victoria
Generación nuevas variables LMK: Yessenia Loayza (desloay@hotmail.com | yessenial@iadb.org)
Última versión: Yessenia Loayza - Email: desloay@hotmail.com | yessenial@iadb.org
Fecha última modificación: octubre 2013

							SCL/LMK - IADB
****************************************************************************/
****************************************************************************/
use `base_in', clear
cap destring _all, replace

************
* Region_c *
************
* YL: En este año se considera la antigua division política administrativa (Que existía antes del 2001)
gen region_c=  entidad
label define region_c  ///
1	"Distrito Federal" ///
2	"Anzoategui" ///
3	"Apure" ///
4	"Aragua" ///
5	"Barinas" ///
6	"Bolivar" ///
7	"Carabobo" ///
8	"Cojedes" ///
9	"Falcon" ///
10	"Guarico" ///
11	"Lara" ///
12	"Merida" ///
13	"Miranda" ///
14	"Monagas" ///
15	"Nueva Esparta" ///
16	"Portuguesa" ///
17	"Sucre" ///
18	"Tachira" ///
19	"Trujillo" ///
20	"Yaracuy" ///
21	"Zulia" ///
22	"Amazonas" ///
23	"Delta Amacuro"
label value region_c region_c
label var region_c " Primera División política - Entidades Federativas"

************************
*** region según BID ***
************************
gen region_BID_c=3 
label var region_BID_c "Regiones BID"
label define region_BID_c 1 "Centroamérica_(CID)" 2 "Caribe_(CCB)" 3 "Andinos_(CAN)" 4 "Cono_Sur_(CSC)"
label value region_BID_c region_BID_c

************
****pais****
************
gen str pais_c="VEN"

***************
****anio_c ****
***************
gen anio_c=1991

gen mes_c=.
/* No se cuenta con informacion especifica sobre la semana de planificacion para esta encuesta */
/*replace mes_c= 7 if sem_lev>=1 & sem_lev<=2
replace mes_c= 8 if sem_lev>=3 & sem_lev<=6
replace mes_c= 9 if sem_lev>=7 & sem_lev<=10
replace mes_c= 10 if sem_lev>=11 & sem_lev<=14
replace mes_c= 11 if sem_lev>=15 & sem_lev<=18
replace mes_c= 12 if sem_lev>=19 & sem_lev<=22
*** average week of the survey is 11.05
replace mes_c=10  if mes_c==.
label variable mes_c "Mes de la Encuesta: Segundo Semestre de 1993"  
label define mes_c 7 "JUL" 8 "AUG" 9 "SEP" 10 "OCT" 11 "NOV" 12 "DEC" 
label values mes_c mes_c*/

gen zona_c=.
replace zona_c=1 if area==1 | area==2
replace zona_c=0 if area==3
label define zona_c 0 "Rural" 1 "Urbana"  
label value zona_c zona_c

egen idh_ch=group(entidad nserie rotacion fecha ncontrol localida entrev trh area)
label var idh_ch "Identificador Unico del Hogar"

gen idp_ci=linea
label var idp_ci "Identificador Individual dentro del Hogar"

gen factor_ch=.
label var factor_ch "Factor de expansion del Hogar"

gen relacion_ci=.
replace relacion_ci=1 if parent==1 | parent==2 | parent==7
replace relacion_ci=2 if parent==3
replace relacion_ci=3 if parent==4
replace relacion_ci=4 if parent==5 /* Otros familiares */
replace relacion_ci=5 if parent==6  
replace relacion_ci=6 if parent==8 /*Es el sevicio domestico*/
label var relacion_ci "Relacion con el Jefe de Hogar"
label define relacion_ci 1 "Jefe de Hogar" 2 "Conyuge/Pareja" 3 "Hijo(a)/Hijastro(a)" 4 "Otros Parientes" 5 "Otros No parientes" 6 "Servicio Domestico"
label value relacion_ci relacion_ci

gen factor_ci=factor
label var factor_ci "Factor de Expansion del Individuo"

*************************
*** VARIABLES DE RAZA ***
*************************

* MGR Oct. 2015: modificaciones realizadas en base a metodología enviada por SCL/GDI Maria Olga Peña

gen raza_idioma_ci = . 
gen id_ind_ci = .
gen id_afro_ci = .
gen raza_ci=.
label define raza_ci 1 "Indígena" 2 "Afro-descendiente" 3 "Otros"
label value raza_ci raza_ci 
label value raza_ci raza_ci
label var raza_ci "Raza o etnia del individuo" 

****************************
***VARIABLES DEMOGRAFICAS***
****************************

gen sexo_ci=sexo
label var sexo_ci "Sexo del Individuo"
label define sexo_ci 1 "Masculino" 2 "Femenino"
label value sexo_ci sexo_ci

** Generating Edad
gen edad_ci=edad
replace edad_ci=. if edad==99
label var edad_ci "Edad del Individuo"

gen byte civil_ci=.
replace civil_ci=1 if ecivi==0 | ecivi==1
replace civil_ci=2 if ecivi==2 | ecivi==3 | ecivi==4 | ecivi==5
replace civil_ci=3 if ecivi==7
replace civil_ci=4 if ecivi==6
label var civil_ci "Estado Civil"
label define civil_ci 1 "Soltero" 2 "Union Formal o Informal" 3 "Divorciado o Separado" 4 "Viudo"
label value civil_ci civil_ci

************
***jefe_ci***
*************

gen jefe_ci=(relacion_ci==1)
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
****************
by idh_ch, sort: egen nempdom_ch=sum(relacion_ci==6)
label variable nempdom_ch "Numero de empleados domesticos"


*****************
***clasehog_ch***
*****************

gen byte clasehog_ch=0
**** unipersonal
replace clasehog_ch=1 if nhijos_ch==0 & nconyuges_ch==0 & notropari_ch==0 & notronopari_ch==0
**** nuclear   (child with or without spouse but without other relatives)
replace clasehog_ch=2 if (nhijos_ch>0| nconyuges_ch>0) & (notropari_ch==0 & notronopari_ch==0)
**** ampliado
replace clasehog_ch=3 if ((clasehog_ch ==2 & notropari_ch>0) & notronopari_ch==0) |(notropari_ch>0 & notronopari_ch==0) 
**** compuesto  (some relatives plus non relative)
replace clasehog_ch=4 if ((nconyuges_ch>0 | nhijos_ch>0 | notropari_ch>0) & (notronopari_ch>0))
**** corresidente
replace clasehog_ch=5 if nhijos_ch==0 & nconyuges_ch==0 & notropari_ch==0 & notronopari_ch>0

label variable clasehog_ch "Tipo de hogar"
label define clasehog_ch 1 " Unipersonal" 2 "Nuclear" 3 "Ampliado" 
label define clasehog_ch 4 "Compuesto" 5 " Corresidente", add
label value clasehog_ch clasehog_ch

******************
***nmiembros_ch***
******************

by idh_ch, sort: egen nmiembros_ch=sum(relacion_ci>=1 & relacion_ci<=4)
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

****************
***miembros_ci***
****************

gen miembros_ci=(relacion_ci<5)
label variable miembros_ci "Miembro del hogar"


************************************
*** VARIABLES DEL MERCADO LABORAL***
************************************

****************
****condocup_ci*
****************
/* Etiquetas tomadas de archivo "Manual de etiquetas global.xls"
codsum:
0	Menos de 10 años de edad
1	Ocupado
2	Ocupado pero no trabajó
3	Cesante busca trabajo
4	Estudiante
5	Oficios del hogar
6	Otra situación
7	Incapacitado para trabajar
8	Buscando trabajo por primera vez
9	Cesante no busca trabajo
*/

gen condocup_ci=.
replace condocup_ci=1 if (codsum >=1 & codsum<=2) 
replace condocup_ci=2 if codsum==3 | codsum==8 
replace condocup_ci=3 if condocup_ci!=1 & condocup_ci!=2
replace condocup_ci=4 if edad_ci<10
label define condocup_ci 1"ocupados" 2"desocupados" 3"inactivos" 4"menor de 10 años"
label value condocup_ci condocup_ci
label var condocup_ci "Condicion de ocupacion utilizando definicion del pais"

****************
*afiliado_ci****
****************
gen afiliado_ci=.
label var afiliado_ci "Afiliado a la Seguridad Social"

****************
*cotizando_ci***
****************
gen cotizando_ci=. /*no puedo didtinguir esta pregunta en el formulario de excel. Parece que no se si existe*/
label var cotizando_ci "Cotizante a la Seguridad Social"

****************
*instpen_ci*****
****************
gen instpen_ci=. /*no puedo didtinguir esta pregunta en el formulario de excel. Parece que no se si existe*/
label var instpen_ci "Institucion proveedora de la pension - variable original de cada pais" 
****************
*tipopen_ci*****
****************
gen tipopen_ci=.
label var tipopen_ci "Tipo de pension - variable original de cada pais" 
********************
*** instcot_ci *****
********************
gen instcot_ci=. /*no puedo didtinguir esta pregunta en el formulario de excel. Parece que no se si existe*/
label var instcot_ci "institución a la cual cotiza"

*****************
*tipocontrato_ci*
*****************
gen tipocontrato_ci=.
label var tipocontrato_ci "Tipo de contrato segun su duracion en act principal"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci

*************
*tamemp_ci***
*************
/*
gen tamemp_ci=p25b
label define tamemp_ci 1"menos de 5 personas" 2"5-20 personas" 3"21-50 personas" 4"de 51-100 personas" 5"Más de 100 personas"
label var tamemp_ci "# empleados en la empresa de la actividad principal"
*/
/* Sintaxis del consultor requirió modificaciones
gen tamemp_ci=1 if p25b==1 | p25b==2 | p25b==3 | p25b==4
label var  tamemp_ci "Tamaño de Empresa" 
*Empresas medianas
replace tamemp_ci=2 if p25b==5
*Empresas grandes
label define tamaño 1"Pequeña" 2"Mediana" 3"Grande"
label values tamemp_ci tamaño
tab tamemp_ci [iw=factor_ci]
*/
*Comentario MGD 07/11/2014: segun el manual de etiquetas, la clasificacion de la variable solo va de 1 a 5 personas,
* es decir, segun la clasificacion adoptada para la armonizacion todas serian peque;as.  No es comparable y se genra como missing.

g tamemp_ci=.
label define tamaño 1"Pequeña" 2"Mediana" 3"Grande"
label values tamemp_ci tamaño
/*
gen tamemp_ci=1 if p25b==1 
label var  tamemp_ci "Tamaño de Empresa" 
*Empresas medianas
replace tamemp_ci=2 if p25b==2 | p25b==3
*Empresas grandes
replace tamemp_ci=3 if p25b==4 | p25b==5
label define tamaño 1"Pequeña" 2"Mediana" 3"Grande"
label values tamemp_ci tamaño
*/
/*
*Genera la variable para clasificar a los inactivos
*Jubilados, pensionados e incapacitados
gen categoinac_ci=1 if p2==6 | p2==7
label var  categoinac_ci "Condición de Inactividad" 
*Estudiantes
replace categoinac_ci=2 if p2==4
*Quehaceres del Hogar
replace categoinac_ci=3 if p2==5
*Otra razon
label define inactivo 1"Pensionado y otros" 2"Estudiante" 3"Hogar" 4"Otros"
label values categoinac_ci inactivo
*/

gen categoinac_ci = .
replace categoinac_ci = 2 if ((p2==4 | p20==5) & condocup_ci==3)
replace categoinac_ci = 3 if ((p2==5 | p20==6) & condocup_ci==3)
replace categoinac_ci = 4 if  ((categoinac_ci ~=1 & categoinac_ci ~=2 & categoinac_ci ~=3) & condocup_ci==3)
label var categoinac_ci "Categoría de inactividad"
label define categoinac_ci 1 "jubilados o pensionados" 2 "Estudiantes" 3 "Quehaceres domésticos" 4 "Otros"
label values categoinac_ci categoinac_ci
notes categoinac_ci: no se puede identificar a los jubilados o pensionados, estarían incluidos en otros.

*************
**pension_ci*
*************
gen pension_ci=.  /*no puedo didtinguir esta pregunta en el formulario de excel. Parece que no se si existe pensionado por el modulo de ingresos*/
label var pension_ci "1=Recibe pension contributiva"
 
*************
*  ypen_ci  *
*************
gen ypen_ci=.
label var ypen_ci "Valor de la pension contributiva"

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
replace cesante_ci=1 if (p22==1) & condocup_ci==2
label var cesante_ci "Desocupado - definicion oficial del pais"

*********
*lp_ci***
*********
*Para octubre c/año*/
gen lp_ci=.
replace lp_ci=2143.3 if zona_c==1
replace lp_ci=1714.6 if zona_c==0
label var lp_ci "Linea de pobreza oficial del pais"

***********
*lpe_ci ***
***********
gen lpe_ci =.
label var lpe_ci "Linea de indigencia oficial del pais"

*************
**salmm_ci***
*************
/*Yessenia Loayza/Nota:
"Con la firma del Decreto Ley de Reconversión Monetaria, 
el presidente Chávez autorizó la eliminación de tres ceros a
 la moneda nacional a partir del 1º de enero de 2008"
 Bs (Bolivares Actuales)
 Bsf (Bolivares Fuertes)
 
 conversion:
 *----------
 1 BsF= 1000Bs/1000
 */

gen salmm_ci=.
replace salmm_ci=6000/1000 if zona_c==1 /*en Bs: me parecen q estan muy altos! */
replace salmm_ci=4500/1000 if zona_c==0 
label var salmm_ci "Salario minimo legal"
*Dividio al salmm_ci entre 1000 para hacerlo comparable en el tiempo

****************
***tecnica_ci **
****************
gen tecnica_ci=.
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

*************
***formal_ci***
*************
*gen formal_ci=(cotizando_ci==1)
*Modificación Mayra Sáenz - Septiembre 2014
*Las variables de afiliado y cotizando se generan como missing.
gen formal_ci=.

capture drop ocupa_ci 
gen ocupa_ci=.
replace ocupa_ci=1 if p24>=0 & p24<=9
replace ocupa_ci=2 if p24>=10 & p24<=19
replace ocupa_ci=3 if p24>=20 & p24<=23
replace ocupa_ci=4 if p24>=25 & p24<=29
replace ocupa_ci=6 if p24>=30 & p24<=35
replace ocupa_ci=7 if p24>=40 & p24<=79
replace ocupa_ci=5 if p24>=80 & p24<=89
replace ocupa_ci=8 if p24>=90 & p24<=91
replace ocupa_ci=9 if p24>=99

label var ocupa_ci "Ocupacion Laboral en la Actividad Principal"
label define ocupa_ci 1 "PROFESIONALES Y TECNICOS" 2 "GERENTES, DIRECTORES Y FUNCIONARIOS SUPERIORES"  3 "PERSONAL ADMINISTRATIVO Y NIVEL INTERMEDIO" 4 "COMERCIANTES Y VENDEDORES" 5 "TRABAJADORES EN SERVICIOS" 6"TRABAJADORES AGRICOLAS Y AFINES" 7"OBREROS NO AGRICOLAS, CONDUCTORES DE MAQUINAS Y VEHICULOS DE TRANSPORTE Y SIMILARES" 8"FUERZAS ARMADAS" 9"OTRAS OCUPACIONES NO CLASIFICADAS ANTERIORMENTE"
label values ocupa_ci ocupa_ci

gen byte rama_ci=.
replace rama=1 if p25>=111 & p25<=130
replace rama=2 if p25>=210 & p25<=290
replace rama=3 if p25>=311 & p25<=390
replace rama=4 if p25>=410 & p25<=420
replace rama=5 if p25==500
replace rama=6 if p25>=610 & p25<=632
replace rama=7 if p25>=711 & p25<=720
replace rama=8 if p25>=810 & p25<=833
replace rama=9 if p25>=910 & p25<=960
label var rama_ci "RAMA"
label define rama_ci 1 "Agricultura, caza, silvicultura y pesca" 2 "Explotación de minas y canteras" 3 "Industrias manufactureras" 4 "Electricidad, gas y agua" 5 "Construcción" 6"Comercio al por mayor y menor, restaurantes, hoteles" 7"Transporte y almacenamiento" 8"Establecimientos financieros, seguros, bienes inmuebles" 9"Servicios sociales, comunales y personales"
label values rama_ci rama_ci


/* A partir de 1999 no se preguntan las horas totales sino las horas en la ocupacion principal */
gen byte horastot_ci=.
replace horastot_ci=p9 if p9<=110 & p9>0
label var horastot_ci "Horas totales trabajadas la semana pasada en todas las Actividades"

capture drop horaspri_ci
gen byte horaspri_ci=horastot_ci
label var horaspri_ci "Horas totales trabajadas la semana pasada en la Actividad Principal"

replace horastot_ci=. if emp_ci==0
replace horaspri_ci=. if emp_ci==0

gen antiguedad_ci=.
label var antiguedad_ci "Antiguedad en la Ocupacion Principal Actual (en anios)"

gen durades0_ci=.
replace durades0_ci=p23 if p23>0 & p23<9
label var durades0_ci "Duracion del Desempleo (en meses)"
label define durades0_ci 1 "hasta 3 meses" 2 "4-6 meses" 3 "7-9 meses" 4 "10-11 meses" 5 "1-2 años" 6 "más de 2 años" 
label values durades0_ci durades0_ci

capture drop desalent_ci
gen byte desalent_ci=0
replace desalent_ci=1 if (p2>3 & p2<=6) & p5==4 & p15==2 & p20==1 
replace desalent_ci=. if edad<10
label var desalent_ci "Trabajadores desalentados, personas que creen que por alguna razon no conseguiran trabajo" 

gen subemp_ci=.
*replace subemp_ci=1 if hrsnort>=0 & hrsnort<=30 & pp32==1 
*replace subemp_ci=. if (hrsnort==. | pp32==. | hrsnort<0 | pp32<0)
/* No esta en el cuestionario: Preferiria trabajar 30 horas o mas por semana? */
label var subemp_ci "Trabajadores subempleados"

gen tiempoparc_ci=.
replace tiempoparc_ci=1 if p11>=0 & p11<=30 & p12==1
replace tiempoparc_ci=. if (p11==. | p12==. | p11<0 | p12<0)
label var tiempoparc_ci "Trabajadores a medio tiempo"

* Modificacion MGD 07/14/2014: Condicionado a que esten ocupados.
gen categopri_ci=.
replace categopri_ci=1 if p27==4 & condocup_ci==1
replace categopri_ci=2 if p27==3 & condocup_ci==1
replace categopri_ci=3 if (p27==1 | p27==2) & condocup_ci==1 
replace categopri_ci=4 if p27==5 & condocup_ci==1
label var categopri_ci "CATEGORIA OCUPACIONAL ACTIVIDAD PRINCIPAL"
label define categopri_ci 1 "Patron" 2 "Cuenta Propia" 3 "Asalariado" 4 "Trabajador No Remunerado" 
label values categopri_ci categopri_ci

gen categosec_ci=.
label var categosec_ci "CATEGORIA OCUPACIONAL ACTIVIDAD SECUNDARIA"

capture drop nempleos_ci
gen byte nempleos_ci=.
replace nempleos=1 if emp==1 & p8==1
replace nempleos=2 if emp==1 & p8==2 
label var nempleos "Numero de empleos"
label define nempleos_ci 1 "un trabajo" 2 "dos o mas trabajos"
label values nempleos_ci nempleos_ci

capture drop tamfirma_ci
gen byte tamfirma_ci=.
replace tamfirma_ci=1 if emp==1 & (p25b==5)
replace tamfirma_ci=0 if emp==1 & (p25b>=1 & p25b<=4)
label var tamfirma "Trabajadores formales"
label define tamfirma_ci 1 "5 o mas trabajadores" 0 "Menos de 5 trabajadores"
label values tamfirma_ci tamfirma_ci

capture drop spublico_ci
gen byte spublico_ci=.
replace spublico_ci=1 if emp==1 & p27==1 
replace spublico_ci=0 if emp==1 & p27>=2 & p27<=5
label var spublico_ci "Personas que trabajan en el sector publico"

*************************************************************************************
*******************************INGRESOS**********************************************
*************************************************************************************
*Mayra Sáenz -Septiembre 2014. Se cambia toda la sintaxis de ingresos.

***************
***ylmpri_ci***
***************
capture drop ylmpri_ci
*Modificación Mayra Sáenz - Septiembre 2014
*Se incluyen los ingresos. Adicionalmente, se divide para 1000 como se realiza a partir de 1994 por motivos de comparabilidad.
gen ylmpri_ci=.
replace ylmpri_ci=ingmon*10 if ingmon<9999 & ingtip==2
replace ylmpri_ci=ingmon*10*4.3 if ingmon<9999 & ingtip==1
replace ylmpri_ci=. if edad<10
replace ylmpri_ci=ylmpri_ci/1000
label var ylmpri_ci "Ingreso Laboral Monetario de la Actividad Principal"


*****************
***nrylmpri_ci***
*****************

gen nrylmpri_ci=.

**********************************************************************************************
***TCYLMPRI_CH : Identificador de los hogares en donde alguno de los miembros reporta como
*** top-code el ingreso de la actividad principal. .
***********************************************************************************************
gen tcylmpri_ch = .
label var tcylmpri_ch "Id hogar donde algún miembro reporta como top-code el ingr de activ. principal"

***********************************************************************************************
***TCYLMPRI_CI : Identificador de top-code del ingreso de la actividad principal.
***********************************************************************************************
gen tcylmpri_ci = .
label var tcylmpri_ci "Identificador de top-code del ingreso de la actividad principal"

***************
***ylmsec_ci***
***************

gen ylmsec_ci=.

************
***ylm_ci***
************

egen ylm_ci=rsum(ylmpri_ci ylmsec_ci), missing
replace ylm_ci=. if ylmpri_ci==. & ylmsec_ci==.

*************
***ynlm_ci***
*************

gen ynlm_ci=.


***N/A***
gen ylnmpri_ci=.
gen ylnmsec_ci=.
gen ylnm_ci=.
gen ynlnm_ci=.
gen autocons_ci=.
gen remesas_ci=.


************************
*** HOUSEHOLD INCOME ***
************************

*******************
*** nrylmpri_ch ***
*******************

*Creating a Flag label for those households where someone has a ylmpri_ci as missing

by idh_ch, sort: egen nrylmpri_ch=sum(nrylmpri_ci) if miembros_ci==1, missing
replace nrylmpri_ch=1 if nrylmpri_ch>0 & nrylmpri_ch<.
replace nrylmpri_ch=. if nrylmpri_ch==.

**************
*** ylm_ch ***
**************

by idh_ch, sort: egen ylm_ch=sum(ylm_ci) if miembros_ci==1, missing

****************
*** ylmnr_ch ***
****************

by idh_ch, sort: egen ylmnr_ch=sum(ylm_ci) if miembros_ci==1, missing
replace ylmnr_ch=. if nrylmpri_ch==1


***************
*** ynlm_ch ***
***************

by idh_ch, sort: egen ynlm_ch=sum(ynlm_ci) if miembros_ci==1, missing

* N/A*
gen ylnm_ch=.
gen ynlnm_ch=.
gen rentaimp_ch=.
gen autocons_ch=.
gen remesas_ch=.

*****************
***ylhopri_ci ***
*****************

gen ylmhopri_ci=ylmpri_ci/(horaspri_ci*4.3)


***************
***ylmho_ci ***
***************

gen ylmho_ci=ylm_ci/(horastot_ci*4.3)

******************************************************************
**YLMOTROS_CI : Ingreso laboral monetario otros trabajos.    ***
*********************************************************************

gen ylmotros_ci=.
gen ylnmotros_ci=.
label var ylnmotros_ci "Ingreso Laboral NO Monetario Otros Trabajos"

/*
capture drop ylmpri_ci
gen ylmpri_ci=.
label var ylmpri_ci "Ingreso Laboral Monetario de la Actividad Principal"

g nrylmpri_ci=(ylmpri_ci==. & emp_ci==1)
replace nrylmpri_ci=. if emp_ci!=1 | categopri_ci==4 /*excluding unpaid workers*/
label var nrylmpri_ci "Id no respuesta ingreso de la actividad principal"  


gen ylmsec_ci=.	
label var ylmsec_ci "Ingreso Laboral Monetario de la Actividad Secundaria"

gen ylmotros_ci=.
label var ylmotros_ci "Ingreso Laboral Monetario Otros Trabajos"

gen ylnmpri_ci=.
label var ylnmpri_ci "Ingreso Laboral NO Monetario de la Actividad Principal"

gen ylnmsec_ci=.	
label var ylnmsec_ci "Ingreso Laboral NO Monetario de la Actividad Secundaria"

gen ylnmotros_ci=.
label var ylnmotros_ci "Ingreso Laboral NO Monetario Otros Trabajos"

gen ylm0_ci=.
replace ylm0_ci=ingmon*10 if ingmon<9999 & ingtip==2
replace ylm0_ci=ingmon*10*4.3 if ingmon<9999 & ingtip==1
replace ylm0_ci=. if edad<10
label var ylm0_ci "Ingreso Laboral Monetario Total + Horas Extra"

gen ylm_ci=.
label variable ylm_ci "Ingreso Laboral Monetario Total" 

gen ylnm_ci=.
label var ylnm_ci "Ingreso Laboral NO Monetario Total"

gen ynlm0_ci=.
label var ynlm0_ci "Ingreso NO Laboral Monetario"

gen ynlnm_ci=.
label var ynlnm_ci "Ingreso NO Laboral NO Monetario"

capture drop nrylmpri0_ci
gen nrylmpri0_ci=.
replace nrylmpri0_ci=0 if (ingmon>0 & ingmon<9999)
replace nrylmpri0_ci=1 if (ingmon==9999) 
label var nrylmpri0_ci "Identificador de No Respuesta del Ingreso Monetario de todas las Actividades"

gen autocons_ci=.
label var autocons_ci "Autoconsumo Individual"

gen remesas_ci=.
label var remesas_ci "Remesas Individuales"

capture drop nrylmpri0_ch
sort idh
egen nrylmpri0_ch=sum(nrylmpri0_ci) if miembro==1, by(idh_ch) 
replace nrylmpri0_ch=1 if nrylmpri0_ch>1 & nrylmpri0_ch~=. & miembro==1 
label var nrylmpri0_ch "Identificador de Hogares en donde alguno de los miembros No Responde el Ingreso Monetario de todas las Actividades"

egen ylm_ch=sum(ylm_ci) if miembros_ci==1, by(idh_ch)
label var ylm_ch "Ingreso Laboral Monetario del Hogar"

egen ylmnr_ch=sum(ylm_ci) if miembros_ci==1 & nrylmpri0_ch==0, by(idh_ch)
label var ylmnr_ch "Ingreso Laboral Monetario del Hogar, considera 'missing' la No Respuesta"

egen ylm0_ch=sum(ylm0_ci) if miembros_ci==1, by(idh_ch)
label var ylm0_ch "Ingreso Laboral Monetario del Hogar (inc Hrs Extra)"

egen ylmnr0_ch=sum(ylm0_ci) if miembros_ci==1 & nrylmpri0_ch==0, by(idh_ch)
label var ylmnr0_ch "Ingreso Laboral Monetario del Hogar, considera 'missing' la No Respuesta (inc Hrs Extra)"

egen ylnm_ch=sum(ylnm_ci) if miembros_ci==1, by(idh_ch)
label var ylnm_ch "Ingreso Laboral No Monetario del Hogar"

egen ynlm0_ch=sum(ynlm0_ci) if miembros_ci==1, by(idh_ch)
label var ynlm0_ch "Ingreso No Laboral Monetario del Hogar"

egen ynlnm_ch=sum(ynlnm_ci) if miembros_ci==1, by(idh_ch)
label var ynlnm_ch "Ingreso No Laboral No Monetario del Hogar"

egen autocons_ch=sum(autocons_ci) if miembros_ci==1, by(idh_ch)
label var autocons_ch "Autoconsumo del Hogar"

egen remesas_ch=sum(remesas_ci) if miembros_ci==1, by(idh_ch)
label var remesas_ch "Remesas del Hogar (monetario + especies)"

replace ylnm_ch=. if ylnm_ci==.
replace ynlnm_ch=. if ynlnm_ci==.
replace autocons_ch=. if autocons_ci==.
replace remesas_ch=. if remesas_ci==.
replace ynlm0_ch=. if ynlm0_ci==.
replace ylm0_ch =. if miembros_ci==0
replace ylmnr_ch =. if miembros_ci==0
replace ylnm_ch =. if miembros_ci==0
replace ynlnm_ch =. if miembros_ci==0
replace autocons_ch =. if miembros_ci==0
replace remesas_ch =. if miembros_ci==0
replace ynlm0_ch =. if miembros_ci==0

gen ylmhopri_ci=.
replace ylmhopri_ci=ylmpri_ci/(horaspri*4.3)
label var ylmhopri_ci "Salario Horario Monetario de la Actividad Principal"

gen ylmho_ci=.
replace ylmho_ci=ylm_ci/(horastot*4.3)
label var ylmho_ci "Salario Horario Monetario de todas las Actividades"

gen ylmho0_ci=.
replace ylmho0_ci=ylm0_ci/(horastot*4.3)

gen tcylmpri_ci=.
gen tcylmpri_ch=.
gen ynlm_ci=.

by idh_ch, sort: egen ynlm_ch=sum(ynlm_ci) if miembros_ci==1

gen rentaimp_ch=.
gen nrylmpri_ch=.
*/

****************************
***VARIABLES DE EDUCACION***
****************************

gen GRADO=grado if grado>0 & grado<9
gen NIVEL=1 if edad>=10 & nivel==0
replace NIVEL=2 if nivel==1
replace NIVEL=3 if nivel==2
replace NIVEL=4 if nivel>=4 & nivel<=22
replace NIVEL=5 if nivel>=37 & nivel<=69
replace NIVEL=6 if nivel>=23 & nivel<=36
** LAS PREGUNTAS DE EDUCACION SE HACEN A PARTIR DE LOS 10 AÑOS DE EDAD (DESDE 1993), NO COMPARABLE CON LOS AÑOS POSTERIORES EN DONDE SE REALIZAN A PARTIR DE LOS 3 AÑOS ** 

*gen ULTSEM=pp18c if pp18c<99
gen ASIST=aesc if aesc<3
gen EDAD=edad_ci

capture drop asiste_ci
gen byte asiste_ci=.
replace asiste_ci=1 if ASIST==1
replace asiste_ci=0 if ASIST==2
label var asiste_ci "1: Personas que actualmente asisten a centros de enseñanza"

capture drop aedu_ci
gen byte aedu_ci=.
replace aedu=0 if NIVEL==1 | NIVEL==2 
replace aedu=GRADO if NIVEL==3 & GRADO>0
replace aedu=GRADO+6 if NIVEL==4 & GRADO>0 & GRADO<=5
replace aedu=11 if NIVEL==4 & GRADO>5
replace aedu=GRADO+11 if (NIVEL==5 | NIVEL==6) & GRADO>0 
*replace aedu=int(ULTSEM/2)+11 if (NIVEL==5 | NIVEL==6) & ULTSEM>0 
label variable aedu_ci "Años de Educacion"

* Unfortunately, we found people with more years of education that years of life. 
* Then, assuming that everyone enters to school not before 5 years old. To correct this:
forvalues i=0(1)18 {
if `i'==0 {
replace aedu=`i' if (aedu>`i' & aedu~=.) & (edad==3 | edad==4 | edad==5)
}
if `i'~=0 {
replace aedu=`i' if (aedu>`i' & aedu~=.) & edad==(`i'+5)
}
}

gen eduno_ci=.
replace eduno=1 if NIVEL==1
replace eduno=0 if NIVEL>1 & NIVEL<=6
label var eduno_ci "1 = personas sin educacion (excluye preescolar)"

gen edupre_ci=.
replace edupre=1 if NIVEL==2
replace edupre=0 if NIVEL>2 | NIVEL==1
label var edupre_ci "Educacion preescolar"

gen edupi_ci=.
replace edupi=1 if aedu>0 & aedu<6
replace edupi=0 if aedu==0 | (aedu>=6 & aedu!=.)
label var edupi_ci "1 = personas que no han completado el nivel primario"

gen edupc_ci=.
replace edupc=1 if aedu==6
replace edupc=0 if (aedu>=0 & aedu<6)  | (aedu>6 & aedu!=.) 
label var edupc_ci "1 = personas que han completado el nivel primario"

gen edusi_ci=.
replace edusi=1 if aedu>6 & aedu<11
replace edusi=0 if (aedu>=0 & aedu<=6) | (aedu>=11 & aedu!=.)
label var edusi_ci "1 = personas que no han completado el nivel secundario"

gen edusc_ci=.
replace edusc=1 if aedu==11 
replace edusc=0 if (aedu>=0 & aedu<11) | (aedu>11 & aedu!=.) 
label var edusc_ci "1 = personas que han completado el nivel secundario"

/*
OLD CODE:
gen eduui_ci=.
replace eduui=1 if aedu>11 & ((aedu<14 & NIVEL==5) | (aedu<16 & NIVEL==6))
replace eduui=0 if (aedu>=0 & aedu<=11) | (aedu>=16 & aedu!=. & NIVEL==6) | (aedu>=14 & aedu!=. & NIVEL==5) | (NIVEL==4 & GRADO==3 & aedu==12)
label var eduui_ci "1 = personas que no han completado el nivel universitario o superior"

gen eduuc_ci=.
replace eduuc=1 if (aedu>=16 & aedu!=. & NIVEL==6) | (aedu>=14 & aedu!=. & NIVEL==5)
replace eduuc=0 if aedu>=0 & ((aedu<14) | (aedu<16 & NIVEL==6))
label var eduuc_ci "1 = personas que han completado el nivel universitario o superior"
*/

gen eduui_ci=.
replace eduui=1 if aedu>11 & aedu<16
replace eduui=0 if (aedu>=0 & aedu<=11) | (aedu>=16 & aedu!=.) 
label var eduui_ci "1 = personas que no han completado el nivel universitario o superior"

gen eduuc_ci=.
replace eduuc=1 if aedu>=16 & aedu!=.
replace eduuc=0 if (aedu>=1 & aedu<16) 
label var eduuc_ci "1 = personas que han completado el nivel universitario o superior"


gen edus1i_ci=.
replace edus1i=0 if edusi==1 | edusc==1 
replace edus1i=1 if edusi==1 & (NIVEL==4 & (GRADO==1 | GRADO==2))
label var edus1i_ci "1 = personas que no han completado el primer ciclo de la educacion secundaria"

gen edus1c_ci=.
replace edus1c=0 if edusi==1 | edusc==1 
replace edus1c=1 if edusi==1 & (NIVEL==4 & GRADO==3)
label var edus1c_ci "1 = personas que han completado el primer ciclo de la educacion secundaria"

gen edus2i_ci=.
replace edus2i=0 if edusi==1 | edusc==1 
replace edus2i=1 if edusi==1 & (NIVEL==4 & GRADO<4) 
label var edus2i_ci "1 = personas que no han completado el segundo ciclo de la educacion secundaria"

gen edus2c_ci=.
replace edus2c=0 if edusi==1 
replace edus2c=1 if edusc==1
label var edus2c_ci "1 = personas que han completado el segundo ciclo de la educacion secundaria"

gen eduac_ci=.
replace eduac=0 if eduui==1 | eduuc==1
replace eduac=1 if NIVEL==6
label var eduac_ci "Educacion terciaria académica versus educación terciaria no-académica "

gen repite_ci=.
label var repite_ci "Personas que han repetido al menos un año o grado"

gen repiteult_ci=.
label var repiteult_ci "Personas que han repetido el ultimo grado"

gen edupub_ci=.
label var edupub_ci "1 = personas que asisten a centros de enseñanza publicos"

** Generating pqnoasis
gen byte pqnoasis_ci=.
*replace pqnoasis=pp21 if pp21>0
*label variable pqnoasis_ci "Razones para no asistir a centros de enseñanza"
*label define pqnoasis_ci 1 "Culmino sus estudios" 2 "No hay grado o agnos superiores" 3 "No hay cupo" 4 "Falta de recursos economicos" 5 "Bajo rendimiento escolar" 6 "Esta trabajando" 7 "Asiste a un curso de capacitacion" 8 "No quiere estudiar" 9 "enfermedad o defecto fisico" 10 "Problemas de conducta" 11 "Problemas de aprendizaje" 12 "Cambio de residencia" 13 "Desordenes estudiantiles" 14 "Inasistencia frecuente de maestros y profesores" 15 "Esta de vacaciones" 16 "Edad mayor que la regular" 17 "Escuela distante" 18 "Tiene que ayudar en la casa" 19 "edad menor que la regular" 20 "otros"
*label values pqnoasis_ci pqnoasis_ci
drop GRADO NIVEL EDAD ASIST 

**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**
	
**************
*pqnoasis1_ci*
**************
g       pqnoasis1_ci = .
label define pqnoasis1_ci 1 "Problemas económicos" 2 "Por trabajo" 3 "Problemas familiares o de salud" 4 "Falta de interés" 5	"Quehaceres domésticos/embarazo/cuidado de niños/as" 6 "Terminó sus estudios" 7	"Edad" 8 "Problemas de acceso"  9 "Otros"
label value  pqnoasis1_ci pqnoasis1_ci

********************************************
***Variables de Infraestructura del hogar***
********************************************

gen aguared_ch=.
replace aguared_ch=1 if agua==1
replace aguared_ch=0 if agua==2 

gen aguadist_ch=.

gen aguamala_ch=.

gen aguamide_ch=.

gen luz_ch=.
replace luz_ch=1 if alumbrad==1
replace luz_ch=0 if alumbrad==2

gen luzmide_ch=.

gen combust_ch=.

gen bano_ch=.

gen banoex_ch=.

gen des1_ch=.

gen des2_ch=.
replace des2_ch=1 if excretas==1 
replace des2_ch=2 if excretas==2
replace des2_ch=0 if excretas==0

gen piso_ch=.

gen pared_ch=.

gen techo_ch=.

gen resid_ch=.

gen resid2_ch=.

gen aguamejorada_ch =.

gen  banomejorado_ch =.

gen dorm_ch=.

gen cuartos_ch=.
replace cuartos_ch=cuartos if cuartos>=0 & cuartos<9

gen cocina_ch=.

gen telef_ch=.

gen refrig_ch=.

gen freez_ch=.

gen auto_ch=.

gen compu_ch=.

gen internet_ch=.

gen cel_ch=.

gen vivi1_ch=.
replace vivi1_ch=1 if vivtip==1 
replace vivi1_ch=2 if vivtip==2 | vivtip==3 | vivtip==4
replace vivi1_ch=3 if vivtip>=5 & vivtip<=8

gen vivi2_ch=.
replace vivi2_ch=1 if vivi1_ch==1 | vivi1_ch==2
replace vivi2_ch=0 if vivi1_ch==3

gen viviprop_ch=.
replace viviprop_ch=0 if tenencia>=3 & tenencia<=7
replace viviprop_ch=1 if tenencia==1
replace viviprop_ch=2 if tenencia==2
replace viviprop_ch=3 if tenencia==9

gen vivitit_ch=.

gen vivialq_ch=.

gen vivialqimp_ch=.


cap drop casado
cap gen durades_ci=.
cap gen cuartos_ch=.
cap gen viviprop_ch=.
cap gen des1_ch=.
cap gen des2_ch=.
cap gen piso_ch=.
cap gen techo_ch=.
cap gen pared_ch=.
cap gen luz_ch=.
cap gen refrig_ch=.
cap gen freez_ch=.
cap gen auto_ch=.
cap gen compu_ch=.
cap gen aguared_ch=.



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



