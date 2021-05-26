* (Versión Stata 13)
clear
set more off
*________________________________________________________________________________________________________________*

 * Activar si es necesario (dejar desactivado para evitar sobreescribir la base y dejar la posibilidad de 
 * utilizar un loop)
 * Los datos se obtienen de las carpetas que se encuentran en el servidor: ${surveysFolder}
 * Se tiene acceso al servidor únicamente al interior del BID.
 * El servidor contiene las bases de datos MECOVI.
 *________________________________________________________________________________________________________________*
 
global ruta = "${surveysFolder}"

local PAIS VEN
local ENCUESTA EHM
local ANO "1995"
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
Autores: Mayra Sáenz - saenzmayra.a@gmail.com - mayras@iadb.org - Diciembre 2013
Versión 2006: Victoria
Generación nuevas variables LMK: Yessenia Loayza (desloay@hotmail.com | yessenial@iadb.org)
Última versión: Yessenia Loayza - Email: desloay@hotmail.com | yessenial@iadb.org
Fecha última modificación: octubre 2013

							SCL/LMK - IADB
****************************************************************************/
****************************************************************************/
use `base_in', clear
destring _all, replace

***********
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
gen anio_c=1995

gen mes_c=.
/* No se cuenta con informacion especifica sobre la semana de planificacion para esta encuesta */
replace mes_c= 7  if semalev>=1  & semalev<=2
replace mes_c= 8  if semalev>=3  & semalev<=6
replace mes_c= 9  if semalev>=7  & semalev<=10
replace mes_c= 10 if semalev>=11 & semalev<=14
replace mes_c= 11 if semalev>=15 & semalev<=18
replace mes_c= 12 if semalev>=19 & semalev<=24
*** average week of the survey is 11.36 which means mes==10
replace mes_c= 10  if mes_c==.
label variable mes_c "Mes de la Encuesta: Segundo Semestre de 1995"  
label define mes_c 7 "JUL" 8 "AUG" 9 "SEP" 10 "OCT" 11 "NOV" 12 "DEC" 
label values mes_c mes_c

gen zona_c=zona
replace zona_c=0 if zona==2
label define zona_c 0 "Rural" 1 "Urbana"  
label value zona_c zona_c

sort entidad control area linea nrohogsv subdom locali  nrohog
egen idh_ch=group(entidad control area linea nrohogsv subdom locali nrohog)
label var idh_ch "Identificador Unico del Hogar"

gen idp_ci=nroper
label var idp_ci "Identificador Individual dentro del Hogar"

gen factor_ch=pesoh
label var factor_ch "Factor de expansion del Hogar"

gen relacion_ci=.
replace relacion_ci=1 if parentco==1
replace relacion_ci=2 if parentco==2
replace relacion_ci=3 if parentco==3
replace relacion_ci=4 if parentco>=4 & parentco<=14 /* Otros familiares */
replace relacion_ci=5 if parentco==15  
replace relacion_ci=6 if parentco==16 | parentco==17 /*Es el sevicio domestico, Incluye a familiares del Serv. Domestico en parentco==17 */
label var relacion_ci "Relacion con el Jefe de Hogar"
label define relacion_ci 1 "Jefe de Hogar" 2 "Conyuge/Pareja" 3 "Hijo(a)/Hijastro(a)" 4 "Otros Parientes" 5 "Otros No parientes" 6 "Servicio Domestico (inc fam Serv. Dom.)"
label value relacion_ci relacion_ci

gen factor_ci=pesop
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
replace civil_ci=1 if sitcoyu==-1 | sitcoyu==7
replace civil_ci=2 if sitcoyu==1 | sitcoyu==2 | sitcoyu==3 | sitcoyu==4
replace civil_ci=3 if sitcoyu==5
replace civil_ci=4 if sitcoyu==6
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

gen ynlm_ci=.
*gen firmapeq_ci=.

****************
****condocup_ci*
****************
/*
gen condocup_ci=.
replace condocup_ci=1 if (actvsum>=1 & actvsum <=2) 
replace condocup_ci=2 if actvsum==3 | actvsum==8
replace condocup_ci=3 if condocup_ci!=1 & condocup_ci!=2
replace condocup_ci=4 if edad_ci<15
label define condocup_ci 1"ocupados" 2"desocupados" 3"inactivos" 4"menor de PET"
label value condocup_ci condocup_ci
label var condocup_ci "Condicion de ocupacion utilizando definicion del pais"
*/

* Cambio edad minima de la encuesta (10 años). MGD 06/10/2014
gen condocup_ci=.
replace condocup_ci=1 if (actvsum>=1 & actvsum <=2) 
replace condocup_ci=2 if actvsum==3 | actvsum==8 
replace condocup_ci=3 if (condocup_ci!=1 & condocup_ci!=2) & edad_ci>=10
replace condocup_ci=4 if edad_ci<10
label define condocup_ci 1"ocupados" 2"desocupados" 3"inactivos" 4"menor de PET"
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
gen cotizando_ci=0     if condocup_ci==1 | condocup_ci==2
foreach var of varlist benefa benefb benefc benefd benefe beneff benefg benefh benefi benefj benefk benefl benefm  {
replace cotizando_ci=1 if (`var'==4 | `var'==10 | `var'==12) & cotizando_ci==0 
}
label var cotizando_ci "Cotizante a la Seguridad Social"

****************
*instpen_ci*****
****************
gen instpen_ci=.
label var instpen_ci "Institucion proveedora de la pension - variable original de cada pais" 

****************
*tipopen_ci*****
****************
gen tipopen_ci=.
label var tipopen_ci "Tipo de pension - variable original de cada pais" 


********************
*** instcot_ci *****
********************
gen instcot_ci=.
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
gen tamemp_ci=tamestp
label define tamemp_ci 1"menos de 5 personas" 2"5-20 personas" 3"21-50 personas" 4"de 51-100 personas" 5"Más de 100 personas"
label var tamemp_ci "# empleados en la empresa de la actividad principal"
*/
gen tamemp_ci=1 if tamestp==1 
label var  tamemp_ci "Tamaño de Empresa" 
*Empresas medianas
replace tamemp_ci=2 if tamestp==2 | tamestp==3
*Empresas grandes
replace tamemp_ci=3 if tamestp==4 | tamestp==5
label define tamaño 1"Pequeña" 2"Mediana" 3"Grande"
label values tamemp_ci tamaño
tab tamemp_ci [iw=factor_ci]

/* Sintaxis del consultor requirió modificación
*Genera la variable para clasificar a los inactivos
*Jubilados, pensionados e incapacitados
gen categoinac_ci=1 if trabaja==6 | trabaja==7
label var  categoinac_ci "Condición de Inactividad" 
*Estudiantes
replace categoinac_ci=2 if trabaja==4
*Quehaceres del Hogar
replace categoinac_ci=3 if trabaja==5
*Otra razon
label define inactivo 1"Pensionado y otros" 2"Estudiante" 3"Hogar" 4"Otros"
label values categoinac_ci inactivo
tab categoinac_ci [iw=factor_ci]
*/

gen categoinac_ci = .
replace categoinac_ci = 2 if ((trabaja==4 | pqnobus==5) & condocup_ci==3)
replace categoinac_ci = 3 if ((trabaja==5 | pqnobus==6) & condocup_ci==3)
replace categoinac_ci = 4 if  ((categoinac_ci ~=1 & categoinac_ci ~=2 & categoinac_ci ~=3) & condocup_ci==3)
label var categoinac_ci "Categoría de inactividad"
label define categoinac_ci 1 "jubilados o pensionados" 2 "Estudiantes" 3 "Quehaceres domésticos" 4 "Otros"
label values categoinac_ci categoinac_ci
notes categoinac_ci: no se puede identificar a los jubilados o pensionados, estarían incluidos en otros.


*************
**pension_ci*
*************
gen pension_ci=0 
foreach var of varlist yotrosa yotrosb yotrosc yotrosd yotrose yotrosf yotrosg {
replace pension_ci=1 if (`var'==1 |`var'==3 | `var'==4) /*A todas las per mayores de diez años */
}
label var pension_ci "1=Recibe pension contributiva"
 
*************
*  ypen_ci  *
*************
gen ypen_ci=yotros/1000 if pension_ci==1
replace ypen_ci=. if yotros<0
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
replace cesante_ci=1 if (cesante==1) & condocup_ci==2
label var cesante_ci "Desocupado - definicion oficial del pais"

*********
*lp_ci***
*********
gen lp_ci=.
replace lp_ci=9690.1 if zona_c==1
replace lp_ci=7752.1 if zona_c==0
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
replace salmm_ci=15000/1000 if zona_c==1 /*en Bs*/
replace salmm_ci=12500/1000 if zona_c==0 
label var salmm_ci "Salario minimo legal"
*Y.L. divido al salmm_ci entre 1000 para hacerlo comparable a lo largo del tiempo

****************
***tecnica_ci **
****************
gen tecnica_ci=(nivel==5)
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
gen formal_ci=(cotizando_ci==1)

capture drop ocupa_ci 
gen ocupa_ci=.
replace ocupa_ci=1 if ocuprin>=0 & ocuprin<=9
replace ocupa_ci=2 if ocuprin>=10 & ocuprin<=19
replace ocupa_ci=3 if ocuprin>=20 & ocuprin<=23
replace ocupa_ci=4 if ocuprin>=25 & ocuprin<=29
replace ocupa_ci=6 if ocuprin>=30 & ocuprin<=35
replace ocupa_ci=7 if ocuprin>=40 & ocuprin<=79
replace ocupa_ci=5 if ocuprin>=80 & ocuprin<=89
replace ocupa_ci=8 if ocuprin>=90 & ocuprin<=91
replace ocupa_ci=9 if ocuprin>=99

label var ocupa_ci "Ocupacion Laboral en la Actividad Principal"
label define ocupa_ci 1 "PROFESIONALES Y TECNICOS" 2 "GERENTES, DIRECTORES Y FUNCIONARIOS SUPERIORES"  3 "PERSONAL ADMINISTRATIVO Y NIVEL INTERMEDIO" 4 "COMERCIANTES Y VENDEDORES" 5 "TRABAJADORES EN SERVICIOS" 6"TRABAJADORES AGRICOLAS Y AFINES" 7"OBREROS NO AGRICOLAS, CONDUCTORES DE MAQUINAS Y VEHICULOS DE TRANSPORTE Y SIMILARES" 8"FUERZAS ARMADAS" 9"OTRAS OCUPACIONES NO CLASIFICADAS ANTERIORMENTE"
label values ocupa_ci ocupa_ci

***************
*** rama_ci ***
***************
gen rama_ci=.
replace rama_ci=1 if (ramaprin>=111 & ramaprin<=141) & emp_ci==1
replace rama_ci=2 if (ramaprin>=210 & ramaprin<=290) & emp_ci==1
replace rama_ci=3 if (ramaprin>=311 & ramaprin<=390) & emp_ci==1
replace rama_ci=4 if (ramaprin>=410 & ramaprin<=420) & emp_ci==1
replace rama_ci=5 if ramaprin==500 & emp_ci==1
replace rama_ci=6 if (ramaprin>=610 & ramaprin<=632) & emp_ci==1
replace rama_ci=7 if (ramaprin>=711 & ramaprin<=720) & emp_ci==1
replace rama_ci=8 if (ramaprin>=810 & ramaprin<=833) & emp_ci==1
replace rama_ci=9 if (ramaprin>=910 & ramaprin<=960) & emp_ci==1
label var rama_ci "RAMA"
label define rama_ci 1 "Agricultura, caza, silvicultura y pesca" 2 "Explotación de minas y canteras" 3 "Industrias manufactureras" 4 "Electricidad, gas y agua" 5 "Construcción" 6"Comercio al por mayor y menor, restaurantes, hoteles" 7"Transporte y almacenamiento" 8"Establecimientos financieros, seguros, bienes inmuebles" 9"Servicios sociales, comunales y personales"
label values rama_ci rama_ci


/* A partir de 1999 no se preguntan las horas totales sino las horas en la ocupacion principal */
* Modificacion MGD 07/14/2014: se usan las dos variables de horas de actividad principal y horas normalmente trabajadas
capture drop horaspri_ci
gen byte horaspri_ci=.
replace horaspri_ci=hrstot if hrstot<=110 & hrstot>=0
label var horaspri "Horas totales trabajadas la semana pasada en la Actividad Principal"

gen byte horastot_ci=.
replace horastot_ci=hrsnort if hrsnort<=110 & hrsnort>=0 & hrsnort>=hrstot
replace horastot_ci=hrstot if hrstot>=hrsnort & hrstot>=0
label var horastot_ci "Horas totales trabajadas la semana pasada en todas las Actividades"


gen antiguedad_ci=.
replace antiguedad=tmpoemp if tmpoemp>=0 & tmpoemp!=.
label var antiguedad_ci "Antiguedad en la Ocupacion Principal Actual (en anios)"

/*gen durades_ci=.
gen meses=.
gen agnos=.
replace meses=mesesint if mesesint~=-1 & mesesint~=-2 & mesesint~=-3
replace meses=0 if mesesint==-1
replace agnos=anosint if anosint~=-1 & anosint~=-2 & anosint~=-3
replace agnos=0 if anosint==-1

replace durades_ci=(agnos*12)+meses if desemp_ci==1
replace durades_ci=0 if mesesint==0
*/
* Modificacion MGD 07/11/2014: se cambia la variable a pp43a/b.
g meses=mesesint if mesesint>0
g anios=anosint*12 if anosint>0
egen durades_ci = rsum(meses anios), missing
replace durades_ci=. if condocup_ci==3
*Se ponen como missing values las personas que llevan más tiempo desempleadas que tiempo de vida:
gen edad_meses=edad_ci*12
replace durades_ci=. if durades_ci>edad_meses
drop edad_meses

label var durades "Duracion del Desempleo (en meses)"


replace horastot_ci=. if emp_ci==0


capture drop desalent_ci
gen byte desalent_ci=0
replace desalent_ci=1 if (trabaja>3 & trabaja<7) & actv1==15 & tienetra==2 & hechodil==2 & (pqnobus==1 | pqnobus==2)
replace desalent=. if edad_ci<10
label var desalent_ci "Trabajadores desalentados, personas que creen que por alguna razon no conseguiran trabajo" 


*Modificacion MGD 06/24/2014: mal generada la variable
gen subemp_ci=0
replace subemp_ci=1 if (hrsnort>=1 & hrsnort<=30) & pref30h==1 & emp_ci==1
label var subemp "Trabajadores subempleados"

gen tiempoparc_ci=.
replace tiempoparc_ci=1 if hrsnort>=0 & hrsnort<=30 & pref30h==2
replace tiempoparc_ci=. if (hrsnort==. | pref30h==. | hrsnort<0 | pref30h<0)
label var tiempoparc_ci "Trabajadores a medio tiempo"

* Modificacion MGD 07/14/2014: Condicionado a que esten ocupados.
gen categopri_ci=.
replace categopri_ci=1 if categp==7 & condocup_ci==1
replace categopri_ci=2 if (categp==6 | categp==5) & condocup_ci==1
replace categopri_ci=3 if categp>=1 & categp<=4 & condocup_ci==1
replace categopri_ci=4 if (categp==8 | categp==9) & condocup_ci==1
label var categopri_ci "CATEGORIA OCUPACIONAL ACTIVIDAD PRINCIPAL"
label define categopri_ci 1 "Patron" 2 "Cuenta Propia" 3 "Asalariado" 4 "Trabajador No Remunerado" 
label values categopri_ci categopri_ci

gen categosec_ci=.
label var categosec_ci "CATEGORIA OCUPACIONAL ACTIVIDAD SECUNDARIA"

capture drop nempleos_ci
gen byte nempleos_ci=.
replace nempleos=1 if emp==1 & masdeuno==1
replace nempleos=2 if emp==1 & masdeuno==2
label var nempleos "Numero de empleos"
label define nempleos_ci 1 "un trabajo" 2 "dos o mas trabajos"
label values nempleos_ci nempleos_ci

capture drop tamfirma_ci
gen byte tamfirma_ci=.
replace tamfirma_ci=1 if emp==1 & (tamestp>=2 & tamestp<=5)
replace tamfirma_ci=0 if emp==1 & tamestp==1
label var tamfirma "Trabajadores formales"
label define tamfirma_ci 1 "5 o mas trabajadores" 0 "Menos de 5 trabajadores"
label values tamfirma_ci tamfirma_ci

capture drop spublico_ci
gen byte spublico_ci=.
replace spublico=1 if emp==1 & (categp==1 | categp==2)
replace spublico=0 if emp==1 & (categp>2 & categp<=8) 
label var spublico "Personas que trabajan en el sector publico"
*************************************************************************************
*******************************INGRESOS**********************************************
*************************************************************************************
gen pp56d=yocupapm 
gen pp57=yocupam 
gen pp59g=ybonos
gen pp60h=yotros

capture drop ylmpri_ci
gen ylmpri_ci=.
replace ylmpri_ci=pp56d if pp56d!=-1 & pp56d!=-2 & pp56d!=-3 & pp56d!=.
replace ylmpri_ci=. if edad<10
label var ylmpri_ci "Ingreso Laboral Monetario de la Actividad Principal"
replace ylmpri_ci=ylmpri_ci/1000
*Y.L. divido al ingreso entre 1000 para hacerlo comparable a lo largo del tiempo

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

gen ylm_ci=.
replace ylm_ci=pp57 if (pp57>=0 & pp57!=. & pp56d>=0 & pp56d!=.) & (pp56d<=pp57)
replace ylm_ci=pp56d if (pp56d>=0 & pp56d!=.) & (pp56d>pp57)
replace ylm_ci=. if edad<10
label var ylm_ci "Ingreso Laboral Monetario Total"
replace ylm_ci=ylm_ci/1000
*Y.L. divido al ingreso entre 1000 para hacerlo comparable a lo largo del tiempo

gen hrsext=.
replace hrsext=pp59g if pp59g>=0 & pp59g!=.
replace hrsext=. if edad<10
label variable hrsext "Ingreso por Horas Extra" /* Para todos los trabajos, tambien incluye bono de transporte, bono de alimentacion, aporte patronal, comisiones y propinas */

gen ylm0_ci=.
replace ylm0_ci=pp57 if (pp57>=0 & pp57!=. & pp56d>=0 & pp56d!=.) & (pp56d<=pp57)
replace ylm0_ci=pp56d if (pp56d>=0 & pp56d!=. & pp57>0 & pp57!=.) & (pp56d>pp57)
replace ylm0_ci=pp57+pp59g if (pp56d>=0 & pp56d!=. & pp57>0 & pp57!=.) & (pp56d<=pp57) & pp59g>=0 & pp59g!=.
replace ylm0_ci=pp56d+pp59g if (pp56d>=0 & pp56d!=. & pp57!=.) & (pp56d>pp57) & pp59g>=0 & pp59g!=.
replace ylm0_ci=ylm_ci if ylm0_ci==. & ylm_ci<.
replace ylm0_ci=. if edad<10
label variable ylm0_ci "Ingreso Laboral Monetario Total + Horas Extra" 

gen ylnm_ci=.
label var ylnm_ci "Ingreso Laboral NO Monetario Total"

gen ynlm0_ci=.
replace ynlm0_ci=pp60h if pp60h!=-1 & pp60h!=-2 & pp60h!=-3 & pp60h!=.
replace ynlm0_ci=. if edad<10
label var ynlm0_ci "Ingreso NO Laboral Monetario"

gen ynlnm_ci=.
label var ynlnm_ci "Ingreso NO Laboral NO Monetario"

capture drop nrylmpri_ci
gen nrylmpri_ci=.
replace nrylmpri_ci=0 if (pp56d!=-1 & pp56d!=-2 & pp56d!=-3 & pp56d!=.)
replace nrylmpri_ci=1 if (pp56d==-2 | pp56d==-3) 
label var nrylmpri_ci "Identificador de No Respuesta del Ingreso Monetario de la Actividad Principal"

gen autocons_ci=.
label var autocons_ci "Autoconsumo Individual"

gen remesas_ci=.
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

egen ylm0_ch=sum(ylm0_ci) if miembros_ci==1, by(idh_ch)
label var ylm0_ch "Ingreso Laboral Monetario del Hogar (inc Hrs Extra)"

egen ylmnr0_ch=sum(ylm0_ci) if miembros_ci==1 & nrylmpri_ch==0, by(idh_ch)
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
replace ylm_ch =. if miembros_ci==0
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
replace ylmho0_ci=ylm0_ci/(horastot_ci*4.3)
gen tcylmpri_ci=.
gen tcylmpri_ch=.
by idh_ch, sort: egen ynlm_ch=sum(ynlm_ci) if miembros_ci==1
gen rentaimp_ch=.

****************************
***VARIABLES DE EDUCACION***
****************************

gen NIVEL=nivel
gen GRADO=grado
gen ULTSEM=ultsem
gen EDAD=edad
gen ASIST=asist

capture drop asiste_ci
gen byte asiste_ci=.
replace asiste_ci=1 if ASIST==1
replace asiste_ci=0 if ASIST==2
label var asiste "Personas que actualmente asisten a centros de enseñanza"

capture drop aedu_ci
gen byte aedu_ci=.
replace aedu=0 if NIVEL==1 | NIVEL==2
replace aedu=GRADO if NIVEL==3 & GRADO>0
replace aedu=GRADO+9 if NIVEL==4 & GRADO>0 & GRADO<=2
replace aedu=11 if NIVEL==4 & GRADO>2
replace aedu=GRADO+11 if (NIVEL==5 | NIVEL==6) & GRADO>0 
replace aedu=int(ULTSEM/2)+11 if (NIVEL==5 | NIVEL==6) & ULTSEM>0 
label variable aedu_ci "Años de Educacion"


* Unfortunately, we found people with more years of education that years of life. 
* Then, assuming that everyone enters to school not before 5 years old. To correct this:
forvalues i=0(1)18 {
if `i'==0 {
replace aedu=`i' if (aedu>`i' & aedu~=.) & (edad_ci==3 | edad_ci==4 | edad_ci==5)
}
if `i'~=0 {replace aedu=`i' if (aedu>`i' & aedu~=.) & edad_ci==(`i'+5)
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
replace edus1i=1 if edusi==1 & (NIVEL==3 & (GRADO==7 | GRADO==8))
label var edus1i_ci "1 = personas que no han completado el primer ciclo de la educacion secundaria"

gen edus1c_ci=.
replace edus1c=0 if edusi==1 | edusc==1 
replace edus1c=1 if edusi==1 & (NIVEL==3 & GRADO==9)
label var edus1c_ci "1 = personas que han completado el primer ciclo de la educacion secundaria"

gen edus2i_ci=.
replace edus2i=0 if edusi==1 | edusc==1 
replace edus2i=1 if edusi==1 & (NIVEL==4 & GRADO<2) 
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
replace pqnoasis=rznoasis if rznoasis>0
label variable pqnoasis_ci "Razones para no asistir a centros de enseñanza"
label define pqnoasis_ci 1 "Culmino sus estudios" 2 "No hay grado o agnos superiores" 3 "No hay cupo" 4 "Falta de recursos economicos" 5 "Bajo rendimiento escolar" 6 "Esta trabajando" 7 "Asiste a un curso de capacitacion" 8 "No quiere estudiar" 9 "enfermedad o defecto fisico" 10 "Problemas de conducta" 11 "Problemas de aprendizaje" 12 "Cambio de residencia" 13 "Desordenes estudiantiles" 14 "Inasistencia frecuente de maestros y profesores" 15 "Esta de vacaciones" 16 "Edad mayor que la regular" 17 "Escuela distante" 18 "Tiene que ayudar en la casa" 19 "edad menor que la regular" 20 "otros"
label values pqnoasis_ci pqnoasis_ci

**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**
	
**************
*pqnoasis1_ci*
**************
g       pqnoasis1_ci = 1 if rznoasis ==4
replace pqnoasis1_ci = 2 if rznoasis ==6
replace pqnoasis1_ci = 3 if rznoasis ==9 | rznoasis ==11
replace pqnoasis1_ci = 4 if rznoasis ==8
replace pqnoasis1_ci = 5 if rznoasis ==18
replace pqnoasis1_ci = 6 if rznoasis ==1
replace pqnoasis1_ci = 7 if rznoasis ==16 
replace pqnoasis1_ci = 8 if rznoasis ==2  | rznoasis ==3 | rznoasis ==14 | rznoasis ==17
replace pqnoasis1_ci = 9 if rznoasis ==5  | rznoasis ==7 | rznoasis ==10 | rznoasis ==12 | rznoasis ==13 | rznoasis ==15 | rznoasis ==19

label define pqnoasis1_ci 1 "Problemas económicos" 2 "Por trabajo" 3 "Problemas familiares o de salud" 4 "Falta de interés" 5	"Quehaceres domésticos/embarazo/cuidado de niños/as" 6 "Terminó sus estudios" 7	"Edad" 8 "Problemas de acceso"  9 "Otros"
label value  pqnoasis1_ci pqnoasis1_ci


********************************************
***Variables de Infraestructura del hogar***
********************************************

gen aguared_ch=.
replace aguared_ch=1 if agua==1
replace aguared_ch=0 if agua==2 | agua==3 | agua==4

gen aguadist_ch=.

gen aguamala_ch=.

gen aguamide_ch=.

gen luz_ch=.
replace luz_ch=1 if elect==1
replace luz_ch=0 if elect==2

gen luzmide_ch=.

gen combust_ch=.
replace combust_ch=1 if cocinag==1
replace combust_ch=0 if cocinag==2

gen bano_ch=.
replace bano_ch=1 if sanita==1 | sanita==2 | sanita==3
replace bano_ch=0 if sanita==4

gen banoex_ch=.
replace banoex_ch=1 if banos==1
replace banoex_ch=0 if banos==2

gen des1_ch=.

gen des2_ch=.
replace des2_ch=1 if sanita==1 | sanita==2
replace des2_ch=2 if sanita==3
replace des2_ch=0 if sanita==4

gen piso_ch=.
replace piso_ch=0 if piso==3
replace piso_ch=1 if piso==1 | piso==2
replace piso_ch=2 if piso==4

gen pared_ch=.
replace pared_ch=0 if paredes==5 | paredes==6 
replace pared_ch=1 if paredes==1 | paredes==2 | paredes==3 | paredes==4
replace pared_ch=2 if paredes==7

gen techo_ch=.
replace techo_ch=0 if techo==1 
replace techo_ch=1 if techo==2 | techo==3 | techo==4 
replace techo_ch=2 if techo==5

gen resid_ch=.

gen resid2_ch=.
replace resid2_ch=1 if basura==1
replace resid2_ch=0 if basura==2

**Daniela Zuluaga- Enero 2018: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**
	
*********************
***aguamejorada_ch***
*********************
gen aguamejorada_ch =.
		
		
*********************
***banomejorado_ch***
*********************
gen  banomejorado_ch =.

gen dorm_ch=.
replace dorm_ch=nrodormv if nrodormv>=0

gen cuartos_ch=.
replace cuartos_ch=nrocuarv if nrocuarv>=0

gen cocina_ch=.

gen telef_ch=.
replace telef_ch=1 if telefono==1
replace telef_ch=0 if telefono==2

gen refrig_ch=.
replace refrig_ch=1 if nevera==1
replace refrig_ch=0 if nevera==2

gen freez_ch=.

gen auto_ch=.
replace auto_ch=1 if nroauto>0 & nroauto<.
replace auto_ch=0 if nroauto<=0

gen compu_ch=.

gen internet_ch=.

gen cel_ch=.

gen vivi1_ch=.
replace vivi1_ch=1 if tipoviv==1 | tipoviv==2 | tipoviv==5
replace vivi1_ch=2 if tipoviv==3 | tipoviv==4
replace vivi1_ch=3 if tipoviv>5 & tipoviv<.

gen vivi2_ch=.
replace vivi2_ch=1 if vivi1_ch==1 | vivi1_ch==2
replace vivi2_ch=0 if vivi1_ch==3

gen viviprop_ch=.
replace viviprop_ch=0 if tenenviv==3 | tenenviv==4
replace viviprop_ch=1 if tenenviv==1
replace viviprop_ch=2 if tenenviv==2
replace viviprop_ch=3 if tenenviv>4 & tenenviv<.

gen vivitit_ch=.

gen vivialq_ch=.
replace vivialq_ch=montobs if montobs>=0

gen vivialqimp_ch=.


drop EDAD NIVEL GRADO ULTSEM ASIST

**********************
*** VENEZUELA 1995 ***
**********************

/* 
Relación de parentesco
 1. Jefe del hogar
 2. Esposa(o), compañero(a)
 3. Hijos(as), hijastros(as)
 4. Nietos(as)
 5. Yernos, nueras
 6. Padre, madre
 7. Suegro(a)
 8. Hermano(a)
 9. Cuñado(a)
 10. Sobrino(a)
 11. Tío(a)
 12. Primo(a)
 13. Abuelo(a)
 14. Otro pariente
 15. No pariente
 16. Servicio doméstico
 17. Familiares del servicio doméstico
*/

 gen 	 incl=1 if (parentco>=1  & parentco<=15)
 replace incl=0 if (parentco>=16 & parentco<=17)

* Variables

 rename area usm
 rename zona area /* 1. Urbana 2. Rural */
 rename pesop factorex

 
** Gender classification of the population refering to the head of the household.

* Household ID 

 gen vectoruno=1
   
 sort id_hogar parentco nroper
 
 by id_hogar: gen nper=sum(vectoruno)
 
 sort id_hogar nper

 gen     sexo_d_=1 if parentco==1 & sexo==1
 replace sexo_d_=2 if parentco==1 & sexo==2

 egen sexo_d=max(sexo_d_), by(id_hogar)
 
** Years of education. 

/*
alfabet ¿Sabe leer y escribir? (pp17)

NIVEL (pp18a)	
-3: No recuerda
-2: No responde
-1: No aplicable
 1: Sin nivel
 2: Preescolar
 3: Básica
 4: Media diversificada y profesional
 5: Técnico superior
 6: Universitario

GRADO (pp18b) 
-3 a 9

ULTSEM (pp19c)
-3 a 14
*/

drop anoest

 gen	 anoest=0  if (nivel==1) | (nivel==2)
 replace anoest=1  if (nivel==3 & grado==1) | (nivel==3 & (grado==-3 | grado==-2)) 
 replace anoest=2  if (nivel==3 & grado==2)
 replace anoest=3  if (nivel==3 & grado==3)
 replace anoest=4  if (nivel==3 & grado==4)
 replace anoest=5  if (nivel==3 & grado==5)
 replace anoest=6  if (nivel==3 & grado==6)
 replace anoest=7  if (nivel==3 & grado==7)
 replace anoest=8  if (nivel==3 & grado==8)
 replace anoest=9  if (nivel==3 & grado==9)
 replace anoest=10 if (nivel==4 & grado==1) | (nivel==4 & (grado==-3 | grado==-2))
 replace anoest=11 if (nivel==4 & grado==2)
 replace anoest=12 if ((nivel==6 | nivel==5) & (grado==1 | (ultsem==1  | ultsem==2))) | (nivel==4 & grado==3) | ((nivel==6 | nivel==5) & ((ultsem==-3 | ultsem==-2) | (grado==-3 | grado==-2)))
 replace anoest=13 if ((nivel==6 | nivel==5) & (grado==2 | (ultsem==3  | ultsem==4)))
 replace anoest=14 if ((nivel==6 | nivel==5) & (grado==3 | (ultsem==5  | ultsem==6)))
 replace anoest=15 if ((nivel==6 | nivel==5) & (grado==4 | (ultsem==7  | ultsem==8)))
 replace anoest=16 if ((nivel==6 | nivel==5) & (grado==5 | (ultsem==9  | ultsem==10)))
 replace anoest=17 if ((nivel==6 | nivel==5) & (grado==6 | (ultsem==11 | ultsem==12)))
 replace anoest=18 if ((nivel==6 | nivel==5) & (grado==7 | (ultsem==13 | ultsem==14)))

** Economic Active Population  (10 years or more of age)
/*
actvsum	
CONDICIÓN SUMARIA DE ACTIVIDAD
1. Trabajo	
2. No trabajo pero tiene Trebajo	
3. Busca Trabajo	
4. Estudiante 	
5. Oficios del Hogar	
6. Otra Situación	
7. Incapacidad para Trabajar	
8. Buscando Trabajo por Primera vez 	
9. Desocupado que no  Busca Trabajo	

*/

 gen	 peaa=1 if (actvsum==1 | actvsum==2)
 replace peaa=2 if (actvsum==3 | actvsum==8 | actvsum==9)
 replace peaa=3 if (actvsum==4 | actvsum==5 | actvsum==6 | actvsum==7)
 
 gen	 TASADESO=0 if peaa==1
 replace TASADESO=1 if peaa==2

 destring entidad, replace
 
 gen	 region=1 if entidad==1  | entidad==13 
 replace region=2 if entidad==4  | entidad==7  | entidad==8
 replace region=3 if entidad==3  | entidad==10
 replace region=4 if entidad==9  | entidad==11 | entidad==16 | entidad==20
 replace region=5 if entidad==21
 replace region=6 if entidad==5  | entidad==12 | entidad==18 | entidad==19
 replace region=7 if entidad==2  | entidad==14 | entidad==15 | entidad==17
 replace region=8 if entidad==6  | entidad==22 | entidad==23


************************
*** MDGs CALCULATION ***
************************

/*
alfabet ¿Sabe leer y escribir? (pp17)

NIVEL (pp18a)	
-3: No recuerda
-2: No responde
-1: No aplicable
 1: Sin nivel
 2: Preescolar
 3: Básica
 4: Media diversificada y profesional
 5: Técnico superior
 6: Universitario

GRADO (pp18b) 
-3 a 9

ULTSEM (pp19c)
-3 a 14

ASIST (pp20)
Personas entre 3 y 21 años
Asistencia a un centro de educación

*/

gen asiste = asist

*** GOAL 2. ACHIEVE UNIVERSAL PRIMARY EDUCATION

** Target 3, Indicator: Net Attendance Ratio in Primary
* ISCED 1

 gen	 NERP=0 if (edad>=6 & edad<=11) & (asiste==1 | asiste==2)  
 replace NERP=1 if (edad>=6 & edad<=11) & (asiste==1) & ((nivel==2) | (nivel==3 & (grado>=1 & grado<=5)))
	
** Target 3, Additional Indicator: Net Attendance Ratio in Secondary
* ISCED 2 & 3

 gen	 NERS=0 if (edad>=12 & edad<=16) & (asiste==1 | asiste==2) 
 replace NERS=1 if (edad>=12 & edad<=16) & (asiste==1) & ((nivel==3 & (grado>=6 & grado<=9)) | (nivel==4 & grado==1))

* Upper secondary
* Media diversificada y profesional

 gen	 NERS2=0 if (edad>=15 & edad<=16) & (asiste==1 | asiste==2)
 replace NERS2=1 if (edad>=15 & edad<=16) & (asiste==1) & ((nivel==3 & grado==9) | (nivel==4 & grado==1))
	
** Target 3, Indicator: Literacy Rate of 15-24 Years Old
* At least 5 years of formal education

 gen	 ALFABET=0 if (edad>=15 & edad<=24) & (anoest>=0 & anoest<99) 
 replace ALFABET=1 if (edad>=15 & edad<=24) & (anoest>=5 & anoest<99) 

** Target 3, Indicator: Literacy Rate of 15-24 Years Old
* Read & write

 gen	 ALFABET2=0 if (edad>=15 & edad<=24) & (alfabet==1 | alfabet==2)
 replace ALFABET2=1 if (edad>=15 & edad<=24) & (alfabet==1)

*** GOAL 3 PROMOTE GENDER EQUALITY AND EMPOWER WOMEN

 gen prim=1 if  (asiste==1) & ((nivel==2) | (nivel==3 & (grado>=1 & grado<=5)))
 gen sec=1  if  (asiste==1) & ((nivel==3 & (grado>=6 & grado<=9)) | (nivel==4 & grado==1))
 gen ter=1  if  (asiste==1) & (anoest>=11 & anoest<=15)

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

 gen MA=1 if ((anoest>=5 & anoest<99) & (edad>=15 & edad<=24) & (sexo==2)) 
 replace MA=0 if MA==.
 gen HA=1 if ((anoest>=5 & anoest<99) & (edad>=15 & edad<=24) & (sexo==1)) 
 replace HA=0 if HA==.

 gen     RATIOLIT=0 if ((anoest>=5 & anoest<99) & (edad>=15 & edad<=24) & (sexo==2)) 
 replace RATIOLIT=1 if ((anoest>=5 & anoest<99) & (edad>=15 & edad<=24) & (sexo==1)) 

** Target 4, Indicator: Ratio of literate women to men 15-24 year olds*
* Knows how to read & write

 gen MA2=1 if ((alfabet==1) & (edad>=15 & edad<=24) & (sexo==2)) 
 replace MA2=0 if MA2==.
 gen HA2=1 if ((alfabet==1) & (edad>=15 & edad<=24) & (sexo==1)) 
 replace HA2=0 if HA2==.

 gen     RATIOLIT2=0 if ((alfabet==1) & (edad>=15 & edad<=24) & (sexo==2)) 
 replace RATIOLIT2=1 if ((alfabet==1) & (edad>=15 & edad<=24) & (sexo==1)) 
	
** Target 4, Indicator: Share of women in wage employment in the non-agricultural sector (%)

/*
PP49 CATEGORIA DE OCUPACIÓN(categp) 	PP45a RAMAPRIN	PP44a OCUPACION (OCUPPRIN)
49: En su trabajo principal es (o era:)			81. Trabajadores de servicios 	
							domésticos (en hogares particulares)
1: Empleado gubernamental
2: Obrero gubernamental
3: Empleado en empresa particular
4: Obrero en empresa particular
5: Miembro de cooperativa
6: Trabaja por cuenta propia
7: Patrono o empleador
8: Ayudante no familiar no remunerado
9: Ayudante familiar no remunerado
*/

* Without Domestic Service

 gen	 WENAS=0 if (edad>=15 & edad<=64) & (categp>=1 & categp<=4) & (ramaprin>=210 & ramaprin<=960) & (peaa==1) & (ocuprin!=81)
 replace WENAS=1 if (edad>=15 & edad<=64) & (categp>=1 & categp<=4) & (ramaprin>=210 & ramaprin<=960) & (peaa==1) & (ocuprin!=81) & (sexo==2)

** Target 4, Indicator: Share of women in wage employment in the non-agricultural sector (%)
* With domestic servants

 gen     WENASD=0 if (edad>=15 & edad<=64) & (categp>=1 & categp<=4) & (ramaprin>=210 & ramaprin<=960) & (peaa==1)
 replace WENASD=1 if (edad>=15 & edad<=64) & (categp>=1 & categp<=4) & (ramaprin>=210 & ramaprin<=960) & (peaa==1) & (sexo==2)

*** GOAL 7 ENSURE ENVIROMENTAL SUSTAINABILITY

/*
tipoviv. Tipo de vivienda
 1. Quinta (o casa quinta 2003)
 2. Casa
 3. Apartamento en edificio
 4. Apartamento en quinta o casa-quinta (ó casa solo 2003)
 5. Casa de vecindad
 6. Vivienda Rustica o (Rancho)
 7. Rancho campesino
 8. Otro tipo ==> -1 in other dwelling variables
 9: Colectividad
*/	

 gen	 excl_hou=1 if tipoviv==8
 recode  excl_hou (.=0)

** Electricity. Additional Indicator

/*
 12: Posee esta vivienda los siguientes servicios: 
 (Puede marcar más de una opción)
SERVICIO ELÉCTRICO PÚBLICO
*/

* Gender classification of the population refers to the head of the household.

 gen	 ELEC=0 if (elect==1 | elect==2) /* Total population excluding missing information */
 replace ELEC=1 if (elect==1)

** Target 10, Indicator: Proportion of the population with sustainable access to an improved water source (%)

/*
AGUA. Agua
8: A esta vivienda llega el agua por:
-1. No aplicable
 1. Acueducto
 2. Pila pública 
 3. Camión
 4. Otros medios
*/

* Gender classification of the population refers to the head of the household.

 gen	 WATER=0 if (agua>=1 & agua<=4) /* Total population excluding missing information */
 replace WATER=1 if (agua>=1 & agua<=2)
 	
** Target 10, Indicator: Proportion of Population with Access to Improved Sanitation, Urban and Rural (%)
/*
SANITA. excretas
9: Esta vivienda tiene:
-1. No aplicable
 1. Poceta a cloaca
 2. Poceta a pozo séptico
 3. Excusado a hoyo o letrina
 4. No tiene poceta o excusado
*/

* Gender classification of the population refers to the head of the household.

 gen	 SANITATION=0 if (sanita>=1 & sanita<=4) /* Total population excluding missing information */
 replace SANITATION=1 if (sanita>=1 & sanita<=2)
 	
** Target 11, Indicator: Proportion of the population with access to secure tenure (%)

/*
tipoviv. Tipo de vivienda
 1. Quinta (o casa quinta 2003)
 2. Casa
 3. Apartamento en edificio
 4. Apartamento en quinta o casa-quinta (ó casa solo 2003)
 5. Casa de vecindad
 6. Vivienda Rustica o (Rancho)
 7. Rancho campesino
 8. Otro tipo ==> -1 in other dwelling variables
 9: Colectividad

tenenviv. Tenencia de la vivienda	
6: Para este hogar la vivienda es:

-3: No recuerda
-2: No responde
 1: Propia pagada totalmente
 2: Propia pagándose
 3: Alquilada
 4: Alquilada parte de la vivienda
 5: Cedida por razones de trabajo
 6: Cedida por familiar o amigo
 7: Tomada
 8: Otra forma

paredes. Paredes
2: El material predominante en las paredes exteriores es:

-1: No aplicable
 1: Bloque o ladrillo frisado
 2: Bloque o ladrillo sin frisar
 3: Concreto (prefabricado)
 4: Madera aserrada
 5: Adobe, tapia o bahareque frisado
 6: Adobe, tapia o bahareque sin frisar
 7: Otros (caña, palma, tablas y similares)

piso. Piso
4: El material predominante en el piso es:
-1: No aplicable
 1: Mosaico, granito, vinil, cerámica, ladrillo, terracota, parquet, alfombra y similares
 2: Cemento
 3: Tierra
 4: Otros (tablas, tablones y similares)

nrocuarv. Número de cuartos
Contando sala, comedor, cuartos para dormir y otros cuartos.
¿Cuántos cuartos en total tiene esta vivienda?
-1. No aplicable
*/

 gen nrocuart=nrocuarv if nrocuarv>0

 gen persroom=pers/nrocuart 

* Indicator components

* 1. Non secure tenure or type of dwelling.

 gen secten_1=0     if ((tenenviv>=1 & tenenviv<=8) & (tipoviv>=1 & tipoviv<=9)) /* Total population excluding missing information */
 replace secten_1=1 if ((tenenviv>=5 & tenenviv<=8) | (tipoviv>=6 & tipoviv<=9))

* 2. Low quality of the floor or walls materials.

 gen secten_2=0     if ((paredes>=1 & paredes<=7) & (piso>=1 & piso<=4))         /* Total population excluding missing information */
 replace secten_2=1 if ((paredes>=6 & paredes<=7) | (piso>=3 & piso<=4))

* 3. Crowding (defined as not more than two people sharing the same room)

 gen secten_3=1     if (persroom>2) 
 
* 4. Lack of basic services

 gen secten_4=1	   if (SANITATION==0 | WATER==0) 

* Gender classification of the population refers to the head of the household.

 gen     SECTEN=1 if  (secten_1>=0 & secten_1<=1) & (secten_2>=0 & secten_2<=1) /* Total population excluding missing information */
 replace SECTEN=0 if  (secten_1==1 | secten_2==1 | secten_3==1 | secten_4==1)

* Dirt floors ** Additional indicator

* Gender classification of the population refers to the head of the household.

 gen	 DIRT=0 if (piso>=1 & piso<=4)
 replace DIRT=1 if (piso==3)

** GOAL 8. DEVELOP A GLOBAL PARTNERSHIP FOR DEVELOPMENT

** Target 16, Indicator: Unemployment Rate of 15 year-olds (%)

 gen	 UNMPLYMENT15=0 if (TASADESO==0 | TASADESO==1) & (edad>=15 & edad<=24)
 replace UNMPLYMENT15=1 if (TASADESO==1) 	       & (edad>=15 & edad<=24)

** Target 18, Indicator: "Telephone lines and celullar subscribers per 100 population"
/*
11.4: Posee esta vivienda los siguientes servicios: 
telefono
*/

* Gender classification of the population refers to the head of the household.

 gen     TELCEL=0 if  (telefono==1 | telefono==2)
 replace TELCEL=1 if  (telefono==1)

** FIXED LINES

* Gender classification of the population refers to the head of the household.

 gen     TEL=0 if  (telefono==1 | telefono==2)
 replace TEL=1 if  (telefono==1)
	
*************************************************************************
**** ADDITIONAL SOCIO - ECONOMIC COMMON COUNTRY ASESSMENT INDICATORS ****
*************************************************************************

** CCA 19. Proportion of children under 15 who are working
* 12 to 14

 gen     CHILDREN=0 if (edad>=12 & edad<=14) 
 replace CHILDREN=1 if (edad>=12 & edad<=14) & peaa==1

** CCA 41 Number of Persons per Room*

 generate PERSROOM2=persroom if parentco==1

 gen 	 popinlessthan2=1 if persroom<=2
 replace popinlessthan2=0 if popinlessthan2==.

* Gender classification of the population refers to the head of the household.

 gen     PLT2=0 if persroom<. 		/* Total population excluding missing information */
 replace PLT2=1 if (popinlessthan2==1)

******************************************************
**** ADDITIONAL INDICATORS RELATED TO EDUCATION ******
******************************************************

*** Rezago escolar

 gen	 rezago=0	if (anoest>=0 & anoest<99)  & edad==6 /* This year of age is not included in the calculations */
	 
 replace rezago=1 	if (anoest>=0 & anoest<1 )  & edad==7
 replace rezago=0 	if (anoest>=1 & anoest<99)  & edad==7

 replace rezago=1 	if (anoest>=0 & anoest<2 )  & edad==8
 replace rezago=0	if (anoest>=2 & anoest<99)  & edad==8

 replace rezago=1 	if (anoest>=0 & anoest<3 )  & edad==9
 replace rezago=0	if (anoest>=3 & anoest<99)  & edad==9

 replace rezago=1 	if (anoest>=0 & anoest<4 )  & edad==10
 replace rezago=0	if (anoest>=4 & anoest<99)  & edad==10

 replace rezago=1 	if (anoest>=0 & anoest<5 )  & edad==11
 replace rezago=0	if (anoest>=5 & anoest<99)  & edad==11

 replace rezago=1	if (anoest>=0 & anoest<6)   & edad==12
 replace rezago=0	if (anoest>=6 & anoest<99)  & edad==12

 replace rezago=1 	if (anoest>=0 & anoest<7)   & edad==13
 replace rezago=0	if (anoest>=7 & anoest<99)  & edad==13

 replace rezago=1 	if (anoest>=0 & anoest<8)   & edad==14
 replace rezago=0	if (anoest>=8 & anoest<99)  & edad==14

 replace rezago=1 	if (anoest>=0 & anoest<9 )  & edad==15
 replace rezago=0	if (anoest>=9 & anoest<99)  & edad==15
 
 replace rezago=1 	if (anoest>=0  & anoest<10) & edad==16
 replace rezago=0	if (anoest>=10 & anoest<99) & edad==16
 
* Primary and Secondary [ISCED 1, 2 & 3]

 gen 	 REZ=0 if  (edad>=7 & edad<=16) & (rezago==1 | rezago==0)
 replace REZ=1 if  (edad>=7 & edad<=16) & (rezago==1)

* Primary completion rate [15 - 24 years of age]

 gen     PRIMCOMP=0 if  (edad>=15 & edad<=24) & (anoest>=0  & anoest<99)
 replace PRIMCOMP=1 if  (edad>=15 & edad<=24) & (anoest>=6  & anoest<99)
	
* Average years of education of the population 15+

 gen     AEDUC_15=anoest if  ((edad>=15) & (anoest>=0 & anoest<99))
	
 gen     AEDUC_15_24=anoest if  ((edad>=15 & edad<=24) & (anoest>=0 & anoest<99))

 gen     AEDUC_25=anoest if  ((edad>=25) & (anoest>=0 & anoest<99))
	
* Grade for age

 gen GFA=(anoest/(edad-6)) if (edad>=7 & edad<=16) & (anoest>=0 & anoest<99)
	
* Grade for age primary

 gen GFAP=(anoest/(edad-6)) if (edad>=7 & edad<=11) & (anoest>=0 & anoest<99)
	
* Grade for age Secondary

 gen GFAS=(anoest/(edad-6)) if (edad>=12 & edad<=16) & (anoest>=0 & anoest<99)
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



