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
local ANO "2000"
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

************
****pais****
************
gen str pais_c="VEN"

***************
****anio_c ****
***************
gen anio_c=2000

*********
***mes***
*********
gen mes_c=.
/* No se cuenta con informacion especifica sobre la semana de planificacion para esta encuesta */
replace mes_c= 7  if SEMAPLA>=1  & SEMAPLA<=4
replace mes_c= 8  if SEMAPLA>=5  & SEMAPLA<=8
replace mes_c= 9  if SEMAPLA>=9  & SEMAPLA<=12
replace mes_c= 10 if SEMAPLA>=13 & SEMAPLA<=16
replace mes_c= 11 if SEMAPLA>=17 & SEMAPLA<=20
replace mes_c= 12 if SEMAPLA>=21 & SEMAPLA<=24
*** average week of the survey is 11 which means mes==9
replace mes_c =9  if mes_c==.
label var mes_c "Mes de la Encuesta: Segundo Semestre de 2000"
label define mes_c 7 "JUL" 8 "AUG" 9 "SEP" 10 "OCT" 11 "NOV" 12 "DEC" 
label values mes_c mes_c

**********
***zona***
**********
gen zona_c=.
replace zona_c=1 if DOMINIO==1 | DOMINIO==2 | DOMINIO==3 | DOMINIO==4
recode zona_c .=0
label define zona_c 0 "Rural" 1 "Urbana"
label value zona_c zona_c

****************
*** idh_ch ***
****************
sort ENTIDAD CONTROL AREA LINEA NROHOGSV SUBDOM LOCALI NROHOG
egen idh_ch=group( ENTIDAD CONTROL AREA LINEA NROHOGSV SUBDOM LOCALI NROHOG)
label var idh_ch "Identificador Unico del Hogar"
gen idp_ci=NROPER
label var idp_ci "Identificador Individual dentro del Hogar"

***************
***factor_c***
***************
gen factor_ch=FACTORH
label var factor_ch "Factor de expansion del Hogar"
gen factor_ci=FACTORP
label var factor_ci "Factor de Expansion del Individuo"

***************
***upm_ci***
***************
clonevar upm_ci=CONTROL
label variable upm_ci "Unidad Primaria de Muestreo"

***************
***estrato_ci***
***************
gen estrato_ci=ESTRATO
label variable estrato_ci "Estrato"

***********
* Region_c *
************
* YL: En este año se considera la antigua division política administrativa (Que existía antes del 2001)
gen region_c=  ENTIDAD
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

*************
*** ine01 ***
*************
gen ine01=ENTIDAD
label define ine01 1 "Distrito Capital" 2 "Amazonas" 3 "Anzoategui" 4 "Apure" 5 "Aragua" 6 "Barinas" 7 "Bolivar" 8 "Carabobo" 9 "Cojedes" 10 "Delta Amacuro" 11 "Falcon" 12 "Guarico" 13 "Lara" 14 "Merida" 15 "Miranda" 16 "Monagas" 17 "Nueva Esparta" 18 "Portuguesa" 19 "Sucre" 20 "Tachira" 21 "Trujillo" 22 "Yaracuy" 23 "Zulia" 24 "Vargas"
label value ine01 ine01

************************
*** region según BID ***
************************
gen region_BID_c=3 
label var region_BID_c "Regiones BID"
label define region_BID_c 1 "Centroamérica_(CID)" 2 "Caribe_(CCB)" 3 "Andinos_(CAN)" 4 "Cono_Sur_(CSC)"
label value region_BID_c region_BID_c

		****************************
		***VARIABLES DEMOGRAFICAS***
		****************************

*****************
***relacion_ci***
*****************
gen relacion_ci=.
replace relacion_ci=1 if PARENT==1
replace relacion_ci=2 if PARENT==2
replace relacion_ci=3 if PARENT==3
replace relacion_ci=4 if PARENT>=4 & PARENT<=14 /* Otros familiares */
replace relacion_ci=5 if PARENT==15  
replace relacion_ci=6 if PARENT==16 | PARENT==17 /*Es el sevicio domestico, Incluye a familiares del Serv. Domestico en PARENT==17 */
label var relacion_ci "Relacion con el Jefe de Hogar"
label define relacion_ci 1 "Jefe de Hogar" 2 "Conyuge/Pareja" 3 "Hijo(a)/Hijastro(a)" 4 "Otros Parientes" 5 "Otros No parientes" 6 "Servicio Domestico (inc fam Serv. Dom.)"
label value relacion_ci relacion_ci

**********
***sexo***
**********
gen sexo_ci=SEXO
label var sexo "Sexo del Individuo"
label define sexo_ci 1 "Masculino" 2 "Femenino"
label value sexo_ci sexo_ci

**********
***edad***
**********
gen edad_ci=EDAD
replace edad=. if EDAD==99
label var edad_ci "Edad del Individuo"

**************
***civil_ci***
**************
gen byte civil_ci=.
replace civil_ci=1 if SITCOYU==-1 | SITCOYU==7
replace civil_ci=2 if SITCOYU==1 | SITCOYU==2 | SITCOYU==3 | SITCOYU==4
replace civil_ci=3 if SITCOYU==5
replace civil_ci=4 if SITCOYU==6
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


		*******************************************************
		***           VARIABLES DE DIVERSIDAD               ***
		*******************************************************				
		* Maria Antonella Pereira & Nathalia Maya - Julio 2021	
		
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

*******************
***dis_ci***
*******************
gen dis_ci=. 

*******************
***dis_ch***
*******************
gen dis_ch=. 


		************************************
		*** VARIABLES DEL MERCADO LABORAL***
		************************************
		
**************
***ocupa_ci***
**************
capture drop ocupa_ci 
gen ocupa_ci=.
replace ocupa_ci=1 if OCUPP>=0 & OCUPP<=9
replace ocupa_ci=2 if OCUPP>=10 & OCUPP<=19
replace ocupa_ci=3 if OCUPP>=20 & OCUPP<=23
replace ocupa_ci=4 if OCUPP>=25 & OCUPP<=29
replace ocupa_ci=6 if OCUPP>=30 & OCUPP<=35
replace ocupa_ci=7 if OCUPP>=40 & OCUPP<=79
replace ocupa_ci=5 if OCUPP>=80 & OCUPP<=89
replace ocupa_ci=8 if OCUPP>=90 & OCUPP<=91
replace ocupa_ci=9 if OCUPP>=99

label variable ocupa_ci "Ocupacion Laboral en la Actividad Principal"
label define ocupa_ci 1 "PROFESIONALES Y TECNICOS" 2 "GERENTES, DIRECTORES Y FUNCIONARIOS SUPERIORES"  3 "PERSONAL ADMINISTRATIVO Y NIVEL INTERMEDIO" 4 "COMERCIANTES Y VENDEDORES" 5 "TRABAJADORES EN SERVICIOS" 6"TRABAJADORES AGRICOLAS Y AFINES" 7 "OBREROS NO AGRICOLAS, CONDUCTORES DE MAQUINAS Y VEHICULOS DE TRANSPORTE Y SIMILARES" 8"FUERZAS ARMADAS" 9"OTRAS OCUPACIONES NO CLASIFICADAS ANTERIORMENTE"
label values ocupa_ci ocupa_ci

*****************
***horastot_ci***
*****************
gen byte horastot_ci=.
replace horastot_ci=HRSNORTP  if HRSNORTP<=110 & HRSNORTP>=0 & HRSNORTP>=HRSTOTP
replace horastot_ci=HRSTOTP if HRSTOTP>=HRSNORTP & HRSTOTP>=0
label var horastot_ci "Horas totales trabajadas la semana pasada en todas las Actividades"

****************
****condocup_ci*
****************
/*
gen condocup_ci=.
replace condocup_ci=1 if (ACTVSUM>=1 & ACTVSUM <=3) 
replace condocup_ci=2 if ACTVSUM==4 | ACTVSUM==11 
replace condocup_ci=3 if condocup_ci!=1 & condocup_ci!=2
replace condocup_ci=4 if edad_ci<15
label define condocup_ci 1"ocupados" 2"desocupados" 3"inactivos" 4"menor de PET"
label value condocup_ci condocup_ci
label var condocup_ci "Condicion de ocupacion utilizando definicion del pais"
*/

* Cambio edad minima de la encuesta (10 años). MGD 06/10/2014
gen condocup_ci=.
replace condocup_ci=1 if (ACTVSUM>=1 & ACTVSUM <=3) 
replace condocup_ci=2 if ACTVSUM==4 | ACTVSUM==11 
replace condocup_ci=3 if (condocup_ci!=1 & condocup_ci!=2) & edad_ci>=10
replace condocup_ci=4 if edad_ci<10
label define condocup_ci 1"ocupados" 2"desocupados" 3"inactivos" 4"menor de PET"
label value condocup_ci condocup_ci
label var condocup_ci "Condicion de ocupacion utilizando definicion del pais"

******************
***categopri_ci***
******************
* Modificacion MGD 07/14/2014: Condicionado a que esten ocupados.
gen categopri_ci=.
replace categopri_ci=1 if CATEGP==7 & condocup_ci==1
replace categopri_ci=2 if CATEGP==6 | CATEGP==5 & condocup_ci==1
replace categopri_ci=3 if CATEGP>=1 & CATEGP<=4  & condocup_ci==1
replace categopri_ci=4 if CATEGP==8 & condocup_ci==1
label var categopri_ci "CATEGORIA OCUPACIONAL ACTIVIDAD PRINCIPAL"
label define categopri_ci 1 "Patron" 2 "Cuenta Propia" 3 "Asalariado" 4 "Trabajador No Remunerado" 
label values categopri_ci categopri_ci

****************
*afiliado_ci****
****************
gen afiliado_ci=.
label var afiliado_ci "Afiliado a la Seguridad Social"

****************
*cotizando_ci***
****************
gen cotizando_ci=0     if condocup_ci==1 | condocup_ci==2 
foreach var of varlist BENEFA  BENEFB BENEFC {
replace cotizando_ci=1 if (`var'==3) & cotizando_ci==0 /*solo a emplead@s y asalariad@s, difiere con los otros paises*/
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
gen tamemp_ci=TAMESTP
label define tamemp_ci 1"menos de 5 personas" 2"5-10 personas" 3"11-20 personas" 4"Más de 20 personas"
label var tamemp_ci "# empleados en la empresa de la actividad principal"
*/
gen tamemp_ci=1 if TAMESTP==1 
label var  tamemp_ci "Tamaño de Empresa" 
*Empresas medianas
replace tamemp_ci=2 if TAMESTP==2 | TAMESTP==3
*Empresas grandes
replace tamemp_ci=3 if TAMESTP==4
label define tamaño 1"Pequeña" 2"Mediana" 3"Grande"
label values tamemp_ci tamaño
tab tamemp_ci [iw=factor_ci]
/*
*Genera la variable para clasificar a los inactivos
*Jubilados, pensionados e incapacitados

gen categoinac_ci=1 if TRABAJA==7
label var  categoinac_ci "Condición de Inactividad" 
*Estudiantes
replace categoinac_ci=2 if TRABAJA==5
*Quehaceres del Hogar
replace categoinac_ci=3 if TRABAJA==6
*Otra razon
replace categoinac_ci=4 if TRABAJA==2 | TRABAJA==8 | TRABAJA==9 | TRABAJA==10
label define inactivo 1"Pensionado y otros" 2"Estudiante" 3"Hogar" 4"Otros"
label values categoinac_ci inactivo
tab categoinac_ci [iw=factor_ci]
*/

*****************
*categoinac_ci***
*****************
gen categoinac_ci = .
replace categoinac_ci = 1 if ((PQNOBUS==7) & condocup_ci==3)
replace categoinac_ci = 2 if ((PQNOBUS==5) & condocup_ci==3)
replace categoinac_ci = 3 if ((PQNOBUS==6) & condocup_ci==3)
replace categoinac_ci = 4 if  ((categoinac_ci ~=1 & categoinac_ci ~=2 & categoinac_ci ~=3) & condocup_ci==3)
label var categoinac_ci "Categoría de inactividad"
label define categoinac_ci 1 "Jubilados o pensionados" 2 "Estudiantes" 3 "Quehaceres domésticos" 4 "Otros"
label values categoinac_ci categoinac_ci

*************
**pension_ci*
*************
gen pension_ci=0 
foreach var of varlist YOTROSA YOTROSB YOTROSC YOTROSD YOTROSE YOTROSF YOTROSG YOTROSH YOTROSI {
replace pension_ci=1 if (`var'==1 | `var'==5 | `var'==6 ) /*A todas las per mayores de diez años */
}
label var pension_ci "1=Recibe pension contributiva"
 
*************
*  ypen_ci  *
*************
gen ypen_ci=YOTROS/1000 if pension_ci==1
replace ypen_ci=. if YOTROS<0
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
replace cesante_ci=1 if (CESANTE==1) & condocup_ci==2
label var cesante_ci "Desocupado - definicion oficial del pais"

*********
*lp_ci***
*********
gen lp_ci=.
replace lp_ci=59405.2 if zona_c==1
replace lp_ci=47524.1 if zona_c==0
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
* 2015 MGD: actualizados salarios
gen salmm_ci=.
replace salmm_ci=138000/1000 if zona_c==1 /*en Bs*/
replace salmm_ci=129600/1000 if zona_c==0 
label var salmm_ci "Salario minimo legal"
*Y.L. divido al salmm_ci entre 1000 para hacerlo comparable a lo largo del tiempo

*************
***tecnica_ci**
*************
gen tecnica_ci=(NIVEL==5)
label var tecnica_ci "=1 formacion terciaria tecnica"	

************
***emp_ci***
************
gen emp_ci=(condocup_ci==1)
label var emp_ci "Empleado en la semana de referencia"

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

*****************
***horaspri_ci***
*****************
capture drop horaspri_ci
gen byte horaspri_ci=.
replace horaspri_ci=HRSTOTP if HRSTOTP<=110 & HRSTOTP>=0
label var horaspri_ci "Horas totales trabajadas la semana pasada en la Actividad Principal"

****************
***durades_ci***
****************
* Modificacion MGD 07/11/2014: si hay la variable, para este anio es la pp41a/b.
g mesess=MESESINT if MESESINT>0
g anioss=ANOSINT*12 if ANOSINT>0
egen durades_ci = rsum(mesess anioss), missing
replace durades_ci=. if condocup_ci==3
*Se ponen como missing values las personas que llevan más tiempo desempleadas que tiempo de vida:
gen edad_meses=edad_ci*12
replace durades_ci=. if durades_ci>edad_meses
drop edad_meses
label var durades "Duracion del Desempleo (en meses)"

*****************
***desalent_ci***
*****************
capture drop desalent_ci
gen byte desalent_ci=0
replace desalent_ci=1 if (TRABAJA>4 & TRABAJA<10) & OTRACTVY==11 & TIENETRA ==2 & HECHODIL==2 & (PQNOBUS==1 | PQNOBUS==2 )
replace desalent=. if edad_ci<10
label var desalent_ci "Trabajadores desalentados, personas que creen que por alguna razon no conseguiran trabajo" 

***************
***subemp_ci***
***************
gen subemp_ci=.
label var subemp "Trabajadores subempleados"

*******************
***tiempoparc_ci***
*******************
gen tiempoparc_ci=.
label var tiempoparc_ci "Trabajadores a medio tiempo"

*************
**rama_ci ***
*************
gen rama_ci=.
replace rama_ci=1 if (RAMAP>=111 & RAMAP<=141) & emp_ci==1
replace rama_ci=2 if (RAMAP>=210 & RAMAP<=290) & emp_ci==1
replace rama_ci=3 if (RAMAP>=311 & RAMAP<=390) & emp_ci==1
replace rama_ci=4 if (RAMAP>=410 & RAMAP<=420) & emp_ci==1
replace rama_ci=5 if RAMAP==500 & emp_ci==1
replace rama_ci=6 if (RAMAP>=610 & RAMAP<=632) & emp_ci==1
replace rama_ci=7 if (RAMAP>=711 & RAMAP<=720) & emp_ci==1
replace rama_ci=8 if (RAMAP>=810 & RAMAP<=833) & emp_ci==1
replace rama_ci=9 if (RAMAP>=910 & RAMAP<=960) & emp_ci==1
label var rama_ci "RAMA"
label define rama_ci 1 "Agricultura, caza, silvicultura y pesca" 2 "Explotación de minas y canteras" 3 "Industrias manufactureras" 4 "Electricidad, gas y agua" 5 "Construcción" 6"Comercio al por mayor y menor, restaurantes, hoteles" 7"Transporte y almacenamiento" 8"Establecimientos financieros, seguros, bienes inmuebles" 9"Servicios sociales, comunales y personales"
label values rama_ci rama_ci

* rama secundaria
g ramasec_ci=. 
label var ramasec_ci "Rama de actividad de la ocupación secundaria"
label val ramasec_ci ramasec_ci

******************
***categosec_ci***
******************
gen categosec_ci=.
label var categosec_ci "CATEGORIA OCUPACIONAL ACTIVIDAD SECUNDARIA"

*****************
***nempleos_ci***
*****************
capture drop nempleos_ci
gen byte nempleos_ci=.
replace nempleos_ci=1 if emp==1 & OTRACTV==2
replace nempleos_ci=2 if emp==1 & OTRACTV==1 & NROACTV>=1 & NROACTV!=.
label var nempleos "Numero de empleos"
label define nempleos_ci 1 "un trabajo" 2 "dos o mas trabajos"
label values nempleos_ci nempleos_ci

*****************
***spublico_ci***
*****************
capture drop spublico_ci
gen byte spublico_ci=.
replace spublico_ci=1 if emp==1 & (CATEGP==1 | CATEGP==2)
replace spublico_ci=0 if emp==1 & (CATEGP>2 & CATEGP<=8) 
label var spublico "Personas que trabajan en el sector publico"

*******************
***antiguedad_ci***
*******************
gen antiguedad_ci=.
label var antiguedad_ci "Antiguedad en la Ocupacion Actual (en anios)"

*****************
*otras variables*
*****************
capture drop tamfirma_ci
gen byte tamfirma_ci=.
replace tamfirma_ci=1 if emp==1 & (TAMESTP>=2 & TAMESTP<=4)
replace tamfirma_ci=0 if emp==1 & TAMESTP==1
label var tamfirma "Trabajadores formales"
label define tamfirma_ci 1 "5 o mas trabajadores" 0 "Menos de 5 trabajadores"
label values tamfirma_ci tamfirma_ci


		************************************
		**************INGRESOS**************
		************************************

****************
***ylmpri_ci ***
****************
capture drop ylmpri_ci
gen ylmpri_ci=.
replace ylmpri_ci=YOCUPAPM if YOCUPAPM~=-1 & YOCUPAPM~=-2 & YOCUPAPM~=-3
* The values '-3': '-2' and '-1' are 'he/she doesn't remember'; 'he/she doesn't answer' and 'don't aply' respectively
replace ylmpri_ci=. if EDAD<10
label var ylmpri_ci "Ingreso Laboral Monetario de la Actividad Principal"
replace ylmpri_ci=ylmpri_ci/1000
*Y.L. divido al ingreso entre 1000 para hacerlo comparable a lo largo del tiempo

*******************
*** ylmhopri_ci ***
*******************
gen ylmhopri_ci=.
replace ylmhopri_ci=ylmpri_ci/(horaspri*4.3)
label var ylmhopri_ci "Salario Horario Monetario de la Actividad Principal"

*******************
*** nrylmpri_ci ***
*******************
g nrylmpri_ci=(ylmpri_ci==. & emp_ci==1)
replace nrylmpri_ci=. if emp_ci!=1 | categopri_ci==4 /*excluding unpaid workers*/
label var nrylmpri_ci "Id no respuesta ingreso de la actividad principal"  

***************
***ylmsec_ci***
***************
gen ylmsec_ci=.	
label var ylmsec_ci "Ingreso Laboral Monetario de la Actividad Secundaria"

******************
*** ylmotros_ci***
******************
gen ylmotros_ci=.
label var ylmotros_ci "Ingreso Laboral Monetario Otros Trabajos"

*****************
*** ylnmpri_ci***
*****************
gen ylnmpri_ci=.
label var ylnmpri_ci "Ingreso Laboral NO Monetario de la Actividad Principal"

***************
***ylmsec_ci***
***************
gen ylnmsec_ci=.	
label var ylnmsec_ci "Ingreso Laboral NO Monetario de la Actividad Secundaria"

******************
***ylnmotros_ci***
******************
gen ylnmotros_ci=.
label var ylnmotros_ci "Ingreso Laboral NO Monetario Otros Trabajos"

************
***ylm_ci***
************
gen ylm_ci=.
replace ylm_ci=YOCUPAM if (YOCUPAM~=-1 & YOCUPAM~=-2 & YOCUPAM~=-3) & /*
	*/(YOCUPAPM~=-1 & YOCUPAPM~=-2 & YOCUPAPM~=-3) & (YOCUPAPM<=YOCUPAM)
replace ylm_ci=YOCUPAPM if (YOCUPAPM~=-1 & YOCUPAPM~=-2 & YOCUPAPM~=-3) & (YOCUPAPM>YOCUPAM)
* The values '-3': '-2' and '-1' are 'he/she doesn't remember'; 'he/she doesn't answer' and 'don't aply' respectively
* The survey gives directly ylmpri_ci and ylm_ci through YOCUPAPM and YOCUPAM but for some observations YOCUPAPM > YOCUPAM;
replace ylm_ci=. if EDAD<10
label var ylm_ci "Ingreso Laboral Monetario Total"
replace ylm_ci=ylm_ci/1000
*Y.L. divido al ingreso entre 1000 para hacerlo comparable a lo largo del tiempo

*************
***ylnm_ci***
*************
gen ylnm_ci=.
label var ylnm_ci "Ingreso Laboral NO Monetario Total"

*************
***ynlm_ci***
*************
gen ynlm_ci=.
replace ynlm_ci=YOTROS if YOTROS~=-1 & YOTROS~=-2 & YOTROS~=-3
replace ynlm_ci=. if EDAD<10
label var ynlm_ci "Ingreso NO Laboral Monetario"
replace ynlm_ci=ynlm_ci/1000
*Y.L. divido al ingreso entre 1000 para hacerlo comparable a lo largo del tiempo

*************
***ynlnm_ci**
*************
gen ynlnm_ci=.
label var ynlnm_ci "Ingreso NO Laboral NO Monetario"

******************
*** tcylmpri_ci***
******************
gen tcylmpri_ci=.
label var tcylmpri_ci "Identificador de top-code del ingreso de la actividad principal"

*******************
*** autocons_ci ***
*******************
gen autocons_ci=.
label var autocons_ci "Autoconsumo Individual"

*****************
***remesas_ci***
*****************
gen remesas_ci=.
label var remesas_ci "Remesas Individuales"

***************
***ylmho_ci ***
***************
gen ylmho_ci=.
label var ylmho_ci "Salario Horario Monetario de todas las Actividades"

******************
*** nrylmpri_ch***
******************
/*
capture drop nrylmpri_ci
gen nrylmpri_ci=.
replace nrylmpri_ci=0 if (YOCUPAPM~=-1 & YOCUPAPM~=-2 & YOCUPAPM~=-3)
replace nrylmpri_ci=1 if (YOCUPAPM==-2 | YOCUPAPM==-3) 
label var nrylmpri_ci "Identificador de No Respuesta del Ingreso Monetario de la Actividad Principal"*/
capture drop nrylmpri_ch
sort idh
egen nrylmpri_ch=sum(nrylmpri_ci) if miembro==1, by(idh) 
replace nrylmpri_ch=1 if nrylmpri_ch>1 & nrylmpri_ch~=. & miembro==1 
label var nrylmpri_ch "Identificador de Hogares en donde alguno de los miembros No Responde el Ingreso Monetario de la Actividad Principal"

******************
*** tcylmpri_ch***
******************
gen tcylmpri_ch=.

*************
*** ylm_ch***
*************
egen ylm_ch=sum(ylm_ci) if miembros_ci==1, by(idh_ch)
label var ylm_ch "Ingreso Laboral Monetario del Hogar"

****************
*** ylmnr_ch ***
****************
egen ylmnr_ch=sum(ylm_ci) if miembros_ci==1 & nrylmpri_ch==0, by(idh_ch)
label var ylmnr_ch "Ingreso Laboral Monetario del Hogar, considera 'missing' la No Respuesta"

***************
*** ylnm_ch ***
***************
egen ylnm_ch=sum(ylnm_ci) if miembros_ci==1, by(idh_ch)
label var ylnm_ch "Ingreso Laboral No Monetario del Hogar"

***************
*** ynlm_ch ***
***************
egen ynlm_ch=sum(ynlm_ci) if miembros_ci==1, by(idh_ch)
label var ynlm_ch "Ingreso No Laboral Monetario del Hogar"

****************
*** ynlnm_ch ***
****************
egen ynlnm_ch=sum(ynlnm_ci) if miembros_ci==1, by(idh_ch)
label var ynlnm_ch "Ingreso No Laboral No Monetario del Hogar"

*******************
*** rentaimp_ch ***
*******************
gen rentaimp_ch=.

*******************
*** autocons_ch ***
*******************
egen autocons_ch=sum(autocons_ci) if miembros_ci==1, by(idh_ch)
label var autocons_ch "Autoconsumo del Hogar"

******************
*** remesas_ch ***
******************
egen remesas_ch=sum(remesas_ci) if miembros_ci==1, by(idh_ch)
label var remesas_ch "Remesas del Hogar (monetario + especies)"

replace ylnm_ch=. if ylnm_ci==.
replace ynlnm_ch=. if ynlnm_ci==.
replace autocons_ch=. if autocons_ci==.
replace remesas_ch=. if remesas_ci==.
replace ylm_ch =. if miembros_ci==0
replace ylmnr_ch =. if miembros_ci==0
replace ylnm_ch =. if miembros_ci==0
replace ynlnm_ch =. if miembros_ci==0
replace autocons_ch =. if miembros_ci==0
replace remesas_ch =. if miembros_ci==0
replace ynlm_ch =. if miembros_ci==0


		****************************
		***VARIABLES DE EDUCACION***
		****************************

gen pp25a = NIVEL
gen pp25b = GRADO
gen pp25c = ULTSEM
gen pp27 = ASIST

** Se eliminan los valores negativos
replace pp25a =. if pp25a < 0   //nivel
replace pp25b =. if pp25b < 0   //grado
replace pp25c =. if pp25c < 0   //ultimo sem
replace pp27 =. if pp27 < 0     // asistencia

***************
***asiste_ci***
***************
gen byte asiste_ci=.
replace asiste_ci=1 if pp27 == 1
replace asiste_ci=0 if pp27 == 2
label var asiste "Personas que actualmente asisten a centros de enseñanza"

*************
***aedu_ci***
*************
/*
capture drop aedu_ci
gen byte aedu_ci=.
replace aedu=0 if NIVEL==1 | NIVEL==2
replace aedu=GRADO if NIVEL==3 & GRADO>0
replace aedu=GRADO+9 if NIVEL==4 & GRADO>0 & GRADO<=2
replace aedu=11 if NIVEL==4 & GRADO>2
replace aedu=GRADO+11 if (NIVEL==5 | NIVEL==6) & GRADO>0 
replace aedu=int(ULTSEM/2)+11 if (NIVEL==5 | NIVEL==6) & ULTSEM>0 
label variable aedu_ci "Años de Educacion" */

cap drop aedu_ci 
gen byte aedu_ci= .
// Para aquellos que declaran anios solamente
replace aedu_ci = 0 if (pp25a == 1 | pp25a == 2) // Ninguno, Prescolar
replace aedu_ci = pp25b if pp25a == 3 & (pp25b !=. & pp25c ==.) // Primaria
replace aedu_ci = pp25b + 6 if pp25a == 4 & (pp25b !=. & pp25c ==.) // Secundaria
replace aedu_ci = pp25b + 11 if (pp25a == 5 & pp25b != . & pp25c ==.| pp25a == 6 & pp25b !=. & pp25c == .) // Tecnico, Universitario

// Para aquellos que declaran semestres solamente
replace aedu_ci = (0.5 * pp25c) if pp25a == 3 & (pp25b == . & pp25c != .) // Primaria
replace aedu_ci = ((0.5 * pp25c) + 6) if pp25a == 4 & (pp25b == . & pp25c != .) // Secundaria
replace aedu_ci = ((0.5 * pp25c) + 11) if (pp25a == 5 & pp25b == . & pp25c != .| pp25a == 6 & pp25b == . & pp25c != .) // Tecnico, Universitario

// Para aquellos que declaran anio y semestre a la vez
replace aedu_ci = max(pp25b , 0.5 * pp25c) if pp25a == 3 & (pp25b != . & pp25c != .) // Primaria
replace aedu_ci = (max(pp25b , 0.5 * pp25c) + 6) if pp25a == 4 & (pp25b != . & pp25c != .) // Secundaria
replace aedu_ci = (max(pp25b , 0.5 * pp25c) + 11) if (pp25a == 5 & pp25b != . & pp25c != .| pp25a == 6 & pp25b != . & pp25c != .) // Tecnico, Universitario

// Para aquellos que declaran nivel pero no anio o semestre
replace aedu_ci = 0 if pp25a == 3 & aedu_ci == . // Primaria
replace aedu_ci = 6 if pp25a == 4 & aedu_ci == . // Media
replace aedu_ci = 11 if (pp25a == 5 | pp25a == 6) & aedu_ci == . // Técnico (TSU),  Universitario
replace aedu_ci=floor(aedu_ci) // se redondea la variable
label variable aedu_ci "Años de Educacion"

tab aedu if aedu>edad_ci

* Unfortunately, we found people with more years of education that years of life. 
* Then, assuming that everyone enters to school not before 5 years old. To correct this:
forvalues i=0(1)18 {
if `i'==0 {
replace aedu=`i' if (aedu>`i' & aedu~=.) & (edad_ci==3 | edad_ci==4 | edad_ci==5)
}
if `i'~=0 {
replace aedu=`i' if (aedu>`i' & aedu~=.) & edad_ci==(`i'+5)
}
}


**************
***eduno_ci***
**************
gen eduno_ci=(aedu_ci==0)
replace eduno=. if aedu_ci==.
label var eduno_ci "1 = personas sin educacion (excluye preescolar)"

***************
***edupre_ci***
***************
gen edupre_ci=.
replace edupre=1 if pp25a==2
replace edupre=0 if pp25a>2 | pp25a==1
label var edupre_ci "Educacion preescolar"

**************
***edupi_ci***
**************
gen edupi_ci=(aedu_ci>0 & aedu_ci<6)
replace edupi_ci=. if aedu_ci==.
label var edupi_ci "1 = personas que no han completado el nivel primario"

**************
***edupc_ci***
**************
gen edupc_ci=(aedu_ci==6)
replace edupc_ci=. if aedu_ci==.
label var edupc_ci "1 = personas que han completado el nivel primario"

**************
***edusi_ci***
**************
gen edusi_ci=(aedu_ci>6 & aedu_ci<11) // No se puede identificar técnica. De 2021 en adelante si. En 2021 el codigo cambia
replace edusi=. if aedu_ci==.
label var edusi_ci "1 = personas que no han completado el nivel secundario"

**************
***edusc_ci***
**************
gen edusc_ci=(aedu_ci==11)
replace edusc=. if aedu_ci==.
label var edusc_ci "1 = personas que han completado el nivel secundario"

**************
***eduui_ci***
**************
gen eduui_ci=(aedu_ci>11 & aedu_ci<14)
replace eduui_ci=. if aedu_ci==.
label var eduui_ci "1 = personas que no han completado el nivel universitario o superior"

***************
***eduuc_ci***
***************
gen byte eduuc_ci=(aedu_ci>=14)
replace eduuc_ci=. if aedu_ci==.
label var eduuc_ci "1 = personas que han completado el nivel universitario o superior"

***************
***edus1i_ci***
***************
gen edus1i_ci=(aedu_ci>6 & aedu_ci<9)
replace edus1i_ci=. if aedu_ci==.
label variable edus1i_ci "1er ciclo de la secundaria incompleto"

***************
***edus1c_ci***
***************
gen edus1c_ci=(aedu_ci==9)
replace edus1c_ci=. if aedu_ci==.
label variable edus1c_ci "1er ciclo de la secundaria completo"

***************
***edus2i_ci***
***************
gen edus2i_ci=(aedu_ci>9 & aedu_ci<11)
replace edus2i_ci=. if aedu_ci==.
label variable edus2i_ci "2do ciclo de la secundaria incompleto"

***************
***edus2c_ci***
***************
gen edus2c_ci=(aedu_ci==11)
replace edus2c_ci=. if aedu_ci==.
label variable edus2c_ci "2do ciclo de la secundaria completo"

**************
***eduac_ci***
**************
gen eduac_ci=.
label var eduac_ci "Educacion terciaria académica versus educación terciaria no-académica "

***************
***asispre_ci**
***************
g asispre_ci = (asiste_ci == 1 & pp25a == 2)
la var asispre_ci "Asiste a educacion prescolar"

***************
***repite_ci***
***************
gen repite_ci=.
label var repite_ci "Personas que han repetido al menos un año o grado"

******************
***repiteult_ci***
******************
gen repiteult_ci=.
label var repiteult_ci "Personas que han repetido el ultimo grado"

***************
***edupub_ci***
***************
gen edupub_ci=.
label var edupub_ci "1 = personas que asisten a centros de enseñanza publicos"

**************
***pqnoasis***
**************
gen byte pqnoasis_ci=.
replace pqnoasis=RZNOASIS if RZNOASIS>0
capture label drop pqnoasis
label variable pqnoasis "Razones para no asistir a centros de enseñanza"
label define pqnoasis 1 "Culmino sus estudios" 2 "No hay grado o agnos superiores" 3 "No hay cupo, escuela distante, desordenes estudiantiles, inasistencia de maestros o profesores" /*
*/ 4 "falta de recursos economicos" 5 "esta trabajando" 6 "asiste a un curso de capacitacion" 7 "no quiere estudiar" 8 "enfermedad o defecto fisico" /*
*/ 9 "problemas de conducta o de aprendizaje" 10 "cambio de residencia" 11 "edad mayor que la regular" 12 "tiene que ayudar en la casa" /*
*/ 13 "edad menor que la regular" 14 "va a tener un hijo o se caso" 15 "otros"
label values pqnoasis_ci pqnoasis_ci

**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**
	
**************
*pqnoasis1_ci*
**************
g       pqnoasis1_ci = 1 if RZNOASIS ==4
replace pqnoasis1_ci = 2 if RZNOASIS ==5
replace pqnoasis1_ci = 3 if RZNOASIS ==8  | RZNOASIS ==9
replace pqnoasis1_ci = 4 if RZNOASIS ==7
replace pqnoasis1_ci = 5 if RZNOASIS ==12 | RZNOASIS ==14
replace pqnoasis1_ci = 6 if RZNOASIS ==1
replace pqnoasis1_ci = 7 if RZNOASIS ==11 | RZNOASIS ==13
replace pqnoasis1_ci = 8 if RZNOASIS ==2  | RZNOASIS ==3 
replace pqnoasis1_ci = 9 if RZNOASIS ==6  | RZNOASIS ==10 | RZNOASIS ==15

label define pqnoasis1_ci 1 "Problemas económicos" 2 "Por trabajo" 3 "Problemas familiares o de salud" 4 "Falta de interés" 5	"Quehaceres domésticos/embarazo/cuidado de niños/as" 6 "Terminó sus estudios" 7	"Edad" 8 "Problemas de acceso"  9 "Otros"
label value  pqnoasis1_ci pqnoasis1_ci


		********************************************
		***Variables de Infraestructura del hogar***
		********************************************

****************
***aguared_ch***
****************
/*pv7 =AGUA A esta vivienda llega el agua por:
01	Acueducto
02	Pila pública
03	Camión
04	Otros medios */
gen aguared_ch=0
replace aguared_ch=1 if (AGUA==1 | AGUA==2 | AGUA==3)
label var aguared_ch "Acceso a una fuente de agua por red"
label define aguared_ch 0 "No tiene acceso por red" 1 "Sí tiene acceso por red"
label values aguared_ch aguared_ch

*****************
*aguafconsumo_ch*
*****************
*fuentes principales de agua para beber de los miembros de su hogar
gen aguafconsumo_ch =0

*****************
*aguafuente_ch*
*****************
gen aguafuente_ch=.
replace aguafuente_ch=1 if AGUA==1
replace aguafuente_ch=2 if AGUA==2
replace aguafuente_ch=6 if AGUA==3
replace aguafuente_ch= 10 if AGUA==4 | AGUA==-1

*************
*aguadist_ch*
*************
gen aguadist_ch=0
replace aguadist_ch=1 if AGUA==1|AGUA==3
replace aguadist_ch=3 if AGUA==2

**************
*aguadisp1_ch*
**************
gen aguadisp1_ch=9

**************
*aguadisp2_ch*
**************
gen aguadisp2_ch=9

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
gen aguamide_ch = .
label var aguamide_ch "Usan medidor para pagar consumo de agua"

*****************
*bano_ch         *  Altered
*****************
*pv8 = SANITA
gen bano_ch=.
replace bano_ch=0 if SANITA==4
replace bano_ch=1 if SANITA==1
replace bano_ch=2 if SANITA==2
replace bano_ch=6 if SANITA==3|SANITA==-1

***************
***banoex_ch***
***************
generate banoex_ch=9
la var banoex_ch "El servicio sanitario es exclusivo del hogar"

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
replace sinbano_ch = 0 if SANITA!=4

*************
*aguatrat_ch*
*************
gen aguatrat_ch =.

************
***luz_ch***
************
gen luz_ch=.

****************
***luzmide_ch***
****************
gen luzmide_ch=.

****************
***combust_ch***
****************
gen combust_ch=.

*************
***des1_ch***
*************
gen des1_ch=.
replace des1_ch=0 if SANITA==4
replace des1_ch=1 if SANITA==1 | SANITA==2 
replace des1_ch=2 if SANITA==3
label var des1_ch "Tipo de desagüe incluyendo definición de unimproved del MDG"

*************
***des2_ch***
*************
gen des2_ch=.
replace des2_ch=0 if SANITA==4
replace des2_ch=1 if SANITA==1 | SANITA==2 | SANITA==3
label var des2_ch "Tipo de desagüe sin incluir la definición de unimproved de los MDG"

*************
***piso_ch***
*************
* pv4=PISO
gen piso_ch=.
replace piso_ch=0 if PISO==3
replace piso_ch=1 if PISO==1 | PISO==2
replace piso_ch=2 if PISO==4 | PISO==-1
label var piso_ch "Material predominante en el piso de la casa"
label define piso_ch 0 "Tierra" 1 "Materiales permanentes" 2 "Otros"
label values piso_ch piso_ch

**************
***pared_ch***
**************
*PAREDES=pv2
*basado en 2021
gen pared_ch=.
replace pared_ch=0 if PAREDES==3 // no permanentes
replace pared_ch=1 if PAREDES==1 | PAREDES==2 | PAREDES==4 | PAREDES==5  // permanentes
replace pared_ch=2 if PAREDES==6 | PAREDES==-1 // otros
label var pared_ch "Materiales de construcción de las paredes"
label define pared_ch 0 "No permanentes" 1 "Materiales permanentes" 2 "Otros materiales"
label values pared_ch pared_ch

**************
***techo_ch***
**************
/* solo va de 01-05 
01	01	01	Platabanda
02	02	02	Teja
--	03	03	Láminas asfálticas (solo a partir de 2003) 
03	03	--	Fibrocemento, cemento, ligero y similares
04	04	04	Láminas métalicass (Zinc y similares)
--	05	05	Asbesto y Similares (solo a partir de 2003)  
05	06	06	Otros (Palmas, tabla y similares) */

*pv3=TECHO
gen techo_ch=.
replace techo_ch=0 if TECHO==5
replace techo_ch=1 if TECHO==1 | TECHO==2 | TECHO==3 | TECHO==4
label var techo_ch "Material de construcción del techo"
label define techo_ch 0 "No permanentes" 1 "Materiales permanentes" 2 "Otros materiales" 
label values techo_ch techo_ch

**************
***resid_ch***
**************
/*
PV11B	BASURA  Recolección directa de basura
PV11C	CONTBAS Container de basura
*/
gen resid_ch=.
replace resid_ch=3 if CONTBAS==1|CONTBAS==2
replace resid_ch=0 if BASURA==1 //remplazo por 0 si es recolección directa aunque registre también container
label var resid_ch "Método de eliminación de residuos"
label def resid_ch 0"Recolección pública o privada" 1"Quemados o enterrados"
label def resid_ch 2"Tirados a un espacio abierto" 3"Otros", add
label val resid_ch resid_ch

*************
***dorm_ch***
*************
*pv6=NRODORMV
gen dorm_ch=.
replace dorm_ch=NRODORMV if NRODORMV>=0
label var dorm_ch "Cantidad de habitaciones que se destinan exclusivamente para dormir"

****************
***cuartos_ch***
****************
*pv5=NROCUARV
gen cuartos_ch=.
replace cuartos_ch=NROCUARV if NROCUARV>=0
label var cuartos_ch "Cantidad de habitaciones en el hogar"

***************
***cocina_ch***
***************
gen cocina_ch=.
label var cocina_ch "Cuarto separado y exclusivo para cocinar"

**************
***telef_ch***
**************
* TELEFONO = pv11d Servicio telefónico (Telefónico fijo, anexo 2003)
gen telef_ch=.
replace telef_ch=1 if TELEFONO==1
replace telef_ch=0 if TELEFONO==2 | TELEFONO==-1
label var telef_ch "Hogar tiene servicio telefónico fijo"
label values telef_ch telef_ch

***************
***refrig_ch***
***************
*ph14a=NEVERA
gen refrig_ch=.
replace refrig_ch=1 if NEVERA==1
replace refrig_ch=0 if NEVERA==2
label var refrig_ch "El hogar posee heladera o refrigerador"
label define refrig_ch 0 "No" 1 "Sí"
label values refrig_ch refrig_ch

**************
***freez_ch***
**************
gen freez_ch=.

*************
***auto_ch***
*************
*ph15=NROAUTO
gen auto_ch=.
replace auto_ch=1 if NROAUTO>=1
replace auto_ch=0 if NROAUTO<1
label var auto_ch "El hogar posee automóvil particular"
label define auto_ch 0 "No" 1 "Sí"
label values auto_ch auto_ch

**************
***compu_ch***
**************
gen compu_ch=.
label var compu_ch "El hogar posee computadora"
label define compu_ch 0 "No" 1 "Sí"
label values compu_ch compu_ch

*****************
***internet_ch***
*****************
gen internet_ch=.
label var internet_ch "El hogar posee conexión a internet"
label define internet_ch 0 "No" 1 "Sí"
label values internet_ch internet_ch

************	
***cel_ch***
************
gen cel_ch=.
label var cel_ch "El hogar tiene servicio telefónico celular"
label define cel_ch 0 "No" 1 "Sí"
label values cel_ch cel_ch

**************
***vivi1_ch***
**************
gen vivi1_ch=.
replace vivi1_ch=1 if TIPOVIV==1 | TIPOVIV==2 | TIPOVIV==5
replace vivi1_ch=2 if TIPOVIV==3 | TIPOVIV==4
replace vivi1_ch=3 if TIPOVIV>5 & TIPOVIV<.
label var vivi1_ch "Tipo de vivienda en la que reside el hogar"
label define vivi1_ch 1 "Casa" 2 "Departamento" 3 "Otros"
label values vivi1_ch vivi1_ch

**************
***vivi2_ch***
**************
gen vivi2_ch=.
replace vivi2_ch=1 if vivi1_ch==1 | vivi1_ch==2
replace vivi2_ch=0 if vivi1_ch==3
label var vivi2_ch "La vivienda en la que reside el hogar es una casa o un departamento"
label define vivi2_ch 0 "No" 1 "Sí"
label values vivi2_ch vivi2_ch

*****************
***viviprop_ch***
*****************
gen viviprop_ch=.
replace viviprop_ch=0 if TENENVIV==3 | TENENVIV==4
replace viviprop_ch=1 if TENENVIV==1
replace viviprop_ch=2 if TENENVIV==2
replace viviprop_ch=3 if TENENVIV>4 & TENENVIV<=10
label var viviprop_ch "Propiedad de la vivienda"
label define viviprop_ch 0 "Alquilada" 1 "Propia y totalmente pagada" 2 "Propia en proceso de pago" 3 "Ocupada (propia de facto)"
label values viviprop_ch viviprop_ch

****************
***vivitit_ch***
****************
gen vivitit_ch=.

****************
***vivialq_ch***
****************
*Alquiler ph16b = MONTOBS
gen vivialq_ch=.
destring MONTOBS, replace
replace vivialq_ch=MONTOBS if MONTOBS>=0 &viviprop_ch==0 //solo para los que declararon que alquilan
label var vivialq_ch "Alquiler mensual de la vivienda"

*******************
***vivialqimp_ch***
*******************
gen vivialqimp_ch=.

*******************
***  benefdes_ci  *
*******************
g benefdes_ci=.
label var benefdes_ci "1 si tiene seguro de desempleo"

*******************
*** ybenefdes_ci  *
*******************
g ybenefdes_ci=.
label var ybenefdes_ci "Monto de seguro de desempleo"

		******************************
		*** VARIABLES DE MIGRACION ***
		******************************

*******************
*** migrante_ci ***
*******************
* la variable LUGAR_NAC está con puro 0
gen migrante_ci=.
label var migrante_ci "=1 si es migrante"
	
**********************
*** migantiguo5_ci ***
**********************
gen migantiguo5_ci=.
label var migantiguo5_ci "=1 si es migrante antiguo (5 anos o mas)"

**********************
*** migrantelac_ci ***
**********************
gen migrantelac_ci=.
label var migrantelac_ci "=1 si es migrante proveniente de un pais LAC"




/*_____________________________________________________________________________________________________*/
* Asignación de etiquetas e inserción de variables externas: tipo de cambio, Indice de Precios al 
* Consumidor (2011=100), líneas de pobreza
/*_____________________________________________________________________________________________________*/


do "$gitFolder\armonizacion_microdatos_encuestas_hogares_scl\_DOCS\\Labels&ExternalVars_Harmonized_DataBank.do"

/*_____________________________________________________________________________________________________*/
* Verificación de que se encuentren todas las variables armonizadas 
/*_____________________________________________________________________________________________________*/


order region_BID_c region_c pais_c anio_c mes_c zona_c factor_ch	idh_ch	idp_ci	factor_ci upm_ci estrato_ci sexo_ci edad_ci ///
afroind_ci afroind_ch afroind_ano_c dis_ci dis_ch relacion_ci civil_ci jefe_ci nconyuges_ch nhijos_ch notropari_ch notronopari_ch nempdom_ch ///
clasehog_ch nmiembros_ch miembros_ci nmayor21_ch nmenor21_ch nmayor65_ch nmenor6_ch	nmenor1_ch	condocup_ci ///
categoinac_ci nempleos_ci emp_ci antiguedad_ci	desemp_ci cesante_ci durades_ci	pea_ci desalent_ci subemp_ci ///
tiempoparc_ci categopri_ci categosec_ci rama_ci spublico_ci tamemp_ci cotizando_ci instcot_ci	afiliado_ci ///
formal_ci tipocontrato_ci ocupa_ci horaspri_ci horastot_ci	pensionsub_ci pension_ci tipopen_ci instpen_ci	ylmpri_ci nrylmpri_ci ///
tcylmpri_ci ylnmpri_ci ylmsec_ci ylnmsec_ci	ylmotros_ci	ylnmotros_ci ylm_ci	ylnm_ci	ynlm_ci	ynlnm_ci ylm_ch	ylnm_ch	ylmnr_ch  ///
ynlm_ch	ynlnm_ch ylmhopri_ci ylmho_ci rentaimp_ch autocons_ci autocons_ch nrylmpri_ch tcylmpri_ch remesas_ci remesas_ch	ypen_ci	ypensub_ci ///
salmm_ci aedu_ci eduno_ci edupi_ci edupc_ci	edusi_ci edusc_ci eduui_ci eduuc_ci	edus1i_ci ///
edus1c_ci edus2i_ci edus2c_ci edupre_ci eduac_ci asiste_ci pqnoasis_ci pqnoasis1_ci	repite_ci repiteult_ci edupub_ci  ///
aguared_ch aguafconsumo_ch aguafuente_ch aguadist_ch aguadisp1_ch aguadisp2_ch aguamala_ch aguamejorada_ch aguamide_ch bano_ch banoex_ch banomejorado_ch sinbano_ch aguatrat_ch luz_ch luzmide_ch combust_ch des1_ch des2_ch piso_ch ///
pared_ch techo_ch resid_ch dorm_ch cuartos_ch cocina_ch telef_ch refrig_ch freez_ch auto_ch compu_ch internet_ch cel_ch ///
vivi1_ch vivi2_ch viviprop_ch vivitit_ch vivialq_ch	vivialqimp_ch migrante_ci migantiguo5_ci migrantelac_ci, first

compress

saveold "`base_out'", replace
log close







