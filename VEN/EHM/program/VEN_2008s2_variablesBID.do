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
local ANO "2008"
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
Autores: Yessenia Loayza; Mayra Sáenz - saenzmayra.a@gmail.com - mayras@iadb.org - Diciembre 2013
Generación nuevas variables LMK: Yessenia Loayza (desloay@hotmail.com | yessenial@iadb.org)
Fecha última modificación: octubre 2013

							SCL/LMK - IADB
****************************************************************************/
****************************************************************************/
use `base_in', clear
qui destring _all, replace

***********
* Region_c *
************
gen region_c=  entidad
label define region_c  ///
1	"Distrito Federal"  ///
2	"Amazonas " ///
3	"Anzoategui"  ///
4	"Apure " ///
5	"Aragua " ///
6	"Barinas " ///
7	"Bolívar " ///
8	"Carabobo " ///
9	"Cojedes " ///
10	"Delta Amacuro"  ///
11	"Falcón"  ///
12	"Guárico"  ///
13	"Lara"  ///
14	"Mérida"  ///
15	"Miranda"  ///
16	"Monagas"  ///
17	"Nueva Esparta"  /// 
18	"Portuguesa"  ///
19	"Sucre"  ///
20	"Táchira"  ///
21	"Trujillo"  ///
22	"Yaracuy"  ///
23	"Zulia"  ///
24	"Vargas" 
	    
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

**********
***anio***
**********
gen anio_c=2008

*********
***mes***
*********
gen mes_c=.

**********
***zona***
**********

gen zona_c=.
label define zona 0 "Rural" 1 "Urbana"
label value zona zona

****************
*** idh_ch ***
****************
generate parentco  = pp19 
sort entidad control linea num_hog parentco num_per
cap drop id_hogar
sort entidad control linea serie num_hog
egen idh_ch = group(entidad control linea serie num_hog)
gen vectoruno=1
bys idh_ch: gen idp_ci=sum(vectoruno)
drop vectoruno

***************
***factor_c***
***************
gen factor_ci=pesop
label var factor_ci "Factor de Expansion del Individuo"

gen factor_ch=pesoh
label var factor_ch "Factor de expansion del Hogar"


*****************
***relacion_ci***
*****************
gen relacion_ci=.
replace relacion_ci=1 if pp19==1
replace relacion_ci=2 if pp19==2
replace relacion_ci=3 if pp19==3
replace relacion_ci=4 if pp19>=4 & pp19<=14 /* Otros familiares */
replace relacion_ci=5 if pp19==15  
replace relacion_ci=6 if pp19==16 | pp19==17 /*Es el sevicio domestico, Incluye a familiares del Serv. Domestico en pp19==17 */
label var relacion_ci "Relacion con el Jefe de Hogar"
label define relacion_ci 1 "Jefe de Hogar" 2 "Conyuge/Pareja" 3 "Hijo(a)/Hijastro(a)" 4 "Otros Parientes" 5 "Otros No parientes" 6 "Servicio Domestico (inc fam Serv. Dom.)"
label value relacion_ci relacion_ci


****************************
***VARIABLES DEMOGRAFICAS***
****************************


**********
***sexo***
**********
gen sexo_ci=pp18
label var sexo_ci "Sexo del Individuo"
label define sexo_ci 1 "Masculino" 2 "Femenino"
label value sexo_ci sexo_ci


**********
***edad***
**********
** Generating Edad
gen edad_ci=pp20
*replace edad=. if edad==99
label var edad_ci "Edad del Individuo"

*****************
***civil_ci***
*****************
gen byte civil_ci=.
replace civil_ci=1 if pp21==-1 | pp21==7
replace civil_ci=2 if pp21==1 | pp21==2 | pp21==3 | pp21==4
replace civil_ci=3 if pp21==5
replace civil_ci=4 if pp21==6
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


*******************************************************
***           VARIABLES DE DIVERSIDAD               ***
*******************************************************				
* Maria Antonella Pereira & Nathalia Maya - Marzo 2021	

			
	***************
	*** afroind_ci ***
	***************
gen afroind_ci=. 

	***************
	*** afroind_ch ***
	***************
gen afroind_ch=. 

	*******************
	*** afroind_ano_c ***
	*******************
gen afroind_ano_c=.		

	*******************
	*** dis_ci ***
	*******************
gen dis_ci=. 

	*******************
	*** dis_ch ***
	*******************
gen dis_ch=. 

*************************************************************************************
*******************************VARIABLES LABORALES***********************************
*************************************************************************************
**************
***ocupa_ci***
**************
 
gen ocupa_ci=.
replace ocupa_ci=1 if pp48>=0  & pp48<=9
replace ocupa_ci=2 if pp48>=10 & pp48<=19
replace ocupa_ci=3 if pp48>=20 & pp48<=23
replace ocupa_ci=4 if pp48>=25 & pp48<=29
replace ocupa_ci=6 if pp48>=30 & pp48<=35
replace ocupa_ci=7 if pp48>=40 & pp48<=79
replace ocupa_ci=5 if pp48>=80 & pp48<=89
replace ocupa_ci=8 if pp48>=90 & pp48<=91
replace ocupa_ci=9 if pp48>=99
label var ocupa_ci "Ocupacion Laboral en la Actividad Principal"
label define ocupa_ci 1 "PROFESIONALES Y TECNICOS" 2 "GERENTES, DIRECTORES Y FUNCIONARIOS SUPERIORES"  3 "PERSONAL ADMINISTRATIVO Y NIVEL INTERMEDIO" 4 "COMERCIANTES Y VENDEDORES" 5 "TRABAJADORES EN SERVICIOS" 6"TRABAJADORES AGRICOLAS Y AFINES" 7 "OBREROS NO AGRICOLAS, CONDUCTORES DE MAQUINAS Y VEHICULOS DE TRANSPORTE Y SIMILARES" 8"FUERZAS ARMADAS" 9"OTRAS OCUPACIONES NO CLASIFICADAS ANTERIORMENTE"
label values ocupa_ci ocupa_ci



*****************
***horastot_ci***
*****************
gen byte horastot_ci=.
replace horastot_ci=pp35 if pp35<=110 & pp35>=0 & pp35>=pp34
replace horastot_ci=pp34 if pp34>=pp35 & pp34>=0
label var horastot_ci "Horas totales trabajadas la semana pasada en todas las Actividades"

*Note: No se encuentra en la base la variable pp37 que detalla el formulario para las horas totales.


gen YOCUPAPM=pp59
gen YOCUPAM=pp60
gen YOTROS=pp61k 
gen EDAD=edad_ci


*******************
***antiguedad_ci***
*******************
gen antiguedad_ci=.
label var antiguedad_ci "Antiguedad en la actividad actual"


************************************
*** VARIABLES DEL MERCADO LABORAL***
************************************

****************
****condocup_ci*
****************
/*
gen condocup_ci=.
replace condocup_ci=1 if (codigo_sum>=1 & codigo_sum <=3) 
replace condocup_ci=2 if codigo_sum==4 | codigo_sum==11 
replace condocup_ci=3 if condocup_ci!=1 & condocup_ci!=2
replace condocup_ci=4 if edad_ci<15
label define condocup_ci 1"ocupados" 2"desocupados" 3"inactivos" 4"menor de PET"
label value condocup_ci condocup_ci
label var condocup_ci "Condicion de ocupacion utilizando definicion del pais"
*/

* Cambio edad minima de la encuesta (10 años). MGD 06/10/2014
gen condocup_ci=.
replace condocup_ci=1 if (codigo_sum>=1 & codigo_sum <=3) 
replace condocup_ci=2 if codigo_sum==4 | codigo_sum==11 
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
replace cotizando_ci=1 if (pp56c==1 ) & cotizando_ci==0 /*solo a emplead@s y asalariad@s, difiere con los otros paises*/
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
gen tamemp_ci=pp50a
label define pp50a 1"una" 2"2-4 personas" 3"5 personas" 4"6-10 personas" 5"11-20 personas" 6"más de 20 personas"
label var tamemp_ci "# empleados en la empresa de la actividad principal"
*/
gen tamemp_ci=1 if pp50a==1 | pp50a==2
label var  tamemp_ci "Tamaño de Empresa" 
*Empresas medianas
replace tamemp_ci=2 if pp50a==3 | pp50a==4 | pp50a==5
*Empresas grandes
replace tamemp_ci=3 if pp50a==6
label define tamaño 1"Pequeña" 2"Mediana" 3"Grande"
label values tamemp_ci tamaño
tab tamemp_ci [iw=factor_ci]
/*
*Genera la variable para clasificar a los inactivos
*Jubilados, pensionados e incapacitados
gen categoinac_ci=1 if pp29==7
label var  categoinac_ci "Condición de Inactividad" 
*Estudiantes
replace categoinac_ci=2 if pp29==5
*Quehaceres del Hogar
replace categoinac_ci=3 if pp29==6
*Otra razon
replace categoinac_ci=4 if pp29==2 | pp29==8 | pp29==9 | pp29==0
label define inactivo 1"Pensionado y otros" 2"Estudiante" 3"Hogar" 4"Otros"
label values categoinac_ci inactivo
*/
destring pp29, replace
gen categoinac_ci = .
replace categoinac_ci = 1 if ((pp29=="07") & condocup_ci==3)
replace categoinac_ci = 2 if ((pp29=="05") & condocup_ci==3)
replace categoinac_ci = 3 if ((pp29=="06") & condocup_ci==3)
replace categoinac_ci = 4 if  ((categoinac_ci ~=1 & categoinac_ci ~=2 & categoinac_ci ~=3) & condocup_ci==3)
label var categoinac_ci "Categoría de inactividad"
label define categoinac_ci 1 "Jubilados o pensionados" 2 "Estudiantes" 3 "Quehaceres domésticos" 4 "Otros"
label values categoinac_ci categoinac_ci

*************
**pension_ci*
*************
gen pension_ci=0 
replace pension_ci=1 if (pp61a==1 | pp61e==1 | pp61f==1) /*A todas las per mayores de diez años */
label var pension_ci "1=Recibe pension contributiva"
 
*************
*ypen_ci*
*************
gen ypen_ci=pp61k if pension_ci==1
replace ypen_ci=. if pp61k<0
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
replace cesante_ci=1 if (pp45==1) & condocup_ci==2
label var cesante_ci "Desocupado - definicion oficial del pais"

*********
*lp_ci***
*********
gen lp_ci =.
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

*gen salmm_ci=615.79 /*En BsF */
* 2015 MGD: promedio del semestre y segun zona urbana y rural
g salmm_ci=799.23
label var salmm_ci "Salario minimo legal"

*************
***tecnica_ci**
*************
gen tecnica_ci=(pp25a==5)
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

*****************
***horaspri_ci***
*****************
gen byte horaspri_ci=.
replace horaspri_ci=pp34 if pp34<=110 & pp34>=0
replace horaspri_ci=. if emp_ci==0
label var horaspri_ci "Horas totales trabajadas la semana pasada en la Actividad Principal"
****************
***durades_ci***
****************
/*gen durades_ci=pp46a  if pp46a >0 /* sin filtros & desemp_ci==1*/
replace durades_ci=pp46b*12 if pp46b>0 & durades_ci==./* sin filtros & desemp_ci==1*/
*/

g meses=pp46a if pp46a>0
g anios=pp46b*12 if pp46b>0
egen durades_ci = rsum(meses anios), missing
replace durades_ci=. if condocup_ci==3
*Se ponen como missing values las personas que llevan más tiempo desempleadas que tiempo de vida:
gen edad_meses=edad_ci*12
replace durades_ci=. if durades_ci>edad_meses
drop edad_meses

*****************
***desalent_ci***
*****************
gen desalent_ci=.
replace desalent_ci=1 if pp43==1 | pp43==2 | pp43==3  
replace desalent_ci=0 if desalent_ci==.
label var desalent_ci "Trabajadores desalentados: personas que creen que por alguna razón no conseguirán trabajo"
label define desalent_ci 1 "Trabajador desalentado" 0 "No es trabajador desalentado" 
label values desalent_ci desalent_ci 
*Note esta la variable p44

***************
***subemp_ci***
***************
/*Sobre las horas normalmente trabajadas*/

gen subemp_ci=.
label var subemp_ci "Personas que trabajan 30 horas a la semana o menos y están dispuestas a trabajar más"
label define subemp_ci 1 "Subempleado " 0 "No subempleado" 
label values subemp_ci subemp_ci 

*No está la variable pp38: Ha hecho algo para trabajar horas adicionales?

*******************
***tiempoparc_ci***
*******************
/*Sobre las horas normalmente trabajadas*/

gen tiempoparc_ci=.
*No está la variable pp38: Ha hecho algo para trabajar horas adicionales?

******************
***categopri_ci***
******************
*Se corrige a partir del anio 2008

gen categopri_ci=.
*2014, 01 Modificacion MLO, miembros de cooperativa y sociedad de personas pasan a cuenta propia
* se condiciono tambien a solo los ocupados (condocup_ci==1)
replace categopri_ci=1 if pp54==8 & condocup_ci==1
replace categopri_ci=2 if pp54==7  |pp54==5 |pp54==6  & condocup_ci==1
replace categopri_ci=3 if pp54>=1 & pp54<=4   & condocup_ci==1
*2014, 01 Modificacion MLO
*replace categopri_ci=4 if pp60==0 & categopri_ci!=1 & categopri_ci!=2 & categopri_ci!=3   & condocup_ci==1
replace categopri_ci=4 if pp54==9 & condocup_ci==1
/*
replace categopri_ci=1 if pp54==8
replace categopri_ci=2 if pp54==7 
replace categopri_ci=3 if pp54>=1 & pp54<=4 
replace categopri_ci=4 if pp60==0 & categopri_ci!=1 & categopri_ci!=2 & categopri_ci!=3 */
label var categopri_ci "CATEGORIA OCUPACIONAL ACTIVIDAD PRINCIPAL"
label define categopri_ci 1 "Patron" 2 "Cuenta Propia" 3 "Asalariado" 4 "Trabajador No Remunerado" 
label values categopri_ci categopri_ci

******************
***categosec_ci***
******************
gen categosec_ci=.
label var categosec_ci "CATEGORIA OCUPACIONAL ACTIVIDAD SECUNDARIA"
/*NA */

*************
***rama_ci***
*************

destring pp49, replace force
gen rama_ci=.
replace rama_ci=1 if (pp49>=111 & pp49<=141) & emp_ci==1
replace rama_ci=2 if (pp49>=210 & pp49<=290) & emp_ci==1
replace rama_ci=3 if (pp49>=311 & pp49<=390) & emp_ci==1
replace rama_ci=4 if (pp49>=410 & pp49<=420) & emp_ci==1
replace rama_ci=5 if pp49==500 & emp_ci==1
replace rama_ci=6 if (pp49>=610 & pp49<=632) & emp_ci==1
replace rama_ci=7 if (pp49>=711 & pp49<=720) & emp_ci==1
replace rama_ci=8 if (pp49>=810 & pp49<=833) & emp_ci==1
replace rama_ci=9 if (pp49>=910 & pp49<=960) & emp_ci==1
label var rama_ci "RAMA"
label define rama_ci 1 "Agricultura, caza, silvicultura y pesca" 2 "Explotación de minas y canteras" 3 "Industrias manufactureras" 4 "Electricidad, gas y agua" 5 "Construcción" 6"Comercio al por mayor y menor, restaurantes, hoteles" 7"Transporte y almacenamiento" 8"Establecimientos financieros, seguros, bienes inmuebles" 9"Servicios sociales, comunales y personales"
label values rama_ci rama_ci

*****************
***nempleos_ci***
*****************
capture drop nempleos_ci
gen byte nempleos_ci=.
replace nempleos_ci=1 if emp_ci==1 & pp33a==2
replace nempleos_ci=2 if emp_ci==1 & pp33a==1 & pp33b>=1 & pp33b!=.
label var nempleos "Numero de empleos"


/*
*****************
***firmapeq_ci***
*****************
gen byte tamfirma_ci=.
replace tamfirma_ci=1 if emp_ci==1 & (pp50a>3 & pp50a<=6)
replace tamfirma_ci=0 if emp_ci==1 & (pp50a<=3)
label var tamfirma_ci "Trabajadores formales"
label define tamfirma_ci 1 "Mas de 5 trabajadores" 0 "5 o Menos trabajadores"
label values tamfirma_ci tamfirma_ci


gen firmapeq_ci=1 if tamfirma_ci==0
replace firmapeq_ci=0 if tamfirma_ci==1
replace firmapeq_ci=. if emp_ci==0
		*/
*****************
***spublico_ci***
*****************
gen byte spublico_ci=.
replace spublico_ci=1 if emp_ci==1 & (pp54==1 | pp54==2) 
replace spublico_ci=0 if emp_ci==1 & (pp54>2 & pp54<=9) 
label var spublico "Personas que trabajan en el sector publico"

****************
***ylmpri_ci ***
****************
gen ylmpri_ci=.
replace ylmpri_ci=YOCUPAPM if YOCUPAPM>=0
* The values '-3': '-2' and '-1' are 'he/she doesn't remember'; 'he/she doesn't answer' and 'don't aply' respectively
replace ylmpri_ci=. if EDAD<10
label var ylmpri_ci "Ingreso Laboral Monetario de la Actividad Principal"

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
replace ylm_ci=YOCUPAM if (YOCUPAM>=0) & (YOCUPAPM>=0) & (YOCUPAPM<=YOCUPAM)
replace ylm_ci=YOCUPAPM if (YOCUPAPM>=0) & (YOCUPAPM>YOCUPAM)
* The values '-3': '-2' and '-1' are 'he/she doesn't remember'; 'he/she doesn't answer' and 'don't aply' respectively
* The survey gives directly ylmpri_ci and ylm_ci through YOCUPAPM and YOCUPAM but for some observations YOCUPAPM > YOCUPAM;
replace ylm_ci=. if EDAD<10
label var ylm_ci "Ingreso Laboral Monetario Total"

*************
***ylnm_ci***
*************
gen ylnm_ci=.
label var ylnm_ci "Ingreso Laboral NO Monetario Total"

*************
***ynlm_ci***
*************
gen ynlm_ci=.
replace ynlm_ci=YOTROS if YOTROS>=0
replace ynlm_ci=. if EDAD<10
label var ynlm_ci "Ingreso NO Laboral Monetario"

*************
***ynlnm_ci***
*************
gen ynlnm_ci=.
label var ynlnm_ci "Ingreso NO Laboral NO Monetario"
/*
******************
*** nrylmpri_ch***
******************
gen nrylmpri_ci=.
replace nrylmpri_ci=0 if (YOCUPAPM>=0)
replace nrylmpri_ci=1 if (YOCUPAPM==-2 | YOCUPAPM==-3) 
label var nrylmpri_ci "Identificador de No Respuesta del Ingreso Monetario de la Actividad Principal"
*/
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

************************
*** HOUSEHOLD INCOME ***
************************

******************
*** nrylmpri_ch***
******************
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


***************
***ylmho_ci ***
***************
gen ylmho_ci=.
replace ylmho_ci=ylm_ci/(horastot*4.3)
label var ylmho_ci "Salario Horario Monetario de todas las Actividades"

*******************
*** rentaimp_ch ***
*******************
gen rentaimp_ch=.
label var rentaimp_ch "Rentas imputadas del hogar"


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

*******************
*** ylmhopri_ci ***
*******************
gen ylmhopri_ci=.
replace ylmhopri_ci=ylmpri_ci/(horaspri*4.3)
label var ylmhopri_ci "Salario Horario Monetario de la Actividad Principal"
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

/* 
Notas construcción aedu_ci: 

Sin nivel  	               01
Inicial (Preescolar)       02
Primaria                   03
Secundaria                 04
Técnico Superior           05
Universitario              06

Existen personas que:

	- Declaran último grado aporbado en anios (pp25b)
	- Declaran último semestre aprobado (pp25b) 
	- Declaran anio y semestre a la vez 
	- No declaran ninguno de los dos pero si nivel

Para aquellos que declaran anio y semeste a la vez se imputa 
el máximo valor en anios correspondiente a su reporte.

Para aquellos que no declaran grado aprobado de ninguna forma
se les imputa la cantidad maxima de anios del nivel educativo 
anterior.

(Febrero 2023)
*/

// Se eliminan los valores negativos
replace pp25a = . if pp25a < 0
replace pp25b = . if pp25b < 0
replace pp25c = . if pp25c < 0 
replace pp27 = . if pp27 < 0 

***************
***asiste_ci***
***************
gen byte asiste_ci=.
replace asiste_ci=1 if pp27 == 1
replace asiste_ci=0 if pp27 == 2
label var asiste "Personas que actualmente asisten a centros de enseñanza"


*************
** aedu_ci **
*************

gen byte aedu_ci= .
// Para aquellos que declaran anios solamente
replace aedu_ci = 0 if (pp25a == 1 | pp25a == 2) // Ninguno, Prescolar
replace aedu_ci = pp25b if pp25a == 3 & (pp25b != . & pp25c == .) // Primaria
replace aedu_ci = pp25b + 6 if pp25a == 4 & (pp25b != . & pp25c == .) // Secundaria
replace aedu_ci = pp25b + 11 if (pp25a == 5 & pp25b != . & pp25c == .| pp25a == 6 & pp25b != . & pp25c == .) // Tecnico, Universitario

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


**************
***eduno_ci***
**************
gen eduno_ci=(aedu_ci==0)
replace eduno=. if aedu_ci==.

***************
***edupre_ci***
***************
gen edupre_ci=.
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
gen edupub_ci = .
label var edupub_ci "1 = personas que asisten a centros de enseñanza publicos"

**************
***pqnoasis***
**************
gen byte pqnoasis_ci=.
replace pqnoasis=pp28 if pp28>0
label var pqnoasis_ci "Razones para no asistir a centros de enseñanza"
label define pqnoasis_ci 1 "Culmino sus estudios" 2 "No hay grado o anios superiores" 3 "No hay cupo, escuela distante" 4 "falta de recursos economicos" 5 "esta trabajando" 6 "asiste a un curso de capacitacion" 7 "no quiere estudiar" 8 "enfermedad o defecto fisico" 9 "problemas de conducta o de aprendizaje" 10 "cambio de residencia" 11 "edad mayor que la regular" 12 "tiene que ayudar en la casa" 13 "edad menor que la regular" 14 "va a tener un hijo o se caso" 15 "otros"
label values pqnoasis_ci pqnoasis_ci

**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**
	
**************
*pqnoasis1_ci*
**************
g       pqnoasis1_ci = 1 if pp28 ==4
replace pqnoasis1_ci = 2 if pp28 ==5
replace pqnoasis1_ci = 3 if pp28 ==8  | pp28 ==9
replace pqnoasis1_ci = 4 if pp28 ==7
replace pqnoasis1_ci = 5 if pp28 ==12 | pp28 ==14
replace pqnoasis1_ci = 6 if pp28 ==1
replace pqnoasis1_ci = 7 if pp28 ==11 | pp28 ==13
replace pqnoasis1_ci = 8 if pp28 ==2  | pp28 ==3 
replace pqnoasis1_ci = 9 if pp28 ==6  | pp28 ==10 | pp28 ==15

label define pqnoasis1_ci 1 "Problemas económicos" 2 "Por trabajo" 3 "Problemas familiares o de salud" 4 "Falta de interés" 5	"Quehaceres domésticos/embarazo/cuidado de niños/as" 6 "Terminó sus estudios" 7	"Edad" 8 "Problemas de acceso"  9 "Otros"
label value  pqnoasis1_ci pqnoasis1_ci

********************************************
***Variables de Infraestructura del hogar***
********************************************

***************
**aguared_ch***
***************

/*
gen aguared_ch =.
*gen aguared_ch = (pv7==1)
label var aguared_ch "Acceso a fuente de agua por red"
*/

* MGR Jul, 2015: activo variable
gen aguared_ch = (pv7==1)

label var aguared_ch "Acceso a una fuente de agua por red"

*****************
***aguadist_ch***
*****************
gen aguadist_ch=.

*****************
***aguamala_ch***
*****************
gen aguamala_ch=.

*****************
***aguamide_ch***
*****************
gen aguamide_ch=.

************
***luz_ch***
************
gen luz_ch=.
*gen luz_ch=(pv11a==1)
label var luz_ch "La principal fuente de iluminación es electricidad"

****************
***luzmide_ch***
****************
gen luzmide_ch=.

****************
***combust_ch***
****************
gen combust_ch=.

*************
***bano_ch***
*************
gen bano_ch=.
/*
replace bano_ch = 1 if pv8>=1 & pv8<=3
replace bano_ch = 0 if pv8==4
*/
label var bano_ch "El hogar tiene algun tipo de servicio higienico"

***************
***banoex_ch***
***************

gen banoex_ch=.
replace banoex_ch=1 if bano_ch==1 & ph13a==1
replace banoex_ch=0 if bano_ch==1 & ph13a==2

label var banoex_ch "El servicio higiénico es de uso exclusivo del hogar"

*************
***des1_ch***
*************

* Modificaciones Marcela Rubio Septiembre 2014: variable fué generada como missing

gen des1_ch=.
replace des1_ch=0 if pv8==4
replace des1_ch=1 if pv8==1 | pv8==2 
replace des1_ch=2 if pv8==3
label var des1_ch "Tipo de desagüe incluyendo definición de unimproved del MDG"

*************
***des2_ch***
*************

* Modificaciones Marcela Rubio Septiembre 2014: variable fué generada como missing

gen des2_ch=.
replace des2_ch=0 if pv8==4
replace des2_ch=1 if pv8==1 | pv8==2 | pv8==3
label var des2_ch "Tipo de desagüe sin incluir la definición de unimproved de los MDG"

*************
***piso_ch***
*************

* Jul 2015, activado MGR
gen piso_ch=.
replace piso_ch=0 if pv4==3
replace piso_ch=1 if pv4==1 | pv4==2
replace piso_ch=2 if pv4==4
label var piso_ch "Material predominante en el piso de la casa"
label define piso_ch 0 "Tierra" 1 "Materiales permanentes" 2 "Otros"
label values piso_ch piso_ch

**************
***pared_ch***
**************

* Jul 2015, activado MGR
gen pared_ch=.
replace pared_ch=0 if pv2==3
replace pared_ch=1 if pv2==1 | pv2==2 | pv2==4 | pv2==5
replace pared_ch=2 if pv2==6
label var pared_ch "Materiales de construcción de las paredes"
label define pared_ch 0 "No permanentes" 1 "Materiales permanentes" 2 "Otros materiales"
label values pared_ch pared_ch

**************
***techo_ch***
**************

* Jul 2015, activado MGR
gen techo_ch=.
replace techo_ch=0 if pv3==2 | pv3==4
replace techo_ch=1 if pv3==1 | pv3==3 | pv3==5
replace techo_ch=2 if pv3==6
label var techo_ch "Material de construcción del techo"
label define techo_ch 0 "No permanentes" 1 "Materiales permanentes" 2 "Otros materiales" 
label values techo_ch techo_ch

**************
***resid_ch***
**************
gen resid_ch=.
label var resid_ch "Método de eliminación de residuos"

**Daniela Zuluaga- Enero 2018: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**
	
*********************
***aguamejorada_ch***
*********************
gen aguamejorada_ch =.
		
		
*********************
***banomejorado_ch***
*********************
gen  banomejorado_ch =.


*************
***dorm_ch***
*************

gen dorm_ch=.
*replace dorm_ch=pv6
label var dorm_ch "Cantidad de habitaciones que se destinan exclusivamente para dormir"

****************
***cuartos_ch***
****************

* Activado Jul, 2015 MGR
gen cuartos_ch=.
replace cuartos_ch=pv5
label var cuartos_ch "Cantidad de habitaciones en el hogar"
	
***************
***cocina_ch***
***************

gen cocina_ch=.
label var cocina_ch "Cuarto separado y exclusivo para cocinar"

**************
***telef_ch***
**************

gen telef_ch=.
/*
replace telef_ch=1 if pv11d==1
replace telef_ch=0 if pv11d==2
*/
label var telef_ch "Hogar tiene servicio telefónico fijo"
label define telef_ch 0 "No" 1 "Sí"
label values telef_ch telef_ch

***************
***refrig_ch***
***************

gen refrig_ch=.

replace refrig_ch=1 if ph14a==1
replace refrig_ch=0 if ph14a==2

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
gen auto_ch=.

replace auto_ch=1 if ph15>0 & ph15 !=.
replace auto_ch=0 if ph15==0
label var auto_ch "El hogar posee automóvil particular"
label define auto_ch 0 "No" 1 "Sí"
label values auto_ch auto_ch

**************
***compu_ch***
**************

gen compu_ch=.

replace compu_ch=1 if ph14n==1
replace compu_ch=0 if ph14n==2
label var compu_ch "El hogar posee computadora"
label define compu_ch 0 "No" 1 "Sí"
label values compu_ch compu_ch


*****************
***internet_ch***
*****************

gen internet_ch=.
replace internet_ch=1 if ph14o==1
replace internet_ch=0 if ph14o==2

label var internet_ch "El hogar posee conexión a internet"
label define internet_ch 0 "No" 1 "Sí"
label values internet_ch internet_ch

************
***cel_ch***
************
gen cel_ch=.

replace cel_ch=1 if ph14l==1
replace cel_ch=0 if ph14l==2

label var cel_ch "El hogar tiene servicio telefónico celular"
label define cel_ch 0 "No" 1 "Sí"
label values cel_ch cel_ch

**************
***vivi1_ch***
**************
gen vivi1_ch=.
/*
replace vivi1_ch=1 if pv1==2 | pv1==5
replace vivi1_ch=2 if pv1==3
replace vivi1_ch=3 if pv1==1 | pv1==4 | pv1==6 | pv1==7 | pv1==8
*/
label var vivi1_ch "Tipo de vivienda en la que reside el hogar"
label define vivi1_ch 1 "Casa" 2 "Departamento" 3 "Otros"
label values vivi1_ch vivi1_ch


**************
***vivi2_ch***
**************

gen vivi2_ch=.
/*
replace vivi2_ch=1 if vivi1_ch==1 | vivi1_ch==2
replace vivi2_ch=0 if vivi1_ch==3
*/
label var vivi2_ch "La vivienda en la que reside el hogar es una casa o un departamento"
label define vivi2_ch 0 "No" 1 "Sí"
label values vivi2_ch vivi2_ch


*****************
***viviprop_ch***
*****************
gen viviprop_ch=.

replace viviprop_ch=0 if ph16a==3 | ph16a==4
replace viviprop_ch=1 if ph16a==1
replace viviprop_ch=2 if ph16a==2
replace viviprop_ch=3 if ph16a==7

label var viviprop_ch "Propiedad de la vivienda"
label define viviprop_ch 0 "Alquilada" 1 "Propia y totalmente pagada" 2 "Propia en proceso de pago" 3 "Ocupada (propia de facto)"
label values viviprop_ch viviprop_ch

****************
***vivitit_ch***
****************
gen vivitit_ch=.
label var vivitit_ch "El hogar posee un título de propiedad"

****************
***vivialq_ch***
****************
gen vivialq_ch=.
*replace vivialq_ch=ph16b if viviprop_ch==0
label var vivialq_ch "Alquiler mensual de la vivienda"


*******************
***vivialqimp_ch***
*******************

gen vivialqimp_ch=.


/*_____________________________________________________________________________________________________*/
* Asignación de etiquetas e inserción de variables externas: tipo de cambio, Indice de Precios al 
* Consumidor (2011=100), líneas de pobreza
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
aguared_ch aguadist_ch aguamala_ch aguamide_ch luz_ch luzmide_ch combust_ch	bano_ch banoex_ch des1_ch des2_ch piso_ch aguamejorada_ch banomejorado_ch  ///
pared_ch techo_ch resid_ch dorm_ch cuartos_ch cocina_ch telef_ch refrig_ch freez_ch auto_ch compu_ch internet_ch cel_ch ///
vivi1_ch vivi2_ch viviprop_ch vivitit_ch vivialq_ch	vivialqimp_ch , first



compress


saveold "`base_out'", replace


log close



