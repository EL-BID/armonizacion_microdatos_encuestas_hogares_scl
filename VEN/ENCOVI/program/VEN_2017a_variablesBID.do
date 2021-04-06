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
local ENCUESTA ENCOVI
local ANO "2017"
local ronda a 
local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
                        
capture log close
log using "`log_file'", replace 

/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: 
Encuesta: ENCOVI
Round: a
Autores: Daniela Zuluaga (DZ) -Email: danielazu@iadb.org,  da.zuluaga@hotmail.com
Fecha última modificación: Marzo 2018

							SCL/LMK - IADB
****************************************************************************/
****************************************************************************/
use `base_in', clear



cap destring, replace

************
****pais****
************
gen str pais_c="VEN"

**********
***anio***
**********
gen anio_c=2017

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

gen idh_ch = control


gen idp_ci=lin


***************
***factor_c***
***************
gen factor_ci=pesopersona
label var factor_ci "Factor de Expansion del Individuo"

gen factor_ch=pesohogar
label var factor_ch "Factor de expansion del Hogar"

***************
***upm_ci***
***************

gen upm_ci=.
label variable upm_ci "Unidad Primaria de Muestreo"

***************
***estrato_ci***
***************

clonevar estrato_ci=estrato
label variable estrato_ci "Estrato"

*************************
*** VARIABLES DE RAZA ***
*************************

gen raza_idioma_ci = . 
gen id_ind_ci = .
gen id_afro_ci = .
gen raza_ci=.

***********
* Region_c *
************

gen region_c=  enti
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

*****************
***relacion_ci***
*****************
gen relacion_ci=.
replace relacion_ci=1 if cmhp17==1
replace relacion_ci=2 if cmhp17==2
replace relacion_ci=3 if cmhp17==3 | cmhp17==4
replace relacion_ci=4 if cmhp17>=5 & cmhp17<=10 /* Otros familiares */
replace relacion_ci=5 if cmhp17==11  
replace relacion_ci=6 if cmhp17==12
label var relacion_ci "Relacion con el Jefe de Hogar"
label define relacion_ci 1 "Jefe de Hogar" 2 "Conyuge/Pareja" 3 "Hijo(a)/Hijastro(a)" 4 "Otros Parientes" 5 "Otros No parientes" 6 "Servicio Domestico (inc fam Serv. Dom.)"
label value relacion_ci relacion_ci


****************************
***VARIABLES DEMOGRAFICAS***
****************************


**********
***sexo***
**********
gen sexo_ci=cmhp19
label var sexo_ci "Sexo del Individuo"
label define sexo_ci 1 "Masculino" 2 "Femenino"
label value sexo_ci sexo_ci


**********
***edad***
**********
** Generating Edad

gen edad_ci=cmhp18
label var edad_ci "Edad del Individuo"

*****************
***civil_ci***
*****************
gen byte civil_ci=.
replace civil_ci=1 if cmhp22  ==8
replace civil_ci=2 if (cmhp22  >=1  & cmhp22<=4)
replace civil_ci=3 if cmhp22  ==5 | cmhp22==6
replace civil_ci=4 if cmhp22  ==7
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


*************************************************************************************
*******************************INGRESOS**********************************************
*************************************************************************************
**************
***ocupa_ci***
**************
*La clasificación no corresponde a la clasificación estandarizada de ocupación tp47
tab tmhp40

gen ocupa_ci=.
replace ocupa_ci=1 if tmhp40>=2 & tmhp40<4
replace ocupa_ci=2 if tmhp40==1
replace ocupa_ci=3 if tmhp40==4
replace ocupa_ci=4 if tmhp40==5 
replace ocupa_ci=6 if tmhp40==6
replace ocupa_ci=7 if tmhp40>=7 & tmhp40<9
replace ocupa_ci=8 if tmhp40==10
replace ocupa_ci=8 if tmhp40==9
label var ocupa_ci "Ocupacion Laboral en la Actividad Principal"
label define ocupa_ci 1 "PROFESIONALES Y TECNICOS" 2 "GERENTES, DIRECTORES Y FUNCIONARIOS SUPERIORES"  3 "PERSONAL ADMINISTRATIVO Y NIVEL INTERMEDIO" 4 "COMERCIANTES Y VENDEDORES" 5 "TRABAJADORES EN SERVICIOS" 6"TRABAJADORES AGRICOLAS Y AFINES" 7 "OBREROS NO AGRICOLAS, CONDUCTORES DE MAQUINAS Y VEHICULOS DE TRANSPORTE Y SIMILARES" 8"FUERZAS ARMADAS" 9"OTRAS OCUPACIONES NO CLASIFICADAS ANTERIORMENTE"
label values ocupa_ci ocupa_ci

*****************
***horastot_ci***
*****************
recode tmhp48 (98=.) (99=.)

gen byte horastot_ci=.
replace horastot_ci=tmhp48 if tmhp48>=0 & tmhp48 <168
label var horastot_ci "Horas totales trabajadas la semana pasada en todas las Actividades"


************************************
*** VARIABLES DEL MERCADO LABORAL***
************************************

****************
****condocup_ci*
****************
gen condocup_ci=.
replace condocup_ci=1 if (tmhp36>=1 & tmhp36<=2) 
replace condocup_ci=2 if (tmhp36 >=3 & tmhp36 <=4)  
replace condocup_ci=3 if condocup_ci!=1 & condocup_ci!=2
replace condocup_ci=4 if edad_ci<10
label define condocup_ci 1"ocupados" 2"desocupados" 3"inactivos" 4"menor de PET"
label value condocup_ci condocup_ci
label var condocup_ci "Condicion de ocupacion utilizando definicion del pais"



******************
***categopri_ci***
******************

gen categopri_ci=.
replace categopri_ci=1 if tmhp43  ==5 & condocup_ci==1
replace categopri_ci=2 if (tmhp43  ==6 | tmhp43  ==7 | tmhp43  ==9)  & condocup_ci==1
replace categopri_ci=3 if (tmhp43  >=1 & tmhp43  <=4)   & condocup_ci==1
replace categopri_ci=4 if tmhp43  ==8 & condocup_ci==1
label var categopri_ci "CATEGORIA OCUPACIONAL ACTIVIDAD PRINCIPAL"
label define categopri_ci 1 "Patron" 2 "Cuenta Propia" 3 "Asalariado" 4 "Trabajador No Remunerado" 
label values categopri_ci categopri_ci


****************
*afiliado_ci****
****************

gen afiliado_ci=0     if condocup_ci==1 | condocup_ci==2 
replace afiliado_ci=1 if ( tmhp47ss==4 | cmhp25a ==1 | cmhp25b ==2 | pmhp60a==1) & afiliado_ci==0
label var afiliado_ci "Afiliado a la Seguridad Social"

****************
*cotizando_ci***
****************
*Las preguntas relacionadas con pensiones son respondidas por personas a partir de los 40 años pp61, pp61ss, pp61ep 

gen cotizando_ci=0     if condocup_ci==1 | condocup_ci==2 
replace cotizando_ci=1 if (pmhp611 ==1 | pmhp612==2 | pmhp613==3 | pmhp614==4) & cotizando_ci==0
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
replace tipocontrato_ci =1 if tmhp46 ==1
replace tipocontrato_ci =2 if tmhp46 ==2
replace tipocontrato_ci =3 if tmhp46 ==3 | tmhp46==4
label var tipocontrato_ci "Tipo de contrato segun su duracion en act principal"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci

*************
*tamemp_ci***
*************
gen tamemp_ci=1 if tmhp42>=1 & tmhp42<=3
label var  tamemp_ci "Tamaño de Empresa" 
*Empresas medianas
replace tamemp_ci=2 if tmhp42>=4 & tmhp42<=5
*Empresas grandes
replace tamemp_ci=3 if tmhp42>=6 & tmhp42<=7
label define tamaño 1"Pequeña" 2"Mediana" 3"Grande"
label values tamemp_ci tamaño
tab tamemp_ci [iw=factor_ci]

gen categoinac_ci = .
replace categoinac_ci = 1 if ((tmhp36==9) & condocup_ci==3)
replace categoinac_ci = 2 if ((tmhp36==8) & condocup_ci==3)
replace categoinac_ci = 3 if ((tmhp36==5) & condocup_ci==3)
replace categoinac_ci = 4 if ((categoinac_ci ~=1 & categoinac_ci ~=2 & categoinac_ci ~=3) & condocup_ci==3)
label var categoinac_ci "Categoría de inactividad"
label define categoinac_ci 1 "Jubilados o pensionados" 2 "Estudiantes" 3 "Quehaceres domésticos" 4 "Otros"
label values categoinac_ci categoinac_ci


*************
**pension_ci*
*************
*DZ se agrega la variable pension contributiva*
gen pension_ci=.
replace pension_ci=1 if ((pmhp58j ==1 | pmhp58p==2) | (pmhp60a==1 | pmhp60b==2 | pmhp60c==3 | pmhp60d==4))
replace pension_ci=0 if (( pmhp58njp ==3) | (pmhp60a==0 & pmhp60b==0 & pmhp60c==0 & pmhp60d==0))
label var pension_ci "1=Recibe pension contributiva"
 
*************
*ypen_ci*
*************
*DZ se agrega la variable valor de la pension contributiva*

gen ypen_ci= pmhp60bs if pension_ci==1
replace ypen_ci =. if pmhp60bs==98| pmhp60bs==99
label var ypen_ci "Valor de la pension contributiva"


***************
*pensionsub_ci*
***************
*DZ Octubre 2017-Se crea la variable de pension subsidiada-Gran Mision en amor mayor

gen pensionsub_ci=( mmhp63m1==14 |  mmhp63m2==14 | mmhp63m3==14)
replace pensionsub_ci=. if (( mmhp63m1>=98 & mmhp63m1<=99) & ( mmhp63m2>=98 & mmhp63m2<=99) & ( mmhp63m3>=98 & mmhp63m3<=99))

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
replace cesante_ci=1 if ( tmhp36==4) & condocup_ci==2
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
/*La encuesta toma como periodo de referencia el periodo Agosto 2017(Fue levantada entre julio y septiembre)
Se toma el salario de Julio, http://www.sistematemis.com.ve/Salarios-Minimos-Venezuela?action=  */
gen salmm_ci=97531.56
label var salmm_ci "Salario minimo legal"


*************
***tecnica_ci**
*************
gen tecnica_ci=(emhp28n==5)
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
*No se pregunta específicamente la actividad principal, se pregunta las horas en promedio que trabajó la semana anterior.
gen byte horaspri_ci=.
replace horaspri_ci=tmhp48  if tmhp48>=0 & tmhp48 <168
label var horaspri_ci "Horas totales trabajadas la semana pasada en la Actividad Principal"


****************
***durades_ci***
****************
* Variable categórica tmhp38
gen durades_ci = .


*****************
***desalent_ci***
*****************
gen desalent_ci=(tmhp39 ==1 | tmhp39 ==3)
label var desalent_ci "Trabajadores desalentados: personas que creen que por alguna razón no conseguirán trabajo"
label define desalent_ci 1 "Trabajador desalentado" 0 "No es trabajador desalentado" 
label values desalent_ci desalent_ci 


***************
***subemp_ci***
***************
/*No es posible construir esta variable ya que la información que brinda no corresponde a lo que se pregunta, esto ocurre también para la pregunta tmhp51*

tab tmhp50
¿Preferiría... trabajar más de 35 horas |
                            por semana? |      Freq.     Percent        Cum.
----------------------------------------+-----------------------------------
                    Alimentación/Mercal |        544        2.27        2.27
                         Barrio Adentro |        564        2.36        4.63
                              No aplica |     22,597       94.46       99.09
                    No sabe/No responde |        217        0.91      100.00
----------------------------------------+-----------------------------------
                                  Total |     23,922      100.00
								  
. tab tmhp51

    ¿...Ha hecho algo para trabajar más |
                                 horas? |      Freq.     Percent        Cum.
----------------------------------------+-----------------------------------
                    Alimentación/Mercal |        377        1.58        1.58
                         Barrio Adentro |        146        0.61        2.19
                              No aplica |     23,097       96.55       98.74
                    No sabe/No responde |        302        1.26      100.00
----------------------------------------+-----------------------------------
                                  Total |     23,922      100.00
*/

gen subemp_ci=.
label var subemp_ci "Personas que trabajan 30 horas a la semana o menos y están dispuestas a trabajar más"
label define subemp_ci 1 "Subempleado " 0 "No subempleado" 
label values subemp_ci subemp_ci 

*******************
***tiempoparc_ci***
*******************
*No se puede construir esta variable por la misma razón que no se pudo construir la variable anterior, la información no corresponde a la pregunta.*

gen tiempoparc_ci=.


*************
***rama_ci***
*************

gen rama_ci=.
label var rama_ci "RAMA"
label define rama_ci 1 "Agricultura, caza, silvicultura y pesca" 2 "Explotación de minas y canteras" 3 "Industrias manufactureras" 4 "Electricidad, gas y agua" 5 "Construcción" 6"Comercio al por mayor y menor, restaurantes, hoteles" 7"Transporte y almacenamiento" 8"Establecimientos financieros, seguros, bienes inmuebles" 9"Servicios sociales, comunales y personales"
label values rama_ci rama_ci

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
label var nempleos "Numero de empleos"

*****************
***spublico_ci***
*****************
gen byte spublico_ci=.
replace spublico_ci=1 if emp_ci==1 & (tmhp43  ==1 | tmhp43  ==2) 
replace spublico_ci=0 if emp_ci==1 & (tmhp43  >2 & tmhp43  <=9) 
label var spublico "Personas que trabajan en el sector publico"

****************
***ylmpri_ci ***
****************
recode tmhp44bs (99=.) (98=.)
gen ylmpri_ci=tmhp44bs
label var ylmpri_ci "Ingreso Laboral Monetario de la Actividad Principal"

*******************
*** ylmhopri_ci ***
*******************
gen ylmhopri_ci=.
replace ylmhopri_ci=ylmpri_ci/(horaspri_ci*4.3)
label var ylmhopri_ci "Salario Horario Monetario de la Actividad Principal"

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
egen ylm_ci= rsum(ylmpri_ci ylmsec_ci),m
replace ylm_ci=. if ylmpri_ci==. &  ylmsec_ci==.
label var ylm_ci "Ingreso laboral monetario total"  

*************
***ylnm_ci***
*************
gen ylnm_ci=.
label var ylnm_ci "Ingreso Laboral NO Monetario Total"

*************
***ynlm_ci***
*************

recode tmhp45bs (98=.) (99=.)
gen pensiones=pmhp60bs if pmhp60bs>99
*La cifra declarada en tmhp45bs (incluida en ylmpri_ci) a veces se duplica en las preguntas pmhp60bs* relacionadas con pensiones porque los mayores a 40 años que aún trabajan declaran el mismo monto en los dos
g aux = tmhp45bs-pensiones

replace pensiones = pensiones - tmhp45bs if aux== 0 // se hace este ajuste para evitar duplicar las cifras.

egen ynlm_ci= rsum(pensiones tmhp45bs), missing
label var ynlm_ci "Ingreso NO Laboral Monetario"

*************
***ynlnm_ci***
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

************************
*** HOUSEHOLD INCOME ***
************************

******************
*** nrylmpri_ch***
******************
capture drop nrylmpri_ch
sort idh
egen nrylmpri_ch=sum(nrylmpri_ci) if miembros_ci==1, by(idh)
replace nrylmpri_ch=1 if nrylmpri_ch>1 & nrylmpri_ch~=. & miembros_ci==1
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
***antiguedad_ci***
*******************
gen antiguedad_ci=.
label var antiguedad_ci "Antiguedad en la actividad actual"


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

***************
***asiste_ci***
***************
gen byte asiste_ci=.
replace asiste_ci=1 if emhp29 ==1 
replace asiste_ci=0 if emhp29 ==2
label var asiste "Personas que actualmente asisten a centros de enseñanza"


***************
***/aedu_ci****
***************

recode emhp28a emhp28n  emhp28s  (99=.) (98=.)

gen byte aedu_ci=.
replace aedu_ci=0               if emhp28n==1 | emhp28n==2
replace aedu_ci=emhp28a           if emhp28n==3
replace aedu_ci=emhp28a+6         if emhp28n==4
replace aedu_ci=(11+(0.5*emhp28s))  if (emhp28n==5 | emhp28n==6)  // técnico (TSU) | Universitario
replace aedu_ci=(17+(0.5*emhp28s))  if emhp28n==7 //posgrado
label variable aedu_ci "Años de Educacion"



**************
***eduno_ci***
**************
gen eduno_ci=.
replace eduno=1 if emhp28n==1
replace eduno=0 if emhp28n>1 & emhp28n<=7
label var eduno_ci "1 = personas sin educacion (excluye preescolar)"

***************
***edupre_ci***
***************
gen edupre_ci=.
replace edupre=1 if emhp28n==2
replace edupre=0 if emhp28n>2 | emhp28n==1
label var edupre_ci "Educacion preescolar"

	q
**************
***edupi_ci***
**************
gen edupi_ci=.
replace edupi_ci=1 if aedu_ci>0 & aedu_ci<6
replace edupi_ci=0 if aedu_ci==0 | (aedu_ci>=6 & aedu_ci!=.)
label var edupi_ci "1 = personas que no han completado el nivel primario"

**************
***edupc_ci***
**************
gen edupc_ci=.
replace edupc_ci=1 if aedu_ci==6
replace edupc_ci=0 if (aedu_ci>=0 & aedu_ci<6)  | (aedu_ci>6 & aedu_ci!=.) 
label var edupc_ci "1 = personas que han completado el nivel primario"

**************
***edusi_ci***
**************
gen edusi_ci=.
replace edusi=1 if aedu_ci>6 & aedu_ci<12
replace edusi=0 if (aedu_ci>=0 & aedu_ci<=6) | (aedu_ci>=11 & aedu_ci!=.)
label var edusi_ci "1 = personas que no han completado el nivel secundario"

**************
***edusc_ci***
**************
gen edusc_ci=.
replace edusc=1 if aedu_ci==12 
replace edusc=0 if (aedu_ci>=0 & aedu_ci<12) 
label var edusc_ci "1 = personas que han completado el nivel secundario"


**************
***eduui_ci***
**************


gen eduui_ci= (aedu_ci>12 & aedu_ci<17)
label var eduui_ci "1 = personas que no han completado el nivel universitario o superior"

***************
***eduuc_ci***
***************
gen byte eduuc_ci= (aedu_ci>=17)
label var eduuc_ci "1 = personas que han completado el nivel universitario o superior"

***************
***edus1i_ci***
***************
gen edus1i_ci=0
replace edus1i_ci=1 if (aedu_ci>6 & aedu_ci<9)
label variable edus1i_ci "1er ciclo de la secundaria incompleto"

***************
***edus1c_ci***
***************
gen edus1c_ci=0
replace edus1c_ci=1 if aedu_ci==9
label variable edus1c_ci "1er ciclo de la secundaria completo"

***************
***edus2i_ci***
***************
gen edus2i_ci=0
replace edus2i_ci=1 if aedu_ci>9 & aedu_ci<12
label variable edus2i_ci "2do ciclo de la secundaria incompleto"

***************
***edus2c_ci***
***************
gen edus2c_ci=0
replace edus2c_ci=1 if aedu_ci==12
label variable edus2c_ci "2do ciclo de la secundaria completo"

local var = "eduno edupi edupc edusi edusc edusc eduui eduuc edus1i edus1c edus2i edus2c"
foreach x of local var {
replace `x'_ci=. if aedu_ci==.
}


**************
***eduac_ci***
**************
gen eduac_ci=.
label var eduac_ci "Educacion terciaria académica versus educación terciaria no-académica "


***************
***asispre_ci**
***************
g asispre_ci=.
replace asispre_ci=1 if asiste_ci==1 & (emhp28n==2)
recode asispre_ci (.=0)
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
replace edupub_ci = 1 if emhp32 == 2
replace edupub_ci = 0 if emhp32 == 1
label var edupub_ci "1 = personas que asisten a centros de enseñanza publicos"


**************
***pqnoasis***
**************
gen byte pqnoasis_ci=.
replace pqnoasis=emhp31 if emhp31>0 & emhp31<98
label var pqnoasis_ci "Razones para no asistir a centros de enseñanza"
label define pqnoasis_ci 1 "Culmino sus estudios" 2 "Escuela distante" 3 "Escuela cerrada" 4 "Muchos paros/inasistencia de maestros" 5 "Costo de los útiles" 6 "Costo de los uniformes" 7 "Enfermedad/Discapacidad " 8 "Tiene que trabajar " 9 "No quiso seguir estudiando "  10 " Inseguridad al asistir al centro educat " 11 "Discriminación o violencia" 12 "Por embarazo/cuidar los hijos" 13 "Tiene que ayudar en tareas del hogar " 14 "No lo considera importante " 15 "otros"
label values pqnoasis_ci pqnoasis_ci

**************
*pqnoasis1_ci*
**************
**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**

g       pqnoasis1_ci = 1 if pqnoasis_ci ==5 & pqnoasis_ci==6
replace pqnoasis1_ci = 2 if pqnoasis_ci ==8
replace pqnoasis1_ci = 3 if pqnoasis_ci ==7  
replace pqnoasis1_ci = 4 if pqnoasis_ci ==14 | pqnoasis_ci==9
replace pqnoasis1_ci = 5 if pqnoasis_ci ==13 | pqnoasis_ci==12
replace pqnoasis1_ci = 6 if pqnoasis_ci ==1
replace pqnoasis1_ci = 8 if pqnoasis_ci ==2  
replace pqnoasis1_ci = 9 if pqnoasis_ci ==15 | pqnoasis_ci==11 | pqnoasis_ci==10 | pqnoasis_ci==4 | pqnoasis_ci==3

label define pqnoasis1_ci 1 "Problemas económicos" 2 "Por trabajo" 3 "Problemas familiares o de salud" 4 "Falta de interés" 5	"Quehaceres domésticos/embarazo/cuidado de niños/as" 6 "Terminó sus estudios" 7	"Edad" 8 "Problemas de acceso"  9 "Otros"
label value  pqnoasis1_ci pqnoasis1_ci

********************************************
***Variables de Infraestructura del hogar***
********************************************

***************
**aguared_ch***
***************

gen aguared_ch = (vsp5==1)
label var aguared_ch "Acceso a fuente de agua por red"

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
replace bano_ch = 1 if vsp8>=1 & vsp8<=4
replace bano_ch = 0 if vsp8==5

label var bano_ch "El hogar tiene algun tipo de servicio higienico"

***************
***banoex_ch***
***************

gen banoex_ch=1 if dhp11==1
replace banoex_ch=0 if dhp11==2
label var banoex_ch "El servicio higiénico es de uso exclusivo del hogar"


*************
***des1_ch***
*************

gen des1_ch=.
replace des1_ch=0 if vsp8==5
replace des1_ch=1 if vsp8==1 | vsp8==2 
replace des1_ch=2 if vsp8==4 | vsp8==3
label var des1_ch "Tipo de desagüe incluyendo definición de unimproved del MDG"

*************
***des2_ch***
*************


gen des2_ch=.
replace des2_ch=0 if vsp8==5
replace des2_ch=1 if vsp8==1 | vsp8==2 | vsp8==3 | vsp8==4
label var des2_ch "Tipo de desagüe sin incluir la definición de unimproved de los MDG"

*************
***piso_ch***
*************

gen piso_ch=.
replace piso_ch=0 if vsp1==3
replace piso_ch=1 if vsp1==1 | vsp1==2 
replace piso_ch=2 if vsp1==4 | vsp1==5

label var piso_ch "Material predominante en el piso de la casa"
label define piso_ch 0 "Tierra" 1 "Materiales permanentes" 2 "Otros"
label values piso_ch piso_ch

**************
***pared_ch***
**************
gen pared_ch=.
replace pared_ch=0 if vsp2==8
replace pared_ch=1 if vsp2>=1 & vsp2<5
replace pared_ch=2 if vsp2==7 | vsp2==6

label var pared_ch "Materiales de construcción de las paredes"
label define pared_ch 0 "No permanentes" 1 "Materiales permanentes" 2 "Otros materiales"
label values pared_ch pared_ch

**************
***techo_ch***
**************

gen techo_ch=.
replace techo_ch=0 if vsp3==5
replace techo_ch=1 if vsp3>=1 & vsp3<=4
*No hay categorías que pueden clasificarse en "Otros materiales"

label var techo_ch "Material de construcción del techo"
label define techo_ch 0 "No permanentes" 1 "Materiales permanentes" 2 "Otros materiales" 
label values techo_ch techo_ch

**************
***resid_ch***
**************
gen resid_ch=.
*Se elimina la pregunta en 2016
label var resid_ch "Método de eliminación de residuos"
label def resid_ch 0"Recolección pública o privada" 1"Quemados o enterrados"
label def resid_ch 2"Tirados a un espacio abierto" 3"Otros", add
label val resid_ch resid_ch

**Daniela Zuluaga- Enero 2018: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**
	
*********************
***aguamejorada_ch***
*********************
g       aguamejorada_ch = .
		
*********************
***banomejorado_ch***
*********************
g       banomejorado_ch = .

*************
***dorm_ch***
*************
recode dhp10 (98=.) (99=.)
gen dorm_ch=dhp10 
label var dorm_ch "Cantidad de habitaciones que se destinan exclusivamente para dormir"

****************
***cuartos_ch***
****************

gen cuartos_ch=.
label var cuartos_ch "Cantidad de habitaciones en el hogar"
	
***************
***cocina_ch***
***************

gen cocina_ch=.
label var cocina_ch "Cuarto separado y exclusivo para cocinar"

**************
***telef_ch***
**************

gen telef_ch=1 if hp14t==1
replace telef_ch=0 if hp14t==2
label var telef_ch "Hogar tiene servicio telefónico fijo"
label define telef_ch 0 "No" 1 "Sí"
label values telef_ch telef_ch

***************
***refrig_ch***
***************

gen refrig_ch=.
replace refrig_ch=1 if hp14n ==1
replace refrig_ch=0 if hp14n ==2
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
gen auto_ch=(dhp13 >=1 & dhp13 <8)
label var auto_ch "El hogar posee automóvil particular"
label define auto_ch 0 "No" 1 "Sí"
label values auto_ch auto_ch

**************
***compu_ch***
**************

gen compu_ch=.
replace compu_ch=1 if hp14c ==1
replace compu_ch=0 if hp14c ==2
label var compu_ch "El hogar posee computadora"
label define compu_ch 0 "No" 1 "Sí"
label values compu_ch compu_ch


*****************
***internet_ch***
*****************

gen internet_ch=.
replace internet_ch=1 if hp14i==1
replace internet_ch=0 if hp14i==2
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
replace vivi1_ch = 1 if vsp4 ==1 | vsp4 ==2
replace vivi1_ch = 2 if vsp4 ==3
replace vivi1_ch = 3 if vsp4 >=4 & vsp4 <=7
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
*Revisar la categor'ia 3
gen viviprop_ch=.
replace viviprop_ch=0 if hp15==3 
replace viviprop_ch=1 if hp15==1
replace viviprop_ch=2 if hp15==2
replace viviprop_ch=3 if hp15>=4 & hp15  <=7
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
label var vivialq_ch "Alquiler mensual de la vivienda"

*******************
***vivialqimp_ch***
*******************

gen vivialqimp_ch=.

*******************
***  benefdes_ci  ***
*******************

g benefdes_ci=.
label var benefdes_ci "=1 si tiene seguro de desempleo"

*******************
*** ybenefdes_ci  ***
*******************
g ybenefdes_ci=.
label var ybenefdes_ci "Monto de seguro de desempleo"


******************************
*** VARIABLES DE MIGRACION ***
******************************

* Variables incluidas por SCL/MIG Fernando Morales

	*******************
	*** migrante_ci ***
	*******************
	
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


do "$ruta\harmonized\_DOCS\\Labels&ExternalVars_Harmonized_DataBank.do"

/*_____________________________________________________________________________________________________*/
* Verificación de que se encuentren todas las variables armonizadas 
/*_____________________________________________________________________________________________________*/

order region_BID_c region_c pais_c anio_c mes_c zona_c factor_ch	idh_ch	idp_ci	factor_ci upm_ci estrato_ci sexo_ci edad_ci ///
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
vivi1_ch vivi2_ch viviprop_ch vivitit_ch vivialq_ch	vivialqimp_ch migrante_ci migantiguo5_ci migrantelac_ci, first



compress


saveold "`base_out'", replace


log close

