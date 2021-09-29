

* (Versión Stata 12)
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

local PAIS GTM
local ENCUESTA ENCOVI
local ANO "2006"
local ronda m7_m11


local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
*local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\\`PAIS'_`ANO'`ronda'.dta"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
                                                    
capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Guatemala
Encuesta: ENCOVI
Round: a
Autores: 
Última versión: Mayra Sáenz E-mail: mayras@iadb.org / saenzmayra.a@gmail.com
Fecha última modificación: 24 de Septiembre de 2013
Nueva modificacion: 1/23/2014, by MLO
							SCL/LMK - IADB
****************************************************************************/
/***************************************************************************
Detalle de procesamientos o modificaciones anteriores:

****************************************************************************/


use `base_in', clear
/*
foreach v of varlist _all {
      capture rename `v' `=lower("`v'")'
   }

*/
*****************************************************************************************************************
******                                    GUATEMALA 2006                                                   ******
******                ENCOVI 2006 (ENCUESTA NACIONAL DE CONDICIONES DE VIDA)                               ******
******                                    68.739 personas                                                  ****** 
*****************************************************************************************************************




gen factor_ci=FACTOR
label var factor_ci "Factor de Expansion del Individuo"
**************************************************************************************************************
* HOUSEHOLD VARIABLES
**************************************************************************************************************

	****************
	* region_BID_c *
	****************
	
* REGION

gen region_c=.
	
gen region_BID_c=1
label var region_BID_c "Regiones BID"
label define region_BID_c 1 "Centroamérica_(CID)" 2 "Caribe_(CCB)" 3 "Andinos_(CAN)" 4 "Cono_Sur_(CSC)"
label value region_BID_c region_BID_c


gen factor_ch=FACTOR
label var factor_ch "Factor de expansion del Hogar"

* ZONA
gen byte zona_c=1 if AREA==1 /* Urbana */
replace zona_c=0 if AREA==2 /* Rural */
label variable zona_c "ZONA GEOGRAFICA"
label define zona_c 0 "Rural" 1 "Urbana"
label value zona_c zona_c

* COUNTRY - YEAR
gen str3 pais_c="GTM"
label variable pais_c "Nonmbre del Pais"

gen anio_c=2006
label variable anio_c "Año de la Encuesta"

* Periodo de Referencia: del 07/00 al 11/00.
* This is the middle of the reference period
gen byte mes_c=9
label variable mes_c "Mes de la Encuesta"

* SEXO
gen sexo_ci=PPA02
label variable sexo "Sex of the individual"
label var sexo_ci "Sexo del Individuo"
label define sexo_ci 1 "Hombre" 2 "Mujer"  
label value sexo_ci sexo_ci

* PARENTESCO
gen relacion_ci=1 if PPA05==1
replace relacion_ci=2 if PPA05==2
replace relacion_ci=3 if PPA05==3
replace relacion_ci=4 if ( PPA05==4 | PPA05==5 | PPA05==6 | PPA05==7 | PPA05==8 | PPA05==9 | PPA05==10 )
replace relacion_ci=5 if ( PPA05==12 | PPA05==13 )
replace relacion_ci=6 if PPA05==11
label var relacion_ci "Parentesco o relacion con el Jefe del Hogar"
label define relacion_ci 1 "Jefe(a)" 2 "Esposo(a) o compañero(a)" 3 "Hijo(a)" 4 "Otro pariente" 5 "Otro NO pariente" 6 "Empleada domestica" 
label value relacion_ci relacion_ci


* PPA03
* 99 is the top-code, not that age is missing 
* meses is also available
gen edad_ci=PPA03
label var edad_ci "Edad del Individuo"

* IDENTIFICADOR DEL HOGAR
gen idh_ch=NUM_HOG 
label var idh_ch "Identificador Unico del Hogar"

* IDENTIFICADOR DE LA PERSONA
egen idp_ci=group(NUM_HOG ID) 
label var idp_ci "Identificador Individual dentro del Hogar"

sort idh_ch idp_ci

egen nconyuges_ch=sum(relacion_ci==2), by (idh_ch)
label variable nconyuges_ch "Numero de Conyuges"

egen nhijos_ch=sum(relacion_ci==3), by (idh_ch)
label variable nhijos_ch "Numero de Hijos"
egen notropari_ch=sum(relacion_ci>3 & relacion_ci<5), by (idh_ch)
label variable notropari_ch "Numero de Otros Parientes "
egen notronopari_ch=sum(relacion_ci==5), by (idh_ch)
label variable notronopari_ch "Numero de Otros NO Parientes "
egen nempdom_ch=sum(relacion_ci==6), by (idh_ch)
label variable nempdom_ch "Numero de Empleados Domesticos"

* HOUSEHOLD TYPE (unipersonal, nuclear, ampliado, compuesto, corresidentes)    
* note: These are all defined in terms of relationship to household head

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



* HOUSEHOLD COMPOSITION VARIABLES 
/* note: These are unrelated to who is the head
   note: That childh denotes the number of children of the head, while numkids counts the number of all kids in the household */

sort idh_ch

* NUMBER OF PERSONS IN THE HOUSEHOLD (not including domestic employees or other relatives)
egen nmiembros_ch=sum(relacion_ci>0 & relacion_ci<5 ), by (idh_ch)
label variable nmiembros_ch "Numero de miembros en el Hogar"

egen nmayor21_ch=sum((relacion_ci>0 & relacion_ci<5 ) & (edad>=21)), by (idh_ch)
label variable nmayor21_ch "Numero de personas de 21 años o mas dentro del Hogar"

egen nmenor21_ch=sum((relacion_ci>0 & relacion_ci<5 ) & (edad<21)), by (idh_ch)
label variable nmenor21_ch "Numero de personas menores a 21 años dentro del Hogar"

egen nmayor65_ch=sum((relacion_ci>0 & relacion_ci<5 ) & (edad>=65)), by (idh_ch)
label variable nmayor65_ch "Numero de personas de 65 años o mas dentro del Hogar"

egen nmenor6_ch=sum((relacion_ci>0 & relacion_ci<5 ) & (edad<6)), by (idh_ch)
label variable nmenor6_ch "Numero de niños menores a 6 años dentro del Hogar"

egen nmenor1_ch=sum((relacion_ci>0 & relacion_ci<5 ) & (edad<1)),  by (idh_ch)
label variable nmenor1_ch "Numero de niños menores a 1 año dentro del Hogar"



*** ESTADO CIVIL PARA PERSONAS DE 10 AÑOS O MAS DE PPA03
gen civil_ci=.  
replace civil_ci=1 if PPA06==7 /* SOLTERO */
replace civil_ci=2 if PPA06==1 | PPA06==2 /* UNION FORMAL O INFORMAL */
replace civil_ci=3 if PPA06==3 | PPA06==4 | PPA06==5 /* SEPARADO O DIVORCIADO */
replace civil_ci=4 if PPA06==6 /* VIUDO */
label var civil_ci "Estado Civil"
label define civil_ci 1 "Soltero" 2 "Union Formal o Informal" 3 "Divorciado o Separado" 4 "Viudo"
label value civil_ci civil_ci

*** REPORTED HEAD OF HOUSEHOLD
gen jefe_ci=0
replace jefe_ci=1 if relacion_ci==1
label var jefe_ci "Jefe de Hogar Declarado"

*** We want to know if there is only one head in each hh and if there is a hh with no head:
egen hh=sum(jefe_ci), by (idh_ch)
capture assert hh==1

*** HOUSING ***

gen aguared_ch=.
replace aguared_ch=1 if P01A05A==1
replace aguared_ch=0 if P01A05A==2

gen aguadist_ch=.
replace aguadist_ch=1 if  P01D12A==0
replace aguadist_ch=2 if  P01D12A>0 & P01D12A<=1000
replace aguadist_ch=3 if  P01D12A>1000

gen aguamala_ch=.
replace aguamala_ch=1 if P01D06==5 | P01D06==6 | P01D06==7 | P01D06==8
replace aguamala_ch=0 if P01D06==1 | P01D06==2 | P01D06==3 | P01D06==4

gen aguamide_ch=.
replace aguamide_ch=1 if P01A05E==1
replace aguamide_ch=0 if P01A05E==2

gen luz_ch=.
replace luz_ch=1 if P01A05C==1
replace luz_ch=0 if P01A05C==2

gen luzmide_ch=.
replace luzmide_ch=1 if P01A05F==1
replace luzmide_ch=0 if P01A05F==2

/*gen combust_ch=.
replace combust_ch=1 if 
replace combust_ch=0 if 
*/

gen bano_ch=.
replace bano_ch=1 if P01D16==1 | P01D16==2 | P01D16==3 | P01D16==4
replace bano_ch=0 if P01D16==5

gen banoex_ch=.
replace banoex=1 if P01D17==1
replace banoex=0 if P01D17==2


* Modificaciones Marcela Rubio Septiembre 2014: variable habia sido generada como missing

gen des1_ch=.
replace des1_ch=0 if P01D16==5
replace des1_ch=1 if P01D16==1 | P01D16==2 | P01D16==3
replace des1_ch=2 if P01D16==4

* Modificaciones Marcela Rubio Septiembre 2014: variable habia sido generada como missing
 
gen des2_ch=.
replace des2_ch=0 if P01D16==5
replace des2_ch=1 if P01D16==1 | P01D16==2 | P01D16==3 | P01D16==4


gen piso_ch=.
replace piso_ch=0 if P01A04==7
replace piso_ch=1 if P01A04>=1 & P01A04<=6
replace piso_ch=2 if P01A04==98

gen pared_ch=.
replace pared_ch=0 if P01A02==7 | P01A02==8
replace pared_ch=1 if P01A02>=1 & P01A02<=6
replace pared_ch=2 if P01A02==98

gen techo_ch=.
replace techo_ch=0 if P01A03==5 
replace techo_ch=1 if P01A03>=1 & P01A03<=4
replace techo_ch=2 if P01A03==98

gen resid_ch=.
replace resid_ch=0 if P01D20==1 | P01D20==2
replace resid_ch=1 if P01D20==3 | P01D20==4
replace resid_ch=2 if P01D20==5
replace resid_ch=3 if P01D20==6 | P01D20==98

**Daniela Zuluaga- Enero 2018: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**
	
*********************
***aguamejorada_ch***
*********************
g       aguamejorada_ch = 1 if (P01D06 >=1 & P01D06  <=4) | P01D06  ==7
replace aguamejorada_ch = 0 if (P01D06  >=5 & P01D06  <=6) | P01D06  ==98
		
*********************
***banomejorado_ch***
*********************
g       banomejorado_ch = 1 if (P01D16 >=1 & P01D16 <=4) & P01D17 == 1
replace banomejorado_ch = 0 if ((P01D16 >=1 & P01D16 <=4) & P01D17 == 2) & P01D16 ==5 

gen dorm_ch=.
replace dorm_ch=P01D02 if P01D02>=0

gen cuartos_ch=.
replace cuartos_ch= P01D01 if  P01D01>=0

gen cocina_ch=.
replace cocina_ch=1 if P01D04==1
replace cocina_ch=0 if P01D04>=2 & P01D04<=7

gen telef_ch=.
replace telef_ch=1 if P01D18A==1
replace telef_ch=0 if P01D18A==2

gen refrig_ch=.
replace refrig_ch=1 if  P14A01A4 ==1
replace refrig_ch=0 if  P14A01A4 ==0

gen freez_ch=.

gen auto_ch=.
replace auto_ch=1 if P14A01A37==1 
replace auto_ch=0 if P14A01A37==0

gen compu_ch=.
replace compu_ch=1 if P14A01A12==1
replace compu_ch=0 if P14A01A12==0

gen internet_ch=.
replace internet_ch=1 if  P01D18C==1 
replace internet_ch=0 if  P01D18C==2

gen cel_ch=.
replace cel_ch=1 if P01D18B==1
replace cel_ch=0 if P01D18B==2

gen vivi1_ch=.
replace vivi1_ch=1 if P01A01==1
replace vivi1_ch=2 if P01A01==2
replace vivi1_ch=3 if P01A01>=3 & P01A01<=98

gen vivi2_ch=.
replace vivi2_ch=1 if vivi1_ch==1 | vivi1_ch==2
replace vivi2_ch=0 if vivi1_ch==3

gen viviprop_ch=.
replace viviprop_ch=0 if P01B01==3
replace viviprop_ch=1 if P01B01==1
replace viviprop_ch=2 if P01B01==2
replace viviprop_ch=3 if P01B01==4 | P01B01==98 

gen vivitit_ch=.


gen vivialq_ch=.
replace vivialq_ch=P01B03 if P01B03<99999

gen vivialqimp_ch=.
replace vivialqimp_ch=P01B02 if P01B02<99999



*******************************************************************************************
* VARIABLES DEL MERCADO LABORAL

* PERSONAS DE 5 AÑOS Y MAS DE PPA03 *
* EN 1998 ESTE BLOQUE DE PREGUNTAS ESTABA DIRIGIDO A LAS PERSONAS DE 7 AÑOS Y MAS DE PPA03 *
*******************************************************************************************
/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/

*************
**salmm_ci***
*************

*1 = GUA 2006
gen salmm_ci= 	1291.5
label var salmm_ci "Salario minimo legal"

*********
*lp_ci***
*********

gen lp_ci =6574
label var lp_ci "Linea de pobreza oficial del pais"

*********
*lpe_ci**
*********

gen lpe_ci =3206
label var lpe_ci "Linea de indigencia oficial del pais"

/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/

****************
*cotizando_ci***
****************
gen cotizando_ci=.
label var cotizando_ci "Cotizante a la Seguridad Social"

****************
*tipopen_ci*****
****************
gen tipopen_ci=.
label var tipopen_ci "Tipo de pension - variable original de cada pais" 

****************
*instpen_ci*****
****************
gen instpen_ci=.
label var instpen_ci "Institucion proveedora de la pension - variable original de cada pais" 

****************
*instcot_ci*****
****************
gen instcot_ci=P05D20
label var instcot_ci "Institucion proveedora de la pension - variable original de cada pais" 

****************
****condocup_ci*
****************

* Se considero ta,bien a quienes buscan trabajo hasta cuatro semanas anteriores. 05/19/2014 MGD
* MGR: Modifico serie en base a correcciones Laura Castrillo: delimitar la condición de edad para que no tome los missing en caso que existan
gen condocup_ci=.
replace condocup_ci=1 if (P10A02==1 | P10A04==1)
replace condocup_ci=2 if (P10A02==2 & P10A04==2) & (P10A06==1 | P10A07==1)
recode condocup_ci .=3 if edad_ci>=7 & edad_ci!=.
replace condocup_ci=4 if edad<7
label var condocup_ci "Condicion de ocupación de acuerdo a def de cada pais"
label define condocup_ci 1 "Ocupado" 2 "Desocupado" 3 "Inactivo" 4 "Menor de PET" 
label value condocup_ci condocup_ci

****************
*afiliado_ci****
****************
gen afiliado_ci=.	
replace afiliado_ci=1 if P10B26A==1
recode afiliado_ci .=0 if P10B26A!=1
replace afiliado_ci=0 if condocup_ci==2
label var afiliado_ci "Afiliado a la Seguridad Social"

*************
*cesante_ci* 
*************
* Correccion MGD 07/17/2014: mal generada la variable, se consideraba a inactivos P10F02 y no desempleados.
gen cesante_ci=1 if P10E05==2  
replace cesante_ci=0 if P10E05==1
label var cesante_ci "Desocupado - definicion oficial del pais"	


*************
*tamemp_ci
*************

gen tamemp_ci=1 if P10B25>=1 & P10B25<=5
replace tamemp_ci=2 if P10B25>=6 & P10B25<=50
replace tamemp_ci=3 if P10B25>=50 & P10B25!=.

label var tamemp_ci "# empleados en la empresa segun rangos"
label define tamemp_ci 1 "Pequena" 2 "Mediana" 3 "Grande"
label value tamemp_ci tamemp_ci

label var tamemp_ci "# empleados en la empresa"

*************
**pension_ci*
*************

gen pension_ci=1 if P11A04B>0 & P11A04B!=.
recode pension_ci .=0 
label var pension_ci "1=Recibe pension contributiva"

*************
**ypen_ci*
*************

gen ypen_ci=P11A04B/3 if P11A04B>0 & P11A04B!=.

label var ypen_ci "Valor de la pension contributiva"

***************
*pensionsub_ci*
***************

gen pensionsub_ci=.
label var pensionsub_ci "1=recibe pension subsidiada / no contributiva"

*****************
**ypensub_ci*
*****************

gen byte ypensub_ci=.
label var ypensub_ci "Valor de la pension subsidiada / no contributiva"


/*

* EMP
gen byte emp_ci=1 if ( P10B01==1 | P10B01==2| P10B01==3 )
replace emp_ci=0 if ( P10B01==. )
label var emp_ci "Empleado en la semana de referencia"

* DESEMP1
* Isolating workers self declared searchers that did not work (at all) last week
gen byte desemp1_ci=1 if ( (P10A06==1 & emp~=1) | (P10A02==2 & emp~=1))
replace desemp1_ci=0 if emp_ci==1 | (P10A06==2 & emp~=1 )
label var desemp1_ci "Personas que no tienen trabajo y han buscado trabajo la semana pasada"

/* DESEMP2 *** No esta la opcion de que la persona esta esperando una repsuesta ****
gen byte desemp2_ci=desemp1_ci
replace desemp2_ci=1 if (emp_ci~=1 & P10A04~=1 & P10A09==1)
replace desemp2_ci=0 if emp_ci==1 | (P10A04~=1 & P10A09>1 & P10A09<13)
label var desemp2_ci "desemp1 + personas que no tienen trabajo ni lo buscaron, pero esperan respuesta de una solicitud de empleo, entrevista" 


gen byte desemp20_ci=desemp1_ci
replace desemp20_ci=1 if (emp_ci~=1 & P10A04~=1 & (P10A09==1 | P10A09==2))
replace desemp20_ci=0 if emp_ci==1 | (P10A04~=1 & P10A09>2 & P10A09<13)
label var desemp20_ci "desemp1 + personas que no tienen trabajo ni lo buscaron, pero esperan respuesta de una solicitud de empleo, entrevista o temporada agricola" /* Incluye temporada agricola */
*/


gen byte desemp3_ci=.
replace desemp3_ci=desemp1_ci
replace desemp3_ci=1 if (emp~=1 & P10A06~=1 &  P10A07==1)
label var desemp3_ci "desemp2 + personas que no tienen trabajo pero han buscado trabajo durante las 4 semanas anteriores a la semana pasada"

* PEA: economically active population
gen byte pea1_ci=1 if ( emp==1 | desemp1_ci==1 )
label var pea1_ci "Poblacion Economicamente Activa utilizando la definicion 'desemp1'"
*gen byte pea2_ci=1 if ( emp==1 | desemp2_ci==1 )
*label var pea2_ci "Poblacion Economicamente Activa utilizando la definicion 'desemp2'"
gen byte pea3_ci=.
label var pea3_ci "Poblacion Economicamente Activa utilizando la definicion 'desemp3'"

replace pea1_ci=0 if pea1_ci~=1
*replace pea2_ci=0 if pea2_ci~=1
*/

************
***emp_ci***
************
gen byte emp_ci=(condocup_ci==1)
label var emp_ci "Ocupado (empleado)"

****************
***desemp_ci***
****************
gen desemp_ci=(condocup_ci==2)
label var desemp_ci "Desempleado que buscó empleo en el periodo de referencia"
  
*************
***pea_ci***
*************
gen pea_ci=0
replace pea_ci=1 if emp_ci==1 |desemp_ci==1
label var pea_ci "Población Económicamente Activa"

/* TRABAJADORES DESALENTADOS
gen byte desalent_ci=1 if pea2_ci~=1  & (P10A09==9 | P10A09==11) 
replace desalent_ci=0 if pea2_ci==1 | (P10A09!=9 & P10A09!=11)
label var desalent_ci "Trabajadores desalentados, personas que creen que por alguna razon no conseguiran trabajo" 
*/

gen desalent_ci=.


*** HORAS ACTIVIDAD PRINCIPAL
egen horaspri_ci=rsum(P10B27A P10B27B P10B27C P10B27D P10B27E P10B27F P10B27G), missing
replace horaspri_ci=. if P10B27A==. & P10B27B==. & P10B27C==. & P10B27D==. & P10B27E==. & P10B27F==. & P10B27G==. 
label var horaspri_ci "Horas totales trabajadas la semana pasada en la Actividad Principal"

*** HORAS ACTIVIDAD SECUNDARIA
*** 98 is top code
gen horassec_ci=P10C15 if P10C15<99
label variable horassec_ci "Horas totales trabajadas la semana pasada en la Actividad Secundaria"

gen horastot_ci=horaspri_ci+horassec_ci if horaspri_ci!=. & horassec_ci!=.
replace horastot_ci=horaspri_ci if horassec_ci==.
label var horastot_ci "Horas totales trabajadas la semana pasada en todas las Actividades"


* SUBEMPLEADO

* Modificacion: subempleo visible (desea trabajar mas horas y esta disponible para hacerlo). MGD 06/19/2014
gen subemp_ci=0
replace subemp_ci=1 if horaspri_ci<=30 &  P10D01==1 & emp_ci==1 & P10D05==1
label var subemp_ci "Trabajadores subempleados"

* Trabajadores a Medio Tiempo
gen byte tiempoparc_ci=.
replace tiempoparc_ci=1 if horastot_ci<=30  & ( P10D01==2) & emp==1
replace tiempoparc_ci=0 if ((horastot_ci>30  &  P10D01==2) |  P10D01==1) & emp_ci==1
label var tiempoparc_ci "Trabajadores a medio tiempo"

/** Contratos
gen contrato_ci=.
replace contrato_ci=1 if P10B05==1
replace contrato_ci=1 if P10B05==2
label var contrato_ci "Personas empleadas que han firmado un contrato de trabajo"
*/
* BENEFICIOS (Seguridad Social)
** La pregunta exacta, dirigida a todos los trabajadores, de la encuesta es: Paga una cuota al Seguro Social (IGSS) por el trabajo que tuvo?
/*gen segsoc_ci=1 if P10B26A==1
replace segsoc_ci=0 if P10B26A==2 | P10B26A==3 |P10B26A==4
label variable segsoc_ci "Personas que cuentan con seguro social"
*/
* Numero de ocupaciones
gen nempleos_ci=1 if emp_ci==1 & P10B01==1 /* 1 empleo */
replace nempleos_ci=2 if ( (emp_ci==1 & P10B01==2) | (emp_ci==1 & P10B01==3)  ) /* 'P10C01' pregunta si trabajo en una ocupacion secundaria */ 
replace nempleos_ci=0 if emp_ci~=1
label var nempleos_ci "Numero de empleos"
label define nempleos_ci 1 "un trabajo" 2 "dos o mas trabajos"
label values nempleos_ci nempleos_ci
/*
gen firmapeq_ci= .
replace firmapeq_ci= 1 if P10B25<=5
recode firmapeq_ci .=0 if P10B26A==1

* Tamaño de la firma
gen byte tamfirma_ci=.
replace tamfirma_ci=0 if P10B25<=5
replace tamfirma_ci=1 if P10B25>5 
label var tamfirma_ci "Trabajadores formales"
label define tamfirma_ci 1 "Mas de 5 trabajadores" 0 "5 o menos trabajadores"
label values tamfirma_ci tamfirma_ci
*/
*** Sector Publico
gen spublico_ci=1 if P10B04==1
replace spublico_ci=0 if P10B04>=2 & P10B04<=9
label var spublico_ci "Personas que trabajan en el sector publico"

****SELF-EMPLOYMENT
* Cuenta propia
gen byte selfemp=1 if P10B04==5 | P10B04==7
   
* INDEPENDENT WORKERS
* Patrono, cuenta propia 
gen byte indep=1 if ( P10B04>=5 & P10B04<=8 )

************************************************************************************************************
*** VARIABLES DE DEMANDA LABORAL
************************************************************************************************************

* P10B01 is the ocupa variable

* OCCUPACION
gen ocupa_ci=.
* preparado por MLO segun excel ${surveysFolder}\survey\GTM\ENCOVI\2000\m7_m11\docsocup_ci_clasificacion_propuesta.xlsx
replace ocupa_ci=1 if (P10B02B>=21 & P10B02B<=34) & emp_ci==1
replace ocupa_ci=2 if (P10B02B>=11 & P10B02B<=13) & emp_ci==1
replace ocupa_ci=3 if (P10B02B>=41 & P10B02B<=43) & emp_ci==1
replace ocupa_ci=4 if P10B02B==52 & emp_ci==1
replace ocupa_ci=5 if (P10B02B==51 | P10B02B==91)  & emp_ci==1
replace ocupa_ci=6 if ((P10B02B>=61 & P10B02B<=64) | P10B02B==92) & emp_ci==1
replace ocupa_ci=7 if ((P10B02B>=71 & P10B02B<=83) | P10B02B==93) & emp_ci==1
replace ocupa_ci=8 if P10B02B==1 & emp_ci==1
replace ocupa_ci=9 if P10B02B==94 & emp_ci==1
label var ocupa_ci "Ocupacion Laboral en la Actividad Principal"
label define ocupa_ci 1 "Profesionales y tecnicos" 2 "Directores y funcionarios superiores" 3 "Personal administrativo y nivel intermedio" 4 "Comerciantes y vendedores" 5 "Trabajadores en servicios" 6 "Trabajadores agricolas y afines" 7 "Obreros no agricolas, conductores de maquinas y vehiculos de transporte y similares" 8 "Fuerzas armadas" 9 "Otras ocupaciones no clasificadas en las anteriores"
label values ocupa_ci ocupa_ci



* RAMA
gen rama_ci=.
* MLO = supongo que se mantiene CIIU rev 3
replace rama_ci=1 if (P10B03B>=1 &  P10B03B<=5) & emp_ci==1
replace rama_ci=2 if (P10B03B>=10 & P10B03B<=14) & emp_ci==1
replace rama_ci=3 if (P10B03B>=15 & P10B03B<=37) & emp_ci==1
replace rama_ci=4 if (P10B03B>=40 & P10B03B<=41) & emp_ci==1
replace rama_ci=5 if P10B03B==45 & emp_ci==1
replace rama_ci=6 if (P10B03B>=50 & P10B03B<=55) & emp_ci==1
replace rama_ci=7 if (P10B03B>=60 & P10B03B<=64) & emp_ci==1
replace rama_ci=8 if (P10B03B>=65 & P10B03B<=74) & emp_ci==1
replace rama_ci=9 if (P10B03B>=75 & P10B03B<=99) & emp_ci==1
replace rama_ci=. if emp_ci!=1
tab P10B03B  rama_ci
label var rama_ci "Rama Laboral en la Ocupacion Principal"
label define rama_ci 1 "Agricultura, caza, sivicultura y pesca" 2 "Explotacion de minas y canteras" 3 "Industrias manufactureras" 4 "Electricidad, gas y agua" 5 "Construccion" 6 "Comercio al por mayor y menor, restaurantes, hoteles" 7 "Transporte y almacenamiento" 8 "Establecimientos financieros, seguros, bienes inmuebles" 9 "Servicios sociales, comunales y personales"
label values rama_ci rama_ci


*** ANTIGUPPA03 (AÑOS) (En total, cuantos años lleva trabajando (ocup principal)?) 

gen antiguedad_ci=.
replace antiguedad_ci=P10B07 if P10B07!=99 & emp_ci==1
label var antiguedad_ci "Antiguedad en la Ocupacion Actual (en años)"

*** DURACION DEL DESEMPLEO
gen durades_ci=P10E01/4.3 /* Solo a los DESOCUPADOS: Cuantas semanas hace que esta buscando trabajo? */
replace durades_ci= 0.23 if P10E01==0
replace durades_ci=. if P10E01==.
label var durades_ci "Duracion del Desempleo (en meses)"


******************************************************************************************************
* VARIABLES DE EDUCACION

* PARA PERSONAS DE 7 AÑOS O MAS DE PPA03 
******************************************************************************************************

* AÑOS DE EDUCACION

* There are two variables 'P06B25A' & 'P06B25B' that have nivel and grado 


/* P06B25A: 
	      Ninguno 
		  Preprimaria 
		  Prímaria  
	      Básicos   
		  Diversificado   
	      Superior  
	      Postgrado 

*/

gen byte aedu_ci=.

*** Ninguno 
replace aedu_ci=0 if P06B25A==1 

*** Preparatoria
replace aedu_ci=0 if P06B25A==2

*** Primaria
replace aedu_ci=P06B25B  if P06B25A==3 


*** BAsica
replace aedu_ci=6 + P06B25B if P06B25A==4

*** Diversificado
replace aedu_ci=6 + P06B25B if P06B25A==5


*** Educacion Superior
replace aedu_ci=12 + P06B25B if P06B25A==6 

*** Post-grado
replace aedu_ci=17 + P06B25B if P06B25A==7 

label variable aedu_ci "Años de Educacion"



** Categorias educativas excluyentes

gen eduno_ci=.
replace eduno_ci=1 if aedu_ci==0 
replace eduno_ci=0 if aedu_ci>0 & aedu_ci!=.
label var eduno_ci "1 = personas sin educacion (excluye preescolar)"

gen edupre_ci=.
replace edupre=1 if P06B25A==2
replace edupre=0 if P06B25A!=2 
label var edupre_ci "Educacion preescolar"

gen edupi_ci=.
replace edupi_ci=1 if aedu_ci>0 & aedu_ci<6
replace edupi_ci=0 if aedu_ci==0 | (aedu_ci>=6 & aedu_ci!=.)
label var edupi_ci "1 = personas que no han completado el nivel primario"

gen edupc_ci=.
replace edupc_ci=1 if aedu_ci==6
replace edupc_ci=0 if (aedu_ci>=0 & aedu_ci<6)  | (aedu_ci>6 & aedu_ci!=.) 
label var edupc_ci "1 = personas que han completado el nivel primario"

gen edusi_ci=.
replace edusi_ci=1 if aedu_ci>6 & aedu_ci<12
replace edusi_ci=0 if (aedu_ci>=0 & aedu_ci<=6) | (aedu_ci>=12 & aedu_ci!=.)
label var edusi_ci "1 = personas que no han completado el nivel secundario"

gen edusc_ci=.
replace edusc_ci=1 if aedu_ci==12
replace edusc_ci=0 if (aedu_ci>=0 & aedu_ci<12) | (aedu_ci>12 & aedu_ci!=.) 
label var edusc_ci "1 = personas que han completado el nivel secundario"

gen eduui_ci=.
replace eduui_ci=1 if aedu_ci>12 & aedu_ci<17
replace eduui_ci=0 if (aedu_ci>=0 & aedu_ci<=12) | (aedu_ci>=17 & aedu_ci!=.)
label var eduui_ci "1 = personas que no han completado el nivel universitario o superior"

gen eduuc_ci=.
replace eduuc_ci=1 if aedu_ci>=17 & aedu_ci!=.
replace eduuc_ci=0 if aedu_ci>=0 & aedu_ci<17
label var eduuc_ci "1 = personas que han completado el nivel universitario o superior"


gen edus1i_ci=.
replace edus1i=0 if edusi==1 | edusc==1 
replace edus1i=1 if edusi==1 & (P06B25A==4 & (P06B25B==1 | P06B25B==2))
label var edus1i_ci "1 = personas que no han completado el primer ciclo de la educacion secundaria"

gen edus1c_ci=.
replace edus1c=0 if edusi==1 | edusc==1 
replace edus1c=1 if edusi==1 & (P06B25A==4 & P06B25B==3)
label var edus1c_ci "1 = personas que han completado el primer ciclo de la educacion secundaria"

gen edus2i_ci=.
replace edus2i=0 if edusi==1 | edusc==1 
replace edus2i=1 if edusi==1 & (P06B25A==5 & (P06B25B==4 |P06B25B==5)) 
label var edus2i_ci "1 = personas que no han completado el segundo ciclo de la educacion secundaria"

gen edus2c_ci=.
replace edus2c=0 if edusi==1 
replace edus2c=1 if edusc==1
label var edus2c_ci "1 = personas que han completado el segundo ciclo de la educacion secundaria"

gen eduac_ci=.
label var eduac_ci "Educacion terciaria académica versus educación terciaria no-académica "

gen repite_ci=.
label var repite_ci "Personas que han repetido al menos un año o grado"

gen repiteult_ci=.


* ASISTENCIA ESCOLAR
gen asiste_ci=.
replace asiste_ci=1 if P06B22==1 & P06B05==1
replace asiste_ci=0 if P06B22==2 & P06B05==1
replace asiste_ci=0 if P06B05==2
*Modificación Mayra Sáenz 10/22/2015 : Se incluyen a los menores de 5 que están asistiendo a guarder'ia
replace asiste_ci=1 if P06A01==1
label var asiste_ci "Personas que actualmente asisten a centros de enseñanza"

* POR QUE NO ASISTE (En 1998 la pregunta sobre causa de inasistencia es solo para personas de 7 a 14 años de edad)

gen pqnoasis_ci=.
replace pqnoasis_ci=P06B23 if P06B23>0 & P06B23<99
label var pqnoasis_ci "Razon principal por la cual ha abandonado o ha dejado de asistir a clases este año"
label define pqnoasis_ci 1 "Enfermedad" 2 "Falta de maestro" 3 "La madre trabaja" 4 "Oficios de la casa" 5  "Falta de dinero" 6"Trabajo" 7"No le interesa" 8"Mal tiempo" 9"Embarazo" 10"Migracion temporal" 11"Acoso de maras" 12"Violencia" 98"Otra"
label value pqnoasis_ci pqnoasis_ci

**************
*pqnoasis1_ci*
**************
**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**

g       pqnoasis1_ci = 1 if pqnoasis_ci==5
replace pqnoasis1_ci= 2 if  pqnoasis_ci==6
replace pqnoasis1_ci= 3 if  pqnoasis_ci==1 | pqnoasis_ci==3
replace pqnoasis1_ci= 4 if  pqnoasis_ci==7
replace pqnoasis1_ci= 5 if  pqnoasis_ci==4 | pqnoasis_ci==9
replace pqnoasis1_ci= 9 if  pqnoasis_ci==98 | pqnoasis_ci==12 | pqnoasis_ci==11 | pqnoasis_ci==10 | pqnoasis_ci==8 | pqnoasis_ci==2

label define pqnoasis1_ci 1 "Problemas económicos" 2 "Por trabajo" 3 "Problemas familiares o de salud" 4 "Falta de interés" 5	"Quehaceres domésticos/embarazo/cuidado de niños/as" 6 "Terminó sus estudios" 7	"Edad" 8 "Problemas de acceso"  9 "Otros"
label value  pqnoasis1_ci pqnoasis1_ci
* EDUCACION PUBLICA OR PRIVADA
/* 
	 
       El plantel eductivo donde se |
                  inscribio (..) es: |      Freq.     Percent        Cum.
-------------------------------------+-----------------------------------
             Ministerio de Educación |     11,813       63.89       63.89
                             PRONADE |      1,273        6.88       70.77
    Nuevas escuelas unitarias -NEUS- |         14        0.08       70.85
Otra institución de Gobierno central |        533        2.88       73.73
                           Municipal |        187        1.01       74.74
                         Cooperativa |        860        4.65       79.39
                         Comunitario |        105        0.57       79.96
                             Privado |      3,465       18.74       98.70
                               ONG´s |        146        0.79       99.49
                                Otro |         94        0.51      100.00
-------------------------------------+-----------------------------------
                               Total |     18,490      100.00

	 
	 
	 

Se han considerado como PUBLICAS las opciones 1, 2, 3, 4 y 5 */


gen edupub_ci=0
replace edupub_ci=1 if ( P06B09==1 | P06B09==2 | P06B09==3 | P06B09==4 | P06B09==5 )
replace edupub_ci=. if ( P06B09==. | P06B09==99)
label var edupub_ci "1 = personas que asisten a centros de enseñanza publicos"

*************
**tecnica_ci*
*************

gen tecnica_ci=.
label var tecnica_ci "=1 formacion terciaria tecnica"




**********************************
*** INCOME VARIABLES (MONTHLY) ***
**********************************

* Create a dummy indicating this person's income should NOT be included in y*_ch
gen miembros_ci=1
replace miembros_ci=0 if  (relacion_ci==0 | relacion_ci==6 | relacion_ci==.)
replace miembros_ci=0 if factor_ci==.
label variable miembros_ci "Variable dummy que indica las personas que son miembros del Hogar"

sort idh_ch
			
*******************************************************
***           VARIABLES DE DIVERSIDAD               ***
*******************************************************				
* Maria Antonella Pereira & Nathalia Maya - Marzo 2021	

			
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


*****************************************************************
*** INGRESOS LABORALES (PARA PESONAS DE 5 AÑOS O MAS DE PPA03) ***
*****************************************************************

***************************
*** OCUPACION PRINCIPAL ***
***************************

/* P10B04 CATEGORIA OCUPACIONAL ACTIVIDAD PRINCIPAL
1 - 4 ASALARIADOS
5 - 6 INDEPENDIENTES
7 - 8 TRAB SIN PAGO
*/
gen categopri_ci=.
replace categopri_ci=1 if (P10B04==6 | P10B04==8) & emp_ci==1
replace categopri_ci=2 if (P10B04==5 | P10B04==7) & emp_ci==1
replace categopri_ci=3 if (P10B04>=1 & P10B04<=4) & emp_ci==1
replace categopri_ci=4 if (P10B04==9) & emp_ci==1
label var categopri_ci "CATEGORIA OCUPACIONAL ACTIVIDAD PRINCIPAL"
label define categopri_ci 1 "Patron" 2 "Cuenta propia" 3 "Empleado" 4 "Trabajador no remunerado"
label value categopri_ci categopri_ci


*****************
*tipocontrato_ci*
*****************
/*
gen tipocontrato_ci=.
replace tipocontrato_ci=1 if P10B06 ==1 
replace tipocontrato_ci=2 if P10B06 ==2 
replace tipocontrato_ci=3 if P10B05 ==2 
label var tipocontrato_ci "Tipo de contrato segun su duracion"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci
*/

gen tipocontrato_ci=.
replace tipocontrato_ci=1 if (P10B05 ==1 & P10B06 ==1) & categopri_ci==3
replace tipocontrato_ci=2 if (P10B05 ==1 & P10B06 ==2) & categopri_ci==3
replace tipocontrato_ci=3 if (P10B05 ==2 | tipocontrato_ci==.) & categopri_ci==3
label var tipocontrato_ci "Tipo de contrato segun su duracion"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci


****** INGRESOS MONETARIOS ******

* INGRESO INDEPENDIENTE (SE PREGUNTA DIRECTAMENTE POR MES)
gen iindep=.
replace iindep=P10B22 if (P10B04==5 | P10B04==6) 
replace iindep=P10B23 if (P10B04==7 | P10B04==8) 
label var iindep "Ingreso de los Independientes ocupacion principal "


* SALARIO O SUELDO BRUTO MENSUAL (Sin incluir horas extra, comisiones, dietas y otas prestaciones de ley)
gen salario=.
replace salario=P10B08 if ( P10B04>=1 & P10B04<=4 ) 
label var salario "Salario o Sueldo Mensual Bruto ocupacion principal"

* BONO 14
gen bono14=.
replace bono14=P10B11B/12 if ( P10B04>=1 & P10B04<=4 ) & P10B11A==1

* TIPS
gen tips=.
replace tips=P10B10B if ( P10B04>=1 & P10B04<=4 ) & P10B10A==1

* AGUINALDO
gen aguin=.
replace aguin=P10B12B/12 if ( P10B04>=1 & P10B04<=4 ) & P10B12A==1


* DIFERIDO
gen diferido=.
replace diferido=P10B13B/12 if ( P10B04>=1 & P10B04<=4 ) & P10B13A==1

* BONO VACACIONAL
gen bonovac=.
replace bonovac=P10B14B/12 if ( P10B04>=1 & P10B04<=4 ) & P10B14A==1

* OTROS INGRESOS
gen otros=.
replace otros=P10B15B/12 if ( P10B04>=1 & P10B04<=4 ) & P10B15A==1



****** INGRESO MONETARIO LABORAL ACTIVIDAD PRINCIPAL ******
egen ylmpri_ci=rsum(salario bono14 tips aguin diferido bonovac otros) if ( P10B04>=1 & P10B04<=4 ), missing
replace ylmpri_ci=. if salario==. & bono14==. & tips==. & aguin==. & diferido==. & bonovac==. & otros==.
replace ylmpri_ci=iindep if ( P10B04==5 | P10B04==6 | P10B04==7 | P10B04==8 ) 
label var ylmpri_ci "Ingreso Laboral Monetario de la Actividad Principal"

****** INGRESOS NO MONETARIOS ******
gen alim=0
replace alim=P10B19B if ( P10B04>=1 & P10B04<=4 )  & P10B19A==1
gen vivi=0
replace vivi=P10B20B if ( P10B04>=1 & P10B04<=4 ) & P10B20A==1
gen ropa=0
replace ropa=P10B16B/12 if ( P10B04>=1 & P10B04<=4 )  & P10B16A==1
gen transp=0
replace transp=P10B21B if ( P10B04>=1 & P10B04<=4 ) & P10B21A==1


****** INGRESO LABORAL NO MONETARIO ACTIVIDAD PRINCIPAL ******
egen ylnmpri_ci=rsum(alim vivi ropa transp), missing
replace ylnmpri_ci=. if (alim==. & vivi==. & ropa==. & transp==.) | P10B04>4   
label var ylnmpri_ci " Ingreso Laboral NO Monetario ocupacion principal"

****************************
*** OCUPACION SECUNDARIA ***
****************************

gen categosec_ci=.
replace categosec_ci=1 if (P10C04==6 | P10C04==8) & emp_ci==1
replace categosec_ci=2 if (P10C04==5 | P10C04==7) & emp_ci==1
replace categosec_ci=3 if (P10C04>=1 & P10C04<=4) & emp_ci==1
replace categosec_ci=4 if (P10C04==9) & emp_ci==1
label var categosec_ci "CATEGORIA OCUPACIONAL ACTIVIDAD SECUNDARIA"
label define categosec_ci 1 "Patron" 2 "Cuenta propia" 3 "Empleado" 4 "Trabajador no remunerado"
label value categosec_ci categosec_ci





****** INGRESOS MONETARIOS ******

* INGRESO INDEPENDIENTE (SE DEBE MENSUALIZAR)

gen iindep2=.
replace iindep2=P10C11 if (P10C04==5 | P10C04==6) 
replace iindep2=P10C12 if (P10C04==7 | P10C04==8) 
label var iindep "Ingreso de los Independientes ocupacion SECUNDARIA "



* SALARIO O SUELDO BRUTO MENSUAL (NO Incluye horas extra, comisiones, dietas y otas prestaciones de ley)
gen salario2=.
replace salario2=P10C05 if ( P10C04>=1 & P10C04<=4 ) 
label var salario2 "Salario o Sueldo Mensual Bruto ocupacion secundaria"

* BONO 14
gen bono142=.
replace bono142=P10C09B/12 if (  P10C04>=1 & P10C04<=4 ) & P10C09A==1

* TIPS
gen tips2=.
replace tips2=P10C06B if (  P10C04>=1 & P10C04<=4 ) & P10C06A==1

* OTROS INGRESOS
gen otros2=.
replace otros2 =P10C10B/12 if ( P10B04>=1 & P10B04<=4 ) & P10C10A==1




****** INGRESO LABORAL MONETARIO ACTIVIDAD SECUNDARIA ******
egen ylmsec_ci=rsum(salario2 bono142 tips2 otros2) if ( P10C04>=1 & P10C04<=4 ), missing
replace ylmsec_ci=. if salario2==. & bono142==. & tips2==. & otros2==.
replace ylmsec_ci=iindep2 if ( P10C04==5 | P10C04==6 | P10C04==7 | P10C04==8  ) 


****** INGRESO LABORAL NO MONETARIO ACTIVIDAD SECUNDARIA ******

****** INGRESOS NO MONETARIOS ******
gen alim2=0
replace alim2=P10C07B if ( P10B04>=1 & P10B04<=4 )  & P10C07A==1
gen vivi2=0
replace vivi2=P10C08B if ( P10B04>=1 & P10B04<=4 ) & P10C08A==1


*****************
***ylmotros_ci***
*****************
gen ylmotros_ci=.
label var ylmotros_ci "Ingreso laboral monetario de otros trabajos" 
*****************
***ylnmotros_ci***
******************

gen ylnmotros_ci=.
label var ylnmotros_ci "Ingreso laboral NO monetario de otros trabajos" 


****** INGRESO LABORAL NO MONETARIO ACTIVIDAD SECUNDARIA ******
egen ylnmsec_ci=rsum(alim2 vivi2 ), missing
replace ylnmsec_ci=. if (alim2==. & vivi2==.) | P10C04>4   
label var ylnmsec_ci " Ingreso Laboral NO Monetario ocupacion secundaria"




****************************************************************************
*** INGRESO LABORAL MONETARIO TOTAL ( OCUP PRINCIPAL + OCUP SECUNDARIA ) ***
****************************************************************************
egen ylm_ci=rsum(ylmpri_ci ylmsec_ci), missing
replace ylm_ci=. if ylmpri_ci==. & ylmsec_ci==.
replace ylmpri_ci=. if iindep==. & salario==. & bono14==. & aguin==. & tips==.
replace ylm_ci=. if ylmpri_ci==. & iindep2==. & salario2==. & bono142==. & tips2==.
label var ylm_ci "Ingreso Laboral Monetario Total"


*******************************************************************************
*** INGRESO LABORAL NO MONETARIO TOTAL ( OCUP PRINCIPAL + OCUP SECUNDARIA ) ***
*******************************************************************************
egen ylnm_ci=rsum(ylnmpri_ci ylnmsec_ci), missing
replace ylnm_ci=. if ylnmpri_ci==. & ylnmsec_ci==.
replace ylnm_ci=. if P10B04>4 & P10C04>4
label var ylnm_ci "Ingreso Laboral NO Monetario Total"


****************************************************************************************************
*** OTRAS FUENTES DE INGRESOS RELACIONADAS CON EL TRABAJO(PARA PERSONAS DE 7 AÑOS O MAS DE PPA03) ***
****************************************************************************************************

*** PENSIONES ***
gen pensa=. /* dinero por pension alimentos */
replace pensa=P11A08B/3 if P11A08A==1

gen pensj=. /* dinero por pension jubilacion */
replace pensj=P11A04B/3 if P11A04A==1


*** AYUDAS EN DINERO ***
gen ayins=.
replace ayins=P11A05B/3 if P11A05A==1

gen remesas_ci=. 
replace remesas_ci=P11A06B/3 if P11A06A==1
label var remesas_ci "Remesas Individuales"

egen ynlm_ci=rsum(pensa pensj ayins remesas_ci), missing
replace ynlm_ci=. if pensa==.  & pensj==. & ayins==. & remesas_ci==.
replace ynlm_ci=0 if P11A08A==2 &  P11A04A==2 &  P11A05A==2 &  P11A06A==2
label var ynlm_ci "Ingreso NO Laboral Monetario"

gen autocons_ci=.
label var autocons_ci "Autoconsumo Individual"

gen ynlnm_ci=.
label var ynlnm_ci "Ingreso NO Laboral NO Monetario"

************************************************************************************
*** INGRESOS DISTINTOS DEL TRABAJO EN DINERO O BIENES (MONTOS TOTALES DEL HOGAR) ***
************************************************************************************
* Quienes tienen 0 en estos ingresos es porque reportan que no los han recibido en el hogar * 
gen halquiler=0 /* Alquileres de edificios, casas, etc */
replace halquiler=P11A01B/3 if P11A01A==1
gen hdividendo=0 /* Dividendos de acciones.... */
replace hdividendo=P11A03B/3 if P11A03A==1.
gen hintereses=0 /* Dividendos de acciones.... */
replace hintereses=P11A02B/3 if P11A02A==1

gen hindemn=0 /* Indemnizaciones  */
replace hindemn=P11A09B/3 if P11A09A==1


gen hbeca=0  /* Becas y prestamos para estudios recibidos en efectivo */
replace hbeca=P11A07B/3 if P11A07A==1

gen hazar=0 /* Loteria y juegos de azar */
replace hazar=P11A10B/3 if P11A10A==1


egen ynl_ch=rsum(halquiler hdividendo hintereses hindemn hbeca hazar) if miembros_ci==1, missing
label var ynl_ch "Ingresos distintos del trabajo en dinero o bienes recibidos en el hogar" 

/*** FLAGS
gen byte nrylmpri_ci=0
replace nrylmpri_ci=1 if
label var nrylmpri_ci "Identificador de No Respuesta del Ingreso Monetario de la Actividad Principal"

*** Dummy para el Hogar
capture drop nrylmpri_ch
sort idh_ch
egen nrylmpri_ch=sum(nrylmpri_ci) if miembro==1, by(idh) 
replace nrylmpri_ch=1 if nrylmpri_ch>1 & nrylmpri_ch~=. & miembro==1 
label var nrylmpri_ch "Identificador de Hogares en donde alguno de los miembros No Responde el Ingreso Monetario de la Actividad Principal"

*/


egen ylm_ch=sum(ylm_ci) if miembros_ci==1, by(idh_ch) missing
label var ylm_ch "Ingreso Laboral Monetario del Hogar"

*egen ylmnr_ch=sum(ylm_ci) if miembros_ci==1 & nrylmpri_ch==0, by(idh_ch)
*label var ylmnr_ch "Ingreso Laboral Monetario del Hogar, considera 'missing' la No Respuesta"

egen ylnm_ch=sum(ylnm_ci) if miembros_ci==1, by(idh_ch) missing
label var ylnm_ch "Ingreso Laboral No Monetario del Hogar"

egen ynlm_ch=sum(ynlm_ci) if miembros_ci==1, by(idh_ch) missing
label var ynlm_ch "Ingreso No Laboral Monetario del Hogar"

egen ynlnm_ch=sum(ynlnm_ci) if miembros_ci==1, by(idh_ch) missing
label var ynlnm_ch "Ingreso No Laboral No Monetario del Hogar"

egen autocons_ch=sum(autocons_ci) if miembros_ci==1, by(idh_ch) missing
label var autocons_ch "Autoconsumo del Hogar"

egen remesas_ch=sum(remesas_ci) if miembros_ci==1, by(idh_ch) missing
label var remesas_ch "Remesas del Hogar (monetario + especies)"

replace ynlnm_ch=. if ynlnm_ci==.
replace autocons_ch=. if autocons_ci==.
replace ylm_ch =. if miembros_ci==0
*replace ylmnr_ch =. if miembros_ci==0
replace ylnm_ch =. if miembros_ci==0
replace ynlnm_ch =. if miembros_ci==0
replace autocons_ch =. if miembros_ci==0
replace remesas_ch =. if miembros_ci==0
replace ynlm_ch =. if miembros_ci==0

*** INGRESO HORARIO DE TODOS LOS TRABAJOS ***
/* this is not accurate in the sense that if you have more than one job
you will have wage averaged over several jobs */
gen ylmho_ci=ylm_ci/(horastot_ci*4.3) 
label var ylmho_ci "Salario Horario Monetario de todas las Actividades"

*** INGRESO HORARIO DE LA OCUPACION PRINCIPAL ***
gen ylmhopri_ci=ylmpri_ci/(horaspri_ci*4.3) 
label var ylmhopri_ci "Salario Horario Monetario de la Actividad Principal"


drop iindep* salario* bono14* tips* aguin* alim vivi ropa transp hh pensa pensj ayins


******************
***categoinac_ci**
******************
gen categoinac_ci=.
replace categoinac_ci=1 if P10F01 ==4 & condocup_ci==3
replace categoinac_ci=2 if P10F01 ==1 & condocup_ci==3
replace categoinac_ci=3 if P10F01 ==2 & condocup_ci==3
recode categoinac_ci .= 0 if condocup_ci==3

label var categoinac_ci "Condición de inactividad"
	label define categoinac_ci 1 "jubilado/pensionado" 2 "estudiante" 3 "quehaceres_domesticos" 4 "otros_inactivos" 
	label value categoinac_ci categoinac_ci
*******************
***formal_ci*******
*******************
	
gen formal_aux=1 if cotizando_ci==1
replace formal_aux=1 if afiliado_ci==1 & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1 & pais_c=="GTM" & anio_c>1998
gen byte formal_ci=1 if formal_aux==1 & condocup_ci==1
recode formal_ci .=0 if condocup_ci==1
label var formal_ci "1=afiliado o cotizante / ocupados"

drop formal_aux

gen nrylmpri_ci=.
gen tcylmpri_ci=.
gen nrylmpri_ch =.
gen tcylmpri_ch=.
gen ylmnr_ch=.
gen rentaimp_ch=.
gen combust_ch=.

/*_____________________________________________________________________________________________________*/
* Asignación de etiquetas e inserción de variables externas: tipo de cambio, Indice de Precios al 
* Consumidor (2011=100), Paridad de Poder Adquisitivo (PPA 2011),  líneas de pobreza
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
edus1c_ci edus2i_ci edus2c_ci edupre_ci eduac_ci asiste_ci pqnoasis_ci pqnoasis1_ci	repite_ci repiteult_ci edupub_ci tecnica_ci ///
aguared_ch aguadist_ch aguamala_ch aguamide_ch luz_ch luzmide_ch combust_ch	bano_ch banoex_ch des1_ch des2_ch piso_ch aguamejorada_ch banomejorado_ch  ///
pared_ch techo_ch resid_ch dorm_ch cuartos_ch cocina_ch telef_ch refrig_ch freez_ch auto_ch compu_ch internet_ch cel_ch ///
vivi1_ch vivi2_ch viviprop_ch vivitit_ch vivialq_ch	vivialqimp_ch , first



compress


saveold "`base_out'", replace


log close




