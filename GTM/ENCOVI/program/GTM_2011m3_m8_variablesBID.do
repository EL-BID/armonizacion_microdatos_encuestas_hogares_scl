

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
local ANO "2011"
local ronda m3_m8


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
Última modificación: Daniela Zuluaga E-mail: danielazu@iadb.org - da.zuluaga@hotmail.com
Fecha última modificación: Octubre de 2017

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




gen factor_ci=factor
label var factor_ci "Factor de Expansion del Individuo"
**************************************************************************************************************
* HOUSEHOLD VARIABLES
**************************************************************************************************************

	****************
	* region_BID_c *
	****************
	
* REGION

gen region_c=depto
	
gen region_BID_c=1
label var region_BID_c "Regiones BID"
label define region_BID_c 1 "Centroamérica_(CID)" 2 "Caribe_(CCB)" 3 "Andinos_(CAN)" 4 "Cono_Sur_(CSC)"
label value region_BID_c region_BID_c


gen factor_ch=factor 
label var factor_ch "Factor de expansion del Hogar"

* ZONA
gen byte zona_c=1 if area==1 /* Urbana */
replace zona_c=0 if area==2 /* Rural */
label variable zona_c "ZONA GEOGRAFICA"
label define zona_c 0 "Rural" 1 "Urbana"
label value zona_c zona_c

* COUNTRY - YEAR
gen str3 pais_c="GTM"
label variable pais_c "Nonmbre del Pais"

gen anio_c=2011
label variable anio_c "Año de la Encuesta"

* Periodo de Referencia: del 07/00 al 11/00.
* This is the middle of the reference period
gen byte mes_c=9
label variable mes_c "Mes de la Encuesta"

*sexo
gen sexo_ci=ppa02
label variable sexo "sex of the individual"
label var sexo_ci "sexo del individuo"
label define sexo_ci 1 "hombre" 2 "mujer"  
label value sexo_ci sexo_ci

* parentesco
gen relacion_ci=1 if ppa05==1
replace relacion_ci=2 if ppa05==2
replace relacion_ci=3 if ppa05==3
replace relacion_ci=4 if ( ppa05==4 | ppa05==5 | ppa05==6 | ppa05==7 | ppa05==8 | ppa05==9 | ppa05==10 )
replace relacion_ci=5 if ( ppa05==12 | ppa05==13 )
replace relacion_ci=6 if ppa05==11
label var relacion_ci "parentesco o relacion con el jefe del hogar"
label define relacion_ci 1 "jefe(a)" 2 "esposo(a) o compañero(a)" 3 "hijo(a)" 4 "otro pariente" 5 "otro no pariente" 6 "empleada domestica" 
label value relacion_ci relacion_ci


* ppa03
* 99 is the top-code, not that age is missing 
* meses is also available
gen edad_ci=ppa03
label var edad_ci "edad del individuo"

* identificador del hogar
gen idh_ch=formulario 
label var idh_ch "identificador unico del hogar"

* identificador de la persona
egen idp_ci=group(formulario) 
label var idp_ci "identificador individual dentro del hogar"

sort idh_ch idp_ci

egen nconyuges_ch=sum(relacion_ci==2), by (idh_ch)
label variable nconyuges_ch "numero de conyuges"

egen nhijos_ch=sum(relacion_ci==3), by (idh_ch)
label variable nhijos_ch "numero de hijos"
egen notropari_ch=sum(relacion_ci>3 & relacion_ci<5), by (idh_ch)
label variable notropari_ch "numero de otros parientes "
egen notronopari_ch=sum(relacion_ci==5), by (idh_ch)
label variable notronopari_ch "numero de otros no parientes "
egen nempdom_ch=sum(relacion_ci==6), by (idh_ch)
label variable nempdom_ch "numero de empleados domesticos"

* household type (unipersonal, nuclear, ampliado, compuesto, corresidentes)    
* note: these are all defined in terms of relationship to household head

gen clasehog_ch=.
replace clasehog_ch=1 if nhijos_ch==0 & nconyuges_ch==0 & notropari_ch==0 & notronopari_ch==0 /* unipersonal*/
replace clasehog_ch=2 if nhijos_ch>0 & notropari_ch==0 & notronopari_ch==0 /* nuclear (child with or without spouse but without other relatives)*/
replace clasehog_ch=2 if nconyuges_ch>0 & notropari_ch==0 & notronopari_ch==0 /* nuclear (spouse with or without children but without other relatives)*/
replace clasehog_ch=3 if notropari_ch>0 & notronopari_ch==0 /* ampliado*/
replace clasehog_ch=4 if (nconyuges_ch>0 | nhijos_ch>0 | (notropari_ch>0 & notropari_ch<.)) & (notronopari_ch>0 & notronopari_ch<.) /* compuesto  (some relatives plus non relative)*/
replace clasehog_ch=5 if nhijos_ch==0 & nconyuges_ch==0 & notropari_ch==0 & notronopari_ch>0 & notronopari_ch<./** corresidente*/

label variable clasehog_ch "clase hogar"
label define clasehog_ch 1 "unipersonal" 2 "nuclear" 3 "ampliado" 4 "compuesto" 5 "corresidente"
label value clasehog_ch clasehog_ch



* household composition variables 
/* note: these are unrelated to who is the head
   note: that childh denotes the number of children of the head, while numkids counts the number of all kids in the household */

sort idh_ch

* number of persons in the household (not including domestic employees or other relatives)
egen nmiembros_ch=sum(relacion_ci>0 & relacion_ci<5 ), by (idh_ch)
label variable nmiembros_ch "numero de miembros en el hogar"

egen nmayor21_ch=sum((relacion_ci>0 & relacion_ci<5 ) & (edad>=21)), by (idh_ch)
label variable nmayor21_ch "numero de personas de 21 años o mas dentro del hogar"

egen nmenor21_ch=sum((relacion_ci>0 & relacion_ci<5 ) & (edad<21)), by (idh_ch)
label variable nmenor21_ch "numero de personas menores a 21 años dentro del hogar"

egen nmayor65_ch=sum((relacion_ci>0 & relacion_ci<5 ) & (edad>=65)), by (idh_ch)
label variable nmayor65_ch "numero de personas de 65 años o mas dentro del hogar"

egen nmenor6_ch=sum((relacion_ci>0 & relacion_ci<5 ) & (edad<6)), by (idh_ch)
label variable nmenor6_ch "numero de niños menores a 6 años dentro del hogar"

egen nmenor1_ch=sum((relacion_ci>0 & relacion_ci<5 ) & (edad<1)),  by (idh_ch)
label variable nmenor1_ch "numero de niños menores a 1 año dentro del hogar"



*** estado civil para personas de 10 años o mas de ppa03
gen civil_ci=.  
replace civil_ci=1 if ppa06==7 /* soltero */
replace civil_ci=2 if ppa06==1 | ppa06==2 /* union formal o informal */
replace civil_ci=3 if ppa06==3 | ppa06==4 | ppa06==5 /* separado o divorciado */
replace civil_ci=4 if ppa06==6 /* viudo */
label var civil_ci "estado civil"
label define civil_ci 1 "soltero" 2 "union formal o informal" 3 "divorciado o separado" 4 "viudo"
label value civil_ci civil_ci

*** reported head of household
gen jefe_ci=0
replace jefe_ci=1 if relacion_ci==1
label var jefe_ci "jefe de hogar declarado"

*** we want to know if there is only one head in each hh and if there is a hh with no head:
egen hh=sum(jefe_ci), by (idh_ch)
capture assert hh==1

*** housing ***

gen aguared_ch=.
replace aguared_ch=1 if p01a05a==1
replace aguared_ch=0 if p01a05a==2

gen aguadist_ch=.


gen aguamala_ch=.
replace aguamala_ch=1 if p01d06==5 | p01d06==6 | p01d06==7 | p01d06==8
replace aguamala_ch=0 if p01d06==1 | p01d06==2 | p01d06==3 | p01d06==4

gen aguamide_ch=.
replace aguamide_ch=1 if p01a05e==1
replace aguamide_ch=0 if p01a05e==2

gen luz_ch=.
replace luz_ch=1 if p01a05c==1
replace luz_ch=0 if p01a05c==2

gen luzmide_ch=.
replace luzmide_ch=1 if p01a05f==1
replace luzmide_ch=0 if p01a05f==2

/*gen combust_ch=.
replace combust_ch=1 if 
replace combust_ch=0 if 
*/

gen bano_ch=.
replace bano_ch=1 if p01d16==1 | p01d16==2 | p01d16==3 | p01d16==4
replace bano_ch=0 if p01d16==5

gen banoex_ch=.
replace banoex=1 if p01d17==1
replace banoex=0 if p01d17==2


* modificaciones marcela rubio septiembre 2014: variable habia sido generada como missing

gen des1_ch=.
replace des1_ch=0 if p01d16==5
replace des1_ch=1 if p01d16==1 | p01d16==2 | p01d16==3
replace des1_ch=2 if p01d16==4

* modificaciones marcela rubio septiembre 2014: variable habia sido generada como missing
 
gen des2_ch=.
replace des2_ch=0 if p01d16==5
replace des2_ch=1 if p01d16==1 | p01d16==2 | p01d16==3 | p01d16==4


gen piso_ch=.
replace piso_ch=0 if p01a04==7
replace piso_ch=1 if p01a04>=1 & p01a04<=6
replace piso_ch=2 if p01a04==98

gen pared_ch=.
replace pared_ch=0 if p01a02==7 | p01a02==8
replace pared_ch=1 if p01a02>=1 & p01a02<=6
replace pared_ch=2 if p01a02==98

gen techo_ch=.
replace techo_ch=0 if p01a03==5 
replace techo_ch=1 if p01a03>=1 & p01a03<=4
replace techo_ch=2 if p01a03==98

gen resid_ch=.
replace resid_ch=0 if p01d21==1 | p01d21==2
replace resid_ch=1 if p01d21==3 | p01d21==4
replace resid_ch=2 if p01d21==5
replace resid_ch=3 if p01d21==6 | p01d21==98

**Daniela Zuluaga- Enero 2018: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**
	
*********************
***aguamejorada_ch***
*********************
g       aguamejorada_ch = 1 if (p01d06 >=1 & p01d06  <=4) | p01d06  ==7
replace aguamejorada_ch = 0 if (p01d06  >=5 & p01d06  <=6) | p01d06  ==98
		
*********************
***banomejorado_ch***
*********************
g       banomejorado_ch = 1 if (p01d17 >=1 & p01d17 <=4) & p01d18 == 1
replace banomejorado_ch = 0 if ((p01d17 >=1 & p01d17 <=4) & p01d18 == 2) & p01d17 ==5 


gen dorm_ch=.
replace dorm_ch=p01d02 if p01d02>=0

gen cuartos_ch=.
replace cuartos_ch= p01d01 if  p01d01>=0

gen cocina_ch=.
replace cocina_ch=1 if p01d04==1
replace cocina_ch=0 if p01d04>=2 & p01d04<=7

gen telef_ch=.
replace telef_ch=1 if p01d19a==1
replace telef_ch=0 if p01d19a==2

gen refrig_ch=.


gen freez_ch=.

gen auto_ch=.


gen compu_ch=.

gen internet_ch=.
replace internet_ch=1 if  p01d19c ==1 
replace internet_ch=0 if  p01d19c ==2

gen cel_ch=.
replace cel_ch=1 if p01d19b==1
replace cel_ch=0 if p01d19b==2

gen vivi1_ch=.
replace vivi1_ch=1 if p01a01==1
replace vivi1_ch=2 if p01a01==2
replace vivi1_ch=3 if p01a01>=3 & p01a01<=98

gen vivi2_ch=.
replace vivi2_ch=1 if vivi1_ch==1 | vivi1_ch==2
replace vivi2_ch=0 if vivi1_ch==3

gen viviprop_ch=.
replace viviprop_ch=0 if p01b01==3
replace viviprop_ch=1 if p01b01==1
replace viviprop_ch=2 if p01b01==2
replace viviprop_ch=3 if p01b01==4 | p01b01==98 

gen vivitit_ch=.


gen vivialq_ch=.
replace vivialq_ch=p01b03 if p01b03<99999

gen vivialqimp_ch=.
replace vivialqimp_ch=p01b02 if p01b02<99999



*******************************************************************************************
* variables del mercado laboral

* personas de 5 años y mas de ppa03 *
* en 1998 este bloque de preguntas estaba dirigido a las personas de 7 años y mas de ppa03 *
*******************************************************************************************
/************************************************************************************************************
* 3. creación de nuevas variables de ss and lmk a incorporar en armonizadas
************************************************************************************************************/

*************
**salmm_ci***
*************

*1 = GUA 2011
gen salmm_ci= 	1911
label var salmm_ci "Salario minimo legal"

*********
*lp_ci***
*********

gen lp_ci =9030.93
label var lp_ci "Linea de pobreza oficial del pais"

*********
*lpe_ci**
*********

gen lpe_ci =4380
label var lpe_ci "Linea de indigencia oficial del pais"

/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/



****************
*lp25_2005_ci***
****************

*Mayra Sáenz, Octubre 2015: se modifican líneas de pobreza internacionales con año base PPP2011. Se renombran líneas con año base PPP2005. 
gen lp25_2005_ci = 491.7475
label var lp25_2005_ci  "Línea de pobreza USD2.5 por día en moneda local a precios corrientes a PPP 2005"

***************
*lp4_2005_ci***
***************

*Mayra Sáenz, Octubre 2015: se modifican líneas de pobreza internacionales con año base PPP2011. Se renombran líneas con año base PPP2005. 
gen lp4_2005_ci = 786.796
label var lp4_2005_ci "Línea de pobreza USD4 por día en moneda local a precios corrientes a PPP 2005"

********* 
*lp25_ci
*********

gen lp25_ci =   294.5276 
capture label var lp25_ci  "Línea de pobreza USD2.5 por día en moneda local a precios corrientes a PPP 2011"

*********
*lp4_ci*
*********
gen lp4_ci =471.2441 
capture label var lp4_ci "Línea de pobreza USD4 por día en moneda local a precios corrientes a PPP 2011"


****************
*cotizando_ci***
****************
gen cotizando_ci=.
label var cotizando_ci "cotizante a la seguridad social"

****************
*afiliado_ci****
****************
gen afiliado_ci=.	
replace afiliado_ci=1 if p10b26a==1
recode afiliado_ci .=0 if p10b26a!=1

label var afiliado_ci "afiliado a la seguridad social"

****************
*tipopen_ci*****
****************
gen tipopen_ci=.
label var tipopen_ci "tipo de pension - variable original de cada pais" 

****************
*instpen_ci*****
****************
gen instpen_ci=.
label var instpen_ci "institucion proveedora de la pension - variable original de cada pais" 

****************
*instcot_ci*****
****************
gen instcot_ci=.
label var instcot_ci "institucion proveedora de la pension - variable original de cada pais" 

****************
****condocup_ci*
****************

* se considero ta,bien a quienes buscan trabajo hasta cuatro semanas anteriores. 05/19/2014 mgd
* mgr: modifico serie en base a correcciones laura castrillo: delimitar la condición de edad para que no tome los missing en caso que existan
gen condocup_ci=.
replace condocup_ci=1 if (p10a02==1 | p10a04==1)
replace condocup_ci=2 if condocup_ci!=1 & (p10a02==2 & (p10a04==2 & (p10a06==1 | p10a07==1)))
recode condocup_ci .=3 if edad_ci>=7 & edad_ci!=.
replace condocup_ci=4 if edad<7
label var condocup_ci "condicion de ocupación de acuerdo a def de cada pais"
label define condocup_ci 1 "ocupado" 2 "desocupado" 3 "inactivo" 4 "menor de pet" 
label value condocup_ci condocup_ci

*************
*cesante_ci* 
*************
* correccion mgd 07/17/2014: mal generada la variable, se consideraba a inactivos p10f02 y no desempleados.
gen cesante_ci=1 if p10e05==2  
replace cesante_ci=0 if p10e05==1
label var cesante_ci "desocupado - definicion oficial del pais"	


*************
*tamemp_ci
*************

gen tamemp_ci=1 if p10b25>=1 & p10b25<=5
replace tamemp_ci=2 if p10b25>=6 & p10b25<=50
replace tamemp_ci=3 if p10b25>=50 & p10b25!=.

label var tamemp_ci "# empleados en la empresa segun rangos"
label define tamemp_ci 1 "pequena" 2 "mediana" 3 "grande"
label value tamemp_ci tamemp_ci

label var tamemp_ci "# empleados en la empresa"

*************
**pension_ci*
*************

gen pension_ci=1 if p11a04b>0 & p11a04b!=.
recode pension_ci .=0 
label var pension_ci "1=recibe pension contributiva"

*************
**ypen_ci*
*************

gen ypen_ci=p11a04b/3 if p11a04b>0 & p11a04b!=.

label var ypen_ci "valor de la pension contributiva"

***************
*pensionsub_ci*
***************
*DZ Octubre 2017- Se crea la variable pension subsidiada*
*La variable se encuentra por individuo beneficiario*

gen pensionsub_ci=(p03c02==13| p03c06==13 | p03c10==13)
label var pensionsub_ci "1=recibe pension subsidiada / no contributiva"

*****************
**ypensub_ci*
*****************
*DZ Octubre 2017- Se crea la variable valor de la pension subsidiada*

gen ypensub_ci=.
replace ypensub_ci=p03c04 if p03c02==13
replace ypensub_ci=p03c08 if p03c06==13
replace ypensub_ci=p03c12 if p03c10==13

label var ypensub_ci "valor de la pension subsidiada / no contributiva"


/*

* emp
gen byte emp_ci=1 if ( p10b01==1 | p10b01==2| p10b01==3 )
replace emp_ci=0 if ( p10b01==. )
label var emp_ci "empleado en la semana de referencia"

* desemp1
* isolating workers self declared searchers that did not work (at all) last week
gen byte desemp1_ci=1 if ( (p10a06==1 & emp~=1) | (p10a02==2 & emp~=1))
replace desemp1_ci=0 if emp_ci==1 | (p10a06==2 & emp~=1 )
label var desemp1_ci "personas que no tienen trabajo y han buscado trabajo la semana pasada"

/* desemp2 *** no esta la opcion de que la persona esta esperando una repsuesta ****
gen byte desemp2_ci=desemp1_ci
replace desemp2_ci=1 if (emp_ci~=1 & p10a04~=1 & p10a09==1)
replace desemp2_ci=0 if emp_ci==1 | (p10a04~=1 & p10a09>1 & p10a09<13)
label var desemp2_ci "desemp1 + personas que no tienen trabajo ni lo buscaron, pero esperan respuesta de una solicitud de empleo, entrevista" 


gen byte desemp20_ci=desemp1_ci
replace desemp20_ci=1 if (emp_ci~=1 & p10a04~=1 & (p10a09==1 | p10a09==2))
replace desemp20_ci=0 if emp_ci==1 | (p10a04~=1 & p10a09>2 & p10a09<13)
label var desemp20_ci "desemp1 + personas que no tienen trabajo ni lo buscaron, pero esperan respuesta de una solicitud de empleo, entrevista o temporada agricola" /* incluye temporada agricola */
*/


gen byte desemp3_ci=.
replace desemp3_ci=desemp1_ci
replace desemp3_ci=1 if (emp~=1 & p10a06~=1 &  p10a07==1)
label var desemp3_ci "desemp2 + personas que no tienen trabajo pero han buscado trabajo durante las 4 semanas anteriores a la semana pasada"

* pea: economically active population
gen byte pea1_ci=1 if ( emp==1 | desemp1_ci==1 )
label var pea1_ci "poblacion economicamente activa utilizando la definicion 'desemp1'"
*gen byte pea2_ci=1 if ( emp==1 | desemp2_ci==1 )
*label var pea2_ci "poblacion economicamente activa utilizando la definicion 'desemp2'"
gen byte pea3_ci=.
label var pea3_ci "poblacion economicamente activa utilizando la definicion 'desemp3'"

replace pea1_ci=0 if pea1_ci~=1
*replace pea2_ci=0 if pea2_ci~=1
*/

************
***emp_ci***
************
gen byte emp_ci=(condocup_ci==1)
label var emp_ci "ocupado (empleado)"

****************
***desemp_ci***
****************
gen desemp_ci=(condocup_ci==2)
label var desemp_ci "desempleado que buscó empleo en el periodo de referencia"
  
*************
***pea_ci***
*************
gen pea_ci=0
replace pea_ci=1 if emp_ci==1 |desemp_ci==1
label var pea_ci "población económicamente activa"

/* trabajadores desalentados
gen byte desalent_ci=1 if pea2_ci~=1  & (p10a09==9 | p10a09==11) 
replace desalent_ci=0 if pea2_ci==1 | (p10a09!=9 & p10a09!=11)
label var desalent_ci "trabajadores desalentados, personas que creen que por alguna razon no conseguiran trabajo" 
*/

gen desalent_ci=.


*** horas actividad principal
egen horaspri_ci=rsum(p10b27a p10b27b p10b27c p10b27d p10b27e p10b27f p10b27g), missing
replace horaspri_ci=. if p10b27a==. & p10b27b==. & p10b27c==. & p10b27d==. & p10b27e==. & p10b27f==. & p10b27g==. 
label var horaspri_ci "horas totales trabajadas la semana pasada en la actividad principal"

*** horas actividad secundaria
*** 98 is top code
gen horassec_ci=p10c15 if p10c15<99
label variable horassec_ci "horas totales trabajadas la semana pasada en la actividad secundaria"

gen horastot_ci=horaspri_ci+horassec_ci if horaspri_ci!=. & horassec_ci!=.
replace horastot_ci=horaspri_ci if horassec_ci==.
label var horastot_ci "horas totales trabajadas la semana pasada en todas las actividades"


* subempleado

* modificacion: subempleo visible (desea trabajar mas horas y esta disponible para hacerlo). mgd 06/19/2014
gen subemp_ci=0
replace subemp_ci=1 if horaspri_ci<=30 &  p10d01==1 & emp_ci==1 & p10d05==1
label var subemp_ci "trabajadores subempleados"

* trabajadores a medio tiempo
gen byte tiempoparc_ci=.
replace tiempoparc_ci=1 if horastot_ci<=30  & ( p10d01==2) & emp==1
replace tiempoparc_ci=0 if ((horastot_ci>30  &  p10d01==2) |  p10d01==1) & emp_ci==1
label var tiempoparc_ci "trabajadores a medio tiempo"

/** contratos
gen contrato_ci=.
replace contrato_ci=1 if p10b05==1
replace contrato_ci=1 if p10b05==2
label var contrato_ci "personas empleadas que han firmado un contrato de trabajo"
*/
* beneficios (seguridad social)
** la pregunta exacta, dirigida a todos los trabajadores, de la encuesta es: paga una cuota al seguro social (igss) por el trabajo que tuvo?
/*gen segsoc_ci=1 if p10b26a==1
replace segsoc_ci=0 if p10b26a==2 | p10b26a==3 |p10b26a==4
label variable segsoc_ci "personas que cuentan con seguro social"
*/
* numero de ocupaciones
gen nempleos_ci=1 if emp_ci==1 & p10b01==1 /* 1 empleo */
replace nempleos_ci=2 if ( (emp_ci==1 & p10b01==2) | (emp_ci==1 & p10b01==3)  ) /* 'p10c01' pregunta si trabajo en una ocupacion secundaria */ 
replace nempleos_ci=0 if emp_ci~=1
label var nempleos_ci "numero de empleos"
label define nempleos_ci 1 "un trabajo" 2 "dos o mas trabajos"
label values nempleos_ci nempleos_ci
/*
gen firmapeq_ci= .
replace firmapeq_ci= 1 if p10b25<=5
recode firmapeq_ci .=0 if p10b26a==1

* tamaño de la firma
gen byte tamfirma_ci=.
replace tamfirma_ci=0 if p10b25<=5
replace tamfirma_ci=1 if p10b25>5 
label var tamfirma_ci "trabajadores formales"
label define tamfirma_ci 1 "mas de 5 trabajadores" 0 "5 o menos trabajadores"
label values tamfirma_ci tamfirma_ci
*/
*** sector publico
gen spublico_ci=1 if p10b04==1
replace spublico_ci=0 if p10b04>=2 & p10b04<=9
label var spublico_ci "personas que trabajan en el sector publico"

****self-employment
* cuenta propia
gen byte selfemp=1 if p10b04==5 | p10b04==7
   
* independent workers
* patrono, cuenta propia 
gen byte indep=1 if ( p10b04>=5 & p10b04<=8 )

************************************************************************************************************
*** variables de demanda laboral
************************************************************************************************************

* p10b01 is the ocupa variable

* occupacion
gen ocupa_ci=.
* preparado por mlo segun excel ${surveysFolder}\survey\gtm\encovi\2000\m7_m11\docsocup_ci_clasificacion_propuesta.xlsx
replace ocupa_ci=1 if (p10b02b>=21 & p10b02b<=34) & emp_ci==1
replace ocupa_ci=2 if (p10b02b>=11 & p10b02b<=13) & emp_ci==1
replace ocupa_ci=3 if (p10b02b>=41 & p10b02b<=43) & emp_ci==1
replace ocupa_ci=4 if p10b02b==52 & emp_ci==1
replace ocupa_ci=5 if (p10b02b==51 | p10b02b==91)  & emp_ci==1
replace ocupa_ci=6 if ((p10b02b>=61 & p10b02b<=64) | p10b02b==92) & emp_ci==1
replace ocupa_ci=7 if ((p10b02b>=71 & p10b02b<=83) | p10b02b==93) & emp_ci==1
replace ocupa_ci=8 if p10b02b==1 & emp_ci==1
replace ocupa_ci=9 if p10b02b==94 & emp_ci==1
label var ocupa_ci "ocupacion laboral en la actividad principal"
label define ocupa_ci 1 "profesionales y tecnicos" 2 "directores y funcionarios superiores" 3 "personal administrativo y nivel intermedio" 4 "comerciantes y vendedores" 5 "trabajadores en servicios" 6 "trabajadores agricolas y afines" 7 "obreros no agricolas, conductores de maquinas y vehiculos de transporte y similares" 8 "fuerzas armadas" 9 "otras ocupaciones no clasificadas en las anteriores"
label values ocupa_ci ocupa_ci



* rama
gen rama_ci=.
* mlo = supongo que se mantiene ciiu rev 3
replace rama_ci=1 if (p10b03b>=1 &  p10b03b<=5) & emp_ci==1
replace rama_ci=2 if (p10b03b>=10 & p10b03b<=14) & emp_ci==1
replace rama_ci=3 if (p10b03b>=15 & p10b03b<=37) & emp_ci==1
replace rama_ci=4 if (p10b03b>=40 & p10b03b<=41) & emp_ci==1
replace rama_ci=5 if p10b03b==45 & emp_ci==1
replace rama_ci=6 if (p10b03b>=50 & p10b03b<=55) & emp_ci==1
replace rama_ci=7 if (p10b03b>=60 & p10b03b<=64) & emp_ci==1
replace rama_ci=8 if (p10b03b>=65 & p10b03b<=74) & emp_ci==1
replace rama_ci=9 if (p10b03b>=75 & p10b03b<=99) & emp_ci==1
replace rama_ci=. if emp_ci!=1
tab p10b03b  rama_ci
label var rama_ci "rama laboral en la ocupacion principal"
label define rama_ci 1 "agricultura, caza, sivicultura y pesca" 2 "explotacion de minas y canteras" 3 "industrias manufactureras" 4 "electricidad, gas y agua" 5 "construccion" 6 "comercio al por mayor y menor, restaurantes, hoteles" 7 "transporte y almacenamiento" 8 "establecimientos financieros, seguros, bienes inmuebles" 9 "servicios sociales, comunales y personales"
label values rama_ci rama_ci


*** antiguppa03 (años) (en total, cuantos años lleva trabajando (ocup principal)?) 

gen antiguedad_ci=.
replace antiguedad_ci=p10b07 if p10b07!=99 & emp_ci==1
label var antiguedad_ci "antiguedad en la ocupacion actual (en años)"

*** duracion del desempleo
gen durades_ci=p10e01/4.3 /* solo a los desocupados: cuantas semanas hace que esta buscando trabajo? */
replace durades_ci= 0.23 if p10e01==0
replace durades_ci=. if p10e01==.
label var durades_ci "duracion del desempleo (en meses)"


******************************************************************************************************
* variables de educacion

* para personas de 7 años o mas de ppa03 
******************************************************************************************************

* años de educacion

* there are two variables 'p06b25a' & 'p06b25b' that have nivel and grado 


/* p06b25a: 
	      ninguno 
		  preprimaria 
		  prímaria  
	      básicos   
		  diversificado   
	      superior  
	      postgrado 

*/

gen byte aedu_ci = .

*Modificación Mayra Sáenz - se utiliza la p06b06a p06b06b en lugar de la p06b25a p06b25b

*** preprimaria
replace aedu_ci=0 if p06b06a==1 


*** primaria
replace aedu_ci=p06b06b  if p06b06a==2


*** basica
replace aedu_ci=6 + p06b06b if p06b06a==3

*** diversificado
replace aedu_ci=9 + p06b06b if p06b06a==4


*** educacion superior
replace aedu_ci=12 + p06b06b if p06b06a==5 

*** post-grado
replace aedu_ci=17 + p06b06b if p06b06a==6


*Para los que no están asistiendo actualmente 
replace aedu_ci=aÑosedu if  aedu_ci ==.



 

label variable aedu_ci "años de educacion"



** categorias educativas excluyentes

gen eduno_ci=.
replace eduno_ci=1 if aedu_ci==0 
replace eduno_ci=0 if aedu_ci>0 & aedu_ci!=.
label var eduno_ci "1 = personas sin educacion (excluye preescolar)"

gen edupre_ci=.
replace edupre=1 if p06b25a==2
replace edupre=0 if p06b25a!=2 
label var edupre_ci "educacion preescolar"

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
replace edus1i=1 if edusi==1 & (p06b25a==4 & (p06b25b==1 | p06b25b==2))
label var edus1i_ci "1 = personas que no han completado el primer ciclo de la educacion secundaria"

gen edus1c_ci=.
replace edus1c=0 if edusi==1 | edusc==1 
replace edus1c=1 if edusi==1 & (p06b25a==4 & p06b25b==3)
label var edus1c_ci "1 = personas que han completado el primer ciclo de la educacion secundaria"

gen edus2i_ci=.
replace edus2i=0 if edusi==1 | edusc==1 
replace edus2i=1 if edusi==1 & (p06b25a==5 & (p06b25b==4 |p06b25b==5)) 
label var edus2i_ci "1 = personas que no han completado el segundo ciclo de la educacion secundaria"

gen edus2c_ci=.
replace edus2c=0 if edusi==1 
replace edus2c=1 if edusc==1
label var edus2c_ci "1 = personas que han completado el segundo ciclo de la educacion secundaria"

gen eduac_ci=.
label var eduac_ci "educacion terciaria académica versus educación terciaria no-académica "

gen repite_ci=.
label var repite_ci "personas que han repetido al menos un año o grado"

gen repiteult_ci=.


* asistencia escolar
gen asiste_ci=.
replace asiste_ci=1 if p06b22==1 & p06b05==1
replace asiste_ci=0 if p06b22==2 & p06b05==1
replace asiste_ci=0 if p06b05==2
*modificación mayra sáenz 10/22/2015 : se incluyen a los menores de 5 que están asistiendo a guarder'ia
replace asiste_ci=1 if p06a01==1
label var asiste_ci "personas que actualmente asisten a centros de enseñanza"

* por que no asiste (en 1998 la pregunta sobre causa de inasistencia es solo para personas de 7 a 14 años de edad)

gen pqnoasis_ci=.
replace pqnoasis_ci=p06b23 if p06b23>0 & p06b23<99
label var pqnoasis_ci "razon principal por la cual ha abandonado o ha dejado de asistir a clases este año"
label define pqnoasis_ci 1 "enfermedad" 2 "falta de maestro" 3 "la madre trabaja" 4 "oficios de la casa" 5  "falta de dinero" 6"trabajo" 7"no le interesa" 8"mal tiempo" 9"embarazo" 10"migracion temporal" 11"acoso de maras" 12"violencia" 98"otra"
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

* educacion publica or privada
/* 
	 
       el plantel eductivo donde se |
                  inscribio (..) es: |      freq.     percent        cum.
-------------------------------------+-----------------------------------
             ministerio de educación |     11,813       63.89       63.89
                             pronade |      1,273        6.88       70.77
    nuevas escuelas unitarias -neus- |         14        0.08       70.85
otra institución de gobierno central |        533        2.88       73.73
                           municipal |        187        1.01       74.74
                         cooperativa |        860        4.65       79.39
                         comunitario |        105        0.57       79.96
                             privado |      3,465       18.74       98.70
                               ong´s |        146        0.79       99.49
                                otro |         94        0.51      100.00
-------------------------------------+-----------------------------------
                               total |     18,490      100.00

	 
	 
	 

se han considerado como publicas las opciones 1, 2, 3, 4 y 5 */


gen edupub_ci=0
replace edupub_ci=1 if ( p06b09==1 | p06b09==2 | p06b09==3 | p06b09==4 | p06b09==5 )
replace edupub_ci=. if ( p06b09==. | p06b09==99)
label var edupub_ci "1 = personas que asisten a centros de enseñanza publicos"

*************
**tecnica_ci*
*************

gen tecnica_ci=.
label var tecnica_ci "=1 formacion terciaria tecnica"




**********************************
*** income variables (monthly) ***
**********************************

* create a dummy indicating this person's income should not be included in y*_ch
gen miembros_ci=1
replace miembros_ci=0 if  (relacion_ci==0 | relacion_ci==6 | relacion_ci==.)
replace miembros_ci=0 if factor_ci==.
label variable miembros_ci "variable dummy que indica las personas que son miembros del hogar"

sort idh_ch

**********
***raza***
**********
/*
gen ethnic2=1 if ( p05b05>=1 & p05b05<=5 )  maya 
replace ethnic2=2 if ( p05b05==6 | p05b05==7 )  no maya 
replace ethnic2=3 if  p05b05==8  no indigena 

gen ethnic=1 if ethnic2==1 | ethnic2==2  indigena 
replace ethnic=2 if ethnic2==3  no indigena */
*/
gen raza_ci=.
replace raza_ci=1 if  p04a11a>=1 & p04a11a<=23
replace raza_ci=2 if  (p04a11a==24) & raza_ci==.
replace raza_ci=3 if  (p04a11a==29 |  p04a11a==30 |  p04a11a==96) & raza_ci==.
bys idh_ch: gen aux=raza_ci if relacion_ci==1
bys idh_ch: egen aux1 = max(aux)
replace raza_ci=aux1 if (raza_ci ==. & relacion_ci ==3)  
replace raza_ci=3 if raza_ci==. 
drop aux aux1
label define raza_ci 1 "indígena" 2 "afro-descendiente" 3 "otros"
label value raza_ci raza_ci 
label var raza_ci "raza o etnia del individuo" 

g raza_idioma_ci =.
/*
p04a11a:
           1 k´iche´
           2 q´eqchi´
           3 kaqchikel
           4 mam
           5 q´anjob´al
           6 achi
           7 ixil
           8 itza´
           9 poqomchi´
          10 chuj
          11 awakateko
          12 poqomam
          13 ch´orti´
          14 jakalteko (popti)
          15 sakapulteco
          16 mopan
          17 uspanteko
          18 tz´utujil
          19 tektiteko
          20 sipakapense
          21 chalchiteko
          22 akateko
          23 xinka
          24 garifuna
          29 no indigena
          30 extranjero
          96 ningún otro idioma

*/

*****************************************************************
*** ingresos laborales (para pesonas de 5 años o mas de ppa03) ***
*****************************************************************

***************************
*** ocupacion principal ***
***************************

/* p10b04 categoria ocupacional actividad principal
1 - 4 asalariados
5 - 6 independientes
7 - 8 trab sin pago
*/
gen categopri_ci=.
replace categopri_ci=1 if (p10b04==6 | p10b04==8) & emp_ci==1
replace categopri_ci=2 if (p10b04==5 | p10b04==7) & emp_ci==1
replace categopri_ci=3 if (p10b04>=1 & p10b04<=4) & emp_ci==1
replace categopri_ci=4 if (p10b04==9) & emp_ci==1
label var categopri_ci "categoria ocupacional actividad principal"
label define categopri_ci 1 "patron" 2 "cuenta propia" 3 "empleado" 4 "trabajador no remunerado"
label value categopri_ci categopri_ci


*****************
*tipocontrato_ci*
*****************
/*
gen tipocontrato_ci=.
replace tipocontrato_ci=1 if p10b06 ==1 
replace tipocontrato_ci=2 if p10b06 ==2 
replace tipocontrato_ci=3 if p10b05 ==2 
label var tipocontrato_ci "tipo de contrato segun su duracion"
label define tipocontrato_ci 1 "permanente/indefinido" 2 "temporal" 3 "sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci
*/

gen tipocontrato_ci=.
replace tipocontrato_ci=1 if (p10b05 ==1 & p10b06 ==1) & categopri_ci==3
replace tipocontrato_ci=2 if (p10b05 ==1 & p10b06 ==2) & categopri_ci==3
replace tipocontrato_ci=3 if (p10b05 ==2 | tipocontrato_ci==.) & categopri_ci==3
label var tipocontrato_ci "tipo de contrato segun su duracion"
label define tipocontrato_ci 1 "permanente/indefinido" 2 "temporal" 3 "sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci


****** ingresos monetarios ******

* ingreso independiente (se pregunta directamente por mes)
gen iindep=.
replace iindep=p10b22 if (p10b04==5 | p10b04==6) 
replace iindep=p10b23 if (p10b04==7 | p10b04==8) 
label var iindep "ingreso de los independientes ocupacion principal "


* salario o sueldo bruto mensual (sin incluir horas extra, comisiones, dietas y otas prestaciones de ley)
gen salario=.
replace salario=p10b08 if ( p10b04>=1 & p10b04<=4 ) 
label var salario "salario o sueldo mensual bruto ocupacion principal"

* bono 14
gen bono14=.
replace bono14=p10b11b/12 if ( p10b04>=1 & p10b04<=4 ) & p10b11a==1

* tips
gen tips=.
replace tips=p10b10b if ( p10b04>=1 & p10b04<=4 ) & p10b10a==1

* aguinaldo
gen aguin=.
replace aguin=p10b12b/12 if ( p10b04>=1 & p10b04<=4 ) & p10b12a==1


* diferido
gen diferido=.
replace diferido=p10b13b/12 if ( p10b04>=1 & p10b04<=4 ) & p10b13a==1

* bono vacacional
gen bonovac=.
replace bonovac=p10b14b/12 if ( p10b04>=1 & p10b04<=4 ) & p10b14a==1

* otros ingresos
gen otros=.
replace otros=p10b15b/12 if ( p10b04>=1 & p10b04<=4 ) & p10b15a==1



****** ingreso monetario laboral actividad principal ******
egen ylmpri_ci=rsum(salario bono14 tips aguin diferido bonovac otros) if ( p10b04>=1 & p10b04<=4 ), missing
replace ylmpri_ci=. if salario==. & bono14==. & tips==. & aguin==. & diferido==. & bonovac==. & otros==.
replace ylmpri_ci=iindep if ( p10b04==5 | p10b04==6 | p10b04==7 | p10b04==8 ) 
label var ylmpri_ci "ingreso laboral monetario de la actividad principal"

****** ingresos no monetarios ******
gen alim=0
replace alim=p10b19b if ( p10b04>=1 & p10b04<=4 )  & p10b19a==1
gen vivi=0
replace vivi=p10b20b if ( p10b04>=1 & p10b04<=4 ) & p10b20a==1
gen ropa=0
replace ropa=p10b16b/12 if ( p10b04>=1 & p10b04<=4 )  & p10b16a==1
gen transp=0
replace transp=p10b21b if ( p10b04>=1 & p10b04<=4 ) & p10b21a==1


****** ingreso laboral no monetario actividad principal ******
egen ylnmpri_ci=rsum(alim vivi ropa transp), missing
replace ylnmpri_ci=. if (alim==. & vivi==. & ropa==. & transp==.) | p10b04>4   
label var ylnmpri_ci " ingreso laboral no monetario ocupacion principal"

****************************
*** ocupacion secundaria ***
****************************

gen categosec_ci=.
replace categosec_ci=1 if (p10c04==6 | p10c04==8) & emp_ci==1
replace categosec_ci=2 if (p10c04==5 | p10c04==7) & emp_ci==1
replace categosec_ci=3 if (p10c04>=1 & p10c04<=4) & emp_ci==1
replace categosec_ci=4 if (p10c04==9) & emp_ci==1
label var categosec_ci "categoria ocupacional actividad secundaria"
label define categosec_ci 1 "patron" 2 "cuenta propia" 3 "empleado" 4 "trabajador no remunerado"
label value categosec_ci categosec_ci





****** ingresos monetarios ******

* ingreso independiente (se debe mensualizar)

gen iindep2=.
replace iindep2=p10c11 if (p10c04==5 | p10c04==6) 
replace iindep2=p10c12 if (p10c04==7 | p10c04==8) 
label var iindep "ingreso de los independientes ocupacion secundaria "



* salario o sueldo bruto mensual (no incluye horas extra, comisiones, dietas y otas prestaciones de ley)
gen salario2=.
replace salario2=p10c05 if ( p10c04>=1 & p10c04<=4 ) 
label var salario2 "salario o sueldo mensual bruto ocupacion secundaria"

* bono 14
gen bono142=.
replace bono142=p10c09b/12 if (  p10c04>=1 & p10c04<=4 ) & p10c09a==1

* tips
gen tips2=.
replace tips2=p10c06b if (  p10c04>=1 & p10c04<=4 ) & p10c06a==1

* otros ingresos
gen otros2=.
replace otros2 =p10c10b/12 if ( p10b04>=1 & p10b04<=4 ) & p10c10a==1




****** ingreso laboral monetario actividad secundaria ******
egen ylmsec_ci=rsum(salario2 bono142 tips2 otros2) if ( p10c04>=1 & p10c04<=4 ), missing
replace ylmsec_ci=. if salario2==. & bono142==. & tips2==. & otros2==.
replace ylmsec_ci=iindep2 if ( p10c04==5 | p10c04==6 | p10c04==7 | p10c04==8  ) 


****** ingreso laboral no monetario actividad secundaria ******

****** ingresos no monetarios ******
gen alim2=0
replace alim2=p10c07b if ( p10b04>=1 & p10b04<=4 )  & p10c07a==1
gen vivi2=0
replace vivi2=p10c08b if ( p10b04>=1 & p10b04<=4 ) & p10c08a==1


*****************
***ylmotros_ci***
*****************
gen ylmotros_ci=.
label var ylmotros_ci "ingreso laboral monetario de otros trabajos" 
*****************
***ylnmotros_ci***
******************

gen ylnmotros_ci=.
label var ylnmotros_ci "ingreso laboral no monetario de otros trabajos" 


****** ingreso laboral no monetario actividad secundaria ******
egen ylnmsec_ci=rsum(alim2 vivi2 ), missing
replace ylnmsec_ci=. if (alim2==. & vivi2==.) | p10c04>4   
label var ylnmsec_ci " ingreso laboral no monetario ocupacion secundaria"




****************************************************************************
*** ingreso laboral monetario total ( ocup principal + ocup secundaria ) ***
****************************************************************************
egen ylm_ci=rsum(ylmpri_ci ylmsec_ci), missing
replace ylm_ci=. if ylmpri_ci==. & ylmsec_ci==.
replace ylmpri_ci=. if iindep==. & salario==. & bono14==. & aguin==. & tips==.
replace ylm_ci=. if ylmpri_ci==. & iindep2==. & salario2==. & bono142==. & tips2==.
label var ylm_ci "ingreso laboral monetario total"


*******************************************************************************
*** ingreso laboral no monetario total ( ocup principal + ocup secundaria ) ***
*******************************************************************************
egen ylnm_ci=rsum(ylnmpri_ci ylnmsec_ci), missing
replace ylnm_ci=. if ylnmpri_ci==. & ylnmsec_ci==.
replace ylnm_ci=. if p10b04>4 & p10c04>4
label var ylnm_ci "ingreso laboral no monetario total"


****************************************************************************************************
*** otras fuentes de ingresos relacionadas con el trabajo(para personas de 7 años o mas de ppa03) ***
****************************************************************************************************

*** pensiones ***
gen pensa=. /* dinero por pension alimentos */
replace pensa=p11a08b/3 if p11a08a==1

gen pensj=. /* dinero por pension jubilacion */
replace pensj=p11a04b/3 if p11a04a==1


*** ayudas en dinero ***
gen ayins=.
replace ayins=p11a05b/3 if p11a05a==1

gen remesas_ci=. 
replace remesas_ci=p11a06b/3 if p11a06a==1
label var remesas_ci "remesas individuales"

egen ynlm_ci=rsum(pensa pensj ayins remesas_ci), missing
replace ynlm_ci=. if pensa==.  & pensj==. & ayins==. & remesas_ci==.
replace ynlm_ci=0 if p11a08a==2 &  p11a04a==2 &  p11a05a==2 &  p11a06a==2
label var ynlm_ci "ingreso no laboral monetario"

gen autocons_ci=.
label var autocons_ci "autoconsumo individual"

gen ynlnm_ci=.
label var ynlnm_ci "ingreso no laboral no monetario"

************************************************************************************
*** ingresos distintos del trabajo en dinero o bienes (montos totales del hogar) ***
************************************************************************************
* quienes tienen 0 en estos ingresos es porque reportan que no los han recibido en el hogar * 
gen halquiler=0 /* alquileres de edificios, casas, etc */
replace halquiler=p11a01b/3 if p11a01a==1
gen hdividendo=0 /* dividendos de acciones.... */
replace hdividendo=p11a03b/3 if p11a03a==1.
gen hintereses=0 /* dividendos de acciones.... */
replace hintereses=p11a02b/3 if p11a02a==1

gen hindemn=0 /* indemnizaciones  */
replace hindemn=p11a09b/3 if p11a09a==1


gen hbeca=0  /* becas y prestamos para estudios recibidos en efectivo */
replace hbeca=p11a07b/3 if p11a07a==1

gen hazar=0 /* loteria y juegos de azar */
replace hazar=p11a10b/3 if p11a10a==1


egen ynl_ch=rsum(halquiler hdividendo hintereses hindemn hbeca hazar) if miembros_ci==1, missing
label var ynl_ch "ingresos distintos del trabajo en dinero o bienes recibidos en el hogar" 

/*** flags
gen byte nrylmpri_ci=0
replace nrylmpri_ci=1 if
label var nrylmpri_ci "identificador de no respuesta del ingreso monetario de la actividad principal"

*** dummy para el hogar
capture drop nrylmpri_ch
sort idh_ch
egen nrylmpri_ch=sum(nrylmpri_ci) if miembro==1, by(idh) 
replace nrylmpri_ch=1 if nrylmpri_ch>1 & nrylmpri_ch~=. & miembro==1 
label var nrylmpri_ch "identificador de hogares en donde alguno de los miembros no responde el ingreso monetario de la actividad principal"

*/


egen ylm_ch=sum(ylm_ci) if miembros_ci==1, by(idh_ch) missing
label var ylm_ch "ingreso laboral monetario del hogar"

*egen ylmnr_ch=sum(ylm_ci) if miembros_ci==1 & nrylmpri_ch==0, by(idh_ch)
*label var ylmnr_ch "ingreso laboral monetario del hogar, considera 'missing' la no respuesta"

egen ylnm_ch=sum(ylnm_ci) if miembros_ci==1, by(idh_ch) missing
label var ylnm_ch "ingreso laboral no monetario del hogar"

egen ynlm_ch=sum(ynlm_ci) if miembros_ci==1, by(idh_ch) missing
label var ynlm_ch "ingreso no laboral monetario del hogar"

egen ynlnm_ch=sum(ynlnm_ci) if miembros_ci==1, by(idh_ch) missing
label var ynlnm_ch "ingreso no laboral no monetario del hogar"

egen autocons_ch=sum(autocons_ci) if miembros_ci==1, by(idh_ch) missing
label var autocons_ch "autoconsumo del hogar"

egen remesas_ch=sum(remesas_ci) if miembros_ci==1, by(idh_ch) missing
label var remesas_ch "remesas del hogar (monetario + especies)"

replace ynlnm_ch=. if ynlnm_ci==.
replace autocons_ch=. if autocons_ci==.
replace ylm_ch =. if miembros_ci==0
*replace ylmnr_ch =. if miembros_ci==0
replace ylnm_ch =. if miembros_ci==0
replace ynlnm_ch =. if miembros_ci==0
replace autocons_ch =. if miembros_ci==0
replace remesas_ch =. if miembros_ci==0
replace ynlm_ch =. if miembros_ci==0

*** ingreso horario de todos los trabajos ***
/* this is not accurate in the sense that if you have more than one job
you will have wage averaged over several jobs */
gen ylmho_ci=ylm_ci/(horastot_ci*4.3) 
label var ylmho_ci "salario horario monetario de todas las actividades"

*** ingreso horario de la ocupacion principal ***
gen ylmhopri_ci=ylmpri_ci/(horaspri_ci*4.3) 
label var ylmhopri_ci "salario horario monetario de la actividad principal"


drop iindep* salario* bono14* tips* aguin* alim vivi ropa transp hh pensa pensj ayins


******************
***categoinac_ci**
******************
gen categoinac_ci=.
replace categoinac_ci=1 if p10f01 ==4 & condocup_ci==3
replace categoinac_ci=2 if p10f01 ==1 & condocup_ci==3
replace categoinac_ci=3 if p10f01 ==2 & condocup_ci==3
recode categoinac_ci .= 0 if condocup_ci==3

label var categoinac_ci "condición de inactividad"
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
gen id_ind_ci = .
gen id_afro_ci = .
/*_____________________________________________________________________________________________________*/
* verificación de que se encuentren todas las variables del sociometro y las nuevas de mercado laboral
* también se incluyen variables que se manejaban en versiones anteriores, estas son:
* firmapeq_ci nrylmpri_ch nrylmpri_ci tcylmpri_ch tcylmpri_ci tipopen_ci
/*_____________________________________________________________________________________________________*/

do "$gitFolder\armonizacion_microdatos_encuestas_hogares_scl\_DOCS\\Labels&ExternalVars_Harmonized_DataBank.do"

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
*firmapeq_ci


	
qui destring $var, replace


* activar solo si es necesario
*keep *_ci  *_c  idh_ch 
set more off
compress





saveold "`base_out'", replace


log close
