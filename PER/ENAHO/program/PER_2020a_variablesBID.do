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

local PAIS PER
local ENCUESTA ENAHO
local ANO "2020"
local ronda a 
local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
                        
capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Perú
Encuesta: ENAHO
Round: a
Autores: Marcela G. Rubio E-mail: marcelarubio28@gmail.com - mrubio@iadb.org
Última modificación: Stephanie González. E-mail: stephaniego@iadb.org
Fecha última modificación: 25 de junio de 2020

							SCL/LMK - IADB
****************************************************************************/
****************************************************************************/

use `base_in', clear

***************
***region_c ***
***************
gen region_c=real(substr(ubigeo,1,2))
gen ine01=region_c 

label define region_c ///
1"Amazonas"	          ///
2"Ancash"	          ///
3"Apurimac"	          ///
4"Arequipa"	          ///
5"Ayacucho"	          ///
6"Cajamarca"	      ///
7"Callao"	          ///
8"Cusco"	          ///
9"Huancavelica"	      ///
10"Huanuco"	          ///
11"Ica"	              ///
12"Junin"	          ///
13"La libertad"	      ///
14"Lambayeque"	      ///
15"Lima"	          ///
16"Loreto"	          ///
17"Madre de Dios"	  ///
18"Moquegua"	      ///
19"Pasco"	          ///
20"Piura"	          ///
21"Puno"	          ///
22"San Martín"	      ///
23"Tacna"	          ///
24"Tumbes"	          ///
25"Ucayali"	
label value region_c region_c
label var region_c "division politico-administrativa, departamento"

************************
*** region según BID ***
************************
gen region_BID_c=3 
label var region_BID_c "Regiones BID"
label define region_BID_c 1 "Centroamérica_(CID)" 2 "Caribe_(CCB)" 3 "Andinos_(CAN)" 4 "Cono_Sur_(CSC)"
label value region_BID_c region_BID_c

***************
***factor_ch***
***************
gen factor_ch= factor07
label variable factor_ch "Factor de expansion del hogar"
/*Nota: La variable "factlab" se crea solo para PERU porque el factor para las variables laborales
 es diferente al factor de las demas variables, como las demograficas */
gen factlab=.
replace factlab=fac500a
label var factlab "factor de expansion para variables laborales, solo PERU"

***************
****idh_ch*****
***************

sort conglome vivienda hogar 
cap egen idh_ch= group(conglome vivienda hogar)
label variable idh_ch "ID del hogar"

*************
*****idp_ci****
**************

gen idp_ci = codperso
label variable idp_ci "ID de la persona en el hogar"

**********
***zona***
**********
* SGR comment: Cambió la clasificación.
/*
estrato:

Área Urbana:	
	1	De 500 000 o más habitantes
	2	De 100 000 a 499 999 habitantes
	3	De 50 000 a 99 999 habitantes
	4	De 20 000 a 49 999 habitantes
	5	De 2 000 a 19 999 habitantes

Area Rural:	 
	6	De 500 a 1 999 habitantes
	7	Área de empadronamiento rural	-	aer	compuesto
	8	Área de empadronamiento rural	-	aer	simple
*/


gen byte zona_c= 0 if estrato>=6
replace  zona_c= 1 if estrato<6

label variable zona_c "Zona del pais"
label define zona_c 1 "Urbana" 0 "Rural"
label value zona_c zona_c

************
****pais****
************

gen str3 pais_c="PER"
label variable pais_c "Pais"

**********
***anio***
**********

gen anio_c=2020
label variable anio_c "Anio de la encuesta"

*********
***mes***
*********
tostring fecent, replace 
gen mes_c=real(substr(fecent,5,2))
label variable mes_c "Mes de la encuesta"

*****************
***relacion_ci***
*****************

/*
p203:
           0 panel
           1 jefe/jefa
           2 esposo/esposa
           3 hijo/hija
           4 yerno/nuera
           5 nieto
           6 padres/suegros
           7 otros parientes
           8 trabajador hogar
           9 pensionista
          10 otros no parientes
		  11 Hermano(a)
*/


gen relacion_ci=.
replace relacion_ci=1 if p203==1
replace relacion_ci=2 if p203==2
replace relacion_ci=3 if p203==3
*Add SGR p203==11
replace relacion_ci=4 if p203>=4 & p203<=7 | p203==11
replace relacion_ci=5 if p203==10 | p203==9
replace relacion_ci=6 if p203==8

label variable relacion_ci "Relacion con el jefe del hogar"
label define relacion_ci 1 "Jefe/a" 2 "Esposo/a" 3 "Hijo/a" 4 "Otros parientes" 5 "Otros no parientes"
label define relacion_ci 6 "Empleado/a domestico/a", add

label value relacion_ci relacion_ci



****************************
***VARIABLES DEMOGRAFICAS***
****************************

***************
***factor_ci***
***************

gen factor_ci=facpob07
label variable factor_ci "Factor de expansion del individuo"

***************
***upm_ci***
***************

gen upm_ci=conglome

***************
***estrato_ci***
***************

gen estrato_ci=estrato

**********
***sexo***
**********

gen sexo_ci=p207
label define sexo_ci 1 "Hombre" 2 "Mujer"
label value sexo_ci sexo_ci

**********
***edad***
**********

gen edad_ci=p208a
*Add SGR Julio 2020
replace edad_ci=. if edad_ci==99
label variable edad_ci "Edad del individuo"

*****************
***civil_ci***
*****************

/*
p209:
           1 conviviente
           2 casado (a)
           3 viudo (a)
           4 divorciado (a)
           5 separado (a)
           6 soltero (a)
*/

gen civil_ci=.
replace civil_ci=1 if p209==6
replace civil_ci=2 if p209==1 | p209==2
replace civil_ci=3 if p209==4 | p209==5
replace civil_ci=4 if p209==3

label variable civil_ci "Estado civil"
label define civil_ci 1 "Soltero" 2 "Union formal o informal"
label define civil_ci 3 "Divorciado o separado" 4 "Viudo" , add
label value civil_ci civil_ci

*************
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



         ******************************
         *** VARIABLES DE DIVERSIDAD **
         ******************************
*Nathalia Maya & Antonella Pereira
*Feb 2021	

	***************
	***afroind_ci**
	***************
**Pregunta por sus antepasados y de acuerdo a sus costumbres, �ud. se considera:(p558c) (1 quechua; 2 aymara; 3 nativo o indígena de la amazonía; 4 negro/ mulato/zambo/afroperuano; 5 blanco; 6 mestizo; 7 otro; 8 no sabe;  9 Perteneciente o parte de otro Pubelo indígena u originario) 
** En el 2019 se agrega 9 Perteneciente o parte de otro Pubelo indígena u originario
gen afroind_ci=. 
replace afroind_ci=1 if p558c == 1 |  p558c == 2 |  p558c ==3 | p558c==9 
replace afroind_ci=2 if p558c == 4
replace afroind_ci=3 if p558c ==5 | p558c ==6 | p558c ==7 
replace afroind_ci=. if p558c ==8 | p558c ==.
replace afroind_ci=9 if p558c ==. & edad_ci<14


	***************
	***afroind_ch***
	***************
gen afroind_jefe= afroind_ci if relacion_ci==1
egen afroind_ch  = min(afroind_jefe), by(idh_ch) 
drop afroind_jefe

	*******************
	***afroind_ano_c***
	*******************
gen afroind_ano_c=2017


	*******************
	***dis_ci***
	*******************
	
	/* Respuestas binarias (1 = si; 2 = no)
	 p401h1. Moverse o caminar, para usar brazos o piernas?  
	 p401h2. Ver, aun usando anteojos?  
	 p401h3. Hablar o comunicarse, aún usando el lenguaje de señas u otro?  
	 p401h4. Oír, aún usando audífonos ?  
	 p401h5. Entender o aprender (concentrarse y recordar)? 
	 
	 No considerar p401h6:
	 p401h6. Relacionarse con los demás, por sus pensamientos, sentimientos, emociones o conductas? 
	
	*/
	
	
gen dis_ci =.
replace dis_ci = 1 if (p401h1 == 1 | p401h2 == 1 | p401h3 == 1 | p401h4 == 1 | p401h5 == 1) 
replace dis_ci = 0 if (p401h1 == 2 & p401h2 == 2 & p401h3 == 2 & p401h4 == 2 & p401h5 == 2) 


	*******************
	***dis_ch***
	*******************
	
egen dis_ch = sum(dis_ci), by(idh_ch) 
replace dis_ch = 1 if (dis_ch > 0)



************************************
*** VARIABLES DEL MERCADO LABORAL***
************************************
/* Esta sección es para los residentes habituales del hogar mayores a 14 años */ 

****************
****condocup_ci*
****************

gen condocup_ci=.
replace condocup_ci=1 if ocu500==1
replace condocup_ci=2 if ocu500==2
replace condocup_ci=3 if condocup_ci!=1 & condocup_ci!=2
replace condocup_ci=4 if edad_ci<14
label define condocup_ci 1"ocupados" 2"desocupados" 3"inactivos" 4"menor de PET"
label value condocup_ci condocup_ci
label var condocup_ci "Condicion de ocupacion utilizando definicion del pais"

* MGD 06/06/2014: Se conserva la generacion de condocup_ci con la variable creada ya que coincide con series externas, 
* al hacerla considerando la definicion con variables originales, no coincide la serie.
* Sin embargo, a continuacion la sintaxis con variables originales.

/*
Alternativa 2 con variables originales MGD 06/06/2014
g condocup_ci1=.
replace condocup_ci1=1 if p501==1 | p502==1 | p503==1 | p5041==1 | p5042==1 | p5043==1 | p5044==1 | p5045==1 | p5046==1 | p5047==1 | p5048==1 | p5049==1 | p50410==1 | p50411==1 
replace condocup_ci1=2 if condocup_ci1!=1 & (p545==1 |  p546<=2)
recode condocup_ci1 .=3 if edad_ci>=14
recode condocup_ci1 .=4 if edad_ci<14
*/

****************
*afiliado_ci****
****************
gen afiliado_ci=0     if condocup_ci==1 | condocup_ci==2 
*replace afiliado_ci=1 if (p558a1==1 | p558a2==1 | p558a3==1 | p558a4==1) & afiliado_ci==0 
replace afiliado_ci=1 if (p558a1==1 | p558a2==2 | p558a3==3 | p558a4==4) & afiliado_ci==0 
label var afiliado_ci "Afiliado a la Seguridad Social"

* Formalidad sin restringir a PEA
gen afiliado_ci1=0     if condocup_ci>=1 & condocup_ci<=3
*replace afiliado_ci=1 if (p558a1==1 | p558a2==1 | p558a3==1 | p558a4==1) & afiliado_ci==0 
replace afiliado_ci1=1 if (p558a1==1 | p558a2==2 | p558a3==3 | p558a4==4) & afiliado_ci1==0 
label var afiliado_ci1 "Afiliado a la Seguridad Social"

****************
*tipopen_ci*****
****************
gen tipopen_ci=.
********************
*** instcot_ci *****
********************
gen instcot_ci=. /*La variable */
label var instcot_ci "institución a la cual cotiza"

****************
*cotizando_ci***
****************
*Agregado por SGR. Julio 2020
replace p524b1=. if p524b1==999999
replace p538b1=. if p538b1==999999

gen cotizando_ci=0     if condocup_ci==1 | condocup_ci==2
replace cotizando_ci=1 if ((p524b1>0 & p524b1!=.) | (p538b1>0 & p538b1!=.)) & cotizando_ci==0 /*a ocupados subordinados: empleados u obreros*/
label var cotizando_ci "Cotizante a la Seguridad Social"

* Formalidad sin restringir a PEA
gen cotizando_ci1=0     if condocup_ci>=1 & condocup_ci<=3 
replace cotizando_ci1=1 if ((p524b1>0 & p524b1!=.) | (p538b1>0 & p538b1!=.)) & cotizando_ci1==0 /*a ocupados subordinados: empleados u obreros*/
label var cotizando_ci1 "Cotizante a la Seguridad Social"

gen cotizapri_ci=0     if condocup_ci==1 | condocup_ci==2 
replace cotizapri_ci=1 if (p524b1>0 & p524b1!=.)  
label var cotizapri_ci "Cotizante a la Seguridad Social por su trabajo principal"

gen cotizasec_ci=0     if condocup_ci==1 | condocup_ci==2 
replace cotizasec_ci=1 if (p538b1>0 & p538b1!=.)  
label var cotizasec_ci "Cotizante a la Seguridad Social por su trabajo secundario"


*************
*tamemp_ci***
*************
/*
gen tamemp_ci=p512b
label var tamemp_ci "# empleados en la empresa de la actividad principal"
*/
gen tamemp_ci=1 if p512b>=1 &  p512b<=5 
label var  tamemp_ci "Tamaño de Empresa" 
*Empresas medianas
replace tamemp_ci=2 if p512b>=6 &  p512b<=50
*Empresas grandes
replace tamemp_ci=3 if p512b>=51 &  p512b<=9998
label define tamaño 1"Pequeña" 2"Mediana" 3"Grande"
label values tamemp_ci tamaño

****************
**categoinac_ci*
****************
gen categoinac_ci =1 if (p546==6 & condocup_ci==3)
replace categoinac_ci = 2 if  (p546==4 & condocup_ci==3)
replace categoinac_ci = 3 if  (p546==5 & condocup_ci==3)
replace categoinac_ci = 4 if  ((categoinac_ci ~=1 & categoinac_ci ~=2 & categoinac_ci ~=3) & condocup_ci==3)
label var categoinac_ci "Categoría de inactividad"
label define categoinac_ci 1 "jubilados o pensionados" 2 "Estudiantes" 3 "Quehaceres domésticos" 4 "Otros"
label value categoinac_ci categoinac_ci

*************
   *ypen_ci*
*************
*Comm SGR. Esto hace referencia a la pensión y cesantía.

generat pjub=p5564c*30 if p5564b==1 
replace pjub=p5564c*4.3  if p5564b==2 
replace pjub=p5564c*2  if p5564b==3 
replace pjub=p5564c    if p5564b==4 
replace pjub=p5564c/2  if p5564b==5 
replace pjub=p5564c/3  if p5564b==6 
replace pjub=p5564c/6  if p5564b==7 
replace pjub=p5564c/12 if p5564b==8 
replace pjub=.         if p5564c==999999

generat pviudz=p5565c*30 if p5565b==1 
replace pviudz=p5565c*4.3 if p5565b==2 
replace pviudz=p5565c*2  if p5565b==3 
replace pviudz=p5565c    if p5565b==4 
replace pviudz=p5565c/2  if p5565b==5 
replace pviudz=p5565c/3  if p5565b==6 
replace pviudz=p5565c/6  if p5565b==7 
replace pviudz=p5565c/12 if p5565b==8 
replace pviudz=.         if p5565c==999999

egen ypen_ci= rsum(pjub pviudz), m
replace ypen_ci=. if pjub==. & pviudz==.
label var ypen_ci "Valor de la pension contributiva"


*************
**pension_ci*
*************
/*gen pension_ci=0 
replace pension_ci=1 if (p5564a==1 | p5565a==1 ) /* A todas las per mayores de 13*/
replace pension_ci=. if p5564a==. & p5565a==.
label var pension_ci "1=Recibe pension contributiva"*/

*MGD 12/29/2015: recibe pension si declara ingreso por pension
gen pension_ci= (ypen_ci>0 & ypen_ci!=.)
label var pension_ci "1=Recibe pension contributiva"

****************
*instpen_ci*****
****************
gen instpen_ci=.
label var instpen_ci "Institucion proveedora de la pension - variable original de cada pais" 

***************
*pensionsub_ci*
***************

*Modificación Mayra Sáenz- Julio 2015: Se reemplazan por variables originales
gen pensionsub_ci = ingtpu03>0 & ingtpu03!=.
label var pensionsub_ci "1=recibe pension subsidiada / no contributiva"

*****************
**  ypensub_ci  *
*****************

*Modificación Mayra Sáenz- Julio 2015: Se reemplazan por variables originales
gen ypensub_ci    = ingtpu03/12
label var ypensub_ci "Valor de la pension subsidiada / no contributiva"

*************
*cesante_ci* 
*************
gen cesante_ci=0 if condocup_ci==2
replace cesante_ci=1 if p552==1 & condocup_ci==2
label var cesante_ci "Desocupado - definicion oficial del pais"


***********************
*llave lp nacionales***
***********************
egen llave_lp=group(linea linpe)

*********
*lp_ci***
*********
gen lp_ci =linea
label var lp_ci "Linea de pobreza oficial del pais"

*********
*li_ci***
*********
gen lpe_ci =linpe
label var lpe_ci "Linea de indigencia oficial del pais"

/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/

*************
**salmm_ci***
*************

gen salmm_ci= 930 /*se mantiene para todos los meses de 2020*/
label var salmm_ci "Salario minimo legal"


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
* Formalidad sin restringir a PEA
gen formal_1=(cotizando_ci1==1)

*****************
***desalent_ci***
*****************

gen desalent_ci=(emp_ci==0 & p545==2 & (p549==1 | p549==2))

*****************
***horaspri_ci***
*****************

gen horaspri_ci=p513t 
replace horaspri_ci=. if emp_ci~=1

*****************
***horastot_ci***
*****************
*Add SGR. Julio 2020
replace p518=. if p518==99
egen horastot_ci=rsum(horaspri_ci p518)
replace horastot_ci=. if horaspri_ci==. & p518==.
replace horastot_ci=. if emp_ci~=1

***************
***subemp_ci***
***************
gen subemp_ci=0
replace subemp_ci=1 if horaspri_ci<=30 & p521==1 & p521a==1 & emp_ci==1 

*******************
***tiempoparc_ci***
*******************
/*Sobre las horas normalmente trabajadas*/
gen tiempoparc_ci=0
/*replace tiempoparc_ci=1 if (p519==1 & horaspri_ci>=1 & horaspri_ci<=30) & p521==2
replace tiempoparc_ci=1 if (p519==2 & p520<=30) & p521==2
replace tiempoparc_ci=. if emp_ci==.*/
* 10/20/2015 MGD: no se usa las horas totales trabajadas sino solo las de la actividad principal.
replace tiempoparc_ci=1 if (horaspri_ci>=1 & horaspri_ci<30) & p521==2 & emp_ci==1
replace tiempoparc_ci=. if emp_ci==0


******************
***categopri_ci***
******************
* 10/20/2015 MGD: se aña de la categoria otra clasificacion para sacarlos de los no remunerados

gen categopri_ci=.
replace categopri_ci=0 if p507==7
replace categopri_ci=1 if p507==1
replace categopri_ci=2 if p507==2 
replace categopri_ci=3 if p507==3 | p507==4 | p507==6 
replace categopri_ci=4 if p507==5 /*| p507==7*/

label define categopri_ci 0 "Otra clasificación" 1"Patron" 2"Cuenta propia" 
label define categopri_ci 3"Empleado" 4" No remunerado", add
label value categopri_ci categopri_ci
label variable categopri_ci "Categoria ocupacional trabajo principal"

/*
p507:
           1 empleador o patrono
           2 trabajador independiente
           3 empleado
           4 obrero
           5 trabajador familiar no remunerado
           6 trabajador del hogar
           7 otro
*/

******************
***categosec_ci***
******************
* 10/20/2015 MGD: se aña de la categoria otra clasificacion para sacarlos de los no remunerados
gen categosec_ci=.
replace categosec_ci=0 if p517==7
replace categosec_ci=1 if p517==1
replace categosec_ci=2 if p517==2 
replace categosec_ci=3 if p517==3 | p517==4 | p517==6
replace categosec_ci=4 if p517==5 /*| p517==7*/

label define categosec_ci 0 "Otra clasificación" 1"Patron" 2"Cuenta propia" 
label define categosec_ci 3"Empleado" 4 "No remunerado" , add
label value categosec_ci categosec_ci
label variable categosec_ci "Categoria ocupacional trabajo secundario"

/*
p517:
           1 empleador o patrono
           2 trabajador independiente
           3 empleado
           4 obrero
           5 trabajador familiar no remunerado
           6 trabajador del hogar
           7 otro
*/

*****************
*tipocontrato_ci*

*****************
gen tipocontrato_ci=. /* Solo disponible para asalariados*/
replace tipocontrato_ci=1 if (p511a==1) & categopri_ci==3
replace tipocontrato_ci=2 if (p511a>=2 & p511a<=6) & categopri_ci==3
replace tipocontrato_ci=3 if (p511a==7 | tipocontrato_ci==.) & categopri_ci==3
label var tipocontrato_ci "Tipo de contrato segun su duracion en act principal"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci
   

*****************
***nempleos_ci***
*****************

gen nempleos_ci=.
replace nempleos_ci=1 if emp_ci==1
replace nempleos_ci=2 if emp_ci==1 & p514==1

#delimit ;
replace nempleos_ci=2 if emp_ci==1 & p514==2 & (p5151==1 | p5152==1 | p5153==1 |
        p5154==1 | p5155==1 | p5156==1 | p5157==1 | p5158==1 | p5159==1 | p51510==1 |
        p51511==1);
#delimit cr


*****************
***spublico_ci***
*****************

gen spublico_ci=(p510==1 | p510==2 | p510==3)
replace spublico_ci=. if emp_ci~=1

**************
***ocupa_ci***
**************
*CIOU-88

gen ocupa_ci=.
replace ocupa_ci=1 if (p505>=211 & p505<=396) & emp_ci==1
replace ocupa_ci=2 if (p505>=111 & p505<=148) & emp_ci==1
replace ocupa_ci=3 if (p505>=411 & p505<=462) & emp_ci==1
replace ocupa_ci=4 if (p505>=571 & p505<=583) | (p505>=911 & p505<=931) & emp_ci==1
replace ocupa_ci=5 if (p505>=511 & p505<=565) | (p505>=941 & p505<=961) & emp_ci==1
replace ocupa_ci=6 if (p505>=611 & p505<=641) | (p505>=971 & p505<=973) & emp_ci==1
replace ocupa_ci=7 if (p505>=711 & p505<=886) | (p505>=981 & p505<=987) & emp_ci==1
replace ocupa_ci=8 if (p505>=11 & p505<=24) & emp_ci==1

label variable ocupa_ci "Ocupacion laboral"
label define ocupa_ci 1"profesional y tecnico" 2"director o funcionario sup" 3"administrativo y nivel intermedio"
label define ocupa_ci  4 "comerciantes y vendedores" 5 "en servicios" 6 "trabajadores agricolas", add
label define ocupa_ci  7 "obreros no agricolas, conductores de maq y ss de transporte", add
label define ocupa_ci  8 "FFAA" 9 "Otras ", add
label value ocupa_ci ocupa_ci


/*
gen ocupa_ci=.
replace ocupa_ci=1 if (p505>=211 & p505<=396) & emp_ci==1
replace ocupa_ci=2 if (p505>=111 & p505<=148) & emp_ci==1
replace ocupa_ci=3 if (p505>=411 & p505<=462) & emp_ci==1
replace ocupa_ci=4 if (p505>=571 & p505<=583) | (p505>=911 & p505<=931) & emp_ci==1
replace ocupa_ci=5 if (p505>=511 & p505<=565) | (p505>=941 & p505<=961) & emp_ci==1
replace ocupa_ci=6 if (p505>=611 & p505<=641) | (p505>=971 & p505<=973) & emp_ci==1
replace ocupa_ci=7 if (p505>=711 & p505<=886) | (p505>=981 & p505<=987) & emp_ci==1
replace ocupa_ci=8 if (p505>=11 & p505<=24) & emp_ci==1

label variable ocupa_ci "Ocupacion laboral"
label define ocupa_ci 1"profesional y tecnico" 2"director o funcionario sup" 3"administrativo y nivel intermedio"
label define ocupa_ci  4 "comerciantes y vendedores" 5 "en servicios" 6 "trabajadores agricolas", add
label define ocupa_ci  7 "obreros no agricolas, conductores de maq y ss de transporte", add
label define ocupa_ci  8 "FFAA" 9 "Otras ", add
label value ocupa_ci ocupa_ci
*/
*************
***rama_ci***
*************
*Add SGR, aunque en la base de datos la clasificación 500 llega solo a 500, en el clasificador oficial llega a 502*
gen rama_ci=.
replace rama_ci=1 if (p506>=111 & p506<=502) & emp_ci==1
replace rama_ci=2 if (p506>=1010 & p506<=1429) & emp_ci==1
replace rama_ci=3 if (p506>=1511 & p506<=3720) & emp_ci==1
replace rama_ci=4 if (p506>=4010 & p506<=4100) & emp_ci==1
replace rama_ci=5 if (p506>=4510 & p506<=4550) & emp_ci==1
replace rama_ci=6 if (p506>=5010 & p506<=5520) & emp_ci==1 
replace rama_ci=7 if (p506>=6010 & p506<=6420) & emp_ci==1
replace rama_ci=8 if (p506>=6511 & p506<=7020) & emp_ci==1
replace rama_ci=9 if (p506>=7111 & p506<=9900) & emp_ci==1

label var rama_ci "Rama de actividad de la ocupación principal"
label def rama_ci 1"Agricultura, caza, silvicultura y pesca" 2"Explotación de minas y canteras" 3"Industrias manufactureras"
label def rama_ci 4"Electricidad, gas y agua" 5"Construcción" 6"Comercio, restaurantes y hoteles" 7"Transporte y almacenamiento", add
label def rama_ci 8"Establecimientos financieros, seguros e inmuebles" 9"Servicios sociales y comunales", add
label val rama_ci rama_ci


* rama secundaria
gen ramasec_ci=.
replace ramasec_ci=1 if (p516>=111 & p516<=502) & emp_ci==1
replace ramasec_ci=2 if (p516>=1010 & p516<=1429) & emp_ci==1
replace ramasec_ci=3 if (p516>=1511 & p516<=3720) & emp_ci==1
replace ramasec_ci=4 if (p516>=4010 & p516<=4100) & emp_ci==1
replace ramasec_ci=5 if (p516>=4510 & p516<=4550) & emp_ci==1
replace ramasec_ci=6 if (p516>=5010 & p516<=5520) & emp_ci==1 
replace ramasec_ci=7 if (p516>=6010 & p516<=6420) & emp_ci==1
replace ramasec_ci=8 if (p516>=6511 & p516<=7020) & emp_ci==1
replace ramasec_ci=9 if (p516>=7111 & p516<=9900) & emp_ci==1
label var ramasec_ci "Rama de actividad de la ocupación secundaria"
label def ramasec_ci 1"Agricultura, caza, silvicultura y pesca" 2"Explotación de minas y canteras" 3"Industrias manufactureras"
label def ramasec_ci 4"Electricidad, gas y agua" 5"Construcción" 6"Comercio, restaurantes y hoteles" 7"Transporte y almacenamiento", add
label def ramasec_ci 8"Establecimientos financieros, seguros e inmuebles" 9"Servicios sociales y comunales", add
label val ramasec_ci ramasec_ci



****************
***durades_ci***
****************
/*En meses*/
* no se puede identificar si hace mas de un año esta buscando.
*Add SGR. Julio 2020
replace p551=. if p551==999
gen durades_ci=p551/4.3 /* calculo sin filtros if desemp_ci==1*/

*******************
***antiguedad_ci***
*******************

gen anios_ant=p513a1
gen meses_ant=p513a2/12

egen antiguedad_ci = rsum(anios_ant meses_ant)
replace antig=. if anios_ant==. & meses_ant==.

*************************************************************************************
*******************************INGRESOS**********************************************
*************************************************************************************

*Mayra Sáenz- Julio 2015: En este quietly se encuentra la generación de ingresos anterior.
/*
quietly {
***************
***ylmpri_ci***
***************

*Para los trabajadores dependientes

gen ypridbd=.
replace ypridbd=p524e1*30 	if p523==1 
replace ypridbd=p524e1*4.3 	if p523==2 
replace ypridbd=p524e1*2 	if p523==3 
replace ypridbd=p524e1 		if p523==4 
replace ypridbd=. if categopri_ci==4 

/* A los trabajadores no remunerados que trabajan menos de 15 horas la encuesta no les
pregunta acerca de sus ingresos y los manda a la sección de desempleo. Como este grupo en
realidad está trabajando, reemplazo su ingreso missing por cero*/

replace ypridbd=. if categopri_ci<=2
*reemplaza por missing porque es solo ingreso para dependientes

*Para los trabajadores independientes 
/*En este caso la pregunta no permite distinguir entre ingreso monetario
y en especie, vamos a asumir que es monetario*/

gen yprijbi=.
replace yprijbi= p530a
replace yprijbi=. if p530a==999999
replace yprijbi=. if categopri_ci>2


*Ingreso laboral monetario para todos

egen ylmpri_ci=rsum(yprijbi ypridbd)
replace ylmpri_ci=. if ypridbd==. & yprijbi==. 
replace ylmpri_ci=. if p51112==1
replace ylmpri_ci=. if emp_ci~=1


*******************
*** nrylmpri_ci ***
*******************

gen nrylmpri_ci=(ylmpri_ci==. & emp_ci==1)


******************
*** ylnmpri_ci ***
******************

*Ingreso laboral no monetario de los dependientes

forvalues i =1(1) 6 {
gen especie`i'=.
replace especie`i'=p529`i'b*30  if p529`i'a==1
replace especie`i'=p529`i'b*4.3 if p529`i'a==2
replace especie`i'=p529`i'b*2   if p529`i'a==3
replace especie`i'=p529`i'b     if p529`i'a==4
replace especie`i'=p529`i'b/2   if p529`i'a==5
replace especie`i'=p529`i'b/3   if p529`i'a==6
replace especie`i'=p529`i'b/6   if p529`i'a==7
replace especie`i'=p529`i'b/12  if p529`i'a==8
replace especie`i'=. 		if p529`i'b==999999
}

egen ylnmprid=rsum(especie1 especie2 especie3 especie4 especie5 especie6)
replace ylnmprid=. if especie1==. &  especie2==. & especie3==. & especie4==. & especie5==. & especie6==.
replace ylnmprid=0 if categopri_ci==4
replace ylnmprid=0 if p528==2
replace ylnmprid=. if categopri_ci<=2 | categopri_ci==.

*Ingreso laboral no monetario de los independientes (autoconsumo)

gen ylnmprii=p536
replace ylnmprii=. if p536==999999
replace ylnmprii=. if categopri_ci>2

*Ingreso laboral no monetario para todos

egen ylnmpri_ci=rsum(ylnmprid ylnmprii)
replace ylnmpri_ci=. if ylnmprid==. & ylnmprii==.
replace ylnmpri_ci=. if emp_ci~=1

***************
***ylmsec_ci***
***************

*Para los trabajadores dependientes

gen ysecbd=.
replace ysecbd=p538e1
replace ysecbd=. if p538e1==999999
replace ysecbd=. if categosec_ci==4
replace ysecbd=. if categosec_ci<=2 | categosec_ci==.

*Para los trabajadores independientes
/*En este caso la pregunta no permite distinguir entre ingreso monetario
y en especie, vamos a asumir que es monetario*/

gen ysecjbi=.
replace ysecjbi=p541a
replace ysecjbi=. if p541a==999999
replace ysecjbi=. if categosec_ci>2

*Ingreso laboral monetario para todos

egen ylmsec_ci=rsum(ysecjbi ysecbd)
replace ylmsec_ci=. if ysecjbi==. & ysecbd==.
replace ylmsec_ci=. if p53712==1
replace ylmsec_ci=. if emp_ci~=1


******************
****ylnmsec_ci****
******************

*Ingreso laboral no monetario de los dependientes

forvalues i =1(1) 6 {
gen especiesec`i'=.
replace especiesec`i'=p540`i'b*30  if p540`i'a==1
replace especiesec`i'=p540`i'b*4.3 if p540`i'a==2
replace especiesec`i'=p540`i'b*2   if p540`i'a==3
replace especiesec`i'=p540`i'b     if p540`i'a==4
replace especiesec`i'=p540`i'b/2   if p540`i'a==5
replace especiesec`i'=p540`i'b/3   if p540`i'a==6
replace especiesec`i'=p540`i'b/6   if p540`i'a==7
replace especiesec`i'=p540`i'b/12  if p540`i'a==8
replace especiesec`i'=. if p540`i'b==999999
}

egen ylnmsecd=rsum(especiesec1 especiesec2 especiesec3 especiesec4 especiesec5 especiesec6)
replace ylnmsecd=. if especiesec1==. &  especiesec2==. & especiesec3==. & especiesec4==. & especiesec5==. & especiesec6==.
replace ylnmsecd=0 if categosec_ci==4
replace ylnmsecd=. if emp_ci~=1

*Ingreso laboral no monetario de los independientes (autoconsumo)

gen ylnmseci=p543
replace ylnmseci=. if p543==999999
replace ylnmseci=. if emp_ci~=1

*Ingreso laboral no monetario para todos

egen ylnmsec_ci=rsum(ylnmsecd ylnmseci)
replace ylnmsec_ci=. if ylnmsecd==. & ylnmseci==.
replace ylnmsec_ci=. if emp_ci==0


************
***ylm_ci***
************

*Sumamos ingresos extraordinarios por trabajo dependiente

gen ingext=p544t/12
replace ingext=. if p544t==999999

egen ylm_ci=rsum(ylmpri_ci ylmsec_ci ingext)
replace ylm_ci=. if ylmpri_ci==. & ylmsec_ci==. & ingext==.


*************
***ylnm_ci***
*************

egen ylnm_ci=rsum(ylnmpri_ci ylnmsec_ci)
replace ylnm_ci=. if ylnmpri_ci==. & ylnmsec_ci==.


*************
***ynlm_ci***
*************

*Transferencias corrientes

forvalues i=1(1) 9 {

gen transl`i'=.
replace transl`i'=p556`i'c*30  if p556`i'b==1
replace transl`i'=p556`i'c*4.3 if p556`i'b==2
replace transl`i'=p556`i'c*2   if p556`i'b==3
replace transl`i'=p556`i'c     if p556`i'b==4
replace transl`i'=p556`i'c/2   if p556`i'b==5
replace transl`i'=p556`i'c/3   if p556`i'b==6
replace transl`i'=p556`i'c/6   if p556`i'b==7
replace transl`i'=p556`i'c/12  if p556`i'b==8
replace transl`i'=. if p556`i'c==999999
}

egen transltot=rsum(transl1 transl2 transl3 transl4 transl5 transl6 transl7 transl8 transl9)
replace transltot=. if transl1==. & transl2==. & transl3==. & transl4==. & transl5==. & transl6==. & transl7==. & transl8==.  & transl9==.


forvalues i=1(1) 9 {

gen transe`i'=.
replace transe`i'=p556`i'e*30  if p556`i'd==1
replace transe`i'=p556`i'e*4.3 if p556`i'd==2
replace transe`i'=p556`i'e*2   if p556`i'd==3
replace transe`i'=p556`i'e     if p556`i'd==4
replace transe`i'=p556`i'e/2   if p556`i'd==5
replace transe`i'=p556`i'e/3   if p556`i'd==6
replace transe`i'=p556`i'e/6   if p556`i'd==7
replace transe`i'=p556`i'e/12  if p556`i'd==8
replace transe`i'=. if p556`i'e==999999
}

egen transetot=rsum(transe1 transe2 transe3 transe4 transe5 transe6 transe7 transe8 transe9)
replace transetot=. if transe1==. & transe2==. & transe3==. & transe4==. & transe5==. & transe6==. & transe7==. & transe8==. & transe9==.

egen ynlm_ci=rsum(transltot transetot )
replace ynlm_ci=. if transltot==. & transetot==. 

**************
***ynlnm_ci***
**************

gen ynlnm_ci=.

****************
***remesas_ci***
****************

gen remesas_ci=transe3

************************
*** HOUSEHOLD INCOME ***
************************

*******************
*** nrylmpri_ch ***
*******************
*Creating a Flag label for those households where someone has a ylmpri_ci as missing

by idh_ch, sort: egen nrylmpri_ch=sum(nrylmpri_ci) if miembros_ci==1
replace nrylmpri_ch=1 if nrylmpri_ch>0 & nrylmpri_ch<.
replace nrylmpri_ch=. if nrylmpri_ch==.

**************
*** ylm_ch ***
**************

by idh_ch, sort: egen ylm_ch=sum(ylm_ci) if miembros_ci==1

****************
*** ylmnr_ch ***
****************

by idh_ch, sort: egen ylmnr_ch=sum(ylm_ci) if miembros_ci==1
replace ylmnr_ch=. if nrylmpri_ch==1

***************
*** ylnm_ch ***
***************

by idh_ch, sort: egen ylnm_ch=sum(ylnm_ci) if miembros_ci==1

*******************
*** remesas_ch ***
*******************

by idh_ch, sort: egen remesas_ch=sum(remesas_ci) if miembros_ci==1

***************
*** ynlm_ch ***
***************

by idh_ch, sort: egen ynlm_ch=sum(ynlm_ci) if miembros_ci==1

****************
*** ynlnm_ch ***
****************
gen ynlnm_ch=.

*******************
*** autocons_ci ***
*******************

gen p536_alt=p536
replace p536_alt=. if p536==999999

gen p543_alt=p543
replace p543_alt=. if p543==999999


egen autocons_ci=rsum(p536_alt p543_alt)
replace autocons_ci=. if p536_alt==. & p543_alt==.
replace autocons_ci=. if emp_ci~=1

drop p536_alt p543_alt


*******************
*** autocons_ch ***
*******************

by idh_ch, sort: egen autocons_ch=sum(autocons_ci) if miembros_ci==1

*******************
*** rentaimp_ch ***
*******************
gen rentaimp_ch=p106
replace rentaimp=. if p106==99999


*****************
***ylhopri_ci ***
*****************
gen ylmhopri_ci=ylmpri_ci/(horaspri_ci*4.3)


***************
***ylmho_ci ***
***************

gen ylmho_ci=ylm_ci/(horastot_ci*4.3)

***************
**ylmotros_ci**
***************

gen ylmotros_ci=.

****************
**ylnmotros_ci**
****************

gen ylnmotros_ci=.

***************
**tcylmpri_ci**
***************
gen tcylmpri_ci=.

***************
**tcylmpri_ch**
***************
gen tcylmpri_ch=.
}



*/
*==================================================================================================================================================================*
* Mayra Sáenz- Julio 2015: Se reemplazan los ingresos por los originales del instituto de estadística del país, de acuerdo a sintaxis elaborada por Marcos Robles.
*==================================================================================================================================================================*
*** Las variables son las mismas para 2004-2014 

**************
***ylmpri_ci***
***************
*Modificación SGR Julio 2020, se evita colocar ingreso 0 cuando las personas no reportan ingreso.
*recode i524e1 i530a i538e1 i541a d544t d556t1 d556t2 d557t d558t d529t d536 d540t d543 (.=0) (999999=0)
recode i524e1 i530a i538e1 i541a d544t d556t1 d556t2 d557t d558t d529t d536 d540t d543 (999999=.)
gen ypridbd = i524e1 /12
gen yprijbi = i530a  /12
gen ysecbd  = i538e1 /12
gen ysecjbi = i541a  /12
gen ingext  = d544t  /12

egen ylmpri_ci=rsum(ypridbd yprijbi ingext), missing


*******************
*** nrylmpri_ci ***
*******************

gen nrylmpri_ci=.


******************
*** ylnmpri_ci ***
******************

gen ylnmprid = d529t /12
gen ylnmprii = d536  /12
gen ylnmsecd = d540t /12
gen ylnmseci = d543  /12

egen ylnmpri_ci=rsum(ylnmprid ylnmprii), missing



***************
***ylmsec_ci***
***************
egen ylmsec_ci=rsum(ysecbd ysecjbi), missing


******************
****ylnmsec_ci****
******************

egen ylnmsec_ci=rsum(ylnmsecd ylnmseci), missing


************
***ylm_ci***
************
egen ylm_ci = rowtotal(ypridbd yprijbi ysecbd ysecjbi ingext), missing

*************
***ylnm_ci***
*************

egen ylnm_ci = rowtotal(ylnmprid ylnmprii ylnmsecd ylnmseci), missing


*************
***ynlm_ci***
*************

gen transltot = d556t1_e/12
gen transetot = d556t2/12
gen remesas_ci= d556t2/12 if p5563a==1
*Modificación SGR Julio 2020, se evita colocar ingreso 0 cuando las personas no reportan ingreso.
*recode remesas_ci (.=0)
gen d557t_1 = d557t/12 
gen d558t_1 = d558t/12
egen ynlm_ci  = rowtotal(transltot transetot d557t_1 d558t_1), missing  // d557t es renta de propiedad y d558t ing extraord

**************
***ynlnm_ci***
**************

gen vivialqimp = ia01hd /12
gen ynlnm_ci = (ig06hd+ig08hd+sig24+sig26+gru13hd1+gru13hd2+gru13hd3+gru23hd1+gru23hd2+gru23hd3+gru24hd+gru33hd1+gru33hd2+gru33hd3+(gru34hd-ga04hd)+gru43hd1+gru43hd2+gru43hd3+gru44hd+gru53hd1+gru53hd2+gru53hd3+gru54hd+gru63hd1+gru63hd2+gru63hd3+gru64hd+gru73hd1+gru73hd2+gru73hd3+gru74hd+gru83hd1+gru83hd2+gru83hd3+gru84hd+gru14hd3+gru14hd4+gru14hd5+sg42d+sg42d1+sg42d2+sg42d3) /(12 * nmiembros_ch)

********************
***Transferencias***
********************

*-Monetarias
* 2014
*Add SGR. Julio 2020.
replace d556t1=. if d556t1==999999

gen trac_pri = d556t1/12 if p5561a==1 | p5562a==1 | p5563a==1 | p5569a==1
*Modifiacion SGR 2017 Junio 
gen trac_pub = d556t1/12 if p5566a==1 | p5568a==1
*gen trac_pub = d556t1/12 if p5561a==6 | p5562a==8

*-No Monetarias
* 2004-2014: donaciones publicas y privadas
gen dona_pub = (gru13hd1+gru23hd1+gru33hd1+gru43hd1+gru53hd1+gru63hd1+gru73hd1+gru83hd1+gru14hd3) /(12 * mieperho)
gen dona_pri = ynlnm_ci - dona_pub

* TOTAL (las privadas incluyen transferencias del exterior)
egen trat_pri = rsum( trac_pri  dona_pri  transetot), missing
egen trat_pub = rsum( trac_pub  dona_pub), missing

****************
*Rentas y otros*
****************
egen rtasot = rsum(d557t_1  d558t_1), missing
label var rtasot "Rentas y otros"

************************
*** HOUSEHOLD INCOME ***
************************

*******************
*** nrylmpri_ch ***
*******************
*Creating a Flag label for those households where someone has a ylmpri_ci as missing

by idh_ch, sort: egen nrylmpri_ch=sum(nrylmpri_ci) if miembros_ci==1
replace nrylmpri_ch=1 if nrylmpri_ch>0 & nrylmpri_ch<.
replace nrylmpri_ch=. if nrylmpri_ch==.

**************
*** ylm_ch ***
**************

by idh_ch, sort: egen ylm_ch=sum(ylm_ci) if miembros_ci==1

****************
*** ylmnr_ch ***
****************

by idh_ch, sort: egen ylmnr_ch=sum(ylm_ci) if miembros_ci==1
replace ylmnr_ch=. if nrylmpri_ch==1

***************
*** ylnm_ch ***
***************

by idh_ch, sort: egen ylnm_ch=sum(ylnm_ci) if miembros_ci==1

*******************
*** remesas_ch ***
*******************

by idh_ch, sort: egen remesas_ch=sum(remesas_ci) if miembros_ci==1

***************
*** ynlm_ch ***
***************

by idh_ch, sort: egen ynlm_ch=sum(ynlm_ci) if miembros_ci==1

****************
*** ynlnm_ch ***
****************
by idh_ch, sort: egen ynlnm_ch=sum(ynlnm_ci) if miembros_ci==1

*******************
*** autocons_ci ***
*******************

gen p536_alt=p536
replace p536_alt=. if p536==999999

gen p543_alt=p543
replace p543_alt=. if p543==999999


egen autocons_ci=rsum(p536_alt p543_alt)
replace autocons_ci=. if p536_alt==. & p543_alt==.
replace autocons_ci=. if emp_ci~=1

drop p536_alt p543_alt


*******************
*** autocons_ch ***
*******************

by idh_ch, sort: egen autocons_ch=sum(autocons_ci) if miembros_ci==1

*******************
*** rentaimp_ch ***
*******************
*gen rentaimp_ch=p106
*replace rentaimp=. if p106==99999

*Modificación Mayra Sáenz - Julio 2015
gen rentaimp_ch= vivialqimp


*****************
***ylhopri_ci ***
*****************
gen ylmhopri_ci=ylmpri_ci/(horaspri_ci*4.3)


***************
***ylmho_ci ***
***************

gen ylmho_ci=ylm_ci/(horastot_ci*4.3)

***************
**ylmotros_ci**
***************

gen ylmotros_ci=.

****************
**ylnmotros_ci**
****************

gen ylnmotros_ci=.

***************
**tcylmpri_ci**
***************
gen tcylmpri_ci=.

***************
**tcylmpri_ch**
***************
gen tcylmpri_ch=.

******************
*Ingreso Nacional*
******************
gen yoficial_ch=gashog2d /12
label var yoficial_ch "Gasto del hogar total generado por el país"

gen ypeoficial_ch=yoficial_ch/ mieperho
label var yoficial_ch "Gasto per cápita generado por el país"

destring yoficial_ch ypeoficial_ch, replace

****************************
***VARIABLES DE EDUCACION***
****************************
replace p301a=. if p301a==99
replace p301b=. if p301b==99
replace p301c=. if p301c==99

/*Nota Importante del Manual de diligenciamiento de la ENAHO:
Si para el nivel primario (código 3 ó 4) el informante responde en grados, anote la información en
el recuadro GRADO y en el recuadro AÑO trace una diagonal y viceversa. Tenga en cuenta la
edad del informante, es decir a las personas menores de 35 años considérelos en grados .*/


*Los q responden con antiguo sistema de prima(p301b): habian cinco años de primaria.
gen byte aedu_ci=.
replace aedu_ci=0  if p301a==1 | p301a==2 // Sin nivel o educación inicial o prescolar
replace aedu_ci=p301b if p301a==3 & p301c==. 
*Como se responde actual sist prim (p301c): seis años de primaria.
replace aedu_ci=p301c if p301a==3 & p301b==0
replace aedu_ci=6  if p301a==4 
replace aedu_ci=6+p301b if p301a==5
replace aedu_ci=11 if p301a==6 
replace aedu_ci=11+p301b if p301a==7 
replace aedu_ci=11+p301b if p301a==8 
replace aedu_ci=11+p301b if p301a==9 
replace aedu_ci=11+p301b if p301a==10 
replace aedu_ci=16+p301b if p301a==11
replace aedu_ci=0 if p301a==12


**************
***eduno_ci***
**************

gen byte eduno_ci=(aedu_ci==0) 
replace eduno_ci=. if aedu_ci==.
label variable eduno_ci "Cero anios de educacion"

**************
***edupi_ci***
**************

gen byte edupi_ci=(aedu_ci>0 & aedu_ci<6)
replace edupi_ci=. if aedu_ci==.
label variable edupi_ci "Primaria incompleta"

**************
***edupc_ci***
**************

gen byte edupc_ci=(aedu_ci==6)
replace edupc_ci=. if aedu_ci==.
label variable edupc_ci "Primaria completa"

**************
***edusi_ci***
**************

gen byte edusi_ci=(aedu_ci>6 & aedu_ci<11)
replace edusi_ci=. if aedu_ci==.
label variable edusi_ci "Secundaria incompleta"

**************
***edusc_ci***
**************

gen byte edusc_ci=(aedu_ci==11)
replace edusc_ci=. if aedu_ci==.
label variable edusc_ci "Secundaria completa"

***************
***edus1i_ci***
***************

gen byte edus1i_ci=(aedu_ci>6 & aedu_ci<9)
replace edus1i_ci=. if aedu_ci==.
label variable edus1i_ci "1er ciclo de la secundaria incompleto"

***************
***edus1c_ci***
***************

gen byte edus1c_ci=(aedu_ci==9)
replace edus1c_ci=. if aedu_ci==.
label variable edus1c_ci "1er ciclo de la secundaria completo"

***************
***edus2i_ci***
***************

gen byte edus2i_ci=(aedu_ci==10)
replace edus2i_ci=. if aedu_ci==.
label variable edus2i_ci "2do ciclo de la secundaria incompleto"

***************
***edus2c_ci***
***************

gen byte edus2c_ci=(aedu_ci==11)
replace edus2c_ci=. if aedu_ci==.
label variable edus2c_ci "2do ciclo de la secundaria completo"

**************
***eduui_ci***
**************

gen byte eduui_ci=aedu_ci>=12 & (p301a==7 | p301a==9)
replace eduui_ci=. if aedu_ci==.
label variable eduui_ci "Universitaria incompleta"

***************
***eduuc_ci***
***************

gen byte eduuc_ci=(aedu_ci>=12) & (p301a==8 | p301a==10 | p301a==11)
replace eduuc_ci=. if aedu_ci==.
label variable eduuc_ci "Universitaria completa o mas"


***************
***edupre_ci***
***************

gen byte edupre_ci=.
label variable edupre_ci "Educacion preescolar"

****************
***asispre_ci***
****************
g asispre_ci= p308a==1  // matriculado en nivel inicial (sin edad)
replace asispre_ci=0 if p307==2 & p313!=6 // matriculado pero no asiste (y no por vacaciones)
la var asispre_ci "Asiste a educacion prescolar"
	
**************
***eduac_ci***
**************
gen byte eduac_ci=.
replace eduac_ci=1 if (p301a==9 | p301a==10 | p301a==11)
replace eduac_ci=0 if (p301a==7 | p301a==8)
label variable eduac_ci "Superior universitario vs superior no universitario"

***************
***asiste_ci***
***************
/*Se considera la variable de matricula y de asistencia, codificando como 1 a los que estan matriculados y no asisten por vacaciones*/
g asiste_ci = p306==1 // matriculados 
replace asiste_ci=0 if p307==2 & p313!=6
label variable asiste_ci "Asiste actualmente a la escuela"

**************
***pqnoasis_ci***
**************

gen pqnoasis_ci=p313 if asiste_ci==0


**************
*pqnoasis1_ci*
**************
**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**

g       pqnoasis1_ci = 1 if p313==1
replace pqnoasis1_ci = 2 if p313==2
replace pqnoasis1_ci = 3 if p313==5
replace pqnoasis1_ci = 4 if p313==9
replace pqnoasis1_ci = 5 if p313==10
replace pqnoasis1_ci = 6 if p313==3
replace pqnoasis1_ci = 7 if p313==4
replace pqnoasis1_ci = 8 if p313==7
replace pqnoasis1_ci = 9 if p313==11
replace pqnoasis1_ci = . if asiste_ci==1
label define pqnoasis1_ci 1 "Problemas económicos" 2 "Por trabajo" 3 "Problemas familiares o de salud" 4 "Falta de interés" 5	"Quehaceres domésticos/embarazo/cuidado de niños/as" 6 "Terminó sus estudios" 7	"Edad" 8 "Problemas de acceso"  9 "Otros"
label value  pqnoasis1_ci pqnoasis1_ci

***************
***repite_ci***
***************

gen repite_ci=.

***************
***repiteult_ci***
***************

gen repiteult_ci=.

***************
***edupub_ci***
***************
gen edupub_ci=.
replace edupub_ci=1 if (p308d==1) & asiste_ci==1
replace edupub_ci=0 if (p308d==2) & asiste_ci==1

**********************************
**** VARIABLES DE LA VIVIENDA ****
**********************************

gen aguadist_ch=.
replace aguadist_ch= 1 if p110==1 
replace aguadist_ch= 2 if p110==2
replace aguadist_ch= 3 if p110 == 3 | p110 == 6
replace aguadist_ch= 0 if p110==4 | p110==5
label var aguadist_ch "Ubicación de la principal fuente de agua"
label def aguadist_ch 1"Dentro de la vivienda" 2"Fuera de la vivienda pero en el terreno"
label def aguadist_ch 3"Fuera de la vivienda y del terreno", add
label val aguadist_ch aguadist_ch


*****************
*aguafconsumo_ch*
*****************

gen aguafconsumo_ch = 0
replace aguafconsumo_ch = 0 if p110a1==2
replace aguafconsumo_ch = 7 if p110a1==1
replace aguafconsumo_ch = 1 if (p110==1 |p110==2) & p110a1==1
replace aguafconsumo_ch = 2 if p110a1==1 & p110==3
replace aguafconsumo_ch = 6 if p110a1==1 & p110==4
replace aguafconsumo_ch = 8 if p110a1==1 & p110==8
replace aguafconsumo_ch = 10 if p110a1==1 & (p110==5|  p110==6 |p110==7)

*****************
*aguafuente_ch*
*****************

gen aguafuente_ch =.
replace aguafuente_ch = 7 if p110a1==1
replace aguafuente_ch = 1 if (p110==1|p110==2) 
replace aguafuente_ch = 2 if p110==3
replace aguafuente_ch = 6 if p110==4
replace aguafuente_ch = 8 if p110==8 
replace aguafuente_ch = 10 if (p110==5 |p110==7| p110==6)


**************
*aguadisp1_ch*
**************
gen aguadisp1_ch = 9
label var aguadisp1 "= 9 la encuesta no pregunta si el servicio de agua es constante"

**************
*aguadisp2_ch*
**************

gen aguadisp2_ch =.
replace aguadisp2_ch = 1 if (p110c2<4 | p110c1 < 12 | p110c3 <12) 
replace aguadisp2_ch = 2 if p110c2>=4 & (p110c1>=12 | p110c3 <12)
replace aguadisp2_ch = 3 if p110c==1 & p110c1 == 24


************
*sinbano_ch*
************
gen sinbano_ch = .


*************
*aguatrat_ch*
*************
gen aguatrat_ch = 9
*label var aguatrat_ch "= 9 la encuesta no pregunta de si se trata el agua antes de consumirla"


*************
*aguamala_ch*  Altered
*************
gen aguamala_ch = 2
replace aguamala_ch = 0 if aguafuente_ch<=7
replace aguamala_ch = 1 if aguafuente_ch>7 & aguafuente_ch!=10
*label var aguamala_ch "= 1 si la fuente de agua no es mejorada"

*****************
*aguamejorada_ch*  Altered
*****************
gen aguamejorada_ch = 2
replace aguamejorada_ch = 0 if aguafuente_ch>7 & aguafuente_ch!=10
replace aguamejorada_ch = 1 if aguafuente_ch<=7



*****************
*bano_ch         *  Altered
*****************

gen bano_ch=0
replace bano_ch=1 if (p111a==1|p111a==2)
replace bano_ch = 2 if p111a==4
replace bano_ch=3 if p111a==5
replace bano_ch=4 if (p111a==6|p111a==8)
replace bano_ch = 6 if (p111a == 3 | p111a ==7)



*****************
*banomejorado_ch*  Altered
*****************
gen banomejorado_ch= 2
replace banomejorado_ch =1 if bano_ch<=3 & bano_ch!=0
replace banomejorado_ch =0 if (bano_ch ==0 | bano_ch>=4) & bano_ch!=6

**#		
****************
***aguared_ch***
****************

gen aguared_ch=0
replace aguared_ch=1 if (p110==1 | p110==2)
label var aguared_ch "Acceso a fuente de agua por red"


/*
p110:
           1 red pública, dentro de la vivienda
           2 red pública, fuera de la vivienda pero dentro del edificio
           3 pilón de uso público
           4 camión - cisterna u otro similar
           5 pozo
           6 río, acequia, manantial o similar
           7 otra
*/




*****************
***aguamide_ch***
*****************

gen aguamide_ch=.

************
***luz_ch***
************

gen luz_ch=p1121

****************
***luzmide_ch***
****************

gen luzmide_ch=1 if p112a ==1 | p112a ==2
replace luzmide_ch=0 if p112a==3

****************
***combust_ch***
****************
gen combust_ch=1 if p113a==1 | p113a==2 | p113a==3
replace combust_ch=0 if p113a==5 | p113a==6 | p113a==7 | p113a==4


/*
p111:
¿Red pública de desagüe dentro de la vivienda? .................................................................. 1
¿Red pública de desagüe fuera de la vivienda pero dentro del edificio? ........................................... 2
¿Letrina? .................................................................. 3
¿Pozo séptico? ......................................................... 4
¿Pozo ciego o negro?............................................... 5
¿Río, acequia o canal? ............................................. 6
¿Otra? __________________________________ 7
NO TIENE ................................................................. 8
*/
***************
***banoex_ch***
***************
gen banoex_ch=9

*************
***des1_ch***
*************

* MGR Aug, 2015: corrección de sintaxis
/*
gen des1_ch=0 if p111==8
replace des1_ch=1 if p111>=1 & p111<=2
replace des1_ch=2 if p111>=3 & p111<=5
replace des1_ch=3 if p111>=6 & p111<=7 /*incluye otra */
label var des1_ch "Tipo de desague según unimproved de MDG"
label def des1_ch 0"No tiene servicio sanitario" 1"Conectado a red general o cámara séptica"
label def des1_ch 2"Letrina o conectado a pozo ciego" 3"Desemboca en río o calle", add
label val des1_ch des1_ch
*/
/*
gen des1_ch=0 if p111==8
replace des1_ch=1 if p111==1 | p111==2 | p111==4 
replace des1_ch=2 if p111==3 | p111==5
replace des1_ch=3 if p111>=6 & p111<=7 /*incluye otra */
label var des1_ch "Tipo de desague según unimproved de MDG"
label def des1_ch 0"No tiene servicio sanitario" 1"Conectado a red general o cámara séptica"
label def des1_ch 2"Letrina o conectado a pozo ciego" 3"Desemboca en río o calle", add
label val des1_ch des1_ch
*/
*Corrección SGR. Julio 2020*
gen des1_ch=0 if p111==9
replace des1_ch=1 if p111==1 | p111==2 | p111==4 
replace des1_ch=2 if p111==3 | p111==5
replace des1_ch=3 if p111>=6 & p111<=7 /*incluye otra */
label var des1_ch "Tipo de desague según unimproved de MDG"
label def des1_ch 0"No tiene servicio sanitario" 1"Conectado a red general o cámara séptica"
label def des1_ch 2"Letrina o conectado a pozo ciego" 3"Desemboca en río o calle", add
label val des1_ch des1_ch

	*************
	***des2_ch***
	*************

*Primera línea cambio SGR Julio 2020*
gen des2_ch=0 if p111==9
replace des2_ch=1 if p111>=1 & p111<=5
replace des2_ch=2 if p111>=6 & p111<=7
label var des2_ch "Tipo de desague sin incluir definición MDG"
label def des2_ch 0"No tiene servicio sanitario" 1"Conectado a red general, cámara séptica, pozo o letrina"
label def des2_ch 2"Cualquier otro caso", add
label val des2_ch des2_ch
	
	*************
	***piso_ch***
	*************
gen piso_ch=0 if p103==6
replace piso_ch=1 if p103>=1 & p103<=5
replace piso_ch=2 if p103==7
label var piso_ch "Materiales de construcción del piso"  
label def piso_ch 0"Piso de tierra" 1"Materiales permanentes" 2"Otros materiales"
label val piso_ch piso_ch

/*
p103:el material predominante en los pisos
           1 parquet o madera pulida
           2 láminas asfálticas, vinílicos o similares
           3 losetas, terrazos o similares
           4 madera (entablados)
           5 cemento
           6 tierra
           7 otro material
*/

	**************
	***pared_ch***
	**************
	
gen pared_ch=0 if p102==3 | p102==4
replace pared_ch=1 if p102==1 | p102==2 | p102==5 | p102==6 | p102==7
replace pared_ch=2 if p102>=8

/*
p102:el material predominante en las paredes

           1 ladrillo o bloque de cemento
           2 piedra o sillar con cal o cemento
           3 adobe
           4 tapia
           5 quincha (caña con barro)
           6 piedra con barro
           7 madera
           8 estera
           9 otro material
*/

	**************
	***techo_ch***
	**************
	
gen techo_ch=0 if p103a>=5 & p103a<=7
replace techo_ch=1 if p103a>=1 & p103a<=4
replace techo_ch=2 if p103a==8

/*p103a:el material predominante en los techos
           1 concreto armado
           2 madera
           3 tejas
           4 planchas de calamina, fibra de cemento o similares
           5 caña o estera con torta de barro
           6 estera
           7 paja, hojas de palmera
           8 otro material
*/
   	**************
	***resid_ch***
	**************
	
gen resid_ch=.
/*NA*/


	

		


	*************
	***dorm_ch***
	*************

* MGR: se imputa 1 a aquellos hogares que indican tener 0 habitaciones exclusivas para dormir
gen dorm_ch=p104a
replace dorm_ch=1 if p104a==0
	
	****************
	***cuartos_ch***
	****************

gen cuartos_ch=p104

	***************
	***cocina_ch***
	***************
	gen cocina_ch=.
	label var cocina_ch "Cuarto separado y exclusivo para cocinar"
/*NA*/

	**************
	***telef_ch***
	**************
gen telef_ch=(p1141==1)
label var telef_ch "El hogar tiene servicio telefónico fijo"

	***************
	***refrig_ch***
	***************
	
gen refrig_ch=(p61212==1)	
	**************
	***freez_ch***
	**************
gen freez_ch=.

	*************
	***auto_ch***
	*************
gen auto_ch =(p61217==1)

	**************
	***compu_ch***
	**************
gen compu_ch=(p6127==1)

	*****************
	***internet_ch***
	*****************

gen internet_ch=(p1144==1)

	************
	***cel_ch***
	************
gen cel_ch=(p1142==1)

	**************
	***vivi1_ch***
	**************
gen vivi1_ch=1 if p101==1
replace vivi1_ch=2 if p101==2
replace vivi1_ch=3 if p101>2 & p101!=.

label var vivi1_ch "Tipo de vivienda en la que reside el hogar"
label def vivi1_ch 1"Casa" 2"Departamento" 3"Otros"
label val vivi1_ch vivi1_ch

/*p101:
           1 casa independiente
           2 departamento en edificio
           3 vivienda en quinta
           4 vivienda en casa de vecindad (callejón, solar o corralón)
           5 choza o cabaña
           6 vivienda improvisada
           7 local no destinado para habitación humana
           8 otro
*/

	**************
	***vivi2_ch***
	**************
	
gen vivi2_ch=(p101<=2)
replace vivi2_ch=. if p101==. 
label var vivi2_ch "La vivienda es casa o departamento"
	
	*****************
	***viviprop_ch***
	*****************

gen viviprop_ch=0 if p105a==1
replace viviprop_ch=1 if p105a==2
replace viviprop_ch=2 if p105a==4
replace viviprop_ch=3 if p105a==3 | (p105a>4 & p105a!=.) 
label define viviprop_ch 0 "Alquilada" 1 "Propia y totalmente pagada" 2 "Propia y en proceso de pago" 3 "Ocupada (propia de facto)"
label value viviprop_ch viviprop_ch

/*
p105a:
           1 alquilada
           2 propia, totalmente pagada
           3 propia, por invasión
           4 propia, comprándola a plazos
           5 cedida por el centro de trabajo
           6 cedida por otro hogar o institución
           7 otra forma
*/
	
	****************
	***vivitit_ch***
	****************
	
gen vivitit_ch=(p106a==1)
label var vivitit_ch "El hogar posee un título de propiedad"
	
	****************
	***vivialq_ch***
	****************
*Add SGR.
replace p105b=. if p105b==99999
gen vivialq_ch=p105b if viviprop_ch==0
label var vivialq_ch "Alquiler mensual"

	*******************
	***vivialqimp_ch***
	*******************
	
*gen vivialqimp_ch=p106
*replace vivialqimp_ch=. if p106==99999

*Modificación Mayra Sáenz - Julio 2015
gen vivialqimp_ch= vivialqimp
label var vivialqimp_ch "Alquiler mensual imputado"

*******************
***  seguro_ci  ***
*******************

g benefdes_ci=0 if desemp_ci==1
replace benefdes_ci=1 if  p5585a==1  & desemp_ci==1
label var benefdes_ci "=1 si tiene seguro de desempleo"

*******************
*** yseguro_ci  ***
*******************
*Add SGR. Julio 2020*
replace p5585b=. if p5585b==999999
g ybenefdes_ci=p5585b if  benefdes_ci==1
label var ybenefdes_ci "Monto de seguro de desempleo"

*******************
*** SALUD  ***
*******************

*******************
*** cobsalud_ci ***
*******************

gen cobsalud_ci=.
replace cobsalud_ci=0 if p4191==2 & p4192==2 & p4193==2 & p4194==2 & p4195==2 & p4196==2 &  p4197==2 & p4198==2
replace cobsalud_ci=1 if p4191==1 | p4192==1 |  p4193==1 | p4194==1 | p4195==1 |  p4196==1 | p4197==1 | 4198==1

label var cobsalud_ci "Tiene cobertura de salud"
label define cobsalud_ci 0 "No" 1 "Si" 
label value cobsalud_ci cobsalud_ci

************************
*** tipocobsalud_ci  ***
************************

gen tipocobsalud_ci=.
replace tipocobsalud_ci=0 if cobsalud_ci==0
replace tipocobsalud_ci=1 if p4191==1
replace tipocobsalud_ci=2 if p4192==1
replace tipocobsalud_ci=3 if p4193==1
replace tipocobsalud_ci=4 if p4194==1
replace tipocobsalud_ci=5 if p4195==1
replace tipocobsalud_ci=6 if p4196==1
replace tipocobsalud_ci=7 if p4197==1
replace tipocobsalud_ci=8 if p4198==1

label var tipocobsalud_ci "Tipo cobertura de salud"
lab def tipocobsalud_ci 0"Sin cobertura" 1"essalud" 2"Privado" 3"entidad prestadora" 4"policiales" 5"sis" 6"universitario" 7"escolar privado" 8"otro" 
lab val tipocobsalud_ci tipocobsalud_ci


*********************
*** probsalud_ci  ***
*********************
* Nota: se pregunta si tuvieron problemas de salud en últimas 4 semanas. 
gen probsalud_ci=1 if p4021==1 | p4022==1 | p4023==1 | p4024==1 
replace probsalud_ci=0 if p4025==1

label var probsalud_ci "Tuvo algún problema de salud en los ultimos días"
lab def probsalud_ci 0 "No" 1 "Si"
lab val probsalud_ci probsalud_ci


*********************
*** distancia_ci  ***
*********************
gen distancia_ci=0 if p4092==0
replace distancia_ci=1 if p4092==1

label var distancia_ci "Dificultad de acceso a salud por distancia"
lab def distancia_ci 0 "No" 1 "Si"
lab val distancia_ci distancia_ci

*****************
*** costo_ci  ***
*****************
gen costo_ci=0 if p4091==0
replace costo_ci=1 if p4091==1

label var costo_ci "Dificultad de acceso a salud por costo"
lab def costo_ci 0 "No" 1 "Si"
lab val costo_ci costo_ci

********************
*** atencion_ci  ***
********************
gen atencion_ci=0 if p4093==0 
replace atencion_ci=1 if p4093==1

label var atencion_ci "Dificultad de acceso a salud por problemas de atencion"
lab def atencion_ci 0 "No" 1 "Si"
lab val atencion_ci atencion_ci


******************************
*** VARIABLES DE MIGRACION ***
******************************

* Variables incluidas por SCL/MIG Fernando Morales

	*******************
	*** migrante_ci ***
	*******************
	
	gen migrante_ci=(p401g2<10000) if p401g2!=. & p401g2!=999999
	label var migrante_ci "=1 si es migrante"
	
	**********************
	*** migantiguo5_ci ***
	**********************
	
	gen migantiguo5_ci=(migrante_ci==1 & (p401f==1 | (p401g>10000 & p401g!=.))) if migrante_ci!=. & p401f!=3 & p401g!=999999 & p401f!=. & !inrange(edad_ci,0,4)
	label var migantiguo5_ci "=1 si es migrante antiguo (5 anos o mas)"
		
	**********************
	*** migrantelac_ci ***
	**********************
	
	gen migrantelac_ci=(inlist(p401g2,4002,4003,4004,4005,4006,4007,4009,4010,4011,4014,4015,4018,4019,4021,4022,4023,4024,4025,4026,4027,4030,4034,4035,4036,4037) & migrante_ci==1) if migrante_ci!=.
	label var migrantelac_ci "=1 si es migrante proveniente de un pais LAC"
	** Fuente: Los codigos de paises se obtiene del censo de peru (redatam)
	
	**********************
	*** migrantiguo5_ci ***
	**********************
	
	gen migrantiguo5_ci=(migrante_ci==1 & (p401f==1 | (p401g>10000 & p401g!=.))) if migrante_ci!=. & p401f!=3 & p401g!=999999 & p401f!=. & !inrange(edad_ci,0,4)
	replace migrantiguo5_ci = 0 if migrantiguo5_ci != 1 & migrante_ci == 1
	replace migrantiguo5_ci = . if migrante_ci == 0 | p401f==3 & p401g==999999
	label var migrantiguo5_ci "=1 si es migrante antiguo (5 anos o mas)"
		
	**********************
	*** miglac_ci ***
	**********************
	
	gen miglac_ci=(inlist(p401g2,4002,4003,4004,4005,4006,4007,4009,4010,4011,4014,4015,4018,4019,4021,4022,4023,4024,4025,4026,4027,4030,4034,4035,4036,4037) & migrante_ci==1) if migrante_ci!=.
	replace miglac_ci = 0 if migrantelac_ci != 1  & migrante_ci == 1
	replace miglac_ci = . if migrante_ci == 0
	label var miglac_ci "=1 si es migrante proveniente de un pais LAC"
	** Fuente: Los codigos de paises se obtiene del censo de peru (redatam)



******************************
* Variables SPH - PMTC y PNC *
******************************

* PTMC:  Juntos (ingtpu01)
* PNC: 	 Pensión 65 (ingtpu03)


egen ingreso_total = rowtotal(ylm_ci ylnm_ci ynlm_ci ynlnm_ci), missing
bys idh_ch: egen y_hog = sum(ingreso_total)

* Beneficiarios 
gen ptmc_ci=(ingtpu01>0 & ingtpu01!=.)
bys idh_ch: egen ptmc_ch=max(ptmc_ci)

gen pnc_ci=(ingtpu03>0 & ingtpu03!=.)

* Montos
gen ing_ptmc=ingtpu01/12
gen ing_pension=ingtpu03/12

gen y_pc     = y_hog / nmiembros_ch 
gen y_pc_net = (y_hog - ing_ptmc - ing_pension) / nmiembros_ch

* Adultos mayores 
gen mayor64_ci=(edad>64 & edad!=.)


lab def ptmc_ch 1 "Beneficiario PTMC" 0 "No beneficiario PTMC"
lab val ptmc_ch ptmc_ch

lab def pnc_ci 1 "Beneficiario PNC" 0 "No beneficiario PNC"
lab val pnc_ci pnc_ci


	
/*_____________________________________________________________________________________________________*/
* Asignación de etiquetas e inserción de variables externas: tipo de cambio, Indice de Precios al 
* Consumidor (2011=100), líneas de pobreza
/*_____________________________________________________________________________________________________*/


do "$gitFolder\armonizacion_microdatos_encuestas_hogares_scl\_DOCS\\Labels&ExternalVars_Harmonized_DataBank.do"

*_____________________________________________________________________________________________________*

*  Pobres extremos, pobres moderados, vulnerables y no pobres 
* con base en ingreso neto (Sin transferencias)
* y líneas de pobreza internacionales
gen     grupo_int = 1 if (y_pc_net<lp31_ci)
replace grupo_int = 2 if (y_pc_net>=lp31_ci & y_pc_net<(lp31_ci*1.6))
replace grupo_int = 3 if (y_pc_net>=(lp31_ci*1.6) & y_pc_net<(lp31_ci*4))
replace grupo_int = 4 if (y_pc_net>=(lp31_ci*4) & y_pc_net<.)

tab grupo_int, gen(gpo_ingneto)

* Crear interacción entre recibirla la PTMC y el gpo de ingreso
gen ptmc_ingneto1 = 0
replace ptmc_ingneto1 = 1 if ptmc_ch == 1 & gpo_ingneto1 == 1

gen ptmc_ingneto2 = 0
replace ptmc_ingneto2 = 1 if ptmc_ch == 1 & gpo_ingneto2 == 1

gen ptmc_ingneto3 = 0
replace ptmc_ingneto3 = 1 if ptmc_ch == 1 & gpo_ingneto3 == 1

gen ptmc_ingneto4 = 0
replace ptmc_ingneto4 = 1 if ptmc_ch == 1 & gpo_ingneto4 == 1

lab def grupo_int 1 "Pobre extremo" 2 "Pobre moderado" 3 "Vulnerable" 4 "No pobre"
lab val grupo_int grupo_int



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
salmm_ci tc_c ipc_c lp19_c lp31_c lp5_c lp_ci lpe_ci aedu_ci eduno_ci edupi_ci edupc_ci	edusi_ci edusc_ci eduui_ci eduuc_ci	edus1i_ci ///
edus1c_ci edus2i_ci edus2c_ci edupre_ci eduac_ci asiste_ci pqnoasis_ci pqnoasis1_ci	repite_ci repiteult_ci edupub_ci ///
aguared_ch aguafconsumo_ch aguafuente_ch aguadist_ch aguadisp1_ch aguadisp2_ch aguamala_ch aguamejorada_ch aguamide_ch bano_ch banoex_ch banomejorado_ch sinbano_ch aguatrat_ch luz_ch luzmide_ch combust_ch des1_ch des2_ch piso_ch  ///
pared_ch techo_ch resid_ch dorm_ch cuartos_ch cocina_ch telef_ch refrig_ch freez_ch auto_ch compu_ch internet_ch cel_ch ///
vivi1_ch vivi2_ch viviprop_ch vivitit_ch vivialq_ch	vivialqimp_ch migrante_ci migantiguo5_ci migrantelac_ci, first

/*Homologar nombre del identificador de ocupaciones (isco, ciuo, etc.) y de industrias y dejarlo en base armonizada 
para análisis de trends (en el marco de estudios sobre el futuro del trabajo)*/
clonevar codocupa = p505r4
clonevar codindustria = p506r4


compress

saveold "`base_out'", version(12) replace
