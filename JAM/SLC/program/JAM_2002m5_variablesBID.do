/*Modificado por Analia: April 19, 2006
The following line:
egen remesas=rsum(k02702 k02703)
gen remesas_ci=remesas/(365/30)
was replaced by:
gen remesas_ci=k02703/(365/30)
Since k02702 is money received from relatives WHO LIVE IN Jamaica!
*/

/* Revision 15 Set 2006, Victoria
Se decide dividir el factor de expansion entre 1000000 para evitar problemas
al momento de utilizar el comando "weight" 
*/


* (Versi??tata 12)
clear
set more off
*________________________________________________________________________________________________________________*

 * Activar si es necesario (dejar desactivado para evitar sobreescribir la base y dejar la posibilidad de 
 * utilizar un loop)
 * Los datos se obtienen de las carpetas que se encuentran en el servidor: ${surveysFolder}
 * Se tiene acceso al servidor ??amente al interior del BID.
 * El servidor contiene las bases de datos MECOVI.
 *________________________________________________________________________________________________________________*
 

global ruta = "${surveysFolder}"

local PAIS JAM
local ENCUESTA SLC
local ANO "2002"
local ronda m5 

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
*local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\\`PAIS'_`ANO'`ronda'.dta"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"


capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Jamaica
Encuesta: SLC
Round: a
Autores: Mayra Sáenz E-mail: saenzmayra.a@gmail.com - mayras@iadb.org
Versiones anteriores: Yessenia Loayza
Fecha última modificación: 17 de noviembre de 2016

							SCL/LMK - IADB
****************************************************************************/
****************************************************************************/



use `base_in', clear


gen str pais_c="JAM"

gen anio_c=2002

gen mes_c=5 
label var mes_c "Mes de la Encuesta: Mayo de 2002"
label define mes_c 5 "MAY"
label values mes_c mes_c

/** VARIABLE AREA 
1 = Kingston
2 = Other towns
3 = Rural areas
4 = Portmore (se incluye en Kingston Metropolitan Area)
5 = Spanish town (se incluye en Kingston Metropolitan Area)
*/

gen zona_c=.
replace zona_c=1 if area==1 | area==2 | area==4 | area==5
replace zona_c=0 if area==3
label define zona_c 0 "Rural" 1 "Urbana"
label value zona_c zona_c

egen idh_ch=group(serial)
label var idh_ch "Identificador Unico del Hogar"
gen idp_ci=ind
label var idp_ci "Identificador Individual dentro del Hogar"


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

/* It is not a “weighting factor” but a “non response factor”. It means that when the weight command is applied, the proportions found are right but absolute numbers cannot be derived. */

gen factor2_ci=finwght*100000000 
label var factor2_ci "Factor de Expansion del Individuo (no suma la poblacion)"

gen factor_ch=finwght
label var factor_ch "Factor de expansion del Hogar"

** POBLACION DA JAMAICA AL 2002 = 2,624,700 **
* Se reajustan los ponderadores:
sum finwght
scalar pob=r(sum)
gen pop=finwght*(2624700/pob)

/*
THIS 1000000 CAUSES PROBLEMS WHEN WEIGHTING SO IT WILL BE DROPPED 
sum pop
ret list
gen factor_ci=pop*1000000  Se debe dividir la poblacion por 1000000 
drop pop
label var factor_ci "Factor de Expansion del Individuo (Se debe dividir la poblacion por 1000000)"
*/

sum pop
ret list
gen factor_ci=pop 
drop pop
label var factor_ci "Factor de Expansion del Individuo"



gen sexo_ci=1 if sex==1
replace sexo_ci=2 if sex==2
label var sexo_ci "Sexo del Individuo"
label define sexo_ci 1 "Masculino" 2 "Femenino"
label value sexo_ci sexo_ci

** Generating Edad
gen edad_ci=age
label var edad_ci "Edad del Individuo"

gen byte civil_ci=.
replace civil_ci=1 if  marital==2
replace civil_ci=2 if  marital==1
replace civil_ci=3 if  marital==3 | marital==4
replace civil_ci=4 if  marital==5
label var civil_ci "Estado Civil"
label define civil_ci 1 "Soltero" 2 "Union Formal o Informal" 3 "Divorciado o Separado" 4 "Viudo"
label value civil_ci civil_ci

gen jefe_ci=(relatn==1)
label var jefe_ci "Jefe de Hogar Declarado"

sort idh

egen byte nconyuges_ch=sum(relacion_ci==2), by (idh)
label variable nconyuges_ch "Numero de Conyuges"

egen byte nhijos_ch=sum(relacion_ci==3), by (idh)
label variable nhijos_ch "Numero de Hijos"

egen byte notropari_ch=sum(relacion_ci==4),by (idh)
label variable notropari_ch "Numero de Otros Parientes "

egen byte notronopari_ch=sum(relacion_ci==5), by (idh)
label variable notronopari_ch "Numero de Otros NO Parientes "

egen byte nempdom_ch=sum(relacion_ci==6), by (idh)
label variable nempdom_ch "Numero de Empleados Domesticos"

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

sort idh idp
egen nmiembros_ch=sum(relacion_ci>0 & relacion_ci<=5), by (idh)
replace nmiembros_ch=. if relacion_ci==.
label variable nmiembros_ch "Numero de miembros del Hogar"

gen miembros_ci=.
replace miembros_ci=1 if relacion_ci>=1 & relacion_ci<=5
replace miembros_ci=0 if relacion_ci==6 /*Empleados domesticos y sus familiares */
label variable miembros_ci "Variable dummy que indica las personas que son miembros del Hogar"

egen nmayor21_ch=sum((relacion_ci>0 & relacion_ci<=5) & (edad_ci>=21)), by (idh)
label variable nmayor21_ch "Numero de personas de 21 años o mas dentro del Hogar"

egen nmenor21_ch=sum((relacion_ci>0 & relacion_ci<=5) & (edad_ci<21)), by (idh)
label variable nmenor21_ch "Numero de personas menores a 21 años dentro del Hogar"

egen nmayor65_ch=sum((relacion_ci>0 & relacion_ci<=5) & (edad_ci>=65)), by (idh)
label variable nmayor65_ch "Numero de personas de 65 años o mas dentro del Hogar"

egen nmenor6_ch=sum((relacion_ci>0 & relacion_ci<=5) & (edad_ci<6)), by (idh)
label variable nmenor6_ch "Numero de niños menores a 6 años dentro del Hogar"

egen nmenor1_ch=sum((relacion_ci>0 & relacion_ci<=5) & (edad_ci<1)),  by (idh)
label variable nmenor1_ch "Numero de niños menores a 1 año dentro del Hogar"


**** Demanda Laboral
** con la variable l01 de JSLC **
capture drop ocupa2_ci 
gen ocupa2_ci=.
replace ocupa2_ci=1 if l01>=2000 & l01<=3999
replace ocupa2_ci=2 if l01>=1000 & l01<=1999
replace ocupa2_ci=3 if l01>=4000 & l01<4999
replace ocupa2_ci=4 if l01>=5200 & l01<5999
replace ocupa2_ci=5 if l01>=5000 & l01<=5199
replace ocupa2_ci=6 if l01>=6000 & l01<=6999
replace ocupa2_ci=7 if l01>=7000 & l01<=8999
replace ocupa2_ci=8 if l01>0 & l01<=999 
replace ocupa2_ci=9 if l01>=9000 & l01<9999

label var ocupa2_ci "Ocupacion Laboral en la Actividad Principal"
label define ocupa2_ci 1 "PROFESIONALES Y TECNICOS" 2 "GERENTES, DIRECTORES Y FUNCIONARIOS SUPERIORES"  3 "PERSONAL ADMINISTRATIVO Y NIVEL INTERMEDIO" 4 "COMERCIANTES Y VENDEDORES" 5 "TRABAJADORES EN SERVICIOS" 6 "TRABAJADORES AGRICOLAS Y AFINES" 7 "OBREROS NO AGRICOLAS, CONDUCTORES DE MAQUINAS Y VEHICULOS DE TRANSPORTE Y SIMILARES" 8 "FUERZAS ARMADAS" 9 "OTRAS OCUPACIONES NO CLASIFICADAS ANTERIORMENTE"
label values ocupa2_ci ocupa2_ci

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

gen byte rama_ci=.
replace rama_ci=1 if Q39M>0 & Q39M<1000
replace rama_ci=2 if Q39M>=1000 & Q39M<2000
replace rama_ci=3 if Q39M>=2000 & Q39M<4000
replace rama_ci=4 if Q39M>=4000 & Q39M<5000
replace rama_ci=5 if Q39M>=5000 & Q39M<6000
replace rama_ci=6 if Q39M>=6000 & Q39M<7000
replace rama_ci=7 if Q39M>=7000 & Q39M<8000
replace rama_ci=8 if Q39M>=8000 & Q39M<9000
replace rama_ci=9 if Q39M>=9000 & Q39M<10000

label var rama_ci "Rama Laboral en la Ocupacion Principal"
label define rama_ci 1 "Agricultura, caza, silvicultura y pesca" 2 "Explotación de minas y canteras" 3 "Industrias manufactureras" 4 "Electricidad, gas y agua" 5 "Construcción" 6 "Comercio al por mayor y menor, restaurantes, hoteles" 7 "Transporte y almacenamiento" 8 "Establecimientos financieros, seguros, bienes inmuebles" 9 "Servicios sociales, comunales y personales"
label values rama_ci rama_ci

capture drop horaspri_ci horastot_ci
gen byte horaspri_ci=.
label var horaspri_ci "Horas totales trabajadas la semana pasada en la Actividad Principal"

gen byte horastot_ci=.
replace horastot_ci=Q33
label var horastot_ci "Horas totales trabajadas la semana pasada en todas las Actividades"

*** INGRESOS ***

capture drop ylmpri_ci
gen ylmpri_ci=Q325E if Q325A==4
replace ylmpri_ci=Q325E*(30/7) if Q325A==1
replace ylmpri_ci=Q325E/(365/30) if Q325A==7
label var ylmpri_ci "Ingreso Laboral Monetario de la Actividad Principal (Labour Force Survey)"

gen ylmsec_ci=.	
replace ylmsec_ci=Q325O if Q325B==4
replace ylmsec_ci=Q325O*(30/7) if Q325B==1
replace ylmsec_ci=Q325O/(365/30) if Q325B==7
label var ylmsec_ci "Ingreso Laboral Monetario de la Actividad Secundaria (Labour Force Survey)"

gen ylmotros_ci=.
label var ylmotros_ci "Ingreso Laboral Monetario Otros Trabajos"

gen ylnmpri_ci=.
label var ylnmpri_ci "Ingreso Laboral NO Monetario de la Actividad Principal"

gen ylnmsec_ci=.	
label var ylnmsec_ci "Ingreso Laboral NO Monetario de la Actividad Secundaria"

gen ylnmotros_ci=.
label var ylnmotros_ci "Ingreso Laboral NO Monetario Otros Trabajos"

egen ylm_ci=rsum(ylmpri_ci ylmsec_ci)
replace ylm_ci=. if ylmpri_ci==. & ylmsec_ci==.
label var ylm_ci "Ingreso Laboral Monetario Total (Labour Force Survey)"

gen ylnm_ci=.
label var ylnm_ci "Ingreso Laboral NO Monetario Total"


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

gen antiguedad_ci=.
label var antiguedad_ci "Antiguedad en la Ocupacion Actual (en anios)"

gen antiguedad2_ci=Q311 if Q311<9
label var antiguedad2_ci "Antiguedad en la Ocupacion Actual (categorias)"
label define antiguedad2_ci 1 "Menos de 3 meses" 2 "3 meses a menos de 6 meses" 3 "6 a menos de 9 meses" 4 "9 meses a menos de 12 meses" 5 "1 año a menos de 2 años" 6 "2 o mas años"
label values antiguedad2_ci antiguedad2_ci

gen durades_ci=.
label var durades_ci "Duracion del Desempleo (en meses)"

gen durades2_ci=Q41 if Q41<9
label var durades2_ci "Duracion del Desempleo (categorias)"
label define durades2_ci 1 "Menos de 1 mes" 2 "1 mes a menos de 3 meses" 3 "3 meses a menos de 6 meses" 4 "6 meses a menos de 9 meses" 5 "9 meses a menos de 12 meses" 6 "12 meses" 
label values durades2_ci durades2_ci

capture drop emp_ci
gen byte emp_ci=0
replace emp_ci=1 if Q21==1 | Q21==2 | Q22==1 | Q23==1
label var emp_ci "Empleado en la semana de referencia"

capture drop desemp1_ci
gen desemp1_ci=0
replace desemp1_ci=1 if Q21==3 & Q22==2 & Q23==2
label var desemp1_ci "Personas que no tienen trabajo y han buscado trabajo la semana pasada"

capture drop desemp2_ci
gen desemp2_ci=0
replace desemp2_ci=desemp1_ci
replace desemp2_ci=1 if Q21==4 & Q25==2
label var desemp2_ci "desemp1 + personas que no tienen trabajo ni lo buscaron, pero esperan respuesta de una solicitud de empleo, entrevista o temporada agricola"

capture drop desemp3_ci
gen byte desemp3_ci=0
replace desemp3_ci= desemp2_ci
replace desemp3_ci=1 if Q21==4 & Q25==1
label var desemp3_ci "desemp2 + personas que no tienen trabajo pero han buscado trabajo durante las 4 semanas anteriores a la semana pasada"

gen byte pea1_ci=.
replace pea1=1 if emp==1 | desemp1==1
replace pea1=0 if emp==0 & desemp1==0
label var pea1_ci "Poblacion Economicamente Activa utilizando la definicion 'desemp1'"

gen byte pea2_ci=.
replace pea2=1 if emp==1 | desemp2==1
replace pea2=0 if emp==0 & desemp2==0
label var pea2_ci "Poblacion Economicamente Activa utilizando la definicion 'desemp2'"

gen byte pea3_ci=.
replace pea3=1 if emp==1 | desemp3==1
replace pea3=0 if emp==0 & desemp3==0
label var pea3_ci "Poblacion Economicamente Activa utilizando la definicion 'desemp3'"


capture drop desalent_ci
gen byte desalent_ci=0
replace desalent_ci=1 if Q44==4
replace desalent=. if edad<10
label var desalent_ci "Trabajadores desalentados, personas que creen que por alguna razon no conseguiran trabajo" 

gen subemp_ci=. 
label var subemp "Trabajadores subempleados"

gen tiempoparc_ci=.
replace tiempoparc_ci=1 if horastot_ci<=30 & Q34==1 | Q34==3
label var tiempoparc_ci "Trabajadores a medio tiempo"

gen categopri_ci=.
replace categopri_ci=1 if Q323==5 
replace categopri_ci=2 if Q323==6 
replace categopri_ci=3 if Q323==1 | Q323==2 | Q323==3 
replace categopri_ci=4 if Q323==4

label var categopri_ci "CATEGORIA OCUPACIONAL ACTIVIDAD PRINCIPAL"
label define categopri_ci 1 "Patron" 2 "Cuenta Propia" 3 "Asalariado" 4 "Trabajador No Remunerado" 
label values categopri_ci categopri_ci

gen categosec_ci=.
label var categosec_ci "CATEGORIA OCUPACIONAL ACTIVIDAD SECUNDARIA"

gen contrato_ci=.
label var contrato "Personas empleadas que han firmado un contrato de trabajo"

capture drop segsoc_ci
gen segsoc_ci=.
label variable segsoc_ci "Personas que cuentan con seguro social"

capture drop nempleos_ci
gen byte nempleos_ci=.
replace nempleos_ci=1 if emp_ci==1 
replace nempleos_ci=2 if emp_ci==1 & Q38S<.
label var nempleos_ci "Numero de empleos"
label define nempleos_ci 1 "un trabajo" 2 "dos trabajos"
label values nempleos_ci nempleos_ci

capture drop tamfirma_ci
gen byte tamfirma_ci=.
replace tamfirma_ci=1 if emp_ci==1 & (Q324==3 | Q324==4)
replace tamfirma_ci=0 if emp_ci==1 & (Q324==1 | Q324==2)
label var tamfirma "Trabajadores formales"
label define tamfirma_ci 1 "5 o mas trabajadores" 0 "Menos de 5 trabajadores"
label values tamfirma_ci tamfirma_ci

capture drop spublico_ci
gen byte spublico_ci=.
replace spublico_ci=1 if emp_ci==1 & (Q323==1 | Q323==2) 
replace spublico_ci=0 if emp_ci==1 & (Q323>2 & Q323<=6) 
label var spublico_ci "Personas que trabajan en el sector publico"

gen NIVEL=0 if b01==1
replace NIVEL=1 if b01==2 | b01==3 | b01==5 /* PRIMARIO */
replace NIVEL=2 if b01==4 | b01==6 | b01==7 /* 1 CICLO SECUNDARIO */
replace NIVEL=3 if b01==8 | b01==9 /* 2 CICLO SECUNDARIO */
replace NIVEL=4 if b01>=11 & b01<=13 /* TERCIARIO / UNIVERSITARO */
/* b01: 10, 14, 15, 16 son vocacional, clases de adulto, clases de adulto nocturnas y educacion especial, respectivamente */

gen NIVEL2=0 if b21==1
replace NIVEL2=1 if b21==2 | b21==3 | b21==5 /* PRIMARIO */
replace NIVEL2=2 if b21==4 | b21==6 | b21==7 /* 1 CICLO SECUNDARIO */
replace NIVEL2=3 if b21==8 | b21==9 | b21==10 | b21==11 /* 2 CICLO SECUNDARIO */
replace NIVEL2=4 if b21>=13 & b21<=15 /* TERCIARIO / UNIVERSITARO */
/* b21: 12, 16, 17, 18 son vocacional, clases de adulto, clases de adulto nocturnas y educacion especial, respectivamente */


gen GRADO=b04 if NIVEL>0 & NIVEL<=3 /* son los grados para los que asisten actualmente a primaria o secundaria */ 
/* y los que asisten a universitario ????*/

gen GRADO2=b22 if NIVEL2>0 & NIVEL2<=3 /* son los grados de primaria o secundaria para los que no asisten actualmente */

capture drop asiste_ci
gen byte asiste_ci=.
replace asiste_ci=1 if b01>=1 & b01<17
replace asiste_ci=0 if b01==17
label var asiste "Personas que actualmente asisten a centros de enseñanza"

capture drop aedu_ci
gen byte aedu_ci=.
replace aedu_ci=0 if NIVEL==0 | NIVEL2==0
replace aedu_ci=GRADO-1 if NIVEL==1 | NIVEL==2 | NIVEL==3
replace aedu_ci=GRADO2 if NIVEL2==1 | NIVEL2==2 | NIVEL2==3

replace aedu_ci=GRADO-1 if aedu>=age & GRADO<. & GRADO2<.
replace aedu_ci=0 if b01==17 & b21==19
label variable aedu_ci "Años de Educacion (no incluye terciaria o universitaria)"



gen eduno_ci=.
replace eduno_ci=1 if b01==17 & b21==19
replace eduno_ci=0 if (NIVEL>=1 & NIVEL<=4) | (NIVEL2>=1 & NIVEL2<=4)
label var eduno_ci "1 = personas sin educacion (excluye preescolar)"

gen edupre_ci=.
replace edupre_ci=1 if NIVEL==0 | NIVEL2==0
replace edupre_ci=0 if (NIVEL>=1 & NIVEL<=4) | (NIVEL2>=1 & NIVEL2<=4)
label var edupre_ci "Educacion preescolar"

gen edupi_ci=.
replace edupi_ci=1 if aedu_ci>0 & aedu_ci<6
replace edupi_ci=0 if aedu_ci==0 | (aedu>=6 & aedu!=.) | NIVEL==4 | NIVEL2==4
label var edupi_ci "1 = personas que no han completado el nivel primario"

gen edupc_ci=.
replace edupc_ci=1 if aedu_ci==6
replace edupc_ci=0 if (aedu_ci>=0 & aedu_ci<6)  | (aedu_ci>6 & aedu!=.) | NIVEL==4 | NIVEL2==4
label var edupc_ci "1 = personas que han completado el nivel primario"

gen edusi_ci=.
replace edusi_ci=0 if (aedu_ci>=0 & aedu_ci<=6) | (aedu_ci>=11 & aedu_ci!=.) | NIVEL==4 | NIVEL2==4
replace edusi_ci=1 if ((aedu_ci>6 & aedu_ci<11) & asiste==0) | (NIVEL==2 | NIVEL==3)
label var edusi_ci "1 = personas que no han completado el nivel secundario"

gen edusc_ci=.
replace edusc_ci=1 if (GRADO2==11 | GRADO2==12)
replace edusc_ci=0 if (aedu_ci>=0 & aedu<11) | (aedu>12 & aedu!=.) | NIVEL==4 | NIVEL2==4 | edusi_ci==1
label var edusc_ci "1 = personas que han completado el nivel secundario"

gen eduui_ci=.
replace eduui_ci=1 if NIVEL==4 | (NIVEL2==4 & b24<10)
replace eduui_ci=0 if (aedu>=0 & aedu<=12) | (NIVEL2==4 & (b24==10 | b24==11))
label var eduui_ci "1 = personas que no han completado el nivel universitario o superior"

gen eduuc_ci=.
replace eduuc_ci=1 if NIVEL2==4 & (b24==10 | b24==11)
replace eduuc_ci=0 if (aedu>=0 & aedu<=12) | NIVEL==4 | (NIVEL2==4 & b24<10)
label var eduuc_ci "1 = personas que han completado el nivel universitario o superior"


gen edus1i_ci=.
replace edus1i_ci=0 if NIVEL==2 | NIVEL2==2 | NIVEL==3 | NIVEL2==3
replace edus1i_ci=1 if edusi==1 & (NIVEL==2 | (NIVEL2==2 & GRADO2<9))
label var edus1i_ci "1 = personas que no han completado el primer ciclo de la educacion secundaria"

gen edus1c_ci=.
replace edus1c_ci=0 if NIVEL==2 | NIVEL2==2 | NIVEL==3 | NIVEL2==3 
replace edus1c_ci=1 if edusi==1 & (NIVEL2==2 & GRADO2==9)
label var edus1c_ci "1 = personas que han completado el primer ciclo de la educacion secundaria"

gen edus2i_ci=.
replace edus2i_ci=0 if NIVEL==2 | NIVEL2==2 | NIVEL==3 | NIVEL2==3 
replace edus2i_ci=1 if edusi==1 & (NIVEL==3 | (NIVEL2==3 & GRADO2<11))
label var edus2i_ci "1 = personas que no han completado el segundo ciclo de la educacion secundaria"

gen edus2c_ci=.
replace edus2c_ci=0 if edusi==1 
replace edus2c_ci=1 if (NIVEL2==3 & (GRADO2==11 | GRADO2==12)) | edusc_ci==1
label var edus2c_ci "1 = personas que han completado el segundo ciclo de la educacion secundaria"

gen eduac_ci=.
replace eduac_ci=0 if b01==12 | b01==13 | b21==14 | b21==15
replace eduac_ci=1 if b01==11 | b21==13
label var eduac_ci "Educacion terciaria académica versus educación terciaria no-académica "

gen repite_ci=.
label var repite_ci "Personas que han repetido al menos un año o grado"

gen repiteult_ci=.
label var repiteult_ci "Personas que han repetido el ultimo grado"

gen edupub_ci=.
replace edupub_ci=1 if b03==1
replace edupub_ci=0 if b03==2
label var edupub_ci "1 = personas que asisten a centros de enseñanza publicos"

** Generating pqnoasis
gen byte pqnoasis_ci=.
replace pqnoasis_ci=b10a
label var pqnoasis_ci "Razones para no asistir a centros de enseñanza"
label define pqnoasis_ci 1 "Enfermedad" 2 "Haraganeria" 3 "Trabaja fuera del hogar" 4 "Necesita estar en el hogar" 5 "Dia de mercado" 6 "Problema de Transporte" 7 "Escuela cerrada" 8 "Carece de zapatos o uniforme, sucios, o mojados" 9 "Lluvia" 10 "Problemas monetarios" 11 "Tiene que realizar alguna diligencia" 12 "No esta seguro en la escuela" 13 "No esta seguro en la comunidad" 14 "Otro" 
label values pqnoasis_ci pqnoasis_ci

**************
*pqnoasis1_ci*
**************
**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**

g       pqnoasis1_ci = 1 if b10a==8 | b10a==10
replace pqnoasis1_ci = 2 if b10a==3
replace pqnoasis1_ci = 3 if b10a==1
replace pqnoasis1_ci = 4 if b10a==2
replace pqnoasis1_ci = 5 if b10a==4 | b10a==5
replace pqnoasis1_ci = 8 if b10a==6 | b10a==7
replace pqnoasis1_ci = 9 if b10a==9 | b10a==11 | b10a==12 | b10a==13 | b10a==14


label define pqnoasis1_ci 1 "Problemas económicos" 2 "Por trabajo" 3 "Problemas familiares o de salud" 4 "Falta de interés" 5	"Quehaceres domésticos/embarazo/cuidado de niños/as" 6 "Terminó sus estudios" 7	"Edad" 8 "Problemas de acceso"  9 "Otros"
label value  pqnoasis1_ci pqnoasis1_ci

gen luz_ch=.
replace luz_ch=1 if m01==1
replace luz_ch=0 if m01==2


gen cuartos_ch=.
replace cuartos_ch=i03 if i03>=0

gen piso_ch=. // sÃ³lo hay la pregunta de pared.

gen region_c =.

************
* pared_ch *
************

destring i02, replace

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
replace pared_ch=1 if i02>=1 & i02<=5
replace pared_ch=0 if i02>=6 & i02<=7


gen techo_ch=.


*** HOUSING ***

/*gen aguared_ch=.
replace aguared_ch=1 if pv7==1
replace aguared_ch=0 if pv7==2 | pv7==3 | pv7==4

gen aguadist_ch=.

gen aguamala_ch=.

gen aguamide_ch=.

gen luz_ch=.
replace luz_ch=1 if pv11a==1
replace luz_ch=0 if pv11a==2

gen luzmide_ch=.

gen combust_ch=.
replace combust_ch=1 if ph14d==1
replace combust_ch=0 if ph14d==2

gen bano_ch=.
replace bano_ch=1 if pv8==1 | pv8==2 | pv8==3
replace bano_ch=0 if pv8==4

gen banoex_ch=.
replace banoex=1 if ph13a==1
replace banoex=0 if ph13a==2

gen des1_ch=.

gen des2_ch=.
replace des2_ch=1 if pv8==1
replace des2_ch=2 if pv8==2 | pv8==3
replace des2_ch=0 if pv8==4

gen piso_ch=.
replace piso_ch=0 if pv4==3
replace piso_ch=1 if pv4==1 | pv4==2
replace piso_ch=2 if pv4==4

gen pared_ch=.
replace pared_ch=2 if pv2==6

gen techo_ch=.
replace techo_ch=2 if pv3==6

gen resid_ch=.
replace resid_ch=0 if pv11b==1

gen dorm_ch=.
replace dorm_ch=pv6 if pv6>=0

gen cuartos_ch=.
replace cuartos_ch=pv5 if pv5>=0

gen cocina_ch=.

gen telef_ch=.
replace telef_ch=1 if pv11d==1
replace telef_ch=0 if pv11d==2

gen refrig_ch=.
replace refrig_ch=1 if ph14a==1
replace refrig_ch=0 if ph14a==2

gen freez_ch=.

gen auto_ch=.
replace auto_ch=1 if ph15>0 & ph15<.
replace auto_ch=0 if ph15<=0

gen compu_ch=.
replace compu_ch=1 if ph14n==1
replace compu_ch=0 if ph14n==2

gen internet_ch=.
replace internet_ch=1 if ph14o==1 
replace internet_ch=0 if ph14o==2

gen cel_ch=.
replace cel_ch=1 if ph14l==1
replace cel_ch=0 if ph14l==2

gen vivi1_ch=.
replace vivi1_ch=1 if pv1==1 | pv1==2 | pv1==5
replace vivi1_ch=2 if pv1==3 | pv1==4
replace vivi1_ch=3 if pv1>5 & pv1<.

gen vivi2_ch=.
replace vivi2_ch=1 if vivi1_ch==1 | vivi1_ch==2
replace vivi2_ch=0 if vivi1_ch==3

gen viviprop_ch=.
replace viviprop_ch=0 if ph16a==3 | ph16a==4
replace viviprop_ch=1 if ph16a==1
replace viviprop_ch=2 if ph16a==2
replace viviprop_ch=3 if ph16a>4 & ph16a<.

gen vivitit_ch=.

gen vivialq_ch=.
replace vivialq_ch=ph16b if ph16b>=0

gen vivialqimp_ch=. */

drop NIVEL GRADO NIVEL2 GRADO2


g region_BID_c = .
g raza_idioma_ci =.
g id_ind_ci  =.
g id_afro_ci=. 
g raza_ci =.
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
g tcylmpri_ch=.
g ypen_ci=.
g ypensub_ci=.
g salmm_ci=.
g lp_ci=.
g lpe_ci=.
g  tecnica_ci=.
g rentaimp_ch =.
g aguared_ch=.
g aguadist_ch=.
g aguamala_ch =.
g aguamide_ch=.
g luzmide_ch=.
g combust_ch=.
g bano_ch=.
g  banoex_ch=.
g des1_ch=.
g des2_ch=.
g resid_ch=.

**Daniela Zuluaga- Enero 2018: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**
	
*********************
***aguamejorada_ch***
*********************
g       aguamejorada_ch = 1 if (i17 >=1 & i17 <=4) | i17==6
replace aguamejorada_ch = 0 if  i17==5 | (i17 >=7 & i17 <=9)
		
*********************
***banomejorado_ch***
*********************
g       banomejorado_ch = 1 if  i04 ==1
replace banomejorado_ch = 0 if (i04>=2 & i04 <=5)
	 
g dorm_ch=.
g cocina_ch=.
g telef_ch=.
g refrig_ch=.
g freez_ch=.
g auto_ch=.
g compu_ch=.
g internet_ch=.
g cel_ch=.
g vivi1_ch =.
g vivi2_ch=. 
g viviprop_ch=. 
g vivitit_ch =.
g vivialq_ch =.
g vivialqimp_ch=.
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


 
/*


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

** For further information on this do file contact Pavel Luengas (pavell@iadb.org)

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

 
