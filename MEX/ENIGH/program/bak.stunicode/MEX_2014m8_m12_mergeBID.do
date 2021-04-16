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
 
global ruta = "${surveysFolder}\\survey\MEX\ENIGH\2014\m8_m12\data_orig"

local PAIS MEX
local ENCUESTA ENIGH
local ANO "2014"
local ronda m8_m12

local log_file = "${surveysFolder}\harmonized\\`PAIS'\\`ENCUESTA'\\log\\`PAIS'_`ANO'`ronda'_mergeBID.log"
local base_out = "${surveysFolder}\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\\data_merge\\`PAIS'_`ANO'`ronda'.dta"

capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Mexico
Encuesta: ENIGH 
Round: Septiembre-Diciembre
Autores:
Versión 2013: Mayra Sáenz
Última versión: Mayra Sáenz - Email: mayras@iadb.org, saenzmayra.a@gmail.com
Fecha última modificación: 13 de Agosto de 2015

							SCL/LMK - IADB
****************************************************************************/



*Mayra Sáenz - Agosto 2015: Se realiza el merge con base en la sintaxis de CONEVAL, 
*pero con algunas modificaciones, y generando nuevas variables.


*********************************************************
*Parte VII 
*Bienestar (ingresos)
*********************************************************

/*Para la construcción del ingreso corriente del hogar es necesario utilizar
información sobre la condición de ocupación y los ingresos de los individuos.
Se utiliza la información contenida en la base "$ruta\trabajo.dta" para 
identificar a la población ocupada que declara tener como prestación laboral aguinaldo, 
ya sea por su trabajo principal o secundario, a fin de incorporar los ingresos por este 
concepto en la medición*/

*Creación del ingreso monetario deflactado a pesos de agosto del 2014

*Ingresos
/*
use "$ruta\trabajos.dta", clear

*keep proyecto folioviv foliohog numren id_trabajo pres_2

keep folioviv foliohog numren id_trabajo pres_2
destring pres_2 id_trabajo, replace

preserve
keep if id_trabajo ==2
rename pres_2 pres_22
sort folioviv foliohog numren, stable
saveold "$ruta\aguinaldo2.dta", replace
restore

keep if id_trabajo ==1
rename pres_2 pres_21

merge 1:1 folioviv foliohog numren using "$ruta\aguinaldo2.dta"
drop _merge
*reshape wide pres_2, i(folioviv foliohog numren) j(id_trabajo)

gen trab=1

label var trab "Población con al menos un empleo"

gen aguinaldo1=.
replace aguinaldo1=1 if pres_21==2
recode aguinaldo1 (.=0)

gen aguinaldo2=.
replace aguinaldo2=1 if pres_22==2 
recode aguinaldo2 (.=0)

label var aguinaldo1 "Aguinaldo trabajo principal"
label define aguinaldo 0 "No dispone de aguinaldo"
                       1 "Dispone de aguinaldo"
label value aguinaldo1 aguinaldo
label var aguinaldo2 "Aguinaldo trabajo secundario"
label value aguinaldo2 aguinaldo

*keep proyecto folioviv foliohog numren aguinaldo1 aguinaldo2 trab
keep folioviv foliohog numren aguinaldo1 aguinaldo2 trab

sort folioviv foliohog numren 

save "$ruta\aguinaldo.dta", replace
*/


*==============================================================================================*

*Ahora se incorpora a la base de ingresos

use "$ruta\ingresos.dta", clear

*sort proyecto folioviv foliohog numren
sort folioviv foliohog numren

*merge folioviv foliohog numren using "$ruta\aguinaldo.dta"

*tab _merge
*drop _merge

*sort proyecto folioviv foliohog numren

*drop if (clave=="P009" & aguinaldo1!=1)
*drop if (clave=="P016" & aguinaldo2!=1)


/*Una vez realizado lo anterior, se procede a deflactar el ingreso recibido
por los hogares a precios de agosto de 2014. Para ello, se utilizan las 
variables meses, las cuales toman los valores 2 a 10 e indican el mes en
que se recibió el ingreso respectivo*/

sort folioviv foliohog numren

*Definición de los deflactores 2014 

scalar	dic13	=	0.9829863009	
scalar	ene14	=	0.9917752429	
scalar	feb14	=	0.9942876285	
scalar	mar14	=	0.9970115834	
scalar	abr14	=	0.9951515365	
scalar	may14	=	0.9919691814	
scalar	jun14	=	0.9936881821	
scalar	jul14	=	0.9964209524	
scalar	ago14	=	1.0000000000	
scalar	sep14	=	1.0044165095	
scalar	oct14	=	1.0099702040	
scalar	nov14	=	1.0181156226	
scalar	dic14	=	1.0186836881	


destring mes_*, replace
replace ing_6=ing_6/feb14 if mes_6==2
replace ing_6=ing_6/mar14 if mes_6==3
replace ing_6=ing_6/abr14 if mes_6==4
replace ing_6=ing_6/may14 if mes_6==5


replace ing_5=ing_5/mar14 if mes_5==3
replace ing_5=ing_5/abr14 if mes_5==4
replace ing_5=ing_5/may14 if mes_5==5
replace ing_5=ing_5/jun14 if mes_5==6

replace ing_4=ing_4/abr14 if mes_4==4
replace ing_4=ing_4/may14 if mes_4==5
replace ing_4=ing_4/jun14 if mes_4==6
replace ing_4=ing_4/jul14 if mes_4==7

replace ing_3=ing_3/may14 if mes_3==5
replace ing_3=ing_3/jun14 if mes_3==6
replace ing_3=ing_3/jul14 if mes_3==7
replace ing_3=ing_3/ago14 if mes_3==8

replace ing_2=ing_2/jun14 if mes_2==6
replace ing_2=ing_2/jul14 if mes_2==7
replace ing_2=ing_2/ago14 if mes_2==8
replace ing_2=ing_2/sep14 if mes_2==9

replace ing_1=ing_1/jul14 if mes_1==7
replace ing_1=ing_1/ago14 if mes_1==8
replace ing_1=ing_1/sep14 if mes_1==9
replace ing_1=ing_1/oct14 if mes_1==10


/*Se deflactan las claves P008 y P015 (Reparto de utilidades) 
y P009 y P016 (aguinaldo)
con los deflactores de mayo a agosto 2014 
y de diciembre de 2013 a agosto 2014, 
respectivamente, y se obtiene el promedio mensual.*/

replace ing_1=(ing_1/may14)/12 if clave=="P008" | clave=="P015"
replace ing_1=(ing_1/dic13)/12 if clave=="P009" | clave=="P016"

recode ing_2 ing_3 ing_4 ing_5 ing_6 (0=.) if clave=="P008" | clave=="P009" | clave=="P015" | clave=="P016"

/*Una vez realizada la deflactación, se procede a obtener el 
ingreso mensual promedio en los últimos seis meses, para 
cada persona y clave de ingreso*/

egen double ing_mens=rmean(ing_1 ing_2 ing_3 ing_4 ing_5 ing_6)

*Para obtener el ingreso corriente monetario, se seleccionan 
*las claves de ingreso correspondientes

gen double ing_mon=ing_mens if (clave>="P001" & clave<="P009") | (clave>="P011" & clave<="P016") ///
                             | (clave>="P018" & clave<="P048") | (clave>="P067" & clave<="P081")

*Para obtener el ingreso laboral, se seleccionan 
*las claves de ingreso correspondientes
gen double ing_lab=ing_mens if (clave>="P001" & clave<="P009") | (clave>="P011" & clave<="P016") ///
                             | (clave>="P018" & clave<="P022") | (clave>="P067" & clave<="P081")

/*Para obtener el ingreso por rentas, se seleccionan 
las claves de ingreso correspondientes*/
gen double ing_rent=ing_mens if (clave>="P023" & clave<="P031")

/*Para obtener el ingreso por transferencias, se seleccionan 
las claves de ingreso correspondientes*/
gen double ing_tran=ing_mens if (clave>="P032" & clave<="P048")


*Modificación Mayra Sáenz - Agosto 2015 - Se divide al ingreso laboral en principal y secundario

gen double ing_trab1=ing_mens if (clave>="P001" & clave<="P009") | (clave>="P011" & clave<="P013") 
gen double ing_trab2=ing_mens if (clave>="P014" & clave<="P016") | (clave>="P018" & clave<="P020") | (clave=="P067")

gen double ing_negp1=ing_mens if (clave>="P068" & clave<="P074") 
gen double ing_negp2=ing_mens if (clave>="P075" & clave<="P081") | (clave>="P021" & clave<="P022") 


*Modificacion Mayra Saenz -Julio, 2015: Se desagrega el ingreso no laboral monetario
g double ypension = ing_mens  if  (clave=="P032" )                                    //Jubilaciones y pensiones originados dentro del país P032, No se incluyen las provenientes de otros países P033
g double trat_pr  = ing_mens  if  ((clave>="P034" & clave<="P036") | clave=="P041")    //Indemnizaciones recibidas de seguros contra riesgos y terceros,  Indemnizaciones por accidentes de trabajo, Indemnizaciones por despido y retiro voluntario ,Ingresos provenientes de otros paises
g double trat_pu  = ing_mens  if  (clave>="P042"  & clave<="P048")                      //Desde 1996 aparece PROCAMPO, en 2002 se incluye Beneficio de Oportunidades; en 2008 se incluye programa para adultos mayores y beneficios de otros programas sociales; en 2010 Se desagregan los programas de adultos mayores y los otros programas sociales: Beneficio del programa 70 y más, Beneficio de otros programas para adultos mayores, Beneficio del programa Alimentario, Beneficio del programa Empleo Temporal, Beneficios de otros programas sociales.
g double dona_pu  = ing_mens  if  (clave=="P038")                                      //Becas provenientes del gobierno                      
g double dona_pr  = ing_mens  if  (clave=="P037" | clave=="P039" | clave=="P040")      //Becas y donativos provenientes de organizaciones no gubernamentales; Regalos o donativos en dinero provenientes de otros hogares
g double otros    = ing_mens  if  ((clave>="P049" & clave<="P066"))                    //Otros ingresos no considerados en los anteriores (especifique); żcuánto dinero recibió por rendimientos de acciones que posea de alguna empresa en la que no trabajó?; Retiro de inversiones, ahorros, tandas, cajas de ahorro, etc.;  Pagos recibidos de préstamos que usted hizo a personas no miembros del hogar;  Préstamos recibidos de personas no miembros del hogar o instituciones; Venta de monedas, metales preciosos, joyas y obras de arte; Venta de acciones; Venta de bonos; Venta de cédulas; Venta de marcas, patentes y derechos de autor; Herencias, dotes y legados; Loterías y juegos de azar;  Venta de casas, terrenos, condominios, etc. que están dentro del país propiedad de algún miembro del hogar;  Venta de casas, terrenos, condominios, etc.que están fuera del país propiedad de algún miembro del hogar; Venta de maquinaria, equipos, animales de producción, vehículos, etc. utilizados en el negocio propiedad del hogar; Venta de vehículos, aparátos eléctricos de segunda mano, etc.;  Préstamos hipotecarios por bienes inmuebles: casa, terrenos, edificios y locales; Seguro de vida; Otras percepciones financieras de capital no consideradas en las anteriores (especifique).
g double remesas  = ing_mens  if  (clave=="P041")                                      //Ingresos provenientes de otros paises


*Modificación Mayra Sáenz Julio 2015
levelsof clave, local(clave)
foreach k of local clave {
g `k' = ing_mens if clave == "`k'"
}



*Modificacion Mayra Saenz - Julio 2015: En los archivos de coneval se calcula a nivel de hogar.

/**Se estima el total de ingresos de cada  hogar

collapse (sum) ing_mon ing_lab ing_ren ing_tra, by(proyecto folioviv foliohog)

label var ing_mon "Ingreso corriente monetario del hogar"
label var ing_lab "Ingreso corriente monetario laboral"
label var ing_ren "Ingreso corriente monetario por rentas"
label var ing_tra "Ingreso corriente monetario por transferencias"
							 
sort proyecto folioviv foliohog*/
gen str folio= folioviv + foliohog

collapse (sum) ing_mens ing_mon ing_lab ing_trab* ing_negp* ing_rent ing_tran ypension trat_pr trat_pu dona_pu dona_pr otros remesas P*, by(folio numren)


label var folio     "Identificador del hogar"
label var ing_mon   "Ingreso corriente monetario del individuo sin otros ingresos"  //Antes era ingreso del hogar.
label var ing_lab   "Ingreso laboral act. princ. y sec"
label var ing_trab1 "Ingreso por remuneraciones al trabajo act. princ."
label var ing_negp1 "Ingresos por negocios propios act. princ."
label var ing_trab2 "Ingreso por remuneraciones al trabajo act. sec."
label var ing_negp2 "Ingresos por negocios propios act. sec."
label var ing_rent  "Ingresos por renta de la propiedad"
label var ing_tran  "Ingresos por transferencias"
label var ypension  "Ingresos por Jubilacion"
label var trat_pr   "Transferencias monetarias y no monetarias privadas" 
label var trat_pu   "Transferencias monetarias y no monetarias publicas"
label var dona_pu   "Donaciones Publicas"
label var dona_pr   "Donaciones Privadas"
label var remesas   "Remesas"
label var otros     "Otros"

sort folio numren, stable


saveold "$ruta\ingreso_deflactado14_per.dta", replace



*********************************************************

/*Creación del ingreso no monetario deflactado a pesos de 
agosto del 2014.*/

*********************************************************

*No Monetario

use "$ruta\gastoshogar.dta", clear
gen base=1
append using "$ruta\gastospersona.dta"
replace base =2 if base ==.

label var base "Origen del monto"
label define base 1 "Monto del hogar" 2 "Monto de personas"
label value base base

/*En el caso de la información de gasto no monetario, para 
deflactar se utiliza la decena de levantamiento de la 
encuesta, la cual se encuentra en la octava posición del 
folio de la vivienda. En primer lugar se obtiene una variable que 
identifique la decena de levantamiento*/

gen decena=real(substr(folioviv,8,1))

*Definición de los deflactores

*Rubro 1.1 semanal, Alimentos
scalar d11w07=	0.9901303261	
scalar d11w08=	1.0000000000	
scalar d11w09=	1.0095550367	
scalar d11w10=	1.0117409376	
scalar d11w11=	1.0192922318	

*Rubro 1.2 semanal, Bebidas alcohólicas y tabaco
scalar d12w07=	0.9953130285	
scalar d12w08=	1.0000000000	
scalar d12w09=	1.0018441864	
scalar d12w10=	1.0031971266	
scalar d12w11=	1.0026011887	

*Rubro 2 trimestral, Vestido, calzado y accesorios
scalar d2t05=	0.9937183977	
scalar d2t06=	0.9952148881	
scalar d2t07=	0.9984303614	
scalar d2t08=	1.0044224187	

*Rubro 3 mensual, Vivienda
scalar d3m07=	0.9997777670	
scalar d3m08=	1.0000000000	
scalar d3m09=	1.0003671675	
scalar d3m10=	1.0150731919	
scalar d3m11=	1.0376346683	

*Rubro 4.2 mensual, Accesorios y artículos de limpieza para el hogar
scalar d42m07=	0.9971952310	
scalar d42m08=	1.0000000000	
scalar d42m09=	1.0002691884	
scalar d42m10=	0.9973081165	
scalar d42m11=	0.9989492971	

*Rubro 4.2 trimestral, Accesorios y artículos de limpieza para el hogar
scalar d42t05=	0.9977191352	
scalar d42t06=	0.9980635806	
scalar d42t07=	0.9991548065	
scalar d42t08=	0.9991924349	

*Rubro 4.1 semestral, Muebles y aparatos dómesticos
scalar d41s02=	0.9952434028	
scalar d41s03=	0.9949403103	
scalar d41s04=	0.9946616608	
scalar d41s05=	0.9951032633	

*Rubro 5.1 trimestral, Salud
scalar d51t05=	0.9944286156	
scalar d51t06=	0.9972835892	
scalar d51t07=	0.9995669908	
scalar d51t08=	1.0015184188	

*Rubro 6.1.1 semanal, Transporte público urbano
scalar d611w07=	0.9987640919	
scalar d611w08=	1.0000000000	
scalar d611w09=	1.0008100209	
scalar d611w10=	1.0015448852	
scalar d611w11=	1.0027807933	

*Rubro 6 mensual, Transporte
scalar d6m07=	0.9996805033	
scalar d6m08=	1.0000000000	
scalar d6m09=	1.0023265911	
scalar d6m10=	1.0054068667	
scalar d6m11=	1.0051119467	

*Rubro 6 semestral, Transporte
scalar d6s02=	0.9903700427	
scalar d6s03=	0.9935445288	
scalar d6s04=	0.9964773444	
scalar d6s05=	0.9991370859	

*Rubro 7 mensual, Educación y esparcimiento
scalar d7m07=	0.9998214971	
scalar d7m08=	1.0000000000	
scalar d7m09=	1.0121560472	
scalar d7m10=	1.0141642048	
scalar d7m11=	1.0154762011	

*Rubro 2.3 mensual, Accesorios y cuidados del vestido
scalar d23m07=	0.9931651984	
scalar d23m08=	1.0000000000	
scalar d23m09=	1.0016681244	
scalar d23m10=	1.0051576603	
scalar d23m11=	1.0059240958	

*Rubro 2.3 trimestral,  Accesorios y cuidados del vestido
scalar d23t05=	0.9945267486	
scalar d23t06=	0.9963391432	
scalar d23t07=	0.9982777743	
scalar d23t08=	1.0022752616	

*INPC semestral
scalar dINPCs02=	0.9947548441	
scalar dINPCs03=	0.9957069060	
scalar dINPCs04=	0.9969410603	
scalar dINPCs05=	0.9994108382	


*Una vez definidos los deflactores, se seleccionan los rubros
/*
Valor Etiqueta
G1 Gasto monetario en bienes y servicios para el hogar
G2 Gasto monetario en bienes y servicios para otro hogar
G3 Gasto no monetario procedente de autoconsumo
G4 Gasto no monetario por remuneraciones en especie
G5 Gasto no monetario por regalos recibidos de otro hogar
G6 Gasto no monetario por transferencias de instituciones
G7 Gasto imputado por estimación del alquiler
*/
           
*Modificado Mayra Saenz- Abril 2017
gen double gasmon=gasto_tri/3
gen double gasnomon=gas_nm_tri/3

gen esp=1 if tipo_gasto=="G4"
gen reg=1 if tipo_gasto=="G5"
replace reg=1 if tipo_gasto=="G6"
drop if tipo_gasto=="G2" | tipo_gasto=="G3" | tipo_gasto=="G7"

*Control para la frecuencia de los regalos recibidos por el hogar
drop if ((frecu>="5" & frecu<="6") | frecu=="" | frecu=="0") & base==1 & tipo_gasto=="G5"

*Control para la frecuencia de los regalos recibidos por persona

drop if ((frecu>="11" & frecu<="12") | frecu=="") & base==2 & tipo_gasto=="G5"

*Gasto en Alimentos deflactado (semanal) 

gen ali_nm=gasnomon if (clave>="A001" & clave<="A222") | (clave>="A242" & clave<="A247")

replace ali_nm=ali_nm/d11w08 if decena==0
replace ali_nm=ali_nm/d11w08 if decena==1
replace ali_nm=ali_nm/d11w08 if decena==2
replace ali_nm=ali_nm/d11w09 if decena==3
replace ali_nm=ali_nm/d11w09 if decena==4
replace ali_nm=ali_nm/d11w09 if decena==5
replace ali_nm=ali_nm/d11w10 if decena==6
replace ali_nm=ali_nm/d11w10 if decena==7
replace ali_nm=ali_nm/d11w10 if decena==8
replace ali_nm=ali_nm/d11w11 if decena==9

*Gasto en Alcohol y tabaco deflactado (semanal)

gen alta_nm=gasnomon if (clave>="A223" & clave<="A241")

replace alta_nm=alta_nm/d12w08 if decena==0
replace alta_nm=alta_nm/d12w08 if decena==1
replace alta_nm=alta_nm/d12w08 if decena==2
replace alta_nm=alta_nm/d12w09 if decena==3
replace alta_nm=alta_nm/d12w09 if decena==4
replace alta_nm=alta_nm/d12w09 if decena==5
replace alta_nm=alta_nm/d12w10 if decena==6
replace alta_nm=alta_nm/d12w10 if decena==7
replace alta_nm=alta_nm/d12w10 if decena==8
replace alta_nm=alta_nm/d12w11 if decena==9

*Gasto en Vestido y calzado deflactado (trimestral)

gen veca_nm=gasnomon if (clave>="H001" & clave<="H122") | (clave=="H136")

replace veca_nm=veca_nm/d2t05 if decena==0
replace veca_nm=veca_nm/d2t05 if decena==1
replace veca_nm=veca_nm/d2t06 if decena==2
replace veca_nm=veca_nm/d2t06 if decena==3
replace veca_nm=veca_nm/d2t06 if decena==4
replace veca_nm=veca_nm/d2t07 if decena==5
replace veca_nm=veca_nm/d2t07 if decena==6
replace veca_nm=veca_nm/d2t07 if decena==7
replace veca_nm=veca_nm/d2t08 if decena==8
replace veca_nm=veca_nm/d2t08 if decena==9

*Gasto en Vivienda y servicios de conservación deflactado (mensual)

gen viv_nm=gasnomon if (clave>="G001" & clave<="G016") | (clave>="R001" & clave<="R004") | (clave=="R013")

replace viv_nm=viv_nm/d3m07 if decena==0
replace viv_nm=viv_nm/d3m07 if decena==1
replace viv_nm=viv_nm/d3m08 if decena==2
replace viv_nm=viv_nm/d3m08 if decena==3
replace viv_nm=viv_nm/d3m08 if decena==4
replace viv_nm=viv_nm/d3m09 if decena==5
replace viv_nm=viv_nm/d3m09 if decena==6
replace viv_nm=viv_nm/d3m09 if decena==7
replace viv_nm=viv_nm/d3m10 if decena==8
replace viv_nm=viv_nm/d3m10 if decena==9

*Gasto en Artículos de limpieza deflactado (mensual)

gen lim_nm=gasnomon if (clave>="C001" & clave<="C024")

replace lim_nm=lim_nm/d42m07 if decena==0
replace lim_nm=lim_nm/d42m07 if decena==1
replace lim_nm=lim_nm/d42m08 if decena==2
replace lim_nm=lim_nm/d42m08 if decena==3
replace lim_nm=lim_nm/d42m08 if decena==4
replace lim_nm=lim_nm/d42m09 if decena==5
replace lim_nm=lim_nm/d42m09 if decena==6
replace lim_nm=lim_nm/d42m09 if decena==7
replace lim_nm=lim_nm/d42m10 if decena==8
replace lim_nm=lim_nm/d42m10 if decena==9

*Gasto en Cristalería y blancos deflactado (trimestral)

gen cris_nm=gasnomon if (clave>="I001" & clave<="I026")

replace cris_nm=cris_nm/d42t05 if decena==0
replace cris_nm=cris_nm/d42t05 if decena==1
replace cris_nm=cris_nm/d42t06 if decena==2
replace cris_nm=cris_nm/d42t06 if decena==3
replace cris_nm=cris_nm/d42t06 if decena==4
replace cris_nm=cris_nm/d42t07 if decena==5
replace cris_nm=cris_nm/d42t07 if decena==6
replace cris_nm=cris_nm/d42t07 if decena==7
replace cris_nm=cris_nm/d42t08 if decena==8
replace cris_nm=cris_nm/d42t08 if decena==9

*Gasto en Enseres domésticos y muebles deflactado (semestral)

gen ens_nm=gasnomon if (clave>="K001" & clave<="K037")

replace ens_nm=ens_nm/d41s02 if decena==0
replace ens_nm=ens_nm/d41s02 if decena==1
replace ens_nm=ens_nm/d41s03 if decena==2
replace ens_nm=ens_nm/d41s03 if decena==3
replace ens_nm=ens_nm/d41s03 if decena==4
replace ens_nm=ens_nm/d41s04 if decena==5
replace ens_nm=ens_nm/d41s04 if decena==6
replace ens_nm=ens_nm/d41s04 if decena==7
replace ens_nm=ens_nm/d41s05 if decena==8
replace ens_nm=ens_nm/d41s05 if decena==9

*Gasto en Salud deflactado (trimestral)

gen sal_nm=gasnomon if (clave>="J001" & clave<="J072")

replace sal_nm=sal_nm/d51t05 if decena==0
replace sal_nm=sal_nm/d51t05 if decena==1
replace sal_nm=sal_nm/d51t06 if decena==2
replace sal_nm=sal_nm/d51t06 if decena==3
replace sal_nm=sal_nm/d51t06 if decena==4
replace sal_nm=sal_nm/d51t07 if decena==5
replace sal_nm=sal_nm/d51t07 if decena==6
replace sal_nm=sal_nm/d51t07 if decena==7
replace sal_nm=sal_nm/d51t08 if decena==8
replace sal_nm=sal_nm/d51t08 if decena==9

*Gasto en Transporte público deflactado (semanal)

gen tpub_nm=gasnomon if (clave>="B001" & clave<="B007")

replace tpub_nm=tpub_nm/d611w08 if decena==0
replace tpub_nm=tpub_nm/d611w08 if decena==1
replace tpub_nm=tpub_nm/d611w08 if decena==2
replace tpub_nm=tpub_nm/d611w09 if decena==3
replace tpub_nm=tpub_nm/d611w09 if decena==4
replace tpub_nm=tpub_nm/d611w09 if decena==5
replace tpub_nm=tpub_nm/d611w10 if decena==6
replace tpub_nm=tpub_nm/d611w10 if decena==7
replace tpub_nm=tpub_nm/d611w10 if decena==8
replace tpub_nm=tpub_nm/d611w11 if decena==9


*Gasto en Transporte foráneo deflactado (semestral)

gen tfor_nm=gasnomon if (clave>="M001" & clave<="M018") | (clave>="F007" & clave<="F014")

replace tfor_nm=tfor_nm/d6s02 if decena==0
replace tfor_nm=tfor_nm/d6s02 if decena==1
replace tfor_nm=tfor_nm/d6s03 if decena==2
replace tfor_nm=tfor_nm/d6s03 if decena==3
replace tfor_nm=tfor_nm/d6s03 if decena==4
replace tfor_nm=tfor_nm/d6s04 if decena==5
replace tfor_nm=tfor_nm/d6s04 if decena==6
replace tfor_nm=tfor_nm/d6s04 if decena==7
replace tfor_nm=tfor_nm/d6s05 if decena==8
replace tfor_nm=tfor_nm/d6s05 if decena==9

*Gasto en Comunicaciones deflactado (mensual)

gen com_nm=gasnomon if (clave>="F001" & clave<="F006") | (clave>="R005" & clave<="R008") | (clave>="R010" & clave<="R011")

replace com_nm=com_nm/d6m07 if decena==0
replace com_nm=com_nm/d6m07 if decena==1
replace com_nm=com_nm/d6m08 if decena==2
replace com_nm=com_nm/d6m08 if decena==3
replace com_nm=com_nm/d6m08 if decena==4
replace com_nm=com_nm/d6m09 if decena==5
replace com_nm=com_nm/d6m09 if decena==6
replace com_nm=com_nm/d6m09 if decena==7
replace com_nm=com_nm/d6m10 if decena==8
replace com_nm=com_nm/d6m10 if decena==9

*Modificado Mayra Sáenz Abril, 2017
*Gasto no monetario sólo educación

gen edu_gtosnm=gasnomon if (clave>="E001" & clave<="E021") | (clave>="H134" & clave<="H135") 

replace edu_gtosnm=edu_gtosnm/d7m07 if decena==0
replace edu_gtosnm=edu_gtosnm/d7m07 if decena==1
replace edu_gtosnm=edu_gtosnm/d7m08 if decena==2
replace edu_gtosnm=edu_gtosnm/d7m08 if decena==3
replace edu_gtosnm=edu_gtosnm/d7m08 if decena==4
replace edu_gtosnm=edu_gtosnm/d7m09 if decena==5
replace edu_gtosnm=edu_gtosnm/d7m09 if decena==6
replace edu_gtosnm=edu_gtosnm/d7m09 if decena==7
replace edu_gtosnm=edu_gtosnm/d7m10 if decena==8
replace edu_gtosnm=edu_gtosnm/d7m10 if decena==9


*Gasto en Educación y recreación deflactado (mensual)

gen edre_nm=gasnomon if (clave>="E001" & clave<="E034") | (clave>="H134" & clave<="H135") | (clave>="L001" & clave<="L029") | (clave>="N003" & clave<="N005") | clave=="R009"

replace edre_nm=edre_nm/d7m07 if decena==0
replace edre_nm=edre_nm/d7m07 if decena==1
replace edre_nm=edre_nm/d7m08 if decena==2
replace edre_nm=edre_nm/d7m08 if decena==3
replace edre_nm=edre_nm/d7m08 if decena==4
replace edre_nm=edre_nm/d7m09 if decena==5
replace edre_nm=edre_nm/d7m09 if decena==6
replace edre_nm=edre_nm/d7m09 if decena==7
replace edre_nm=edre_nm/d7m10 if decena==8
replace edre_nm=edre_nm/d7m10 if decena==9

*Gasto en Educación básica deflactado (mensual)

gen edba_nm=gasnomon if (clave>="E002" & clave<="E003") | (clave>="H134" & clave<="H135")

replace edba_nm=edba_nm/d7m07 if decena==0
replace edba_nm=edba_nm/d7m07 if decena==1
replace edba_nm=edba_nm/d7m08 if decena==2
replace edba_nm=edba_nm/d7m08 if decena==3
replace edba_nm=edba_nm/d7m08 if decena==4
replace edba_nm=edba_nm/d7m09 if decena==5
replace edba_nm=edba_nm/d7m09 if decena==6
replace edba_nm=edba_nm/d7m09 if decena==7
replace edba_nm=edba_nm/d7m10 if decena==8
replace edba_nm=edba_nm/d7m10 if decena==9

*Gasto en Cuidado personal deflactado (mensual)

gen cuip_nm=gasnomon if (clave>="D001" & clave<="D026") | (clave=="H132")

replace cuip_nm=cuip_nm/d23m07 if decena==0
replace cuip_nm=cuip_nm/d23m07 if decena==1
replace cuip_nm=cuip_nm/d23m08 if decena==2
replace cuip_nm=cuip_nm/d23m08 if decena==3
replace cuip_nm=cuip_nm/d23m08 if decena==4
replace cuip_nm=cuip_nm/d23m09 if decena==5
replace cuip_nm=cuip_nm/d23m09 if decena==6
replace cuip_nm=cuip_nm/d23m09 if decena==7
replace cuip_nm=cuip_nm/d23m10 if decena==8
replace cuip_nm=cuip_nm/d23m10 if decena==9

*Gasto en Accesorios personales deflactado (trimestral)

gen accp_nm=gasnomon if (clave>="H123" & clave<="H131") | (clave=="H133")

replace accp_nm=accp_nm/d23t05 if decena==0
replace accp_nm=accp_nm/d23t05 if decena==1
replace accp_nm=accp_nm/d23t06 if decena==2
replace accp_nm=accp_nm/d23t06 if decena==3
replace accp_nm=accp_nm/d23t06 if decena==4
replace accp_nm=accp_nm/d23t07 if decena==5
replace accp_nm=accp_nm/d23t07 if decena==6
replace accp_nm=accp_nm/d23t07 if decena==7
replace accp_nm=accp_nm/d23t08 if decena==8
replace accp_nm=accp_nm/d23t08 if decena==9

*Gasto en Otros gastos y transferencias deflactado (semestral)

gen otr_nm=gasnomon if (clave>="N001" & clave<="N002") | (clave>="N006" & clave<="N016") | (clave>="T901" & clave<="T915") | (clave=="R012")

replace otr_nm=otr_nm/dINPCs02 if decena==0
replace otr_nm=otr_nm/dINPCs02 if decena==1
replace otr_nm=otr_nm/dINPCs03 if decena==2
replace otr_nm=otr_nm/dINPCs03 if decena==3
replace otr_nm=otr_nm/dINPCs03 if decena==4
replace otr_nm=otr_nm/dINPCs04 if decena==5
replace otr_nm=otr_nm/dINPCs04 if decena==6
replace otr_nm=otr_nm/dINPCs04 if decena==7
replace otr_nm=otr_nm/dINPCs05 if decena==8
replace otr_nm=otr_nm/dINPCs05 if decena==9

*Gasto en Regalos Otorgados deflactado

gen reda_nm=gasnomon if (clave>="T901" & clave<="T915") | (clave=="N013")

replace reda_nm=reda_nm/dINPCs02 if decena==0
replace reda_nm=reda_nm/dINPCs02 if decena==1
replace reda_nm=reda_nm/dINPCs03 if decena==2
replace reda_nm=reda_nm/dINPCs03 if decena==3
replace reda_nm=reda_nm/dINPCs03 if decena==4
replace reda_nm=reda_nm/dINPCs04 if decena==5
replace reda_nm=reda_nm/dINPCs04 if decena==6
replace reda_nm=reda_nm/dINPCs04 if decena==7
replace reda_nm=reda_nm/dINPCs05 if decena==8
replace reda_nm=reda_nm/dINPCs05 if decena==9

save "$ruta\ingresonomonetario_def14.dta", replace

use "$ruta\ingresonomonetario_def14.dta", clear

*Construcción de la base de pagos en especie a partir de la base 
*de gasto no monetario

keep if esp==1

*collapse (sum) *_nm, by(proyecto folioviv foliohog)

collapse (sum) *_nm edu_gtosnm , by(folioviv foliohog)

rename  ali_nm ali_nme
rename  alta_nm alta_nme
rename  veca_nm veca_nme
rename  viv_nm viv_nme
rename  lim_nm lim_nme
rename  cris_nm cris_nme
rename  ens_nm ens_nme
rename  sal_nm sal_nme
rename  tpub_nm tpub_nme
rename  tfor_nm tfor_nme
rename  com_nm com_nme 
rename  edu_gtosnm edu_gtosnme
rename  edre_nm edre_nme
rename  edba_nm edba_nme
rename  cuip_nm cuip_nme
rename  accp_nm accp_nme
rename  otr_nm otr_nme
rename  reda_nm reda_nme

*sort proyecto folioviv foliohog
sort folioviv foliohog
save "$ruta\esp_def14.dta", replace


*Construcción de base de regalos a partir de la base no monetaria

use "$ruta\ingresonomonetario_def14.dta", clear

keep if reg==1

*collapse (sum) *_nm, by(proyecto folioviv foliohog)
collapse (sum) *_nm edu_gtosnm , by(folioviv foliohog)

rename  ali_nm ali_nmr
rename  alta_nm alta_nmr
rename  veca_nm veca_nmr
rename  viv_nm viv_nmr
rename  lim_nm lim_nmr
rename  cris_nm cris_nmr
rename  ens_nm ens_nmr
rename  sal_nm sal_nmr
rename  tpub_nm tpub_nmr
rename  tfor_nm tfor_nmr
rename  com_nm com_nmr 
rename  edu_gtosnm edu_gtosnmr
rename  edre_nm edre_nmr
rename  edba_nm edba_nmr
rename  cuip_nm cuip_nmr
rename  accp_nm accp_nmr
rename  otr_nm otr_nmr
rename  reda_nm reda_nmr

*sort proyecto folioviv foliohog
sort folioviv foliohog

save "$ruta\reg_def14.dta", replace


*Modificación Mayra Saenz, Abril 2017

*Construcción de base de gasto monetario en educación

use "$ruta\ingresonomonetario_def14.dta", clear

*********************************************************

/*Creación del gasto monetario deflactado a pesos de 
agosto del 2014.*/

*********************************************************
*Modificado Mayra Saenz- Abril 2017
*Gasto monetario en educación

use "$ruta\gastoshogar.dta", clear

/*En el caso de la información de gasto no monetario, para 
deflactar se utiliza la decena de levantamiento de la 
encuesta, la cual se encuentra en la octava posición del 
folio de la vivienda. En primer lugar se obtiene una variable que 
identifique la decena de levantamiento*/

gen decena=real(substr(folioviv,8,1))

*Rubro 7 mensual, Educación y esparcimiento
scalar d7m07=	0.9998214971	
scalar d7m08=	1.0000000000	
scalar d7m09=	1.0121560472	
scalar d7m10=	1.0141642048	
scalar d7m11=	1.0154762011	


*Una vez definidos los deflactores, se seleccionan los rubros
/*
Valor Etiqueta
G1 Gasto monetario en bienes y servicios para el hogar
G2 Gasto monetario en bienes y servicios para otro hogar
G3 Gasto no monetario procedente de autoconsumo
G4 Gasto no monetario por remuneraciones en especie
G5 Gasto no monetario por regalos recibidos de otro hogar
G6 Gasto no monetario por transferencias de instituciones
G7 Gasto imputado por estimación del alquiler
*/



gen double gasmon=gasto_tri/3


keep if tipo_gasto=="G1" // Sólo gasto dentro del hogar



*Gasto monetario sólo educación
gen edu_gtosm=gasmon if (clave>="E001" & clave<="E021") | (clave>="H134" & clave<="H135") 

replace edu_gtosm=edu_gtosm/d7m07 if decena==0
replace edu_gtosm=edu_gtosm/d7m07 if decena==1
replace edu_gtosm=edu_gtosm/d7m08 if decena==2
replace edu_gtosm=edu_gtosm/d7m08 if decena==3
replace edu_gtosm=edu_gtosm/d7m08 if decena==4
replace edu_gtosm=edu_gtosm/d7m09 if decena==5
replace edu_gtosm=edu_gtosm/d7m09 if decena==6
replace edu_gtosm=edu_gtosm/d7m09 if decena==7
replace edu_gtosm=edu_gtosm/d7m10 if decena==8
replace edu_gtosm=edu_gtosm/d7m10 if decena==9


collapse (sum) edu_gtosm  , by(folioviv foliohog)

rename  edu_gtosm edu_gtosmh

*sort proyecto folioviv foliohog
sort folioviv foliohog

save "$ruta\edu_gtosmh", replace


*Gasto personas

use "$ruta\gastospersona.dta", clear

/*En el caso de la información de gasto no monetario, para 
deflactar se utiliza la decena de levantamiento de la 
encuesta, la cual se encuentra en la octava posición del 
folio de la vivienda. En primer lugar se obtiene una variable que 
identifique la decena de levantamiento*/

gen decena=real(substr(folioviv,8,1))

*Definición de los deflactores

*Rubro 7 mensual, Educación y esparcimiento
scalar d7m07=	0.9998214971	
scalar d7m08=	1.0000000000	
scalar d7m09=	1.0121560472	
scalar d7m10=	1.0141642048	
scalar d7m11=	1.0154762011	


*Una vez definidos los deflactores, se seleccionan los rubros
/*
Valor Etiqueta
G1 Gasto monetario en bienes y servicios para el hogar
G2 Gasto monetario en bienes y servicios para otro hogar
G3 Gasto no monetario procedente de autoconsumo
G4 Gasto no monetario por remuneraciones en especie
G5 Gasto no monetario por regalos recibidos de otro hogar
G6 Gasto no monetario por transferencias de instituciones
G7 Gasto imputado por estimación del alquiler
*/


gen double gasmon=gasto_tri/3


keep if tipo_gasto=="G1" // Sólo gasto dentro del hogar



*Gasto monetario sólo educación
gen edu_gtosm=gasmon if (clave>="E001" & clave<="E021") | (clave>="H134" & clave<="H135") 

replace edu_gtosm=edu_gtosm/d7m07 if decena==0
replace edu_gtosm=edu_gtosm/d7m07 if decena==1
replace edu_gtosm=edu_gtosm/d7m08 if decena==2
replace edu_gtosm=edu_gtosm/d7m08 if decena==3
replace edu_gtosm=edu_gtosm/d7m08 if decena==4
replace edu_gtosm=edu_gtosm/d7m09 if decena==5
replace edu_gtosm=edu_gtosm/d7m09 if decena==6
replace edu_gtosm=edu_gtosm/d7m09 if decena==7
replace edu_gtosm=edu_gtosm/d7m10 if decena==8
replace edu_gtosm=edu_gtosm/d7m10 if decena==9

sort folioviv foliohog numren
collapse (sum) edu_gtosm  , by(folioviv foliohog numren)

rename  edu_gtosm edu_gtosmp

*sort proyecto folioviv foliohog
sort folioviv foliohog

save "$ruta\edu_gtosmp", replace


*********************************************************

*Construcción del ingreso corriente total

*********************************************************

use "$ruta\concentradohogar.dta", clear

*keep proyecto folioviv foliohog tam_loc factor tot_integ est_dis upm ubica_geo


/*Incorporación de la base de ingreso monetario deflactado

sort proyecto folioviv foliohog

merge proyecto folioviv foliohog using "$ruta\ingreso_deflactado14.dta"
tab _merge
drop _merge*/

*Incorporación de la base de ingreso no monetario deflactado: pago en especie

*sort proyecto folioviv foliohog
sort folioviv foliohog

*Se incorpora la base de hogares

merge 1:1 folioviv foliohog using "$ruta\hogares.dta"
tab _merge
drop _merge

merge m:1 folioviv using "$ruta\viviendas.dta"
tab _merge
drop _merge


merge 1:1 folioviv foliohog using "$ruta\esp_def14.dta"
tab _merge
drop _merge

*Incorporación de la base de ingreso no monetario deflactado: regalos en especie

sort folioviv foliohog

merge 1:1 folioviv foliohog using "$ruta\reg_def14.dta"
tab _merge
drop _merge

gen rururb=1 if tam_loc=="4"
replace rururb=0 if tam_loc<="3"
label define rururb 1 "Rural" 0 "Urbano"
label value rururb rururb


egen double pago_esp=rsum(ali_nme alta_nme veca_nme viv_nme lim_nme ens_nme cris_nme sal_nme tpub_nme tfor_nme com_nme edre_nme cuip_nme accp_nme otr_nme)

egen double reg_esp=rsum(ali_nmr alta_nmr veca_nmr viv_nmr lim_nmr ens_nmr cris_nmr sal_nmr tpub_nmr tfor_nmr com_nmr edre_nmr cuip_nmr accp_nmr otr_nmr)

egen double nomon=rsum(pago_esp reg_esp)

egen double gtos_edu =rsum(edu_gtosnme edu_gtosnmr)


label var nomon "Ingreso corriente no monetario"
label var pago_esp "Ingreso corriente no monetario pago especie"
label var reg_esp "Ingreso corriente no monetario regalos especie"
label var gtos_edu "Gastos en Educación - ya están incluidos en pago_esp y reg_esp"

sort  folioviv foliohog

save "$ruta\ingresotot14.dta", replace

saveold "$ruta\gtos_autoc14.dta", replace

*--------------------------------------------*
*Base de Trabajos
*--------------------------------------------*

*nivel de personas, (hacer reshape )
use "$ruta\trabajos.dta",clear
	keep folioviv foliohog numren id_trabajo  trapais subor indep personal pago contrato tipocontr htrab sinco scian clas_emp tam_emp no_ing tiene_suel pres_*
	egen per = concat(folioviv foliohog numren)
	rename pres_1 pres_0
	reshape wide trapais subor indep personal pago contrato tipocontr htrab sinco scian clas_emp tam_emp no_ing tiene_suel pres_*, i(per) j(id_trabajo) string
	rename pres_01 pres_11
	rename pres_02 pres_12
	
	
	foreach var of varlist trapais1 subor1 indep1 personal1 pago1 contrato1 tipocontr1 htrab1 sinco1 scian1 clas_emp1 tam_emp1 no_ing1 tiene_suel1 ///
	pres_11 pres_21 pres_31 pres_41 pres_51 pres_61 pres_71 pres_81 pres_91 pres_101 pres_111 pres_121 pres_131 pres_141 pres_151 pres_161 pres_171 pres_181 pres_191 pres_201 {
	    label var `var' "`var' del primer trabajo"
	}
	foreach var of varlist trapais2 subor2 indep2 personal2 pago2 contrato2 tipocontr2 htrab2 sinco2 scian2 clas_emp2 tam_emp2 no_ing2 tiene_suel2 ///
	pres_12 pres_22 pres_32 pres_42 pres_52 pres_62 pres_72 pres_82 pres_92 pres_102 pres_112 pres_122 pres_132 pres_142 pres_152 pres_162 pres_172 pres_182 pres_192 pres_202 {
	    label var `var' "`var' del segundo trabajo"
	}
		
	drop per
	sort  folioviv foliohog numren
	saveold "$ruta\trabajos_reshape.dta", replace

*_________________________________________________________________________________________________________*
* Modificación Mayra Sáenz: Se unifica con la base de personas con la de ingresos, de vivienda y de gastos
*_________________________________________________________________________________________________________*


use "$ruta\poblacion.dta", clear //Base nueva
gen str folio= folioviv + foliohog
order folio, first
sort folio numren, stable

merge 1:1 folioviv foliohog numren using "$ruta\trabajos_reshape.dta"
drop _merge

merge 1:1 folio numren using "$ruta\ingreso_deflactado14_per.dta"
rename _merge _merge_inge
sort folio numren, stable

merge m:1 folioviv foliohog numren using "$ruta\edu_gtosmp.dta"
drop _merge

merge m:1 folioviv foliohog using "$ruta\gtos_autoc14.dta"
drop _merge

merge m:1 folioviv foliohog using "$ruta\edu_gtosmh.dta"
drop _merge

*Modificación Mayra Sáenz: Total Ingreso monetario del hogar
bys folio: egen ing_monh = sum(ing_mon)

egen double ict=rsum(ing_monh nomon)  if parentesco=="101" | parentesco=="102" //Mayra Sáenz Agosto 2015 - Aumento esta condición porque esta base está a nivel de personas

label var  ict "Ingreso corriente total"

saveold "`base_out'", replace


log close


















