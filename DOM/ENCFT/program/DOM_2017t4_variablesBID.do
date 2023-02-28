
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

local PAIS DOM
local ENCUESTA ENCFT
local ANO "2017"
local ronda t4 

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
   

capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Republica Dominicana
Encuesta: ENCFT
Round: t4
Autores: Alvaro Altamirano (alvaroalt@iadb.org)
Fecha última modificación: Junio de 2018
							SCL/LMK - IADB
****************************************************************************/
/***************************************************************************
Detalle de procesamientos o modificaciones anteriores:

****************************************************************************/


use `base_in', clear


		**********************************
		***VARIABLES DEL IDENTIFICACION***
		**********************************
		
	****************
	* region_BID_c *
	****************
	
gen region_BID_c=1

label var region_BID_c "Regiones BID"
label define region_BID_c 1 "Centroamérica_(CID)" 2 "Caribe_(CCB)" 3 "Andinos_(CAN)" 4 "Cono_Sur_(CSC)"
label value region_BID_c region_BID_c


***************
*** region_c **
***************

*Con el objeto de mantener la comparabilidad con años anteriores, se mantiene la variable original.
gen region_c=id_provincia
label define region_c 1 "Distrito Nacional" ///
2 "Azua" ///
3 "Bahoruco" ///
4 "Barahona" ///
5 "Dajabon" ///
6 "Duarte" ///
7 "Elias Piña" ///
8 "El Seibo" ///
9 "Espaillat" ///
10 "Independencia" ///
11 "La Altagracia" ///
12 "La Romana" ///
13 "La Vega" ///
14 "Maria Trinidad Sanchez" ///
15 "Monte Cristi" ///
16 "Pedernales" ///
17 "Peravia" ///
18 "Puerto Plata" ///
19 "Salcedo" ///
20 "Samana" ///
21 "San Cristobal" ///
22 "San Juan" ///
23 "San Pedro De Macoris" ///
24 "Sanchez Ramirez" ///
25 "Santiago" ///
26 "Santiago Rodriguez" ///
27 "Valverde" ///
28 "Monseñor Nouel" ///
29 "Monte Plata" ///
30 "Hato Mayor" ///
31 "San Jose De Ocoa" ///
32 "Santo Domingo" 
label value region_c region_c
label var region_c "Region - id_provincias"  

***************
***factor_ch***
***************

gen factor_ch= factor_expansion
label variable factor_ch "Factor de expansion del hogar"

***************
****idh_ch*****
***************

sort vivienda hogar
egen idh_ch = group(vivienda hogar)
label variable idh_ch "ID del hogar"


*************
****idp_ci****
**************

gen idp_ci=miembro
label variable idp_ci "ID de la persona en el hogar"

**********
***zona***
**********

gen byte zona_c=1 if zona==1
replace zona_c=0 if zona==2

label variable zona_c "Zona del pais"
label define zona_c 1 "Urbana" 0 "Rural"
label value zona_c zona_c



************
****pais****
************

gen str3 pais_c="DOM"
label variable pais_c "Pais"

**********
***anio***
**********

gen anio_c=2017
label variable anio_c "Anio de la encuesta"

***************
***trimestre***
***************
*se usa el cuarto trimestre porque la encuesta anterior, ENFT era representativa para el mes 10,
*la muestra mensual de esta encuesta no lo es
gen byte trimestre_c=4
label variable trimestre_c "trimestre de la encuesta"

*********
***mes***
*********

gen mes_c=mes
label var mes_c "Mes de la encuesta"  


*****************
***relacion_ci***
*****************

* No hay manera de identificar a empleado doméstico
gen relacion_ci=1 if parentesco==1
replace relacion_ci=2 if parentesco==2
replace relacion_ci=3 if parentesco==3 | parentesco==4 
replace relacion_ci=4 if parentesco>=5 & parentesco<=11
replace relacion_ci=5 if parentesco==12

label variable relacion_ci "Relacion con el jefe del hogar"
label define relacion_ci 1 "Jefe/a" 2 "Esposo/a" 3 "Hijo/a" 4 "Otros parientes" 5 "Otros no parientes"
label value relacion_ci relacion_ci


	****************************
	***VARIABLES DEMOGRAFICAS***
	****************************

***************
***factor_ci***
***************

gen factor_ci=factor_ch
label variable factor_ci "Factor de expansion del individuo"

***************
***upm_ci***
***************

clonevar upm_ci=upm
label variable upm_ci "Unidad Primaria de Muestreo"

***************
***estrato_ci***
***************

clonevar estrato_ci=estrato
label variable estrato_ci "Estrato"

**********
***sexo***
**********

gen sexo_ci=sexo
label var sexo_ci "Sexo del individuo" 
label define sexo_ci 1 "Hombre" 2 "Mujer"
label value sexo_ci sexo_ci

**********
***edad***
**********

gen edad_ci=edad
label variable edad_ci "Edad del individuo"


*****************
***civil_ci***
*****************

gen civil_ci=.
replace civil_ci=1 if estado_civil==6
replace civil_ci=2 if estado_civil==1 | estado_civil==2 
replace civil_ci=3 if estado_civil==3 | estado_civil==4
replace civil_ci=4 if estado_civil==5

label variable civil_ci "Estado civil"
label define civil_ci 1 "Soltero" 2 "Union formal o informal"
label define civil_ci 3 "Divorciado o separado" 4 "Viudo" , add
label value civil_ci civil_ci


**************
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
*NOTA: dentro de las relaciones de parentesco no es posible identificar a los empleados domésticos
gen nempdom_ch=.
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
****************
****condocup_ci*
****************

gen condocup_ci=.
replace condocup_ci=1 if trabajo_semana_pasada==1 | tenia_empleo_negocio==1 | (realizo_actividad!=8 & realizo_actividad!=.)
replace condocup_ci=2 if (trabajo_semana_pasada==2 | tenia_empleo_negocio==2 | realizo_actividad==8 ) & (busco_trabajo_establ_negocio==1)
recode condocup_ci (.=3) if edad_ci>=10
replace condocup_ci=4 if edad_ci<10
label var condocup_ci "Condicion de ocupación de acuerdo a def de cada pais"
label define condocup_ci 1 "Ocupado" 2 "Desocupado" 3 "Inactivo" 4 "Menor que 10" 
label value condocup_ci condocup_ci

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

*****************
***desalent_ci***
*****************

gen desalent_ci=(motivo_no_busca_trabajo==5 | motivo_no_busca_trabajo==6)
replace desalent_ci=. if motivo_no_busca_trabajo==.
label var desalent_ci "Trabajadores desalentados"
*Utilizo para desalentado la opcion 5 y la 6 del nuevo cuestionario

 ***************
 ***subemp_ci***
 ***************    
 
*Horas semanales trabajadas empleo principal
gen promhora=horas_trabaja_semana_principal if emp_ci==1 

*Horas semanales trabajadas empleo secundario
gen promhora1=horas_trabajo_ocup_secun if emp_ci==1
egen tothoras=rowtotal(promhora promhora1)
replace tothoras=. if promhora==. & promhora1==. 
replace tothoras=. if tothoras>=168

*Modificacion MGD 06/20/2014: condiciona solo a horas en ocupacion primaria.
gen subemp_ci=0  
replace  subemp_ci=1 if (promhora>=1 & promhora<=30) & emp_ci==1 & desea_trabajar_mas_horas==1
label var subemp_ci "Personas en subempleo por horas"


*****************
***horaspri_ci***
*****************

gen horaspri_ci=horas_trabaja_semana_principal
replace horaspri_ci=. if emp_ci==0
label var horaspri_ci "Horas trabajadas semanalmente en el trabajo principal"

*****************
***horastot_ci***
*****************

gen horastot_ci=tothoras  if emp_ci==1 
label var horastot_ci "Horas trabajadas semanalmente en todos los empleos"

*******************
***tiempoparc_ci***
*******************

gen tiempoparc_ci=(tothoras>=1 & horastot_ci<=30) &  emp_ci==1 & desea_trabajar_mas_horas==2
replace tiempoparc_ci=. if emp_ci==0
label var tiempoparc_c "Personas que trabajan medio tiempo" 

******************
***categopri_ci***
******************
*clasificación cambió en relación la de las ENFTs (<-2016)
*Muchos trabajadores que la encuesta clasifica como no remunerados nuestra clasificación los clasifica commo inactivos.
*Para recuperar información del ingreso de estos trabajadores los reclasificamos como no remunerados si observan ingresos en categorías de no_remunerados
egen noremunerados=rsum(crianza_no_remun_monto pesca_no_remun_monto alimentos_no_remun_monto), missing
gen categopri_ci=.
replace categopri_ci=1 if categoria_principal==6
replace categopri_ci=2 if categoria_principal==7 & noremunerados==.
replace categopri_ci=3 if categoria_principal==1 | categoria_principal==2 | categoria_principal==3 | categoria_principal==4 | categoria_principal==5
replace categopri_ci=4 if categoria_principal==8 | grupo_categoria=="Familiar no remunerado" | noremunerados!=.
replace categopri_ci=. if emp_ci==0 & noremunerados==.

label define categopri_ci 1"Patron" 2"Cuenta propia" 0"Otro"
label define categopri_ci 3"Empleado" 4" No remunerado" , add
label value categopri_ci categopri_ci
label variable categopri_ci "Categoria ocupacional"



******************
***categosec_ci***
******************

gen categosec_ci=.
replace categosec_ci=1 if categoria_secundaria==6
replace categosec_ci=2 if categoria_secundaria==7 
replace categosec_ci=3 if categoria_secundaria==1 | categoria_secundaria==2 | categoria_secundaria==3 | categoria_secundaria==4 | categoria_secundaria==5
replace categosec_ci=4 if categoria_secundaria==8
replace categosec_ci=. if emp_ci==0

label define categosec_ci 1"Patron" 2"Cuenta propia" 0"Otro" 
label define categosec_ci 3"Empleado" 4"No remunerado" , add
label value categosec_ci categosec_ci
label variable categosec_ci "Categoria ocupacional trabajo secundario"


*****************
***nempleos_ci***
*****************

gen nempleos_ci=.
replace nempleos_ci=1 if emp_ci==1 & cuantos_trabajos_tiene==1
replace nempleos_ci=cuantos_trabajos_tiene_cant if emp_ci==1 & cuantos_trabajos_tiene==2
replace nempleos_ci=. if emp_ci==0
label var nempleos_ci "Número de empleos" 

*****************
***spublico_ci***
*****************

gen spublico_ci=(categoria_principal==1 |categoria_principal==2) 
replace spublico_ci=. if emp_ci==0 
label var spublico_ci "Personas que trabajan en el sector público"


**************
***ocupa_ci***
**************
*CIUO-08, base anterior usaba CIUO-88 a 3 dígitos, esto crea saltos en la distribución de ocupaciones ilustrada por el SIMS
generat ocupa_ci=.
replace ocupa_ci=1 if (ocupacion_principal_cod>=2111 & ocupacion_principal_cod<=3522) & emp_ci==1
replace ocupa_ci=2 if (ocupacion_principal_cod>=1111 & ocupacion_principal_cod<=1439) & emp_ci==1
replace ocupa_ci=3 if (ocupacion_principal_cod>=4110 & ocupacion_principal_cod<=4419) & emp_ci==1
replace ocupa_ci=4 if ((ocupacion_principal_cod>=5211 & ocupacion_principal_cod<=5249) | (ocupacion_principal_cod>=9510 & ocupacion_principal_cod<=9520)) & emp_ci==1
replace ocupa_ci=5 if ((ocupacion_principal_cod>=5110 & ocupacion_principal_cod<=5169) | (ocupacion_principal_cod>=5311 & ocupacion_principal_cod<=5419) | (ocupacion_principal_cod>=9111 & ocupacion_principal_cod<=9129) | (ocupacion_principal_cod>=9610 & ocupacion_principal_cod<=9624))  & emp_ci==1
replace ocupa_ci=6 if ((ocupacion_principal_cod>=6110 & ocupacion_principal_cod<=6340) | (ocupacion_principal_cod>=9210 & ocupacion_principal_cod<=9216)) & emp_ci==1
replace ocupa_ci=7 if ((ocupacion_principal_cod>=7111 & ocupacion_principal_cod<=8350) | (ocupacion_principal_cod>=9310 & ocupacion_principal_cod<=9412))  & emp_ci==1
replace ocupa_ci=8 if (ocupacion_principal_cod>=110 & ocupacion_principal_cod<=310) & emp_ci==1
replace ocupa_ci=9 if ocupacion_principal_cod>=9629 & ocupacion_principal_cod!=. & emp_ci==1
label define ocupa_ci 1"profesional y tecnico" 2"director o funcionario sup" 3"administrativo y nivel intermedio"
label define ocupa_ci  4 "comerciantes y vendedores" 5 "en servicios" 6 "trabajadores agricolas", add
label define ocupa_ci  7 "obreros no agricolas, conductores de maq y ss de transporte", add
label define ocupa_ci  8 "FFAA" 9 "Otras ", add
label value ocupa_ci ocupa_ci
label variable ocupa_ci "Ocupacion laboral" 

*************
***rama_ci***
*************
*Nota: para esta nueva base, ENCFT se contruye la variable rama_ci siguiendo la CIUU revision4, y no la Rev3 como en bases anteriores
*esto resulta en saltos en el share de ramas en la serie SIMS
*AJAM, junio 2018

rename rama_principal_cod ramac

gen rama_ci=.
replace rama_ci = 1 if (ramac>=111 & ramac<=322)  & emp_ci==1
replace rama_ci = 2 if (ramac>=510 & ramac<=990)  & emp_ci==1
replace rama_ci = 3 if (ramac>=1010 & ramac<=3320)  & emp_ci==1
replace rama_ci = 4 if (ramac>=3510 & ramac<=3900)  & emp_ci==1
replace rama_ci = 5 if (ramac>=4100 & ramac<=4390)  & emp_ci==1
replace rama_ci = 6 if (ramac>=4510 & ramac<=4799)  & emp_ci==1
replace rama_ci = 7 if (ramac>=4911 & ramac<=6399)  & emp_ci==1
replace rama_ci = 8 if (ramac>=6411 & ramac<=6820)  & emp_ci==1
replace rama_ci = 9 if (ramac>=6910 & ramac<=9990)  & emp_ci==1

label var rama_ci "Rama de actividad"
label def rama_ci 1"Agricultura, caza, silvicultura y pesca" 2"Explotación de minas y canteras" 3"Industrias manufactureras"
label def rama_ci 4"Electricidad, gas y agua" 5"Construcción" 6"Comercio, restaurantes y hoteles" 7"Transporte y almacenamiento", add
label def rama_ci 8"Establecimientos financieros, seguros e inmuebles" 9"Servicios sociales y comunales", add
label val rama_ci rama_ci

************
*durades_ci*
************
/*
que_tiempo_busca_trabajo 
1 Menos de 1 mes
2 1 mes a menos de 2 meses
3 2 meses a menos de 3 meses.
4 3 meses a menos de 6 meses.
5 6 meses a menos de 1 año.
6 más de un año.
*Mayra Sáenz-Febrero 2014
*Se cambia la variable utilizada y se estima el número de meses como el promedio, al igual que en el caso de Jamaica.
*Alvaro Altamirano-2018, las categorías se reduceron a 4, por lo que los promedios serán aún menos acertados.
*/
gen durades_ci=.
replace durades_ci=1 if que_tiempo_busca_trabajo==1
replace durades_ci=(1+6)/2 if que_tiempo_busca_trabajo==2
replace durades_ci=(6+12)/2 if que_tiempo_busca_trabajo==3
replace durades_ci=(12+12)/2 if que_tiempo_busca_trabajo==4

label variable durades_ci "Duracion del desempleo en meses"
label def durades_ci 1 "Menos de un mes" 2"1 mes a menos de 2 meses" 3"2 meses a menos de 3 meses"
label def durades_ci 4 "3 meses a menos de 6 meses" 5"6 meses a menos de 1 año" 6"Más de 1 año", add
label val durades_ci durades1_ci

*******************
***antiguedad_ci***
*******************

gen temp1=tiempo_empleo_dias/365
gen temp2=tiempo_empleo_meses/12

egen antiguedad_ci= rsum(tiempo_empleo_anos temp1 temp2), missing  
replace antiguedad_ci=. if emp_ci==0
replace antiguedad_ci=. if tiempo_empleo_dias==. & tiempo_empleo_meses==. & tiempo_empleo_anos==.
label var antiguedad_ci "Antiguedad en la actividad actual en anios"
drop temp*

************************************************************************
**************************INGRESOS**************************************
************************************************************************

*Asalariados
gen ymensual= 	   sueldo_bruto_ap_monto*tiempo_recibe_pago_dias_ap*4.3 if tiempo_recibe_pago_ap==1
replace ymensual=  sueldo_bruto_ap_monto*4.3   if tiempo_recibe_pago_ap==2
replace ymensual=  sueldo_bruto_ap_monto*2   if tiempo_recibe_pago_ap==3
replace ymensual=  sueldo_bruto_ap_monto     if tiempo_recibe_pago_ap==4

*Independientes
gen ymensualindep= 	   ingreso_actividad_in_monto*ingreso_actividad_in_dias*4.3 if ingreso_actividad_in_periodo==1
replace ymensualindep=  ingreso_actividad_in_monto*4.3   if ingreso_actividad_in_periodo==2
replace ymensualindep=  ingreso_actividad_in_monto*2   if ingreso_actividad_in_periodo==3
replace ymensualindep=  ingreso_actividad_in_monto     if ingreso_actividad_in_periodo==4

*variables en la base con el mismo nombre
rename comisiones otrascomisionesoriginales
rename propinas otraspropinasoriginales
rename bonificaciones bonificacionesoriginales
gen comisiones=comisiones_ap_monto 
gen propinas=propinas_ap_monto 
gen horasextra=horas_extra_ap_monto 

gen vacaciones=vacaciones_ap_monto/12 
gen dividendos=dividendos_ap_monto/12
gen bonificaciones=bonificacion_ap_monto/12
gen regalia=regalia_ap_monto/12
gen utilidades=utilidad_empresarial_ap_monto/12                                               
gen beneficios=beneficios_marginales_ap_monto/12                                             
gen bonoantiguedad=incentivo_antiguedad_ap_monto/12
gen otrosbeneficios=otros_beneficios_ap_monto/12

gen alimentos=alimentacion_especie_ap_monto if alimentacion_especie_ap==1
gen vivienda1=vivienda_especie_ap_monto if vivienda_especie_ap ==1
gen transporte=transporte_especie_ap_monto if transporte_especie_ap==1
gen gasolina=gasolina_especie_ap_monto if gasolina_especie_ap==1
gen cellular=celular_especie_ap_monto if celular_especie_ap==1
gen otros=otros_especie_ap_monto if otros_especie_ap==1

gen pension=pension_nac_monto  	    if pension_nac==1
gen intereses= intereses_nac_monto 	if intereses_nac==1 
gen alquiler= alquiler_nac_monto 	if alquiler_nac==1
gen remesasnales=remesas_nac_monto  if remesas_nac==1  
gen otrosing=ayuda_especie_nac_monto  if ayuda_especie_nac==1 
egen gobierno=rsum(alimentos_escuela_nac_monto gob_comer_primero_monto gob_inc_asis_escolar_monto gob_bono_luz_monto gob_bonogas_choferes_monto gob_bonogas_hogares_monto  ///
gob_proteccion_vejez_monto gob_bono_estudiante_prog_monto gob_inc_educacion_sup_monto gob_inc_policia_prev_monto gob_inc_marina_guerra_monto) if gobierno_nac==1, missing
recode ingreso_asalariado_secun (0=.)
recode ingreso_independientes_secun (0=.)
egen ymensual2=rsum(ganancia_secun_imp_monto ingreso_asalariado_secun ingreso_independientes_secun), missing

*REMESAS*
*Para República Dominicana hay dos módulos especiales: remesas e ingresos del exterior.
*Aquí se trabaja sobre esas variables:
*Módulo de ingresos del exterior
********************************
*Información cambiaria que viene en la base de excel
*Dado que se necesita la información en moneda local se calcula el factor de conversión a pesos
*Si la información está en pesos se deja como está
*Si la información está en dólares se multiplica por 47.5, Euros-luego de ser convertidos en dolares, por 0.87 (promedio para los meses del cuarto trimestre d2 2017)


*Modulo Ingresos del Exterior
gen pension_int=pension_ext_monto	 		    if  pension_ext_moneda=="DOP"
replace pension_int=pension_ext_monto*47.5   	if  pension_ext_moneda=="USD"
replace pension_int=(pension_ext_monto*0.87)*47.5   if  pension_ext_moneda=="EUR"
replace pension_int=. if pension_ext==2

/* No se reportaron intereses, ni de alquileres,ni otros ingresos, del exterior en este periodo
tab interes_ext

INTERES_EXT |      Freq.     Percent        Cum.
------------+-----------------------------------
          2 |     18,716      100.00      100.00
------------+-----------------------------------
      Total |     18,716      100.00

gen interes_int=interes_ext_monto	 		    if interes_ext_moneda=="DOP"
replace interes_int=interes_ext_monto*47.5	    if interes_ext_moneda=="USD"
replace interes_int=(interes_ext_monto*0.87)*47.5  if interes_ext_moneda=="EUR"
replace interes_int=. if interes_ext==2

gen regalos_int= monto_equiv_regalo	if recibio_regalos ==1
*/

*Módulo de remesas
******************
*En este módulo se pregunta por el monto de remesas recibido durante los ultimos 6 meses, por ello se saca el promedio.

forvalues y=1/6  {
forvalues x=1/3  {
g remesasaux`y'_`x'=mes`y'_`x'_ext_monto if mes`y'_`x'_ext_moneda=="DOP"
replace remesasaux`y'_`x'=mes`y'_`x'_ext_monto*47.5 if mes`y'_`x'_ext_moneda=="USD"
replace remesasaux`y'_`x'=(mes`y'_`x'_ext_monto*.87)*47.5 if mes`y'_`x'_ext_moneda=="EUR"
}
}
egen remesas_mes=rsum(remesasaux*_*), missing 
replace remesas_mes=. if recibio_remesa_ext1!=1 & recibio_remesa_ext2!=1 & recibio_remesa_ext3!=1
gen remesas_prom=remesas_mes/6

***************
***ylmpri_ci***
***************
*2018 AJAM, Obs. variable ymensual tiene muchos más missings que en bases anteriores (ENFT -<2016), para ver correr un: mdesc ymensual.
*Esto explica en parte porque se observa una media más baja en el ingreso laboral total
egen ylmpri_ci=rsum(ymensual comisiones propinas horasextra vacaciones bonificaciones regalia utilidades beneficios otrosbeneficios bonoantiguedad otros_pagos_ap_monto ymensualindep), missing 
replace ylmpri_ci=. if ymensual==. & comisiones==. & propinas==. & horasextra==. & vacaciones==. & bonificaciones==. & regalia==. & utilidades==. & beneficios==. & otrosbeneficios==. & bonoantiguedad==. & ymensualindep==.
replace ylmpri_ci=. if emp_ci==0
replace ylmpri_ci=0 if categopri_ci==4
label var ylmpri_ci "Ingreso laboral monetario actividad principal" 

*****************
***nrylmpri_ci***
*****************

gen nrylmpri_ci=(ylmpri_ci==. & emp_ci==1)
label var nrylmpri_ci "Id no respuesta ingreso de la actividad principal"  

****************
***ylnmpri_ci***
****************
egen ylnmpri_ci=rsum(alimentos vivienda1 transporte gasolina cellular otros), missing 
replace ylnmpri_ci=ylnmpri_ci+noremunerados if categopri_ci==4
replace ylnmpri_ci=. if alimentos==. & vivienda1==. & transporte==. & gasolina==. & cellular==. & otros==. & noremunerados==.
label var ylnmpri_ci "Ingreso laboral NO monetario actividad principal"   

***************
***ylmsec_ci***
***************
*Modificación Mayra Sáenz - Febrero 2014.
gen ylmsec_ci=ymensual2 if emp_ci==1 & cuantos_trabajos_tiene==2
replace ylmsec_ci=. if (ymensual2==99999) & emp_ci==1 
label var ylmsec_ci "Ingreso laboral monetario segunda actividad" 


****************
***ylnmsec_ci***
****************

gen ylnmsec_ci=.
label var ylnmsec_ci "Ingreso laboral NO monetario actividad secundaria"

*****************
***ylmotros_ci***
*****************

gen ylmotros_ci=.
label var ylmotros_ci "Ingreso laboral monetario de otros trabajos" 


******************
***ylnmotros_ci***
******************

gen ylnmotros_ci=.
label var ylnmotros_ci "Ingreso laboral NO monetario de otros trabajos" 


************
***ylm_ci***
************

egen ylm_ci= rsum(ylmpri_ci ylmsec_ci), missing 
replace ylm_ci=. if ylmpri_ci==. & ylmsec_ci==.
label var ylm_ci "Ingreso laboral monetario total"  


*************
***ylnm_ci***
*************

egen ylnm_ci=rsum(ylnmpri_ci ylnmsec_ci), missing 
replace ylnm_ci=. if ylnmpri_ci==. &  ylnmsec_ci==.
label var ylnm_ci "Ingreso laboral NO monetario total"  


*************
***ynlm_ci***
*************
*Se alimenta de módulo de ingresos del exterior, ver variables que no reportaron ingresos en este período
egen ynlm_ci=rsum(pension intereses alquiler remesasnales otrosing gobierno pension_int /*interes_int*/ remesas_prom dividendos), missing 
replace ynlm_ci=. if pension==. & intereses==. & alquiler==. & remesasnales==. & otrosing==. & gobierno==. & pension_int==. /*& interes_int==.*/ & remesas_prom==.
label var ynlm_ci "Ingreso no laboral monetario"  


**************
***ynlnm_ci***
**************
gen ynlnm_ci=regalos_ext_monto
replace ynlnm_ci=. if regalos_ext_monto==.
label var ynlnm_ci "Ingreso no laboral no monetario" 
                                                                                                                    
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
label var nrylmpri_ch "Hogares con algún miembro que no respondió por ingresos"


**************
*** ylm_ch ***
**************

by idh_ch, sort: egen ylm_ch=sum(ylm_ci) if miembros_ci==1, missing
label var ylm_ch "Ingreso laboral monetario del hogar"

***************
*** ylnm_ch ***
***************

by idh_ch, sort: egen ylnm_ch=sum(ylnm_ci) if miembros_ci==1, missing
label var ylnm_ch "Ingreso laboral no monetario del hogar"

*****************************************************************
*identificador de top-code del ingreso de la actividad principal*
*****************************************************************

gen tcylmpri_ci=.
**************************************************
*Identificador de los hogares en donde (top code)*
**************************************************
gen tcylmpri_ch=.

****************
*** ylmnr_ch ***
****************

by idh_ch, sort: egen ylmnr_ch=sum(ylm_ci) if miembros_ci==1, missing
replace ylmnr_ch=. if nrylmpri_ch==1
label var ylmnr_ch "Ingreso laboral monetario del hogar"

***************
*** ynlm_ch ***
***************

by idh_ch, sort: egen ynlm_ch=sum(ynlm_ci) if miembros_ci==1, missing
label var ynlm_ch "Ingreso no laboral monetario del hogar"


**************
***ynlnm_ch***
**************
by idh_ch, sort: egen ynlnm_ch=sum(ynlnm_ci) if miembros_ci==1, missing
label var ynlnm_ch "Ingreso no laboral no monetario del hogar"
********
***NA***
********
gen autocons_ci=.
label var autocons_ci "Autoconsumo reportado por el individuo"

gen autocons_ch=.
label var autocons_ch "Autoconsumo reportado por el hogar"

****************
***remesas_ci***
****************

*Aqui se toma el valor mensual de las remesas

gen remesas_ci=remesas_prom
label var remesas_ci "Remesas mensuales reportadas por el individuo" 


****************
***remesas_ch***
****************

*Aqui se toma el valor mensual de las remesas

by idh_ch, sort: egen remesas_ch=sum(remesas_ci) if miembros_ci==1, missing
label var remesas_ch "Remesas mensuales del hogar" 

*****************
***ylhopri_ci ***
*****************

gen ylmhopri_ci=ylmpri_ci/(horaspri_ci*4.3)
label var ylmhopri_ci "Salario monetario de la actividad principal" 


***************
***ylmho_ci ***
***************

gen ylmho_ci=ylm_ci/(horastot_ci*4.3)
label var ylmho_ci "Salario monetario de todas las actividades" 

	****************************
	***VARIABLES DE EDUCACION***
	****************************

*************
***aedu_ci*** 
*************

gen	 aedu_ci=.

label define nivel 1 "Pre-escolar" 2 "Primario" 3 "Secundario" 4 "Secundario-técnico" 5 "Universitario" 6 "Post-grado" 7 "Maestria" 8 "Doctorado" 9 "Ninguno" 10 "Quisqueya Aprende" 99 "Otro"
label values nivel_ultimo_ano_aprobado nivel 
label values nivel_se_matriculo nivel

replace aedu_ci= 0 if nivel_ultimo_ano_aprobado==1  
replace aedu_ci= 0 if nivel_ultimo_ano_aprobado==9
replace aedu_ci= 0 if nivel_ultimo_ano_aprobado==10
replace aedu_ci= . if inlist(nivel_ultimo_ano_aprobado, 47, 58, 82)
replace aedu_ci= ultimo_ano_aprobado if nivel_ultimo_ano_aprobado==2 
replace aedu_ci = ultimo_ano_aprobado+8 if nivel_ultimo_ano_aprobado == 3 | nivel_ultimo_ano_aprobado == 4  
replace aedu_ci = ultimo_ano_aprobado+12 if nivel_ultimo_ano_aprobado == 5  
replace aedu_ci = ultimo_ano_aprobado+12+4 if nivel_ultimo_ano_aprobado==6 | nivel_ultimo_ano_aprobado==7 
replace aedu_ci = ultimo_ano_aprobado+12+4+2 if nivel_ultimo_ano_aprobado==8 
replace aedu_ci=.  if nivel_ultimo_ano_aprobado==.

label var aedu_ci "Anios de educacion aprobados" 

**************
***eduno_ci***
**************

gen byte eduno_ci=aedu_ci==0
replace eduno_ci=. if aedu_ci==.
label variable eduno_ci "Sin educacion"


**************
***edupi_ci***
**************

gen byte edupi_ci=(aedu_ci>=1 & aedu_ci<6) 
replace edupi_ci=. if aedu_ci==. 
label variable edupi_ci "Primaria incompleta"


**************
***edupc_ci***
**************

gen byte edupc_ci=aedu_ci==6
replace edupc_ci=. if aedu_ci==.
label variable edupc_ci "Primaria completa"


**************
***edusi_ci***
**************

gen byte edusi_ci=(aedu_ci>=7 & aedu_ci<12) 
replace edusi_ci=. if aedu_ci==.
label variable edusi_ci "Secundaria incompleta"


**************
***edusc_ci***
**************

gen byte edusc_ci=aedu_ci==12 
replace edusc_ci=. if aedu_ci==.
label variable edusc_ci "Secundaria completa"


**************
***eduui_ci***
**************

gen byte eduui_ci=aedu_ci>12 & aedu_ci<16 
replace eduui_ci=. if aedu_ci==.
label variable eduui_ci "Universitaria incompleta"

***************
***eduuc_ci****
***************

gen byte eduuc_ci=aedu_ci>=16 // mas de 16 todos
replace eduuc_ci=. if aedu_ci==. 
label variable eduuc_ci "Universitaria completa o mas"

***************
***edus1i_ci***
***************

gen byte edus1i_ci=aedu_ci>6 & aedu_ci<8 
replace edus1i_ci=. if aedu_ci==. 
label variable edus1i_ci "1er ciclo de la secundaria incompleto"

***************
***edus1c_ci***
***************

gen byte edus1c_ci=aedu_ci==8
replace edus1c_ci=. if aedu_ci==. 
label variable edus1c_ci "1er ciclo de la secundaria completo"

***************
***edus2i_ci***
***************

gen byte edus2i_ci=aedu_ci>8 & aedu_ci<12 
replace edus2i_ci=. if aedu_ci==.
label variable edus2i_ci "2do ciclo de la secundaria incompleto"

***************
***edus2c_ci***
***************

gen byte edus2c_ci=aedu_ci==12 
replace edus2c_ci=. if aedu_ci==. 
label variable edus2c_ci "2do ciclo de la secundaria completo"


***************
***edupre_ci***
***************

gen byte edupre_ci=.
label variable edupre_ci "Educacion preescolar completa"

****************
***asispre_ci***
****************
g asispre_ci= 1 if nivel_se_matriculo==1 & asiste_centro_educativo ==1
replace asispre_ci=0 if nivel_se_matriculo!=1 & asiste_centro_educativo ==1
label variable asispre_ci "Asistencia a Educacion preescolar"
	
**************
***eduac_ci***
**************
gen byte eduac_ci=.  
label variable eduac_ci "Superior universitario vs superior no universitario"


***************
***asiste_ci***
***************

generat asiste_ci=1 if asiste_centro_educativo ==1 
replace asiste_ci=0 if asiste_centro_educativo ==2
replace asiste_ci=. if asiste_centro_educativo ==.
label variable asiste_ci "Asiste actualmente a la escuela"

**************
***pqnoasis***
**************
*ver que labels cambiaron en esta nueva encuesta
gen pqnoasis_ci=porque_no_estudia
label var pqnoasis_ci "Razones para no asistir a la escuela"
label def pqnoasis_ci 1"En espera del inicio de un nuevo período" 2"Finalizó sus estudios" 3"Muy lejos" 4"Le fue mal" 5"Nunca lo inscribieron" 6"No tiene documentos" 7"El trabajo no se lo permite" 8"Muy caro" 9"Por incapacidad física o mental" 10"Por edad" 11"Razones familiares" 12"No quiere / No le gusta" 99 "Otra"
label value  pqnoasis_ci pqnoasis_ci

**************
*pqnoasis1_ci*
**************
g		pqnoasis1_ci = .						
replace pqnoasis1_ci = 1 if porque_no_estudia==8
replace pqnoasis1_ci = 2 if porque_no_estudia==7
replace pqnoasis1_ci = 3 if porque_no_estudia==9  | porque_no_estudia==11
replace pqnoasis1_ci = 4 if porque_no_estudia==12
replace pqnoasis1_ci = 6 if porque_no_estudia==2
replace pqnoasis1_ci = 7 if porque_no_estudia==10 
replace pqnoasis1_ci = 8 if porque_no_estudia==3
replace pqnoasis1_ci = 9 if porque_no_estudia==4  | porque_no_estudia==5 | porque_no_estudia==6

label define pqnoasis1_ci 1 "Problemas económicos" 2 "Por trabajo" 3 "Problemas familiares o de salud" 4 "Falta de interés" 5	"Quehaceres domésticos/embarazo/cuidado de niños/as" 6 "Terminó sus estudios" 7	"Edad" 8 "Problemas de acceso"  9 "Otros"
label value  pqnoasis1_ci pqnoasis1_ci

***************
***repite_ci***
***************

gen repite_ci=.
label var repite_ci "Ha repetido al menos un grado"


******************
***repiteult_ci***
******************

gen repiteult_ci=.
label var repiteult "Ha repetido el último grado"


***************
***edupub_ci***
***************
gen edupub_ci=.
replace edupub_ci=1 if tipo_centro_estudios==3 & asiste_centro_educativo==1 //publico
replace edupub_ci=0 if tipo_centro_estudios==1 & asiste_centro_educativo==1 // privado
replace edupub_ci=0 if tipo_centro_estudios==2 & asiste_centro_educativo==1 // semiprivado
label var edupub_ci "Asiste a un centro de enseñanza público"



**********************************
**** VARIABLES DE LA VIVIENDA ****
**********************************

****************
***aguared_ch***
****************
generate aguared_ch =.
replace aguared_ch = 1 if tiene_agua_red_publica==1 
replace aguared_ch = 0 if tiene_agua_red_publica==2
la var aguared_ch "Acceso a fuente de agua por red"

*****************
*aguafconsumo_ch*
*****************
*se asume por el cuestionario y por los datos que agua para consumo es agua de red,
gen aguafconsumo_ch = 0



*****************
*aguafuente_ch*
*****************
gen aguafuente_ch = 1 if (donde_proviene_agua==1 | donde_proviene_agua==2)
replace aguafuente_ch = 2 if (donde_proviene_agua==3 | donde_proviene_agua==4 | donde_proviene_agua==5)
replace aguafuente_ch = 5 if donde_proviene_agua==7
replace aguafuente_ch = 6 if donde_proviene_agua==9
replace aguafuente_ch = 10 if (donde_proviene_agua==6 | donde_proviene_agua==8 | donde_proviene_agua==99)


*************
*aguadist_ch*
*************
gen aguadist_ch=0
replace aguadist_ch=1 if donde_proviene_agua==1
replace aguadist_ch=2 if donde_proviene_agua==2
replace aguadist_ch=3 if donde_proviene_agua>2 & donde_proviene_agua!=99


**************
*aguadisp1_ch*
**************
gen aguadisp1_ch =9

**************
*aguadisp2_ch*
**************
gen aguadisp2_ch = 9
*label var aguadisp2_ch "= 9 la encuesta no pregunta si el servicio de agua es constante"


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
*label var aguamejorada_ch "= 1 si la fuente de agua es mejorada"

*****************
***aguamide_ch***
*****************
gen aguamide_ch =.
label var aguamide_ch "Usan medidor para pagar consumo de agua"


*****************
*bano_ch         *  Altered
*****************
gen bano_ch=.
replace bano_ch=0 if tipo_sanitario==5
replace bano_ch=1 if (tipo_sanitario==1 | tipo_sanitario==2) & se_encuentra_conectada_a==2
replace bano_ch=2 if (tipo_sanitario==1 | tipo_sanitario==2) & se_encuentra_conectada_a==1
replace bano_ch=3 if (tipo_sanitario==3 | tipo_sanitario==4)

***************
***banoex_ch***
***************
generate banoex_ch=9
replace banoex_ch= 1 if (tipo_sanitario==1 | tipo_sanitario==3)
replace banoex_ch= 0 if (tipo_sanitario==2 | tipo_sanitario==4)
la var banoex_ch "El servicio sanitario es exclusivo del hogar"


*****************
*banomejorado_ch*  Altered
*****************
gen banomejorado_ch= 2

replace banomejorado_ch =1 if bano_ch<=3

replace banomejorado_ch =0 if bano_ch>=4 & bano_ch!=6


************
*sinbano_ch*
************
gen sinbano_ch = 3
replace sinbano_ch = 0 if tipo_sanitario!=5

*label var sinbano_ch "= 0 si tiene baño en la vivienda o dentro del terreno"

*************
*aguatrat_ch*
*************
gen aguatrat_ch = 9
*label var aguatrat_ch "= 9 la encuesta no pregunta de si se trata el agua antes de consumirla"




************
***luz_ch***
************

gen luz_ch=0
replace luz_ch=1 if tipo_alumbrado==1 | tipo_alumbrado==2 | tipo_alumbrado==3 | tipo_alumbrado==6  //*2017, 6 es panel solar
* 2015, 05 modif LC se incorporó en valor cero opciones 4, y 5 que se refieren a gas, *2017, 7 a vela.
replace luz_ch=0 if tipo_alumbrado==99 | tipo_alumbrado==4 | tipo_alumbrado==5 | tipo_alumbrado==7
label var luz_ch  "La principal fuente de iluminación es electricidad"

****************
***luzmide_ch***
****************

gen luzmide_ch=.
label var luzmide_ch "Usan medidor para pagar consumo de electricidad"


****************
***combust_ch***
****************

gen combust_ch=0
replace combust_ch=1 if combustible_para_cocinar==1 | combustible_para_cocinar==3 | combustible_para_cocinar==2
*2015, 5 se incorporó las opciones 4 (leña) y 5 (carbón)
replace combust_ch=0 if combustible_para_cocinar==99 | combustible_para_cocinar==4 | combustible_para_cocinar==5
label var combust_ch "Principal combustible gas o electricidad" 


*************
***des1_ch***
*************
*AJAM 2018, opciones de respuesta cambiaron, y tipo de conexión se indica en nueva variable, y se vuelve igual a la variable bano_ch
gen des1_ch=.
replace des1_ch=0 if bano_ch==0
replace des1_ch=1 if tipo_sanitario==1 | tipo_sanitario==2
label var des1_ch "Tipo de desague según unimproved de MDG"
label def des1_ch 0"No tiene servicio sanitario" 1"Conectado a red general o cámara séptica"
label def des1_ch 2"Letrina o conectado a pozo ciego", add
label val des1_ch des1_ch

*************
***des2_ch***
*************
*idem a comentario anterior
gen des2_ch=.
replace des2_ch=0 if bano_ch==0
replace des2_ch=1 if bano_ch==1
label var des2_ch "Tipo de desague sin incluir definición MDG"
label def des2_ch 0"No tiene servicio sanitario" 1"Conectado a red general, cámara séptica, pozo o letrina"
label def des2_ch 2"Cualquier otro caso", add
label val des2_ch des2_ch

*************
***material_piso_ch***
*************

gen piso_ch=1
replace piso_ch=0 if material_piso==3 | material_piso==9 
replace piso_ch = 2 if material_piso==99
label var piso_ch "Materiales de construcción del material_piso"  
label def piso_ch 0"material_piso de tierra" 1"Materiales permanentes" 2"otros materiales"
label val piso_ch piso_ch

**************
***pared_ch***
**************

gen pared_ch=1 
replace pared_ch=0 if material_pared_exterior==3 | material_pared_exterior==11 | material_pared_exterior==12 | material_pared_exterior==13
replace pared_ch=2 if material_pared_exterior==99
label var pared_ch "Materiales de construcción de las paredes"
label def pared_ch 0"No permanentes" 1"Permanentes" 2"otros materiales"
label val pared_ch pared_ch


**************
***material_techo_ch***
**************

gen techo_ch=1
replace techo_ch=0 if material_techo==3 | material_techo==5
replace techo_ch=2 if material_techo==99
label var techo_ch "Materiales de construcción del material_techo"
label def techo_ch 0"No permanentes" 1"Permanentes" 2"otros materiales"
label val techo_ch techo_ch

**************
***resid_ch***
**************

gen resid_ch =0    if como_elimina_basura==1 | como_elimina_basura==2 | como_elimina_basura==3
replace resid_ch=1 if como_elimina_basura==4
replace resid_ch=2 if como_elimina_basura==5 | como_elimina_basura==6 | como_elimina_basura==7
replace resid_ch=3 if como_elimina_basura==99
label var resid_ch "Método de eliminación de residuos"
label def resid_ch 0"Recolección pública o privada" 1"Quemados o enterrados"
label def resid_ch 2"Tirados a un espacio abierto" 3"Otros", add
label val resid_ch resid_ch


*************
***dorm_ch***
*************
*Hay hogares que reportan no tener cuartos exclusivamente para dormir y cuentan con un solo espacio. Para estas 
*observaciones se cambia el 0 que tienen por 1. Porque aunque no sea exclusivo tienen un espacio para dormitorio

gen dorm_ch=cant_dormitorios_vivienda
replace dorm_ch=1 if cant_dormitorios_vivienda==0
label var dorm_ch "Habitaciones para dormir"

****************
***cuartos_ch***
****************

gen cuartos_ch=cant_cuartos_vivienda
label var cuartos_ch "Habitaciones en el hogar"
 
***************
***cocina_ch***
***************

gen cocina_ch=.
label var cocina_ch "Cuarto separado y exclusivo para cocinar"

**************
***telef_ch***
**************
gen telef_ch=0
replace telef_ch=1 if telefono==1
replace telef_ch=. if telefono==.
label var telef_ch "El hogar tiene servicio telefónico fijo"

***************
***refrig_ch***
***************

gen refrig_ch=0
replace refrig_ch=1 if  refrigerador==1
replace refrig_ch=. if  refrigerador==.
label var refrig_ch "El hogar posee refrigerador o heladera"


**************
***freez_ch***
**************

gen freez_ch=.
label var freez_ch "El hogar posee congelador"

*************
***auto_ch***
*************

gen auto_ch=0
replace auto_ch=1 if automovil==1
replace auto_ch=. if automovil==.
label var auto_ch "El hogar posee automovil particular"

**************
***compu_ch***
**************

gen compu_ch=0
replace compu_ch=1 if computador==1
replace compu_ch=. if computador==.
label var compu_ch "El hogar posee computador"

*****************
***internet_ch***
*****************

gen internet_ch=0
replace internet_ch=1 if internet==1
replace internet_ch=. if internet==.
label var internet_ch "El hogar posee conexión a Internet"

************
***cel_ch***
************

gen cel_ch=0
replace cel_ch=1 if celular==1
replace cel_ch=. if celular==.
label var cel_ch "El hogar tiene servicio telefonico celular"

**************
***vivi1_ch***
**************

gen vivi1_ch=1 if tipo_vivienda==1 | tipo_vivienda==2 | tipo_vivienda==3
replace vivi1_ch=2 if tipo_vivienda==4 | tipo_vivienda==5 
replace vivi1_ch=3 if tipo_vivienda==6 | tipo_vivienda==7 | tipo_vivienda==8 
replace vivi1_ch=. if tipo_vivienda==99
label var vivi1_ch "Tipo de vivienda en la que reside el hogar"
label def vivi1_ch 1"Casa" 2"Departamento" 3"Otros"
label val vivi1_ch vivi1_ch

*************
***vivi2_ch***
*************

gen vivi2_ch=0
replace vivi2_ch=1 if vivi1_ch==1 | vivi1_ch==2
replace vivi2_ch=. if vivi1_ch==.
label var vivi2_ch "La vivienda es casa o departamento"

*****************
***viviprop_ch***
*****************
*2015, revisar herencia si no deberia ir como propia
gen viviprop_ch=0 if tenencia_vivienda==9
replace viviprop_ch=1 if tenencia_vivienda==1 | tenencia_vivienda==5
replace viviprop_ch=2 if tenencia_vivienda==2 | tenencia_vivienda==3
replace viviprop_ch=3 if tenencia_vivienda==4 | tenencia_vivienda==6 | tenencia_vivienda==7
replace viviprop_ch=. if tenencia_vivienda==8
label var viviprop_ch "Propiedad de la vivienda"
label def viviprop_ch 0"Alquilada" 1"Propia" 2"Propia en proceso de pago"
label def viviprop_ch 3"Ocupada (propia de facto)", add
label val viviprop_ch viviprop_ch

****************
***vivitit_ch***
****************

gen vivitit_ch=.
label var vivitit_ch "El hogar posee un título de propiedad"


****************
***vivialq_ch***
****************
g monto_alquiler=(monto_alquiler_dolares_viv*47.5)+monto_alquiler_pesos_viv if tenencia_vivienda==9
gen vivialq_ch = . if tenencia_vivienda==9
replace vivialq_ch=monto_alquiler if periodo_pago_alquiler_viv==2
replace vivialq_ch=monto_alquiler*4.3   if periodo_pago_alq==1
replace vivialq_ch=monto_alquiler*2   if periodo_pago_alq==3
replace vivialq_ch=monto_alquiler/12  if periodo_pago_alq==4
replace vivialq_ch=. if monto_alquiler==0
label var vivialq_ch "Alquiler mensual"

*******************
***vivialqimp_ch***
*******************
gen vivialqimp_ch=monto_alquilaria_vivienda_mes
label var vivialqimp_ch "Alquiler mensual imputado"

*Modificación Mayra Sáenz - Febrero 2014
gen rentaimp_ch= vivialqimp_ch
label var rentaimp_ch "Rentas imputadas del hogar"

/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/

*********
*lp_ci***
*********

gen lp_ci =.
replace lp_ci =  5033.9 if zona_c==1
replace lp_ci =  4482 if zona_c==0
label var lp_ci "Linea de pobreza oficial del pais"

*********
*lpe_ci***
*********
gen lpe_ci =.
replace lpe_ci = 2266.6   if zona_c==1
replace lpe_ci =  2171.8 if zona_c==0
label var lpe_ci "Linea de indigencia oficial del pais"

****************
*cotizando_ci***
****************
gen cotizando_ci=.
label var cotizando_ci "Cotizante a la Seguridad Social"

****************
*afiliado_ci****
****************
gen afiliado_ci=.	
replace afiliado_ci =1 if afiliado_afp_princ==1 
*replace afiliado_ci =1 if eft_afiliado_afp_princ==1 | (eft_afiliado_seguro_salud >=1 & eft_afiliado_seguro_salud <=3)
recode afiliado_ci .=0
label var afiliado_ci "Afiliado a la Seguridad Social"

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

****************
*instpen_ci*****
****************
gen instpen_ci=.
label var instpen_ci "Institucion proveedora de la pension - variable original de cada pais" 


*****************
*tipocontrato_ci*
*****************
/*2018, Alvaro A. Obs. la tasa de respuesta de trabajadores con contrato es muy alta, 99%. Esto crea un salto en serie SIMS para esta variable
En encuestas anteriores se preguntaba si había firmado contrato, con una tasas de respuesta positiva de 61% de la muestra

 tab tiene_contrato

TIENE_CONTR |
        ATO |      Freq.     Percent        Cum.
------------+-----------------------------------
          1 |      4,806       99.75       99.75
          2 |         12        0.25      100.00
------------+-----------------------------------
      Total |      4,818      100.00
*/
*Modificacion MGD 06/13/2014

gen tipocontrato_ci=. 
replace tipocontrato_ci=1 if (tiene_contrato==1 & tipo_contrato==1) & categopri_ci==3
replace tipocontrato_ci=2 if (tiene_contrato==1 & (tipo_contrato==2 | tipo_contrato==3)) & categopri_ci==3
replace tipocontrato_ci=3 if (tiene_contrato==2 | contrato_verbal_escrito==2 | tipocontrato_ci==.) & categopri_ci==3
label var tipocontrato_ci "Tipo de contrato segun su duracion"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci

**********
*busca_ci* 
**********
gen busca_ci=0 if condocup==2
replace busca_ci=1 if busco_trabajo_establ_negocio==1 
label var busca_ci "Busco trabajo la semana anterior"
label define busca_ci 0 "No" 1 "Si"
label value busca_ci busca_ci

*************
*cesante_ci* 
*************
gen cesante_ci=1 if trabajo_antes==1
replace cesante_ci=0 if trabajo_antes==2
label var cesante_ci "Desocupado - definicion oficial del pais"	

*************
*tamemp_ci***
*************
/*variable total_personas_trabajan_emp
1 1 a 10 personas
2 de 11 a 19 personas
3 de 20 a 30 personas
4 de 31 a 50 personas
5 de 51 a 99
6 de 100 a 99
7 No sabe
*/
gen tamemp_ci=1 if cantidad_personas_trabajan_emp>0 & cantidad_personas_trabajan_emp<=5
replace tamemp_ci=2 if (cantidad_personas_trabajan_emp>=6 & cantidad_personas_trabajan_emp<=10) | total_personas_trabajan_emp==2
replace tamemp_ci=3 if total_personas_trabajan_emp>=3 & total_personas_trabajan_emp!=. & total_personas_trabajan_emp!=98

/*
gen tamemp_ci=1 if cant_pers_trab>0 & cant_pers_trab<=5
replace tamemp_ci=2 if cant_pers_trab>5 & cant_pers_trab<=50
replace tamemp_ci=3 if cant_pers_trab>50 & cant_pers_trab!=.*/
label var tamemp_ci "# empleados en la empresa segun rangos"
label define tamemp_ci 1 "Pequena" 2 "Mediana" 3 "Grande"
label value tamemp_ci tamemp_ci

*************
**pension_ci*
*************
gen pension_ci=1 if pension_nac_monto!=0 & pension_nac_monto!=.
recode pension_ci .=0 
label var pension_ci "1=Recibe pension contributiva"

*************
*ypen_ci*
*************

*Modificado Mayra Sáenz -Febrero 2014
/*
gen ypen_ci=pension_nac_monto
recode ypen_ci .=0
label var ypen_ci "Valor de la pension contributiva"
*/

*gen ypen_ci=pension_nac_monto if recibio_ing_pension_mes ==1

* 2014, 02 vuelvo a hacer modificacion sobre cambio de Mayra. MLO

gen ypen_ci=pension_nac_monto if pension_nac==1
label var ypen_ci "Valor de la pension contributiva"

***************
*pensionsub_ci*
***************
*DZ Octubre 2017-Se crea la variable pension subsidiada*
gen pensionsub_ci= (ps_apoyo_adultos_mayores==1)
label var pensionsub_ci "1=recibe pension subsidiada / no contributiva"

*****************
**ypensub_ci*
*****************
*DZ Octubre 2017-Se crea la variable valor de la pension subsidiada*
gen ypensub_ci=gob_proteccion_vejez_monto
replace ypensub_ci=. if gob_proteccion_vejez_monto==0
label var ypensub_ci "Valor de la pension subsidiada / no contributiva"

*************
**salmm_ci***
*************
*Salario se ajusta cada dos años, se ajustó en 2017
***Para empresas com menos de RD$2 millones en activos, ver: http://mt.gob.do/images/docs/acuerdos_y_convenios/tarifayresolucion0517/resolucion.pdf
gen salmm_ci=9411.6
label var salmm_ci "Salario minimo legal"



*******************
***categoinac_ci*** 
*******************
gen categoinac_ci =1 if ((motivo_no_busca_trabajo==10) & condocup_ci==3)
replace categoinac_ci = 2 if  (motivo_no_busca_trabajo==7 & condocup_ci==3)
replace categoinac_ci = 3 if  (motivo_no_busca_trabajo==8 & condocup_ci==3)
replace categoinac_ci = 4 if  ((categoinac_ci ~=1 & categoinac_ci ~=2 & categoinac_ci ~=3) & condocup_ci==3)
label var categoinac_ci "Categoría de inactividad"
label define categoinac_ci 1 "jubilados o pensionados" 2 "Estudiantes" 3 "Quehaceres domésticos" 4 "Otros" 
label value categoinac_ci categoinac_ci

*******************
***formal***
*******************
capture drop formal
gen formal=1 if cotizando_ci==1

replace formal=1 if afiliado_ci==1 & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1 & pais_c=="BOL"   /* si se usa afiliado, se restringiendo a ocupados solamente*/
replace formal=1 if afiliado_ci==1 & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1 & pais_c=="CRI"
replace formal=1 if afiliado_ci==1 & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1 & pais_c=="GTM" & anio_c>1998
replace formal=1 if afiliado_ci==1 & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1 & pais_c=="PAN"
replace formal=1 if afiliado_ci==1 & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1 & pais_c=="PRY" & anio_c<=2006
replace formal=1 if afiliado_ci==1 & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1 & pais_c=="DOM"
replace formal=1 if afiliado_ci==1 & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1 & pais_c=="MEX" & anio_c>=2008

gen byte formal_ci=.
replace formal_ci=1 if formal==1 & (condocup_ci==1 | condocup_ci==2)
replace formal_ci=0 if formal_ci==. & (condocup_ci==1 | condocup_ci==2) 
label var formal_ci "1=afiliado o cotizante / PEA"
* Formalidad sin restringir a PEA
g formal_1=afiliado_ci

*******************
*** benefdes_ci ***
*******************

g benefdes_ci=0 if desemp_ci==1
replace benefdes_ci=1 if  recibio_cesantia==1 & desemp_ci==1
label var benefdes_ci "=1 si tiene seguro de desempleo"

*******************
*** ybenefdes_ci***
*******************
*Encuesta no muestra monto de cesantía
*g ybenefdes_ci=monto_cesantia if benefdes_ci==1
*label var ybenefdes_ci "Monto de seguro de desempleo"

******************************
*** VARIABLES DE MIGRACION ***
******************************

* Variables incluidas por SCL/MIG Fernando Morales

	*******************
	*** migrante_ci ***
	*******************
	
	gen migrante_ci=(pais_nacimiento!=647 & pais_nacimiento!=.)
	label var migrante_ci "=1 si es migrante"
	
	**********************
	*** migantiguo5_ci ***
	**********************
	
	gen migantiguo5_ci=.
	label var migantiguo5_ci "=1 si es migrante antiguo (5 anos o mas)"
		
	**********************
	*** migrantelac_ci ***
	**********************
	
	gen migrantelac_ci=(migrante_ci==1 & inlist(pais_nacimiento,63,77,83,88,97,105,169,196,211,239,242,317,325,341,345,391,493,580,586,589,770,810,845,850)) if migrante_ci!=.
	label var migrantelac_ci "=1 si es migrante proveniente de un pais LAC"
	/* Codigos obtenidos de la carpeta de docs originales de DOM de 2005, archivo llamado Diccionario ENFT Octubre 2005 */
	
	**********************
	*** migrantiguo5_ci ***
	**********************
	
	gen migrantiguo5_ci=.
	label var migrantiguo5_ci "=1 si es migrante antiguo (5 anos o mas)"
		
	**********************
	*** miglac_ci ***
	**********************
	
	gen miglac_ci=(migrante_ci==1 & inlist(pais_nacimiento,63,77,83,88,97,105,169,196,211,239,242,317,325,341,345,391,493,580,586,589,770,810,845,850)) if migrante_ci!=.
	replace miglac_ci = 0 if !inlist(pais_nacimiento,63,77,83,88,97,105,169,196,211,239,242,317,325,341,345,391,493,580,586,589,770,810,845,850) & migrante_ci==1
	replace miglac_ci = . if migrante_ci==0 
	label var migrantelac_ci "=1 si es migrante proveniente de un pais LAC"	

	
	************************** 
	** PROVINCIAS ************
	**************************

   gen ine01=.   
   replace ine01=1  if  id_provincia==1			    /*Distrito Nacional*/
   replace ine01=2  if  id_provincia==2				/*Azua*/
   replace ine01=3  if  id_provincia==3				/*Bahoruco*/
   replace ine01=4  if  id_provincia==4				/*Barahona*/
   replace ine01=5  if  id_provincia==5		    	/*Dajabon*/
   replace ine01=6  if  id_provincia==6				/*Duarte*/
   replace ine01=7  if  id_provincia==7				/*Elias Piña*/
   replace ine01=8  if  id_provincia==8				/*El Seibo*/
   replace ine01=9  if  id_provincia==9				/*Espaillat*/
   replace ine01=10 if  id_provincia==10			/*Independencia*/
   replace ine01=11 if  id_provincia==11			/*La Altagracia*/
   replace ine01=12 if  id_provincia==12			/*La Romana*/
   replace ine01=13 if  id_provincia==13			/*La Vega*/
   replace ine01=14 if  id_provincia==14			/*Maria Trinidad Sanchez*/
   replace ine01=15 if  id_provincia==15			/*Monte Cristi*/
   replace ine01=16 if  id_provincia==16			/*Pedernales*/
   replace ine01=17 if  id_provincia==17			/*Peravia*/
   replace ine01=18 if  id_provincia==18			/*Puerto Plata*/
   replace ine01=19 if  id_provincia==19			/*Salcedo*/
   replace ine01=20 if  id_provincia==20		    /*Samana*/
   replace ine01=21 if  id_provincia==21			/*San Cristobal*/
   replace ine01=22 if  id_provincia==22			/*San Juan*/
   replace ine01=23 if  id_provincia==23			/*San Pedro De Macoris*/
   replace ine01=24 if  id_provincia==24			/*Sanchez Ramirez*/
   replace ine01=25 if  id_provincia==25			/*Santiago*/
   replace ine01=26 if  id_provincia==26			/*Santiago Rodriguez*/
   replace ine01=27 if  id_provincia==27			/*Valverde*/
   replace ine01=28 if  id_provincia==28			/*Monseñor Nouel*/
   replace ine01=29 if  id_provincia==29			/*Monte Plata*/
   replace ine01=30 if  id_provincia==30			/*Hato Mayor*/
   replace ine01=31 if  id_provincia==31			/*San Jose De Ocoa*/
   replace ine01=32 if  id_provincia==32			/*Santo Domingo*/

	label define ine01 1"Distrito Nacional" 2"Azua" 3"Bahoruco" 4"Barahona" 5"Dajabon" 6"Duarte" 7"Elias Piña" 8"El Seibo" 9"Espaillat" 10"Independencia" 11"La Altagracia" 12"La Romana" 13"La Vega" 14"Maria Trinidad Sanchez" 15"Monte Cristi" 16"Pedernales" 17"Peravia" 18"Puerto Plata" 19"Salcedo" 20"Samana" 21"San Cristobal" 22"San Juan" 23"San Pedro De Macoris" 24"Sanchez Ramirez" 25"Santiago" 26"Santiago Rodriguez" 27"Valverde" 28"Monseñor Nouel" 29"Monte Plata" 30"Hato Mayor" 31"San Jose De Ocoa" 32"Santo Domingo"
	label value ine01 ine01
	label var ine01 " Primera division politico-administrativa, Provincia"
	
	
******************************
* Variables SPH - PMTC y PNC *
******************************

* PTMC:  Comer primero (ps_comer_es_p gob_comer_pri)
*		 Incentivo de asistencia escoalr (ps_incentivo_ gob_inc_asis_)
* 		 Bono de estudiante (bono_estudiante_progreso gob_bono_estu)
* PNC: 	 Apoyo de adultos mayores (gob_proteccio ps_apoyo_adul)
* 		 Bonos (ps_bono_luz ps_bono_gas gob_bono_luz_ gob_bonogas_h)

* Ingreso del hogar
egen ingreso_total = rowtotal(ylm_ci ylnm_ci ynlm_ci ynlnm_ci), missing
bys idh_ch: egen y_hog = sum(ingreso_total)

* Personas que reciben transferencias monetarias condicionadas
egen aux = rowtotal(gob_comer_primero_monto gob_inc_asis_escolar_monto ///
			gob_bono_estudiante_prog_monto),m
bys idh_ch: egen ing_ptmc = sum(aux)
drop aux

gen ptmc_ci=(ps_comer_es_primero==1) | (ps_incentivo_asist_escolar==1) ///
| (bono_estudiante_progreso==1)

bys idh_ch: egen ptmc_ch=max(ptmc_ci)
replace ing_ptmc=. if y_hog==.

* Adultos mayores 
gen mayor64_ci=(edad>64 & edad!=.)

* PNC
bys idh_ch: egen ing_pension = sum(gob_proteccion_vejez_monto)
replace ing_pension=. if y_hog==.
gen pnc_ci=(ps_apoyo_adultos_mayores==1)

*ingreso neto del hogar
gen y_pc     = y_hog / nmiembros_ch 
gen y_pc_net = (y_hog - ing_ptmc - ing_pension) / nmiembros_ch


lab def ptmc_ch 1 "Beneficiario PTMC" 0 "No beneficiario PTMC"
lab val ptmc_ch ptmc_ch

lab def pnc_ci 1 "Beneficiario PNC" 0 "No beneficiario PNC"
lab val pnc_ci pnc_ci
	
/*_____________________________________________________________________________________________________*/
* Asignación de etiquetas e inserción de variables externas: tipo de cambio, Indice de Precios al 
* Consumidor (2011=100), Paridad de Poder Adquisitivo (PPA 2011),  líneas de pobreza
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

order region_BID_c region_c pais_c anio_c mes_c zona_c factor_ch idh_ch	idp_ci	factor_ci sexo_ci edad_ci upm_ci estrato_ci ///
afroind_ci afroind_ch afroind_ano_c dis_ci dis_ch relacion_ci civil_ci jefe_ci nconyuges_ch nhijos_ch notropari_ch notronopari_ch nempdom_ch ///
clasehog_ch nmiembros_ch miembros_ci nmayor21_ch nmenor21_ch nmayor65_ch nmenor6_ch	nmenor1_ch	condocup_ci ///
categoinac_ci nempleos_ci emp_ci antiguedad_ci	desemp_ci cesante_ci durades_ci	pea_ci desalent_ci subemp_ci ///
tiempoparc_ci categopri_ci categosec_ci rama_ci spublico_ci tamemp_ci cotizando_ci instcot_ci	afiliado_ci ///
formal_ci tipocontrato_ci ocupa_ci horaspri_ci horastot_ci	pensionsub_ci pension_ci tipopen_ci instpen_ci	ylmpri_ci nrylmpri_ci ///
tcylmpri_ci ylnmpri_ci ylmsec_ci ylnmsec_ci	ylmotros_ci	ylnmotros_ci ylm_ci	ylnm_ci	ynlm_ci	ynlnm_ci ylm_ch	ylnm_ch	ylmnr_ch  ///
ynlm_ch	ynlnm_ch ylmhopri_ci ylmho_ci rentaimp_ch autocons_ci autocons_ch nrylmpri_ch tcylmpri_ch remesas_ci remesas_ch	ypen_ci	ypensub_ci ///
salmm_ci tc_c ipc_c lp19_c lp31_c lp5_c lp_ci lpe_ci aedu_ci eduno_ci edupi_ci edupc_ci	edusi_ci edusc_ci eduui_ci eduuc_ci	edus1i_ci ///
edus1c_ci edus2i_ci edus2c_ci edupre_ci eduac_ci asiste_ci pqnoasis_ci pqnoasis1_ci	repite_ci repiteult_ci edupub_ci  ///
aguared_ch aguafconsumo_ch aguafuente_ch aguadist_ch aguadisp1_ch aguadisp2_ch aguamala_ch aguamejorada_ch aguamide_ch bano_ch banoex_ch banomejorado_ch sinbano_ch aguatrat_ch luz_ch luzmide_ch combust_ch des1_ch des2_ch piso_ch  ///
pared_ch techo_ch resid_ch dorm_ch cuartos_ch cocina_ch telef_ch refrig_ch freez_ch auto_ch compu_ch internet_ch cel_ch ///
vivi1_ch vivi2_ch viviprop_ch vivitit_ch vivialq_ch	vivialqimp_ch migrante_ci migantiguo5_ci migrantelac_ci, first


compress


saveold "`base_out'", version(12) replace


log close



