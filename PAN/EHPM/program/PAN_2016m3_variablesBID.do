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

local PAIS PAN
local ENCUESTA EHPM
local ANO "2016"
local ronda m3

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
   

capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Panama
Encuesta: EHPM
Round: Marzo
Autores: 
Versión 2018:Daniela Zuluaga (DZG) - Email: danielazu@iadb.org, da.zuluaga@hotmail.com
Última versión: Cesar Lins (SCL/GDI) - Marzo 2021

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

*** LABELS

label var	llave_sec	"Llave secuencial única de unión entre Vivienda – Hogar – Persona"
label var	prov	"	Provincia	"
label var	cuest	"	Cuestionario/uno para cada hogar	"
label var	hogar	"	Hogar número	"
label var	nper	"	Número de la persona	"
label var	p1	"	Parentesco	"
label var	p1a	"	Otro pariente	"
label var	p2	"	Sexo	"
label var	p3	"	Edad	"
label var	p4_ssocial	"	¿Tiene ud. Seguro social?	"
label var	p4a_ss_fic	"	Tiene usted la ficha que le da acceso a recibir los servicios del Seguro Social?	"
label var	p4b	"	¿Está inscrito en el registro civil?	"
label var	p4c	"	¿Por qué motivo no está inscrito?	"
label var	p4c_otro_e	"	Otro motivo por el que no está inscrito	"
label var	p4d	"	¿Se considera usted indígena?	"
label var	p4e	"	¿A qué grupo indígena pertenece?	"
label var	p4e_otro	"	Otro grupo indígena al que pertenece	"
label var	p4f	"	¿Se considera usted negro(a) o afrodescendiente?	"
label var	p4g	"	¿Se considera...	"
label var	p4g_otro	"	Otro afrodescendiente	"
*label var	p4h	"	¿En qué provincia o comarca indígena vivía su madre cuando usted nació?	"
*label var	p4i	"	¿En qué período llegó usted a vivir a panamá?	"
*label var	p4i_anio	"	¿En qué año llegó?	"
*label var	p4j	"	¿En qué provincia o comarca indígena vivía usted en marzo del año pasado?	"
label var	p4k	"¿Está usted afiliado a algún fondo privado de pensión o jubilación?"
label var	p4l	"	¿Cuál es su estado conyugal actual?	"
label var p4m_hijos "¿Cuántos hijos(as) nacidos(as) vivos(as) ha tenido en los últimos 10 años? "
label var p4n "Alguno(a) de ellos(as), falleció antes de cumplir los 5 años de edad"
label var p4n_cuanto "Cuántos fallecieron?"
label var	p5	"	¿Asiste a la escuela?	"
label var	p5_tipo	"	Tipo de escuela:	"
label var	p5_asistio	"	Alguna vez asistió?	"
label var	p5a	"	¿Motivo no asiste?	"
label var	p5a_otro_m	"	Otro Motivo de la no asistencia	"
label var	p5b	"	¿Con qué regularidad asiste a la escuela?	"
label var	p6	"	¿Qué grado aprobó?	"
label var	p7	"	¿Sabe leer y escribir?	"
label var	p7_titulo	"	¿Diploma o título?	"
label var	p7b	"	¿Usted ha repetido algún grado o año escolar?	"
label var	p7b_cuanto	"	¿Cuántas veces ha repetido?	"
label var	p08_16	"	Condición de actividad	"
label var	p11_meses	"	Meses	"
label var	p08_16b_ot	"	Otros inactivos	"
label var	p17	"	¿Piensa buscar trabajo en los próximos 6 meses?	"
label var	p18	"	¿…No estuvo buscando, ni piensa buscar…?	"
label var	p18a	"	Otro de la opción j de la pregunta P20	"
label var	p19	"	¿Cuánto tiempo hace que estuvo o está trabajo…?	"
label var	p20	"	¿Hizo alguna gestión para conseguir empleo...?	"
label var	p21	"	¿Qué gestión hizo...?	"
label var	p21a_otro	"	Otro de la opción j de la pregunta P21	"
label var	p22a	"	¿Estuvo disponible para trabajar durante...las dos semanas anteriores?	"
label var	p22b	"	¿Estuvo disponible para trabajar durante... actualmente?	"
label var	p22c	"	¿Estuvo disponible para trabajar durante... las próximas dos semanas?	"
label var	p23	"	¿Para qué tipo de trabajo…?	"
label var	p24	"	¿Cuánto tiempo hace…?	"
label var	p25	"	¿Motivo dejó su último..?	"
label var	p25_otro	"	Otro de la pregunta P25	"
label var	p27	"	¿Dónde trabaja o trabajó...?	"
label var	p27a_otro	"	Otros de la pregunta P27	"
label var	p29	"	¿Cuántas personas trabajan..?	"
label var	p29a	"	Especifique	"
label var	p30	"	¿Cuántos... eran empleados..	"
label var	p31	"	¿Dónde usted trabaja... lo hizo como...?	"
label var	p32	"	¿Es o era empleado...?	"
label var	p33_sit	"	Provincia – DISTRITO – CORREGIMIENTO	"
label var	p34	"	¿Tiempo de trabajar en esa empresa...?	"
label var	p35	"	¿Trabajó ud. Por sueldo fijo? Sí = 1/No = 2	"
label var	p351	"	Por día?	"
label var	p352	"	Por tarea?	"
label var	p353	"	Por hora?	"
label var	p354	"	Días	"
label var	p355	"	Tareas	"
label var	p356	"	Horas	"
label var	p361	"	Salario en efectivo	"
label var	p362	"	Salario en especie	"
label var	p362a	"	Tipo de especie	"
label var	p363	"	Ingreso... independiente...	"
label var	p364	"	Ingreso... especie...	"
label var	p364a_tipo	"	Tipo de especie	"
label var	p365	"	Autoconsumo o autosuministro	"
label var	p37	"	¿Cuántas horas trabajó la semana..?	"
label var	p38	"	¿Tuvo algún otro trabajo..?	"
label var	p39c	"	¿Dónde usted trabaja... lo hizo como...?	"
label var	p39d	"	¿Tiempo de trabajar en esa empresa... ?	"
label var	p39e	"	¿Cuántas horas trabajó la semana pasada en su otro trabajo..?	"
label var	p39	"	¿Ingreso mensual en el otro trabajo?	"
label var	p40	"	¿En los últimos 6 meses utilizó un teléfono móvil para uso personal?"
label var	p41	"	¿En los últimos 6 meses utilizó alguna computadora?	"
label var	p44	"	¿En los últimos 6 meses utilizó internet?	"
label var	p72a	"	Jubilación?...	"
label var	p72b	"	Pensión?	"
label var	p72c1	"	Pensión alimenticia?...	"
label var	p72c2	"	Dinero?...	"
label var	p72c3	"	Alimentación escolar..?	"
label var	p72c4	"	Alimentos..?	"
label var	p72c5	"	Artículos escolares?	"
label var	p72c7	"	Ropa/Calzado	"
label var	p72c8	"	Regalos	"
label var	p72c6	"	Otros..?	"
label var	p72c6_otro	"	¿Cuál otro?	"
label var	p72d	"	Alquileres, rentas..?...	"
label var	p72e	"	Premios de lotería...?...	"
label var	p72f1	"	Becas ?... Institución pública	"
label var	p72f2	"	Becas ?... Universal	"
label var	p72f3	"	Becas ?... Institución privada	"
label var	p72f4	"	Becas ? Otras	"
label var	p72g1	"	Subsidios..? Transferencia	"
label var	p72g2	"	Subsidios..? SENAPAN	"
label var	p72g3	"	Subsidios..? Suplementos	"
label var	p72g4	"	Subsidios..? Insumos agropecuarios	"
label var	p72g5	"	Subsidios..? 100 a los 70	"
label var	p72g6	"	Ángel guardián	"
label var	p72h	"	¿Décimo tercer mes?...	"
label var	p72i	"	¿Ingresos agropecuarios?	"
*label var	p72j	"	¿Parvis mejorado?	"
label var	p72k	"	¿Asistencia habitacional..?	"
label var	p72l	"	¿Otros ingresos..?	"
label var	p72m	"	Sin ninguno de estos ingresos?	"
label var	areareco	"	Urbano = “1”  / rural = “2”	"
label var	fac15_e	"	Factor de expansión de 15  y más/menos 15 años de edad.	"
label var	indi_rec	"	No indígena = 1/Indígena = 2	"
label var	div_panama	"	División de las provincias de Panamá y Panamá Oeste	"
label var	pea_nea	"PEA= Pobl Económicamente Activa NEA =Pobl No económicamente Activa"
label var	desagreg	"	Condición en la actividad económica desagregada de la PEA:	"
label var ocu_des " Condición en la actividad económica desagregada de la PEA: "
label var p24_reco "Tiempo de haber realizado su último trabajo:"
label var	p26reco	"	Ocupación principal a 1 dígito:	"
label var	p28reco	"	Categoría en la actividad económica principal a 1 dígito.	"
*label var	p3_reco	"	Recodificación de edades	"
label var	grado_ap	"	Grado aprobado:	"
label var	horas	"	Horas trabajadas:	"
label var	salario	"	Salario de los empleados:	"
label var	ingreso	"	Ingreso de los Ocupados:	"
*label var	p26_3dig	"	Ocupación principal a 3 dígitos.	"

label var	v0_condici	"Condición de la vivienda	"
label var	v0_cond_ot	"Otra condición de la vivienda	"
label var	v1a_tipo_d	"Tipo de vivienda	"
label var	v1b_tenenc	"¿La vivienda que habita este hogar es…?	"
label var	v1b_pago_m	"Pago mensual en B/.	"
label var	v1b_otra_t	"Otra?  Especifique…	"
label var	v1c_pagari	"¿Si tuviera que pagar alquiler por la vivienda…?	"
label var	v1d_materi	"¿De qué material es la mayor parte de las paredes exteriores del edificio"
label var	v1e_materi	"¿De qué material es la mayor parte del techo del edificio o casa?	"
label var	v1f_materi	"¿De qué material es la mayor parte del piso de esta vivienda?	"
label var	v1g_cuarto	"¿Cuántos cuartos tiene la vivienda?	"
label var	v1h_dormit	"De estos: ¿cuántos cuartos son sólo para dormir?	"
label var	v1i_agua_b	"¿De dónde obtienen principalmente el agua para beber?	"
label var	v1i_otra	"Otra ubicación	"
label var	v1i_pago_a	"Pago mensual por servicio de agua	"
label var	v1j_ubicac	"¿Las plumas o las llaves de agua de esta vivienda están ubicadas…	"
label var	v1k_servic	"¿Tiene esta vivienda servicio sanitario...	"
label var	v1l_uso_sa	"¿El uso del servicio sanitario es...	"
label var	v1m_basura	"¿Cómo eliminan la basura en esta vivienda?	"
label var	v1m_otra	"Otra forma de eliminar la basura	"
label var	v1m_pago_b	"Pago mensual por servicio de basura	"
label var	v1n_combus	"¿Qué combustible utilizan con más frecuencia para cocinar…	"
label var	v1o_luz	"Qué alumbrado tiene la vivienda	"
label var	v1o_otro	"Otro tipo de alumbrado.	"
label var	v1o_pago_l	"Cuánto paga en luz regularmente al mes?	"
label var	cuantos_ho	"¿Cuántos hogares residen en esta vivienda?	"
label var	fac15_e	"	Factor de expansión.	"

label var	h2b_radio	"	¿Radio…?	"
label var	h2c_telefo	"	¿Teléfono residencial…?	"
label var	h2d_celula	"	¿Teléfono celular…?	"
label var	h2e_televi	"	¿Televisor…?	"
label var	h2e1_tv_cu	"	¿Cuántos TV...?	"
label var	h2e2_cable	"	Señal de Cable TV	"
label var	h4_computa	"	¿Computadora…?	"
label var	h4a_pc	"	Computador de mesa	"
label var	h4b_portat	"	Computador portátil (incluidos laptops, netbooks	"
label var	h4c_tablet	"	Tablets?	"
label var	h5_in_mov	"	¿Internet móvil...?	"
label var	h5_in_fija	"	¿Internet fija...?	"
label var	h5aa_no_ne	"Porqué este hogar no tiene acceso internet? No necesita internet"
label var	h5ab_otro	"Porqué este hogar no tiene acceso internet? Accesan a Internet en otro lugar?"
label var	h5ac_confi	"Porqué este hogar no tiene acceso internet? Falta de confianza,..?"
label var	h5ad_costo	"Porqué este hogar no tiene acceso internet? Costo del equipo "
label var	h5ae_priva	"Porqué este hogar no tiene acceso internet? privacidad"
label var	h5af_servi	"Porqué este hogar no tiene acceso internet? no disponible"
label var	h5ag_otros	"Porqué este hogar no tiene acceso internet? Otros?	"
label var	h5a_otra_r	"Porqué este hogar no tiene acceso internet?	"
label var	h7_transfe	"¿Red de oportunidades…?"
label var	h7a_anio    "¿Desde que año recibe…?"
label var	h7b_act_re	"¿Actualmente lo recibe la red de oportunidades?"
label var	h7c_anio_n 	"¿Desde qué año no recibe la red de oportunidades?"
label var	tot_per	"Total de personas."
label var	tot_perh	"	Total de personas, sexo masculino.	"
label var	tot_perm	"	Total de personas, sexo femenino.	"
label var	tot_e10	"	Total de 10 y más de edad.	"
label var	per_e10h	"	Total de personas de 10 y más años, sexo masculino.	"
label var	per_e10m	"	Total de personas de 10 y más años, sexo femenino.	"
label var	tot_e15	"	Total de personas de 15 y más años de edad.	"
label var	per_e15h	"	Total de personas de 15 y más años, sexo masculino.	"
label var	per_e15m	"	Total de personas de 15 y más años, sexo femenino.	"
label var	residian	"	Personas que residían agosto del año pasado	"
*label var	fac_hog_e	"	Factor de expansión de 15  y más/menos 15 años de edad.	"


******************************************************************************
*	HOUSEHOLD and DEMOGRAPHIC VARIABLES
******************************************************************************

************
* region_c *
************

destring prov, replace
gen region_c=  prov

label define region_c  ///
1	"Bocas del Toro" ///
2	"Coclé" ///
3	"Colón" ///
4	"Chiriquí" ///
5	"Darién" ///
6	"Herrera" ///
7	"Los Santos" ///
8	"Panamá" ///
9	"Veraguas" ///
10	"Kuna Yala" ///
11	"Emberá" ///
12	"Ngäbe-Buglé"		  
label value region_c region_c
label var region_c "División política, provincias"

ta region_c 
******************************
*	factor_ci
******************************

gen factor_ci= fac15_e   
label var factor_ci "Factor de expansion del individuo"

	***************
	***upm_ci***
	***************
gen upm_ci=. 

	***************
	***estrato_ci***
	***************
gen estrato_ci=.


******************************
*	idh_ch
******************************
sort llave_sec hogar nper
egen idh_ch = group(llave_sec hogar)
label var idh_ch "ID del hogar"


******************************
*	idp_ci
******************************

destring nper, replace
gen idp_ci = nper
label var idp_ci "ID de la persona en el hogar"

******************************
*	relacion_ci
******************************
gen relacion_ci=.
replace relacion_ci=1 if p1==1
replace relacion_ci=2 if p1==2
replace relacion_ci=3 if p1==3
replace relacion_ci=4 if p1==4 
replace relacion_ci=5 if p1==6
replace relacion_ci=6 if p1==5
label var relacion_ci "Relacion con el jefe del hogar"
label define relacion_ci 1 "Jefe/a" 2 "Esposo/a" 3 "Hijo/a" 4 "Otros parientes" 5 "Otros no parientes" 6 "Empleado/a domestico/a"
label value relacion_ci relacion_ci

ta relacion_ci [w=factor_ci]
******************************
*	factor_ch
******************************
gen factorjefe=factor_ci if relacion_ci==1
by idh_ch, sort: egen factor_ch=sum(factorjefe)
label var factor_ch "Factor de expansion del hogar"
drop factorjefe

******************************
*	zona_c
******************************

encode areareco, gen(area)

gen zona_c=0 if area==1
replace zona_c=1 if area==2
label var zona_c "Zona del pais"
label define zona_c 1 "Urban" 0 "Rural"
label value zona_c zona_c

******************************
*	pais_c
******************************
gen str3 pais_c="PAN"
label var pais_c "Pais"

******************************
*	anio_c
******************************
gen anio_c=2016
label var anio_c "Year of the survey"

******************************
*	mes_c
******************************
gen mes_c=3
label var mes_c "Month of the survey"
label value mes_c mes_c

******************************
*	sexo_ci
******************************
gen sexo_ci=p2
label define sexo_ci 1 "Hombre" 2 "Mujer"
label value sexo_ci sexo_ci

******************************
*	edad_ci
******************************
gen edad_ci=p3
label var edad_ci "Edad del individuo"


******************************
*	civil_ci
******************************

gen civil_ci=.
*MGR: no se pregunta en 2014 la condicion civil en el cuestionario
label var civil_ci "Estado civil"
label define civil_ci 1 "Soltero" 2 "Union formal o informal" 3 "Divorciado o separado" 4 "Viudo"
label value civil_ci estcivil_ci

******************************
*	jefe_ci
******************************
gen jefe_ci=(relacion_ci==1)
label var jefe_ci "Jefe de hogar"

***************************************************************************
*	nconyuges_ch & nhijos_ch & notropari_ch & notronopari_ch & nempdom_ch
****************************************************************************
by idh_ch, sort: egen nconyuges_ch=sum(relacion_ci==2)
by idh_ch, sort: egen nhijos_ch=sum(relacion_ci==3)
by idh_ch, sort: egen notropari_ch=sum(relacion_ci==4)
by idh_ch, sort: egen notronopari_ch=sum(relacion_ci==5)
by idh_ch, sort: egen nempdom_ch=sum(relacion_ci==6)
label var nconyuges_ch "Numero de conyuges"
label var nhijos_ch "Numero de hijos"
label var notropari_ch "Numero de otros familiares"
label var notronopari_ch "Numero de no familiares"
label var nempdom_ch "Numero de empleados domesticos"

******************************
*	clasehog_ch
******************************
gen clasehog_ch=.
replace clasehog_ch=1 if nhijos_ch==0 & nconyuges_ch==0 & notropari_ch==0 & notronopari_ch==0 
replace clasehog_ch=2 if nhijos_ch>0 & notropari_ch==0 & notronopari_ch==0 
replace clasehog_ch=2 if nconyuges_ch>0 & notropari_ch==0 & notronopari_ch==0 
replace clasehog_ch=3 if ((clasehog_ch ==2 & notropari_ch>0) & notronopari_ch==0) |(notropari_ch>0 & notronopari_ch==0)   
replace clasehog_ch=4 if (nconyuges_ch>0 | nhijos_ch>0 | (notropari_ch>0 & notropari_ch<.)) & (notronopari_ch>0 & notronopari_ch<.)
replace clasehog_ch=5 if nhijos_ch==0 & nconyuges_ch==0 & notropari_ch==0 & notronopari_ch>0 & notronopari_ch<.
label var clasehog_ch "Tipo de hogar"
label define clasehog_ch 1 "Unipersonal" 2 "Nuclear" 3 "Ampliado" 4 "Compuesto" 5 "Corresidente"
label value clasehog_ch clasehog_ch

***************************************************************************************
*	nmiembros_ch & nmayor21_ch & nmenor21_ch & nmayor65_ch & nmenor6_ch & nmenor1_ch  
***************************************************************************************
by idh_ch, sort: egen nmiembros_ch=sum(relacion_ci>=1 & relacion_ci<=4)
label var nmiembros_ch "Numero de familiares en el hogar"

by idh_ch, sort: egen nmayor21_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad_ci>=21)
label var nmayor21_ch "Numero de familiares mayores a 21 anios"

by idh_ch, sort: egen nmenor21_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad_ci<21)
label var nmenor21_ch "Numero de familiares menores a 21 anios"

by idh_ch, sort: egen nmayor65_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad_ci>=65)
label var nmayor65_ch "Numero de familiares mayores a 65 anios"

by idh_ch, sort: egen nmenor6_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad_ci<6)
label var nmenor6_ch "Numero de familiares menores a 6 anios"

by idh_ch, sort: egen nmenor1_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad_ci<1)
label var nmenor1_ch "Numero de familiares menores a 1 anio"

******************************
*	miembros_ci
******************************
gen miembros_ci=(relacion_ci<5)
label var miembros_ci "Miembro del hogar"

	
*******************************************************
***           VARIABLES DE DIVERSIDAD               ***
*******************************************************				
* Maria Antonella Pereira & Nathalia Maya - Marzo 2021	

	***************
	*** afroind_ci ***
	***************
**Pregunta: ¿Se considera usted indígena? (p4d) ¿Se considera usted negro(a) o afrodescendiente? (p4f) 1- SI, 2- NO
**Puedes reportar más de una identidad por lo que hay una población afroindigena. Estos se consideran afrodescendientes puesto que la muestra total de afros es mayor.

gen afroind_ci=. 
replace afroind_ci=1 if p4d==1 
replace afroind_ci=2 if p4f==1 
replace afroind_ci=3 if p4d==2 & p4f==2


	***************
	*** afroind_ch ***
	***************
gen afroind_jefe= afroind_ci if relacion_ci==1
egen afroind_ch  = min(afroind_jefe), by(idh_ch) 

drop afroind_jefe

	*******************
	*** afroind_ano_c ***
	*******************
gen afroind_ano_c=2015


	*******************
	*** dis_ci ***
	*******************
gen dis_ci=. 


	*******************
	*** dis_ch ***
	*******************
gen dis_ch=. 


******************************************************************************
*	LABOR MARKET
******************************************************************************

****************
****condocup_ci*
****************
/*
* Alternativa 2: condicionado a la busqueda de empleo. MGD 06/06/2014
gen condocup_ci=.
replace condocup_ci=1 if p8_16 >= 1 & p8_16 <= 5 
replace condocup_ci=2 if  (p8_16>=6 & p8_16<=9) 
recode condocup_ci .=3 if  edad_ci>=10
recode condocup_ci .=4 if  edad_ci<10
label var condocup_ci "Condicion de ocupación de acuerdo a def de cada pais"
label define condocup_ci 1 "Ocupado" 2 "Desocupado" 3 "Inactivo" 4 "Menor de PET" 
label value condocup_ci condocup_ci
*/


encode ocu_des, gen (ocup_deso)
gen condocup_ci=.
replace condocup_ci=1 if ocup_deso==2 
replace condocup_ci=2 if ocup_deso==1 
recode condocup_ci .=3 if  edad_ci>=10
recode condocup_ci .=4 if  edad_ci<10
label var condocup_ci "Condicion de ocupación de acuerdo a def de cada pais"
label define condocup_ci 1 "Ocupado" 2 "Desocupado" 3 "Inactivo" 4 "Menor de PET" 
label value condocup_ci condocup_ci

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

******************************
*	horaspri_ci
******************************
gen horaspri_ci=p37 if p37>0 & p37<99
replace horaspri_ci=. if emp_ci==0
label var horaspri_ci "Hs totales (semanales) trabajadas en act. principal"

******************************
*	horastot_ci
******************************
egen horastot_ci=rsum(p37 p39e) if p39e>0 & p39e<99, missing
replace horastot_ci=horaspri_ci if p39e==99 | p39e==0
replace horastot_ci=. if (p37==0 & p39e==0) | (p37==99 | p39e==99)| emp_ci==0
label var horastot_ci "Hs totales (semanales)trabajadas en toda actividad"

******************************
*	desalent_ci
******************************

generat desalent_ci = 1  if  p8_16==10
replace desalent_ci = 0  if p8_16!=10
replace desalent_ci =.   if p8_16==.

******************************
*	subemp_ci
******************************
gen subemp_ci=.
* no esta la pregunta si desea trabajar mas horas en el cuestionario
/*
gen subemp_ci=(emp_ci==1 & p50==1 & horastot_ci<=30)
replace subemp_ci=. if emp_ci==0 | emp_ci==.
*/
******************************
*	tiempoparc_ci
******************************
gen tiempoparc_ci=.
/*gen tiempoparc_ci=(emp_ci==1 & p50==2 & horastot_ci<=30)
replace tiempoparc_ci=. if emp_ci==0 | emp_ci==.*/
label var tiempoparc_ci "Trabajan menos de 30 hs semanales y no quieren trabajar mas"


******************************************************************************
*		LABOR DEMAND
******************************************************************************

******************************
*	ocupa_ci
******************************

destring p26reco, replace

g ocupa_ci=.
replace ocupa_ci=1 if (p26reco==2 | p26reco==3) & emp_ci==1
replace ocupa_ci=2 if (p26reco==1) & emp_ci==1
replace ocupa_ci=3 if (p26reco==4) & emp_ci==1
replace ocupa_ci=4 if (p26reco==5) & emp_ci==1
*replace ocupa_ci=5 if (p26reco==5) & emp_ci==1 /*trabajadores de servicios y vendedores están en la misma categoría por lo que lo incluyo en la 4*/
replace ocupa_ci=6 if (p26reco==6) & emp_ci==1
replace ocupa_ci=7 if (p26reco==8) & emp_ci==1
*replace ocupa_ci=8  /*pregunta no incluye categoría de "Fuerzas armadas"*/
replace ocupa_ci=9 if (p26reco==7 | p26reco==9) & emp_ci==1

******************************
*	rama_ci
******************************

destring p28reco, replace
gen rama_ci=. 
replace rama_ci=1 if (p28reco==1)  & emp_ci==1
replace rama_ci=2 if (p28reco==2) & emp_ci==1
replace rama_ci=3 if (p28reco==3) & emp_ci==1 
replace rama_ci=4 if (p28reco==4 | p28reco==5) & emp_ci==1
replace rama_ci=5 if (p28reco==6) & emp_ci==1
replace rama_ci=6 if (p28reco==7 | p28reco==9)& emp_ci==1
replace rama_ci=7 if (p28reco==8) & emp_ci==1
replace rama_ci=8 if (p28reco==11 | p28reco==12) & emp_ci==1
replace rama_ci=9 if (p28reco==10 | (p28reco>=13 & p28reco<=21)) & emp_ci==1

label var rama_ci "Rama actividad principal"
label define rama_ci 1 "Agricultura, caza, silvicultura y pesca" 2 "Explotación de minas y canteras" 3 "Industrias manufactureras" 4 "Electricidad, gas y agua" 5 "Construcción" 6 "Comercio al por mayor y menor, restaurantes, hoteles" 7 "Transporte y almacenamiento" 8 "Establecimientos financieros, seguros, bienes inmuebles" 9 "Servicios sociales, comunales y personales"
label values rama_ci rama_ci

******************************
*	categopri_ci
******************************
gen categopri_ci=0     if                      emp_ci==1
replace categopri_ci=1 if p31==8             & emp_ci==1
replace categopri_ci=2 if (p31==7 | p31==9)  & emp_ci==1
replace categopri_ci=3 if (p31>=1 & p31<=6 ) & emp_ci==1
*puse Miembro de una cooperativa de produccion (p33==9)dentro de cuenta propia
replace categopri_ci=4 if p31==10            & emp_ci==1
label var categopri_ci "Categoria ocupacional en la actividad principal"
label define categopri_ci 1 "Patron" 2 "Cuenta Propia" 3 "Empleado" 4 "Trabajador no remunerado"
label value categopri_ci categopri_ci

******************************
*	categosec_ci
******************************
destring p39c, replace
gen categosec_ci=.
replace categosec_ci=1 if p39c==8             & emp_ci==1
replace categosec_ci=2 if (p39c==7 | p39c==9)  & emp_ci==1
replace categosec_ci=3 if (p39c>=1 & p39c<=5 ) & emp_ci==1
*puse Miembro de una cooperativa de produccion (p33==9)dentro de cuenta propia
replace categosec_ci=4 if p39c==10            & emp_ci==1

label define categosec_ci 1 "Patron" 2 "Cuenta Propia" 3 "Empleado" 4 "Familiar no remunerado" 
label value categosec_ci categosec_ci
label var categosec_ci "Categoria ocupacional en la actividad secundaria"

******************************
*	nempleos_ci
******************************
destring p38, replace
gen nempleos_ci=0     if emp_ci==1
replace nempleos_ci=1 if p38==3
replace nempleos_ci=2 if p38==1 | p38==2

******************************
*	spublico_ci
******************************
gen spublico_ci=0 if emp_ci==1
replace spublico_ci=1 if p31==1 & emp_ci==1
replace spublico_ci=. if emp_ci==0
label var spublico_ci "Trabaja en sector publico"

******************************
*	durades_ci
******************************
*  MGD: para el codigo p19=100 que se refiere a menos de un mes, se codifica como 0.5 meses
destring p19, replace
recode p19 298=. 299=.
gen durades_ci=. 
replace durades_ci=0.5 if p19==100
replace durades_ci=p19-200 if p19>=201 /*& p21<298*/

label var durades_ci "Duracion del desempleo en meses"

******************************
*	antiguedad_ci
******************************
destring p34, replace force
gen m=p34-100 if p34>=100 & p34<=111
gen a=p34-200 if p34>=201 & p34<=299 

gen antiguedad_ci=.
replace antiguedad_ci=a
replace antiguedad_ci=m/12 if a==.
drop a m

******************************************************************************
*		INCOME
******************************************************************************
foreach var of varlist _all {
qui destring `var', replace
qui capture recode `var' (99999=.) (999=.) (9999=.) (99998=.) (999998=.) (999999=.) (99999.99 =.) (9999.99=.) (9999999=.)
}
******************************
*	ylmpri_ci & ylmpri1_ci
******************************
recode p361 (99999=.) (99998=.) (99997=.) (77777=.) (9999=.)
generat ylmpri_ci=p361 if p361>0 & p361<999998 & categopri_ci==3
replace ylmpri_ci=p363 if p363>0 & p363<999998 & (categopri_ci==1 | categopri_ci==2) 
replace ylmpri_ci=0    if categopri==4
replace ylmpri_ci=.    if emp_ci==0
label var ylmpri_ci "Ingreso laboral monetario act. principal (mes)"

*Parece que el salario bruto incluye los aguinaldos. No estoy segura, por eso no sumo 
*el decimo tercero al ingreso principal. Se deja una variable alternativa "ylmpri1_ci"


gen aguin=p72h if p72h>0 & p72h<999998
egen ylmpri1_ci=rsum(ylmpri_ci aguin), missing
replace ylmpri1_ci=. if ylmpri_ci==. & aguin==.
replace ylmpri1_ci=. if emp_ci==0
replace ylmpri1_ci=. if ylmpri_ci==. & (p362==0 | p362==999999)

******************************
*	nrylmpri_ci & nrylmpri1_ci
******************************
gen nrylmpri_ci=(((p361>=99998 & p361<.) | (p363>=99998 & p363<.)) & emp_ci==1)
replace nrylmpri_ci=. if emp_ci==0
label var nrylmpri_ci "Identificador de NR de ingreso"

******************************
*	ylmsec_ci
******************************
gen ylmsec_ci=p39 if p39>0 & p39<99999  
replace ylmsec_ci=. if emp_ci==0
label var ylmsec_ci "Ingreso laboral monetario act secundaria (mes)"

****************
***ylnmsec_ci***
****************

g ylnmsec_ci=.
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


******************************
*	ylm_ci & ylm1_ci
******************************
egen ylm_ci=rsum(ylmpri_ci ylmsec_ci), missing
label var ylm_ci "Ingreso laboral monetario total"

egen ylm1_ci= rsum(ylmpri1_ci ylmsec_ci), missing

******************************
*	ylnm_ci
******************************

egen ylnmpri_ci=rsum(p362 p364), missing
replace ylnmpri_ci=. if emp_ci==0
gen ylnm_ci=ylnmpri_ci
label var ylnm_ci "Ingreso laboral no monetario total"

******************************
*	ynlm_ci
******************************

sum p72a p72c1 p72c2 p72b p72d p72e p72f1 p72f2 p72f3 p72f4 p72g1 p72g2 p72g3 p72g4 p72g5 p72g6 p72i p72k p72l

gen jub=p72a if p72a>0 & p72a<999999
replace p72c1=. if p72c1>=99999
replace p72c2=. if p72c2>=99999
egen ayfam=rsum(p72c1 p72c2), missing
replace  ayfam=. if p72c1==999999 & p72c2 ==999999 

gen pension=p72b  if p72b>0 & p72b <9999999
gen alqui=p72d if p72d>0 & p72d<9999999
gen loter=p72e if p72e>0 & p72e<9999999
*gen parvis=p72j if p72j>0 & p72j<9999999
egen becas=rsum(p72f1 p72f2 p72f3 p72f4), missing

egen subsidios=rsum(p72g1 p72g2 p72g3 p72g4 p72g5 p72g6), missing
gen agro_aux=p72i if p72i>0 & p72i<999999 /*es laboral?*/
gen otroy=p72l if p72l>0 & p72l<999999
gen habit=p72k if p72k>0 & p72k<999999

egen ynlme= rsum(jub pension ayfam alqui loter becas subsidios habit otroy) if emp_ci==1, missing
egen ynlmd= rsum(jub pension ayfam alqui loter becas aguin subsidios habit otroy) if emp_ci==0, missing
egen ynlm_ci=rsum(ynlme ynlmd), missing
label var ynlm_ci "Ingreso no laboral monetario(mes)"


******************************
*	nrylmpri_ch
******************************
by idh_ch, sort: egen nrylmpri_ch=sum(nrylmpri_ci) if miembros_ci==1, missing
replace nrylmpri_ch=1 if nrylmpri_ch>0 & nrylmpri_ch<.
label var nrylmpri_ch "Identificador de hogares donde miembro NS/NR ingreso"

******************************
*	ylm_ch & ylm1_ch 
******************************
by idh_ch, sort: egen ylm_ch=sum(ylm_ci) if miembros_ci==1, missing
by idh_ch, sort: egen ylm1_ch=sum(ylm1_ci) if miembros_ci==1, missing
label var ylm_ch "Ingreso laboral monetario del Hogar-ignora NR"

****************************
*    ylmnr_ch & ylmnr1_ch  
****************************
by idh_ch, sort: egen ylmnr_ch=sum(ylm_ci) if miembros_ci==1 & nrylmpri_ch==0, missing
by idh_ch, sort: egen ylmnr1_ch=sum(ylm1_ci) if miembros_ci==1 & nrylmpri_ch==0, missing
*replace ylmnr1_ch=. if nrylmpri1_ch==1

label var ylmnr_ch "Ing laboral monetario del Hogar"

******************************
*	ylnm_ch  
******************************
by idh_ch, sort: egen ylnm_ch=sum(ylnm_ci) if miembros_ci==1, missing
label var ylnm_ch "Ing laboral no monetario del Hogar - ignora NR" 

*******************************************************
*** Ingreso no laboral no monetario (otras fuentes).***
*******************************************************

egen ynlnm_ci=rsum(p72c3 p72c4 p72c5 p72c6 p72c7 p72c8), missing
replace ynlnm_ci=. if  p72c3==999999 & p72c4==999999 & p72c5==999999 & p72c6==99999 & p72c7==99999 & p72c8==99999

label var ynlnm_ci "Ingreso no laboral no monetario"
***********************************************************
*** Ingreso no laboral no monetario del Hogar.
************************************************************
gen ynlnm_ch=.


label var ynlnm_ch "Ingreso no laboral no monetario del Hogar" 
******************************
*	remesas_ci & remesas_ch 
******************************
gen remesas_ci=.
gen remesas_ch=.

******************************
*	ynlm_ch 
******************************
by idh_ch, sort: egen ynlm_ch=sum(ynlm_ci) if miembros_ci
label var ynlm_ch "Ingreso no laboral monetario del Hogar" 

*****************************************************************
*identificador de top-code del ingreso de la actividad principal*
*****************************************************************

gen tcylmpri_ci=.
**************************************************
*Identificador de los hogares en donde (top code)*
**************************************************
gen tcylmpri_ch=.
******************************
*	autocons_ci 
******************************

gen autocons_ci=p365
label var autocons_ci "Autoconsumo reportado por el individuo"

******************************
*	autocons_ch 
******************************

by idh_ch, sort: egen autocons_ch=sum(autocons_ci) if miembros_ci

******************************
*	rentaimp_ch 
******************************
gen rentaimp_ch=.

******************************
*	ylmhopri_ci & ylmhopri1_ci
******************************
gen ylmhopri_ci=ylmpri_ci/(horaspri_ci*4.3)
label var ylmhopri_ci "Salario monetario de la actividad principal"
gen ylmhopri1_ci=ylmpri1_ci/(horaspri_ci*4.3)

******************************
*	ylmho_ci & ylm1ho_ci
******************************
gen ylmho_ci=ylm_ci/(horastot_ci*4.3)
label var ylmho_ci "Salario monetario de todas las actividades"
gen ylmho1_ci=ylm1_ci/(horastot_ci*4.3)

******************************************************************************
*	VARIABLES OF HOUSEHOLD INFRAESTRUCTURE 
*****************************************************************************


gen aguared_ch=(v1i_agua_b>=1 &  v1i_agua_b<=3)
replace aguared_ch=. if  v1i_agua_b==.

gen aguadist_ch=.
replace aguadist_ch=1 if v1j_ubicac==1
replace aguadist_ch=2 if v1j_ubicac==2
replace aguadist_ch=3 if v1j_ubicac==3 | v1j_ubicac==4

gen aguamala_ch=(v1i_agua_b==6 | v1i_agua_b==8)
replace aguamala_ch=. if v1i_agua_b==.

gen aguamide_ch=.

gen luz_ch=(v1o_luz>=1 & v1o_luz<=3)
replace luz_ch=. if v1o_luz==.

gen luzmide_ch=.

gen combust_ch=(v1n_combus==1 | v1n_combus==3)
replace combust_ch=. if v1n_combus==.

gen bano_ch=(v1k_servic>=1 & v1k_servic<=3)
replace bano_ch=. if v1k_servic==.

gen banoex_ch=(v1l_uso_sa==1)

gen des1_ch=.
replace des1_ch=0 if v1k_servic==4
replace des1_ch=1 if v1k_servic==2 | v1k_servic==3
replace des1_ch=2 if v1k_servic==1

gen des2_ch=.
replace des2_ch=0 if v1k_servic==4
replace des2_ch=1 if v1k_servic==2 | v1k_servic==3 | v1k_servic==1


gen piso_ch=.
replace piso_ch = 0 if v1f_materi==5
replace piso_ch = 1 if v1f_materi>=1 & v1f_materi<=4
replace piso_ch = 2 if v1f_materi==6

gen pared_ch=.
replace pared_ch=0 if v1d_materi==7 | v1d_materi==5
replace pared_ch=1 if v1d_materi>=1 & v1d_materi<=4
replace pared_ch=2 if v1d_materi==6


gen techo_ch=.
replace techo_ch=0 if v1e_materi==6
replace techo_ch=1 if v1e_materi>=1 &  v1e_materi<=5
replace techo_ch=2 if v1e_materi==7


gen resid_ch=. 
replace resid_ch=0 if v1m_basura==1 | v1m_basura==2
replace resid_ch=1 if v1m_basura==3 | v1m_basura==5
replace resid_ch=2 if v1m_basura==4 | v1m_basura==6 | v1m_basura==7

**Daniela Zuluaga- Enero 2018: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**
	
*********************
***aguamejorada_ch***
*********************
g       aguamejorada_ch = 1 if (v1i_agua_b >=1 & v1i_agua_b <=4) |  v1i_agua_b ==6
replace aguamejorada_ch = 0 if  v1i_agua_b ==5 | (v1i_agua_b >=7 & v1i_agua_b <=11)
		
*********************
***banomejorado_ch***
*********************
g       banomejorado_ch = 1 if ((v1k_servic >=1 &  v1k_servic <=2) & v1l_uso_sa == 1)
replace banomejorado_ch = 0 if ((v1k_servic >=1 &  v1k_servic <=2) & (v1l_uso_sa >=2 &  v1l_uso_sa <=3)) | (v1k_servic >=3 &  v1k_servic <=4)

gen dorm_ch=v1h_dormit

gen cuartos_ch=v1g_cuarto

gen cocina_ch=.

gen telef_ch= (h2c_telefo == 1)

gen refrig_ch=.

gen freez_ch=.

gen auto_ch=.

gen compu_ch=(h4_computa==1)

gen internet_ch= (h5_in_mov ==1 | h5_in_fija==1)

gen cel_ch=(h2d_celula==1)

gen vivi1_ch=.
replace vivi1_ch = 1 if (v1a_tipo_d>=1 & v1a_tipo_d<=3)
replace vivi1_ch = 2 if v1a_tipo_d==4
replace vivi1_ch = 3 if v1a_tipo_d==5 

gen vivi2_ch=(vivi1_ch==1 | vivi1_ch==2)
replace vivi2_ch=. if vivi1_ch==.

gen viviprop_ch=.
replace viviprop_ch=0 if v1b_tenenc==1
replace viviprop_ch=1 if v1b_tenenc==3
replace viviprop_ch=2 if v1b_tenenc==2
replace viviprop_ch=3 if v1b_tenenc>=4 & v1b_tenenc<=6 

gen vivitit_ch=.

gen vivialq_ch=v1b_pago_m

gen vivialqimp_ch=v1c_pagari

******************************************************************************
*	EDUCATION
******************************************************************************

******************************
*	aedu_ci
******************************

/*
generat grado=p6-10 if p6>=11 & p6<=16
replace grado=p6-20 if p6>=21 & p6<=26
replace grado=p6-30 if p6>=31 & p6<=33
replace grado=p6-40 if p6>=41 & p6<=49
replace grado=p6-50 if p6>=51 & p6<=53

replace grado=p6-60 if p6>=61 & p6<=63
replace grado=p6-70 if p6>=71 & p6<=73
replace grado=p6-80 if p6>=81 & p6<=84
replace grado=0 if p6==60

gen nivel=0 if p6==60
replace nivel=1 if p6>=11 & p6<=16
replace nivel=2 if p6>=21 & p6<=26
replace nivel=3 if p6>=31 & p6<=33
replace nivel=4 if p6>=41 & p6<=49
replace nivel=5 if p6>=51 & p6<=53

replace nivel=6 if p6>=61 & p6<=84
gen aedu_ci=0            if nivel==0 
replace aedu_ci=grado    if nivel==1
replace aedu_ci=grado+6  if nivel==2 | nivel==3
replace aedu_ci=grado+12 if nivel==4 | nivel==5
replace aedu_ci=grado+18 if nivel==6

*replace aedu_ci=0 if edad_ci<5
*/

gen grado = .
replace grado = p6 if p6>=0 & p6<=4
replace grado = p6-10 if p6>=11 & p6<=16
replace grado = p6-20 if p6>=21 & p6<=23
replace grado = p6-30 if p6>=31 & p6<=36
replace grado = p6-40 if p6>=41 & p6<=43
replace grado = p6-50 if p6>=51 & p6<=56
replace grado = p6-60 if p6>=61 & p6<=62
replace grado = p6-70 if p6>=71 & p6<=72
replace grado = p6-80 if p6>=81 & p6<=84

gen nivel = .
replace nivel = 0 if p6>=0 & p6<=4
replace nivel = 1 if p6>=11 & p6<=16
replace nivel = 2 if p6>=21 & p6<=23
replace nivel = 3 if p6>=31 & p6<=36
replace nivel = 4 if p6>=41 & p6<=43
replace nivel = 5 if p6>=51 & p6<=56
replace nivel = 6 if p6>=61 & p6<=62
replace nivel = 7 if p6>=71 & p6<=72
replace nivel = 8 if p6>=81 & p6<=84

gen aedu_ci = .
replace aedu_ci = 0 if nivel== 0 
replace aedu_ci = grado if nivel == 1
replace aedu_ci = grado+6 if nivel == 2
replace aedu_ci = grado+6 if nivel == 3
replace aedu_ci = grado+12 if nivel == 4
replace aedu_ci = grado+12 if nivel == 5
replace aedu_ci = grado+17 if nivel == 6 | nivel == 7 | nivel == 8

******************************
*	eduno_ci
******************************
gen eduno_ci=(aedu_ci==0)
replace eduno_ci=. if aedu_ci==.
label var eduno_ci "Personas sin educacion"

******************************
*	edupi_ci
******************************
gen edupi_ci=(aedu_ci>=1 & aedu_ci<6)
replace edupi_ci=. if aedu_ci==.
label var edupi_ci "Personas que no han completado Primaria"

******************************
*	edupc_ci
******************************
gen edupc_ci=(aedu_ci==6)
replace edupc_ci=. if aedu_ci==.
label var edupc_ci "Primaria Completa"

******************************
*	edusi_ci
******************************
gen edusi_ci=(aedu_ci>6 & aedu_ci<12)
replace edupc_ci=. if aedu_ci==.
label var edusi_ci "Secundaria Incompleta"

******************************
*	edusc_ci
******************************
gen edusc_ci=(aedu_ci==12)
replace edusc_ci=. if aedu_ci==.
label var edusc_ci "Secundaria Completa"

******************************
*	edus1i_ci
******************************
gen edus1i_ci=(aedu_ci>6 & aedu_ci<9)
replace edus1i_ci=. if aedu_ci==.
label var edus1i_ci "1er ciclo de Educacion Secundaria Incompleto"

******************************
*	edus1c_ci
******************************
gen edus1c_ci=(aedu_ci==9)
replace edus1c_ci=. if aedu_ci==.
label var edus1c_ci "1er ciclo de Educacion Secundaria Completo"

******************************
*	edus2i_ci
******************************
gen edus2i_ci=(aedu_ci>9 & aedu_ci<12)
replace edus2i_ci=. if aedu_ci==.
label var edus2i_ci "2do ciclo de Educacion Secundaria Incompleto"

******************************
*	edus2c_ci
******************************
gen edus2c_ci=(aedu_ci==12)
replace edus2c_ci=. if aedu_ci==.
label var edus2c_ci "2do ciclo de Educacion Secundaria Completo"
*pongo primaria y secundaria, como equivalente a basica y media

******************************
*	eduui_ci
******************************
gen eduui_ci=(aedu_ci>12 & aedu_ci<17) 
replace eduui_ci=. if aedu_ci==.
label var eduui_ci "Universitaria o Terciaria Incompleta"

******************************
*	eduuc_ci
******************************
gen eduuc_ci=(aedu_ci>=17)
replace eduuc_ci=. if aedu_ci==.
label var eduuc_ci "Universitaria o Terciaria Completa"

******************************
*	edupre_ci
******************************
gen edupre_ci=.
label var edupre_ci "Educacion preescolar"
notes: la encuesta no tiene codigo de educacion preescolar 

******************************
*	asispre_ci
******************************
gen asispre_ci=.
label var asispre_ci "Asistencia a Educacion preescolar"
notes: la encuesta no tiene codigo de educacion preescolar 

******************************
*	eduac_ci
******************************
gen eduac_ci=.
replace eduac_ci=0 if nivel==5
replace eduac_ci=1 if nivel==4
label var eduac_ci "Educ terciaria academica vs Educ terciaria no academica"

******************************
*	asiste_ci
******************************
gen asiste_ci=(p5==1)
replace asiste_ci=. if p5==.
label var asiste "Personas que actualmente asisten a centros de enseñanza"

******************************
*	pqnoasis_ci_ci
******************************
gen pqnoasis_ci=p5a if p5a>0
label var pqnoasis_ci "Razones para no asistir a la escuela"
label define pqnoasis_ci 1 "No se ofrece el nivel o grado escolar en la comunidad" 2 "Necesita trabajar",add
label define pqnoasis_ci 3 "Falta de recursos económicos" 4 "Quehaceres domesticos", add 
label define pqnoasis_ci 5 "Falta de interes" 6 "Embarazo" 7 "Enfermedad" , add
label define pqnoasis_ci 8 "No tiene la edad requerida" 9 "Está muy distante" 10 "Ya se graduó" 11 "Se casó" 12 "Otros", add
label value pqnoasis_ci pqnoasis_ci

**************
*pqnoasis1_ci*
**************
**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**

g       pqnoasis1_ci = 1 if p5a==3
replace pqnoasis1_ci = 2 if p5a==2
replace pqnoasis1_ci = 3 if p5a==7
replace pqnoasis1_ci = 4 if p5a==5
replace pqnoasis1_ci = 5 if p5a==4 | p5a==6 | p5a==11
replace pqnoasis1_ci = 6 if p5a==10
replace pqnoasis1_ci = 7 if p5a==8
replace pqnoasis1_ci = 8 if p5a==1 | p5a==9
replace pqnoasis1_ci = 9 if p5a==12

label define pqnoasis1_ci 1 "Problemas económicos" 2 "Por trabajo" 3 "Problemas familiares o de salud" 4 "Falta de interés" 5	"Quehaceres domésticos/embarazo/cuidado de niños/as" 6 "Terminó sus estudios" 7	"Edad" 8 "Problemas de acceso"  9 "Otros"
label value  pqnoasis1_ci pqnoasis1_ci


gen edupub_ci=.
label var edupub_ci "Personas que asisten a centros de ensenanza publicos"

******************************
*	repiteult_ci  & repite_ci
******************************
gen repiteult_ci=.
gen repite_ci=.
*NA
drop nivel grado

/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/
*************
**salmm_ci***
*************

* PAN 2016-2017: http://gacetas.procuraduria-admon.gob.pa/27934_2015.pdf  Se mantiene igual
* Se estima en base a tabla en general documentation - external data
gen salmm_ci= . 
replace salmm_ci= 398.40 if rama_ci==1
replace salmm_ci= 643.20 if rama_ci==2
replace salmm_ci= 465.60 if rama_ci==3
replace salmm_ci= 708.00 if rama_ci==4
replace salmm_ci= 686.40 if rama_ci==5
replace salmm_ci= 490.20 if rama_ci==6
replace salmm_ci= 595.20 if rama_ci==7
replace salmm_ci= 692.40 if rama_ci==8
replace salmm_ci= 614.40 if rama_ci==9
replace salmm_ci= 588.20 if salmm_ci==.

label var salmm_ci "Salario minimo legal"

*********
*lp_ci***
*********

* Fuente 2016: http://www.mef.gob.pa/es/informes/Documents/Pobreza%20y%20distribucion%20del%20ingreso%20-%20marzo%202016.pdf
gen lp_ci =.
replace lp_ci= 143.50 if zona_c==1 
replace lp_ci= 106.49 if zona_c==0

label var lp_ci "Linea de pobreza oficial del pais"

*********
*lpe_ci***
*********

* Fuente 2016: http://www.mef.gob.pa/es/informes/Documents/Pobreza%20y%20distribucion%20del%20ingreso%20-%20marzo%202016.pdf
gen lpe_ci =.
replace lpe_ci= 70.36 if zona_c==1 
replace lpe_ci= 59.19 if zona_c==0

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
replace afiliado_ci =1 if p4_ssocial==1  /* afiliado directo */
recode afiliado_ci .=0 if condocup_ci==1 | condocup_ci==2
label var afiliado_ci "Afiliado a la Seguridad Social"

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
gen instcot_ci=.
label var instcot_ci "Institucion proveedora de la pension - variable original de cada pais" 

*****************
*tipocontrato_ci*
*****************

gen tipocontrato_ci=.
replace tipocontrato_ci=1 if (p32==1 | p32==4) & categopri_ci==3
replace tipocontrato_ci=2 if (p32==2 | p32==3) & categopri_ci==3
replace tipocontrato_ci=3 if (p32==5 | tipocontrato_ci==.) & categopri_ci==3
label var tipocontrato_ci "Tipo de contrato segun su duracion"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci


*************
*cesante_ci* 
*************
* MGD 12/4/2015: se corrige la inclusion de ceros con p24 >100 ya que antes solamnte constaba p24!=999
gen cesante_ci=1 if p24>100 & p24!=999 & p24!=. & condocup_ci==2
recode cesante_ci .=0 if condocup_ci==2
label var cesante_ci "Desocupado - definicion oficial del pais"		

*******************
***formal***
*******************
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

*************
*tamemp_ci
*************
gen tamemp_ci=1 if p29==1 & emp_ci==1
label var  tamemp_ci "Tamaño de Empresa" 
*Empresas medianas
replace tamemp_ci=2 if (p29==2 | p29==3 | p29==4  )& emp_ci==1
*Empresas grandes
replace tamemp_ci=3 if p29==5  & emp_ci==1
label define tamaño 1"Pequeña" 2"Mediana" 3"Grande"
label values tamemp_ci tamaño
tab tamemp_ci [iw= fac15_e]

*************
*categoinac_ci
*************
gen categoinac_ci=1 if p8_16==11 | p8_16==12
label var  categoinac_ci "Condición de Inactividad" 
*Estudiantes
replace categoinac_ci=2 if p8_16==13
*Quehaceres del Hogar
replace categoinac_ci=3 if p8_16==14
*Otra razon
replace categoinac_ci=4 if p8_16==15 | p8_16==16 | p8_16==17
label define inactivo 1"Pensionado y otros" 2"Estudiante" 3"Hogar" 4"Otros"
label values categoinac_ci inactivo

*************
**pension_ci*
*************

egen aux_p=rsum(p72a p72b), missing
destring aux_p, replace
gen pension_ci=1 if aux_p>0 & aux_p!=. & aux_p!=99999
recode pension_ci .=0
label var pension_ci "1=Recibe pension contributiva"

*************
*ypen_ci*
*************
gen ypen_ci=aux_p

label var ypen_ci "Valor de la pension contributiva"

***************
*pensionsub_ci*
***************

gen pensionsub_ci=1 if p72g5>0 & p72g5<=99999
recode pensionsub_ci .=0
label var pensionsub_ci "1=recibe pension subsidiada / no contributiva"

*****************
**ypensub_ci*
*****************

gen ypensub_ci=p72g5 if p72g5>0 & p72g5<=99999
label var ypensub_ci "Valor de la pension subsidiada / no contributiva"


*************
*tecnica_ci**
*************

gen tecnica_ci=.
* falta generar
label var tecnica_ci "1=formacion terciaria tecnica"

	*******************
	*** benefdes_ci ***
	*******************

	g benefdes_ci=.
	label var benefdes_ci "=1 si tiene seguro de desempleo"

	*******************
	*** ybenefdes_ci***
	*******************
	g ybenefdes_ci=.
	label var ybenefdes_ci "Monto de seguro de desempleo"

******************************
*** VARIABLES DE MIGRACION ***
******************************

* Variables incluidas por SCL/MIG Fernando Morales

** Para la encuesta del 2016 no se pregunto sobre pais de nacimiento ni tiempo que lleva viviendo en el pais

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
	** Fuente: Los codigos de paises se obtiene del censo de panama (redatam)
	
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
salmm_ci tc_c ipc_c lp19_c lp31_c lp5_c lp_ci lpe_ci aedu_ci eduno_ci edupi_ci edupc_ci	edusi_ci edusc_ci eduui_ci eduuc_ci	edus1i_ci ///
edus1c_ci edus2i_ci edus2c_ci edupre_ci eduac_ci asiste_ci pqnoasis_ci pqnoasis1_ci	repite_ci repiteult_ci edupub_ci tecnica_ci ///
aguared_ch aguadist_ch aguamala_ch aguamide_ch luz_ch luzmide_ch combust_ch	bano_ch banoex_ch des1_ch des2_ch piso_ch aguamejorada_ch banomejorado_ch  ///
pared_ch techo_ch resid_ch dorm_ch cuartos_ch cocina_ch telef_ch refrig_ch freez_ch auto_ch compu_ch internet_ch cel_ch ///
vivi1_ch vivi2_ch viviprop_ch vivitit_ch vivialq_ch	vivialqimp_ch migrante_ci migantiguo5_ci migrantelac_ci , first




compress


saveold "`base_out'", replace v(12)


log close



