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
local ENCUESTA EH
local ANO "2009"
local ronda m8

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
   

capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Panama
Encuesta: EH
Round: Agosto
Autores: 
Versión 2012: Julio, Yessenia Loaysa (YL)
María Laura Oliveri (MLO) - Email: mloliveri@iadb.org, lauraoliveri@yahoo.com - 10 de Octubre de 2013
Mayra Sáenz - Email: mayras@iadb.org - saenzmayra.a@gmail.com - 2014
Última actualización: Cesar Lins - Marzo 2021

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

*labels

label var	prov	"Provincia"
label var	dist	"Distrito"
label var	corre	"Corregimiento"
label var	estra	"Estrato"
label var	unidad	"Unidadprimaria"
label var	cuest	"Cuestionario/unoparacadahogar"
label var	hogar	"Hogarnúmero"
label var	recorri	"Actualizacióncartográfica,recorrido."
label var	semana	"Semanaenqueseencuestó"
label var	empa	"Empadronador/Encuestador"
label var	super	"Supervisor/Coordinador"
label var	tot_per	"Totaldepersonas."
label var	tot_per_h	"Totaldepersonas,sexomasculino."
label var	tot_per_m	"Totaldepersonas,sexofemenino."
label var	tot_per_10	"Totalde10ymásdeedad."
label var	per_10_h	"Totaldepersonasde10ymásaños,sexomasculino."
label var	per_10_m	"Totaldepersonasde10ymásaños,sexofemenino."
label var	tot_per_15	"Totaldepersonasde15ymásañosdeedad."
label var	per_15_h	"Totaldepersonasde15ymásaños,sexomasculino."
label var	per_15_m	"Totaldepersonasde15ymásaños,sexofemenino."
label var	residia	"Personasqueresidíanagostodelañopasado"
label var	matri	"Matriculadosen1er.Grado"
label var	g4to	"Aprobaron4to.Grado?"
label var	fracaso	"Fracasó"
label var	recurso	"Faltaderecursos"
label var	trabajar	"Tienequetrabajar"
label var	interes	"Faltadeinterés"
label var	enfermed	"Enfermedad"
label var	otros	"Otrosmotivos"
label var	regireco	"Regiones(1,2,3,4)"
label var	areareco	"Urbano=“1”/rural=“2”"
label var	fac15_e	"Factordeexpansiónde15ymás/menos15añosde edad"
label var	indi_rec	"Noindígena=1/Indígena2"
label var	nper	"2Númerodelapersona"
*label var	nombre_ape	"Nombreyapellido"
label var	info	"Informanteenelinteriordelcuestionario(esq.sup.der.)"
label var	p1	"Parentesco"
label var	p1_1	"Otropariente"
label var	p2	"Sexo"
label var	p3	"Edad"
label var	p4	"¿Tieneud.Segurosocial?"
label var	p5	"¿Cuálessuestadoconyugalactual?"
label var	p6	"¿Dónderesidíaenagostode2008?"
label var	p7	"¿Asistealaescuelaactualmente?"
label var	p7a	"¿Motivoprincipaldenoasistir?"
label var	p7a_1	"Códigosdeotrosdelapregunta7ª"
label var	p8	"¿Gradooañomásaltoaprobó?"
label var	p9	"¿Quédiplomaotítulotiene?"
label var	p10_18	"Condicióndeactividad"
label var	p10_18a	"Meses"
label var	p10_18b	"CódigosdeOtros"
label var	p19	"¿Piensabuscartrabajoenlospróximos6meses?"
label var	p20	"¿…Noestuvobuscando,nipiensabuscar…?"
label var	p20a	"CódigodelaopciónidelapreguntaP20"
label var	p21	"¿Cuántotiempohacequeestuvooestátrabajo…?"
label var	p22	"¿Hizoalgunagestiónparaconseguirempleo...?"
label var	p23	"¿Quégestiónhizo...?"
label var	p23a	"CódigodelaopciónjdelapreguntaP23"
label var	p24	"¿Estuvodisponibleparatrabajardurantelasdossemanas…?"
label var	p25	"¿Paraquétipodetrabajoestuvooestará....?"
label var	p26	"¿Cuántotiempohace…?"
label var	p27	"¿Porquémotivodejósuúltimotrabajo…?"
label var	p27a	"CódigodeOtrodelapreguntaP27"
label var	p28_texto	"Textodelaocupación"
label var	p28	"¿Quéocupación,oficiootrabajo...?Código"
label var	p29	"¿Dóndetrabajaotrabajó...?"
label var	p29_texto	"CuandoP29=1Nombredelainstitución."
label var	p29a	"CódigosdeOtrosdelapreguntaP29"
label var	p30_texto	"¿Aquésededicaelnegocio?Texto"
label var	p30	"¿Aquésededicaelnegocio?Código"
label var	p31	"¿Cuántaspersonastrabajan..?"
label var	p31a	"Especifique"
label var	p32	"¿Cuántos...eranempleados.."
label var	p33	"¿Dóndeustedtrabaja...lohizocomo...?"
label var	p34	"¿Esoeraempleado...?"
label var	p35	"¿Laempresa...esoera..."
label var	p36	"Laempresa...teníalicencia.."
label var	p37	"¿Teníaud.Socios(as)?"
label var	p38	"¿Esaactividad,empresa..."
label var	p39	"Sitiohabitualdetrabajo.Incluyeloscódigos95-98"
label var	p40	"¿Quétiempotienedetrabajarenesenegocio...?"
label var	p41	"¿Trabajóud.Porsueldofijo...?Sí=1/No=2"
label var	p411	"Por día?"
label var	p414	"Días"
label var	p412	"Por tarea?"
label var	p415	"Tareas"
label var	p413	"Por hora?"
label var	p416	"Horas"
label var	p421	"Salarioenefectivo"
label var	p422	"Enespecie"
label var	p422_a	"Códigodeespecie"
label var	p423	"Ingreso...independiente..."
label var	p424	"Ingreso...especie..."
label var	p424_a	"Códigodeespecie"
label var	p43	"¿Cuántashorastrabajó...?"
label var	p44	"¿Tuvoalgúnotrotrabajo...?"
label var	p45_texto	"¿Ocupaciónenelotrotrabajo...?Texto"
label var	p45	"¿Ocupaciónenelotrotrabajo...?Código"
label var	p46_texto	"...negociodelotrotrabajo?Texto."
label var	p46	"...negociodelotrotrabajo?Código."
label var	p47	"¿Tiempodetrabajarenesenegocio...?"
label var	p48	"¿Cuántashoras...otrotrabajo"
label var	p49	"¿Salariooingresodelotrotrabajo...?"
label var	p50	"¿Deseabatrabajarmáshoras...?"
label var	p51	"¿Porquénotrabajómáshoras...?"
label var	p52	"¿Buscótrabajoadicional...?"
label var	p52a	"CódigodeotrosdelapreguntaP52"
label var	p53	"¿Paraquéclasedetrabajo...disponible...?"
label var	p54	"¿…trabajodisponible..?"
label var	p55a	"Jubilación?..."
label var	p55b	"Pensión?"
label var	p55c1	"Pensiónalimenticia?..."
label var	p55c2	"Dinero?..."
label var	p55c3	"Alimentaciónescolar?..."
label var	p55c4	"Alimentos?..."
label var	p55c5	"Otros"
label var	p55c5x	"Códigosdeotrosdelapreguntaanterior"
label var	p55d	"Alquileres,rentas..?..."
label var	p55e	"Premiosdelotería...?..."
label var	p55f1	"Becas?...InstituciónPublica"
label var	p55f2	"Becas?...InstituciónPrivada"
label var	p55g1	"Transferenciamonetáriacondicionada."
label var	p55g2	"SENAPAN"
label var	p55g3	"Suplementosalimentícios."
label var	p55g4	"Insumosagropecuários."
label var	p55h	"¿Décimotercermes?..."
label var	p55i	"¿Ingresosagropecuarios?"
label var	p55j	"¿Parvismejorado?"
label var	p55k	"¿AsistenciaHabitacional?"
label var	p55l	"Otrosingresos."
label var	p55m	"Sinningúningresos"
label var	areareco	"Urbano=1/rural=2"
label var	ocu_reco	"Condicióndeactividadrecodificada.Códigosdel01al50"
label var	rango15	"15ymás=1;menos15=2"
label var	fac15_e	"Factordeexpansiónde15ymás/menos15añosde"
label var	v_info	"Informantedelaportadadelcuestionario(vivienda)"
label var	v_1	"¿Laviviendaquehabitaestehogares…?"
label var	v_1a	"PagomensualenB"
label var	v_1b	"Otra?Especifique…"
label var	v_2	"¿Situvieraquepagaralquilerporla"
label var	v_3	"¿Cuántoshogaresresidenenestavivienda?"
label var	regireco	"Regiones(1,2,3,4)"
label var	areareco	"Urbano=1/rural=2"
label var div_pa "distritos"
label var divi_reco "distritos"

******************************************************************************
*	HOUSEHOLD and DEMOGRAPHIC VARIABLES
******************************************************************************
************
* Region_c *
************
*Inclusión Mayra Sáenz - Julio 2013

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

******************************
*	factor_ci
******************************

gen factor_ci= fac15_e   
label var factor_ci "Factor de expansion del individuo"

******************************
*	idh_ch
******************************
sort prov dist corre estra unidad cuest hogar
egen idh_ch = group(prov dist corre estra unidad cuest hogar)
label var idh_ch "ID del hogar"


******************************
*	idp_ci
******************************

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
destring areareco, replace 
gen zona_c=0 if areareco==2
replace zona_c=1 if areareco==1
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
gen anio_c=2009
label var anio_c "Year of the survey"

******************************
*	mes_c
******************************
gen mes_c=8
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
replace civil_ci=1 if p5==7 | p5==8
replace civil_ci=2 if p5==1 | p5==4
replace civil_ci=3 if p5==2 | p5==3 | p5==5
replace civil_ci=4 if p5==6
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

by idh_ch, sort: egen nmayor21_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad>=21)
label var nmayor21_ch "Numero de familiares mayores a 21 anios"

by idh_ch, sort: egen nmenor21_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad<21)
label var nmenor21_ch "Numero de familiares menores a 21 anios"

by idh_ch, sort: egen nmayor65_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad>=65)
label var nmayor65_ch "Numero de familiares mayores a 65 anios"

by idh_ch, sort: egen nmenor6_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad<6)
label var nmenor6_ch "Numero de familiares menores a 6 anios"

by idh_ch, sort: egen nmenor1_ch=sum((relacion_ci>=1 & relacion_ci<=4) & edad<1)
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
	***afroind_ci***
	***************
**Pregunta: ¿Se considera usted indígena? (indi_rec) (1 - no indígena; 2 - indígena)
**No se identifica a personas afrodescendientes. Todos los no-indígenas se categorizan como "otro". 
**En el 2011 se convierte en la EHPM (no solo EH) 

gen afroind_ci=. 
replace afroind_ci=1 if indi_rec==2
replace afroind_ci=2 if indi_rec==0
replace afroind_ci=3 if indi_rec==1


	***************
	***afroind_ch***
	***************
gen afroind_jefe= afroind_ci if relacion_ci==1
egen afroind_ch  = min(afroind_jefe), by(idh_ch) 
drop afroind_jefe

	*******************
	***afroind_ano_c***
	*******************
gen afroind_ano_c=2001

	*******************
	***dis_ci***
	*******************
gen dis_ci=. 

	*******************
	***dis_ch***
	*******************
gen dis_ch=. 

******************************************************************************
*	LABOR MARKET
******************************************************************************

****************
****condocup_ci*
****************
/*
gen condocup_ci=.
replace condocup_ci=1 if p10_18 >= 1 & p10_18 <= 5 
replace condocup_ci=2 if  p10_18 == 6 |  p10_18 == 7 
replace condocup_ci=3 if  p10_18 >= 10 &  p10_18 <= 17 |  p10_18 == 0 |  p10_18 == 9 | p10_18 == 8
replace condocup_ci=4 if edad_ci<10
label var condocup_ci "Condicion de ocupación de acuerdo a def de cada pais"
label define condocup_ci 1 "Ocupado" 2 "Desocupado" 3 "Inactivo" 4 "Menor de PET" 
label value condocup_ci condocup_ci
*/

* Alternativa 2: segun la clasificacion de p10_18. MGD 06/06/2014
gen condocup_ci=.
replace condocup_ci=1 if p10_18>= 1 & p10_18<= 5 
replace condocup_ci=2 if  (p10_18>=6 & p10_18<=8) 
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

/*
******************************
*	emp_ci
******************************
/*
gen emp_ci=(p10_18>=1 & p10_18<=5)
replace emp_ci=. if  p10_18==0
*Se coloca missing a aquellos que no respondieron porque son menores de 10 años
*/
gen emp_ci=(p10_18>=1 & p10_18<=5)

******************************
*	desemp1_ci	
******************************
*No se coloca missing a aquellos menores de 10 años porque el sociometro filtra por grupos de edad 
*entonces da lo mismo si filtro o no solo a los que respondieron

gen desemp1_ci=(emp_ci==0 & p10_18==6)
label var desemp1_ci "Personas que no tienen trabajo y han buscado trabajo en el periodo de referencia"

******************************
*	desemp2_ci
******************************
gen desemp2_ci=(emp_ci==0 & (p10_18==6 | p10_18==9))
label var desemp2_ci "des1 + no trabajaron ni buscaron en la ultima semana pero esperan respuesta de solicit"

******************************
*	desemp3_ci
******************************
gen desemp3_ci=(emp_ci==0 & (p10_18==6 | p10_18==7 | p10_18==8 | p10_18==9))
label var desemp3_ci "des2 + no tienen trabajo pero buscaron antes de la semana pasada"

******************************
*	pea1_ci, pea2_ci, pea3_ci
******************************
gen pea1_ci=(emp_ci==1 | desemp1_ci==1)
gen pea2_ci=(emp_ci==1 | desemp2_ci==1)
gen pea3_ci=(emp_ci==1 | desemp3_ci==1)*/

******************************
*	horaspri_ci
******************************
gen horaspri_ci=p43 if p43>0 & p43<99
replace horaspri_ci=. if emp_ci==0
label var horaspri_ci "Hs totales (semanales) trabajadas en act. principal"

******************************
*	horastot_ci
******************************
egen horastot_ci=rsum(p43 p48) if p43>0 & p43<99, missing
replace horastot_ci=horaspri_ci if p48==99 | p43==0
replace horastot_ci=. if (p43==0 & p48==0) | (p43==99 | p48==99)| emp_ci==0
label var horastot_ci "Hs totales (semanales)trabajadas en toda actividad"

******************************
*	desalent_ci
******************************

generat desalent_ci = 1  if  p10_18==10
replace desalent_ci = 0  if p10_18!=10
replace desalent_ci =.   if p10_18==.

******************************
*	subemp_ci
******************************

gen subemp_ci=0
replace subemp_ci=1 if (emp_ci==1 & p50==1 & horaspri_ci<=30)

* Alternativa considerando disponibilidad. MGD 06/19/2014
gen subemp_ci1=0
replace subemp_ci1=1 if (emp_ci==1 & p50==1 & horaspri_ci<=30) & (p53==1 | (p54>=1 & p54<=3))

******************************
*	tiempoparc_ci
******************************
gen tiempoparc_ci=(emp_ci==1 & p50==2 & horastot_ci<=30)
replace tiempoparc_ci=. if emp_ci==0 | emp_ci==.
label var tiempoparc_ci "Trabajan menos de 30 hs semanales y no quieren trabajar mas"


******************************************************************************
*		LABOR DEMAND
******************************************************************************

******************************
*	ocupa_ci
******************************
* Modificacion MGD 07/22/2014: se utiliza la clasificacion CNO de Panama
g aux_p28=p28
destring aux_p28, replace

g ocupa_ci=.
replace ocupa_ci=1 if (aux_p28>=140 & aux_p28<=662) & emp_ci==1
replace ocupa_ci=2 if (aux_p28>=1 & aux_p28<=139) & emp_ci==1
replace ocupa_ci=3 if (aux_p28>=663 & aux_p28<=817) & emp_ci==1
replace ocupa_ci=4 if ((aux_p28>=880 & aux_p28<=917) | (aux_p28>=1555 & aux_p28<=1565)) & emp_ci==1
replace ocupa_ci=5 if ((aux_p28>=818 & aux_p28<=879) | (aux_p28>=1566 & aux_p28<=1608)) & emp_ci==1
replace ocupa_ci=6 if (aux_p28>=918 & aux_p28<=1000)  & emp_ci==1
replace ocupa_ci=7 if ((aux_p28>=1001 & aux_p28<=1554) | (aux_p28>=1609 & aux_p28<=1649)) & emp_ci==1
replace ocupa_ci=8 if  (aux_p28>=1650 & aux_p28<=1651)& emp_ci==1
replace ocupa_ci=9 if (aux_p28>=1652 & aux_p28<=1653 ) & emp_ci==1

drop aux_p28

label define ocupa_ci 1"profesional y tecnico" 2"director o funcionario sup" 3"administrativo y nivel intermedio"
label define ocupa_ci  4 "comerciantes y vendedores" 5 "en servicios" 6 "trabajadores agricolas", add
label define ocupa_ci  7 "obreros no agricolas, conductores de maq y ss de transporte", add
label define ocupa_ci  8 "FFAA" 9 "Otras ", add
label value ocupa_ci ocupa_ci
label variable ocupa_ci "Ocupacion laboral" 

******************************
*	rama_ci
******************************
destring p30, replace
gen rama_ci=. 
replace rama_ci=1 if (p30>=111 & p30<=502)  & emp_ci==1
replace rama_ci=2 if (p30>=1010 & p30<=1429) & emp_ci==1
replace rama_ci=3 if (p30>=1511 & p30<=3720)  & emp_ci==1
replace rama_ci=4 if (p30>=4010 & p30<=4100)  & emp_ci==1
replace rama_ci=5 if (p30>=4510 & p30<=4550)  & emp_ci==1
replace rama_ci=6 if (p30>=5110 & p30<=5530)  & emp_ci==1
replace rama_ci=7 if (p30>=6010 & p30<=6420)  & emp_ci==1
replace rama_ci=8 if (p30>=6511 & p30<=7020)  & emp_ci==1
replace rama_ci=9 if (p30>=7111 & p30<=9900)  & emp_ci==1

label var rama_ci "Rama actividad principal"
label define rama_ci 1 "Agricultura, caza, silvicultura y pesca" 2 "Explotación de minas y canteras" 3 "Industrias manufactureras" 4 "Electricidad, gas y agua" 5 "Construcción" 6 "Comercio al por mayor y menor, restaurantes, hoteles" 7 "Transporte y almacenamiento" 8 "Establecimientos financieros, seguros, bienes inmuebles" 9 "Servicios sociales, comunales y personales"
label values rama_ci rama_ci

******************************
*	categopri_ci
******************************
gen categopri_ci=0 if                         emp_ci==1
replace categopri_ci=1 if p33==8             & emp_ci==1
replace categopri_ci=2 if (p33==7 | p33==9 ) & emp_ci==1
replace categopri_ci=3 if (p33>=1 & p33<=6)  & emp_ci==1
*puse Miembro de una cooperativa de produccion (p33==9)dentro de cuenta propia
replace categopri_ci=4 if p33==10 & emp_ci==1
*MLO= inclui condicion que sea ocupado
label var categopri_ci "Categoria ocupacional en la actividad principal"
label define categopri_ci 1 "Patron" 2 "Cuenta Propia" 3 "Empleado" 4 "Trabajador no remunerado"
label value categopri_ci categopri_ci

******************************
*	categosec_ci
******************************
gen categosec_ci=.
label define categosec_ci 1 "Patron" 2 "Cuenta Propia" 3 "Empleado" 4 "Familiar no remunerado" 
label value categosec_ci categosec_ci
label var categosec_ci "Categoria ocupacional en la actividad secundaria"
/*
******************************
*	contrato_ci
******************************
gen contrato_ci=0 if emp_ci==1
replace contrato_ci=1 if p34>=2 & p34<=4 
label var contrato_ci "Persona empleada que firmo contrato de trabajo"

*con values 0 quedarian individuos sin contrato o trabajadores permanentes

******************************
*	segsoc_ci
******************************
gen segsoc_ci=.
*para mi si se puede generar con p4 */

******************************
*	nempleos_ci
******************************
gen nempleos_ci=0     if emp_ci==1
replace nempleos_ci=1 if p44==3
replace nempleos_ci=2 if p44==1 | p44==2

/******************************
*	firmapeq_ci
******************************
gen firmapeq_ci=0 if emp_ci==1
replace firmapeq_ci=1 if p31==1 
replace firmapeq_ci=. if emp_ci==0
label var firmapeq_ci "1=5 o menos trabajadores"*/

******************************
*	spublico_ci
******************************
gen spublico_ci=0 if emp_ci==1
replace spublico_ci=1 if p33==1
replace spublico_ci=. if emp_ci==0
label var spublico_ci "Trabaja en sector publico"

******************************
*	durades_ci
******************************
gen durades_ci=. 
replace durades_ci=0.5 if p21==100
replace durades_ci=p21-200 if p21>=201 /*& p21<298*/
label var durades_ci "Duracion del desempleo en meses"

******************************
*	antiguedad_ci
******************************
gen m=p40-100 if p40>=100 & p40<=111
gen a=p40-200 if p40>=201 & p40<=299 

gen antiguedad_ci=.
replace antiguedad_ci=a
replace antiguedad_ci=m/12 if a==.
drop a m

******************************************************************************
*		INCOME
******************************************************************************

******************************
*	ylmpri_ci & ylmpri1_ci
******************************
generat ylmpri_ci=p421 if p421>0 & p421<9999 & categopri_ci==3
replace ylmpri_ci=p423 if p423>0 & p423<9999 & (categopri_ci==1 | categopri_ci==2) 
replace ylmpri_ci=0    if categopri==4
replace ylmpri_ci=.    if emp_ci==0
label var ylmpri_ci "Ingreso laboral monetario act. principal (mes)"

*Parece que el salario bruto incluye los aguinaldos. No estoy segura, por eso no sumo 
*el decimo tercero al ingreso principal. Se deja una variable alternativa "ylmpri1_ci"
gen aguin=p55h if p55h>0 & p55h<9999
egen ylmpri1_ci=rsum(ylmpri_ci aguin), missing
replace ylmpri1_ci=. if ylmpri_ci==. & aguin==.
replace ylmpri1_ci=. if emp_ci==0
replace ylmpri1_ci=. if ylmpri_ci==. & (p422==0 | p422==9999)

******************************
*	nrylmpri_ci & nrylmpri1_ci
******************************
gen nrylmpri_ci=(((p421>=9999 & p421<.) | (p423>=9999 & p423<.)) & emp_ci==1)
replace nrylmpri_ci=. if emp_ci==0
label var nrylmpri_ci "Identificador de NR de ingreso"

******************************
*	ylmsec_ci
******************************
gen ylmsec_ci=p49 if p49>0 & p49<9999  
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
gen ylnmpri_ci=p422 if p422>0 & p422<9999
replace ylnmpri_ci=. if emp_ci==0
gen ylnm_ci=ylnmpri_ci
label var ylnm_ci "Ingreso laboral no monetario total"

*******************************************************
*** Ingreso no laboral no monetario (otras fuentes).***
*******************************************************
gen ynlnm_ci=.
label var ynlnm_ci "Ingreso no laboral no monetario"
***********************************************************
*** Ingreso no laboral no monetario del Hogar.
************************************************************
gen ynlnm_ch=.
label var ynlnm_ch "Ingreso no laboral no monetario del Hogar" 

******************************
*	ynlm_ci
******************************
gen jub=p55a if p55a>0 & p55a<99999
*2014,02 modificacion mLO
replace p55c1=. if p55c1==99999 
replace p55c2=. if p55c2==99999 
egen ayfam=rsum(p55c1 p55c2 p55c3 p55c4 p55c5), missing 
*2014, 02 MLO
*gen ayfam=p55c1+p55c2+p55c3+p55c4+p55c5 if p55c1<9999 & p55c2 <9999 & p55c3 <9999 & p55c4<9999 & p55c5 <9999  
gen pension=p55b  if p55b >0 & p55b <99999
gen alqui=p55d if p55d>0 & p55d<99999
gen loter=p55e if p55e>0 & p55e<10000
replace p55f2=. if p55f2==99999
egen becas=rsum(p55f1 p55f2), missing 
*2014, 02 MLO
*gen becas=p55f1+p55f2 if (p55f1>0 & p55f1<9999) | (p55f2>0 & p55f2<9999)
gen subsidios= p55g1+p55g2+p55g3+p55g4 if p55g1<9999 | p55g2<9999 | p55g3<9999 | p55g4<9999
gen agro=p55i if p55i>0 & p55i<9999 /*es laboral?*/
gen otroy=p55l if p55l>0 & p55l<99999
gen habit=p55k if p55k>0 & p55k<99999

egen ynlme= rsum(jub pension ayfam alqui loter becas subsidios habit otroy) if emp_ci==1, missing
egen ynlmd= rsum(jub pension ayfam alqui loter becas aguin subsidios habit otroy) if emp_ci==0, missing
egen ynlm_ci=rsum(ynlme ynlmd), missing
label var ynlm_ci "Ingreso no laboral monetario(mes)"

drop jub alqui loter becas agro otroy ayfam ynlme ynlmd aguin pension habit

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
gen autocons_ci=.
label var autocons_ci "Autoconsumo reportado por el individuo"

******************************
*	autocons_ch 
******************************
gen autocons_ch=.

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
******************************************************************************
notes: Survey de Panama no pregunta caracteristicas de vivienda por lo que no se pueden construir las variables aguared_ch,aguadist_ch,aguamala_ch,aguamide_ch,luz_ch,luzmide_ch,combust_ch,bano_ch,banoex_ch,des1_ch,des2_ch,piso_ch,pared_ch,techo_ch,resid_ch,dorm_ch,cuartos_ch,cocina_ch,telef_ch,refrig_ch,freez_ch,auto_ch,compu_ch,internet_ch,cel_ch,vivi1_ch,vivi2_ch,viviprop_ch,vivitit_ch,vivialq_ch,vivialqimp_ch.
 
gen aguared_ch=.
gen aguadist_ch=.
gen aguamala_ch=.
gen aguamide_ch=.
gen luz_ch=.
gen luzmide_ch=.
gen combust_ch=.
gen bano_ch=.
gen banoex_ch=.
gen des1_ch=.
gen des2_ch=.
gen piso_ch=.
gen pared_ch=.
gen techo_ch=.
gen resid_ch=. 
**Daniela Zuluaga- Enero 2018: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**	
gen aguamejorada_ch = .
gen  banomejorado_ch = . 
gen dorm_ch=.
gen cuartos_ch=.
gen cocina_ch=.
gen telef_ch=.
gen refrig_ch=.
gen freez_ch=.
gen auto_ch=.
gen compu_ch=.
gen internet_ch=.
gen cel_ch=.
gen vivi1_ch=.
gen vivi2_ch=.
gen viviprop_ch=.
gen vivitit_ch=.
gen vivialq_ch=.
gen vivialqimp_ch=.

******************************************************************************
*	EDUCATION
******************************************************************************

******************************
*	aedu_ci
******************************
generat grado=p8-10 if p8>=11 & p8<=16
replace grado=p8-20 if p8>=21 & p8<=26
replace grado=p8-30 if p8>=31 & p8<=33
replace grado=p8-40 if p8>=41 & p8<=49
replace grado=p8-50 if p8>=51 & p8<=53
replace grado=0 if p8==60

gen nivel=0 if p8==60
replace nivel=1 if p8>=11 & p8<=16
replace nivel=2 if p8>=21 & p8<=26
replace nivel=3 if p8>=31 & p8<=33
replace nivel=4 if p8>=41 & p8<=49
replace nivel=5 if p8>=51 & p8<=53

gen aedu_ci=0            if nivel==0 
replace aedu_ci=grado    if nivel==1
replace aedu_ci=grado+6  if nivel==2 | nivel==3
replace aedu_ci=grado+12 if nivel==4 | nivel==5

*replace aedu_ci=0 if edad_ci<5

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
*	eduac_ci
******************************
gen eduac_ci=.
replace eduac_ci=0 if nivel==5
replace eduac_ci=1 if nivel==4
label var eduac_ci "Educ terciaria academica vs Educ terciaria no academica"

******************************
*	asiste_ci
******************************
gen asiste_ci=(p7==1)
replace asiste_ci=. if p7==0
label var asiste "Personas que actualmente asisten a centros de enseñanza"

******************************
*	asispre_ci
******************************
gen asispre_ci=.
label var asispre_ci "Asistencia a Educacion preescolar"
notes: la encuesta no tiene codigo de educacion preescolar 

******************************
*	pqnoasis_ci
******************************
gen pqnoasis_ci=p7a if p7a>0
label var pqnoasis_ci "Razones para no asistir a la escuela"
label define pqnoasis_ci 1 "No se ofrece el nivel o grado escolar en la comunidad" 2 "Necesita trabajar",add
label define pqnoasis_ci 3 "Falta de recursos económicos" 4 "Quehaceres domesticos", add 
label define pqnoasis_ci 5 "Falta de interes" 6 "Embarazo" 7 "Enfermedad" , add
label define pqnoasis_ci 8 "No tiene la edad requerida" 9 "Está muy distante" 10 "Otros", add
label value pqnoasis_ci pqnoasis_ci

**************
*pqnoasis1_ci*
**************
**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**

g       pqnoasis1_ci = 1 if p7a==3
replace pqnoasis1_ci = 2 if p7a==2
replace pqnoasis1_ci = 3 if p7a==7
replace pqnoasis1_ci = 4 if p7a==5
replace pqnoasis1_ci = 5 if p7a==4 | p7a==6
replace pqnoasis1_ci = 7 if p7a==8
replace pqnoasis1_ci = 8 if p7a==1 | p7a==9
replace pqnoasis1_ci = 9 if p7a==10

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

* PAN 2009
gen salmm_ci= . /*289.1*/
replace salmm_ci= 284.4 if rama_ci==1
replace salmm_ci= 340.8 if rama_ci==2
replace salmm_ci= 288.8 if rama_ci==3
replace salmm_ci= 333.6 if rama_ci==4
replace salmm_ci= 404.8 if rama_ci==5
replace salmm_ci= 302.13 if rama_ci==6
replace salmm_ci= 329.6 if rama_ci==7
replace salmm_ci= 371.2 if rama_ci==8
replace salmm_ci= 355.2 if rama_ci==9
replace salmm_ci= 334.5 if salmm_ci==.

label var salmm_ci "Salario minimo legal"

*********
*lp_ci***
*********
destring areareco dist, replace
gen lp_ci =.
replace lp_ci= 78.39 if areareco==1 & dist==1 /* Cdad. Panamá*/
replace lp_ci= 78.39 if areareco==1 & dist==3 /* Zona urbana districto san miguelito*/
replace lp_ci= 86.81 if ((dist!=1 & dist!=3) & areareco==1) | areareco==2  /* resto urbano o rural*/


label var lp_ci "Linea de pobreza oficial del pais"

*********
*lpe_ci***
*********

gen lpe_ci =.
replace lpe_ci= 42.00 if areareco==1 & dist==1 /* Cdad. Panamá*/
replace lpe_ci= 42.00 if areareco==1 & dist==3 /* Zona urbana districto san miguelito*/
replace lpe_ci= 43.87 if ((dist!=1 & dist!=3) & areareco==1) | areareco==2  /* resto urbano o rural*/

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
replace afiliado_ci =1 if p4==1  /* afiliado directo */
recode afiliado_ci .=0 if p10_18 >= 1 & p10_18 <= 7
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
replace tipocontrato_ci=1 if p34==1 | p34==4 & categopri_ci==3
replace tipocontrato_ci=2 if (p34==2 | p34==3) & categopri_ci==3
recode tipocontrato_ci .=3 if (p34==5 | tipocontrato_ci==.) & categopri_ci==3
label var tipocontrato_ci "Tipo de contrato segun su duracion"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci

*************
*cesante_ci* 
*************
* MGD 12/4/2015: se corrige la inclusion de ceros con p26 >100. Antes solamnte se incluia a los missings ==999
gen cesante_ci=1 if p26>100 & p26!=999 & p26!=. & condocup_ci==2
*gen cesante_ci=1 if p26==999 
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
gen tamemp_ci=1 if p31==1 
label var  tamemp_ci "Tamaño de Empresa" 
*Empresas medianas
replace tamemp_ci=2 if p31==2 | p31==3 | p31==4
*Empresas grandes
replace tamemp_ci=3 if p31==5
label define tamaño 1"Pequeña" 2"Mediana" 3"Grande"
label values tamemp_ci tamaño
tab tamemp_ci [iw= fac15_e]

*************
*categoinac_ci
*************

gen categoinac_ci=1 if p10_18==11 | p10_18==12
label var  categoinac_ci "Condición de Inactividad" 
*Estudiantes
replace categoinac_ci=2 if p10_18==13
*Quehaceres del Hogar
replace categoinac_ci=3 if p10_18==14
*Otra razon
replace categoinac_ci=4 if p10_18==15 | p10_18==16 | p10_18==17
label define inactivo 1"Pensionado y otros" 2"Estudiante" 3"Hogar" 4"Otros"
label values categoinac_ci inactivo
	

*************
**pension_ci*
*************
* 2014, 02 MLO modificacion cambio de missing en 99999
replace p55a=. if p55a==99999
replace p55b=. if p55b==99999
egen aux_p=rsum(p55a p55b), missing
gen pension_ci=1 if aux_p>0 & aux_p!=. & aux_p!=999999
recode pension_ci .=0
label var pension_ci "1=Recibe pension contributiva"

*************
*ypen_ci*
*************

egen ypen_ci=rsum(p55a p55b), missing
*replace ypen_ci=.
label var ypen_ci "Valor de la pension contributiva"

***************
*pensionsub_ci*
***************

gen byte pensionsub_ci=.
label var pensionsub_ci "1=recibe pension subsidiada / no contributiva"

*****************
**ypensub_ci*
*****************

gen byte ypensub_ci=.
label var ypensub_ci "Valor de la pension subsidiada / no contributiva"


*************
*tecnica_ci**
*************

gen tecnica_ci=.
replace tecnica_ci=1 if p8==31 | p8==32| p8==33 
recode tecnica_ci .=0
label var tecnica_ci "1=formacion terciaria tecnica"


/*_____________________________________________________________________________________________________*/
* Asignación de etiquetas e inserción de variables externas: tipo de cambio, Indice de Precios al 
* Consumidor (2011=100), líneas de pobreza
/*_____________________________________________________________________________________________________*/


do "$ruta\harmonized\_DOCS\\Labels&ExternalVars_Harmonized_DataBank.do"

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








