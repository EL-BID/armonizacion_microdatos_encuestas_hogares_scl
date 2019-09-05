* (Versión Stata 12)
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

local PAIS PAN
local ENCUESTA EHPM
local ANO "2011"
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
Versión 2014: Melany Gualavisi
Última versión: Melany Gualavisi (MGD) - Email: melanyg@iadb.org
Fecha última modificación: Octubre de 2017
Modificación 2014: Melany Gualavisi (MGD) - Email: melanyg@iadb.org
Última modificación: Daniela Zuluaga (DZ) -Email: danielazu@iadb.org -da.zuluaga@hotmail.com
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

label var	prov		"Provincia"
label var	cuest		"Cuestionario/unoparacadahogar"
label var   llave_unic 	"Llaveunica"
label var	hogar		"Hogarnúmero"
label var	tot_per		"Totaldepersonas."
label var	tot_per_h	"Totaldepersonas,sexomasculino."
label var	tot_per_m	"Totaldepersonas,sexofemenino."
label var	tot_per_e1	"Totalde10ymásdeedad."
label var	per_e10_h	"Totaldepersonasde10ymásaños,sexomasculino."
label var	per_e10_m	"Totaldepersonasde10ymásaños,sexofemenino."
label var	per_e15_h	"Totaldepersonasde15ymásaños,sexomasculino."
label var	per_e15_m	"Totaldepersonasde15ymásaños,sexofemenino."
label var   tot_per__a 	"Total de personas de 15 y más años de edad"
label var	residia		"Personasqueresidíanagostodelañopasado"
*label var	matri		"Matriculadosen1er.Grado"
*label var	g4to		"Aprobaron4to.Grado?"
*label var	fracaso		"Fracasó"
*label var	recurso		"Faltaderecursos"
*label var	trabajar	"Tienequetrabajar"
*label var	interes		"Faltadeinterés"
*label var	enfermed	"Enfermedad"
*label var	otros		"Otrosmotivos"
label var	regireco	"Regiones(1,2,3,4)"
label var	areareco	"Urbano=“1”/rural=“2”"
label var	fac15_e		"Factordeexpansiónde15ymás/menos15añosde edad"
label var	indi_rec	"Noindígena=1/Indígena2"
label var	nper		"2Númerodelapersona"
*label var	nombre_ape	"Nombreyapellido"
label var	info		"Informanteenelinteriordelcuestionario(esq.sup.der.)"
label var	p1			"Parentesco"
label var	p1a_otro	"Otropariente"
label var   p1_nucleo	"Nucleo"
label var	p2			"Sexo"
label var	p3			"Edad"
label var	p4			"¿Tieneud.Segurosocial?"
label var	p5_prov_te	"Provinciaviviasumadre"
label var	p5_vivia_a	"Paisviviasumadre"
label var	p5a_anio_l	"Llegoavivirapanama"
label var	p5b_period	"antesodespuesdel2000"
label var	p5c_pais_t	"paisviviaantes"
label var	p6_prov_te	"provviviamarzo2008"
label var	p6_pais_te	"paisviviamarzo2008"
label var	p7			"¿Asistealaescuelaactualmente?"
label var	p7a			"¿Motivoprincipaldenoasistir?"
label var	p7a_otro_m	"Códigosdeotrosdelapregunta7ª"
label var	p8			"¿Gradooañomásaltoaprobó?"
label var	p9			"¿Sabeleeryescribir?"
label var	p9_titulo	"¿diplomaotitulo1?"
label var	p9_titul_a	"¿diplomaotitulo2?"
label var	p10_18		"Condicióndeactividad"
*label var	p10_18a		"Meses"
*label var	p10_18b		"CódigosdeOtros"
label var	p19			"¿Piensabuscartrabajoenlospróximos6meses?"
label var	p20			"¿…Noestuvobuscando,nipiensabuscar…?"
label var	p20a		"CódigodelaopciónidelapreguntaP20"
label var	p21			"¿Cuántotiempohacequeestuvooestátrabajo…?"
label var	p22			"¿Hizoalgunagestiónparaconseguirempleo...?"
label var	p23			"¿Quégestiónhizo...?"
label var	p23a		"CódigodelaopciónjdelapreguntaP23"
label var	p24			"¿Estuvodisponibleparatrabajardurantelasdossemanas…?"
label var	p25			"¿Paraquétipodetrabajoestuvooestará....?"
label var	p26			"¿Cuántotiempohace…?"
label var	p27			"¿Porquémotivodejósuúltimotrabajo…?"
label var	p27a		"CódigodeOtrodelapreguntaP27"
label var	p28_codigo	"Queocupacionoficiootrabajo"
label var	p29_donde	"¿Dóndetrabajaotrabajó...?"
label var	p29a_otro	"Otrospregunta29"
label var	p30_codigo	"¿Aquésededicaelnegocio?Codigo"
label var	p31_trabaj	"¿Cuántaspersonastrabajan..?"
label var	p31a_espec	"Especifique"
label var	p32_emplea	"¿Cuántos...eranempleados.."
label var	p33			"¿Dóndeustedtrabaja...lohizocomo...?"
label var	p34			"¿Esoeraempleado...?"
label var	p35_provin	"Provincia"
label var	p35_distri	"Distrito"
label var	p35_correg	"Corregimiento"
label var	p36_tiempo 	"Tiempodetrabajarenesaempresa"
label var	p37_sueldo 	"Trabajaustedporsueldofijo"
label var	p371		"Por día?"
label var	p374		"Días"
label var	p372		"Por tarea?"
label var	p375		"Tareas"
label var	p373		"Por hora?"
label var	p376		"Horas"
label var	p381		"Salarioenefectivo"
label var	p382		"Enespecie"
label var	p382a_tipo	"Tipodeespecie"
label var	p383		"Ingreso...independiente..."
label var	p384		"Ingreso...especie..."
label var	p384a_tipo	"Tipodeespecie"
label var	p39			"¿Cuántashorastrabajó...?"
label var	p40_otro_t	"¿Tuvoalgúnotrotrabajo...?"
label var	p41			"¿Ingresomensualenelotro?"
*label var	p42			"Recibiodinerodelextrangero,ultimos12meses"
*label var	p43			"FrecuenciaRecibiodinerodelextrangero"
*label var	p43a		"Otrodelap43"
*label var	p44			"Totaldedienrorecibido"
*label var	p45a		"Empresaderemesas?"
*label var	p45b		"Transferenciabancaria?"
*label var	p45c		"Alguienqueviaja?"
*label var	p45d		"Personaquesededicaatraerdinero?"
*label var	p45e		"Porencomienda?"
*label var	p45f		"Otro"
*label var	p45f_1		"Cual?Especificar"
*label var	p46a		"Gastoeneducacion?"
*label var	p46b		"Gastoensalud?"
*label var	p46c		"Comprademuebles?"
*label var	p46d		"Reparaciondelavivienda?"
*label var	p46e		"Ahorros?"
*label var	p46f		"Inversiones?"
*label var	p46g		"Consumodelhogar?"
*label var	p46h		"Pagodedeudas?"
*label var	p46i		"Otro"
*label var	p46i_1		"Cual?Especificar"
*** Restode variables falta etiquetar
label var	areareco	"Urbano=1/rural=2"
label var	ocu_reco	"Condicióndeactividadrecodificada.Códigosdel01al50"
label var	fac15_e	"Factordeexpansiónde15ymás/menos15añosde"
label var	v1_tenenci	"¿Laviviendaquehabitaestehogares…?"
label var	v1_otra_te	"PagomensualenB"
label var	v1a_pago_m	"Otra?Especifique…"
label var	cuantos_ho	"¿Cuántoshogaresresidenenestavivienda?"
label var	regireco	"Regiones(1,2,3,4)"
label var	areareco	"Urbano=1/rural=2"
label var divi_reco "distritos"


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

******************************
*	factor_ci
******************************

gen factor_ci= fac15_e   
label var factor_ci "Factor de expansion del individuo"

******************************
*	idh_ch
******************************
sort prov llave_unic cuest hogar
egen idh_ch = group(prov llave_unic cuest hogar)
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
gen zona_c=0 if areareco=="Rural"
replace zona_c=1 if areareco=="Urbana"
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
gen anio_c=2011
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

**********
***raza***
**********

destring indi_rec, replace
gen raza_ci=1 if indi_rec==2
replace raza_ci=3 if indi_rec==1
label define raza_ci 1 "Indígena" 2 "Afro-descendiente" 3 "Otros"
label value raza_ci raza_ci 
label value raza_ci raza_ci
label var raza_ci "Raza o etnia del individuo" 
*nota raza_ci: En el cuestionario no consta una pregunta relacionada con raza.

gen raza_idioma_ci = .
gen id_ind_ci      = .
gen id_afro_ci     = .


******************************
*	civil_ci
******************************

gen civil_ci=.
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

******************************************************************************
*	LABOR MARKET
******************************************************************************

****************
****condocup_ci*
****************
/*
gen condocup_ci=.
replace condocup_ci=1 if p10_18>= 1 & p10_18 <= 5 
replace condocup_ci=2 if  p10_18 == 6 |  p10_18 == 7 
replace condocup_ci=3 if  p10_18 >= 10 &  p10_18 <= 17 |  p10_18 == 0 |  p10_18 == 9 | p10_18 == 8
replace condocup_ci=4 if edad_ci<10
label var condocup_ci "Condicion de ocupación de acuerdo a def de cada pais"
label define condocup_ci 1 "Ocupado" 2 "Desocupado" 3 "Inactivo" 4 "Menor de PET" 
label value condocup_ci condocup_ci
*/
* Alternativa 2: segun codificacion de la variable y considerando a todos los que buacan trabajo. MGD 06/06/2014
gen condocup_ci=.
replace condocup_ci=1 if p10_18>= 1 & p10_18<= 5 
replace condocup_ci=2 if  (p10_18>=6 & p10_18<=9) 
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
gen pea3_ci=(emp_ci==1 | desemp3_ci==1)
*/
******************************
*	horaspri_ci
******************************
gen horaspri_ci=p39 if p39>0 & p39<99
replace horaspri_ci=. if emp_ci==0
label var horaspri_ci "Hs totales (semanales) trabajadas en act. principal"

******************************
*	horastot_ci
******************************
* Solo se declaran las horas para la primera actividad. 
g horastot_ci=horaspri_ci
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

gen subemp_ci=.
* No hay la pregunta de si desea trabajar mas horas.

******************************
*	tiempoparc_ci
******************************
gen tiempoparc_ci=.
label var tiempoparc_ci "Trabajan menos de 30 hs semanales y no quieren trabajar mas"
* No hay la pregunta de si desea trabajar mas horas.


******************************************************************************
*		LABOR DEMAND
******************************************************************************

******************************
*	ocupa_ci
******************************
*No se encuentra la clasificacion usada.  Se puso la generica pero hay que comprobar.
* Modificacion MGD 07/22/2014: se utiliza la clasificacion CIUO-08
gen aux_p28=substr(p28_codigo,1,4)
destring aux_p28, replace

g ocupa_ci=.
replace ocupa_ci=1 if (aux_p28>=2110 & aux_p28<=3999) & emp_ci==1
replace ocupa_ci=2 if (aux_p28>=1110 & aux_p28<=1999) & emp_ci==1
replace ocupa_ci=3 if (aux_p28>=4110 & aux_p28<=4999) & emp_ci==1
replace ocupa_ci=4 if ((aux_p28>=5211 & aux_p28<=5249) | (aux_p28>=9510 & aux_p28<=9522)) & emp_ci==1
replace ocupa_ci=5 if ((aux_p28>=5111 & aux_p28<=5169) | (aux_p28>=5311 & aux_p28<=5999) | (aux_p28>=9111 & aux_p28<=9129) | (aux_p28>=9611 & aux_p28<=9624)) & emp_ci==1
replace ocupa_ci=6 if ((aux_p28>=6110 & aux_p28<=6341) | (aux_p28>=9211 & aux_p28<=9216)) & emp_ci==1
replace ocupa_ci=7 if ((aux_p28>=7110 & aux_p28<=8999) | (aux_p28>=9311 & aux_p28<=9412)) & emp_ci==1
replace ocupa_ci=8 if  (aux_p28>=110 & aux_p28<=310)& emp_ci==1
replace ocupa_ci=9 if (aux_p28==9629 | aux_p28==9999 ) & emp_ci==1

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
* Se utiliza CIIU rev. 4
destring p30_codigo, replace
gen rama_ci=. 
replace rama_ci=1 if (p30_codigo>=0111 & p30_codigo<=0322)  & emp_ci==1
replace rama_ci=2 if (p30_codigo>=0729  & p30_codigo<=0990) & emp_ci==1
replace rama_ci=3 if (p30_codigo>=1010 & p30_codigo<=3320)  & emp_ci==1
replace rama_ci=4 if (p30_codigo>=3510  & p30_codigo<=3900)  & emp_ci==1
replace rama_ci=5 if (p30_codigo>=4100 & p30_codigo<=4390)  & emp_ci==1
replace rama_ci=6 if ((p30_codigo>=4510 & p30_codigo<=4799) | (p30_codigo>=5510 & p30_codigo<=5632)) & emp_ci==1
replace rama_ci=7 if (p30_codigo>=4921  & p30_codigo<=5320)  & emp_ci==1
replace rama_ci=8 if (p30_codigo>=6419 & p30_codigo<=6822)  & emp_ci==1
replace rama_ci=9 if ((p30_codigo>=5813 & p30_codigo<=6391) | (p30_codigo>=6910 & p30_codigo<=9990)) & emp_ci==1

label var rama_ci "Rama actividad principal"
label define rama_ci 1 "Agricultura, caza, silvicultura y pesca" 2 "Explotación de minas y canteras" 3 "Industrias manufactureras" 4 "Electricidad, gas y agua" 5 "Construcción" 6 "Comercio al por mayor y menor, restaurantes, hoteles" 7 "Transporte y almacenamiento" 8 "Establecimientos financieros, seguros, bienes inmuebles" 9 "Servicios sociales, comunales y personales"
label values rama_ci rama_ci

******************************
*	categopri_ci
******************************
gen categopri_ci=0     if                      emp_ci==1
replace categopri_ci=1 if p33==8             & emp_ci==1
replace categopri_ci=2 if (p33==7 | p33==9)  & emp_ci==1
replace categopri_ci=3 if (p33>=1 & p33<=6 ) & emp_ci==1
replace categopri_ci=4 if p33==10            & emp_ci==1
label var categopri_ci "Categoria ocupacional en la actividad principal"
label define categopri_ci 1 "Patron" 2 "Cuenta Propia" 3 "Empleado" 4 "Trabajador no remunerado"
label value categopri_ci categopri_ci
*se puso Miembro de una cooperativa de produccion (p33==9)dentro de cuenta propia

******************************
*	categosec_ci
******************************
gen categosec_ci=.
label define categosec_ci 1 "Patron" 2 "Cuenta Propia" 3 "Empleado" 4 "Familiar no remunerado" 
label value categosec_ci categosec_ci
label var categosec_ci "Categoria ocupacional en la actividad secundaria"
* No se declara actividad secundaria

******************************
*	nempleos_ci
******************************
gen nempleos_ci=0     if emp_ci==1
replace nempleos_ci=1 if p40_otro_t==3
replace nempleos_ci=2 if p40_otro_t==1 | p40_otro_t==2

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
destring p36, replace force
gen m=p36-100 if p36>=100 & p36<=137
gen a=p36-200 if p36>=201 & p36<=273 

gen antiguedad_ci=.
replace antiguedad_ci=a
replace antiguedad_ci=m/12 if a==.
drop a m

******************************************************************************
*		INCOME
******************************************************************************
*Modificación Mayra Saenz Mayo 2014
foreach var of varlist _all {
qui destring `var', replace
qui capture recode `var' (99999=.) (999=.) (9999=.) (99998=.) (99999.99 =.) (9999.99=.)
}
******************************
*	ylmpri_ci & ylmpri1_ci
******************************
recode p381 p383 (99999.99 =.)
generat ylmpri_ci=p381 if p33<=6
replace ylmpri_ci=p383 if (p33>=7 & p33<=9) & p383<60000
replace ylmpri_ci=0    if categopri==4
replace ylmpri_ci=.    if emp_ci==0
label var ylmpri_ci "Ingreso laboral monetario act. principal (mes)"

******************************
*	nrylmpri_ci & nrylmpri1_ci
******************************
gen nrylmpri_ci=(((p381>=9999 & p381<.) | (p383>=9999 & p383<.)) & emp_ci==1)
replace nrylmpri_ci=. if emp_ci==0
label var nrylmpri_ci "Identificador de NR de ingreso"

******************************
* ylnmpri_ci 
******************************

gen ylnmpri_ci=p382 if p33<=6
replace ylnmpri_ci=p384 if (p33>=7 & p33<=9)
replace ylnmpri_ci=. if emp_ci==0
label var ylnmpri_ci "Ingreso laboral no monetario act. principal (mes)"


******************************
*	ylmsec_ci
******************************
* No se declara actividad secundaria
gen ylmsec_ci=. 
label var ylmsec_ci "Ingreso laboral monetario act secundaria (mes)"


****************
***ylnmsec_ci***
****************

g ylnmsec_ci=.
label var ylnmsec_ci "Ingreso laboral NO monetario actividad secundaria"


*****************
***ylmotros_ci***
*****************
recode p41 (9999.99=.) (99999=.) (99999.99=.)
gen ylmotros_ci=p41 if p40_otro_t==1 | p40_otro_t==2
replace ylmotros_ci = . if emp_ci==0
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

*egen ylm1_ci= rsum(ylmpri1_ci ylmsec_ci), missing

******************************
*	ylnm_ci
******************************

gen ylnm_ci=ylnmpri_ci
label var ylnm_ci "Ingreso laboral no monetario total"

******************************
*	ynlm_ci
******************************
recode p65a (99999=.)
gen jub=p65a if p65a>0 

recode p65c2_dine (99999 =.)
egen ayfam=rsum(p65c1_alim p65c2_dine p65c3_esco p65c4_alim p65c5_otro), missing 

recode p65b (99999=.)
gen pension=p65b  if p65b >0 

recode  p65d_alqui (99999=.)
gen alqui= p65d_alqui  if  p65d_alqui>0

gen loter= p65e_loter  if  p65e_loter >0 

egen becas=rsum(p65f1_beca p65f2_beca), missing

egen subsidios=rsum(p65g1_tran p65g2_sena p65g3_supl p65g4_insu p65g5_100a), missing 

recode p65h_decim p65l_otros (99999=.)

* 2014, 04. MLO reemplace el nombre de agro x agrop 
gen agrop=p65i_agrop if p65i_agrop>0 
egen otroy=rsum(p65l_otros p65j_parvi)  
gen habit=p65k_habit if p65k_habit>0 

egen ynlme= rsum(jub pension ayfam alqui loter becas subsidios habit otroy agrop) if emp_ci==1, missing
egen ynlmd= rsum(jub pension ayfam alqui loter becas subsidios habit otroy agrop) if emp_ci==0, missing

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
*by idh_ch, sort: egen ylm1_ch=sum(ylm1_ci) if miembros_ci==1, missing
label var ylm_ch "Ingreso laboral monetario del Hogar-ignora NR"

****************************
*    ylmnr_ch & ylmnr1_ch  
****************************
by idh_ch, sort: egen ylmnr_ch=sum(ylm_ci) if miembros_ci==1 & nrylmpri_ch==0, missing
*by idh_ch, sort: egen ylmnr1_ch=sum(ylm1_ci) if miembros_ci==1 & nrylmpri_ch==0, missing
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
gen ynlnm_ci=.
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
*gen ylmhopri1_ci=ylmpri1_ci/(horaspri_ci*4.3)

******************************
*	ylmho_ci & ylm1ho_ci
******************************
gen ylmho_ci=ylm_ci/(horastot_ci*4.3)
label var ylmho_ci "Salario monetario de todas las actividades"
*gen ylmho1_ci=ylm1_ci/(horastot_ci*4.3)

******************************************************************************
*	VARIABLES OF HOUSEHOLD INFRAESTRUCTURE 
******************************************************************************
notes: Survey de Panama no pregunta caracteristicas de vivienda por lo que no se pueden construir las variables aguared_ch,aguadist_ch,aguamala_ch,aguamide_ch,luz_ch,luzmide_ch,combust_ch,bano_ch,banoex_ch,des1_ch,des2_ch,piso_ch,pared_ch,techo_ch,resid_ch,dorm_ch,cuartos_ch,cocina_ch,telef_ch,refrig_ch,freez_ch,auto_ch,compu_ch,internet_ch,cel_ch,vivi1_ch,vivi2_ch,viviprop_ch,vivitit_ch,vivialq_ch,vivialqimp_ch.
 
gen luz_ch=1 if  h6a_luz_el==1
replace luz_ch=0 if  h6a_luz_el==2

gen combust_ch=0
replace combust_ch=1 if h5_combust==3

gen telef_ch=1 if h6c_telefo==1
replace telef_ch=0 if h6c_telefo==2

gen compu_ch=1 if h6b_comput==1
replace compu_ch=0 if h6b_comput ==2 

gen internet_ch=0
replace internet_ch=1 if h6b2_inter ==1

gen cel_ch=0
replace cel_ch=1 if h6d_celula ==1

gen aguared_ch=.
gen aguadist_ch=.
gen aguamala_ch=.
gen aguamide_ch=.
gen vivi1_ch=.
gen vivi2_ch=.
gen viviprop_ch=.
gen vivitit_ch=.
gen vivialq_ch=.
gen vivialqimp_ch=.
gen luzmide_ch=.
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
gen refrig_ch=.
gen freez_ch=.
gen auto_ch=.
gen dorm_ch=h4a_dormit
gen cuartos_ch=.
gen cocina_ch=.

******************************************************************************
*	EDUCATION
******************************************************************************

******************************
*	aedu_ci
******************************
generat grado=p8-10 if p8>=11 & p8<=16
replace grado=p8-20 if p8>=21 & p8<=23
replace grado=p8-30 if p8>=31 & p8<=36
replace grado=p8-40 if p8>=41 & p8<=43
replace grado=p8-50 if p8>=51 & p8<=59
replace grado=p8-60 if p8==61 
replace grado=p8-70 if p8>=71 & p8<=72
replace grado=p8-80 if p8>=82 & p8<=84

gen nivel=0 
replace nivel=1 if p8>=11 & p8<=16
replace nivel=2 if p8>=21 & p8<=23
replace nivel=3 if p8>=31 & p8<=36
replace nivel=4 if p8>=41 & p8<=43
replace nivel=5 if p8>=51 & p8<=59
replace nivel=6 if p8==61
replace nivel=7 if p8>=71 & p8<=72
replace nivel=8 if p8>=82 & p8<=84

gen aedu_ci=0            if nivel==0 
replace aedu_ci=grado    if nivel==1
replace aedu_ci=grado+6  if nivel==2 | nivel==3
replace aedu_ci=grado+12 if (nivel==4 | nivel==5) & grado<=7
replace aedu_ci=grado+17 if nivel>=6 & nivel<=8

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
gen asiste_ci=(p7==1)
replace asiste_ci=. if p7==0
label var asiste "Personas que actualmente asisten a centros de enseñanza"

******************************
*	pqnoasis_ci_ci
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

* PAN 2011
gen salmm_ci= . 
replace salmm_ci= 307.2 if rama_ci==1
replace salmm_ci= 434.4 if rama_ci==2
replace salmm_ci= 337.2 if rama_ci==3
replace salmm_ci= 480 if rama_ci==4
replace salmm_ci= 468 if rama_ci==5
replace salmm_ci= 351.6 if rama_ci==6
replace salmm_ci= 424 if rama_ci==7
replace salmm_ci= 469.2 if rama_ci==8
replace salmm_ci= 421.6 if rama_ci==9
replace salmm_ci= 410.36 if salmm_ci==.

*********
*lp_ci***
*********
destring areareco divi_reco, replace
gen lp_ci =.
replace lp_ci= 83.58 if areareco=="Urbana" & divi_reco==1 /* Cdad. Panamá*/
replace lp_ci= 83.58 if areareco=="Urbana" & divi_reco==3 /* Zona urbana districto san miguelito*/
replace lp_ci= 92.45 if ((divi_reco!=1 & divi_reco!=3) & areareco=="Urbana") | areareco=="Rural"  /* resto urbano o rural*/


label var lp_ci "Linea de pobreza oficial del pais"

*********
*lpe_ci***
*********

gen lpe_ci =.
replace lpe_ci= 45.20 if areareco=="Urbana" & divi_reco==1 /* Cdad. Panamá*/
replace lpe_ci= 45.20 if areareco=="Urbana" & divi_reco==3 /* Zona urbana districto san miguelito*/
replace lpe_ci= 47.05 if ((divi_reco!=1 & divi_reco!=3) & areareco=="Urbana") | areareco=="Rural"  /* resto urbano o rural*/

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
replace tipocontrato_ci=1 if (p34==1 | p34==4) & categopri_ci==3
replace tipocontrato_ci=2 if (p34==2 | p34==3) & categopri_ci==3
replace tipocontrato_ci=3 if (p34==5 | tipocontrato_ci==.) & categopri_ci==3
label var tipocontrato_ci "Tipo de contrato segun su duracion"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci


*************
*cesante_ci* 
*************
* MGD 12/4/2015: se corrige la inclusion de ceros con p24 >100 ya que antes solamnte constaba p24!=999
gen cesante_ci=1 if p26>100 & p26!=999 & p26!=. & condocup_ci==2
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
gen tamemp_ci=1 if p31_trabaj==1 
label var  tamemp_ci "Tamaño de Empresa" 
*Empresas medianas
replace tamemp_ci=2 if p31_trabaj==2 | p31_trabaj==3 | p31_trabaj==4
*Empresas grandes
replace tamemp_ci=3 if p31_trabaj==5
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
*2014, 02 incorporacion MLO
egen aux_p=rsum(p65a p65b), missing
destring aux_p, replace

gen pension_ci=1 if aux_p>0 & aux_p!=. & aux_p!=999999 & aux_p!=99999
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
**DZ Octubre 2017- Se genera variable pensionsub_ci
gen pensionsub_ci=1 if p65g5_100a>0 & p65g5_100a<=99999
recode pensionsub_ci .=0
label var pensionsub_ci "1=recibe pension subsidiada / no contributiva"

*****************
**ypensub_ci*
*****************
**DZ Octubre 2017- Se genera variable ypensub_ci
* este año se incluye programa 100 a los 70
gen ypensub_ci=p65g5_100a if p65g5_100a>0 & p65g5_100a<=99999
label var ypensub_ci "Valor de la pension subsidiada / no contributiva"


*************
*tecnica_ci**
*************

gen tecnica_ci=.
replace tecnica_ci=1 if  p8==41 | p8==42| p8==43 
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

