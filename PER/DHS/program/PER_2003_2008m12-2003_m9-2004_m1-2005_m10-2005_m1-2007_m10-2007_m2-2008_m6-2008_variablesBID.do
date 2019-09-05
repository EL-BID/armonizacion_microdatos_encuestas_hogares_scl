* (Versión Stata 12)
clear
set more off
*______________________________________________________________________________________________________*

	 * Activar si es necesario (dejar desactivado para evitar sobreescribir la base y dejar la 
	 *posibilidad de utilizar un loop)
	 * Los datos se obtienen de las carpetas que se encuentran en el servidor: \\Sdssrv03\surveys
	 * Se tiene acceso al servidor únicamente al interior del BID.
	 * El servidor contiene las bases de datos MECOVI.
	 * Las DHS son encuestas cuya población objetivo son las madres de 15-49 años y sus respectivos hijos.
*______________________________________________________________________________________________________*
 

global ruta = "\\Sdssrv03\surveys"

local PAIS PER
local ENCUESTA DHS
local ANO "2003_2008"
local ronda m12-2003_m9-2004_m1-2005_m10-2005_m1-2007_m10-2007_m2-2008_m6-2008 

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
*local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
                        
capture log close
log using "`log_file'", replace 

                        
/*______________________________________________________________________________________________________*
						 BASES DE DATOS DE ENCUESTAS DE SALUD - SOCIOMETRO 
	País: Perú
	Encuesta: DHS
	Round: m12-2003_m9-2004_m1-2005_m10-2005_m1-2007_m10-2007_m2-2008_m6-2008
	Versiones anteriores: Mayra Sáenz   E-mail: saenzmayra.a@gmail.com - mayras@iadb.org
						  Marcela Rubio E-mail: marcelarubio28@gmail.com - mrubio@IADB.ORG
	Última versión: Mayra Sáenz   E-mail: saenzmayra.a@gmail.com - mayras@iadb.org		
	Fecha última modificación: Marzo 16, 2015
									SCL - IADB*/
*______________________________________________________________________________________________________*


use `base_in', clear


	*____________________________________________________*
	*        Variables de identificación                 *
	*____________________________________________________*
	
	
	*Region según BID
	gen region_BID_c=3 
	label var region_BID_c "Regiones BID"
	label define region_BID_c 1 "Centroamérica_(CID)" 2 "Caribe_(CCB)" 3 "Andinos_(CAN)" 4 "Cono_Sur_(CSC)"
	label value region_BID_c region_BID_c
	
	
	*Region
	g region_c =v101
	replace region_c = hv024 if region_c ==.
	label define region_c 1 "Amazonas" 2 "Ancash" 3 "Apurimac" 4 "Arequipa" 5 "Ayacucho" 6 "Cajamarca" ///
	7 "Callao" 8 "Cusco" 9 "Huancavelica" 10 "Huanuco" 11 "Inca" 12 "Junin" 13 "La Libertad" 14 "Lambayeque" ///
	15 "Lima" 16 "Loreto" 17 "Madre De Dios" 18 "Moquegua" 19 "Pasco" 20 "Piura" 21 "Puno" 22 "San Martin" ///
	23 "Tacna" 24 "Tumbes" 25 "Ucayali" 26 "A.A. Cáceres" 27 "Chavín" 28 "Grau" 29 "Mariátegui" 30 "Libertadores" ///
	31 "Nor-Oriental", add modify
	label value region_c region_c
    label var region_c "DPA de cada País"
	
	
	*Para identificar si vive en Lima Capital - Tomo la variable de facto place of residence
	g capital_c = v026 == 0
	replace capital_c =. if v026==.
	replace capital_c =. if v026==.
	replace capital_c =1 if hv026==0 & capital_c ==.
	replace capital_c =0 if (hv026!=0 & hv026!=.) & capital_c ==.
	label var capital_c "Personas que viven en Lima"

	
	*Zona Urbana vs Rural
	g zona_c = v025
	replace zona_c = 0 if v025 == 2
	replace zona_c = hv025 if zona_c ==. & hv025 !=.
	replace zona_c = 0 if zona_c ==2
	label define zona_c 1 "Urban" 0 "Rural"
	label value zona_c zona_c
	label var zona_c "Zona Urbana vs Rural"
	
	
		
	*Unidad Primaria de muestreo
	g upm_c = v021
	replace upm_c = hv021 if upm_c ==. 
	label var upm_c "Unidad Primaria de Muestreo"
	
	
	*Estrato
	egen estrato_c = group(region_c zona_c), label
	
	
	* Número de conglomerado
	g conglomerado_c  = v001
	replace conglomerado_c  = hv001 if conglomerado_c =="."
	label var conglomerado_c "Número de conglomerado"	
	

	*País 
	g pais_c   = "PER"
	label var pais_c "Nombre del pais"
	
	
	*Anio de la encuesta
	g anio_c   = v007
	replace anio_c = hv007 if anio_c ==.
	label var anio_c "Año de la Encuesta"
	
	
	*Factor de Expansion a nivel individual
	g factor_c   = hv005
	label var factor_c "Factor de Expansion Madre e Hijos"
	
	
	*Identificador del hogar 
	g idh_ch =  idhogar
	*hhid
	label var idh_ch "ID del hogar"


	
	*Identificador de personas
	*g idp_ci = hvidx
	replace hvidx = "999" if hvidx == ""
	sort idh_ch hvidx
	replace hvidx = "" if hvidx == "999"
	bys idh_ch: gen idp_ci = _n
	label var idp_ci "ID de la persona en el hogar"
	
	*Mes de la encuesta 
	g mes_c = hv006
	*replace mes_c =  hv006 if mes_c ==. & hv006 !=.
	label var mes_c "Mes de la encuesta"
	
	*Quintil
	g quintil_ch = hv270
	*v190
	*replace quintil_c = hv270 if quintil_c ==. & hv270 !=.
	label var quintil_ch "Quintil de riqueza del hogar"
	
	
	*Relación de parentesco
	g relacion_ci = 1       if hv101 == 1
	replace relacion_ci = 2 if hv101 == 2
	replace relacion_ci = 3 if hv101 == 3
	replace relacion_ci = 4 if (hv101 >= 4 & hv101 <= 11)
	replace relacion_ci = 5 if (hv101 == 12)
	replace relacion_ci = 6 if (hv101 == 15)
	label variable relacion_ci "Relacion con el jefe del hogar"
	label define relacion_ci 1 "Jefe/a" 2 "Esposo/a" 3 "Hijo/a" 4 "Otros parientes" 5 "Otros no parientes"
	label define relacion_ci 6 "Empleado/a domestico/a", add
	label value relacion_ci relacion_ci
	label var relacion_ci "Relacion o parentesco con el jefe del hogar"
	
	*Sexo
	g sexo_ci = hv104
	label define sexo_ci 1 "Hombre" 2 "Mujer"
	label value sexo_ci sexo_ci
	label var sexo_ci "Sexo del individuo" 
	
	
	*Edad
	g edad_ci= hv105
	replace edad_ci =. if hv105 ==98
	label variable edad_ci "Edad del individuo"
	
	
	*Estado civil
	g civil_ci = 1 if hv115 ==0
	replace civil_ci = 2 if (hv115 >=1 & hv115 <=2)
	replace civil_ci = 3 if (hv115 ==4 | hv115 ==5)
	replace civil_ci = 4 if (hv115 ==3)
	
	label variable civil_ci "Estado civil"
	label define civil_ci 1 "Soltero" 2 "Union formal o informal"
	label define civil_ci 3 "Divorciado o separado" 4 "Viudo" , add
	label value civil_ci civil_ci
	label var civil_ci "Estado civil"
	
	
	*Número de familiares en el hogar
	by idh_ch, sort: egen nmiembros_ch=sum(relacion_ci>=1 & relacion_ci<=4)
	label variable nmiembros_ch "Numero de familiares en el hogar"
	
	
	*Miembro del hogar
	gen miembros_ci=(relacion_ci<5)
	label variable miembros_ci "Miembro del hogar"
	
	
	*Años de educación
	recode hv108 (98=.)
	g aedu_ci = hv108
	label var aedu_ci "Anios de educacion aprobados"
	
	*Nivel de educación de los miembros del hogar
	g       nivedu_ci =.
	replace nivedu_ci = 1 if (hv109>=0 & hv109<=1)
	replace nivedu_ci = 2 if (hv109==2)
	replace nivedu_ci = 3 if (hv109==3)
	replace nivedu_ci = 4 if (hv109==4) 
	replace nivedu_ci = 5 if (hv109==5) 
	label define nivedu_ci 1 "Primaria incompleta o menos" 2 "Primaria completa" 3 "Secundaria incompleta" 4 "Secundaria completa" 5 "Más de secundaria completa"
	label value nivedu_ci nivedu_ci
	label var nivedu_ci "Nivel de educación de la madre" 
	
	
	*Fuente de Agua potable
	g agua_ch = 1 if hv201 >=10 & hv201 <= 11
	replace agua_ch = 2 if (hv201 == 12) 
	replace agua_ch = 3 if (hv201 == 13)
	replace agua_ch = 4 if (hv201 >= 20 & hv201 <= 22)
	replace agua_ch = 5 if (hv201 ==43)
	replace agua_ch = 6 if (hv201 ==41 | hv201 ==42)
	replace agua_ch = 7 if (hv201 ==61)
	replace agua_ch = 8 if (hv201 ==40 | hv201 ==51 | hv201 ==71 | hv201 ==96)
	
	label define agua_ch  1 "Agua entubada dentro de la vivienda" 2 "Agua entubada fuera de la vivienda dentro del terreno" ///
	3 "Llave pública" 4 "Pozo" 5 "Río, acequia, lago, canal" 6 "Manantial protec/unprotec" 7 "Camión tanque agua" 8 "Otros"
	label value agua_ch agua_ch
	
	label var agua_ch "Fuente de agua para beber"
	
	
	*Luz electrica en el hogar
	g luz_ch = hv206
	label var luz_ch  "La principal fuente de iluminación es electricidad"

	
	*Radio en el hogar
	g radio_ch = hv207
	label var radio_ch "Tienen radio en el hogar"
	
	
	*Televisión en el hogar
	g tv_ch = hv208
	label var tv_ch "Tienen televisión en el hogar"
	
	
	*Refrigerador en el hogar
	g refrig_ch = hv209
	label var refrig_ch "Tienen refrigerador en el hogar"
	
	
	*Bicicleta en el hogar
	g bici_ch = hv210
	label var bici_ch "Tienen bicicleta en el hogar"
	
	
	*Motocicleta en el hogar
	g moto_ch = hv211
	label var moto_ch "Tienen motocicleta en el hogar"
	
	
	*Automovil en el hogar
	g auto_ch = hv212
	label var auto_ch "Tienen automovil en el hogar"
	
	
	*Teléfono en el hogar
	g telef_ch = hv221
	label var telef_ch "Tienen teléfono en el hogar"
		
	
	
*========================================================================================================*
*                                   VARIABLES MADRE E HIJO
*========================================================================================================*

	
	
	*Identificador de la madre
	g id_ma = caseid
	label var id_ma "Identificador de la madre"

	
	*Factor de Expansion a nivel individual
	g factor_c   = v005
	label var factor_c "Factor de Madre e Hijos"
		
	
	*________________________________________________________________*
	*        Características del niño                                *
	*________________________________________________________________*
	
	*Año de nacimiento del niño
	g anacim_ni = b2
	g anacimaux = int((b3-1)/12)+1900
	replace anacim_ni = anacimaux if anacim_ni ==.
	label var anacim_ni "Año de nacimiento del niño"
	
	
	*Mes de nacimiento del niño
	g mnacim_ni = (b3-((anacimaux-1900)*12))
	label var mnacim_ni "Mes de nacimiento del niño"
	
	
	*Edad del niño en meses
	g edadm_ni  = hw1 
	label var edadm_ni "Edad del niño en meses"
	
	
	*Edad del niño en años
	g edada_ni = b8
	label var edada_ni "Edad del niño en años"
	
	
	*Sexo del niño
	g sexo_ni     = b4
	label define sexo_ni 1 "Masculino" 2 "Femenino"
	label value  sexo_ni sexo_ni
	label var sexo_ni "Sexo del niño"
	
	
	*Gemelo
	g gemelo_ni  = b0>=1 & b0<=3
	label define gemelo_ni 1 "Si" 0 "No"
	label value  gemelo_ni gemelo_ni 
	label var gemelo_ni "El niño es gemelo =1"
	
	
	*Orden de nacimiento de los hijos
	g ordnacim_ni  = bord
	label var ordnacim_ni "Orden de nacimiento de los hijos"
	
	
	*Número de hermanos del niño
	sort anio_c caseid -ordnacim_ni
	g nherma_ni = v201 - 1
	label var nherma_ni "Número de hermanos del niño"
	
	
	*El niño está vivo 1=Sí
	g vivo_ni = b5
	label define vivo_ni 1 "Si" 0 "No"
	label value vivo_ni vivo_ni 
	label var vivo_ni "El niño está vivo 1=Sí"
	
	
	*Diferencia de edad (años) con los hermanos mayores
	sort id_ma -ordnacim_ni
	bys id_ma: g difedada_ni = ((anacim_ni[_n+1] - anacim_ni)*-1)*12
	label var difedada_ni "Diferencia sólo de años de edad (sin considerar meses) con los hermanos mayores" 
	
	
	*Diferencia de edad (meses) con los hermanos mayores
	bys id_ma: g difedadm_ni = (mnacim_ni[_n+1] - mnacim_ni)*-1
	label var difedadm_ni "Diferencia sólo de los meses de edad (sin considerar años) con los hermanos mayores" 
	
	
	*Diferencia de edad total (años y meses) con los hermanos mayores
	g difedad_ni =difedada_ni+difedadm_ni
	replace difedad_ni =0 if ordnacim_ni ==1
	label var difedad_ni "Diferencia de edad (suma años y meses) con los hermanos mayores" 
	
		
	*Edad del niño al morir (meses)
	g edadalmorm_ni =b7
	label var edadalmorm_ni "Edad del niño al morir (meses)"
	
	
	*Edad del niño al morir (días, meses, años)
	g edadalmord_ni =b6
    label var edadalmord_ni "Edad del niño al morir (días(1), meses(2), años(3))"
	
	
	*Diferencia entre año de la encuesta y año de nacimiento del niño
	g difencnacim_ni = anio_c - anacim_ni
	label var difencnacim_ni "Diferencia entre año de la encuesta y año de nacimiento del niño"
	
	
	*________________________________________________________________*
	*        Características de la madre                             *
	*________________________________________________________________*
	
	
	*Autoidentificación etnica del individuo
	g raza_ma = (v131==2 | v131==3 | v131==4)
	label define raza_ma 1 "Indígena" 0 "No Indígena"
	label value raza_ma raza_ma
	label var raza_ma "Autoidentificación etnica de la madre"

	
	*Mes de nacimiento de la madre 
	g mnacim_ma = v009
	label var mnacim_ma "Mes de nacimiento de la madre" 
	
	
	*Año de nacimiento de la madre
	g anacim_ma = v010
	label var anacim_ma "Año de nacimiento de la madre" 
	
	
	*Edad de la madre al momento de la encuesta
	g edad_ma  = v012
	label var edad_ma "Edad de la madre al momento de la encuesta"
	
	*Edad de la madre al momento del parto
	g aux1 = (anacim_ni-anacim_ma)*12
	g aux2 = (mnacim_ni-mnacim_ma)

	g edadparto_ma = round((aux1+aux2)/12)
	label var edadparto_ma "Edad de la madre al momento del parto"

	
	*Años de escolaridad de la madre
	g aedu_ma = v133
	label var aedu_ma "Años de escolaridad de la madre"
	
	
	*Nivel de educación de la madre
	g       nivedu_ma =.
	replace nivedu_ma = 1 if v106==0 | ((v107>=1 & v107<=5) & v106==1)
	replace nivedu_ma = 2 if (v107==6 & v106==1)
	replace nivedu_ma = 3 if ((v107>=1 & v107<=4) & v106==2)
	replace nivedu_ma = 4 if (v107>=5 & v106==2) 
	replace nivedu_ma = 5 if ((v107>=1) & v106==3)
	label define nivedu_ma 1 "Primaria incompleta o menos" 2 "Primaria completa" 3 "Secundaria incompleta" 4 "Secundaria completa" 5 "Más de secundaria completa"
	label value nivedu_ma nivedu_ma
	label var nivedu_ma "Nivel de educación de la madre"
	



/*_____________________________________________________________________________________________________*/
* Verificación de que se encuentren todas las variables armonizadas
/*_____________________________________________________________________________________________________*/

drop anacimaux aux1 aux2


order region_BID_c region_c capital_c conglomerado_c pais_c anio_c factor_c zona_c id_ma anacim_ni     ///
mnacim_ni edadm_ni edada_ni sexo_ni gemelo_ni ordnacim_ni nherma_ni vivo_ni difedada_ni difedadm_ni    ///
difedad_ni edadalmorm_ni edadalmord_ni difencnacim_ni raza_ma mnacim_ma anacim_ma edad_ma edadparto_ma ///  
aedu_ma nivedu_ma, first

set more off
compress


saveold "`base_out'", replace

log close












