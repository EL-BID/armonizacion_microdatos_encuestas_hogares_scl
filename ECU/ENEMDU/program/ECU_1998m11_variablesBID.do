
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

local PAIS ECU
local ENCUESTA ENEMDU
local ANO "1998"
local ronda m11 


local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\\`PAIS'_`ANO'`ronda'.dta"
*local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
   


capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Ecuador
Encuesta: ENEMDU
Round: m12
Autores: Mayra Sáenz
Última versión: Mayra Sáenz E-mail: mayras@iadb.org / saenzmayra.a@gmail.com
Fecha última modificación: 24 de Septiembre de 2013

							SCL/LMK - IADB
****************************************************************************/
/***************************************************************************
Detalle de procesamientos o modificaciones anteriores:
*2014, 01 MLO cambio del limite de edad de condocup_ci a 5+
****************************************************************************/


use `base_in', clear

		*************************
		***VARIABLES DEL HOGAR***
		*************************
  ************
* Region_BID *
************
gen region_BID_c=.
replace region_BID_c=3 
label var region_BID_c "Regiones BID"
label define region_BID_c 1 "Centroamérica_(CID)" 2 "Caribe_(CCB)" 3 "Andinos_(CAN)" 4 "Cono_Sur_(CSC)"
label value region_BID_c region_BID_c

*********************
*** Region_c *********
**********************
destring ciudad, replace
gen region_c=int(ciudad/10000)
	recode  region_c (14/16=89) (19/22=89)
	label define region_c ///
	1 "Azuay" ///
	2 "Bolívar" ///
	3 "Cañar" ///
	4 "Carchi" /// 
	5 "Cotopaxi" ///
	6 "Chimborazo" ///
	7 "El Oro" ///
	8 "Esmeraldas" ///
	9 "Guayas" ///
	10 "Imbabura" ///
	11 "Loja" ///
	12 "Los Ríos" ///
	13 "Manabí" ///
	17 "Pichincha" ///
	18 "Tungurahua" ///
	89 "Amazonia" 
   label value region_c region_c


   	***************
	***  ine01  ***
	***************
	gen ine01=.
	replace ine01=int(ciudad/10000)
	
	label define ine01 ///
	1 "Azuay" ///
	2 "Bolívar" ///
	3 "Cañar" ///
	4 "Carchi" /// 
	5 "Cotopaxi" ///
	6 "Chimborazo" ///
	7 "El Oro" ///
	8 "Esmeraldas" ///
	9 "Guayas" ///
	10 "Imbabura" ///
	11 "Loja" ///
	12 "Los Ríos" ///
	13 "Manabí" ///
	14 "Morona Santiago" ///
	15 "Napo" ///
	16 "Pastaza" ///
	17 "Pichincha" ///
	18 "Tungurahua" ///
	19 "Zamora Chinchipe" ///
	20 "Galápagos" ///
	21 "Sucumbíos" ///
	22 "Orellana" ///
	23 "Santo Domingo de los Tsáchilas" ///
    24 "Santa Elena" 
   label value ine01 ine01
   label var ine01 "division politico-administrativa, provincia"


   	***************
	***  ine02  ***
	***************
	gen ine02=.
	replace ine02=int(ciudad/100)
	
	label define ine02 101 "Cuenca" 102 "Girón" 103 "Gualaceo" 104 "Nabón" 105 "Paute" 106 "Pucará" 107 "San Fernando" /*
	*/ 108 "Santa Isabel" 109 "Sigsig" 110 "Oña" 111 "Chordeleg" 112 "El Pan" 113 "Sevilla De Oro" 114 "Guachapala" /*
	*/ 115 "Camilo Ponce Enríquez" 201 "Guaranda" 202 "Chillanes" 203 "Chimbo" 204 "Echeandia" 205 "San Miguel" /*
	*/ 206 "Caluma" 207 "Las Naves" 301 "Azoques" 302 "Biblian" 303 "Cañar" 304 "La Troncal" 305 "El Tambo" 306 "Deleg"/*
	*/ 307 "Suscal" 401 "Tulcán" 402 "Bolívar" 403 "Espejo" 404 "Mira" 405 "Montúfar" 406 "San Pedro De Huaca" 501 "Latacunga" /*
	*/ 502 "La Maná" 503 "Pangua" 504 "Pujilí" 505 "Salcedo" 506 "Saquisilí" 507 "Sigchos" 601 "Riobamba" /*
	*/ 602 "Alausí" 603 "Colta" 604 "Chambo" 605 "Chunchi" 606 "Guamote" 607 "Guano" 608 "Pallatanga" 609 "Penípe" /*
	*/ 610 "Cumandá" 701 "Machala" 702 "Arenillas" 703 "Atahualpa" 704 "Balsas" 705 "Chilla" 706 "El Guabo" /*
	*/ 707 "Huaquillas" 708 "Marcabelí" 709 "Pasaje" 710 "Piñas" 711 "Portovelo" 712 "Santa Rosa" 713 "Zaruma" /*
	*/ 714 "Las Lajas" 801 "Esmeraldas" 802 "Eloy Alfaro" 803 "Muisne" 804 "Quininde" 805 "San Lorenzo" 806 "Atacames" /*
	*/ 807 "Ríoverde" 901 "Guayaquil" 902 "Alfredo Baquerizo Moreno" 903 "Balao" 904 "Balzar" 905 "Colimes" /*
	*/ 906 "Daule" 907 "Durán" 908 "Empalme" 909 "El Triunfo" 910 "Milagro" 911 "Naranjal"	912 "Naranjito" /*
	*/ 913 "Palestina" 914 "Pedro Carbo" 916 "Samborondón" 918 "Santa Lucia" 919 "Salitre" 920 "San Jacinto De Yaguachi" /*
	*/ 921 "Playas" 922 "Simón Bolívar" 923 "Crnel. Marcelino Maridueña" 924 "Lomas De Sargentillo" 925 "Nobol" /*
	*/ 927 "Gnral. Antonio Elizalde" 928 "Isidro Ayora" 1001 "Ibarra" 1002 "Antonio Ante" 1003 "Cotacahi" 1004 "Otavalo" /*
	*/ 1005 "Pimampiro" 1006 "San Miguel De Urcuqui" 1101 "Loja" 1102 "Calvas" 1103 "Catamayo" 1104 "Célica"/*
	*/ 1105 "Chaguarpamba" 1106 "Espíndola" 1107 "Gonzanamá" 1108 "Macará" 1109 "Paltas" 1110 "Puyango" 1111 "Saraguro" /*
	*/ 1112 "Sozoranga" 1113 "Zapotillo" 1114 "Pindal" 1115 "Quilanga" 1116 "Olemdo" 1201 "Babahoyo" 1202 "Baba" /*
	*/ 1203 "Montalvo" 1204 "Puebloviejo" 1205 "Quevedo" 1206 "Urdaneta" 1207 "Ventanas" 1208 "Vinces" 1209 "Palenque" /*
	*/ 1210 "Buena Fé" 1211 "Valencia" 1212 "Mocache" 1213 "Quinsaloma" 1301 "Portoviejo" 1302 "Bolívar" 1303 "Chone" /*
	*/ 1304 "El Carmen" 1305 "Flavio Alfaro" 1306 "Jipijapa" 1307 "Junín" 1308 "Manta" 1309 "Montecristi" 1310 "Paján" /*
	*/ 1311 "Pichincha" 1312 "Rocafuerte" 1313 "Santa Ana" 1314 "Sucre" 1315 "Tosagua" 1316 "24 De Mayo" 1317 "Pedernales" /*
	*/ 1318 "Olmedo" 1319 "Puerto López" 1320 "Jama" 1321 "Jaramijó" 1322 "San Vicente" 1401 "Morona" 1402 "Gualaquiza" /*
	*/ 1403 "Limón Indanza" 1404 "Palora" 1405 "Santiago" 1406 "Sucúa" 1407 "Huamboya" 1408 "San Juan Bosco" 1409 "Taisha" /*
	*/ 1410 "Logroño" 1411 "Pablo Sexto" 1412 "Tiwintza" 1501 "Tena" 1503 "Archidona" 1504 "El Chaco" 1507 "Quijos" /*
	*/ 1509 "Carlos Julio Arrosemena Tola" 1601 "Pastaza" 1602 "Mera" 1603 "Santa Clara" 1604 "Arajuno" 1701 "Quito" /*
	*/ 1702 "Cayambe" 1703 "Mejía" 1704 "Pedro Moncayo" 1705 "Rumiñahui" 1707 "San Miguel De Los Bancos" 1708 "Pedro Vicente Maldonado" /*
	*/ 1709 "Puerto Quito" 1801 "Ambato" 1802 "Baños De Agua Santa" 1803 "Cevallos" 1804 "Mocha" 1805 "Patate" 1806 "Quero" /*
	*/ 1807 "San Pedro De Pelileo" 1808 "Santiago De Pillaro" 1809 "Tisaleo" 1901 "Zamora" 1902 "Chinchipe" 1903 "Nangaritza" /*
	*/ 1904 "Yacuambi" 1905 "Yantzaza" 1906 "El Pangui" 1907 "Centinela Del Cóndor" 1908 "Palanda" 1909 "Paquisha" 2001 "San Cristobal" /*
	*/ 2002 "Isabela" 2003 "Santa Cruz" 2101 "Lago Agrio" 2102 "Gonzalo Pizarro" 2103 "Putumayo" 2104 "Shushufindi" 2105 "Sucumbios" /*
	*/ 2106 "Cascales" 2107 "Cuyabeno" 2201 "Orellana" 2202 "Aguarico" 2203 "La Joya De Los Sachas" 2204 "Loreto" 2301 "Santo Domingo" /*
	*/ 2302 "La Condordía" 2401 "Santa Elena" 2402 "La Libertad" 2403 "Salinas"
	label value ine02 ine02
	label var ine02 "division politico-administrativa, cantones"

	
	***************
	***factor_ch***
	***************
	gen factor_ch=fexp
	label variable factor_ch "Factor de expansion del hogar"
	
	*************
	****idh_ch***
	*************
	sort ciudad zona sector vivienda hogar
	egen idh_ch = group( ciudad zona sector vivienda hogar)
	label variable idh_ch "ID del hogar"
	
	*************
	****idh_ci***
	*************
	gen idp_ci=persona
	label variable idp_ci "ID de la persona en el hogar"
	
	*************
	****zona_c***
	*************
	
	*Sólo urbana.
	gen zona_c=1 		
	label variable zona_c "Zona del pais"
	label define zona_c 1 "Urbana" 0 "Rural"
	label value zona_c zona_c

	*************
	****pais_c***
	*************
	gen str3 pais_c="ECU"
	label variable pais_c "Pais"

	************
	***anio_c***
	************
	
	gen anio_c=1998
	label variable anio_c "Anio de la encuesta" 
	
	***********
	***mes_c***
	***********
	gen mes_c=11
	label var mes_c "Mes de la encuesta" 
	
	*****************
	***relacion_ci***
	*****************
	generat relacion_ci=1 if reljefe==1
	replace relacion_ci=2 if reljefe==2
	replace relacion_ci=3 if reljefe==3
	replace relacion_ci=4 if reljefe>=4 & reljefe<=7
	replace relacion_ci=5 if reljefe==9
	replace relacion_ci=6 if reljefe==8
	label variable relacion_ci "Relacion con el jefe del hogar"
	label define relacion_ci 1 "Jefe/a" 2 "Esposo/a" 3 "Hijo/a" 4 "Otros parientes" 5 "Otros no parientes" 6 "Empleado/a domestico/a"
	label value relacion_ci relacion_ci


			****************************
			***VARIABLES DEMOGRAFICAS***
			****************************
	
	***************
	***factor_ci***
	***************
	gen factor_ci=fexp
	label variable factor_ci "Factor de expansion del individuo"
	
	*************
	***sexo_ci***
	*************
	gen sexo_ci=sexo
	label var sexo_ci "Sexo del individuo" 
	label def sexo_ci 1 "Hombre" 2 "Mujer" 
	label val sexo_ci sexo_ci

	**********
	***edad***
	**********
	gen edad_ci=edad if edad<99
	label variable edad_ci "Edad del individuo"

	*************************
	*** VARIABLES DE RAZA ***
	*************************

	* MGR Oct. 2015: modificaciones realizadas en base a metodología enviada por SCL/GDI Maria Olga Peña

	gen raza_idioma_ci = . 
	gen id_ind_ci = .
	gen id_afro_ci = .	
	*En este año  no se dispone de esta variable.
   gen raza_ci=.
   label define raza_ci 1 "Indígena" 2 "Afro-descendiente" 3 "Otros"
   label value raza_ci raza_ci 
   label var raza_ci "Raza o etnia del individuo"  

	**************
	***civil_ci***
	**************
	gen civil_ci=.
	label var civil_ci "Estado civil" 
	label def civil_ci 1"Soltero"  2"Union formal o informal" 3"Divorciado o separado" 4"Viudo" 
	label val civil_ci civil_ci
		
	*************
	***jefe_ci***
	*************
	gen jefe_ci=(relacion_ci==1)
	label var jefe_ci "Jefe de hogar"
	label def jefe_ci 1"Si" 0"No"
	label val jefe_ci jefe_ci

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
	by idh_ch, sort: egen nmiembros_ch=sum(relacion_ci>=1 & relacion_ci<5)
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

	*****************
	***miembros_ci***
	*****************
	gen miembros_ci=(relacion_ci<5)
	label variable miembros_ci "Miembro del hogar"


			***********************************
			***VARIABLES DEL MERCADO LABORAL***
			***********************************
        	***********************************

************************
*Condición de ocupación*
************************
/*2014, 01 MLO cambio del limite de edad de condocup_ci a 5+
*Ocupado*
gen condocup_ci=1 if trabajo==1 | aunotra==1
*Desocupado*
replace condocup_ci=2 if (trabajo==2 | aunotra==2) & bustrama==1 
*Inactivo*
replace condocup_ci=3 if (condocup_ci ~=1 & condocup_ci ~=2)
*No responde*
*replace condocup_ci=4 if edad<=9
replace condocup_ci=4 if edad<5
*/
*MGD-> el limite de la encuesta pasa a ser 10 anios;
*ademas, se genera con las variables originales para corregir el desempleo oculto. 06/04/2014
	generat condocup_ci=.
	replace condocup_ci=1 if trabajo==1 | actayuda<12 | aunotra==1 
	replace condocup_ci=2 if (trabajo==2 | actayuda==12 | aunotra==2 ) & (bustrasa==1 | bustrama==1)
	replace condocup_ci=3 if condocup_ci!=1 & condocup_ci!=2
	replace condocup_ci=4 if edad_ci<10
	label define condocup_ci 1 "ocupados" 2 "desocupados" 3 "inactivos" 4 "menor de PET"
	label value condocup_ci condocup_ci
	label var condocup_ci "Condicion de ocupacion utilizando definicion del pais"

***********
* Cesante *
***********
gen cesante_ci=1 if condocup_ci==2 & trabant==1
replace cesante_ci=0 if condocup_ci==2 & trabant==2
label var cesante_ci "Cesante. Desocupado con intención de trabajar "

	/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/
****************
*cotizando_ci***
****************
gen cotizando_ci=.
label var cotizando_ci "Cotizante a la Seguridad Social"

********************
*** instcot_ci *****
********************
gen instcot_ci=.
label var instcot_ci "institución a la cual cotiza"

****************
*afiliado_ci****
****************
gen afiliado_ci=1 if iess==1	
replace afiliado_ci=0 if iess==2
label var afiliado_ci "Afiliado a la Seguridad Social"

****************
*instpen_ci*****
****************
gen instpen_ci=.
label var instpen_ci "Institucion proveedora de la pension - variable original de cada pais" 
gen tipopen_ci=.



**************
**ypen_ci*
**************
*2014,01 Modificacion MLO
*gen ypen_ci=ingjub
* MGD 12/17/2015: faltaba dividir por el tipo de cambio
gen ypen_ci=ingjub/6449 if ingjub>0 & ingjub!=. & ingjub<9999998
label var ypen_ci "Valor de la pension contributiva"

***************
*pensionsub_ci*
***************
gen pensionsub_ci=.
label var pensionsub_ci "1=recibe pension subsidiada / no contributiva"

*****************
**ypensub_ci*
*****************
gen ypensub_ci=.
label var ypensub_ci "Valor de la pension subsidiada / no contributiva"

***************
***tecnica_ci**
***************
gen tecnica_ci=.
label var tecnica_ci "1=formacion terciaria tecnica"

***************
***pension_ci**
***************
*2014, 01 modificacion MLO
*gen pension_ci=1 if ingjub!=.
gen pension_ci=1 if ingjub>0 & ingjub!=. & ingjub<9999998
recode pension_ci .=0
label var pension_ci "1=Recibe pension contributiva"

*********
*lp_ci***
*********
gen lp_ci=29.1
label var lp_ci "Linea de pobreza oficial del pais"

*********
*lpe_ci***
*********
gen lpe_ci=26.2
label var lpe_ci "Linea de indigencia oficial del pais"

*********
*salmm_ci***
*********
* Se utiliza el tipo de cambio en mercado libre de cambios

gen salmm_ci=(100000/6499)
label var salmm_ci "Salario mínimo legal"			
			
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
/*
	************
	***emp_ci***
	************

	gen emp_ci= (condact>=0 & condact<=4)
	label var emp_ci "Ocupado (empleado)"

	****************
	***desemp1_ci***
	****************
	
	gen desemp1_ci=0 		
	replace desemp1_ci=1 if (condact==5 | condact==6) & bustrasa==1
	label var desemp1_ci "Desempleado que buscó empleo en el periodo de referencia"


	****************
	***desemp2_ci***
	****************
	gen desemp2_ci=0
	replace desemp2_ci=1 if desemp1_ci==1 | (motnobus==7 | motnobus==8) 
	label var desemp2_ci "desemp1_ci + personas que esperan respuesta a solicitud o temporada alta"

	****************
	***desemp3_ci***
	**************** 
	gen desemp3_ci=0
	replace desemp3_ci =1 if desemp2_ci==1 |  bustrama==1
	label var desemp3_ci "desemp2_ci + personas que buscaron antes del periodo de referencia"

	*************
	***pea1_ci***
	*************
	gen pea1_ci=(emp_ci==1 | desemp1_ci==1)
	label var pea1_ci "Población Económicamente Activa con desemp1_ci"

	*************
	***pea2_ci***
	*************
	gen pea2_ci= (emp_ci==1 | desemp2_ci==1)
	label var pea2_ci "Población Económicamente Activa con desemp2_ci"	

	*************
	***pea3_ci***
	*************
	gen pea3_ci=(emp_ci==1 | desemp3_ci==1)
	label var pea3_ci "Población Económicamente Activa con desemp3_ci"
*/
	*****************
	***desalent_ci***
	*****************
	gen desalent_ci=(motnobus==5 | motnobus==6)
	label var desalent_ci "Trabajadores desalentados"
	
	*****************
	***horaspri_ci***
	*****************
	gen horaspri_ci=hortrahp if hortrahp<999 
	replace horaspri_ci=. if emp_ci==0
	label var horaspri_ci "Horas trabajadas semanalmente en el trabajo principal"

	*****************
	***horastot_ci***
	*****************
	for var hortrahp: recode X (999=.) 
	for var hortrahs hortraho: recode X (99=.)
	egen horastot_ci=rsum(hortrahp hortrahs hortraho) if emp_ci==1 
	replace horastot_ci=. if hortrahp==. & hortrahs==. & hortraho==.
	replace horastot_ci=. if emp_ci==0
	label var horastot_ci "Horas trabajadas semanalmente en todos los empleos"
	
	***************
	***subemp_ci***
	***************
	*Modificacion MGD 06/18/2014 solo horas de actividad principal.
	gen subemp_ci=0
	replace subemp_ci=1 if (hormas>=4 & hormas<=8) & horaspri_ci<=30 & emp_ci==1
	label var subemp_ci "Personas en subempleo por horas"

	*******************
	***tiempoparc_ci***
	*******************
	
	* MGR: Modifico serie en base a correcciones Laura Castrillo: se debe utilizar horaspri en lugar de horastot como había sido generada antes
	gen tiempoparc_ci=((horaspri_ci>=1 & horaspri_ci<30) & ratmeh==3 & emp_ci==1)
	replace tiempoparc_ci=. if emp_ci==0
	label var tiempoparc_c "Personas que trabajan medio tiempo" 

	******************
	***categopri_ci***
	******************
	gen categopri_ci=.
	replace categopri_ci=1 if catetrab==3
	replace categopri_ci=2 if catetrab==4
	replace categopri_ci=3 if catetrab==6 | catetrab==7 | catetrab == 8
	replace categopri_ci=4 if catetrab==5
	replace categopri_ci=. if catetrab==9
	replace categopri_ci=. if emp_ci==0
	label define categopri_ci 1"Patron" 2"Cuenta propia" 0"Otro"
	label define categopri_ci 3"Empleado" 4" No remunerado" , add
	label value categopri_ci categopri_ci
	label variable categopri_ci "Categoria ocupacional"

	******************
	***categosec_ci***
	******************
	gen categosec_ci=.
	replace categosec_ci=1 if cates==3
	replace categosec_ci=2 if cates==4	
	replace categosec_ci=3 if cates==6 | cates==7 | cates==8
	replace categosec_ci=4 if cates==5	
	replace categosec_ci=. if cates==9								
	replace categosec_ci=. if emp_ci==0
	label define categosec_ci 1"Patron" 2"Cuenta propia" 0"Otro"
	label define categosec_ci 3"Empleado" 4" No remunerado" , add
	label value categosec_ci categosec_ci
	label variable categosec_ci "Categoria ocupacional en la segunda actividad"

	*****************
	*tipocontrato_ci*
	*****************
	/*
	gen tipocontrato_ci=estabil
	label var tipocontrato_ci "Tipo de contrato segun su duracion"
	label define tipocontrato_ci 1 "nombramiento" 2 "Definitivo o indefinido" 3 "Temporal u obra cierta" 4"Otros"
	label value tipocontrato_ci tipocontrato_ci
	*/
	* Segun la clasificacion de la encuesta. MGD 6/12/2014
	generat tipocontrato_ci=. /* Solo disponible para asalariados*/
	replace tipocontrato_ci=1 if (estabil>=1 & estabil<=2) & categopri_ci==3
	replace tipocontrato_ci=2 if (estabil==3)              & categopri_ci==3
	replace tipocontrato_ci=3 if ((estabil==4) | tipocontrato_ci==.) & categopri_ci==3
	label var tipocontrato_ci "Tipo de contrato segun su duracion en act principal"
	label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
	label value tipocontrato_ci tipocontrato_ci


	*****************
	***contrato_ci***
	*****************
	gen contrato_ci=(estabil==2 | estabil==3)
	label var contrato_ci "Ocupados que tienen contrato firmado de trabajo"

	***************
	***segsoc_ci***
	***************
	gen segsoc_ci=(iess==1) 
	replace segsoc_ci=. if emp_ci~=1
	label var segsoc_ci "Personas que tienen seguridad social en PENSIONES por su trabajo"

	*****************
	***nempleos_ci***
	*****************
	generat nempleos_ci=1 if numtrab==3
	replace nempleos_ci=2 if numtrab==4
	replace nempleos_ci=. if emp_ci!=1
	label var nempleos_ci "Número de empleos"
	label define nempleos_ci 1 "Un empleo" 2 "Mas de un empleo"
	label value nempleos_ci nempleos_ci
/*
	*****************
	***firmapeq_ci***
	*****************
	gen firmapeq_ci=.
	replace firmapeq_ci=1 if (npertra>=1 & npertra<=5)
	replace firmapeq_ci=0 if (npertra>=6 & npertra<99)| (pertrabn==2)
	label var firmapeq_ci "Trabajadores informales"
	label def firmapeq_ci 1"5 o menos trabajadores" 0"Mas de 5 trabajadores"
	label val firmapeq_ci firmapeq_ci
	*/	
*****************
***tamemp_ci***
*****************
gen tamemp_ci=1 if npertra>=1 & npertra<=5
label var  tamemp_ci "Tamaño de Empresa" 
*Empresas medianas
replace tamemp_ci=2 if npertra>=6 & npertra<=50
*Empresas grandes
replace tamemp_ci=3 if pertrabn==2 | (npertra>50 & npertra!=.) 
label define tamaño 1"Pequeña" 2"Mediana" 3"Grande"
label values tamemp_ci tamaño		
*******************
***categoinac_ci***
*******************
gen categoinac_ci =1 if (condina==4 & condocup_ci==3)
replace categoinac_ci = 2 if  ( condina==5 & condocup_ci==3)
replace categoinac_ci = 3 if  ( condina==6 & condocup_ci==3)
replace categoinac_ci = 4 if  ((categoinac_ci ~=1 & categoinac_ci ~=2 & categoinac_ci ~=3) & condocup_ci==3)
label var categoinac_ci "Categoría de inactividad"
label define categoinac_ci 1 "jubilados o pensionados" 2 "Estudiantes" 3 "Quehaceres domésticos" 4 "Otros"	
	
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
* MLO 2014, 04 se utiliza afiliacion para la seria entes de 2001 en ECU
replace formal=1 if afiliado_ci==1 & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1 & pais_c=="ECU" & anio_c<=2000


gen byte formal_ci=.
replace formal_ci=1 if formal==1 & (condocup_ci==1 | condocup_ci==2)
replace formal_ci=0 if formal_ci==. & (condocup_ci==1 | condocup_ci==2) 
label var formal_ci "1=afiliado o cotizante / PEA"

* MGD 03/2016: Para comprobar nueva serie
g formal_1=formal_ci

	*****************
	***spublico_ci***
	*****************
	gen spublico_ci=(catetrab==6 & emp_ci==1)
	replace spublico_ci=. if emp_ci==.
	label var spublico_ci "Personas que trabajan en el sector público"

	**************
	***ocupa_ci***
	**************
	rename grupo p41
	* Modificacion MGD 07/29/2014: se utiliza CIUO-68.
	generat ocupa_ci=.
	replace ocupa_ci=1 if (p41>=0 & p41<=128) & emp_ci==1
	replace ocupa_ci=2 if (p41>=200 & p41<=279) & emp_ci==1
	replace ocupa_ci=3 if (p41>=300 & p41<=399) & emp_ci==1
	replace ocupa_ci=4 if (p41>=400 & p41<=490) & emp_ci==1
	replace ocupa_ci=5 if (p41>=500 & p41<=599) & emp_ci==1
	replace ocupa_ci=6 if (p41>=600 & p41<=699) & emp_ci==1
	replace ocupa_ci=7 if ((p41>=700 & p41<=989) | (p41>=9300 & p41<=9333)) & emp_ci==1
	replace ocupa_ci=9 if (p41==999) & emp_ci==1
	label define ocupa_ci 1"profesional y tecnico" 2"director o funcionario sup" 3"administrativo y nivel intermedio"
	label define ocupa_ci  4 "comerciantes y vendedores" 5 "en servicios" 6 "trabajadores agricolas", add
	label define ocupa_ci  7 "obreros no agricolas, conductores de maq y ss de transporte", add
	label define ocupa_ci  8 "FFAA" 9 "Otras ", add
	label value ocupa_ci ocupa_ci
	label variable ocupa_ci "Ocupacion laboral" 
	
	
	*************
	***rama_ci***
	*************
	*Nota: se contruye la variable rama_ci siguiendo a CIUU revision2 
	* Ultima actualizacion MGD 4/16/2014
	gen rama_ci=.
	replace rama_ci = 1 if (rama>=1110 & rama<=1302) & emp_ci==1
	replace rama_ci = 2 if (rama>=2100 & rama<=2909) & emp_ci==1
	replace rama_ci = 3 if (rama>=3111 & rama<=3909) & emp_ci==1
	replace rama_ci = 4 if (rama>=4101 & rama<=4200) & emp_ci==1
	replace rama_ci = 5 if rama==5000 & emp_ci==1
	replace rama_ci = 6 if (rama>=6100 & rama<=6320) & emp_ci==1
	replace rama_ci = 7 if (rama>=7111 & rama<=7200) & emp_ci==1
	replace rama_ci = 8 if (rama>=8101 & rama<=8330) & emp_ci==1
	replace rama_ci = 9 if (rama>=9100 & rama<=9600) & emp_ci==1
	label var rama_ci "Rama de actividad"
	label def rama_ci 1"Agricultura, caza, silvicultura y pesca" 2"Explotación de minas y canteras" 3"Industrias manufactureras"
	label def rama_ci 4"Electricidad, gas y agua" 5"Construcción" 6"Comercio, restaurantes y hoteles" 7"Transporte y almacenamiento", add
	label def rama_ci 8"Establecimientos financieros, seguros e inmuebles" 9"Servicios sociales y comunales", add
	label val rama_ci rama_ci	
	
	****************
	***durades_ci***
	****************
	
	gen durades_ci=tiembus if (tiembus!=88 & tiembus!=99)
	replace durades_ci=0.5 if tiembus==88	
	label variable durades_ci "Duracion del desempleo en meses"

	***************
	*antiguedad_ci*
	***************
	gen antiguedad_ci=.
	label var antiguedad_ci "antiguedad laboral (anios) - aproximacion"	



			**************************
			***VARIABLES DE INGRESO***
			**************************
/*
foreach var of varlist ingpat ingasg ingepv ingdom ingjub ingalq ingotr ingrl {
replace `var'=. if `var'==999
replace `var'=. if `var'==9999
replace `var'=. if `var'==99999
replace `var'=. if `var'==999999
replace `var'=. if `var'==9999998
replace `var'=. if `var'==9999999
replace `var'=. if `var'==99999999
replace `var'=. if `var'==999999999
replace `var'=. if `var'==39999999
replace `var'=. if `var'==89999999
replace `var'=. if `var'==89999999
replace `var'=. if `var'==10000010
replace `var'=. if `var'==10333333
}
*/

*2014, 01 modificacion MLO
foreach var of varlist ingpat ingasg ingepv ingdom ingjub ingalq ingotr ingrl {
replace `var'=. if `var'>=9999998
}

	
/*

              storage  display     value
variable name   type   format      label      variable label
---------------------------------------------------------------
ingpat          long   %12.0g                 ingresos como
                                           patrono o cuenta
                                           propia
ingasg          long   %12.0g                 ingresos como
                                           asalariado de
                                           gobierno
ingepv          long   %12.0g                 ingreso
                                           asalariado empresa
                                           privada
ingdom          long   %12.0g                 ingreso como
                                           empleada doméstica
ingjub          long   %12.0g                 ingresos por
                                           jubilación o pensión
ingalq          long   %12.0g                 ingreso por
                                           alquileres, rentas o
                                           intereses
ingotr          long   %12.0g                 por otros
                                           ingresos
ingrl           long   %12.0g                 ingresos

*/
			
	***************
	***ylmpri_ci***           
	***************
	*Aquí se genera como valor perdido porque en este año la pregunta es: Cuáles fueron sus
	*ingresos en el mes de octubre para todos sus trabajos. Por lo tanto, no se puede distinguir
	*entre ingresos de la actividad principal y la secundaria.
	
	gen ylmpri_ci = .
	label var ylmpri_ci "Ingreso laboral monetario actividad principal" 

	*****************
	***nrylmpri_ci***
	*****************
	gen nrylmpri_ci=.
	label var nrylmpri_ci "Id no respuesta ingreso de la actividad principal"  

	****************
	***ylnmpri_ci***
	****************
	gen ylnmpri_ci=.
	label var ylnmpri_ci "Ingreso laboral NO monetario actividad principal"   

	***************
	***ylmsec_ci***
	***************
	gen ylmsec_ci= . 
	label var ylmsec_ci "Ingreso laboral monetario segunda actividad" 

	****************
	***ylnmsec_ci***
	****************
	gen ylnmsec_ci=. 
	label var ylnmsec_ci "Ingreso laboral NO monetario actividad secundaria"
	
	**********************************************************************************************
***TCYLMPRI_CH : Identificador de los hogares en donde alguno de los miembros reporta como
*** top-code el ingreso de la actividad principal. .
***********************************************************************************************
gen tcylmpri_ch = .
label var tcylmpri_ch "Id hogar donde algún miembro reporta como top-code el ingr de activ. principal"

***********************************************************************************************
***TCYLMPRI_CI : Identificador de top-code del ingreso de la actividad principal.
***********************************************************************************************
gen tcylmpri_ci = .
label var tcylmpri_ci "Identificador de top-code del ingreso de la actividad principal"

	*****************
	***ylmotros_ci***
	*****************

	gen ylmotros_ci= .
	label var ylmotros_ci "Ingreso laboral monetario de otros trabajos" 

	******************
	***ylnmotros_ci***
	******************
	gen ylnmotros_ci=.
	label var ylnmotros_ci "Ingreso laboral NO monetario de otros trabajos" 

	************
	***ylm_ci***
	************
	* Tipo de cambio Noviembre 1998. Dólar (compra) = 6449
    * Fuente: Banco Central del Ecuador (BCE), 2013.
	
	egen ylm_ci1= rsum(ingpat ingasg ingepv ingdom) 
	gen ylm_ci= (ylm_ci1/6449)
	replace ylm_ci=. if ingpat==. & ingasg==. &	ingepv==. 
	label var ylm_ci "Ingreso laboral monetario total"  

	*************
	***ylnm_ci***
	*************
	gen ylnm_ci=.
	label var ylnm_ci "Ingreso laboral NO monetario total"  

	*************
	***ynlm_ci***
	*************
	egen ynlm_ci1=rsum(ingjub ingalq ingotr)
	gen ynlm_ci=(ynlm_ci1/6449)
	replace ynlm_ci = . if ingjub==. & ingalq==. & ingotr==. 
	label var ynlm_ci "Ingreso no laboral monetario"  

	**************
	***ynlnm_ci***
	**************
	gen ynlnm_ci=.
	label var ynlnm_ci "Ingreso no laboral no monetario" 

	****************
	***remesas_ci***
	****************
	gen remesas_ci=.
	label var remesas_ci "Remesas mensuales reportadas por el individuo" 



		************************
		***INGRESOS DEL HOGAR***
		************************

	*****************
	***nrylmpri_ch***
	*****************
	by idh_ch, sort: egen nrylmpri_ch=sum(nrylmpri_ci) if miembros_ci==1
	replace nrylmpri_ch=1 if nrylmpri_ch>0 & nrylmpri_ch<.
	replace nrylmpri_ch=. if nrylmpri_ch==.
	label var nrylmpri_ch "Hogares con algún miembro que no respondió por ingresos"

	************
	***ylm_ch***
	************
	by idh_ch, sort: egen ylm_ch=sum(ylm_ci) if miembros_ci==1
	label var ylm_ch "Ingreso laboral monetario del hogar"

	*************
	***ylnm_ch***
	*************
	by idh_ch, sort: egen ylnm_ch=sum(ylnm_ci) if miembros_ci==1
	label var ylnm_ch "Ingreso laboral no monetario del hogar"

	**************
	***ylmnr_ch***
	**************
	by idh_ch, sort: egen ylmnr_ch=sum(ylm_ci) if miembros_ci==1
	replace ylmnr_ch=. if nrylmpri_ch==1
	label var ylmnr_ch "Ingreso laboral monetario del hogar"

	*************
	***ynlm_ch***
	*************
	by idh_ch, sort: egen ynlm_ch=sum(ynlm_ci) if miembros_ci==1
	label var ynlm_ch "Ingreso no laboral monetario del hogar"

	**************
	***ynlnm_ch***
	**************
	gen ynlnm_ch=.
	label var ynlnm_ch "Ingreso no laboral no monetario del hogar"

	*****************
	***ylmhopri_ci***
	*****************
	gen ylmhopri_ci=ylmpri_ci/(4.3*horaspri_ci)
	replace ylmhopri_ci=. if ylmhopri_ci<=0
	label var ylmhopri_ci "Salario monetario de la actividad principal" 

	**************
	***ylmho_ci***
	**************
	gen ylmho_ci=ylm_ci/(horastot_ci*4.3)
	label var ylmho_ci "Salario monetario de todas las actividades" 

	*****************
	***rentaimp_ch***
	*****************
	gen rentaimp_ch=.
	label var rentaimp_ch "Rentas imputadas del hogar"

	*****************
	***autocons_ci***
	*****************
	gen autocons_ci=.
	label var autocons_ci "Autoconsumo reportado por el individuo"
	
	*****************
	***autocons_ch***
	*****************	
	gen autocons_ch=.
	label var autocons_ch "Autoconsumo reportado por el hogar"

	****************
	***remesas_ch***
	****************
	by idh_ch, sort: egen remesas_ch=sum(remesas_ci) if miembros_ci==1
	label var remesas_ch "Remesas mensuales del hogar"	



			****************************
			***VARIABLES DE EDUCACION***
			****************************
			
     /*
	 
	 nivinst:
           1 ninguno
           2 centro de alfabetización
           3 primaria
           4 secundaria
           5 superior
*/
	 	
	*************
	***aedu_ci***
	*************
	
	generat aedu_ci = .
	replace aedu_ci = 0          if nivinst==1
	replace aedu_ci = anoinst    if nivinst==2 
	replace aedu_ci = anoinst    if nivinst==3
	replace aedu_ci = anoinst+6  if nivinst==4
	replace aedu_ci = anoinst+12 if nivinst==5
	replace aedu_ci =. if (nivinst==. | anoinst==.)
	label var aedu_ci "Anios de educacion aprobados" 
	label value aedu_ci aedu_ci 

	**************
	***eduno_ci***
	**************
	gen eduno_ci=0
	replace eduno_ci=1 if aedu_ci==0
	label variable eduno_ci "Sin educacion"

	**************
	***edupi_ci***
	**************
	gen edupi_ci=0
	replace edupi_ci=1 if (aedu_ci>=1 & aedu_ci<6) 
	label variable edupi_ci "Primaria incompleta"

	**************
	***edupc_ci***
	**************
	gen edupc_ci=0
	replace edupc_ci=1 if aedu_ci==6 
	label variable edupc_ci "Primaria completa"

	**************
	***edusi_ci***
	**************
	gen edusi_ci=0
	replace edusi_ci=1 if (aedu_ci>=7 & aedu_ci<12) 
	label variable edusi_ci "Secundaria incompleta"

	**************
	***edusc_ci***
	**************
	gen edusc_ci=0
	replace edusc_ci=1 if aedu_ci==12 
	label variable edusc_ci "Secundaria completa"

	**************
	***eduui_ci***
	**************
	gen eduui_ci=(aedu_ci>12 & aedu_ci<16)
	label variable eduui_ci "Superior incompleto"

	***************
	***eduuc_ci***
	***************
	gen byte eduuc_ci= (aedu_ci>=16)
	label variable eduuc_ci "Superior completo"

	***************
	***edus1i_ci***
	***************
	gen edus1i_ci=0
	replace edus1i_ci=1 if (aedu_ci>6 & aedu_ci<9)
	label variable edus1i_ci "1er ciclo de la secundaria incompleto"

	***************
	***edus1c_ci***
	***************
	gen edus1c_ci=0
	replace edus1c_ci=1 if aedu_ci==9
	label variable edus1c_ci "1er ciclo de la secundaria completo"

	***************
	***edus2i_ci***
	***************
	gen edus2i_ci=0
	replace edus2i_ci=1 if aedu_ci>9 & aedu_ci<12
	label variable edus2i_ci "2do ciclo de la secundaria incompleto"

	***************
	***edus2c_ci***
	***************
	gen edus2c_ci=0
	replace edus2c_ci=1 if aedu_ci==12
	label variable edus2c_ci "2do ciclo de la secundaria completo"

	local var = "eduno edupi edupc edusi edusc edusc eduui eduuc edus1i edus1c edus2i edus2c"
	foreach x of local var {
	replace `x'_ci=. if aedu_ci==.
	}

	***************
	***edupre_ci***
	***************
	gen edupre_ci=.
	label variable edupre_ci "Educacion preescolar"

	**************
	***eduac_ci***
	**************
	gen eduac_ci=.
	label variable eduac_ci "Superior universitario vs superior no universitario"

	***************
	***asiste_ci***
	***************
	gen asiste_ci=(asistea==1)
	label variable asiste_ci "Asiste actualmente a la escuela"

	**************
	***pqnoasis_ci***
	**************
	*Esta variable no tiene opciones predeterminadas por los que se mantienen las opiciones del país
	gen pqnoasis_ci=.
	label var pqnoasis_ci "Razones para no asistir a la escuela"
	label def pqnoasis_ci 1"edad" 2"termino sus estudios" 3"falta recursos económicos" 4"fracaso escolar"
	label def pqnoasis_ci 5"por trabajo" 6"temor maestros" 7"enfermedad o discapacidad" 8"quehaceres del hogar", add
	label def pqnoasis_ci 9"familia no permite" 10"no hay establecimientos educativos" 11"no está interesado", add
	label def pqnoasis_ci 12"por embarazo" 13"por falta de cupo" 14"otra razón", add
	label val pqnoasis_ci pqnoasis_ci
	
	**************
    *pqnoasis1_ci*
    **************
    **Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**

    g       pqnoasis1_ci = .

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
	label var edupub_ci "Asiste a un centro de ensenanza público"



		**********************************
		**** VARIABLES DE LA VIVIENDA ****
		**********************************
	
	*El año 1998 la ENEMDU no levanta informacion de vivienda (no existe este modulo)
	

****************
***aguared_ch***
****************
gen aguared_ch =.
label var aguared_ch "Acceso a fuente de agua por red"

*****************
*aguafconsumo_ch*
*****************
gen aguafconsumo_ch = .

*****************
*aguafuente_ch*
*****************
gen aguafuente_ch =.


*************
*aguadist_ch*
*************

gen aguadist_ch=.


**************
*aguadisp1_ch*
**************
gen aguadisp1_ch = 9 


**************
*aguadisp2_ch*
**************
gen aguadisp2_ch = 9



*************
*aguamala_ch*  Altered
*************
gen aguamala_ch = .


*****************
*aguamejorada_ch*  Altered
*****************

gen aguamejorada_ch =.

*label var aguamejorada_ch "= 1 si la fuente de agua es mejorada"

*****************
***aguamide_ch***
*****************
gen aguamide_ch=.
label var aguamide_ch "Usan medidor para pagar consumo de agua"


*****************
*bano_ch         *  Altered
*****************
gen bano_ch=.

***************
***banoex_ch***

gen banoex_ch=9

label var banoex_ch "El servicio sanitario es exclusivo del hogar"

*****************
*banomejorado_ch*  Altered
*****************

gen banomejorado_ch=.



************
*sinbano_ch*
************

gen sinbano_ch = .
*label var sinbano_ch "= 0 si tiene baño en la vivienda o dentro del terreno"

*************
*aguatrat_ch*
*************
gen aguatrat_ch =9


	************
	***luz_ch***
	************
	gen luz_ch=.
	label var luz_ch "La principal fuente de iluminación es electricidad"	
	
		****************
	***luzmide_ch***
	****************
	gen luzmide_ch=.
	label var luzmide_ch "Usan medidor para pagar consumo de electricidad"

	****************
	***combust_ch***
	****************
	
	gen combust_ch=.
	label var combust_ch "Principal combustible gas o electricidad"	

	*************
	***des1_ch***
	*************
	
	gen des1_ch=.
	label var des1_ch "Tipo de desague según unimproved de MDG"
	label def des1_ch 0"No tiene servicio sanitario" 1"Conectado a red general o cámara séptica"
	label def des1_ch 2"Letrina o conectado a pozo ciego" 3"Desemboca en río o calle", add
	label val des1_ch des1_ch
		
	*************
	***des2_ch***
	*************
	
	gen des2_ch=.
	label var des2_ch "Tipo de desague sin incluir definición MDG"
	label def des2_ch 0"No tiene servicio sanitario" 1"Conectado a red general, cámara séptica, pozo o letrina"
	label def des2_ch 2"Cualquier otro caso", add
	label val des2_ch des2_ch
	
	*************
	***piso_ch***
	*************
	gen piso_ch=.
	label var piso_ch "Materiales de construcción del piso"	
	
	**************
	***pared_ch***
	**************
	gen pared_ch=.
	label var pared_ch "Materiales de construcción de las paredes"	
	
	**************
	***techo_ch***
	**************
	
	gen techo_ch=.
	label var techo_ch "Materiales de construcción del techo"	
	
	**************
	***resid_ch***
	**************
	
	gen resid_ch =.
	label var resid_ch "Método de eliminación de residuos"
	label def resid_ch 0"Recolección pública o privada" 1"Quemados o enterrados"
	label def resid_ch 2"Tirados a un espacio abierto" 3"Otros", add
	label val resid_ch resid_ch
	

	*************
	***dorm_ch***
	*************
	
	gen dorm_ch=.
	label var dorm_ch "Habitaciones para dormir"	
	
	
	****************
	***cuartos_ch***
	****************
	
	gen cuartos_ch=.
	label var cuartos_ch "Habitaciones en el hogar"	
	
	***************
	***cocina_ch***
	***************
	gen cocina_ch=.
	label var cocina_ch "Cuarto separado y exclusivo para cocinar"

	**************
	***telef_ch***
	**************
	gen telef_ch=.
	label var telef_ch "El hogar tiene servicio telefónico fijo"
	
	
	***************
	***refrig_ch***
	***************
	gen refrig_ch=. 
	label var refrig_ch "El hogar posee refrigerador o heladera"
	
		
	**************
	***freez_ch***
	**************
	gen freez_ch=.
	label var freez_ch "El hogar posee congelador"

	*************
	***auto_ch***
	*************
		
	gen auto_ch=.
	label var auto_ch "El hogar posee automovil particular"
		
	**************
	***compu_ch***
	**************
	gen compu_ch=.
	label var compu_ch "El hogar posee computador"
	
	*****************
	***internet_ch***
	*****************
	gen internet_ch=.
	label var internet_ch "El hogar posee conexión a Internet"
	

	************
	***cel_ch***
	************
	
	gen cel_ch=.
	label var cel_ch "El hogar tiene servicio telefonico celular"
	
	**************
	***vivi1_ch***
	**************
		
	gen vivi1_ch=.
	label var vivi1_ch "Tipo de vivienda en la que reside el hogar"
	label def vivi1_ch 1"Casa" 2"Departamento" 3"Otros"
	label val vivi1_ch vivi1_ch
	
	**************
	***vivi2_ch***
	**************
	
	gen vivi2_ch=.
	label var vivi2_ch "La vivienda es casa o departamento"
	
	
	*****************
	***viviprop_ch***
	*****************
	gen viviprop_ch=.
	label var viviprop_ch "Propiedad de la vivienda"
	label def viviprop_ch 0"Alquilada" 1"Propia y totalmente pagada" 2"Propia y en proceso de pago"
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
	gen vivialq_ch=.
	label var vivialq_ch "Alquiler mensual"

	*******************
	***vivialqimp_ch***
	*******************
	gen vivialqimp_ch=.
	label var vivialqimp_ch "Alquiler mensual imputado"

/*_____________________________________________________________________________________________________*/
* Asignación de etiquetas e inserción de variables externas: tipo de cambio, Indice de Precios al 
* Consumidor (2011=100), Paridad de Poder Adquisitivo (PPA 2011),  líneas de pobreza
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

/*Homologar nombre del identificador de ocupaciones (isco, ciuo, etc.) y de industrias y dejarlo en base armonizada 
para análisis de trends (en el marco de estudios sobre el futuro del trabajo)*/
rename p41 codocupa
rename rama codindustria

compress

saveold "`base_out'", version(12) replace


log close




