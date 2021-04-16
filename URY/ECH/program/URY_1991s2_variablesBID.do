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

local PAIS URY
local ENCUESTA ECH
local ANO "1991"
local ronda s2 


local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
*local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_orig\\`PAIS'_`ANO'`ronda'.dta"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
                                                    
capture log close
log using "`log_file'", replace 


/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Uruguay
Encuesta: ECH
Round: s2
Autores: Mayra Sáenz
Generación de Nuevas Variables de Mercado laboral: Mayra Sáenz
Última versión: Mayra Sáenz E-mail: mayras@iadb.org / saenzmayra.a@gmail.com
Fecha última modificación: 30 de Octubre de 2013

							SCL/LMK - IADB
****************************************************************************/
/***************************************************************************
Detalle de procesamientos o modificaciones anteriores:

****************************************************************************/



use `base_in', clear

/********************************/
/*    VARIABLES DEL HOGAR	*/
/********************************/


gen idh_ch=id_hogar
gen idp_ci=id_pers

* En el diccionario se señala que los factores de expansión son: pesotri, pesosem, pesoan. Sin embargo, en la base
* no consta ninguno de ellos.
gen factor_ch=1
gen zona_c=1 /*La encuesta es solo urbana!*/
gen str3 pais_c="URY"
gen anio_c=1991
gen mes_c=.
replace mes_c=1 if ha2 >=1 & ha2 <=4
replace mes_c=2 if ha2 >=5 & ha2 <=8
replace mes_c=3 if ha2 >=9 & ha2 <=12
replace mes_c=4 if ha2 >=13 & ha2 <=16
replace mes_c=5 if ha2 >=17 & ha2 <=20
replace mes_c=6 if ha2 >=21 & ha2 <=24
replace mes_c=7 if ha2 >=25 & ha2 <=28
replace mes_c=8 if ha2 >=29 & ha2 <=32
replace mes_c=9 if ha2 >=33 & ha2 <=36
replace mes_c=10 if ha2 >=37 & ha2 <=40
replace mes_c=11 if ha2 >=41 & ha2 <=44
replace mes_c=12 if ha2 >=45 & ha2 <=48

/*
PE4		34	1	PARENTESCO                    
                                       1  JEFE                
                                       2  CONYUGE             
                                       3  HIJO/A              
                                       4  PADRES/SUEGROS      
                                       5  OTRO FAMILIAR       
                                       6  OTRO NO PARIENTE    
                                       7  SERV. DOMESTICO     

									   */
gen relacion_ci=.
replace relacion_ci=1 if pe4 ==	1
replace relacion_ci=2 if pe4==2
replace relacion_ci=3 if pe4==3
replace relacion_ci=4 if pe4==4 | pe4==5
replace relacion_ci=5 if pe4==6
replace relacion_ci=6 if pe4==7
label define relacion_ci 1 "Jefe" 2 "Conyuge" 3 "Hijo" 4 "Otros Parientes" 5 "Otros no Parientes" 6 "Servicio Domestico"
label values relacion_ci relacion_ci


****************
* region_BID_c *
****************
gen region_BID_c=.
replace region_BID_c=4 
label var region_BID_c "Regiones BID"
label define region_BID_c 1 "Centroamérica_(CID)" 2 "Caribe_(CCB)" 3 "Andinos_(CAN)" 4 "Cono_Sur_(CSC)"
label value region_BID_c region_BID_c

************
* Region_c *
************
destring ha3, replace
gen region_c=ha3
label define region_c  1 "Montevideo" ///
           2 "Artigas" /// 
           3 "Canelones" /// 
           4 "Cerro Largo" /// 
           5 "Colonia" /// 
           6 "Durazno" /// 
           7 "Flores" /// 
           8 "Florida" /// 
           9 "Lavalleja" /// 
          10 "Maldonado" /// 
          11 "Paysandú" /// 
          12 "Río Negro" /// 
          13 "Rivera" /// 
          14 "Rocha" /// 
          15 "Salto" /// 
          16 "San José" /// 
          17 "Soriano" /// 
          18 "Tacuarembó" ///
          19 "Treinta y Tres" 
label value region_c region_c
label var region_c "División política"

/************************************************************************/
/*			VARIABLES DE INFRAESTRUCTURA DEL HOGAR		*/
/************************************************************************/	
gen aguared_ch=(hd5==1)

gen aguadist_ch=.
replace aguadist_ch=1 if hd6==1
replace aguadist_ch=2 if hd6==2 
replace aguadist_ch=3 if hd6==3


gen aguamala_ch=(hd5==4) /*Cachimba=ojo de agua*/	
gen aguamide_ch=.
gen luz_ch=(hd101==1 | hd101==2)
gen luzmide_ch=.
gen combust_ch=(hd102==1 | hd102==2 | hd102==3)
gen bano_ch=(hd7!=3)

gen banoex_ch=(hd8==1)
/*
HD9		53	1	EVACUACION SERV.SANITARIO     
                                       1  RED GRAL            
                                       2  FOSA SEPT/POZO      
                                       3  OTRO                
*/
gen des1_ch=.
replace des1_ch=0 if hd7==3
replace des1_ch=1 if hd9==1  
replace des1_ch=2 if hd9==2
replace des1_ch=3 if hd9==3

/*
des1_ch Tipo de desagüe incluyendo la definición de "Unimproved" del MDG
0 No corresponde: El hogar no tiene servicio higiénico.
1 El desagüe está conectado a la red general o a una cámara séptica
2 El desagüe está conectado a un pozo ciego o es una letrina.
3 El desagüe se comunica con la superficie: desemboca en un río o en la calle.*/
label var des1_ch "Tipo de desague según unimproved de MDG"
label def des1_ch 0"No tiene servicio sanitario" 1"Conectado a red general o cámara séptica"
label def des1_ch 2"Letrina o conectado a pozo ciego" 3"Desemboca en río o calle", add
label val des1_ch des1_ch

recode des1_ch (1 2=1) (3=2), g (des2_ch)

/*
0 No corresponde: El hogar no tiene servicio higiénico.
1 El desagüe está conectado a la red general, a una cámara o fosa séptica, o a un pozo ciego o letrina.
2 Cualquier otro*/
label var des2_ch "Tipo de desague sin incluir definición MDG"
label def des2_ch 0"No tiene servicio sanitario" 1"Conectado a red general, cámara séptica, pozo o letrina"
label def des2_ch 2"Cualquier otro caso", add
label val des2_ch des2_ch

gen piso_ch=.
gen pared_ch=.
gen techo_ch=.
gen resid_ch=.

**Daniela Zuluaga- Enero 2018: Se agregan las variables aguamejorada_ch y banomejorado_ch cuya sintaxis fue elaborada por Mayra Saenz**
	
*********************
***aguamejorada_ch***
*********************
g       aguamejorada_ch = 1 if hd5 ==1 | hd5 ==3
replace aguamejorada_ch = 0 if hd5 ==2 | (hd5 >=4 & hd5 <=6)
		
		
*********************
***banomejorado_ch***
*********************
g       banomejorado_ch = 1 if (hd7 ==1 | hd7 ==2) & hd8 ==1 & (hd9 ==1 | hd9 ==2)
replace banomejorado_ch = 0 if ((hd7 ==1 | hd7 ==2) & hd8 ==2) | hd7 ==3 | ((hd7 ==1 | hd7 ==2) & hd8 ==1 & (hd9 ==3))


gen dorm_ch=hd42
replace dorm_ch=. if hd42==9
gen cuartos_ch=hd41
replace cuartos_ch=. if hd41==99
gen cocina_ch=.
gen refrig_ch=(hd113==1)
gen freez_ch=(hd112==1)
gen auto_ch=(hd1110==1)
gen telef_ch=.
gen compu_ch=.
gen internet_ch=.
gen cel_ch=.
*No se puede identificar si es casa o departamento, puesto que ambos están en la misma categoría.
gen vivi1_ch=.

gen vivi2_ch=(hc1==1)
/*
HD3		44	1	TENENCIA                      
                                       1  PROPIETARIO/PAGO    
                                       2  PROPIETARIO/PAGANDO 
                                       3  ARRENDATARIO        
                                       4  OCUPA C/PERMISO     
                                       5  OCUPA S/PERMISO     
*/

gen viviprop_ch=.
replace viviprop_ch=0 if hd3==3
replace viviprop_ch=1 if hd3==1
replace viviprop_ch=2 if hd3==2
replace viviprop_ch=3 if hd3==4 | hd3==5
gen vivialq_ch=ht14 if viviprop_ch==0
gen vivialqimp_ch=pg14
gen vivitit_ch=.
label var vivitit_ch "El hogar posee un título de propiedad"



/************************************************************************/
/*				VARIABLES DEMOGRAFICAS			*/
/************************************************************************/
gen factor_ci=1 /* pesotri, pesosem, pesoan no constan en la base */
gen sexo_ci=pe2
label define sexo_ci 1 "Hombre" 2 "Mujer"
label value sexo_ci sexo_ci

**********
***edad***
**********

gen edad_ci=pe3
replace edad_ci=. if pe3==99
label variable edad_ci "Edad del individuo"

/*
PE5		35	1	ESTADO CIVIL                  
                                       1  UNION LIBRE         
                                       2  CASADO              
                                       3  DIVORCIADO/SEPARADO 
                                       4  VIUDO               
                                       5  SOLTERO             
*/
destring pe5, replace
gen civil_ci=1 if pe5==5
replace civil_ci=2 if pe5==1 | pe5==2
replace civil_ci=3 if pe5==3
replace civil_ci=4 if pe5==4

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

**********
***raza***
**********
gen raza_ci= .
label define raza_ci 1 "Indígena" 2 "Afro-descendiente" 3 "Otros"
label value raza_ci raza_ci 
label value raza_ci raza_ci
label var raza_ci "Raza o etnia del individuo" 

gen raza_idioma_ci = .
gen id_ind_ci      = .
gen id_afro_ci     = .

************************************
*** VARIABLES DEL MERCADO LABORAL***
************************************
/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/


****************
****condocup_ci*
****************
gen condocup_ci=.
replace condocup_ci=1 if pobpcoac>=11 & pobpcoac<=12
replace condocup_ci=2 if pobpcoac>=21 & pobpcoac<=23
replace condocup_ci=3 if pobpcoac>=30 & pobpcoac<=38
replace condocup_ci=4 if edad_ci<14
label define condocup_ci 1"ocupados" 2"desocupados" 3"inactivos" 4"menor que 14"
label value condocup_ci condocup_ci
label var condocup_ci "Condicion de ocupacion utilizando definicion del pais"

****************
*afiliado_ci****
****************
gen disse       =(pe6==4) 
gen bps         =(pe6==7) 
gen iamc        =(pe6==5) 

gen afiliado_ci=(disse==1 | bps==1 | iamc==1)
replace afiliado_ci=. if disse==. & bps==. & iamc==.
label var afiliado_ci "Afiliado a la Seguridad Social"
drop disse bps iamc
*Nota: seguridad social comprende solo los que en el futuro me ofrecen una pension.

****************
*tipopen_ci*****
****************
gen tipopen_ci=.
label define  t 1 "Jubilacion" 2 "Viudez/orfandad" 3 "Benemerito" 4 "Invalidez" 12 "Jub y viudez" 13 "Jub y benem" 23 "Viudez y benem" 123 "Todas"
label value tipopen_ci t
label var tipopen_ci "Tipo de pension - variable original de cada pais" 

****************
*cotizando_ci***
****************
gen cotizando_ci=.
label var cotizando_ci "Cotizante a la Seguridad Social"
*Nota: solo seguro social publico, con el cual tenga derecho a pensiones en el futuro.

****************
*instpen_ci*****
****************
gen instpen_ci= .
label var instpen_ci "Institucion a la cual esta afiliado variable original de cada pais" 

********************
*** instcot_ci *****
********************
gen instcot_ci=.
label define  instcot_ci 1"bps" 2"bps y afap" 3"policial" 4"militar" 5"profesional" 6 "notarial" 7"bancaria"
label var instcot_ci "institución a la cual cotiza por su trabajo"

*****************
*tipocontrato_ci*
*****************
gen tipocontrato_ci=. /*Se pregunta solamente a empleados del sector publico que es el 22% de los empleados (no es comparable con el resto de paises)*/
label var tipocontrato_ci "Tipo de contrato segun su duracion en act principal"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci

*************
*ypen_ci*
*************
egen ypen_ci= rsum(pg911 pg912), missing
label var ypen_ci "Valor de la pension contributiva"

*************
**pension_ci*
*************
gen pension_ci=(ypen_ci>0 & ypen_ci!=.)
label var pension_ci "1=Recibe pension contributiva"

*****************
**  ypensub_ci  *
*****************
gen ypensub_ci=.
label var ypensub_ci "Valor de la pension subsidiada / no contributiva"

***************
*pensionsub_ci*
***************
gen pensionsub_ci=.
label var pensionsub_ci "1=recibe pension subsidiada / no contributiva"

*************
* cesante_ci* 
*************
generat cesante_ci=0 if condocup_ci==2
replace cesante_ci=1 if pf32==1 & condocup_ci==2
label var cesante_ci "Desocupado - definicion oficial del pais"

*********
*lp_ci***
*********
gen lp_ci =.
label var lp_ci "Linea de pobreza oficial del pais"

*********
*lpe_ci***
*********
gen lpe_ci =.
label var lpe_ci "Linea de indigencia oficial del pais"


/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/

*************
**salmm_ci***
*************
* 2015 MGD: salario minimo segun mes
gen salmm_ci = 118.3 if mes_c>=1 & mes_c<=4
replace salmm_ci=155 if mes_c>=5 & mes_c<=8
replace salmm_ci=200 if mes_c>=9 
label var	salmm_ci	"Salario minimo legal 1991"

************
***emp_ci***
************

gen byte emp_ci=(condocup_ci==1)

****************
***desemp_ci***
****************

gen desemp_ci=(condocup_ci==2)

*************
***pea_ci***
*************
gen pea_ci=0
replace pea_ci=1 if emp_ci==1 |desemp_ci==1



gen ocupa_ci=.
replace ocupa_ci=1 if pf39>=0 & pf39<=98 & emp_ci==1
replace ocupa_ci=2 if pf39>=100 & pf39<=186 & emp_ci==1
replace ocupa_ci=3 if pf39>=200 & pf39<=290 & emp_ci==1
replace ocupa_ci=4 if pf39>=300 & pf39<=361 & emp_ci==1
replace ocupa_ci=5 if pf39>=900 & pf39<=999 & emp_ci==1
replace ocupa_ci=6 if pf39>=400 & pf39<=453 & emp_ci==1
replace ocupa_ci=7 if pf39>=500 & pf39<=792 & emp_ci==1 /*Incluye artesanos y operarios en hilanderias*/
replace ocupa_ci=9 if pf39>=800 & pf39<=890 & emp_ci==1
/*No hay una categoria para las fuerzas armadas*/
gen ramaaux=substr(string(pf40),1,1)
gen rama_ci=real(ramaaux)
drop ramaaux
replace rama_ci=. if rama_ci==0

gen horaspri_ci=pf051
replace horaspri_ci=. if pf051==99
replace horaspri_ci=. if emp_ci==0

gen horastot_ci=pf053
replace horastot_ci=. if pf053==99
replace horastot_ci=. if horaspri_ci==.


gen durades_ci=pf26/4.3 if pf26>0
replace durades_ci=. if pf26==99

*Corresponde a la variabel pf37 y pf38 pero no constan en la base
* pf37 antiguedad en anos
* pf38 antiguedad en meses
gen pf38m = pf38/12
replace pf38m=. if pf38==99 

egen antiguedad_ci=rsum(pf37 pf38m), missing

gen desalent_ci=(pobpcoac==37)


* Modificacion MGD 06/23/2014: horas de la actividad principal.
gen subemp_ci=0
replace subemp_ci=1 if (horaspri_ci>=1 & horaspri_ci<=30 & pf18==1)
* Mod. 2015/11 MLO
*gen tiempoparc_ci=(horastot_ci>=1 & horastot_ci<=30 & pf18==2)
gen tiempoparc_ci=(horaspri_ci>=1 & horaspri_ci<30 & pf18==2)
replace tiempoparc_ci=. if emp_ci==0


/*
PF41		169	1	CATEGORIA DE OCUPACION        
                                       1  EMPLEADO PRIVADO    
                                       2  EMPLEADO PUBLICO    
                                       3  COOP. PRODUCCION    
                                       4  PATRON              
                                       5  CUENTA PROP. S/LOCAL
                                       6  CUENTA PROP. C/LOCAL
                                       7  FAMIL. NO REMUNERADO
                                       8  OTRAS ACTIVIDADES   
*/


gen categopri_ci=.
replace categopri_ci=1 if pf41==4
replace categopri_ci=2 if pf41==5 | pf41==6 | pf41==3
replace categopri_ci=3 if pf41==1 | pf41==2 
replace categopri_ci=4 if pf41==7 | pf41==8
replace categopri_ci=. if emp_ci!=1

gen categosec_ci=.
gen contrato_ci=.
gen segsoc_ci=.
gen nempleos_ci= .
replace nempleos_ci=1 if pf07==1
replace nempleos_ci=2 if pf07>1 & pf07<=7

/*
gen firmapeq_ci=.
replace firmapeq_ci=1 if emp_ci==1 &  pf081==1 &  (pf082>=1 &  pf082<=5)
replace firmapeq_ci=0 if emp_ci==1 & (pf081==1 & pf082>5)| pf081==2
replace firmapeq_ci=. if emp_ci==0 
label var firmapeq_ci "Trabajadores informales"
label def firmapeq_ci 1"5 o menos trabajadores" 0"Mas de 5 trabajadores"
label val firmapeq_ci firmapeq_ci
		*/


gen spublico_ci=.
replace spublico_ci=1 if pf41==2
replace spublico_ci=. if emp_ci==0 
la var spublico_ci "Trabaja en sector publico"


*************
*tamemp_ci
*************
* En el documento base no consta la clasificación de tamaño de empresa para antes de 1995
* Se utiliza la clasificación de 1995.
*Uruguay Pequeña 1 a 4 Mediana 5 a 9 Grande más de 9

gen tamemp_ci=.
replace tamemp_ci=1 if pf082>0 & pf082<=4
replace tamemp_ci=2 if pf082>=5 & pf082<=9
replace tamemp_ci=3 if pf081==2 & tamemp_ci!=1 & tamemp_ci!=2
label var tamemp_ci "# empleados en la empresa segun rangos"
label define tamemp_ci 1 "Pequena" 2 "Mediana" 3 "Grande"
label value tamemp_ci tamemp_ci


gen categoinac_ci =1 if ((pf1311 ==1 | pf1312==1) & condocup_ci==3)
replace categoinac_ci = 2 if  (pf133==1 & condocup_ci==3)
replace categoinac_ci = 3 if  (pf134==1 & condocup_ci==3)
replace categoinac_ci = 4 if  ((categoinac_ci ~=1 & categoinac_ci ~=2 & categoinac_ci ~=3) & condocup_ci==3)
label var categoinac_ci "Categoría de inactividad"
label define categoinac_ci 1 "Jubilados o pensionados" 2 "Estudiantes" 3 "Quehaceres domésticos" 4 "Otros" 
label value categoinac_ci categoinac_ci

*******************
***formal***
*******************
/*gen formal=1 if cotizando_ci==1
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
label var formal_ci "1=afiliado o cotizante / PEA"*/

*Modificación Mayra Sáenz - Septiembre 2014
*También se incluye como formales a los empleados públicos o si son beneficiarios de disse, 
*pues en este año no se dispone de la variable cotizando
gen formal=1 if cotizando_ci==1
replace formal=1 if afiliado_ci==1 & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1
replace formal=1 if (pf41==2 | pe6==4) & (cotizando_ci!=1 | cotizando_ci!=0) & condocup_ci==1 

gen byte formal_ci=.
replace formal_ci=1 if formal==1 & (condocup_ci==1 | condocup_ci==2)
replace formal_ci=0 if formal_ci==. & (condocup_ci==1 | condocup_ci==2) 
label var formal_ci "1=afiliado o cotizante / PEA"


*************
***INGRESOS
*************
*************
* ylmpri_ci *
*************
/*
pg11p		170	5	sueldo privado ppal.          
pg12p		175	5	comisiones privado ppal.      
pg13p		180	5	beneficios privado ppal.      
pg14p		185	5	aguinaldo privado ppal.       
pg15p		190	5	salario vac. privado ppal.    
pg16p		195	5	propinas privado ppal.              
pg21p		205	5	sueldo publico ppal.          
pg22p		210	5	comisiones publ.ppal.         
pg23p		215	5	beneficios publ. ppal.        
pg24p		220	5	aguinaldo publico ppal        
pg25p		225	5	salario vac. publico ppal     
*pg26p		230	5	propinas publico ppal                   
pg31p		240	5	cuenta prop. dinero s/loc ppal
*pg32p		245	5	cuenta prop. asig. s/loc ppal.         
pg41p		255	5	cta.propia dinero c/loc ppal  
*pg42p		260	5	cta.propia asig. c/loc ppal.          
pg51p		270	5	patron mes dinero ppal.       
pg61p		280	5	dinero patron anio ppal (12 meses)      
pg71p		290	5	coop. dinero mes ppal         
*pg72p		295	5	coop. asig. mes ppal 
*pg81p		305	5	coop. dinero anio ppal (12 meses)                 

*/

gen pg61pm = pg61p/12
gen pg81pm = pg81p/12

egen ylmpri_ci=rsum(pg11p pg12p pg13p pg14p pg15p pg16p pg21p pg22p pg23p pg24p pg25p pg26p pg31p pg32p pg41p pg42p pg51p pg61pm pg71p pg72p pg81pm) if emp_ci==1, missing

* Nota Marcela G. Rubio - Abril 2014
* Se filtra ingreso sólo para las personas ocupadas emp_ci==1

*****************
***nrylmpri_ci***
*****************
gen nrylmpri_ci=(ylmpri_ci==. & emp_ci==1)
label var nrylmpri_ci "Id no respuesta ingreso de la actividad principal"  


	****************
	***ylnmpri_ci***
	****************
	/*pg17p		200	5	especie privado ppal.         
pg27p		235	5	especie publico ppal          
pg33p		250	5	cta.propia especie s/loc ppal  (autoconsumo)
pg43p		265	5	cta.propia especie c/loc ppal  (autoconsumo)
*pg52p		275	5	patron especie mes ppal.  
*pg62p		285	5	especie patron anio ppal (últimos 12 meses)     
pg73p		300	5	coop. especie mes ppal (autoconsumo)       
*pg82p		310	5	coop. especie anio ppal  (últimos 12 meses) 
*/
gen pg62pm = pg62p/12
gen pg82pm = pg82p/12	
	egen ylnmpri_ci= rsum(pg17p pg27p pg33p pg43p pg52p pg62pm pg73p pg82pm) if emp_ci==1, missing
	label var ylnmpri_ci "Ingreso laboral NO monetario actividad principal"   

* Nota Marcela G. Rubio - Abril 2014
* Se filtra ingreso sólo para las personas ocupadas emp_ci==1

	***************
	***ylmsec_ci***
	***************
	/*
pg11o		315	5	sueldo privado otro          
pg12o		320	5	comisiones privado otro      
pg13o		325	5	beneficios privado otro      
pg14o		330	5	aguinaldo privado otro       
pg15o		335	5	salario vac. privado otro    
*pg16o		340	5	propinas privado otro           
*pg21o		250	5	sueldo publico otro        
*pg22o		355	5	comisiones publ.otro                 
*pg23o		360	5	beneficios publ. otro               
*pg24o		365	5	aguinaldo ublicoo otro           
*pg25o		370	5	salario vac. publico otro           
*pg26o		375	5	propinas publico otro             
pg31o		385	5	cuenta prop. dinero s/loc otro
*pg32o		390	5	cuenta prop. asig. s/loc otro       
pg41o		400	5	cta.propia dinero c/loc otro  
*pg42o		405	5	cta.propia asig. c/loc otro         
pg51o		415	5	patron dinero mes otro        
pg61o		425	5	dinero patron anio otro (últimos 12 meses)       
pg71o		435	5	coop. dinero mes otro         
*pg72o		440	5	coop. asig. mes otro          
*pg81o		450	5	coop. dinero anio otro (últimos 12 meses) 
*/

gen pg61om = pg61o/12
gen pg81om = pg81o/12
	
	
	egen ylmsec_ci=rsum(pg11o pg12o pg13o pg14o pg15o pg16o pg21o pg22o pg23o pg24o pg25o pg26o pg31o pg32o pg41o pg42o pg51o pg61om pg71o pg72o pg81om) if emp_ci==1, missing
	label var ylmsec_ci "Ingreso laboral monetario segunda actividad" 

	* Nota Marcela G. Rubio - Abril 2014
	* Se filtra ingreso sólo para las personas ocupadas emp_ci==1
	
	
	****************
	***ylnmsec_ci***
	****************
	/*
	pg17o		345	5	especie privado otro
*pg27o		380	5	especie publico otro       
pg33o		395	5	cta.propia especie s/loc otro (autoconsumo)
pg43o		410	5	cta.propia especie s/loc otro (autoconsumo)
*pg52o		420	5	patron especie mes otro   
*pg62o		430	5	especie patron anio otro (últimos 12 meses)                 
pg73o		445	5	coop. especie mes otro (autoconsumo)       
*pg82o		455	5	coop. especie anio otro   (últimos 12 meses)        
*/

gen pg62om = pg62o/12
gen pg82om = pg82o/12
	
	egen ylnmsec_ci=rsum(pg17o pg27o pg33o pg43o pg52o pg62o pg73o pg82o) if emp_ci==1, missing
	label var ylnmsec_ci "Ingreso laboral NO monetario actividad secundaria"

	* Nota Marcela G. Rubio - Abril 2014
	* Se filtra ingreso sólo para las personas ocupadas emp_ci==1
	
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

	egen ylmotros_ci= rsum(pg11p pg12p pg13p pg14p pg15p pg16p pg21p pg22p pg23p pg24p pg25p pg26p pg31p pg32p pg41p pg42p pg51p pg61pm pg71p pg72p pg81pm pg11o pg12o pg13o pg14o pg15o pg16o pg21o pg22o pg23o pg24o pg25o pg26o pg31o pg32o pg41o pg42o pg51o pg61om pg71o pg72o pg81om) if emp_ci==0, missing
	label var ylmotros_ci "Ingreso laboral monetario de otros trabajos" 

	* Nota Marcela G. Rubio - Abril 2014
	* estimo variable ingreso laboral monetario otros trabajos para todos los años
		
	******************
	***ylnmotros_ci***
	******************
	egen ylnmotros_ci=  rsum(pg17p pg27p pg33p pg43p pg52p pg62pm pg73p pg82pm pg17o pg27o pg33o pg43o pg52o pg62o pg73o pg82o) if emp_ci==0, missing
	label var ylnmotros_ci "Ingreso laboral NO monetario de otros trabajos" 

	* Nota Marcela G. Rubio - Abril 2014
	* estimo variable ingreso laboral no monetario otros trabajos para todos los años
	
	************
	***ylm_ci***
	************
	egen ylm_ci= rsum(ylmpri_ci ylmsec_ci ylmotros_ci) 
	replace ylm_ci=. if ylmpri_ci==. &  ylmsec_ci==. & ylmotros_ci==.
	label var ylm_ci "Ingreso laboral monetario total"  

	* Nota Marcela G. Rubio - Abril 2014
	* Incluyo ingreso laboral monetario otros como parte del ingreso laboral monetario total ya que no había sido incluido
	
	*************
	***ylnm_ci***
	*************
	egen ylnm_ci=rsum(ylnmpri_ci ylnmsec_ci ylnmotros_ci)
	replace ylnm_ci=. if ylnmpri_ci==. &  ylnmsec_ci==. & ylnmotros_ci==.
	label var ylnm_ci "Ingreso laboral NO monetario total"  
	
	* Nota Marcela G. Rubio - Abril 2014
	* Incluyo ingreso laboral no monetario otros como parte del ingreso laboral no monetario total ya que no había sido incluido
	
	*************
	***ynlm_ci***
	*************
	/*
*pg911		460	5	jubilacion del pais           
*pg912		465	5	pension del pais              
*pg921		470	5	jubilacion del exterior       
*pg922		475	5	pension del exterior          
pg101		480	5	subsidios del pais            
pg102		485	5	subsidios del exterior        
*pg111		490	5	contribuciones pais           
*pg112		495	5	contribuciones exterior       
pg121		500	5	alquileres del pais           
pg122		505	5	alquileres del exterior  
pg131		510	5	intereses pais (últimos 12 meses)               
pg132		515	5	intereses exterior  (últimos 12 meses)               
*/

gen pg131m = pg131/12 
gen pg132m = pg132/12

egen ynlm_ci=rsum(pg911 pg912 pg921 pg922 pg101 pg102 pg111 pg112 pg121 pg122 pg131 pg132), missing
label var ynlm_ci "Ingreso no laboral monetario"  


	**************
	***ynlnm_ci***
	**************
	gen ynlnm_ci=.
	label var ynlnm_ci "Ingreso no laboral no monetario" 

	****************
	***remesas_ci***
	****************
	gen remesas_ci=pg112
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
	gen rentaimp_ch= vivialqimp_ch
	label var rentaimp_ch "Rentas imputadas del hogar"

	*****************
	***autocons_ci***
	*****************
* Act. principal pg33p pg43p pg73p
* Act. secundaria pg33o pg43o pg73o
	
	egen autocons_ci= rsum(pg33p pg43p pg73p pg33o pg43o pg73o), missing
	label var autocons_ci "Autoconsumo reportado por el individuo"
	
	*****************
	***autocons_ch***
	*****************	
bys idh_ch: egen autocons_ch=sum(autocons_ci) if miembros_ci==1
la var autocons_ch "Autoconsumo del Hogar"

	****************
	***remesas_ch***
	****************
	by idh_ch, sort: egen remesas_ch=sum(remesas_ci) if miembros_ci==1
	label var remesas_ch "Remesas mensuales del hogar"	


/******************************************************************************************/
/*						VARIABLES EDUCATIVAS			  */
/******************************************************************************************/


gen aedu_ci=.
replace aedu_ci=0 if pe141==8 | pe141==0 
replace aedu_ci=pe142 if pe141==1
replace aedu_ci=0 if pe141==1 & pe142==0
replace aedu_ci=6 if pe141==1 & pe142>6
replace aedu_ci=6+pe142 if pe141==2 
replace aedu_ci=6 if (pe141==2 & pe142==0) 
replace aedu_ci=10+pe142 if pe141==3
replace aedu_ci=10 if (pe141==3 & pe142==0)
replace aedu_ci=12 if (pe141==3 & pe142>2)
replace aedu_ci=9+pe142 if pe141==4
replace aedu_ci=9 if pe141==4 & pe142==0
replace aedu_ci=12+pe142 if pe141==5 | pe141==6 | pe141==7
replace aedu_ci=12 if (pe141==5 & pe142==0) | (pe141==6 & pe142==0) | (pe141==7 & pe142==0)
replace aedu_ci=. if pe141==9 | pe141==.


/*U.T.U (pe141=4)>><< Enseñanza técnica. Aunque se llama Universidad Tecnica de Uruguay no esta contado como una carrera universitaria, sino que, dado
que solo pide como requisito el primer ciclo, es como una enseñanza secundaria de segundo ciclo que puede durar muchos años.*/

**************
***eduno_ci***
**************

gen byte eduno_ci=(aedu_ci==0) 
replace eduno_ci=. if aedu_ci==.
label variable eduno_ci "Cero anios de educacion"

**************
***edupi_ci***
**************

gen byte edupi_ci=(aedu_ci>=1 & aedu_ci<6)
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

gen byte edusi_ci=(aedu_ci>6 & aedu_ci<12)
replace edusi_ci=. if aedu_ci==.
label variable edusi_ci "Secundaria incompleta"

**************
***edusc_ci***
**************


gen byte edusc_ci=(aedu_ci==12)
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

gen byte edus2i_ci=(aedu_ci>9 & aedu_ci<12)
replace edus2i_ci=. if aedu_ci==.
label variable edus2i_ci "2do ciclo de la secundaria incompleto"

***************
***edus2c_ci***
***************

gen byte edus2c_ci=(aedu_ci==12)
replace edus2c_ci=. if aedu_ci==.
label variable edus2c_ci "2do ciclo de la secundaria completo"

**************
***eduui_ci***
**************

gen byte eduui_ci=(aedu_ci>12 & aedu_ci<16)
replace eduui_ci=. if aedu_ci==.
label variable eduui_ci "Universitaria incompleta"

***************
***eduuc_ci***
***************

gen byte eduuc_ci=(aedu_ci>=16) 
replace eduuc_ci=. if aedu_ci==.
label variable eduuc_ci "Universitaria completa o mas"



gen edupre_ci=(pe141==8)
replace edupre_ci=. if aedu_ci==.
label variable edupre_ci "Educacion preescolar"

gen byte eduac_ci=.
replace eduac_ci=1 if (pe141==6)
replace eduac_ci=0 if (pe141==4 | pe141==5 | pe141==7)
label variable eduac_ci "Superior universitario vs superior no universitario"



foreach var of varlist edu* {
replace `var'=. if aedu_ci==. 
}
gen asiste_ci=(pe11==1)
gen pqnoasis_ci=.

**Daniela Zuluaga- Enero 2018: Se agrega la variable pqnoasis1_ci cuya sintaxis fue elaborada por Mayra Saenz**
	
**************
*pqnoasis1_ci*
**************
g       pqnoasis1_ci =.

gen repite_ci=.
gen edupub_ci=(pe13==1)
label var  aedu_ci "Anios de Educacion"

******************************
*	repiteult_ci 
******************************
gen repiteult_ci=.
label var repiteult_ci "Personas que han repetido el ultimo grado"

****************
***tecnica_ci **
****************
gen tecnica_ci=.
replace tecnica_ci=1 if pe141==4
replace tecnica_ci=0 if tecnica_ci ~=1 & ( pe141!=9)
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

rename pf40 codindustria 
rename pf39 codocupa 
compress

saveold "`base_out'", replace


log close
