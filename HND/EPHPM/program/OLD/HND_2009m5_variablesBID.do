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
 


*global ruta = "${surveysFolder}"

local PAIS HND
local ENCUESTA EPHPM
local ANO "2009"
local ronda m5 

local log_file = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\log\\`PAIS'_`ANO'`ronda'_variablesBID.log"
local base_in  = "$ruta\survey\\`PAIS'\\`ENCUESTA'\\`ANO'\\`ronda'\data_merge\\`PAIS'_`ANO'`ronda'.dta"
local base_out = "$ruta\harmonized\\`PAIS'\\`ENCUESTA'\data_arm\\`PAIS'_`ANO'`ronda'_BID.dta"
   
capture log close
log using "`log_file'", replace 

log off
/***************************************************************************
                 BASES DE DATOS DE ENCUESTA DE HOGARES - SOCIOMETRO 
País: Honduras
Encuesta: EPHPM
Round: m5
Autores: Yessenia Loaysa (abr-2013)
Última versión: María Laura Oliveri (MLO) - Email: mloliveri@iadb.org, lauraoliveri@yahoo.com
Fecha última modificación: 26 de Agosto de 2013

							SCL/LMK - IADB
****************************************************************************/
/***************************************************************************
Detalle de procesamientos o modificaciones anteriores:
RAM 27/05/2010
*Inclusión Mayra Sáenz - Julio 2013
****************************************************************************/

clear all
set more off
use "`base_in'", clear


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

	****************
	* region_c *
	****************
gen region_c= depto

label define region_c  ///
           1 "Atlantida" ///
           2 "Colon" ///
           3 "Comayagua" ///
           4 "Copan" ///
           5 "Cortes" ///
           6 "Choluteca" ///
           7 "El Paraiso" ///
           8 "Francisco Morazan" ///
           9 "Gracias a Dios" ///
          10 "Intibuca" ///
          11 "Islas de la bahia" ///
          12 "La paz" ///
          13 "Lempira" ///
          14 "Ocotepeque" ///
          15 "Olancho" ///
          16 "Santa Barbara " ///
          17 "Valle" ///
          18 "Yoro"
 
label value region_c region_c
label var region_c "Division política, departamentos"

***********
*factor_ch*
***********

gen factor_ch=factor
label var factor_ch "Factor de Expansion del Hogar"


********
*idh_ch*
********

gen idh_ch=hogar
*format idh_ch %20.0g 

********
*idp_ci*
********

gen idp_ci=nper

********
*zona_c*
********

gen zona_c=1 if domi==1 | domi==2 | domi==3 
replace zona_c=0 if domi==4
label define zona_c 0 "Rural" 1 "Urbana" 
label value zona_c zona_c

********
*pais_c*
********

gen pais_c="HND"

********
*anio_c*
********

gen anio_c=2009

*******
*mes_c*
*******
cap drop mes_c
gen mes_c=""
tostring fecha, replace
replace mes_c =substr(fecha,2,2) if length(fecha)==7
replace mes_c =substr(fecha,3,2) if length(fecha)==8
destring mes, replace
label define mes_c 9 "Septiembre" 10 "Octubre" 11 "Noviembre" 12 "Diciembre" 1 "Enero" 2 "Febrero" 3 "Marzo" 4 "Abril" 5 "Mayo" 6 "Junio" 7 "Julio" 8 "Agosto"
label value mes_c mes_c



*************
*relacion_ci*
*************

gen relacion_ci=.
replace relacion_ci=1 if Rela_j==1
replace relacion_ci=2 if Rela_j==2
replace relacion_ci=3 if Rela_j==3 | Rela_j==4
replace relacion_ci=4 if Rela_j>=5 & Rela_j<= 8 
replace relacion_ci=5 if Rela_j==9 | Rela_j==11
replace relacion_ci=6 if Rela_j==10
label var relacion_ci "Relacion con el Jefe de Hogar"
label define relacion_ci 1 "Jefe de Hogar" 2 "conyuge" 3 "Hijos" 4 "Otros Parientes" 5 "Otros no Parientes" 6 "Servicio Domestico"
label value relacion_ci relacion_ci

***********
*factor_ci*
***********

gen factor_ci=factor_ch


*********
*sexo_ci*
*********

gen sexo_ci=Sexo
label var sexo "Sexo del Individuo"
label define sexo_ci 1 "Masculino" 2 "Femenino"
label value sexo_ci sexo_ci

*********
*edad_ci*
*********

gen edad_ci=Edad
label var edad_ci "Edad del Individuo"
drop Edad

**********
*civil_ci*
**********

gen civil_ci=.
replace civil_ci=1 if Civil==5
replace civil_ci=2 if Civil==1 | Civil==6
replace civil_ci=3 if Civil==3 | Civil==4
replace civil_ci=4 if Civil==2
label var civil_ci "Estado Civil"
label define civil_ci 1 "Soltero" 2 "Union Formal o Informal" 3 "Divorciado o Separado" 4 "Viudo"
label value civil_ci civil_ci

*********
*jefe_ci*
*********

gen jefe_ci=0
replace jefe_ci=1 if Rela_j==1
label var jefe_ci "Jefe de Hogar Declarado"

**************
*nconyuges_ch*
**************

egen byte nconyuges_ch=sum(Rela_j==2), by (idh)
label variable nconyuges "Numero de Conyuges"

***********
*nhijos_ch*
***********

egen byte nhijos_ch=sum((Rela_j==3 | Rela_j==4)), by (idh)
label variable nhijos_ch "Numero de Hijos"

**************
*notropari_ch*
**************


egen byte notropari_ch=sum(Rela_j==5 | Rela_j==6 | Rela_j==8),by (idh)
label variable notropari_ch "Numero de Otros Parientes "

****************
*notronopari_ch*
****************

egen byte notronopari_ch=sum(Rela_j==7 | Rela_j==9 | Rela_j==11), by (idh)
label variable notronopari_ch "Numero de Otros NO Parientes "

************
*nempdom_ch*
************

egen byte nempdom_ch=sum(Rela_j==10), by (idh)
label variable nempdom_ch "Numero de Empleados Domesticos"


*************
*clasehog_ch*
*************

gen clasehog_ch=.
replace clasehog_ch=1 if nhijos_ch==0 & nconyuges_ch==0 & notropari_ch==0 & notronopari_ch==0 /* unipersonal*/
replace clasehog_ch=2 if nhijos_ch>0 & notropari_ch==0 & notronopari_ch==0 /* nuclear (child with or without spouse but without other relatives)*/
replace clasehog_ch=2 if nconyuges_ch>0 & notropari_ch==0 & notronopari_ch==0 /* nuclear (spouse with or without children but without other relatives)*/
replace clasehog_ch=3 if notropari_ch>0 & notronopari_ch==0 /* ampliado*/
replace clasehog_ch=4 if (nconyuges_ch>0 | nhijos_ch>0 | (notropari_ch>0 & notropari_ch<.)) & (notronopari_ch>0 & notronopari_ch<.) /* compuesto  (some relatives plus non relative)*/
replace clasehog_ch=5 if nhijos_ch==0 & nconyuges_ch==0 & notropari_ch==0 & notronopari_ch>0 & notronopari_ch<./** corresidente*/

label variable clasehog_ch "clasehog_ch - Clase de Hogar"
label define clasehog_ch 1"Unipersonal" 2"Nuclear" 3"Ampliado" 4"Compuesto" 5"Corresidente"
label value clasehog_ch clasehog_ch

*************
*miembros_ci*
*************

gen miembros_ci=1 if Rela_j>=1 & Rela_j<=9
replace miembros_ci=0 if Rela_j==10 | Rela_j==11

**************
*nmiembros_ch*
**************

gen uno=1 if miembros_ci==1
egen nmiembros_ch=sum(uno), by(idh_ch)
replace nmiembros_ch=. if miembros_ci!=1
label var nmiembros_ch "Numero de miembros del Hogar"
drop uno


*************
*nmayor21_ch*
*************

egen nmayor21_ch=sum((Rela_j>0 & Rela_j<=9) & (edad_ci>=21)), by (idh)
label variable nmayor21_ch "Numero de personas de 21 años o mas dentro del Hogar"

**************
*nmmenor21_ch*
**************

egen nmenor21_ch=sum((Rela_j>0 & Rela_j<=9) & (edad_ci<21)), by (idh)
label variable nmenor21_ch "Numero de personas menores a 21 años dentro del Hogar"

**************
*nmmayor65_ch*
**************

egen nmayor65_ch=sum((Rela_j>0 & Rela_j<=9) & (edad_ci>=65)), by (idh)
label variable nmayor65_ch "Numero de personas de 65 años o mas dentro del Hogar"

************
*nmenor6_ch*
************

egen nmenor6_ch=sum((Rela_j>0 & Rela_j<=9) & (edad_ci<6)), by (idh)
label variable nmenor6_ch "Numero de niños menores a 6 años dentro del Hogar"

************
*nmenor1_ch*
************

egen nmenor1_ch=sum((Rela_j>0 & Rela_j<=9) & (edad_ci<1)),  by (idh)
label variable nmenor1_ch "Numero de niños menores a 1 año dentro del Hogar"

****************
****condocup_ci*
****************

gen condocup_ci=condact
replace condocup_ci=4 if edad_ci<10
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

/*********
*emp_ci*
********

gen emp_ci=.
replace emp_ci=1 if p401==1 | p402==1
replace emp_ci=0 if p401==2 & p402==2
label var emp_ci "Empleado en la semana de referencia"

*/
**********
*ocupa_ci*
**********

tostring p425, replace
replace p425 = "0" + p425 if length(p425)==6
gen labor=substr(p425,1,4)
destring labor, replace 

gen ocupa_ci=.
replace ocupa_ci=1 if labor>=2000 & labor<=3999
replace ocupa_ci=2 if labor>=1000 & labor<=1999
replace ocupa_ci=3 if labor>=4000 & labor<=4999
replace ocupa_ci=4 if labor>=5200 & labor<=5999
replace ocupa_ci=5 if labor>=5000 & labor<=5199
replace ocupa_ci=6 if labor>=6000 & labor<=6999
replace ocupa_ci=7 if labor>=7000 & labor<=8999
replace ocupa_ci=8 if labor>=0 & labor<=999
replace ocupa_ci=9 if (labor>=9000 & labor<=9996) | labor==9999
drop labor 

label var ocupa_ci "Ocupacion Laboral en la Actividad Principal"
label define ocupa_ci 1 "PROFESIONALES Y TECNICOS" 2 "GERENTES, DIRECTORES Y FUNCIONARIOS SUPERIORES"  3 "PERSONAL ADMINISTRATIVO Y NIVEL INTERMEDIO" 4 "COMERCIANTES Y VENDEDORES" 5 "TRABAJADORES EN SERVICIOS" 6 "TRABAJADORES AGRICOLAS Y AFINES" 7 "OBREROS NO AGRICOLAS, CONDUCTORES DE MAQUINAS Y VEHICULOS DE TRANSPORTE Y SIMILARES" 8 "FUERZAS ARMADAS" 9 "OTRAS OCUPACIONES NO CLASIFICADAS ANTERIORMENTE"
label values ocupa_ci ocupa_ci
*********
*rama_ci*
*********

gen rama_ci=Ramaop
replace rama_ci=. if Ramaop==10 | Ramaop==11 | emp_ci==0

*************
*horaspri_ci*
*************

gen horaspri_ci=p430
replace horaspri_ci=. if horaspri_ci>168

************
*horastot_ci
************


gen horassec_ci=p461
replace horassec_ci=. if p461>168

gen horastot_ci=horaspri_ci+horassec_ci
replace horastot_ci=horaspri_ci if horassec_ci==.
replace horastot_ci=horassec_ci if horaspri_ci==.

***********
*ylmpri_ci*
***********

gen ylmpri_ci=.
replace ylmpri_ci=p442 if edad_ci>4 & p441==1 & p443==1
replace ylmpri_ci=p442*p443 if edad_ci>4 & ((p441==2 & (p443==1 | p443==2)) | (p441==3 & p443>=1 & p443<=4) | (p441==4 & p443>=1 & p443<=31))
replace ylmpri_ci=p452 if edad_ci>4 & p452>=0 & p452~=.
replace ylmpri_ci=0 if (p442==0 | p452==0) & edad_ci>4 & (p401==1 | p402==1)


replace ylmpri_ci=0 if ( p431==12 |  p431==13) & edad_ci>4 & (p401==1 | p402==1)
label var ylmpri_ci "Ingreso Laboral Monetario de la Actividad Principal"




**************
*categopri_ci*
**************


/*2009


. tab  p431

      p431. En la O.P. Ud trabaja como: |      Freq.     Percent        Cum.
----------------------------------------+-----------------------------------
     1. Empleado(a) u obrero(a) público |      2,495        6.41        6.41
     2. Empleado(a) u obrero(a) privado |     14,716       37.78       44.18
            3. Empleado(a) Doméstico(a) |      1,030        2.64       46.83
4. Miembro de cooperativa de producción |         14        0.04       46.86
5. Cuenta propia que no contrata mano d |      8,112       20.83       67.69
6. Cuenta Propia que contrata mano de o |      1,300        3.34       71.03
            7. Empleador o socio activo |        832        2.14       73.16
              8. Miembro de cooperativa |         38        0.10       73.26
9. Cuenta propia que no contrata mano d |      3,389        8.70       81.96
10. Cuenta propia que contrata mano de  |      2,261        5.80       87.76
         11. Patrón o socio de la finca |        107        0.27       88.04
  12. Trabajador familiar no remunerado |      4,486       11.52       99.56
           13. Trabajador no remunerado |        173        0.44      100.00
----------------------------------------+-----------------------------------
                                  Total |     38,953      100.00


Atención: Se excluye de la categorización a los miembros de cooperativas de producción. (RAM 05/10)
*/
gen categopri_ci=.
replace categopri_ci=1 if		p431==10 | p431==11 | p431==6 | p431==7
replace categopri_ci=2 if		p431==5  | p431==9
replace categopri_ci=3 if 		p431==1  | p431==2  | p431==3
replace categopri_ci=4 if  		p431==12 | p431==13  


label var categopri_ci "Categoria ocupacional actividad principal"
label define categopri_ci 1 "Patron" 2 "Cuenta Propia" 3 "Empleado" 4 "Trabajador no remunerado"
label value categopri_ci categopri_ci






************
*ylnmpri_ci*
************


gen yalim2    =   p444_1	if p444_1>=0
gen yropa2    =   p444_2	if p444_2>=0
gen yhabita2  =   p444_3	if p444_3>=0
gen ytrans2   =   p444_4	if p444_4>=0
egen yotro2   =  rsum(p444_5 p444_6 p444_7 p444_8 p444_9) 
replace yotro2=. if yotro2==0 & p444_5==.
gen yprodu2   =  p453 if  p453>=0 

egen ylnmpri_ci=rsum(yalim2 yropa2 yhabita2 ytrans2 yotro2 yprodu2)
replace ylnmpri_ci=. if yalim2==. & yropa2==. & yhabita2==. & ytrans2==. & yotro2==. & yprodu2==.
replace ylnmpri_ci=0 if ((p444_1==. & p444_2==. & p444_3==. & p444_4==. &  p444_5==. & p444_6==. & p444_7==. & p444_9==. &  categopri==3) | (p453==. & (categopri==1 | categopri==2)))    & edad_ci>4 & (p401==1 | p402==1)
label var ylnmpri_ci "Ingreso Laboral No Monetario de la Actividad Principal"

***********
*ylmsec_ci*
***********

gen ylmsec_ci=.
replace ylmsec_ci=p473 if p473<99999 & edad_ci>4 & p472==1 & p474==1
replace ylmsec_ci=p473*p474 if p473<99999 & edad_ci>4 & ((p472==2 & (p474==1 | p474==2)) | (p472==3 & p474>=1 & p474<=4) | (p472==4 & p474>=1 & p474<=31))
replace ylmsec_ci= p483 if  p483<99999 & edad_ci>4 &  p483>=0
replace ylmsec_ci=0 if p473==0 &  p483==0 & edad_ci>4 & p455==1
label var ylmsec_ci "Ingreso Laboral Monetario de la Actividad Secundaria"

*p473-> Cuanto le pagan en actividad ->

*p105   p472 
*p106   p473
*p107   p474
*p89    p455
*p117   p484

**************
*categosec_ci*
**************

/* CATEGOSEC_CI
   Ocupacion Secundaria,
   

 P462 En la ocupación de... ¿Ud trabaja |
                                  como: |      Freq.     Percent        Cum.
----------------------------------------+-----------------------------------
     1. Empleado(a) u obrero(a) público |        115        1.20        1.20
     2. Empleado(a) u obrero(a) privado |      1,897       19.84       21.05
            3. Empleado(a) Doméstico(a) |         27        0.28       21.33
4. Miembro de cooperativa de producción |          7        0.07       21.40
5. Cuenta propia que no contrata mano d |      1,831       19.15       40.55
6. Cuenta Propia que contrata mano de o |        167        1.75       42.30
            7. Empleador o socio activo |        101        1.06       43.36
              8. Miembro de cooperativa |         20        0.21       43.57
9. Cuenta propia que no contrata mano d |      2,020       21.13       64.70
10. Cuenta propia que contrata mano de  |      1,310       13.70       78.40
         11. Patrón o socio de la finca |         52        0.54       78.94
  12. Trabajador familiar no remunerado |      1,945       20.35       99.29
           13. Trabajador no remunerado |         68        0.71      100.00
----------------------------------------+-----------------------------------
                                  Total |      9,560      100.00
 	
*/

gen categosec_ci=.
replace categosec_ci=1 if (p462==7 | p462==11)
replace categosec_ci=2 if (p462==4 | p462==5 | p462==6 | p462==8 | p462==9 | p462==10)
replace categosec_ci=3 if (p462==1 | p462==2 | p462==3)
replace categosec_ci=4 if (p462==12| p462==13)
label var categosec_ci "Categoria ocupacional actividad secundaria"
label define categosec_ci 1 "Patron" 2 "Cuenta Propia" 3 "Empleado" 4 "Trabajador no remunerado"
label value categosec_ci categosec_ci




************
*ylnmsec_ci*
************


gen yalim3    =   p475_1	if p475_1>=0
gen yropa3    =   p475_2	if p475_2>=0
gen yhabita3  =   p475_3	if p475_3>=0
gen ytrans3   =   p475_4	if p475_4>=0
egen yotro3   =  rsum(p475_5 p475_6 p475_7 p475_8 p475_9) 
replace yotro3=. if yotro3==0 & p475_5==.
gen yprodu3   = p484 if p484>=0 

egen ylnmsec_ci=rsum(yalim3 yropa3 yhabita3 ytrans3 yotro3 yprodu3)
replace ylnmsec_ci=. if yalim3==. & yropa3==. & yhabita3==. & ytrans3==. & yotro3==. & yprodu3==.

replace ylnmsec_ci=0 if ((p475_1==. & p475_2==. & p475_3==. & p475_4==. & ///
                          p475_5==. & p475_6==. & p475_7==. & p475_9==. & ///
                          categosec==3) | (p484==. & (categosec==1 | categosec==2))) ///
                          & edad_ci>4 & p455==1
label var ylnmsec_ci "Ingreso Laboral No Monetario de la Actividad Secundaria"


*************
*ylmotros_ci*
*************

gen ylmotros_ci=yotrf

**************
*ylnmotros_ci*
**************

gen ylnmotros_ci=.

*************
*nrylmpri_ci*
*************

gen nrylmpri_ci=.

*************
*tcylmpri_ci*
*************

gen tcylmpri_ci=.

********
*ylm_ci*
********

egen ylm_ci=rsum(ylmotros_ci ylnmotros_ci ylmsec_ci ylmpri_ci)

replace ylm_ci=. if ylmotros_ci==. & ylnmotros_ci==. & ylmsec_ci==. & ylmpri_ci==.


*********
*ylnm_ci*
*********

egen ylnm_ci=rsum( ylnmsec_ci ylnmpri_ci)

replace ylnm_ci=. if ylnmsec_ci==. & ylnmpri_ci==.

*********
*ynlm_ci*
*********

gen ynlm_ci=yotrf

**********
*ynlnm_ci*
**********

gen ynlnm_ci=.

*************
*nrylmpri_ch*
*************

gen nrylmpri_ch=.

*************
*tcylmpri_ch*
*************

gen tcylmpri_ch=.


********
*ylm_ch*
********


egen ylm_ch=sum(ylm_ci) if miembros_ci==1, by(idh_ch)
label var ylm_ch "Ingreso Laboral Monetario del Hogar"


**********
*ylmnr_ch*
**********

egen ylmnr_ch=sum(ylm_ci) if miembros_ci==1 & nrylmpri_ch==0, by(idh_ch)
label var ylmnr_ch "Ingreso Laboral Monetario del Hogar, considera 'missing' la No Respuesta"

*********
*ylnm_ch*
*********


egen ylnm_ch=sum(ylnm_ci) if miembros_ci==1, by(idh_ch)
label var ylnm_ch "Ingreso Laboral No Monetario del Hogar"

*********
*ynlm_ch*
*********
cap drop ynlm_ch
egen ynlm_ch=sum(ynlm_ci) if miembros_ci==1, by(idh_ch)
label var ynlm_ch "Ingreso No Laboral del Hogar (monetario + no monetario)"


**********
*ynlnm_ch*
**********

egen ynlnm_ch=sum(ynlnm_ci) if miembros_ci==1, by(idh_ch)
label var ynlnm_ch "Ingreso No Laboral Monetario del Hogar"

*************
*ylmhopri_ci*
*************


gen ylmhopri_ci=ylmpri_ci/(4.3*horaspri)
label var ylmhopri_ci "Salario Horario Monetario de la Actividad Principal"

**********
*ylmho_ci*
**********

gen ylmho_ci=ylm_ci/(4.3*horastot)
label var ylmho_ci "Salario Horario Monetario de todas las Actividades"

*************
*rentaimp_ch*
*************

gen rentaimp_ch=.

****************
*autoconsumo_ci*
****************


gen autoconsumop_ci=yprodu2 
replace autoconsumop_ci=0 if p484==. & edad_ci>4 & (categopri==1 | categopri==2) & ( p401==1| p402==1 )
label var autoconsumop_ci "Autoconsumo Individual Actividad Principal(Trabajadores Independientes)"

gen autoconsumos_ci=yprodu2 
replace autoconsumos_ci=0 if  p484==. & edad_ci>4 & (categosec==1 | categosec==2) &  p455==1
label var autoconsumos_ci "Autoconsumo Individual Actividad Secundaria(Trabajadores Independientes)"

egen autoconsumo_ci=rsum(autoconsumop_ci autoconsumos_ci)
replace autoconsumo_ci=. if autoconsumop_ci==. & autoconsumos_ci==.
label var autoconsumo_ci "Autoconsumo Individual (Trabajadores Independientes)"

****************
*autoconsumo_ch*
****************

egen autoconsumo_ch=sum(autoconsumo_ci) if miembros_ci==1, by(idh_ch)
label var autoconsumo_ch "Autoconsumo del Hogar"

************
*remesas_ci*
************

gen remesas_ci=.

************
*remesas_ch*
************

gen remesas_ch=.

************
*durades_ci*
************

gen durades_ci=.
replace durades_ci=p411_cant/(365/12)      if p411_frec==1
replace durades_ci=p411_cant/((365/7)/12)  if p411_frec==2
replace durades_ci=p411_cant               if p411_frec==3
label var durades "Duracion del Desempleo (en meses)"



***************
*antiguedad_ci*
***************


gen antiguedad=.
replace antiguedad=p433_tiempo/365           if  p433_cant==1
replace antiguedad=p433_tiempo/52            if  p433_cant==2
replace antiguedad=p433_tiempo/12            if  p433_cant==3
replace antiguedad=p433_tiempo               if  p433_cant==4



 

label var antiguedad "Antiguedad en la Ocupacion Actual (en anios)"

/*
************
*desemp1_ci*
************

gen desemp1_ci=.
replace desemp1_ci=1 if p401==2 & p402==2 & p403==2 & p405==1
replace desemp1_ci=0 if p401==1 | p402==1 | p403==3 | p405==2

************
*desemp2_ci*
************

gen desemp2_ci=desemp1_ci

replace desemp2_ci=1 if p409==1 | p409==2 | p409==3

************
*desemp3_ci*
************

gen desemp3_ci=desemp2_ci
replace desemp3_ci=1 if p406==1


*********
*pea1_ci*
*********


gen byte pea1_ci=.
replace pea1=1 if emp==1 | desemp1==1
replace pea1=0 if emp==0 & desemp1==0
label variable pea1_ci "Poblacion Economicamente Activa utilizando la definicion 'desemp1'"

*********
*pea2_ci*
*********

gen byte pea2_ci=.
replace pea2=1 if emp==1 | desemp2==1
replace pea2=0 if emp==0 & desemp2==0
label variable pea2_ci "Poblacion Economicamente Activa utilizando la definicion 'desemp2'"

*********
*pea3_ci*
*********

gen byte pea3_ci=.
replace pea3=1 if emp==1 | desemp3==1
replace pea3=0 if emp==0 & desemp3==0
label variable pea3_ci "Poblacion Economicamente Activa utilizando la definicion 'desemp3'"
*/

*************
*desalent_ci*
*************

gen desalent_ci=.
replace desalent_ci=1 if p409==6
replace desalent_ci=0 if p409!=6 & p409!=.

***********
*subemp_ci*
***********

gen subemp_ci=.
replace subemp=0 if emp_ci==0 | emp_ci==1
replace subemp=1 if horastot<30 & p486==1
label var subemp "Trabajadores subempleados"

***************
*tiempoparc_ci*
***************

gen tiempoparc=.
replace tiempoparc=0 if emp_ci==0 | emp_ci==1
replace tiempoparc=1 if horastot<30 & p486==2
label var tiempoparc "Trabajadores a medio tiempo"

/**************
*contrato_ci*
*************

gen contrato_ci=.
replace contrato_ci=1 if p434==1 | p465==1
replace contrato_ci=0 if (p434==2) & (p465==2 | p465==.)
replace contrato_ci=0 if p465==2 & p434==.
label var contrato "Peronas empleadas que han firmado un contrato de trabajo"*/

***********
*segsoc_ci*
***********
*gen segsoc_ci=.

*************
*nempleos_ci*
*************

gen nempleos_ci=1 if p455==2
replace nempleos_ci=2 if p455==1


**************
*firmpapeq_ci*
**************

**************
*firmpapeq_ci*
**************
* modificacion MLO 2013
gen firmapeq_ci=.
replace firmapeq_ci=1 if p432_cant>=1 & p432_cant<=5 /*asalariados*/
replace firmapeq_ci=1 if p446_cant>=1 & p446_cant<=5  /*cuenta propia*/
replace firmapeq_ci=0 if  p432_cant>=6 &  p432_cant<9999	
replace firmapeq_ci=0 if p446_cant>=6 & p446_cant<9999	
tab firmapeq_ci [w=int(factor_ci)] 

/*
gen firmapeq_ci=0 if p454_cant>5 & p454_cant<10000
replace firmapeq_ci=1 if p454_cant<=5 & p454_cant!=0
*/
*************
*spublico_ci*
*************

gen spublico_ci=1 if p431==1 | p462==1
replace spublico_ci=0 if p431!=1 & p462!=1



************************************************************************
************************************************************************
****************************EDUCACION***********************************
************************************************************************
************************************************************************

gen asiste_ci=.
replace asiste_ci=1 if p103==1
replace asiste_ci=0 if p103==2
label var asiste "Personas que actualmente asisten a centros de enseñanza"

ren p104 pqnoasis_ci



label var pqnoasis_ci "Razones para no asistir a centros de enseñanza"





gen repiteult_ci=.
replace repiteult_ci=1 if p113==1
replace repiteult_ci=0 if p113==2
label var repiteult_ci "Personas que están repetiendo el ultimo grado"





gen repite_ci=.
label var repite_ci "Personas que han repetido al menos un año o grado"


** para quienes ya no asisten
gen aedu_ci=anosest
label var aedu_ci "Años de educacion aprobados"


/* En 2007, en la variable P05 y P11
   
   P05. ¿Cual es el nivel educativo más alto que alcanzó?
   P11. ¿Cual es el nivel educativo en el que estudia actualmente?
   
   apareció la clasificación 8. Superior no Universitario. 
   Para ser consistentes con las anteriores harmonizaciones
   se decidió incluirla dentro de la categoría: 7. Técnico Superior
   
   También recodificamos los valores 9 a 8 y 10 a 9, para asegurar la consistencia. */
   
replace p105=7 if p105==8
replace p105=8 if p105==9
replace p105=9 if p105==10

label define nivel_educativo_nuevo 1"1. Ninguno" 2"2. Programa de alfabetizacion" 3"3. Pre-escolar"	///
                                   4"4. Primaria" 5"5. Ciclo comun" 6"6. Diversificado" 		///
                                   7"7. Tecnico superior" 8"8. Superior universitaria" 9"9. Post-grado" ///
                                   99"99. No sabe/no responde"
label value p105 nivel_educativo_nuevo

replace p111=7 if p111==8
replace p111=8 if p111==9
replace p111=9 if p111==10
label value p111 nivel_educativo_nuevo

gen eduno_ci=.
replace eduno=1 if (p105==1 & edad_ci>=5) | (p111==3 & p115==1)
replace eduno=0 if (p105>3 & p105<=9 & edad_ci>=5) | ((p111>2 & p111<9) | (p111==3 & p115>1))    
label var eduno_ci "1 = personas sin educacion (excluye preescolar)"

gen edupi_ci=.
replace edupi=1 if (p105==4 & p108<6 & p108>=0) | (p111==3 & p115<7 & p115>=1)
replace edupi=0 if ((p105>=5 & p105<=9) | (p105==4 & p108>=6)) | ((p111>=4 & p111<9) | (p111==3 & p115>=7)) | (eduno==1)
label var edupi_ci "1 = personas que no han completado el nivel primario"

gen edupc_ci=.
replace edupc=1 if (p105==4 & p108==6) 
replace edupc=0 if (edupi==1 | eduno==1) | (p105==4 & p108>6) | (p111==3 & p115>=7) | (p105>4 & p105<=9) | (p111>3 & p111<9)
replace edupi=1 if p105==4 & (p108==0 | p108==.)
label var edupc_ci "1 = personas que han completado el nivel primario"

gen edusi_ci=.
replace edusi=1 if (p105==4 & p108>6 & p108<.) | (p111==3 & p115>=7 & p115<.) | (p105==5 & p108<3) | (p105==6 & p108<4) | (p111==4 & p115<=3) | (p111==5 & p115<=4)
replace edusi=0 if (edupi==1 | eduno==1 | edupc==1) | (p105==5 & p108>=3 & p108<.) | (p105==6 & p108>=4 & p108<.) | (p105>=7 & p105<=9) | (p111>=6 & p111<9) 
label var edusi_ci "1 = personas que no han completado el nivel secundario"

gen edusc_ci=.
replace edusc=1 if (p105==5 & p108>=3 & p108<.) | (p105==6 & p108>=4 & p108<.) 
replace edusc=0 if (edupi==1 | eduno==1 | edupc==1 | edusi==1) | (p105>6 & p105<=9) | ( p111>5& p111<9)
label var edusc_ci "1 = personas que han completado el nivel secundario"

gen eduui_ci=.
replace eduui=1 if (p105==7 & p108<3) | (p105==8 & p108<5) | (p111==6 & p115<3) | (p111==7 & p115<5)
replace eduui=0 if (edupi==1 | eduno==1 | edupc==1 | edusi==1 | edusc==1) | (p105==7 & p108>=3) | (p105==8 & p108>=5) | (p111==6 & p115>=3) | (p111==7 & p115>=5) | (p105==9) | (p111==8)
label var eduui_ci "1 = personas que no han completado el nivel universitario"

gen eduuc_ci=.
replace eduuc=1 if (p105==7 & p108>=3 & p108<.) | (p105==8 & p108>=5 & p108<.) | (p111==6 & p115>=3 & p115<.) | (p111==7 & p115>=5 & p115<.) | p105==9 | p111==8
replace eduuc=0 if (edupi==1 | eduno==1 | edupc==1 | edusi==1 | edusc==1 | eduui==1) 
label var eduuc_ci "1 = personas que han completado el nivel universitario"

replace edupi=1 if p105==4 & p108==.
replace edupc=0 if p105==4 & p108==.
replace edusi=1 if (p105==5 | p105==6) & p108==.
replace edusc=0 if (p105==5 | p105==6) & p108==.
replace eduui=1 if (p105==7 | p105==8) & p108==.
replace eduuc=0 if (p105==7 | p105==8) & p108==.

gen edus1i_ci=.
replace edus1i=0 if edusi==1 | edusc==1 
replace edus1i=1 if edusi==1 & (p105==4 & (p108==7 | p108==8)) | (p111==3 & (p115==7| p115==8| p115==9))
label var edus1i_ci "1 = personas que no han completado el primer ciclo de la educacion secundaria"

gen edus1c_ci=.
replace edus1c=0 if edusi==1 | edusc==1 
replace edus1c=1 if edusi==1 & (p105==4 & p108==9)
label var edus1c_ci "1 = personas que han completado el primer ciclo de la educacion secundaria"

gen edus2i_ci=.
replace edus2i=0 if edusi==1 | edusc==1 
replace edus2i=1 if edusi==1 & (p105==5 & p108<3) | (p105==6 & p108<4) | (p111==4) | (p111==5)
label var edus2i_ci "1 = personas que no han completado el segundo ciclo de la educacion secundaria"

gen edus2c_ci=.
replace edus2c=0 if edusi==1 
replace edus2c=1 if edusc==1
label var edus2c_ci "1 = personas que han completado el segundo ciclo de la educacion secundaria"

gen edupre_ci=.
replace edupre=0 if eduno==1 | edupi==1 | edupc==1 | edusi==1 | edusc==1 | eduui==1 | eduuc==1
replace edupre=1 if p105==3 | p111==2
label var edupre_ci "Educacion preescolar"

gen eduac_ci=.
replace eduac=0 if eduui==1 | eduuc==1
replace eduac=1 if p105==8 | p111==7
label var eduac_ci "Educacion universitaria vs educacion terciaria"

/* RECODIFIED, Option 10, 11 and 12 were considered Private = 246 obs

      p09 A traves de que sistema estudió |      Freq.     Percent        Cum.
    --------------------------------------+-----------------------------------
                               1. PROHECO |         94        0.20        0.20
                            2. EDUCATODOS |        285        0.61        0.81
                              3. PRALEBAH |        149        0.32        1.12
          4. Presencial en centro público |     42,841       91.20       92.33
          5. Presencial en centro privado |      2,917        6.21       98.54
 6. Presencial en centro privado bilingue |         32        0.07       98.61
            7. Por radio(maestro en casa) |         90        0.19       98.80
         8. A distancia en centro publico |        144        0.31       99.10
         9. A distancia en centro privado |         37        0.08       99.18
                     10. En el extranjero |        203        0.43       99.61
  11. Educacion virtual publica o privada |          3        0.01       99.62
                    12. ONG,s o Fundación |         40        0.09       99.71
                              13. CCEPREB |          3        0.01       99.71
                  99. No sabe/no responde |        135        0.29      100.00
    --------------------------------------+-----------------------------------
                                    Total |     46,973      100.00          */

gen edupub_ci=.
replace edupub_ci=1 if (p109==1|p109==2|p109==3|p109==4|p109==7|p109==8|p109==13)
replace edupub_ci=0 if (p109==5|p109==6|p109==9|p109==10|p109==11|p109==12)

replace edupub_ci=1 if (p116==1|p116==2|p116==3|p116==4|p116==7|p116==8|p116==13)
replace edupub_ci=0 if (p116==5|p116==6|p116==9|p116==10|p116==11|p116==12)



label var edupub_ci "1 = personas que asisten a centros de enseñanza publicos"

************
*aguared_ch*
************

gen aguared_ch=.
replace aguared_ch=1 if v05==1
replace aguared_ch=0 if v05==2 

*************
*aguadist_ch*
*************

gen aguadist_ch=.
replace aguadist_ch=1 if v09==1
replace aguadist_ch=2 if (v09==2 | v09==3)
replace aguadist_ch=3 if v09==4

*************
*aguamala_ch*
*************

gen aguamala_ch=.
replace aguamala_ch=1 if v06>=5 & v06<=8
replace aguamala_ch=0 if v06>=1 & v06<=4

*************
*aguamide_ch*
*************


gen aguamide_ch=.

********
*luz_ch*
********


gen luz_ch=1 if v10==1 |v10==2 |v10==3 
replace luz_ch=0 if v10>=4 & v10<=8

************
*luzmide_ch*
************

gen luzmide_ch=.


************
*combust_ch*
************

gen combust_ch=1 if h04==3 | h04==2 | h04==4
replace combust_ch=0 if h04==5 | h04==1


*********
*bano_ch*
*********

gen bano_ch=.
replace bano_ch=1 if h05==1
replace bano_ch=0 if h05==2

gen banoex_ch=.
replace banoex_ch=1 if h07==1
replace banoex_ch=0 if h07==2

gen des1_ch=.
replace des1_ch=0 if h05==2
replace des1_ch=1 if (h06==1|h06==2)
replace des1_ch=2 if (h06==5|h06==6|h06==7)
replace des1_ch=3 if (h06==3|h06==4)

gen des2_ch=.
replace des2_ch=1 if (h06==1|h06==2|h06==3)
replace des2_ch=2 if (h06==4|h06==5|h06==6|h06==7)
replace des2_ch=0 if h05==2

gen piso_ch=.
replace piso_ch=0 if v03==7
replace piso_ch=1 if v03>=1 & v03<=6 
replace piso_ch=2 if v03==8 

gen techo_ch=.
replace techo_ch=0 if v04==6 | v04==7
replace techo_ch=1 if v04>=1 & v04<=5
replace techo_ch=2 if v04==8| v04==9 | v04==10

gen resid_ch=.
replace resid_ch=0 if (v11==1|v11==3)
replace resid_ch=1 if (v11==4|v11==6)
replace resid_ch=2 if (v11==2|v11==7)
replace resid_ch=3 if (v11==5|v11==8)

gen dorm_ch=.
replace dorm_ch=h01 if h01>=0 

gen cuartos_ch=.
replace cuartos_ch=v16 if v16>=0 



***********
*cocina_ch*
***********

gen cocina_ch=.


*Encuesta 2009 no tiene informaci'on sobre bienes durables de los hogares

**********
*telef_ch*
**********

gen telef_ch=.

***********
*regrig_ch*
***********

gen refrig_ch=.

**********
*freez_ch*
**********

gen freez_ch=.

*********
*auto_ch*
*********

gen auto_ch=.

**********
*compu_ch*
**********

gen compu_ch=.

*************
*internet_ch*
*************

gen internet_ch=(p306_1==1)

********
*cel_ch*
********

gen cel_ch=(p308==1)

**********
*vivi1_ch*
**********


gen vivi1_ch=.
replace vivi1_ch=1 if v01==1 | v01==2
replace vivi1_ch=2 if v01==4
replace vivi1_ch=3 if (v01>=5 & v01<=7) | v01==3 


**********
*vivi2_ch*
**********


gen vivi2_ch=.
replace vivi2_ch=1 if vivi1_ch==1 | vivi1_ch==2
replace vivi2_ch=0 if vivi1_ch==3


*************
*viviprop_ch*
*************

gen viviprop_ch=.
replace viviprop_ch=0 if v14==1
replace viviprop_ch=1 if v14==3
replace viviprop_ch=2 if v14==2
replace viviprop_ch=3 if (v14==4 | v14==5 | v14==6 | v14==7)

gen vivitit_ch=.
replace vivitit_ch=1 if v17==1
replace vivitit_ch=0 if v17==2

/* Tipo de cambio lempiras por dolares = 19.92
   Variable cambio en la Base de otros Ingresos */

gen vivialq_ch=.
replace vivialq_ch=v15 if v15m==1
replace vivialq_ch=v15/19.92 if v15m==2



gen antiguedad_ci=.
replace antiguedad_ci = p433_cant
replace antiguedad_ci = p433_cant if p433_tiempo==3
replace antiguedad_ci = p433_cant/30 if p433_tiempo ==1
replace antiguedad_ci = p433_cant/4 if p433_tiempo==2
replace antiguedad_ci = p433_cant*12 if p433_tiempo==4


/************************************************************************************************************
* 2. Recategorización de variables/ corrección de errores de las bases armonizadas
************************************************************************************************************/

tostring p423_1 p423_2 ,replace
*gen ce23_lmk=substr(p423_1,1,1) 
gen aux_a=substr(p423_1,1,1) if p423_1!="9999999999" & p423_1!="."
gen aux_b=substr(p423_1,2,1) if p423_1!="9999999999" & p423_1!="."
gen aux_c=substr(p423_1,3,1) if p423_1!="9999999999" & p423_1!="."
gen aux_d=substr(p423_1,4,1) if p423_1!="9999999999" & p423_1!="."
gen aux_e=substr(p423_1,5,1) if p423_1!="9999999999" & p423_1!="."
gen aux_f=substr(p423_1,6,1) if p423_1!="9999999999" & p423_1!="."
destring aux_*, replace
*destring p423_1 p423_2 ce23_lmk,replace
*replace ce23_lmk=. if ce23_lmk==0
*replace ce23_lmk=10 if p423_1==10
*replace ce23_lmk=11 if p423_2==1

gen semestre_c=.

/************************************************************************************************************
* 3. Creación de nuevas variables de SS and LMK a incorporar en Armonizadas
************************************************************************************************************/


**********
**tc_ci***
**********
gen tc_ci=19.03
label var tc_ci "Tipo de cambio LCU/USD"

*************
**salmm_ci***
*************
* HND2010
gen salmm_ci= 	4777.50
label var salmm_ci "Salario minimo legal"




****************
*afiliado_ci****
****************
gen afiliado_ci=.
label var afiliado_ci "Afiliado a la Seguridad Social"

****************
*cotizando_ci***
****************
gen cotizando_ci=.
replace cotizando_ci=1 if (aux_a>=1 & aux_a<=6) |  (aux_b>=1 & aux_b<=6)| (aux_c>=1 & aux_c<=6)| (aux_d>=1 & aux_d<=6) | (aux_e>=1 & aux_e<=6) | (aux_f>=1 & aux_f<=6)
recode cotizando_ci .=0 if condact>=1 & condact<=2
label var cotizando_ci "Cotizante a la Seguridad Social"
label define cotizando_ci 0"No cotiza" 1"Cotiza a la SS" 
label value cotizando_ci cotizando_ci

*****************
*tipocontrato_ci*
*****************

recode p434 (1=1) (2=3) (nonmissing=.), gen(tipocontrato_ci)
label var tipocontrato_ci "Tipo de contrato segun su duracion"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci

*************
*cesante_ci* 
*************

gen cesante_ci=p412 if p412==1 & condocup_ci==2
replace cesante_ci=. if p412==9
label var cesante_ci "Desocupado -definicion oficial del pais- que ha trabajado antes"	


*************
*tamemp_ci
*************


egen tamemp_ci=rsum(p432_cant p446_cant) if p446_cant!= 99999 & p432_cant!=99999 
replace tamemp_ci= p432_cant  if p446_cant==99999 & p432_cant!= .
replace tamemp_ci= p446_cant  if p432_cant==99999 & p446_cant!= .

label var tamemp_ci "# empleados en la empresa"


****************
*tipopen_ci*****
****************
gen tipopen_ci=.
label var tipopen_ci "Tipo de pension - variable original de cada pais" 


****************
*instcot_ci*****
****************
gen instcot_ci=.

****************
*instpen_ci*****
****************
gen instpen_ci=.
label var instpen_ci "Institucion proveedora de la pension - variable original de cada pais" 

*************
**ypen_ci*
*************
egen ypen_ci=rsum(pension jubilacion)
label var ypen_ci "Valor de la pension contributiva"


*************
**pension_ci*
*************

gen pension_ci=1 if ypen_ci!=. & ypen_ci!=0
recode pension_ci .=0

label var pension_ci "1=Recibe pension contributiva"

*****************
**ypensub_ci*
*****************

gen ypensub_ci=bonpraf
label var ypensub_ci "Valor de la pension subsidiada / no contributiva"

***************
*pensionsub_ci*
***************

gen pensionsub_ci=ypensub_ci!=.
label var pensionsub_ci "1=recibe pension subsidiada / no contributiva"


*************
***tecnica_ci**
*************
gen tecnica_ci=.
replace tecnica_ci=1 if p111==7
recode tecnica_ci .=0 
label var tecnica_ci "=1 formacion terciaria tecnica"

*Poverty
*********
*lp25_ci
*********
gen lp25_ci = 974.8764

label var lp25_ci "Linea de pobreza de uds1.25 por dia en moneda local"

*********
*lp4_ci*
*********
gen lp4_ci =1559.802

label var lp4_ci "Linea de pobreza de uds4 por dia en moneda local"


*********
*lp_ci***
*********
capture drop lp_ci
gen lp_ci =.
label var lp_ci "Linea de pobreza oficial del pais"

*********
*lpe_ci***
*********

gen lpe_ci =.
label var lpe_ci "Linea de indigencia oficial del pais"

*YL: genero las siguientes variables con missing para correr sociometro. Estas variables deben ser creadas correctamente.

gen pared_ch=.

gen autocons_ci=.
gen autocons_ch=.
gen tiempoparc_ci=.
gen vivialqimp_ch=.
gen raza_ci=.

**Verificación de que se encuentren todas las variables del SOCIOMETRO y las nuevas de mercado laboral
qui sum factor_ch	idh_ch	idp_c	zona_c	pais_c	anio_c	mes_c	relacion_ci	factor_ci	sexo_ci	edad_ci	civil_ci	///
jefe_ci	nconyuges_ch	nhijos_ch	notropari_ch	notronopari_ch	nempdom_ch	clasehog_ch	nmiembros_ch	///
miembros_ci	nmayor21_ch	nmenor21_ch	nmayor65_ch	nmenor6_ch	nmenor1_ch	ocupa_ci	rama_ci	horaspri_ci	///
horastot_ci	ylmpri_ci	ylnmpri_ci	ylmsec_ci	ylnmsec_ci	ylmotros_ci	ylnmotros_ci	nrylmpri_ci	tcylmpri_ci ///
ylm_ci	ylnm_ci	ynlm_ci	ynlnm_ci	nrylmpri_ch	tcylmpri_ch	ylm_ch	ylnm_ch	ylmnr_ch	ynlm_ch	ynlnm_ch	///
ylmhopri_ci	ylmho_ci	rentaimp_ch	autocons_ci	autocons_ch	remesas_ci	remesas_ch	durades_ci	antiguedad_ci ///
emp_ci	desemp_ci	pea_ci	 desalent_ci	subemp_ci	tiempoparc_ci ///
categopri_ci	categosec_ci	nempleos_ci	firmapeq_ci	spublico_ci	aedu_ci	eduno_ci ///
edupi_ci	edupc_ci	edusi_ci	edusc_ci	eduui_ci	eduuc_ci	edus1i_ci	edus1c_ci	edus2i_ci ///
edus2c_ci	edupre_ci	eduac_ci	asiste_ci	pqnoasis	repite_ci	repiteult_ci	edupub_ci	///
aguared_ch	aguadist_ch	aguamala_ch	aguamide_ch	luz_ch	luzmide_ch	combust_ch	bano_ch	banoex_ch	///
des1_ch	des2_ch	piso_ch	pared_ch	techo_ch	resid_ch	dorm_ch	cuartos_ch	cocina_ch	telef_ch ///
refrig_ch	freez_ch	auto_ch	compu_ch	internet_ch	cel_ch	vivi1_ch	vivi2_ch	viviprop_ch	///
vivitit_ch	vivialq_ch	vivialqimp_ch region_BID_c region_c raza_ci        lp25_ci	       lp4_ci	 ///
lp_ci	       lpe_ci	       cotizando_ci	             afiliado_ci	///
tipopen_ci	   instpen_ci	   instcot_ci	   instpen_ci	   tipocontrato_ci 	   condocup_ci 	   cesante_ci ///
tamemp_ci 	   pension_ci 	   ypen_ci 	   pensionsub_ci 	   ypensub_ci 	   salmm_ci	   tecnica_ci	


qui destring $var, replace
 

* Activar solo si es necesario
*keep *_ci  *_c  idh_ch 
qui compress



saveold "`base_out'", replace


log close





