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
 


*global ruta = "\\Sdssrv03\surveys"

local PAIS HND
local ENCUESTA EPHPM
local ANO "2010"
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
Mayra Sáenz (Julio 2013)
Última versión: María Laura Oliveri (MLO) - Email: mloliveri@iadb.org, lauraoliveri@yahoo.com
Fecha última modificación: 26 de Agosto de 2013

							SCL/LMK - IADB
****************************************************************************/
/***************************************************************************
Detalle de procesamientos o modificaciones anteriores:
****************************************************************************/

clear all
set more off
use "`base_in'", clear


*************************************************************************
*************************           HONDURAS                *************
************************* MAYO 2010 (No se dispone de Sept10)************
*************************************************************************

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
gen anio_c=2010

*******
*mes_c*
*******
cap drop mes_c
gen mes_c= mes 
label define mes_c 9 "Septiembre" 10 "Octubre" 11 "Noviembre" 12 "Diciembre" 1 "Enero" 2 "Febrero" 3 "Marzo" 4 "Abril" 5 "Mayo" 6 "Junio" 7 "Julio" 8 "Agosto"
label value mes_c mes_c

*************
*relacion_ci*
*************
gen relacion_ci=.
replace relacion_ci=1 if rela_j==1
replace relacion_ci=2 if rela_j==2
replace relacion_ci=3 if rela_j==3 | rela_j==4
replace relacion_ci=4 if rela_j>=5 & rela_j<= 8 
replace relacion_ci=5 if rela_j==9 | rela_j==11
replace relacion_ci=6 if rela_j==10
label var relacion_ci "Relacion con el Jefe de Hogar"
label define relacion_ci 1 "Jefe de Hogar" 2 "conyuge" 3 "Hijos" 4 "Otros Parientes" 5 "Otros no Parientes" 6 "Servicio Domestico"
label value relacion_ci relacion_ci

*********
*raza_ci*
*********
gen raza_ci=.
***********
*factor_ci*
***********
gen factor_ci=factor_ch

*********
*sexo_ci*
*********
gen sexo_ci=sexo
label var sexo "Sexo del Individuo"
label define sexo_ci 1 "Masculino" 2 "Femenino"
label value sexo_ci sexo_ci

*********
*edad_ci*
*********
gen edad_ci=edad
label var edad_ci "Edad del Individuo"
drop edad

**********
*civil_ci*
**********
gen civil_ci=.
replace civil_ci=1 if civil==5
replace civil_ci=2 if civil==1 | civil==6
replace civil_ci=3 if civil==3 | civil==4
replace civil_ci=4 if civil==2
label var civil_ci "Estado Civil"
label define civil_ci 1 "Soltero" 2 "Union Formal o Informal" 3 "Divorciado o Separado" 4 "Viudo"
label value civil_ci civil_ci

*********
*jefe_ci*
*********
gen jefe_ci=0
replace jefe_ci=1 if rela_j==1
label var jefe_ci "Jefe de Hogar Declarado"

**************
*nconyuges_ch*
**************
egen byte nconyuges_ch=sum(rela_j==2), by (idh)
label variable nconyuges "Numero de Conyuges"

***********
*nhijos_ch*
***********
egen byte nhijos_ch=sum((rela_j==3 | rela_j==4)), by (idh)
label variable nhijos_ch "Numero de Hijos"

**************
*notropari_ch*
**************


egen byte notropari_ch=sum(rela_j==5 | rela_j==6 | rela_j==8),by (idh)
label variable notropari_ch "Numero de Otros Parientes "

****************
*notronopari_ch*
****************

egen byte notronopari_ch=sum(rela_j==7 | rela_j==9 | rela_j==11), by (idh)
label variable notronopari_ch "Numero de Otros NO Parientes "

************
*ndom_ch*
************

egen byte nempdom_ch=sum(rela_j==10), by (idh)
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

gen miembros_ci=1 if rela_j>=1 & rela_j<=9
replace miembros_ci=0 if rela_j==10 | rela_j==11

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

egen nmayor21_ch=sum((rela_j>0 & rela_j<=9) & (edad_ci>=21)), by (idh)
label variable nmayor21_ch "Numero de personas de 21 años o mas dentro del Hogar"

**************
*nmmenor21_ch*
**************

egen nmenor21_ch=sum((rela_j>0 & rela_j<=9) & (edad_ci<21)), by (idh)
label variable nmenor21_ch "Numero de personas menores a 21 años dentro del Hogar"

**************
*nmmayor65_ch*
**************

egen nmayor65_ch=sum((rela_j>0 & rela_j<=9) & (edad_ci>=65)), by (idh)
label variable nmayor65_ch "Numero de personas de 65 años o mas dentro del Hogar"

************
*nmenor6_ch*
************

egen nmenor6_ch=sum((rela_j>0 & rela_j<=9) & (edad_ci<6)), by (idh)
label variable nmenor6_ch "Numero de niños menores a 6 años dentro del Hogar"

************
*nmenor1_ch*
************
egen nmenor1_ch=sum((rela_j>0 & rela_j<=9) & (edad_ci<1)),  by (idh)
label variable nmenor1_ch "Numero de niños menores a 1 año dentro del Hogar"
/*
********
*emp_ci*
********
gen emp_ci=0
replace emp_ci=1 if ce01==1 | ce02==1 | ce03==1  /* revisar hacia atras*/
replace emp_ci=. if ce01==. & ce02==. & ce03==.
label var emp_ci "Empleado en la semana de referencia"
*/

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

**********
*ocupa_ci*
**********
tostring ce25, replace
replace ce25 = "0" + ce25 if length(ce25)==6
gen labor=substr(ce25,1,4)
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
gen rama_ci=ramaop
replace rama_ci=. if ramaop==10 | ramaop==11 | emp_ci==0

*************
*horaspri_ci*
*************
gen horaspri_ci=ce30
replace horaspri_ci=. if horaspri_ci>168

************
*horastot_ci
************
gen horassec_ci=ce77
replace horassec_ci=. if ce77>168

gen horastot_ci=horaspri_ci+horassec_ci
replace horastot_ci=horaspri_ci if horassec_ci==.
replace horastot_ci=horassec_ci if horaspri_ci==.

***********
*ylmpri_ci*
***********
gen ylmpri_ci=.
replace ylmpri_ci=ce42      if edad_ci>4 & ce41==1 & ce43==1
replace ylmpri_ci=ce42*ce43 if edad_ci>4 & ((ce41==2 & (ce43==1 | ce43==2)) | (ce41==3 & ce43>=1 & ce43<=4) | (ce41==4 & ce43>=1 & ce43<=31))
replace ylmpri_ci=ce69      if edad_ci>4 & ce69>=0 & ce69~=.
replace ylmpri_ci=0         if (ce42==0 | ce69==0)     & edad_ci>4 & (emp_ci==1)
replace ylmpri_ci=0         if ( ce31==12 |  ce31==13) & edad_ci>4 & (emp_ci==1)
replace ylmpri_ci=ce20      if (ce20>0 & emp_ci==0) /*incremento el ingreso mensual de los desocupados)*/
replace ylmpri_ci=ce22      if (ce22>0 & emp_ci==0) 
label var ylmpri_ci "Ingreso Laboral Monetario de la Actividad Principal"

**************
*categopri_ci*
**************
/*2010
ce31:
	1	1. empleado(a) u obrero(a) público
	2	2. empleado(a) u obrero(a) privado
	3	3. empleado(a) doméstico(a)
	4	4. miembro de cooperativa de producción
	5	5. cuenta propia que no contrata mano de obra temporal
	6	6. cuenta propia que contrata mano de obra temporal
	7	7. empleador o socio activo
	8	8. miembro de cooperativa
	9	9. cuenta propia que no contrata mano de obra temporal
	10	10. cuenta propia que contrata mano de obra temporal
	11	11. patrón o socio de la finca
	12	12. trabajador familiar no remunerado
	13	13. trabajador no remunerado
Atención: Se excluye de la categorización a los miembros de cooperativas de producción. 

Nota: los miembros de coop deberian ser cuenta propia. No es muy alto en 2010. Solo 20 casos.
*/
gen categopri_ci=.
replace categopri_ci=1 if ce31==10 | ce31==11 | ce31==6 | ce31==7
replace categopri_ci=2 if ce31==5  | ce31==9
replace categopri_ci=3 if ce31==1  | ce31==2  | ce31==3
replace categopri_ci=4 if ce31==12 | ce31==13  
label var categopri_ci "Categoria ocupacional actividad principal"
label define categopri_ci 1 "Patron" 2 "Cuenta Propia" 3 "Empleado" 4 "Trabajador no remunerado"
label value categopri_ci categopri_ci


************
*ylnmpri_ci*
************
gen yalim2    =   ce45	if ce45>=0 
gen yropa2    =   ce47	if ce47>=0
gen yhabita2  =   ce49	if ce49>=0
gen ytrans2   =   ce51	if ce51>=0
egen yotro2   =  rsum(ce53 ce55 ce57 ce59 ce61) 
replace yotro2=. if yotro2==0 & ce53==.
gen yprodu2   =  ce70 if  ce70>=0 

egen ylnmpri_ci=rsum(yalim2 yropa2 yhabita2 ytrans2 yotro2 yprodu2)
replace ylnmpri_ci=. if yalim2==. & yropa2==. & yhabita2==. & ytrans2==. & yotro2==. & yprodu2==.
replace ylnmpri_ci=0 if ((ce45==. & ce47==. & ce49==. & ce51==. &  ce53==. & ce55==. & ce57==. & ce61==. & categopri==3) | (ce70==. & (categopri==1 | categopri==2))) & edad_ci>4 & (emp_ci==1)
label var ylnmpri_ci "Ingreso Laboral No Monetario de la Actividad Principal"

***********
*ylmsec_ci*
***********

gen ylmsec_ci=.
replace ylmsec_ci=ce90 if ce90<99999 & edad_ci>4 & ce89==1 & ce91==1
replace ylmsec_ci=ce90*ce91 if edad_ci>4 & ((ce89==2 & (ce91==1 | ce91==2)) | (ce89==3 & ce91>=1 & ce91<=4) | (ce89==4 & ce91>=1 & ce91<=31))
replace ylmsec_ci= ce117 if  edad_ci>4 &  ce117>=0
replace ylmsec_ci=0 if ce90==0 &  ce117==0 & edad_ci>4 & ce72==1
replace ylmsec_ci=. if ylmsec_ci==99999
label var ylmsec_ci "Ingreso Laboral Monetario de la Actividad Secundaria"

****************
* categosec_ci *
****************
gen categosec_ci=.
replace categosec_ci=1 if (ce79==6 | ce79==7 | ce79==10 | ce79==11)
replace categosec_ci=2 if (ce79==4 | ce79==5 | ce79==8 | ce79==9)
replace categosec_ci=3 if (ce79>=1 & ce79<=3)
replace categosec_ci=4 if (ce79==12| ce79==13)
label var categosec_ci "Categoria ocupacional actividad secundaria"
label define categosec_ci 1 "Patron" 2 "Cuenta Propia" 3 "Empleado" 4 "Trabajador no remunerado"
label value categosec_ci categosec_ci
*Nota: modifico, revisar hacia atras (sigo la def d cetegpri_ci). Considero tamb a los miembros de cooperativo como cta propia.

************
*ylnmsec_ci*
************
gen yalim3    =   ce93	if (ce93>=0 & ce93!=.)
gen yropa3    =   ce95	if (ce95>=0 & ce95!=.)
gen yhabita3  =   ce97	if (ce97>=0 & ce97!=.)
gen ytrans3   =   ce99	if (ce99>=0 & ce99!=.)
egen yotro3   =  rsum(ce101 ce103 ce105 ce107 ce109) 
replace yotro3=. if yotro3==0 & ce101==.
gen yprodu3   = ce118 if ce118>=0 

egen ylnmsec_ci=rsum(yalim3 yropa3 yhabita3 ytrans3 yotro3 yprodu3)
replace ylnmsec_ci=. if yalim3==. & yropa3==. & yhabita3==. & ytrans3==. & yotro3==. & yprodu3==.
replace ylnmsec_ci=0 if ((ce93==. & ce95==. & ce97==. & ce99==. & ///
                          ce101==. & ce103==. & ce105==. & ce109==. & ///
                          categosec==3) | (ce118==. & (categosec==1 | categosec==2))) ///
                          & edad_ci>4 & ce72==1
label var ylnmsec_ci "Ingreso Laboral No Monetario de la Actividad Secundaria"

*************
*ylmotros_ci*
*************
gen ylmotros_ci=.
label var ylmotros_ci "Ingreso laboral monetario de otros trabajos"

**************
*ylnmotros_ci*
**************
gen ylnmotros_ci=.
label var ylnmotros_ci "Ingreso laboral NO monetario de otros trabajos" 

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
replace autoconsumop_ci=0 if ce118==. & edad_ci>4 & (categopri==1 | categopri==2) & (emp_ci==1)
label var autoconsumop_ci "Autoconsumo Individual Actividad Principal(Trabajadores Independientes)"

gen autoconsumos_ci=yprodu3 
replace autoconsumos_ci=0 if  ce118==. & edad_ci>4 & (categosec==1 | categosec==2) &  ce72==1
label var autoconsumos_ci "Autoconsumo Individual Actividad Secundaria(Trabajadores Independientes)"

egen autocons_ci=rsum(autoconsumop_ci autoconsumos_ci)
replace autocons_ci=. if autoconsumop_ci==. & autoconsumos_ci==.
label var autocons_ci "Autoconsumo Individual (Trabajadores Independientes)"

****************
*autoconsumo_ch*
****************
egen autocons_ch=sum(autocons_ci) if miembros_ci==1, by(idh_ch)
label var autocons_ch "Autoconsumo del Hogar"

************
*remesas_ci*
************
egen remesas_ci=rsum(remesaext_esp remesaext_efec)
replace remesas_ci=. if remesaext_esp==. & remesaext_efec==.

************
*remesas_ch*
************
egen remesas_ch=sum(remesas_ci)  if miembros_ci==1, by(idh_ch)

************
*durades_ci*
************
gen durades_ci=.
replace durades_ci=ce11/(365/12)      if ce11tiempo==1
replace durades_ci=ce11/((365/7)/12)  if ce11tiempo==2
replace durades_ci=ce11               if ce11tiempo==3
label var durades "Duracion del Desempleo (en meses)"

***************
*antiguedad_ci*
***************
generat antiguedad_ci=.
replace antiguedad_ci=ce33/(365/12)     if ce33_tiempo==1 /* modifico esta var, revisar hacia atras*/
replace antiguedad_ci=ce33/((365/7)/12) if ce33_tiempo==2
replace antiguedad_ci=ce33              if ce33_tiempo==3
replace antiguedad_ci=ce33*12           if ce33_tiempo==4
replace antiguedad_ci=.                 if ce33==99
label var antiguedad "Antiguedad en la Ocupacion Actual (en anios)"
/*
************
*desemp1_ci*
************
gen desemp1_ci=.
replace desemp1_ci=1 if ce01==2 & ce02==2 & ce03==2 & ce05==1
replace desemp1_ci=0 if ce01==1 | ce02==1 | ce03==3 | ce05==2

************
*desemp2_ci*
************
gen desemp2_ci=desemp1_ci
replace desemp2_ci=1 if ce09==1 | ce09==2 | ce09==3

************
*desemp3_ci*
************
gen desemp3_ci=desemp2_ci
replace desemp3_ci=1 if ce06==1

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
replace desalent_ci=1 if ce09==6
replace desalent_ci=0 if ce09!=6 & ce09!=.

***********
*subemp_ci*
***********
gen subemp_ci=.
replace subemp=0 if emp_ci==0 | emp_ci==1
replace subemp=1 if horastot<30 & ce120==1
label var subemp "Trabajadores subempleados"

***************
*tiempoparc_ci*
***************
gen tiempoparc_ci=.
replace tiempoparc_ci=0 if emp_ci==0 | emp_ci==1
replace tiempoparc_ci=1 if horastot<30 & ce120==2
label var tiempoparc_ci "Trabajadores a medio tiempo"
/*
*************
*contrato_ci*
*************
gen contrato_ci=.
replace contrato_ci=1 if ce34==1 | ce82==1
replace contrato_ci=0 if (ce34==2) & (ce82==2 | ce82==.)
replace contrato_ci=0 if ce82==2 & ce34==.
label var contrato "Peronas empleadas que han firmado un contrato de trabajo"*/
/*
***********
*segsoc_ci*
***********
gen segsoc_ci=. 
/*la variable ce23 presenta categorias diferentes a la del cuesttionario?  */
*/
*************
*nempleos_ci*
*************
generat nempleos_ci=1 if ce72==2
replace nempleos_ci=2 if ce72==1

**************
*firmpapeq_ci*
**************
gen firmapeq_ci=0 if (ce32_cantidad>5 & ce32_cantidad<99999) | (ce63_cantidad>5 & ce63_cantidad<99999)
replace firmapeq_ci=1 if (ce32_cantidad<=5 & ce32_cantidad!=0) | (ce63_cantidad<=5 & ce63_cantidad!=0) 
*Modifico, revisar hacia atras!

*************
*spublico_ci*
*************
gen spublico_ci=1 if ce31==1 | ce79==1
replace spublico_ci=0 if ce31!=1 & ce79!=1

************************************************************************
****************************EDUCACION***********************************
************************************************************************

generat asiste_ci=.
replace asiste_ci=1 if ed03==1
replace asiste_ci=0 if ed03==2
label var asiste "Personas que actualmente asisten a centros de enseñanza"

ren ed04 pqnoasis_ci
label var pqnoasis_ci "Razones para no asistir a centros de enseñanza"

gen repiteult_ci=.
replace repiteult_ci=1 if ed13==1
replace repiteult_ci=0 if ed13==2
label var repiteult_ci "Personas que están repetiendo el ultimo grado"

gen repite_ci=.
label var repite_ci "Personas que han repetido al menos un año o grado"


	*************
	***aedu_ci***
	*************
	
gen aedu_ci=anosest
label var aedu_ci "Años de educacion aprobados"

replace ed10=7 if ed10==8
replace ed10=8 if ed10==9
replace ed10=9 if ed10==10
label value ed10 nivel_educativo_nuevo
label define nivel_educativo_nuevo 1"1. Ninguno" 2"2. Programa de alfabetizacion" 3"3. Pre-escolar"	///
4"4. Primaria" 5"5. Ciclo comun" 6"6. Diversificado" 7"7. Tecnico superior" 8"8. Superior universitaria" ///
9"9. Post-grado" 99"99. No sabe/no responde" 
label value ed05 nivel_educativo_nuevo

gen eduno_ci=.
replace eduno=1 if (ed05==1 & edad_ci>=5) | (ed10==3 & ed15==1)
replace eduno=0 if (ed05>3 & ed05<=9 & edad_ci>=5) | ((ed10>2 & ed10<9) | (ed10==3 & ed15>1))    
label var eduno_ci "1 = personas sin educacion (excluye preescolar)"

gen edupi_ci=.
replace edupi=1 if (ed05==4 & ed08<6 & ed08>=0) | (ed10==3 & ed15<7 & ed15>=1)
replace edupi=0 if ((ed05>=5 & ed05<=9) | (ed05==4 & ed08>=6)) | ((ed10>=4 & ed10<9) | (ed10==3 & ed15>=7)) | (eduno==1)
label var edupi_ci "1 = personas que no han completado el nivel primario"

gen edupc_ci=.
replace edupc=1 if (ed05==4 & ed08==6) 
replace edupc=0 if (edupi==1 | eduno==1) | (ed05==4 & ed08>6) | (ed10==3 & ed15>=7) | (ed05>4 & ed05<=9) | (ed10>3 & ed10<9)
replace edupi=1 if ed05==4 & (ed08==0 | ed08==.)
label var edupc_ci "1 = personas que han completado el nivel primario"

gen edusi_ci=.
replace edusi=1 if (ed05==4 & ed08>6 & ed08<.) | (ed10==3 & ed15>=7 & ed15<.) | (ed05==5 & ed08<3) | (ed05==6 & ed08<4) | (ed10==4 & ed15<=3) | (ed10==5 & ed15<=4)
replace edusi=0 if (edupi==1 | eduno==1 | edupc==1) | (ed05==5 & ed08>=3 & ed08<.) | (ed05==6 & ed08>=4 & ed08<.) | (ed05>=7 & ed05<=9) | (ed10>=6 & ed10<9) 
label var edusi_ci "1 = personas que no han completado el nivel secundario"

gen edusc_ci=.
replace edusc=1 if (ed05==5 & ed08>=3 & ed08<.) | (ed05==6 & ed08>=4 & ed08<.) 
replace edusc=0 if (edupi==1 | eduno==1 | edupc==1 | edusi==1) | (ed05>6 & ed05<=9) | ( ed10>5& ed10<9)
label var edusc_ci "1 = personas que han completado el nivel secundario"

gen eduui_ci=.
replace eduui=1 if (ed05==7 & ed08<3) | (ed05==8 & ed08<5) | (ed10==6 & ed15<3) | (ed10==7 & ed15<5)
replace eduui=0 if (edupi==1 | eduno==1 | edupc==1 | edusi==1 | edusc==1) | (ed05==7 & ed08>=3) | (ed05==8 & ed08>=5) | (ed10==6 & ed15>=3) | (ed10==7 & ed15>=5) | (ed05==9) | (ed10==8)
label var eduui_ci "1 = personas que no han completado el nivel universitario"

gen eduuc_ci=.
replace eduuc=1 if (ed05==7 & ed08>=3 & ed08<.) | (ed05==8 & ed08>=5 & ed08<.) | (ed10==6 & ed15>=3 & ed15<.) | (ed10==7 & ed15>=5 & ed15<.) | ed05==9 | ed10==8
replace eduuc=0 if (edupi==1 | eduno==1 | edupc==1 | edusi==1 | edusc==1 | eduui==1) 
label var eduuc_ci "1 = personas que han completado el nivel universitario"

replace edupi=1 if ed05==4 & ed08==.
replace edupc=0 if ed05==4 & ed08==.
replace edusi=1 if (ed05==5 | ed05==6) & ed08==.
replace edusc=0 if (ed05==5 | ed05==6) & ed08==.
replace eduui=1 if (ed05==7 | ed05==8) & ed08==.
replace eduuc=0 if (ed05==7 | ed05==8) & ed08==.

gen edus1i_ci=.
replace edus1i=0 if edusi==1 | edusc==1 
replace edus1i=1 if edusi==1 & (ed05==4 & (ed08==7 | ed08==8)) | (ed10==3 & (ed15==7| ed15==8| ed15==9))
label var edus1i_ci "1 = personas que no han completado el primer ciclo de la educacion secundaria"

gen edus1c_ci=.
replace edus1c=0 if edusi==1 | edusc==1 
replace edus1c=1 if edusi==1 & (ed05==4 & ed08==9)
label var edus1c_ci "1 = personas que han completado el primer ciclo de la educacion secundaria"

gen edus2i_ci=.
replace edus2i=0 if edusi==1 | edusc==1 
replace edus2i=1 if edusi==1 & (ed05==5 & ed08<3) | (ed05==6 & ed08<4) | (ed10==4) | (ed10==5)
label var edus2i_ci "1 = personas que no han completado el segundo ciclo de la educacion secundaria"

gen edus2c_ci=.
replace edus2c=0 if edusi==1 
replace edus2c=1 if edusc==1
label var edus2c_ci "1 = personas que han completado el segundo ciclo de la educacion secundaria"

gen edupre_ci=.
replace edupre=0 if eduno==1 | edupi==1 | edupc==1 | edusi==1 | edusc==1 | eduui==1 | eduuc==1
replace edupre=1 if ed05==3 | ed10==2
label var edupre_ci "Educacion preescolar"

gen eduac_ci=.
replace eduac=0 if eduui==1 | eduuc==1
replace eduac=1 if ed05==8 | ed10==7
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
replace edupub_ci=1 if (ed09==1|ed09==2|ed09==3|ed09==4|ed09==7|ed09==8|ed09==13)
replace edupub_ci=0 if (ed09==5|ed09==6|ed09==9|ed09==10|ed09==11|ed09==12)

replace edupub_ci=1 if (ed16==1|ed16==2|ed16==3|ed16==4|ed16==7|ed16==8|ed16==13)
replace edupub_ci=0 if (ed16==5|ed16==6|ed16==9|ed16==10|ed16==11|ed16==12)
label var edupub_ci "1 = personas que asisten a centros de enseñanza publicos"

************
*aguared_ch*
************
gen aguared_ch=.
replace aguared_ch=1 if dv05==1
replace aguared_ch=0 if dv05==2 

*************
*aguadist_ch*
*************
gen aguadist_ch=.
replace aguadist_ch=1 if dv09==1
replace aguadist_ch=2 if (dv09==2 | dv09==3)
replace aguadist_ch=3 if dv09==4

*************
*aguamala_ch*
*************
gen aguamala_ch=.
replace aguamala_ch=1 if dv06>=5 & dv06<=8
replace aguamala_ch=0 if dv06>=1 & dv06<=4

*************
*aguamide_ch*
*************
gen aguamide_ch=.

********
*luz_ch*
********
gen luz_ch=1 if dv10==1 |dv10==2 |dv10==3 
replace luz_ch=0 if dv10>=4 & dv10<=8

************
*luzmide_ch*
************
gen luzmide_ch=.

************
*combust_ch*
************
gen combust_ch=1 if dh04==3 | dh04==2 | dh04==4
replace combust_ch=0 if dh04==5 | dh04==1

*********
*bano_ch*
*********
gen bano_ch=.
replace bano_ch=1 if dh05==1
replace bano_ch=0 if dh05==2

gen banoex_ch=.
replace banoex_ch=1 if dh07==1
replace banoex_ch=0 if dh07==2

gen des1_ch=.
replace des1_ch=0 if dh05==2
replace des1_ch=1 if (dh06==1|dh06==2)
replace des1_ch=2 if (dh06==5|dh06==6|dh06==7)
replace des1_ch=3 if (dh06==3|dh06==4)
label var des1_ch "Tipo de desague según unimproved de MDG"
label def des1_ch 0"No tiene servicio sanitario" 1"Conectado a red general o cámara séptica"
label def des1_ch 2"Letrina o conectado a pozo ciego" 3"Desemboca en río o calle", add
label val des1_ch des1_ch
	
gen des2_ch=.
replace des2_ch=1 if (dh06==1|dh06==2|dh06==3)
replace des2_ch=2 if (dh06==4|dh06==5|dh06==6|dh06==7)
replace des2_ch=0 if dh05==2
label var des2_ch "Tipo de desague sin incluir definición MDG"
label def des2_ch 0"No tiene servicio sanitario" 1"Conectado a red general, cámara séptica, pozo o letrina"
label def des2_ch 2"Cualquier otro caso", add
label val des2_ch des2_ch
	
gen piso_ch=.
replace piso_ch=0 if dv03==7
replace piso_ch=1 if dv03>=1 & dv03<=6 
replace piso_ch=2 if dv03==8 

gen techo_ch=.
replace techo_ch=0 if dv04==6 | dv04==7
replace techo_ch=1 if dv04>=1 & dv04<=5
replace techo_ch=2 if dv04==8| dv04==9 | dv04==10

gen pared_ch=(dv02>=1 & dv02<=4)
replace pared_ch=. 

gen resid_ch=.
replace resid_ch=0 if (dv11==1|dv11==3)
replace resid_ch=1 if (dv11==4|dv11==6)
replace resid_ch=2 if (dv11==2|dv11==7)
replace resid_ch=3 if (dv11==5|dv11==8)

gen dorm_ch=.
replace dorm_ch=dh01 if dh01>=0 

gen cuartos_ch=.
replace cuartos_ch=dv16 if dv16>=0 

***********
*cocina_ch*
***********
gen cocina_ch=(dh02==1)
replace cocina_ch=. if dh02==.

**********
*telef_ch*
**********

gen telef_ch=(dh08_7==1 | dh08_8==1)

***********
*regrig_ch*
***********
gen refrig_ch=(dh08_1==1)

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
gen compu_ch=(dh08_14==1)

*************
*internet_ch*
*************
gen internet_ch=(at04==1)
replace internet_ch=. if at04==. | at04==9

********
*cel_ch*
********
gen cel_ch=(at08==1)
replace cel_ch=. if at08==.

**********
*vivi1_ch*
**********
gen vivi1_ch=.
replace vivi1_ch=1 if dv01==1 | dv01==2
replace vivi1_ch=2 if dv01==4
replace vivi1_ch=3 if (dv01>=5 & dv01<=7) | dv01==3 
label var vivi1_ch "Tipo de vivienda en la que reside el hogar"
label def vivi1_ch 1"Casa" 2"Departamento" 3"Otros"
label val vivi1_ch vivi1_ch

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
replace viviprop_ch=0 if dv14==1
replace viviprop_ch=1 if dv14==3
replace viviprop_ch=2 if dv14==2
replace viviprop_ch=3 if (dv14==4 | dv14==5 | dv14==6 | dv14==7)
label var viviprop_ch "Propiedad de la vivienda"
label def viviprop_ch 0"Alquilada" 1"Propia y totalmente pagada" 2"Propia y en proceso de pago"
label def viviprop_ch 3"Ocupada (propia de facto)", add
label val viviprop_ch viviprop_ch
	
gen vivitit_ch=.
replace vivitit_ch=1 if dv17==1
replace vivitit_ch=0 if dv17==2

gen vivialq_ch=.
replace vivialq_ch=dv15       if dv15moneda==1 /*solo hay lempiras*/
replace vivialq_ch=dv15/19.92 if dv15moneda==2

*******************
***vivialqimp_ch***
*******************
gen vivialqimp_ch=.
label var vivialqimp_ch "Alquiler mensual imputado"
	
* correccion
tostring ce23 ,replace

gen aux_a=substr(ce23,1,1) if ce23!="9999999999" & ce23!="."
gen aux_b=substr(ce23,2,1) if ce23!="9999999999" & ce23!="."
gen aux_c=substr(ce23,3,1) if ce23!="9999999999" & ce23!="."
gen aux_d=substr(ce23,4,1) if ce23!="9999999999" & ce23!="."
gen aux_e=substr(ce23,5,1) if ce23!="9999999999" & ce23!="."
gen aux_f=substr(ce23,6,1) if ce23!="9999999999" & ce23!="."
gen aux_g=substr(ce23,7,1) if ce23!="9999999999" & ce23!="."
destring aux_*, replace
	
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
* HON 2010
gen salmm_ci= 	4949.59
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
replace cotizando_ci=1 if (aux_a>=1 & aux_a<=6) |  (aux_b>=1 & aux_b<=6)| (aux_c>=1 & aux_c<=6)| (aux_d>=1 & aux_d<=6) | (aux_e>=1 & aux_e<=6) | (aux_f>=1 & aux_f<=6) | (aux_g>=1 & aux_g<=6)
recode cotizando_ci .=0 if condact>=1 & condact<=2
label var cotizando_ci "Cotizante a la Seguridad Social"
label define cotizando_ci 0"No cotiza" 1"Cotiza a la SS" 
label value cotizando_ci cotizando_ci

*****************
*tipocontrato_ci*
*****************

recode ce82 (1=1) (3=3) (nonmissing=.), gen(tipocontrato_ci)
label var tipocontrato_ci "Tipo de contrato segun su duracion"
label define tipocontrato_ci 1 "Permanente/indefinido" 2 "Temporal" 3 "Sin contrato/verbal" 
label value tipocontrato_ci tipocontrato_ci

*************
*cesante_ci* 
*************

gen cesante_ci=ce12 if ce12==1 & condocup_ci==2
replace cesante_ci=. if ce12==9
label var cesante_ci "Desocupado -definicion oficial del pais- que ha trabajado antes"	


*************
*tamemp_ci
*************

gen tamemp_ci=ce32_cantidad
replace tamemp_ci=. if ce32_cantidad ==99999 /*ce32_cantidad ==99999 missing*/
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

gen pension_ci=1 if ypen_ci!=0 & ypen_ci!=.
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
replace tecnica_ci=1 if ed05==7
recode tecnica_ci .=0 
label var tecnica_ci "=1 formacion terciaria tecnica"

*********
*lp25_ci
*********
gen lp25_ci = 1020.701

label var lp25_ci "Linea de pobreza de uds1.25 por dia en moneda local"

*********
*lp4_ci*
*********
gen lp4_ci =1633.122

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

