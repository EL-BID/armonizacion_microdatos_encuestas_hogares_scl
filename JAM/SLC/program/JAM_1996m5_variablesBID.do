*Mayra Sáenz - Septiembre 2015
*Esta base no contiene todas las variables armonizadas, se debe armonizar el resto de variables.


clear
set more off
set virtual on
set memory 300m
set matsize 800

***********************************************************************************************************
*****                                      JAMAICA 1996                                               *****
*****                            LFS 2002 (LABOUR FORCE SURVEY)                                       *****
*****                                         ABRIL                                                   *****
***********************************************************************************************************
use "${surveysFolder}\survey\JAM\SLC\1996\m5\data_merge\JAM_1996m5.dta"

gen str pais_c="JAM"

gen anio_c=1996

gen mes_c=5 
label var mes_c "Mes de la Encuesta: Mayo de 2002"
label define mes_c 5 "MAY"
label values mes_c mes_c

/** VARIABLE AREA 
1 = Kingston
2 = Other towns
3 = Rural areas
4 = Portmore (se incluye en Kingston Metropolitan Area)
5 = Spanish town (se incluye en Kingston Metropolitan Area)
*/

gen zona_c=.
replace zona_c=1 if area==1 | area==2 | area==4 | area==5
replace zona_c=0 if area==3
label define zona_c 0 "Rural" 1 "Urbana"
label value zona_c zona_c
g region_c =.

egen idh_ch=group(serial)
label var idh_ch "Identificador Unico del Hogar"
gen idp_ci=ind
label var idp_ci "Identificador Individual dentro del Hogar"


gen relacion_ci=.
replace relacion_ci=1 if relation==1
replace relacion_ci=2 if relation==2
replace relacion_ci=3 if relation==3
replace relacion_ci=4 if relation>=4 & relation<=7 /* Otros familiares */
replace relacion_ci=5 if relation==9
replace relacion_ci=6 if relation==8 /*Es el sevicio domestico, Incluye a familiares del Serv. Domestico*/

label var relacion_ci "Relacion con el Jefe de Hogar"
label define relacion_ci 1 "Jefe de Hogar" 2 "Conyuge/Pareja" 3 "Hijo(a)/Hijastro(a)" 4 "Otros Parientes" 5 "Otros No parientes" 6 "Servicio Domestico"
label value relacion_ci relacion_ci

*Jamaica no necesita factor de expansión.
g factor_ch =1
label var factor_ch "Factor de expasion"

g factor_ci =1
label var factor_ci "Factor de expasion"

gen sexo_ci=1 if sex==1
replace sexo_ci=2 if sex==2
label var sexo_ci "Sexo del Individuo"
label define sexo_ci 1 "Masculino" 2 "Femenino"
label value sexo_ci sexo_ci

** Generating Edad
gen edad_ci=age
label var edad_ci "Edad del Individuo"

gen byte civil_ci=.
replace civil_ci=1 if  marital==2
replace civil_ci=2 if  marital==1
replace civil_ci=3 if  marital==3 | marital==4
replace civil_ci=4 if  marital==5
label var civil_ci "Estado Civil"
label define civil_ci 1 "Soltero" 2 "Union Formal o Informal" 3 "Divorciado o Separado" 4 "Viudo"
label value civil_ci civil_ci

gen jefe_ci=(relation==1)
label var jefe_ci "Jefe de Hogar Declarado"

sort idh

egen byte nconyuges_ch=sum(relacion_ci==2), by (idh)
label variable nconyuges_ch "Numero de Conyuges"

egen byte nhijos_ch=sum(relacion_ci==3), by (idh)
label variable nhijos_ch "Numero de Hijos"

egen byte notropari_ch=sum(relacion_ci==4),by (idh)
label variable notropari_ch "Numero de Otros Parientes "

egen byte notronopari_ch=sum(relacion_ci==5), by (idh)
label variable notronopari_ch "Numero de Otros NO Parientes "

egen byte nempdom_ch=sum(relacion_ci==6), by (idh)
label variable nempdom_ch "Numero de Empleados Domesticos"

gen clasehog_ch=.
replace clasehog_ch=1 if nhijos_ch==0 & nconyuges_ch==0 & notropari_ch==0 & notronopari_ch==0 /* unipersonal*/
replace clasehog_ch=2 if nhijos_ch>0 & notropari_ch==0 & notronopari_ch==0 /* nuclear (child with or without spouse but without other relatives)*/
replace clasehog_ch=2 if nconyuges_ch>0 & notropari_ch==0 & notronopari_ch==0 /* nuclear (spouse with or without children but without other relatives)*/
replace clasehog_ch=3 if notropari_ch>0 & notronopari_ch==0 /* ampliado*/
replace clasehog_ch=4 if (nconyuges_ch>0 | nhijos_ch>0 | (notropari_ch>0 & notropari_ch<.)) & (notronopari_ch>0 & notronopari_ch<.) /* compuesto  (some relatives plus non relative)*/
replace clasehog_ch=5 if nhijos_ch==0 & nconyuges_ch==0 & notropari_ch==0 & notronopari_ch>0 & notronopari_ch<./** corresidente*/

label variable clasehog_ch "CLASE HOGAR"
label define clasehog_ch 1 "Unipersonal" 2 "Nuclear" 3 "Ampliado" 4 "Compuesto" 5 "Corresidente"
label value clasehog_ch clasehog_ch

sort idh idp
egen nmiembros_ch=sum(relacion_ci>0 & relacion_ci<=5), by (idh)
replace nmiembros_ch=. if relacion_ci==.
label variable nmiembros_ch "Numero de miembros del Hogar"

gen miembros_ci=.
replace miembros_ci=1 if relacion_ci>=1 & relacion_ci<=5
replace miembros_ci=0 if relacion_ci==6 /*Empleados domesticos y sus familiares */
label variable miembros_ci "Variable dummy que indica las personas que son miembros del Hogar"

egen nmayor21_ch=sum((relacion_ci>0 & relacion_ci<=5) & (edad_ci>=21)), by (idh)
label variable nmayor21_ch "Numero de personas de 21 años o mas dentro del Hogar"

egen nmenor21_ch=sum((relacion_ci>0 & relacion_ci<=5) & (edad_ci<21)), by (idh)
label variable nmenor21_ch "Numero de personas menores a 21 años dentro del Hogar"

egen nmayor65_ch=sum((relacion_ci>0 & relacion_ci<=5) & (edad_ci>=65)), by (idh)
label variable nmayor65_ch "Numero de personas de 65 años o mas dentro del Hogar"

egen nmenor6_ch=sum((relacion_ci>0 & relacion_ci<=5) & (edad_ci<6)), by (idh)
label variable nmenor6_ch "Numero de niños menores a 6 años dentro del Hogar"

egen nmenor1_ch=sum((relacion_ci>0 & relacion_ci<=5) & (edad_ci<1)),  by (idh)
label variable nmenor1_ch "Numero de niños menores a 1 año dentro del Hogar"


saveold "${surveysFolder}\harmonized\JAM\SLC\data_arm\JAM1996EA_BID_incompleta.dta", replace
