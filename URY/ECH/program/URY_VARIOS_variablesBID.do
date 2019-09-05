clear
capture log close
set mem 400m
local base1992 "\\\resb636-a\database\ARM\URU\ECH\1992\Orig_data\ury92.dta"
local base1995 "\\\resb636-a\database\ARM\URU\ECH\1995\Orig_data\ury95.dta"
local base1996 "\\\resb636-a\database\ARM\URU\ECH\1996\Orig_data\ury96.dta"
local base1997 "\\\resb636-a\database\ARM\URU\ECH\1997\Orig_data\ury97.dta"
local base1998 "\\\resb636-a\database\ARM\URU\ECH\1998\Orig_data\ury98.dta"


quietly{
local bases "1992 1995 1996 1997 1998"
foreach i of local bases{
use `base`i'', clear
/************************************
 Del '92 al '98 es la misma encuesta. 
2000 es otra y2001-2003 otra.
*************************************/
/********************************/
/*    VARIABLES DEL HOGAR	*/
/********************************/

if `i'==1992 {
rename zona zonaorig /*En la encuesta original aparece la variable zona, pero no detalla que significa. Para que no se
			     superponga le cambio el nombre*/
} /*Ojo! Los identificadores del '92 no sirven para hacer panel. Se supone que a partir del '95 si*/
gen idh_ch=id_hogar
gen idp_ci=norden
gen factor_ch=factorex
gen zona_c=1 /*La encuesta es solo urbana!*/
gen str3 pais_c="URY"
gen anio_c=`i'
gen mes_c=0
forvalues j=1(1)12{
local h=`j'*4
local m=`h'-3
replace mes_c=`j' if semana>=`m'& semana<=`h'
}
gen relacion_ci=parentco
replace relacion_ci=4 if parentco==5
replace relacion_ci=5 if parentco==6
replace relacion_ci=6 if parentco==7
label define relacion_ci 1 "Jefe" 2 "Conyuge" 3 "Hijo" 4 "Otros Parientes" 5 "Otros no Parientes" 6 "Servicio Domestico"
label values relacion_ci relacion_ci

/************************************************************************/
/*			VARIABLES DE INFRAESTRUCTURA DEL HOGAR		*/
/************************************************************************/	
gen aguared_ch=(agua==1)
gen aguadist_ch=insagua
replace aguadist_ch=0 if insagua==4
gen aguamala_ch=(agua==4) /*Cachimba=ojo de agua*/	
gen aguamide_ch=.
gen luz_ch=(luz==1 | luz==2)
gen luzmide_ch=.
gen combust_ch=(energia==1 | energia==2 | energia==3)
gen bano_ch=(servsan!=3)
gen banoex_ch=(usoserv==1)
replace banoex_ch=. if bano_ch==0
gen des1_ch=evacuac
gen des2_ch=des1_ch
replace des2_ch=0 if des2_ch==3
gen piso_ch=.
gen pared_ch=.
gen techo_ch=.
gen resid_ch=.
gen dorm_ch=nrodorm
replace dorm_ch=. if nrodorm==9
gen cuartos_ch=tothabit
replace cuartos_ch=. if tothabit==99
gen cocina_ch=.
gen refrig_ch=(refri==1)
gen freezer_ch=(refrizer==1)
gen auto_ch=(auto==1)
gen telef_ch=.
gen compu_ch=.
gen internet_ch=.
gen cel_ch=.
gen viv1_ch=.
gen viv2_ch=(tipoviv==1)
gen vivprop_ch=0 if tenviv==3
replace vivprop_ch=1 if tenviv==1
replace vivprop_ch=2 if tenviv==2
replace vivprop_ch=3 if tenviv==4 | tenviv==5
replace vivprop_ch=4 if tenviv==6
gen vivialq_ch=ealq
sort idh_ch
by idh_ch: egen vivialqimp_ch=max(ylocali)

/************************************************************************/
/*				VARIABLES DEMOGRAFICAS			*/
/************************************************************************/
gen factor_ci=factorex
gen sexo_ci=sexo
gen edad_ci=edad
replace edad_ci=. if edad==99
gen civil_ci=1 if estcivil==5
replace civil_ci=2 if estcivil==1 | estcivil==2
replace civil_ci=3 if estcivil==3
replace civil_ci=4 if estcivil==4
gen jefe_ci=(parentco==1)
sort idh_ch
by idh_ch: egen byte nconyuges_ch=sum(relacion_ci==2) 
by idh_ch: egen byte nhijos_ch=sum((relacion_ci==3) & edad_ci<18)
by idh_ch: egen byte notropari_ch=sum((relacion_ci==4) & edad_ci>=18)
by idh_ch: egen byte notronopari_ch=sum(relacion_ci==5)
by idh_ch: egen byte nempdom_ch=sum(relacion_ci==6)
gen byte clasehog_ch=0
replace clasehog_ch=1 if nhijos_ch==0 & nconyuges_ch==0 & notropari_ch==0 & notronopari_ch==0 /*Unipersonal*/
replace clasehog_ch=2 if nhijos_ch>0 & notropari_ch==0 & notronopari_ch==0 /*Nuclear (child with or without spouse but without other relatives)*/
replace clasehog_ch=2 if nhijos_ch==0 & nconyuges_ch>0 & notropari_ch==0 & notronopari_ch==0 /*Nuclear (spouse with or without children but without other relatives)*/
replace clasehog_ch=3 if notropari_ch>0 & notronopari_ch==0 /*Ampliado*/
replace clasehog_ch=4 if ((nconyuges_ch>0 | nhijos_ch>0 | notropari_ch>0) & (notronopari_ch>0))/*Compuesto (some relatives plus non relative)*/
replace clasehog_ch=5 if nhijos_ch==0 & nconyuges_ch==0 & notropari_ch==0 & notronopari_ch>0 /*Corresidente*/
sort idh_ch
by idh_ch:egen byte nmiembros_ch=sum(relacion_ci>0 & relacion_ci<=5) if relacion_ci~=6
by idh_ch:egen byte nmayor21_ch=sum((relacion_ci>0 & relacion_ci<=5) & (edad_ci>=21 & edad_ci<=98))
by idh_ch:egen byte nmenor21_ch=sum((relacion_ci>0 & relacion_ci<=5) & (edad_ci<21))
by idh_ch:egen byte nmayor65_ch=sum((relacion_ci>0 & relacion_ci<=5) & (edad_ci>=65))
by idh_ch:egen byte nmenor6_ch=sum((relacion_ci>0 & relacion_ci<=5) & (edad_ci<6))
by idh_ch:egen byte nmenor1_ch=sum((relacion_ci>0 & relacion_ci<=5) & (edad_ci<1))

/******************************************************************************/
/*				VARIABLES DE DEMANDA LABORAL		      */
/******************************************************************************/
gen emp_ci=(pobpcoac==11|pobpcoac==12)

gen ocupa_ci=.
replace ocupa_ci=1 if ocup>=0 & ocup<=98 & emp_ci==1
replace ocupa_ci=2 if ocup>=100 & ocup<=186 & emp_ci==1
replace ocupa_ci=3 if ocup>=200 & ocup<=290 & emp_ci==1
replace ocupa_ci=4 if ocup>=300 & ocup<=361 & emp_ci==1
replace ocupa_ci=5 if ocup>=900 & ocup<=990 & emp_ci==1
replace ocupa_ci=6 if ocup>=400 & ocup<=453 & emp_ci==1
replace ocupa_ci=7 if ocup>=500 & ocup<=792 & emp_ci==1 /*Incluye artesanos y operarios en hilanderias*/
replace ocupa_ci=9 if ocup>=800 & ocup<=890 & emp_ci==1
/*No hay una categoria para las fuerzas armadas*/
gen ramaaux=substr(string(ramactv),1,1)
gen rama_ci=real(ramaaux)
drop ramaaux
replace rama_ci=. if rama_ci==0
gen horaspri_ci=hrsing
replace horaspri_ci=. if hrsing==99
replace horaspri_ci=. if emp_ci==0
gen horastot_ci=tothrs
replace horastot_ci=. if tothrs==99
replace horastot_ci=. if horaspri_ci==.


/****************************************YLMPRI_CI**********************************************/
egen ylmpri_ci=rsum(ypsyspr ypextpr ypbenpr ypagupr ypvacpr yppropr) if catego==1 & emp_ci==1
replace ylmpri_ci=. if ypsyspr==. & ypextpr==. & ypbenpr==. & ypagupr==. & ypvacpr==. & yppropr==. & catego==1  
egen ylmpri_ci2=rsum(ypsyspu ypextpu ypbenpu ypagupu ypvacpu yppropu) 
replace ylmpri_ci2=. if ypsyspu==. & ypextpu==. & ypbenpu==. & ypagupu==. & ypvacpu==. & yppropu==. 
replace ylmpri_ci=ylmpri_ci2 if catego==2 & emp_ci==1

gen ypcoputfmo=ypcoputf/12 if emp_ci==1/*ypcoputf esta definido en una base anual!*/
egen ylmpri_ci3=rsum(ypcopef ypcoputfmo) 
replace ylmpri_ci3=. if ypcopef==. & ypcoputfmo==. 
replace ylmpri_ci=ylmpri_ci3 if catego==3 & emp_ci==1

gen yputiefmo=yputief/12 if emp_ci==1/*yputief esta definido en una base anual!*/
egen ylmpri_ci4=rsum(yppatef yputiefmo) 
replace ylmpri_ci4=. if yppatef==. & yputiefmo==. 
replace ylmpri_ci=ylmpri_ci4 if catego==4 & emp_ci==1

egen ylmpri_ci5=rsum(ypctasl ypbensl) 
replace ylmpri_ci5=. if ypctasl==. & ypbensl==. 
replace ylmpri_ci=ylmpri_ci5 if catego==5 & emp_ci==1

egen ylmpri_ci6=rsum(ypctacl ypbencl) 
replace ylmpri_ci6=. if ypctacl==. & ypbencl==.
replace ylmpri_ci=ylmpri_ci6 if catego==6 & emp_ci==1

drop ylmpri_ci2 ylmpri_ci3 ylmpri_ci4 ylmpri_ci5 ylmpri_ci6


/******************************************YLMPRI1_CI**********************************************/
gen ylmpri1_ci=ylmpri_ci if emp_ci==1
egen ylmpri1_ci1=rsum(ypcopef ypcopben ypcoputfmo) 
replace ylmpri1_ci1=. if ypcopef==. & ypcopben==. & ypcoputfmo==. 
replace ylmpri1_ci=ylmpri1_ci1 if catego==3 & emp==1
drop ylmpri1_ci1

/*********************************************YLNMPRI_CI******************************************/
gen ypcoputsmo=ypcoputs/12 if emp_ci==1/*ypcoputs esta definido en una base anual!*/
egen ylnmpri_ci=rsum(ypcopes ypcoputsmo) if catego==3 & emp_ci==1
replace ylnmpri_ci=. if ypcopes==. & ypcoputsmo==. & catego==3
replace ylnmpri_ci=ypesppr if catego==1 & emp_ci==1
replace ylnmpri_ci=ypesppu if catego==2 & emp_ci==1

gen yputiesmo=yputies/12 if emp_ci==1/*yputies esta definido en una base anual!*/
egen ylnmpri_ci4=rsum(yppates yputiesmo) 
replace ylnmpri_ci4=. if yppates==. & yputiesmo==. 
replace ylnmpri_ci=ylnmpri_ci4 if catego==4 & emp==1

replace ylnmpri_ci=ypespsl if catego==5 & emp_ci==1
replace ylnmpri_ci=ypespcl if catego==6 & emp_ci==1

drop ylnmpri_ci4
/*******************************************************************************************************/

gen ylmsec_ci=.
gen ylnmsec_ci=.

gen ysutiefmo=ysutief/12 if emp_ci==1/*ysutief esta definido en una base anual!*/
gen yscoputfmo=yscoputf/12 if emp_ci==1/*yscoputf esta definido en una base anual!*/
egen ylmaux=rsum(yssyspr ysextpr ysbenpr ysagupr ysvacpr yspropr yssyspu ysextpu ysbenpu ysagupu ysvacpu yspropu ysctasl ysbensl ysctacl ysbencl yspatef ysutiefmo yscopef yscopben yscoputfmo) if emp_ci==1
replace ylmaux=. if yssyspr==. & ysextpr==. & ysbenpr==. & ysagupr==. & ysvacpr==. & yspropr==. & yssyspu==. & ysextpu==. & ysbenpu==. & ysagupu==. & ysvacpu==. & yspropu==. & ysctasl==. & ysbensl==. & ysctacl==. & ysbencl==. & yspatef==. & ysutiefmo==. & yscopef==. & yscopben==. & yscoputfmo==.

gen ysutiesmo=ysuties/12 if emp_ci==1/*ysuties esta definido en una base anual!*/
gen yscoputsmo=yscoputs/12 if emp_ci==1/*yscoputs esta definido en una base anual!*/
egen ylnmaux=rsum(ysesppr ysesppu ysespsl ysespcl yspates ysutiesmo yscopes yscoputsmo) if emp_ci==1
replace ylnmaux=. if ysesppr==. & ysesppu==. & ysespsl==. & ysespcl==. & yspates==. & ysutiesmo==. & yscopes==. & yscoputsmo==.

gen nrylmpri_ci=.

egen ylm_ci=rsum(ylmpri_ci ylmaux)
replace ylm_ci=. if ylmpri_ci==. & ylmaux==.

egen ylm1_ci=rsum(ylmpri1_ci ylmaux)
replace ylm1_ci=. if ylmpri1_ci==. & ylmaux==.

egen ylnm_ci=rsum(ylnmpri_ci ylnmaux)
replace ylnm_ci=. if ylnmpri_ci==. & ylnmaux==.
drop ylnmaux ylmaux

gen yintpamo=yintpa/12 /*yintpa esta definido en una base anual!*/
gen yintexmo=yintex/12 /*yintex esta definido en una base anual!*/
egen ynlm_ci=rsum(yjubpa ypenpa ybecpa yayupa yarrepa yintpamo yjubex ypenex ybecex yayuex yarrex yintexmo)
replace ynlm_ci=. if yjubpa==. & ypenpa==. & ybecpa==. & yayupa==. & yarrepa==. & yintpamo==. & yjubex==. & ypenex==. & ybecex==. & yayuex==. & yarrex==. & yintexmo==.
gen ynlnm_ci=.

sort idh_ch 
by idh_ch: egen nrylmpri_ch=max(nrylmpri_ci) if relacion_ci!=6
by idh_ch: egen ylm_ch=sum(ylm_ci)if relacion_ci!=6
by idh_ch: egen ylnm_ch=sum(ylnm_ci)if relacion_ci!=6
gen ylmnr_ch=. if nrylmpri_ch==1
by idh_ch: egen ynlm_ch=sum(ynlm_ci)if relacion_ci!=6
gen ynlnm_ch=.
gen ylmhopri_ci=ylmpri_ci/(horaspri_ci*4.2)
gen ylmhopri1_ci=ylmpri1_ci/(horaspri_ci*4.2)
replace ylmhopri_ci=. if ylmhopri_ci<=0
gen ylmho_ci=ylm_ci/(horastot_ci*4.2)
replace ylmho_ci=. if ylmho_ci<=0
gen rentaimp_ch=vivialqimp_ch
gen autocons_ch=.
gen autocons_ci=.

egen remesas_ci=rsum(yjubex ypenex ybecex yayuex yarrex yintexmo)
replace remesas_ci=. if yjubex==. & ypenex==. & ybecex==. & yayuex==. & yarrex==. & yintexmo==.

drop ypcoputfmo yputiefmo ypcoputsmo yputiesmo ysutiefmo yscoputfmo ysutiesmo yscoputsmo yintpamo yintexmo
sort idh_ch
by idh_ch: egen remesas_ch=sum(remesas_ci)if relacion_ci!=6
gen durades_ci=tpobus/4 if pobpcoac==21 | pobpcoac==23 /*De los que estan desempleados cuanto hace (en meses) que buscan*/
replace durades_ci=. if tpobus==99
gen antiguedad_ci=ctosanos
replace antiguedad_ci=. if ctosanos==99

/******************************************************************************************/
/*					VARIABLES DEL MERCADO LABORAL			  */
/******************************************************************************************/

gen desemp1_ci=(real(substr(string(pobpcoac),1,1))==2 & buscosp==1)
gen desemp2_ci=(desemp1_ci==1 | (buscosp==2 & (pqnobus==2 | pqnobus==3)))
gen desemp3_ci=(desemp2_ci==1 | (busult==1 | tpobus>=4))
gen pea1_ci=(emp_ci==1 | desemp1_ci==1)
gen pea2_ci=(emp_ci==1 | desemp2_ci==1)
gen pea3_ci=(emp_ci==1 | desemp3_ci==1)
gen desalent_ci=(pobpcoac==37)
gen subemp_ci=(horastot_ci>=1 & horastot_ci<=30 & deseamas==1)
replace subemp=. if emp_ci==0
gen tiempoparc_ci=(horastot_ci>=1 & horastot_ci<=30 & deseamas==2)
replace tiempoparc_ci=. if emp_ci==0
gen categopri_ci=1 if catego==4
replace categopri_ci=2 if catego==5 | catego==6
replace categopri_ci=3 if catego==1 | catego==2 | catego==3
replace categopri_ci=4 if catego==7 | catego==8
replace categopri_ci=. if emp_ci!=1
gen categosec_ci=.
gen contrato_ci=.
gen segsoc_ci=.
gen nempleos_ci= 1 if nrocup==1
replace nempleos_ci=2 if nrocup>1
gen firmapeq_ci=.
replace firmapeq_ci=0 if emp_ci==1 & trabmeno==1 & tamest>=1 & tamest<=5
replace firmapeq_ci=1 if emp_ci==1 & (trabmeno==1 & tamest>5)|trabmeno==2
gen spublico_ci=(emp_ci==1 & catego==2)

/******************************************************************************************/
/*						VARIABLES EDUCATIVAS			  */
/******************************************************************************************/
gen aedu_ci=0
replace aedu_ci=ultano if nivel==1
replace aedu_ci=6+ultano if nivel==2
replace aedu_ci=9+ultano if nivel==3 | nivel==4
replace aedu_ci=12+ultano if nivel>=5 & nivel<=7
replace aedu_ci=. if nivel==0 | ultano==0 | nivel>=7 /*Estamos droppeando a los del "Instituto Militar" porque son muy pocos (algo asi como el
0.2% de la muestra) y no esta claro en que categoría deberían entrar*/
/*U.T.U (nivel=4)>><< Enseñanza técnica. Aunque se llama Universidad Tecnica de Uruguay no esta contado como una carrera universitaria, sino que, dado
que solo pide como requisito el primer ciclo, es como una enseñanza secundaria de segundo ciclo que puede durar muchos años.*/
gen eduno_ci=(asist==2 & asistio==2)
gen edupi_ci=(nivel==1 & finalizo==2)
gen edupc_ci=(nivel==1 & finalizo==1)
gen edusi_ci=((nivel==3 | nivel==4) & finalizo==2)| nivel==2
gen edusc_ci=((nivel==3 | nivel==4) & finalizo==1)
gen eduui_ci=(nivel==5 | nivel==6) & finalizo==2
gen eduuc_ci=(nivel==5 | nivel==6) & finalizo==1
gen edus1i_ci=(nivel==2 & finalizo==2)
gen edus1c_ci=(nivel==2 & finalizo==1)
gen edus2i_ci=((nivel==3 | nivel==4) & finalizo==2)
gen edus2c_ci=((nivel==3 | nivel==4) & finalizo==1)
gen edupre_ci=(nivel==8)
gen eduac_ci=.
replace eduac_ci=1 if nivel==6 & (eduuc_ci==1 | eduui_ci==1)
replace eduac_ci=0 if (eduuc_ci==1 | eduui_ci==1) & nivel!=6
foreach var of varlist edu* {
replace `var'=. if aedu_ci==0 | aedu_ci==.| finalizo==0
}
gen asiste_ci=(asist==1)
gen pqnoasist_ci=.
gen repite_ci=.
gen edupub_ci=(tipoense==1)
label var  aedu_ci "Anios de Educacion"
label var  eduno_ci "Sin Educacion"
label var  edupi_ci "Primaria Incompleta"		
label var  edupc_ci "Primaria Completa"		
label var  edusi_ci "Secundaria Incompleta"		
label var  edusc_ci "Secundaria Completa"
label var  eduui_ci "Universitaria o Terciaria Incompleta"		
label var  eduuc_ci  "Universitaria o Terciaria Completa"
*********************************************************************************************
save "\\\resb636-a\database\ARM\URU\ECH\\`i'\Arm_data\URU`i'EA_BID.dta", replace
}
}



	
