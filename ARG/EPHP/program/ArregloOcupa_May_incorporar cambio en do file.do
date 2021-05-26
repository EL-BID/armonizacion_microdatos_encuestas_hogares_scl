clear
set mem 500m
local in1 "${surveysFolder}\ARM\ARG\EPHMay\1992\Arm_data\ARG1992EA_BID.dta"
local in2 "${surveysFolder}\ARM\ARG\EPHMay\1993\Arm_data\ARG1993EA_BID.dta"
local in3 "${surveysFolder}\ARM\ARG\EPHMay\1994\Arm_data\ARG1994EA_BID.dta"
local in4 "${surveysFolder}\ARM\ARG\EPHMay\1995\Arm_data\ARG1995EA_BID.dta"
local in5 "${surveysFolder}\ARM\ARG\EPHMay\1996\Arm_data\ARG1996EA_BID.dta"
local in6 "${surveysFolder}\ARM\ARG\EPHMay\1997\Arm_data\ARG1997EA_BID.dta"
local in7 "${surveysFolder}\ARM\ARG\EPHMay\1998\Arm_data\ARG1998EA_BID.dta"
local in8 "${surveysFolder}\ARM\ARG\EPHMay\1999\Arm_data\ARG1999EA_BID.dta"
local in9 "${surveysFolder}\ARM\ARG\EPHMay\2000\Arm_data\ARG2000EA_BID.dta"
local in10 "${surveysFolder}\ARM\ARG\EPHMay\2001\Arm_data\ARG2001EA_BID.dta"
local in11 "${surveysFolder}\ARM\ARG\EPHMay\2002\Arm_data\ARG2002EA_BID.dta"

/*
Evolucion del Numero de digitos de ocupa_ci por Aglomerado
----------------------------------------------------------

	1992	1993	1994	1995	1996	1997	1998
1	2	3	3	3	3	3	3
2		2	2		3	3	3
3				2	2	2	3
4	2	2	2	3	3	3	3
5		2	3	3	3	3	3
6	2	2	3	3	3	3	3
7					2	3	3
8			2	2	2	3	3
9	2	2	2	2	2	2	3
10	2	2	3	3	3	3	3
12			2	2	2	3	3
13	2	2	3	3	3	3	3
14					3	3	3
15					2	3	3
17	2	3	3	3	3		3
18			2	2	2	3	3
19	2	2	2	2	2	3	3
20		2	2	2	2	2	3
22		2		2	2	3	3
23	2	2	2	2	2	3	3
25			2		2	3	3
26	2	2	2	2	2	2	3
27	2		2	2	2	2	3
29	2	2	3	3	3	3	3
30		3	3	3	3	3	3
31	2	2	2	2	2	2	3
32						3	3
33						3	3
34					3	3	3
36					3	3	3
*/
/*
# delimit;
forvalues i=1(1)6{;
use `in`i'', clear;
capture drop ocupa_ci;
gen ocupa = .;
if `i'<4{;
capture destring p20, replace;
};
if `i'>=4{;
gen aux=real(p20);
drop p20;
rename aux p20;
};
if `i'==1 | (`i'==2 & ((aglomera>=2 & aglomera<=13) | (aglomera>=19 & aglomera<=29) | aglomera==31)) | 
(`i'==3 & (aglomera==2 | aglomera==4 | aglomera==8 | aglomera==9 | aglomera==12 | (aglomera>=18 & aglomera<=27) | 
aglomera==31)) | (`i'==4 & (aglomera==3 | aglomera==8 | aglomera==9 | aglomera==12 | (aglomera>=18 & aglomera<=27) | 
aglomera==31)) | (`i'==5 & (aglomera==3 | (aglomera>=7 & aglomera<=9) | aglomera==12 | aglomera==15 | 
(aglomera>=18 & aglomera<=27) | aglomera==31)) | (`i'==6 & (aglomera==3 | aglomera==9 | aglomera==20 | aglomera==26 | 
aglomera==27 | aglomera==31)){;
local ocup = "ocupa";
local var = "p20";
replace `ocup' = 1 if `var' ==14 | `var' ==13 | `var' ==23 | `var' ==43 | `var' ==44 | `var' ==45 | `var' ==46 | `var' ==94
| `var' ==41 | `var' ==21 | `var' ==11;								
replace `ocup' = 2 if `var' ==01 | `var' ==02 | `var' ==03 | `var' ==04 | `var' ==05;		
replace `ocup' = 3 if `var' ==12 | `var' ==32 | `var' ==22 | `var' ==36 | `var' ==42 | `var' ==52 | `var' ==62 | `var' ==66
| `var' ==72 | `var' ==82 | `var' ==86 | `var' ==92 | `var' == 97;													
replace `ocup' = 4 if `var' ==31 | `var' ==33 | `var' ==34; 		 									 						
replace `ocup' = 5 if `var' ==35 | `var' ==51 | `var' ==96 | `var' ==37 | `var' ==39 | `var' ==47 | `var' ==48 | `var' ==53
| `var' ==54 | `var' ==55 | `var' ==56 | `var' ==57 | `var' == 58 | `var' ==59 | `var' ==93 | `var' ==87 | `var' ==98 |
`var' == 95 | `var' ==30 | `var' ==49 | `var' ==85;				
replace `ocup' = 6 if `var' ==61 | `var' ==63 | `var' ==64 | `var' ==65 | `var' ==67 | `var' ==73;																									
replace `ocup' = 7 if `var' ==38 | `var' ==50 | `var' ==68 | `var' ==74 | `var' ==76 | `var' ==78 | `var' ==84;															
replace `ocup' = 8 if `var' ==40;			 																	
replace `ocup' = 9 if `var' ==71 | `var' ==81 | `var' ==91 | `var' ==75 | `var' ==77 | `var' ==83;																											
label var     ocupa "ocupation in primary job";
label define  ocupa 1 "Profesionales y técnicos", add;
label define  ocupa 2 "Directores y funcionarios superiores", add;
label define  ocupa 3 "Personal administrativo y nivel intermedio", add;
label define  ocupa 4 "Comerciantes y vendedores", add;
label define  ocupa 5 "Trabajadores en servicios", add;
label define  ocupa 6 "Trabajadores agrícolas y afines", add;
label define  ocupa 7 "Obreros no agrícolas, conductores de maquinas y vehículos de   transporte y similares", add;
label define  ocupa 8 "Fuerzas Armadas", add;
label define  ocupa 9 "Otras ocupaciones no clasificadas en las anteriores", add;
rename ocupa ocupa_ci;
};
# delimit cr
else{
replace ocupa = 1 if (p20>=130 & p20<150) | (p20>=230 & p20<240)| (p20 >=430 & p20<470) | (p20 >=940 & p20<950)| (p20>=410 & p20<420) | (p20>=210 & p20<220) | (p20>=110 & p20<120)								
replace ocupa = 2 if p20 ==11 | p20 ==21 | p20 ==31 | p20 ==41 | p20 ==51		
replace ocupa = 3 if (p20>=120 & p20<130) | (p20>=320 & p20<330) | (p20>=220 & p20<230) | (p20>=360 & p20<370) | (p20>=420 & p20<430) | (p20>=520 & p20<530) | (p20>=620 & p20<630) | (p20>=660 & p20<670) | (p20>=720 & p20<730) | (p20>=820 & p20<830) | (p20>=860 & p20<870) | (p20>=920 & p20<930) | (p20>=970 & p20<980)													
replace ocupa = 4 if (p20>=310 & p20<320) | (p20>=330 & p20<350) 		 									 						
replace ocupa = 5 if (p20>=300 & p20<310) | (p20>=350 & p20<360) | (p20>=370 & p20<380) | (p20>=390 & p20<400) | (p20>=470 & p20<500) | (p20>=510 & p20<520) | (p20>=530 & p20<600) | (p20>=850 & p20<860) | (p20>=870 & p20<880) | (p20>=930 & p20<940) | (p20>=950 & p20<970) | (p20>=980 & p20<990)  						
replace ocupa = 6 if (p20>=610 & p20<620) | (p20>=630 & p20<660) | (p20>=670 & p20<680) | (p20>=730 & p20<740)																									
replace ocupa = 7 if (p20>=380 & p20<390) | (p20>=500 & p20<510) | (p20>=680 & p20<690) | (p20>=740 & p20<750) | (p20>=760 & p20<770) | (p20>=780 & p20<790) | (p20>=840 & p20<850)															
replace ocupa = 8 if (p20>=400 & p20<410)			 																	
replace ocupa = 9 if (p20>=710 & p20<720) | (p20>=810 & p20<820) | (p20>=910 & p20<920) | (p20>=750 & p20<760) | (p20>=770 & p20<780) | (p20>=830 & p20<840)																											
label var     ocupa "ocupation in primary job"
label define  ocupa 1 "Profesionales y técnicos", add
label define  ocupa 2 "Directores y funcionarios superiores", add
label define  ocupa 3 "Personal administrativo y nivel intermedio", add
label define  ocupa 4 "Comerciantes y vendedores", add
label define  ocupa 5 "Trabajadores en servicios", add
label define  ocupa 6 "Trabajadores agrícolas y afines", add
label define  ocupa 7 "Obreros no agrícolas, conductores de maquinas y vehículos de   transporte y similares", add
label define  ocupa 8 "Fuerzas Armadas", add
label define  ocupa 9 "Otras ocupaciones no clasificadas en las anteriores", add
rename ocupa ocupa_ci
}
save `in`i'', replace
}
*/

forvalues i=7(1)11{
use `in`i'', clear
capture drop ocupa_ci
gen aux=real(p20)
drop p20
rename aux p20
gen ocupa = .
replace ocupa = 1 if (p20>=130 & p20<150) | (p20>=230 & p20<240)| (p20 >=430 & p20<470) | (p20 >=940 & p20<950)| (p20>=410 & p20<420) | (p20>=210 & p20<220) | (p20>=110 & p20<120)								
replace ocupa = 2 if p20 ==11 | p20 ==21 | p20 ==31 | p20 ==41 | p20 ==51		
replace ocupa = 3 if (p20>=120 & p20<130) | (p20>=320 & p20<330) | (p20>=220 & p20<230) | (p20>=360 & p20<370) | (p20>=420 & p20<430) | (p20>=520 & p20<530) | (p20>=620 & p20<630) | (p20>=660 & p20<670) | (p20>=720 & p20<730) | (p20>=820 & p20<830) | (p20>=860 & p20<870) | (p20>=920 & p20<930) | (p20>=970 & p20<980)													
replace ocupa = 4 if (p20>=310 & p20<320) | (p20>=330 & p20<350) 		 									 						
replace ocupa = 5 if (p20>=300 & p20<310) | (p20>=350 & p20<360) | (p20>=370 & p20<380) | (p20>=390 & p20<400) | (p20>=470 & p20<500) | (p20>=510 & p20<520) | (p20>=530 & p20<600) | (p20>=850 & p20<860) | (p20>=870 & p20<880) | (p20>=930 & p20<940) | (p20>=950 & p20<970) | (p20>=980 & p20<990)  						
replace ocupa = 6 if (p20>=610 & p20<620) | (p20>=630 & p20<660) | (p20>=670 & p20<680) | (p20>=730 & p20<740)																									
replace ocupa = 7 if (p20>=380 & p20<390) | (p20>=500 & p20<510) | (p20>=680 & p20<690) | (p20>=740 & p20<750) | (p20>=760 & p20<770) | (p20>=780 & p20<790) | (p20>=840 & p20<850)															
replace ocupa = 8 if (p20>=400 & p20<410)			 																	
replace ocupa = 9 if (p20>=710 & p20<720) | (p20>=810 & p20<820) | (p20>=910 & p20<920) | (p20>=750 & p20<760) | (p20>=770 & p20<780) | (p20>=830 & p20<840)																											
label var     ocupa "ocupation in primary job"
label define  ocupa 1 "Profesionales y técnicos", add
label define  ocupa 2 "Directores y funcionarios superiores", add
label define  ocupa 3 "Personal administrativo y nivel intermedio", add
label define  ocupa 4 "Comerciantes y vendedores", add
label define  ocupa 5 "Trabajadores en servicios", add
label define  ocupa 6 "Trabajadores agrícolas y afines", add
label define  ocupa 7 "Obreros no agrícolas, conductores de maquinas y vehículos de   transporte y similares", add
label define  ocupa 8 "Fuerzas Armadas", add
label define  ocupa 9 "Otras ocupaciones no clasificadas en las anteriores", add
rename ocupa ocupa_ci
save `in`i'', replace
}
