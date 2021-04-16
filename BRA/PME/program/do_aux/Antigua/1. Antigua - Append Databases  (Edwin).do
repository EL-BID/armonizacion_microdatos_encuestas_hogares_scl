/*
** Description: Read the rawdata of the PME (Brazil) 1982 - 2001. The data was received from Edwin Goni and it contains the PME from 1982 to the ninth month of 2001.
** This program appends the data in two pieces from 1982-1991 and from 1992 to 2001. It also labels and renames the variables of interest. 
** Note: The user has to change some values in the local variables if it is the case.
** by Julian Messina and David Argente
*/

clear
set mem 1250m
set more off


** Declare Global Directory **

local propiodir = "${surveysFolder}\Users\wb377246\Documents\Julian\LM_W"

cd "`propiodir'\BRA\Data\pme\"


/* Loop # 1 to use all the complete information. All 12 months of information for every year*/

use _pme1.dta

foreach x of numlist 2/108 {

qui append using _pme`x'.dta
keep v001 nserie unidade ordemd ordem condom sex nasdia nasmes nasano v001 ordem condom npess ano mes v302 v307 ///
nserie ncont unidade ordemd grau serie ativ sexo confam nfam nasmes nasano horas posocu cart cativ idade nasdia escola ocup rend
display `x'
}

* Variables added for id (Manuel): v001 condom npess
*unidade nserie sexo condom nasdia nasmes nasano npess (165 A 167)
*v001 nserie unidade ordemd ordem condom sex nasdia nasmes nasano  (RESTO)
*v001 condom npess (Cambiar el keep)

compress


qui rename	ano		year
qui rename 	mes		month
qui rename	v302		morejob
qui rename	v307		type_salary
qui rename	serie		serie_ed
qui rename	nserie	serie
qui rename	ncont		control
qui rename	unidade	metro_area
qui rename	ordemd	orden
qui rename	grau		grau_ed
qui rename	ativ		industry
qui rename	sexo		male
qui rename	confam	cfamilia
qui rename	nfam		nfamilia
qui rename	nasmes	mob
qui rename	nasano	yobN
qui rename	horas		hours_a
qui rename	posocu	toccu
qui rename	cart		cartera
qui rename	cativ		work_status
qui rename	idade		age
qui rename	nasdia	dob
qui rename	escola	attending
qui rename	ocup		occupation
qui rename	rend		V309




** drop those individuals with wrong born year

drop if yobN < 900 
drop if yobN == 999

/*
** sum of year, month and day of born
sum yobN, d
sum mob, d
sum dob, d
*/

count

*** Proc to create the proper age ***

** gen new year of born  + 1000 .
gen yob = yobN+1000

** gen continuos variable for the ANO and MES of the survey. 
g calendar_s = ym(year,month)

** gen continuos variable for Year and Month of born
g calendar_b = ym(yob,mob)

g age_a = calendar_s - calendar_b
drop  calendar_s calendar_b yobN

g age_f = age_a/12

g age_calculed = int(age_f)

drop age_a age_f 

** compare with age from PME (IBGE)
/*
sum age if age~=., d 
sum age_c if age~=., d 

gen ind = 1 if age == age_ca & age~=.
replace ind=0 if ind!=1 & age~=.
tab ind
*/
			** recode
			recode work_status  	(0 = . )	
			recode cartera  		(2 = 1)
			recode cartera  		(4 = 0)
			** wage	
			gen wage_a = .
			replace wage_a = V309 if toccu==2
			replace wage_a = V309 if toccu==4
			replace wage_a = . if wage_a == 1.000e+09

			drop V309  type_salary morejob type_salary
		

			** change values for some  variables			
			
			replace male = 0 if male == 3
			replace attending = 0 if attending == 3

			** label define
			
			label define malel 1"yes" 0"no"
			label values male malel
		
			label define cartera_l 2"yes" 4"no"
			label values cartera cartera_l
						
			label define metro_area_l 26"recife" 29"salvador" 31"belo horizonte" 33"rio de janeiro" 35"sao paulo" 43"porto alegre"
			label values metro_area metro_area_l
	
			label define work_statuslf 1"worker" 2"worker but no work" 3"job search" 4"retired" 5"student" 6"domestic" 7"other" 
			label values work_status work_statuslf

			label define toccul 2"salaried worker" 4"self employed" 6"employer" 8"worker w.o wage"
			label values toccu toccul
			
			label define attending_l 1"yes" 0"no"
			label values attending attending_l
	
			
			** label variables 
			
			label var wage_a 		"wage (actual)"
			label var male 		"=1 if person is male"
			label var cartera 	"=1 if person has carteira de trabalho (only salaried workers)"
			label var hours_a		"how many hours work per week? - Actual"
			label var age 		"age"
			label var age_c 		"age calculed by LCRCE"
			label var attending	"=1 if attending school"
			label var serie_ed	"what was the last grade ... concluded?"
			label var grau_ed		"highest educational level achieved"
			label var year 		"survey year"
			label var month 		"survey month"
			label var serie		"serie number"
			*label var metro_area	"metropolitan area"
			label var yob		"year of birth"
			label var wage_a 		"wage (actual)"
			label var work_status 	"status in the labor force"
			label var toccu	 	"type of occupation"	
			label var occupation	"occupation in this job"	
			label var industry	"industry of this job"		
			

			** generates education variable
			
			gen yeduc = .
			replace yeduc = 1 if grau_ed == 0
			replace yeduc = 2 if ((grau_ed == 1 | grau_ed == 3) & ( serie_ed >=1 & serie_ed <=3))
			replace yeduc = 3 if ((grau_ed == 1 | grau_ed == 3) & ( serie_ed >=4 & serie_ed <=6)) | (grau_ed == 2 & (serie_ed>= 1 & serie_ed <= 3)) | (grau_ed == 3 & serie_ed == 7)
			replace yeduc = 4 if (grau_ed == 2 & (serie_ed >=4 & serie_ed <= 5)) 
			replace yeduc = 4 if (grau_ed == 3 & serie_ed == 8)
			replace yeduc = 4 if (grau_ed == 4 & (serie_ed>= 1 & serie_ed<=2))
			replace yeduc = 4 if (grau_ed == 5 & (serie_ed>= 1 & serie_ed<=2))
			replace yeduc = 5 if ((grau_ed == 4 | grau_ed ==5) & ( serie_ed>=3 & serie_ed<=4)) | grau_ed == 6 | grau_ed == 7
			label define yeduc_l 1"< 1" 2"1 to 3" 3"4 to 7" 4"8 to 10" 5"11 or more" 
			label values yeduc yeduc_l
			label var yeduc "categories of years of education, adapted from IGBE"				


			** industry classification (CIIU) - New variable
			
			g ciiu = .
			replace ciiu = 0 if ( industry  >= 011 & industry <= 037) 												/*agriculture, hunting and forestry*/
			replace ciiu = 1 if ( industry  >= 041 & industry <= 042)		 							   			/*fishing*/	
			replace ciiu = 2 if ( industry  >= 050 & industry <= 059)  												/*mining and quarrying*/
			replace ciiu = 3 if ( industry  >= 100 & industry <= 300)  								   				/*industry*/
			replace ciiu = 4 if ( industry  >= 351 & industry <= 354)							   					/*electricity, gas and water supply*/
			replace ciiu = 5 if ( industry  == 340)										   					/*construction*/ 	
			replace ciiu = 6 if ( industry  >= 410 & industry <= 424)						   						/*whosale and retail trade*/ 	
			replace ciiu = 7 if ( industry  >= 511 & industry <= 512)									   			/*hotels and restaurants*/	
			replace ciiu = 8 if ( industry  >= 471 & industry <= 477) |(industry >= 481 & industry < = 482)			   			/*transport*/	
			replace ciiu = 9 if ( industry  >= 451 & industry <= 453)					   							/*financial intermediation*/
			replace ciiu = 10 if (industry  >= 461 & industry <= 464)							   					/*real estate, renting and business activities*/	
			replace ciiu = 11 if (industry  >= 711 & industry <= 727)									   			/*public administration and defense*/	
			replace ciiu = 12 if (industry  >= 631 & industry <= 632)									   			/*education*/
			replace ciiu = 13 if (industry  >= 610 & industry <= 624)									   			/*health and social work*/			
			replace ciiu = 14 if (industry  >= 521 & industry <= 589)							   		/*other social services*/
			replace ciiu = 15 if (industry  == 801)									   			/*extra territorial org*/	
			label var ciiu		"ciiu activities"

#delimit;

			label define ciiu_l 

			0 "agriculture, hunting and forestry" 
			1"fishing" 2"mining and quarrying" 
			3"industry" 
			4"electricity, gas and water supply"
			5"construction"
			6"whosale and retail trade"
			7"hotels and restaurants"	
			8"transport"
			9"financial intermediation"
			10"real estate, renting and business activities"
			11"public administration and defense"
			12"education"
			13"health and social work"
			14"other social services"
			15"extra territorial org";

#delimit cr

			label values ciiu ciiu_l	

			** Create notes for variables
			notes wage_a: gross monthly wage for salaried workers and self employed. actual
			notes ciiu: variable created using the industry reported in the PME


			sort year month metro_area
			merge year month metro_area using factor2.dta 
			drop _merge

			label var peso 		"weight"
			tostring control, replace

drop serie_ed grau_ed age
drop if year > 1990

compress
save PMEANTIGUA_82_91.dta, replace

******************************

/* Loop # 1 to use all the complete information. All 12 months of information for every year*/
*/
clear
use _pme109.dta

foreach x of numlist 110/237 {

qui append using _pme`x'.dta
keep v001 condom npess  ano mes v302 v307 nserie ncont unidade ordemd grau serie ativ sexo confam ///
nfam nasmes nasano horas posocu cart cativ idade nasdia escola ocup rend v001 nserie unidade ordemd ///
ordem condom sex nasdia nasmes nasano v001 ordem condom npess ano mes v302 v307 ///
nserie ncont unidade ordemd grau serie ativ sexo confam nfam nasmes nasano horas posocu cart cativ idade nasdia escola ocup rend
display `x' in green
}
rename nasano yobN
drop if yobN < 900 
drop if yobN == 999


compress


qui rename	ano		year
qui rename 	mes		month
qui rename	v302		morejob
qui rename	v307		type_salary
qui rename	serie		serie_ed
qui rename	nserie	serie
qui rename	ncont		control
qui rename	unidade	metro_area
qui rename	ordemd	orden
qui rename	grau		grau_ed
qui rename	ativ		industry
qui rename	sexo		male
qui rename	confam	cfamilia
qui rename	nfam		nfamilia
qui rename	nasmes	mob
qui rename	horas		hours_a
qui rename	posocu	toccu
qui rename	cart		cartera
qui rename	cativ		work_status
qui rename	idade		age
qui rename	nasdia	dob
qui rename	escola	attending
qui rename	ocup		occupation
qui rename	rend		V309



** drop those individuals with wrong born year
drop if yobN < 900 
drop if yobN == 999

/*
** sum of year, month and day of born
sum yobN, d
sum mob, d
sum dob, d
*/

count

*** Proc to create the proper age ***

** gen new year of born  + 1000 .
gen yob = yobN+1000

** gen continuos variable for the ANO and MES of the survey. 
g calendar_s = ym(year,month)

** gen continuos variable for Year and Month of born
g calendar_b = ym(yob,mob)

g age_a = calendar_s - calendar_b
drop  calendar_s calendar_b yobN

g age_f = age_a/12

g age_calculed = int(age_f)

drop age_a age_f 

** compare with age from PME (IBGE)
/*
sum age if age~=., d 
sum age_c if age~=., d 

gen ind = 1 if age == age_ca & age~=.
replace ind=0 if ind!=1 & age~=.
tab ind
*/
			** recode
			recode work_status  	(0 = . )	
			recode cartera  		(2 = 1)
			recode cartera  		(4 = 0)
			** wage	
			gen wage_a = .
			replace wage_a = V309 if toccu==2
			replace wage_a = V309 if toccu==4
			replace wage_a = . if wage_a == 1.000e+09

			drop V309  type_salary morejob type_salary
		

			** change values for some  variables			
			replace male = 0 if male == 3
			replace attending = 0 if attending == 3

			** label define
			label define malel 1"yes" 0"no"
			label values male malel
		
			label define cartera_l 2"yes" 4"no"
			label values cartera cartera_l
						
			label define metro_area_l 26"recife" 29"salvador" 31"belo horizonte" 33"rio de janeiro" 35"sao paulo" 43"porto alegre"
			label values metro_area metro_area_l
	
			label define work_statuslf 1"worker" 2"worker but no work" 3"job search" 4"retired" 5"student" 6"domestic" 7"other" 
			label values work_status work_statuslf

			label define toccul 2"salaried worker" 4"self employed" 6"employer" 8"worker w.o wage"
			label values toccu toccul
			
			label define attending_l 1"yes" 0"no"
			label values attending attending_l
	
			** label variables 
			label var wage_a 		"wage (actual)"
			label var male 		"=1 if person is male"
			label var cartera 	"=1 if person has carteira de trabalho (only salaried workers)"
			label var hours_a		"how many hours work per week? - Actual"
			label var age 		"age"
			label var age_c 		"age calculed by LCRCE"
			label var attending	"=1 if attending school"
			label var serie_ed	"what was the last grade ... concluded?"
			label var grau_ed		"highest educational level achieved"
			label var year 		"survey year"
			label var month 		"survey month"
			label var serie		"serie number"
			*label var metro_area	"metropolitan area"
			label var yob		"year of birth"
			label var wage_a 		"wage (actual)"
			label var work_status 	"status in the labor force"
			label var toccu	 	"type of occupation"	
			label var occupation	"occupation in this job"	
			label var industry	"industry of this job"		
			
** generates education variable
			gen yeduc = .
			replace yeduc = 1 if grau_ed == 0
			replace yeduc = 2 if ((grau_ed == 1 | grau_ed == 3) & ( serie_ed >=1 & serie_ed <=3))
			replace yeduc = 3 if ((grau_ed == 1 | grau_ed == 3) & ( serie_ed >=4 & serie_ed <=6)) | (grau_ed == 2 & (serie_ed>= 1 & serie_ed <= 3)) | (grau_ed == 3 & serie_ed == 7)
			replace yeduc = 4 if (grau_ed == 2 & (serie_ed >=4 & serie_ed <= 5)) 
			replace yeduc = 4 if (grau_ed == 3 & serie_ed == 8)
			replace yeduc = 4 if (grau_ed == 4 & (serie_ed>= 1 & serie_ed<=2))
			replace yeduc = 4 if (grau_ed == 5 & (serie_ed>= 1 & serie_ed<=2))
			replace yeduc = 5 if ((grau_ed == 4 | grau_ed ==5) & ( serie_ed>=3 & serie_ed<=4)) | grau_ed == 6 | grau_ed == 7
			label define yeduc_l 1"< 1" 2"1 to 3" 3"4 to 7" 4"8 to 10" 5"11 or more" 
			label values yeduc yeduc_l
			label var yeduc "categories of years of education, adapted from IGBE"				


			** industry classification (CIIU) - New variable
			g ciiu = .
			replace ciiu = 0 if ( industry  >= 011 & industry <= 037) 							   					/*agriculture, hunting and forestry*/
			replace ciiu = 1 if ( industry  >= 041 & industry <= 042)		 							   			/*fishing*/	
			replace ciiu = 2 if ( industry  >= 050 & industry <= 059)  												/*mining and quarrying*/
			replace ciiu = 3 if ( industry  >= 100 & industry <= 300)  								   				/*industry*/
			replace ciiu = 4 if ( industry  >= 351 & industry <= 354)							   					/*electricity, gas and water supply*/
			replace ciiu = 5 if ( industry  == 340)										   					/*construction*/ 	
			replace ciiu = 6 if ( industry  >= 410 & industry <= 424)						   						/*whosale and retail trade*/ 	
			replace ciiu = 7 if ( industry  >= 511 & industry <= 512)									   			/*hotels and restaurants*/	
			replace ciiu = 8 if ( industry  >= 471 & industry <= 477) |(industry >= 481 & industry < = 482)			   			/*transport*/	
			replace ciiu = 9 if ( industry  >= 451 & industry <= 453)					   							/*financial intermediation*/
			replace ciiu = 10 if (industry  >= 461 & industry <= 464)							   					/*real estate, renting and business activities*/	
			replace ciiu = 11 if (industry  >= 711 & industry <= 727)									   			/*public administration and defense*/	
			replace ciiu = 12 if (industry  >= 631 & industry <= 632)									   			/*education*/
			replace ciiu = 13 if (industry  >= 610 & industry <= 624)									   			/*health and social work*/			
			replace ciiu = 14 if (industry  >= 521 & industry <= 589)							   		/*other social services*/
			replace ciiu = 15 if (industry  == 801)									   			/*extra territorial org*/	
			label var ciiu		"ciiu activities"

#delimit;

			label define ciiu_l 

			0 "agriculture, hunting and forestry" 
			1"fishing" 2"mining and quarrying" 
			3"industry" 
			4"electricity, gas and water supply"
			5"construction"
			6"whosale and retail trade"
			7"hotels and restaurants"	
			8"transport"
			9"financial intermediation"
			10"real estate, renting and business activities"
			11"public administration and defense"
			12"education"
			13"health and social work"
			14"other social services"
			15"extra territorial org";

#delimit cr

			label values ciiu ciiu_l	

			** Create notes for variables
			notes wage_a: gross monthly wage for salaried workers and self employed. actual
			notes ciiu: variable created using the industry reported in the PME


			sort year month metro_area
			merge year month metro_area using factor2.dta 
			drop _merge

			label var peso 		"weight"
			tostring control, replace

drop serie_ed grau_ed age
drop if year<1991

compress
save PMEANTIGUA_92_01.dta, replace
