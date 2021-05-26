/*
** Description: Program that uses the Annual database created in the program 2 and renames and labels the variables. 
** by Carlos Prada
** Survey: PME (2002-2010)
*/

clear
set mem 700m, perm
set more off

**Note: this local should be change by the user
local propiodir = "${surveysFolder}\Users\wb377246\Documents\Julian\LM_W"

*** Declare Global Directory
global dir	  = "`propiodir'\BRA\Data"


* 1. *** Rename and generate variables of interest ***
	
	local year 2002 2003 2004 2005 2006 2007 2008 2009 2010
	foreach x of local year {
	
		use "$dir\\PME`x'_TOTAL.dta", clear

		** clean database: drop those individuals with wrong born year
		qui drop if V204 == 99 
		qui drop if V214 == 99 
		qui drop if V224 == 9999


		qui destring V075, g(year)
		qui destring V070, g(month)	
		drop V070 V075
	
		qui compress
		
	** gen new panel variable (V060) to improve the memory capcity
	qui gen panel =.
	qui replace panel = 1 if V060 == "A"
	qui replace panel = 2 if V060 == "B"
	qui replace panel = 3 if V060 == "C"
	qui replace panel = 4 if V060 == "D"
	qui replace panel = 5 if V060 == "E"
	qui replace panel = 6 if V060 == "F"
	qui replace panel = 7 if V060 == "G"
	qui replace panel = 8 if V060 == "H"
	qui replace panel = 9 if V060 == "I"
	qui replace panel = 10 if V060 == "J"
	qui replace panel = 11 if V060 == "K"
	qui replace panel = 12 if V060 == "L"
	qui replace panel = 13 if V060 == "M"
	qui replace panel = 14 if V060 == "N"
	qui replace panel = 15 if V060 == "O"
	
	drop V060

	** gen new rotational group  variable (V063) to improve the memory capacity

	qui gen rgroup = .
	qui replace rgroup = 1 if V063 =="1"
	qui replace rgroup = 2 if V063 =="2"
	qui replace rgroup = 3 if V063 =="3"
	qui replace rgroup = 4 if V063 =="4"
	qui replace rgroup = 5 if V063 =="5"
	qui replace rgroup = 6 if V063 =="6"
	qui replace rgroup = 7 if V063 =="7"
	qui replace rgroup = 8 if V063 =="8"
	
	drop V063
	** gen continuos variable for the ANO and MES of the survey. 
	g calendar_s = ym(year,month)

	** gen continuos variable for Year and Month of born
	g calendar_b = ym(V224,V214)

	g age_a = calendar_s - calendar_b

	g age_f = age_a/12

	g age_calculed = int(age_f)
	
	** Drop missing observations with age calculed
	drop  calendar_s calendar_b age_a age_f 
	drop if age_calculed ==. 
	
	qui gen ind = 1 if age_c == V234
	qui replace ind = 0 if ind~=1 & V234!=.	
	label var ind "==1 if age calculed is equal to age from IGBE"

	label var panel 	"panel"
	label var rgroup 	"rotational group" 
	label var age_c 	"age calculed by LCRCE"

	qui order V040 V050 V201

	** Misc

		** Treat with missing values
		 
		qui replace V204 = .  if V204== 99
		qui replace V214 = .  if V214== 99
		qui replace V224 = .  if V224== 9999
		qui replace V301 = .  if V301==9
		qui replace VDAE1 = . if  VDAE1 == 6

	
		** Recode Variables
		recode V203 ( 2 = 0) /* Recode gender*/
		recode V301 ( 1 = 0) ( 2 = 1) /* Recode illiterate*/
		recode V302 ( 2 = 0) /* Recode attending*/
		recode V302 ( 9 = .) /* Recode attending*/
		recode V308 ( 2 = 0) /* Recode seriated course*/
		recode V309 ( 2 = 0) /* Recode finished first level*/
		recode V401 ( 2 = 0) /* Recode worked last week*/
		recode V411 ( 2 = 0) /* Recode type of work (private or public)*/
		recode V415 ( 2 = 0) /* Recode carteira*/
		recode V417 ( 2 = 0) /* Recode temporal job*/
		recode V468 ( 2 = 0) /* Recode direct survey*/
		recode V468 ( 9 = .) /* Recode direct survey*/

		** Generate new variables
			** Salaries
			gen wage_r = .
			replace wage_r = V4182  if V409==2
			replace wage_r = V4231  if V409==3
			replace wage_r = . if wage_r == 1.000e+09
		
			gen wage_a = .
			replace wage_a = V4191 if V409==2
			replace wage_a = V4241 if V409==3
			replace wage_a = . if wage_a == 1.000e+09

			gen wage_a_im = .
			replace wage_a_im =  VI4191 if V409==2
			replace wage_a_im =  VI4241 if V409==3
			replace wage_a_im =  . if wage_a_im == 1.00e+08

			** social security
			gen ssecurity = .
			replace ssecurity = V416 if V416!=.
			replace ssecurity = V425 if V425!=.
			recode ssecurity (2 = 0)	
		
			** firm size
			gen fsize = .
			replace fsize = V412 if V412!=.
			replace fsize = V426 if V426!=.

				
			** industry classification (CIIU) - New variable
			g ciiu = .
			replace ciiu = 0 if ( V408 == 1 | V408==2 ) 							   /*agriculture, hunting and forestry*/
			replace ciiu = 1 if ( V408 == 5)		 							   /*fishing*/	
			replace ciiu = 2 if ( V408 == 10 | V408 == 11 | V408 == 12 | V408 == 13 | V408 == 14)  /*mining and quarrying*/
			replace ciiu = 3 if ( V408 > 14 & V408<= 37)  							   /*industry*/
			replace ciiu = 4 if ( V408 == 40 | V408 == 41)							   /*electricity, gas and water supply*/
			replace ciiu = 5 if ( V408 == 45)									   /*construction*/ 	
			replace ciiu = 6 if ( V408 == 50 | V408 == 51 | V408 ==53)					   /*whosale and retail trade*/ 	
			replace ciiu = 7 if ( V408 == 55)									   /*hotels and restaurants*/	
			replace ciiu = 8 if ( V408 > 59 & V408 <= 64)							   /*transport*/	
			replace ciiu = 9 if ( V408 == 65 | V408 == 66 | V408 == 67)					   /*financial intermediation*/
			replace ciiu = 10 if (V408 > 69 & V408 <=74)							   /*real estate, renting and business activities*/	
			replace ciiu = 11 if (V408  == 75)									   /*public administration and defense*/	
			replace ciiu = 12 if (V408  == 80)									   /*education*/
			replace ciiu = 13 if (V408  == 85)									   /*health and social work*/			
			replace ciiu = 14 if (V408  > 89 & V408 <= 93) | (V408 ==95)				   /*other social services*/
			replace ciiu = 15 if (V408 == 99)									   /*extra territorial org*/	
			notes ciiu: variable created using the industry reported in the PME
		
			** drop unnecesary variables
			drop V055 V412 V416 V425 V426 V4182 V4231  V4191 VI4191 V4241 VI4241 V311 V312

		** Rename variables
		qui rename  V035  metropol_area
		qui rename  V234	age_c
		qui rename  V301  illiterate
		qui rename  V406	number_works
		qui rename  V411  private
		qui rename  V415  cartera
		qui rename  V417  temporal
		qui rename  V4271 tenure_days
		qui rename  V4272 tenure_months
		qui rename  V4273 tenure_years_comp
		qui rename  V4275 tenure_months_comp
		qui rename  V4274 tenure_years
		qui rename  V428  hours_r
		qui rename 	V429	hours_a
		qui rename  V040  control_number
		qui rename  V050  serie_number
		qui rename  V201  order
		qui rename  V072  round
		qui rename  V215  peso
		qui rename  V203  male
		qui rename  V208  race
		qui rename  VDAE1 yeduc
		qui rename 	VDAE2 yeduc2
		qui rename  V204  dob
		qui rename  V214	mob
		qui rename  V224  yob
		qui rename 	V302  attending
		qui rename  V307	grau_ed 
		qui rename  V308  seriated
		qui rename  V309  finished_course
		qui rename  V310	serie_ed
		qui rename  V468  direct_survey

		qui rename  VD1	work_status
		qui rename  V409  toccu
		qui rename  V407  occupation	
		qui rename  V408  industry	
	
		** Recode tenure in one variable (in months) 

		** Generate a new variable for tenure
		g tenurem = .
		replace tenurem = 0 if tenure_days<.
		replace tenurem = tenure_months if tenure_months<.
		replace tenurem = tenure_months_comp+12 if  tenure_months_comp<.
		replace tenurem = tenure_years*12 if tenure_years<.
		
		** Create label for some variables
		label var male 			"=1 if person is male"
		label var wage_r 			"wage (regular)"
		label var wage_a 			"wage (actual)"
		label var wage_a_im 		"wage (actual-imputed) "
		label var ssecurity		"=1 if person has social security (pension)"
		label var fsize			"firm size"
		label var ciiu			"ciiu activities"
		label var tenurem			"tenure in months"
		label var illiterate		"=1 if illiterate"
		label var attending		"=1 if attending school"
		label var grau_ed			"highest educational level achieved"
		label var number_w		"how many works ... has?"
		label var private			"=1 if worker is private (only salaried workers)"
		label var cartera 		"carteira de trabalho in this job? (only salaried workers)"
		label var temporal 		"the term of contract is temporal? (only salaried workers)"
		label var seriated 		"this level ... attended was seriated?"
		label var finished_course	"... finished almost the first grade in the level?"
 		label var direct_survey		"=1 if survey was realized directly with the individual"
		label var serie_ed		"what was the last grade ... concluded?"
		label var hours_r			"how many hours work per week? - Regular"	
		label var hours_a			"how many hours work per week? - Actual"	
		label var yeduc			"categories of years of education created by IGBE"
		label var peso			"expansion factor"
		
		label var work_status 		"status in the labor force"
		label var toccu			"type of occupation"
		label var occupation		"occupation in this job"	
		label var industry		"industry of this job"

		** Label values for the selected variables
		label define direct_survey_l 1"yes" 0"no"
		label values direct_survey direct_survey_l

		label define illi_l 1 "yes" 0"no"
		label values illi illi_l
		
		label define toccu_l 1 "domestic worker" 2 "salaried worker" 3 "self employed" 4 "employer" 5"worker w.o wage - sw o se" 6"worker w.o wage - employer"
		label values toccu toccu_l
		notes toccu: 5"worker w.o wage - se or employer" Indicates worker with out wage in a family with self employed or employer
		notes toccu: 6"worker w.o wage - sw" Indicates worker with out wage in a family with salaried worker

		label define private_l 1"private" 0"public"
		label values private private_l 
	
		label define fsize_l 1"2 to 5" 2"6 to 10" 3"11 o more"
		label values fsize fsize_l
	
		label define cartera_l 1"yes" 0"no"
		label values cartera cartera_l

		label define ssecurity_l 1"yes" 0"no"
		label values ssecurity ssecurity_l

		label define temporal_l 1"yes"  0"no"
		label values temporal temporal_l

		destring metropol_area, g (metro_area)
		label define metro_area_l 26"recife" 29"salvador" 31"belo horizonte" 33"rio de janeiro" 35"sao paulo" 43"porto alegre"
		label values metro_area metro_area_l
		drop metropol_area
		drop if metro_area ==	39

		label define male_l 1"male" 0"female"
		label values male male_l

		label define race_l 1"white" 2"black" 3"yellow" 4"brown" 5"indigena"
		label values race race_l

		label define yeduc_l 1"< 1" 2"1 to 3" 3"4 to 7" 4"8 to 10" 5"11 or more" 
		label values yeduc yeduc_l

		label define yeduc2_l 1"< 8" 2"8 to 10" 3"11 or more"	
		label values yeduc2 yeduc2_l
		
		label define work_status_l 1"occupied" 2"unemployed" 3"inactive"
		label values work_status work_status_l
		
		label define seriated_l 1"yes" 0"no"
		label values seriated seriated_l

		label define attending_l 1"yes" 0"no"
		label values attending attending_l
		notes attend: 5,567,812 does not answered the question

		label define finished_course_l 1"yes" 0"no"
		label values finished_course finished_course_l

		drop  tenure_days tenure_months tenure_years_comp tenure_months_comp tenure_years  age_c hours_r
#delimit ;

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

#delimit ;		
	label define industry_l 
			1"AGRICULTURA, PECUÄŹĹĽËťRIA E SERVIÄŹĹĽËťOS RELACIONADOS COM ESSAS ATIVIDADES"
			2"SILVICULTURA, EXPLORAÄŹĹĽËťÄŹĹĽËťO FLORESTAL E SERVIÄŹĹĽËťOS RELACIONADOS COM ESTAS ATIVIDADES"
			5"PESCA, AQÄŹĹĽËťICULTURA E ATIVIDADES DOS SERVIÄŹĹĽËťOS RELACIONADOS COM ESTAS ATIVIDADES"
			10"EXTRAÄŹĹĽËťÄŹĹĽËťO DE CARVÄŹĹĽËťO MINERAL"
			11"EXTRAÄŹĹĽËťÄŹĹĽËťO DE PETRÄŹĹĽËťLEO E SERVIÄŹĹĽËťOS CORRELATOS"
			12"EXTRAÄŹĹĽËťÄŹĹĽËťO DE MINERAIS RADIOATIVOS"
			13"EXTRAÄŹĹĽËťÄŹĹĽËťO DE MINERAIS METÄŹĹĽËťLICOS"
			14"EXTRAÄŹĹĽËťÄŹĹĽËťO DE MINERAIS NÄŹĹĽËťO-METÄŹĹĽËťLICOS"
			15"FABRICAÄŹĹĽËťÄŹĹĽËťO DE PRODUTOS ALIMENTÄŹĹĽËťCIOS E BEBIDAS"
			16"FABRICAÄŹĹĽËťÄŹĹĽËťO DE PRODUTOS DO FUMO"
			17"FABRICAÄŹĹĽËťÄŹĹĽËťO DE PRODUTOS TÄŹĹĽËťXTEIS"
			18"CONFECÄŹĹĽËťÄŹĹĽËťO DE ARTIGOS DO VESTUÄŹĹĽËťRIO E ACESSÄŹĹĽËťRIOS"
			19"PREPARAÄŹĹĽËťÄŹĹĽËťO DE COUROS E FABRICAÄŹĹĽËťÄŹĹĽËťO DE ARTEFATOS DE COURO, ARTIGOS DE VIAGEM E CALÄŹĹĽËťADOS"
			20"FABRICAÄŹĹĽËťÄŹĹĽËťO DE PRODUTOS DE MADEIRA"
			21"FABRICAÄŹĹĽËťÄŹĹĽËťO DE CELULOSE, PAPEL E PRODUTOS DE PAPEL"
			22"EDIÄŹĹĽËťÄŹĹĽËťO, IMPRESSÄŹĹĽËťO E REPRODUÄŹĹĽËťÄŹĹĽËťO DE GRAVAÄŹĹĽËťÄŹĹĽËťES"
			23"COQUERIAS"
			24"FABRICAÄŹĹĽËťÄŹĹĽËťO DE PRODUTOS QUÄŹĹĽËťMICOS"
			25"FABRICAÄŹĹĽËťÄŹĹĽËťO DE PRODUTOS DE BORRACHA E PLÄŹĹĽËťSTICO"
			26"FABRICAÄŹĹĽËťÄŹĹĽËťO DE PRODUTOS DE MINERAIS NÄŹĹĽËťO-METÄŹĹĽËťLICOS"
			27"METALURGIA BÄŹĹĽËťSICA"
			28"FABRICAÄŹĹĽËťÄŹĹĽËťO DE PRODUTOS DE METAL - EXCLUSIVE MÄŹĹĽËťQUINAS E EQUIPAMENTOS"
			29"FABRICAÄŹĹĽËťÄŹĹĽËťO DE MÄŹĹĽËťQUINAS E EQUIPAMENTOS"
			30"FABRICAÄŹĹĽËťÄŹĹĽËťO DE MÄŹĹĽËťQUINAS E EQUIPAMENTOS DE SISTEMAS ELETRÄŹĹĽËťNICOS PARA PROCESSAMENTO DE DADOS"
			31"FABRICAÄŹĹĽËťÄŹĹĽËťO DE MÄŹĹĽËťQUINAS, APARELHOS E MATERIAIS ELÄŹĹĽËťTRICOS"
			32"FABRICAÄŹĹĽËťÄŹĹĽËťO DE MATERIAL ELETRÄŹĹĽËťNICO E DE APARELHOS E EQUIPAMENTOS DE COMUNICAÄŹĹĽËťÄŹĹĽËťES "
			33"FABRICAÄŹĹĽËťÄŹĹĽËťO DE EQUIPAMENTOS DE INSTRUMENTAÄŹĹĽËťÄŹĹĽËťO MÄŹĹĽËťDICO-HOSPITALARES, INSTRUMENTO DE PRECISÄŹĹĽËťO E ÄŹĹĽËťPTICOS, EQUIPAMENTOS PARA AUTOMAÄŹĹĽËťÄŹĹĽËťO INDUSTRIAL, CRONÄŹĹĽËťMETROS E RELÄŹĹĽËťGIOS"
			34"FABRICAÄŹĹĽËťÄŹĹĽËťO E MONTAGEM DE VEÄŹĹĽËťCULOS AUTOMOTORES, REBOQUES E CARROCERIAS"
			35"FABRICAÄŹĹĽËťÄŹĹĽËťO DE OUTROS EQUIPAMENTOS DE TRANSPORTES"
			36"FABRICAÄŹĹĽËťÄŹĹĽËťO DE MÄŹĹĽËťVEIS E INDÄŹĹĽËťSTRIAS DIVERSAS "
			37"RECICLAGEM"
			40"PRODUÄŹĹĽËťÄŹĹĽËťO E DISTRIBUIÄŹĹĽËťÄŹĹĽËťO DE ELETRICIDADE, GÄŹĹĽËťS E ÄŹĹĽËťGUA"
			41"CAPTAÄŹĹĽËťÄŹĹĽËťO, TRATAMENTO E DISTRIBUIÄŹĹĽËťÄŹĹĽËťO DE ÄŹĹĽËťGUA"
			45"CONSTRUÄŹĹĽËťÄŹĹĽËťO"
			50"COMÄŹĹĽËťRCIO E REPARAÄŹĹĽËťÄŹĹĽËťO DE VEÄŹĹĽËťCULOS AUTOMOTORES E MOTOCICLETAS;  E COMÄŹĹĽËťRCIO A VAREJO DE COMBUSTÄŹĹĽËťVEIS "
			53"INTERMEDIÄŹĹĽËťRIOS DO COMÄŹĹĽËťRCIO, COMÄŹĹĽËťRCIO E REPARAÄŹĹĽËťÄŹĹĽËťO DE OBJETOS PESSOAIS E DOMÄŹĹĽËťSTICOS"
			55"ALOJAMENTO E ALIMENTAÄŹĹĽËťÄŹĹĽËťO"
			60"TRANSPORTE TERRESTRE"
			61"TRANSPORTE AQUAVIÄŹĹĽËťRIO"
			62"TRANSPORTE AÄŹĹĽËťREO"
			63"ATIVIDADES ANEXAS E AUXILIARES DO TRANSPORTE E AGÄŹĹĽËťNCIAS DE VIAGENS"
			64"CORREIOS E TELECOMUNICAÄŹĹĽËťÄŹĹĽËťES"
			65"INTERMEDIAÄŹĹĽËťÄŹĹĽËťO FINANCEIRA, EXCLUSIVE DE SEGUROS E PREVIDÄŹĹĽËťNCIA PRIVADA"
			66"SEGUROS E PREVIDÄŹĹĽËťNCIA PRIVADA"
			67"ATIVIDADES AUXILIARES DA INTERMEDIAÄŹĹĽËťÄŹĹĽËťO FINANCEIRA"
			70"ATIVIDADES IMOBILIÄŹĹĽËťRIAS"
			71"ALUGUEL DE VEÄŹĹĽËťCULOS, MÄŹĹĽËťQUINAS E EQUIPAMENTOS SEM CONDUTORES OU OPERADORES E DE OBJETOS PESSOAIS E DOMÄŹĹĽËťSTICOS"
			72"ATIVIDADES DE INFORMÄŹĹĽËťTICAS E CONEXAS"
			73"PESQUISA E DESENVOLVIMENTO"
			74"SERVIÄŹĹĽËťOS PRESTADOS PRINCIPALMENTE ÄŹĹĽËťS EMPRESAS"
			75"ADMINISTRAÄŹĹĽËťÄŹĹĽËťO PÄŹĹĽËťBLICA, DEFESA E SEGURIDADE SOCIAL                       "
			80"EDUCAÄŹĹĽËťÄŹĹĽËťO"
			85"SAÄŹĹĽËťDE E SERVIÄŹĹĽËťOS SOCIAIS"
			90"LIMPEZA URBANA E ESGOTO; E ATIVIDADES CONEXAS"
			91"ATIVIDADES ASSOCIATIVAS"
			92"ATIVIDADES RECREATIVAS, CULTURAIS E DESPORTIVAS"
			93"SEVIÄŹĹĽËťOS PESSOAIS"
			95"SERVIÄŹĹĽËťOS DOMÄŹĹĽËťSTICOS"
			99"ORGANISMOS INTERNACIONAIS E OUTRAS INSTITUIÄŹĹĽËťÄŹĹĽËťES EXTRATERRITORIAIS"
			0"ATVIDADES NÄŹĹĽËťO ESPECIFICADAS";

#delimit ;	

label define occupation_l  
			100"MEMBROS SUPERIORES DO PODER LEGISLATIVO, EXECUTIVO E JUDICIÄŹĹĽËťRIO; DIRIGENTES DE PRODUÄŹĹĽËťÄŹĹĽËťO, OPERAÄŹĹĽËťÄŹĹĽËťES E APOIO DA ADMINISTRAÄŹĹĽËťÄŹĹĽËťO PÄŹĹĽËťBLICA"
			11"CHEFES DE PEQUENAS POPULAÄŹĹĽËťÄŹĹĽËťES, DIRIGENTES E ADMINISTRADORES DE ORGANIZAÄŹĹĽËťÄŹĹĽËťO DE INTERESSE PÄŹĹĽËťBLICO"
			12"DIRETORES GERAIS DE EMPRESA E ORAGNIZAÄŹĹĽËťÄŹĹĽËťES (EXCETO DE INTERESSE PÄŹĹĽËťBLICO) "
			13"GERENTES DE PRODUÄŹĹĽËťÄŹĹĽËťO E OPERAÄŹĹĽËťÄŹĹĽËťES E DE ÄŹĹĽËťREAS DE APOIO"
			101"PROFISSIONAIS DE NÄŹĹĽËťVEL SUPERIOR (EXCLUSIVE DA NAVEGAÄŹĹĽËťÄŹĹĽËťO AÄŹĹĽËťREA, MARÄŹĹĽËťTIMA E FLUVIAL, DAS COMUNICAÄŹĹĽËťÄŹĹĽËťES E DAS ARTES, E MEMBROS DE CULTOS RELIGIOSOS)"
			102"PROFISSIONAIS EM NAVEGAÄŹĹĽËťÄŹĹĽËťO AÄŹĹĽËťREA, MARÄŹĹĽËťTIMA E FLUVIAL "
			26"PROFISSIONAIS DA COMUNICAÄŹĹĽËťÄŹĹĽËťO, PROFISSIONAIS DE ESPETÄŹĹĽËťCULOS E DAS ARTES"
			103"MEMBROS DE CULTOS RELIGIOSOS E AFINS"
			30"TÄŹĹĽËťCNICOS ELETROMECÄŹĹĽËťNICOS E MECATRÄŹĹĽËťNICOS, TÄŹĹĽËťCNICOS EM LABORATÄŹĹĽËťRIO "
			31"TÄŹĹĽËťCNICOS DE NÄŹĹĽËťVEL MÄŹĹĽËťDIO DA ENGENHARIA, CIÄŹĹĽËťNCIAS FÄŹĹĽËťSICAS, QUÄŹĹĽËťMICAS E AFINS"
			32"TÄŹĹĽËťCNICOS DE NÄŹĹĽËťVEL MÄŹĹĽËťDIO DA DAS CIÄŹĹĽËťNCIAS BIOLÄŹĹĽËťGICAS, BIOQUÄŹĹĽËťMICAS DA SAÄŹĹĽËťDE E AFINS"
			33"PROFESSORES LEIGOS E DE NÄŹĹĽËťVEL MÄŹĹĽËťDIO NA EDUCAÄŹĹĽËťÄŹĹĽËťO INFANTIL, NO ENSINO FUNDAMENTAL, PROFISSIONALIZANTE E NAS ESCOLAS LIVRES"
			104"TÄŹĹĽËťCNICOS EM NAVEGAÄŹĹĽËťÄŹĹĽËťO AÄŹĹĽËťREA, MARÄŹĹĽËťTIMA, FLUVIAL E METROFERROVIÄŹĹĽËťRIA"
			34"TÄŹĹĽËťCNICOS EM TRANSPORTES (LOGÄŹĹĽËťSTICA)"
			35"TÄŹĹĽËťCNICOS DAS CIÄŹĹĽËťNCIAS ADMINISTRATIVAS"
			37"TÄŹĹĽËťCNICOS DE NÄŹĹĽËťVEL MÄŹĹĽËťDIO DOS SERVIÄŹĹĽËťOS CULTURAIS DAS COMUNICAÄŹĹĽËťÄŹĹĽËťES E DOS DESPORTO"
			39"OUTROS TÄŹĹĽËťCNICOS DE NIVEL MÄŹĹĽËťDIO EM OPERAÄŹĹĽËťÄŹĹĽËťES INDUSTRIAIS"
			41"TRABALHADORES DOS DE SERVIÄŹĹĽËťOS ADMINISTRATIVOS (Exceto de atendimento ao pÄŹĹĽËťblico)"
			42"TRABALHADORES DOS DE SERVIÄŹĹĽËťOS ADMINISTRATIVOS (Somente de atendimento ao pÄŹĹĽËťblico)"
			51"SUPERVISORES DOS SERVIÄŹĹĽËťOS E DO COMÄŹĹĽËťRCIO, TRABALHADORES DOS SERVIÄŹĹĽËťOS DE HOTELARIA E ALIMENTAÄŹĹĽËťÄŹĹĽËťO, TRABALHADORES NOS SERVIÄŹĹĽËťOS DE ADMINISTRAÄŹĹĽËťÄŹĹĽËťO, CONSERVAÄŹĹĽËťÄŹĹĽËťO E MANUTENÄŹĹĽËťÄŹĹĽËťO DE EDIFÄŹĹĽËťCIOS E LOGRADOUROS, TRABALHADORES DOS SERVIÄŹĹĽËťOS DE SAÄŹĹĽËťDE, ATENDENTE DE CRECHE E ACOMPANHANTE DE IDOSOS E TRABALHADORES DOS SERVIÄŹĹĽËťOS FUNERÄŹĹĽËťRIOS, TRABALHADORES NOS SERVIÄŹĹĽËťOS DE PROTEÄŹĹĽËťÄŹĹĽËťO E SEGURANÄŹĹĽËťA, OUTROS TRABALHADORES DE SERVIÄŹĹĽËťOS DIVERSOS"
			105"TRABALHADORES DOS SERVIÄŹĹĽËťOS DE TRANSPORTE E TURISMO"
			106"TRABALHADORES DOS SERVIÄŹĹĽËťOS DOMÄŹĹĽËťSTICOS EM GERAL"
			107"TRABALHADORES NOS SERVIÄŹĹĽËťOS DE EMBELEZAMENTO E CUIDADOS PESSOAIS (EXCLUSIVE ATENDENTE DE CRECHE E ACOMPANHANTE DE IDOSOS E TRABALHADORES DOS SERVIÄŹĹĽËťOS FUNERÄŹĹĽËťRIOS)"
			108"SUPERVISORES DE VENDAS E DE PRESTAÄŹĹĽËťÄŹĹĽËťO DE SERVIÄŹĹĽËťOS DO COMÄŹĹĽËťRCIO"
			109"VENDEDORES, DEMONSTRADORES"
			110"REPOSITORES, REMARCADORES DO COMÄŹĹĽËťRCIO"
			111"INSTALADORES DE PRODUTOS E ACESSÄŹĹĽËťRIOS"
			112"VENDEDORES AMBULANTES E CAMELÄŹĹĽËťS"
			61"PRODUTORES AGROPECUÄŹĹĽËťRIOS EM GERAL, PRODUTORES AGRÄŹĹĽËťCOLAS, PRODUTORES EM PECUÄŹĹĽËťRIA"
			62"SUPERVISORRES NA EXPLORAÄŹĹĽËťÄŹĹĽËťO AGROPECUÄŹĹĽËťRIA, TRABALHADORES NA EXPLORAÄŹĹĽËťÄŹĹĽËťO AGROPECUÄŹĹĽËťRIA EM GERAL TRABALHADORES AGRÄŹĹĽËťCOLAS, TRABALHADORES NA PECUÄŹĹĽËťRIA"
			63"SUPERVISORES NA EXPLORAÄŹĹĽËťÄŹĹĽËťO FLORESTAL, CAÄŹĹĽËťA E PESCA, PESCADORES, CAÄŹĹĽËťADORES, EXTRATIVISTAS FLORESTAIS"
			64"TRABALHADORES DA MECANIZAÄŹĹĽËťÄŹĹĽËťO AGROPECUÄŹĹĽËťRIA, TRABALHADORES DA MECANIZAÄŹĹĽËťÄŹĹĽËťO FLORESTAL, TRABALHADORES DA IRRIGAÄŹĹĽËťÄŹĹĽËťO E DRENAGEM"
			71"TRABALHADORES DA INDÄŹĹĽËťSTRIA EXTRATIVA E DA CONSTRUÄŹĹĽËťÄŹĹĽËťO CIVIL"
			72"TRABALHADORES DA TRANSFORMAÄŹĹĽËťÄŹĹĽËťO  DE METAIS E DE COMPÄŹĹĽËťSITOS"
			73"TRABALHADORES DA FABRICAÄŹĹĽËťÄŹĹĽËťO E INSTALAÄŹĹĽËťÄŹĹĽËťO ELETRO-ELETRÄŹĹĽËťNICOS EM GERAL"
			74"SUPERVISORES DA MECÄŹĹĽËťNICA DE PRECISÄŹĹĽËťO E INSTRUMENTOS MUSICAIS"
			75"JOALHEIROS E OURIVES, VIDREIROS, CERAMISTAS E AFINS"
			76"TRABALHADORES DAS INDUSTRIAS TÄŹĹĽËťXTEIS, DO CURTIMENTO, DO VESTÄŹĹĽËťARIO E DAS ARTES GRÄŹĹĽËťFICAS"
			77"SUPERVISORES DA INDÄŹĹĽËťSTRIA DA MADEIRA, MOBILIÄŹĹĽËťRIO E DA CARPINTARIA VEICULAR, MARCENEIROS E AFINS, TRABALHADORES DA PREPARAÄŹĹĽËťÄŹĹĽËťO DAS MADEIRAS E DO MOBILIÄŹĹĽËťRIO, TRABALHADORES DA TRANSFORMAÄŹĹĽËťÄŹĹĽËťO DE MADEIRAS E DA FABRICAÄŹĹĽËťAO DO MOBILIÄŹĹĽËťRIO, TRABALHADORES DE MONTAGEM, TRABALHADORES DO ACABAMENTO DE MADEIRA E MOBILIÄŹĹĽËťRIO, TRABALHADORES ARTESANAIS DA MADEIRA E DO MOBILIÄŹĹĽËťRIO, TRABALHADORES DA CARPINTARIA VEICULAR"
			78"SUPERVISORES DE EMBALAGEM E ETIQUETAGEM, OPERADORES DE ROBÄŹĹĽËťS E EQUIPAMENTOS ESPECIAIS, CONDUTORES DE VEÄŹĹĽËťCULOS E OPERADORES DE EQUIPAMENTOS DE ELEVAÄŹĹĽËťÄŹĹĽËťO E DE MOVIMENTAÄŹĹĽËťÄŹĹĽËťO DE CARGAS, TRABALHADORES DE LOGÄŹĹĽËťSTICA E ACOMPANHAMENTO DE SERVIÄŹĹĽËťOS DE TRANSPORTE, EMBALADORES E ALIMENTADORES DE PRODUÄŹĹĽËťÄŹĹĽËťO"
			81"SUPERVISORES DAS INDÄŹĹĽËťSTRIAS QUÄŹĹĽËťMICAS, PETROQUÄŹĹĽËťMICAS E AFINS, OPERADORES DE INSTALAÄŹĹĽËťÄŹĹĽËťES QUÄŹĹĽËťMICAS, PETROQUÄŹĹĽËťMICAS E AFINS, TRABALHADORES DA FABRICAÄŹĹĽËťÄŹĹĽËťO DE MUNIÄŹĹĽËťÄŹĹĽËťO E EXPLOSIVOS QUÄŹĹĽËťMICOS, OPERADORES DE OUTRAS INSTALAÄŹĹĽËťÄŹĹĽËťES QUÄŹĹĽËťMICAS, PETROQUÄŹĹĽËťMICAS E AFINS, OPERADORES DE OPERAÄŹĹĽËťÄŹĹĽËťO UNITÄŹĹĽËťRIA DE LABORATÄŹĹĽËťRIO (TRANSVERSAL PARA TODA INDÄŹĹĽËťSTRIA DE PROCESSOS)  "
			82"SUPERVISORES DA SIDERURGIA E DE MATERIAIS DE CONSTRUÄŹĹĽËťÄŹĹĽËťO, OPERADORES DE INSTALAÄŹĹĽËťÄŹĹĽËťES E EQUIPAMENTOS DE PRODUÄŹĹĽËťÄŹĹĽËťO DE METAIS E LIGAS- 1ÄŹĹĽËť FUSÄŹĹĽËťO, OPERADORES DE INSTALAÄŹĹĽËťÄŹĹĽËťES E EQUIPAMENTOS DE PRODUÄŹĹĽËťÄŹĹĽËťO DE METAIS E LIGAS - 2ÄŹĹĽËť FUSÄŹĹĽËťO, TRABALHADORES DE INSTALAÄŹĹĽËťÄŹĹĽËťES E EQUIPAMENTOS DE MATERIAL DE CONSTRUÄŹĹĽËťÄŹĹĽËťO, CERÄŹĹĽËťMICA E VIDRO, TRABALHADORES ARTESANAIS DA SIDERURGIA E DE MATERIAIS DE CONSTRUÄŹĹĽËťÄŹĹĽËťO"
			83"SUPERVISORES DA FABRICAÄŹĹĽËťÄŹĹĽËťO DE CELULOSE E PAPEL, TRABALHADORES DA PREPARAÄŹĹĽËťÄŹĹĽËťO DE PASTA DE PAPEL, TRABALHADORES DA FABRICAÄŹĹĽËťÄŹĹĽËťO DE PAPEL, CONFECCIONADORES DE PRODUTOS DE PAPEL E PAPELÄŹĹĽËťO"
			84"TRABALHADORES NA FABRICAÄŹĹĽËťÄŹĹĽËťO DE ALIMENTOS, BEBIDAS E FUMO AGROINDUSTRIAIS"
			86"OPERADORES DE INSTALAÄŹĹĽËťÄŹĹĽËťES DE GERAÄŹĹĽËťÄŹĹĽËťO E DISTRIBUIÄŹĹĽËťÄŹĹĽËťO DE ENERGIA TÄŹĹĽËťRMICA, ELÄŹĹĽËťTRICA E NUCLEAR, TRATAMENTO E DISTRIBUIÄŹĹĽËťÄŹĹĽËťO DE ÄŹĹĽËťGUA"
			87"OPERADORES DE OUTRAS INSTALAÄŹĹĽËťÄŹĹĽËťES INDUSTRIAIS, OUTROS TRABALHADORES ELEMENTARES INDUSTRIAIS"
			91"TRABALHADROES  DE REPARAÄŹĹĽËťÄŹĹĽËťO E MANUTENÄŹĹĽËťÄŹĹĽËťO MECÄŹĹĽËťNICA, MECÄŹĹĽËťNICOS DE MANUTENÄŹĹĽËťÄŹĹĽËťO DE MÄŹĹĽËťQUINAS E EQUIPAMENTOS INDUSTRIAIS, COMERCIAIS E RESIDENCIAIS, MECÄŹĹĽËťNICOS DE MANUTENÄŹĹĽËťÄŹĹĽËťO DE MÄŹĹĽËťQUINAS PESADAS E EQUIPAMENTOS AGRÄŹĹĽËťCOLAS, MECÄŹĹĽËťNICOS DE MANUTENÄŹĹĽËťÄŹĹĽËťO VEICULAR, REPARADORES DE INSTRUMENTOS E EQUIPAMENTOS DE PRECISÄŹĹĽËťO, OUTROS MECÄŹĹĽËťNICOS DE MANUTENÄŹĹĽËťÄŹĹĽËťO"
			95"SUPERVISORES DE MANUTENÄŹĹĽËťÄŹĹĽËťO ELETRO-ELETRÄŹĹĽËťNICA E ELETROMECÄŹĹĽËťNICA, ELETRICISTAS-ELETRÄŹĹĽËťNICOS DE MANUTENÄŹĹĽËťÄŹĹĽËťO INDUSTRIAL, COMERCIAL E RESIDENCIAL, ELETRICISTAS-ELETRÄŹĹĽËťNICOS DE MANUTENÄŹĹĽËťÄŹĹĽËťO VEICULAR, MANTENEDORES ELETROMECÄŹĹĽËťNICOS"
			99"OUTROS TRABALHADORES DA CONSERVAÄŹĹĽËťÄŹĹĽËťO E DA CONSERVAÄŹĹĽËťÄŹĹĽËťO E MANUTENÄŹĹĽËťÄŹĹĽËťO (EXCETO TRABALHADORES ELEMENTARES), TRABALHADORES ELEMENTARES DA MANUTENÄŹĹĽËťÄŹĹĽËťO "
			1"MILITARES DA AERONÄŹĹĽËťUTICA"
			2"MILITARES DO EXÄŹĹĽËťRCITO"
			3"MILITARES DA MARINHA"
			4"OFICIAIS DE POLÄŹĹĽËťCIA MILITAR, PRAÄŹĹĽËťAS DE POLÄŹĹĽËťCIA MILITAR"
			5"OFICIAIS DE BOMBEIRO MILITAR, PRAÄŹĹĽËťAS DE BOMBEIRO MILITAR"
;
	
#delimit cr

			label values industry industry_l
			label values ciiu ciiu_l	
			label values occupation occupation_l			

qui compress 

** Create notes for variables
		notes age_c: age calculated by IBGE
		notes wage_r: gross monthly wage for salaried workers and self employed. regular
		notes wage_a: gross monthly wage for salaried workers and self employed. actual
		notes wage_a_im: gross monthly wage for salaried workers and self employed. actual - imputed
		notes grau_ed: original variable V307 
		notes serie_ed: original variable V310
		notes yeduc: Categories of years of scholling constructed by IGBE
		notes yeduc2: Categories of years of scholling constructed by IGBE
		notes ssecurity: constructed using V416 and V425. Contributed to instituto de providencia, for this work?
	
qui save "$dir\\PME`x'_TOTAL_FINAL.dta", replace

}


