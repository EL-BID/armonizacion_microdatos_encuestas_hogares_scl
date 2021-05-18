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
			1"AGRICULTURA, PECU�RIA E SERVI�OS RELACIONADOS COM ESSAS ATIVIDADES"
			2"SILVICULTURA, EXPLORA��O FLORESTAL E SERVI�OS RELACIONADOS COM ESTAS ATIVIDADES"
			5"PESCA, AQ�ICULTURA E ATIVIDADES DOS SERVI�OS RELACIONADOS COM ESTAS ATIVIDADES"
			10"EXTRA��O DE CARV�O MINERAL"
			11"EXTRA��O DE PETR�LEO E SERVI�OS CORRELATOS"
			12"EXTRA��O DE MINERAIS RADIOATIVOS"
			13"EXTRA��O DE MINERAIS MET�LICOS"
			14"EXTRA��O DE MINERAIS N�O-MET�LICOS"
			15"FABRICA��O DE PRODUTOS ALIMENT�CIOS E BEBIDAS"
			16"FABRICA��O DE PRODUTOS DO FUMO"
			17"FABRICA��O DE PRODUTOS T�XTEIS"
			18"CONFEC��O DE ARTIGOS DO VESTU�RIO E ACESS�RIOS"
			19"PREPARA��O DE COUROS E FABRICA��O DE ARTEFATOS DE COURO, ARTIGOS DE VIAGEM E CAL�ADOS"
			20"FABRICA��O DE PRODUTOS DE MADEIRA"
			21"FABRICA��O DE CELULOSE, PAPEL E PRODUTOS DE PAPEL"
			22"EDI��O, IMPRESS�O E REPRODU��O DE GRAVA��ES"
			23"COQUERIAS"
			24"FABRICA��O DE PRODUTOS QU�MICOS"
			25"FABRICA��O DE PRODUTOS DE BORRACHA E PL�STICO"
			26"FABRICA��O DE PRODUTOS DE MINERAIS N�O-MET�LICOS"
			27"METALURGIA B�SICA"
			28"FABRICA��O DE PRODUTOS DE METAL - EXCLUSIVE M�QUINAS E EQUIPAMENTOS"
			29"FABRICA��O DE M�QUINAS E EQUIPAMENTOS"
			30"FABRICA��O DE M�QUINAS E EQUIPAMENTOS DE SISTEMAS ELETR�NICOS PARA PROCESSAMENTO DE DADOS"
			31"FABRICA��O DE M�QUINAS, APARELHOS E MATERIAIS EL�TRICOS"
			32"FABRICA��O DE MATERIAL ELETR�NICO E DE APARELHOS E EQUIPAMENTOS DE COMUNICA��ES "
			33"FABRICA��O DE EQUIPAMENTOS DE INSTRUMENTA��O M�DICO-HOSPITALARES, INSTRUMENTO DE PRECIS�O E �PTICOS, EQUIPAMENTOS PARA AUTOMA��O INDUSTRIAL, CRON�METROS E REL�GIOS"
			34"FABRICA��O E MONTAGEM DE VE�CULOS AUTOMOTORES, REBOQUES E CARROCERIAS"
			35"FABRICA��O DE OUTROS EQUIPAMENTOS DE TRANSPORTES"
			36"FABRICA��O DE M�VEIS E IND�STRIAS DIVERSAS "
			37"RECICLAGEM"
			40"PRODU��O E DISTRIBUI��O DE ELETRICIDADE, G�S E �GUA"
			41"CAPTA��O, TRATAMENTO E DISTRIBUI��O DE �GUA"
			45"CONSTRU��O"
			50"COM�RCIO E REPARA��O DE VE�CULOS AUTOMOTORES E MOTOCICLETAS;  E COM�RCIO A VAREJO DE COMBUST�VEIS "
			53"INTERMEDI�RIOS DO COM�RCIO, COM�RCIO E REPARA��O DE OBJETOS PESSOAIS E DOM�STICOS"
			55"ALOJAMENTO E ALIMENTA��O"
			60"TRANSPORTE TERRESTRE"
			61"TRANSPORTE AQUAVI�RIO"
			62"TRANSPORTE A�REO"
			63"ATIVIDADES ANEXAS E AUXILIARES DO TRANSPORTE E AG�NCIAS DE VIAGENS"
			64"CORREIOS E TELECOMUNICA��ES"
			65"INTERMEDIA��O FINANCEIRA, EXCLUSIVE DE SEGUROS E PREVID�NCIA PRIVADA"
			66"SEGUROS E PREVID�NCIA PRIVADA"
			67"ATIVIDADES AUXILIARES DA INTERMEDIA��O FINANCEIRA"
			70"ATIVIDADES IMOBILI�RIAS"
			71"ALUGUEL DE VE�CULOS, M�QUINAS E EQUIPAMENTOS SEM CONDUTORES OU OPERADORES E DE OBJETOS PESSOAIS E DOM�STICOS"
			72"ATIVIDADES DE INFORM�TICAS E CONEXAS"
			73"PESQUISA E DESENVOLVIMENTO"
			74"SERVI�OS PRESTADOS PRINCIPALMENTE �S EMPRESAS"
			75"ADMINISTRA��O P�BLICA, DEFESA E SEGURIDADE SOCIAL                       "
			80"EDUCA��O"
			85"SA�DE E SERVI�OS SOCIAIS"
			90"LIMPEZA URBANA E ESGOTO; E ATIVIDADES CONEXAS"
			91"ATIVIDADES ASSOCIATIVAS"
			92"ATIVIDADES RECREATIVAS, CULTURAIS E DESPORTIVAS"
			93"SEVI�OS PESSOAIS"
			95"SERVI�OS DOM�STICOS"
			99"ORGANISMOS INTERNACIONAIS E OUTRAS INSTITUI��ES EXTRATERRITORIAIS"
			0"ATVIDADES N�O ESPECIFICADAS";

#delimit ;	

label define occupation_l  
			100"MEMBROS SUPERIORES DO PODER LEGISLATIVO, EXECUTIVO E JUDICI�RIO; DIRIGENTES DE PRODU��O, OPERA��ES E APOIO DA ADMINISTRA��O P�BLICA"
			11"CHEFES DE PEQUENAS POPULA��ES, DIRIGENTES E ADMINISTRADORES DE ORGANIZA��O DE INTERESSE P�BLICO"
			12"DIRETORES GERAIS DE EMPRESA E ORAGNIZA��ES (EXCETO DE INTERESSE P�BLICO) "
			13"GERENTES DE PRODU��O E OPERA��ES E DE �REAS DE APOIO"
			101"PROFISSIONAIS DE N�VEL SUPERIOR (EXCLUSIVE DA NAVEGA��O A�REA, MAR�TIMA E FLUVIAL, DAS COMUNICA��ES E DAS ARTES, E MEMBROS DE CULTOS RELIGIOSOS)"
			102"PROFISSIONAIS EM NAVEGA��O A�REA, MAR�TIMA E FLUVIAL "
			26"PROFISSIONAIS DA COMUNICA��O, PROFISSIONAIS DE ESPET�CULOS E DAS ARTES"
			103"MEMBROS DE CULTOS RELIGIOSOS E AFINS"
			30"T�CNICOS ELETROMEC�NICOS E MECATR�NICOS, T�CNICOS EM LABORAT�RIO "
			31"T�CNICOS DE N�VEL M�DIO DA ENGENHARIA, CI�NCIAS F�SICAS, QU�MICAS E AFINS"
			32"T�CNICOS DE N�VEL M�DIO DA DAS CI�NCIAS BIOL�GICAS, BIOQU�MICAS DA SA�DE E AFINS"
			33"PROFESSORES LEIGOS E DE N�VEL M�DIO NA EDUCA��O INFANTIL, NO ENSINO FUNDAMENTAL, PROFISSIONALIZANTE E NAS ESCOLAS LIVRES"
			104"T�CNICOS EM NAVEGA��O A�REA, MAR�TIMA, FLUVIAL E METROFERROVI�RIA"
			34"T�CNICOS EM TRANSPORTES (LOG�STICA)"
			35"T�CNICOS DAS CI�NCIAS ADMINISTRATIVAS"
			37"T�CNICOS DE N�VEL M�DIO DOS SERVI�OS CULTURAIS DAS COMUNICA��ES E DOS DESPORTO"
			39"OUTROS T�CNICOS DE NIVEL M�DIO EM OPERA��ES INDUSTRIAIS"
			41"TRABALHADORES DOS DE SERVI�OS ADMINISTRATIVOS (Exceto de atendimento ao p�blico)"
			42"TRABALHADORES DOS DE SERVI�OS ADMINISTRATIVOS (Somente de atendimento ao p�blico)"
			51"SUPERVISORES DOS SERVI�OS E DO COM�RCIO, TRABALHADORES DOS SERVI�OS DE HOTELARIA E ALIMENTA��O, TRABALHADORES NOS SERVI�OS DE ADMINISTRA��O, CONSERVA��O E MANUTEN��O DE EDIF�CIOS E LOGRADOUROS, TRABALHADORES DOS SERVI�OS DE SA�DE, ATENDENTE DE CRECHE E ACOMPANHANTE DE IDOSOS E TRABALHADORES DOS SERVI�OS FUNER�RIOS, TRABALHADORES NOS SERVI�OS DE PROTE��O E SEGURAN�A, OUTROS TRABALHADORES DE SERVI�OS DIVERSOS"
			105"TRABALHADORES DOS SERVI�OS DE TRANSPORTE E TURISMO"
			106"TRABALHADORES DOS SERVI�OS DOM�STICOS EM GERAL"
			107"TRABALHADORES NOS SERVI�OS DE EMBELEZAMENTO E CUIDADOS PESSOAIS (EXCLUSIVE ATENDENTE DE CRECHE E ACOMPANHANTE DE IDOSOS E TRABALHADORES DOS SERVI�OS FUNER�RIOS)"
			108"SUPERVISORES DE VENDAS E DE PRESTA��O DE SERVI�OS DO COM�RCIO"
			109"VENDEDORES, DEMONSTRADORES"
			110"REPOSITORES, REMARCADORES DO COM�RCIO"
			111"INSTALADORES DE PRODUTOS E ACESS�RIOS"
			112"VENDEDORES AMBULANTES E CAMEL�S"
			61"PRODUTORES AGROPECU�RIOS EM GERAL, PRODUTORES AGR�COLAS, PRODUTORES EM PECU�RIA"
			62"SUPERVISORRES NA EXPLORA��O AGROPECU�RIA, TRABALHADORES NA EXPLORA��O AGROPECU�RIA EM GERAL TRABALHADORES AGR�COLAS, TRABALHADORES NA PECU�RIA"
			63"SUPERVISORES NA EXPLORA��O FLORESTAL, CA�A E PESCA, PESCADORES, CA�ADORES, EXTRATIVISTAS FLORESTAIS"
			64"TRABALHADORES DA MECANIZA��O AGROPECU�RIA, TRABALHADORES DA MECANIZA��O FLORESTAL, TRABALHADORES DA IRRIGA��O E DRENAGEM"
			71"TRABALHADORES DA IND�STRIA EXTRATIVA E DA CONSTRU��O CIVIL"
			72"TRABALHADORES DA TRANSFORMA��O  DE METAIS E DE COMP�SITOS"
			73"TRABALHADORES DA FABRICA��O E INSTALA��O ELETRO-ELETR�NICOS EM GERAL"
			74"SUPERVISORES DA MEC�NICA DE PRECIS�O E INSTRUMENTOS MUSICAIS"
			75"JOALHEIROS E OURIVES, VIDREIROS, CERAMISTAS E AFINS"
			76"TRABALHADORES DAS INDUSTRIAS T�XTEIS, DO CURTIMENTO, DO VEST�ARIO E DAS ARTES GR�FICAS"
			77"SUPERVISORES DA IND�STRIA DA MADEIRA, MOBILI�RIO E DA CARPINTARIA VEICULAR, MARCENEIROS E AFINS, TRABALHADORES DA PREPARA��O DAS MADEIRAS E DO MOBILI�RIO, TRABALHADORES DA TRANSFORMA��O DE MADEIRAS E DA FABRICA�AO DO MOBILI�RIO, TRABALHADORES DE MONTAGEM, TRABALHADORES DO ACABAMENTO DE MADEIRA E MOBILI�RIO, TRABALHADORES ARTESANAIS DA MADEIRA E DO MOBILI�RIO, TRABALHADORES DA CARPINTARIA VEICULAR"
			78"SUPERVISORES DE EMBALAGEM E ETIQUETAGEM, OPERADORES DE ROB�S E EQUIPAMENTOS ESPECIAIS, CONDUTORES DE VE�CULOS E OPERADORES DE EQUIPAMENTOS DE ELEVA��O E DE MOVIMENTA��O DE CARGAS, TRABALHADORES DE LOG�STICA E ACOMPANHAMENTO DE SERVI�OS DE TRANSPORTE, EMBALADORES E ALIMENTADORES DE PRODU��O"
			81"SUPERVISORES DAS IND�STRIAS QU�MICAS, PETROQU�MICAS E AFINS, OPERADORES DE INSTALA��ES QU�MICAS, PETROQU�MICAS E AFINS, TRABALHADORES DA FABRICA��O DE MUNI��O E EXPLOSIVOS QU�MICOS, OPERADORES DE OUTRAS INSTALA��ES QU�MICAS, PETROQU�MICAS E AFINS, OPERADORES DE OPERA��O UNIT�RIA DE LABORAT�RIO (TRANSVERSAL PARA TODA IND�STRIA DE PROCESSOS)  "
			82"SUPERVISORES DA SIDERURGIA E DE MATERIAIS DE CONSTRU��O, OPERADORES DE INSTALA��ES E EQUIPAMENTOS DE PRODU��O DE METAIS E LIGAS- 1� FUS�O, OPERADORES DE INSTALA��ES E EQUIPAMENTOS DE PRODU��O DE METAIS E LIGAS - 2� FUS�O, TRABALHADORES DE INSTALA��ES E EQUIPAMENTOS DE MATERIAL DE CONSTRU��O, CER�MICA E VIDRO, TRABALHADORES ARTESANAIS DA SIDERURGIA E DE MATERIAIS DE CONSTRU��O"
			83"SUPERVISORES DA FABRICA��O DE CELULOSE E PAPEL, TRABALHADORES DA PREPARA��O DE PASTA DE PAPEL, TRABALHADORES DA FABRICA��O DE PAPEL, CONFECCIONADORES DE PRODUTOS DE PAPEL E PAPEL�O"
			84"TRABALHADORES NA FABRICA��O DE ALIMENTOS, BEBIDAS E FUMO AGROINDUSTRIAIS"
			86"OPERADORES DE INSTALA��ES DE GERA��O E DISTRIBUI��O DE ENERGIA T�RMICA, EL�TRICA E NUCLEAR, TRATAMENTO E DISTRIBUI��O DE �GUA"
			87"OPERADORES DE OUTRAS INSTALA��ES INDUSTRIAIS, OUTROS TRABALHADORES ELEMENTARES INDUSTRIAIS"
			91"TRABALHADROES  DE REPARA��O E MANUTEN��O MEC�NICA, MEC�NICOS DE MANUTEN��O DE M�QUINAS E EQUIPAMENTOS INDUSTRIAIS, COMERCIAIS E RESIDENCIAIS, MEC�NICOS DE MANUTEN��O DE M�QUINAS PESADAS E EQUIPAMENTOS AGR�COLAS, MEC�NICOS DE MANUTEN��O VEICULAR, REPARADORES DE INSTRUMENTOS E EQUIPAMENTOS DE PRECIS�O, OUTROS MEC�NICOS DE MANUTEN��O"
			95"SUPERVISORES DE MANUTEN��O ELETRO-ELETR�NICA E ELETROMEC�NICA, ELETRICISTAS-ELETR�NICOS DE MANUTEN��O INDUSTRIAL, COMERCIAL E RESIDENCIAL, ELETRICISTAS-ELETR�NICOS DE MANUTEN��O VEICULAR, MANTENEDORES ELETROMEC�NICOS"
			99"OUTROS TRABALHADORES DA CONSERVA��O E DA CONSERVA��O E MANUTEN��O (EXCETO TRABALHADORES ELEMENTARES), TRABALHADORES ELEMENTARES DA MANUTEN��O "
			1"MILITARES DA AERON�UTICA"
			2"MILITARES DO EX�RCITO"
			3"MILITARES DA MARINHA"
			4"OFICIAIS DE POL�CIA MILITAR, PRA�AS DE POL�CIA MILITAR"
			5"OFICIAIS DE BOMBEIRO MILITAR, PRA�AS DE BOMBEIRO MILITAR"
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


