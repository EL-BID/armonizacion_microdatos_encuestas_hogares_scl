/*
** Description: Read the rawdata of the PME (Brazil) 2002 - Present
** Note: The user has to change some values in the local variables if is the case.
** by Julian Messina and Carlos Prada 
*/

clear
set mem 450m
set more off


** Declare Global Directory **

global dir = "${surveysFolder}\Users\wb377246\Documents\Julian\LM_W\BRA\Data"


/* Loop # 1 to use all the complete information. All 12 months of information*/

*** Declaring the global and local variables to inflie the raw data

local  year     = 2002   /*Initial year. No change*/
local  f_year   = 2009   /*Last year of complete observations. 12 months. (change if we have addtional year of complete obs)*/	
local  f_month  = 12     /*Final month. No change*/

	while `year' <= `f_year' {
	
	** Define the raw data location

		cd "$dir\\`year'"
			local month = 1
				while `month'<=`f_month' {

	** Infile the raw data using the dictionary located in the Data folder

	qui infile using "$dir\DictionaryPME.dct", using ("PME`month'`year'.txt") clear

	keep  V035 V040 V050 V055 V060 V063 V070 V072 V075 V201 V203 V204 V208 V214 V215 V224 V234 V307 V308 V309 V310 V311 V312 V301 V302 V307 V309 V310 V401 V406 V407A V408A V409 V411 V412 V417 V4182  V415 V416 V4182 V4191 VI4191 V4241 VI4241 V425 V426 V429 V4231 V4271 V4272 V4273 V4275 V4274 V428 V468 VD1 VDAE1 VDAE2

	qui save PME`year'`month'.dta, replace

	local month=`month'+1
	}
local year = `year' + 1
}

*--- End subprogram ---*

/* Loop # 2 to use the last year of information. Generally incomplete (less than 12 months)*/

*** Declaring the local variables to inflie the raw data ***

local ff_year   = 2010  /*This is the last of information we have. (change if we have addtional year of complete obs)*/	
local f_year_f  = 2010  
local ff_month  = 11    /*This is the last of information we have. (change if we have addtional month of obs)*/	
	
	while `ff_year'  <= `f_year_f' {
	
	** Define the raw data location
	cd "$dir\\`ff_year'"
			local month = 1
				while `month'<=`ff_month' {

	** Infile the raw data using the dictionary located in the Data folder

	qui infile using "$dir\\DictionaryPME.dct", using ("PME`month'`ff_year'.txt") clear

	keep  V035 V040 V050 V055 V060 V063 V070 V072 V075 V201 V203 V204 V208 V214 V215 V224 V234 V307 V308 V309 V310 V311 V312 V301 V302 V307 V309 V310 V401 V406 V407A V408A V409 V411 V412 V417 V4182  V415 V416 V4182 V4191 VI4191 V4241 VI4241 V425 V426 V429 V4231 V4271 V4272 V4273 V4275 V4274 V428 V468 VD1 VDAE1 VDAE2

	qui save PME`ff_year'`month'.dta, replace

	local month=`month'+1
	}
local ff_year = `ff_year' + 1
}

*--- End subprogram ---*

*--- End Program ---*


