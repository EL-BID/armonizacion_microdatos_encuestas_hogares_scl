/*
** Description: This loop append monthly observations in annual observations. 
		    Use two different loops. The user has to change only specific inputs in local variables.
** Survey: PME (2002-2009)
** by Julian Messina and Carlos Prada
*/

clear
set mem 650m, perm
set more off
*set trace on

**Note: this local should be change by the user
local propiodir = "C:\Users\wb377246\Documents\Julian\LM_W"

*** Declare Global Directory
global dir	  = "`propiodir'\BRA\Data"

/* Loop # 1 to use all the complete information. All 12 months of information. This loop append monthly observations in annual obs*/

local year    = 2002  /*Initial year. No change*/
local f_year  = 2009  /*Last year of complete observations. 12 months. (change if we have addtional year of complete obs)*/	
local f_month = 12    /*Final month. No change*/
local i_month = 1	    /*Initial month. No change*/
	

while `year' <= `f_year' {
	
	** Define the raw data location

		cd "$dir\\`year'"
			
		use "PME`year'`i_month'.dta", clear

			local month = `i_month' + 1
				while `month'<=`f_month' {
			qui append using "PME`year'`month'.dta"
		qui compress

local month=`month'+1

	}

	save $dir\\PME`year'_TOTAL.dta, replace
local year = `year' + 1
}

*--- End subprogram ---*

/* Loop # 2 to use the last year of information. Generally incomplete (less than 12 months)*/

** Declaring the local variables to append the data **

local ff_year   = 2010  /*This is the last of information we have. Change if we have addtional year of complete obs.*/	
local f_year_f  = 2010  
local ff_month  = 11    /*This is the last of information we have. Change if we have more information*/	
local imonth    = 1


while `ff_year' <= `f_year_f' {

** Define the raw data location **

		cd "$dir\\`ff_year'"
			
		use "PME`ff_year'`imonth'.dta", clear

			local month_f = `imonth' + 1
				while `month_f'<=`ff_month' {
			qui append using "PME`ff_year'`month_f'.dta"
		qui compress

local month_f=`month_f' +1

	}

	qui save "$dir\\PME`ff_year'_TOTAL.dta", replace
local ff_year = `ff_year' + 1
}

*--- End subprogram ---*

*--- End Program ---*
