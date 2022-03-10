*---------------------------------------------------------------*
*Generación de las nuevas líneas de pobreza de 3.1 USD diarios
*---------------------------------------------------------------*


*Elaboracion: Daniela Zuluaga
*Fecha: Julio de 2017

*Versiones anteriores elaboradas por: Yessenia Loayza, Marcela Rubio y Mayra Saenz se encuentran disponibles en: \\Sdssrv03\surveys\general_documentation\data_externa\poverty\old_files


clear
global ruta ="\\Sdssrv03\surveys\general_documentation\data_externa\poverty\International_Poverty_Lines\"



*----------------------------------------------------------------------------------------------------------------*
*----------------------------------------------------------------------------------------------------------------*

*Purchasing power parity conversion factor (PPP) and Official exchange rate (LCU per US$, period average)

*----------------------------------------------------------------------------------------------------------------*
*----------------------------------------------------------------------------------------------------------------*

	* Source: World Development Indicators
	* Source Organization ppp: World Bank, International Comparison Program database.
	* Source Organization tc: International Monetary Fund, International Financial Statistics.
	
	* Esta base no se debe modificar, ya que el anio base es 2011

	*Tipo de cambio (tc) = PA.NUS.FCRF

	*2011 benchmark: PA.NUS.PRVT.PP
	*2005 benchmark: PA.NUS.PRVT.PP.05 (Ya no se utiliza y por lo tanto no se tiene en cuenta)
	

	ssc install wbopendata, replace

		wbopendata, indicator(PA.NUS.PRVT.PP; PA.NUS.FCRF) clear long
		**Lina Arias 06/24/2021
		rename pa_nus_prvt_pp pa_nus_prvt_pp1
		rename pa_nus_fcrf pa_nus_fcrf1
		tempfile ppp
		save "`ppp'.dta", replace  

		use "$ruta\1_ppp&tc_WDI.dta", clear
		merge 1:1 countrycode year using "`ppp'.dta"
		replace pa_nus_prvt_pp=pa_nus_prvt_pp1 if year==2020 & pa_nus_prvt_pp==.
		drop *1 _merge
		*--------------------------*
		saveold "$ruta\1_ppp&tc_WDI.dta", replace


*----------------------------------------------------------------------------------------------------------------*
*----------------------------------------------------------------------------------------------------------------*

*  Consumer price index (CPI)

*----------------------------------------------------------------------------------------------------------------*
*----------------------------------------------------------------------------------------------------------------*

	* Source: International Monetary Fund, World Economic Outlook Database (WEO), April 2020. Inflation, average consumer prices (Index)

		
		clear all

		import excel "$ruta\For_2_cpi_IMF.xlsx", sheet ("For_Stata") firstrow case(lower)

		reshape long y, i( countrycode countryname) j(year)

		rename y cpi

		label var cpi "Consumer Price Index (Source: IMF except ARG 90-96, 07-15)"

*----------------------------------------------------------------------------------------------------------------*
*----------------------------------------------------------------------------------------------------------------*

*Venezuela

*----------------------------------------------------------------------------------------------------------------*
*----------------------------------------------------------------------------------------------------------------*
		
		*se le aplica inflación al cpi 2016 para obtener el dato 2017 y posteriormente 2018
		**Datos de inflación suministrados por Marcos Robles, ver archivo Inflacion venezula.xls
		replace cpi=cpi[_n-1]*(1+1087.52/100) if countrycode=="VEN"& year==2017
		replace cpi=cpi[_n-1]*(1+929789.5/100) if countrycode=="VEN"& year==2018
		replace cpi=cpi[_n-1]*(1+17365.2243211428/100) if countrycode=="VEN"& year==2019 //valor observado Asamblea Nacional.
	
		*Dividir las líneas internacionales de pobreza entre 100,000, dado que las nuevas bases están expresadas en la nueva moneda (bolívares soberanos en lugar de bolívares fuertes). En el 2019 la base de datos está expresada en miles de Bolívares Soberanos (BS).
		
		replace cpi=cpi/100000 if ((countrycode=="VEN")&(year==2018))
        replace cpi=cpi/(100000*1000) if ((countrycode=="VEN")&(year==2019))

		saveold "$ruta\2_cpi_IMF.dta", v(12) replace  



*----------------------------------------------------------------------------------------------------------------*
*----------------------------------------------------------------------------------------------------------------*

*For Argentina, the CPI from an alternative source is used 2011 = 100.

*----------------------------------------------------------------------------------------------------------------*
*----------------------------------------------------------------------------------------------------------------*

	*Source: http://www.mit.edu/~afc/papers/FillingTheGap_es.pdf 
	*Page 5

		
		clear

		import excel "$ruta\For_3_cpi_ARG.xlsx", sheet("For_STATA") firstrow case(lower)

		*Annual average 
			collapse (mean) yoy , by(year)
			
			*tsset year



		*Merge with IMF data
			g countrycode = "ARG"
			merge 1:1 countrycode year using "$ruta\2_cpi_IMF.dta"
			drop _merge
			order countryname countrycode year cpi, first
			encode countrycode, gen(countryscode)
			tsset  countryscode year
			
	   /*HACER ESTE PROCEDIMIENTO A PARTIR DEL 2017*
	   Para lograr empatar las bases de datos Alternas con la del IMF a partir de 2017 se debe sacar la tasa de inflación 2016-2016,
	   con datos del IMF de la siguiente manera*/


replace yoy=(cpi[_n]-cpi[_n-1])/cpi[_n-1]*100 if countrycode=="ARG" &year>=2017 & yoy==.


		*CPI Argentina con datos alternos
         
			g cpi_arg = .
			replace cpi_arg=100 if year==2011 & countrycode=="ARG"

			*Se cambia el intervalo cada vez que se haga este procedimiento para el siguiente año, se le agrega un año más*
			forval i = 2012/2020 {
			replace cpi_arg=cpi_arg[_n-1]*(1+yoy[_n]/100) if year==`i'  & countrycode=="ARG"
			}
			
			foreach i of numlist 2010/1990 {
			replace cpi_arg=cpi_arg[_n+1]/(1+yoy[_n+1]/100) if year==`i'  & countrycode=="ARG"
			}
				
			
		*Repace CPI Argentina in IMF database

		replace cpi = cpi_arg if year >= 1990 & countrycode == "ARG"

		keep countrycode countryname year cpi



		*Dataset with CPI updated for Argentina

		saveold "$ruta\3_cpi_IMF&ARG", v(12) replace 

		


		
*________________________________________________________________________________________________________________________________________*
*________________________________________________________________________________________________________________________________________*
* 
*   DATASET FOR FUTURE UPDATES:        

*                                       ppp_cpi.dta

* Just the currently year must be updated in this dataset. Data from past years must not be modified.
*________________________________________________________________________________________________________________________________________*
*________________________________________________________________________________________________________________________________________*

	
	use "$ruta\1_ppp&tc_WDI.dta", clear
	
	keep if  region=="LCN"
	
	keep countrycode year pa_nus_prvt_pp 

	rename pa_nus_prvt_pp      ppp11

	

	*=========================================================================================================*
	*PPP 2011 (New poverty lines use ppp 2011, but old ones use ppp 2005)
	*=========================================================================================================*
				
	replace ppp11 = . if year != 2011
	bys countrycode: egen ppp_2011= max(ppp11) 

	drop ppp11 year
	
	duplicates drop countrycode ppp_2011, force
	
	*=========================================================================================================*
	*Merge PPP 2011 with CPI IMF (for all countries, but ARG) and Alternative CPI (just for ARG)
	*=========================================================================================================*
	
	merge 1:m countrycode using "$ruta\3_cpi_IMF&ARG.dta"
	drop _merge
	
	*=========================================================================================================*
	*Merge PPP 2011 and CPI with exchange rate (pa_nus_fcrf)
	*=========================================================================================================*
	
	
	merge m:m countrycode year using "$ruta\1_ppp&tc_WDI.dta", keepusing(countrycode year pa_nus_prvt_pp pa_nus_fcrf)
	
	rename pa_nus_prvt_pp ppp_wdi2011
	label var ppp_wdi2011 "Purchasing power parity conversion factor (PPP)2011"
	rename pa_nus_fcrf    tc_wdi 
	label var tc_wdi "Official exchange rate (LCU per US$, period average)"
	
	drop if _merge ==2
	drop _merge
	drop if year <1989 | year > 2020 //There is no harmonized data for anio_cs previous to 1989
	
	*=========================================================================================================*
	*Se mantienen los 26 países con los que trabaja el Banco
	*=========================================================================================================*
	
	keep if countrycode=="ARG" | countrycode=="BOL" | countrycode=="BRA" | countrycode=="CHL" | countrycode=="COL" | countrycode=="CRI" | countrycode=="DOM" | /// 
    countrycode=="ECU" | countrycode=="GTM" | countrycode=="HND" | countrycode=="MEX" | countrycode=="NIC" | countrycode=="PAN" | countrycode=="PER" | ///
	countrycode=="PRY" | countrycode=="SLV" | countrycode=="URY" | countrycode=="VEN" | countrycode=="JAM" | countrycode=="BLZ" | countrycode=="BRB" | ///
	countrycode=="TTO" | countrycode=="HTI" | countrycode=="BHS" | countrycode=="GUY" | countrycode=="SUR"
	
	order countryname countrycode year ppp_2011 cpi ppp_wdi2011, first
	
	sort countrycode year
	
	saveold "$ruta\\4_ppp_tc_cpi.dta", v(12)replace

	*/
	
	
	
	
	
*----------------------------------------------------------------------------------------*
*----------------------------------------------------------------------------------------*

*                Poverty Lines for Latin America and Caribbean countries                 *

*----------------------------------------------------------------------------------------*
*----------------------------------------------------------------------------------------*


	use "$ruta\\4_ppp_tc_cpi.dta", clear  // **This dataset cannot be modified for years before 2016**
	
	*Same names as in the harmonized datasets
	
	rename countrycode pais_c
	rename year        anio_c



	*=========================================================================================================*
	*                             CPI 2011 = 100
	*=========================================================================================================*


	separate cpi if anio_c==2011, by(anio_c)

	
	bys pais_c: egen cpi_11= max(cpi2011) 

	gen cpi_2011 = cpi/cpi_11

	drop cpi20*  cpi_11




	*=========================================================================================================*
	*                     Poverty line 1.9, 3.1 and 5 USD PPP 2011
	*=========================================================================================================*
	gen lp19_2011 = 1.9*(365/12)*cpi_2011*ppp_2011
	gen lp31_2011 = 3.1*(365/12)*cpi_2011*ppp_2011
	gen lp5_2011  = 5*(365/12)*cpi_2011*ppp_2011


	foreach var of varlist cpi_2011 lp19_2011 lp31_2011 lp5_2011 tc_wdi {

		
		format `var' %18.4f
		}
	
	* Save in long format to merge with harmonized dataset
	
	saveold "$ruta\\5_International_Poverty_Lines_LAC_long.dta", v(12) replace


	*=========================================================================================================*
	*Generating datasets related to: cpi_2011 lp25_2011 lp4_2011 lp31_2011 lp5_2011 
	*=========================================================================================================*


	local n= 1

	foreach var of varlist cpi_2011 lp19_2011 lp31_2011 lp5_2011 tc_wdi {

		preserve

		keep pais_c anio_c `var'

		rename `var' y

		reshape wide y, i( pais_c) j(anio_c)

		g var = "`var'"
		
		order var, first
		format y1989-y2020 %18.4f
		
		export excel using "$ruta\\5_International_Poverty_Lines_LAC.xls", sheet("`var'") cell(B2) firstrow(variables) sheetmodify
		
		saveold "$ruta\\`n'.dta", v(12) replace

		restore

	local n = `n'+1

	}

	*=========================================================================================================*
	*Generating datasets related to: ppp2011 
	*=========================================================================================================*

	keep pais_c ppp_20*
	
	duplicates drop pais_c, force
	saveold "$ruta\\9.dta", v(12) replace
	export excel using "$ruta\\5_International_Poverty_Lines_LAC.xls", sheet("ppp") cell(B2) firstrow(variables) sheetmodify

	*==========================================================================================================*
	*                                                  Append datasets  
	*==========================================================================================================*
	  
	  use "$ruta\\1.dta", clear
	  
			foreach num of numlist 2/5 {
				   append using "$ruta\\`num'.dta"
			}

			
	order var pais_c , first

	format y1989-y2020 %18.4f
	

	saveold "$ruta\\5_International_Poverty_Lines_LAC_wide.dta", v(12) replace

	*==========================================================================================================*
	*Erase datasets that won't be used
	*==========================================================================================================*

	foreach num of numlist 1,2,3,4,5,9 {
				   erase "$ruta\\`num'.dta"
			}



