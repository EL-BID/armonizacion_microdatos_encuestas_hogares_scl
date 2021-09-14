*Para correr do files
*Mayra Saenz
*Agosto, 2017

* last modification: Angela Lopez /05/19/21

local paises ARG BHS BOL BRA BRB BLZ BRA CHL COL CRI ECU SLV GTM GUY HTI HND JAM MEX NIC PAN PRY PER DOM SUR TTO URY VEN 
local anos 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019
local paises ARG BHS BOL BRA BRB BLZ BRA CHL COL CRI ECU SLV GTM GUY HTI HND JAM MEX NIC PAN PRY PER DOM SUR TTO URY VEN
local anos 2000 2001 2002 2003 2004


foreach pais of local paises {
	foreach ano of local anos {
		
		include "${gitFolder}\armonizacion_microdatos_encuestas_hogares_scl\_DOCS\Directorio HS LAC.do" 		
		do "${gitFolder}\armonizacion_microdatos_encuestas_hogares_scl\\`pais'\\`encuestas'\program\\`pais'_`ano'`rondas'_variablesBID.do"		
			
	}
}
	
	/*
	local paises  URY
local anos 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019

foreach pais of local paises {
	foreach ano of local anos {
		include "$gitFolder/armonizacion_microdatos_encuestas_hogares_scl\_DOCS\Directorio HS LAC.do" 	
		cap use "\\sdssrv03\surveys\harmonized\\`pais'\\`encuestas'\data_arm\\`pais'_`ano'`rondas'_BID", clear
		tab afroind_ci
		
	}
}
*/