*Para correr do files
*Mayra Saenz
*Agosto, 2017

* last modification: Angela Lopez /05/19/21

global surveysFolder "\\Sdssrv03\surveys"
global gitArm "C:\Users\ALOP\OneDrive - Inter-American Development Bank Group\Desktop\Git_repositories\armonizacion_microdatos_encuestas_hogares_scl"

local paises ARG BHS BOL BRA BRB BLZ BRA CHL COL CRI ECU SLV GTM GUY HTI HND JAM MEX NIC PAN PRY PER DOM SUR TTO URY VEN 
local anos 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019
local paises BHS BRB SLV GUY HND NIC PRY DOM SUR TTO VEN



foreach pais of local paises {
	foreach ano of local anos {
		
		include "${gitArm}\_DOCS\Directorio HS LAC.do" 		
		do "${gitArm}\\`pais'\\`encuestas'\program\\`pais'_`ano'`rondas'_variablesBID.do"		
			
	}
}
			
			
	


