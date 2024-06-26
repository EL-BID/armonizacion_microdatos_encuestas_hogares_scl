/*====================================================================
project:       Directorio de encuestas y paises LAC
Author:        Angela Lopez 
----------------------------------------------------------------------
Creation Date:    19 Jul 2018 - 11:14:13
====================================================================*/

/*====================================================================
                        0: Program set up
====================================================================*/

* Encuestas 

	if "`pais'" == "ARG"   & ("`ano'" < "2003")                   local encuestas EPHP
	if "`pais'" == "ARG"    & ("`ano'" >= "2003")                    local encuestas EPHC
	
	if "`pais'" == "BHS" | "`pais'" == "BLZ" | "`pais'" == "JAM" | "`pais'" == "GUY" local encuestas LFS
	if "`pais'" == "BRB"                       local encuestas CLFS
	if "`pais'" == "BOL"                       local encuestas ECH
	
	if "`pais'" == "BRA" & ("`ano'" < "2016")  local encuestas PNAD
	if "`pais'" == "BRA" & ("`ano'" >= "2016") local encuestas PNADC
	
	if "`pais'" == "CHL"                       local encuestas CASEN
	
	if "`pais'" == "COL"   & ("`ano'" == "2000")                     local encuestas ENH-FT
	if "`pais'" == "COL"   & ("`ano'" < "2006")                    local encuestas ECH
	if "`pais'" == "COL"   & ("`ano'" >= "2006")                    local encuestas GEIH
	
	if "`pais'" == "CRI"  & ("`ano'" >= "1990" & "`ano'" <= "2009") local encuestas EHPM
	if "`pais'" == "CRI"  & ("`ano'" > "2009") local encuestas ENAHO
	
	if "`pais'" == "ECU"                       local encuestas ENEMDU
	if "`pais'" == "SLV"                       local encuestas EHPM
	if "`pais'" == "GTM"                       local encuestas ENEI
	
	if "`pais'" == "HND"                       local encuestas EPHPM
	if "`pais'" == "MEX"                       local encuestas ENIGH                      
	
	if "`pais'" == "NIC" & ("`ano'" == "2009" | "`ano'" == "2014") local encuestas EMNV
	if "`pais'" == "NIC" & ("`ano'" <  "2009" | ("`ano'" > "2009" & "`ano'" < "2014") | "`ano'" > "2014") local encuestas ECH
	
	if "`pais'" == "PAN" & ("`ano'" >= "2005" & "`ano'" <= "2010" ) local encuestas EH
	if "`pais'" == "PAN" & ("`ano'" >  "2010") local encuestas EHPM
	
	if "`pais'" == "PRY" & ("`ano'" <  "2018") local encuestas EPH
	if "`pais'" == "PRY" & ("`ano'" >= "2018") local encuestas EPHC
	
	
	if "`pais'" == "PER"                       local encuestas ENAHO
	
	if "`pais'" == "DOM" & ("`ano'" >= "2017") local encuestas ENCFT
	if "`pais'" == "DOM"  & ("`ano'" < "2017") local encuestas ENFT
	
	if "`pais'" == "SUR"                       local encuestas SLC
	if "`pais'" == "TTO"                       local encuestas CSSP
	if "`pais'" == "URY"                       local encuestas ECH
	
	if "`pais'" == "VEN" & ("`ano'" <= "2015") local encuestas EHM
	if "`pais'" == "VEN" & ("`ano'" >  "2015") local encuestas ENCOVI

* Rondas

* Argentina 
	if "`pais'" == "ARG" &("`ano'" < "2003" )   local rondas m10
	if "`pais'" == "ARG" &("`ano'" == "2015" )   local rondas s1
	if "`pais'" == "ARG" &("`ano'" >= "2003" & "`ano'" <= "2014") | "`ano'" >= "2016"  local rondas s2
* Bahamas
	if "`pais'" == "BHS"  | "`pais'" == "SLV" | "`pais'" == "TTO" | "`pais'" == "URY" local rondas a
	*| "`pais'" == "BLZ"
	if "`pais'" == "BRB" & "`ano'" <= "2015" local rondas a
	if "`pais'" == "BRB" & "`ano'" >= "2016" local rondas m1_m6
* Bolivia
	if "`pais'" == "BOL" & ("`ano'" >= "2000" & "`ano'" <= "2002")  local rondas m11_m12
	if "`pais'" == "BOL" & ("`ano'" >= "2003" & "`ano'" <= "2004")  local rondas a2003-a2004
	if "`pais'" == "BOL" & ("`ano'" >= "2005" & "`ano'" <= "2011")  local rondas m11_m12
	if "`pais'" == "BOL" & ("`ano'" > "2011")  local rondas m11
* Brasil 
	if "`pais'" == "BRA" & ("`ano'" < "2016")   local rondas m9
	if "`pais'" == "BRA" & ("`ano'" >= "2016")  local rondas a
* Chile
	if "`pais'" == "CHL" & ("`ano'" <= "2006" | "`ano'" == "2009" ) local rondas m11_m12
	if "`pais'" == "CHL" & ("`ano'" == "2008" | "`ano'" >= "2010" )  local rondas m11_m12_m1
* Colombia
	if "`pais'" == "COL" & ("`ano'" <= "2000")    local rondas t3
	if "`pais'" == "COL"  & ("`ano'" > "2000")    local rondas t3
* Costa Rica
	if "`pais'" == "CRI"                       local rondas m7
* Ecuador
	if "`pais'" == "ECU" & ("`ano'" == "2000" | "`ano'" == "2002")  local rondas m12
	if "`pais'" == "ECU"  & ("`ano'" == "2001" | "`ano'" > "2003") local rondas m12
* Guatemala
	if "`pais'" == "GTM" & ("`ano'" == "2011" | "`ano'" == "2012" ) local rondas m6_m7    
	if "`pais'" == "GTM" & (("`ano'" >= "2005" & "`ano'" <= "2010") | ("`ano'" > "2012" & "`ano'" <= "2017")) local rondas m10
	if "`pais'" == "GTM" & ("`ano'" >= "2018" ) local rondas m6    
	
* Guyana
	if "`pais'" == "GUY"                       local rondas t4
* Honduras
	if "`pais'" == "HND" & ("`ano'" == "2006" | "`ano'" == "2007" | "`ano'" <= "2003") local rondas m9
	if "`pais'" == "HND" & ("`ano'" == "2005")  local rondas m10
	if "`pais'" == "HND" & ("`ano'" >= "2008" & "`ano'" <= "2013" ) local rondas m5
	if "`pais'" == "HND" & ("`ano'" > "2013")  local rondas m6
* Jamaica
	if "`pais'" == "JAM"   & ( "`ano'" >= "1997" & "`ano'" >= "2007")                    local rondas m4
	if "`pais'" == "JAM"   & (  "`ano'" >= "2010")                    local rondas m4
    if "`pais'" == "JAM"   & ( "`ano'" == "1993" | "`ano'" == "1994" | "`ano'" == "2008" | "`ano'" == "2007")                    local rondas m4
	if "`pais'" == "JAM"   & ("`ano'" == "1991" | "`ano'" == "1992" | "`ano'" == "1995" | "`ano'" == "1996")                    local rondas m5
	if "`pais'" == "JAM"   & ("`ano'" == "2009" | "`ano'" == "2007")                    local rondas m10
	
* México
	if "`pais'" == "MEX" & ("`ano'" >= "2000" & "`ano'" <= "2011") local rondas m8_m11
	if "`pais'" == "MEX" & ("`ano'" >  "2011") local rondas m8_m12
* Nicaragua
	if "`pais'" == "NIC" & ("`ano'" == "2001" ) local rondas m2_m6
	if "`pais'" == "NIC" & ("`ano'" == "2005" | "`ano'" == "2009") local rondas m7_m10
	if "`pais'" == "NIC" & ("`ano'" < "2009" | ("`ano'" > "2009" & "`ano'" < "2014") ) local rondas m7_m9
	if "`pais'" == "NIC" & ("`ano'" == "2014") local rondas m9_m12
* Panama
	if "`pais'" == "PAN" & ( "`ano'" <= "2008") local rondas m8
	if "`pais'" == "PAN" & ("`ano'" >  "2010") local rondas m3
	
* Peru
	
	if "`pais'" == "PER" & ("`ano'" <  "2003" ) local rondas t4
	if "`pais'" == "PER" & ("`ano'" >=  "2003" ) local rondas a
	
* Paraguay
	if "`pais'" == "PRY" & ( "`ano'" == "2000") local rondas m9-2000_m8-2001
	if "`pais'" == "PRY" & ( "`ano'" == "2004") local rondas m8_m12
	if "`pais'" == "PRY" & ("`ano'" ==  "2003" ) local rondas m8_m11
	if "`pais'" == "PRY" & ("`ano'" ==  "2005" ) local rondas m10_m12
	if "`pais'" == "PRY" & ("`ano'" == "2006") local rondas m11_m12
	if "`pais'" == "PRY" & ("`ano'" >  "2006" & "`ano'" <=  "2017" ) local rondas m10_m12
	if "`pais'" == "PRY" & ("`ano'" > "2017") local rondas t4
* Republica Dominicana	
	if "`pais'" == "DOM" & ("`ano'" <  "2017") local rondas m10
	if "`pais'" == "DOM" & ("`ano'" >= "2017") local rondas t4
* Surinam 	
	if "`pais'" == "SUR"                       local rondas m10_m9
* Venezuela	
	if "`pais'" == "VEN" & ("`ano'" <= "2015") local rondas s2
	if "`pais'" == "VEN" & ("`ano'" >  "2015") local rondas a



/* End of do-file */


