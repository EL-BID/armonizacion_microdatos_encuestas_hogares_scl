*Para correr do files
*Mayra Saenz
*Agosto, 2017
* last modification: Angela Lopez /05/19/21

global surveysFolder "\\Sdssrv03\surveys"
global gitArm "C:\Users\ALOP\OneDrive - Inter-American Development Bank Group\Desktop\Git_repositories\armonizacion_microdatos_encuestas_hogares_scl"

local paises ARG BHS BOL BRA BRB BLZ BRA CHL COL CRI ECU SLV GTM GUY HTI HND JAM MEX NIC PAN PRY PER DOM SUR TTO URY VEN 
local anos 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019

foreach pais of local paises {
	foreach ano of local anos {

		*Argentina
		if "`pais'" == "ARG" { 
			do "${gitArm}\ARG\EPHC\program\ARG_2005s2_variablesBID.do"
			do "${gitArm}\ARG\EPHC\program\ARG_2006s2_variablesBID.do"
			do "${gitArm}\ARG\EPHC\program\ARG_2007s2_variablesBID.do"
			do "${gitArm}\ARG\EPHC\program\ARG_2008s2_variablesBID.do"
			do "${gitArm}\ARG\EPHC\program\ARG_2009s2_variablesBID.do"
			do "${gitArm}\ARG\EPHC\program\ARG_2010s2_variablesBID.do"
			do "${gitArm}\ARG\EPHC\program\ARG_2011s2_variablesBID.do"
			do "${gitArm}\ARG\EPHC\program\ARG_2012s2_variablesBID.do"
			do "${gitArm}\ARG\EPHC\program\ARG_2013s2_variablesBID.do"
			do "${gitArm}\ARG\EPHC\program\ARG_2014s2_variablesBID.do"
			do "${gitArm}\ARG\EPHC\program\ARG_2015s1_variablesBID.do"
			do "${gitArm}\ARG\EPHC\program\ARG_2016s2_variablesBID.do"
			do "${gitArm}\ARG\EPHC\program\ARG_2017s2_variablesBID.do"
			do "${gitArm}\ARG\EPHC\program\ARG_2018s2_variablesBID.do"
			do "${gitArm}\ARG\EPHC\program\ARG_2019s2_variablesBID.do"
		}

		*Bahamas
		if "`pais'" == "BHS" {
			do "${gitArm}\BHS\LFS\program\BHS_2001a_variablesBID.do"
			do "${gitArm}\BHS\LFS\program\BHS_2002a_variablesBID.do"
			do "${gitArm}\BHS\LFS\program\BHS_2003a_variablesBID.do"
			do "${gitArm}\BHS\LFS\program\BHS_2004a_variablesBID.do"
			do "${gitArm}\BHS\LFS\program\BHS_2005a_variablesBID.do"
			do "${gitArm}\BHS\LFS\program\BHS_2006a_variablesBID.do"
			do "${gitArm}\BHS\LFS\program\BHS_2007a_variablesBID.do"
			do "${gitArm}\BHS\LFS\program\BHS_2008a_variablesBID.do"
			do "${gitArm}\BHS\LFS\program\BHS_2009a_variablesBID.do"
			do "${gitArm}\BHS\LFS\program\BHS_2011a_variablesBID.do"
			do "${gitArm}\BHS\LFS\program\BHS_2012a_variablesBID.do"
			do "${gitArm}\BHS\LFS\program\BHS_2013a_variablesBID.do"
			do "${gitArm}\BHS\LFS\program\BHS_2014a_variablesBID.do"
		}

		*Bolivia
		if "`pais'" == "BOL" {
		do "${gitArm}\BOL\ECH\program\BOL_2003a2003_a2004_variablesBID.do"
		do "${gitArm}\BOL\ECH\program\BOL_2005m11_m12_variablesBID.do"
		do "${gitArm}\BOL\ECH\program\BOL_2006m11_m12_variablesBID.do"
		do "${gitArm}\BOL\ECH\program\BOL_2007m11_m12_variablesBID.do"
		do "${gitArm}\BOL\ECH\program\BOL_2008m11_m12_variablesBID.do"
		do "${gitArm}\BOL\ECH\program\BOL_2009m11_m12_variablesBID.do"
		do "${gitArm}\BOL\ECH\program\BOL_2011m11_m12_variablesBID.do"
		do "${gitArm}\BOL\ECH\program\BOL_2012m11_variablesBID.do"
		do "${gitArm}\BOL\ECH\program\BOL_2013m11_variablesBID.do"
		do "${gitArm}\BOL\ECH\program\BOL_2014m11_variablesBID.do"
		do "${gitArm}\BOL\ECH\program\BOL_2015m11_variablesBID.do"
		do "${gitArm}\BOL\ECH\program\BOL_2016m11_variablesBID.do"
		do "${gitArm}\BOL\ECH\program\BOL_2017m11_variablesBID.do"
		do "${gitArm}\BOL\ECH\program\BOL_2018m11_variablesBID.do"
		}

		*Brazil
		if "`pais'" == "BRA" {
		do "${gitArm}\BRA\PNAD\program\BRA_2008m9_variablesBID.do"
		do "${gitArm}\BRA\PNAD\program\BRA_2009m9_variablesBID.do"
		do "${gitArm}\BRA\PNAD\program\BRA_2011m9_variablesBID.do"
		do "${gitArm}\BRA\PNAD\program\BRA_2012m9_variablesBID.do"
		do "${gitArm}\BRA\PNAD\program\BRA_2013m9_variablesBID.do"
		do "${gitArm}\BRA\PNAD\program\BRA_2014m9_variablesBID.do"
		do "${gitArm}\BRA\PNAD\program\BRA_2015m9_variablesBID.do"
		do "${gitArm}\BRA\PNADC\program\BRA_2016t1_variablesBID.do"
		do "${gitArm}\BRA\PNADC\program\BRA_2017t1_variablesBID.do"
		do "${gitArm}\BRA\PNADC\program\BRA_2018t1_variablesBID.do"
		}

		*Dominican Republic
		if "`pais'" == "DOM" {
		do "${gitArm}\DOM\ENFT\program\DOM_1995m6_variablesBID.do"
		do "${gitArm}\DOM\ENFT\program\DOM_1996m2_variablesBID.do"
		do "${gitArm}\DOM\ENFT\program\DOM_1997m4_variablesBID.do"
		do "${gitArm}\DOM\ENFT\program\DOM_2000m10_variablesBID.do"
		do "${gitArm}\DOM\ENFT\program\DOM_2001m10_variablesBID.do"
		do "${gitArm}\DOM\ENFT\program\DOM_2002m10_variablesBID.do"
		do "${gitArm}\DOM\ENFT\program\DOM_2003m10_variablesBID.do"
		do "${gitArm}\DOM\ENFT\program\DOM_2004m10_variablesBID.do"
		do "${gitArm}\DOM\ENFT\program\DOM_2005m10_variablesBID.do"
		do "${gitArm}\DOM\ENFT\program\DOM_2006m10_variablesBID.do"
		do "${gitArm}\DOM\ENFT\program\DOM_2007m10_variablesBID.do"
		do "${gitArm}\DOM\ENFT\program\DOM_2008m10_variablesBID.do"
		do "${gitArm}\DOM\ENFT\program\DOM_2009m10_variablesBID.do"
		do "${gitArm}\DOM\ENFT\program\DOM_2010m10_variablesBID.do"
		do "${gitArm}\DOM\ENFT\program\DOM_2011m10_variablesBID.do"
		do "${gitArm}\DOM\ENFT\program\DOM_2012m10_variablesBID.do"
		do "${gitArm}\DOM\ENFT\program\DOM_2013m10_variablesBID.do"
		do "${gitArm}\DOM\ENFT\program\DOM_2014m4_variablesBID.do"
		do "${gitArm}\DOM\ENFT\program\DOM_2014m10_variablesBID.do"
		}

		*El Salvador
		if "`pais'" == "SLV" {
		do "${gitArm}\SLV\EHPM\program\SLV_1995a_variablesBID.do"
		do "${gitArm}\SLV\EHPM\program\SLV_1996a_variablesBID.do"
		do "${gitArm}\SLV\EHPM\program\SLV_1997a_variablesBID.do"
		do "${gitArm}\SLV\EHPM\program\SLV_1998a_variablesBID.do"
		do "${gitArm}\SLV\EHPM\program\SLV_1999a_variablesBID.do"
		do "${gitArm}\SLV\EHPM\program\SLV_2000a_variablesBID.do"
		do "${gitArm}\SLV\EHPM\program\SLV_2001a_variablesBID.do"
		do "${gitArm}\SLV\EHPM\program\SLV_2002a_variablesBID.do"
		do "${gitArm}\SLV\EHPM\program\SLV_2003a_variablesBID.do"
		do "${gitArm}\SLV\EHPM\program\SLV_2004a_variablesBID.do"
		do "${gitArm}\SLV\EHPM\program\SLV_2005a_variablesBID.do"
		do "${gitArm}\SLV\EHPM\program\SLV_2006a_variablesBID.do"
		do "${gitArm}\SLV\EHPM\program\SLV_2007a_variablesBID.do"
		do "${gitArm}\SLV\EHPM\program\SLV_2008a_variablesBID.do"
		do "${gitArm}\SLV\EHPM\program\SLV_2009a_variablesBID.do"
		do "${gitArm}\SLV\EHPM\program\SLV_2010a_variablesBID.do"
		do "${gitArm}\SLV\EHPM\program\SLV_2011a_variablesBID.do"
		do "${gitArm}\SLV\EHPM\program\SLV_2012a_variablesBID.do"
		do "${gitArm}\SLV\EHPM\program\SLV_2013a_variablesBID.do"
		do "${gitArm}\SLV\EHPM\program\SLV_2014a_variablesBID.do"
		}

		*Honduras
		if "`pais'" == "HND" {
		do	"${gitArm}\HND\EPHPM\program\HND_1986m9_variablesBID.do"
		do	"${gitArm}\HND\EPHPM\program\HND_1989m9_variablesBID.do"
		do	"${gitArm}\HND\EPHPM\program\HND_1990m9_variablesBID.do"
		do	"${gitArm}\HND\EPHPM\program\HND_1991m3_variablesBID.do"
		do	"${gitArm}\HND\EPHPM\program\HND_1992m9_variablesBID.do"
		do	"${gitArm}\HND\EPHPM\program\HND_1993m10_variablesBID.do"
		do	"${gitArm}\HND\EPHPM\program\HND_1994m10_variablesBID.do"
		do	"${gitArm}\HND\EPHPM\program\HND_1995m10_variablesBID.do"
		do	"${gitArm}\HND\EPHPM\program\HND_1996m9_variablesBID.do"
		do	"${gitArm}\HND\EPHPM\program\HND_1997m9_variablesBID.do"
		do	"${gitArm}\HND\EPHPM\program\HND_1998m9_variablesBID.do"
		do	"${gitArm}\HND\EPHPM\program\HND_1999m9_variablesBID.do"
		do	"${gitArm}\HND\EPHPM\program\HND_2001m9_variablesBID.do"
		do	"${gitArm}\HND\EPHPM\program\HND_2002m5_variablesBID.do"
		do	"${gitArm}\HND\EPHPM\program\HND_2002m9_variablesBID.do"
		do	"${gitArm}\HND\EPHPM\program\HND_2003m9_variablesBID.do"
		do	"${gitArm}\HND\EPHPM\program\HND_2004m3_variablesBID.do"
		do	"${gitArm}\HND\EPHPM\program\HND_2005m3_variablesBID.do"
		do	"${gitArm}\HND\EPHPM\program\HND_2005m10_variablesBID.do"
		do	"${gitArm}\HND\EPHPM\program\HND_2006m5_variablesBID.do"
		do	"${gitArm}\HND\EPHPM\program\HND_2006m9_variablesBID.do"
		do	"${gitArm}\HND\EPHPM\program\HND_2007m9_variablesBID.do"
		do	"${gitArm}\HND\EPHPM\program\HND_2008m5_variablesBID.do"
		do	"${gitArm}\HND\EPHPM\program\HND_2009m5_variablesBID.do"
		do	"${gitArm}\HND\EPHPM\program\HND_2010m5_variablesBID.do"
		do	"${gitArm}\HND\EPHPM\program\HND_2011m5_variablesBID.do"
		do	"${gitArm}\HND\EPHPM\program\HND_2012m5_variablesBID.do"
		do	"${gitArm}\HND\EPHPM\program\HND_2013m5_variablesBID.do"
		do	"${gitArm}\HND\EPHPM\program\HND_2014m6_variablesBID.do"
		}

		*Jamaica
		if "`pais'" == "JAM" {
		do "${gitArm}\JAM\LFS\program\JAM_1990m4_variablesBID.do"
		do "${gitArm}\JAM\LFS\program\\JAM_1990m5_variablesBID.do"
		do "${gitArm}\JAM\LFS\program\\JAM_1991m5_variablesBID.do"
		do "${gitArm}\JAM\LFS\program\\JAM_1992m5_variablesBID.do"
		do "${gitArm}\JAM\LFS\program\\JAM_1993m4_variablesBID.do"
		do "${gitArm}\JAM\LFS\program\\JAM_1993m5_variablesBID.do"
		do "${gitArm}\JAM\LFS\program\\JAM_1994m4_variablesBID.do"
		do "${gitArm}\JAM\LFS\program\\JAM_1995m5_variablesBID.do"
		do "${gitArm}\JAM\LFS\program\\JAM_1996m5_variablesBID.do"
		do "${gitArm}\JAM\LFS\program\\JAM_1997m4_variablesBID.do"
		do "${gitArm}\JAM\LFS\program\\JAM_1997m5_variablesBID.do"
		do "${gitArm}\JAM\LFS\program\\JAM_1998m4_variablesBID.do"
		do "${gitArm}\JAM\LFS\program\\JAM_1998m5_variablesBID.do"
		do "${gitArm}\JAM\LFS\program\\JAM_1999m4_variablesBID.do"
		do "${gitArm}\JAM\LFS\program\\JAM_2000m4_variablesBID.do"
		do "${gitArm}\JAM\LFS\program\\JAM_2001m4_variablesBID.do"
		do "${gitArm}\JAM\LFS\program\\JAM_2002m4_variablesBID.do"
		do "${gitArm}\JAM\LFS\program\\JAM_2003m4_variablesBID.do"
		do "${gitArm}\JAM\LFS\program\\JAM_2004m4_variablesBID.do"
		do "${gitArm}\JAM\LFS\program\\JAM_2005m4_variablesBID.do"
		do "${gitArm}\JAM\LFS\program\\JAM_2006m4_variablesBID.do"
		do "${gitArm}\JAM\LFS\program\\JAM_2007m4_variablesBID.do"
		do "${gitArm}\JAM\LFS\program\\JAM_2007m10_variablesBID.do"
		do "${gitArm}\JAM\LFS\program\\JAM_2008m4_variablesBID.do"
		do "${gitArm}\JAM\LFS\program\\JAM_2009m4_variablesBID.do"
		do "${gitArm}\JAM\LFS\program\\JAM_2009m10_variablesBID.do"
		do "${gitArm}\JAM\LFS\program\\JAM_2010m4_variablesBID.do"
		do "${gitArm}\JAM\LFS\program\\JAM_2012m4_variablesBID.do"
		}

		* Mexico 
		if "`pais'" == "MEX" {
		do "${gitArm}\MEX\ENIGH\program\MEX_1992m8_m11_variablesBID.do"
		do "${gitArm}\MEX\ENIGH\program\MEX_1994m8_m9_variablesBID.do"
		do "${gitArm}\MEX\ENIGH\program\MEX_1996m7_m10_variablesBID.do"
		do "${gitArm}\MEX\ENIGH\program\MEX_1998m7_m12_variablesBID.do"
		do "${gitArm}\MEX\ENIGH\program\MEX_2000m8_m11_variablesBID.do"
		do "${gitArm}\MEX\ENIGH\program\MEX_2002m8_m11_variablesBID.do"
		do "${gitArm}\MEX\ENIGH\program\MEX_2004m8_m11_variablesBID.do"
		do "${gitArm}\MEX\ENIGH\program\MEX_2005m8_m11_variablesBID.do"
		do "${gitArm}\MEX\ENIGH\program\MEX_2006m8_m11_variablesBID.do"
		do "${gitArm}\MEX\ENIGH\program\MEX_2008m8_m11_variablesBID.do"
		do "${gitArm}\MEX\ENIGH\program\MEX_2010m8_m11_variablesBID.do"
		do "${gitArm}\MEX\ENIGH\program\MEX_2012m8_m12_variablesBID.do"
		do "${gitArm}\MEX\ENIGH\program\MEX_2014m8_m12_variablesBID.do"

		*Mexico Nueva construcción
		do "${gitArm}\MEX\ENIGH_NC\program\MEX_2008m8_m11_variablesBID.do"
		do "${gitArm}\MEX\ENIGH_NC\program\MEX_2010m8_m11_variablesBID.do"
		do "${gitArm}\MEX\ENIGH_NC\program\MEX_2012m8_m12_variablesBID.do"
		}

		*Nicaragua
		if "`pais'" == "NIC" {
		*EMNV
		do	"${gitArm}\NIC\EMNV\program\NIC_1993m2_m6_variablesBID.do"
		do	"${gitArm}\NIC\EMNV\program\NIC_1998m2_m6_variablesBID.do"
		do	"${gitArm}\NIC\EMNV\program\NIC_1999m2_m6_variablesBID.do"
		do	"${gitArm}\NIC\EMNV\program\NIC_2001m2_m6_variablesBID.do"
		do	"${gitArm}\NIC\EMNV\program\NIC_2005m7_m10_variablesBID.do"
		do	"${gitArm}\NIC\EMNV\program\NIC_2009m7_m10variablesBID.do"
		do	"${gitArm}\NIC\EMNV\program\NIC_2014m9_m12variablesBID.do"
		*ECH
		do	"${gitArm}\NIC\ECH\program\NIC_2010m7_m9_variablesBID.do"
		do	"${gitArm}\NIC\ECH\program\NIC_2011m7_m9_variablesBID.do"
		do	"${gitArm}\NIC\ECH\program\NIC_2012m7_m9_variablesBID.do"
		}

		*Panama
		if "`pais'" == "PAN" {
		do "${gitArm}\PAN\EH\program\PAN_1991m8_variablesBID.do"
		do "${gitArm}\PAN\EH\program\PAN_1995m8_variablesBID.do"
		do "${gitArm}\PAN\EH\program\PAN_1996m8_variablesBID.do"
		do "${gitArm}\PAN\EH\program\PAN_1997m8_variablesBID.do"
		do "${gitArm}\PAN\EH\program\PAN_1998m8_variablesBID.do"
		do "${gitArm}\PAN\EH\program\PAN_1999m8_variablesBID.do"
		do "${gitArm}\PAN\EH\program\PAN_2000m8_variablesBID.do"
		do "${gitArm}\PAN\EH\program\PAN_2001m8_variablesBID.do"
		do "${gitArm}\PAN\EH\program\PAN_2002m8_variablesBID.do"
		do "${gitArm}\PAN\EH\program\PAN_2003m8_variablesBID.do"
		do "${gitArm}\PAN\EH\program\PAN_2004m8_variablesBID.do"
		do "${gitArm}\PAN\EH\program\PAN_2005m8_variablesBID.do"
		do "${gitArm}\PAN\EH\program\PAN_2006m8_variablesBID.do"
		do "${gitArm}\PAN\EH\program\PAN_2007m8_variablesBID.do"
		do "${gitArm}\PAN\EH\program\PAN_2008m8_variablesBID.do"
		do "${gitArm}\PAN\EH\program\PAN_2009m8_variablesBID.do"
		do "${gitArm}\PAN\EH\program\PAN_2010m8_variablesBID.do"
		do "${gitArm}\PAN\EHPM\program\PAN_2011m3_variablesBID.do"
		do "${gitArm}\PAN\EHPM\program\PAN_2012m3_variablesBID.do"
		do "${gitArm}\PAN\EHPM\program\PAN_2013m3_variablesBID.do"
		do "${gitArm}\PAN\EHPM\program\PAN_2014m3_variablesBID.do"
		do "${gitArm}\PAN\EHPM\program\PAN_2015m3_variablesBID.do"
		}

		*Paraguay
		if "`pais'" == "PRY" {

		do "${gitArm}\PRY\EHM\program\PRY_1990m6_m8_variablesBID.do"
		do "${gitArm}\PRY\EHM\program\PRY_1991m10_m11_variablesBID.do"
		do "${gitArm}\PRY\EHM\program\PRY_1992m11_m12_variablesBID.do"
		do "${gitArm}\PRY\EHM\program\PRY_1993m9_m10_variablesBID.do"
		do "${gitArm}\PRY\EHM\program\PRY_1994m8_m10_variablesBID.do"
		do "${gitArm}\PRY\EHM\program\PRY_1995m8_m11_variablesBID.do"
		do "${gitArm}\PRY\EHM\program\PRY_1996m8_m12_variablesBID.do"
		do "${gitArm}\PRY\EIH\program\PRY_1997m8-1997_m7-1998_variablesBID.do"
		do "${gitArm}\PRY\EIH\program\PRY_2000m9-2000_m8-2001_variablesBID.do"
		do "${gitArm}\PRY\EPH\program\PRY_1999m8_m12_variablesBID.do"
		do "${gitArm}\PRY\EPH\program\PRY_2002m11_m12_variablesBID.do"
		do "${gitArm}\PRY\EPH\program\PRY_2003m8_m12_variablesBID.do"
		do "${gitArm}\PRY\EPH\program\PRY_2004m8_m11_variablesBID.do"
		do "${gitArm}\PRY\EPH\program\PRY_2005m10_m12_variablesBID.do"
		do "${gitArm}\PRY\EPH\program\PRY_2006m11_m12_variablesBID.do"
		do "${gitArm}\PRY\EPH\program\PRY_2007m10_m12_variablesBID.do"
		do "${gitArm}\PRY\EPH\program\PRY_2008m10_m12_variablesBID.do"
		do "${gitArm}\PRY\EPH\program\PRY_2009m10_m12_variablesBID.do"
		do "${gitArm}\PRY\EPH\program\PRY_2010m10_m12_variablesBID.do"
		do "${gitArm}\PRY\EPH\program\PRY_2011m10_m12_variablesBID.do"
		do "${gitArm}\PRY\EPH\program\PRY_2012m10_m12_variablesBID.do"
		do "${gitArm}\PRY\EPH\program\PRY_2013m10_m12_variablesBID.do"
		do "${gitArm}\PRY\EPH\program\PRY_2014m10_m12_variablesBID.do"
		do "${gitArm}\PRY\EPH\program\PRY_2015m10_m12_variablesBID.do"
		}

		*Trinidad y Tobago
		if "`pais'" == "TTO" {

		do "${gitArm}\TTO\CSSP\program\TTO_1999a_variablesBID.do"
		do "${gitArm}\TTO\CSSP\program\TTO_2000a_variablesBID.do"
		do "${gitArm}\TTO\CSSP\program\TTO_2001a_variablesBID.do"
		do "${gitArm}\TTO\CSSP\program\TTO_2002a_variablesBID.do"
		do "${gitArm}\TTO\CSSP\program\TTO_2003a_variablesBID.do"
		do "${gitArm}\TTO\CSSP\program\TTO_2004a_variablesBID.do"
		do "${gitArm}\TTO\CSSP\program\TTO_2005a_variablesBID.do"
		do "${gitArm}\TTO\CSSP\program\TTO_2006a_variablesBID.do"
		do "${gitArm}\TTO\CSSP\program\TTO_2007a_variablesBID.do"
		do "${gitArm}\TTO\CSSP\program\TTO_2008a_variablesBID.do"
		do "${gitArm}\TTO\CSSP\program\TTO_2009a_variablesBID.do"
		do "${gitArm}\TTO\CSSP\program\TTO_2010a_variablesBID.do"
		do "${gitArm}\TTO\CSSP\program\TTO_2011a_variablesBID.do"
		do "${gitArm}\TTO\CSSP\program\TTO_2012a_variablesBID.do"
		do "${gitArm}\TTO\CSSP\program\TTO_2013a_variablesBID.do"
		}

		*Uruguay
		if "`pais'" == "URY" {

		do "${gitArm}\URY\ECH\program\URY_1990s2_variablesBID.do"
		do "${gitArm}\URY\ECH\program\URY_1991s2_variablesBID.do"
		do "${gitArm}\URY\ECH\program\URY_1992s2_variablesBID.do"
		do "${gitArm}\URY\ECH\program\URY_1993s2_variablesBID.do"
		do "${gitArm}\URY\ECH\program\URY_1994s2_variablesBID.do"
		do "${gitArm}\URY\ECH\program\URY_1995a_variablesBID.do"
		do "${gitArm}\URY\ECH\program\URY_1996a_variablesBID.do"
		do "${gitArm}\URY\ECH\program\URY_1997a_variablesBID.do"
		do "${gitArm}\URY\ECH\program\URY_1998a_variablesBID.do"
		do "${gitArm}\URY\ECH\program\URY_1999a_variablesBID.do"
		do "${gitArm}\URY\ECH\program\URY_2000a_variablesBID.do"
		do "${gitArm}\URY\ECH\program\URY_2001a_variablesBID.do"
		do "${gitArm}\URY\ECH\program\URY_2002a_variablesBID.do"
		do "${gitArm}\URY\ECH\program\URY_2003a_variablesBID.do"
		do "${gitArm}\URY\ECH\program\URY_2004a_variablesBID.do"
		do "${gitArm}\URY\ECH\program\URY_2005a_variablesBID.do"
		do "${gitArm}\URY\ECH\program\URY_2006a_variablesBID.do"
		do "${gitArm}\URY\ECH\program\URY_2007a_variablesBID.do"
		do "${gitArm}\URY\ECH\program\URY_2008a_variablesBID.do"
		do "${gitArm}\URY\ECH\program\URY_2009a_variablesBID.do"
		do "${gitArm}\URY\ECH\program\URY_2010a_variablesBID.do"
		do "${gitArm}\URY\ECH\program\URY_2011a_variablesBID.do"
		do "${gitArm}\URY\ECH\program\URY_2012a_variablesBID.do"
		do "${gitArm}\URY\ECH\program\URY_2013a_variablesBID.do"
		do "${gitArm}\URY\ECH\program\URY_2014a_variablesBID.do"
		do "${gitArm}\URY\ECH\program\URY_2015a_variablesBID.do"
		}

		*Venezuela
		if "`pais'" == "VEN" {

		do "${gitArm}\VEN\EHM\program\VEN_1989s2_variablesBID.do"
		do "${gitArm}\VEN\EHM\program\VEN_1991s2_variablesBID.do"
		do "${gitArm}\VEN\EHM\program\VEN_1992s2_variablesBID.do"
		do "${gitArm}\VEN\EHM\program\VEN_1993s2_variablesBID.do"
		do "${gitArm}\VEN\EHM\program\VEN_1994s2_variablesBID.do"
		do "${gitArm}\VEN\EHM\program\VEN_1995s2_variablesBID.do"
		do "${gitArm}\VEN\EHM\program\VEN_1996s2_variablesBID.do"
		do "${gitArm}\VEN\EHM\program\VEN_1997s2_variablesBID.do"
		do "${gitArm}\VEN\EHM\program\VEN_1998s2_variablesBID.do"
		do "${gitArm}\VEN\EHM\program\VEN_1999s2_variablesBID.do"
		do "${gitArm}\VEN\EHM\program\VEN_2000s2_variablesBID.do"
		do "${gitArm}\VEN\EHM\program\VEN_2001s2_variablesBID.do"
		do "${gitArm}\VEN\EHM\program\VEN_2002s2_variablesBID.do"
		do "${gitArm}\VEN\EHM\program\VEN_2003s2_variablesBID.do"
		do "${gitArm}\VEN\EHM\program\VEN_2004s2_variablesBID.do"
		do "${gitArm}\VEN\EHM\program\VEN_2005s2_variablesBID.do"
		do "${gitArm}\VEN\EHM\program\VEN_2006s2_variablesBID.do"
		do "${gitArm}\VEN\EHM\program\VEN_2007s2_variablesBID.do"
		do "${gitArm}\VEN\EHM\program\VEN_2008s2_variablesBID.do"
		do "${gitArm}\VEN\EHM\program\VEN_2009s2_variablesBID.do"
		do "${gitArm}\VEN\EHM\program\VEN_2010s2_variablesBID.do"
		do "${gitArm}\VEN\EHM\program\VEN_2011s2_variablesBID.do"
		do "${gitArm}\VEN\EHM\program\VEN_2012s2_variablesBID.do"
		do "${gitArm}\VEN\EHM\program\VEN_2013s2_variablesBID.do"
		do "${gitArm}\VEN\EHM\program\VEN_2014s2_variablesBID.do"
		do "${gitArm}\VEN\EHM\program\VEN_2015s2_variablesBID.do"
		}
		
	} // cierro anos
} // cierro paises

