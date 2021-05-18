*=:=__=:=__=:=__=:=__=:=__=:=__=:=__=:=__=:=__=:=__=:=__=:=__=:=__=:=__=:=__=:=__=:=__=:=*
*                 Trend analysis by category of variables                                *
*=:=__=:=__=:=__=:=__=:=__=:=__=:=__=:=__=:=__=:=__=:=__=:=__=:=__=:=__=:=__=:=__=:=__=:=*

*Mayra Sáenz
*February, 2014
*SCL
*Inter American Development Bank


*______________*
/*Instructions
*______________*

* This do file has two sections. The first one is related to the generation of results and the second one is referred
to the generation of final results in excel format to create of pivot tables (Section two is at the end of this do file).

!!!The users need to MODIFY BOTH SECTIONS, but just the lines with the symbol: *###

*______________*
*General notes
*______________*

* The percentages are calculated with respect to the total population. That is why the percentages differ from
harmonized databases. However, you can see the trend of each category of variables. Also, it is posible identify
variables that have missing values, but they could have been generated (Although, contract command lets users to add the option
nonmiss or if).

* The final outputs in excel let users to see different indicators to categorical variables (percentages, frequencies)
 and continuos variables (min, max, mean, frecuencies and gini for ylm_ci and ylmhopri_ci).

* Variables are not weighted.

* idh_ch idp_ci factor_ch factor_ci were included just to verify that they were generated correctly, but they do not
 have any economic meaning.

 */



*________________________________________________________________________________________________*
*     S E C T I O N     O N E:     G E N E R A T I O N   O F   R E S U L T S
*________________________________________________________________________________________________*

*Please, modify just the lines that start with: *###

*### p: it is the path of the output; this is, where temporary files will be stored (It is recommended to generate a folder).
global p = "${surveysFolder}\DATA.IDB\Armonización de Bases AL\Revisión_tendencias\output\ALL"

*### FROM which database (Databases listed below)
local from = 277

*### TO which database (Databases listed below)
local to = 295


*________________________________________________________________________________________________*





*### db: database that are required. 
*Paths are taken from current_directory do file, which is used to generate SOCIOMETRO BID indicators.

*Argentina
local in1 =  "${surveysFolder}/harmonized/ARG/EPHP/data_arm\ARG_1992m10_BID"
local in2 =  "${surveysFolder}/harmonized/ARG/EPHP/data_arm\ARG_1993m10_BID"
local in3 =  "${surveysFolder}/harmonized/ARG/EPHP/data_arm\ARG_1994m10_BID"
local in4 =  "${surveysFolder}/harmonized/ARG/EPHP/data_arm\ARG_1995m10_BID"
local in5 =  "${surveysFolder}/harmonized/ARG/EPHP/data_arm\ARG_1996m10_BID"
local in6 =  "${surveysFolder}/harmonized/ARG/EPHP/data_arm\ARG_1997m10_BID"
local in7 =  "${surveysFolder}/harmonized/ARG/EPHP/data_arm\ARG_1998m10_BID"
local in8 =  "${surveysFolder}/harmonized/ARG/EPHP/data_arm\ARG_1999m10_BID"
local in9 =  "${surveysFolder}/harmonized/ARG/EPHP/data_arm\ARG_2000m10_BID"
local in10 =  "${surveysFolder}/harmonized/ARG/EPHP/data_arm\ARG_2001m10_BID"
local in11 =  "${surveysFolder}/harmonized/ARG/EPHP/data_arm\ARG_2002m10_BID"
local in12 =  "${surveysFolder}/harmonized/ARG/EPHC/data_arm\ARG_2003s2_BID"
local in13 =  "${surveysFolder}/harmonized/ARG/EPHC/data_arm\ARG_2004s2_BID"
local in14 =  "${surveysFolder}/harmonized/ARG/EPHC/data_arm\ARG_2005s2_BID"
local in15 =  "${surveysFolder}/harmonized/ARG/EPHC/data_arm\ARG_2006s2_BID"
local in16 =  "${surveysFolder}/harmonized/ARG/EPHC/data_arm\ARG_2007s2_BID"
local in17 =  "${surveysFolder}/harmonized/ARG/EPHC/data_arm\ARG_2008s2_BID"
local in18 =  "${surveysFolder}/harmonized/ARG/EPHC/data_arm\ARG_2009s2_BID"
local in19 =  "${surveysFolder}/harmonized/ARG/EPHC/data_arm\ARG_2010s2_BID"
local in20 =  "${surveysFolder}/harmonized/ARG/EPHC/data_arm\ARG_2011s2_BID"
local in21 =  "${surveysFolder}/harmonized/ARG/EPHC/data_arm\ARG_2012s2_BID"
*Bolivia
local in22 =  "${surveysFolder}/harmonized/BOL/EIH/data_arm\BOL_1990m11_BID"
local in23 =  "${surveysFolder}/harmonized/BOL/EIH/data_arm\BOL_1991m11_BID"
local in24 =  "${surveysFolder}/harmonized/BOL/EIH/data_arm\BOL_1992m11_BID"
local in25 =  "${surveysFolder}/harmonized/BOL/EIH/data_arm\BOL_1993m11_BID"
local in26 =  "${surveysFolder}/harmonized/BOL/EIH/data_arm\BOL_1994m7_m12_BID"
local in27 =  "${surveysFolder}/harmonized/BOL/EIH/data_arm\BOL_1995m6_BID"
local in28 =  "${surveysFolder}/harmonized/BOL/ENE/data_arm\BOL_1996m6_BID"
local in29 =  "${surveysFolder}/harmonized/BOL/ENE/data_arm\BOL_1997m11_BID"
local in30 =  "${surveysFolder}/harmonized/BOL/ECH/data_arm\BOL_1999m11_BID"
local in31 =  "${surveysFolder}/harmonized/BOL/ECH/data_arm\BOL_2000m11_BID"
local in32 =  "${surveysFolder}/harmonized/BOL/ECH/data_arm\BOL_2001m11_m12_BID"
local in33 =  "${surveysFolder}/harmonized/BOL/ECH/data_arm\BOL_2002m11_m12_BID"
local in34 =  "${surveysFolder}/harmonized/BOL/ECH/data_arm\BOL_2003a2003-a2004_BID"
local in35 =  "${surveysFolder}/harmonized/BOL/ECH/data_arm\BOL_2005m11_m12_BID"
local in36 =  "${surveysFolder}/harmonized/BOL/ECH/data_arm\BOL_2006m11_m12_BID"
local in37 =  "${surveysFolder}/harmonized/BOL/ECH/data_arm\BOL_2007m11_m12_BID"
local in38 =  "${surveysFolder}/harmonized/BOL/ECH/data_arm\BOL_2008m11_m12_BID"
local in39 =  "${surveysFolder}/harmonized/BOL/ECH/data_arm\BOL_2009m11_m12_BID"
local in40 =  "${surveysFolder}/harmonized/BOL/ECH/data_arm\BOL_2011m11_m12_BID"
*Chile
local in41 =  "${surveysFolder}/harmonized/CHL/CASEN/data_arm\CHL_1990m11_m12_BID"
local in42 =  "${surveysFolder}/harmonized/CHL/CASEN/data_arm\CHL_1992m11_m12_BID"
local in43 =  "${surveysFolder}/harmonized/CHL/CASEN/data_arm\CHL_1994m11_m12_BID"
local in44 =  "${surveysFolder}/harmonized/CHL/CASEN/data_arm\CHL_1996m11_m12_BID"
local in45 =  "${surveysFolder}/harmonized/CHL/CASEN/data_arm\CHL_1998m11_m12_BID"
local in46 =  "${surveysFolder}/harmonized/CHL/CASEN/data_arm\CHL_2000m11_m12_BID"
local in47 =  "${surveysFolder}/harmonized/CHL/CASEN/data_arm\CHL_2003m11_m12_BID"
local in48 =  "${surveysFolder}/harmonized/CHL/CASEN/data_arm\CHL_2006m11_m12_BID"
local in49 =  "${surveysFolder}/harmonized/CHL/CASEN/data_arm\CHL_2009m11_m12_BID"
local in50 =  "${surveysFolder}/harmonized/CHL/CASEN/data_arm\CHL_2011m11_m12_m1_BID"
*Costa Rica
local in51 =  "${surveysFolder}/harmonized/CRI/EHPM/data_arm\CRI_1990m7_BID"
local in52 =  "${surveysFolder}/harmonized/CRI/EHPM/data_arm\CRI_1991m7_BID"
local in53 =  "${surveysFolder}/harmonized/CRI/EHPM/data_arm\CRI_1992m7_BID"
local in54 =  "${surveysFolder}/harmonized/CRI/EHPM/data_arm\CRI_1993m7_BID"
local in55 =  "${surveysFolder}/harmonized/CRI/EHPM/data_arm\CRI_1994m7_BID"
local in56 =  "${surveysFolder}/harmonized/CRI/EHPM/data_arm\CRI_1995m7_BID"
local in57 =  "${surveysFolder}/harmonized/CRI/EHPM/data_arm\CRI_1996m7_BID"
local in58 =  "${surveysFolder}/harmonized/CRI/EHPM/data_arm\CRI_1997m7_BID"
local in59 =  "${surveysFolder}/harmonized/CRI/EHPM/data_arm\CRI_1998m7_BID"
local in60 =  "${surveysFolder}/harmonized/CRI/EHPM/data_arm\CRI_1999m7_BID"
local in61 =  "${surveysFolder}/harmonized/CRI/EHPM/data_arm\CRI_2000m7_BID"
local in62 =  "${surveysFolder}/harmonized/CRI/EHPM/data_arm\CRI_2001m7_BID"
local in63 =  "${surveysFolder}/harmonized/CRI/EHPM/data_arm\CRI_2002m7_BID"
local in64 =  "${surveysFolder}/harmonized/CRI/EHPM/data_arm\CRI_2003m7_BID"
local in65 =  "${surveysFolder}/harmonized/CRI/EHPM/data_arm\CRI_2004m7_BID"
local in66 =  "${surveysFolder}/harmonized/CRI/EHPM/data_arm\CRI_2005m7_BID"
local in67 =  "${surveysFolder}/harmonized/CRI/EHPM/data_arm\CRI_2006m7_BID"
local in68 =  "${surveysFolder}/harmonized/CRI/EHPM/data_arm\CRI_2007m7_BID"
local in69 =  "${surveysFolder}/harmonized/CRI/EHPM/data_arm\CRI_2008m7_BID"
local in70 =  "${surveysFolder}/harmonized/CRI/EHPM/data_arm\CRI_2009m7_BID"
local in71 =  "${surveysFolder}/harmonized/CRI/ENAHO/data_arm\CRI_2010m7_BID"
local in72 =  "${surveysFolder}/harmonized/CRI/ENAHO/data_arm\CRI_2011m7_BID"
local in73 =  "${surveysFolder}/harmonized/CRI/ENAHO/data_arm\CRI_2012m7_BID"
*Guatemala
local in74 =  "${surveysFolder}/harmonized/GTM/ENCOVI/data_arm\GTM_2000m7_m11_BID"
local in75 =  "${surveysFolder}\harmonized\GTM\ENEI\data_arm\GTM_2002m10_m11_BID"
local in76 =  "${surveysFolder}\harmonized\GTM\ENEI\data_arm\GTM_2003m2_m3_BID"
local in77 =  "${surveysFolder}\harmonized\GTM\ENEI\data_arm\GTM_2004m9_m11_BID"
local in78 =  "${surveysFolder}/harmonized/GTM/ENCOVI/data_arm\GTM_2006m7_m11_BID"
local in79 =  "${surveysFolder}\harmonized\GTM\ENEI\data_arm\GTM_2010m10_BID"
local in80 =  "${surveysFolder}\harmonized\GTM\ENEI\data_arm\GTM_2011m6_m7_BID"
local in81 =  "${surveysFolder}\harmonized\GTM\ENEI\data_arm\GTM_2012m6_m7_BID"
*Honduras
local in82 =  "${surveysFolder}/harmonized/HND/EPHPM/data_arm\HND_1990m9_BID"
local in83 =  "${surveysFolder}/harmonized/HND/EPHPM/data_arm\HND_1992m9_BID"
local in84 =  "${surveysFolder}/harmonized/HND/EPHPM/data_arm\HND_1994m10_BID"
local in85 =  "${surveysFolder}/harmonized/HND/EPHPM/data_arm\HND_1995m10_BID"
local in86 =  "${surveysFolder}/harmonized/HND/EPHPM/data_arm\HND_1996m9_BID"
local in87 =  "${surveysFolder}/harmonized/HND/EPHPM/data_arm\HND_1997m9_BID"
local in88 =  "${surveysFolder}/harmonized/HND/EPHPM/data_arm\HND_1998m9_BID"
local in89 =  "${surveysFolder}/harmonized/HND/EPHPM/data_arm\HND_1999m9_BID"
local in90 =  "${surveysFolder}/harmonized/HND/EPHPM/data_arm\HND_2001m9_BID"
local in91 =  "${surveysFolder}/harmonized/HND/EPHPM/data_arm\HND_2002m9_BID"
local in92 =  "${surveysFolder}/harmonized/HND/EPHPM/data_arm\HND_2003m9_BID"
local in93 =  "${surveysFolder}/harmonized/HND/EPHPM/data_arm\HND_2004m3_BID"
local in94 =  "${surveysFolder}/harmonized/HND/EPHPM/data_arm\HND_2005m10_BID"
local in95 =  "${surveysFolder}/harmonized/HND/EPHPM/data_arm\HND_2006m9_BID"
local in96 =  "${surveysFolder}/harmonized/HND/EPHPM/data_arm\HND_2007m9_BID"
local in97 =  "${surveysFolder}/harmonized/HND/EPHPM/data_arm\HND_2008m5_BID"
local in98 =  "${surveysFolder}/harmonized/HND/EPHPM/data_arm\HND_2009m5_BID"
local in99 =  "${surveysFolder}/harmonized/HND/EPHPM/data_arm\HND_2010m5_BID"
local in100 =  "${surveysFolder}/harmonized/HND/EPHPM/data_arm\HND_2011m5_BID"
local in101 =  "${surveysFolder}/harmonized/HND/EPHPM/data_arm\HND_2012m5_BID"
*Panama
local in102 =  "${surveysFolder}/harmonized/PAN/EH/data_arm\PAN_1991m8_BID"
local in103 =  "${surveysFolder}/harmonized/PAN/EH/data_arm\PAN_1995m8_BID"
local in104 =  "${surveysFolder}/harmonized/PAN/EH/data_arm\PAN_1996m8_BID"
local in105 =  "${surveysFolder}/harmonized/PAN/EH/data_arm\PAN_1997m8_BID"
local in106 =  "${surveysFolder}/harmonized/PAN/EH/data_arm\PAN_1998m8_BID"
local in107 =  "${surveysFolder}/harmonized/PAN/EH/data_arm\PAN_1999m8_BID"
local in108 =  "${surveysFolder}/harmonized/PAN/EH/data_arm\PAN_2000m8_BID"
local in109 =  "${surveysFolder}/harmonized/PAN/EH/data_arm\PAN_2001m8_BID"
local in110 =  "${surveysFolder}/harmonized/PAN/EH/data_arm\PAN_2002m8_BID"
local in111 =  "${surveysFolder}/harmonized/PAN/EH/data_arm\PAN_2003m8_BID"
local in112 =  "${surveysFolder}/harmonized/PAN/EH/data_arm\PAN_2004m8_BID"
local in113 =  "${surveysFolder}/harmonized/PAN/EH/data_arm\PAN_2005m8_BID"
local in114 =  "${surveysFolder}/harmonized/PAN/EH/data_arm\PAN_2006m8_BID"
local in115 =  "${surveysFolder}/harmonized/PAN/EH/data_arm\PAN_2007m8_BID"
local in116 =  "${surveysFolder}/harmonized/PAN/EH/data_arm\PAN_2008m8_BID"
local in117 =  "${surveysFolder}/harmonized/PAN/EH/data_arm\PAN_2009m8_BID"
local in118 =  "${surveysFolder}/harmonized/PAN/EH/data_arm\PAN_2010m8_BID"
*Paraguay
local in119 =  "${surveysFolder}/harmonized/PRY/EHM/data_arm\PRY_1990m6_m8_BID"
local in120 =  "${surveysFolder}/harmonized/PRY/EHM/data_arm\PRY_1991m10_m11_BID"
local in121 =  "${surveysFolder}/harmonized/PRY/EHM/data_arm\PRY_1992m11_m12_BID"
local in122 =  "${surveysFolder}/harmonized/PRY/EHM/data_arm\PRY_1993m9_m10_BID"
local in123 =  "${surveysFolder}/harmonized/PRY/EHM/data_arm\PRY_1994m8_m10_BID"
local in124 =  "${surveysFolder}/harmonized/PRY/EHM/data_arm\PRY_1995m8_m11_BID"
local in125 =  "${surveysFolder}/harmonized/PRY/EHM/data_arm\PRY_1996m8_m12_BID"
local in126 =  "${surveysFolder}/harmonized/PRY/EIH/data_arm\PRY_1997m8-1997_m7-1998_BID"
local in127 =  "${surveysFolder}/harmonized/PRY/EPH/data_arm\PRY_1999m8_m12_BID"
local in128 =  "${surveysFolder}/harmonized/PRY/EIH/data_arm\PRY_2000m9-2000_m8-2001_BID"
local in129 =  "${surveysFolder}/harmonized/PRY/EPH/data_arm\PRY_2002m11_m12_BID"
local in130 =  "${surveysFolder}/harmonized/PRY/EPH/data_arm\PRY_2003m8_m12_BID"
local in131 =  "${surveysFolder}/harmonized/PRY/EPH/data_arm\PRY_2004m8_m11_BID"
local in132 =  "${surveysFolder}/harmonized/PRY/EPH/data_arm\PRY_2005m10_m12_BID"
local in133 =  "${surveysFolder}/harmonized/PRY/EPH/data_arm\PRY_2006m11_m12_BID"
local in134 =  "${surveysFolder}/harmonized/PRY/EPH/data_arm\PRY_2007m10_m12_BID"
local in135 =  "${surveysFolder}/harmonized/PRY/EPH/data_arm\PRY_2008m10_m12_BID"
local in136 =  "${surveysFolder}/harmonized/PRY/EPH/data_arm\PRY_2009m10_m12_BID"
local in137 =  "${surveysFolder}/harmonized/PRY/EPH/data_arm\PRY_2010m10_m12_BID"
local in138 =  "${surveysFolder}/harmonized/PRY/EPH/data_arm\PRY_2011m10_m12_BID"
local in139 =  "${surveysFolder}/harmonized/PRY/EPH/data_arm\PRY_2012m10_m12_BID"
*Dominican Republic
local in140 =  "${surveysFolder}/harmonized/DOM/ENFT/data_arm\DOM_1995m6_BID"
local in141 =  "${surveysFolder}/harmonized/DOM/ENFT/data_arm\DOM_1996m2_BID"
local in142 =  "${surveysFolder}/harmonized/DOM/ENFT/data_arm\DOM_1997m4_BID"
local in143 =  "${surveysFolder}/harmonized/DOM/ENFT/data_arm\DOM_2000m10_BID"
local in144 =  "${surveysFolder}/harmonized/DOM/ENFT/data_arm\DOM_2001m10_BID"
local in145 =  "${surveysFolder}/harmonized/DOM/ENFT/data_arm\DOM_2002m10_BID"
local in146 =  "${surveysFolder}/harmonized/DOM/ENFT/data_arm\DOM_2003m10_BID"
local in147 =  "${surveysFolder}/harmonized/DOM/ENFT/data_arm\DOM_2004m10_BID"
local in148 =  "${surveysFolder}/harmonized/DOM/ENFT/data_arm\DOM_2005m10_BID"
local in149 =  "${surveysFolder}/harmonized/DOM/ENFT/data_arm\DOM_2006m10_BID"
local in150 =  "${surveysFolder}/harmonized/DOM/ENFT/data_arm\DOM_2007m10_BID"
local in151 =  "${surveysFolder}/harmonized/DOM/ENFT/data_arm\DOM_2008m10_BID"
local in152 =  "${surveysFolder}/harmonized/DOM/ENFT/data_arm\DOM_2009m10_BID"
local in153 =  "${surveysFolder}/harmonized/DOM/ENFT/data_arm\DOM_2010m10_BID"
local in154 =  "${surveysFolder}/harmonized/DOM/ENFT/data_arm\DOM_2011m10_BID"
local in155 =  "${surveysFolder}/harmonized/DOM/ENFT/data_arm\DOM_2012m10_BID"
*Brazil
local in156 = "${surveysFolder}\harmonized\BRA\PNAD\data_arm\BRA_1990m9_BID.dta"
local in157 = "${surveysFolder}\harmonized\BRA\PNAD\data_arm\BRA_1992m9_BID.dta"
local in158 = "${surveysFolder}\harmonized\BRA\PNAD\data_arm\BRA_1993m9_BID.dta"
local in159 = "${surveysFolder}\harmonized\BRA\PNAD\data_arm\BRA_1995m9_BID.dta"
local in160 = "${surveysFolder}\harmonized\BRA\PNAD\data_arm\BRA_1996m9_BID.dta"
local in161 = "${surveysFolder}\harmonized\BRA\PNAD\data_arm\BRA_1997m9_BID.dta"
local in162 = "${surveysFolder}\harmonized\BRA\PNAD\data_arm\BRA_1998m9_BID.dta"
local in163 = "${surveysFolder}\harmonized\BRA\PNAD\data_arm\BRA_1999m9_BID.dta"
local in164 = "${surveysFolder}\harmonized\BRA\PNAD\data_arm\BRA_2001m9_BID.dta"
local in165 = "${surveysFolder}\harmonized\BRA\PNAD\data_arm\BRA_2002m9_BID.dta"
local in166 = "${surveysFolder}\harmonized\BRA\PNAD\data_arm\BRA_2003m9_BID.dta"
local in167 = "${surveysFolder}\harmonized\BRA\PNAD\data_arm\BRA_2004m9_BID.dta"
local in168 = "${surveysFolder}\harmonized\BRA\PNAD\data_arm\BRA_2005m9_BID.dta"
local in169 = "${surveysFolder}\harmonized\BRA\PNAD\data_arm\BRA_2006m9_BID.dta"
local in170 = "${surveysFolder}\harmonized\BRA\PNAD\data_arm\BRA_2007m9_BID.dta"
local in171 = "${surveysFolder}\harmonized\BRA\PNAD\data_arm\BRA_2008m9_BID.dta"
local in172 = "${surveysFolder}\harmonized\BRA\PNAD\data_arm\BRA_2009m9_BID.dta"
local in173 = "${surveysFolder}\harmonized\BRA\PNAD\data_arm\BRA_2011m9_BID.dta"
local in174 = "${surveysFolder}\harmonized\BRA\PNAD\data_arm\BRA_2012m9_BID.dta"
*Colombia
local  in175 = "${surveysFolder}\harmonized\COL\ENH-FT\data_arm\COL_1991m9_BID.dta"
local  in176 = "${surveysFolder}\harmonized\COL\ENH-FT\data_arm\COL_1992m9_BID.dta"
local  in177 = "${surveysFolder}\harmonized\COL\ENH-FT\data_arm\COL_1993m9_BID.dta"
local  in178 = "${surveysFolder}\harmonized\COL\ENH-FT\data_arm\COL_1994m9_BID.dta"
local  in179 = "${surveysFolder}\harmonized\COL\ENH-FT\data_arm\COL_1995m9_BID.dta"
local  in180 = "${surveysFolder}\harmonized\COL\ENH-FT\data_arm\COL_1996m9_BID.dta"
local  in181 = "${surveysFolder}\harmonized\COL\ENH-FT\data_arm\COL_1997m9_BID.dta"
local  in182 = "${surveysFolder}\harmonized\COL\ENH-FT\data_arm\COL_1998m9_BID.dta"
local  in183 = "${surveysFolder}\harmonized\COL\ENH-FT\data_arm\COL_1999m9_BID.dta"
local  in184 = "${surveysFolder}\harmonized\COL\ENH-FT\data_arm\COL_2000m9_BID.dta"
local  in185 = "${surveysFolder}\harmonized\COL\ECH\data_arm\COL_2001t3_BID.dta"
local  in186 = "${surveysFolder}\harmonized\COL\ECH\data_arm\COL_2002t3_BID.dta"
local  in187 = "${surveysFolder}\harmonized\COL\ECH\data_arm\COL_2003t3_BID.dta"
local  in188 = "${surveysFolder}\harmonized\COL\ECH\data_arm\COL_2004t3_BID.dta"
local  in189 = "${surveysFolder}\harmonized\COL\ECH\data_arm\COL_2005t3_BID.dta"
local  in190 = "${surveysFolder}\harmonized\COL\GEIH\data_arm\COL_2006t3_BID.dta"
local  in191 = "${surveysFolder}\harmonized\COL\GEIH\data_arm\COL_2007t3_BID.dta"
local  in192 = "${surveysFolder}\harmonized\COL\GEIH\data_arm\COL_2008t3_BID.dta"
local  in193 = "${surveysFolder}\harmonized\COL\GEIH\data_arm\COL_2009t3_BID.dta"
local  in194 = "${surveysFolder}\harmonized\COL\GEIH\data_arm\COL_2010t3_BID.dta"
local  in195 = "${surveysFolder}\harmonized\COL\GEIH\data_arm\COL_2011t3_BID.dta"
*Ecuador
local in196 = "${surveysFolder}\harmonized\ECU\ENEMDU\data_arm\ECU_1990m11_BID.dta"
local in197 = "${surveysFolder}\harmonized\ECU\ENEMDU\data_arm\ECU_1991m11_BID.dta"
local in198 = "${surveysFolder}\harmonized\ECU\ENEMDU\data_arm\ECU_1992m11_BID.dta"
local in199 = "${surveysFolder}\harmonized\ECU\ENEMDU\data_arm\ECU_1993m11_BID.dta"
local in200 = "${surveysFolder}\harmonized\ECU\ENEMDU\data_arm\ECU_1994m11_BID.dta"
local in201 = "${surveysFolder}\harmonized\ECU\ENEMDU\data_arm\ECU_1995m11_BID.dta"
local in202 = "${surveysFolder}\harmonized\ECU\ENEMDU\data_arm\ECU_1996m11_BID.dta"
local in203 = "${surveysFolder}\harmonized\ECU\ENEMDU\data_arm\ECU_1997m11_BID.dta"
local in204 = "${surveysFolder}\harmonized\ECU\ENEMDU\data_arm\ECU_1998m11_BID.dta"
local in205 = "${surveysFolder}\harmonized\ECU\ENEMDU\data_arm\ECU_1999m11_BID.dta"
local in206 = "${surveysFolder}\harmonized\ECU\ENEMDU\data_arm\ECU_2000m11_BID.dta"
local in207 = "${surveysFolder}\harmonized\ECU\ENEMDU\data_arm\ECU_2001m12_BID.dta"
local in208 = "${surveysFolder}\harmonized\ECU\ENEMDU\data_arm\ECU_2002m11_BID.dta"
local in209 = "${surveysFolder}\harmonized\ECU\ENEMDU\data_arm\ECU_2003m12_BID.dta"
local in210 = "${surveysFolder}\harmonized\ECU\ENEMDU\data_arm\ECU_2004m12_BID.dta"
local in211 = "${surveysFolder}\harmonized\ECU\ENEMDU\data_arm\ECU_2005m12_BID.dta"
local in212 = "${surveysFolder}\harmonized\ECU\ENEMDU\data_arm\ECU_2006m12_BID.dta"
local in213 = "${surveysFolder}\harmonized\ECU\ENEMDU\data_arm\ECU_2007m12_BID.dta"
local in214 = "${surveysFolder}\harmonized\ECU\ENEMDU\data_arm\ECU_2008m12_BID.dta"
local in215 = "${surveysFolder}\harmonized\ECU\ENEMDU\data_arm\ECU_2009m12_BID.dta"
local in216 = "${surveysFolder}\harmonized\ECU\ENEMDU\data_arm\ECU_2010m12_BID.dta"
local in217 = "${surveysFolder}\harmonized\ECU\ENEMDU\data_arm\ECU_2011m12_BID.dta"
local in218 = "${surveysFolder}\harmonized\ECU\ENEMDU\data_arm\ECU_2012m12_BID.dta"
*Mexico
local in219 = "${surveysFolder}\harmonized\MEX\ENIGH\data_arm\MEX_1992m8_m11_BID.dta"
local in220 = "${surveysFolder}\harmonized\MEX\ENIGH\data_arm\MEX_1994m8_m9_BID.dta"
local in221 = "${surveysFolder}\harmonized\MEX\ENIGH\data_arm\MEX_1996m7_m10_BID.dta"
local in222 = "${surveysFolder}\harmonized\MEX\ENIGH\data_arm\MEX_1998m7_m12_BID.dta"
local in223 = "${surveysFolder}\harmonized\MEX\ENIGH\data_arm\MEX_2000m8_m11_BID.dta"
local in224 = "${surveysFolder}\harmonized\MEX\ENIGH\data_arm\MEX_2002m8_m11_BID.dta"
local  in225 = "${surveysFolder}\harmonized\MEX\ENIGH\data_arm\MEX_2004m8_m11_BID.dta"
local  in226 = "${surveysFolder}\harmonized\MEX\ENIGH\data_arm\MEX_2005m8_m11_BID.dta"
local  in227 = "${surveysFolder}\harmonized\MEX\ENIGH\data_arm\MEX_2006m8_m11_BID.dta"
local  in228 = "${surveysFolder}\harmonized\MEX\ENIGH\data_arm\MEX_2008m8_m11_BID.dta"
local  in229 = "${surveysFolder}\harmonized\MEX\ENIGH\data_arm\MEX_2010m8_m11_BID.dta"
local  in230 = "${surveysFolder}\harmonized\MEX\ENIGH\data_arm\MEX_2012m8_m12_BID.dta"
*Nicaragua
local in231 = "${surveysFolder}\harmonized\NIC\EMNV\data_arm\NIC_1993m2_m6_BID.dta"
local in232 = "${surveysFolder}\harmonized\NIC\EMNV\data_arm\NIC_1998m2_m6_BID.dta"
*local in233 = "${surveysFolder}\harmonized\NIC\EMNV\data_arm\NIC_1999m2_m6_BID.dta"
local in234 = "${surveysFolder}\harmonized\NIC\EMNV\data_arm\NIC_2001m2_m6_BID.dta"
local in235 = "${surveysFolder}\harmonized\NIC\EMNV\data_arm\NIC_2005m7_m10_BID.dta"
local in236 = "${surveysFolder}\harmonized\NIC\EMNV\data_arm\NIC_2009m7_m10_BID.dta"
local in237 = "${surveysFolder}\harmonized\NIC\ECH\data_arm\NIC_2010m7_m9_BID.dta"
local in238 = "${surveysFolder}\harmonized\NIC\ECH\data_arm\NIC_2011m7_m9_BID.dta"
*Peru
local  in239 = "${surveysFolder}\harmonized\PER\ENAHO\data_arm\PER_1997t4_BID.dta"
local  in240 = "${surveysFolder}\harmonized\PER\ENAHO\data_arm\PER_1998t4_BID.dta"
local  in241 = "${surveysFolder}\harmonized\PER\ENAHO\data_arm\PER_1999t4_BID.dta"
local  in242 = "${surveysFolder}\harmonized\PER\ENAHO\data_arm\PER_2000t4_BID.dta"
local  in243 = "${surveysFolder}\harmonized\PER\ENAHO\data_arm\PER_2001t4_BID.dta"
local  in244 = "${surveysFolder}\harmonized\PER\ENAHO\data_arm\PER_2002t4_BID.dta"
local  in245 = "${surveysFolder}\harmonized\PER\ENAHO\data_arm\PER_2003a_BID.dta"
local  in246 = "${surveysFolder}\harmonized\PER\ENAHO\data_arm\PER_2004a_BID.dta"
local  in247 = "${surveysFolder}\harmonized\PER\ENAHO\data_arm\PER_2005a_BID.dta"
local  in248 = "${surveysFolder}\harmonized\PER\ENAHO\data_arm\PER_2006a_BID.dta"
local  in249 = "${surveysFolder}\harmonized\PER\ENAHO\data_arm\PER_2007a_BID.dta"
local  in250 = "${surveysFolder}\harmonized\PER\ENAHO\data_arm\PER_2008a_BID.dta"
local  in251 = "${surveysFolder}\harmonized\PER\ENAHO\data_arm\PER_2009a_BID.dta"
local  in252 = "${surveysFolder}\harmonized\PER\ENAHO\data_arm\PER_2010a_BID.dta"
local  in253 = "${surveysFolder}\harmonized\PER\ENAHO\data_arm\PER_2011a_BID.dta"
local  in254 = "${surveysFolder}\harmonized\PER\ENAHO\data_arm\PER_2012a_BID.dta"
*Salvador
local in255 = "${surveysFolder}\harmonized\SLV\EHPM\data_arm\SLV_1995a_BID.dta"
local in256 = "${surveysFolder}\harmonized\SLV\EHPM\data_arm\SLV_1996a_BID.dta"
local in257 = "${surveysFolder}\harmonized\SLV\EHPM\data_arm\SLV_1997a_BID.dta"
local in258 = "${surveysFolder}\harmonized\SLV\EHPM\data_arm\SLV_1998a_BID.dta"
local in259 = "${surveysFolder}\harmonized\SLV\EHPM\data_arm\SLV_1999a_BID.dta"
local in260 = "${surveysFolder}\harmonized\SLV\EHPM\data_arm\SLV_2000a_BID.dta"
local in261 = "${surveysFolder}\harmonized\SLV\EHPM\data_arm\SLV_2001a_BID.dta"
local in262 = "${surveysFolder}\harmonized\SLV\EHPM\data_arm\SLV_2002a_BID.dta"
local in263 = "${surveysFolder}\harmonized\SLV\EHPM\data_arm\SLV_2003a_BID.dta "
local in264 = "${surveysFolder}\harmonized\SLV\EHPM\data_arm\SLV_2004a_BID.dta"
local in265 = "${surveysFolder}\harmonized\SLV\EHPM\data_arm\SLV_2005a_BID.dta"
local in266 = "${surveysFolder}\harmonized\SLV\EHPM\data_arm\SLV_2006a_BID.dta"
local in267 = "${surveysFolder}\harmonized\SLV\EHPM\data_arm\SLV_2007a_BID.dta"
local in268 = "${surveysFolder}\harmonized\SLV\EHPM\data_arm\SLV_2008a_BID.dta"
local in269 = "${surveysFolder}\harmonized\SLV\EHPM\data_arm\SLV_2009a_BID.dta"
local in270 = "${surveysFolder}\harmonized\SLV\EHPM\data_arm\SLV_2010a_BID.dta"
local in271 = "${surveysFolder}\harmonized\SLV\EHPM\data_arm\SLV_2011a_BID.dta"
local in272 = "${surveysFolder}\harmonized\SLV\EHPM\data_arm\SLV_2012a_BID.dta"
*Uruguay
local in273 = "${surveysFolder}\harmonized\URY\ECH\data_arm\URY_1990s2_BID.dta"
local in274 = "${surveysFolder}\harmonized\URY\ECH\data_arm\URY_1991s2_BID.dta"
local in275 = "${surveysFolder}\harmonized\URY\ECH\data_arm\URY_1992s2_BID.dta"
local in276 = "${surveysFolder}\harmonized\URY\ECH\data_arm\URY_1993s2_BID.dta"
local in277 = "${surveysFolder}\harmonized\URY\ECH\data_arm\URY_1994s2_BID.dta"
local in278 = "${surveysFolder}\harmonized\URY\ECH\data_arm\URY_1995a_BID.dta"
local in279 = "${surveysFolder}\harmonized\URY\ECH\data_arm\URY_1996a_BID.dta"
local in280 = "${surveysFolder}\harmonized\URY\ECH\data_arm\URY_1997a_BID.dta"
local in281 = "${surveysFolder}\harmonized\URY\ECH\data_arm\URY_1998a_BID.dta"
local in282 = "${surveysFolder}\harmonized\URY\ECH\data_arm\URY_1999a_BID.dta"
local in283 = "${surveysFolder}\harmonized\URY\ECH\data_arm\URY_2000a_BID.dta"
local in284 = "${surveysFolder}\harmonized\URY\ECH\data_arm\URY_2001a_BID.dta"
local in285 = "${surveysFolder}\harmonized\URY\ECH\data_arm\URY_2002a_BID.dta"
local in286 = "${surveysFolder}\harmonized\URY\ECH\data_arm\URY_2003a_BID.dta"
local in287 = "${surveysFolder}\harmonized\URY\ECH\data_arm\URY_2004a_BID.dta"
local in288 = "${surveysFolder}\harmonized\URY\ECH\data_arm\URY_2005a_BID.dta"
local in289 = "${surveysFolder}\harmonized\URY\ECH\data_arm\URY_2006a_BID.dta"
local in290 = "${surveysFolder}\harmonized\URY\ECH\data_arm\URY_2007a_BID.dta"
local in291 = "${surveysFolder}\harmonized\URY\ECH\data_arm\URY_2008a_BID.dta"
local in292 = "${surveysFolder}\harmonized\URY\ECH\data_arm\URY_2009a_BID.dta"
local in293 = "${surveysFolder}\harmonized\URY\ECH\data_arm\URY_2010a_BID.dta"
local in294 = "${surveysFolder}\harmonized\URY\ECH\data_arm\URY_2011a_BID.dta"
local in295 = "${surveysFolder}\harmonized\URY\ECH\data_arm\URY_2012a_BID.dta"
*Venezuela
local in296 = "${surveysFolder}\harmonized\VEN\EHM\data_arm\VEN_1989s2_BID.dta"
local in297 = "${surveysFolder}\harmonized\VEN\EHM\data_arm\VEN_1991s2_BID.dta"
local in298 = "${surveysFolder}\harmonized\VEN\EHM\data_arm\VEN_1992s2_BID.dta"
local in299 = "${surveysFolder}\harmonized\VEN\EHM\data_arm\VEN_1993s2_BID.dta"
local in300 = "${surveysFolder}\harmonized\VEN\EHM\data_arm\VEN_1994s2_BID.dta"
local in301 = "${surveysFolder}\harmonized\VEN\EHM\data_arm\VEN_1995s2_BID.dta"
local in302 = "${surveysFolder}\harmonized\VEN\EHM\data_arm\VEN_1996s2_BID.dta"
local in303 = "${surveysFolder}\harmonized\VEN\EHM\data_arm\VEN_1997s2_BID.dta"
local in304 = "${surveysFolder}\harmonized\VEN\EHM\data_arm\VEN_1998s2_BID.dta"
local in305 = "${surveysFolder}\harmonized\VEN\EHM\data_arm\VEN_1999s2_BID.dta"
local in306 = "${surveysFolder}\harmonized\VEN\EHM\data_arm\VEN_2000s2_BID.dta"
local in307 = "${surveysFolder}\harmonized\VEN\EHM\data_arm\VEN_2001s2_BID.dta"
local in308 = "${surveysFolder}\harmonized\VEN\EHM\data_arm\VEN_2002s2_BID.dta"
local in309 = "${surveysFolder}\harmonized\VEN\EHM\data_arm\VEN_2003s2_BID.dta"
local in310 = "${surveysFolder}\harmonized\VEN\EHM\data_arm\VEN_2004s2_BID.dta"
local in311 = "${surveysFolder}\harmonized\VEN\EHM\data_arm\VEN_2005s2_BID.dta"
local in312 = "${surveysFolder}\harmonized\VEN\EHM\data_arm\VEN_2006s2_BID.dta"
local in313 = "${surveysFolder}\harmonized\VEN\EHM\data_arm\VEN_2007s2_BID.dta"
local in314 = "${surveysFolder}\harmonized\VEN\EHM\data_arm\VEN_2008s2_BID.dta"
local in315 = "${surveysFolder}\harmonized\VEN\EHM\data_arm\VEN_2009s2_BID.dta"
local in316 = "${surveysFolder}\harmonized\VEN\EHM\data_arm\VEN_2010s2_BID.dta"
*Jamaica
local in317 = "${surveysFolder}\harmonized\JAM\LFS\data_arm\JAM_1990m5_BID.dta"
local in318 = "${surveysFolder}\harmonized\JAM\LFS\data_arm\JAM_1991m5_BID.dta"
local in319 = "${surveysFolder}\harmonized\JAM\LFS\data_arm\JAM_1992m5_BID.dta"
local in320 = "${surveysFolder}\harmonized\JAM\LFS\data_arm\JAM_1993m5_BID.dta"
local in321 = "${surveysFolder}\harmonized\JAM\LFS\data_arm\JAM_1995m5_BID.dta"
local in322 = "${surveysFolder}\harmonized\JAM\LFS\data_arm\JAM_1996m5_BID.dta"
local in323 = "${surveysFolder}\harmonized\JAM\LFS\data_arm\JAM_1997m5_BID.dta"
local in324 = "${surveysFolder}\harmonized\JAM\LFS\data_arm\JAM_2002m4_BID.dta"
local in325 = "${surveysFolder}\harmonized\JAM\LFS\data_arm\JAM_2005m4_BID.dta"
local in326 = "${surveysFolder}\harmonized\JAM\LFS\data_arm\JAM_2006m4_BID.dta"
local in327 = "${surveysFolder}\harmonized\JAM\LFS\data_arm\JAM_2007m10_BID.dta"
local in328 = "${surveysFolder}\harmonized\JAM\LFS\data_arm\JAM_2008m4_BID.dta"
local in329 = "${surveysFolder}\harmonized\JAM\LFS\data_arm\JAM_2009m10_BID.dta"
local in330 = "${surveysFolder}\harmonized\JAM\LFS\data_arm\JAM_2010m4_BID.dta"
local in331 = "${surveysFolder}\harmonized\JAM\LFS\data_arm\JAM_2012m4_BID.dta"


set more off


forvalues i=`from'(1)`to' {
	use `in`i'', clear
	

capture drop pc

*region_BID_c and mes_c were not included.

*________________________________________________________________*
*                   Categorical variables                        *
*________________________________________________________________*


local categoricas region_c zona_c sexo_ci raza_ci relacion_ci civil_ci jefe_ci nconyuges_ch nhijos_ch	notropari_ch notronopari_ch	nempdom_ch clasehog_ch nmiembros_ch miembros_ci nmayor21_ch nmenor21_ch nmayor65_ch nmenor6_ch	nmenor1_ch	condocup_ci categoinac_ci nempleos_ci emp_ci desemp_ci cesante_ci pea_ci desalent_ci subemp_ci tiempoparc_ci categopri_ci categosec_ci rama_ci spublico_ci tamemp_ci cotizando_ci instcot_ci afiliado_ci formal_ci tipocontrato_ci ocupa_ci pensionsub_ci pension_ci tipopen_ci instpen_ci aedu_ci eduno_ci edupi_ci edupc_ci	edusi_ci edusc_ci eduui_ci eduuc_ci	edus1i_ci edus1c_ci edus2i_ci edus2c_ci edupre_ci eduac_ci asiste_ci pqnoasis_ci	repite_ci repiteult_ci edupub_ci tecnica_ci aguared_ch aguadist_ch aguamala_ch aguamide_ch luz_ch luzmide_ch combust_ch	bano_ch banoex_ch des1_ch des2_ch piso_ch pared_ch techo_ch resid_ch dorm_ch cuartos_ch cocina_ch telef_ch refrig_ch freez_ch auto_ch compu_ch internet_ch cel_ch vivi1_ch vivi2_ch viviprop_ch vivitit_ch 

foreach x of local categoricas {
preserve
contract pais_c anio_c `x', zero freq(freq) percent(pc)
g var = "`x'"
capture rename `x' categories
saveold "$p\\`x'`i'.dta", replace
restore
        }


*________________________________________________________________*
*                Continuous variables                          *
*________________________________________________________________*



*Incomes (nrylmpri_ci tcylmpri_ci nrylmpri_ch tcylmpri_ch were not included)

*Note: idh_ch idp_ci factor_ch factor_ci were included just to verify that they were generated correctly, but they do not have any economic meaning.

local cont idh_ch idp_ci factor_ch factor_ci edad_ci antiguedad_ci durades_ci horaspri_ci horastot_ci ylmpri_ci  ylnmpri_ci ylmsec_ci ylnmsec_ci ylmotros_ci	ylnmotros_ci ylm_ci	ylnm_ci	ynlm_ci	ynlnm_ci ylm_ch	ylnm_ch	ylmnr_ch ynlm_ch ynlnm_ch ylmhopri_ci ylmho_ci rentaimp_ch autocons_ci autocons_ch remesas_ci remesas_ch ypen_ci ypensub_ci vivialq_ch vivialqimp_ch

foreach x of local cont {

sum `x'

capture drop var*
capture drop pc
gen mean`x' = r(mean)  
gen min`x' = r(min)
gen max`x'= r(max)

preserve
capture rename mean`x' var1
capture rename min`x' var2
capture rename max`x' var3
capture rename anio_c var4
local anio = var4

contract pais_c var1 var2 var3, zero freq(var5) percent(var6) 
reshape long var, i( pais_c) j(variable)

g anio_c = `anio'

label define variable 1 "Mean" 2"Min" 3 "Max" 5"Freq" 6"Percent" 
label value variable variable
rename var stat
rename variable varname 
g var = "`x'"
destring stat, replace
saveold "$p\\`x'`i'.dta", replace
restore
        }


*________________________________________________________________*
*                Scalars                                         *
*________________________________________________________________*


local scalars salmm_ci lp25_ci lp4_ci lp_ci lpe_ci

capture drop pc
foreach x of local scalars {
capture drop var*

sum `x'
gen mean`x' = r(mean)  
gen min`x' = r(min)
gen max`x'= r(max)
preserve
capture rename mean`x' var1
capture rename min`x' var2
capture rename max`x' var3
capture rename anio_c var4
local anio = var4
contract pais_c var1 var2 var3 , zero freq(var5) percent(var6)
reshape long var, i( pais_c) j(variable)
g anio_c = `anio'
label define variable 1 "Mean" 2"Min" 3 "Max" 5"Freq" 6"Percent"
label value variable variable
rename var stat
rename variable varname 
g var = "`x'"
destring stat, replace
saveold "$p\\`x'`i'.dta", replace
restore
        }

*________________________________________________________________*
*                 Gini coefficient                               *
*________________________________________________________________*
* Gini ylmhopri_ci and ylm_ci

local ingresos ylmhopri_ci  ylm_ci
capture drop pc
foreach x of local ingresos {
capture inequal7 `x' [aw=factor_ci] if (edad_ci>=15 & edad_ci<=64) & (`x' > 0 & `x' !=.)
capture g gini_`x' = r(gini)
}

local gini gini_ylmhopri_ci gini_ylm_ci
foreach x of local gini {
	
preserve

contract pais_c anio_c `x', freq(freq)
capture g var = "`x'"
capture g varname = "Gini_15-64_yrs"
capture rename `x' stat
destring stat, replace
saveold "$p\\`x'`i'.dta", replace
restore
	}	

}






drop _all

*________________________________________________________________________________________________*
*     S E C T I O N     T W O:  R E S U L T S   I N   E X C E L  F O R M A T
*________________________________________________________________________________________________*

*Please, modify just the lines that start with: *###

*### FROM which database. Lower limit for tables of results (Databases listed at the beginning of this do file)
local from = 234

*### TO which database. Upper limit for tables of results (Databases listed at the beginning of this do file)
local to = 331

*### r: path where results in excel format will be stored.
global r ="${surveysFolder}\DATA.IDB\Armonización de Bases AL\Revisión_tendencias\output"

*### p: it is the path of the output; this is, where temporary files were stored (same "p" than the beginning of this do file)
global p = "${surveysFolder}\DATA.IDB\Armonización de Bases AL\Revisión_tendencias\output\ALL"

*### n: name of the results file in excel format
local n = "trendsdb"

*________________________________________________________________________________________________*






set more off


forval i= `from'/`to'{

foreach file in "$p\region_c`i'.dta" "$p\zona_c`i'.dta" "$p\sexo_ci`i'.dta" "$p\raza_ci`i'.dta" "$p\relacion_ci`i'.dta" "$p\civil_ci`i'.dta" "$p\jefe_ci`i'.dta" "$p\nconyuges_ch`i'.dta" "$p\nhijos_ch`i'.dta"	"$p\notropari_ch`i'.dta" "$p\notronopari_ch`i'.dta" "$p\nempdom_ch`i'.dta" "$p\clasehog_ch`i'.dta" "$p\nmiembros_ch`i'.dta" "$p\miembros_ci`i'.dta" "$p\nmayor21_ch`i'.dta" "$p\nmenor21_ch`i'.dta" "$p\nmayor65_ch`i'.dta" "$p\nmenor6_ch`i'.dta" "$p\nmenor1_ch`i'.dta" "$p\condocup_ci`i'.dta" "$p\categoinac_ci`i'.dta" "$p\nempleos_ci`i'.dta" "$p\emp_ci`i'.dta" "$p\desemp_ci`i'.dta" "$p\cesante_ci`i'.dta" "$p\pea_ci`i'.dta" "$p\desalent_ci`i'.dta" "$p\subemp_ci`i'.dta" "$p\tiempoparc_ci`i'.dta" "$p\categopri_ci`i'.dta" "$p\categosec_ci`i'.dta" "$p\rama_ci`i'.dta" "$p\spublico_ci`i'.dta" "$p\tamemp_ci`i'.dta" "$p\cotizando_ci`i'.dta" "$p\instcot_ci`i'.dta" "$p\afiliado_ci`i'.dta" "$p\formal_ci`i'.dta" "$p\tipocontrato_ci`i'.dta" "$p\ocupa_ci`i'.dta" "$p\pensionsub_ci`i'.dta" "$p\pension_ci`i'.dta" "$p\tipopen_ci`i'.dta" "$p\instpen_ci`i'.dta" "$p\aedu_ci`i'.dta" "$p\eduno_ci`i'.dta" "$p\edupi_ci`i'.dta" "$p\edupc_ci`i'.dta"	"$p\edusi_ci`i'.dta" "$p\edusc_ci`i'.dta" "$p\eduui_ci`i'.dta" "$p\eduuc_ci`i'.dta" "$p\edus1i_ci`i'.dta" "$p\edus1c_ci`i'.dta" "$p\edus2i_ci`i'.dta" "$p\edus2c_ci`i'.dta" "$p\edupre_ci`i'.dta" "$p\eduac_ci`i'.dta" "$p\asiste_ci`i'.dta" "$p\pqnoasis_ci`i'.dta" "$p\repite_ci`i'.dta" "$p\repiteult_ci`i'.dta" "$p\edupub_ci`i'.dta" "$p\tecnica_ci`i'.dta" "$p\aguared_ch`i'.dta" "$p\aguadist_ch`i'.dta" "$p\aguamala_ch`i'.dta" "$p\aguamide_ch`i'.dta" "$p\luz_ch`i'.dta" "$p\luzmide_ch`i'.dta" "$p\combust_ch`i'.dta" "$p\bano_ch`i'.dta" "$p\banoex_ch`i'.dta" "$p\des1_ch`i'.dta" "$p\des2_ch`i'.dta" "$p\piso_ch`i'.dta" "$p\pared_ch`i'.dta" "$p\techo_ch`i'.dta" "$p\resid_ch`i'.dta" "$p\dorm_ch`i'.dta" "$p\cuartos_ch`i'.dta" "$p\cocina_ch`i'.dta" "$p\telef_ch`i'.dta" "$p\refrig_ch`i'.dta" "$p\freez_ch`i'.dta" "$p\auto_ch`i'.dta" "$p\compu_ch`i'.dta" "$p\internet_ch`i'.dta" "$p\cel_ch`i'.dta" "$p\vivi1_ch`i'.dta" "$p\vivi2_ch`i'.dta" "$p\viviprop_ch`i'.dta" "$p\vivitit_ch`i'.dta" {
                append using "`file'", force
        }
		
}

capture drop varname stat

levelsof pais_c, local(country)

foreach x of local country {

export excel using "$r\\`n'" if pais_c == "`x'" , sheet("`x'") sheetmodify cell(B10) firstrow(variables)

}



drop _all
	

forval i= `from'/`to'{

foreach file in "$p\idh_ch`i'.dta" "$p\idp_ci`i'.dta" "$p\factor_ch`i'.dta" "$p\factor_ci`i'.dta" "$p\edad_ci`i'.dta" "$p\antiguedad_ci`i'.dta" "$p\durades_ci`i'.dta" "$p\horaspri_ci`i'.dta" "$p\horastot_ci`i'.dta" "$p\ylmpri_ci`i'.dta" "$p\ylnmpri_ci`i'.dta" "$p\ylmsec_ci`i'.dta" "$p\ylnmsec_ci`i'.dta" "$p\ylmotros_ci`i'.dta" "$p\ylnmotros_ci`i'.dta" "$p\ylm_ci`i'.dta" "$p\ylnm_ci`i'.dta" "$p\ynlm_ci`i'.dta" "$p\ynlnm_ci`i'.dta" "$p\ylm_ch`i'.dta" "$p\ylnm_ch`i'.dta" "$p\ylmnr_ch`i'.dta" "$p\ynlm_ch`i'.dta" "$p\ynlnm_ch`i'.dta" "$p\ylmhopri_ci`i'.dta" "$p\ylmho_ci`i'.dta" "$p\rentaimp_ch`i'.dta" "$p\autocons_ci`i'.dta" "$p\autocons_ch`i'.dta" "$p\remesas_ci`i'.dta" "$p\remesas_ch`i'.dta" "$p\ypen_ci`i'.dta" "$p\ypensub_ci`i'.dta" "$p\vivialq_ch`i'.dta" "$p\vivialqimp_ch`i'.dta" "$p\salmm_ci`i'.dta" "$p\lp25_ci`i'.dta" "$p\lp4_ci`i'.dta" "$p\lp_ci`i'.dta" "$p\lpe_ci`i'.dta" "$p\gini_ylmhopri_ci`i'.dta" "$p\gini_ylm_ci`i'.dta" {
                append using "`file'", force
        }
		
}



levelsof pais_c, local(country)

foreach x of local country {

export excel using "$r\\`n'" if pais_c == "`x'" , sheet("`x'") sheetmodify cell(K10) firstrow(variables)

}








/*This is to save all countries in one sheet by categorical and continuous.

set more off


forval i= `from'/`to'{

foreach file in "$p\region_c`i'.dta" "$p\zona_c`i'.dta" "$p\sexo_ci`i'.dta" "$p\raza_ci`i'.dta" "$p\relacion_ci`i'.dta" "$p\civil_ci`i'.dta" "$p\jefe_ci`i'.dta" "$p\nconyuges_ch`i'.dta" "$p\nhijos_ch`i'.dta"	"$p\notropari_ch`i'.dta" "$p\notronopari_ch`i'.dta" "$p\nempdom_ch`i'.dta" "$p\clasehog_ch`i'.dta" "$p\nmiembros_ch`i'.dta" "$p\miembros_ci`i'.dta" "$p\nmayor21_ch`i'.dta" "$p\nmenor21_ch`i'.dta" "$p\nmayor65_ch`i'.dta" "$p\nmenor6_ch`i'.dta" "$p\nmenor1_ch`i'.dta" "$p\condocup_ci`i'.dta" "$p\categoinac_ci`i'.dta" "$p\nempleos_ci`i'.dta" "$p\emp_ci`i'.dta" "$p\desemp_ci`i'.dta" "$p\cesante_ci`i'.dta" "$p\pea_ci`i'.dta" "$p\desalent_ci`i'.dta" "$p\subemp_ci`i'.dta" "$p\tiempoparc_ci`i'.dta" "$p\categopri_ci`i'.dta" "$p\categosec_ci`i'.dta" "$p\rama_ci`i'.dta" "$p\spublico_ci`i'.dta" "$p\tamemp_ci`i'.dta" "$p\cotizando_ci`i'.dta" "$p\instcot_ci`i'.dta" "$p\afiliado_ci`i'.dta" "$p\formal_ci`i'.dta" "$p\tipocontrato_ci`i'.dta" "$p\ocupa_ci`i'.dta" "$p\pensionsub_ci`i'.dta" "$p\pension_ci`i'.dta" "$p\tipopen_ci`i'.dta" "$p\instpen_ci`i'.dta" "$p\aedu_ci`i'.dta" "$p\eduno_ci`i'.dta" "$p\edupi_ci`i'.dta" "$p\edupc_ci`i'.dta"	"$p\edusi_ci`i'.dta" "$p\edusc_ci`i'.dta" "$p\eduui_ci`i'.dta" "$p\eduuc_ci`i'.dta" "$p\edus1i_ci`i'.dta" "$p\edus1c_ci`i'.dta" "$p\edus2i_ci`i'.dta" "$p\edus2c_ci`i'.dta" "$p\edupre_ci`i'.dta" "$p\eduac_ci`i'.dta" "$p\asiste_ci`i'.dta" "$p\pqnoasis_ci`i'.dta" "$p\repite_ci`i'.dta" "$p\repiteult_ci`i'.dta" "$p\edupub_ci`i'.dta" "$p\tecnica_ci`i'.dta" "$p\aguared_ch`i'.dta" "$p\aguadist_ch`i'.dta" "$p\aguamala_ch`i'.dta" "$p\aguamide_ch`i'.dta" "$p\luz_ch`i'.dta" "$p\luzmide_ch`i'.dta" "$p\combust_ch`i'.dta" "$p\bano_ch`i'.dta" "$p\banoex_ch`i'.dta" "$p\des1_ch`i'.dta" "$p\des2_ch`i'.dta" "$p\piso_ch`i'.dta" "$p\pared_ch`i'.dta" "$p\techo_ch`i'.dta" "$p\resid_ch`i'.dta" "$p\dorm_ch`i'.dta" "$p\cuartos_ch`i'.dta" "$p\cocina_ch`i'.dta" "$p\telef_ch`i'.dta" "$p\refrig_ch`i'.dta" "$p\freez_ch`i'.dta" "$p\auto_ch`i'.dta" "$p\compu_ch`i'.dta" "$p\internet_ch`i'.dta" "$p\cel_ch`i'.dta" "$p\vivi1_ch`i'.dta" "$p\vivi2_ch`i'.dta" "$p\viviprop_ch`i'.dta" "$p\vivitit_ch`i'.dta" {
                append using "`file'", force
        }
		
}




export excel using "${surveysFolder}\DATA.IDB\Armonización de	Bases AL\Revisión_tendencias\output\trendsdb.xls", sheet("All catego2") sheetmodify cell(K10) firstrow(variables)  nolabel


drop _all


forval i= `from'/`to'{

foreach file in "$p\idh_ch`i'.dta" "$p\idp_ci`i'.dta" "$p\factor_ch`i'.dta" "$p\factor_ci`i'.dta" "$p\edad_ci`i'.dta" "$p\antiguedad_ci`i'.dta" "$p\durades_ci`i'.dta" "$p\horaspri_ci`i'.dta" "$p\horastot_ci`i'.dta" "$p\ylmpri_ci`i'.dta" "$p\ylnmpri_ci`i'.dta" "$p\ylmsec_ci`i'.dta" "$p\ylnmsec_ci`i'.dta" "$p\ylmotros_ci`i'.dta" "$p\ylnmotros_ci`i'.dta" "$p\ylm_ci`i'.dta" "$p\ylnm_ci`i'.dta" "$p\ynlm_ci`i'.dta" "$p\ynlnm_ci`i'.dta" "$p\ylm_ch`i'.dta" "$p\ylnm_ch`i'.dta" "$p\ylmnr_ch`i'.dta" "$p\ynlm_ch`i'.dta" "$p\ynlnm_ch`i'.dta" "$p\ylmhopri_ci`i'.dta" "$p\ylmho_ci`i'.dta" "$p\rentaimp_ch`i'.dta" "$p\autocons_ci`i'.dta" "$p\autocons_ch`i'.dta" "$p\remesas_ci`i'.dta" "$p\remesas_ch`i'.dta" "$p\ypen_ci`i'.dta" "$p\ypensub_ci`i'.dta" "$p\vivialq_ch`i'.dta" "$p\vivialqimp_ch`i'.dta" "$p\salmm_ci`i'.dta" "$p\lp25_ci`i'.dta" "$p\lp4_ci`i'.dta" "$p\lp_ci`i'.dta" "$p\lpe_ci`i'.dta" "$p\gini_ylmhopri_ci`i'.dta" "$p\gini_ylm_ci`i'.dta" {
                append using "`file'", force
        }
		
}



export excel using "${surveysFolder}\DATA.IDB\Armonización de Bases AL\Revisión_tendencias\output\trendsdb.xls", sheet("All continuo2") sheetmodify cell(B10) firstrow(variables) nolabel




