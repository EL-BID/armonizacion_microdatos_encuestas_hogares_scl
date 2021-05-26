clear all
cd "${surveysFolder}\harmonized\ARG\EPHC\program"
set more off
foreach file in ARG_2003s2_mergeBID.do ARG_2003s2_variablesBID.do ARG_2004-2012s2_variablesBID_old.do ARG_2004-2012t3_mergeBID_old.do ARG_2004s1_mergeBID.do ARG_2004s1_variablesBID.do ARG_2004s2_mergeBID.do ///
ARG_2004s2_variablesBID.do ARG_2005s1_mergeBID.do ARG_2005s1_variablesBID.do ARG_2005s2_mergeBID.do ARG_2005s2_variablesBID.do ARG_2006s1_mergeBID.do ARG_2006s1_variablesBID.do ///
ARG_2006s2_mergeBID.do ARG_2006s2_variablesBID.do ARG_2007s1_mergeBID.do ARG_2007s1_variablesBID.do ARG_2007s2_mergeBID.do ARG_2007s2_variablesBID.do ARG_2008s1_mergeBID.do ARG_2008s1_variablesBID.do ///
ARG_2008s2_mergeBID.do ARG_2008s2_variablesBID.do ARG_2009s1_mergeBID.do ARG_2009s1_variablesBID.do ARG_2009s2_mergeBID.do ARG_2009s2_variablesBID.do ARG_2010s1_mergeBID.do ARG_2010s1_variablesBID.do ///
ARG_2010s2_mergeBID.do ARG_2010s2_variablesBID.do ARG_2011s1_mergeBID.do ARG_2011s1_variablesBID.do ARG_2011s2_mergeBID.do ARG_2011s2_variablesBID.do ARG_2012s1_mergeBID.do ARG_2012s1_variablesBID.do ///
ARG_2012s2_mergeBID.do ARG_2012s2_variablesBID.do ARG_2013s1_mergeBID.do ARG_2013s1_variablesBID.do ARG_2013s2_mergeBID.do ARG_2013s2_variablesBID.do ARG_2014s1_mergeBID.do ARG_2014s1_variablesBID.do ///
ARG_2014s2_mergeBID.do ARG_2014s2_variablesBID.do ARG_2015s1_mergeBID.do ARG_2015s1_variablesBID.do ARG_2016s2_mergeBID.do ARG_2016s2_variablesBID.do  ARG_2017s1_mergeBID.do ARG_2017s1_variablesBID.do  ///
ARG_2017s2_mergeBID.do ARG_2017s2_variablesBID.do ARG_2018s2_mergeBID.do ARG_2018s2_variablesBID.do {

unicode analyze  "`file'"
unicode encoding set "latin1"
unicode translate  "`file'"
}


clear all
cd "${surveysFolder}\harmonized\ECU\ENEMDU\program"
set more off
foreach file in ECU_2017m12_mergeBID.do ECU_2017m12_variablesBID.do ECU_2018m12_mergeBID.do ECU_2018m12_variablesBID.do{
unicode analyze  "`file'"
unicode encoding set "latin1"
unicode translate  "`file'"
}
*

clear all
cd "${surveysFolder}\harmonized\ARG\EPHC\program"
set more off
foreach file in ARG_2003s2_variablesBID.do ARG_2004s1_variablesBID.do ARG_2004s2_variablesBID.do ARG_2005s1_variablesBID.do ARG_2005s2_variablesBID.do ARG_2006s1_variablesBID.do ///
ARG_2006s2_variablesBID.do ARG_2007s1_variablesBID.do ARG_2007s2_variablesBID.do ARG_2008s1_variablesBID.do ///
ARG_2008s2_variablesBID.do ARG_2009s1_variablesBID.do ARG_2009s2_variablesBID.do ARG_2010s1_variablesBID.do ///
ARG_2010s2_variablesBID.do ARG_2011s1_variablesBID.do ARG_2011s2_variablesBID.do ARG_2012s1_variablesBID.do ///
ARG_2012s2_variablesBID.do ARG_2013s1_variablesBID.do ARG_2013s2_variablesBID.do ARG_2014s1_variablesBID.do ///
ARG_2014s2_variablesBID.do ARG_2015s1_variablesBID.do ARG_2016s2_variablesBID.do ARG_2017s1_variablesBID.do  ///
ARG_2017s2_variablesBID.do ARG_2018s2_variablesBID.do {

do "`file'"
}
*

