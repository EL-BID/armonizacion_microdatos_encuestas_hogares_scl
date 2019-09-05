* PARAGUAY: EIGCV 2011-2012


use Z:\survey\PRY\EIGyCV\2011_2012\reg02t.dta, clear
joinby upm nvivi nhoga using "D:\DATA.IDB\A_PER\EPH_2003-11\EIGyCV_2011_2012\ingrefam.dta", unm(b)

destring p06d p06m p06a fva fvm fvd, replace
recode p06d p06m p06a (99 9999 =.)
recode si21 si24 (999.99 999.999 =.)
recode si22 (9=.)

g edadmes = (fva-p06a)*12 + (fvm-p06m) + (fvd-p06d)/30.416667 if edad<5
recode p05 (1=1) (6=2) if edad<5, g(sexo)
recode edadmes (0/5.999=1) (6/8.999=2) (9/11.999=3) (12/17.999=4) (18/23.999=5) (24/35.999=6) (36/47.999=7) (48/59.999=8), g(gedad)

g       educ = 1 if (ed03==6 | (ed04>=100 & ed04<=309) | ed04==1701 | ed04==1800)
replace educ = 2 if ((ed04>=401 & ed04<=406)  | (ed04>=1201 & ed04<=1204) | (ed04>=1401 & ed04<=1403))
replace educ = 3 if ((ed04>=507 & ed04<=1103) | (ed04>=1301 & ed04<=1304) | (ed04>=1501 & ed04<=1504) | (ed04>=1901 & ed04<=2306))

g      educm1 = educ if (p02==1 | p02==2) & p05==6
egen   medad = max(edad) if (p02==3 | p02==4) & p05==6, by(upm nvivi nhoga)
g      educm2 = educ if medad==1
egen educ_ma  = total(educm1), by(upm nvivi nhoga)
egen educ_mah = total(educm2), by(upm nvivi nhoga)
replace educ_ma = educ_mah if p02==5 & educ_ma==0
recode educ_ma (0=1)

keep if edad<5

zscore06, a(edadmes) s(sexo) h(si21) w(si24) measure(si22)

* indicadores (proporcion de ninos menores a 5)
g stunting    = (haz06> -6 & haz06<-2) if (haz06> -6 & haz06< 6)
g wasting     = (whz06>=-5 & whz06<-2) if (whz06>=-5 & whz06< 5)
g underweight = (waz06> -6 & waz06<-2) if (waz06> -6 & waz06< 5)
g overweight  = (whz06>= 2 & whz06< 5) if (whz06>=-5 & whz06< 5)
g obesity     = (whz06>= 3 & whz06< 5) if (whz06>=-5 & whz06< 5)

* tabla malnutrition
tabstat stunting wasting underweight overweight obesity [w=fex], by(area)
tabstat stunting wasting underweight overweight obesity [w=fex], by(sexo)
tabstat stunting wasting underweight overweight obesity [w=fex], by(gedad)
tabstat stunting wasting underweight overweight obesity [w=fex], by(educ_ma)
tabstat stunting wasting underweight overweight obesity [w=fex], by(quintili)











* tabla gasto en alimentos vs desnutricion
joinby upm nvivi nhoga using "D:\temp1.dta", unm(b)
keep if _merge==3
tabstat TOTAL1011 - TOTAL1051 [w=fex], by(stunting)
tabstat TOTAL1011 - TOTAL1051 [w=fex], by(overweight)


* tabla gasto en alimentos vs desnutricion - 9 digitos

use "D:\DATA.IDB\A_PER\EPH_2003-11\EIGyCV_2011_2012\agregado_de_gastos.dta", clear
keep if ga01c>=100000000 & ga01c<200000000             // solo alimentos
keep upm nvivi nhoga ga01c TOTAL
joinby upm nvivi nhoga using "D:\desnut.dta", unm(b)
keep if _merge==3

set more off
table ga01c stunting [w=fex], c(mean TOTAL)
