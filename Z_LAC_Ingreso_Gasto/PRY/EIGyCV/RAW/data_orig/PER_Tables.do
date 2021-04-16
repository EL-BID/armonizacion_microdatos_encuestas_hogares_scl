

*** 2000: desnutricion

cd ${surveysFolder}\DATA.IDB\A_PER\PARAGUAY\EIH_2000
use r05_00, clear
joinby upm nvivi nhoga l02 using r02_00, unm(b)
drop _merge-p06 p09-recc02
joinby upm nvivi nhoga using r01_00, unm(b)
drop _merge-totnm fecha2-nbia1
joinby upm nvivi nhoga using "ingreso gasto 2000-2001", unm(b)

tostring fecha1, replace
g e_ano  = substr(fecha1,-2,.)
g e_mes  = substr(fecha1,-4,2)
g e_dia  = substr(fecha1,-6,2)
g e_dia1 = substr(fecha1,-5,1)
destring e_ano e_mes e_dia e_dia1, replace
replace e_dia = e_dia1 if e_dia==.
replace e_ano = 2000+e_ano
drop fecha1 e_dia1

g       edadm = (e_ano - p08a)*12 + (e_mes - p08m) + (e_dia - p08d)/30.416667
replace edadm=. if edadm<0

recode sexo (1=1) (6=2)      // 1 hombre, 2 mujer

recode si22 (999.9=.)
recode si25 (99.999=.)

* z-scores 
zscore06, a(edadm) s(sexo) h(si22) w(si25)

* denutricion (stunting): talla / edad)
g       desn1 = 1 if haz06>-50 & haz06<-2
replace desn1 = 2 if haz06>=-2 & haz06< 2
replace desn1 = 3 if haz06>= 2 & haz06<50
*1 cronico (talla baja) 2 normal 3 talla alta 

* denutricion (wasting):(peso / talla)
g       desn2 = 1 if whz06>-30 & whz06<-2
replace desn2 = 2 if whz06>=-2 & whz06< 2
replace desn2 = 3 if whz06>= 2 & whz06<50
*1 aguda 2 normal 3 sobrepeso u obesidad

gen dcron  = (desn1==1)*100 if (desn1>=1 & desn1<=3)
gen sobreo = (desn2==3)*100 if (desn2>=1 & desn2<=3)

table pobrezai area  [w=fex], c(mean dcron) col row
table pobrezai area  [w=fex], c(mean sobreo) col row

*** CUADRO 4

****************
*** malnutricion
****************

cd ${surveysFolder}\DATA.IDB\A_PER\EPH_2003-11\Eph2005
use r04_05, clear
joinby upm nvivi nhoga l02 using r02_05, unm(b)
drop _merge-p05c p09-npad tipohoga-recrama
joinby upm nvivi nhoga using r01_05, unm(b)
drop _merge-totnm f2-cg53
joinby upm nvivi nhoga using ingrefam_05, unm(b)

* fecha de entrevista
tostring f1, replace
g e_ano  = substr(f1,-2,.)
g e_mes  = substr(f1,-4,2)
g e_dia  = substr(f1,-6,2)
g e_dia1 = substr(f1,-5,1)
destring e_ano e_mes e_dia e_dia1, replace
replace e_dia = e_dia1 if e_dia==.
replace e_ano = 2000+e_ano
drop f1 e_dia1
			 
* edad en meses, sexo, talla, peso
g       edadm = (e_ano - p08a)*12 + (e_mes - p08m) + (e_dia - p08d)/30.416667
replace edadm=. if edadm<0

recode sexo (1=1) (6=2)      // 1 hombre, 2 mujer

recode si21 (999.9=.)
recode si24 (99.999=.)

* z-scores 
zscore06, a(edadm) s(sexo) h(si21) w(si24)

g stunting    = (haz06> -6 & haz06<-2) if (haz06> -6 & haz06< 6)
g wasting     = (whz06>=-5 & whz06<-2) if (whz06>=-5 & whz06< 5)
g underweight = (waz06> -6 & waz06<-2) if (waz06> -6 & waz06< 5)
g overweight  = (whz06>= 2 & whz06< 5) if (whz06>=-5 & whz06< 5)
g obesity     = (whz06>= 3 & whz06< 5) if (whz06>=-5 & whz06< 5)

* tabla malnutrition
tabstat stunting wasting underweight overweight obesity [w=fex], by(quintili)
tabstat stunting wasting underweight overweight obesity [w=fex], by(area)
tabstat stunting wasting underweight overweight obesity [w=fex], by(sexo)

*** educacion de la madre (menos de 6 anos de educacion)
g educ6 = 100*((nmad>=0 & nmad<=305) | (nmad>=1001 & nmad<=1003) | nmad==9999) if (desn1>=1 & desn1<=3)

*** tablas

table pobrezai area  [w=fex], c(mean dcron) col row
table pobrezai area  [w=fex], c(mean sobreo) col row
table pobrezai area  [w=fex], c(mean sobrep) col row
table pobrezai dcron [w=fex], c(mean educ6) col row
