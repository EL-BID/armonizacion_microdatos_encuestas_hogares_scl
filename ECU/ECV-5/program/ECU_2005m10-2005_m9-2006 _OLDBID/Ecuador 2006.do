clear


use "${surveysFolder}\Data.idb\Victoria\Food\Agregadoi Ecuador 2006.dta" 

*rename numhog idh_ch
egen idh_ch=group(region ciudad zona sector numhog)
sort idh_ch

save "${surveysFolder}\Data.idb\Victoria\Food\GastosECU.dta", replace


use "${surveysFolder}\ARM\ECU\ECV\2006\Data\ecv0506.dta" 


gen yindep=.
replace yindep=pa22a/12 if   pa22b==7 
replace yindep=pa22a/6 if   pa22b==6
replace yindep=pa22a/3 if   pa22b==5
replace yindep=pa22a if   pa22b==4 
replace yindep=pa22a*2 if   pa22b==3
replace yindep=pa22a*4.2 if   pa22b==2
replace yindep=pa22a*(pa18)*4.2 if   pa22b==1 

egen bonosaux=rsum(pa28b pa30b pa30d pa30f)
replace bonosaux=. if pa28b==. & pa30b==. & pa30d==. & pa30f==. 
gen bonos=bonosaux/12

 
gen yasalariados=pa35
  
egen extrasaux=rsum(pa38b pa39b) 
replace extrasaux=. if pa38b==. &  pa39b==.
gen extras=extrasaux/12

gen alimentos=.
replace alimentos=pa40b if pa40c==4
replace alimentos=pa40b*2 if pa40c==3
replace alimentos=pa40b*4.2 if pa40c==2
replace alimentos=pa40b*20 if pa40c==4

gen yvivienda=pa41b
gen transport=pa43b

gen yindepsec=.
replace yindepsec=pa56a/12 if   pa56b==7 
replace yindepsec=pa56a/6 if   pa56b==6
replace yindepsec=pa56a/3 if   pa56b==5
replace yindepsec=pa56a if   pa56b==4 
replace yindepsec=pa56a*2 if   pa56b==3
replace yindepsec=pa56a*4.2 if   pa56b==2
replace yindepsec=pa56a*20 if   pa56b==1 

gen ysalsec=.
replace ysalsec=pa57a if pa57b==4
replace ysalsec=pa57a*2 if pa57b==3
replace ysalsec=pa57a*4.2 if pa57b==2
replace ysalsec=pa57a*20 if pa57b==4

gen ysalsec2=pa58


egen extrasec=rsum(pa61b pa62b)
replace extrasec=. if pa61b==. &  pa62b==.
 
gen rempais=.
replace rempais=pa77b/12 if   pa77c==7 
replace rempais=pa77b/6 if   pa77c==6
replace rempais=pa77b/3 if   pa77c==5
replace rempais=pa77b if   pa77c==4 
replace rempais=pa77b*2 if   pa77c==3
replace rempais=pa77b*4.2 if   pa77c==2
replace rempais=pa77b*30 if   pa77c==1 

 
gen remext=.
replace remext=pa78b/12 if   pa78c==7 
replace remext=pa78b/6 if   pa78c==6
replace remext=pa78b/3 if   pa78c==5
replace remext=pa78b if   pa78c==4 
replace remext=pa78b*2 if   pa78c==3
replace remext=pa78b*4.2 if   pa78c==2
replace remext=pa78b*30 if   pa78c==1 


gen bdh=pa81b
 
gen ayudaorg=.
replace ayudaorg=pa83b/12 if   pa83c==7 
replace ayudaorg=pa83b/6 if   pa83c==6
replace ayudaorg=pa83b/3 if   pa83c==5
replace ayudaorg=pa83b if   pa83c==4 
replace ayudaorg=pa83b*2 if   pa83c==3
replace ayudaorg=pa83b*4.2 if   pa83c==2
replace ayudaorg=pa83b*30 if   pa83c==1 

gen alquiler=ia0102

egen interesaux=rsum(ib0104 ib0204) 
replace interesaux=. if ib0104==. & ib0204==. 
gen interes=interesaux/3
  
gen jubi= ib0102

egen becasaux=rsum(ia0104 ia0204 ia0304) 
replace becasaux=. if ia0104==. & ia0204==. & ia0304==.
gen becas=becasaux/12

egen otrosaux=rsum(ic0102 ic0202 ic0302 ic0402) 
replace otrosaux=. if  ic0102==. &  ic0202==. &  ic0302==. &  ic0402==. 
gen otros=otrosaux/12

egen transacaux=rsum( id0102 id0202 id0302 id0402) 
replace transacaux=. if  id0102==. &  id0202==. &  id0302==. &  id0402==. 
gen transac=transacaux/12

egen ingtotal=rsum(yindep bonos yasalariados extras alimentos yvivienda transport yindepsec ysalsec ysalsec2 extrasec rempais remext bdh ayudaorg alquiler interes jubi becasaux otrosaux transacaux)
replace ingtotal=. if yindep==. & bonos==. &  yasalariados==. &  extras==. &  alimentos==. &  vivienda==. &  transport==. &  yindepsec==. &  ysalsec==. &  ysalsec2==. &  extrasec==. &  rempais==. &  remext==. &  bdh==. &  ayudaorg==. &  alquiler==. &  interes==. &  jubi==. &  becasaux==. &  otrosaux==. &  transacaux==. 



egen ingnolab=rsum(rempais remext bdh ayudaorg alquiler interes jubi becasaux otrosaux transacaux)
replace ingnolab=. if rempais==. &  remext==. &  bdh==. &  ayudaorg==. &  alquiler==. &  interes==. &  jubi==. &  becasaux==. &  otrosaux==. &  transacaux==. 


egen ing_agro=rsum(yindep bonos yasalariados extras alimentos yvivienda transport yindepsec ysalsec ysalsec2 extrasec) if (pa21>=8 & pa21<=13) | (pa55>=8 & pa55<=13)
replace ing_agro=. if yindep==. & bonos==. &  yasalariados==. &  extras==. &  alimentos==. &  vivienda==. &  transport==. &  yindepsec==. &  ysalsec==. &  ysalsec2==. &  extrasec==.

gen relacion_ci=pd04

*gen idh_ch=id
egen idh_ch=group(region ciudad zona sector id)

by idh_ch, sort: egen nmiembros_ch=sum(relacion_ci>=1 & relacion_ci<=10)
label variable nmiembros_ch "Numero de familiares en el hogar"

by idh_ch, sort: egen nmenor6_ch=sum((relacion_ci>=1 & relacion_ci<=9) & edad<6)
label variable nmenor6_ch "Numero de familiares menores a 6 anios"


by idh_ch, sort: egen ing_agro_ch=sum(ing_agro)
by idh_ch, sort: egen ingtotal_ch=sum(ingtotal)



collapse (mean) ingtotal_ch ing_agro_ch nmiembros_ch nmenor6_ch factor_f ciudad zona sector, by(idh_ch)


sort idh_ch

merge  idh_ch using "${surveysFolder}\Data.idb\Victoria\Food\GastosECU.dta"

tab _merge

gen alimentos= gastalim*totper

g consum_n = alimentos >ing_agro_ch
g produc_n = ing_agro_ch > alimentos


g product=0
replace product=1 if ing_agro_ch>0


gen m0_5=0
replace m0_5=1 if  nmenor6_ch>0

gen consumotot= consumo

g exp_pc = consumotot / (totper)


gen urban=area1


xtile deciles    = exp_pc [w=int(fexp)], nq(10) 
xtile quintilu   = exp_pc [w=int(fexp)] if urban==1, nq(5) 
xtile quintilr   = exp_pc [w=int(fexp)] if urban==0, nq(5) 



tabstat  consumotot alimentos consum_n produc_n product  m0_5 [w=fexp], stats(mean) f(%10.2f) col(var)
tabstat  consumotot alimentos consum_n produc_n product  m0_5 [w=fexp], stats(mean) f(%10.2f) col(var) by(deciles)
tabstat  consumotot alimentos consum_n produc_n product  m0_5 [w=fexp], stats(mean) f(%10.2f) col(var) by(pobre)
tabstat  consumotot alimentos consum_n produc_n product  m0_5 [w=fexp], stats(mean) f(%10.2f) col(var) by(urban)
tabstat  consumotot alimentos consum_n produc_n product  m0_5 [w=fexp], stats(mean) f(%10.2f) col(var) by(quintilu)
tabstat  consumotot alimentos consum_n produc_n product  m0_5 [w=fexp], stats(mean) f(%10.2f) col(var) by(quintilr)





























