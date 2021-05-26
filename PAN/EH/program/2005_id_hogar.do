destring nper, replace

gen hogarnumber=1

local i=2
local i_1=`i'-1
display "`i'"
display "`i_1'"
	local persona=nper in `i'
	local persona_1=nper in `i_1'
	
display "`persona' `persona_1'"


set more off
forvalues i=2(1)48596{
	local persona=nper in `i'
	local i_1=`i'-1
	local persona_1=nper in `i_1'
	
	if `persona'<=`persona_1' {
		quietly replace hogarnumber=`i' in `i'/48596
		}
		
	}
	
