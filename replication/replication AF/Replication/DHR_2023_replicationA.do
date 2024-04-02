** set working directory. 
cd.... 

*** START
use "AllDataMerged_15May2023_weighted.dta", clear 


// rename a few variables 
gen idle_index = IDLE_index
gen ym = date_month
lab var idle_index "Idle Index"

// set panel structure
xtset objectid ym

gen SCADantigov = 0
replace SCADantigov = 1 if n_etype8>0 | n_etype9>0 
replace SCADantigov = . if n_etype8==.

btscs SCADantigov year objectid, g(py_SCADantigov)

gen py2  = py_SCADantigov*py_SCADantigov
gen py3  = py_SCADantigov*py_SCADantigov*py_SCADantigov

***************************************************************************************
***************************************************************************************
** Figure 1: Distribution of Idle Index
***************************************************************************************
***************************************************************************************

reghdfe SCADantigov idle_index , absorb(objectid ) vce(r)  
gen sample = 1 if e(sample)==1

hist idle_index if sample==1 , scheme(s1mono)  percent ytitle(% of Observations) color(green%60) name(hist, replace) bin(20)
//graph export "Idlehist.pdf", replace

tabstat idle_index if sample==1 , by(mon)
graph bar (mean) idle_index if sample==1, over(Month, ) bar(1, fcolor(navy%60)) scheme(s1mono) ytitle(Mean Idle Index)  title(Mean by Month, size(medium)) name(meanovermon, replace)
///scatter cultivated idle_index if sample==1 , ytitle("% of cultivated land") scheme(s1mono) name(cult, replace) msymbol(oh) mcolor(red%30)

graph combine hist meanovermon, scheme(s1mono)
graph export "FigTbl/Fig1_Idlediag.pdf", replace


***************************************************************************************
***************************************************************************************
******** Figure 2: see DoFile_CreateMap_11Sep2020.do
***************************************************************************************
***************************************************************************************


***************************************************************************************
***************************************************************************************
******** Table 1: Agricultural idle time and armed conflict 
***************************************************************************************
***************************************************************************************


** SCAD analysis:

egen yearmon = group(year mon) 
egen oyfe = group(objectid year)
// gen object year FE

//gen lnpy_SCAD = ln(py_SCADantigov+.1)


est clear 
sum SCADantigov if sample==1 
local bl = r(mean) 

eststo: reghdfe SCADantigov idle_index , absorb(objectid ) vce(r)  
estadd local FEobj"x"
estadd local perch =  (_b[idle_index]/`bl')*100
di  ((_b[idle_index]*.35)/`bl')*100
//estadd local perch50 =  ((_b[idle_index]*.63)/`bl')*100


*eststo: reghdfe SCADantigov idle_index , absorb(ccode) vce(r)  
*estadd local FEcountry"x"
*estadd local perch =  (_b[idle_index]/`bl')*100

eststo: reghdfe SCADantigov idle_index , absorb(oyfe ) vce(r)  
estadd local FEoy "x"
estadd local perch =  (_b[idle_index]/`bl')*100

eststo: reghdfe SCADantigov idle_index , absorb(objectid oyfe) vce(r )  
estadd local FEobj"x"
estadd local FEoy "x"
estadd local perch =  (_b[idle_index]/`bl')*100

eststo: reghdfe SCADantigov idle_index , absorb(objectid oyfe ym ) vce(r )  
estadd local FEobj"x"
estadd local FEoy "x"
estadd local FEmo "x"
estadd local perch =  (_b[idle_index]/`bl')*100

eststo: reghdfe SCADantigov idle_index temp prec, absorb(objectid oyfe mon ) vce(r)  
estadd local FEobj"x"
estadd local FEoy "x"
estadd local FEmo "x"
estadd local TP "x"
estadd local perch =  (_b[idle_index]/`bl')*100

eststo: reghdfe SCADantigov idle_index py_SCADantigov, absorb(objectid oyfe mon ) vce(r)  
estadd local FEobj"x"
estadd local FEoy "x"
estadd local FEmo "x"
estadd local PY "x"
estadd local perch =  (_b[idle_index]/`bl')*100

#delimit ; 
esttab _all using "FigTbl/Table1_SCAD.csv", label nogaps compress 
keep(idle_index)  se star(* 0.05 ** 0.01 *** 0.001) cells(b(star fmt(%9.4f)) se( fmt(%9.4f)))
stats(perch N r2 FEobj FEoy FEcountry FEmo TP PY, fmt(%2.1f %18.0g %12.2f) labels(`"Per. Change"' `"Observations"' `"R-squared"' `"Location FE"' `"Country-Year FE"' `"Country FE"' `"Calendar Month FE"' `"Temp & Precipitation"' `"Peace Months"') )
replace ; 
#delimit cr 


** ACLED initiator ***

est clear 

*gen acled_bi = (ACLED_initiator_count>0)
*replace acled_bi = . if ACLED_initiator_count==.

btscs acled_bi year objectid, g(py_acled_bi)

gen py2_acled  = py_acled_bi*py_acled_bi
gen py3_acled  = py_acled_bi*py_acled_bi*py_acled_bi



sum acled_bi if sample==1 
local bl = r(mean) 

est clear
eststo: reghdfe acled_bi idle_index , absorb(objectid ) vce(r)  
estadd local FEobj"x"
estadd local perch =  (_b[idle_index]/`bl')*100

eststo: reghdfe acled_bi idle_index , absorb( oyfe) vce(r )  
estadd local FEoy "x"
estadd local perch =  (_b[idle_index]/`bl')*100

eststo: reghdfe acled_bi idle_index , absorb(objectid oyfe) vce(r )  
estadd local FEobj"x"
estadd local FEoy "x"
estadd local perch =  (_b[idle_index]/`bl')*100

eststo: reghdfe acled_bi idle_index , absorb(objectid oyfe mon ) vce(r )  
estadd local FEobj"x"
estadd local FEoy "x"
estadd local FEmo "x"
estadd local perch =  (_b[idle_index]/`bl')*100

eststo: reghdfe acled_bi idle_index temp prec, absorb(objectid oyfe mon ) vce(r)  
estadd local FEobj"x"
estadd local FEoy "x"
estadd local FEmo "x"
estadd local TP "x"
estadd local perch =  (_b[idle_index]/`bl')*100

eststo: reghdfe acled_bi idle_index py_acled_bi, absorb(objectid oyfe mon ) vce(r)  
estadd local FEobj"x"
estadd local FEoy "x"
estadd local FEmo "x"
estadd local PY "x"
estadd local perch =  (_b[idle_index]/`bl')*100

#delimit ; 
esttab _all using "FigTbl/Table1_ACLED.csv", label nogaps compress 
keep(idle_index)  se star(* 0.05 ** 0.01 *** 0.001) 
stats(perch N r2 FEobj FEoy FEmo TP PY, fmt(%3.2f %18.0g %12.2f) labels(`"Per. Change"' `"Observations"' `"R-squared"' `"Location FE"' `"Location-Year FE"' `"Calendar Month FE"' `"Temp & Precipitation"' `"Peace Months"') )
replace ; 
#delimit cr 


** UCDP ***


*gen UCDP_bi = (UCDP_Violent_init_count>0)
*replace UCDP_bi = . if UCDP_Violent_init_count==.

reghdfe UCDP_bi idle_index , absorb(objectid ) vce(r)  
gen sample_ucdp =1 if e(sample)==1

btscs UCDP_bi year objectid, g(py_UCDP_bi)

gen py2_ucdp  = py_UCDP_bi*py_UCDP_bi
gen py3_ucdp  = py_UCDP_bi*py_UCDP_bi*py_UCDP_bi





est clear 
sum UCDP_bi if sample_ucdp==1 
local bl = r(mean) 


est clear
eststo: reghdfe UCDP_bi idle_index , absorb(objectid ) vce(r)  
estadd local FEobj"x"
estadd local perch =  (_b[idle_index]/`bl')*100

eststo: reghdfe UCDP_bi idle_index , absorb( oyfe) vce(r )  
estadd local FEoy "x"
estadd local perch =  (_b[idle_index]/`bl')*100

eststo: reghdfe UCDP_bi idle_index , absorb(objectid oyfe) vce(r )  
estadd local FEobj"x"
estadd local FEoy "x"
estadd local perch =  (_b[idle_index]/`bl')*100

eststo: reghdfe UCDP_bi idle_index , absorb(objectid oyfe mon ) vce(r )  
estadd local FEobj"x"
estadd local FEoy "x"
estadd local FEmo "x"
estadd local perch =  (_b[idle_index]/`bl')*100

eststo: reghdfe UCDP_bi idle_index temp prec, absorb(objectid oyfe mon ) vce(r)  
estadd local FEobj"x"
estadd local FEoy "x"
estadd local FEmo "x"
estadd local TP "x"
estadd local perch =  (_b[idle_index]/`bl')*100

eststo: reghdfe UCDP_bi idle_index py_UCDP_bi, absorb(objectid oyfe mon ) vce(r)  
estadd local FEobj"x"
estadd local FEoy "x"
estadd local FEmo "x"
estadd local PY "x"
estadd local perch =  (_b[idle_index]/`bl')*100

#delimit ; 
esttab _all using "FigTbl/Table1_UCDP.csv", label nogaps compress 
keep(idle_index)  se star(* 0.05 ** 0.01 *** 0.001) 
stats(perch N r2 FEobj FEoy FEmo TP PY, fmt(%3.2f %18.0g %12.2f) labels(`"Per. Change"' `"Observations"' `"R-squared"' `"Location FE"' `"Location-Year FE"' `"Calendar Month FE"' `"Temp & Precipitation"' `"Peace Months"') )
replace ; 
#delimit cr 

***************************************************************************************
***************************************************************************************
******* Table 2: Idle index post-2000 - SCAD
***************************************************************************************
***************************************************************************************
est clear 
sum SCADantigov  if sample==1 &  year>2000
local bl = r(mean) 

est clear
eststo: reghdfe SCADantigov idle_index if year>2000, absorb(objectid ) vce(r)  
estadd local FEobj"x"
estadd local perch =  (_b[idle_index]/`bl')*100

eststo: reghdfe SCADantigov idle_index  if year>2000, absorb( oyfe) vce(r )  
estadd local FEoy "x"
estadd local perch =  (_b[idle_index]/`bl')*100

eststo: reghdfe SCADantigov idle_index  if year>2000, absorb(objectid oyfe) vce(r )  
estadd local FEobj"x"
estadd local FEoy "x"
estadd local perch =  (_b[idle_index]/`bl')*100

eststo: reghdfe SCADantigov idle_index  if year>2000, absorb(objectid oyfe mon ) vce(r )  
estadd local FEobj"x"
estadd local FEoy "x"
estadd local FEmo "x"
estadd local perch =  (_b[idle_index]/`bl')*100

eststo: reghdfe SCADantigov idle_index temp prec  if year>2000, absorb(objectid oyfe mon ) vce(r)  
estadd local FEobj"x"
estadd local FEoy "x"
estadd local FEmo "x"
estadd local TP "x"
estadd local perch =  (_b[idle_index]/`bl')*100

eststo: reghdfe SCADantigov idle_index py_SCADantigov  if year>2000, absorb(objectid oyfe mon ) vce(r)  
estadd local FEobj"x"
estadd local FEoy "x"
estadd local FEmo "x"
estadd local PY "x"
estadd local perch =  (_b[idle_index]/`bl')*100

#delimit ; 
esttab _all using "FigTbl/Table2_POST2000.csv", label nogaps compress 
keep(idle_index)  se star(* 0.05 ** 0.01 *** 0.001)  cells(b(star fmt(%9.4f)) se( fmt(%9.4f)))
stats(perch N r2 FEobj FEoy FEmo TP PY, fmt(%3.2f %18.0g %12.2f) labels(`"Per. Change"' `"Observations"' `"R-squared"' `"Location FE"' `"Location-Year FE"' `"Calendar Month FE"' `"Temp & Precipitation"' `"Peace Months"') )
replace ; 
#delimit cr 


***************************************************************************************
***************************************************************************************
****** Figure 3: Constrained sample by year
***************************************************************************************
***************************************************************************************

est clear 
sum SCADantigov  if sample==1 &  year>2000
local bl = r(mean) 

cap drop y_* sim_year
gen y_coef =.
gen y_ub=.
gen y_lb =. 
gen sim_year = .
forval i = 1990/2010{
qui reghdfe SCADantigov idle_index if year>`i', absorb(objectid oyfe mon) vce(r)  
replace y_coef = _b[idle_index] if _n==`i'
replace y_ub = _b[idle_index] + (1.96 * _se[idle_index]) if _n==`i'
replace y_lb = _b[idle_index] - (1.96 * _se[idle_index]) if _n==`i'
replace sim_year=`i' if _n==`i'
}

#delimit ; 
twoway  (rcap y_ub y_lb sim_year, sort msize(medium) lcolor(blue)) 
		(scatter y_coef sim_year, mcolor(red)), 
		scheme(s1mono) yline(0) legend(off) 
		xtitle("Year Constraint") ytitle("Coef. Idle Index");
#delimit cr
graph export "FigTbl/Fig3_yearconst.png", replace

***************************************************************************************
***************************************************************************************
***************************************************************************************
***************************************************************************************
**** Figure 4: The marginal effect of idleness across the log of the percentage of cultivated land.
**** Table A4: Conditional Relationship across the log of % of cultivated land 
***************************************************************************************
***************************************************************************************
***************************************************************************************
***************************************************************************************

est clear
cap drop X
cap gen lncult = ln(cultivated+1)
lab var lncult "log of % Cultivated Land"
gen X = lncult*idle_index
lab var X "idle X log of % Cultivated Land"

eststo: reghdfe SCADantigov idle_index lncult X, absorb( objectid oyfe mon ) vce(r)  
estadd local FEobj"x"
estadd local FEoy "x"
estadd local FEmo "x"

grinter idle_index, inter(X) const02(lncult) scheme(s1mono) kdensity yline(0) 
graph export "FigTbl/Fig4_mfx_cultivated.pdf", replace


eststo: reghdfe SCADantigov idle_index lncult X temp prec, absorb( objectid oyfe mon ) vce(r)  
estadd local FEobj"x"
estadd local FEoy "x"
estadd local FEmo "x"
estadd local TP "x"


eststo: reghdfe SCADantigov idle_index lncult X py_SCADantigov, absorb( objectid oyfe mon ) vce(r)  
estadd local FEobj"x"
estadd local FEoy "x"
estadd local FEmo "x"
estadd local PY "x"


#delimit ; 
esttab _all using "FigTbl/Appendix_TblA4_cond.csv", label nogaps compress 
keep(idle_index X)  se star(* 0.05 ** 0.01 *** 0.001) 
stats(perch N r2 FEobj FEoy FEcountry FEmo TP PY, fmt(%3.2f %18.0g %12.2f) labels(`"Per. Change"' `"Observations"' `"R-squared"' `"Location FE"' `"Location-Year FE"' `"Country FE"' `"Calendar Month FE"' `"Temp & Precipitation"' `"Peace Months"') )
replace ; 
#delimit cr 


******************************************************************************************************************************************************
******************************************************************************************************************************************************
******************************************************************************************************************************************************
******************************************************************************************************************************************************
** APPENDIX TABLES AND FIGURES
******************************************************************************************************************************************************
******************************************************************************************************************************************************
******************************************************************************************************************************************************
******************************************************************************************************************************************************



***************************************************************************************
***************************************************************************************
****** Table A1: Logit Models
***************************************************************************************
***************************************************************************************

set matsize 10000

xtset objectid ym


est clear
eststo:  xtlogit SCADantigov idle_index, fe 
estadd local FEobj"x"

eststo:  xtlogit SCADantigov i.year idle_index temp prec py_SCADantigov, fe
estadd local FEobj"x"
estadd local FEoy "x"
estadd local TP "x"
estadd local PY "x"

 
eststo:  xtlogit acled_bi idle_index, fe 
estadd local FEobj"x"

eststo:  xtlogit acled_bi i.year idle_index temp prec py_acled_bi, fe
estadd local FEobj"x"
estadd local FEoy "x"
estadd local TP "x"
estadd local PY "x"


eststo:  xtlogit UCDP_bi idle_index, fe 
estadd local FEobj"x"

eststo:  xtlogit UCDP_bi i.year idle_index temp prec py_UCDP_bi, fe
estadd local FEobj"x"
estadd local FEoy "x"
estadd local TP "x"
estadd local PY "x"


#delimit ; 
esttab _all using "FigTbl/Appendix_TblA1_Logit.csv", label nogaps compress 
keep(idle_index)  se star(* 0.05 ** 0.01 *** 0.001) 
stats(perch N r2 FEobj FEoy FEcountry FEmo TP PY, fmt(%3.2f %18.0g %12.2f) labels(`"Per. Change"' `"Observations"' `"R-squared"' `"Location FE"' `"Country-Year FE"' `"Country FE"' `"Calendar Month FE"' `"Temp & Precipitation"' `"Peace Months"') )
replace ; 
#delimit cr 


***************************************************************************************
***************************************************************************************
****** Table A2: . Estimations results for count dependent variables
***************************************************************************************
***************************************************************************************

gen SCAD_count=0
replace SCAD_count = n_etype8+n_etype9 if n_etype8>0 & n_etype9>0
replace SCAD_count = n_etype8 if n_etype8>0 & n_etype9==.
replace SCAD_count = n_etype9 if n_etype8==. & n_etype9>0
replace SCAD_count = . if n_etype8==. & n_etype9==.


sum SCAD_count UCDP_Violent_init_count ACLED_initiator_count


kdensity SCAD_count

kdensity UCDP_Violent_init_count 

kdensity ACLED_initiator_count


** large difference between s.d. and mean means, the data does not follow poisson distribution, so we use Negative Binomial 

set matsize 10000

xtset objectid ym

est clear

eststo: reghdfe SCAD_count idle_index temp prec py_SCADantigov, absorb(oyfe) vce(r) 
estadd local FEobj"x"
estadd local FEoy "x"
estadd local TP "x"
estadd local PY "x"
 
eststo: reghdfe ACLED_initiator_count idle_index temp prec py_acled_bi , absorb(oyfe) vce(r)
estadd local FEobj"x"
estadd local FEoy "x"
estadd local TP "x"
estadd local PY "x"
  
eststo: reghdfe UCDP_Violent_init_count idle_index temp prec py_UCDP_bi , absorb(oyfe) vce(r)  
estadd local FEobj"x"
estadd local FEoy "x"
estadd local TP "x"
estadd local PY "x"


#delimit ; 
esttab _all using "FigTbl/Appendix_TblA2_Count.csv", label nogaps compress 
keep(idle_index)  se star(* 0.05 ** 0.01 *** 0.001) 
stats(perch N r2 FEobj FEoy FEcountry FEmo TP PY, fmt(%3.2f %18.0g %12.2f) labels(`"Per. Change"' `"Observations"' `"R-squared"' `"Location FE"' `"Country-Year FE"' `"Country FE"' `"Calendar Month FE"' `"Temp & Precipitation"' `"Peace Months"') )
replace ; 
#delimit cr 






***************************************************************************************
***************************************************************************************
****** Table A3: SCAD outcomes excluding desserts (areas without crops).
***************************************************************************************
***************************************************************************************


sort oyfe 
by oyfe: egen idleyear = min(idle_index)

est clear 
sum SCADantigov if sample==1 
local bl = r(mean) 

eststo: reghdfe SCADantigov idle_index if idleyear!=1, absorb(objectid ) vce(r)  
estadd local FEobj"x"
estadd local perch =  (_b[idle_index]/`bl')*100
di  ((_b[idle_index]*.35)/`bl')*100
//estadd local perch50 =  ((_b[idle_index]*.63)/`bl')*100

eststo: reghdfe SCADantigov idle_index if idleyear!=1, absorb(oyfe ) vce(r)  
estadd local FEoy "x"
estadd local perch =  (_b[idle_index]/`bl')*100

eststo: reghdfe SCADantigov idle_index if idleyear!=1, absorb(objectid oyfe) vce(r )  
estadd local FEobj"x"
estadd local FEoy "x"
estadd local perch =  (_b[idle_index]/`bl')*100

eststo: reghdfe SCADantigov idle_index if idleyear!=1, absorb(objectid oyfe ym ) vce(r )  
estadd local FEobj"x"
estadd local FEoy "x"
estadd local FEmo "x"
estadd local perch =  (_b[idle_index]/`bl')*100

eststo: reghdfe SCADantigov idle_index temp prec if idleyear!=1, absorb(objectid oyfe mon ) vce(r)  
estadd local FEobj"x"
estadd local FEoy "x"
estadd local FEmo "x"
estadd local TP "x"
estadd local perch =  (_b[idle_index]/`bl')*100

eststo: reghdfe SCADantigov idle_index py_SCADantigov if idleyear!=1, absorb(objectid oyfe mon ) vce(r)  
estadd local FEobj"x"
estadd local FEoy "x"
estadd local FEmo "x"
estadd local PY "x"
estadd local perch =  (_b[idle_index]/`bl')*100

#delimit ; 
esttab _all using "FigTbl/Appendix_TblA3_desert.csv", label nogaps compress 
keep(idle_index)  se star(* 0.05 ** 0.01 *** 0.001) cells(b(star fmt(%9.4f)) se( fmt(%9.4f)))
stats(perch N r2 FEobj FEoy FEcountry FEmo TP PY, fmt(%2.1f %18.0g %12.2f) labels(`"Per. Change"' `"Observations"' `"R-squared"' `"Location FE"' `"Country-Year FE"' `"Country FE"' `"Calendar Month FE"' `"Temp & Precipitation"' `"Peace Months"') )
replace ; 
#delimit cr 


***************************************************************************************
***************************************************************************************
****** Table A4: See Above (Figure 3)
***************************************************************************************
***************************************************************************************



***************************************************************************************
***************************************************************************************
****** Table A5: Models with spatially weighted lagged dependent variables
***************************************************************************************
***************************************************************************************

**SCAD

est clear 
sum SCADantigov if sample==1 
local bl = r(mean) 

eststo: reghdfe SCADantigov wy_SCADantigov_1 idle_index , absorb(objectid ) vce(r)  
estadd local FEobj"x"
estadd local perch =  (_b[idle_index]/`bl')*100
di  ((_b[idle_index]*.35)/`bl')*100
//estadd local perch50 =  ((_b[idle_index]*.63)/`bl')*100

eststo: reghdfe SCADantigov wy_SCADantigov_1 idle_index , absorb(oyfe ) vce(r)  
estadd local FEoy "x"
estadd local perch =  (_b[idle_index]/`bl')*100

eststo: reghdfe SCADantigov wy_SCADantigov_1 idle_index , absorb(objectid oyfe) vce(r )  
estadd local FEobj"x"
estadd local FEoy "x"
estadd local perch =  (_b[idle_index]/`bl')*100

eststo: reghdfe SCADantigov wy_SCADantigov_1 idle_index , absorb(objectid oyfe ym ) vce(r )  
estadd local FEobj"x"
estadd local FEoy "x"
estadd local FEmo "x"
estadd local perch =  (_b[idle_index]/`bl')*100

eststo: reghdfe SCADantigov wy_SCADantigov_1 idle_index temp prec, absorb(objectid oyfe mon ) vce(r)  
estadd local FEobj"x"
estadd local FEoy "x"
estadd local FEmo "x"
estadd local TP "x"
estadd local perch =  (_b[idle_index]/`bl')*100

eststo: reghdfe SCADantigov wy_SCADantigov_1 idle_index py_SCADantigov, absorb(objectid oyfe mon ) vce(r)  
estadd local FEobj"x"
estadd local FEoy "x"
estadd local FEmo "x"
estadd local PY "x"
estadd local perch =  (_b[idle_index]/`bl')*100

#delimit ; 
esttab _all using "FigTbl/Appendix_TblA5_WYDV_SCAD.csv", label nogaps compress 
keep(idle_index wy_SCADantigov_1)  se star(* 0.05 ** 0.01 *** 0.001) cells(b(star fmt(%9.4f)) se( fmt(%9.4f)))
stats(perch N r2 FEobj FEoy FEcountry FEmo TP PY, fmt(%2.1f %18.0g %12.2f) labels(`"Per. Change"' `"Observations"' `"R-squared"' `"Location FE"' `"Country-Year FE"' `"Country FE"' `"Calendar Month FE"' `"Temp & Precipitation"' `"Peace Months"') )
replace ; 
#delimit cr 

*********************************************************

*ACLED

sum acled_bi if sample==1 
local bl = r(mean) 

est clear
eststo: reghdfe acled_bi wy_acled_bi_1 idle_index , absorb(objectid ) vce(r)  
estadd local FEobj"x"
estadd local perch =  (_b[idle_index]/`bl')*100

eststo: reghdfe acled_bi wy_acled_bi_1 idle_index , absorb( oyfe) vce(r )  
estadd local FEoy "x"
estadd local perch =  (_b[idle_index]/`bl')*100

eststo: reghdfe acled_bi wy_acled_bi_1 idle_index , absorb(objectid oyfe) vce(r )  
estadd local FEobj"x"
estadd local FEoy "x"
estadd local perch =  (_b[idle_index]/`bl')*100

eststo: reghdfe acled_bi wy_acled_bi_1 idle_index , absorb(objectid oyfe mon ) vce(r )  
estadd local FEobj"x"
estadd local FEoy "x"
estadd local FEmo "x"
estadd local perch =  (_b[idle_index]/`bl')*100

eststo: reghdfe acled_bi wy_acled_bi_1 idle_index temp prec, absorb(objectid oyfe mon ) vce(r)  
estadd local FEobj"x"
estadd local FEoy "x"
estadd local FEmo "x"
estadd local TP "x"
estadd local perch =  (_b[idle_index]/`bl')*100

eststo: reghdfe acled_bi wy_acled_bi_1 idle_index py_acled_bi, absorb(objectid oyfe mon ) vce(r)  
estadd local FEobj"x"
estadd local FEoy "x"
estadd local FEmo "x"
estadd local PY "x"
estadd local perch =  (_b[idle_index]/`bl')*100

#delimit ; 
esttab _all using "FigTbl/Appendix_TblA5_WYDV_ACLED.csv", label nogaps compress 
keep(idle_index wy_acled_bi_1)  se star(* 0.05 ** 0.01 *** 0.001) 
stats(perch N r2 FEobj FEoy FEmo TP PY, fmt(%3.2f %18.0g %12.2f) labels(`"Per. Change"' `"Observations"' `"R-squared"' `"Location FE"' `"Location-Year FE"' `"Calendar Month FE"' `"Temp & Precipitation"' `"Peace Months"') )
replace ; 
#delimit cr 

***************************************

*UCDP

est clear 
sum UCDP_bi if sample_ucdp==1 
local bl = r(mean) 


est clear
eststo: reghdfe UCDP_bi wy_UCDP_bi_1 idle_index , absorb(objectid ) vce(r)  
estadd local FEobj"x"
estadd local perch =  (_b[idle_index]/`bl')*100

eststo: reghdfe UCDP_bi wy_UCDP_bi_1 idle_index , absorb( oyfe) vce(r )  
estadd local FEoy "x"
estadd local perch =  (_b[idle_index]/`bl')*100

eststo: reghdfe UCDP_bi wy_UCDP_bi_1 idle_index , absorb(objectid oyfe) vce(r )  
estadd local FEobj"x"
estadd local FEoy "x"
estadd local perch =  (_b[idle_index]/`bl')*100

eststo: reghdfe UCDP_bi wy_UCDP_bi_1 idle_index , absorb(objectid oyfe mon ) vce(r )  
estadd local FEobj"x"
estadd local FEoy "x"
estadd local FEmo "x"
estadd local perch =  (_b[idle_index]/`bl')*100

eststo: reghdfe UCDP_bi wy_UCDP_bi_1 idle_index temp prec, absorb(objectid oyfe mon ) vce(r)  
estadd local FEobj"x"
estadd local FEoy "x"
estadd local FEmo "x"
estadd local TP "x"
estadd local perch =  (_b[idle_index]/`bl')*100

eststo: reghdfe UCDP_bi wy_UCDP_bi_1 idle_index py_UCDP_bi, absorb(objectid oyfe mon ) vce(r)  
estadd local FEobj"x"
estadd local FEoy "x"
estadd local FEmo "x"
estadd local PY "x"
estadd local perch =  (_b[idle_index]/`bl')*100

#delimit ; 
esttab _all using "FigTbl/Appendix_TblA5_WYDV_UCDP.csv", label nogaps compress 
keep(idle_index wy_UCDP_bi_1)  se star(* 0.05 ** 0.01 *** 0.001) 
stats(perch N r2 FEobj FEoy FEmo TP PY, fmt(%3.2f %18.0g %12.2f) labels(`"Per. Change"' `"Observations"' `"R-squared"' `"Location FE"' `"Location-Year FE"' `"Calendar Month FE"' `"Temp & Precipitation"' `"Peace Months"') )
replace ; 
#delimit cr 








