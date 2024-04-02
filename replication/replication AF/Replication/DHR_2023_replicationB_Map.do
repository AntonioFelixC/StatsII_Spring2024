** set working directory
cd...

** limit to year 2000 (as an example)

*qui  import delimited "FinalMergedAllData_06092020.csv", clear 
use "AllDataMerged_15May2023_weighted.dta", clear
qui keep if time_month=="Jan 2000"
qui save  "FinalMergedAllData_Jan2000.dta", replace

*qui  import delimited "FinalMergedAllData_06092020.csv", clear 
use "AllDataMerged_15May2023_weighted.dta", clear
qui keep if time_month=="Apr 2000"
qui save  "FinalMergedAllData_Apr2000.dta", replace

*qui  import delimited "FinalMergedAllData_06092020.csv", clear 
use "AllDataMerged_15May2023_weighted.dta", clear
qui keep if time_month=="Jul 2000"
qui save  "FinalMergedAllData_Jul2000.dta", replace

*qui  import delimited "FinalMergedAllData_06092020.csv", clear 
use "AllDataMerged_15May2023_weighted.dta", clear
qui keep if time_month=="Oct 2000"
qui save  "FinalMergedAllData_Oct2000.dta", replace

shp2dta using "Africa_admin1", ///
 database(AfricaAdmin) coordinates(coor) genid(id) gencentroids(centroid) replace
 
use AfricaAdmin, clear

rename OBJECTID objectid

joinby objectid objectid using "FinalMergedAllData_Jan2000.dta", unmatched(master)
drop _merge

rename IDLE_index idle_index_Jan2000
format idle_index_Jan2000 %5.1f

joinby objectid objectid using "FinalMergedAllData_Apr2000.dta", unmatched(master)
drop _merge

rename IDLE_index idle_index_Apr2000
format idle_index_Apr2000 %5.1f

joinby objectid objectid using "FinalMergedAllData_Jul2000.dta", unmatched(master)
drop _merge

rename IDLE_index idle_index_Jul2000
format idle_index_Jul2000 %5.1f

joinby objectid objectid using "FinalMergedAllData_Oct2000.dta", unmatched(master)
drop _merge

rename IDLE_index idle_index_Oct2000
format idle_index_Oct2000 %5.1f


spmap  idle_index_Jan2000 using coor, id(id)  fcolor(Greys) ///
 ocolor(black ..) osize(thin ..) legend(position(2)) ///
 legtitle("IDLE index")  clmethod(quantile) title("Jan 2000")
graph save "FigTbl/Map_idle_Jan2000.gph", replace
graph export "FigTbl/Map_idle_Jan2000.tif", replace width(1800) height(1200)

spmap  idle_index_Apr2000 using coor, id(id)  fcolor(Greys) ///
 ocolor(black ..) osize(thin ..) legend(position(2)) ///
 legtitle("IDLE index")  clmethod(quantile) title("Apr 2000")
graph save "FigTbl/Map_idle_Apr2000.gph", replace
 graph export "FigTbl/Map_idle_Apr2000.tif", replace width(1800) height(1200)

spmap  idle_index_Jul2000 using coor, id(id)  fcolor(Greys) ///
 ocolor(black ..) osize(thin ..) legend(position(2)) ///
 legtitle("IDLE index")  clmethod(quantile) title("Jul 2000")
graph save "FigTbl/Map_idle_Jul2000.gph", replace 
 graph export "FigTbl/Map_idle_Jul2000.tif", replace width(1800) height(1200)

spmap  idle_index_Oct2000 using coor, id(id)  fcolor(Greys) ///
 ocolor(black ..) osize(thin ..) legend(position(2)) ///
 legtitle("IDLE index")  clmethod(quantile) title("Oct 2000")
graph save "FigTbl/Map_idle_Oct2000.gph", replace
graph export "FigTbl/Map_idle_Oct2000.tif", replace width(1800) height(1200)

gr combine "FigTbl/Map_idle_Jan2000.gph" "FigTbl/Map_idle_Apr2000.gph" ///
		   "FigTbl/Map_idle_Jul2000.gph" "FigTbl/Map_idle_Oct2000.gph", scheme(plotting) graphregion(fcolor(white))
		   
		   
