import sasxport5 DR1TOT_I.XPT, clear
keep seqn dr1ttfat dr1tsugr dr1tprot dr1tfibe dr1tsodi dr1tiron dr1talco dr1tcaff dr1_320z
// rename
rename dr1ttfat fat
rename dr1tsugr sugar
rename dr1tprot protein
rename dr1tfibe fiber
rename dr1tsodi sodium
rename dr1tiron iron
rename dr1talco alcohol
rename dr1tcaff caff
rename dr1_320z water

// get rid of missing values
generate missing = 0
replace missing = 1 if fat == . | sugar == . | protein == . | fiber == . | sodium == . | iron == . | alcohol ==  . | caff == . | water == .
keep if missing == 0
// get standardized variables
// calculate mean and std
foreach i in fat sugar protein fiber sodium iron alcohol caff water {
	egen `i'_mean = mean(`i')
	egen `i'_std = sd(`i')
	generate `i'_s = (`i'-`i'_mean)/`i'_std
}

// keep only standardized values
keep seqn fat_s sugar_s protein_s fiber_s sodium_s iron_s alcohol_s caff_s water_s
generate day = 1

//saved

import sasxport5 DR2TOT_I.XPT, clear
keep seqn dr2ttfat dr2tsugr dr2tprot dr2tfibe dr2tsodi dr2tiron dr2talco dr2tcaff dr2_320z
// rename
rename dr2ttfat fat
rename dr2tsugr sugar
rename dr2tprot protein
rename dr2tfibe fiber
rename dr2tsodi sodium
rename dr2tiron iron
rename dr2talco alcohol
rename dr2tcaff caff
rename dr2_320z water

// get rid of missing values
generate missing = 0
replace missing = 1 if fat == . | sugar == . | protein == . | fiber == . | sodium == . | iron == . | alcohol ==  . | caff == . | water == .
keep if missing == 0
// get standardized variables
// calculate mean and std
foreach i in fat sugar protein fiber sodium iron alcohol caff water {
	egen `i'_mean = mean(`i')
	egen `i'_std = sd(`i')
	generate `i'_s = (`i'-`i'_mean)/`i'_std
}

// keep only standardized values
keep seqn fat_s sugar_s protein_s fiber_s sodium_s iron_s alcohol_s caff_s water_s
generate day = 2

append using nutrient1.dta
merge m:1 seqn using demo.dta
keep if _merge == 3
drop _merge
merge m:1 seqn using blood_pressure.dta
keep if _merge == 3
drop _merge

//drop missing values
generate missing = 0
replace missing = 1 if systolic == . | diastolic == .
keep if missing == 0
drop missing

// save data for analysis

// for males
cvlasso systolic fat_s sugar_s protein_s fiber_s sodium_s iron_s alcohol_s caff_s water_s if gender == "Male", lopt
lasso2 systolic fat_s sugar_s protein_s fiber_s sodium_s iron_s alcohol_s caff_s water_s if gender == "Male", lic(bic)
regress systolic fat_s sugar_s protein_s fiber_s sodium_s iron_s alcohol_s caff_s water_s if gender == "Male"

// for females
cvlasso systolic fat_s sugar_s protein_s fiber_s sodium_s iron_s alcohol_s caff_s water_s if gender == "Female", lopt
lasso2 systolic fat_s sugar_s protein_s fiber_s sodium_s iron_s alcohol_s caff_s water_s if gender == "Female", lic(bic)
regress systolic fat_s sugar_s protein_s fiber_s sodium_s iron_s alcohol_s caff_s water_s if gender == "Female"
