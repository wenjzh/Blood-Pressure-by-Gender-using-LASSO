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
generate day = 1

// save as nutrient2.dta

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
generate day = 2

append using nutrient2.dta
merge m:1 seqn using demo.dta
keep if _merge == 3
drop _merge
merge m:1 seqn using blood_pressure.dta
keep if _merge == 3
drop _merge

generate missing = 0
replace missing = 1 if systolic == . | diastolic == . | gender == "." | fat == . | sugar == . | protein == . | fiber == . | sodium == . | iron == . | alcohol ==  . | caff == . | water == .
keep if missing == 0

foreach i in systolic diastolic fat sugar protein fiber sodium iron alcohol caff water {
	egen `i'_mean = mean(`i')
	egen `i'_std = sd(`i')
	generate `i'_s = (`i'-`i'_mean)/`i'_std
}

keep seqn day age pir gender systolic_s diastolic_s fat_s sugar_s protein_s fiber_s sodium_s iron_s alcohol_s caff_s water_s

// for males
cvlasso systolic_s fat_s sugar_s protein_s fiber_s sodium_s iron_s alcohol_s caff_s water_s if gender == "Male", lopt
lasso2 systolic_s fat_s sugar_s protein_s fiber_s sodium_s iron_s alcohol_s caff_s water_s if gender == "Male", lambda(0.10)
regress systolic_s fat_s sugar_s protein_s fiber_s sodium_s iron_s alcohol_s caff_s water_s if gender == "Male"

// for females
cvlasso systolic_s fat_s sugar_s protein_s fiber_s sodium_s iron_s alcohol_s caff_s water_s if gender == "Female", lopt
lasso2 systolic_s fat_s sugar_s protein_s fiber_s sodium_s iron_s alcohol_s caff_s water_s if gender == "Female", lic(bic)
regress systolic_s fat_s sugar_s protein_s fiber_s sodium_s iron_s alcohol_s caff_s water_s if gender == "Female"
