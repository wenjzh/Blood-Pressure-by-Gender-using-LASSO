// Work with Demo dataset
/// set up data
import sasxport5 DEMO_I.XPT, clear
keep seqn riagendr ridageyr indfmpir
/// rename for ease
rename riagendr gender2
rename ridageyr age
rename indfmpir pir
/// label gender
label define gender_name 1 "Male" 2 "Female"
label values gender2 gender_name
decode gender2, generate(gender)
drop gender2
//
// *save as demo.dta*

// Work with Blood Pressure dataset
/// set up data
import sasxport5 BPX_I.XPT, clear
keep seqn bpxsy1 bpxdi1 bpxsy2 bpxdi2 bpxsy3 bpxdi3
/// find average blood pressures for each respondent
egen systolic = rowmean(bpxsy1 bpxsy2 bpxsy3)
egen diastolic = rowmean(bpxdi1 bpxdi2 bpxdi3)
keep seqn systolic diastolic
/// get rid of missing values
generate missing = 0
replace missing = 1 if systolic == . | diastolic == .
drop missing
//
// *save as blood_pressure.dta*

// Work with Day 1 Data
/// set up day 1 data
import sasxport5 DR1TOT_I.XPT, clear
keep seqn dr1ttfat dr1tsugr dr1tprot dr1tfibe dr1tsodi dr1tiron dr1talco dr1tcaff dr1_320z
/// rename for ease
rename dr1ttfat fat
rename dr1tsugr sugar
rename dr1tprot protein
rename dr1tfibe fiber
rename dr1tsodi sodium
rename dr1tiron iron
rename dr1talco alcohol
rename dr1tcaff caff
rename dr1_320z water
/// create day 1 label
generate day = 1
//
// *save as nutrient2.dta*


// Work with Day 2 Data
/// set up day 2 data
import sasxport5 DR2TOT_I.XPT, clear
keep seqn dr2ttfat dr2tsugr dr2tprot dr2tfibe dr2tsodi dr2tiron dr2talco dr2tcaff dr2_320z
// rename for ease
rename dr2ttfat fat
rename dr2tsugr sugar
rename dr2tprot protein
rename dr2tfibe fiber
rename dr2tsodi sodium
rename dr2tiron iron
rename dr2talco alcohol
rename dr2tcaff caff
rename dr2_320z water
/// create day 2 label
generate day = 2

// Combine all datasets
/// bind day 1 data
append using nutrient2.dta
/// merge demo data
merge m:1 seqn using demo.dta
keep if _merge == 3
drop _merge
/// merge blood pressure data
merge m:1 seqn using blood_pressure.dta
keep if _merge == 3
drop _merge

// Standardize for LASSO
//
/// get rid of missing values
generate missing = 0
replace missing = 1 if age == . | pir == .| systolic == . | diastolic == . | gender == "." | fat == . | sugar == . | protein == . | fiber == . | sodium == . | iron == . | alcohol ==  . | caff == . | water == .
keep if missing == 0
//
/// have only one row of values for each respondent
collapse (mean) age pir systolic diastolic fat sugar protein fiber sodium iron alcohol caff water, by(gender seqn)
//
/// standardize
foreach i in age pir systolic diastolic fat sugar protein fiber sodium iron alcohol caff water {
	egen `i'_mean = mean(`i')
	egen `i'_std = sd(`i')
	generate `i'_s = (`i'-`i'_mean)/`i'_std
}
//
/// keep only important variables
keep seqn age pir gender systolic_s diastolic_s fat_s sugar_s protein_s fiber_s sodium_s iron_s alcohol_s caff_s water_s
generate diff = systolic_s - diastolic_s

// *save as proj_data2.dta*


// NOT USING THIS METHOD
// lasso and then ols with interaction (use r(coef) and then r(table))
/// set up gend as binary
//encode gender, generate(gend)
//
/// SYSTOLIC: use lasso to find most important coeff and regress interaction
//lasso linear systolic_s fat_s sugar_s protein_s fiber_s sodium_s iron_s alcohol_s caff_s water_s 
//lassocoef, display(coef, standardized)
//regress systolic_s c.fat_s#gend c.sugar_s#gend c.fiber_s#gend c.iron_s#gend c.alcohol_s#gend c.caff_s#gend c.water_s#gend
//
/// DIFF: use lasso to find most important coeff and regress interaction
//lasso linear diff fat_s sugar_s protein_s fiber_s sodium_s iron_s alcohol_s caff_s water_s 
//lassocoef, display(coef, standardized)
//regress diff c.sugar_s#gend c.protein_s#gend c.fiber_s#gend c.sodium_s#gend c.alcohol_s#gend c.caff_s#gend c.water_s#gend


// Penalty Weighted LASSO (use r(coef))
/// set up gend as continuous
generate gend = 0
replace gend = 0.5 if gender == "Male"
replace gend = -0.5 if gender == "Female"
//
/// set up penalty weight matrix with gend weighted at 0
mata: wt = (0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
mata: st_matrix("mweight", wt)
//
/// SYSTOLIC: lasso
lasso linear systolic_s gend fat_s c.fat_s#c.gend sugar_s c.sugar_s#c.gend protein_s c.protein_s#c.gend fiber_s c.fiber_s#c.gend sodium_s c.sodium_s#c.gend iron_s c.iron_s#c.gend alcohol_s c.alcohol_s#c.gend caff_s c.caff_s#c.gend water_s c.water_s#c.gend, penaltywt(mweight)
lassocoef, display(coef, standardized)
//
/// DIFF: lasso
lasso linear diff gend fat_s c.fat_s#c.gend sugar_s c.sugar_s#c.gend protein_s c.protein_s#c.gend fiber_s c.fiber_s#c.gend sodium_s c.sodium_s#c.gend iron_s c.iron_s#c.gend alcohol_s c.alcohol_s#c.gend caff_s c.caff_s#c.gend water_s c.water_s#c.gend, penaltywt(mweight)
lassocoef, display(coef, standardized)

