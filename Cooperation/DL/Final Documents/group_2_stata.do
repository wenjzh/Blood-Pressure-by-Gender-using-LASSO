/* 506 Group Project: Group 2 Stata Analysis*/
/* 
/* Author: Diana Liang
/* Last Update: 12/10/2019
/* --------------------------------------------------
/* Which predictor variables of blood pressure differ the greatest between males and females?
/*
/* The code below will answer this question by first preparing the data and then using LASSO to penalize the interaction terms between the predictor variables and gender.
/*---------------------------------------------------


// Work with Demo dataset
/// set up data
import sasxport5 DEMO_I.XPT, clear
keep seqn riagendr //ridageyr indfmpir
/// rename for ease
rename riagendr gender
/// get rid of missing values
drop if gender == .
//
// *save as demo.dta*

// Work with Blood Pressure dataset
/// set up data
import sasxport5 BPX_I.XPT, clear
keep seqn bpxsy1 bpxdi1 bpxsy2 bpxdi2 bpxsy3 bpxdi3
/// get rid of missing values
generate missing = 0
replace missing = 1 if bpxsy1 == . | bpxsy2 == . | bpxsy3 == . | bpxdi1 == . | bpxdi2 == . | bpxdi3 == .
keep if missing == 0
drop missing
/// find average blood pressures for each respondent
egen systolic = rowmean(bpxsy1 bpxsy2 bpxsy3)
egen diastolic = rowmean(bpxdi1 bpxdi2 bpxdi3)
keep seqn systolic diastolic

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
replace missing = 1 if systolic == . | diastolic == . | gender == . | fat == . | sugar == . | protein == . | fiber == . | sodium == . | iron == . | alcohol ==  . | caff == . | water == .
keep if missing == 0
/// only keep if there are 2 days of data
by seqn, sort: generate n = _N
keep if n == 2
/// have only one row of values for each respondent
collapse (mean) gender systolic diastolic fat sugar protein fiber sodium iron alcohol caff water, by(seqn)
/// standardize
foreach i in systolic diastolic fat sugar protein fiber sodium iron alcohol caff water {
	egen `i'_mean = mean(`i')
	egen `i'_std = sd(`i')
	generate `i'_s = (`i'-`i'_mean)/`i'_std
}
//
/// keep only important variables
keep seqn gender systolic_s diastolic_s fat_s sugar_s protein_s fiber_s sodium_s iron_s alcohol_s caff_s water_s
generate diff = systolic_s - diastolic_s

// *save as proj_data2.dta*


// Penalty Weighted LASSO (use r(coef))
/// set up gend as continuous
generate gend = 0
replace gend = 0.5 if gender == 1
replace gend = -0.5 if gender == 2
//
/// set up penalty weight matrix with interaction at 1
mata: wt = (0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1)
mata: st_matrix("mweight", wt)
//
/// SYSTOLIC #1: lasso
lasso linear systolic_s gend fat_s c.fat_s#c.gend sugar_s c.sugar_s#c.gend protein_s c.protein_s#c.gend fiber_s c.fiber_s#c.gend sodium_s c.sodium_s#c.gend iron_s c.iron_s#c.gend alcohol_s c.alcohol_s#c.gend caff_s c.caff_s#c.gend water_s c.water_s#c.gend, penaltywt(mweight)
lassocoef, display(coef, standardized)
matrix sys1 = r(coef)
//
/// DIFF #1: lasso
lasso linear diff gend fat_s c.fat_s#c.gend sugar_s c.sugar_s#c.gend protein_s c.protein_s#c.gend fiber_s c.fiber_s#c.gend sodium_s c.sodium_s#c.gend iron_s c.iron_s#c.gend alcohol_s c.alcohol_s#c.gend caff_s c.caff_s#c.gend water_s c.water_s#c.gend, penaltywt(mweight)
lassocoef, display(coef, standardized)
matrix diff1 = r(coef)

// Let's do it again
/// SYSTOLIC #2: lasso
lasso linear systolic_s gend fat_s c.fat_s#c.gend sugar_s c.sugar_s#c.gend protein_s c.protein_s#c.gend fiber_s c.fiber_s#c.gend sodium_s c.sodium_s#c.gend iron_s c.iron_s#c.gend alcohol_s c.alcohol_s#c.gend caff_s c.caff_s#c.gend water_s c.water_s#c.gend, penaltywt(mweight)
lassocoef, display(coef, standardized)
matrix sys2 = r(coef)
//
/// DIFF #2: lasso
lasso linear diff gend fat_s c.fat_s#c.gend sugar_s c.sugar_s#c.gend protein_s c.protein_s#c.gend fiber_s c.fiber_s#c.gend sodium_s c.sodium_s#c.gend iron_s c.iron_s#c.gend alcohol_s c.alcohol_s#c.gend caff_s c.caff_s#c.gend water_s c.water_s#c.gend, penaltywt(mweight)
lassocoef, display(coef, standardized)
matrix diff2 = r(coef)

// Put into excel
putexcel set group_2.xlsx, sheet(Stata_sys) replace
putexcel B1 = "Sys #1"
putexcel B2 = matrix(sys1)
putexcel D1 = "Sys #2"
putexcel D2 = matrix(sys2)

putexcel set group_2.xlsx, sheet(Stata_diff) modify
putexcel B1 = "Diff #1"
putexcel B2 = matrix(diff1)
putexcel D1 = "Diff #2"
putexcel D2 = matrix(diff2)

