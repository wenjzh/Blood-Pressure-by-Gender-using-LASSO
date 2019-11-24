merge 1:m seqn using blood_pressure.dta
merge 1:m seqn using nutrient1.dta

rename dr1itfat fat
rename dr1isugr sugar
rename dr1iprot protein
rename dr1ifibe fiber
rename dr1isodi sodium
rename dr1iiron iron
rename dr1ialco alcohol
rename dr1icaff caffeine

drop _merge
generate missing = 0
replace missing = 1 if age == . | pir == . | gender == "." | systolic == . | diastolic == . | fat == . | sugar == . | protein == . | fiber == . | sodium == . | iron == . | alcohol == . | caffeine == .

keep if missing == 0

net install lassopack, from("https://raw.githubusercontent.com/statalasso/lassopack/master/lassopack_v131/")

lasso2 systolic fat sugar protein fiber sodium iron alcohol caffeine, lic(ebic)

lasso2, lic(ebic)