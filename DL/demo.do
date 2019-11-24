import sasxport5 DEMO_I.XPT, clear
keep seqn riagendr ridageyr indfmpir

rename riagendr gender2
rename ridageyr age
rename indfmpir pir

label define gender_name 1 "Male" 2 "Female"
label values gender2 gender_name
decode gender2, generate(gender)
drop gender2