import sasxport5 BPX_I.XPT, clear
keep seqn bpxsy1 bpxdi1 bpxsy2 bpxdi2 bpxsy3 bpxdi3

egen systolic = rowmean(bpxsy1 bpxsy2 bpxsy3)
egen diastolic = rowmean(bpxdi1 bpxdi2 bpxdi3)
keep seqn systolic diastolic

generate missing = 0
replace missing = 1 if systolic == . | diastolic == .
drop missing