** Cleaning simulation datasets 
cd "G:\BACKUP\Chris\Project_2\Main_Simulations\propodds\Results"


foreach num of numlist 1/162{
    import delimited results_`num', clear
    save "Analysis\results_`num'.dta", replace
}


use "Analysis\results_1",clear

foreach num of numlist 2/162{
    append using "Analysis\results_`num'"
}



// Basic checks 
// Performance measures 
summ bias, d
assert bias != .

summ coverage, d
assert coverage != .

summ mse, d 
assert mse != .

summ relbias, d
assert relbias != .


// MCSE 
summ bias_se, d
assert bias_se != .

summ coverage_se, d
assert coverage_se != .

summ mse_se, d 
assert mse_se != .

summ relbias_se, d
assert relbias_se != .


//Check other variables are in range 
tab cumlogit, m 
assert cumlogit != .

tab model 
assert model != ""

tab category
assert category != .

tab effectsize
assert effectsize == "0.06 increase" if truemod == "Linear"
assert effectsize != ""


tab truemod 
assert truemod != ""

tab controlprob //These should be equal numbers (apart from zero)
assert controlprob != ""

tab sampsize 
assert inlist(sampsize,1500,4000,10000)


//Row headers - don't need these 
drop v1 

// Create value labels 
gen truemod_ = 1 if truemod == "Divergent"
replace truemod_ = 2 if truemod == "Linear"
replace truemod_ = 3 if truemod == "Random PO [no violation]"
replace truemod_ = 4 if truemod == "Random PO [slight violation]"
replace truemod_ = 5 if truemod == "Random PO [moderate violation]"
replace truemod_ = 6 if truemod == "U-shape"

lab def truemod 1 "Divergent" 2 "Linear" 3 "Random PO [no violation]" 4 "Random PO [slight violation]" 5 "Random PO [moderate violation]" 6 "U-shape"
lab val truemod_ truemod 
drop truemod 
rename truemod_ truemod 

gen effectsize_ = 1 if effectsize == "0.06 increase"
replace effectsize_ = 2 if effectsize == "Small [1.10]"
replace effectsize_ = 3 if effectsize == "Moderate [1.50]"
replace effectsize_ = 4 if effectsize == "U-shape (1.50 to 1.10 to 1.50)"

lab def effect 1 "0.06 increase" 2 "Small [OR = 1.10]" 3 "Moderate [OR = 1.50]" 4 "U-shape (1.50 to 1.10 to 1.50)"
lab val effectsize_ effect 
drop effectsize
rename effectsize_ effectsize 


gen model_ = 1 if model == "PO"
replace model_ = 2 if model == "PPO"
replace model_ = 3 if model == "LR"
replace model_ = 4 if model == "CPO"
replace model_ = 5 if model == "LCPO"
replace model_ = 6 if model == "UCPO"

lab def model 1 "PO" 2 "Unconstrained PPO" 3 "LR" 4 "CPPO (divergent OR)" 5 "Linear CPPO" 6 "U-shape CPPO"
lab val model_ model 

drop model 
rename model_ model 

encode controlprob, gen(controlprob_)
drop controlprob 
rename controlprob_ controlprob 

sort truemod effectsize category model cumlogit 

order truemod effectsize category model cumlogit 

gen coveragepct = coverage*100
drop coverage 
rename coveragepct coverage 
order coverage, after(bias_se)

*Where there should not be duplicates 
duplicates list bias bias_se coverage coverage_se mse mse_se relbias relbias_se if model == 2
duplicates list bias bias_se coverage coverage_se mse mse_se relbias relbias_se if model == 3
duplicates list bias bias_se coverage coverage_se mse mse_se relbias relbias_se if model == 5

order relbias relbias_se, after(controlprob)

replace relbias = relbias*100 
export excel using "Analysis\Graphs\dataforgraph.xlsx", replace firstrow(var) keepcellfmt


** SENSITIVITY ANALYSIS 
import delimited results_3_sens, clear
save "Analysis\results_3_sens.dta", replace

import delimited results_7_sens, clear
save "Analysis\results_7_sens.dta", replace

import delimited results_10_sens, clear
save "Analysis\results_10_sens.dta", replace

import delimited results_13_sens, clear
save "Analysis\results_13_sens.dta", replace

import delimited results_90_sens, clear
save "Analysis\results_90_sens.dta", replace

import delimited results_102_sens, clear
save "Analysis\results_102_sens.dta", replace

import delimited results_103_sens, clear
save "Analysis\results_103_sens.dta", replace

import delimited results_112_sens, clear
save "Analysis\results_112_sens.dta", replace

import delimited results_115_sens, clear
save "Analysis\results_115_sens.dta", replace

import delimited results_164_sens, clear
save "Analysis\results_164_sens.dta", replace
	
** Import data 
use "Analysis\results_3_sens",clear
append using "Analysis\results_7_sens"
append using "Analysis\results_10_sens"
append using "Analysis\results_13_sens"
append using "Analysis\results_90_sens"
append using "Analysis\results_102_sens"
append using "Analysis\results_103_sens"
append using "Analysis\results_112_sens"
append using "Analysis\results_115_sens"
append using "Analysis\results_164_sens"


// Basic checks 
// Performance measures 
summ bias, d
assert bias != .

summ coverage, d
assert coverage != .

summ mse, d 
assert mse != .

summ relbias, d
assert relbias != .



// MCSE 
summ bias_se, d
assert bias_se != .

summ coverage_se, d
assert coverage_se != .

summ mse_se, d 
assert mse_se != .

summ relbias_se, d
assert relbias_se != .


//Check other variables are in range 
tab cumlogit, m 
assert cumlogit != .

tab model 
assert model != ""

tab category
assert category != .

tab effectsize
assert effectsize == "0.06 increase" if truemod == "Linear"
assert effectsize != ""


tab truemod 
assert truemod != ""

tab controlprob //These should be equal numbers (apart from zero)
assert controlprob != ""

tab sampsize 
assert inlist(sampsize,1500,4000,10000)


//Row headers - don't need these 
drop v1 

// Create value labels 
gen truemod_ = 1 if truemod == "Divergent"
replace truemod_ = 2 if truemod == "Linear"
replace truemod_ = 3 if truemod == "Random PO [no violation]"
replace truemod_ = 4 if truemod == "Random PO [slight violation]"
replace truemod_ = 5 if truemod == "Random PO [moderate violation]"
replace truemod_ = 6 if truemod == "U-shape"

lab def truemod 1 "Divergent" 2 "Linear" 3 "Random PO [no violation]" 4 "Random PO [slight violation]" 5 "Random PO [moderate violation]" 6 "U-shape"
lab val truemod_ truemod 
drop truemod 
rename truemod_ truemod 

gen effectsize_ = 1 if effectsize == "0.06 increase"
replace effectsize_ = 2 if effectsize == "Small [1.10]"
replace effectsize_ = 3 if effectsize == "Moderate [1.50]"
replace effectsize_ = 4 if effectsize == "U-shape (1.50 to 1.10 to 1.50)"

lab def effect 1 "0.06 increase" 2 "Small [OR = 1.10]" 3 "Moderate [OR = 1.50]" 4 "U-shape (1.50 to 1.10 to 1.50)"
lab val effectsize_ effect 
drop effectsize
rename effectsize_ effectsize 


gen model_ = 1 if model == "PO"
replace model_ = 2 if model == "PPO"
replace model_ = 3 if model == "LR"
replace model_ = 4 if model == "CPO"
replace model_ = 5 if model == "LCPO"
replace model_ = 6 if model == "UCPO"

lab def model 1 "PO" 2 "Unconstrained PPO" 3 "LR" 4 "CPPO (divergent OR)" 5 "Linear CPPO" 6 "U-shape CPPO"
lab val model_ model 

drop model 
rename model_ model 

encode controlprob, gen(controlprob_)
drop controlprob 
rename controlprob_ controlprob 

sort truemod effectsize category model cumlogit 

order truemod effectsize category model cumlogit 

gen coveragepct = coverage*100
drop coverage 
rename coveragepct coverage 
order coverage, after(bias_se)

*Where there should not be duplicates 
duplicates list bias bias_se coverage coverage_se mse mse_se relbias relbias_se if model == 2
duplicates list bias bias_se coverage coverage_se mse mse_se relbias relbias_se if model == 3
duplicates list bias bias_se coverage coverage_se mse mse_se relbias relbias_se if model == 5

order relbias relbias_se, after(controlprob)

replace relbias = relbias*100 
export excel using "Analysis\Graphs\Sensitivity\dataforgraph_sens.xlsx", replace firstrow(var) keepcellfmt



** EXTRA SCENARIOS 

foreach num of numlist 163/164{
    import delimited results_`num', clear
    save "Analysis\results_`num'.dta", replace
}


use "Analysis\results_163",clear

foreach num of numlist 164/164{
    append using "Analysis\results_`num'"
}

drop if model == "UCPO"

// Basic checks 
// Performance measures 
summ bias, d
assert bias != .

summ coverage, d
assert coverage != .

summ mse, d 
assert mse != .

summ relbias, d
assert relbias != .


// MCSE 
summ bias_se, d
assert bias_se != .

summ coverage_se, d
assert coverage_se != .

summ mse_se, d 
assert mse_se != .

summ relbias_se, d
assert relbias_se != .


//Check other variables are in range 
tab cumlogit, m 
assert cumlogit != .

tab model 
assert model != ""

tab category
assert category != .

tab effectsize
assert effectsize == "0.06 increase" if truemod == "Linear"
assert effectsize != ""


tab truemod 
assert truemod != ""

tab controlprob //These should be equal numbers (apart from zero)
assert controlprob != ""

tab sampsize 
assert inlist(sampsize,1500,4000,10000)


//Row headers - don't need these 
drop v1 

// Create value labels 
gen truemod_ = 1 if truemod == "Divergent"
replace truemod_ = 2 if truemod == "Linear"
replace truemod_ = 3 if truemod == "Random PO [no violation]"
replace truemod_ = 4 if truemod == "Random PO [slight violation]"
replace truemod_ = 5 if truemod == "Random PO [moderate violation]"
replace truemod_ = 6 if truemod == "U-shape"

lab def truemod 1 "Divergent" 2 "Linear" 3 "Random PO [no violation]" 4 "Random PO [slight violation]" 5 "Random PO [moderate violation]" 6 "U-shape"
lab val truemod_ truemod 
drop truemod 
rename truemod_ truemod 

gen effectsize_ = 1 if effectsize == "0.06 increase"
replace effectsize_ = 2 if effectsize == "Small [1.10]"
replace effectsize_ = 3 if effectsize == "Moderate [1.50]"
replace effectsize_ = 4 if effectsize == "U-shape (1.50 to 1.10 to 1.50)"

lab def effect 1 "0.06 increase" 2 "Small [OR = 1.10]" 3 "Moderate [OR = 1.50]" 4 "U-shape (1.50 to 1.10 to 1.50)"
lab val effectsize_ effect 
drop effectsize
rename effectsize_ effectsize 


gen model_ = 1 if model == "PO"
replace model_ = 2 if model == "PPO"
replace model_ = 3 if model == "LR"
replace model_ = 4 if model == "CPO"
replace model_ = 5 if model == "LCPO"
replace model_ = 6 if model == "UCPO"

lab def model 1 "PO" 2 "Unconstrained PPO" 3 "LR" 4 "CPPO (divergent OR)" 5 "Linear CPPO" 6 "U-shape CPPO"
lab val model_ model 

drop model 
rename model_ model 

encode controlprob, gen(controlprob_)
drop controlprob 
rename controlprob_ controlprob 

sort truemod effectsize category model cumlogit 

order truemod effectsize category model cumlogit 

gen coveragepct = coverage*100
drop coverage 
rename coveragepct coverage 
order coverage, after(bias_se)

*Where there should not be duplicates 
duplicates list bias bias_se coverage coverage_se mse mse_se relbias relbias_se if model == 2
duplicates list bias bias_se coverage coverage_se mse mse_se relbias relbias_se if model == 3
duplicates list bias bias_se coverage coverage_se mse mse_se relbias relbias_se if model == 5

order relbias relbias_se, after(controlprob)

replace relbias = relbias*100 
export excel using "Analysis\Graphs\dataforgraph_extra.xlsx", replace firstrow(var) keepcellfmt



exit 



