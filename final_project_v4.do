clear
global repo "~/teach/435" // student: substitute with "https://econweb.ucsd.edu/muendler/teach/20f/435"
global datadir "~/Documents/UCSD/Class Work/Prior Classes/Intl Trade/Code"  // student: substitute with your local path for main data (such as "~/gpec435/gen")
cd "~/Documents/UCSD/Class Work/Prior Classes/Intl Trade/final_project_2"  // student: substitute with your local path for this lecture/exercise (such as "~/gpec435/lec03")


set more off
set matsize 400
capture matrix drop _all
capture log close
log using "final_project_2.log", text replace

adopath + "$repo/ado"


*** Reshape LSBCI Data****

import delimited "lsbci_2015.csv", varnames(1)
rename *, upper

foreach x of var * { 
	rename `x' v_`x' 
} 

drop if v_DZA == "_"

rename v_COUNTRY source
reshape long v_, i(source) j(lsbci, string)
rename lsbci destination
rename source sorc_iso3
rename destination dest_iso3
gen lsbci = real(v_)
drop v_
gen l_lsbci = log(lsbci)
gen year = 2015

save "lsbci_2015.dta", replace

clear
import excel "lsbci_2014.xlsx", firstrow
rename *, upper

foreach x of var * { 
	rename `x' v_`x' 
} 

drop if v_DZA == "_"

rename v_COUNTRY source
reshape long v_, i(source) j(lsbci, string)
rename lsbci destination
rename source sorc_iso3
rename destination dest_iso3
gen lsbci = real(v_)
drop v_
gen l_lsbci = log(lsbci)
gen year = 2014

save "lsbci_2014.dta", replace

clear
import excel "lsbci_2013.xlsx", firstrow
rename *, upper

foreach x of var * { 
	rename `x' v_`x' 
} 

drop if v_DZA == "_"

rename v_COUNTRY source
reshape long v_, i(source) j(lsbci, string)
rename lsbci destination
rename source sorc_iso3
rename destination dest_iso3
gen lsbci = real(v_)
drop v_
gen l_lsbci = log(lsbci)
gen year = 2013

save "lsbci_2013.dta", replace

append using "lsbci_2014.dta"
append using "lsbci_2015.dta"
save "lsbci_v2.dta", replace




*** Clean and import ISO codes into LSBCI data***

* Constant elasticity of substitutin
scalar theta = 5  // trade eslasticity of -5 (sigma-1, benchmark in literature)


**** SAMPLE AND REGRESSOR CONSTRUCTION ****
*=========================================*

*** Read and prepare CEPII gravity data, distance plus
use iso3_o iso3_d year distw contig using "$datadir/cepii-gravdata.dta", clear
keep if year>=2013
label var year "Calendar year"
label var distw "Population-weighted Distance (km)"  // note: not statutory miles
label var contig "Shared land border"
rename iso3_o sorc_iso3
rename iso3_d dest_iso3
summ distw contig // some observations to be lost from using fta_wto in regressions
order sorc_iso3 dest_iso3 year distw* contig
sort sorc_iso3 dest_iso3 year
duplicates report sorc_iso3 dest_iso3 year  // check that data exhaustively indexed at country industry year level
 merge 1:1 sorc_iso3 dest_iso3 year using "lsbci_v2.dta"
drop _merge
replace lsbci = .25 if sorc_iso3==dest_iso3
gen lsbci_new = lsbci
replace lsbci_new = lsbci*1.07 if sorc_iso3 == "IND" || dest_iso3 == "IND"
gen l_lsbci_new = log(lsbci_new)
gen multiplier = 1
replace multiplier = 1.07 if sorc_iso3 == "IND" || dest_iso3 == "IND"
save "cepii-distw-final.dta", replace


*** Read and prepare ITPD data at source-destination-year level (aggregating over industries)
use exporter_iso3 importer_iso3 year industry_id broad_sector trade using "$datadir/itpd.dta", clear
keep if year>=2013 & year<=2015  // for three-year average
drop if broad_sector==4  // remove services, retain agriculture, manufacturing and mining/energy
desc trade
collapse (sum) trade, by(exporter_iso3 importer_iso3 year) fast
collapse (mean) trade, by(exporter_iso3 importer_iso3 year) fast
replace trade = trade/(10^3)
label var trade "Bilateral trade of goods (current US$ billion, 3-year average)"
foreach var of varlist exp* imp* {  // rename country variables to source and destination
  local newname = subinstr("`var'","exporter","sorc",.)
  local newname = subinstr("`newname'","importer","dest",.)
  rename `var' `newname'
  }
duplicates report sorc_iso3 dest_iso3 year  // check that data exhaustively indexed at country industry year level
compress
sort sorc_iso3 dest_iso3 year


*** Identify 68 top-exporting source countries
tempfile srcslct
preserve
  qui summ trade
  local tottrade = `r(sum)'
  collapse (sum) trade, by(sorc_iso3) fast
  qui count
  local src_count = `r(N)'
  egen _rnk=rank(trade)
  gen percentile = _rnk/`src_count'
  drop _rnk
  gsort -percentile
  gen cum_trade = trade/`tottrade' if _n==1
  replace cum_trade = trade/`tottrade' + cum_trade[_n-1] if _n>1
  keep in 1/68
  disp "Share of exports accounted for by top 68 countries: " cum_trade[_N]
  keep sorc_iso3
  rename sorc_iso3 iso_a3
  sort iso_a3
  save `srcslct'
restore


*** Combine ITPD and CEPII
merge 1:1 sorc_iso3 dest_iso3 year using "cepii-distw-final.dta"
tab year if _merge==1  // loss of information (several ITPD countries not covered in CEPII), year 2016 missing in CEPII
drop if _merge==2
drop _merge
sort sorc_iso3 dest_iso3 year


*** Reduce ITPD sample to 68 top-exporting Source countries plus Rest of World
foreach sd in sorc dest {
  rename `sd'_iso3 iso_a3
  merge m:1 iso_a3 using `srcslct'
  gen str _`sd'_iso3 = iso_a3 if _merge==3
  replace _`sd'_iso3 = "ROW" if _merge==1 // Rest of World
  rename iso_a3 `sd'_iso3
  drop _merge
  }
collapse (sum) trade (mean) distw (max) contig lsbci l_lsbci lsbci_new l_lsbci_new multiplier, by(_sorc_iso3 _dest_iso3 year) fast
label var trade "Bilateroal trade of goods (current US$ billion)"
label var distw "Population-weighted Distance (km)"
label var contig "Shared land border"
foreach sd in sorc dest {
  rename _`sd'_iso3 `sd'_iso3
  }
label var sorc_iso3 "Exporter ISO 3-letter alpha code (and ROW)"
label var dest_iso3 "Importer ISO 3-letter alpha code (and ROW)"
order sorc_iso3 dest_iso3 year
sort sorc_iso3 dest_iso3 year


keep if year == 2015


*** Naive gravity variables
egen production = sum(trade), by(sorc_iso3)
label var production "Source country production of goods (current US$ billion)"
egen marketsize = sum(trade), by(dest_iso3)
label var marketsize "Destination country market size of goods (current US$ billion)"

*** Variables in logs
foreach var of varlist trade distw production marketsize {  // zero will be missing
  local lbl : variable label `var'
  local newlbl = "Log " + trim(subinstr("`lbl'","of goods (current US$ billion)","",.))
  gen ln_`var' = log(`var')
  label var ln_`var' "`newlbl'"
  }
  
 

/*
*** International border
gen byte intlbrdr = (sorc_iso3~=dest_iso3)
label var intlbrdr "Cross-border trade flow"
	*/
	
*** Reference country
* Set iso3 to "ZZZ" for reference oountry so fixed effects of reference country last ones created
* Reference country: United States
gen marketsize_ref_bln = marketsize if dest_iso3=="ITA"
replace sorc_iso3 = "ZZZ" if sorc_iso3=="ITA"
replace dest_iso3 = "ZZZ" if dest_iso3=="ITA"
egen marketsize_ref = mean(marketsize_ref_bln)  // spread to all observations
label var marketsize_ref "Reference country (Italy) market size of goods (current US$ billion)"
drop marketsize_ref_bln
gen _imports = trade if sorc_iso3~=dest_iso3
egen imports_dst = sum(_imports), by(dest_iso3 year)
gen selftrade = marketsize - imports
drop _imports imports
label var selftrade "Self trade of goods (current US$ billion)"

*** Source and destination IDs (for PPML-HDFE)
encode sorc_iso3, gen(src_id) label(src_iso3)
encode dest_iso3, gen(dst_id) label(dest_iso3)
qui count if src_id==dst_id & sorc_iso3~=dest_iso3
if `r(N)'>0  disp as err "Source and Destination IDs do not match"

* Remove observations with missing values
qui reghdfe trade ln_distw contig lsbci, absorb(src_id dst_id) nocons
drop if ~e(sample)

order sorc_iso3 dest_iso3 src_id dst_id
sort sorc_iso3 dest_iso3
save "itpd-cepii-2014-final.dta", replace



**** GE-PPML Step I: Solve the baseline gravity model ****
*========================================================*

use "itpd-cepii-2014-final.dta", clear

*** Step I.a. Obtain estimates of trade costs and trade elasticities baseline indexes
***           Estimate the gravity model in the "baseline" scenario with the PPML estimator

* Estimate (PPML-HDFE)
ppmlhdfe trade ln_distw contig lsbci, absorb(src_id dst_id, savefe) noconst d
est store m1
predict tradehat_bln, mu  // log-linear prediction of bilateral trade at sample mean
esttab m1 using "regression.rtf", replace ar2 label se star nogaps compress addnotes("PPML HDFE with source and destination fixed effects")


* Predict source and destination effects: exp_srcfe_bln, exp_dstfe_bln
qui summ __hdfe2__ if dest_iso3=="ZZZ"
local dst_fe_zzz = `r(mean)'
gen double exp_srcfe_bln = exp(__hdfe1__ + `dst_fe_zzz' + _b[_cons])
gen double exp_dstfe_bln = exp(__hdfe2__ - `dst_fe_zzz')

* Predict bilateral trade costs		
gen double tau_sd_bln = exp(_b[ln_distw]*ln_distw + _b[contig]*contig + _b[lsbci]*lsbci) 


*** Step I.b. Construct baseline indexes	

* Compute outward and inward multilateral resistances
* using additive property of the PPML estimator that links source and destination fixed effects
* with their respective multilateral resistances, taking into account normalisation
gen omr_bln = production * marketsize_ref / exp_srcfe_bln
gen imr_bln = marketsize / (exp_dstfe_bln * marketsize_ref)	

* Predict international trade in baseline for given values of production and market size			
gen _exports_src_bln = tradehat_bln if sorc_iso3~=dest_iso3
egen exports_src_bln = sum(_exports_src_bln), by(sorc_iso3 year)
gen _imports_dst_bln = tradehat_bln if sorc_iso3~=dest_iso3
egen imports_dst_bln = sum(_imports_dst_bln), by(dest_iso3 year)
drop _exports_src_bln _imports_dst_bln
gen production_bln = production
gen marketsize_bln = marketsize
rename selftrade selftrade_bln

		
**** GE-PPML Step II: Define a conterfactual scenario ****
*========================================================*

* Counterfactual scenario: Remove international borders
* Constrain parameter associated with intlbrdr to zero, using ppmlhdfe, offset(ln_tau_sd_cfl)
* and assume coefficient on other variables (ln_distw and contig) remain same
gen tau_sd_cfl = exp(_b[ln_distw]*ln_distw + _b[contig]*contig + _b[lsbci]*(lsbci_new))
gen ln_tau_sd_cfl = log(tau_sd_cfl)	


**** GE-PPML Step III: Solve the counterfactual model ****
*========================================================*

* Step III.a.: Obtain conditional general equilibrium effects

* (i):	Estimate gravity under constraints from counterfactual, predict trade value
ppmlhdfe trade, absorb(src_id dst_id, savefe) noconst offset(ln_tau_sd_cfl) d
predict tradehat_cdl, mu  // log-linear prediction of bilateral trade at sample mean

* (ii):	Predict source and destination effects: exp_srcfe_cdl, exp_dstfe_cdl
qui summ __hdfe2__ if dest_iso3=="ZZZ"
local dst_fe_zzz = `r(mean)'
gen double exp_srcfe_cdl = exp(__hdfe1__ + `dst_fe_zzz' + _b[_cons])
gen double exp_dstfe_cdl = exp(__hdfe2__ - `dst_fe_zzz')

*       Compute outward and inward multilateral resistances
gen omr_cdl = production * marketsize_ref / exp_srcfe_cdl
gen imr_cdl = marketsize / (exp_dstfe_cdl * marketsize_ref)

*       Predict conditional exports for given values of production and market size			
gen _exports_src_cdl = tradehat_cdl if sorc_iso3~=dest_iso3
egen exports_src_cdl = sum(_exports_src_cdl), by(sorc_iso3 year)
drop _exports_src_cdl

*       Predict conditional imports for given values of production and market size			
gen _imports_dst_cdl = tradehat_cdl if sorc_iso3~=dest_iso3
egen imports_dst_cdl = sum(_imports_dst_cdl), by(dest_iso3 year)
drop _imports_dst_cdl

save "itpd-cepii-2014-upto-step3a.dta", replace // if version 13
capture saveold "itpd-cepii-2014-upto-step3a.dta", replace version(13)
use "itpd-cepii-2014-upto-step3a.dta", clear

* Step III.b: Obtain full endowment general equilibrium effects
*             Launch iterative procedure by specifying initial variables,
*             s = 0 stands for baseline (bln) value and s = 1 stands for onditional general equilibrium (cdl) value

*            Parameter phi>0 regulates trade imbalance (deficit iff phi>1)
gen phi = marketsize/production if sorc_iso3==dest_iso3 // country-specific savings behavior
			
*            Change in bilateral trade costs resulting from counterfactual (cfl)
gen change_tau_sd = tau_sd_cfl / tau_sd_bln	

*            Starting values for baseline and conditional scenarios
*            Production
gen production_0 = production
gen production_1 = production
				
*            Market size (expenditures), including reference country
gen marketsize_0 = marketsize
gen marketsize_ref_0 = marketsize_ref
gen marketsize_1 = marketsize
gen marketsize_ref_1 = marketsize_ref			
			
*            Predicted trade flow
gen tradehat_1 = tradehat_cdl

* (i) Endogenous factory-gate prices and multilateral resistances
	
*     Starting values for factory-gate prices under baseline and conditional scenarios				
gen exp_srcfe_0 = exp_srcfe_bln
gen _exp_srcfe_src_0 = exp_srcfe_0 if sorc_iso3==dest_iso3
egen exp_srcfe_dst_0 = mean(_exp_srcfe_src_0), by(dest_iso3 year)
gen exp_srcfe_1 = exp_srcfe_cdl
gen _exp_srcfe_src_1 = exp_srcfe_1 if sorc_iso3==dest_iso3
egen exp_srcfe_dst_1 = mean(_exp_srcfe_src_1), by(dest_iso3 year)
drop _exp_srcfe_src_*
gen exp_dstfe_0 = exp_dstfe_bln	
gen exp_dstfe_1 = exp_dstfe_cdl	
			
*     First order change in factory-gate prices	in baseline and conditional scenarios
gen change_price_src_0 = 0				
gen change_price_src_1 = ((exp_srcfe_1 / exp_srcfe_0) / (marketsize_ref_1 / marketsize_ref_0))^(1/(-theta))
gen change_price_dst_1 = ((exp_srcfe_dst_1 / exp_srcfe_dst_0) / (marketsize_ref_1 / marketsize_ref_0))^(1/(-theta))
		
*    Starting values for outward and inward multilateral resistances in baseline and conditional scenarios
gen omr_full_0 = production_0 * marketsize_ref_0 / exp_srcfe_0
gen imr_full_0 = marketsize_0 / (exp_dstfe_0 * marketsize_ref_0)		
gen omr_full_1 = production_1 * marketsize_ref_1 / exp_srcfe_1
gen imr_full_1 = marketsize_1 / (exp_dstfe_1 * marketsize_ref_1)
			
*    Starting values for change in outward and multilateral resitances (set to zero)
gen change_imr_full_1 = exp(0)		
gen change_omr_full_1 = exp(0)


******************** Start of the Iterative Procedure (Step III.b) *********************

* Criterion for convergence (s is number of iterations):
* Either standard errors or maximum difference between two iterations of factory-gate prices smaller than 0.01
local s = 3	 // iteration counter plus 2
di "here1"
local sd_dif_change_pi = 1
di "here2"
local max_dif_change_pi = 1
di "here3"
while (`sd_dif_change_pi' > 0.01) | (`max_dif_change_pi' > 0.01) {  // start of while loop
    local s_1 = `s' - 1  // one iteration ago
	di "here4"
    local s_2 = `s' - 2  // two iterations ago (conditional general equilibrium effects for first iteration)
    di "here5"
	local s_3 = `s' - 3  // three iterations ago (baseline for first iteration)
	di "here6"
    * (ii) Update endogenous income, expenditures and trade	
	gen trade_`s_1' =  tradehat_`s_2' * change_price_src_`s_2' * change_price_dst_`s_2' / (change_omr_full_`s_2' * change_imr_full_`s_2')
		sum trade_`s_1' tradehat_`s_2' change_price_src_`s_2' change_price_dst_`s_2' change_omr_full_`s_2' change_imr_full_`s_2'
		di "here7"
    * (iii) Estimation of structural gravity model
	capture noi ppmlhdfe trade_`s_1', noconst absorb(src_id dst_id, savefe) offset(ln_tau_sd_cfl) d
	di "here8"
	predict tradehat_`s_1', mu
	di "here9"
				
    * Update production & market size		
    egen production_`s_1' = total(tradehat_`s_1'), by(sorc_iso3 year)
    qui gen _marketsize_`s_1' = phi * production_`s_1' if sorc_iso3==dest_iso3
    egen marketsize_`s_1' = mean(_marketsize_`s_1'), by(dest_iso3 year)
    qui gen _marketsize_ref_`s_1' = marketsize_`s_1' if dest_iso3=="ZZZ"
    egen marketsize_ref_`s_1' = mean(_marketsize_ref_`s_1')
    drop _marketsize_`s_1' _marketsize_ref_`s_1'
			
    * Update fixed effects estimates
    qui summ __hdfe2__ if dest_iso3=="ZZZ"
    local dst_fe_zzz = `r(mean)'
    gen double exp_srcfe_`s_1' = exp(__hdfe1__ + `dst_fe_zzz' + _b[_cons])
    gen double exp_dstfe_`s_1' = exp(__hdfe2__ - `dst_fe_zzz')
    qui gen _exp_srcfe = exp_srcfe_`s_1' if sorc_iso3==dest_iso3
    egen exp_srcfe_dst_`s_1' = mean(_exp_srcfe), by(dest_iso3 year)
    drop _exp_srcfe	
				
    * Update factory-gate prices and multilateral resistances
    gen change_price_src_`s_1' = ((exp_srcfe_`s_1' / exp_srcfe_`s_2') / (marketsize_ref_`s_1' / marketsize_ref_`s_2'))^(1/(-theta))
    gen change_price_dst_`s_1' = ((exp_srcfe_dst_`s_1' / exp_srcfe_dst_`s_2') / (marketsize_ref_`s_1' / marketsize_ref_`s_2'))^(1/(-theta))
    gen omr_full_`s_1' = (production_`s_1' * marketsize_ref_`s_1') / exp_srcfe_`s_1'
    gen change_omr_full_`s_1' = omr_full_`s_1' / omr_full_`s_2'					
    gen imr_full_`s_1' = marketsize_`s_1' / (exp_dstfe_`s_1' * marketsize_ref_`s_1')
    gen change_imr_full_`s_1' = imr_full_`s_1' / imr_full_`s_2'
			
    * Check if change in factory-gate prices has converged to zero
    gen dif_change_omr_`s_1' = change_price_src_`s_2' - change_price_src_`s_3'
    display "************************* iteration count " `s_2' " *************************"
    summarize dif_change_omr_`s_1', format
    display "**********************************************************************"
    display " "
    local sd_dif_change_pi = r(sd)         // first convergence criterion (must be satisfied)
    local max_dif_change_pi = abs(r(max))  // second convergence criterion (must be satisfied)

    * Remove long-past iterations to manage memory use
    if `s'>=6 {
      local 2del = `s'-4
      drop trade_`2del' tradehat_`2del' production_`2del' marketsize_`2del' marketsize_ref_`2del' exp_srcfe_`2del' exp_dstfe_`2del' exp_srcfe_dst_`2del' change_price_src_`2del' change_price_dst_`2del' omr_full_`2del' change_omr_full_`2del' imr_full_`2del' change_imr_full_`2del' dif_change_omr_`2del'
      }

    local s = `s' + 1
    }  // end of while loop

********************* End of the Iterative Procedure  **********************
		
* (iv) Construction of the "full endowment general equilibrium" effects indexes
*      Use the result of the latest iteration S
local S = `s' - 2

* Compute full endowment general equilibrium of factory-gate price
gen change_price_src_full = ((exp_srcfe_`S' / exp_srcfe_0) / (marketsize_ref_`S' / marketsize_ref_0))^(1/(-theta))		

* Compute full endowment general equilibrium of value output
gen production_full = change_price_src_full  * production_bln

* Compute full endowment general equilibrium of value of expenditures
gen _marketsize_full = phi * production_full if sorc_iso3==dest_iso3
egen marketsize_full = mean(_marketsize_full), by(dest_iso3 year)
drop _marketsize_full

* Compute full endowment general equilibrium of outward and inward multilateral resistances
gen omr_full = production_full * marketsize_ref_`S' / exp_srcfe_`S'
gen imr_full = marketsize_`S' / (exp_dstfe_`S' * marketsize_ref_`S')	
			
* Compute full endowment general equilibrium of value of bilateral trade
gen trade_full = (production_full * marketsize_full * tau_sd_cfl) /(imr_full * omr_full)			

* Compute full endowment general equilibrium of value of total international trade (by source country)
gen _exports_src_full = trade_full if sorc_iso3~=dest_iso3
egen exports_src_full = sum(_exports_src_full), by(sorc_iso3 year)
drop _exports_src_full
					
* Compute full endowment general equilibrium of self trade (by destination country)
gen _imports_dst_full = trade_full if sorc_iso3~=dest_iso3
egen imports_dst_full = sum(_imports_dst_full), by(dest_iso3 year)
drop _imports_dst_full

* Save conditional and general equilibrium effects results	
compress
replace sorc_iso3 = "ITA" if sorc_iso3=="ZZZ"
replace dest_iso3 = "ITA" if dest_iso3=="ZZZ"
sort sorc_iso3 dest_iso3 year
save "trade-lsbci-fullge.dta", replace


**** GE-PPML Step IV: Collect, construct, and report indexes of interest ****
*===========================================================================*

*** Construct percentage changes on multilateral resistances/production side
use "trade-lsbci-fullge.dta", clear
collapse (mean) omr_full omr_cdl omr_bln change_price_src_full exports_src_* production_bln production_full, by(sorc_iso3 year)
rename sorc_iso3 country
sort country
	
* Percent change in full endowment general equilibrium of factory-gate prices
gen change_price_full = (change_price_src_full - 1) / 1 * 100
		
* Percent change in full endowment general equilibirum of outward multilateral resistances
gen change_omr_cdl = (omr_cdl^(1/(-theta)) - omr_bln^(1/(-theta))) / omr_bln^(1/(-theta)) * 100
	
* Percent change in full endowment general equilibrium of outward multilateral resistances			
gen change_omr_full = (omr_full^(1/(-theta)) - omr_bln^(1/(-theta))) / omr_bln^(1/(-theta)) * 100

* Percent change in conditional general equilibrium of bilateral trade
gen change_exports_src_cdl = (exports_src_cdl - exports_src_bln) / exports_src_bln * 100	
		
* Percent change in full endowment general equilibrium of bilateral trade		
gen change_exports_src_full = (exports_src_full - exports_src_bln) / exports_src_bln * 100
		
* Save results
save "trade-lsbci-fullge-prod.dta", replace

*** Construct percentage changes on import/consumption side
use "trade-lsbci-fullge.dta", clear
collapse(mean) imr_full imr_cdl imr_bln imports_dst_* selftrade_* marketsize* distw, by(dest_iso3 year)
rename dest_iso3 country
sort country		

* Percent change in conditional general equilibrium of inward multilateral resistances
gen change_imr_cdl = (imr_cdl^(1/(-theta)) - imr_bln^(1/(-theta))) / imr_bln^(1/(-theta)) * 100
		
* Percent change in full endowment general equilibrium of inward multilateral resistances
gen change_imr_full = (imr_full^(1/(-theta)) - imr_bln^(1/(-theta))) / imr_bln^(1/(-theta)) * 100
		
* Percent change in conditional general equilibrium of bilateral trade
gen change_imports_dst_cdl = (imports_dst_cdl - imports_dst_bln) / imports_dst_bln * 100	
		
* Percent change in full endowment general equilibrium of bilateral trade		
gen change_imports_dst_full = (imports_dst_full - imports_dst_bln) / imports_dst_bln * 100
		
* Percent change in full endowment general equilibrium of self trade (by destination country)
gen selftrade_dst_cdl = marketsize_bln - imports_dst_cdl
gen change_gt_cdl = ((selftrade_dst_cdl / selftrade_bln) ^(1 / (-theta)) - 1) * 100

* Percent change in full endowment general equilibrium of self trade (by destination country)
gen selftrade_dst_full = marketsize_full - imports_dst_full
gen change_gt_full = ((selftrade_dst_full / selftrade_bln) ^(1 / (-theta)) - 1) * 100

* Save results
save "trade-lsbci-fullge-cons.dta", replace

* Merge general equilibrium results from production and consumption sides
use "trade-lsbci-fullge-prod.dta", clear
merge 1:1 country year using "trade-lsbci-fullge-cons.dta"
	
* Full endowment general equilibrium of real GDP
gen rGDP_bln = production_bln / (imr_bln ^(1 / (-theta)))
gen rGDP_full = production_full / (imr_full ^(1 / (-theta)))
gen change_rGDP_full = (rGDP_full - rGDP_bln) / rGDP_bln * 100

* Keep indexes of interest	
keep country change_exports_src_cdl change_exports_src_full change_imports_dst_cdl change_imports_dst_full change_gt_cdl change_gt_full change_price_full change_imr_full change_rGDP_full production_bln distw
order country change_exports_src_cdl change_exports_src_full change_imports_dst_cdl change_imports_dst_full change_gt_cdl change_gt_full change_price_full change_imr_full change_rGDP_full production_bln distw
			
*** Export the results to Excel file
export excel using "trade-lsbci-fullge.xls", firstrow(variables) replace

*** Graphs

* Create a graphic showing the conditional and full endowment general equilibrium on exports
gen ln_production = log(production_bln)
twoway (scatter change_exports_src_cdl ln_production, mfcolor(ltblue) mlcolor(navy)) (scatter change_exports_src_full ln_production, mfcolor(midblue) mlcolor(navy)), xtitle("Log Production") ytitle("Percentage change of Exports") legend(lab(1 "Conditional general equilibrium")  lab(2 "Full endowment general equilibrium")) graphr(color(white)) plotr(color(white))
graph export "scatter_exports_output.png", replace
tab country if change_exports_src_cdl>100

* Create a graphic showing the conditional and full endowment general equilibrium on imports
twoway (scatter change_imports_dst_cdl ln_production, mfcolor(ltblue) mlcolor(navy)) (scatter change_imports_dst_full ln_production, mfcolor(midblue) mlcolor(navy)), xtitle("Log Production") ytitle("Percentage change of Exports") legend(lab(1 "Conditional general equilibrium")  lab(2 "Full endowment general equilibrium")) graphr(color(white)) plotr(color(white))
graph export "scatter_imports_output.png", replace
tab country if change_imports_dst_full<0

* Create a graphic showing the conditional and full endowment general equilibrium on the gains from trade, against log output
twoway (scatter change_gt_cdl ln_production, mfcolor(ltblue) mlcolor(navy)) (scatter change_gt_full ln_production, mfcolor(midblue) mlcolor(navy)), xtitle("Log Production") ytitle("Percentage change of Gains from Trade") legend(lab(1 "Conditional general equilibrium")  lab(2 "Full endowment general equilibrium")) graphr(color(white)) plotr(color(white))
graph export "scatter_gt_output.png", replace

* Create a graphic showing the conditional and full endowment general equilibrium on the gains from trade, against log distance
gen ln_distw = log(distw)
twoway (scatter change_gt_cdl ln_dist, mfcolor(ltblue) mlcolor(navy)) (scatter change_gt_full ln_dist, mfcolor(midblue) mlcolor(navy)), xtitle("Mean Log Distance (km) of Destination") ytitle("Percentage change of Gains from Trade") legend(lab(1 "Conditional general equilibrium")  lab(2 "Full endowment general equilibrium")) graphr(color(white)) plotr(color(white))
graph export "scatter_gt_dist.png", replace

* Create a graphic showing the impact on real GDP, factory-gate prices and -1* inward multilateral resistances
gen _change_imr_full = -1 * change_imr_full
twoway (scatter change_rGDP_full ln_production, mfcolor(navy) mlcolor(navy)) (scatter change_price_full ln_production, mfcolor(midblue) mlcolor(navy)) (scatter _change_imr_full ln_production, mfcolor(bluishgray) mlcolor(navy)), xtitle("Log Production") ytitle("Percentage changes") legend(lab(1 "Real GDP")  lab(2 "Factory-gate Price") lab(3 "-(Inward Multilateral Resistance)")) graphr(color(white)) plotr(color(white))
graph export "scatter_rgdp_output.png", replace


clear
import excel using "trade-lsbci-fullge.xls", firstrow
save "trade-lsbci-fullge.dta", replace

* Clean map data 
clear
use "~/Documents/UCSD/Class Work/Prior Classes/Intl Trade/Code/ne_110m_admin_0_countries_data_2020sep30.dta", replace
drop if type == "Dependency"
drop if sov_a3=="-99" | admin=="Antarctica"
rename adm0_a3 country

* Merge with prior files. 
merge 1:1 country using "trade-lsbci-fullge.dta"

gen change_rGDP_full_pos = change_rGDP_full
replace change_rGDP_full_pos = . if change_rGDP_full < 0

gen change_rGDP_full_neg = change_rGDP_full
replace change_rGDP_full_neg = . if change_rGDP_full_neg > 0

spmap change_rGDP_full_pos using "~/Documents/UCSD/Class Work/Prior Classes/Intl Trade/Code/ne_110m_admin_0_countries_coor_2020sep30.dta", id(id) fcolor(Greens) clmethod(custom) clbreaks(0 10 20 30 40) legend(symy(*2) symx(*2) size(*2)) legorder(lohi) title("Countries with positive real GDP growths") subtitle("new LSBCI") 
graph export "map_positive.png", replace

spmap change_rGDP_full_neg using "~/Documents/UCSD/Class Work/Prior Classes/Intl Trade/Code/ne_110m_admin_0_countries_coor_2020sep30.dta", id(id) fcolor(Reds) clmethod(custom) clbreaks(-40 -30 -20 -10 0) legend(symy(*2) symx(*2) size(*2)) legorder(lohi) title("Countries with negative real GDP growths") subtitle("New LSBCI")
graph export "map_negative.png", replace

log close






