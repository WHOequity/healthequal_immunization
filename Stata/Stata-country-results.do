
/************************************************************************************
Quantifying inequalities in childhood immunization using summary measures of health 
inequality: An application of WHO Stata and R 'healthequal' packages
*************************************************************************************/

//	Prepare immunization dataset from the Health Inequality Data Repository (HIDR)

	*Import data using HIDR API
	import excel "https://datasafe-h5afbhf4gwctabaa.z01.azurefd.net/api/Download/TOP/rep_imm/data", firstrow clear
	*Keep required indicators and dimensions
	keep if inlist(indicator_abbr,"vdpt","vzdpt")
	keep if inlist(dimension,"Place of residence","Economic status (wealth quintile)","Subnational region")
	drop if wbincome=="High income"
	*Drop if all subgroups have missing data or all values equal to zero
	drop if inlist(dimension,"Economic status (wealth quintile)","Place of residence") & estimate==.
	bysort setting date indicator_abbr dimension: gen count=_N
	drop if dimension=="Economic status (wealth quintile)" & count!=5
	drop if dimension=="Place of residence" & count!=2
	bysort setting date indicator_abbr dimension: egen mean=mean(estimate)
	drop if inlist(dimension,"Economic status (wealth quintile)","Subnational region","Place of residence") & mean==0
	*Drop if >85% of subnational regions missing data 
	egen missing = rowmiss(estimate)
	bysort setting date indicator_abbr dimension: egen nmissing=total(missing)
	gen pmissing=nmissing/count
	keep if pmissing<=0.15
	drop missing nmissing pmissing
	*Keep latest data between 2013-2022
	keep if date>=2013 & date<=2022
	bysort setting indicator_abbr: egen max=max(date)
	gen latest=date==max
	keep if latest==1
	drop max
	tempfile immunization_data
	save `immunization_data', replace

//	Prepare Global Data Lab dataset from the Health Inequality Data Repository
	
	*Import data using HIDR API
	import excel "https://datasafe-h5afbhf4gwctabaa.z01.azurefd.net/api/Download/TOP/rep_gdl2/data", firstrow clear	
	*Keep latest data between 2013-2022
	keep if date>=2013 & date<=2022
	bysort setting indicator_abbr: egen max=max(date)
	gen latest=date==max
	keep if latest==1
	drop max
	*Keep required immunization indicators
	keep if inlist(indicator_abbr,"dtp3age1","dtp1age1") & dimension=="Subnational region"
	replace estimate=100-estimate if indicator_abbr=="dtp1age1"
	replace indicator_name="One-year-old children who did not receive any doses of the DTP vaccine" if indicator_abbr=="dtp1age1"
	replace indicator_abbr="vzdpt" if indicator_abbr=="dtp1age1"
	replace indicator_abbr="vdpt" if indicator_abbr=="dtp3age1"
	replace favourable_indicator=0 if indicator_abbr=="vzdpt"
	drop if wbincome=="High income"
	*Import subnational HDI values
	preserve
		import excel "https://datasafe-h5afbhf4gwctabaa.z01.azurefd.net/api/Download/TOP/rep_gdl1/data", firstrow clear
		keep if indicator_abbr=="shdi"
		rename estimate hdi
		recast str subgroup
		tempfile hdi
		save `hdi'
	restore
	*Merge subnational HDI values
	recast str subgroup
	merge m:1 setting date subgroup using `hdi', keep(3) keepusing(hdi) nogen
	*Order subnational regions
	drop subgroup_order
	gsort setting date indicator_abbr hdi
	bysort setting date indicator_abbr: gen subgroup_order=_n
	replace ordered_dimension=1
	replace dimension="Subnational region - HDI"
	tempfile subnational_hdi_data
	save `subnational_hdi_data', replace
	
//	Append immunization and HDI datasets

	use `immunization_data', clear
	append using `subnational_hdi_data'
	tempfile data
	save `data', replace
	
//	Calculate summary measures of health inequality 

	*Binary dimension (place of residence)
	use `data', clear
	local measure "d r"
	foreach m of local measure {
		preserve
			keep if dimension=="Place of residence"
			replace favourable_indicator=1 if inlist("`m'","d","r") 	// so that directionality/interpretation is the same across measures
			gen `m'_est=.
			gen `m'_lb=.
			gen `m'_ub=.
			egen group=group(setting date indicator_abbr dimension)
			qui levelsof group
			foreach g in `r(levels)'{
				healthequal estimate if group==`g', measure(`m') se(se) dim(ordered_dimension) fav(favourable_indicator) scale(indicator_scale) ref(reference_subgroup) noprint
				qui replace `m'_est=r(`m') if group==`g'
				qui replace `m'_lb=r(`m'_ll) if group==`g'
				qui replace `m'_ub=r(`m'_ul) if group==`g'
			}
			keep setting date source indicator_abbr indicator_name dimension setting_average iso3 whoreg6 wbincome `m'_*
			duplicates drop
			rename (`m'_est `m'_lb `m'_ub) (est1 ll1 ul1)
			reshape long est ll ul, i(setting date source indicator_abbr) j(measure)
			tostring measure, replace
			replace measure="`m'"
			tempfile `m'
			save ``m''
		restore
	}
	use `d', clear
	append using `r'
	tempfile binary
	save `binary', replace

	*Ordered dimensions (economic status and subnational region ordered by HDI)
	use `data', clear
	local measure "d r aci rci sii rii par paf"
	foreach m of local measure {
		preserve
			keep if inlist(dimension,"Economic status (wealth quintile)","Subnational region - HDI")
			replace favourable_indicator=1 if inlist("`m'","d","r") 	// so that directionality/interpretation is the same across measures
			gen `m'_est=.
			gen `m'_lb=.
			gen `m'_ub=.
			egen group=group(setting date indicator_abbr dimension)
			qui levelsof group
			foreach g in `r(levels)'{
				healthequal estimate [pw=population] if group==`g', measure(`m') se(se) dim(ordered_dimension) ///
				order(subgroup_order) fav(favourable_indicator) scale(indicator_scale) noprint
				qui replace `m'_est=r(`m') if group==`g'
				qui replace `m'_lb=r(`m'_ll) if group==`g'
				qui replace `m'_ub=r(`m'_ul) if group==`g'
			}
			keep setting date source indicator_abbr indicator_name dimension setting_average iso3 whoreg6 wbincome `m'_*
			duplicates drop
			rename (`m'_est `m'_lb `m'_ub) (est1 ll1 ul1)
			reshape long est ll ul, i(setting date source indicator_abbr) j(measure)
			tostring measure, replace
			replace measure="`m'"
			tempfile `m'
			save ``m''
		restore
	}
	use `d', clear
	append using `r'
	append using `aci'
	append using `rci'
	append using `sii'
	append using `rii'
	append using `par'
	append using `paf'
	tempfile ordered
	save `ordered', replace
	
	*Non-ordered dimension (subnational region)
	use `data', clear
	local measure "d r mdmw mdmu idisw idisu bgv bgsd cov ti par paf"
	foreach m of local measure {	
		preserve
			keep if dimension=="Subnational region"
			replace favourable_indicator=1 if inlist("`m'","d","r") 	// so that directionality/interpretation is the same across measures
			gen `m'_est=.
			gen `m'_lb=.
			gen `m'_ub=.
			egen group=group(setting date indicator_abbr dimension)
			qui levelsof group
			foreach g in `r(levels)'{
				healthequal estimate [pw=population] if group==`g', measure(`m') se(se) dim(ordered_dimension) ///
				fav(favourable_indicator) scale(indicator_scale) noprint
				qui replace `m'_est=r(`m') if group==`g'
				qui replace `m'_lb=r(`m'_ll) if group==`g'
				qui replace `m'_ub=r(`m'_ul) if group==`g'
			}
			keep setting date source indicator_abbr indicator_name dimension setting_average iso3 whoreg6 wbincome `m'_*
			duplicates drop
			rename (`m'_est `m'_lb `m'_ub) (est1 ll1 ul1)
			reshape long est ll ul, i(setting date source indicator_abbr) j(measure)
			tostring measure, replace
			replace measure="`m'"
			tempfile `m'
			save ``m''
		restore
	}
	use `d', clear
	append using `r'
	append using `mdmw'
	append using `mdmu'
	append using `idisw'
	append using `idisu'
	append using `bgv'
	append using `bgsd'
	append using `cov'
	append using `ti'
	append using `par'
	append using `paf'
	tempfile nonordered
	save `nonordered', replace
	
//	Append results

	use `binary', clear
	append using `ordered'
	append using `nonordered'
	
	replace measure="Difference" if measure=="d"
	replace measure="Ratio" if measure=="r"
	replace measure="Absolute concentration index" if measure=="aci"
	replace measure="Relative concentration index" if measure=="rci"
	replace measure="Slope index of inequality" if measure=="sii"
	replace measure="Relative index of inequality" if measure=="rii"
	replace measure="Between-group variance" if measure=="bgv"
	replace measure="Between-group standard deviation" if measure=="bgsd"
	replace measure="Coefficient of variation" if measure=="cov"
	replace measure="Weighted mean difference from mean" if measure=="mdmw"
	replace measure="Unweighted mean difference from mean" if measure=="mdmu"
	replace measure="Weighted index of disparity" if measure=="idisw"
	replace measure="Unweighted index of disparity" if measure=="idisu"
	replace measure="Theil index" if measure=="ti"
	replace measure="Population attributable risk" if measure=="par"
	replace measure="Population attributable fraction" if measure=="paf"
	
	sort measure dimension indicator_abbr setting
	save "Stata-country-results"
	