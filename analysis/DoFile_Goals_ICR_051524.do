***************************
** ICR sample 
** Created: 1/20/22 LB
** Updated: 5/15/24 bf
***************************

// set working directory
cd "/Users/settingbirdsfree/Library/CloudStorage/Dropbox-WMP/Breeze Floyd/WMP/Data/Ad Coding 2020/2-CODING General/2020 Entity Linking/FB Coding For Entity Linking/2 - Cleaning"

// load full dataset
use FBEL_1.0_datamgt_041222.dta, clear

	*4,475

order row

//Drop nocode ads
	drop if NOCODE==1 //21 obs dropped
//Drop spanish ads not coded by spanish coder
	drop if PROB_SPANISH==1 //45 obs dropped
//Drop other problems
	drop if PROB_VID_NOCODE==1 //0
	drop if PROB_VID_PARTIAL==1 //5
	drop if PROB_DONTUSE==1 //0
	drop if PROB_OTH==1 //4

// set missing to 0
ds, has(type double)

foreach var of var `r(varlist)' {
	replace `var'=0 if `var'==.
	}

//Keep only double coded ads
	gsort ad_id -RecordedDate -row
	quietly by ad_id:  gen dup = cond(_N==1,0,_n)
	tab dup, m
		order dup
			*browse if dup!=0
	keep if dup==1 | dup==2 // 3,408 obs deleted
	
	*992 obs
	
// drop ads where both the original coder and icr coder did not code any goals
drop if ad_id == "x_1013802839061651" | ad_id == "x_1018774888588233" | ad_id == "x_1022946704849820" | ad_id == "x_1044064442702708" | ad_id == "x_1050514808722682" | ad_id == "x_1123462268019275" | ad_id == "x_1159341084422111" | ad_id == "x_1197344737289060" | ad_id == "x_1210752179294106" | ad_id == "x_1218818301826571" | ad_id == "x_1220492008302204" | ad_id == "x_1275101456169508" | ad_id == "x_1399339740262352" | ad_id == "x_1658338494333235" | ad_id == "x_1749513885214640" | ad_id == "x_2407014902936767" | ad_id == "x_246427943420770" | ad_id == "x_2799526450149720" | ad_id == "x_250000553080959" | ad_id == "x_272763564016545" | ad_id == "x_280404492964915" | ad_id == "x_3304851472901345" | ad_id == "x_342656400326132" | ad_id == "x_347049963173544" | ad_id == "x_360743228615314" | ad_id == "" | ad_id == "x_361669098259641" | ad_id == "x_376712830150403" | ad_id == "x_413642760028639" | ad_id == "x_618636602132423" | ad_id == "x_657297251580404" | ad_id == "x_803982207079156" 
* 62 obs deleted

// prepare for icr
replace coder="100" if dup==1
replace coder="200" if dup==2


//Drop string vars
	drop othercand5_txt othercand5_txt othercand4_txt othercand3_txt othercand2_txt ///
	othercand1_txt MORETHAN8_TXT ISSUE97_TXT NOCODE_TXT pic_oth_txt ///
	PAGENAME_INFO_TXT OTHERNOTES_TXT PROB_OTH_TXT PROB_QUEST_TXT CAND7 CAND6 CAND5 ///
	CAND4 CAND3 CAND2 CAND1 CAND8 CAND8_pic CAND1_pic TGT8 TGT1 FAV8 FAV1 TGT3 TGT2 ///
	CAND3_pic CAND4_pic CAND5_pic CAND6_pic FAV7 TGT7 FAV6 FAV5 FAV4 FAV3 FAV2 ///
	CAND7_pic TGT6 TGT5 TGT4 CAND2_pic candnumber NOMINATE_TXT FECID1 FECID2 FECID3 ///
	FECID4 FECID5 FECID6 FECID7 FECID8 dup RecordedDate row rec_num
	
	

recast str150 ad_id
	format %30s ad_id
recast strL coder	

// uncomment to install
*ssc install krippalpha

ds, has(type double)

global reshapevars `r(varlist)'
	

// Reshape to wide	
	reshape wide $reshapevars, i(ad_id) j(coder) string


	
****************
***ALPHAS
****************
	
	foreach var in $reshapevars {
		capture krippalpha `var'100 `var'200
		gen alpha`var'=r(k_alpha)
		}
		
		*, method(ordinal)
		
	collapse (first) alphaNOCODE-alphaPROB_QUEST

	foreach var of varlist alpha* {
		local newname : subinstr local var "alpha" ""
		rename `var' `newname'
		}

	xpose, clear varname 
	order _varname
	rename _varname variable
	rename v1 alphas




	