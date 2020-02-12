/*
Title: Coastal wetlands reduce property damage during tropical cyclones
Authors: Fanglin Sun and Richard T. Carson
Code Version: 01/28/2020*/


global path1 "YOURPATH\PNAS_wetland"
global path2 "YOURPATH\PNAS_wetland\mv"

clear
cd $path1
use pnas_upload.dta 

/*Table 1 Conditional damage model estimates*/
eststo: quietly reghdfe lnpropdam lnwetland lnwind right lnstormarea lnriskproperty , vce(cluster hurricane_id county_id) absorb(year state_id)
eststo: quietly reghdfe lnpropdam lnwetland 1.c1#c.lnwetland 1.c2#c.lnwetland 1.c345#c.lnwetland lnwind right lnstormarea lnriskproperty, vce(cluster hurricane_id county_id) absorb(year state_id)
eststo: quietly reghdfe lnpropdam lnwetland 1.saltwet#c.lnwetland lnwind right lnstormarea lnriskproperty, vce(cluster hurricane_id county_id) absorb(year state_id)
eststo: quietly reghdfe lnpropdam 1.forested#c.lnwetland lnwetland lnwind right lnstormarea lnriskproperty, vce(cluster hurricane_id county_id) absorb(year state_id)
eststo: quietly reghdfe lnpropdam  1.strictcode#c.lnwetland lnwetland lnwind right lnstormarea lnriskproperty, vce(cluster hurricane_id county_id) absorb(year state_id)
esttab using"table1.rtf", title(Table 1. Conditional damage model estimates. Standard errors (in parentheses) are clustered two-ways at the county and storm level. N=946. All models include state and year fixed effects. *P<0.10, **P<0.05, ***P<0.01.) mtitles("log(damage)" "log(damage)" "log(damage)" "log(damage)" "log(damage)") coeflabel(right "Right" 1.ts#c.lnwetland "Tropical storms X log(wetland)" 1.c1#c.lnwetland "C1 hurricanes X log(wetland)" 1.c2#c.lnwetland "C2 hurricanes X log(wetland)" 1.c345#c.lnwetland "C3-C5 hurricanes X log(wetland)" 1.strictcode#c.lnwetland "Strict building code X log(wetland)" 1.saltwet#c.lnwetland "Saltwater wetlands X log(wetland)" 1.forested#c.lnwetland "Forested wetlands X log(wetland)" lnpropdam "log(damage)" lnwetland "log(wetland)" lnwind "log(wind)" lnstormarea "log(storm area)" lnriskproperty "log(property at risk)" ) b(%9.4f) se(4) ar2(2)  star(* 0.10 ** 0.05 *** 0.01) modelwidth(8) replace
eststo clear

/*Table S1 Summary Statistics of the Conditional Damage Model*/
sum propdam2016 wind stormarea wetland riskproperty2016 right freshwet saltwet forested nonforested ts c1 c2 c345 strictcode loosecode if lnpropdam!=. & lnwetland!=. & lnriskproperty!=. & lnstormarea!=.

/*Table S2 Summary statistics for property damage across different tropical cyclone classes*/
tab category damage
tabstat propdam2016 if lnpropdam!=. & lnwetland!=. & lnriskproperty!=. & lnstormarea!=., by (category) stat (median mean min max sd)

/*Table S3. Probit Model*/
eststo: quietly probit damage wetland wind stormarea riskproperty right, vce(r)
esttab using"tableS3.rtf", title(Table S3. Probit model assessing effect of wetlands on reducing probability of experiencing property damage during a tropical cyclone hitting the U.S. from 1996 to 2016. *P<0.10, **P<0.05, ***P<0.01. Robust standard errors are given in parenthesis.)  coeflabel(wetland "Wetland" wind "Wind" storm "Storm area" riskproperty2016 "Property at risk" _cons "Constant" right "Right" ) mtitles("Prob(damage)") b(%9.3f) se(4) ar2(2)  star(* 0.10 ** 0.05 *** 0.01) replace
eststo clear

/*Table S4. Conditional damage model (marginal effect)*/
eststo: quietly reghdfe lnpropdam lnwetland lnwind right lnstormarea lnriskproperty, vce(cluster hurricane_id county_id) absorb(year state_id)
eststo: quietly reghdfe lnpropdam 1.ts#c.lnwetland 1.c1#c.lnwetland 1.c2#c.lnwetland 1.c345#c.lnwetland lnwind right lnstormarea lnriskproperty, vce(cluster hurricane_id county_id) absorb(year state_id)
eststo: quietly reghdfe lnpropdam 1.freshwet#c.lnwetland 1.saltwet#c.lnwetland lnwind right lnstormarea lnriskproperty, vce(cluster hurricane_id county_id) absorb(year state_id)
eststo: quietly reghdfe lnpropdam 1.nonforested#c.lnwetland 1.forested#c.lnwetland lnwind right lnstormarea lnriskproperty, vce(cluster hurricane_id county_id) absorb(year state_id)
eststo: quietly reghdfe lnpropdam 1.strictcode#c.lnwetland 1.loosecode#c.lnwetland lnwind right lnstormarea lnriskproperty, vce(cluster hurricane_id county_id) absorb(year state_id)
esttab using"tableS4.rtf", b(%9.4f) se(4) ar2(2)  star(* 0.10 ** 0.05 *** 0.01) title(Table S4. Conditional damage model estimates (with the marginal effects of wetlands reported in the table). Standard errors (in parenthese) are clustered two-ways at the county level and the hurricane level. *P<0.10, **P<0.05, ***P<0.01.) mtitles("log(damage)" "log(damage)" "log(damage)" "log(damage)" "log(damage)") coeflabel(right "Right" 1.ts#c.lnwetland "Tropical storms X log(wetland)" 1.c1#c.lnwetland "C1 hurricanes X log(wetland)" 1.c2#c.lnwetland "C2 hurricanes X log(wetland)" 1.c345#c.lnwetland "C3-C5 hurricanes X log(wetland)" 1.loosecode#c.lnwetland "Less strict building codes X log(wetland)" 1.strictcode#c.lnwetland "Strict building codes X log(wetland)" 1.saltwet#c.lnwetland "Saltwater wetlands X log(wetland)" 1.freshwet#c.lnwetland "Freshwater wetlands X log(wetland)" 1.nonforested#c.lnwetland "Non-forested wetlands X log(wetland)" 1.forested#c.lnwetland "Forested wetlands X log(wetland)" lnpropdam "log(damage)" lnwetland "log(wetland)" lnwind "log(wind)" lnstormarea "log(storm area)" lnriskproperty "log(property at risk)"  ) modelwidth(8) replace
eststo clear

/*Table S5. Robustness*/
eststo: quietly reghdfe lnpropdam lnwetland lnwind right lnstormarea lnriskproperty, vce(cluster hurricane_id county_id) absorb(year state_id)
eststo: quietly reghdfe lnpropdam lnwetland lnwind right lnstormarea lnriskproperty year year2, vce(cluster hurricane_id county_id) absorb(state_id)
eststo: quietly reghdfe lnpropdam lnwetland lnwind right lnstormarea lnriskproperty if storm!="Katrina", vce(cluster hurricane_id county_id) absorb(year state_id)
eststo: quietly reghdfe lnpropdam lnwetland lnwind right lnstormarea lnriskproperty if storm!="Katrina" & storm!="Sandy", vce(cluster hurricane_id county_id) absorb(year state_id)
esttab using"tableS5.rtf", title ("Table S5. Regression results for alternative specifications of the conditional damage model. Standard errors (in parentheses) are clustered two-ways at the county and storm level. *P<0.10, **P<0.05, ***P<0.01. Column 2 includes both linear and quadratic time trends, the coefficients of which are significant different from zero jointly at the 95% confidence level.")  mtitles("Base Model" "Add time trends" "Drop Katrina" "Drop Katrina & Sandy") coeflabel(right "Right" lnpropdam "log(damage)" lnwetland "log(wetland)" lnwind "log(wind)" lnstormarea "log(storm area)" lnriskproperty "log(property at risk)") b(%9.4f) se(4) ar2(2) star(* 0.10 ** 0.05 *** 0.01) modelwidth(5) replace
eststo clear

/*Table S6. Robustness Checks: county-level or sub-state-level fixed effects*/
eststo: quietly reghdfe lnpropdam lnwetland lnwind right lnstormarea lnriskproperty, vce(cluster hurricane_id county_id) absorb(year state_id)
eststo: quietly reghdfe lnpropdam lnwetland lnwind right lnstormarea lnriskproperty, vce(cluster hurricane_id county_id) absorb(year county_id)
eststo: quietly reghdfe lnpropdam lnwetland lnwind lnstormarea lnriskproperty right if n_county_damage>4, vce(cluster hurricane_id county_id) absorb(year state_id)
eststo: quietly reghdfe lnpropdam lnwetland lnwind lnstormarea lnriskproperty right if n_county_damage>4, vce(cluster hurricane_id county_id) absorb(year county_id)
eststo: quietly reghdfe lnpropdam lnwetland lnwind right lnstormarea lnriskproperty, vce(cluster hurricane_id county_id) absorb(year substate_FL_TX_id)
eststo: quietly reghdfe lnpropdam lnwetland lnwind lnstormarea lnriskproperty right, vce(cluster hurricane_id county_id) absorb(year substate_id)
esttab using"tableS6.rtf", title ("Table S6. Regression results for alternative specifications of the conditional damage model. Standard errors (in parentheses) are clustered two-ways at the county and storm level. *P<0.10, **P<0.05, ***P<0.01.")  mtitles("Base Model" "County FE" "State FE" "County FE" "Sub-State FE" "Sub-State FE") coeflabel(right "Right" lnpropdam "log(damage)" lnwetland "log(wetland)" lnwind "log(wind)" lnstormarea "log(storm area)" lnriskproperty "log(property at risk)") b(%9.4f) se(4) ar2(2) star(* 0.10 ** 0.05 *** 0.01) modelwidth(5) replace
eststo clear

/*Table S7. Robustness Checks: Other Coastal Defensive Measures*/
eststo: quietly reghdfe lnpropdam lnwetland lnwind right lnstormarea lnriskproperty, vce(cluster hurricane_id county_id) absorb(year state_id)
eststo: quietly reghdfe lnpropdam lnwetland lnwind lnstormarea lnriskproperty right lnlevee, vce(cluster hurricane_id county_id) absorb(year state_id)
eststo: quietly reghdfe lnpropdam  1.has_levee#c.lnwetland lnwetland lnwind lnstormarea lnriskproperty right has_levee, vce(cluster hurricane_id county_id) absorb(year state_id)
eststo: quietly reghdfe lnpropdam lnwetland lnwind lnstormarea lnriskproperty right hard_share, vce(cluster hurricane_id county_id) absorb(year state_id)
eststo: quietly reghdfe lnpropdam lnwetland lnwind lnstormarea lnriskproperty right lnbeach_nourishment, vce(cluster hurricane_id county_id) absorb(year state_id)
eststo: quietly reghdfe lnpropdam lnwetland lnwind lnstormarea lnriskproperty right lnlevee hard_share lnbeach_nourishment, vce(cluster hurricane_id county_id) absorb(year state_id)
esttab using"tableS7.rtf", title ("Table S7. Regression results for alternative specifications of the conditional damage model. Standard errors (in parentheses) are clustered two-ways at the county and storm level. *P<0.10, **P<0.05, ***P<0.01.")  mtitles( "Base Model" "Levee Length" "Levee Indicator" "Hard Shoreline" "Beach Nourishment" "All Man-made Defensive Measures") coeflabel(right "Right" lnpropdam "log(damage)" lnwetland "log(wetland)" lnwind "log(wind)" lnstormarea "log(storm area)" lnriskproperty "log(property at risk)" lnlevee "log(levee length + 1)" lnbeach_nourishment "log(Beach Nourishment + 1)" 1.has_levee#c.lnwetland "Has Levee X log(wetland)" has_levee "Has Levee" hard_share "Share of Hard Shorelines") b(%9.4f) se(4) ar2(2) star(* 0.10 ** 0.05 *** 0.01) modelwidth(5) replace
eststo clear

/*Table S8. Robustness Checks: Property Value*/
eststo: quietly reghdfe lnpropdam lnwetland lnwind lnstormarea lnriskproperty right, vce(cluster hurricane_id county_id) absorb(year state_id)
eststo: quietly reghdfe lnpropdam lnwetland lnwind lnstormarea lnriskproperty right if (year!=2008 & year!=2009 & year!=2007), vce(cluster hurricane_id county_id) absorb(year state_id)
eststo: quietly reghdfe lnpropdam lnwetland lnwind lnstormarea lnriskproperty 1.bust#c.lnriskproperty right, vce(cluster hurricane_id county_id) absorb(year state_id)
eststo: quietly reghdfe lnpropdam lnwetland lnwind lnstormarea lnriskproperty_est right, vce(cluster hurricane_id county_id) absorb(year state_id)
esttab using"tableS8.rtf", title ("Table S8. Regression results for alternative specifications of the conditional damage model. Standard errors (in parentheses) are clustered two-ways at the county and storm level. *P<0.10, **P<0.05, ***P<0.01.")  mtitles("Base Model" "Drop Recession 2007-2009" "Housing Bust Indicator" "Newly Estimated Property Value") coeflabel(right "Right" lnpropdam "log(damage)" lnwetland "log(wetland)" lnwind "log(wind)" lnstormarea "log(storm area)" lnriskproperty "log(property at risk)" 1.bust#c.lnriskproperty "Bust X log(property at risk)") b(%9.4f) se(4) ar2(2) star(* 0.10 ** 0.05 *** 0.01) modelwidth(5) replace
eststo clear

/*************************************************************
Estimate Marginal Value of Wetland for each Coastal County
**************************************************************/
/*The code below assumes a gamma distribution. To use a Weibull or lognormal distribution instead, comment/uncomment the code where indicated*/
/*The annual distribution of wind speeds projected for each county from (20) is assumed to follow a gamma distribution, and we impose 152kt as the upper bound wind force.*/
*Please run all lines in quietly {} together (Line95-line180).

clear
set more off

quietly{
foreach i of num 1/239{
clear
cd $path2
 use mv_county_19_gamma,clear        //uncomment to assume a gamma distribution
*use mv_county_19_lognormal,clear   //uncomment to assume a lognormal distribution
*use mv_county_19_weibull,clear     //uncomment to assume a weibull distribution
keep if countyid==`i'
expand 10001
gen id=_n
range v 34 152 10000
replace v=0 if v==.
gen wetland=.
replace wetland=0 if v==0
replace wetland=C1_wet if v>82.99&v<83
replace wetland=C2_wet if v>95.99 & v<96
replace wetland=C3_wet if v>112.99 & v<113
replace wetland=C4_wet if v>136.98 & v<137
replace wetland=C5_wet if v>=137
ipolate wetland v, gen(w) 

gen stormarea=.
replace stormarea=0 if v==0
replace stormarea=c1stormarea if v>82.99&v<83
replace stormarea=c2stormarea if v>95.99 & v<96
replace stormarea=c3stormarea if v>112.99 & v<113
replace stormarea=c4stormarea if v>136.98 & v<137
replace stormarea=c5stormarea if v>=137
ipolate stormarea v, gen(s) 

gen property=.
replace property=0 if v==0
replace property=c1house if v>82.99&v<83
replace property=c2house if v>95.99 & v<96
replace property=c3house if v>112.99 & v<113
replace property=c4house if v>136.98 & v<137
replace property=c5house if v>=137
ipolate property v, gen(p) 

/*Coefficients Estimates for Conditional Damage Model*/
gen beta0=-20.73835
gen beta1= -.5755885
gen beta2=  7.18848
gen beta3= .4792971
gen beta4= .3205178
gen beta5= .8820942

/*Coefficients Estimates for Probit Model*/	   
gen gamma0= -2.421184
gen gamma1= -.0005378
gen gamma2=.0351113 
gen gamma3=.0006517 
gen gamma4= -8.43e-13
gen gamma5=.4831304

/*Estimated marginal value of coastal wetlands in each county following Eq.(6)*/
gen p_hat=normprob(gamma0+gamma1*w+gamma2*v+gamma3*s+gamma4*p+gamma5*right)
gen p_margin=gamma1*normalden(gamma0+gamma1*w+gamma2*v+gamma3*s+gamma4*p+gamma5*right)
gen D_hat=10.81*exp(beta0+beta1*ln(w)+beta2*ln(v)+beta3*ln(s)+beta4*ln(p)+beta5*right+yrFE+stateFE)
replace D_hat=c1house if D_hat>c1house & D_hat!=. & c1house>0 & c1house!=. &v<83
replace D_hat=c2house if D_hat>c2house & D_hat!=. & c2house>0 & c2house!=. &v>=83&v<96
replace D_hat=c3house if D_hat>c3house & D_hat!=. & c3house>0 & c3house!=. &v>=96&v<113
replace D_hat=c4house if D_hat>c4house & D_hat!=. & c4house>0 & c4house!=. &v>=113&v<137
replace D_hat=c5house if D_hat>c5house & D_hat!=. & c5house>0 & c5house!=. &v>=137
gen f_v=gammaden(alpha,beta,0,v)                                      //uncomment to assume a gamma distribution
*gen f_v=(1/(v*sd*sqrt(2*_pi)))*exp(-(ln(v)-mean)^2/(2*(sd^2)))       //uncomment to assume lognormal distribution
*gen f_v=(alpha/(beta^alpha))*(v^(alpha-1))*exp(-((v/beta)^alpha))    //uncomment to assume Weibull distribution
gen y=f_v*D_hat*((beta1/w)*p_hat+p_margin)
integ y v,gen(mv)
cd $path2/mv_county_gamma152                         //uncomment to assume a gamma distribution
*cd $path2/mv_county_lognormal152                    //uncomment to assume a lognormal distribution
*cd $path2/mv_county_Weibull152                      //uncomment to assume Weibull distribution
save mv_`i',replace
}

/*Append all coastal counties*/
clear
cd $path2/mv_county_gamma152                          //uncomment to assume a gamma distribution
*cd $path2/mv_county_lognormal152                    //uncomment to assume a lognormal distribution
*cd $path2/mv_county_Weibull152                      //uncomment to assume Weibull distribution

*ssc install fs
fs "*.dta"
append using `r(files)'
save mv_1_239, replace
}

/*Impose 152 kt as the upper bound wind force*/
clear
cd $path2/mv_county_gamma152                         //uncomment to assume a gamma distribution
*cd $path2/mv_county_lognormal152                    //uncomment to assume a lognormal distribution
*cd $path2/mv_county_Weibull152                      //uncomment to assume Weibull distribution
use mv_1_239,clear
keep if v==152
gen marginal=-mv
gen mv_thousand2016=marginal*(241.018/229.594)/1000
keep if C5_wet>0.2
keep sc_fips name_1 name_2 mv_thousand2016
gen mv_thousand2016_30year = mv_thousand2016 * (1/1.028) * (1-(1/1.028)^30)/(1-1/1.028)
gen mv_thousand2016_100year = mv_thousand2016 * (1/1.028) * (1-(1/1.028)^100)/(1-1/1.028)
sum mv_thousand2016,det
/*Export Marginal Value*/
cd $path2 
export delimited using "mv_county_gamma.csv", replace  //uncomment to assume a gamma distribution
* export delimited using "mv_county_lognormal.csv", replace  //uncomment to assume a lognormal distribution
* export delimited using "mv_county_weibull.csv", replace  //uncomment to assume a Weibull distribution
