*Elliot Tu 
*Applied Econometric Analysis (ECON216)
*Assignment 7

*cd "C:\Users\eugen\Desktop\Test"

/*
TO MAKE THIS FILE REPLICABLE: 
1. set your directory using the above command cd "[insert your file path here]" 
2. download the datasets and add them to the file path you chose.
3. Install (command ssc [package]) the packages [asdoc] [outreg2] 
After completing the above steps, you can run this do file in one go. 

See pdf in repository for regression tables and the evaluation of results. 
*/

*Use a dataset of employees
use union.dta, clear

*Comparing average age, experience, and tenure of employees based on whether or not they're in a union or not in a union. 
tab union 
asdoc sum age exper tenure if union == 1, save(hw7q1a) title(Union Members) replace
asdoc sum age exper tenure if union == 0, save(hw7q1a2) title(Non-Union Members) replace

*Estimating the effect of union status on annual earnings whilst controlling for experience.
regress annearn union exper age tenure educ
outreg2 using hw7q1b.xls, replace sdec(3) bdec(3) title("Effects on Annual Earnings")

*Search for outliers (top 3 annual earnings) and determine union status
sum annearn, detail
tab annearn if annearn >= 503500 & union == 1
*0 observations, none are in unions
tab annearn if annearn >= 503500 & union == 0
*3 observations, all not in unions
asdoc tab annearn if annearn >= 503500 & union == 0, save(hw7q1c) title(Top 3 Annual Earnings) replace
sum annearn if union == 0 
*Average salary for non-union members is 17063.99

*Trim the data at the 5% level to remove outliers
sum annearn, detail
return list
gen Trim_annearn = annearn if annearn >=r(p5) & annearn <=r(p95)
label variable Trim_annearn "Trimmed at 5% level"
regress Trim_annearn union exper age tenure educ
outreg2 using hw7q1d.xls, replace sdec(3) bdec(3) title("Trimmed at 5% Effects on Annual Earnings")

*Winsorize data at the 5% level
gen Wins_annearn = annearn
label variable Wins_annearn "Winsorized annual earnings"
sum annearn, detail
replace Wins_annearn = r(p95) if Wins_annearn > r(p95)
replace Wins_annearn = r(p5) if Wins_annearn < r(p5)
regress Wins_annearn union exper age tenure educ
outreg2 using hw7q1e.xls, replace sdec(3) bdec(3) title("Winsorized Effects on Annual Earnings")

*Estimating the effect of union membership and years of experience on annual earnings. Include the SSR stat
regress annearn union exper
outreg2 using hw7q2a.xls, replace sdec(3) bdec(3) title("Effects on Annual Earnings") addstat("SSR", e(rss))

*Estimate the effect of union membership and years of experience on the natural log of annual earnings. Include the SSR stat 
gen ln_annearn = ln(annearn)
label variable ln_annearn "ln of annual earnings"
regress ln_annearn union exper
outreg2 using hw7q2b.xls, replace sdec(3) bdec(3) title("Effects on Natural Log of Annual Earnings") addstat("SSR", e(rss))

*Perform a box cox transformation to compare the above models. Use chi-squared test to evaluate which test is preferred. 
*Box Cox Transformation
tab annearn 
gen observ = r(N)
ameans annearn
scalar gmean = r(mean_g)
gen T_annearn = (annearn/gmean)
tab ln_annearn
ameans ln_annearn
scalar gmean2 = r(mean_g)
gen T_ln_annearn = (ln_annearn/gmean2)
*Regressions
regress T_annearn union exper
outreg2 using hw7q2c.xls, replace sdec(3) bdec(3) ctitle(Annual Earnings) title("Box-Cox Regression") addstat("SSR", e(rss))
scalar RSS2 = e(rss)
regress T_ln_annearn union exper
outreg2 using hw7q2c.xls, append sdec(3) bdec(3) ctitle(Ln Annual Earnings) title("Box-Cox Regression") addstat("SSR", e(rss))
scalar RSS1 = e(rss)
*Chi-squared Calculation
gen Chi2 = ((.5*observ)*ln(RSS2/RSS1))
display Chi2

*Use a dataset of fertility
clear all 
use fertility.dta

*Estimate the effect of age, education, and whether or not someone has been married on the number of children had
regress ceb age educ evermarr
outreg2 using hw7q3.xls, replace sdec(3) bdec(3) ctitle(Standard) addstat("SSR", e(rss))

*We are concerned that the included variables are not sufficient. Conduct a Ramey Reset Test to determine if the above regression suffers from misspecification.
predict Yhat
gen Yhat_2 = (Yhat^2)
gen Yhat_3 = (Yhat^3)
regress ceb age educ evermarr Yhat_2 Yhat_3
outreg2 using hw7q3.xls, append sdec(3) bdec(3) ctitle(Unrestricted) addstat("SSR", e(rss))
