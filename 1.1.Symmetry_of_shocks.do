// First Criterion: Symmetry of Shocks
clear

import 

/// INITIALIZATION OF THE DATA

// Here, I declare data to be time-series and the the unit is years.
tsset YEAR, yearly

///Log-Difference of the data
// Transformation of the time-series in logarithm
gen l_GDP_AB = log(GDP_AB)
gen l_GDP_BC = log(GDP_BC)
gen l_GDP_MB = log(GDP_MB)
gen l_GDP_NB = log(GDP_NB)
gen l_GDP_NL = log(GDP_NL)
gen l_GDP_NS = log(GDP_NS)
gen l_GDP_ON = log(GDP_ON)
gen l_GDP_PEI = log(GDP_PEI)
gen l_GDP_QC = log(GDP_QC)
gen l_GDP_SK = log(GDP_SK)
gen l_UR_AB = log(UR_AB)
gen l_UR_BC = log(UR_BC)
gen l_UR_MB = log(UR_MB)
gen l_UR_NB = log(UR_NB)
gen l_UR_NL = log(UR_NL)
gen l_UR_NS = log(UR_NS)
gen l_UR_ON = log(UR_ON)
gen l_UR_PEI = log(UR_PEI)
gen l_UR_QC = log(UR_QC)
gen l_UR_SK = log(UR_SK)
// Differenciation of the time-series
gen dl_GDP_AB = d.l_GDP_AB
gen dl_GDP_BC = d.l_GDP_BC
gen dl_GDP_MB = d.l_GDP_MB
gen dl_GDP_NB = d.l_GDP_NB
gen dl_GDP_NL = d.l_GDP_NL
gen dl_GDP_NS = d.l_GDP_NS
gen dl_GDP_ON = d.l_GDP_ON
gen dl_GDP_PEI = d.l_GDP_PEI
gen dl_GDP_QC = d.l_GDP_QC
gen dl_GDP_SK = d.l_GDP_SK
gen dl_UR_AB = d.l_UR_AB
gen dl_UR_BC = d.l_UR_BC
gen dl_UR_MB = d.l_UR_MB
gen dl_UR_NB = d.l_UR_NB
gen dl_UR_NL = d.l_UR_NL
gen dl_UR_NS = d.l_UR_NS
gen dl_UR_ON = d.l_UR_ON
gen dl_UR_PEI = d.l_UR_PEI
gen dl_UR_QC = d.l_UR_QC
gen dl_UR_SK = d.l_UR_SK

///Insure Stationarity of the data
dfuller dl_GDP_AB 
dfuller dl_GDP_BC 
dfuller dl_GDP_MB 
dfuller dl_GDP_NB 
dfuller dl_GDP_NL 
dfuller dl_GDP_NS 
dfuller dl_GDP_ON 
dfuller dl_GDP_PEI
dfuller dl_GDP_QC 
dfuller dl_GDP_SK 
dfuller dl_UR_AB 
dfuller dl_UR_BC 
dfuller dl_UR_MB 
dfuller dl_UR_NB 
dfuller dl_UR_NL 
dfuller dl_UR_NS 
dfuller dl_UR_ON 
dfuller dl_UR_PEI
dfuller dl_UR_QC 
dfuller dl_UR_SK 



////GRAPH PART irf descriv
tsline
graph combine mygraph1.gph mygraph2.gph mygraph3.gph mygraph4.gph mygraph5.gph

////SVAR PART

 SVAR Alberta
matrix lr_AB = (.,0\., .)
matrix list lr_AB
varsoc dl_GDP_AB dl_UR_AB, maxlag(30)
Optimal determine lag = 2
varwle 
 All the eigenvalues lie inside the unit circle. SVAR satisfies stability condition.

 The long-run structural VAR is estimated with svar using the lreq() option.
 We place the GDP first in the ordering.
 Then, the indentifying restriction is that the long-run GDP response to the 
 UR shock is zero, which leads us to the restriction C = = (.,0\., .)
 In this matrix, three entries are free and the remaining entry is forced to 0.
svar dl_GDP_AB dl_UR_AB, lags(1/2) lreq(lr_AB)
 Below we save the impulse responses
irf create AB, set(AB.irf, replace) step(20)
 Below, impulse-responses to each shock under the long-run identification scheme
 are held in irf and are graphed
irf graph sirf, xlabel(0(5) 10) irf(AB) yline(0,lcolor(black)) byopts(yrescale)
 GDP is ordered first, the growth impulse here is the supply shock.
 the UR shock is the demand shock.
Les deux graphs du haut representent les reponses à un supply shock (GDP shock)
 Les deux graphs du bas representent les reponses à un demand shock (UR shock)
irf table fevd, impulse(dl_GDP_AB)
irf table fevd, impulse(dl_UR_AB)

//irf graph irf table

//quietly correlate 
 matrix C = r(C)
heatplot C, values(format(%9.3f)) color(hcl diverging, intensity(.6)) legend(off) aspectratio(1)lower nodiagonal



 SVAR British Columbia
matrix lr_BC = (.,0\., .)
matrix list lr_BC
varsoc dl_GDP_BC dl_UR_BC, maxlag(30)
Optimal determine lag = 2
varwle 
varstable
 All the eigenvalues lie inside the unit circle. SVAR satisfies stability condition.
svar dl_GDP_BC dl_UR_BC, lags(1/2) lreq(lr_BC)
irf create BC, set(BC.irf, replace) step(20)
/irf graph sirf, xlabel(0(5) 10) irf(BC) yline(0,lcolor(black)) byopts(yrescale)
irf table sirf, impulse(dl_GDP_BC) 
irf table sirf, impulse(dl_UR_BC)

 SVAR Quebec
matrix lr_QC = (.,0\., .)
matrix list lr_QC
varsoc dl_GDP_QC dl_UR_QC, maxlag(30)
Optimal determine lag = 2
varwle 
varstable
 All the eigenvalues lie inside the unit circle. SVAR satisfies stability condition.
svar dl_GDP_QC dl_UR_QC, lags(1/2) lreq(lr_QC)
irf create QC, set(QC.irf, replace) step(20)
irf graph sirf, xlabel(0(5) 20) irf(QC) yline(0,lcolor(black)) byopts(yrescale)
irf table sirf, impulse(dl_GDP_QC)
irf table sirf, impulse(dl_UR_QC)


 SVAR Saskatchewan
matrix lr_SK = (.,0\., .)
matrix list lr_SK
varsoc dl_GDP_SK dl_UR_SK, maxlag(30)
Optimal determine lag = 2
varwle 
varstable
 All the eigenvalues lie inside the unit circle. SVAR satisfies stability condition.
svar dl_GDP_SK dl_UR_SK, lags(1/2) lreq(lr_SK)
irf create SK, set(SK.irf, replace) step(20)
irf graph sirf, xlabel(0(5) 20) irf(SK) yline(0,lcolor(black)) byopts(yrescale)
irf table sirf, impulse(dl_GDP_SK)
irf table sirf, impulse(dl_UR_SK)

 SVAR Prince Edward Island
matrix lr_PEI = (.,0\., .)
matrix list lr_PEI
varsoc dl_GDP_PEI dl_UR_PEI, maxlag(30)
Optimal determine lag = 2
varwle 
varstable
 All the eigenvalues lie inside the unit circle. SVAR satisfies stability condition.
svar dl_GDP_PEI dl_UR_PEI, lags(1/2) lreq(lr_PEI)
irf create PEI, set(PEI.irf, replace) step(20)
irf graph sirf, xlabel(0(5) 20) irf(PEI) yline(0,lcolor(black)) byopts(yrescale)
irf table sirf, impulse(dl_GDP_PEI)
irf table sirf, impulse(dl_UR_PEI)

 SVAR New Labrador
/matrix lr_NL = (.,0\., .)
matrix list lr_NL
varsoc dl_GDP_NL dl_UR_NL, maxlag(30)
Optimal determine lag = 2
varwle 
varstable
 All the eigenvalues lie inside the unit circle. SVAR satisfies stability condition.
svar dl_GDP_NL dl_UR_NL, lags(1/2) lreq(lr_NL)
irf create NL, set(NL.irf, replace) step(10)
irf graph sirf, xlabel(0(5) 10) irf(NL) yline(0,lcolor(black)) byopts(yrescale)
irf table sirf, impulse(dl_GDP_NL)
irf table sirf, impulse(dl_UR_NL)

 SVAR New Brunswick
matrix lr_NB = (.,0\., .)
matrix list lr_NB
varsoc dl_GDP_NB dl_UR_NB, maxlag(30)
Optimal determine lag = 2
varwle 
varstable
 All the eigenvalues lie inside the unit circle. SVAR satisfies stability condition.
svar dl_GDP_NB dl_UR_NB, lags(1/2) lreq(lr_NB)
irf create NB, set(NB.irf, replace) step(10)
irf graph sirf, xlabel(0(5) 10) irf(NB) yline(0,lcolor(black)) byopts(yrescale)
irf table sirf, impulse(dl_GDP_NB)
irf table sirf, impulse(dl_UR_NB)

 SVAR New Scotia
matrix lr_NS = (.,0\., .)
matrix list lr_NS
varsoc dl_GDP_NS dl_UR_NS, maxlag(30)
Optimal determine lag = 2
varwle 
varstable
 All the eigenvalues lie inside the unit circle. SVAR satisfies stability condition.
svar dl_GDP_NS dl_UR_NS, lags(1/2) lreq(lr_NS)
irf create NS, set(NS.irf, replace) step(10)
irf graph sirf, xlabel(0(5) 10) irf(NS) yline(0,lcolor(black)) byopts(yrescale)
rf table sirf, impulse(dl_GDP_NS)
irf table sirf, impulse(dl_UR_NS)

 SVAR Manitoba
matrix lr_MB = (.,0\., .)
matrix list lr_MB
varsoc dl_GDP_MB dl_UR_MB, maxlag(30)
Optimal determine lag = 2
/varwle 
varstable
 All the eigenvalues lie iMBide the unit circle. SVAR satisfies stability condition.
svar dl_GDP_MB dl_UR_MB, lags(1/2) lreq(lr_MB)
irf create MB, set(MB.irf, replace) step(10)
irf graph sirf, xlabel(0(5) 10) irf(MB) yline(0,lcolor(black)) byopts(yrescale)
irf table sirf, impulse(dl_GDP_MB)
irf table sirf, impulse(dl_UR_MB)

 SVAR Ontario
matrix lr_ON = (.,0\., .)
matrix list lr_ON
varsoc dl_GDP_ON dl_UR_ON, maxlag(30)
Optimal determine lag = 2
varwle 
varstable
 All the eigenvalues lie iONide the unit circle. SVAR satisfies stability condition.
svar dl_GDP_ON dl_UR_ON, lags(1/2) lreq(lr_ON)
irf create ON, set(ON.irf, replace) step(10)
irf graph sirf, xlabel(0(5) 10) irf(ON) yline(0,lcolor(black)) byopts(yrescale)
irf table sirf, impulse(dl_GDP_ON)
irf table sirf, impulse(dl_UR_ON)

//EXPERIMENT HEATPLOT
quietly pwcorr  D_AB D_BC D_MB D_NB D_NL D_NS D_ON D_PEI D_QC D_SK, star(0.1)
quietly pwcorr  S_AB S_BC S_MB S_NB S_NL S_NS S_ON S_PEI S_QC S_SK 
matrix C = r(C)
heatplot C, values(format(%9.3f)) color(hcl diverging, intensity(.6)) legend(off) aspectratio(1)lower nodiagonal
asdoc pwcorr  S_AB S_BC S_MB S_NB S_NL S_NS S_ON S_PEI S_QC S_SK, star(0.1)
pwcorr  D_AB D_BC D_MB D_NB D_NL D_NS D_ON D_PEI D_QC D_SK, star(0.1)
