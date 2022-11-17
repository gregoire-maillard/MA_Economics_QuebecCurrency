// Second Criterion: Labour Mobility

clear

import 


// Here, I declare data to be time-series and the the unit is years.
tsset YEAR, yearly

//Initialization

gen GDPCAP_CAN = GDP_Canada/POP_Canada
gen GDPCAP_AB = GDP_Alberta/POP_Alberta
gen GDPCAP_BC = GDP_BritishColumbia/POP_BritishColumbia
gen GDPCAP_ON = GDP_Ontario/POP_Ontario
gen GDPCAP_QC = GDP_Quebec/POP_Quebec

gen relativewage_AB = log(GDPCAP_AB/GDPCAP_CAN)
gen relativewage_BC = log(GDPCAP_BC/GDPCAP_CAN)
gen relativewage_ON = log(GDPCAP_ON/GDPCAP_CAN)
gen relativewage_QC = log(GDPCAP_QC/GDPCAP_CAN)


gen URratio_AB = UR_Alberta/UR_Canada
gen URratio_BC = UR_BritishColumbia/UR_Canada
gen URratio_ON = UR_Ontario/UR_Canada
gen URratio_QC = UR_Quebec/UR_Canada

gen shareNM_AB = NM_Alberta*100/realpop_Canada
gen shareNM_BC = NM_BritishColumbia*100/realpop_Canada
gen shareNM_ON = NM_Ontario*100/realpop_Canada
gen shareNM_QC = NM_Quebec*100/realpop_Canada

//Regression

reg shareNM_AB wageAB URratio_AB



reg shareNM_BC wageBC URratio_BC

reg shareNM_ON wageON URratio_ON

reg shareNM_QC wageQC URratio_QC
