library(urca)
library(vars)
library(mFilter)
library(tseries)
library(TSstudio)
library(forecast)
library(tidyverse)
library(readxl)

#Loading the Dataset
macro <- read_excel(file.choose())

#Setting the Time Series Variables
#Log-Diff. Output for each Province
y_NL  <- ts(macro$dl_GDP_NL, start = c(1986,1,1), frequency = 1)
y_PEI <- ts(macro$dl_GDP_PEI, start = c(1986,1,1), frequency = 1)
y_NS  <- ts(macro$dl_GDP_NS, start = c(1986,1,1), frequency = 1)
y_NB  <- ts(macro$dl_GDP_NB, start = c(1986,1,1), frequency = 1)
y_QC  <- ts(macro$dl_GDP_QC, start = c(1986,1,1), frequency = 1)
y_ON  <- ts(macro$dl_GDP_ON, start = c(1986,1,1), frequency = 1)
y_MB  <- ts(macro$dl_GDP_MB, start = c(1986,1,1), frequency = 1)
y_SK  <- ts(macro$dl_GDP_SK, start = c(1986,1,1), frequency = 1)
y_AB  <- ts(macro$dl_GDP_AB, start = c(1986,1,1), frequency = 1)
y_BC  <- ts(macro$dl_GDP_BC, start = c(1986,1,1), frequency = 1)
#Log-Diff. Unemployment Rate for each Province
u_NL  <- ts(macro$dl_UR_NL, start = c(1986,1,1), frequency = 1)
u_PEI <- ts(macro$dl_UR_PEI, start = c(1986,1,1), frequency = 1)
u_NS  <- ts(macro$dl_UR_NS, start = c(1986,1,1), frequency = 1)
u_NB  <- ts(macro$dl_UR_NB, start = c(1986,1,1), frequency = 1)
u_QC  <- ts(macro$dl_UR_QC, start = c(1986,1,1), frequency = 1)
u_ON  <- ts(macro$dl_UR_ON, start = c(1986,1,1), frequency = 1)
u_MB  <- ts(macro$dl_UR_MB, start = c(1986,1,1), frequency = 1)
u_SK  <- ts(macro$dl_UR_SK, start = c(1986,1,1), frequency = 1)
u_AB  <- ts(macro$dl_UR_AB, start = c(1986,1,1), frequency = 1)
u_BC  <- ts(macro$dl_UR_BC, start = c(1986,1,1), frequency = 1)

#Time Series Plots
y_data <- cbind(y_AB,y_BC,y_MB,y_NB,y_NL,y_NS,y_ON,y_PEI,y_QC,y_SK)
colnames(y_data) <- c("AB","BC","MB","NB","NL","NS","ON","PEI","QC","SK")
plot.ts((y_data), xlab="Year", main = "Change in Growth Rate across Canadian Provinces over Time", asp=40)
u_data <- cbind(u_AB,u_BC,u_MB,u_NB,u_NL,u_NS,u_ON,u_PEI,u_QC,u_SK)
colnames(u_data) <- c("AB","BC","MB","NB","NL","NS","ON","PEI","QC","SK")
plot.ts((u_data), xlab="Year", main = "Change in Unemployment Rate across Canadian Provinces over Time", asp=10)

#Setting the Restrictions
amat <- diag(2)
amat[1,1] <- NA
amat[2,1] <- NA
amat[2,2] <- NA
amat[1,2] <- 0
amat

#SVAR_Alberta
svAB <- cbind(y_AB, u_AB)
colnames(svAB) <- cbind("OutputGap", "UnemploymentRate")
lagselect <- VARselect(svAB, lag.max = 30, type = "both")
#lagselect$selection
#lagselect$criteria
ModelAB <- VAR(svAB, p = 2, season = NULL, exog = NULL, type = "const", ic = c("AIC", "HQ", "SC", "FPE"))
SVAR_AB <- BQ(ModelAB)
##SVARfevd_AB <- fevd(SVAR_AB, n.ahead = 10)
##plot(#SVARfevd_AB)
irf.dytAB <- irf(SVAR_AB, impulse = "OutputGap", boot = FALSE, n.ahead = 10)
irf.untAB <- irf(SVAR_AB, impulse = "UnemploymentRate", boot = FALSE, n.ahead = 10)
supplyAB <- cbind(irf.dytAB$irf$OutputGap[, 1],1 * irf.untAB$irf$UnemploymentRate[, 1])
demandAB <- cbind( irf.dytAB$irf$OutputGap[, 2], 1 *irf.untAB$irf$UnemploymentRate[, 2])
#Demand Shock plot
plot.ts(demandAB[, 1], col = rgb(0.06, 0.31, 0.55), lwd = 2, ylab = "", xlab = "", main = "Demand Shock for Alberta", xlim = c(0, 10), ylim = c(-0.12,0.1))
abline(h = 0)
lines(demandAB[, 2], col = rgb(0.8, 0.2, 0.2), lwd = 2)
legend(x = "bottomright", c("Output response", "Unemployment response"), col = c(rgb(0.8, 0.2, 0.2),rgb(0.06, 0.31, 0.55)), lwd = 1, bty = "n")
#Supply Shock plot
plot.ts(supplyAB[, 2], col = rgb(0.06, 0.31, 0.55), lwd = 2, ylab = "", xlab = "", main = "Supply Shock for Alberta", xlim = c(0, 10), ylim = c(-0.01, 0.04))
abline(h = 0)
lines(supplyAB[, 1], col = rgb(0.8, 0.2, 0.2), lwd = 2)
legend(x = "topright", c("Output response", "Unemployment response"), col = c(rgb(0.8, 0.2, 0.2), rgb(0.06, 0.31, 0.55)), lwd = 2, bty = "n")


#SVAR_ABritish Columbia
svBC <- cbind(y_BC, u_BC)
colnames(svBC) <- cbind("OutputGap", "UnemploymentRate")
lagselect <- VARselect(svBC, lag.max = 30, type = "both")
lagselect$selection
lagselect$criteria
ModelBC <- VAR(svBC, p = 2, season = NULL, exog = NULL, type = "const", ic = c("AIC", "HQ", "SC", "FPE"))
SVAR_BC <- BQ(ModelBC)
#SVARfevd_BC <- fevd(SVAR_BC, n.ahead = 10)
##plot(#SVARfevd_BC)
irf.dytBC <- irf(SVAR_BC, impulse = "OutputGap", boot = FALSE, n.ahead = 10)
irf.untBC <- irf(SVAR_BC, impulse = "UnemploymentRate", boot = FALSE, n.ahead = 10)
supplyBC <- cbind(irf.dytBC$irf$OutputGap[, 1],1 * irf.untBC$irf$UnemploymentRate[, 1])
demandBC <- cbind( irf.dytBC$irf$OutputGap[, 2], 1 *irf.untBC$irf$UnemploymentRate[, 2])
#Demand Shock plot
plot.ts(demandBC[, 1], col = rgb(0.06, 0.31, 0.55), lwd = 2, ylab = "", xlab = "", main = "Demand Shock for British Columbia", xlim = c(0, 10), ylim = c(-0.12,0.1))
abline(h = 0)
lines(demandBC[, 2], col = rgb(0.8, 0.2, 0.2), lwd = 2)
legend(x = "bottomright", c("Output response", "Unemployment response"), col = c(rgb(0.8, 0.2, 0.2),rgb(0.06, 0.31, 0.55)), lwd = 1, bty = "n")
#Supply Shock plot
plot.ts(supplyBC[, 2], col = rgb(0.06, 0.31, 0.55), lwd = 2, ylab = "", xlab = "", main = "Supply Shock for British Columbia", xlim = c(0, 10), ylim = c(-0.015, 0.02))
abline(h = 0)
lines(supplyBC[, 1], col = rgb(0.8, 0.2, 0.2), lwd = 2)
legend(x = "bottomright", c("Output response", "Unemployment response"), col = c(rgb(0.8, 0.2, 0.2), rgb(0.06, 0.31, 0.55)), lwd = 2, bty = "n")

#SVAR_AManitoba
svMB <- cbind(y_MB, u_MB)
colnames(svMB) <- cbind("OutputGap", "UnemploymentRate")
lagselect <- VARselect(svMB, lag.max = 30, type = "both")
lagselect$selection
lagselect$criteria
ModelMB <- VAR(svMB, p = 2, season = NULL, exog = NULL, type = "const", ic = c("AIC", "HQ", "SC", "FPE"))
SVAR_MB <- BQ(ModelMB)
#SVARfevd_MB <- fevd(SVAR_MB, n.ahead = 10)
##plot(#SVARfevd_MB)
irf.dytMB <- irf(SVAR_MB, impulse = "OutputGap", boot = FALSE, n.ahead = 10)
irf.untMB <- irf(SVAR_MB, impulse = "UnemploymentRate", boot = FALSE, n.ahead = 10)
supplyMB <- cbind(irf.dytMB$irf$OutputGap[, 1],1 * irf.untMB$irf$UnemploymentRate[, 1])
demandMB <- cbind( irf.dytMB$irf$OutputGap[, 2], 1 *irf.untMB$irf$UnemploymentRate[, 2])
#Demand Shock plot
plot.ts(demandMB[, 1], col = rgb(0.06, 0.31, 0.55), lwd = 2, ylab = "", xlab = "", main = "Demand Shock for Manitoba", xlim = c(0, 10), ylim = c(-0.12,0.1))
abline(h = 0)
lines(demandMB[, 2], col = rgb(0.8, 0.2, 0.2), lwd = 2)
legend(x = "bottomright", c("Output response", "Unemployment response"), col = c(rgb(0.8, 0.2, 0.2),rgb(0.06, 0.31, 0.55)), lwd = 1, bty = "n")
#Supply Shock plot
plot.ts(supplyMB[, 2], col = rgb(0.06, 0.31, 0.55), lwd = 2, ylab = "", xlab = "", main = "Supply Shock for Manitoba", xlim = c(0, 10), ylim = c(-0.01, 0.02))
abline(h = 0)
lines(supplyMB[, 1], col = rgb(0.8, 0.2, 0.2), lwd = 2)
legend(x = "topright", c("Output response", "Unemployment response"), col = c(rgb(0.8, 0.2, 0.2), rgb(0.06, 0.31, 0.55)), lwd = 2, bty = "n")

#SVAR_ANew Brunswick
svNB <- cbind(y_NB, u_NB)
colnames(svNB) <- cbind("OutputGap", "UnemploymentRate")
lagselect <- VARselect(svNB, lag.max = 30, type = "both")
lagselect$selection
lagselect$criteria
ModelNB <- VAR(svNB, p = 2, season = NULL, exog = NULL, type = "const", ic = c("AIC", "HQ", "SC", "FPE"))
SVAR_NB <- BQ(ModelNB)
#SVARfevd_NB <- fevd(SVAR_NB, n.ahead = 10)
##plot(#SVARfevd_NB)
irf.dytNB <- irf(SVAR_NB, impulse = "OutputGap", boot = FALSE, n.ahead = 10)
irf.untNB <- irf(SVAR_NB, impulse = "UnemploymentRate", boot = FALSE, n.ahead = 10)
supplyNB <- cbind(irf.dytNB$irf$OutputGap[, 1],1 * irf.untNB$irf$UnemploymentRate[, 1])
demandNB <- cbind( irf.dytNB$irf$OutputGap[, 2],  irf.untNB$irf$UnemploymentRate[, 2])
#Demand Shock plot
plot.ts(demandNB[, 1], col = rgb(0.06, 0.31, 0.55), lwd = 2, ylab = "", xlab = "", main = "Demand Shock for New Brunswick", xlim = c(0, 10), ylim = c(-0.04,0.05))
abline(h = 0)
lines(demandNB[, 2], col = rgb(0.8, 0.2, 0.2), lwd = 2)
legend(x = "bottomright", c("Output response", "Unemployment response"), col = c(rgb(0.8, 0.2, 0.2),rgb(0.06, 0.31, 0.55)), lwd = 1, bty = "n")
#Supply Shock plot
plot.ts(supplyNB[, 2], col = rgb(0.06, 0.31, 0.55), lwd = 2, ylab = "", xlab = "", main = "Supply Shock for New Brunswick", xlim = c(0, 10), ylim = c(-0.01, 0.018))
abline(h = 0)
lines(supplyNB[, 1], col = rgb(0.8, 0.2, 0.2), lwd = 2)
legend(x = "topright", c("Output response", "Unemployment response"), col = c(rgb(0.8, 0.2, 0.2), rgb(0.06, 0.31, 0.55)), lwd = 2, bty = "n")

#SVAR_ANova Scotia
svNS <- cbind(y_NS, u_NS)
colnames(svNS) <- cbind("OutputGap", "UnemploymentRate")
lagselect <- VARselect(svNS, lag.max = 30, type = "both")
lagselect$selection
lagselect$criteria
ModelNS <- VAR(svNS, p = 2, season = NULL, exog = NULL, type = "const", ic = c("AIC", "HQ", "SC", "FPE"))
SVAR_NS <- BQ(ModelNS)
#SVARfevd_NS <- fevd(SVAR_NS, n.ahead = 10)
##plot(#SVARfevd_NS)
irf.dytNS <- irf(SVAR_NS, impulse = "OutputGap", boot = FALSE, n.ahead = 10)
irf.untNS <- irf(SVAR_NS, impulse = "UnemploymentRate", boot = FALSE, n.ahead = 10)
supplyNS <- cbind(irf.dytNS$irf$OutputGap[, 1],1 * irf.untNS$irf$UnemploymentRate[, 1])
demandNS <- cbind( irf.dytNS$irf$OutputGap[, 2], 1 *irf.untNS$irf$UnemploymentRate[, 2])
#Demand Shock plot
plot.ts(demandNS[, 1], col = rgb(0.06, 0.31, 0.55), lwd = 2, ylab = "", xlab = "", main = "Demand Shock for Nova Scotia", xlim = c(0, 10), ylim = c(-0.05,0.08))
abline(h = 0)
lines(demandNS[, 2], col = rgb(0.8, 0.2, 0.2), lwd = 2)
legend(x = "topright", c("Output response", "Unemployment response"), col = c(rgb(0.8, 0.2, 0.2),rgb(0.06, 0.31, 0.55)), lwd = 1, bty = "n")
#Supply Shock plot
plot.ts(supplyNS[, 2], col = rgb(0.06, 0.31, 0.55), lwd = 2, ylab = "", xlab = "", main = "Supply Shock for Nova Scotia", xlim = c(0, 10), ylim = c(-0.002, 0.01))
abline(h = 0)
lines(supplyNS[, 1], col = rgb(0.8, 0.2, 0.2), lwd = 2)
legend(x = "topright", c("Output response", "Unemployment response"), col = c(rgb(0.8, 0.2, 0.2), rgb(0.06, 0.31, 0.55)), lwd = 2, bty = "n")


#SVAR_AOntario
svON <- cbind(y_ON, u_ON)
colnames(svON) <- cbind("OutputGap", "UnemploymentRate")
lagselect <- VARselect(svON, lag.max = 30, type = "both")
lagselect$selection
lagselect$criteria
ModelON <- VAR(svON, p = 2, season = NULL, exog = NULL, type = "const", ic = c("AIC", "HQ", "SC", "FPE"))
SVAR_ON <- BQ(ModelON)
#SVARfevd_ON <- fevd(SVAR_ON, n.ahead = 10)
##plot(#SVARfevd_ON)
irf.dytON <- irf(SVAR_ON, impulse = "OutputGap", boot = FALSE, n.ahead = 10)
irf.untON <- irf(SVAR_ON, impulse = "UnemploymentRate", boot = FALSE, n.ahead = 10)
supplyON <- cbind(irf.dytON$irf$OutputGap[, 1],1 * irf.untON$irf$UnemploymentRate[, 1])
demandON <- cbind( irf.dytON$irf$OutputGap[, 2], 1 *irf.untON$irf$UnemploymentRate[, 2])
#Demand Shock plot
plot.ts(demandON[, 1], col = rgb(0.06, 0.31, 0.55), lwd = 2, ylab = "", xlab = "", main = "Demand Shock for Ontario", xlim = c(0, 10), ylim = c(-0.12,0.1))
abline(h = 0)
lines(demandON[, 2], col = rgb(0.8, 0.2, 0.2), lwd = 2)
legend(x = "bottomright", c("Output response", "Unemployment response"), col = c(rgb(0.8, 0.2, 0.2),rgb(0.06, 0.31, 0.55)), lwd = 1, bty = "n")
#Supply Shock plot
plot.ts(supplyON[, 2], col = rgb(0.06, 0.31, 0.55), lwd = 2, ylab = "", xlab = "", main = "Supply Shock for Ontario", xlim = c(0, 10), ylim = c(-0.01, 0.03))
abline(h = 0)
lines(supplyON[, 1], col = rgb(0.8, 0.2, 0.2), lwd = 2)
legend(x = "topright", c("Output response", "Unemployment response"), col = c(rgb(0.8, 0.2, 0.2), rgb(0.06, 0.31, 0.55)), lwd = 2, bty = "n")


#SVAR_APrinceEdwardIsland
svPEI <- cbind(y_PEI, u_PEI)
colnames(svPEI) <- cbind("OutputGap", "UnemploymentRate")
lagselect <- VARselect(svPEI, lag.max = 30, type = "both")
lagselect$selection
lagselect$criteria
ModelPEI <- VAR(svPEI, p = 2, season = NULL, exog = NULL, type = "const", ic = c("AIC", "HQ", "SC", "FPE"))
SVAR_PEI <- BQ(ModelPEI)
#SVARfevd_PEI <- fevd(SVAR_PEI, n.ahead = 10)
##plot(#SVARfevd_PEI)
irf.dytPEI <- irf(SVAR_PEI, impulse = "OutputGap", boot = FALSE, n.ahead = 10)
irf.untPEI <- irf(SVAR_PEI, impulse = "UnemploymentRate", boot = FALSE, n.ahead = 10)
supplyPEI <- cbind(irf.dytPEI$irf$OutputGap[, 1],1 * irf.untPEI$irf$UnemploymentRate[, 1])
demandPEI <- cbind( irf.dytPEI$irf$OutputGap[, 2], 1 *irf.untPEI$irf$UnemploymentRate[, 2])
#Demand Shock plot
plot.ts(demandPEI[, 1], col = rgb(0.06, 0.31, 0.55), lwd = 2, ylab = "", xlab = "", main = "Demand Shock for PrinceEdwardIsland", xlim = c(0, 10), ylim = c(-0.05,0.05))
abline(h = 0)
lines(demandPEI[, 2], col = rgb(0.8, 0.2, 0.2), lwd = 2)
legend(x = "bottomright", c("Output response", "Unemployment response"), col = c(rgb(0.8, 0.2, 0.2),rgb(0.06, 0.31, 0.55)), lwd = 1, bty = "n")
#Supply Shock plot
plot.ts(supplyPEI[, 2], col = rgb(0.06, 0.31, 0.55), lwd = 2, ylab = "", xlab = "", main = "Supply Shock for PrinceEdwardIsland", xlim = c(0, 10), ylim = c(-0.005, 0.02))
abline(h = 0)
lines(supplyPEI[, 1], col = rgb(0.8, 0.2, 0.2), lwd = 2)
legend(x = "topright", c("Output response", "Unemployment response"), col = c(rgb(0.8, 0.2, 0.2), rgb(0.06, 0.31, 0.55)), lwd = 2, bty = "n")


#SVAR_AQuebec
svQC <- cbind(y_QC, u_QC)
colnames(svQC) <- cbind("OutputGap", "UnemploymentRate")
lagselect <- VARselect(svQC, lag.max = 30, type = "both")
lagselect$selection
lagselect$criteria
ModelQC <- VAR(svQC, p = 2, season = NULL, exog = NULL, type = "const", ic = c("AIC", "HQ", "SC", "FPE"))
SVAR_QC <- BQ(ModelQC)
#SVARfevd_QC <- fevd(SVAR_QC, n.ahead = 10)
##plot(#SVARfevd_QC)
irf.dytQC <- irf(SVAR_QC, impulse = "OutputGap", boot = FALSE, n.ahead = 10)
irf.untQC <- irf(SVAR_QC, impulse = "UnemploymentRate", boot = FALSE, n.ahead = 10)
supplyQC <- cbind(irf.dytQC$irf$OutputGap[, 1],1 * irf.untQC$irf$UnemploymentRate[, 1])
demandQC <- cbind(irf.dytQC$irf$OutputGap[, 2], 1 *irf.untQC$irf$UnemploymentRate[, 2])
#Demand Shock plot
plot.ts(demandQC[, 1], col = rgb(0.06, 0.31, 0.55), lwd = 2, ylab = "", xlab = "", main = "Demand Shock for Quebec", xlim = c(0, 10), ylim = c(-0.12,0.1))
abline(h = 0)
lines(demandQC[, 2], col = rgb(0.8, 0.2, 0.2), lwd = 2)
legend(x = "bottomright", c("Output response", "Unemployment response"), col = c(rgb(0.8, 0.2, 0.2),rgb(0.06, 0.31, 0.55)), lwd = 1, bty = "n")
#Supply Shock plot
plot.ts(supplyQC[, 2], col = rgb(0.06, 0.31, 0.55), lwd = 2, ylab = "", xlab = "", main = "Supply Shock for Quebec", xlim = c(0, 10), ylim = c(-0.01, 0.02))
abline(h = 0)
lines(supplyQC[, 1], col = rgb(0.8, 0.2, 0.2), lwd = 2)
legend(x = "topright", c("Output response", "Unemployment response"), col = c(rgb(0.8, 0.2, 0.2), rgb(0.06, 0.31, 0.55)), lwd = 2, bty = "n")

#SVAR_ABew Labrador
svNL <- cbind(y_NL, u_NL)
colnames(svNL) <- cbind("OutputGap", "UnemploymentRate")
lagselect <- VARselect(svNL, lag.max = 30, type = "both")
lagselect$selection
lagselect$criteria
ModelNL <- VAR(svNL, p = 2, season = NULL, exog = NULL, type = "const", ic = c("AIC", "HQ", "SC", "FPE"))
SVAR_NL <- BQ(ModelNL)
#SVARfevd_NL <- fevd(SVAR_NL, n.ahead = 10)
##plot(#SVARfevd_NL)
irf.dytNL <- irf(SVAR_NL, impulse = "OutputGap", boot = FALSE, n.ahead = 10)
irf.untNL <- irf(SVAR_NL, impulse = "UnemploymentRate", boot = FALSE, n.ahead = 10)
supplyNL <- cbind(irf.dytNL$irf$OutputGap[, 1],1 * irf.untNL$irf$UnemploymentRate[, 1])
demandNL <- cbind(irf.dytNL$irf$OutputGap[, 2], 1 *irf.untNL$irf$UnemploymentRate[, 2])
#Demand Shock plot
plot.ts(demandNL[, 1], col = rgb(0.06, 0.31, 0.55), lwd = 2, ylab = "", xlab = "", main = "Demand Shock for Newfoundland and Labrador", xlim = c(0, 10), ylim = c(-0.04,0.05))
abline(h = 0)
lines(demandNL[, 2], col = rgb(0.8, 0.2, 0.2), lwd = 2)
legend(x = "bottomright", c("Output response", "Unemployment response"), col = c(rgb(0.8, 0.2, 0.2),rgb(0.06, 0.31, 0.55)), lwd = 1, bty = "n")
#Supply Shock plot
plot.ts(supplyNL[, 2], col = rgb(0.06, 0.31, 0.55), lwd = 2, ylab = "", xlab = "", main = "Supply Shock for Newfoundland and Labrador", xlim = c(0, 10), ylim = c(-0.015, 0.05))
abline(h = 0)
lines(supplyNL[, 1], col = rgb(0.8, 0.2, 0.2), lwd = 2)
legend(x = "topright", c("Output response", "Unemployment response"), col = c(rgb(0.8, 0.2, 0.2), rgb(0.06, 0.31, 0.55)), lwd = 2, bty = "n")


#SVAR_ASaskatchewan
svSK <- cbind(y_SK, u_SK)
colnames(svSK) <- cbind("OutputGap", "UnemploymentRate")
lagselect <- VARselect(svSK, lag.max = 30, type = "both")
lagselect$selection
lagselect$criteria
ModelSK <- VAR(svSK, p = 2, season = NULL, exog = NULL, type = "const", ic = c("AIC", "HQ", "SC", "FPE"))
SVAR_SK <- BQ(ModelSK)
#SVARfevd_SK <- fevd(SVAR_SK, n.ahead = 10)
##plot(#SVARfevd_SK)
irf.dytSK <- irf(SVAR_SK, impulse = "OutputGap", boot = FALSE, n.ahead = 10)
irf.untSK <- irf(SVAR_SK, impulse = "UnemploymentRate", boot = FALSE, n.ahead = 10)
supplySK <- cbind(irf.dytSK$irf$OutputGap[, 1],1 * irf.untSK$irf$UnemploymentRate[, 1])
demandSK <- cbind( irf.dytSK$irf$OutputGap[, 2], 1 *irf.untSK$irf$UnemploymentRate[, 2])
#Demand Shock plot
plot.ts(demandSK[, 1], col = rgb(0.06, 0.31, 0.55), lwd = 2, ylab = "", xlab = "", main = "Demand Shock for Saskatchewan", xlim = c(0, 10), ylim = c(-0.12,0.1))
abline(h = 0)
lines(demandSK[, 2], col = rgb(0.8, 0.2, 0.2), lwd = 2)
legend(x = "bottomright", c("Output response", "Unemployment response"), col = c(rgb(0.8, 0.2, 0.2),rgb(0.06, 0.31, 0.55)), lwd = 1, bty = "n")
#Supply Shock plot
plot.ts(supplySK[, 2], col = rgb(0.06, 0.31, 0.55), lwd = 2, ylab = "", xlab = "", main = "Supply Shock for Saskatchewan", xlim = c(0, 10), ylim = c(-0.015, 0.03))
abline(h = 0)
lines(supplySK[, 1], col = rgb(0.8, 0.2, 0.2), lwd = 2)
legend(x = "topright", c("Output response", "Unemployment response"), col = c(rgb(0.8, 0.2, 0.2), rgb(0.06, 0.31, 0.55)), lwd = 2, bty = "n")


#Correlations of Permanent Supply Disturbances
cor.test(c(as.matrix(irf.dytAB[["irf"]][["OutputGap"]])),c(as.matrix(irf.dytAB[["irf"]][["OutputGap"]])), method="pearson")
cor.test(c(as.matrix(irf.dytAB[["irf"]][["OutputGap"]])),c(as.matrix(irf.dytBC[["irf"]][["OutputGap"]])), method="pearson")
cor.test(c(as.matrix(irf.dytAB[["irf"]][["OutputGap"]])),c(as.matrix(irf.dytMB[["irf"]][["OutputGap"]])), method="pearson")
cor.test(c(as.matrix(irf.dytAB[["irf"]][["OutputGap"]])),c(as.matrix(irf.dytNB[["irf"]][["OutputGap"]])), method="pearson")
cor.test(c(as.matrix(irf.dytAB[["irf"]][["OutputGap"]])),c(as.matrix(irf.dytNL[["irf"]][["OutputGap"]])), method="pearson")
cor.test(c(as.matrix(irf.dytAB[["irf"]][["OutputGap"]])),c(as.matrix(irf.dytNS[["irf"]][["OutputGap"]])), method="pearson")
cor.test(c(as.matrix(irf.dytAB[["irf"]][["OutputGap"]])),c(as.matrix(irf.dytON[["irf"]][["OutputGap"]])), method="pearson")
cor.test(c(as.matrix(irf.dytAB[["irf"]][["OutputGap"]])),c(as.matrix(irf.dytPEI[["irf"]][["OutputGap"]])), method="pearson")
cor.test(c(as.matrix(irf.dytAB[["irf"]][["OutputGap"]])),c(as.matrix(irf.dytQC[["irf"]][["OutputGap"]])), method="pearson")
cor.test(c(as.matrix(irf.dytAB[["irf"]][["OutputGap"]])),c(as.matrix(irf.dytSK[["irf"]][["OutputGap"]])), method="pearson")
cor.test(c(as.matrix(irf.dytBC[["irf"]][["OutputGap"]])),c(as.matrix(irf.dytBC[["irf"]][["OutputGap"]])), method="pearson")
cor.test(c(as.matrix(irf.dytBC[["irf"]][["OutputGap"]])),c(as.matrix(irf.dytMB[["irf"]][["OutputGap"]])), method="pearson")
cor.test(c(as.matrix(irf.dytBC[["irf"]][["OutputGap"]])),c(as.matrix(irf.dytNB[["irf"]][["OutputGap"]])), method="pearson")
cor.test(c(as.matrix(irf.dytBC[["irf"]][["OutputGap"]])),c(as.matrix(irf.dytNL[["irf"]][["OutputGap"]])), method="pearson")
cor.test(c(as.matrix(irf.dytBC[["irf"]][["OutputGap"]])),c(as.matrix(irf.dytNS[["irf"]][["OutputGap"]])), method="pearson")
cor.test(c(as.matrix(irf.dytBC[["irf"]][["OutputGap"]])),c(as.matrix(irf.dytON[["irf"]][["OutputGap"]])), method="pearson")
cor.test(c(as.matrix(irf.dytBC[["irf"]][["OutputGap"]])),c(as.matrix(irf.dytPEI[["irf"]][["OutputGap"]])), method="pearson")
cor.test(c(as.matrix(irf.dytBC[["irf"]][["OutputGap"]])),c(as.matrix(irf.dytQC[["irf"]][["OutputGap"]])), method="pearson")
cor.test(c(as.matrix(irf.dytBC[["irf"]][["OutputGap"]])),c(as.matrix(irf.dytSK[["irf"]][["OutputGap"]])), method="pearson")
cor.test(c(as.matrix(irf.dytMB[["irf"]][["OutputGap"]])),c(as.matrix(irf.dytMB[["irf"]][["OutputGap"]])), method="pearson")
cor.test(c(as.matrix(irf.dytMB[["irf"]][["OutputGap"]])),c(as.matrix(irf.dytNB[["irf"]][["OutputGap"]])), method="pearson")
cor.test(c(as.matrix(irf.dytMB[["irf"]][["OutputGap"]])),c(as.matrix(irf.dytNL[["irf"]][["OutputGap"]])), method="pearson")
cor.test(c(as.matrix(irf.dytMB[["irf"]][["OutputGap"]])),c(as.matrix(irf.dytNS[["irf"]][["OutputGap"]])), method="pearson")
cor.test(c(as.matrix(irf.dytMB[["irf"]][["OutputGap"]])),c(as.matrix(irf.dytON[["irf"]][["OutputGap"]])), method="pearson")
cor.test(c(as.matrix(irf.dytMB[["irf"]][["OutputGap"]])),c(as.matrix(irf.dytPEI[["irf"]][["OutputGap"]])), method="pearson")
cor.test(c(as.matrix(irf.dytMB[["irf"]][["OutputGap"]])),c(as.matrix(irf.dytQC[["irf"]][["OutputGap"]])), method="pearson")
cor.test(c(as.matrix(irf.dytMB[["irf"]][["OutputGap"]])),c(as.matrix(irf.dytSK[["irf"]][["OutputGap"]])), method="pearson")
cor.test(c(as.matrix(irf.dytNB[["irf"]][["OutputGap"]])),c(as.matrix(irf.dytNB[["irf"]][["OutputGap"]])), method="pearson")
cor.test(c(as.matrix(irf.dytNB[["irf"]][["OutputGap"]])),c(as.matrix(irf.dytNL[["irf"]][["OutputGap"]])), method="pearson")
cor.test(c(as.matrix(irf.dytNB[["irf"]][["OutputGap"]])),c(as.matrix(irf.dytNS[["irf"]][["OutputGap"]])), method="pearson")
cor.test(c(as.matrix(irf.dytNB[["irf"]][["OutputGap"]])),c(as.matrix(irf.dytON[["irf"]][["OutputGap"]])), method="pearson")
cor.test(c(as.matrix(irf.dytNB[["irf"]][["OutputGap"]])),c(as.matrix(irf.dytPEI[["irf"]][["OutputGap"]])), method="pearson")
cor.test(c(as.matrix(irf.dytNB[["irf"]][["OutputGap"]])),c(as.matrix(irf.dytQC[["irf"]][["OutputGap"]])), method="pearson")
cor.test(c(as.matrix(irf.dytNB[["irf"]][["OutputGap"]])),c(as.matrix(irf.dytSK[["irf"]][["OutputGap"]])), method="pearson")
cor.test(c(as.matrix(irf.dytNL[["irf"]][["OutputGap"]])),c(as.matrix(irf.dytNL[["irf"]][["OutputGap"]])), method="pearson")
cor.test(c(as.matrix(irf.dytNL[["irf"]][["OutputGap"]])),c(as.matrix(irf.dytNS[["irf"]][["OutputGap"]])), method="pearson")
cor.test(c(as.matrix(irf.dytNL[["irf"]][["OutputGap"]])),c(as.matrix(irf.dytON[["irf"]][["OutputGap"]])), method="pearson")
cor.test(c(as.matrix(irf.dytNL[["irf"]][["OutputGap"]])),c(as.matrix(irf.dytPEI[["irf"]][["OutputGap"]])), method="pearson")
cor.test(c(as.matrix(irf.dytNL[["irf"]][["OutputGap"]])),c(as.matrix(irf.dytQC[["irf"]][["OutputGap"]])), method="pearson")
cor.test(c(as.matrix(irf.dytNL[["irf"]][["OutputGap"]])),c(as.matrix(irf.dytSK[["irf"]][["OutputGap"]])), method="pearson")
cor.test(c(as.matrix(irf.dytNS[["irf"]][["OutputGap"]])),c(as.matrix(irf.dytNS[["irf"]][["OutputGap"]])), method="pearson")
cor.test(c(as.matrix(irf.dytNS[["irf"]][["OutputGap"]])),c(as.matrix(irf.dytON[["irf"]][["OutputGap"]])), method="pearson")
cor.test(c(as.matrix(irf.dytNS[["irf"]][["OutputGap"]])),c(as.matrix(irf.dytPEI[["irf"]][["OutputGap"]])), method="pearson")
cor.test(c(as.matrix(irf.dytNS[["irf"]][["OutputGap"]])),c(as.matrix(irf.dytQC[["irf"]][["OutputGap"]])), method="pearson")
cor.test(c(as.matrix(irf.dytNS[["irf"]][["OutputGap"]])),c(as.matrix(irf.dytSK[["irf"]][["OutputGap"]])), method="pearson")
cor.test(c(as.matrix(irf.dytON[["irf"]][["OutputGap"]])),c(as.matrix(irf.dytON[["irf"]][["OutputGap"]])), method="pearson")
cor.test(c(as.matrix(irf.dytON[["irf"]][["OutputGap"]])),c(as.matrix(irf.dytPEI[["irf"]][["OutputGap"]])), method="pearson")
cor.test(c(as.matrix(irf.dytON[["irf"]][["OutputGap"]])),c(as.matrix(irf.dytQC[["irf"]][["OutputGap"]])), method="pearson")
cor.test(c(as.matrix(irf.dytON[["irf"]][["OutputGap"]])),c(as.matrix(irf.dytSK[["irf"]][["OutputGap"]])), method="pearson")
cor.test(c(as.matrix(irf.dytPEI[["irf"]][["OutputGap"]])),c(as.matrix(irf.dytPEI[["irf"]][["OutputGap"]])), method="pearson")
cor.test(c(as.matrix(irf.dytPEI[["irf"]][["OutputGap"]])),c(as.matrix(irf.dytQC[["irf"]][["OutputGap"]])), method="pearson")
cor.test(c(as.matrix(irf.dytPEI[["irf"]][["OutputGap"]])),c(as.matrix(irf.dytSK[["irf"]][["OutputGap"]])), method="pearson")
cor.test(c(as.matrix(irf.dytQC[["irf"]][["OutputGap"]])),c(as.matrix(irf.dytQC[["irf"]][["OutputGap"]])), method="pearson")
cor.test(c(as.matrix(irf.dytQC[["irf"]][["OutputGap"]])),c(as.matrix(irf.dytSK[["irf"]][["OutputGap"]])), method="pearson")
cor.test(c(as.matrix(irf.dytSK[["irf"]][["OutputGap"]])),c(as.matrix(irf.dytSK[["irf"]][["OutputGap"]])), method="pearson")
#Correlations of Transitory Nominal/Demand Disturbances
cor.test(c(as.matrix(irf.untAB[["irf"]][["UnemploymentRate"]])),c(as.matrix(irf.untAB[["irf"]][["UnemploymentRate"]])), method="pearson")
cor.test(c(as.matrix(irf.untAB[["irf"]][["UnemploymentRate"]])),c(as.matrix(irf.untBC[["irf"]][["UnemploymentRate"]])), method="pearson")
cor.test(c(as.matrix(irf.untAB[["irf"]][["UnemploymentRate"]])),c(as.matrix(irf.untMB[["irf"]][["UnemploymentRate"]])), method="pearson")
cor.test(c(as.matrix(irf.untAB[["irf"]][["UnemploymentRate"]])),c(as.matrix(irf.untNB[["irf"]][["UnemploymentRate"]])), method="pearson")
cor.test(c(as.matrix(irf.untAB[["irf"]][["UnemploymentRate"]])),c(as.matrix(irf.untNL[["irf"]][["UnemploymentRate"]])), method="pearson")
cor.test(c(as.matrix(irf.untAB[["irf"]][["UnemploymentRate"]])),c(as.matrix(irf.untNS[["irf"]][["UnemploymentRate"]])), method="pearson")
cor.test(c(as.matrix(irf.untAB[["irf"]][["UnemploymentRate"]])),c(as.matrix(irf.untON[["irf"]][["UnemploymentRate"]])), method="pearson")
cor.test(c(as.matrix(irf.untAB[["irf"]][["UnemploymentRate"]])),c(as.matrix(irf.untPEI[["irf"]][["UnemploymentRate"]])), method="pearson")
cor.test(c(as.matrix(irf.untAB[["irf"]][["UnemploymentRate"]])),c(as.matrix(irf.untQC[["irf"]][["UnemploymentRate"]])), method="pearson")
cor.test(c(as.matrix(irf.untAB[["irf"]][["UnemploymentRate"]])),c(as.matrix(irf.untSK[["irf"]][["UnemploymentRate"]])), method="pearson")
cor.test(c(as.matrix(irf.untBC[["irf"]][["UnemploymentRate"]])),c(as.matrix(irf.untBC[["irf"]][["UnemploymentRate"]])), method="pearson")
cor.test(c(as.matrix(irf.untBC[["irf"]][["UnemploymentRate"]])),c(as.matrix(irf.untMB[["irf"]][["UnemploymentRate"]])), method="pearson")
cor.test(c(as.matrix(irf.untBC[["irf"]][["UnemploymentRate"]])),c(as.matrix(irf.untNB[["irf"]][["UnemploymentRate"]])), method="pearson")
cor.test(c(as.matrix(irf.untBC[["irf"]][["UnemploymentRate"]])),c(as.matrix(irf.untNL[["irf"]][["UnemploymentRate"]])), method="pearson")
cor.test(c(as.matrix(irf.untBC[["irf"]][["UnemploymentRate"]])),c(as.matrix(irf.untNS[["irf"]][["UnemploymentRate"]])), method="pearson")
cor.test(c(as.matrix(irf.untBC[["irf"]][["UnemploymentRate"]])),c(as.matrix(irf.untON[["irf"]][["UnemploymentRate"]])), method="pearson")
cor.test(c(as.matrix(irf.untBC[["irf"]][["UnemploymentRate"]])),c(as.matrix(irf.untPEI[["irf"]][["UnemploymentRate"]])), method="pearson")
cor.test(c(as.matrix(irf.untBC[["irf"]][["UnemploymentRate"]])),c(as.matrix(irf.untQC[["irf"]][["UnemploymentRate"]])), method="pearson")
cor.test(c(as.matrix(irf.untBC[["irf"]][["UnemploymentRate"]])),c(as.matrix(irf.untSK[["irf"]][["UnemploymentRate"]])), method="pearson")
cor.test(c(as.matrix(irf.untMB[["irf"]][["UnemploymentRate"]])),c(as.matrix(irf.untMB[["irf"]][["UnemploymentRate"]])), method="pearson")
cor.test(c(as.matrix(irf.untMB[["irf"]][["UnemploymentRate"]])),c(as.matrix(irf.untNB[["irf"]][["UnemploymentRate"]])), method="pearson")
cor.test(c(as.matrix(irf.untMB[["irf"]][["UnemploymentRate"]])),c(as.matrix(irf.untNL[["irf"]][["UnemploymentRate"]])), method="pearson")
cor.test(c(as.matrix(irf.untMB[["irf"]][["UnemploymentRate"]])),c(as.matrix(irf.untNS[["irf"]][["UnemploymentRate"]])), method="pearson")
cor.test(c(as.matrix(irf.untMB[["irf"]][["UnemploymentRate"]])),c(as.matrix(irf.untON[["irf"]][["UnemploymentRate"]])), method="pearson")
cor.test(c(as.matrix(irf.untMB[["irf"]][["UnemploymentRate"]])),c(as.matrix(irf.untPEI[["irf"]][["UnemploymentRate"]])), method="pearson")
cor.test(c(as.matrix(irf.untMB[["irf"]][["UnemploymentRate"]])),c(as.matrix(irf.untQC[["irf"]][["UnemploymentRate"]])), method="pearson")
cor.test(c(as.matrix(irf.untMB[["irf"]][["UnemploymentRate"]])),c(as.matrix(irf.untSK[["irf"]][["UnemploymentRate"]])), method="pearson")
cor.test(c(as.matrix(irf.untNB[["irf"]][["UnemploymentRate"]])),c(as.matrix(irf.untNB[["irf"]][["UnemploymentRate"]])), method="pearson")
cor.test(c(as.matrix(irf.untNB[["irf"]][["UnemploymentRate"]])),c(as.matrix(irf.untNL[["irf"]][["UnemploymentRate"]])), method="pearson")
cor.test(c(as.matrix(irf.untNB[["irf"]][["UnemploymentRate"]])),c(as.matrix(irf.untNS[["irf"]][["UnemploymentRate"]])), method="pearson")
cor.test(c(as.matrix(irf.untNB[["irf"]][["UnemploymentRate"]])),c(as.matrix(irf.untON[["irf"]][["UnemploymentRate"]])), method="pearson")
cor.test(c(as.matrix(irf.untNB[["irf"]][["UnemploymentRate"]])),c(as.matrix(irf.untPEI[["irf"]][["UnemploymentRate"]])), method="pearson")
cor.test(c(as.matrix(irf.untNB[["irf"]][["UnemploymentRate"]])),c(as.matrix(irf.untQC[["irf"]][["UnemploymentRate"]])), method="pearson")
cor.test(c(as.matrix(irf.untNB[["irf"]][["UnemploymentRate"]])),c(as.matrix(irf.untSK[["irf"]][["UnemploymentRate"]])), method="pearson")
cor.test(c(as.matrix(irf.untNL[["irf"]][["UnemploymentRate"]])),c(as.matrix(irf.untNL[["irf"]][["UnemploymentRate"]])), method="pearson")
cor.test(c(as.matrix(irf.untNL[["irf"]][["UnemploymentRate"]])),c(as.matrix(irf.untNS[["irf"]][["UnemploymentRate"]])), method="pearson")
cor.test(c(as.matrix(irf.untNL[["irf"]][["UnemploymentRate"]])),c(as.matrix(irf.untON[["irf"]][["UnemploymentRate"]])), method="pearson")
cor.test(c(as.matrix(irf.untNL[["irf"]][["UnemploymentRate"]])),c(as.matrix(irf.untPEI[["irf"]][["UnemploymentRate"]])), method="pearson")
cor.test(c(as.matrix(irf.untNL[["irf"]][["UnemploymentRate"]])),c(as.matrix(irf.untQC[["irf"]][["UnemploymentRate"]])), method="pearson")
cor.test(c(as.matrix(irf.untNL[["irf"]][["UnemploymentRate"]])),c(as.matrix(irf.untSK[["irf"]][["UnemploymentRate"]])), method="pearson")
cor.test(c(as.matrix(irf.untNS[["irf"]][["UnemploymentRate"]])),c(as.matrix(irf.untNS[["irf"]][["UnemploymentRate"]])), method="pearson")
cor.test(c(as.matrix(irf.untNS[["irf"]][["UnemploymentRate"]])),c(as.matrix(irf.untON[["irf"]][["UnemploymentRate"]])), method="pearson")
cor.test(c(as.matrix(irf.untNS[["irf"]][["UnemploymentRate"]])),c(as.matrix(irf.untPEI[["irf"]][["UnemploymentRate"]])), method="pearson")
cor.test(c(as.matrix(irf.untNS[["irf"]][["UnemploymentRate"]])),c(as.matrix(irf.untQC[["irf"]][["UnemploymentRate"]])), method="pearson")
cor.test(c(as.matrix(irf.untNS[["irf"]][["UnemploymentRate"]])),c(as.matrix(irf.untSK[["irf"]][["UnemploymentRate"]])), method="pearson")
cor.test(c(as.matrix(irf.untON[["irf"]][["UnemploymentRate"]])),c(as.matrix(irf.untON[["irf"]][["UnemploymentRate"]])), method="pearson")
cor.test(c(as.matrix(irf.untON[["irf"]][["UnemploymentRate"]])),c(as.matrix(irf.untPEI[["irf"]][["UnemploymentRate"]])), method="pearson")
cor.test(c(as.matrix(irf.untON[["irf"]][["UnemploymentRate"]])),c(as.matrix(irf.untQC[["irf"]][["UnemploymentRate"]])), method="pearson")
cor.test(c(as.matrix(irf.untON[["irf"]][["UnemploymentRate"]])),c(as.matrix(irf.untSK[["irf"]][["UnemploymentRate"]])), method="pearson")
cor.test(c(as.matrix(irf.untPEI[["irf"]][["UnemploymentRate"]])),c(as.matrix(irf.untPEI[["irf"]][["UnemploymentRate"]])), method="pearson")
cor.test(c(as.matrix(irf.untPEI[["irf"]][["UnemploymentRate"]])),c(as.matrix(irf.untQC[["irf"]][["UnemploymentRate"]])), method="pearson")
cor.test(c(as.matrix(irf.untPEI[["irf"]][["UnemploymentRate"]])),c(as.matrix(irf.untSK[["irf"]][["UnemploymentRate"]])), method="pearson")
cor.test(c(as.matrix(irf.untQC[["irf"]][["UnemploymentRate"]])),c(as.matrix(irf.untQC[["irf"]][["UnemploymentRate"]])), method="pearson")
cor.test(c(as.matrix(irf.untQC[["irf"]][["UnemploymentRate"]])),c(as.matrix(irf.untSK[["irf"]][["UnemploymentRate"]])), method="pearson")
cor.test(c(as.matrix(irf.untSK[["irf"]][["UnemploymentRate"]])),c(as.matrix(irf.untSK[["irf"]][["UnemploymentRate"]])), method="pearson")

