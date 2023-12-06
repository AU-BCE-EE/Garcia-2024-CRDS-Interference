library(dplyr)
library(ggplot2)
library(anytime)
library(lubridate)
library(writexl)
library("readxl")

setwd("C:/Users/au615105/OneDrive - Aarhus universitet/Pablo García/Papers/Interferences pictures/VOCs")

### Read the treated data ###

#Ethanol
Eth_N2O<-read.table(file="Eth_N2O.txt",header=TRUE, sep = "\t")
Eth_NH3<-read.table(file="Eth_NH3.txt",header=TRUE, sep = "\t")
Eth_CH4<-read.table(file="Eth_CH4.txt",header=TRUE, sep = "\t")
sd_Eth<-read.table(file="sd_Eth.txt",header=TRUE, sep = "\t")
sd_Eth<-rep(sd_Eth$x,7)
CH4_All<-read.table(file="Eth_G2109borrowed_CH4.txt",header=TRUE, sep = "\t")
CH4_own<-read.table(file="Eth_G2109own_CH4.txt",header=TRUE, sep = "\t")
Eth_Range<-read.table(file="Eth_Interval.txt",header=TRUE, sep = "\t")
Eth_G2508b_int<-read.table(file="Eth_G2508borrowed_intervals.txt",header=TRUE, sep = "\t")
Eth_G2508o_int<-read.table(file="Eth_G2508own_intervals.txt",header=TRUE, sep = "\t")
Eth_G2103_int<-read.table(file="Eth_G2103_intervals.txt",header=TRUE, sep = "\t")
Eth_G4301_int<-read.table(file="Eth_G4301_intervals.txt",header=TRUE, sep = "\t")

#Butyric acid
BAc_N2O<-read.table(file="BAc_N2O.txt",header=TRUE, sep = "\t")
BAc_NH3<-read.table(file="BAc_NH3.txt",header=TRUE, sep = "\t")
BAc_CH4<-read.table(file="BAc_CH4.txt",header=TRUE, sep = "\t")
sd_BAc<-read.table(file="sd_BAc.txt",header=TRUE, sep = "\t")
sd_BAc<-rep(sd_BAc$x,5)
BAc_CH4_All<-read.table(file="BAc_G2509_CH4.txt",header=TRUE, sep = "\t")
BAc_CH4_BP<-read.table(file="BAc_G4301_CH4.txt",header=TRUE, sep = "\t")
BAc_Range<-read.table(file="BAc_Interval.txt",header=TRUE, sep = "\t")
BAc_G2509_int<-read.table(file="BAc_G2509_intervals.txt",header=TRUE, sep = "\t")
BAc_G2103_int<-read.table(file="BAc_G2103_intervals.txt",header=TRUE, sep = "\t")
BAc_G4301_int<-read.table(file="BAc_G4301_intervals.txt",header=TRUE, sep = "\t")

#Diacetyl
DA_N2O<-read.table(file="DA_N2O.txt",header=TRUE, sep = "\t")
DA_NH3<-read.table(file="DA_NH3.txt",header=TRUE, sep = "\t")
DA_CH4<-read.table(file="DA_CH4.txt",header=TRUE, sep = "\t")
sd_DA<-read.table(file="sd_DA.txt",header=TRUE, sep = "\t")
sd_DA<-rep(sd_DA$x,5)
DA_CH4_All<-read.table(file="DA_G2509_CH4.txt",header=TRUE, sep = "\t")
DA_CH4_BP<-read.table(file="DA_G4301_CH4.txt",header=TRUE, sep = "\t")
DA_Range<-read.table(file="DA_Interval.txt",header=TRUE, sep = "\t")
DA_G2509_int<-read.table(file="DA_G2509_intervals.txt",header=TRUE, sep = "\t")
DA_G2103_int<-read.table(file="DA_G2103_intervals.txt",header=TRUE, sep = "\t")
DA_G4301_int<-read.table(file="DA_G4301_intervals.txt",header=TRUE, sep = "\t")

#Butanone
Bne_N2O<-read.table(file="Bne_N2O.txt",header=TRUE, sep = "\t")
Bne_NH3<-read.table(file="Bne_NH3.txt",header=TRUE, sep = "\t")
Bne_CH4<-read.table(file="Bne_CH4.txt",header=TRUE, sep = "\t")
sd_Bne<-read.table(file="sd_Bne.txt",header=TRUE, sep = "\t")
sd_Bne<-rep(sd_Bne$x,5)
Bne_CH4_All<-read.table(file="Bne_G2509_CH4.txt",header=TRUE, sep = "\t")
Bne_CH4_BP<-read.table(file="Bne_G4301_CH4.txt",header=TRUE, sep = "\t")
Bne_Range<-read.table(file="Bne_Interval.txt",header=TRUE, sep = "\t")
Bne_G2509_int<-read.table(file="Bne_G2509_intervals.txt",header=TRUE, sep = "\t")
Bne_G2103_int<-read.table(file="Bne_G2103_intervals.txt",header=TRUE, sep = "\t")
Bne_G4301_int<-read.table(file="Bne_G4301_intervals.txt",header=TRUE, sep = "\t")

#1-Butanol
B1_N2O<-read.table(file="B1_N2O.txt",header=TRUE, sep = "\t")
B1_NH3<-read.table(file="B1_NH3.txt",header=TRUE, sep = "\t")
B1_CH4<-read.table(file="B1_CH4.txt",header=TRUE, sep = "\t")
sd_B1<-read.table(file="sd_B1.txt",header=TRUE, sep = "\t")
sd_B1<-rep(sd_B1$x,5)
B1_CH4_All<-read.table(file="B1_G2509_CH4.txt",header=TRUE, sep = "\t")
B1_CH4_BP<-read.table(file="B1_G4301_CH4.txt",header=TRUE, sep = "\t")
B1_Range<-read.table(file="B1_Interval.txt",header=TRUE, sep = "\t")
B1_G2509_int<-read.table(file="B1_G2509_intervals.txt",header=TRUE, sep = "\t")
B1_G2103_int<-read.table(file="B1_G2103_intervals.txt",header=TRUE, sep = "\t")
B1_G4301_int<-read.table(file="B1_G4301_intervals.txt",header=TRUE, sep = "\t")

#1-Propanol
P1_N2O<-read.table(file="P1_N2O.txt",header=TRUE, sep = "\t")
P1_NH3<-read.table(file="P1_NH3.txt",header=TRUE, sep = "\t")
P1_CH4<-read.table(file="P1_CH4.txt",header=TRUE, sep = "\t")
sd_P1<-read.table(file="sd_P1.txt",header=TRUE, sep = "\t")
sd_P1<-rep(sd_P1$x,5)
P1_CH4_All<-read.table(file="P1_G2509_CH4.txt",header=TRUE, sep = "\t")
P1_CH4_BP<-read.table(file="P1_G4301_CH4.txt",header=TRUE, sep = "\t")
P1_Range<-read.table(file="P1_Interval.txt",header=TRUE, sep = "\t")
P1_G2509_int<-read.table(file="P1_G2509_intervals.txt",header=TRUE, sep = "\t")
P1_G2103_int<-read.table(file="P1_G2103_intervals.txt",header=TRUE, sep = "\t")
P1_G4301_int<-read.table(file="P1_G4301_intervals.txt",header=TRUE, sep = "\t")

#2-Propanol
P2_N2O<-read.table(file="P2_N2O.txt",header=TRUE, sep = "\t")
P2_NH3<-read.table(file="P2_NH3.txt",header=TRUE, sep = "\t")
P2_CH4<-read.table(file="P2_CH4.txt",header=TRUE, sep = "\t")
sd_P2<-read.table(file="sd_P2.txt",header=TRUE, sep = "\t")
sd_P2<-rep(sd_P2$x,5)
P2_CH4_All<-read.table(file="P2_G2509_CH4.txt",header=TRUE, sep = "\t")
P2_CH4_BP<-read.table(file="P2_G4301_CH4.txt",header=TRUE, sep = "\t")
P2_Range<-read.table(file="P2_Interval.txt",header=TRUE, sep = "\t")
P2_G2509_int<-read.table(file="P2_G2509_intervals.txt",header=TRUE, sep = "\t")
P2_G2103_int<-read.table(file="P2_G2103_intervals.txt",header=TRUE, sep = "\t")
P2_G4301_int<-read.table(file="P2_G4301_intervals.txt",header=TRUE, sep = "\t")

#Acetaldehyde
AA_N2O<-read.table(file="AA_N2O.txt",header=TRUE, sep = "\t")
AA_NH3<-read.table(file="AA_NH3.txt",header=TRUE, sep = "\t")
AA_CH4<-read.table(file="AA_CH4.txt",header=TRUE, sep = "\t")
sd_AA<-read.table(file="sd_AA.txt",header=TRUE, sep = "\t")
sd_AA<-rep(sd_AA$x,5)
AA_CH4_All<-read.table(file="AA_G2509_CH4.txt",header=TRUE, sep = "\t")
AA_CH4_BP<-read.table(file="AA_G4301_CH4.txt",header=TRUE, sep = "\t")
AA_Range<-read.table(file="AA_Interval.txt",header=TRUE, sep = "\t")
AA_G2509_int<-read.table(file="AA_G2509_intervals.txt",header=TRUE, sep = "\t")
AA_G2103_int<-read.table(file="AA_G2103_intervals.txt",header=TRUE, sep = "\t")
AA_G4301_int<-read.table(file="AA_G4301_intervals.txt",header=TRUE, sep = "\t")

#Acetic acid
AAc_N2O<-read.table(file="AAc_N2O.txt",header=TRUE, sep = "\t")
AAc_NH3<-read.table(file="AAc_NH3.txt",header=TRUE, sep = "\t")
AAc_CH4<-read.table(file="AAc_CH4.txt",header=TRUE, sep = "\t")
sd_AAc<-read.table(file="sd_AAc.txt",header=TRUE, sep = "\t")
sd_AAc<-rep(sd_AAc$x,5)
AAc_CH4_All<-read.table(file="AAc_G2509_CH4.txt",header=TRUE, sep = "\t")
AAc_CH4_BP<-read.table(file="AAc_G4301_CH4.txt",header=TRUE, sep = "\t")
AAc_Range<-read.table(file="AAc_Interval.txt",header=TRUE, sep = "\t")
AAc_G2509_int<-read.table(file="AAc_G2509_intervals.txt",header=TRUE, sep = "\t")
AAc_G2103_int<-read.table(file="AAc_G2103_intervals.txt",header=TRUE, sep = "\t")
AAc_G4301_int<-read.table(file="AAc_G4301_intervals.txt",header=TRUE, sep = "\t")

#Acetone
Ac_N2O<-read.table(file="Ac_N2O.txt",header=TRUE, sep = "\t")
Ac_NH3<-read.table(file="Ac_NH3.txt",header=TRUE, sep = "\t")
Ac_CH4<-read.table(file="Ac_CH4.txt",header=TRUE, sep = "\t")
sd_Ac<-read.table(file="sd_Ac.txt",header=TRUE, sep = "\t")
sd_Ac<-rep(sd_Ac$x,5)
Ac_CH4_All<-read.table(file="Ac_G2509_CH4.txt",header=TRUE, sep = "\t")
Ac_CH4_BP<-read.table(file="Ac_G4301_CH4.txt",header=TRUE, sep = "\t")
Ac_Range<-read.table(file="Ac_Interval.txt",header=TRUE, sep = "\t")
Ac_G2509_int<-read.table(file="Ac_G2509_intervals.txt",header=TRUE, sep = "\t")
Ac_G2103_int<-read.table(file="Ac_G2103_intervals.txt",header=TRUE, sep = "\t")
Ac_G4301_int<-read.table(file="Ac_G4301_intervals.txt",header=TRUE, sep = "\t")

#Methanol
MeOH_N2O<-read.table(file="MeOH_N2O.txt",header=TRUE, sep = "\t")
MeOH_NH3<-read.table(file="MeOH_NH3.txt",header=TRUE, sep = "\t")
MeOH_CH4<-read.table(file="MeOH_CH4.txt",header=TRUE, sep = "\t")
sd_MeOH<-read.table(file="sd_MeOH.txt",header=TRUE, sep = "\t")
sd_MeOH<-rep(sd_MeOH$x,5)
MeOH_CH4_All<-read.table(file="MeOH_G2509_CH4.txt",header=TRUE, sep = "\t")
MeOH_CH4_BP<-read.table(file="MeOH_G4301_CH4.txt",header=TRUE, sep = "\t")
MeOH_Range<-read.table(file="MeOH_Interval.txt",header=TRUE, sep = "\t")
MeOH_G2509_int<-read.table(file="MeOH_G2509_intervals.txt",header=TRUE, sep = "\t")
MeOH_G2103_int<-read.table(file="MeOH_G2103_intervals.txt",header=TRUE, sep = "\t")
MeOH_G4301_int<-read.table(file="MeOH_G4301_intervals.txt",header=TRUE, sep = "\t")

#Propionic acid
PAc_N2O<-read.table(file="PAc_N2O.txt",header=TRUE, sep = "\t")
PAc_NH3<-read.table(file="PAc_NH3.txt",header=TRUE, sep = "\t")
PAc_CH4<-read.table(file="PAc_CH4.txt",header=TRUE, sep = "\t")
sd_PAc<-read.table(file="sd_PAc.txt",header=TRUE, sep = "\t")
sd_PAc<-rep(sd_PAc$x,4)
PAc_CH4_All<-read.table(file="PAc_G2509_CH4.txt",header=TRUE, sep = "\t")
PAc_Range<-read.table(file="PAc_Interval.txt",header=TRUE, sep = "\t")
PAc_G2509_int<-read.table(file="PAc_G2509_intervals.txt",header=TRUE, sep = "\t")
PAc_G2103_int<-read.table(file="PAc_G2103_intervals.txt",header=TRUE, sep = "\t")

### Detrend CH4 ###

### Ethanol 
# Borrowed G2509
x_T<-1:length(CH4_All$CH4_dry[30000:45000]) ##Needed to make the polynomic function work

CH4_All_trend<-CH4_All$CH4_dry[30000:45000] #Select interval with "consistent" trend ~linear
Eq_CH4_All<-lm(formula = CH4_All_trend ~ poly(as.numeric(x_T),18,raw=TRUE)) #polynomial
Coeff_CH4_All<-coefficients(Eq_CH4_All)
CH4_All_detrend<-CH4_All_trend-predict(Eq_CH4_All)
CH4_All_detrend<-CH4_All_detrend+mean(CH4_All_trend)

#G2509 own
CH4_own_trend<-CH4_own$CH4_dry[30000:45000] #Select interval with "consistent" trend ~linear
Eq_CH4_own<-lm(formula = CH4_own_trend ~ poly(as.numeric(x_T),18,raw=TRUE)) #polynomial
Coeff_CH4_own<-coefficients(Eq_CH4_own)
CH4_own_detrend<-CH4_own_trend-predict(Eq_CH4_own)
CH4_own_detrend<-CH4_own_detrend+mean(CH4_own_trend)

setwd("C:/Users/au615105/OneDrive - Aarhus universitet/Pablo García/Papers/Interferences pictures/VOCs/Pictures publication")

##Control plots-->make sure detrend works
Cp<-ggplot()+geom_point(aes(anytime(CH4_All$date.time[30000:45000]),CH4_All_trend,color="original"))+
  geom_line(aes(anytime(CH4_All$date.time[30000:45000]),predict(Eq_CH4_All),color="Fit"))+
  geom_point(aes(anytime(CH4_All$date.time[30000:45000]),CH4_All_detrend,color="detrended"))
ggsave(plot=Cp,"Detrend control_withEtOH_borrowedG2509.png", width = 8, height = 5)

Cp<-ggplot()+geom_point(aes(anytime(CH4_own$date.time[30000:45000]),CH4_own_trend,color="original"))+
  geom_line(aes(anytime(CH4_own$date.time[30000:45000]),predict(Eq_CH4_own),color="Fit"))+
  geom_point(aes(anytime(CH4_own$date.time[30000:45000]),CH4_own_detrend,color="detrended"))
ggsave(plot=Cp,"Detrend control_withEtOH_ownG2509.png", width = 8, height = 5)

### Butyric acid 
# G2509
x_T<-1:length(BAc_CH4_All$CH4_dry[65000:89000]) ##Needed to make the polynomic function work
x_Tc<-1:length(BAc_CH4_All$CH4_dry[46000:69000]) ##Needed to make the polynomic function work

BAc_CH4_All_trend<-BAc_CH4_All$CH4_dry[65000:89000] #Select interval with "consistent" trend ~linear
Eq_CH4_All<-lm(formula = BAc_CH4_All_trend ~ poly(as.numeric(x_T),18,raw=TRUE)) #polynomial
Coeff_CH4_All<-coefficients(Eq_CH4_All)
BAc_CH4_All_detrend<-BAc_CH4_All_trend-predict(Eq_CH4_All)
BAc_CH4_All_detrend<-BAc_CH4_All_detrend+mean(BAc_CH4_All_trend)

#G4301
BAc_CH4_BP_trend<-BAc_CH4_BP$CH4_dry[46000:69000] #Select interval with "consistent" trend ~linear
Eq_CH4_BP<-lm(formula = BAc_CH4_BP_trend ~ poly(as.numeric(x_Tc),18,raw=TRUE)) #polynomial
Coeff_CH4_BP<-coefficients(Eq_CH4_BP)
BAc_CH4_BP_detrend<-BAc_CH4_BP_trend-predict(Eq_CH4_BP)
BAc_CH4_BP_detrend<-BAc_CH4_BP_detrend+mean(BAc_CH4_BP_trend)

##Control plots-->make sure detrend works
Cp<-ggplot()+geom_point(aes(anytime(BAc_CH4_All$date.time[65000:89000]),BAc_CH4_All_trend,color="original"))+
  geom_line(aes(anytime(BAc_CH4_All$date.time[65000:89000]),predict(Eq_CH4_All),color="Fit"))+
  geom_point(aes(anytime(BAc_CH4_All$date.time[65000:89000]),BAc_CH4_All_detrend,color="detrended"))
ggsave(plot=Cp,"Detrend control_withBAc_G2509.png", width = 8, height = 5)

Cp<-ggplot()+geom_point(aes(anytime(BAc_CH4_BP$date.time[46000:69000]),BAc_CH4_BP_trend,color="original"))+
  geom_line(aes(anytime(BAc_CH4_BP$date.time[46000:69000]),predict(Eq_CH4_BP),color="Fit"))+
  geom_point(aes(anytime(BAc_CH4_BP$date.time[46000:69000]),BAc_CH4_BP_detrend,color="detrended"))
ggsave(plot=Cp,"Detrend control_withBAc_G4301.png", width = 8, height = 5)

### Diacetyl 
# G2509
x_T<-1:length(DA_CH4_All$CH4_dry[61000:66800]) ##Needed to make the polynomic function work
x_Tc<-1:length(DA_CH4_All$CH4_dry[44500:48450]) ##Needed to make the polynomic function work

DA_CH4_All_trend<-DA_CH4_All$CH4_dry[61000:66800] #Select interval with "consistent" trend ~linear
Eq_CH4_All<-lm(formula = DA_CH4_All_trend ~ poly(as.numeric(x_T),18,raw=TRUE)) #polynomial
Coeff_CH4_All<-coefficients(Eq_CH4_All)
DA_CH4_All_detrend<-DA_CH4_All_trend-predict(Eq_CH4_All)
DA_CH4_All_detrend<-DA_CH4_All_detrend+mean(DA_CH4_All_trend)

#G4301
DA_CH4_BP_trend<-DA_CH4_BP$CH4_dry[44500:48450] #Select interval with "consistent" trend ~linear
Eq_CH4_BP<-lm(formula = DA_CH4_BP_trend ~ poly(as.numeric(x_Tc),18,raw=TRUE)) #polynomial
Coeff_CH4_BP<-coefficients(Eq_CH4_BP)
DA_CH4_BP_detrend<-DA_CH4_BP_trend-predict(Eq_CH4_BP)
DA_CH4_BP_detrend<-DA_CH4_BP_detrend+mean(DA_CH4_BP_trend)

##Control plots-->make sure detrend works
Cp<-ggplot()+geom_point(aes(anytime(DA_CH4_All$date.time[61000:66800]),DA_CH4_All_trend,color="original"))+
  geom_line(aes(anytime(DA_CH4_All$date.time[61000:66800]),predict(Eq_CH4_All),color="Fit"))+
  geom_point(aes(anytime(DA_CH4_All$date.time[61000:66800]),DA_CH4_All_detrend,color="detrended"))
ggsave(plot=Cp,"Detrend control_withDA_G2509.png", width = 8, height = 5)

Cp<-ggplot()+geom_point(aes(anytime(DA_CH4_BP$date.time[44500:48450]),DA_CH4_BP_trend,color="original"))+
  geom_line(aes(anytime(DA_CH4_BP$date.time[44500:48450]),predict(Eq_CH4_BP),color="Fit"))+
  geom_point(aes(anytime(DA_CH4_BP$date.time[44500:48450]),DA_CH4_BP_detrend,color="detrended"))
ggsave(plot=Cp,"Detrend control_withDA_G4301.png", width = 8, height = 5)


### Butanone 
# G2509
x_T<-1:length(Bne_CH4_All$CH4_dry[55500:63000]) ##Needed to make the polynomic function work
x_Tc<-1:length(Bne_CH4_All$CH4_dry[40600:46000]) ##Needed to make the polynomic function work

Bne_CH4_All_trend<-Bne_CH4_All$CH4_dry[55500:63000] #Select interval with "consistent" trend ~linear
Eq_CH4_All<-lm(formula = Bne_CH4_All_trend ~ poly(as.numeric(x_T),18,raw=TRUE)) #polynomial
Coeff_CH4_All<-coefficients(Eq_CH4_All)
Bne_CH4_All_detrend<-Bne_CH4_All_trend-predict(Eq_CH4_All)
Bne_CH4_All_detrend<-Bne_CH4_All_detrend+mean(Bne_CH4_All_trend)

#G4301
Bne_CH4_BP_trend<-Bne_CH4_BP$CH4_dry[40600:46000] #Select interval with "consistent" trend ~linear
Eq_CH4_BP<-lm(formula = Bne_CH4_BP_trend ~ poly(as.numeric(x_Tc),18,raw=TRUE)) #polynomial
Coeff_CH4_BP<-coefficients(Eq_CH4_BP)
Bne_CH4_BP_detrend<-Bne_CH4_BP_trend-predict(Eq_CH4_BP)
Bne_CH4_BP_detrend<-Bne_CH4_BP_detrend+mean(Bne_CH4_BP_trend)

##Control plots-->make sure detrend works
Cp<-ggplot()+geom_point(aes(anytime(Bne_CH4_All$date.time[55500:63000]),Bne_CH4_All_trend,color="original"))+
  geom_line(aes(anytime(Bne_CH4_All$date.time[55500:63000]),predict(Eq_CH4_All),color="Fit"))+
  geom_point(aes(anytime(Bne_CH4_All$date.time[55500:63000]),Bne_CH4_All_detrend,color="detrended"))
ggsave(plot=Cp,"Detrend control_withBne_G2509.png", width = 8, height = 5)

Cp<-ggplot()+geom_point(aes(anytime(Bne_CH4_BP$date.time[40600:46000]),Bne_CH4_BP_trend,color="original"))+
  geom_line(aes(anytime(Bne_CH4_BP$date.time[40600:46000]),predict(Eq_CH4_BP),color="Fit"))+
  geom_point(aes(anytime(Bne_CH4_BP$date.time[40600:46000]),Bne_CH4_BP_detrend,color="detrended"))
ggsave(plot=Cp,"Detrend control_withBne_G4301.png", width = 8, height = 5)


### 1-Butanol
# G2509
x_T<-1:length(B1_CH4_All$CH4_dry[35500:50000]) ##Needed to make the polynomic function work
x_Tc<-1:length(B1_CH4_All$CH4_dry[25500:38000]) ##Needed to make the polynomic function work

B1_CH4_All_trend<-B1_CH4_All$CH4_dry[35500:50000] #Select interval with "consistent" trend ~linear
Eq_CH4_All<-lm(formula = B1_CH4_All_trend ~ poly(as.numeric(x_T),18,raw=TRUE)) #polynomial
Coeff_CH4_All<-coefficients(Eq_CH4_All)
B1_CH4_All_detrend<-B1_CH4_All_trend-predict(Eq_CH4_All)
B1_CH4_All_detrend<-B1_CH4_All_detrend+mean(B1_CH4_All_trend)

#G4301
B1_CH4_BP_trend<-B1_CH4_BP$CH4_dry[25500:38000] #Select interval with "consistent" trend ~linear
Eq_CH4_BP<-lm(formula = B1_CH4_BP_trend ~ poly(as.numeric(x_Tc),18,raw=TRUE)) #polynomial
Coeff_CH4_BP<-coefficients(Eq_CH4_BP)
B1_CH4_BP_detrend<-B1_CH4_BP_trend-predict(Eq_CH4_BP)
B1_CH4_BP_detrend<-B1_CH4_BP_detrend+mean(B1_CH4_BP_trend)

##Control plots-->make sure detrend works
Cp<-ggplot()+geom_point(aes(anytime(B1_CH4_All$date.time[35500:50000]),B1_CH4_All_trend,color="original"))+
  geom_line(aes(anytime(B1_CH4_All$date.time[35500:50000]),predict(Eq_CH4_All),color="Fit"))+
  geom_point(aes(anytime(B1_CH4_All$date.time[35500:50000]),B1_CH4_All_detrend,color="detrended"))
ggsave(plot=Cp,"Detrend control_withB1_G2509.png", width = 8, height = 5)

Cp<-ggplot()+geom_point(aes(anytime(B1_CH4_BP$date.time[25500:38000]),B1_CH4_BP_trend,color="original"))+
  geom_line(aes(anytime(B1_CH4_BP$date.time[25500:38000]),predict(Eq_CH4_BP),color="Fit"))+
  geom_point(aes(anytime(B1_CH4_BP$date.time[25500:38000]),B1_CH4_BP_detrend,color="detrended"))
ggsave(plot=Cp,"Detrend control_withB1_G4301.png", width = 8, height = 5)

### 1-Propanol
# G2509
x_T<-1:length(P1_CH4_All$CH4_dry[25500:35000]) ##Needed to make the polynomic function work
x_Tc<-1:length(P1_CH4_All$CH4_dry[20500:27000]) ##Needed to make the polynomic function work

P1_CH4_All_trend<-P1_CH4_All$CH4_dry[25500:35000] #Select interval with "consistent" trend ~linear
Eq_CH4_All<-lm(formula = P1_CH4_All_trend ~ poly(as.numeric(x_T),18,raw=TRUE)) #polynomial
Coeff_CH4_All<-coefficients(Eq_CH4_All)
P1_CH4_All_detrend<-P1_CH4_All_trend-predict(Eq_CH4_All)
P1_CH4_All_detrend<-P1_CH4_All_detrend+mean(P1_CH4_All_trend)

#G4301
P1_CH4_BP_trend<-P1_CH4_BP$CH4_dry[20500:27000] #Select interval with "consistent" trend ~linear
Eq_CH4_BP<-lm(formula = P1_CH4_BP_trend ~ poly(as.numeric(x_Tc),18,raw=TRUE)) #polynomial
Coeff_CH4_BP<-coefficients(Eq_CH4_BP)
P1_CH4_BP_detrend<-P1_CH4_BP_trend-predict(Eq_CH4_BP)
P1_CH4_BP_detrend<-P1_CH4_BP_detrend+mean(P1_CH4_BP_trend)

##Control plots-->make sure detrend works
Cp<-ggplot()+geom_point(aes(anytime(P1_CH4_All$date.time[25500:35000]),P1_CH4_All_trend,color="original"))+
  geom_line(aes(anytime(P1_CH4_All$date.time[25500:35000]),predict(Eq_CH4_All),color="Fit"))+
  geom_point(aes(anytime(P1_CH4_All$date.time[25500:35000]),P1_CH4_All_detrend,color="detrended"))
ggsave(plot=Cp,"Detrend control_withP1_G2509.png", width = 8, height = 5)

Cp<-ggplot()+geom_point(aes(anytime(P1_CH4_BP$date.time[20500:27000]),P1_CH4_BP_trend,color="original"))+
  geom_line(aes(anytime(P1_CH4_BP$date.time[20500:27000]),predict(Eq_CH4_BP),color="Fit"))+
  geom_point(aes(anytime(P1_CH4_BP$date.time[20500:27000]),P1_CH4_BP_detrend,color="detrended"))
ggsave(plot=Cp,"Detrend control_withP1_G4301.png", width = 8, height = 5)


### 2-Propanol
# G2509
x_T<-1:length(P2_CH4_All$CH4_dry[63500:68600]) ##Needed to make the polynomic function work
x_Tc<-1:length(P2_CH4_All$CH4_dry[45830:49500]) ##Needed to make the polynomic function work

P2_CH4_All_trend<-P2_CH4_All$CH4_dry[63500:68600] #Select interval with "consistent" trend ~linear
Eq_CH4_All<-lm(formula = P2_CH4_All_trend ~ poly(as.numeric(x_T),18,raw=TRUE)) #polynomial
Coeff_CH4_All<-coefficients(Eq_CH4_All)
P2_CH4_All_detrend<-P2_CH4_All_trend-predict(Eq_CH4_All)
P2_CH4_All_detrend<-P2_CH4_All_detrend+mean(P2_CH4_All_trend)

#G4301
P2_CH4_BP_trend<-P2_CH4_BP$CH4_dry[45830:49500] #Select interval with "consistent" trend ~linear
Eq_CH4_BP<-lm(formula = P2_CH4_BP_trend ~ poly(as.numeric(x_Tc),18,raw=TRUE)) #polynomial
Coeff_CH4_BP<-coefficients(Eq_CH4_BP)
P2_CH4_BP_detrend<-P2_CH4_BP_trend-predict(Eq_CH4_BP)
P2_CH4_BP_detrend<-P2_CH4_BP_detrend+mean(P2_CH4_BP_trend)

##Control plots-->make sure detrend works
Cp<-ggplot()+geom_point(aes(anytime(P2_CH4_All$date.time[63500:68600]),P2_CH4_All_trend,color="original"))+
  geom_line(aes(anytime(P2_CH4_All$date.time[63500:68600]),predict(Eq_CH4_All),color="Fit"))+
  geom_point(aes(anytime(P2_CH4_All$date.time[63500:68600]),P2_CH4_All_detrend,color="detrended"))
ggsave(plot=Cp,"Detrend control_withP2_G2509.png", width = 8, height = 5)

Cp<-ggplot()+geom_point(aes(anytime(P2_CH4_BP$date.time[45830:49500]),P2_CH4_BP_trend,color="original"))+
  geom_line(aes(anytime(P2_CH4_BP$date.time[45830:49500]),predict(Eq_CH4_BP),color="Fit"))+
  geom_point(aes(anytime(P2_CH4_BP$date.time[45830:49500]),P2_CH4_BP_detrend,color="detrended"))
ggsave(plot=Cp,"Detrend control_withP2_G4301.png", width = 8, height = 5)

### Acetaldehyde
# G2509
x_T<-1:length(AA_CH4_All$CH4_dry[46830:56900]) ##Needed to make the polynomic function work
x_Tc<-1:length(AA_CH4_All$CH4_dry[34830:41500]) ##Needed to make the polynomic function work

AA_CH4_All_trend<-AA_CH4_All$CH4_dry[46830:56900] #Select interval with "consistent" trend ~linear
Eq_CH4_All<-lm(formula = AA_CH4_All_trend ~ poly(as.numeric(x_T),18,raw=TRUE)) #polynomial
Coeff_CH4_All<-coefficients(Eq_CH4_All)
AA_CH4_All_detrend<-AA_CH4_All_trend-predict(Eq_CH4_All)
AA_CH4_All_detrend<-AA_CH4_All_detrend+mean(AA_CH4_All_trend)

#G4301
AA_CH4_BP_trend<-AA_CH4_BP$CH4_dry[34830:41500] #Select interval with "consistent" trend ~linear
Eq_CH4_BP<-lm(formula = AA_CH4_BP_trend ~ poly(as.numeric(x_Tc),18,raw=TRUE)) #polynomial
Coeff_CH4_BP<-coefficients(Eq_CH4_BP)
AA_CH4_BP_detrend<-AA_CH4_BP_trend-predict(Eq_CH4_BP)
AA_CH4_BP_detrend<-AA_CH4_BP_detrend+mean(AA_CH4_BP_trend)

##Control plots-->make sure detrend works
Cp<-ggplot()+geom_point(aes(anytime(AA_CH4_All$date.time[46830:56900]),AA_CH4_All_trend,color="original"))+
  geom_line(aes(anytime(AA_CH4_All$date.time[46830:56900]),predict(Eq_CH4_All),color="Fit"))+
  geom_point(aes(anytime(AA_CH4_All$date.time[46830:56900]),AA_CH4_All_detrend,color="detrended"))
ggsave(plot=Cp,"Detrend control_withAA_G2509.png", width = 8, height = 5)

Cp<-ggplot()+geom_point(aes(anytime(AA_CH4_BP$date.time[34830:41500]),AA_CH4_BP_trend,color="original"))+
  geom_line(aes(anytime(AA_CH4_BP$date.time[34830:41500]),predict(Eq_CH4_BP),color="Fit"))+
  geom_point(aes(anytime(AA_CH4_BP$date.time[34830:41500]),AA_CH4_BP_detrend,color="detrended"))
ggsave(plot=Cp,"Detrend control_withAA_G4301.png", width = 8, height = 5)

### Acetic acid
# G2509
x_T<-1:length(AAc_CH4_All$CH4_dry[35000:44500]) ##Needed to make the polynomic function work
x_Tc<-1:length(AAc_CH4_All$CH4_dry[26000:32800]) ##Needed to make the polynomic function work

AAc_CH4_All_trend<-AAc_CH4_All$CH4_dry[35000:44500] #Select interval with "consistent" trend ~linear
Eq_CH4_All<-lm(formula = AAc_CH4_All_trend ~ poly(as.numeric(x_T),18,raw=TRUE)) #polynomial
Coeff_CH4_All<-coefficients(Eq_CH4_All)
AAc_CH4_All_detrend<-AAc_CH4_All_trend-predict(Eq_CH4_All)
AAc_CH4_All_detrend<-AAc_CH4_All_detrend+mean(AAc_CH4_All_trend)

#G4301
AAc_CH4_BP_trend<-AAc_CH4_BP$CH4_dry[26000:32800] #Select interval with "consistent" trend ~linear
Eq_CH4_BP<-lm(formula = AAc_CH4_BP_trend ~ poly(as.numeric(x_Tc),18,raw=TRUE)) #polynomial
Coeff_CH4_BP<-coefficients(Eq_CH4_BP)
AAc_CH4_BP_detrend<-AAc_CH4_BP_trend-predict(Eq_CH4_BP)
AAc_CH4_BP_detrend<-AAc_CH4_BP_detrend+mean(AAc_CH4_BP_trend)

##Control plots-->make sure detrend works
Cp<-ggplot()+geom_point(aes(anytime(AAc_CH4_All$date.time[35000:44500]),AAc_CH4_All_trend,color="original"))+
  geom_line(aes(anytime(AAc_CH4_All$date.time[35000:44500]),predict(Eq_CH4_All),color="Fit"))+
  geom_point(aes(anytime(AAc_CH4_All$date.time[35000:44500]),AAc_CH4_All_detrend,color="detrended"))
ggsave(plot=Cp,"Detrend control_withAAc_G2509.png", width = 8, height = 5)

Cp<-ggplot()+geom_point(aes(anytime(AAc_CH4_BP$date.time[26000:32800]),AAc_CH4_BP_trend,color="original"))+
  geom_line(aes(anytime(AAc_CH4_BP$date.time[26000:32800]),predict(Eq_CH4_BP),color="Fit"))+
  geom_point(aes(anytime(AAc_CH4_BP$date.time[26000:32800]),AAc_CH4_BP_detrend,color="detrended"))
ggsave(plot=Cp,"Detrend control_withAAc_G4301.png", width = 8, height = 5)

### Acetone
# G2509
x_T<-1:length(Ac_CH4_All$CH4_dry[59000:65000]) ##Needed to make the polynomic function work
x_Tc<-1:length(Ac_CH4_All$CH4_dry[43000:46100]) ##Needed to make the polynomic function work

Ac_CH4_All_trend<-Ac_CH4_All$CH4_dry[59000:65000] #Select interval with "consistent" trend ~linear
Eq_CH4_All<-lm(formula = Ac_CH4_All_trend ~ poly(as.numeric(x_T),18,raw=TRUE)) #polynomial
Coeff_CH4_All<-coefficients(Eq_CH4_All)
Ac_CH4_All_detrend<-Ac_CH4_All_trend-predict(Eq_CH4_All)
Ac_CH4_All_detrend<-Ac_CH4_All_detrend+mean(Ac_CH4_All_trend)

#G4301
Ac_CH4_BP_trend<-Ac_CH4_BP$CH4_dry[43000:46100] #Select interval with "consistent" trend ~linear
Eq_CH4_BP<-lm(formula = Ac_CH4_BP_trend ~ poly(as.numeric(x_Tc),18,raw=TRUE)) #polynomial
Coeff_CH4_BP<-coefficients(Eq_CH4_BP)
Ac_CH4_BP_detrend<-Ac_CH4_BP_trend-predict(Eq_CH4_BP)
Ac_CH4_BP_detrend<-Ac_CH4_BP_detrend+mean(Ac_CH4_BP_trend)

##Control plots-->make sure detrend works
Cp<-ggplot()+geom_point(aes(anytime(Ac_CH4_All$date.time[59000:65000]),Ac_CH4_All_trend,color="original"))+
  geom_line(aes(anytime(Ac_CH4_All$date.time[59000:65000]),predict(Eq_CH4_All),color="Fit"))+
  geom_point(aes(anytime(Ac_CH4_All$date.time[59000:65000]),Ac_CH4_All_detrend,color="detrended"))
ggsave(plot=Cp,"Detrend control_withAc_G2509.png", width = 8, height = 5)

Cp<-ggplot()+geom_point(aes(anytime(Ac_CH4_BP$date.time[43000:46100]),Ac_CH4_BP_trend,color="original"))+
  geom_line(aes(anytime(Ac_CH4_BP$date.time[43000:46100]),predict(Eq_CH4_BP),color="Fit"))+
  geom_point(aes(anytime(Ac_CH4_BP$date.time[43000:46100]),Ac_CH4_BP_detrend,color="detrended"))
ggsave(plot=Cp,"Detrend control_withAc_G4301.png", width = 8, height = 5)

### Methanol
# G2509
x_T<-1:length(MeOH_CH4_All$CH4_dry[54000:59000]) ##Needed to make the polynomic function work
x_Tc<-1:length(MeOH_CH4_All$CH4_dry[39100:43000]) ##Needed to make the polynomic function work

MeOH_CH4_All_trend<-MeOH_CH4_All$CH4_dry[54000:59000] #Select interval with "consistent" trend ~linear
Eq_CH4_All<-lm(formula = MeOH_CH4_All_trend ~ poly(as.numeric(x_T),18,raw=TRUE)) #polynomial
Coeff_CH4_All<-coefficients(Eq_CH4_All)
MeOH_CH4_All_detrend<-MeOH_CH4_All_trend-predict(Eq_CH4_All)
MeOH_CH4_All_detrend<-MeOH_CH4_All_detrend+mean(MeOH_CH4_All_trend)

#G4301
MeOH_CH4_BP_trend<-MeOH_CH4_BP$CH4_dry[39100:43000] #Select interval with "consistent" trend ~linear
Eq_CH4_BP<-lm(formula = MeOH_CH4_BP_trend ~ poly(as.numeric(x_Tc),18,raw=TRUE)) #polynomial
Coeff_CH4_BP<-coefficients(Eq_CH4_BP)
MeOH_CH4_BP_detrend<-MeOH_CH4_BP_trend-predict(Eq_CH4_BP)
MeOH_CH4_BP_detrend<-MeOH_CH4_BP_detrend+mean(MeOH_CH4_BP_trend)

##Control plots-->make sure detrend works
Cp<-ggplot()+geom_point(aes(anytime(MeOH_CH4_All$date.time[54000:59000]),MeOH_CH4_All_trend,color="original"))+
  geom_line(aes(anytime(MeOH_CH4_All$date.time[54000:59000]),predict(Eq_CH4_All),color="Fit"))+
  geom_point(aes(anytime(MeOH_CH4_All$date.time[54000:59000]),MeOH_CH4_All_detrend,color="detrended"))
ggsave(plot=Cp,"Detrend control_withMeOH_G2509.png", width = 8, height = 5)

Cp<-ggplot()+geom_point(aes(anytime(MeOH_CH4_BP$date.time[39100:43000]),MeOH_CH4_BP_trend,color="original"))+
  geom_line(aes(anytime(MeOH_CH4_BP$date.time[39100:43000]),predict(Eq_CH4_BP),color="Fit"))+
  geom_point(aes(anytime(MeOH_CH4_BP$date.time[39100:43000]),MeOH_CH4_BP_detrend,color="detrended"))
ggsave(plot=Cp,"Detrend control_withMeOH_G4301.png", width = 8, height = 5)


### Propionic acid
# G2509
x_T<-1:length(PAc_CH4_All$CH4_dry[41200:45000]) ##Needed to make the polynomic function work

PAc_CH4_All_trend<-PAc_CH4_All$CH4_dry[41200:45000] #Select interval with "consistent" trend ~linear
Eq_CH4_All<-lm(formula = PAc_CH4_All_trend ~ poly(as.numeric(x_T),18,raw=TRUE)) #polynomial
Coeff_CH4_All<-coefficients(Eq_CH4_All)
PAc_CH4_All_detrend<-PAc_CH4_All_trend-predict(Eq_CH4_All)
PAc_CH4_All_detrend<-PAc_CH4_All_detrend+mean(PAc_CH4_All_trend)



##Control plots-->make sure detrend works
Cp<-ggplot()+geom_point(aes(anytime(PAc_CH4_All$date.time[41200:45000]),PAc_CH4_All_trend,color="original"))+
  geom_line(aes(anytime(PAc_CH4_All$date.time[41200:45000]),predict(Eq_CH4_All),color="Fit"))+
  geom_point(aes(anytime(PAc_CH4_All$date.time[41200:45000]),PAc_CH4_All_detrend,color="detrended"))
ggsave(plot=Cp,"Detrend control_withPAc_G2509.png", width = 8, height = 5)



### Calculate new means ###

### Ethanol 

## Make the new detrended CH4 have the same positions as the original data
Empty<-rep(NaN,29999) #Same positions as in original data so is easier to calculate the means
CH4_own_new<-c(Empty,CH4_own_detrend) #CH4 detrended
CH4_All_new<-c(Empty,CH4_All_detrend)
Control_own_new<-c(Empty,CH4_own_trend) #Control with old CH4 values to make sure are the same as original data
Control_All_new<-c(Empty,CH4_All_trend)

CH4_All_new<-CH4_All_new[Eth_Range$tAc[1]:Eth_Range$tAc[2]]
CH4_own_new<-CH4_own_new[Eth_Range$tAco[1]:Eth_Range$tAco[2]]
Control_All_new<-Control_All_new[Eth_Range$tAc[1]:Eth_Range$tAc[2]]
Control_own_new<-Control_own_new[Eth_Range$tAco[1]:Eth_Range$tAco[2]]

##Re calculate the intervals for detrended CH4##
## G2509 borrowed
m_CH4_All<-1
sd_CH4_All<-1
for (i in 1:(nrow(Eth_G2508b_int)-1)){
  m_CH4_All[i]<-mean(CH4_All_new[Eth_G2508b_int$x[i]:Eth_G2508b_int$x[i+1]])
  sd_CH4_All[i]<-sd(CH4_All_new[Eth_G2508b_int$x[i]:Eth_G2508b_int$x[i+1]])
}
#Only odd positions are correct (Even positions are from t(2)to t(3))
m_CH4_All_Eth_detrend<-m_CH4_All[c(TRUE,FALSE)]
sd_CH4_All_Eth_detrend<-sd_CH4_All[c(TRUE,FALSE)]
## G2509 own
m_CH4_own<-1
sd_CH4_own<-1
for (i in 1:(nrow(Eth_G2508o_int)-1)){
  m_CH4_own[i]<-mean(CH4_own_new[Eth_G2508o_int$x[i]:Eth_G2508o_int$x[i+1]])
  sd_CH4_own[i]<-sd(CH4_own_new[Eth_G2508o_int$x[i]:Eth_G2508o_int$x[i+1]])
}
#Only odd positions are correct (Even positions are from t(2)to t(3))
m_CH4_own_Eth_detrend<-m_CH4_own[c(TRUE,FALSE)]
sd_CH4_own_Eth_detrend<-sd_CH4_own[c(TRUE,FALSE)]

## Control
m_Control_All<-1
sd_Control_All<-1
for (i in 1:(nrow(Eth_G2508b_int)-1)){
  m_Control_All[i]<-mean(Control_All_new[Eth_G2508b_int$x[i]:Eth_G2508b_int$x[i+1]])
  sd_Control_All[i]<-sd(Control_All_new[Eth_G2508b_int$x[i]:Eth_G2508b_int$x[i+1]])
}
m_Control_All_Eth_detrend<-m_Control_All[c(TRUE,FALSE)]
sd_Control_All_Eth_detrend<-sd_Control_All[c(TRUE,FALSE)]

m_Control_own<-1
sd_Control_own<-1
for (i in 1:(nrow(Eth_G2508o_int)-1)){
  m_Control_own[i]<-mean(Control_own_new[Eth_G2508o_int$x[i]:Eth_G2508o_int$x[i+1]])
  sd_Control_own[i]<-sd(Control_own_new[Eth_G2508o_int$x[i]:Eth_G2508o_int$x[i+1]])
}
m_Control_own_Eth_detrend<-m_Control_own[c(TRUE,FALSE)]
sd_Control_own_Eth_detrend<-sd_Control_own[c(TRUE,FALSE)]

## Control data frame to make sure intervals are the same
Mean_Control<-m_Control_All_Eth_detrend-Eth_CH4$y_CH4[1:10]+m_Control_own_Eth_detrend-Eth_CH4$y_CH4[11:20]
sd_Control<-sd_Control_All_Eth_detrend-Eth_CH4$sd_CH4[1:10]+sd_Control_own_Eth_detrend-Eth_CH4$sd_CH4[11:20]

df_EtOH_Control<-cbind.data.frame(m_Control_All_Eth_detrend,sd_Control_All_Eth_detrend,
                                  m_Control_own_Eth_detrend,sd_Control_own_Eth_detrend,
                                  Eth_CH4$y_CH4[1:10],Eth_CH4$sd_CH4[1:10],
                                  Eth_CH4$y_CH4[11:20],Eth_CH4$sd_CH4[11:20],
                                  Mean_Control,sd_Control)


m_Eth_CH4_detrended<-c(m_CH4_All_Eth_detrend,m_CH4_own_Eth_detrend)
sd_Eth_CH4_detrended<-c(sd_CH4_All_Eth_detrend,sd_CH4_own_Eth_detrend)

##Merge all in one data frame
x<-c(Eth_NH3$x_NH3,Eth_CH4$x_CH4,Eth_N2O$x_N2O)
y<-c(Eth_NH3$y_NH3,m_Eth_CH4_detrended*1000,Eth_N2O$y_N2O*1000)
sd<-c(Eth_NH3$sd_NH3,sd_Eth_CH4_detrended*1000,Eth_N2O$sd_N2O*1000)
CRDS<-c(Eth_NH3$Color_NH3,Eth_CH4$Color_CH4,Eth_N2O$Color_N2O)
Compound<-c(rep("NH3",30),rep("CH4",20),rep("N2O",20))


df_EtOH<-cbind.data.frame(x,y,sd,CRDS,Compound)

my_labeller <- as_labeller(c("NH3"="NH[3]", "CH4"="CH[4]", "N2O"="N[2]*O"),
                           default = label_parsed)
my_labeller_blank <- as_labeller(c("NH3"="", "CH4"="", "N2O"=""),
                                 default = label_parsed)

### Butyric acid 

## Make the new detrended CH4 have the same positions as the original data
Empty<-rep(NaN,64999) #Same positions as in original data so is easier to calculate the means
BAc_CH4_All_new<-c(Empty,BAc_CH4_All_detrend)
BAc_Control_All_new<-c(Empty,BAc_CH4_All_trend)
Empty<-rep(NaN,45999) #Same positions as in original data so is easier to calculate the means
BAc_CH4_BP_new<-c(Empty,BAc_CH4_BP_detrend) #CH4 detrended
BAc_Control_BP_new<-c(Empty,BAc_CH4_BP_trend) #Control with old CH4 values to make sure are the same as original data

BAc_CH4_All_new<-BAc_CH4_All_new[BAc_Range$tAc[1]:BAc_Range$tAc[2]]
BAc_CH4_BP_new<-BAc_CH4_BP_new[BAc_Range$tAcc[1]:BAc_Range$tAcc[2]]
BAc_Control_All_new<-BAc_Control_All_new[BAc_Range$tAc[1]:BAc_Range$tAc[2]]
BAc_Control_BP_new<-BAc_Control_BP_new[BAc_Range$tAcc[1]:BAc_Range$tAcc[2]]

##Re calculate the intervals for detrended CH4##
## G2509 borrowed
m_CH4_All<-1
sd_CH4_All<-1
for (i in 1:(nrow(BAc_G2509_int)-1)){
  m_CH4_All[i]<-mean(BAc_CH4_All_new[BAc_G2509_int$x[i]:BAc_G2509_int$x[i+1]])
  sd_CH4_All[i]<-sd(BAc_CH4_All_new[BAc_G2509_int$x[i]:BAc_G2509_int$x[i+1]])
}
#Only odd positions are correct (Even positions are from t(2)to t(3))
m_CH4_All_BAc_detrend<-m_CH4_All[c(TRUE,FALSE)]
sd_CH4_All_BAc_detrend<-sd_CH4_All[c(TRUE,FALSE)]
## G4301
m_CH4_BP<-1
sd_CH4_BP<-1
for (i in 1:(nrow(BAc_G4301_int)-1)){
  m_CH4_BP[i]<-mean(BAc_CH4_BP_new[BAc_G4301_int$x[i]:BAc_G4301_int$x[i+1]])
  sd_CH4_BP[i]<-sd(BAc_CH4_BP_new[BAc_G4301_int$x[i]:BAc_G4301_int$x[i+1]])
}
#Only odd positions are correct (Even positions are from t(2)to t(3))
m_CH4_BP_BAc_detrend<-m_CH4_BP[c(TRUE,FALSE)]
sd_CH4_BP_BAc_detrend<-sd_CH4_BP[c(TRUE,FALSE)]

## Control
m_Control_All<-1
sd_Control_All<-1
for (i in 1:(nrow(BAc_G2509_int)-1)){
  m_Control_All[i]<-mean(BAc_Control_All_new[BAc_G2509_int$x[i]:BAc_G2509_int$x[i+1]])
  sd_Control_All[i]<-sd(BAc_Control_All_new[BAc_G2509_int$x[i]:BAc_G2509_int$x[i+1]])
}
m_Control_All_BAc_detrend<-m_Control_All[c(TRUE,FALSE)]
sd_Control_All_BAc_detrend<-sd_Control_All[c(TRUE,FALSE)]

m_Control_BP<-1
sd_Control_BP<-1
for (i in 1:(nrow(BAc_G4301_int)-1)){
  m_Control_BP[i]<-mean(BAc_Control_BP_new[BAc_G4301_int$x[i]:BAc_G4301_int$x[i+1]])
  sd_Control_BP[i]<-sd(BAc_Control_BP_new[BAc_G4301_int$x[i]:BAc_G4301_int$x[i+1]])
}
m_Control_BP_BAc_detrend<-m_Control_BP[c(TRUE,FALSE)]
sd_Control_BP_BAc_detrend<-sd_Control_BP[c(TRUE,FALSE)]

## Control data frame to make sure intervals are the same
Mean_Control<-m_Control_All_BAc_detrend-BAc_CH4$y_CH4[1:10]+m_Control_BP_BAc_detrend-BAc_CH4$y_CH4[11:20]
sd_Control<-sd_Control_All_BAc_detrend-BAc_CH4$sd_CH4[1:10]+sd_Control_BP_BAc_detrend-BAc_CH4$sd_CH4[11:20]

df_BAc_Control<-cbind.data.frame(m_Control_All_BAc_detrend,sd_Control_All_BAc_detrend,
                                 m_Control_BP_BAc_detrend,sd_Control_BP_BAc_detrend,
                                 BAc_CH4$y_CH4[1:10],BAc_CH4$sd_CH4[1:10],
                                 BAc_CH4$y_CH4[11:20],BAc_CH4$sd_CH4[11:20],
                                 Mean_Control,sd_Control)


m_BAc_CH4_detrended<-c(m_CH4_All_BAc_detrend,m_CH4_BP_BAc_detrend)
sd_BAc_CH4_detrended<-c(sd_CH4_All_BAc_detrend,sd_CH4_BP_BAc_detrend)

##Merge all in one data frame
x<-c(BAc_NH3$x_NH3,BAc_CH4$x_CH4,BAc_N2O$x_N2O)
y<-c(BAc_NH3$y_NH3,m_BAc_CH4_detrended*1000,BAc_N2O$y_N2O*1000)
sd<-c(BAc_NH3$sd_NH3,sd_BAc_CH4_detrended*1000,BAc_N2O$sd_N2O*1000)
CRDS<-c(BAc_NH3$Color_NH3,BAc_CH4$Color_CH4,BAc_N2O$Color_N2O)
Compound<-c(rep("NH3",20),rep("CH4",20),rep("N2O",10))

df_BAc<-cbind.data.frame(x,y,sd,CRDS,Compound)

### Diacetyl 

## Make the new detrended CH4 have the same positions as the original data
Empty<-rep(NaN,60999) #Same positions as in original data so is easier to calculate the means
DA_CH4_All_new<-c(Empty,DA_CH4_All_detrend)
DA_Control_All_new<-c(Empty,DA_CH4_All_trend)
Empty<-rep(NaN,44499) #Same positions as in original data so is easier to calculate the means
DA_CH4_BP_new<-c(Empty,DA_CH4_BP_detrend) #CH4 detrended
DA_Control_BP_new<-c(Empty,DA_CH4_BP_trend) #Control with old CH4 values to make sure are the same as original data

DA_CH4_All_new<-DA_CH4_All_new[DA_Range$tAc[1]:DA_Range$tAc[2]]
DA_CH4_BP_new<-DA_CH4_BP_new[DA_Range$tAcc[1]:DA_Range$tAcc[2]]
DA_Control_All_new<-DA_Control_All_new[DA_Range$tAc[1]:DA_Range$tAc[2]]
DA_Control_BP_new<-DA_Control_BP_new[DA_Range$tAcc[1]:DA_Range$tAcc[2]]

##Re calculate the intervals for detrended CH4##
## G2509 borrowed
m_CH4_All<-1
sd_CH4_All<-1
for (i in 1:(nrow(DA_G2509_int)-1)){
  m_CH4_All[i]<-mean(DA_CH4_All_new[DA_G2509_int$x[i]:DA_G2509_int$x[i+1]])
  sd_CH4_All[i]<-sd(DA_CH4_All_new[DA_G2509_int$x[i]:DA_G2509_int$x[i+1]])
}
#Only odd positions are correct (Even positions are from t(2)to t(3))
m_CH4_All_DA_detrend<-m_CH4_All[c(TRUE,FALSE)]
sd_CH4_All_DA_detrend<-sd_CH4_All[c(TRUE,FALSE)]
## G4301
m_CH4_BP<-1
sd_CH4_BP<-1
for (i in 1:(nrow(DA_G4301_int)-1)){
  m_CH4_BP[i]<-mean(DA_CH4_BP_new[DA_G4301_int$x[i]:DA_G4301_int$x[i+1]])
  sd_CH4_BP[i]<-sd(DA_CH4_BP_new[DA_G4301_int$x[i]:DA_G4301_int$x[i+1]])
}
#Only odd positions are correct (Even positions are from t(2)to t(3))
m_CH4_BP_DA_detrend<-m_CH4_BP[c(TRUE,FALSE)]
sd_CH4_BP_DA_detrend<-sd_CH4_BP[c(TRUE,FALSE)]

## Control
m_Control_All<-1
sd_Control_All<-1
for (i in 1:(nrow(DA_G2509_int)-1)){
  m_Control_All[i]<-mean(DA_Control_All_new[DA_G2509_int$x[i]:DA_G2509_int$x[i+1]])
  sd_Control_All[i]<-sd(DA_Control_All_new[DA_G2509_int$x[i]:DA_G2509_int$x[i+1]])
}
m_Control_All_DA_detrend<-m_Control_All[c(TRUE,FALSE)]
sd_Control_All_DA_detrend<-sd_Control_All[c(TRUE,FALSE)]

m_Control_BP<-1
sd_Control_BP<-1
for (i in 1:(nrow(DA_G4301_int)-1)){
  m_Control_BP[i]<-mean(DA_Control_BP_new[DA_G4301_int$x[i]:DA_G4301_int$x[i+1]])
  sd_Control_BP[i]<-sd(DA_Control_BP_new[DA_G4301_int$x[i]:DA_G4301_int$x[i+1]])
}
m_Control_BP_DA_detrend<-m_Control_BP[c(TRUE,FALSE)]
sd_Control_BP_DA_detrend<-sd_Control_BP[c(TRUE,FALSE)]

## Control data frame to make sure intervals are the same
Mean_Control<-m_Control_All_DA_detrend-DA_CH4$y_CH4[1:12]+m_Control_BP_DA_detrend-DA_CH4$y_CH4[13:24]
sd_Control<-sd_Control_All_DA_detrend-DA_CH4$sd_CH4[1:12]+sd_Control_BP_DA_detrend-DA_CH4$sd_CH4[13:24]

df_DA_Control<-cbind.data.frame(m_Control_All_DA_detrend,sd_Control_All_DA_detrend,
                                m_Control_BP_DA_detrend,sd_Control_BP_DA_detrend,
                                DA_CH4$y_CH4[1:12],DA_CH4$sd_CH4[1:12],
                                DA_CH4$y_CH4[13:24],DA_CH4$sd_CH4[13:24],
                                Mean_Control,sd_Control)


m_DA_CH4_detrended<-c(m_CH4_All_DA_detrend,m_CH4_BP_DA_detrend)
sd_DA_CH4_detrended<-c(sd_CH4_All_DA_detrend,sd_CH4_BP_DA_detrend)

##Merge all in one data frame
x<-c(DA_NH3$x_NH3,DA_CH4$x_CH4,DA_N2O$x_N2O)
y<-c(DA_NH3$y_NH3,m_DA_CH4_detrended*1000,DA_N2O$y_N2O*1000)
sd<-c(DA_NH3$sd_NH3,sd_DA_CH4_detrended*1000,DA_N2O$sd_N2O*1000)
CRDS<-c(DA_NH3$Color_NH3,DA_CH4$Color_CH4,DA_N2O$Color_N2O)
Compound<-c(rep("NH3",24),rep("CH4",24),rep("N2O",12))

df_DA<-cbind.data.frame(x,y,sd,CRDS,Compound)


### Butanone 

## Make the new detrended CH4 have the same positions as the original data
Empty<-rep(NaN,55499) #Same positions as in original data so is easier to calculate the means
Bne_CH4_All_new<-c(Empty,Bne_CH4_All_detrend)
Bne_Control_All_new<-c(Empty,Bne_CH4_All_trend)
Empty<-rep(NaN,40599) #Same positions as in original data so is easier to calculate the means
Bne_CH4_BP_new<-c(Empty,Bne_CH4_BP_detrend) #CH4 detrended
Bne_Control_BP_new<-c(Empty,Bne_CH4_BP_trend) #Control with old CH4 values to make sure are the same as original data

Bne_CH4_All_new<-Bne_CH4_All_new[Bne_Range$tAc[1]:Bne_Range$tAc[2]]
Bne_CH4_BP_new<-Bne_CH4_BP_new[Bne_Range$tAcc[1]:Bne_Range$tAcc[2]]
Bne_Control_All_new<-Bne_Control_All_new[Bne_Range$tAc[1]:Bne_Range$tAc[2]]
Bne_Control_BP_new<-Bne_Control_BP_new[Bne_Range$tAcc[1]:Bne_Range$tAcc[2]]

##Re calculate the intervals for detrended CH4##
## G2509 borrowed
m_CH4_All<-1
sd_CH4_All<-1
for (i in 1:(nrow(Bne_G2509_int)-1)){
  m_CH4_All[i]<-mean(Bne_CH4_All_new[Bne_G2509_int$x[i]:Bne_G2509_int$x[i+1]])
  sd_CH4_All[i]<-sd(Bne_CH4_All_new[Bne_G2509_int$x[i]:Bne_G2509_int$x[i+1]])
}
#Only odd positions are correct (Even positions are from t(2)to t(3))
m_CH4_All_Bne_detrend<-m_CH4_All[c(TRUE,FALSE)]
sd_CH4_All_Bne_detrend<-sd_CH4_All[c(TRUE,FALSE)]
## G4301
m_CH4_BP<-1
sd_CH4_BP<-1
for (i in 1:(nrow(Bne_G4301_int)-1)){
  m_CH4_BP[i]<-mean(Bne_CH4_BP_new[Bne_G4301_int$x[i]:Bne_G4301_int$x[i+1]])
  sd_CH4_BP[i]<-sd(Bne_CH4_BP_new[Bne_G4301_int$x[i]:Bne_G4301_int$x[i+1]])
}
#Only odd positions are correct (Even positions are from t(2)to t(3))
m_CH4_BP_Bne_detrend<-m_CH4_BP[c(TRUE,FALSE)]
sd_CH4_BP_Bne_detrend<-sd_CH4_BP[c(TRUE,FALSE)]

## Control
m_Control_All<-1
sd_Control_All<-1
for (i in 1:(nrow(Bne_G2509_int)-1)){
  m_Control_All[i]<-mean(Bne_Control_All_new[Bne_G2509_int$x[i]:Bne_G2509_int$x[i+1]])
  sd_Control_All[i]<-sd(Bne_Control_All_new[Bne_G2509_int$x[i]:Bne_G2509_int$x[i+1]])
}
m_Control_All_Bne_detrend<-m_Control_All[c(TRUE,FALSE)]
sd_Control_All_Bne_detrend<-sd_Control_All[c(TRUE,FALSE)]

m_Control_BP<-1
sd_Control_BP<-1
for (i in 1:(nrow(Bne_G4301_int)-1)){
  m_Control_BP[i]<-mean(Bne_Control_BP_new[Bne_G4301_int$x[i]:Bne_G4301_int$x[i+1]])
  sd_Control_BP[i]<-sd(Bne_Control_BP_new[Bne_G4301_int$x[i]:Bne_G4301_int$x[i+1]])
}
m_Control_BP_Bne_detrend<-m_Control_BP[c(TRUE,FALSE)]
sd_Control_BP_Bne_detrend<-sd_Control_BP[c(TRUE,FALSE)]

## Control data frame to make sure intervals are the same
Mean_Control<-m_Control_All_Bne_detrend-Bne_CH4$y_CH4[1:10]+m_Control_BP_Bne_detrend-Bne_CH4$y_CH4[11:20]
sd_Control<-sd_Control_All_Bne_detrend-Bne_CH4$sd_CH4[1:10]+sd_Control_BP_Bne_detrend-Bne_CH4$sd_CH4[11:20]

df_Bne_Control<-cbind.data.frame(m_Control_All_Bne_detrend,sd_Control_All_Bne_detrend,
                                 m_Control_BP_Bne_detrend,sd_Control_BP_Bne_detrend,
                                 Bne_CH4$y_CH4[1:10],Bne_CH4$sd_CH4[1:10],
                                 Bne_CH4$y_CH4[11:20],Bne_CH4$sd_CH4[11:20],
                                 Mean_Control,sd_Control)


m_Bne_CH4_detrended<-c(m_CH4_All_Bne_detrend,m_CH4_BP_Bne_detrend)
sd_Bne_CH4_detrended<-c(sd_CH4_All_Bne_detrend,sd_CH4_BP_Bne_detrend)

##Merge all in one data frame
x<-c(Bne_NH3$x_NH3,Bne_CH4$x_CH4,Bne_N2O$x_N2O)
y<-c(Bne_NH3$y_NH3,m_Bne_CH4_detrended*1000,Bne_N2O$y_N2O*1000)
sd<-c(Bne_NH3$sd_NH3,sd_Bne_CH4_detrended*1000,Bne_N2O$sd_N2O*1000)
CRDS<-c(Bne_NH3$Color_NH3,Bne_CH4$Color_CH4,Bne_N2O$Color_N2O)
Compound<-c(rep("NH3",20),rep("CH4",20),rep("N2O",10))

df_Bne<-cbind.data.frame(x,y,sd,CRDS,Compound)


### 1-Butanol 

## Make the new detrended CH4 have the same positions as the original data
Empty<-rep(NaN,35499) #Same positions as in original data so is easier to calculate the means
B1_CH4_All_new<-c(Empty,B1_CH4_All_detrend)
B1_Control_All_new<-c(Empty,B1_CH4_All_trend)
Empty<-rep(NaN,25499) #Same positions as in original data so is easier to calculate the means
B1_CH4_BP_new<-c(Empty,B1_CH4_BP_detrend) #CH4 detrended
B1_Control_BP_new<-c(Empty,B1_CH4_BP_trend) #Control with old CH4 values to make sure are the same as original data

B1_CH4_All_new<-B1_CH4_All_new[B1_Range$tAc[1]:B1_Range$tAc[2]]
B1_CH4_BP_new<-B1_CH4_BP_new[B1_Range$tAcc[1]:B1_Range$tAcc[2]]
B1_Control_All_new<-B1_Control_All_new[B1_Range$tAc[1]:B1_Range$tAc[2]]
B1_Control_BP_new<-B1_Control_BP_new[B1_Range$tAcc[1]:B1_Range$tAcc[2]]

##Re calculate the intervals for detrended CH4##
## G2509 borrowed
m_CH4_All<-1
sd_CH4_All<-1
for (i in 1:(nrow(B1_G2509_int)-1)){
  m_CH4_All[i]<-mean(B1_CH4_All_new[B1_G2509_int$x[i]:B1_G2509_int$x[i+1]])
  sd_CH4_All[i]<-sd(B1_CH4_All_new[B1_G2509_int$x[i]:B1_G2509_int$x[i+1]])
}
#Only odd positions are correct (Even positions are from t(2)to t(3))
m_CH4_All_B1_detrend<-m_CH4_All[c(TRUE,FALSE)]
sd_CH4_All_B1_detrend<-sd_CH4_All[c(TRUE,FALSE)]
## G4301
m_CH4_BP<-1
sd_CH4_BP<-1
for (i in 1:(nrow(B1_G4301_int)-1)){
  m_CH4_BP[i]<-mean(B1_CH4_BP_new[B1_G4301_int$x[i]:B1_G4301_int$x[i+1]])
  sd_CH4_BP[i]<-sd(B1_CH4_BP_new[B1_G4301_int$x[i]:B1_G4301_int$x[i+1]])
}
#Only odd positions are correct (Even positions are from t(2)to t(3))
m_CH4_BP_B1_detrend<-m_CH4_BP[c(TRUE,FALSE)]
sd_CH4_BP_B1_detrend<-sd_CH4_BP[c(TRUE,FALSE)]

## Control
m_Control_All<-1
sd_Control_All<-1
for (i in 1:(nrow(B1_G2509_int)-1)){
  m_Control_All[i]<-mean(B1_Control_All_new[B1_G2509_int$x[i]:B1_G2509_int$x[i+1]])
  sd_Control_All[i]<-sd(B1_Control_All_new[B1_G2509_int$x[i]:B1_G2509_int$x[i+1]])
}
m_Control_All_B1_detrend<-m_Control_All[c(TRUE,FALSE)]
sd_Control_All_B1_detrend<-sd_Control_All[c(TRUE,FALSE)]

m_Control_BP<-1
sd_Control_BP<-1
for (i in 1:(nrow(B1_G4301_int)-1)){
  m_Control_BP[i]<-mean(B1_Control_BP_new[B1_G4301_int$x[i]:B1_G4301_int$x[i+1]])
  sd_Control_BP[i]<-sd(B1_Control_BP_new[B1_G4301_int$x[i]:B1_G4301_int$x[i+1]])
}
m_Control_BP_B1_detrend<-m_Control_BP[c(TRUE,FALSE)]
sd_Control_BP_B1_detrend<-sd_Control_BP[c(TRUE,FALSE)]

## Control data frame to make sure intervals are the same
Mean_Control<-m_Control_All_B1_detrend-B1_CH4$y_CH4[1:12]+m_Control_BP_B1_detrend-B1_CH4$y_CH4[13:24]
sd_Control<-sd_Control_All_B1_detrend-B1_CH4$sd_CH4[1:12]+sd_Control_BP_B1_detrend-B1_CH4$sd_CH4[13:24]

df_B1_Control<-cbind.data.frame(m_Control_All_B1_detrend,sd_Control_All_B1_detrend,
                                m_Control_BP_B1_detrend,sd_Control_BP_B1_detrend,
                                B1_CH4$y_CH4[1:12],B1_CH4$sd_CH4[1:12],
                                B1_CH4$y_CH4[13:24],B1_CH4$sd_CH4[13:24],
                                Mean_Control,sd_Control)


m_B1_CH4_detrended<-c(m_CH4_All_B1_detrend,m_CH4_BP_B1_detrend)
sd_B1_CH4_detrended<-c(sd_CH4_All_B1_detrend,sd_CH4_BP_B1_detrend)

##Merge all in one data frame
x<-c(B1_NH3$x_NH3,B1_CH4$x_CH4,B1_N2O$x_N2O)
y<-c(B1_NH3$y_NH3,m_B1_CH4_detrended*1000,B1_N2O$y_N2O*1000)
sd<-c(B1_NH3$sd_NH3,sd_B1_CH4_detrended*1000,B1_N2O$sd_N2O*1000)
CRDS<-c(B1_NH3$Color_NH3,B1_CH4$Color_CH4,B1_N2O$Color_N2O)
Compound<-c(rep("NH3",24),rep("CH4",24),rep("N2O",12))

df_B1<-cbind.data.frame(x,y,sd,CRDS,Compound)

### 1-Propanol 

## Make the new detrended CH4 have the same positions as the original data
Empty<-rep(NaN,25499) #Same positions as in original data so is easier to calculate the means
P1_CH4_All_new<-c(Empty,P1_CH4_All_detrend)
P1_Control_All_new<-c(Empty,P1_CH4_All_trend)
Empty<-rep(NaN,20499) #Same positions as in original data so is easier to calculate the means
P1_CH4_BP_new<-c(Empty,P1_CH4_BP_detrend) #CH4 detrended
P1_Control_BP_new<-c(Empty,P1_CH4_BP_trend) #Control with old CH4 values to make sure are the same as original data

P1_CH4_All_new<-P1_CH4_All_new[P1_Range$tAc[1]:P1_Range$tAc[2]]
P1_CH4_BP_new<-P1_CH4_BP_new[P1_Range$tAcc[1]:P1_Range$tAcc[2]]
P1_Control_All_new<-P1_Control_All_new[P1_Range$tAc[1]:P1_Range$tAc[2]]
P1_Control_BP_new<-P1_Control_BP_new[P1_Range$tAcc[1]:P1_Range$tAcc[2]]

##Re calculate the intervals for detrended CH4##
## G2509 borrowed
m_CH4_All<-1
sd_CH4_All<-1
for (i in 1:(nrow(P1_G2509_int)-1)){
  m_CH4_All[i]<-mean(P1_CH4_All_new[P1_G2509_int$x[i]:P1_G2509_int$x[i+1]])
  sd_CH4_All[i]<-sd(P1_CH4_All_new[P1_G2509_int$x[i]:P1_G2509_int$x[i+1]])
}
#Only odd positions are correct (Even positions are from t(2)to t(3))
m_CH4_All_P1_detrend<-m_CH4_All[c(TRUE,FALSE)]
sd_CH4_All_P1_detrend<-sd_CH4_All[c(TRUE,FALSE)]
## G4301
m_CH4_BP<-1
sd_CH4_BP<-1
for (i in 1:(nrow(P1_G4301_int)-1)){
  m_CH4_BP[i]<-mean(P1_CH4_BP_new[P1_G4301_int$x[i]:P1_G4301_int$x[i+1]])
  sd_CH4_BP[i]<-sd(P1_CH4_BP_new[P1_G4301_int$x[i]:P1_G4301_int$x[i+1]])
}
#Only odd positions are correct (Even positions are from t(2)to t(3))
m_CH4_BP_P1_detrend<-m_CH4_BP[c(TRUE,FALSE)]
sd_CH4_BP_P1_detrend<-sd_CH4_BP[c(TRUE,FALSE)]

## Control
m_Control_All<-1
sd_Control_All<-1
for (i in 1:(nrow(P1_G2509_int)-1)){
  m_Control_All[i]<-mean(P1_Control_All_new[P1_G2509_int$x[i]:P1_G2509_int$x[i+1]])
  sd_Control_All[i]<-sd(P1_Control_All_new[P1_G2509_int$x[i]:P1_G2509_int$x[i+1]])
}
m_Control_All_P1_detrend<-m_Control_All[c(TRUE,FALSE)]
sd_Control_All_P1_detrend<-sd_Control_All[c(TRUE,FALSE)]

m_Control_BP<-1
sd_Control_BP<-1
for (i in 1:(nrow(P1_G4301_int)-1)){
  m_Control_BP[i]<-mean(P1_Control_BP_new[P1_G4301_int$x[i]:P1_G4301_int$x[i+1]])
  sd_Control_BP[i]<-sd(P1_Control_BP_new[P1_G4301_int$x[i]:P1_G4301_int$x[i+1]])
}
m_Control_BP_P1_detrend<-m_Control_BP[c(TRUE,FALSE)]
sd_Control_BP_P1_detrend<-sd_Control_BP[c(TRUE,FALSE)]

## Control data frame to make sure intervals are the same
Mean_Control<-m_Control_All_P1_detrend-P1_CH4$y_CH4[1:14]+m_Control_BP_P1_detrend-P1_CH4$y_CH4[15:28]
sd_Control<-sd_Control_All_P1_detrend-P1_CH4$sd_CH4[1:14]+sd_Control_BP_P1_detrend-P1_CH4$sd_CH4[15:28]

df_P1_Control<-cbind.data.frame(m_Control_All_P1_detrend,sd_Control_All_P1_detrend,
                                m_Control_BP_P1_detrend,sd_Control_BP_P1_detrend,
                                P1_CH4$y_CH4[1:14],P1_CH4$sd_CH4[1:14],
                                P1_CH4$y_CH4[15:28],P1_CH4$sd_CH4[15:28],
                                Mean_Control,sd_Control)


m_P1_CH4_detrended<-c(m_CH4_All_P1_detrend,m_CH4_BP_P1_detrend)
sd_P1_CH4_detrended<-c(sd_CH4_All_P1_detrend,sd_CH4_BP_P1_detrend)

##Merge all in one data frame
x<-c(P1_NH3$x_NH3,P1_CH4$x_CH4,P1_N2O$x_N2O)
y<-c(P1_NH3$y_NH3,m_P1_CH4_detrended*1000,P1_N2O$y_N2O*1000)
sd<-c(P1_NH3$sd_NH3,sd_P1_CH4_detrended*1000,P1_N2O$sd_N2O*1000)
CRDS<-c(P1_NH3$Color_NH3,P1_CH4$Color_CH4,P1_N2O$Color_N2O)
Compound<-c(rep("NH3",28),rep("CH4",28),rep("N2O",14))

df_P1<-cbind.data.frame(x,y,sd,CRDS,Compound)


### 2-Propanol 

## Make the new detrended CH4 have the same positions as the original data
Empty<-rep(NaN,63499) #Same positions as in original data so is easier to calculate the means
P2_CH4_All_new<-c(Empty,P2_CH4_All_detrend)
P2_Control_All_new<-c(Empty,P2_CH4_All_trend)
Empty<-rep(NaN,45829) #Same positions as in original data so is easier to calculate the means
P2_CH4_BP_new<-c(Empty,P2_CH4_BP_detrend) #CH4 detrended
P2_Control_BP_new<-c(Empty,P2_CH4_BP_trend) #Control with old CH4 values to make sure are the same as original data

P2_CH4_All_new<-P2_CH4_All_new[P2_Range$tAc[1]:P2_Range$tAc[2]]
P2_CH4_BP_new<-P2_CH4_BP_new[P2_Range$tAcc[1]:P2_Range$tAcc[2]]
P2_Control_All_new<-P2_Control_All_new[P2_Range$tAc[1]:P2_Range$tAc[2]]
P2_Control_BP_new<-P2_Control_BP_new[P2_Range$tAcc[1]:P2_Range$tAcc[2]]

##Re calculate the intervals for detrended CH4##
## G2509 borrowed
m_CH4_All<-1
sd_CH4_All<-1
for (i in 1:(nrow(P2_G2509_int)-1)){
  m_CH4_All[i]<-mean(P2_CH4_All_new[P2_G2509_int$x[i]:P2_G2509_int$x[i+1]])
  sd_CH4_All[i]<-sd(P2_CH4_All_new[P2_G2509_int$x[i]:P2_G2509_int$x[i+1]])
}
#Only odd positions are correct (Even positions are from t(2)to t(3))
m_CH4_All_P2_detrend<-m_CH4_All[c(TRUE,FALSE)]
sd_CH4_All_P2_detrend<-sd_CH4_All[c(TRUE,FALSE)]
## G4301
m_CH4_BP<-1
sd_CH4_BP<-1
for (i in 1:(nrow(P2_G4301_int)-1)){
  m_CH4_BP[i]<-mean(P2_CH4_BP_new[P2_G4301_int$x[i]:P2_G4301_int$x[i+1]])
  sd_CH4_BP[i]<-sd(P2_CH4_BP_new[P2_G4301_int$x[i]:P2_G4301_int$x[i+1]])
}
#Only odd positions are correct (Even positions are from t(2)to t(3))
m_CH4_BP_P2_detrend<-m_CH4_BP[c(TRUE,FALSE)]
sd_CH4_BP_P2_detrend<-sd_CH4_BP[c(TRUE,FALSE)]

## Control
m_Control_All<-1
sd_Control_All<-1
for (i in 1:(nrow(P2_G2509_int)-1)){
  m_Control_All[i]<-mean(P2_Control_All_new[P2_G2509_int$x[i]:P2_G2509_int$x[i+1]])
  sd_Control_All[i]<-sd(P2_Control_All_new[P2_G2509_int$x[i]:P2_G2509_int$x[i+1]])
}
m_Control_All_P2_detrend<-m_Control_All[c(TRUE,FALSE)]
sd_Control_All_P2_detrend<-sd_Control_All[c(TRUE,FALSE)]

m_Control_BP<-1
sd_Control_BP<-1
for (i in 1:(nrow(P2_G4301_int)-1)){
  m_Control_BP[i]<-mean(P2_Control_BP_new[P2_G4301_int$x[i]:P2_G4301_int$x[i+1]])
  sd_Control_BP[i]<-sd(P2_Control_BP_new[P2_G4301_int$x[i]:P2_G4301_int$x[i+1]])
}
m_Control_BP_P2_detrend<-m_Control_BP[c(TRUE,FALSE)]
sd_Control_BP_P2_detrend<-sd_Control_BP[c(TRUE,FALSE)]

## Control data frame to make sure intervals are the same
Mean_Control<-m_Control_All_P2_detrend-P2_CH4$y_CH4[1:9]+m_Control_BP_P2_detrend-P2_CH4$y_CH4[10:18]
sd_Control<-sd_Control_All_P2_detrend-P2_CH4$sd_CH4[1:9]+sd_Control_BP_P2_detrend-P2_CH4$sd_CH4[10:18]

df_P2_Control<-cbind.data.frame(m_Control_All_P2_detrend,sd_Control_All_P2_detrend,
                                m_Control_BP_P2_detrend,sd_Control_BP_P2_detrend,
                                P2_CH4$y_CH4[1:9],P2_CH4$sd_CH4[1:9],
                                P2_CH4$y_CH4[10:18],P2_CH4$sd_CH4[10:18],
                                Mean_Control,sd_Control)


m_P2_CH4_detrended<-c(m_CH4_All_P2_detrend,m_CH4_BP_P2_detrend)
sd_P2_CH4_detrended<-c(sd_CH4_All_P2_detrend,sd_CH4_BP_P2_detrend)

##Merge all in one data frame
x<-c(P2_NH3$x_NH3,P2_CH4$x_CH4,P2_N2O$x_N2O)
y<-c(P2_NH3$y_NH3,m_P2_CH4_detrended*1000,P2_N2O$y_N2O*1000)
sd<-c(P2_NH3$sd_NH3,sd_P2_CH4_detrended*1000,P2_N2O$sd_N2O*1000)
CRDS<-c(P2_NH3$Color_NH3,P2_CH4$Color_CH4,P2_N2O$Color_N2O)
Compound<-c(rep("NH3",18),rep("CH4",18),rep("N2O",9))

df_P2<-cbind.data.frame(x,y,sd,CRDS,Compound)

### Acetaldehyde 

## Make the new detrended CH4 have the same positions as the original data
Empty<-rep(NaN,46829) #Same positions as in original data so is easier to calculate the means
AA_CH4_All_new<-c(Empty,AA_CH4_All_detrend)
AA_Control_All_new<-c(Empty,AA_CH4_All_trend)
Empty<-rep(NaN,34829) #Same positions as in original data so is easier to calculate the means
AA_CH4_BP_new<-c(Empty,AA_CH4_BP_detrend) #CH4 detrended
AA_Control_BP_new<-c(Empty,AA_CH4_BP_trend) #Control with old CH4 values to make sure are the same as original data

AA_CH4_All_new<-AA_CH4_All_new[AA_Range$tAc[1]:AA_Range$tAc[2]]
AA_CH4_BP_new<-AA_CH4_BP_new[AA_Range$tAcc[1]:AA_Range$tAcc[2]]
AA_Control_All_new<-AA_Control_All_new[AA_Range$tAc[1]:AA_Range$tAc[2]]
AA_Control_BP_new<-AA_Control_BP_new[AA_Range$tAcc[1]:AA_Range$tAcc[2]]

##Re calculate the intervals for detrended CH4##
## G2509 borrowed
m_CH4_All<-1
sd_CH4_All<-1
for (i in 1:(nrow(AA_G2509_int)-1)){
  m_CH4_All[i]<-mean(AA_CH4_All_new[AA_G2509_int$x[i]:AA_G2509_int$x[i+1]])
  sd_CH4_All[i]<-sd(AA_CH4_All_new[AA_G2509_int$x[i]:AA_G2509_int$x[i+1]])
}
#Only odd positions are correct (Even positions are from t(2)to t(3))
m_CH4_All_AA_detrend<-m_CH4_All[c(TRUE,FALSE)]
sd_CH4_All_AA_detrend<-sd_CH4_All[c(TRUE,FALSE)]
## G4301
m_CH4_BP<-1
sd_CH4_BP<-1
for (i in 1:(nrow(AA_G4301_int)-1)){
  m_CH4_BP[i]<-mean(AA_CH4_BP_new[AA_G4301_int$x[i]:AA_G4301_int$x[i+1]])
  sd_CH4_BP[i]<-sd(AA_CH4_BP_new[AA_G4301_int$x[i]:AA_G4301_int$x[i+1]])
}
#Only odd positions are correct (Even positions are from t(2)to t(3))
m_CH4_BP_AA_detrend<-m_CH4_BP[c(TRUE,FALSE)]
sd_CH4_BP_AA_detrend<-sd_CH4_BP[c(TRUE,FALSE)]

## Control
m_Control_All<-1
sd_Control_All<-1
for (i in 1:(nrow(AA_G2509_int)-1)){
  m_Control_All[i]<-mean(AA_Control_All_new[AA_G2509_int$x[i]:AA_G2509_int$x[i+1]])
  sd_Control_All[i]<-sd(AA_Control_All_new[AA_G2509_int$x[i]:AA_G2509_int$x[i+1]])
}
m_Control_All_AA_detrend<-m_Control_All[c(TRUE,FALSE)]
sd_Control_All_AA_detrend<-sd_Control_All[c(TRUE,FALSE)]

m_Control_BP<-1
sd_Control_BP<-1
for (i in 1:(nrow(AA_G4301_int)-1)){
  m_Control_BP[i]<-mean(AA_Control_BP_new[AA_G4301_int$x[i]:AA_G4301_int$x[i+1]])
  sd_Control_BP[i]<-sd(AA_Control_BP_new[AA_G4301_int$x[i]:AA_G4301_int$x[i+1]])
}
m_Control_BP_AA_detrend<-m_Control_BP[c(TRUE,FALSE)]
sd_Control_BP_AA_detrend<-sd_Control_BP[c(TRUE,FALSE)]

## Control data frame to make sure intervals are the same
Mean_Control<-m_Control_All_AA_detrend-AA_CH4$y_CH4[1:13]+m_Control_BP_AA_detrend-AA_CH4$y_CH4[14:26]
sd_Control<-sd_Control_All_AA_detrend-AA_CH4$sd_CH4[1:13]+sd_Control_BP_AA_detrend-AA_CH4$sd_CH4[14:26]

df_AA_Control<-cbind.data.frame(m_Control_All_AA_detrend,sd_Control_All_AA_detrend,
                                m_Control_BP_AA_detrend,sd_Control_BP_AA_detrend,
                                AA_CH4$y_CH4[1:13],AA_CH4$sd_CH4[1:13],
                                AA_CH4$y_CH4[14:26],AA_CH4$sd_CH4[14:26],
                                Mean_Control,sd_Control)


m_AA_CH4_detrended<-c(m_CH4_All_AA_detrend,m_CH4_BP_AA_detrend)
sd_AA_CH4_detrended<-c(sd_CH4_All_AA_detrend,sd_CH4_BP_AA_detrend)

##Merge all in one data frame
x<-c(AA_NH3$x_NH3,AA_CH4$x_CH4,AA_N2O$x_N2O)
y<-c(AA_NH3$y_NH3,m_AA_CH4_detrended*1000,AA_N2O$y_N2O*1000)
sd<-c(AA_NH3$sd_NH3,sd_AA_CH4_detrended*1000,AA_N2O$sd_N2O*1000)
CRDS<-c(AA_NH3$Color_NH3,AA_CH4$Color_CH4,AA_N2O$Color_N2O)
Compound<-c(rep("NH3",26),rep("CH4",26),rep("N2O",13))

df_AA<-cbind.data.frame(x,y,sd,CRDS,Compound)


### Acetic acid 

## Make the new detrended CH4 have the same positions as the original data
Empty<-rep(NaN,34999) #Same positions as in original data so is easier to calculate the means
AAc_CH4_All_new<-c(Empty,AAc_CH4_All_detrend)
AAc_Control_All_new<-c(Empty,AAc_CH4_All_trend)
Empty<-rep(NaN,25999) #Same positions as in original data so is easier to calculate the means
AAc_CH4_BP_new<-c(Empty,AAc_CH4_BP_detrend) #CH4 detrended
AAc_Control_BP_new<-c(Empty,AAc_CH4_BP_trend) #Control with old CH4 values to make sure are the same as original data

AAc_CH4_All_new<-AAc_CH4_All_new[AAc_Range$tAc[1]:AAc_Range$tAc[2]]
AAc_CH4_BP_new<-AAc_CH4_BP_new[AAc_Range$tAcc[1]:AAc_Range$tAcc[2]]
AAc_Control_All_new<-AAc_Control_All_new[AAc_Range$tAc[1]:AAc_Range$tAc[2]]
AAc_Control_BP_new<-AAc_Control_BP_new[AAc_Range$tAcc[1]:AAc_Range$tAcc[2]]

##Re calculate the intervals for detrended CH4##
## G2509 borrowed
m_CH4_All<-1
sd_CH4_All<-1
for (i in 1:(nrow(AAc_G2509_int)-1)){
  m_CH4_All[i]<-mean(AAc_CH4_All_new[AAc_G2509_int$x[i]:AAc_G2509_int$x[i+1]])
  sd_CH4_All[i]<-sd(AAc_CH4_All_new[AAc_G2509_int$x[i]:AAc_G2509_int$x[i+1]])
}
#Only odd positions are correct (Even positions are from t(2)to t(3))
m_CH4_All_AAc_detrend<-m_CH4_All[c(TRUE,FALSE)]
sd_CH4_All_AAc_detrend<-sd_CH4_All[c(TRUE,FALSE)]
## G4301
m_CH4_BP<-1
sd_CH4_BP<-1
for (i in 1:(nrow(AAc_G4301_int)-1)){
  m_CH4_BP[i]<-mean(AAc_CH4_BP_new[AAc_G4301_int$x[i]:AAc_G4301_int$x[i+1]])
  sd_CH4_BP[i]<-sd(AAc_CH4_BP_new[AAc_G4301_int$x[i]:AAc_G4301_int$x[i+1]])
}
#Only odd positions are correct (Even positions are from t(2)to t(3))
m_CH4_BP_AAc_detrend<-m_CH4_BP[c(TRUE,FALSE)]
sd_CH4_BP_AAc_detrend<-sd_CH4_BP[c(TRUE,FALSE)]

## Control
m_Control_All<-1
sd_Control_All<-1
for (i in 1:(nrow(AAc_G2509_int)-1)){
  m_Control_All[i]<-mean(AAc_Control_All_new[AAc_G2509_int$x[i]:AAc_G2509_int$x[i+1]])
  sd_Control_All[i]<-sd(AAc_Control_All_new[AAc_G2509_int$x[i]:AAc_G2509_int$x[i+1]])
}
m_Control_All_AAc_detrend<-m_Control_All[c(TRUE,FALSE)]
sd_Control_All_AAc_detrend<-sd_Control_All[c(TRUE,FALSE)]

m_Control_BP<-1
sd_Control_BP<-1
for (i in 1:(nrow(AAc_G4301_int)-1)){
  m_Control_BP[i]<-mean(AAc_Control_BP_new[AAc_G4301_int$x[i]:AAc_G4301_int$x[i+1]])
  sd_Control_BP[i]<-sd(AAc_Control_BP_new[AAc_G4301_int$x[i]:AAc_G4301_int$x[i+1]])
}
m_Control_BP_AAc_detrend<-m_Control_BP[c(TRUE,FALSE)]
sd_Control_BP_AAc_detrend<-sd_Control_BP[c(TRUE,FALSE)]

## Control data frame to make sure intervals are the same
Mean_Control<-m_Control_All_AAc_detrend-AAc_CH4$y_CH4[1:9]+m_Control_BP_AAc_detrend-AAc_CH4$y_CH4[10:18]
sd_Control<-sd_Control_All_AAc_detrend-AAc_CH4$sd_CH4[1:9]+sd_Control_BP_AAc_detrend-AAc_CH4$sd_CH4[10:18]

df_AAc_Control<-cbind.data.frame(m_Control_All_AAc_detrend,sd_Control_All_AAc_detrend,
                                 m_Control_BP_AAc_detrend,sd_Control_BP_AAc_detrend,
                                 AAc_CH4$y_CH4[1:9],AAc_CH4$sd_CH4[1:9],
                                 AAc_CH4$y_CH4[10:18],AAc_CH4$sd_CH4[10:18],
                                 Mean_Control,sd_Control)


m_AAc_CH4_detrended<-c(m_CH4_All_AAc_detrend,m_CH4_BP_AAc_detrend)
sd_AAc_CH4_detrended<-c(sd_CH4_All_AAc_detrend,sd_CH4_BP_AAc_detrend)

##Merge all in one data frame
x<-c(AAc_NH3$x_NH3,AAc_CH4$x_CH4,AAc_N2O$x_N2O)
y<-c(AAc_NH3$y_NH3,m_AAc_CH4_detrended*1000,AAc_N2O$y_N2O*1000)
sd<-c(AAc_NH3$sd_NH3,sd_AAc_CH4_detrended*1000,AAc_N2O$sd_N2O*1000)
CRDS<-c(AAc_NH3$Color_NH3,AAc_CH4$Color_CH4,AAc_N2O$Color_N2O)
Compound<-c(rep("NH3",18),rep("CH4",18),rep("N2O",9))

df_AAc<-cbind.data.frame(x,y,sd,CRDS,Compound)


### Acetone

## Make the new detrended CH4 have the same positions as the original data
Empty<-rep(NaN,58999) #Same positions as in original data so is easier to calculate the means
Ac_CH4_All_new<-c(Empty,Ac_CH4_All_detrend)
Ac_Control_All_new<-c(Empty,Ac_CH4_All_trend)
Empty<-rep(NaN,42999) #Same positions as in original data so is easier to calculate the means
Ac_CH4_BP_new<-c(Empty,Ac_CH4_BP_detrend) #CH4 detrended
Ac_Control_BP_new<-c(Empty,Ac_CH4_BP_trend) #Control with old CH4 values to make sure are the same as original data

Ac_CH4_All_new<-Ac_CH4_All_new[Ac_Range$tAc[1]:Ac_Range$tAc[2]]
Ac_CH4_BP_new<-Ac_CH4_BP_new[Ac_Range$tAcc[1]:Ac_Range$tAcc[2]]
Ac_Control_All_new<-Ac_Control_All_new[Ac_Range$tAc[1]:Ac_Range$tAc[2]]
Ac_Control_BP_new<-Ac_Control_BP_new[Ac_Range$tAcc[1]:Ac_Range$tAcc[2]]

##Re calculate the intervals for detrended CH4##
## G2509 borrowed
m_CH4_All<-1
sd_CH4_All<-1
for (i in 1:(nrow(Ac_G2509_int)-1)){
  m_CH4_All[i]<-mean(Ac_CH4_All_new[Ac_G2509_int$x[i]:Ac_G2509_int$x[i+1]])
  sd_CH4_All[i]<-sd(Ac_CH4_All_new[Ac_G2509_int$x[i]:Ac_G2509_int$x[i+1]])
}
#Only odd positions are correct (Even positions are from t(2)to t(3))
m_CH4_All_Ac_detrend<-m_CH4_All[c(TRUE,FALSE)]
sd_CH4_All_Ac_detrend<-sd_CH4_All[c(TRUE,FALSE)]
## G4301
m_CH4_BP<-1
sd_CH4_BP<-1
for (i in 1:(nrow(Ac_G4301_int)-1)){
  m_CH4_BP[i]<-mean(Ac_CH4_BP_new[Ac_G4301_int$x[i]:Ac_G4301_int$x[i+1]])
  sd_CH4_BP[i]<-sd(Ac_CH4_BP_new[Ac_G4301_int$x[i]:Ac_G4301_int$x[i+1]])
}
#Only odd positions are correct (Even positions are from t(2)to t(3))
m_CH4_BP_Ac_detrend<-m_CH4_BP[c(TRUE,FALSE)]
sd_CH4_BP_Ac_detrend<-sd_CH4_BP[c(TRUE,FALSE)]

## Control
m_Control_All<-1
sd_Control_All<-1
for (i in 1:(nrow(Ac_G2509_int)-1)){
  m_Control_All[i]<-mean(Ac_Control_All_new[Ac_G2509_int$x[i]:Ac_G2509_int$x[i+1]])
  sd_Control_All[i]<-sd(Ac_Control_All_new[Ac_G2509_int$x[i]:Ac_G2509_int$x[i+1]])
}
m_Control_All_Ac_detrend<-m_Control_All[c(TRUE,FALSE)]
sd_Control_All_Ac_detrend<-sd_Control_All[c(TRUE,FALSE)]

m_Control_BP<-1
sd_Control_BP<-1
for (i in 1:(nrow(Ac_G4301_int)-1)){
  m_Control_BP[i]<-mean(Ac_Control_BP_new[Ac_G4301_int$x[i]:Ac_G4301_int$x[i+1]])
  sd_Control_BP[i]<-sd(Ac_Control_BP_new[Ac_G4301_int$x[i]:Ac_G4301_int$x[i+1]])
}
m_Control_BP_Ac_detrend<-m_Control_BP[c(TRUE,FALSE)]
sd_Control_BP_Ac_detrend<-sd_Control_BP[c(TRUE,FALSE)]

## Control data frame to make sure intervals are the same
Mean_Control<-m_Control_All_Ac_detrend-Ac_CH4$y_CH4[1:11]+m_Control_BP_Ac_detrend-Ac_CH4$y_CH4[12:22]
sd_Control<-sd_Control_All_Ac_detrend-Ac_CH4$sd_CH4[1:11]+sd_Control_BP_Ac_detrend-Ac_CH4$sd_CH4[12:22]

df_Ac_Control<-cbind.data.frame(m_Control_All_Ac_detrend,sd_Control_All_Ac_detrend,
                                m_Control_BP_Ac_detrend,sd_Control_BP_Ac_detrend,
                                Ac_CH4$y_CH4[1:11],Ac_CH4$sd_CH4[1:11],
                                Ac_CH4$y_CH4[12:22],Ac_CH4$sd_CH4[12:22],
                                Mean_Control,sd_Control)


m_Ac_CH4_detrended<-c(m_CH4_All_Ac_detrend,m_CH4_BP_Ac_detrend)
sd_Ac_CH4_detrended<-c(sd_CH4_All_Ac_detrend,sd_CH4_BP_Ac_detrend)

##Merge all in one data frame
x<-c(Ac_NH3$x_NH3,Ac_CH4$x_CH4,Ac_N2O$x_N2O)
y<-c(Ac_NH3$y_NH3,m_Ac_CH4_detrended*1000,Ac_N2O$y_N2O*1000)
sd<-c(Ac_NH3$sd_NH3,sd_Ac_CH4_detrended*1000,Ac_N2O$sd_N2O*1000)
CRDS<-c(Ac_NH3$Color_NH3,Ac_CH4$Color_CH4,Ac_N2O$Color_N2O)
Compound<-c(rep("NH3",22),rep("CH4",22),rep("N2O",11))

df_Ac<-cbind.data.frame(x,y,sd,CRDS,Compound)


### Methanol

## Make the new detrended CH4 have the same positions as the original data
Empty<-rep(NaN,53999) #Same positions as in original data so is easier to calculate the means
MeOH_CH4_All_new<-c(Empty,MeOH_CH4_All_detrend)
MeOH_Control_All_new<-c(Empty,MeOH_CH4_All_trend)
Empty<-rep(NaN,39099) #Same positions as in original data so is easier to calculate the means
MeOH_CH4_BP_new<-c(Empty,MeOH_CH4_BP_detrend) #CH4 detrended
MeOH_Control_BP_new<-c(Empty,MeOH_CH4_BP_trend) #Control with old CH4 values to make sure are the same as original data

MeOH_CH4_All_new<-MeOH_CH4_All_new[MeOH_Range$tAc[1]:MeOH_Range$tAc[2]]
MeOH_CH4_BP_new<-MeOH_CH4_BP_new[MeOH_Range$tAcc[1]:MeOH_Range$tAcc[2]]
MeOH_Control_All_new<-MeOH_Control_All_new[MeOH_Range$tAc[1]:MeOH_Range$tAc[2]]
MeOH_Control_BP_new<-MeOH_Control_BP_new[MeOH_Range$tAcc[1]:MeOH_Range$tAcc[2]]

##Re calculate the intervals for detrended CH4##
## G2509 borrowed
m_CH4_All<-1
sd_CH4_All<-1
for (i in 1:(nrow(MeOH_G2509_int)-1)){
  m_CH4_All[i]<-mean(MeOH_CH4_All_new[MeOH_G2509_int$x[i]:MeOH_G2509_int$x[i+1]])
  sd_CH4_All[i]<-sd(MeOH_CH4_All_new[MeOH_G2509_int$x[i]:MeOH_G2509_int$x[i+1]])
}
#Only odd positions are correct (Even positions are from t(2)to t(3))
m_CH4_All_MeOH_detrend<-m_CH4_All[c(TRUE,FALSE)]
sd_CH4_All_MeOH_detrend<-sd_CH4_All[c(TRUE,FALSE)]
## G4301
m_CH4_BP<-1
sd_CH4_BP<-1
for (i in 1:(nrow(MeOH_G4301_int)-1)){
  m_CH4_BP[i]<-mean(MeOH_CH4_BP_new[MeOH_G4301_int$x[i]:MeOH_G4301_int$x[i+1]])
  sd_CH4_BP[i]<-sd(MeOH_CH4_BP_new[MeOH_G4301_int$x[i]:MeOH_G4301_int$x[i+1]])
}
#Only odd positions are correct (Even positions are from t(2)to t(3))
m_CH4_BP_MeOH_detrend<-m_CH4_BP[c(TRUE,FALSE)]
sd_CH4_BP_MeOH_detrend<-sd_CH4_BP[c(TRUE,FALSE)]

## Control
m_Control_All<-1
sd_Control_All<-1
for (i in 1:(nrow(MeOH_G2509_int)-1)){
  m_Control_All[i]<-mean(MeOH_Control_All_new[MeOH_G2509_int$x[i]:MeOH_G2509_int$x[i+1]])
  sd_Control_All[i]<-sd(MeOH_Control_All_new[MeOH_G2509_int$x[i]:MeOH_G2509_int$x[i+1]])
}
m_Control_All_MeOH_detrend<-m_Control_All[c(TRUE,FALSE)]
sd_Control_All_MeOH_detrend<-sd_Control_All[c(TRUE,FALSE)]

m_Control_BP<-1
sd_Control_BP<-1
for (i in 1:(nrow(MeOH_G4301_int)-1)){
  m_Control_BP[i]<-mean(MeOH_Control_BP_new[MeOH_G4301_int$x[i]:MeOH_G4301_int$x[i+1]])
  sd_Control_BP[i]<-sd(MeOH_Control_BP_new[MeOH_G4301_int$x[i]:MeOH_G4301_int$x[i+1]])
}
m_Control_BP_MeOH_detrend<-m_Control_BP[c(TRUE,FALSE)]
sd_Control_BP_MeOH_detrend<-sd_Control_BP[c(TRUE,FALSE)]

## Control data frame to make sure intervals are the same
Mean_Control<-m_Control_All_MeOH_detrend-MeOH_CH4$y_CH4[1:11]+m_Control_BP_MeOH_detrend-MeOH_CH4$y_CH4[12:22]
sd_Control<-sd_Control_All_MeOH_detrend-MeOH_CH4$sd_CH4[1:11]+sd_Control_BP_MeOH_detrend-MeOH_CH4$sd_CH4[12:22]

df_MeOH_Control<-cbind.data.frame(m_Control_All_MeOH_detrend,sd_Control_All_MeOH_detrend,
                                  m_Control_BP_MeOH_detrend,sd_Control_BP_MeOH_detrend,
                                  MeOH_CH4$y_CH4[1:11],MeOH_CH4$sd_CH4[1:11],
                                  MeOH_CH4$y_CH4[12:22],MeOH_CH4$sd_CH4[12:22],
                                  Mean_Control,sd_Control)


m_MeOH_CH4_detrended<-c(m_CH4_All_MeOH_detrend,m_CH4_BP_MeOH_detrend)
sd_MeOH_CH4_detrended<-c(sd_CH4_All_MeOH_detrend,sd_CH4_BP_MeOH_detrend)

##Merge all in one data frame
x<-c(MeOH_NH3$x_NH3,MeOH_CH4$x_CH4,MeOH_N2O$x_N2O)
y<-c(MeOH_NH3$y_NH3,m_MeOH_CH4_detrended*1000,MeOH_N2O$y_N2O*1000)
sd<-c(MeOH_NH3$sd_NH3,sd_MeOH_CH4_detrended*1000,MeOH_N2O$sd_N2O*1000)
CRDS<-c(MeOH_NH3$Color_NH3,MeOH_CH4$Color_CH4,MeOH_N2O$Color_N2O)
Compound<-c(rep("NH3",22),rep("CH4",22),rep("N2O",11))

df_MeOH<-cbind.data.frame(x,y,sd,CRDS,Compound)


### Propionic acid

## Make the new detrended CH4 have the same positions as the original data
Empty<-rep(NaN,41199) #Same positions as in original data so is easier to calculate the means
PAc_CH4_All_new<-c(Empty,PAc_CH4_All_detrend)
PAc_Control_All_new<-c(Empty,PAc_CH4_All_trend)
#Empty<-rep(NaN,39099) #Same positions as in original data so is easier to calculate the means
#PAc_CH4_BP_new<-c(Empty,PAc_CH4_BP_detrend) #CH4 detrended
#PAc_Control_BP_new<-c(Empty,PAc_CH4_BP_trend) #Control with old CH4 values to make sure are the same as original data

PAc_CH4_All_new<-PAc_CH4_All_new[PAc_Range$tAco[1]:PAc_Range$tAco[2]]
#PAc_CH4_BP_new<-PAc_CH4_BP_new[PAc_Range$tAcc[1]:PAc_Range$tAcc[2]]
PAc_Control_All_new<-PAc_Control_All_new[PAc_Range$tAco[1]:PAc_Range$tAco[2]]
#PAc_Control_BP_new<-PAc_Control_BP_new[PAc_Range$tAcc[1]:PAc_Range$tAcc[2]]

##Re calculate the intervals for detrended CH4##
## G2509 borrowed
m_CH4_All<-1
sd_CH4_All<-1
for (i in 1:(nrow(PAc_G2509_int)-1)){
  m_CH4_All[i]<-mean(PAc_CH4_All_new[PAc_G2509_int$x[i]:PAc_G2509_int$x[i+1]])
  sd_CH4_All[i]<-sd(PAc_CH4_All_new[PAc_G2509_int$x[i]:PAc_G2509_int$x[i+1]])
}
#Only odd positions are correct (Even positions are from t(2)to t(3))
m_CH4_All_PAc_detrend<-m_CH4_All[c(TRUE,FALSE)]
sd_CH4_All_PAc_detrend<-sd_CH4_All[c(TRUE,FALSE)]
## G4301
m_CH4_BP<-1
sd_CH4_BP<-1
#for (i in 1:(nrow(PAc_G4301_int)-1)){
# m_CH4_BP[i]<-mean(PAc_CH4_BP_new[PAc_G4301_int$x[i]:PAc_G4301_int$x[i+1]])
#sd_CH4_BP[i]<-sd(PAc_CH4_BP_new[PAc_G4301_int$x[i]:PAc_G4301_int$x[i+1]])
#}
#Only odd positions are correct (Even positions are from t(2)to t(3))
#m_CH4_BP_PAc_detrend<-m_CH4_BP[c(TRUE,FALSE)]
#sd_CH4_BP_PAc_detrend<-sd_CH4_BP[c(TRUE,FALSE)]

## Control
m_Control_All<-1
sd_Control_All<-1
for (i in 1:(nrow(PAc_G2509_int)-1)){
  m_Control_All[i]<-mean(PAc_Control_All_new[PAc_G2509_int$x[i]:PAc_G2509_int$x[i+1]])
  sd_Control_All[i]<-sd(PAc_Control_All_new[PAc_G2509_int$x[i]:PAc_G2509_int$x[i+1]])
}
m_Control_All_PAc_detrend<-m_Control_All[c(TRUE,FALSE)]
sd_Control_All_PAc_detrend<-sd_Control_All[c(TRUE,FALSE)]

m_Control_BP<-1
sd_Control_BP<-1
#for (i in 1:(nrow(PAc_G4301_int)-1)){
# m_Control_BP[i]<-mean(PAc_Control_BP_new[PAc_G4301_int$x[i]:PAc_G4301_int$x[i+1]])
#sd_Control_BP[i]<-sd(PAc_Control_BP_new[PAc_G4301_int$x[i]:PAc_G4301_int$x[i+1]])
#}
#m_Control_BP_PAc_detrend<-m_Control_BP[c(TRUE,FALSE)]
#sd_Control_BP_PAc_detrend<-sd_Control_BP[c(TRUE,FALSE)]

## Control data frame to make sure intervals are the same
Mean_Control<-m_Control_All_PAc_detrend-PAc_CH4$y_CH4[1:7]
#+m_Control_BP_PAc_detrend-PAc_CH4$y_CH4[12:22]
sd_Control<-sd_Control_All_PAc_detrend-PAc_CH4$sd_CH4[1:7]
#+sd_Control_BP_PAc_detrend-PAc_CH4$sd_CH4[12:22]

df_PAc_Control<-cbind.data.frame(m_Control_All_PAc_detrend,sd_Control_All_PAc_detrend,
                                 #m_Control_BP_PAc_detrend,sd_Control_BP_PAc_detrend,                                  
                                 PAc_CH4$y_CH4[1:7],PAc_CH4$sd_CH4[1:7],
                                 #PAc_CH4$y_CH4[12:22],PAc_CH4$sd_CH4[12:22],
                                 Mean_Control,sd_Control)


m_PAc_CH4_detrended<-c(m_CH4_All_PAc_detrend)
#,m_CH4_BP_PAc_detrend)
sd_PAc_CH4_detrended<-c(sd_CH4_All_PAc_detrend)
#,sd_CH4_BP_PAc_detrend)

##Merge all in one data frame
x<-c(PAc_NH3$x_NH3,PAc_CH4$x_CH4,PAc_N2O$x_N2O)
y<-c(PAc_NH3$y_NH3,m_PAc_CH4_detrended*1000,PAc_N2O$y_N2O*1000)
sd<-c(PAc_NH3$sd_NH3,sd_PAc_CH4_detrended*1000,PAc_N2O$sd_N2O*1000)
CRDS<-c(PAc_NH3$Color_NH3,PAc_CH4$Color_CH4,PAc_N2O$Color_N2O)
Compound<-c(rep("NH3",14),rep("CH4",7),rep("N2O",7))

df_PAc<-cbind.data.frame(x,y,sd,CRDS,Compound)

#### PLOTS ####
#Need to adjust error bars width individually for each facet
df_EtOH$wd<-rep(c(0.15,0.45,07.5),c(30,20,20))
df_BAc$wd<-rep(c(0.075,0.75,04.5),c(20,20,10))
df_DA$wd<-rep(c(0.075,01.0,03.5),c(24,24,12))
df_Bne$wd<-rep(c(0.085,1.0,02.5),c(20,20,10))
df_B1$wd<-rep(c(0.095,01.0,02.5),c(24,24,12))
df_P1$wd<-rep(c(0.095,01.0,02.5),c(28,28,14))
df_P2$wd<-rep(c(0.095,01.0,02.5),c(18,18,9))
df_AAc$wd<-rep(c(0.095,01.0,02.5),c(18,18,9))
df_AA$wd<-rep(c(0.095,0.55,02.5),c(26,26,13))
df_Ac$wd<-rep(c(0.095,01.0,02.5),c(22,22,11))
df_MeOH$wd<-rep(c(0.095,0.9,02.5),c(22,22,11))
df_PAc$wd<-rep(c(0.095,0.15,02.5),c(14,7,7))

# Make same axis breaks for all facets
count <- 0
equal_breaks <- function(n = 4, s = 0.05, ...){
  function(x){
    # rescaling
    d <- s * diff(range(x)) / (1+2*s)
    seq(min(x)+d, max(x)-d, length=n)
  }
}
#Same number of decimals
scaleFUN <- function(x) sprintf("%.3f", x)
#Different number of decimals
fmt <- function(){
  function(x) {
    d <- log10(min(diff(x)))
    if(d < 0) format(x,nsmall = abs(round(d)),scientific = FALSE) else x
  }
}
### Ethanol

#

pEtOH<-ggplot(data=df_EtOH,aes(x=x,y=y,group=Compound,color=CRDS,shape=CRDS))+geom_point()+
  facet_grid(rows = vars(Compound),scales="free_y",labeller = my_labeller)+
  geom_errorbar(aes(ymin=y-sd, ymax=y+sd), width=50)+
  geom_errorbar(aes(xmin=x-sd_Eth, xmax=x+sd_Eth,width=wd))+
  xlab("Ethanol (ppb)")+ylab("Concentration (ppb)")+
  theme_bw()+theme(panel.grid=element_blank())+
  theme(panel.border =element_rect(colour = "grey", fill=NA))+
  theme(axis.line = element_line(colour = "black"))+
  theme(panel.spacing.x=unit(10, "lines"),panel.spacing.y=unit(0.6, "lines"))+
  theme(legend.position = "none")+
  theme(strip.background = element_blank())+
  theme(strip.text = element_blank())+
  #ggtitle("(a)")+
  #theme(plot.title = element_text(size = 10, face = "bold"))+
  scale_shape_manual('',values = c(NaN,NaN,NaN),breaks=c("G2103","G2509_1","G2509_2"))+
  scale_color_manual('',values = c("#F8766D", "#00BA38" ,"#C77CFF"),breaks=c("G2103","G2509_1","G2509_2"))
#pEtOH<-pEtOH+scale_y_continuous(labels=scaleFUN,breaks=equal_breaks(n=4, s=0.05),expand = c(0.05, 0))

#Remove BP offset
Of<-which(df_BAc$CRDS=="G4301")
df_BAc$y[Of]<-df_BAc$y[Of]+20
#

### Butyric acid
pBAc<-ggplot(data=df_BAc,aes(x=x,y=y,group=Compound,color=CRDS,shape=CRDS))+geom_point(size=2)+
  facet_grid(rows = vars(Compound),scales="free",labeller = my_labeller)+
  geom_errorbar(aes(ymin=y-sd, ymax=y+sd), width=30)+
  geom_errorbar(aes(xmin=x-sd_BAc, xmax=x+sd_BAc, width=wd))+
  xlab("Butyric acid (ppb)")+#ylab("Concentration (ppb)")+
  ylab("")+
  theme_bw()+theme(panel.grid=element_blank())+
  theme(panel.border =element_rect(colour = "grey", fill=NA))+
  theme(axis.line = element_line(colour = "black"))+
  theme(panel.spacing.x=unit(10, "lines"),panel.spacing.y=unit(0.6, "lines"))+
  theme(legend.position = "none")+
  theme(strip.background = element_blank())+
  #ggtitle("(b)")+
  theme(plot.title = element_text(size = 10, face = "bold"))+
  scale_shape_manual('',values = c(NaN,NaN,NaN),breaks=c("G2103","G2509","G4301"))
#pBAc<-pBAc+scale_y_continuous(labels=scaleFUN,breaks=equal_breaks(n=4, s=0.05),expand = c(0.05, 0))

### Diacetyl
#Remove BP offset
Of<-which(df_DA$CRDS=="G4301")
df_DA$y[Of]<-df_DA$y[Of]+20
#
pDA<-ggplot(data=df_DA,aes(x=x,y=y,group=Compound,color=CRDS,shape=CRDS))+geom_point(size=2)+
  facet_grid(rows = vars(Compound),scales="free")+
  geom_errorbar(aes(ymin=y-sd, ymax=y+sd), width=40)+
  geom_errorbar(aes(xmin=x-sd_DA, xmax=x+sd_DA, width=wd))+
  xlab("Diacetyl (ppb)")+ylab("Concentration (ppb)")+
  theme_bw()+theme(panel.grid=element_blank())+
  theme(panel.border =element_rect(colour = "grey", fill=NA))+
  theme(axis.line = element_line(colour = "black"))+
  theme(panel.spacing.x=unit(10, "lines"),panel.spacing.y=unit(0.6, "lines"))+
  theme(strip.background = element_blank())+
  theme(legend.position = "none")+
  theme(strip.text = element_blank())+
  theme(plot.title = element_text(size = 10, face = "bold"))+
  scale_shape_manual('',values = c(NaN,NaN,NaN),breaks=c("G2103","G2509","G4301"))
#pDA<-pDA+scale_y_continuous(labels=scaleFUN,breaks=equal_breaks(n=4, s=0.05),expand = c(0.05, 0))

### Butanone
#Remove BP offset
Of<-which(df_Bne$CRDS=="G4301")
df_Bne$y[Of]<-df_Bne$y[Of]+20
#
pBne<-ggplot(data=df_Bne,aes(x=x,y=y,group=Compound,color=CRDS,shape=CRDS))+geom_point(size=2)+
  facet_grid(rows = vars(Compound),scales="free",labeller = my_labeller)+
  geom_errorbar(aes(ymin=y-sd, ymax=y+sd), width=50)+
  geom_errorbar(aes(xmin=x-sd_Bne, xmax=x+sd_Bne, width=wd))+
  xlab("Butanone (ppb)")+#ylab("Concentration (ppb)")+
  ylab("")+
  theme_bw()+theme(panel.grid=element_blank())+
  theme(panel.border =element_rect(colour = "grey", fill=NA))+
  theme(axis.line = element_line(colour = "black"))+
  theme(panel.spacing.x=unit(10, "lines"),panel.spacing.y=unit(0.6, "lines"))+
  theme(strip.background = element_blank())+
  theme(legend.position = "none")+
  
  #ggtitle("(d)")+
  theme(plot.title = element_text(size = 10, face = "bold"))+
  scale_shape_manual('',values = c(NaN,NaN,NaN),breaks=c("G2103","G2509","G4301"))
#pBne<-pBne+scale_y_continuous(labels=scaleFUN,breaks=equal_breaks(n=4, s=0.05),expand = c(0.05, 0))

### 1-Butanol
#Remove BP offset
Of<-which(df_B1$CRDS=="G4301")
df_B1$y[Of]<-df_B1$y[Of]+20
#
pB1<-ggplot(data=df_B1,aes(x=x,y=y,group=Compound,color=CRDS,shape=CRDS))+geom_point(size=2)+
  facet_grid(rows = vars(Compound),scales="free")+
  geom_errorbar(aes(ymin=y-sd, ymax=y+sd), width=20)+
  geom_errorbar(aes(xmin=x-sd_B1, xmax=x+sd_B1, width=wd))+
  xlab("1-Butanol (ppb)")+ylab("Concentration (ppb)")+
  theme_bw()+theme(panel.grid=element_blank())+
  theme(panel.border =element_rect(colour = "grey", fill=NA))+
  theme(axis.line = element_line(colour = "black"))+
  theme(panel.spacing.x=unit(10, "lines"),panel.spacing.y=unit(0.6, "lines"))+
  theme(strip.background = element_blank())+
  theme(legend.position = "none")+
  theme(strip.text = element_blank())+
  scale_shape_manual('',values = c(NaN,NaN,NaN),breaks=c("G2103","G2509","G4301"))

### 1-Propanol
#Remove BP offset
Of<-which(df_P1$CRDS=="G4301")
df_P1$y[Of]<-df_P1$y[Of]+20
#
pP1<-ggplot(data=df_P1,aes(x=x,y=y,group=Compound,color=CRDS,shape=CRDS))+geom_point(size=2)+
  facet_grid(rows = vars(Compound),scales="free",labeller = my_labeller)+
  geom_errorbar(aes(ymin=y-sd, ymax=y+sd), width=30)+
  geom_errorbar(aes(xmin=x-sd_P1, xmax=x+sd_P1, width=wd))+
  xlab("1-Propanol (ppb)")+ylab("")+
  theme_bw()+theme(panel.grid=element_blank())+
  theme(panel.border =element_rect(colour = "grey", fill=NA))+
  theme(axis.line = element_line(colour = "black"))+
  theme(panel.spacing.x=unit(10, "lines"),panel.spacing.y=unit(0.6, "lines"))+
  theme(strip.background = element_blank())+
  theme(legend.position = "none")+
  scale_shape_manual('',values = c(NaN,NaN,NaN),breaks=c("G2103","G2509","G4301"))

### 2-Propanol
#Remove BP offset
Of<-which(df_P2$CRDS=="G4301")
df_P2$y[Of]<-df_P2$y[Of]+20
#
pP2<-ggplot(data=df_P2,aes(x=x,y=y,group=Compound,color=CRDS,shape=CRDS))+geom_point(size=2)+
  facet_grid(rows = vars(Compound),scales="free",labeller = my_labeller)+
  geom_errorbar(aes(ymin=y-sd, ymax=y+sd), width=20)+
  geom_errorbar(aes(xmin=x-sd_P2, xmax=x+sd_P2, width=wd))+
  xlab("2-Propanol (ppb)")+ylab("")+
  theme_bw()+theme(panel.grid=element_blank())+
  theme(panel.border =element_rect(colour = "grey", fill=NA))+
  theme(axis.line = element_line(colour = "black"))+
  theme(panel.spacing.x=unit(10, "lines"),panel.spacing.y=unit(0.6, "lines"))+
  theme(strip.background = element_blank())+
  theme(legend.position = "none")+
  scale_shape_manual('',values = c(NaN,NaN,NaN),breaks=c("G2103","G2509","G4301"))

### Acetaldehyde
#Remove BP offset
Of<-which(df_AA$CRDS=="G4301")
df_AA$y[Of]<-df_AA$y[Of]+20
#
pAA<-ggplot(data=df_AA,aes(x=x,y=y,group=Compound,color=CRDS,shape=CRDS))+geom_point(size=2)+
  facet_grid(rows = vars(Compound),scales="free",labeller = my_labeller)+
  geom_errorbar(aes(ymin=y-sd, ymax=y+sd), width=30)+
  geom_errorbar(aes(xmin=x-sd_AA, xmax=x+sd_AA, width=wd))+
  xlab("Acetaldehyde (ppb)")+ylab("")+
  theme_bw()+theme(panel.grid=element_blank())+
  theme(panel.border =element_rect(colour = "grey", fill=NA))+
  theme(axis.line = element_line(colour = "black"))+
  theme(panel.spacing.x=unit(10, "lines"),panel.spacing.y=unit(0.6, "lines"))+
  theme(strip.background = element_blank())+
  theme(legend.position = "none")+
  scale_shape_manual('',values = c(NaN,NaN,NaN),breaks=c("G2103","G2509","G4301"))

### Acetic acid
#Remove BP offset
Of<-which(df_AAc$CRDS=="G4301")
df_AAc$y[Of]<-df_AAc$y[Of]+20
#
pAAc<-ggplot(data=df_AAc,aes(x=x,y=y,group=Compound,color=CRDS,shape=CRDS))+geom_point(size=2)+
  facet_grid(rows = vars(Compound),scales="free")+
  geom_errorbar(aes(ymin=y-sd, ymax=y+sd), width=70)+
  geom_errorbar(aes(xmin=x-sd_AAc, xmax=x+sd_AAc, width=wd))+
  xlab("Acetic acid (ppb)")+ylab("Concentration (ppb)")+
  theme_bw()+theme(panel.grid=element_blank())+
  theme(panel.border =element_rect(colour = "grey", fill=NA))+
  theme(axis.line = element_line(colour = "black"))+
  theme(panel.spacing.x=unit(10, "lines"),panel.spacing.y=unit(0.6, "lines"))+
  theme(strip.background = element_blank())+
  theme(legend.position = "none")+
  theme(strip.text = element_blank())+
  scale_shape_manual('',values = c(NaN,NaN,NaN),breaks=c("G2103","G2509","G4301"))

### Acetone
#Remove BP offset
Of<-which(df_Ac$CRDS=="G4301")
df_Ac$y[Of]<-df_Ac$y[Of]+20
#
pAc<-ggplot(data=df_Ac,aes(x=x,y=y,group=Compound,color=CRDS,shape=CRDS))+geom_point(size=2)+
  facet_grid(rows = vars(Compound),scales="free",labeller = my_labeller)+
  geom_errorbar(aes(ymin=y-sd, ymax=y+sd), width=20)+
  geom_errorbar(aes(xmin=x-sd_Ac, xmax=x+sd_Ac, width=wd))+
  xlab("Acetone (ppb)")+ylab("")+
  theme_bw()+theme(panel.grid=element_blank())+
  theme(panel.border =element_rect(colour = "grey", fill=NA))+
  theme(axis.line = element_line(colour = "black"))+
  theme(panel.spacing.x=unit(10, "lines"),panel.spacing.y=unit(0.6, "lines"))+
  theme(strip.background = element_blank())+
  theme(legend.position = "none")+
  scale_shape_manual('',values = c(NaN,NaN,NaN),breaks=c("G2103","G2509","G4301"))

### Methanol
#Remove BP offset
Of<-which(df_MeOH$CRDS=="G4301")
df_MeOH$y[Of]<-df_MeOH$y[Of]+20
#
pMeOH<-ggplot(data=df_MeOH,aes(x=x,y=y,group=Compound,color=CRDS,shape=CRDS))+geom_point(size=2)+
  facet_grid(rows = vars(Compound),scales="free")+
  geom_errorbar(aes(xmin=x-sd_MeOH, xmax=x+sd_MeOH, width=wd))+
  xlab("Methanol (ppb)")+ylab("Concentration (ppb)")+
  theme_bw()+theme(panel.grid=element_blank())+
  theme_bw()+theme(panel.grid=element_blank())+
  theme(panel.border =element_rect(colour = "grey", fill=NA))+
  theme(axis.line = element_line(colour = "black"))+
  theme(panel.spacing.x=unit(10, "lines"),panel.spacing.y=unit(0.6, "lines"))+
  theme(strip.background = element_blank())+
  theme(legend.position = "none")+
  #ggtitle("(c)")+
  geom_errorbar(aes(ymin=y-sd, ymax=y+sd), width=70)+
  theme(strip.text = element_blank())+
  scale_shape_manual('',values = c(NaN,NaN,NaN),breaks=c("G2103","G2509","G4301"))

### Propionic acid
pPAc<-ggplot(data=df_PAc,aes(x=x,y=y,group=Compound,color=CRDS,shape=CRDS))+geom_point(size=2)+
  facet_grid(rows = vars(Compound),scales="free")+
  geom_errorbar(aes(ymin=y-sd, ymax=y+sd), width=30)+
  geom_errorbar(aes(xmin=x-sd_PAc, xmax=x+sd_PAc, width=wd))+
  xlab("Propionic acid (ppb)")+ylab("Concentration (ppb)")+
  theme_bw()+theme(panel.grid=element_blank())+
  theme(panel.border =element_rect(colour = "grey", fill=NA))+
  theme(axis.line = element_line(colour = "black"))+
  theme(panel.spacing.x=unit(10, "lines"),panel.spacing.y=unit(0.6, "lines"))+
  theme(strip.background = element_blank())+
  theme(legend.position = "none")+
  theme(strip.text = element_blank())+
  scale_shape_manual('',values = c(NaN,NaN),breaks=c("G2103","G2509_2"))+
  scale_color_manual('',values = c("#F8766D", "#C77CFF"),breaks=c("G2103","G2509_2"))
##Plot in same figure with common legends
#Function for common legends
x<-1:4
y<-1:4
CRDS<-c("G2509", "G2103", "G4301","G2509_2")
df_legend<-cbind.data.frame(x,y,CRDS)
plot1_legend <- ggplot(df_legend,
                       aes(x = x, y = y,shape=CRDS,color=CRDS)) +
  geom_point(size=3)+geom_line()+theme_bw()+
  scale_shape_manual("",values=c(20,20,20,20))+
  scale_color_manual("",values=c("#F8766D","#00BA38","#C77CFF","#619CFF"))+
  theme(legend.position = "bottom")

# function to extract legend from plot
get_only_legend <- function(plot) {
  plot_table <- ggplot_gtable(ggplot_build(plot))
  legend_plot <- which(sapply(plot_table$grobs, function(x) x$name) == "guide-box")
  legend <- plot_table$grobs[[legend_plot]]
  return(legend)
}
legend <- get_only_legend(plot1_legend)   

##
### Merge 4 target VOCs in one graph
library(egg)

B<-grid.arrange(arrangeGrob(pEtOH,pBAc,pDA,pBne, ncol = 2,nrow=2),
                legend, heights = c(10, 1))
ggsave(plot=B,"Main3_VOCs_Interferences.png", width = 8, height = 5)

C<-grid.arrange(arrangeGrob(pB1,pP1,pP2,pAA, ncol = 2,nrow=2),
                legend, heights = c(10, 1))
ggsave(plot=C,"Main4_VOCs_Interferences.png", width = 8, height = 5)


D<-grid.arrange(arrangeGrob(pAAc,pAc,pMeOH,pPAc, ncol = 2,nrow=2),
                legend, heights = c(10, 1))
ggsave(plot=D,"Main5_VOCs_Interferences.png", width = 8, height = 5)

#Select 6 for main
pDA<-pDA +  geom_text(data = subset(df_DA, Compound == "CH4"),
                      aes(label = "(a)"),
                      x = -Inf, y = Inf,
                      vjust = 1, hjust = 0,
                      size = 3,
                      color="black")
pBne<-pBne +  geom_text(data = subset(df_Bne, Compound == "CH4"),
                        aes(label = "(b)"),
                        x = -Inf, y = Inf,
                        vjust = 1, hjust = 0,
                        size = 3,
                        color="black")
pMeOH<-pMeOH +  geom_text(data = subset(df_MeOH, Compound == "CH4"),
                          aes(label = "(c)"),
                          x = -Inf, y = Inf,
                          vjust = 1, hjust = 0,
                          size = 3,
                          color="black")
pP1<-pP1 +  geom_text(data = subset(df_P1, Compound == "CH4"),
                      aes(label = "(d)"),
                      x = -Inf, y = Inf,
                      vjust = 1, hjust = 0,
                      size = 3,
                      color="black")
pEtOH<-pEtOH +  geom_text(data = subset(df_EtOH, Compound == "CH4"),
                          aes(label = "(e)"),
                          x = -Inf, y = Inf,
                          vjust = 1, hjust = 0,
                          size = 3,
                          color="black")
pAA<-pAA +  geom_text(data = subset(df_AA, Compound == "CH4"),
                      aes(label = "(f)"),
                      x = -Inf, y = Inf,
                      vjust = 1, hjust = 0,
                      size = 3,
                      color="black")
E<-grid.arrange(arrangeGrob(pDA,pBne,pMeOH,pP1,pEtOH,pAA, ncol = 2,nrow=3),
                legend, heights = c(10, 1))
ggsave(plot=E,"Main6_VOCs_Interferences.png", width = 8, height = 8)

#Select 6 for Sup
pAAc<-pAAc +  geom_text(data = subset(df_AAc, Compound == "CH4"),
                        aes(label = "(a)"),
                        x = -Inf, y = Inf,
                        vjust = 1, hjust = 0,
                        size = 3,
                        color="black")
pAc<-pAc +  geom_text(data = subset(df_Ac, Compound == "CH4"),
                      aes(label = "(b)"),
                      x = -Inf, y = Inf,
                      vjust = 1, hjust = 0,
                      size = 3,
                      color="black")
pPAc<-pPAc +  geom_text(data = subset(df_PAc, Compound == "CH4"),
                        aes(label = "(c)"),
                        x = -Inf, y = Inf,
                        vjust = 1, hjust = 0,
                        size = 3,
                        color="black")
pBAc<-pBAc +  geom_text(data = subset(df_BAc, Compound == "CH4"),
                        aes(label = "(d)"),
                        x = -Inf, y = Inf,
                        vjust = 1, hjust = 0,
                        size = 3,
                        color="black")
pB1<-pB1 +  geom_text(data = subset(df_B1, Compound == "CH4"),
                      aes(label = "(e)"),
                      x = -Inf, y = Inf,
                      vjust = 1, hjust = 0,
                      size = 3,
                      color="black")
pP2<-pP2 +  geom_text(data = subset(df_P2, Compound == "CH4"),
                      aes(label = "(f)"),
                      x = -Inf, y = Inf,
                      vjust = 1, hjust = 0,
                      size = 3,
                      color="black")
F6<-grid.arrange(arrangeGrob(pAAc,pAc,pPAc,pBAc,pB1,pP2, ncol = 2,nrow=3),
                 legend, heights = c(10, 1))
ggsave(plot=F6,"Sup6_VOCs_Interferences.png", width = 8, height = 8)