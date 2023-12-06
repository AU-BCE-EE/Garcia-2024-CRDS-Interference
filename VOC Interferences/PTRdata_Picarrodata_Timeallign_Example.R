library(dplyr)
library(ggplot2)
library(anytime)
library(lubridate)
library(writexl)
library("readxl")



### Interferences 2021-07-05###   

##Extract data from borrowed picarro##  
setwd("O:/Tech_BCE/Environmental engineering/Air Quality Engineering/Picarro interferences (Anna&Pablo)/Interferences/VOCs/5-July")
list_of_files <- list.files(path="G2508 borrowed",pattern = "*.dat",recursive=TRUE) ##Read data from Picarro
L<-length(list_of_files)

setwd("O:/Tech_BCE/Environmental engineering/Air Quality Engineering/Picarro interferences (Anna&Pablo)/Interferences/VOCs/5-July/G2508 borrowed")

#Read all Picarro from the folder
getstats<- function(Picarro){    #Open each Dat file in a list 
  
  listofdfs <- list() 
  
  for(i in 1:length(Picarro)){ #Loop through the numbers of ID's instead of the ID's
    
    
    
    Pic_Data<-read.table(list_of_files[i],header=TRUE)
    listofdfs[[i]] <- Pic_Data # save your dataframes into the list
  }
  
  return(listofdfs) #Return the list of dataframes.
}

Pic_id<- as.character(c(1:L))
Dat_All<-getstats(Picarro = Pic_id) #Able to call each Picarro file
Dat_All<-bind_rows(Dat_All, .id = "column_label") #Join all elements of the list in one dataframe

#Change format of date and time to be plotted
Dat_All$date.time<-paste(Dat_All$DATE,Dat_All$TIME)
Dat_All$date.time<-as.POSIXct(Dat_All$date.time,format="%Y-%m-%d %H:%M:%S")

##Extract data from NH3 picarro##  
setwd("O:/Tech_BCE/Environmental engineering/Air Quality Engineering/Picarro interferences (Anna&Pablo)/Interferences/VOCs/5-July")
list_of_files <- list.files(path="NH3 Picarro",pattern = "*.dat",recursive=TRUE) ##Read data from Picarro
L<-length(list_of_files)

setwd("O:/Tech_BCE/Environmental engineering/Air Quality Engineering/Picarro interferences (Anna&Pablo)/Interferences/VOCs/5-July/NH3 Picarro")

#Read all Picarro from the folder
getstats<- function(Picarro){    #Open each Dat file in a list 
  
  listofdfs <- list() 
  
  for(i in 1:length(Picarro)){ #Loop through the numbers of ID's instead of the ID's
    
    
    
    Pic_Data<-read.table(list_of_files[i],header=TRUE)
    listofdfs[[i]] <- Pic_Data # save your dataframes into the list
  }
  
  return(listofdfs) #Return the list of dataframes.
}

Pic_id<- as.character(c(1:L))
Dat_NH3<-getstats(Picarro = Pic_id) #Able to call each Picarro file
Dat_NH3<-bind_rows(Dat_NH3, .id = "column_label") #Join all elements of the list in one dataframe

#Change format of date and time to be plotted
Dat_NH3$date.time<-paste(Dat_NH3$DATE,Dat_NH3$TIME)
Dat_NH3$date.time<-as.POSIXct(Dat_NH3$date.time,format="%Y-%m-%d %H:%M:%S")

##Extract data from BackPack picarro##  
setwd("O:/Tech_BCE/Environmental engineering/Air Quality Engineering/Picarro interferences (Anna&Pablo)/Interferences/VOCs/5-July")
list_of_files <- list.files(path="BackPack",pattern = "*.dat",recursive=TRUE) ##Read data from Picarro
L<-length(list_of_files)

setwd("O:/Tech_BCE/Environmental engineering/Air Quality Engineering/Picarro interferences (Anna&Pablo)/Interferences/VOCs/5-July/BackPack")

#Read all Picarro from the folder
getstats<- function(Picarro){    #Open each Dat file in a list 
  
  listofdfs <- list() 
  
  for(i in 1:length(Picarro)){ #Loop through the numbers of ID's instead of the ID's
    
    
    
    Pic_Data<-read.table(list_of_files[i],header=TRUE)
    listofdfs[[i]] <- Pic_Data # save your dataframes into the list
  }
  
  return(listofdfs) #Return the list of dataframes.
}

Pic_id<- as.character(c(1:L))
Dat_BP<-getstats(Picarro = Pic_id) #Able to call each Picarro file
Dat_BP<-bind_rows(Dat_BP, .id = "column_label") #Join all elements of the list in one dataframe

#Change format of date and time to be plotted
Dat_BP$date.time<-paste(Dat_BP$DATE,Dat_BP$TIME)
Dat_BP$date.time<-as.POSIXct(Dat_BP$date.time,format="%Y-%m-%d %H:%M:%S")

##Extract data from PTR-TOF##  
setwd("O:/Tech_BCE/Environmental engineering/Air Quality Engineering/Picarro interferences (Anna&Pablo)/Interferences/VOCs/5-July/PTR-TOF")
P2<-read_excel("2-Propanol.xlsx")
P2<-as.data.frame(P2)
Ac<-read_excel("Acetone.xlsx")
Ac<-as.data.frame(Ac)
BAc<-read_excel("Butyric acid.xlsx")
BAc<-as.data.frame(BAc)
MeOH<-read_excel("Methanol.xlsx")
MeOH<-as.data.frame(MeOH)




###Data to plot all the calibration###
##Acetone##

Time_PTR_Ac<-c(Ac$AbsTime)
Ac_PTR<-c(Ac$`m59.0491 (C3H6OH+)`)

#Sincronize all Picarros and PTR
TAc_PTR<-round(as.numeric(Time_PTR_Ac[c(1,length(Time_PTR_Ac))]))
TAc_PTR<-TAc_PTR-as.numeric(seconds(90))
TAc_PTRn<-TAc_PTR-as.numeric(minutes(177))
TAc_PTRb<-TAc_PTR-as.numeric(minutes(119))
TAc_All<-round(as.numeric(Dat_All$date.time))
TAc_NH3<-round(as.numeric(Dat_NH3$date.time))
TAc_BP<-round(as.numeric(Dat_BP$date.time))
PAc<-list()
PAc_N<-list()
PAc_C<-list()
min_dif_Ac<-1
min_dif_AcN<-1
min_dif_AcC<-1

for (i in 1:(length(TAc_PTR))){
  min_dif_Ac[i]<-min(abs(TAc_All-TAc_PTR[i]))
  PAc[[i]]<-which(TAc_All-TAc_PTR[i]==min_dif_Ac[i])
  PAc[[i]]<-PAc[[i]][1]
  min_dif_AcN[i]<-min(abs(TAc_NH3-TAc_PTRn[i]))
  PAc_N[[i]]<-which(TAc_NH3-TAc_PTRn[i]==min_dif_AcN[i])
  PAc_N[[i]]<-PAc_N[[i]][1]
  min_dif_AcC[i]<-min(abs(TAc_BP-TAc_PTRb[i]))
  PAc_C[[i]]<-which(TAc_BP-TAc_PTRb[i]==min_dif_AcC[i])
  PAc_C[[i]]<-PAc_C[[i]][1]
}
tAc<-unlist(PAc)
tAcn<-unlist(PAc_N)
tAcc<-unlist(PAc_C)
Time_All_Ac<-c(Dat_All$date.time[tAc[1]:tAc[2]])
Time_BP_Ac<-c(Dat_BP$date.time[tAcc[1]:tAcc[2]])
Time_NH3_Ac<-c(Dat_NH3$date.time[tAcn[1]:tAcn[2]])
H2O_All_Ac<-c(Dat_All$H2O[tAc[1]:tAc[2]])
H2O_BP_Ac<-c(Dat_BP$H2O[tAcc[1]:tAcc[2]])
H2O_NH3_Ac<-c(Dat_NH3$H2O[tAcn[1]:tAcn[2]])
NH3_All_Ac<-c(Dat_All$NH3[tAc[1]:tAc[2]])
NH3r_NH3_Ac<-c(Dat_NH3$NH3_Raw[tAcn[1]:tAcn[2]])
NH3d_NH3_Ac<-c(Dat_NH3$NH3_dry[tAcn[1]:tAcn[2]])
CH4_All_Ac<-c(Dat_All$CH4_dry[tAc[1]:tAc[2]])
CH4_BP_Ac<-c(Dat_BP$CH4[tAcc[1]:tAcc[2]])
CH4d_BP_Ac<-c(Dat_BP$CH4_dry[tAcc[1]:tAcc[2]])
N2O_All_Ac<-c(Dat_All$N2O_dry[tAc[1]:tAc[2]])
#### Save the intervals for publication ####
setwd("C:/Users/au615105/OneDrive - Aarhus universitet/Pablo Garc?a/Papers/Interferences pictures/VOCs")
Interval_Ac<-cbind.data.frame(tAc,tAcn,tAcc)
write.table(Interval_Ac, file = "Ac_Interval.txt", sep = "\t", row.names = TRUE, col.names = NA)

##Butyric/butanoic acid##

Time_PTR_BAc<-c(BAc$AbsTime)
BAc_PTR<-c(BAc$`m89.0597 (C4H8O2H+)`+BAc$`m71.0491 (C4H6OH+)`)

#Sincronize all Picarros and PTR
TAc_PTR<-round(as.numeric(Time_PTR_BAc[c(1,length(Time_PTR_BAc))]))
TAc_PTR<-TAc_PTR-as.numeric(seconds(90))
TAc_PTRn<-TAc_PTR-as.numeric(minutes(177))
TAc_PTRb<-TAc_PTR-as.numeric(minutes(119))
TAc_All<-round(as.numeric(Dat_All$date.time))
TAc_NH3<-round(as.numeric(Dat_NH3$date.time))
TAc_BP<-round(as.numeric(Dat_BP$date.time))
PAc<-list()
PAc_N<-list()
PAc_C<-list()
min_dif_BAc<-1
min_dif_BAcN<-1
min_dif_BAcC<-1

for (i in 1:(length(TAc_PTR))){
  min_dif_BAc[i]<-min(abs(TAc_All-TAc_PTR[i]))
  PAc[[i]]<-which(TAc_All-TAc_PTR[i]==min_dif_BAc[i])
  PAc[[i]]<-PAc[[i]][1]
  min_dif_BAcN[i]<-min(abs(TAc_NH3-TAc_PTRn[i]))
  PAc_N[[i]]<-which(TAc_NH3-TAc_PTRn[i]==min_dif_BAcN[i])
  PAc_N[[i]]<-PAc_N[[i]][1]
  min_dif_BAcC[i]<-min(abs(TAc_BP-TAc_PTRb[i]))
  PAc_C[[i]]<-which(TAc_BP-TAc_PTRb[i]==min_dif_BAcC[i])
  PAc_C[[i]]<-PAc_C[[i]][1]
}
tAc<-unlist(PAc)
tAcn<-unlist(PAc_N)
tAcc<-unlist(PAc_C)
Time_All_BAc<-c(Dat_All$date.time[tAc[1]:tAc[2]])
Time_BP_BAc<-c(Dat_BP$date.time[tAcc[1]:tAcc[2]])
Time_NH3_BAc<-c(Dat_NH3$date.time[tAcn[1]:tAcn[2]])
H2O_All_BAc<-c(Dat_All$H2O[tAc[1]:tAc[2]])
H2O_BP_BAc<-c(Dat_BP$H2O[tAcc[1]:tAcc[2]])
H2O_NH3_BAc<-c(Dat_NH3$H2O[tAcn[1]:tAcn[2]])
NH3_All_BAc<-c(Dat_All$NH3[tAc[1]:tAc[2]])
NH3r_NH3_BAc<-c(Dat_NH3$NH3_Raw[tAcn[1]:tAcn[2]])
NH3d_NH3_BAc<-c(Dat_NH3$NH3_dry[tAcn[1]:tAcn[2]])
CH4_All_BAc<-c(Dat_All$CH4_dry[tAc[1]:tAc[2]])
CH4_BP_BAc<-c(Dat_BP$CH4[tAcc[1]:tAcc[2]])
CH4d_BP_BAc<-c(Dat_BP$CH4_dry[tAcc[1]:tAcc[2]])
N2O_All_BAc<-c(Dat_All$N2O_dry[tAc[1]:tAc[2]])

#### Save the intervals for publication ####
setwd("C:/Users/au615105/OneDrive - Aarhus universitet/Pablo Garc?a/Papers/Interferences pictures/VOCs")
Interval_BAc<-cbind.data.frame(tAc,tAcn,tAcc)
write.table(Interval_BAc, file = "BAc_Interval.txt", sep = "\t", row.names = TRUE, col.names = NA)

##Methanol##

Time_PTR_MeOH<-c(MeOH$AbsTime) 
MeOH_PTR<-c(MeOH$`m33.0335 (CH4OH+)`)

#Sincronize all Picarros and PTR
TAc_PTR<-round(as.numeric(Time_PTR_MeOH[c(1,length(Time_PTR_MeOH))]))
TAc_PTR<-TAc_PTR-as.numeric(seconds(90))
TAc_PTRn<-TAc_PTR-as.numeric(minutes(177))
TAc_PTRb<-TAc_PTR-as.numeric(minutes(119))
TAc_All<-round(as.numeric(Dat_All$date.time))
TAc_NH3<-round(as.numeric(Dat_NH3$date.time))
TAc_BP<-round(as.numeric(Dat_BP$date.time))
PAc<-list()
PAc_N<-list()
PAc_C<-list()
min_dif_BAc<-1
min_dif_BAcN<-1
min_dif_BAcC<-1

for (i in 1:(length(TAc_PTR))){
  min_dif_BAc[i]<-min(abs(TAc_All-TAc_PTR[i]))
  PAc[[i]]<-which(TAc_All-TAc_PTR[i]==min_dif_BAc[i])
  PAc[[i]]<-PAc[[i]][1]
  min_dif_BAcN[i]<-min(abs(TAc_NH3-TAc_PTRn[i]))
  PAc_N[[i]]<-which(TAc_NH3-TAc_PTRn[i]==min_dif_BAcN[i])
  PAc_N[[i]]<-PAc_N[[i]][1]
  min_dif_BAcC[i]<-min(abs(TAc_BP-TAc_PTRb[i]))
  PAc_C[[i]]<-which(TAc_BP-TAc_PTRb[i]==min_dif_BAcC[i])
  PAc_C[[i]]<-PAc_C[[i]][1]
}
tAc<-unlist(PAc)
tAcn<-unlist(PAc_N)
tAcc<-unlist(PAc_C)
Time_All_MeOH<-c(Dat_All$date.time[tAc[1]:tAc[2]])
Time_BP_MeOH<-c(Dat_BP$date.time[tAcc[1]:tAcc[2]])
Time_NH3_MeOH<-c(Dat_NH3$date.time[tAcn[1]:tAcn[2]])
H2O_All_MeOH<-c(Dat_All$H2O[tAc[1]:tAc[2]])
H2O_BP_MeOH<-c(Dat_BP$H2O[tAcc[1]:tAcc[2]])
H2O_NH3_MeOH<-c(Dat_NH3$H2O[tAcn[1]:tAcn[2]])
NH3_All_MeOH<-c(Dat_All$NH3[tAc[1]:tAc[2]])
NH3r_NH3_MeOH<-c(Dat_NH3$NH3_Raw[tAcn[1]:tAcn[2]])
NH3d_NH3_MeOH<-c(Dat_NH3$NH3_dry[tAcn[1]:tAcn[2]])
CH4_All_MeOH<-c(Dat_All$CH4_dry[tAc[1]:tAc[2]])
CH4_BP_MeOH<-c(Dat_BP$CH4[tAcc[1]:tAcc[2]])
CH4d_BP_MeOH<-c(Dat_BP$CH4_dry[tAcc[1]:tAcc[2]])
N2O_All_MeOH<-c(Dat_All$N2O_dry[tAc[1]:tAc[2]])
setwd("C:/Users/au615105/OneDrive - Aarhus universitet/Pablo Garc?a/Papers/Interferences pictures/VOCs")
Interval_MeOH<-cbind.data.frame(tAc,tAcn,tAcc)
write.table(Interval_MeOH, file = "MeOH_Interval.txt", sep = "\t", row.names = TRUE, col.names = NA)

##2-Propanol##

Time_PTR_P2<-c(P2$AbsTime-hours(2)) #Forgot to delete the 2 hours ofset in the original excel
P2_PTR<-c(P2$`m39.0229 (C3H3+)`+P2$`m41.0386 (C3H5+)`+P2$`m43.0542 (C3H6H+)`)

#Sincronize all Picarros and PTR
TAc_PTR<-round(as.numeric(Time_PTR_P2[c(1,length(Time_PTR_P2))]))
TAc_PTR<-TAc_PTR-as.numeric(seconds(90))
TAc_PTRn<-TAc_PTR-as.numeric(minutes(177))
TAc_PTRb<-TAc_PTR-as.numeric(minutes(119))
TAc_All<-round(as.numeric(Dat_All$date.time))
TAc_NH3<-round(as.numeric(Dat_NH3$date.time))
TAc_BP<-round(as.numeric(Dat_BP$date.time))
PAc<-list()
PAc_N<-list()
PAc_C<-list()
min_dif_BAc<-1
min_dif_BAcN<-1
min_dif_BAcC<-1

for (i in 1:(length(TAc_PTR))){
  min_dif_BAc[i]<-min(abs(TAc_All-TAc_PTR[i]))
  PAc[[i]]<-which(TAc_All-TAc_PTR[i]==min_dif_BAc[i])
  PAc[[i]]<-PAc[[i]][1]
  min_dif_BAcN[i]<-min(abs(TAc_NH3-TAc_PTRn[i]))
  PAc_N[[i]]<-which(TAc_NH3-TAc_PTRn[i]==min_dif_BAcN[i])
  PAc_N[[i]]<-PAc_N[[i]][1]
  min_dif_BAcC[i]<-min(abs(TAc_BP-TAc_PTRb[i]))
  PAc_C[[i]]<-which(TAc_BP-TAc_PTRb[i]==min_dif_BAcC[i])
  PAc_C[[i]]<-PAc_C[[i]][1]
}
tAc<-unlist(PAc)
tAcn<-unlist(PAc_N)
tAcc<-unlist(PAc_C)
Time_All_P2<-c(Dat_All$date.time[tAc[1]:tAc[2]])
Time_BP_P2<-c(Dat_BP$date.time[tAcc[1]:tAcc[2]])
Time_NH3_P2<-c(Dat_NH3$date.time[tAcn[1]:tAcn[2]])
H2O_All_P2<-c(Dat_All$H2O[tAc[1]:tAc[2]])
H2O_BP_P2<-c(Dat_BP$H2O[tAcc[1]:tAcc[2]])
H2O_NH3_P2<-c(Dat_NH3$H2O[tAcn[1]:tAcn[2]])
NH3_All_P2<-c(Dat_All$NH3[tAc[1]:tAc[2]])
NH3r_NH3_P2<-c(Dat_NH3$NH3_Raw[tAcn[1]:tAcn[2]])
NH3d_NH3_P2<-c(Dat_NH3$NH3_dry[tAcn[1]:tAcn[2]])
CH4_All_P2<-c(Dat_All$CH4_dry[tAc[1]:tAc[2]])
CH4_BP_P2<-c(Dat_BP$CH4[tAcc[1]:tAcc[2]])
CH4d_BP_P2<-c(Dat_BP$CH4_dry[tAcc[1]:tAcc[2]])
N2O_All_P2<-c(Dat_All$N2O_dry[tAc[1]:tAc[2]])
#### Save the intervals for publication ####
setwd("C:/Users/au615105/OneDrive - Aarhus universitet/Pablo Garc?a/Papers/Interferences pictures/VOCs")
Interval_P2<-cbind.data.frame(tAc,tAcn,tAcc)
write.table(Interval_P2, file = "P2_Interval.txt", sep = "\t", row.names = TRUE, col.names = NA)

###Data to plot the interferences at each concentration###
##Acetone
Ac0_PTR<-mean(Ac_PTR[(270-65):270])
Ac1_PTR<-mean(Ac_PTR[(490-65):490])
Ac2_PTR<-mean(Ac_PTR[(710-65):710])
Ac3_PTR<-mean(Ac_PTR[730:795])
Ac4_PTR<-mean(Ac_PTR[(1230-65):1230])
Ac5_PTR<-mean(Ac_PTR[(1410-65):1410])
Ac6_PTR<-mean(Ac_PTR[(1570-65):1570])
Ac7_PTR<-mean(Ac_PTR[(1990-65):1990])
Ac8_PTR<-mean(Ac_PTR[(2230-65):2230])
Ac9_PTR<-mean(Ac_PTR[(2570-65):2570])
Ac10_PTR<-mean(Ac_PTR[(2790-65):2790])

sd_Ac0_PTR<-sd(Ac_PTR[(270-65):270])
sd_Ac1_PTR<-sd(Ac_PTR[(490-65):490])
sd_Ac2_PTR<-sd(Ac_PTR[(710-65):710])
sd_Ac3_PTR<-sd(Ac_PTR[730:795])
sd_Ac4_PTR<-sd(Ac_PTR[(1230-65):1230])
sd_Ac5_PTR<-sd(Ac_PTR[(1410-65):1410])
sd_Ac6_PTR<-sd(Ac_PTR[(1570-65):1570])
sd_Ac7_PTR<-sd(Ac_PTR[(1990-65):1990])
sd_Ac8_PTR<-sd(Ac_PTR[(2230-65):2230])
sd_Ac9_PTR<-sd(Ac_PTR[(2570-65):2570])
sd_Ac10_PTR<-sd(Ac_PTR[(2790-65):2790])

length_Ac0_PTR<-length(Ac_PTR[(270-65):270])
length_Ac1_PTR<-length(Ac_PTR[(490-65):490])
length_Ac2_PTR<-length(Ac_PTR[(710-65):710])
length_Ac3_PTR<-length(Ac_PTR[730:795])
length_Ac4_PTR<-length(Ac_PTR[(1230-65):1230])
length_Ac5_PTR<-length(Ac_PTR[(1410-65):1410])
length_Ac6_PTR<-length(Ac_PTR[(1570-65):1570])
length_Ac7_PTR<-length(Ac_PTR[(1990-65):1990])
length_Ac8_PTR<-length(Ac_PTR[(2230-65):2230])
length_Ac9_PTR<-length(Ac_PTR[(2570-65):2570])
length_Ac10_PTR<-length(Ac_PTR[(2790-65):2790])

Tinterval_Ac0_PTR<-difftime(Time_PTR_Ac[(270-65)],Time_PTR_Ac[270])
Tinterval_Ac1_PTR<-difftime(Time_PTR_Ac[(490-65)],Time_PTR_Ac[490])
Tinterval_Ac2_PTR<-difftime(Time_PTR_Ac[(710-65)],Time_PTR_Ac[710])
Tinterval_Ac3_PTR<-difftime(Time_PTR_Ac[730],Time_PTR_Ac[795])
Tinterval_Ac4_PTR<-difftime(Time_PTR_Ac[(1230-65)],Time_PTR_Ac[1230])
Tinterval_Ac5_PTR<-difftime(Time_PTR_Ac[(1410-65)],Time_PTR_Ac[1410])
Tinterval_Ac6_PTR<-difftime(Time_PTR_Ac[(1570-65)],Time_PTR_Ac[1570])
Tinterval_Ac7_PTR<-difftime(Time_PTR_Ac[(1990-65)],Time_PTR_Ac[1990])
Tinterval_Ac8_PTR<-difftime(Time_PTR_Ac[(2230-65)],Time_PTR_Ac[2230])
Tinterval_Ac9_PTR<-difftime(Time_PTR_Ac[(2570-65)],Time_PTR_Ac[2570])
Tinterval_Ac10_PTR<-difftime(Time_PTR_Ac[(2790-65)],Time_PTR_Ac[2790])

m_Ac_PTR<-c(Ac0_PTR,Ac1_PTR,Ac2_PTR,Ac3_PTR,Ac4_PTR,Ac5_PTR,Ac6_PTR,Ac7_PTR,Ac8_PTR,Ac9_PTR,Ac10_PTR)
sd_Ac_PTR<-c(sd_Ac0_PTR,sd_Ac1_PTR,sd_Ac2_PTR,sd_Ac3_PTR,sd_Ac4_PTR,sd_Ac5_PTR,sd_Ac6_PTR,sd_Ac7_PTR,sd_Ac8_PTR,sd_Ac9_PTR,sd_Ac10_PTR)
length_Ac_PTR<-c(length_Ac0_PTR,length_Ac1_PTR,length_Ac2_PTR,length_Ac3_PTR,length_Ac4_PTR,length_Ac5_PTR,length_Ac6_PTR,length_Ac7_PTR,length_Ac8_PTR,length_Ac9_PTR,length_Ac10_PTR)
Tinterval_Ac_PTR<-c(Tinterval_Ac0_PTR,Tinterval_Ac1_PTR,Tinterval_Ac2_PTR,Tinterval_Ac3_PTR,Tinterval_Ac4_PTR,Tinterval_Ac5_PTR,Tinterval_Ac6_PTR,Tinterval_Ac7_PTR,Tinterval_Ac8_PTR,Tinterval_Ac9_PTR,Tinterval_Ac10_PTR)

#Select same time intervals in G2508 as in PTR-TOF
Ti_PTR<-round(as.numeric(Time_PTR_Ac[c(270-65,270,490-65,490,710-65,710,730,795,1230-65,1230,1410-65,1410,1570-65,1570,1990-65,1990,2230-65,2230,2570-65,2570,2790-65,2790)]))
Ti_PTR<-Ti_PTR-as.numeric(seconds(90))
numericTime_All<-round(as.numeric(Time_All_Ac))
Pos<-list()
min_dif<-1
for (i in 1:(length(Ti_PTR))){
  min_dif[i]<-min(abs(numericTime_All-Ti_PTR[i]))
  Pos[[i]]<-which(numericTime_All-Ti_PTR[i]==min_dif[i])
  Pos[[i]]<-Pos[[i]][1]
}
t<-unlist(Pos)
### For Publication--> save means for borrowed G2508 ###
write.table(t, file = "Ac_G2509_intervals.txt", sep = "\t", row.names = TRUE, col.names = NA)
########################################################

# Put the calculated positions in G2508
m_NH3_All<-1
sd_NH3_All<-1
length_All<-1
m_H2O_All<-1
sd_H2O_All<-1
m_CH4_All<-1
sd_CH4_All<-1
m_N2O_All<-1
sd_N2O_All<-1
Tinterval_All_Ac<-1

for (i in 1:(length(Ti_PTR)-1)){
  m_NH3_All[i]<-mean(NH3_All_Ac[t[i]:t[i+1]])
  sd_NH3_All[i]<-sd(NH3_All_Ac[t[i]:t[i+1]])
  length_All[i]<-length(NH3_All_Ac[t[i]:t[i+1]])
  m_CH4_All[i]<-mean(CH4_All_Ac[t[i]:t[i+1]])
  sd_CH4_All[i]<-sd(CH4_All_Ac[t[i]:t[i+1]])
  m_N2O_All[i]<-mean(N2O_All_Ac[t[i]:t[i+1]])
  sd_N2O_All[i]<-sd(N2O_All_Ac[t[i]:t[i+1]])
  m_H2O_All[i]<-mean(H2O_All_Ac[t[i]:t[i+1]])
  sd_H2O_All[i]<-sd(H2O_All_Ac[t[i]:t[i+1]])
  Tinterval_All_Ac[i]<-difftime(Time_All_Ac[t[i]],Time_All_Ac[t[i+1]])
}
#Only odd positions are correct (Even positions are from t(2)to t(3))
m_NH3_All_Ac<-m_NH3_All[c(TRUE,FALSE)]
sd_NH3_All_Ac<-sd_NH3_All[c(TRUE,FALSE)]
length_All_Ac<-length_All[c(TRUE,FALSE)]
m_CH4_All_Ac<-m_CH4_All[c(TRUE,FALSE)]
sd_CH4_All_Ac<-sd_CH4_All[c(TRUE,FALSE)]
m_N2O_All_Ac<-m_N2O_All[c(TRUE,FALSE)]
sd_N2O_All_Ac<-sd_N2O_All[c(TRUE,FALSE)]
m_N2O_All_Ac<-m_N2O_All[c(TRUE,FALSE)]
sd_N2O_All_Ac<-sd_N2O_All[c(TRUE,FALSE)]
m_H2O_All_Ac<-m_H2O_All[c(TRUE,FALSE)]
sd_H2O_All_Ac<-sd_H2O_All[c(TRUE,FALSE)]
Tinterval_All_Ac<-Tinterval_All_Ac[c(TRUE,FALSE)]

Ti_PTR<-round(as.numeric(Time_PTR_Ac[c(270-65,270,490-65,490,710-65,710,730,795,1230-65,1230,1410-65,1410,1570-65,1570,1990-65,1990,2230-65,2230,2570-65,2570,2790-65,2790)]))
Ti_PTR<-(Ti_PTR-as.numeric(seconds(90)))-as.numeric(minutes(177))
numericTime_NH3<-round(as.numeric(Time_NH3_Ac))
Pos<-list()
min_dif<-1
for (i in 1:(length(Ti_PTR))){
  min_dif[i]<-min(abs(numericTime_NH3-Ti_PTR[i]))
  Pos[[i]]<-which(numericTime_NH3-Ti_PTR[i]==min_dif[i])
  Pos[[i]]<-Pos[[i]][1]
}
t<-unlist(Pos)
### For Publication--> save means for borrowed G2103 ###
write.table(t, file = "Ac_G2103_intervals.txt", sep = "\t", row.names = TRUE, col.names = NA)
########################################################

## Put the calculated positions in G2508
m_NH3r_NH3<-1
sd_NH3r_NH3<-1
length_NH3<-1
m_NH3d_NH3<-1
sd_NH3d_NH3<-1
m_H2O_NH3<-1
sd_H2O_NH3<-1
Tinterval_NH3_Ac<-1

for (i in 1:(length(Ti_PTR)-1)){
  m_NH3r_NH3[i]<-mean(NH3r_NH3_Ac[t[i]:t[i+1]])
  sd_NH3r_NH3[i]<-sd(NH3r_NH3_Ac[t[i]:t[i+1]])
  length_NH3[i]<-length(NH3r_NH3_Ac[t[i]:t[i+1]])
  m_NH3d_NH3[i]<-mean(NH3d_NH3_Ac[t[i]:t[i+1]])
  sd_NH3d_NH3[i]<-sd(NH3d_NH3_Ac[t[i]:t[i+1]])
  m_H2O_NH3[i]<-mean(H2O_NH3_Ac[t[i]:t[i+1]])
  sd_H2O_NH3[i]<-sd(H2O_NH3_Ac[t[i]:t[i+1]])
  Tinterval_NH3_Ac[i]<-difftime(Time_NH3_Ac[t[i]],Time_NH3_Ac[t[i+1]])
}
#Only odd positions are correct (Even positions are from t(2)to t(3))
m_NH3r_NH3_Ac<-m_NH3r_NH3[c(TRUE,FALSE)]
sd_NH3r_NH3_Ac<-sd_NH3r_NH3[c(TRUE,FALSE)]
length_NH3_Ac<-length_NH3[c(TRUE,FALSE)]
m_NH3d_NH3_Ac<-m_NH3d_NH3[c(TRUE,FALSE)]
sd_NH3d_NH3_Ac<-sd_NH3d_NH3[c(TRUE,FALSE)]
m_H2O_NH3_Ac<-m_H2O_NH3[c(TRUE,FALSE)]
sd_H2O_NH3_Ac<-sd_H2O_NH3[c(TRUE,FALSE)]
Tinterval_NH3_Ac<-Tinterval_NH3_Ac[c(TRUE,FALSE)]

#Select same time intervals in BackPack as in PTR-TOF
Ti_PTR<-round(as.numeric(Time_PTR_Ac[c(270-65,270,490-65,490,710-65,710,730,795,1230-65,1230,1410-65,1410,1570-65,1570,1990-65,1990,2230-65,2230,2570-65,2570,2790-65,2790)]))
Ti_PTR<-(Ti_PTR-as.numeric(seconds(90)))-as.numeric(minutes(119))
numericTime_BP<-round(as.numeric(Time_BP_Ac))
Pos<-list()
min_dif<-1
for (i in 1:(length(Ti_PTR))){
  min_dif[i]<-min(abs(numericTime_BP-Ti_PTR[i]))
  Pos[[i]]<-which(numericTime_BP-Ti_PTR[i]==min_dif[i])
  Pos[[i]]<-Pos[[i]][1]
}
t<-unlist(Pos)
### For Publication--> save means for borrowed G4301 ###
write.table(t, file = "Ac_G4301_intervals.txt", sep = "\t", row.names = TRUE, col.names = NA)
########################################################

## Put the calculated positions in G2508
m_CH4_BP<-1
sd_CH4_BP<-1
length_BP<-1
m_H2O_BP<-1
sd_H2O_BP<-1
m_CH4d_BP<-1
sd_CH4d_BP<-1
Tinterval_BP_Ac<-1

for (i in 1:(length(Ti_PTR)-1)){
  m_CH4_BP[i]<-mean(CH4_BP_Ac[t[i]:t[i+1]])
  sd_CH4_BP[i]<-sd(CH4_BP_Ac[t[i]:t[i+1]])
  length_BP[i]<-length(CH4_BP_Ac[t[i]:t[i+1]])
  m_CH4d_BP[i]<-mean(CH4d_BP_Ac[t[i]:t[i+1]])
  sd_CH4d_BP[i]<-sd(CH4d_BP_Ac[t[i]:t[i+1]])
  m_H2O_BP[i]<-mean(H2O_BP_Ac[t[i]:t[i+1]])
  sd_H2O_BP[i]<-sd(H2O_BP_Ac[t[i]:t[i+1]])
  Tinterval_BP_Ac[i]<-difftime(Time_BP_Ac[t[i]],Time_BP_Ac[t[i+1]])
}
#Only odd positions are correct (Even positions are from t(2)to t(3))
m_CH4_BP_Ac<-m_CH4_BP[c(TRUE,FALSE)]
sd_CH4_BP_Ac<-sd_CH4_BP[c(TRUE,FALSE)]
length_BP_Ac<-length_BP[c(TRUE,FALSE)]
m_CH4d_BP_Ac<-m_CH4d_BP[c(TRUE,FALSE)]
sd_CH4d_BP_Ac<-sd_CH4d_BP[c(TRUE,FALSE)]
m_H2O_BP_Ac<-m_H2O_BP[c(TRUE,FALSE)]
sd_H2O_BP_Ac<-sd_H2O_BP[c(TRUE,FALSE)]
Tinterval_BP_Ac<-Tinterval_BP_Ac[c(TRUE,FALSE)]

## Create and join all points into one data frame ##
#NH3
y_NH3<-c(m_NH3_All_Ac,m_NH3r_NH3_Ac,m_NH3d_NH3_Ac)
x_NH3<-c(m_Ac_PTR,m_Ac_PTR,m_Ac_PTR)
sd_NH3<-c(sd_NH3_All_Ac,sd_NH3r_NH3_Ac,sd_NH3d_NH3_Ac)
Color_NH3<-c(rep("NH3_G2508",11),rep("NH3_raw",11),rep("NH3_dry",11))
df_NH3_Ac<-cbind.data.frame(x_NH3,y_NH3,sd_NH3,Color_NH3)
#CH4
y_CH4<-c(m_CH4_All_Ac,m_CH4_BP_Ac,m_CH4d_BP_Ac)
x_CH4<-c(m_Ac_PTR,m_Ac_PTR,m_Ac_PTR)
sd_CH4<-c(sd_CH4_All_Ac,sd_CH4_BP_Ac,sd_CH4d_BP_Ac)
Color_CH4<-c(rep("CH4_G2508",11),rep("CH4",11),rep("CH4_dry",11))
df_CH4_Ac<-cbind.data.frame(x_CH4,y_CH4,sd_CH4,Color_CH4)
#N2O
y_N2O<-c(m_N2O_All_Ac)
x_N2O<-c(m_Ac_PTR)
sd_N2O<-c(sd_N2O_All_Ac)
Color_N2O<-c(rep("N2O_G2508",11))
df_N2O_Ac<-cbind.data.frame(x_N2O,y_N2O,sd_N2O,Color_N2O)


##Butyric acid
Ac0_PTR<-mean(BAc_PTR[(1999-100):1999])
Ac1_PTR<-mean(BAc_PTR[(2300-100):2300])
Ac2_PTR<-mean(BAc_PTR[(2610-100):2610])
Ac3_PTR<-mean(BAc_PTR[2790:2890])
Ac4_PTR<-mean(BAc_PTR[(3230-100):3230])
Ac5_PTR<-mean(BAc_PTR[(3460-100):3460])
Ac6_PTR<-mean(BAc_PTR[(3660-100):3660])
Ac7_PTR<-mean(BAc_PTR[3900:4000])
Ac8_PTR<-mean(BAc_PTR[(4338-100):4338])
Ac9_PTR<-mean(BAc_PTR[(10438-100):10438])

sd_Ac0_PTR<-sd(BAc_PTR[(1999-100):1999])
sd_Ac1_PTR<-sd(BAc_PTR[(2300-100):2300])
sd_Ac2_PTR<-sd(BAc_PTR[(2610-100):2610])
sd_Ac3_PTR<-sd(BAc_PTR[2790:2890])
sd_Ac4_PTR<-sd(BAc_PTR[(3230-100):3230])
sd_Ac5_PTR<-sd(BAc_PTR[(3460-100):3460])
sd_Ac6_PTR<-sd(BAc_PTR[(3660-100):3660])
sd_Ac7_PTR<-sd(BAc_PTR[3900:4000])
sd_Ac8_PTR<-sd(BAc_PTR[(4338-100):4338])
sd_Ac9_PTR<-sd(BAc_PTR[(10438-100):10438])

length_Ac0_PTR<-length(BAc_PTR[(1999-100):1999])
length_Ac1_PTR<-length(BAc_PTR[(2300-100):2300])
length_Ac2_PTR<-length(BAc_PTR[(2610-100):2610])
length_Ac3_PTR<-length(BAc_PTR[2790:2890])
length_Ac4_PTR<-length(BAc_PTR[(3230-100):3230])
length_Ac5_PTR<-length(BAc_PTR[(3460-100):3460])
length_Ac6_PTR<-length(BAc_PTR[(3660-100):3660])
length_Ac7_PTR<-length(BAc_PTR[3900:4000])
length_Ac8_PTR<-length(BAc_PTR[(4338-100):4338])
length_Ac9_PTR<-length(BAc_PTR[(10438-100):10438])

Tinterval_Ac0_PTR<-difftime(Time_PTR_BAc[(1999-100)],Time_PTR_BAc[1999])
Tinterval_Ac1_PTR<-difftime(Time_PTR_BAc[(2300-100)],Time_PTR_BAc[2300])
Tinterval_Ac2_PTR<-difftime(Time_PTR_BAc[(2610-100)],Time_PTR_BAc[2610])
Tinterval_Ac3_PTR<-difftime(Time_PTR_BAc[2790],Time_PTR_BAc[2890])
Tinterval_Ac4_PTR<-difftime(Time_PTR_BAc[(3230-100)],Time_PTR_BAc[3230])
Tinterval_Ac5_PTR<-difftime(Time_PTR_BAc[(3460-100)],Time_PTR_BAc[3460])
Tinterval_Ac6_PTR<-difftime(Time_PTR_BAc[(3660-100)],Time_PTR_BAc[3660])
Tinterval_Ac7_PTR<-difftime(Time_PTR_BAc[3900],Time_PTR_BAc[4000])
Tinterval_Ac8_PTR<-difftime(Time_PTR_BAc[(4338-100)],Time_PTR_BAc[4338])
Tinterval_Ac9_PTR<-difftime(Time_PTR_BAc[(10438-100)],Time_PTR_BAc[10438])


m_BAc_PTR<-c(Ac0_PTR,Ac1_PTR,Ac2_PTR,Ac3_PTR,Ac4_PTR,Ac5_PTR,Ac6_PTR,Ac7_PTR,Ac8_PTR,Ac9_PTR)
sd_BAc_PTR<-c(sd_Ac0_PTR,sd_Ac1_PTR,sd_Ac2_PTR,sd_Ac3_PTR,sd_Ac4_PTR,sd_Ac5_PTR,sd_Ac6_PTR,sd_Ac7_PTR,sd_Ac8_PTR,sd_Ac9_PTR)
length_BAc_PTR<-c(length_Ac0_PTR,length_Ac1_PTR,length_Ac2_PTR,length_Ac3_PTR,length_Ac4_PTR,length_Ac5_PTR,length_Ac6_PTR,length_Ac7_PTR,length_Ac8_PTR,length_Ac9_PTR)
Tinterval_BAc_PTR<-c(Tinterval_Ac0_PTR,Tinterval_Ac1_PTR,Tinterval_Ac2_PTR,Tinterval_Ac3_PTR,Tinterval_Ac4_PTR,Tinterval_Ac5_PTR,Tinterval_Ac6_PTR,Tinterval_Ac7_PTR,Tinterval_Ac8_PTR,Tinterval_Ac9_PTR)

#Select same time intervals in G2508 as in PTR-TOF
Ti_PTR<-round(as.numeric(Time_PTR_BAc[c(1999-100,1999,2300-100,2300,2610-100,2610,2790,2890,3230-100,3230,3460-100,3460,3660-100,3660,3900,4000,4338-100,4338,10438-100,10438)]))
Ti_PTR<-Ti_PTR-as.numeric(seconds(90))
numericTime_All<-round(as.numeric(Time_All_BAc))
Pos<-list()
min_dif<-1
for (i in 1:(length(Ti_PTR))){
  min_dif[i]<-min(abs(numericTime_All-Ti_PTR[i]))
  Pos[[i]]<-which(numericTime_All-Ti_PTR[i]==min_dif[i])
  Pos[[i]]<-Pos[[i]][1]
}
t<-unlist(Pos)
### For Publication--> save means for borrowed G2508 ###
write.table(t, file = "BAc_G2509_intervals.txt", sep = "\t", row.names = TRUE, col.names = NA)
########################################################

# Put the calculated positions in G2508
m_NH3_All<-1
sd_NH3_All<-1
length_All<-1
m_H2O_All<-1
sd_H2O_All<-1
m_CH4_All<-1
sd_CH4_All<-1
m_N2O_All<-1
sd_N2O_All<-1
Tinterval_All_BAc<-1

for (i in 1:(length(Ti_PTR)-1)){
  m_NH3_All[i]<-mean(NH3_All_BAc[t[i]:t[i+1]])
  sd_NH3_All[i]<-sd(NH3_All_BAc[t[i]:t[i+1]])
  length_All[i]<-length(NH3_All_BAc[t[i]:t[i+1]])
  m_CH4_All[i]<-mean(CH4_All_BAc[t[i]:t[i+1]])
  sd_CH4_All[i]<-sd(CH4_All_BAc[t[i]:t[i+1]])
  m_N2O_All[i]<-mean(N2O_All_BAc[t[i]:t[i+1]])
  sd_N2O_All[i]<-sd(N2O_All_BAc[t[i]:t[i+1]])
  m_H2O_All[i]<-mean(H2O_All_BAc[t[i]:t[i+1]])
  sd_H2O_All[i]<-sd(H2O_All_BAc[t[i]:t[i+1]])
  Tinterval_All_BAc[i]<-difftime(Time_All_BAc[t[i]],Time_All_BAc[t[i+1]])
}
#Only odd positions are correct (Even positions are from t(2)to t(3))
m_NH3_All_BAc<-m_NH3_All[c(TRUE,FALSE)]
sd_NH3_All_BAc<-sd_NH3_All[c(TRUE,FALSE)]
length_All_BAc<-length_All[c(TRUE,FALSE)]
m_CH4_All_BAc<-m_CH4_All[c(TRUE,FALSE)]
sd_CH4_All_BAc<-sd_CH4_All[c(TRUE,FALSE)]
m_N2O_All_BAc<-m_N2O_All[c(TRUE,FALSE)]
sd_N2O_All_BAc<-sd_N2O_All[c(TRUE,FALSE)]
m_N2O_All_BAc<-m_N2O_All[c(TRUE,FALSE)]
sd_N2O_All_BAc<-sd_N2O_All[c(TRUE,FALSE)]
m_H2O_All_BAc<-m_H2O_All[c(TRUE,FALSE)]
sd_H2O_All_BAc<-sd_H2O_All[c(TRUE,FALSE)]
Tinterval_All_BAc<-Tinterval_All_BAc[c(TRUE,FALSE)]

#Select same time intervals in NH3 Picarro as in PTR-TOF
Ti_PTR<-round(as.numeric(Time_PTR_BAc[c(1999-100,1999,2300-100,2300,2610-100,2610,2790,2890,3230-100,3230,3460-100,3460,3660-100,3660,3900,4000,4338-100,4338,10438-100,10438)]))
Ti_PTR<-(Ti_PTR-as.numeric(seconds(90)))-as.numeric(minutes(177))
numericTime_NH3<-round(as.numeric(Time_NH3_BAc))
Pos<-list()
min_dif<-1
for (i in 1:(length(Ti_PTR))){
  min_dif[i]<-min(abs(numericTime_NH3-Ti_PTR[i]))
  Pos[[i]]<-which(numericTime_NH3-Ti_PTR[i]==min_dif[i])
  Pos[[i]]<-Pos[[i]][1]
}
t<-unlist(Pos)
### For Publication--> save means for borrowed G2508 ###
write.table(t, file = "BAc_G2103_intervals.txt", sep = "\t", row.names = TRUE, col.names = NA)
########################################################

## Put the calculated positions in G2103
m_NH3r_NH3<-1
sd_NH3r_NH3<-1
length_NH3<-1
m_NH3d_NH3<-1
sd_NH3d_NH3<-1
m_H2O_NH3<-1
sd_H2O_NH3<-1
Tinterval_NH3_BAc<-1

for (i in 1:(length(Ti_PTR)-1)){
  m_NH3r_NH3[i]<-mean(NH3r_NH3_BAc[t[i]:t[i+1]])
  sd_NH3r_NH3[i]<-sd(NH3r_NH3_BAc[t[i]:t[i+1]])
  length_NH3[i]<-length(NH3r_NH3_BAc[t[i]:t[i+1]])
  m_NH3d_NH3[i]<-mean(NH3d_NH3_BAc[t[i]:t[i+1]])
  sd_NH3d_NH3[i]<-sd(NH3d_NH3_BAc[t[i]:t[i+1]])
  m_H2O_NH3[i]<-mean(H2O_NH3_BAc[t[i]:t[i+1]])
  sd_H2O_NH3[i]<-sd(H2O_NH3_BAc[t[i]:t[i+1]])
  Tinterval_NH3_BAc[i]<-difftime(Time_NH3_BAc[t[i]],Time_NH3_BAc[t[i+1]])
}
#Only odd positions are correct (Even positions are from t(2)to t(3))
m_NH3r_NH3_BAc<-m_NH3r_NH3[c(TRUE,FALSE)]
sd_NH3r_NH3_BAc<-sd_NH3r_NH3[c(TRUE,FALSE)]
length_NH3_BAc<-length_NH3[c(TRUE,FALSE)]
m_NH3d_NH3_BAc<-m_NH3d_NH3[c(TRUE,FALSE)]
sd_NH3d_NH3_BAc<-sd_NH3d_NH3[c(TRUE,FALSE)]
m_H2O_NH3_BAc<-m_H2O_NH3[c(TRUE,FALSE)]
sd_H2O_NH3_BAc<-sd_H2O_NH3[c(TRUE,FALSE)]
Tinterval_NH3_BAc<-Tinterval_NH3_BAc[c(TRUE,FALSE)]

#Select same time intervals in BackPack as in PTR-TOF
Ti_PTR<-round(as.numeric(Time_PTR_BAc[c(1999-100,1999,2300-100,2300,2610-100,2610,2790,2890,3230-100,3230,3460-100,3460,3660-100,3660,3900,4000,4338-100,4338,10438-100,10438)]))
Ti_PTR<-(Ti_PTR-as.numeric(seconds(90)))-as.numeric(minutes(119))
numericTime_BP<-round(as.numeric(Time_BP_BAc))
Pos<-list()
min_dif<-1
for (i in 1:(length(Ti_PTR))){
  min_dif[i]<-min(abs(numericTime_BP-Ti_PTR[i]))
  Pos[[i]]<-which(numericTime_BP-Ti_PTR[i]==min_dif[i])
  Pos[[i]]<-Pos[[i]][1]
}
t<-unlist(Pos)
### For Publication--> save means for borrowed G2508 ###
write.table(t, file = "BAc_G4301_intervals.txt", sep = "\t", row.names = TRUE, col.names = NA)
########################################################

## Put the calculated positions in G4301
m_CH4_BP<-1
sd_CH4_BP<-1
length_BP<-1
m_H2O_BP<-1
sd_H2O_BP<-1
m_CH4d_BP<-1
sd_CH4d_BP<-1
Tinterval_BP_BAc<-1

for (i in 1:(length(Ti_PTR)-1)){
  m_CH4_BP[i]<-mean(CH4_BP_BAc[t[i]:t[i+1]])
  sd_CH4_BP[i]<-sd(CH4_BP_BAc[t[i]:t[i+1]])
  length_BP[i]<-length(CH4_BP_BAc[t[i]:t[i+1]])
  m_CH4d_BP[i]<-mean(CH4d_BP_BAc[t[i]:t[i+1]])
  sd_CH4d_BP[i]<-sd(CH4d_BP_BAc[t[i]:t[i+1]])
  m_H2O_BP[i]<-mean(H2O_BP_BAc[t[i]:t[i+1]])
  sd_H2O_BP[i]<-sd(H2O_BP_BAc[t[i]:t[i+1]])
  Tinterval_BP_BAc[i]<-difftime(Time_BP_BAc[t[i]],Time_BP_BAc[t[i+1]])
}
#Only odd positions are correct (Even positions are from t(2)to t(3))
m_CH4_BP_BAc<-m_CH4_BP[c(TRUE,FALSE)]
sd_CH4_BP_BAc<-sd_CH4_BP[c(TRUE,FALSE)]
length_BP_BAc<-length_BP[c(TRUE,FALSE)]
m_CH4d_BP_BAc<-m_CH4d_BP[c(TRUE,FALSE)]
sd_CH4d_BP_BAc<-sd_CH4d_BP[c(TRUE,FALSE)]
m_H2O_BP_BAc<-m_H2O_BP[c(TRUE,FALSE)]
sd_H2O_BP_BAc<-sd_H2O_BP[c(TRUE,FALSE)]
Tinterval_BP_BAc<-Tinterval_BP_BAc[c(TRUE,FALSE)]

## Create and join all points into one data frame ##
#NH3
y_NH3<-c(m_NH3_All_BAc,m_NH3r_NH3_BAc,m_NH3d_NH3_BAc)
x_NH3<-c(m_BAc_PTR,m_BAc_PTR,m_BAc_PTR)
sd_NH3<-c(sd_NH3_All_BAc,sd_NH3r_NH3_BAc,sd_NH3d_NH3_BAc)
Color_NH3<-c(rep("NH3_G2508",10),rep("NH3_raw",10),rep("NH3_dry",10))
df_NH3_BAc<-cbind.data.frame(x_NH3,y_NH3,sd_NH3,Color_NH3)
#CH4
y_CH4<-c(m_CH4_All_BAc,m_CH4_BP_BAc,m_CH4d_BP_BAc)
x_CH4<-c(m_BAc_PTR,m_BAc_PTR,m_BAc_PTR)
sd_CH4<-c(sd_CH4_All_BAc,sd_CH4_BP_BAc,sd_CH4d_BP_BAc)
Color_CH4<-c(rep("CH4_G2508",10),rep("CH4",10),rep("CH4_dry",10))
df_CH4_BAc<-cbind.data.frame(x_CH4,y_CH4,sd_CH4,Color_CH4)
#N2O
y_N2O<-c(m_N2O_All_BAc)
x_N2O<-c(m_BAc_PTR)
sd_N2O<-c(sd_N2O_All_BAc)
Color_N2O<-c(rep("N2O_G2508",10))
df_N2O_BAc<-cbind.data.frame(x_N2O,y_N2O,sd_N2O,Color_N2O)

##Methanol
Ac0_PTR<-mean(MeOH_PTR[(730-160):730])
Ac1_PTR<-mean(MeOH_PTR[(1000-160):1000])
Ac2_PTR<-mean(MeOH_PTR[(1420-160):1420])
Ac3_PTR<-mean(MeOH_PTR[(1670-160):1670])
Ac4_PTR<-mean(MeOH_PTR[1770:1930])
Ac5_PTR<-mean(MeOH_PTR[(2240-160):2240])
Ac6_PTR<-mean(MeOH_PTR[(2490-160):2490])
Ac7_PTR<-mean(MeOH_PTR[(2780-160):2780])
Ac8_PTR<-mean(MeOH_PTR[(2980-160):2980])
Ac9_PTR<-mean(MeOH_PTR[(3190-160):3190])
Ac10_PTR<-mean(MeOH_PTR[(3590-160):3590])

sd_Ac0_PTR<-sd(MeOH_PTR[(730-160):730])
sd_Ac1_PTR<-sd(MeOH_PTR[(1000-160):1000])
sd_Ac2_PTR<-sd(MeOH_PTR[(1420-160):1420])
sd_Ac3_PTR<-sd(MeOH_PTR[(1670-160):1670])
sd_Ac4_PTR<-sd(MeOH_PTR[1770:1930])
sd_Ac5_PTR<-sd(MeOH_PTR[(2240-160):2240])
sd_Ac6_PTR<-sd(MeOH_PTR[(2490-160):2490])
sd_Ac7_PTR<-sd(MeOH_PTR[(2780-160):2780])
sd_Ac8_PTR<-sd(MeOH_PTR[(2980-160):2980])
sd_Ac9_PTR<-sd(MeOH_PTR[(3190-160):3190])
sd_Ac10_PTR<-sd(MeOH_PTR[(3590-160):3590])

length_Ac0_PTR<-length(MeOH_PTR[(730-160):730])
length_Ac1_PTR<-length(MeOH_PTR[(1000-160):1000])
length_Ac2_PTR<-length(MeOH_PTR[(1420-160):1420])
length_Ac3_PTR<-length(MeOH_PTR[(1670-160):1670])
length_Ac4_PTR<-length(MeOH_PTR[1770:1930])
length_Ac5_PTR<-length(MeOH_PTR[(2240-160):2240])
length_Ac6_PTR<-length(MeOH_PTR[(2490-160):2490])
length_Ac7_PTR<-length(MeOH_PTR[(2780-160):2780])
length_Ac8_PTR<-length(MeOH_PTR[(2980-160):2980])
length_Ac9_PTR<-length(MeOH_PTR[(3190-160):3190])
length_Ac10_PTR<-length(MeOH_PTR[(3590-160):3590])

Tinterval_Ac0_PTR<-difftime(Time_PTR_MeOH[(730-160)],Time_PTR_MeOH[730])
Tinterval_Ac1_PTR<-difftime(Time_PTR_MeOH[(1000-160)],Time_PTR_MeOH[1000])
Tinterval_Ac2_PTR<-difftime(Time_PTR_MeOH[(1420-160)],Time_PTR_MeOH[1420])
Tinterval_Ac3_PTR<-difftime(Time_PTR_MeOH[(1670-160)],Time_PTR_MeOH[1670])
Tinterval_Ac4_PTR<-difftime(Time_PTR_MeOH[1770],Time_PTR_MeOH[1930])
Tinterval_Ac5_PTR<-difftime(Time_PTR_MeOH[(2240-160)],Time_PTR_MeOH[2240])
Tinterval_Ac6_PTR<-difftime(Time_PTR_MeOH[(2490-160)],Time_PTR_MeOH[2490])
Tinterval_Ac7_PTR<-difftime(Time_PTR_MeOH[(2780-160)],Time_PTR_MeOH[2780])
Tinterval_Ac8_PTR<-difftime(Time_PTR_MeOH[(2980-160)],Time_PTR_MeOH[2980])
Tinterval_Ac9_PTR<-difftime(Time_PTR_MeOH[(3190-160)],Time_PTR_MeOH[3190])
Tinterval_Ac10_PTR<-difftime(Time_PTR_MeOH[(3590-160)],Time_PTR_MeOH[3590])

m_MeOH_PTR<-c(Ac0_PTR,Ac1_PTR,Ac2_PTR,Ac3_PTR,Ac4_PTR,Ac5_PTR,Ac6_PTR,Ac7_PTR,Ac8_PTR,Ac9_PTR,Ac10_PTR)
sd_MeOH_PTR<-c(sd_Ac0_PTR,sd_Ac1_PTR,sd_Ac2_PTR,sd_Ac3_PTR,sd_Ac4_PTR,sd_Ac5_PTR,sd_Ac6_PTR,sd_Ac7_PTR,sd_Ac8_PTR,sd_Ac9_PTR,sd_Ac10_PTR)
length_MeOH_PTR<-c(length_Ac0_PTR,length_Ac1_PTR,length_Ac2_PTR,length_Ac3_PTR,length_Ac4_PTR,length_Ac5_PTR,length_Ac6_PTR,length_Ac7_PTR,length_Ac8_PTR,length_Ac9_PTR,length_Ac10_PTR)
Tinterval_MeOH_PTR<-c(Tinterval_Ac0_PTR,Tinterval_Ac1_PTR,Tinterval_Ac2_PTR,Tinterval_Ac3_PTR,Tinterval_Ac4_PTR,Tinterval_Ac5_PTR,Tinterval_Ac6_PTR,Tinterval_Ac7_PTR,Tinterval_Ac8_PTR,Tinterval_Ac9_PTR,Tinterval_Ac10_PTR)

#Select same time intervals in G2508 as in PTR-TOF
Ti_PTR<-round(as.numeric(Time_PTR_MeOH[c(730-160,730,1000-160,1000,1420-160,1420,1670-160,1670,1770,1930,2240-160,2240,2490-160,2490,2780-160,2780,2980-160,2980,3190-160,3190,3590-160,3590)]))
Ti_PTR<-Ti_PTR-as.numeric(seconds(90))
numericTime_All<-round(as.numeric(Time_All_MeOH))
Pos<-list()
min_dif<-1
for (i in 1:(length(Ti_PTR))){
  min_dif[i]<-min(abs(numericTime_All-Ti_PTR[i]))
  Pos[[i]]<-which(numericTime_All-Ti_PTR[i]==min_dif[i])
  Pos[[i]]<-Pos[[i]][1]
}
t<-unlist(Pos)
### For Publication--> save means for borrowed G2508 ###
write.table(t, file = "MeOH_G2509_intervals.txt", sep = "\t", row.names = TRUE, col.names = NA)
########################################################


# Put the calculated positions in G2508
m_NH3_All<-1
sd_NH3_All<-1
length_All<-1
m_H2O_All<-1
sd_H2O_All<-1
m_CH4_All<-1
sd_CH4_All<-1
m_N2O_All<-1
sd_N2O_All<-1
Tinterval_All_MeOH<-1

for (i in 1:(length(Ti_PTR)-1)){
  m_NH3_All[i]<-mean(NH3_All_MeOH[t[i]:t[i+1]])
  sd_NH3_All[i]<-sd(NH3_All_MeOH[t[i]:t[i+1]])
  length_All[i]<-length(NH3_All_MeOH[t[i]:t[i+1]])
  m_CH4_All[i]<-mean(CH4_All_MeOH[t[i]:t[i+1]])
  sd_CH4_All[i]<-sd(CH4_All_MeOH[t[i]:t[i+1]])
  m_N2O_All[i]<-mean(N2O_All_MeOH[t[i]:t[i+1]])
  sd_N2O_All[i]<-sd(N2O_All_MeOH[t[i]:t[i+1]])
  m_H2O_All[i]<-mean(H2O_All_MeOH[t[i]:t[i+1]])
  sd_H2O_All[i]<-sd(H2O_All_MeOH[t[i]:t[i+1]])
  Tinterval_All_MeOH[i]<-difftime(Time_All_MeOH[t[i]],Time_All_MeOH[t[i+1]])
}
#Only odd positions are correct (Even positions are from t(2)to t(3))
m_NH3_All_MeOH<-m_NH3_All[c(TRUE,FALSE)]
sd_NH3_All_MeOH<-sd_NH3_All[c(TRUE,FALSE)]
length_All_MeOH<-length_All[c(TRUE,FALSE)]
m_CH4_All_MeOH<-m_CH4_All[c(TRUE,FALSE)]
sd_CH4_All_MeOH<-sd_CH4_All[c(TRUE,FALSE)]
m_N2O_All_MeOH<-m_N2O_All[c(TRUE,FALSE)]
sd_N2O_All_MeOH<-sd_N2O_All[c(TRUE,FALSE)]
m_N2O_All_MeOH<-m_N2O_All[c(TRUE,FALSE)]
sd_N2O_All_MeOH<-sd_N2O_All[c(TRUE,FALSE)]
m_H2O_All_MeOH<-m_H2O_All[c(TRUE,FALSE)]
sd_H2O_All_MeOH<-sd_H2O_All[c(TRUE,FALSE)]
Tinterval_All_MeOH<-Tinterval_All_MeOH[c(TRUE,FALSE)]

#Select same time intervals in NH3 Picarro as in PTR-TOF
Ti_PTR<-round(as.numeric(Time_PTR_MeOH[c(730-160,730,1000-160,1000,1420-160,1420,1670-160,1670,1770,1930,2240-160,2240,2490-160,2490,2780-160,2780,2980-160,2980,3190-160,3190,3590-160,3590)]))
Ti_PTR<-(Ti_PTR-as.numeric(seconds(90)))-as.numeric(minutes(177))
numericTime_NH3<-round(as.numeric(Time_NH3_MeOH))
Pos<-list()
min_dif<-1
for (i in 1:(length(Ti_PTR))){
  min_dif[i]<-min(abs(numericTime_NH3-Ti_PTR[i]))
  Pos[[i]]<-which(numericTime_NH3-Ti_PTR[i]==min_dif[i])
  Pos[[i]]<-Pos[[i]][1]
}
t<-unlist(Pos)
### For Publication--> save means for borrowed G2103 ###
write.table(t, file = "MeOH_G2103_intervals.txt", sep = "\t", row.names = TRUE, col.names = NA)
########################################################

## Put the calculated positions in G2103
m_NH3r_NH3<-1
sd_NH3r_NH3<-1
length_NH3<-1
m_NH3d_NH3<-1
sd_NH3d_NH3<-1
m_H2O_NH3<-1
sd_H2O_NH3<-1
Tinterval_NH3_MeOH<-1

for (i in 1:(length(Ti_PTR)-1)){
  m_NH3r_NH3[i]<-mean(NH3r_NH3_MeOH[t[i]:t[i+1]])
  sd_NH3r_NH3[i]<-sd(NH3r_NH3_MeOH[t[i]:t[i+1]])
  length_NH3[i]<-length(NH3r_NH3_MeOH[t[i]:t[i+1]])
  m_NH3d_NH3[i]<-mean(NH3d_NH3_MeOH[t[i]:t[i+1]])
  sd_NH3d_NH3[i]<-sd(NH3d_NH3_MeOH[t[i]:t[i+1]])
  m_H2O_NH3[i]<-mean(H2O_NH3_MeOH[t[i]:t[i+1]])
  sd_H2O_NH3[i]<-sd(H2O_NH3_MeOH[t[i]:t[i+1]])
  Tinterval_NH3_MeOH[i]<-difftime(Time_NH3_MeOH[t[i]],Time_NH3_MeOH[t[i+1]])
}
#Only odd positions are correct (Even positions are from t(2)to t(3))
m_NH3r_NH3_MeOH<-m_NH3r_NH3[c(TRUE,FALSE)]
sd_NH3r_NH3_MeOH<-sd_NH3r_NH3[c(TRUE,FALSE)]
length_NH3_MeOH<-length_NH3[c(TRUE,FALSE)]
m_NH3d_NH3_MeOH<-m_NH3d_NH3[c(TRUE,FALSE)]
sd_NH3d_NH3_MeOH<-sd_NH3d_NH3[c(TRUE,FALSE)]
m_H2O_NH3_MeOH<-m_H2O_NH3[c(TRUE,FALSE)]
sd_H2O_NH3_MeOH<-sd_H2O_NH3[c(TRUE,FALSE)]
Tinterval_NH3_MeOH<-Tinterval_NH3_MeOH[c(TRUE,FALSE)]

#Select same time intervals in MeOHkPack as in PTR-TOF
Ti_PTR<-round(as.numeric(Time_PTR_MeOH[c(730-160,730,1000-160,1000,1420-160,1420,1670-160,1670,1770,1930,2240-160,2240,2490-160,2490,2780-160,2780,2980-160,2980,3190-160,3190,3590-160,3590)]))
Ti_PTR<-(Ti_PTR-as.numeric(seconds(90)))-as.numeric(minutes(119))
numericTime_BP<-round(as.numeric(Time_BP_MeOH))
Pos<-list()
min_dif<-1
for (i in 1:(length(Ti_PTR))){
  min_dif[i]<-min(abs(numericTime_BP-Ti_PTR[i]))
  Pos[[i]]<-which(numericTime_BP-Ti_PTR[i]==min_dif[i])
  Pos[[i]]<-Pos[[i]][1]
}
t<-unlist(Pos)
### For Publication--> save means for borrowed G4301 ###
write.table(t, file = "MeOH_G4301_intervals.txt", sep = "\t", row.names = TRUE, col.names = NA)
########################################################

## Put the calculated positions in G4301
m_CH4_BP<-1
sd_CH4_BP<-1
length_BP<-1
m_H2O_BP<-1
sd_H2O_BP<-1
m_CH4d_BP<-1
sd_CH4d_BP<-1
Tinterval_BP_MeOH<-1

for (i in 1:(length(Ti_PTR)-1)){
  m_CH4_BP[i]<-mean(CH4_BP_MeOH[t[i]:t[i+1]])
  sd_CH4_BP[i]<-sd(CH4_BP_MeOH[t[i]:t[i+1]])
  length_BP[i]<-length(CH4_BP_MeOH[t[i]:t[i+1]])
  m_CH4d_BP[i]<-mean(CH4d_BP_MeOH[t[i]:t[i+1]])
  sd_CH4d_BP[i]<-sd(CH4d_BP_MeOH[t[i]:t[i+1]])
  m_H2O_BP[i]<-mean(H2O_BP_MeOH[t[i]:t[i+1]])
  sd_H2O_BP[i]<-sd(H2O_BP_MeOH[t[i]:t[i+1]])
  Tinterval_BP_MeOH[i]<-difftime(Time_BP_MeOH[t[i]],Time_BP_MeOH[t[i+1]])
}
#Only odd positions are correct (Even positions are from t(2)to t(3))
m_CH4_BP_MeOH<-m_CH4_BP[c(TRUE,FALSE)]
sd_CH4_BP_MeOH<-sd_CH4_BP[c(TRUE,FALSE)]
length_BP_MeOH<-length_BP[c(TRUE,FALSE)]
m_CH4d_BP_MeOH<-m_CH4d_BP[c(TRUE,FALSE)]
sd_CH4d_BP_MeOH<-sd_CH4d_BP[c(TRUE,FALSE)]
m_H2O_BP_MeOH<-m_H2O_BP[c(TRUE,FALSE)]
sd_H2O_BP_MeOH<-sd_H2O_BP[c(TRUE,FALSE)]
Tinterval_BP_MeOH<-Tinterval_BP_MeOH[c(TRUE,FALSE)]

## Create and join all points into one data frame ##
#NH3
y_NH3<-c(m_NH3_All_MeOH,m_NH3r_NH3_MeOH,m_NH3d_NH3_MeOH)
x_NH3<-c(m_MeOH_PTR,m_MeOH_PTR,m_MeOH_PTR)
sd_NH3<-c(sd_NH3_All_MeOH,sd_NH3r_NH3_MeOH,sd_NH3d_NH3_MeOH)
Color_NH3<-c(rep("NH3_G2508",11),rep("NH3_raw",11),rep("NH3_dry",11))
df_NH3_MeOH<-cbind.data.frame(x_NH3,y_NH3,sd_NH3,Color_NH3)
#CH4
y_CH4<-c(m_CH4_All_MeOH,m_CH4_BP_MeOH,m_CH4d_BP_MeOH)
x_CH4<-c(m_MeOH_PTR,m_MeOH_PTR,m_MeOH_PTR)
sd_CH4<-c(sd_CH4_All_MeOH,sd_CH4_BP_MeOH,sd_CH4d_BP_MeOH)
Color_CH4<-c(rep("CH4_G2508",11),rep("CH4",11),rep("CH4_dry",11))
df_CH4_MeOH<-cbind.data.frame(x_CH4,y_CH4,sd_CH4,Color_CH4)
#N2O
y_N2O<-c(m_N2O_All_MeOH)
x_N2O<-c(m_MeOH_PTR)
sd_N2O<-c(sd_N2O_All_MeOH)
Color_N2O<-c(rep("N2O_G2508",11))
df_N2O_MeOH<-cbind.data.frame(x_N2O,y_N2O,sd_N2O,Color_N2O)

##2-Propanol
Ac0_PTR<-mean(P2_PTR[(900-103):900])
Ac1_PTR<-mean(P2_PTR[(1205-103):1205])
Ac2_PTR<-mean(P2_PTR[(1343-103):1343])
Ac3_PTR<-mean(P2_PTR[(1564-103):1564])
Ac4_PTR<-mean(P2_PTR[(1866-103):1866])
Ac5_PTR<-mean(P2_PTR[(2056-103):2056])
Ac6_PTR<-mean(P2_PTR[(2214-103):2214])
Ac7_PTR<-mean(P2_PTR[(2514-103):2514])
Ac8_PTR<-mean(P2_PTR[(2844-103):2844])

sd_Ac0_PTR<-sd(P2_PTR[(900-103):900])
sd_Ac1_PTR<-sd(P2_PTR[(1205-103):1205])
sd_Ac2_PTR<-sd(P2_PTR[(1343-103):1343])
sd_Ac3_PTR<-sd(P2_PTR[(1564-103):1564])
sd_Ac4_PTR<-sd(P2_PTR[(1866-103):1866])
sd_Ac5_PTR<-sd(P2_PTR[(2056-103):2056])
sd_Ac6_PTR<-sd(P2_PTR[(2214-103):2214])
sd_Ac7_PTR<-sd(P2_PTR[(2514-103):2514])
sd_Ac8_PTR<-sd(P2_PTR[(2844-103):2844])

length_Ac0_PTR<-length(P2_PTR[(900-103):900])
length_Ac1_PTR<-length(P2_PTR[(1205-103):1205])
length_Ac2_PTR<-length(P2_PTR[(1343-103):1343])
length_Ac3_PTR<-length(P2_PTR[(1564-103):1564])
length_Ac4_PTR<-length(P2_PTR[(1866-103):1866])
length_Ac5_PTR<-length(P2_PTR[(2056-103):2056])
length_Ac6_PTR<-length(P2_PTR[(2214-103):2214])
length_Ac7_PTR<-length(P2_PTR[(2514-103):2514])
length_Ac8_PTR<-length(P2_PTR[(2844-103):2844])

Tinterval_Ac0_PTR<-difftime(Time_PTR_P2[(900-103)],Time_PTR_P2[900])
Tinterval_Ac1_PTR<-difftime(Time_PTR_P2[(1205-103)],Time_PTR_P2[1205])
Tinterval_Ac2_PTR<-difftime(Time_PTR_P2[(1343-103)],Time_PTR_P2[1343])
Tinterval_Ac3_PTR<-difftime(Time_PTR_P2[(1564-103)],Time_PTR_P2[1564])
Tinterval_Ac4_PTR<-difftime(Time_PTR_P2[(1866-103)],Time_PTR_P2[1866])
Tinterval_Ac5_PTR<-difftime(Time_PTR_P2[(2056-103)],Time_PTR_P2[2056])
Tinterval_Ac6_PTR<-difftime(Time_PTR_P2[(2214-103)],Time_PTR_P2[2214])
Tinterval_Ac7_PTR<-difftime(Time_PTR_P2[(2514-103)],Time_PTR_P2[2514])
Tinterval_Ac8_PTR<-difftime(Time_PTR_P2[(2844-103)],Time_PTR_P2[2844])

m_P2_PTR<-c(Ac0_PTR,Ac1_PTR,Ac2_PTR,Ac3_PTR,Ac4_PTR,Ac5_PTR,Ac6_PTR,Ac7_PTR,Ac8_PTR)
sd_P2_PTR<-c(sd_Ac0_PTR,sd_Ac1_PTR,sd_Ac2_PTR,sd_Ac3_PTR,sd_Ac4_PTR,sd_Ac5_PTR,sd_Ac6_PTR,sd_Ac7_PTR,sd_Ac8_PTR)
length_P2_PTR<-c(length_Ac0_PTR,length_Ac1_PTR,length_Ac2_PTR,length_Ac3_PTR,length_Ac4_PTR,length_Ac5_PTR,length_Ac6_PTR,length_Ac7_PTR,length_Ac8_PTR)
Tinterval_P2_PTR<-c(Tinterval_Ac0_PTR,Tinterval_Ac1_PTR,Tinterval_Ac2_PTR,Tinterval_Ac3_PTR,Tinterval_Ac4_PTR,Tinterval_Ac5_PTR,Tinterval_Ac6_PTR,Tinterval_Ac7_PTR,Tinterval_Ac8_PTR)

#Select same time intervals in G2508 as in PTR-TOF
Ti_PTR<-round(as.numeric(Time_PTR_P2[c(900-103,900,1205-103,1205,1343-103,1343,1564-103,1564,1866-103,1866,2056-103,2056,2214-103,2214,2514-103,2514,2844-103,2844)]))
Ti_PTR<-Ti_PTR-as.numeric(seconds(120))
numericTime_All<-round(as.numeric(Time_All_P2))
Pos<-list()
min_dif<-1
for (i in 1:(length(Ti_PTR))){
  min_dif[i]<-min(abs(numericTime_All-Ti_PTR[i]))
  Pos[[i]]<-which(numericTime_All-Ti_PTR[i]==min_dif[i])
  Pos[[i]]<-Pos[[i]][1]
}
t<-unlist(Pos)
### For Publication--> save means for borrowed G2508 ###
write.table(t, file = "P2_G2509_intervals.txt", sep = "\t", row.names = TRUE, col.names = NA)
########################################################


# Put the calculated positions in G2508
m_NH3_All<-1
sd_NH3_All<-1
length_All<-1
m_H2O_All<-1
sd_H2O_All<-1
m_CH4_All<-1
sd_CH4_All<-1
m_N2O_All<-1
sd_N2O_All<-1
Tinterval_All_P2<-1

for (i in 1:(length(Ti_PTR)-1)){
  m_NH3_All[i]<-mean(NH3_All_P2[t[i]:t[i+1]])
  sd_NH3_All[i]<-sd(NH3_All_P2[t[i]:t[i+1]])
  length_All[i]<-length(NH3_All_P2[t[i]:t[i+1]])
  m_CH4_All[i]<-mean(CH4_All_P2[t[i]:t[i+1]])
  sd_CH4_All[i]<-sd(CH4_All_P2[t[i]:t[i+1]])
  m_N2O_All[i]<-mean(N2O_All_P2[t[i]:t[i+1]])
  sd_N2O_All[i]<-sd(N2O_All_P2[t[i]:t[i+1]])
  m_H2O_All[i]<-mean(H2O_All_P2[t[i]:t[i+1]])
  sd_H2O_All[i]<-sd(H2O_All_P2[t[i]:t[i+1]])
  Tinterval_All_P2[i]<-difftime(Time_All_P2[t[i]],Time_All_P2[t[i+1]])
}
#Only odd positions are correct (Even positions are from t(2)to t(3))
m_NH3_All_P2<-m_NH3_All[c(TRUE,FALSE)]
sd_NH3_All_P2<-sd_NH3_All[c(TRUE,FALSE)]
length_All_P2<-length_All[c(TRUE,FALSE)]
m_CH4_All_P2<-m_CH4_All[c(TRUE,FALSE)]
sd_CH4_All_P2<-sd_CH4_All[c(TRUE,FALSE)]
m_N2O_All_P2<-m_N2O_All[c(TRUE,FALSE)]
sd_N2O_All_P2<-sd_N2O_All[c(TRUE,FALSE)]
m_N2O_All_P2<-m_N2O_All[c(TRUE,FALSE)]
sd_N2O_All_P2<-sd_N2O_All[c(TRUE,FALSE)]
m_H2O_All_P2<-m_H2O_All[c(TRUE,FALSE)]
sd_H2O_All_P2<-sd_H2O_All[c(TRUE,FALSE)]
Tinterval_All_P2<-Tinterval_All_P2[c(TRUE,FALSE)]

#Select same time intervals in NH3 Picarro as in PTR-TOF
Ti_PTR<-round(as.numeric(Time_PTR_P2[c(900-103,900,1205-103,1205,1343-103,1343,1564-103,1564,1866-103,1866,2056-103,2056,2214-103,2214,2514-103,2514,2844-103,2844)]))
Ti_PTR<-(Ti_PTR-as.numeric(seconds(90)))-as.numeric(minutes(177))
numericTime_NH3<-round(as.numeric(Time_NH3_P2))
Pos<-list()
min_dif<-1
for (i in 1:(length(Ti_PTR))){
  min_dif[i]<-min(abs(numericTime_NH3-Ti_PTR[i]))
  Pos[[i]]<-which(numericTime_NH3-Ti_PTR[i]==min_dif[i])
  Pos[[i]]<-Pos[[i]][1]
}
t<-unlist(Pos)
### For Publication--> save means for G2103 ###
write.table(t, file = "P2_G2103_intervals.txt", sep = "\t", row.names = TRUE, col.names = NA)
########################################################


## Put the calculated positions in G2103
m_NH3r_NH3<-1
sd_NH3r_NH3<-1
length_NH3<-1
m_NH3d_NH3<-1
sd_NH3d_NH3<-1
m_H2O_NH3<-1
sd_H2O_NH3<-1
Tinterval_NH3_P2<-1

for (i in 1:(length(Ti_PTR)-1)){
  m_NH3r_NH3[i]<-mean(NH3r_NH3_P2[t[i]:t[i+1]])
  sd_NH3r_NH3[i]<-sd(NH3r_NH3_P2[t[i]:t[i+1]])
  length_NH3[i]<-length(NH3r_NH3_P2[t[i]:t[i+1]])
  m_NH3d_NH3[i]<-mean(NH3d_NH3_P2[t[i]:t[i+1]])
  sd_NH3d_NH3[i]<-sd(NH3d_NH3_P2[t[i]:t[i+1]])
  m_H2O_NH3[i]<-mean(H2O_NH3_P2[t[i]:t[i+1]])
  sd_H2O_NH3[i]<-sd(H2O_NH3_P2[t[i]:t[i+1]])
  Tinterval_NH3_P2[i]<-difftime(Time_NH3_P2[t[i]],Time_NH3_P2[t[i+1]])
}
#Only odd positions are correct (Even positions are from t(2)to t(3))
m_NH3r_NH3_P2<-m_NH3r_NH3[c(TRUE,FALSE)]
sd_NH3r_NH3_P2<-sd_NH3r_NH3[c(TRUE,FALSE)]
length_NH3_P2<-length_NH3[c(TRUE,FALSE)]
m_NH3d_NH3_P2<-m_NH3d_NH3[c(TRUE,FALSE)]
sd_NH3d_NH3_P2<-sd_NH3d_NH3[c(TRUE,FALSE)]
m_H2O_NH3_P2<-m_H2O_NH3[c(TRUE,FALSE)]
sd_H2O_NH3_P2<-sd_H2O_NH3[c(TRUE,FALSE)]
Tinterval_NH3_P2<-Tinterval_NH3_P2[c(TRUE,FALSE)]

#Select same time intervals in P2kPack as in PTR-TOF
Ti_PTR<-round(as.numeric(Time_PTR_P2[c(900-103,900,1205-103,1205,1343-103,1343,1564-103,1564,1866-103,1866,2056-103,2056,2214-103,2214,2514-103,2514,2844-103,2844)]))
Ti_PTR<-(Ti_PTR-as.numeric(seconds(90)))-as.numeric(minutes(119))
numericTime_BP<-round(as.numeric(Time_BP_P2))
Pos<-list()
min_dif<-1
for (i in 1:(length(Ti_PTR))){
  min_dif[i]<-min(abs(numericTime_BP-Ti_PTR[i]))
  Pos[[i]]<-which(numericTime_BP-Ti_PTR[i]==min_dif[i])
  Pos[[i]]<-Pos[[i]][1]
}
t<-unlist(Pos)
### For Publication--> save means for G4301 ###
write.table(t, file = "P2_G4301_intervals.txt", sep = "\t", row.names = TRUE, col.names = NA)
########################################################


## Put the calculated positions in G4301
m_CH4_BP<-1
sd_CH4_BP<-1
length_BP<-1
m_H2O_BP<-1
sd_H2O_BP<-1
m_CH4d_BP<-1
sd_CH4d_BP<-1
Tinterval_BP_P2<-1

for (i in 1:(length(Ti_PTR)-1)){
  m_CH4_BP[i]<-mean(CH4_BP_P2[t[i]:t[i+1]])
  sd_CH4_BP[i]<-sd(CH4_BP_P2[t[i]:t[i+1]])
  length_BP[i]<-length(CH4_BP_P2[t[i]:t[i+1]])
  m_CH4d_BP[i]<-mean(CH4d_BP_P2[t[i]:t[i+1]])
  sd_CH4d_BP[i]<-sd(CH4d_BP_P2[t[i]:t[i+1]])
  m_H2O_BP[i]<-mean(H2O_BP_P2[t[i]:t[i+1]])
  sd_H2O_BP[i]<-sd(H2O_BP_P2[t[i]:t[i+1]])
  Tinterval_BP_P2[i]<-difftime(Time_BP_P2[t[i]],Time_BP_P2[t[i+1]])
}
#Only odd positions are correct (Even positions are from t(2)to t(3))
m_CH4_BP_P2<-m_CH4_BP[c(TRUE,FALSE)]
sd_CH4_BP_P2<-sd_CH4_BP[c(TRUE,FALSE)]
length_BP_P2<-length_BP[c(TRUE,FALSE)]
m_CH4d_BP_P2<-m_CH4d_BP[c(TRUE,FALSE)]
sd_CH4d_BP_P2<-sd_CH4d_BP[c(TRUE,FALSE)]
m_H2O_BP_P2<-m_H2O_BP[c(TRUE,FALSE)]
sd_H2O_BP_P2<-sd_H2O_BP[c(TRUE,FALSE)]
Tinterval_BP_P2<-Tinterval_BP_P2[c(TRUE,FALSE)]

## Create and join all points into one data frame ##
#NH3
y_NH3<-c(m_NH3_All_P2,m_NH3r_NH3_P2,m_NH3d_NH3_P2)
x_NH3<-c(m_P2_PTR,m_P2_PTR,m_P2_PTR)
sd_NH3<-c(sd_NH3_All_P2,sd_NH3r_NH3_P2,sd_NH3d_NH3_P2)
Color_NH3<-c(rep("NH3_G2508",9),rep("NH3_raw",9),rep("NH3_dry",9))
df_NH3_P2<-cbind.data.frame(x_NH3,y_NH3,sd_NH3,Color_NH3)
#CH4
y_CH4<-c(m_CH4_All_P2,m_CH4_BP_P2,m_CH4d_BP_P2)
x_CH4<-c(m_P2_PTR,m_P2_PTR,m_P2_PTR)
sd_CH4<-c(sd_CH4_All_P2,sd_CH4_BP_P2,sd_CH4d_BP_P2)
Color_CH4<-c(rep("CH4_G2508",9),rep("CH4",9),rep("CH4_dry",9))
df_CH4_P2<-cbind.data.frame(x_CH4,y_CH4,sd_CH4,Color_CH4)
#N2O
y_N2O<-c(m_N2O_All_P2)
x_N2O<-c(m_P2_PTR)
sd_N2O<-c(sd_N2O_All_P2)
Color_N2O<-c(rep("N2O_G2508",9))
df_N2O_P2<-cbind.data.frame(x_N2O,y_N2O,sd_N2O,Color_N2O)






### For publication ###
## Remove NH3_raw & CH4
##Add standard deviation of VOC

## Acetone
#NH3
y_NH3<-c(m_NH3_All_Ac,m_NH3d_NH3_Ac)
x_NH3<-c(m_Ac_PTR,m_Ac_PTR)
sd_NH3<-c(sd_NH3_All_Ac,sd_NH3d_NH3_Ac)
sd_Ac<-c(sd_Ac_PTR,sd_Ac_PTR)
Color_NH3<-c(rep("G2509",11),rep("G2103",11))
df_NH3_Ac<-cbind.data.frame(x_NH3,y_NH3,sd_NH3,Color_NH3)
#CH4
y_CH4<-c(m_CH4_All_Ac,m_CH4d_BP_Ac)
x_CH4<-c(m_Ac_PTR,m_Ac_PTR)
sd_CH4<-c(sd_CH4_All_Ac,sd_CH4d_BP_Ac)
Color_CH4<-c(rep("G2509",11),rep("G4301",11))
df_CH4_Ac<-cbind.data.frame(x_CH4,y_CH4,sd_CH4,Color_CH4)
#N2O
y_N2O<-c(m_N2O_All_Ac)
x_N2O<-c(m_Ac_PTR)
sd_N2O<-c(sd_N2O_All_Ac)
Color_N2O<-c(rep("G2509",11))
df_N2O_Ac<-cbind.data.frame(x_N2O,y_N2O,sd_N2O,Color_N2O)

df_Control_Ac<-cbind.data.frame(length_Ac_PTR,length_BP_Ac,length_NH3_Ac,length_All_Ac,
                                Tinterval_Ac_PTR,Tinterval_BP_Ac,Tinterval_NH3_Ac,Tinterval_All_Ac,m_H2O_BP_Ac,m_H2O_NH3_Ac,m_H2O_All_Ac)

## Butyric/Butanoic acid
#NH3
y_NH3<-c(m_NH3_All_BAc,m_NH3d_NH3_BAc)
x_NH3<-c(m_BAc_PTR,m_BAc_PTR)
sd_NH3<-c(sd_NH3_All_BAc,sd_NH3d_NH3_BAc)
sd_BAc<-c(sd_BAc_PTR,sd_BAc_PTR)
Color_NH3<-c(rep("G2509",10),rep("G2103",10))
df_NH3_BAc<-cbind.data.frame(x_NH3,y_NH3,sd_NH3,Color_NH3)
#CH4
y_CH4<-c(m_CH4_All_BAc,m_CH4d_BP_BAc)
x_CH4<-c(m_BAc_PTR,m_BAc_PTR)
sd_CH4<-c(sd_CH4_All_BAc,sd_CH4d_BP_BAc)
Color_CH4<-c(rep("G2509",10),rep("G4301",10))
df_CH4_BAc<-cbind.data.frame(x_CH4,y_CH4,sd_CH4,Color_CH4)
#N2O
y_N2O<-c(m_N2O_All_BAc)
x_N2O<-c(m_BAc_PTR)
sd_N2O<-c(sd_N2O_All_BAc)
Color_N2O<-c(rep("G2509",10))
df_N2O_BAc<-cbind.data.frame(x_N2O,y_N2O,sd_N2O,Color_N2O)

df_Control_BAc<-cbind.data.frame(length_BAc_PTR,length_BP_BAc,length_NH3_BAc,length_All_BAc,
                                 Tinterval_BAc_PTR,Tinterval_BP_BAc,Tinterval_NH3_BAc,Tinterval_All_BAc,m_H2O_BP_BAc,m_H2O_NH3_BAc,m_H2O_All_BAc)

## Methanol
#NH3
y_NH3<-c(m_NH3_All_MeOH,m_NH3d_NH3_MeOH)
x_NH3<-c(m_MeOH_PTR,m_MeOH_PTR)
sd_NH3<-c(sd_NH3_All_MeOH,sd_NH3d_NH3_MeOH)
sd_MeOH<-c(sd_MeOH_PTR,sd_MeOH_PTR)
Color_NH3<-c(rep("G2509",11),rep("G2103",11))
df_NH3_MeOH<-cbind.data.frame(x_NH3,y_NH3,sd_NH3,Color_NH3)
#CH4
y_CH4<-c(m_CH4_All_MeOH,m_CH4d_BP_MeOH)
x_CH4<-c(m_MeOH_PTR,m_MeOH_PTR)
sd_CH4<-c(sd_CH4_All_MeOH,sd_CH4d_BP_MeOH)
Color_CH4<-c(rep("G2509",11),rep("G4301",11))
df_CH4_MeOH<-cbind.data.frame(x_CH4,y_CH4,sd_CH4,Color_CH4)
#N2O
y_N2O<-c(m_N2O_All_MeOH)
x_N2O<-c(m_MeOH_PTR)
sd_N2O<-c(sd_N2O_All_MeOH)
Color_N2O<-c(rep("G2509",11))
df_N2O_MeOH<-cbind.data.frame(x_N2O,y_N2O,sd_N2O,Color_N2O)

df_Control_MeOH<-cbind.data.frame(length_MeOH_PTR,length_BP_MeOH,length_NH3_MeOH,length_All_MeOH,
                                  Tinterval_MeOH_PTR,Tinterval_BP_MeOH,Tinterval_NH3_MeOH,Tinterval_All_MeOH,m_H2O_BP_MeOH,m_H2O_NH3_MeOH,m_H2O_All_MeOH)

## 2-Propanol
#NH3
y_NH3<-c(m_NH3_All_P2,m_NH3d_NH3_P2)
x_NH3<-c(m_P2_PTR,m_P2_PTR)
sd_NH3<-c(sd_NH3_All_P2,sd_NH3d_NH3_P2)
sd_P2<-c(sd_P2_PTR,sd_P2_PTR)
Color_NH3<-c(rep("G2509",9),rep("G2103",9))
df_NH3_P2<-cbind.data.frame(x_NH3,y_NH3,sd_NH3,Color_NH3)
#CH4
y_CH4<-c(m_CH4_All_P2,m_CH4d_BP_P2)
x_CH4<-c(m_P2_PTR,m_P2_PTR)
sd_CH4<-c(sd_CH4_All_P2,sd_CH4d_BP_P2)
Color_CH4<-c(rep("G2509",9),rep("G4301",9))
df_CH4_P2<-cbind.data.frame(x_CH4,y_CH4,sd_CH4,Color_CH4)
#N2O
y_N2O<-c(m_N2O_All_P2)
x_N2O<-c(m_P2_PTR)
sd_N2O<-c(sd_N2O_All_P2)
Color_N2O<-c(rep("G2509",9))
df_N2O_P2<-cbind.data.frame(x_N2O,y_N2O,sd_N2O,Color_N2O)

df_Control_P2<-cbind.data.frame(length_P2_PTR,length_BP_P2,length_NH3_P2,length_All_P2,
                                Tinterval_P2_PTR,Tinterval_BP_P2,Tinterval_NH3_P2,Tinterval_All_P2,m_H2O_BP_P2,m_H2O_NH3_P2,m_H2O_All_P2)


###Save the data frame for publication

##Butyric acid
setwd("C:/Users/au615105/OneDrive - Aarhus universitet/Pablo Garc?a/Papers/Interferences pictures/VOCs")
write.table(df_N2O_BAc, file = "BAc_N2O.txt", sep = "\t", row.names = TRUE, col.names = NA)
write.table(df_NH3_BAc, file = "BAc_NH3.txt", sep = "\t", row.names = TRUE, col.names = NA)
write.table(df_CH4_BAc, file = "BAc_CH4.txt", sep = "\t", row.names = TRUE, col.names = NA)
write.table(sd_BAc_PTR, file = "sd_BAc.txt", sep = "\t", row.names = TRUE, col.names = NA)
write.table(Dat_All, file = "BAc_G2509_CH4.txt", sep = "\t", row.names = TRUE, col.names = NA)
write.table(Dat_BP, file = "BAc_G4301_CH4.txt", sep = "\t", row.names = TRUE, col.names = NA)

##2-Propanol
setwd("C:/Users/au615105/OneDrive - Aarhus universitet/Pablo Garc?a/Papers/Interferences pictures/VOCs")
write.table(df_N2O_P2, file = "P2_N2O.txt", sep = "\t", row.names = TRUE, col.names = NA)
write.table(df_NH3_P2, file = "P2_NH3.txt", sep = "\t", row.names = TRUE, col.names = NA)
write.table(df_CH4_P2, file = "P2_CH4.txt", sep = "\t", row.names = TRUE, col.names = NA)
write.table(sd_P2_PTR, file = "sd_P2.txt", sep = "\t", row.names = TRUE, col.names = NA)
write.table(Dat_All, file = "P2_G2509_CH4.txt", sep = "\t", row.names = TRUE, col.names = NA)
write.table(Dat_BP, file = "P2_G4301_CH4.txt", sep = "\t", row.names = TRUE, col.names = NA)

##Acetone
setwd("C:/Users/au615105/OneDrive - Aarhus universitet/Pablo Garc?a/Papers/Interferences pictures/VOCs")
write.table(df_N2O_Ac, file = "Ac_N2O.txt", sep = "\t", row.names = TRUE, col.names = NA)
write.table(df_NH3_Ac, file = "Ac_NH3.txt", sep = "\t", row.names = TRUE, col.names = NA)
write.table(df_CH4_Ac, file = "Ac_CH4.txt", sep = "\t", row.names = TRUE, col.names = NA)
write.table(sd_Ac_PTR, file = "sd_Ac.txt", sep = "\t", row.names = TRUE, col.names = NA)
write.table(Dat_All, file = "Ac_G2509_CH4.txt", sep = "\t", row.names = TRUE, col.names = NA)
write.table(Dat_BP, file = "Ac_G4301_CH4.txt", sep = "\t", row.names = TRUE, col.names = NA)

##Methanol
setwd("C:/Users/au615105/OneDrive - Aarhus universitet/Pablo Garc?a/Papers/Interferences pictures/VOCs")
write.table(df_N2O_MeOH, file = "MeOH_N2O.txt", sep = "\t", row.names = TRUE, col.names = NA)
write.table(df_NH3_MeOH, file = "MeOH_NH3.txt", sep = "\t", row.names = TRUE, col.names = NA)
write.table(df_CH4_MeOH, file = "MeOH_CH4.txt", sep = "\t", row.names = TRUE, col.names = NA)
write.table(sd_MeOH_PTR, file = "sd_MeOH.txt", sep = "\t", row.names = TRUE, col.names = NA)
write.table(Dat_All, file = "MeOH_G2509_CH4.txt", sep = "\t", row.names = TRUE, col.names = NA)
write.table(Dat_BP, file = "MeOH_G4301_CH4.txt", sep = "\t", row.names = TRUE, col.names = NA)