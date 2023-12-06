library(dplyr)
library(ggplot2)
library(anytime)
library(lubridate)
library(writexl)
library("readxl")



### Interferences 2021-07-07###   
##Ethanol
##Acetic acid

##Extract data from borrowed picarro##  
setwd(".../VOC Interferences/G2508 borrowed")
list_of_files <- list.files(path="07",pattern = "*.dat",recursive=TRUE) ##Read data from Picarro
L<-length(list_of_files)

setwd(".../VOC Interferences/G2508 borrowed/07")

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
setwd(".../VOC Interferences/NH3 Picarro")
list_of_files <- list.files(path="NH3 Picarro",pattern = "*.dat",recursive=TRUE) ##Read data from Picarro
L<-length(list_of_files)

setwd(".../VOC Interferences/NH3 Picarro/07")

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
setwd(".../VOC Interferences/BackPack")
list_of_files <- list.files(path="07",pattern = "*.dat",recursive=TRUE) ##Read data from Picarro
L<-length(list_of_files)

setwd(".../VOC Interferences/BackPack/07")

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

##Extract data from own picarro##  
setwd(".../VOC Interferences/G2508 own")
list_of_files <- list.files(path="07",pattern = "*.dat",recursive=TRUE) ##Read data from Picarro
L<-length(list_of_files)

setwd(".../VOC Interferences/G2508 own/07")

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
Dat_own<-getstats(Picarro = Pic_id) #Able to call each Picarro file
Dat_own<-bind_rows(Dat_own, .id = "column_label") #Join all elements of the list in one dataframe

#Change format of date and time to be plotted
Dat_own$date.time<-paste(Dat_own$DATE,Dat_own$TIME)
Dat_own$date.time<-as.POSIXct(Dat_own$date.time,format="%Y-%m-%d %H:%M:%S")


##Extract data from PTR-TOF##  
setwd(".../VOC Interferences/PTR-TOF")
AAc<-read_excel("acetic acid.xlsx")
AAc<-as.data.frame(AAc)
Eth<-read_excel("Ethanol.xlsx")
Eth<-as.data.frame(Eth)

###Data to plot all the calibration###
##Acetic acid##
Time_PTR_AAc<-c(AAc$AbsTime)
AAc_PTR<-c(AAc$`m43.0178 (C2H2OH+)`+AAc$`m61.0284 (C2H4O2H+)`)

#Sincronize all Picarros and PTR
TAc_PTR<-round(as.numeric(Time_PTR_AAc[c(1,length(Time_PTR_AAc))]))
TAc_PTR<-TAc_PTR-as.numeric(seconds(90))
TAc_PTRn<-TAc_PTR-as.numeric(minutes(177))
TAc_PTRb<-TAc_PTR-as.numeric(minutes(119))
TAc_PTRo<-TAc_PTR-as.numeric(minutes(57))
TAc_All<-round(as.numeric(Dat_All$date.time))
TAc_NH3<-round(as.numeric(Dat_NH3$date.time))
TAc_BP<-round(as.numeric(Dat_BP$date.time))
TAc_Own<-round(as.numeric(Dat_own$date.time))
PAc<-list()
PAc_N<-list()
PAc_C<-list()
PAc_O<-list()
min_dif_Ac<-1
min_dif_AcN<-1
min_dif_AcC<-1
min_dif_AcO<-1

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
  min_dif_AcO[i]<-min(abs(TAc_BP-TAc_PTRo[i]))
  PAc_O[[i]]<-which(TAc_Own-TAc_PTRo[i]==min_dif_AcO[i])
  PAc_O[[i]]<-PAc_O[[i]][1]
}
tAc<-unlist(PAc)
tAcn<-unlist(PAc_N)
tAcc<-unlist(PAc_C)
tAco<-unlist(PAc_O)

Time_All_AAc<-c(Dat_All$date.time[tAc[1]:tAc[2]])
Time_BP_AAc<-c(Dat_BP$date.time[tAcc[1]:tAcc[2]])
Time_NH3_AAc<-c(Dat_NH3$date.time[tAcn[1]:tAcn[2]])
Time_own_AAc<-c(Dat_own$date.time[tAco[1]:tAco[2]])
H2O_All_AAc<-c(Dat_All$H2O[tAc[1]:tAc[2]])
H2O_BP_AAc<-c(Dat_BP$H2O[tAcc[1]:tAcc[2]])
H2O_NH3_AAc<-c(Dat_NH3$H2O[tAcn[1]:tAcn[2]])
H2O_own_AAc<-c(Dat_own$H2O[tAco[1]:tAco[2]])
NH3_All_AAc<-c(Dat_All$NH3[tAc[1]:tAc[2]])
NH3r_NH3_AAc<-c(Dat_NH3$NH3_Raw[tAcn[1]:tAcn[2]])
NH3d_NH3_AAc<-c(Dat_NH3$NH3_dry[tAcn[1]:tAcn[2]])
NH3_own_AAc<-c(Dat_own$NH3[tAco[1]:tAco[2]])
CH4_All_AAc<-c(Dat_All$CH4_dry[tAc[1]:tAc[2]])
CH4_BP_AAc<-c(Dat_BP$CH4[tAcc[1]:tAcc[2]])
CH4d_BP_AAc<-c(Dat_BP$CH4_dry[tAcc[1]:tAcc[2]])
CH4_own_AAc<-c(Dat_own$CH4_dry[tAco[1]:tAco[2]])
N2O_All_AAc<-c(Dat_All$N2O_dry[tAc[1]:tAc[2]])
N2O_own_AAc<-c(Dat_own$N2O_dry[tAco[1]:tAco[2]])

##Ethanol##
Time_PTR_Eth<-c(Eth$AbsTime)
Eth_PTR<-c(Eth$`m45.0335 (C2H4OH+)`+Eth$`m47.0491 (C2H6OH+)`+Eth$`m65.0597 (C2H8O2H+)`)

#Sincronize all Picarros and PTR
TAc_PTR<-round(as.numeric(Time_PTR_Eth[c(1,length(Time_PTR_Eth))]))
TAc_PTR<-TAc_PTR-as.numeric(seconds(120))
TAc_PTRn<-TAc_PTR-as.numeric(minutes(177))
TAc_PTRb<-TAc_PTR-as.numeric(minutes(119))
TAc_PTRo<-TAc_PTR-as.numeric(minutes(57))
TAc_All<-round(as.numeric(Dat_All$date.time))
TAc_NH3<-round(as.numeric(Dat_NH3$date.time))
TAc_BP<-round(as.numeric(Dat_BP$date.time))
TAc_Own<-round(as.numeric(Dat_own$date.time))
PAc<-list()
PAc_N<-list()
PAc_C<-list()
PAc_O<-list()
min_dif_Ac<-1
min_dif_AcN<-1
min_dif_AcC<-1
min_dif_AcO<-1

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
  min_dif_AcO[i]<-min(abs(TAc_BP-TAc_PTRo[i]))
  PAc_O[[i]]<-which(TAc_Own-TAc_PTRo[i]==min_dif_AcO[i])
  PAc_O[[i]]<-PAc_O[[i]][1]
}
tAc<-unlist(PAc)
tAcn<-unlist(PAc_N)
tAcc<-unlist(PAc_C)
tAco<-unlist(PAc_O)

#### Save the intervals for publication ####
Interval_EtOH<-cbind.data.frame(tAc,tAcn,tAcc,tAco)
write.table(Interval_EtOH, file = "Eth_Interval.txt", sep = "\t", row.names = TRUE, col.names = NA)


Time_All_Eth<-c(Dat_All$date.time[tAc[1]:tAc[2]])
Time_BP_Eth<-c(Dat_BP$date.time[tAcc[1]:tAcc[2]])
Time_NH3_Eth<-c(Dat_NH3$date.time[tAcn[1]:tAcn[2]])
Time_own_Eth<-c(Dat_own$date.time[tAco[1]:tAco[2]])
H2O_All_Eth<-c(Dat_All$H2O[tAc[1]:tAc[2]])
H2O_BP_Eth<-c(Dat_BP$H2O[tAcc[1]:tAcc[2]])
H2O_NH3_Eth<-c(Dat_NH3$H2O[tAcn[1]:tAcn[2]])
H2O_own_Eth<-c(Dat_own$H2O[tAco[1]:tAco[2]])
NH3_All_Eth<-c(Dat_All$NH3[tAc[1]:tAc[2]])
NH3r_NH3_Eth<-c(Dat_NH3$NH3_Raw[tAcn[1]:tAcn[2]])
NH3d_NH3_Eth<-c(Dat_NH3$NH3_dry[tAcn[1]:tAcn[2]])
NH3_own_Eth<-c(Dat_own$NH3[tAco[1]:tAco[2]])
CH4_All_Eth<-c(Dat_All$CH4_dry[tAc[1]:tAc[2]])
CH4_BP_Eth<-c(Dat_BP$CH4[tAcc[1]:tAcc[2]])
CH4d_BP_Eth<-c(Dat_BP$CH4_dry[tAcc[1]:tAcc[2]])
CH4_own_Eth<-c(Dat_own$CH4_dry[tAco[1]:tAco[2]])
N2O_All_Eth<-c(Dat_All$N2O_dry[tAc[1]:tAc[2]])
N2O_own_Eth<-c(Dat_own$N2O_dry[tAco[1]:tAco[2]])

###Data to plot the interferences at each concentration###
##Acetic acid
Ac0_PTR<-mean(AAc_PTR[(150-100):150])
Ac1_PTR<-mean(AAc_PTR[(410-100):410])
Ac2_PTR<-mean(AAc_PTR[560:660])
Ac3_PTR<-mean(AAc_PTR[(840-100):840])
Ac4_PTR<-mean(AAc_PTR[(1040-100):1040])
Ac5_PTR<-mean(AAc_PTR[(1240-100):1240])
Ac6_PTR<-mean(AAc_PTR[(1480-100):1480])
Ac7_PTR<-mean(AAc_PTR[(1790-100):1790])
Ac8_PTR<-mean(AAc_PTR[(1980-100):1980])
Ac9_PTR<-mean(AAc_PTR[(3032-100):3032])

sd_Ac0_PTR<-sd(AAc_PTR[(150-100):150])
sd_Ac1_PTR<-sd(AAc_PTR[(410-100):410])
sd_Ac2_PTR<-sd(AAc_PTR[560:660])
sd_Ac3_PTR<-sd(AAc_PTR[(840-100):840])
sd_Ac4_PTR<-sd(AAc_PTR[(1040-100):1040])
sd_Ac5_PTR<-sd(AAc_PTR[(1240-100):1240])
sd_Ac6_PTR<-sd(AAc_PTR[(1480-100):1480])
sd_Ac7_PTR<-sd(AAc_PTR[(1790-100):1790])
sd_Ac8_PTR<-sd(AAc_PTR[(1980-100):1980])
sd_Ac9_PTR<-sd(AAc_PTR[(3032-100):3032])

length_Ac0_PTR<-length(AAc_PTR[(150-100):150])
length_Ac1_PTR<-length(AAc_PTR[(410-100):410])
length_Ac2_PTR<-length(AAc_PTR[560:660])
length_Ac3_PTR<-length(AAc_PTR[(840-100):840])
length_Ac4_PTR<-length(AAc_PTR[(1040-100):1040])
length_Ac5_PTR<-length(AAc_PTR[(1240-100):1240])
length_Ac6_PTR<-length(AAc_PTR[(1480-100):1480])
length_Ac7_PTR<-length(AAc_PTR[(1790-100):1790])
length_Ac8_PTR<-length(AAc_PTR[(1980-100):1980])
length_Ac9_PTR<-length(AAc_PTR[(3032-100):3032])

Tinterval_Ac0_PTR<-difftime(Time_PTR_AAc[(150-100)],Time_PTR_AAc[150])
Tinterval_Ac1_PTR<-difftime(Time_PTR_AAc[(410-100)],Time_PTR_AAc[410])
Tinterval_Ac2_PTR<-difftime(Time_PTR_AAc[560],Time_PTR_AAc[660])
Tinterval_Ac3_PTR<-difftime(Time_PTR_AAc[(840-100)],Time_PTR_AAc[840])
Tinterval_Ac4_PTR<-difftime(Time_PTR_AAc[(1040-100)],Time_PTR_AAc[1040])
Tinterval_Ac5_PTR<-difftime(Time_PTR_AAc[(1240-100)],Time_PTR_AAc[1240])
Tinterval_Ac6_PTR<-difftime(Time_PTR_AAc[(1480-100)],Time_PTR_AAc[1480])
Tinterval_Ac7_PTR<-difftime(Time_PTR_AAc[(1790-100)],Time_PTR_AAc[1790])
Tinterval_Ac8_PTR<-difftime(Time_PTR_AAc[(1980-100)],Time_PTR_AAc[1980])
Tinterval_Ac9_PTR<-difftime(Time_PTR_AAc[(3032-100)],Time_PTR_AAc[3032])

m_AAc_PTR<-c(Ac0_PTR,Ac1_PTR,Ac2_PTR,Ac3_PTR,Ac4_PTR,Ac5_PTR,Ac6_PTR,Ac7_PTR,Ac8_PTR,Ac9_PTR)
sd_AAc_PTR<-c(sd_Ac0_PTR,sd_Ac1_PTR,sd_Ac2_PTR,sd_Ac3_PTR,sd_Ac4_PTR,sd_Ac5_PTR,sd_Ac6_PTR,sd_Ac7_PTR,sd_Ac8_PTR,sd_Ac9_PTR)
length_AAc_PTR<-c(length_Ac0_PTR,length_Ac1_PTR,length_Ac2_PTR,length_Ac3_PTR,length_Ac4_PTR,length_Ac5_PTR,length_Ac6_PTR,length_Ac7_PTR,length_Ac8_PTR,length_Ac9_PTR)
Tinterval_AAc_PTR<-c(Tinterval_Ac0_PTR,Tinterval_Ac1_PTR,Tinterval_Ac2_PTR,Tinterval_Ac3_PTR,Tinterval_Ac4_PTR,Tinterval_Ac5_PTR,Tinterval_Ac6_PTR,Tinterval_Ac7_PTR,Tinterval_Ac8_PTR,Tinterval_Ac9_PTR)

#Select same time intervals in G2508 as in PTR-TOF
Ti_PTR<-round(as.numeric(Time_PTR_AAc[c(150-100,150,410-100,410,560,660,840-100,840,1040-100,1040,1240-100,1240,1480-100,1480,1790-100,1790,1980-100,1980,3032-100,3032)]))
Ti_PTR<-Ti_PTR-as.numeric(seconds(90))
numericTime_All<-round(as.numeric(Time_All_AAc))
Pos<-list()
min_dif<-1
for (i in 1:(length(Ti_PTR))){
  min_dif[i]<-min(abs(numericTime_All-Ti_PTR[i]))
  Pos[[i]]<-which(numericTime_All-Ti_PTR[i]==min_dif[i])
  Pos[[i]]<-Pos[[i]][1]
}
t<-unlist(Pos)


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
Tinterval_All_AAc<-1

for (i in 1:(length(Ti_PTR)-1)){
  m_NH3_All[i]<-mean(NH3_All_AAc[t[i]:t[i+1]])
  sd_NH3_All[i]<-sd(NH3_All_AAc[t[i]:t[i+1]])
  length_All[i]<-length(NH3_All_AAc[t[i]:t[i+1]])
  m_CH4_All[i]<-mean(CH4_All_AAc[t[i]:t[i+1]])
  sd_CH4_All[i]<-sd(CH4_All_AAc[t[i]:t[i+1]])
  m_N2O_All[i]<-mean(N2O_All_AAc[t[i]:t[i+1]])
  sd_N2O_All[i]<-sd(N2O_All_AAc[t[i]:t[i+1]])
  m_H2O_All[i]<-mean(H2O_All_AAc[t[i]:t[i+1]])
  sd_H2O_All[i]<-sd(H2O_All_AAc[t[i]:t[i+1]])
  Tinterval_All_AAc[i]<-difftime(Time_All_AAc[t[i]],Time_All_AAc[t[i+1]])
}
#Only odd positions are correct (Even positions are from t(2)to t(3))
m_NH3_All_AAc<-m_NH3_All[c(TRUE,FALSE)]
sd_NH3_All_AAc<-sd_NH3_All[c(TRUE,FALSE)]
length_All_AAc<-length_All[c(TRUE,FALSE)]
m_CH4_All_AAc<-m_CH4_All[c(TRUE,FALSE)]
sd_CH4_All_AAc<-sd_CH4_All[c(TRUE,FALSE)]
m_N2O_All_AAc<-m_N2O_All[c(TRUE,FALSE)]
sd_N2O_All_AAc<-sd_N2O_All[c(TRUE,FALSE)]
m_N2O_All_AAc<-m_N2O_All[c(TRUE,FALSE)]
sd_N2O_All_AAc<-sd_N2O_All[c(TRUE,FALSE)]
m_H2O_All_AAc<-m_H2O_All[c(TRUE,FALSE)]
sd_H2O_All_AAc<-sd_H2O_All[c(TRUE,FALSE)]
Tinterval_All_AAc<-Tinterval_All_AAc[c(TRUE,FALSE)]

#Select same time intervals in NH3 Picarro as in PTR-TOF
Ti_PTR<-round(as.numeric(Time_PTR_AAc[c(150-100,150,410-100,410,560,660,840-100,840,1040-100,1040,1240-100,1240,1480-100,1480,1790-100,1790,1980-100,1980,3032-100,3032)]))
Ti_PTR<-(Ti_PTR-as.numeric(seconds(90)))-as.numeric(minutes(177))
numericTime_NH3<-round(as.numeric(Time_NH3_AAc))
Pos<-list()
min_dif<-1
for (i in 1:(length(Ti_PTR))){
  min_dif[i]<-min(abs(numericTime_NH3-Ti_PTR[i]))
  Pos[[i]]<-which(numericTime_NH3-Ti_PTR[i]==min_dif[i])
  Pos[[i]]<-Pos[[i]][1]
}
t<-unlist(Pos)


## Put the calculated positions in G2103
m_NH3r_NH3<-1
sd_NH3r_NH3<-1
length_NH3<-1
m_NH3d_NH3<-1
sd_NH3d_NH3<-1
m_H2O_NH3<-1
sd_H2O_NH3<-1
Tinterval_NH3_AAc<-1

for (i in 1:(length(Ti_PTR)-1)){
  m_NH3r_NH3[i]<-mean(NH3r_NH3_AAc[t[i]:t[i+1]])
  sd_NH3r_NH3[i]<-sd(NH3r_NH3_AAc[t[i]:t[i+1]])
  length_NH3[i]<-length(NH3r_NH3_AAc[t[i]:t[i+1]])
  m_NH3d_NH3[i]<-mean(NH3d_NH3_AAc[t[i]:t[i+1]])
  sd_NH3d_NH3[i]<-sd(NH3d_NH3_AAc[t[i]:t[i+1]])
  m_H2O_NH3[i]<-mean(H2O_NH3_AAc[t[i]:t[i+1]])
  sd_H2O_NH3[i]<-sd(H2O_NH3_AAc[t[i]:t[i+1]])
  Tinterval_NH3_AAc[i]<-difftime(Time_NH3_AAc[t[i]],Time_NH3_AAc[t[i+1]])
}
#Only odd positions are correct (Even positions are from t(2)to t(3))
m_NH3r_NH3_AAc<-m_NH3r_NH3[c(TRUE,FALSE)]
sd_NH3r_NH3_AAc<-sd_NH3r_NH3[c(TRUE,FALSE)]
length_NH3_AAc<-length_NH3[c(TRUE,FALSE)]
m_NH3d_NH3_AAc<-m_NH3d_NH3[c(TRUE,FALSE)]
sd_NH3d_NH3_AAc<-sd_NH3d_NH3[c(TRUE,FALSE)]
m_H2O_NH3_AAc<-m_H2O_NH3[c(TRUE,FALSE)]
sd_H2O_NH3_AAc<-sd_H2O_NH3[c(TRUE,FALSE)]
Tinterval_NH3_AAc<-Tinterval_NH3_AAc[c(TRUE,FALSE)]

#Select same time intervals in AAckPack as in PTR-TOF
Ti_PTR<-round(as.numeric(Time_PTR_AAc[c(150-100,150,410-100,410,560,660,840-100,840,1040-100,1040,1240-100,1240,1480-100,1480,1790-100,1790,1980-100,1980,3032-100,3032)]))
Ti_PTR<-(Ti_PTR-as.numeric(seconds(90)))-as.numeric(minutes(119))
numericTime_BP<-round(as.numeric(Time_BP_AAc))
Pos<-list()
min_dif<-1
for (i in 1:(length(Ti_PTR))){
  min_dif[i]<-min(abs(numericTime_BP-Ti_PTR[i]))
  Pos[[i]]<-which(numericTime_BP-Ti_PTR[i]==min_dif[i])
  Pos[[i]]<-Pos[[i]][1]
}
t<-unlist(Pos)


## Put the calculated positions in G4301
m_CH4_BP<-1
sd_CH4_BP<-1
length_BP<-1
m_H2O_BP<-1
sd_H2O_BP<-1
m_CH4d_BP<-1
sd_CH4d_BP<-1
Tinterval_BP_AAc<-1

for (i in 1:(length(Ti_PTR)-1)){
  m_CH4_BP[i]<-mean(CH4_BP_AAc[t[i]:t[i+1]])
  sd_CH4_BP[i]<-sd(CH4_BP_AAc[t[i]:t[i+1]])
  length_BP[i]<-length(CH4_BP_AAc[t[i]:t[i+1]])
  m_CH4d_BP[i]<-mean(CH4d_BP_AAc[t[i]:t[i+1]])
  sd_CH4d_BP[i]<-sd(CH4d_BP_AAc[t[i]:t[i+1]])
  m_H2O_BP[i]<-mean(H2O_BP_AAc[t[i]:t[i+1]])
  sd_H2O_BP[i]<-sd(H2O_BP_AAc[t[i]:t[i+1]])
  Tinterval_BP_AAc[i]<-difftime(Time_BP_AAc[t[i]],Time_BP_AAc[t[i+1]])
}
#Only odd positions are correct (Even positions are from t(2)to t(3))
m_CH4_BP_AAc<-m_CH4_BP[c(TRUE,FALSE)]
sd_CH4_BP_AAc<-sd_CH4_BP[c(TRUE,FALSE)]
length_BP_AAc<-length_BP[c(TRUE,FALSE)]
m_CH4d_BP_AAc<-m_CH4d_BP[c(TRUE,FALSE)]
sd_CH4d_BP_AAc<-sd_CH4d_BP[c(TRUE,FALSE)]
m_H2O_BP_AAc<-m_H2O_BP[c(TRUE,FALSE)]
sd_H2O_BP_AAc<-sd_H2O_BP[c(TRUE,FALSE)]
Tinterval_BP_AAc<-Tinterval_BP_AAc[c(TRUE,FALSE)]

#Select same time intervals in own all Picarro as in PTR-TOF

Ti_PTR<-round(as.numeric(Time_PTR_AAc[c(150-100,150,410-100,410,560,660,840-100,840,1040-100,1040,1240-100,1240,1480-100,1480,1790-100,1790,1980-100,1980,3032-100,3032)]))
Ti_PTR<-(Ti_PTR-as.numeric(seconds(90)))-as.numeric(minutes(57))
numericTime_own<-round(as.numeric(Time_own_AAc))
Pos<-list()
min_dif<-1
for (i in 1:(length(Ti_PTR))){
  min_dif[i]<-min(abs(numericTime_own-Ti_PTR[i]))
  Pos[[i]]<-which(abs(numericTime_own-Ti_PTR[i])==min_dif[i])
  Pos[[i]]<-Pos[[i]][1]
}
t<-unlist(Pos)


# Put the calculated positions in G2508
m_NH3_own<-1
sd_NH3_own<-1
length_own<-1
m_H2O_own<-1
sd_H2O_own<-1
m_CH4_own<-1
sd_CH4_own<-1
m_N2O_own<-1
sd_N2O_own<-1
Tinterval_own_AAc<-1

for (i in 1:(length(Ti_PTR)-1)){
  m_NH3_own[i]<-mean(NH3_own_AAc[t[i]:t[i+1]])
  sd_NH3_own[i]<-sd(NH3_own_AAc[t[i]:t[i+1]])
  length_own[i]<-length(NH3_own_AAc[t[i]:t[i+1]])
  m_CH4_own[i]<-mean(CH4_own_AAc[t[i]:t[i+1]])
  sd_CH4_own[i]<-sd(CH4_own_AAc[t[i]:t[i+1]])
  m_N2O_own[i]<-mean(N2O_own_AAc[t[i]:t[i+1]])
  sd_N2O_own[i]<-sd(N2O_own_AAc[t[i]:t[i+1]])
  m_H2O_own[i]<-mean(H2O_own_AAc[t[i]:t[i+1]])
  sd_H2O_own[i]<-sd(H2O_own_AAc[t[i]:t[i+1]])
  Tinterval_own_AAc[i]<-difftime(Time_own_AAc[t[i]],Time_own_AAc[t[i+1]])
}
#Only odd positions are correct (Even positions are from t(2)to t(3))
m_NH3_own_AAc<-m_NH3_own[c(TRUE,FALSE)]
sd_NH3_own_AAc<-sd_NH3_own[c(TRUE,FALSE)]
length_own_AAc<-length_own[c(TRUE,FALSE)]
m_CH4_own_AAc<-m_CH4_own[c(TRUE,FALSE)]
sd_CH4_own_AAc<-sd_CH4_own[c(TRUE,FALSE)]
m_N2O_own_AAc<-m_N2O_own[c(TRUE,FALSE)]
sd_N2O_own_AAc<-sd_N2O_own[c(TRUE,FALSE)]
m_N2O_own_AAc<-m_N2O_own[c(TRUE,FALSE)]
sd_N2O_own_AAc<-sd_N2O_own[c(TRUE,FALSE)]
m_H2O_own_AAc<-m_H2O_own[c(TRUE,FALSE)]
sd_H2O_own_AAc<-sd_H2O_own[c(TRUE,FALSE)]
Tinterval_own_AAc<-Tinterval_own_AAc[c(TRUE,FALSE)]


## Create and join all points into one data frame ##
#NH3
y_NH3<-c(m_NH3_All_AAc,m_NH3r_NH3_AAc,m_NH3d_NH3_AAc,m_NH3_own_AAc)
x_NH3<-c(m_AAc_PTR,m_AAc_PTR,m_AAc_PTR,m_AAc_PTR)
sd_NH3<-c(sd_NH3_All_AAc,sd_NH3r_NH3_AAc,sd_NH3d_NH3_AAc,sd_NH3_own_AAc)
Color_NH3<-c(rep("NH3_G2508_borrowed",10),rep("NH3_raw",10),rep("NH3_dry",10),rep("NH3_G2508_own",10))
df_NH3_AAc<-cbind.data.frame(x_NH3,y_NH3,sd_NH3,Color_NH3)
#CH4
y_CH4<-c(m_CH4_All_AAc,m_CH4_BP_AAc,m_CH4d_BP_AAc,m_CH4_own_AAc)
x_CH4<-c(m_AAc_PTR,m_AAc_PTR,m_AAc_PTR,m_AAc_PTR)
sd_CH4<-c(sd_CH4_All_AAc,sd_CH4_BP_AAc,sd_CH4d_BP_AAc,sd_CH4_own_AAc)
Color_CH4<-c(rep("CH4_G2508_borrowed",10),rep("CH4",10),rep("CH4_dry",10),rep("CH4_G2508_own",10))
df_CH4_AAc<-cbind.data.frame(x_CH4,y_CH4,sd_CH4,Color_CH4)
#N2O
y_N2O<-c(m_N2O_All_AAc,m_N2O_own_AAc)
x_N2O<-c(m_AAc_PTR,m_AAc_PTR)
sd_N2O<-c(sd_N2O_All_AAc,sd_N2O_own_AAc)
Color_N2O<-c(rep("N2O_G2508_borrowed",10),rep("N2O_G2508_own",10))
df_N2O_AAc<-cbind.data.frame(x_N2O,y_N2O,sd_N2O,Color_N2O)


###Data to plot the interferences at each concentration###
##Ethanol
Ac0_PTR<-mean(Eth_PTR[(1500-110):1500])
Ac1_PTR<-mean(Eth_PTR[(1965-110):1965])
Ac2_PTR<-mean(Eth_PTR[(2390-110):2390])
Ac3_PTR<-mean(Eth_PTR[(2880-110):2880])
Ac4_PTR<-mean(Eth_PTR[(3460-110):3460])
Ac5_PTR<-mean(Eth_PTR[(3850-110):3850])
Ac6_PTR<-mean(Eth_PTR[(4220):4330])
Ac7_PTR<-mean(Eth_PTR[(4640-110):4640])
Ac8_PTR<-mean(Eth_PTR[(5150-110):5150])
Ac9_PTR<-mean(Eth_PTR[(5684-110):5684])

sd_Ac0_PTR<-sd(Eth_PTR[(1500-110):1500])
sd_Ac1_PTR<-sd(Eth_PTR[(1965-110):1965])
sd_Ac2_PTR<-sd(Eth_PTR[(2390-110):2390])
sd_Ac3_PTR<-sd(Eth_PTR[(2880-110):2880])
sd_Ac4_PTR<-sd(Eth_PTR[(3460-110):3460])
sd_Ac5_PTR<-sd(Eth_PTR[(3850-110):3850])
sd_Ac6_PTR<-sd(Eth_PTR[(4220):4330])
sd_Ac7_PTR<-sd(Eth_PTR[(4640-110):4640])
sd_Ac8_PTR<-sd(Eth_PTR[(5150-110):5150])
sd_Ac9_PTR<-sd(Eth_PTR[(5684-110):5684])

length_Ac0_PTR<-length(Eth_PTR[(1500-110):1500])
length_Ac1_PTR<-length(Eth_PTR[(1965-110):1965])
length_Ac2_PTR<-length(Eth_PTR[(2390-110):2390])
length_Ac3_PTR<-length(Eth_PTR[(2880-110):2880])
length_Ac4_PTR<-length(Eth_PTR[(3460-110):3460])
length_Ac5_PTR<-length(Eth_PTR[(3850-110):3850])
length_Ac6_PTR<-length(Eth_PTR[(4220):4330])
length_Ac7_PTR<-length(Eth_PTR[(4640-110):4640])
length_Ac8_PTR<-length(Eth_PTR[(5150-110):5150])
length_Ac9_PTR<-length(Eth_PTR[(5684-110):5684])

Tinterval_Ac0_PTR<-difftime(Time_PTR_Eth[(1500-110)],Time_PTR_Eth[1500])
Tinterval_Ac1_PTR<-difftime(Time_PTR_Eth[(1965-110)],Time_PTR_Eth[1965])
Tinterval_Ac2_PTR<-difftime(Time_PTR_Eth[(2390-110)],Time_PTR_Eth[2390])
Tinterval_Ac3_PTR<-difftime(Time_PTR_Eth[(2880-110)],Time_PTR_Eth[2880])
Tinterval_Ac4_PTR<-difftime(Time_PTR_Eth[(3460-110)],Time_PTR_Eth[3460])
Tinterval_Ac5_PTR<-difftime(Time_PTR_Eth[(3850-110)],Time_PTR_Eth[3850])
Tinterval_Ac6_PTR<-difftime(Time_PTR_Eth[(4220)],Time_PTR_Eth[4330])
Tinterval_Ac7_PTR<-difftime(Time_PTR_Eth[(4640-110)],Time_PTR_Eth[4640])
Tinterval_Ac8_PTR<-difftime(Time_PTR_Eth[(5150-110)],Time_PTR_Eth[5150])
Tinterval_Ac9_PTR<-difftime(Time_PTR_Eth[(5684-110)],Time_PTR_Eth[5684])


m_Eth_PTR<-c(Ac0_PTR,Ac1_PTR,Ac2_PTR,Ac3_PTR,Ac4_PTR,Ac5_PTR,Ac6_PTR,Ac7_PTR,Ac8_PTR,Ac9_PTR)
sd_Eth_PTR<-c(sd_Ac0_PTR,sd_Ac1_PTR,sd_Ac2_PTR,sd_Ac3_PTR,sd_Ac4_PTR,sd_Ac5_PTR,sd_Ac6_PTR,sd_Ac7_PTR,sd_Ac8_PTR,sd_Ac9_PTR)
length_Eth_PTR<-c(length_Ac0_PTR,length_Ac1_PTR,length_Ac2_PTR,length_Ac3_PTR,length_Ac4_PTR,length_Ac5_PTR,length_Ac6_PTR,length_Ac7_PTR,length_Ac8_PTR,length_Ac9_PTR)
Tinterval_Eth_PTR<-c(Tinterval_Ac0_PTR,Tinterval_Ac1_PTR,Tinterval_Ac2_PTR,Tinterval_Ac3_PTR,Tinterval_Ac4_PTR,Tinterval_Ac5_PTR,Tinterval_Ac6_PTR,Tinterval_Ac7_PTR,Tinterval_Ac8_PTR,Tinterval_Ac9_PTR)

#Select same time intervals in G2508 as in PTR-TOF
Ti_PTR<-round(as.numeric(Time_PTR_Eth[c(1500-110,1500,1965-110,1965,2390-110,2390,2880-110,2880,3460-110,3460,3850-110,3850,4220,4330,4640-110,4640,5150-110,5150,5684-110,5684)]))
Ti_PTR<-Ti_PTR-as.numeric(seconds(120))
numericTime_All<-round(as.numeric(Time_All_Eth))
Pos<-list()
min_dif<-1
for (i in 1:(length(Ti_PTR))){
  min_dif[i]<-min(abs(numericTime_All-Ti_PTR[i]))
  Pos[[i]]<-which(numericTime_All-Ti_PTR[i]==min_dif[i])
  Pos[[i]]<-Pos[[i]][1]
}
t<-unlist(Pos)

### For Publication--> save means for borrowed G2508 ###
write.table(t, file = "Eth_G2508borrowed_intervals.txt", sep = "\t", row.names = TRUE, col.names = NA)
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
Tinterval_All_Eth<-1

for (i in 1:(length(Ti_PTR)-1)){
  m_NH3_All[i]<-mean(NH3_All_Eth[t[i]:t[i+1]])
  sd_NH3_All[i]<-sd(NH3_All_Eth[t[i]:t[i+1]])
  length_All[i]<-length(NH3_All_Eth[t[i]:t[i+1]])
  m_CH4_All[i]<-mean(CH4_All_Eth[t[i]:t[i+1]])
  sd_CH4_All[i]<-sd(CH4_All_Eth[t[i]:t[i+1]])
  m_N2O_All[i]<-mean(N2O_All_Eth[t[i]:t[i+1]])
  sd_N2O_All[i]<-sd(N2O_All_Eth[t[i]:t[i+1]])
  m_H2O_All[i]<-mean(H2O_All_Eth[t[i]:t[i+1]])
  sd_H2O_All[i]<-sd(H2O_All_Eth[t[i]:t[i+1]])
  Tinterval_All_Eth[i]<-difftime(Time_All_Eth[t[i]],Time_All_Eth[t[i+1]])
}
#Only odd positions are correct (Even positions are from t(2)to t(3))
m_NH3_All_Eth<-m_NH3_All[c(TRUE,FALSE)]
sd_NH3_All_Eth<-sd_NH3_All[c(TRUE,FALSE)]
length_All_Eth<-length_All[c(TRUE,FALSE)]
m_CH4_All_Eth<-m_CH4_All[c(TRUE,FALSE)]
sd_CH4_All_Eth<-sd_CH4_All[c(TRUE,FALSE)]
m_N2O_All_Eth<-m_N2O_All[c(TRUE,FALSE)]
sd_N2O_All_Eth<-sd_N2O_All[c(TRUE,FALSE)]
m_N2O_All_Eth<-m_N2O_All[c(TRUE,FALSE)]
sd_N2O_All_Eth<-sd_N2O_All[c(TRUE,FALSE)]
m_H2O_All_Eth<-m_H2O_All[c(TRUE,FALSE)]
sd_H2O_All_Eth<-sd_H2O_All[c(TRUE,FALSE)]
Tinterval_All_Eth<-Tinterval_All_Eth[c(TRUE,FALSE)]

#Select same time intervals in NH3 Picarro as in PTR-TOF
Ti_PTR<-round(as.numeric(Time_PTR_Eth[c(1500-110,1500,1965-110,1965,2390-110,2390,2880-110,2880,3460-110,3460,3850-110,3850,4220,4330,4640-110,4640,5150-110,5150,5684-110,5684)]))
Ti_PTR<-(Ti_PTR-as.numeric(seconds(120)))-as.numeric(minutes(177))
numericTime_NH3<-round(as.numeric(Time_NH3_Eth))
Pos<-list()
min_dif<-1
for (i in 1:(length(Ti_PTR))){
  min_dif[i]<-min(abs(numericTime_NH3-Ti_PTR[i]))
  Pos[[i]]<-which(numericTime_NH3-Ti_PTR[i]==min_dif[i])
  Pos[[i]]<-Pos[[i]][1]
}
t<-unlist(Pos)
### For Publication--> save means for borrowed G2103 ###
write.table(t, file = "Eth_G2103_intervals.txt", sep = "\t", row.names = TRUE, col.names = NA)
########################################################


## Put the calculated positions in G2103
m_NH3r_NH3<-1
sd_NH3r_NH3<-1
length_NH3<-1
m_NH3d_NH3<-1
sd_NH3d_NH3<-1
m_H2O_NH3<-1
sd_H2O_NH3<-1
Tinterval_NH3_Eth<-1

for (i in 1:(length(Ti_PTR)-1)){
  m_NH3r_NH3[i]<-mean(NH3r_NH3_Eth[t[i]:t[i+1]])
  sd_NH3r_NH3[i]<-sd(NH3r_NH3_Eth[t[i]:t[i+1]])
  length_NH3[i]<-length(NH3r_NH3_Eth[t[i]:t[i+1]])
  m_NH3d_NH3[i]<-mean(NH3d_NH3_Eth[t[i]:t[i+1]])
  sd_NH3d_NH3[i]<-sd(NH3d_NH3_Eth[t[i]:t[i+1]])
  m_H2O_NH3[i]<-mean(H2O_NH3_Eth[t[i]:t[i+1]])
  sd_H2O_NH3[i]<-sd(H2O_NH3_Eth[t[i]:t[i+1]])
  Tinterval_NH3_Eth[i]<-difftime(Time_NH3_Eth[t[i]],Time_NH3_Eth[t[i+1]])
}
#Only odd positions are correct (Even positions are from t(2)to t(3))
m_NH3r_NH3_Eth<-m_NH3r_NH3[c(TRUE,FALSE)]
sd_NH3r_NH3_Eth<-sd_NH3r_NH3[c(TRUE,FALSE)]
length_NH3_Eth<-length_NH3[c(TRUE,FALSE)]
m_NH3d_NH3_Eth<-m_NH3d_NH3[c(TRUE,FALSE)]
sd_NH3d_NH3_Eth<-sd_NH3d_NH3[c(TRUE,FALSE)]
m_H2O_NH3_Eth<-m_H2O_NH3[c(TRUE,FALSE)]
sd_H2O_NH3_Eth<-sd_H2O_NH3[c(TRUE,FALSE)]
Tinterval_NH3_Eth<-Tinterval_NH3_Eth[c(TRUE,FALSE)]

#Select same time intervals in EthkPack as in PTR-TOF
Ti_PTR<-round(as.numeric(Time_PTR_Eth[c(1500-110,1500,1965-110,1965,2390-110,2390,2880-110,2880,3460-110,3460,3850-110,3850,4220,4330,4640-110,4640,5150-110,5150,5684-110,5684)]))
Ti_PTR<-(Ti_PTR-as.numeric(seconds(120)))-as.numeric(minutes(119))
numericTime_BP<-round(as.numeric(Time_BP_Eth))
Pos<-list()
min_dif<-1
for (i in 1:(length(Ti_PTR))){
  min_dif[i]<-min(abs(numericTime_BP-Ti_PTR[i]))
  Pos[[i]]<-which(numericTime_BP-Ti_PTR[i]==min_dif[i])
  Pos[[i]]<-Pos[[i]][1]
}
t<-unlist(Pos)
### For Publication--> save means for borrowed G2508 ###
write.table(t, file = "Eth_G4301_intervals.txt", sep = "\t", row.names = TRUE, col.names = NA)
########################################################


## Put the calculated positions in G4301
m_CH4_BP<-1
sd_CH4_BP<-1
length_BP<-1
m_H2O_BP<-1
sd_H2O_BP<-1
m_CH4d_BP<-1
sd_CH4d_BP<-1
Tinterval_BP_Eth<-1

for (i in 1:(length(Ti_PTR)-1)){
  m_CH4_BP[i]<-mean(CH4_BP_Eth[t[i]:t[i+1]])
  sd_CH4_BP[i]<-sd(CH4_BP_Eth[t[i]:t[i+1]])
  length_BP[i]<-length(CH4_BP_Eth[t[i]:t[i+1]])
  m_CH4d_BP[i]<-mean(CH4d_BP_Eth[t[i]:t[i+1]])
  sd_CH4d_BP[i]<-sd(CH4d_BP_Eth[t[i]:t[i+1]])
  m_H2O_BP[i]<-mean(H2O_BP_Eth[t[i]:t[i+1]])
  sd_H2O_BP[i]<-sd(H2O_BP_Eth[t[i]:t[i+1]])
  Tinterval_BP_Eth[i]<-difftime(Time_BP_Eth[t[i]],Time_BP_Eth[t[i+1]])
}
#Only odd positions are correct (Even positions are from t(2)to t(3))
m_CH4_BP_Eth<-m_CH4_BP[c(TRUE,FALSE)]
sd_CH4_BP_Eth<-sd_CH4_BP[c(TRUE,FALSE)]
length_BP_Eth<-length_BP[c(TRUE,FALSE)]
m_CH4d_BP_Eth<-m_CH4d_BP[c(TRUE,FALSE)]
sd_CH4d_BP_Eth<-sd_CH4d_BP[c(TRUE,FALSE)]
m_H2O_BP_Eth<-m_H2O_BP[c(TRUE,FALSE)]
sd_H2O_BP_Eth<-sd_H2O_BP[c(TRUE,FALSE)]
Tinterval_BP_Eth<-Tinterval_BP_Eth[c(TRUE,FALSE)]

#Select same time intervals in own all Picarro as in PTR-TOF

Ti_PTR<-round(as.numeric(Time_PTR_Eth[c(1500-110,1500,1965-110,1965,2390-110,2390,2880-110,2880,3460-110,3460,3850-110,3850,4220,4330,4640-110,4640,5150-110,5150,5684-110,5684)]))
Ti_PTR<-(Ti_PTR-as.numeric(seconds(120)))-as.numeric(minutes(57))
numericTime_own<-round(as.numeric(Time_own_Eth))
Pos<-list()
min_dif<-1
for (i in 1:(length(Ti_PTR))){
  min_dif[i]<-min(abs(numericTime_own-Ti_PTR[i]))
  Pos[[i]]<-which(abs(numericTime_own-Ti_PTR[i])==min_dif[i])
  Pos[[i]]<-Pos[[i]][1]
}
t<-unlist(Pos)
### For Publication--> save means for borrowed G2508 ###
write.table(t, file = "Eth_G2508own_intervals.txt", sep = "\t", row.names = TRUE, col.names = NA)
########################################################


# Put the calculated positions in G2508
m_NH3_own<-1
sd_NH3_own<-1
length_own<-1
m_H2O_own<-1
sd_H2O_own<-1
m_CH4_own<-1
sd_CH4_own<-1
m_N2O_own<-1
sd_N2O_own<-1
Tinterval_own_Eth<-1

for (i in 1:(length(Ti_PTR)-1)){
  m_NH3_own[i]<-mean(NH3_own_Eth[t[i]:t[i+1]])
  sd_NH3_own[i]<-sd(NH3_own_Eth[t[i]:t[i+1]])
  length_own[i]<-length(NH3_own_Eth[t[i]:t[i+1]])
  m_CH4_own[i]<-mean(CH4_own_Eth[t[i]:t[i+1]])
  sd_CH4_own[i]<-sd(CH4_own_Eth[t[i]:t[i+1]])
  m_N2O_own[i]<-mean(N2O_own_Eth[t[i]:t[i+1]])
  sd_N2O_own[i]<-sd(N2O_own_Eth[t[i]:t[i+1]])
  m_H2O_own[i]<-mean(H2O_own_Eth[t[i]:t[i+1]])
  sd_H2O_own[i]<-sd(H2O_own_Eth[t[i]:t[i+1]])
  Tinterval_own_Eth[i]<-difftime(Time_own_Eth[t[i]],Time_own_Eth[t[i+1]])
}
#Only odd positions are correct (Even positions are from t(2)to t(3))
m_NH3_own_Eth<-m_NH3_own[c(TRUE,FALSE)]
sd_NH3_own_Eth<-sd_NH3_own[c(TRUE,FALSE)]
length_own_Eth<-length_own[c(TRUE,FALSE)]
m_CH4_own_Eth<-m_CH4_own[c(TRUE,FALSE)]
sd_CH4_own_Eth<-sd_CH4_own[c(TRUE,FALSE)]
m_N2O_own_Eth<-m_N2O_own[c(TRUE,FALSE)]
sd_N2O_own_Eth<-sd_N2O_own[c(TRUE,FALSE)]
m_N2O_own_Eth<-m_N2O_own[c(TRUE,FALSE)]
sd_N2O_own_Eth<-sd_N2O_own[c(TRUE,FALSE)]
m_H2O_own_Eth<-m_H2O_own[c(TRUE,FALSE)]
sd_H2O_own_Eth<-sd_H2O_own[c(TRUE,FALSE)]
Tinterval_own_Eth<-Tinterval_own_Eth[c(TRUE,FALSE)]


## Create and join all points into one data frame ##
#NH3
y_NH3<-c(m_NH3_All_Eth,m_NH3r_NH3_Eth,m_NH3d_NH3_Eth,m_NH3_own_Eth)
x_NH3<-c(m_Eth_PTR,m_Eth_PTR,m_Eth_PTR,m_Eth_PTR)
sd_NH3<-c(sd_NH3_All_Eth,sd_NH3r_NH3_Eth,sd_NH3d_NH3_Eth,sd_NH3_own_Eth)
Color_NH3<-c(rep("NH3_G2508_borrowed",10),rep("NH3_raw",10),rep("NH3_dry",10),rep("NH3_G2508_own",10))
df_NH3_Eth<-cbind.data.frame(x_NH3,y_NH3,sd_NH3,Color_NH3)
#CH4
y_CH4<-c(m_CH4_All_Eth,m_CH4_BP_Eth,m_CH4d_BP_Eth,m_CH4_own_Eth)
x_CH4<-c(m_Eth_PTR,m_Eth_PTR,m_Eth_PTR,m_Eth_PTR)
sd_CH4<-c(sd_CH4_All_Eth,sd_CH4_BP_Eth,sd_CH4d_BP_Eth,sd_CH4_own_Eth)
Color_CH4<-c(rep("CH4_G2508_borrowed",10),rep("CH4",10),rep("CH4_dry",10),rep("CH4_G2508_own",10))
df_CH4_Eth<-cbind.data.frame(x_CH4,y_CH4,sd_CH4,Color_CH4)
#N2O
y_N2O<-c(m_N2O_All_Eth,m_N2O_own_Eth)
x_N2O<-c(m_Eth_PTR,m_Eth_PTR)
sd_N2O<-c(sd_N2O_All_Eth,sd_N2O_own_Eth)
Color_N2O<-c(rep("N2O_G2508_borrowed",10),rep("N2O_G2508_own",10))
df_N2O_Eth<-cbind.data.frame(x_N2O,y_N2O,sd_N2O,Color_N2O)


### For publication ###
## Remove NH3_raw & CH4
##Add standard deviation of VOC

#NH3
y_NH3<-c(m_NH3_All_AAc,m_NH3d_NH3_AAc,m_NH3_own_AAc)
x_NH3<-c(m_AAc_PTR,m_AAc_PTR,m_AAc_PTR)
sd_NH3<-c(sd_NH3_All_AAc,sd_NH3d_NH3_AAc,sd_NH3_own_AAc)
Color_NH3<-c(rep("G2509_1",10),rep("G2103",10),rep("G2509_2",10))
df_NH3_AAc<-cbind.data.frame(x_NH3,y_NH3,sd_NH3,Color_NH3)
sd_AAc<-c(sd_AAc_PTR,sd_AAc_PTR,sd_AAc_PTR)

#CH4
y_CH4<-c(m_CH4_All_AAc,m_CH4_own_AAc)
x_CH4<-c(m_AAc_PTR,m_AAc_PTR)
sd_CH4<-c(sd_CH4_All_AAc,sd_CH4_own_AAc)
Color_CH4<-c(rep("G2509_1",10),rep("G2509_2",10))
df_CH4_AAc<-cbind.data.frame(x_CH4,y_CH4,sd_CH4,Color_CH4)
#N2O
y_N2O<-c(m_N2O_All_AAc,m_N2O_own_AAc)
x_N2O<-c(m_AAc_PTR,m_AAc_PTR)
sd_N2O<-c(sd_N2O_All_AAc,sd_N2O_own_AAc)
Color_N2O<-c(rep("G2509_1",10),rep("G2509_2",10))
df_N2O_AAc<-cbind.data.frame(x_N2O,y_N2O,sd_N2O,Color_N2O)

df_Control_AAc<-cbind.data.frame(length_AAc_PTR,length_BP_AAc,length_NH3_AAc,length_All_AAc,length_own_AAc,
                                 Tinterval_AAc_PTR,Tinterval_BP_AAc,Tinterval_NH3_AAc,Tinterval_All_AAc,Tinterval_own_AAc,
                                 m_H2O_BP_AAc,m_H2O_NH3_AAc,m_H2O_All_AAc,m_H2O_own_AAc)




## Ethanol both
#NH3
y_NH3<-c(m_NH3_All_Eth,m_NH3d_NH3_Eth,m_NH3_own_Eth)
x_NH3<-c(m_Eth_PTR,m_Eth_PTR,m_Eth_PTR)
sd_NH3<-c(sd_NH3_All_Eth,sd_NH3d_NH3_Eth,sd_NH3_own_Eth)
Color_NH3<-c(rep("G2509_1",10),rep("G2103",10),rep("G2509_2",10))
df_NH3_Eth<-cbind.data.frame(x_NH3,y_NH3,sd_NH3,Color_NH3)
sd_Eth<-c(sd_Eth_PTR,sd_Eth_PTR,sd_Eth_PTR)

#CH4
y_CH4<-c(m_CH4_All_Eth,m_CH4_own_Eth)
x_CH4<-c(m_Eth_PTR,m_Eth_PTR)
sd_CH4<-c(sd_CH4_All_Eth,sd_CH4_own_Eth)
Color_CH4<-c(rep("G2509_1",10),rep("G2509_2",10))
df_CH4_Eth<-cbind.data.frame(x_CH4,y_CH4,sd_CH4,Color_CH4)
#N2O
y_N2O<-c(m_N2O_All_Eth,m_N2O_own_Eth)
x_N2O<-c(m_Eth_PTR,m_Eth_PTR)
sd_N2O<-c(sd_N2O_All_Eth,sd_N2O_own_Eth)
Color_N2O<-c(rep("G2509_1",10),rep("G2509_2",10))
df_N2O_Eth<-cbind.data.frame(x_N2O,y_N2O,sd_N2O,Color_N2O)

df_Control_Eth<-cbind.data.frame(length_Eth_PTR,length_BP_Eth,length_NH3_Eth,length_All_Eth,length_own_Eth,
                                 Tinterval_Eth_PTR,Tinterval_BP_Eth,Tinterval_NH3_Eth,Tinterval_All_Eth,Tinterval_own_Eth,
                                 m_H2O_BP_Eth,m_H2O_NH3_Eth,m_H2O_All_Eth,m_H2O_own_Eth)


##Full test acetic acid (both)
#NH3


###Save the data frame for publication

##Ethanol
setwd("C:/Users/au615105/OneDrive - Aarhus universitet/Pablo Garc?a/Papers/Interferences pictures/VOCs")
write.table(df_N2O_Eth, file = "Eth_N2O.txt", sep = "\t", row.names = TRUE, col.names = NA)
write.table(df_NH3_Eth, file = "Eth_NH3.txt", sep = "\t", row.names = TRUE, col.names = NA)
write.table(df_CH4_Eth, file = "Eth_CH4.txt", sep = "\t", row.names = TRUE, col.names = NA)
write.table(sd_Eth_PTR, file = "sd_Eth.txt", sep = "\t", row.names = TRUE, col.names = NA)
write.table(Dat_All, file = "Eth_G2109borrowed_CH4.txt", sep = "\t", row.names = TRUE, col.names = NA)
write.table(Dat_own, file = "Eth_G2109own_CH4.txt", sep = "\t", row.names = TRUE, col.names = NA)
