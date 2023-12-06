library(dplyr)
library(ggplot2)
library(anytime)
library(lubridate)
library(writexl)
library("readxl")




### Interferences 2021-07-06###   

##Diacetyl

##Butanone

##1-butanol

##1-propanol

##Acetaldehyde

##Extract data from borrowed picarro##  
setwd(".../VOC Interferences/G2508 borrowed")
list_of_files <- list.files(path="06",pattern = "*.dat",recursive=TRUE) ##Read data from Picarro
L<-length(list_of_files)

setwd(".../VOC Interferences/G2508 borrowed/06")

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
list_of_files <- list.files(path="06",pattern = "*.dat",recursive=TRUE) ##Read data from Picarro
L<-length(list_of_files)

setwd(".../VOC Interferences/NH3 Picarro/06")

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
list_of_files <- list.files(path="06",pattern = "*.dat",recursive=TRUE) ##Read data from Picarro
L<-length(list_of_files)

setwd(".../VOC Interferences/BackPack/06")

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
setwd(".../VOC Interferences/PTR-TOF")
B1<-read_excel("1-butanol.xlsx")
B1<-as.data.frame(B1)
P1<-read_excel("1-propanol.xlsx")
P1<-as.data.frame(P1)
AA<-read_excel("acetaldehyde.xlsx")
AA<-as.data.frame(AA)
Bne<-read_excel("butanone.xlsx")
Bne<-as.data.frame(Bne)
DA<-read_excel("diacetyl.xlsx")
DA<-as.data.frame(DA)

###Data to plot all the calibration###
##1-butanol##

Time_PTR_B1<-c(B1$AbsTime)
B1_PTR<-c(B1$`m39.0229 (C3H3+)`+B1$`m41.0386 (C3H5+)`+B1$`m55.0542 (C4H6H+)`+B1$`m57.0699 (C4H9+)`+B1$`m73.0648 (C4H8OH+)`)

#Sincronize all Picarros and PTR
TAc_PTR<-round(as.numeric(Time_PTR_B1[c(1,length(Time_PTR_B1))]))
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
Time_All_B1<-c(Dat_All$date.time[tAc[1]:tAc[2]])
Time_BP_B1<-c(Dat_BP$date.time[tAcc[1]:tAcc[2]])
Time_NH3_B1<-c(Dat_NH3$date.time[tAcn[1]:tAcn[2]])
H2O_All_B1<-c(Dat_All$H2O[tAc[1]:tAc[2]])
H2O_BP_B1<-c(Dat_BP$H2O[tAcc[1]:tAcc[2]])
H2O_NH3_B1<-c(Dat_NH3$H2O[tAcn[1]:tAcn[2]])
NH3_All_B1<-c(Dat_All$NH3[tAc[1]:tAc[2]])
NH3r_NH3_B1<-c(Dat_NH3$NH3_Raw[tAcn[1]:tAcn[2]])
NH3d_NH3_B1<-c(Dat_NH3$NH3_dry[tAcn[1]:tAcn[2]])
CH4_All_B1<-c(Dat_All$CH4_dry[tAc[1]:tAc[2]])
CH4_BP_B1<-c(Dat_BP$CH4[tAcc[1]:tAcc[2]])
CH4d_BP_B1<-c(Dat_BP$CH4_dry[tAcc[1]:tAcc[2]])
N2O_All_B1<-c(Dat_All$N2O_dry[tAc[1]:tAc[2]])

#### Save the intervals for publication ####
Interval_B1<-cbind.data.frame(tAc,tAcn,tAcc)
write.table(Interval_B1, file = "B1_Interval.txt", sep = "\t", row.names = TRUE, col.names = NA)

##1-propanol##

Time_PTR_P1<-c(P1$AbsTime)
P1_PTR<-c(P1$`m39.0229 (C3H3+)`+P1$`m41.0386 (C3H5+)`+P1$`m43.0542 (C3H6H+)`)

#Sincronize all Picarros and PTR
TAc_PTR<-round(as.numeric(Time_PTR_P1[c(1,length(Time_PTR_P1))]))
TAc_PTR<-TAc_PTR-as.numeric(seconds(115))
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
Time_All_P1<-c(Dat_All$date.time[tAc[1]:tAc[2]])
Time_BP_P1<-c(Dat_BP$date.time[tAcc[1]:tAcc[2]])
Time_NH3_P1<-c(Dat_NH3$date.time[tAcn[1]:tAcn[2]])
H2O_All_P1<-c(Dat_All$H2O[tAc[1]:tAc[2]])
H2O_BP_P1<-c(Dat_BP$H2O[tAcc[1]:tAcc[2]])
H2O_NH3_P1<-c(Dat_NH3$H2O[tAcn[1]:tAcn[2]])
NH3_All_P1<-c(Dat_All$NH3[tAc[1]:tAc[2]])
NH3r_NH3_P1<-c(Dat_NH3$NH3_Raw[tAcn[1]:tAcn[2]])
NH3d_NH3_P1<-c(Dat_NH3$NH3_dry[tAcn[1]:tAcn[2]])
CH4_All_P1<-c(Dat_All$CH4_dry[tAc[1]:tAc[2]])
CH4_BP_P1<-c(Dat_BP$CH4[tAcc[1]:tAcc[2]])
CH4d_BP_P1<-c(Dat_BP$CH4_dry[tAcc[1]:tAcc[2]])
N2O_All_P1<-c(Dat_All$N2O_dry[tAc[1]:tAc[2]])
#### Save the intervals for publication ####
Interval_P1<-cbind.data.frame(tAc,tAcn,tAcc)
write.table(Interval_P1, file = "P1_Interval.txt", sep = "\t", row.names = TRUE, col.names = NA)


##Acetaldehyde##

Time_PTR_AA<-c(AA$AbsTime)
AA_PTR<-c(AA$`m45.0335 (C2H4OH+)`)

#Sincronize all Picarros and PTR
TAc_PTR<-round(as.numeric(Time_PTR_AA[c(1,length(Time_PTR_AA))]))
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
Time_All_AA<-c(Dat_All$date.time[tAc[1]:tAc[2]])
Time_BP_AA<-c(Dat_BP$date.time[tAcc[1]:tAcc[2]])
Time_NH3_AA<-c(Dat_NH3$date.time[tAcn[1]:tAcn[2]])
H2O_All_AA<-c(Dat_All$H2O[tAc[1]:tAc[2]])
H2O_BP_AA<-c(Dat_BP$H2O[tAcc[1]:tAcc[2]])
H2O_NH3_AA<-c(Dat_NH3$H2O[tAcn[1]:tAcn[2]])
NH3_All_AA<-c(Dat_All$NH3[tAc[1]:tAc[2]])
NH3r_NH3_AA<-c(Dat_NH3$NH3_Raw[tAcn[1]:tAcn[2]])
NH3d_NH3_AA<-c(Dat_NH3$NH3_dry[tAcn[1]:tAcn[2]])
CH4_All_AA<-c(Dat_All$CH4_dry[tAc[1]:tAc[2]])
CH4_BP_AA<-c(Dat_BP$CH4[tAcc[1]:tAcc[2]])
CH4d_BP_AA<-c(Dat_BP$CH4_dry[tAcc[1]:tAcc[2]])
N2O_All_AA<-c(Dat_All$N2O_dry[tAc[1]:tAc[2]])
#### Save the intervals for publication ####
Interval_AA<-cbind.data.frame(tAc,tAcn,tAcc)
write.table(Interval_AA, file = "AA_Interval.txt", sep = "\t", row.names = TRUE, col.names = NA)

##Butanone##

Time_PTR_Bne<-c(Bne$AbsTime)
Bne_PTR<-c(Bne$`m73.0648 (C4H8OH+)`)

#Sincronize all Picarros and PTR
TAc_PTR<-round(as.numeric(Time_PTR_Bne[c(1,length(Time_PTR_Bne))]))
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
Time_All_Bne<-c(Dat_All$date.time[tAc[1]:tAc[2]])
Time_BP_Bne<-c(Dat_BP$date.time[tAcc[1]:tAcc[2]])
Time_NH3_Bne<-c(Dat_NH3$date.time[tAcn[1]:tAcn[2]])
H2O_All_Bne<-c(Dat_All$H2O[tAc[1]:tAc[2]])
H2O_BP_Bne<-c(Dat_BP$H2O[tAcc[1]:tAcc[2]])
H2O_NH3_Bne<-c(Dat_NH3$H2O[tAcn[1]:tAcn[2]])
NH3_All_Bne<-c(Dat_All$NH3[tAc[1]:tAc[2]])
NH3r_NH3_Bne<-c(Dat_NH3$NH3_Raw[tAcn[1]:tAcn[2]])
NH3d_NH3_Bne<-c(Dat_NH3$NH3_dry[tAcn[1]:tAcn[2]])
CH4_All_Bne<-c(Dat_All$CH4_dry[tAc[1]:tAc[2]])
CH4_BP_Bne<-c(Dat_BP$CH4[tAcc[1]:tAcc[2]])
CH4d_BP_Bne<-c(Dat_BP$CH4_dry[tAcc[1]:tAcc[2]])
N2O_All_Bne<-c(Dat_All$N2O_dry[tAc[1]:tAc[2]])

#### Save the intervals for publication ####
Interval_Bne<-cbind.data.frame(tAc,tAcn,tAcc)
write.table(Interval_Bne, file = "Bne_Interval.txt", sep = "\t", row.names = TRUE, col.names = NA)



##Diacetyl##

Time_PTR_DA<-c(DA$AbsTime)
DA_PTR<-c(DA$`m87.0441 (C4H6O2H+)`)

#Sincronize all Picarros and PTR
TAc_PTR<-round(as.numeric(Time_PTR_DA[c(1,length(Time_PTR_DA))]))
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
Time_All_DA<-c(Dat_All$date.time[tAc[1]:tAc[2]])
Time_BP_DA<-c(Dat_BP$date.time[tAcc[1]:tAcc[2]])
Time_NH3_DA<-c(Dat_NH3$date.time[tAcn[1]:tAcn[2]])
H2O_All_DA<-c(Dat_All$H2O[tAc[1]:tAc[2]])
H2O_BP_DA<-c(Dat_BP$H2O[tAcc[1]:tAcc[2]])
H2O_NH3_DA<-c(Dat_NH3$H2O[tAcn[1]:tAcn[2]])
NH3_All_DA<-c(Dat_All$NH3[tAc[1]:tAc[2]])
NH3r_NH3_DA<-c(Dat_NH3$NH3_Raw[tAcn[1]:tAcn[2]])
NH3d_NH3_DA<-c(Dat_NH3$NH3_dry[tAcn[1]:tAcn[2]])
CH4_All_DA<-c(Dat_All$CH4_dry[tAc[1]:tAc[2]])
CH4_BP_DA<-c(Dat_BP$CH4[tAcc[1]:tAcc[2]])
CH4d_BP_DA<-c(Dat_BP$CH4_dry[tAcc[1]:tAcc[2]])
N2O_All_DA<-c(Dat_All$N2O_dry[tAc[1]:tAc[2]])

#### Save the intervals for publication ####
Interval_DA<-cbind.data.frame(tAc,tAcn,tAcc)
write.table(Interval_DA, file = "DA_Interval.txt", sep = "\t", row.names = TRUE, col.names = NA)


###Data to plot the interferences at each concentration###
##1-butanol
Ac0_PTR<-mean(B1_PTR[(1800-120):1800])
Ac1_PTR<-mean(B1_PTR[(2500-120):2500])
Ac2_PTR<-mean(B1_PTR[(3090-120):3090])
Ac3_PTR<-mean(B1_PTR[(3490-120):3490])
Ac4_PTR<-mean(B1_PTR[(3890-120):3890])
Ac5_PTR<-mean(B1_PTR[(4240-120):4240])
Ac6_PTR<-mean(B1_PTR[(4550-120):4550])
Ac7_PTR<-mean(B1_PTR[4880:5000])
Ac8_PTR<-mean(B1_PTR[(5320-120):5320])
Ac9_PTR<-mean(B1_PTR[(5610-120):5610])
Ac10_PTR<-mean(B1_PTR[(5814-120):5814])
Ac11_PTR<-mean(B1_PTR[(7425-120):7425])

sd_Ac0_PTR<-sd(B1_PTR[(1800-120):1800])
sd_Ac1_PTR<-sd(B1_PTR[(2500-120):2500])
sd_Ac2_PTR<-sd(B1_PTR[(3090-120):3090])
sd_Ac3_PTR<-sd(B1_PTR[(3490-120):3490])
sd_Ac4_PTR<-sd(B1_PTR[(3890-120):3890])
sd_Ac5_PTR<-sd(B1_PTR[(4240-120):4240])
sd_Ac6_PTR<-sd(B1_PTR[(4550-120):4550])
sd_Ac7_PTR<-sd(B1_PTR[4880:5000])
sd_Ac8_PTR<-sd(B1_PTR[(5320-120):5320])
sd_Ac9_PTR<-sd(B1_PTR[(5610-120):5610])
sd_Ac10_PTR<-sd(B1_PTR[(5814-120):5814])
sd_Ac11_PTR<-sd(B1_PTR[(7425-120):7425])

length_Ac0_PTR<-length(B1_PTR[(1800-120):1800])
length_Ac1_PTR<-length(B1_PTR[(2500-120):2500])
length_Ac2_PTR<-length(B1_PTR[(3090-120):3090])
length_Ac3_PTR<-length(B1_PTR[(3490-120):3490])
length_Ac4_PTR<-length(B1_PTR[(3890-120):3890])
length_Ac5_PTR<-length(B1_PTR[(4240-120):4240])
length_Ac6_PTR<-length(B1_PTR[(4550-120):4550])
length_Ac7_PTR<-length(B1_PTR[4880:5000])
length_Ac8_PTR<-length(B1_PTR[(5320-120):5320])
length_Ac9_PTR<-length(B1_PTR[(5610-120):5610])
length_Ac10_PTR<-length(B1_PTR[(5814-120):5814])
length_Ac11_PTR<-length(B1_PTR[(7425-120):7425])

Tinterval_Ac0_PTR<-difftime(Time_PTR_B1[(1800-120)],Time_PTR_B1[1800])
Tinterval_Ac1_PTR<-difftime(Time_PTR_B1[(2500-120)],Time_PTR_B1[2500])
Tinterval_Ac2_PTR<-difftime(Time_PTR_B1[(3090-120)],Time_PTR_B1[3090])
Tinterval_Ac3_PTR<-difftime(Time_PTR_B1[(3490-120)],Time_PTR_B1[3490])
Tinterval_Ac4_PTR<-difftime(Time_PTR_B1[(3890-120)],Time_PTR_B1[3890])
Tinterval_Ac5_PTR<-difftime(Time_PTR_B1[(4240-120)],Time_PTR_B1[4240])
Tinterval_Ac6_PTR<-difftime(Time_PTR_B1[(4550-120)],Time_PTR_B1[4550])
Tinterval_Ac7_PTR<-difftime(Time_PTR_B1[4880],Time_PTR_B1[5000])
Tinterval_Ac8_PTR<-difftime(Time_PTR_B1[(5320-120)],Time_PTR_B1[5320])
Tinterval_Ac9_PTR<-difftime(Time_PTR_B1[(5610-120)],Time_PTR_B1[5610])
Tinterval_Ac10_PTR<-difftime(Time_PTR_B1[(5814-120)],Time_PTR_B1[5814])
Tinterval_Ac11_PTR<-difftime(Time_PTR_B1[(7425-120)],Time_PTR_B1[7425])

m_B1_PTR<-c(Ac0_PTR,Ac1_PTR,Ac2_PTR,Ac3_PTR,Ac4_PTR,Ac5_PTR,Ac6_PTR,Ac7_PTR,Ac8_PTR,Ac9_PTR,Ac10_PTR,Ac11_PTR)
sd_B1_PTR<-c(sd_Ac0_PTR,sd_Ac1_PTR,sd_Ac2_PTR,sd_Ac3_PTR,sd_Ac4_PTR,sd_Ac5_PTR,sd_Ac6_PTR,sd_Ac7_PTR,sd_Ac8_PTR,sd_Ac9_PTR,sd_Ac10_PTR,sd_Ac11_PTR)
length_B1_PTR<-c(length_Ac0_PTR,length_Ac1_PTR,length_Ac2_PTR,length_Ac3_PTR,length_Ac4_PTR,length_Ac5_PTR,length_Ac6_PTR,length_Ac7_PTR,length_Ac8_PTR,length_Ac9_PTR,length_Ac10_PTR,length_Ac11_PTR)
Tinterval_B1_PTR<-c(Tinterval_Ac0_PTR,Tinterval_Ac1_PTR,Tinterval_Ac2_PTR,Tinterval_Ac3_PTR,Tinterval_Ac4_PTR,Tinterval_Ac5_PTR,Tinterval_Ac6_PTR,Tinterval_Ac7_PTR,Tinterval_Ac8_PTR,Tinterval_Ac9_PTR,Tinterval_Ac10_PTR,Tinterval_Ac11_PTR)

#Select same time intervals in G2508 as in PTR-TOF
Ti_PTR<-round(as.numeric(Time_PTR_B1[c(1800-120,1800,2500-120,2500,3090-120,3090,3490-120,3490,3890-120,3890,4240-120,4240,4550-120,4550,4880,5000,5320-120,5320,5610-120,5610,5814-120,5810,7425-120,7425)]))
Ti_PTR<-Ti_PTR-as.numeric(seconds(90))
numericTime_All<-round(as.numeric(Time_All_B1))
Pos<-list()
min_dif<-1
for (i in 1:(length(Ti_PTR))){
  min_dif[i]<-min(abs(numericTime_All-Ti_PTR[i]))
  Pos[[i]]<-which(numericTime_All-Ti_PTR[i]==min_dif[i])
  Pos[[i]]<-Pos[[i]][1]
}
t<-unlist(Pos)
### For Publication--> save means for borrowed G2508 ###
write.table(t, file = "B1_G2509_intervals.txt", sep = "\t", row.names = TRUE, col.names = NA)
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
Tinterval_All_B1<-1

for (i in 1:(length(Ti_PTR)-1)){
  m_NH3_All[i]<-mean(NH3_All_B1[t[i]:t[i+1]])
  sd_NH3_All[i]<-sd(NH3_All_B1[t[i]:t[i+1]])
  length_All[i]<-length(NH3_All_B1[t[i]:t[i+1]])
  m_CH4_All[i]<-mean(CH4_All_B1[t[i]:t[i+1]])
  sd_CH4_All[i]<-sd(CH4_All_B1[t[i]:t[i+1]])
  m_N2O_All[i]<-mean(N2O_All_B1[t[i]:t[i+1]])
  sd_N2O_All[i]<-sd(N2O_All_B1[t[i]:t[i+1]])
  m_H2O_All[i]<-mean(H2O_All_B1[t[i]:t[i+1]])
  sd_H2O_All[i]<-sd(H2O_All_B1[t[i]:t[i+1]])
  Tinterval_All_B1[i]<-difftime(Time_All_B1[t[i]],Time_All_B1[t[i+1]])
}
#Only odd positions are correct (Even positions are from t(2)to t(3))
m_NH3_All_B1<-m_NH3_All[c(TRUE,FALSE)]
sd_NH3_All_B1<-sd_NH3_All[c(TRUE,FALSE)]
length_All_B1<-length_All[c(TRUE,FALSE)]
m_CH4_All_B1<-m_CH4_All[c(TRUE,FALSE)]
sd_CH4_All_B1<-sd_CH4_All[c(TRUE,FALSE)]
m_N2O_All_B1<-m_N2O_All[c(TRUE,FALSE)]
sd_N2O_All_B1<-sd_N2O_All[c(TRUE,FALSE)]
m_N2O_All_B1<-m_N2O_All[c(TRUE,FALSE)]
sd_N2O_All_B1<-sd_N2O_All[c(TRUE,FALSE)]
m_H2O_All_B1<-m_H2O_All[c(TRUE,FALSE)]
sd_H2O_All_B1<-sd_H2O_All[c(TRUE,FALSE)]
Tinterval_All_B1<-Tinterval_All_B1[c(TRUE,FALSE)]

#Select same time intervals in NH3 Picarro as in PTR-TOF
Ti_PTR<-round(as.numeric(Time_PTR_B1[c(1800-120,1800,2500-120,2500,3090-120,3090,3490-120,3490,3890-120,3890,4240-120,4240,4550-120,4550,4880,5000,5320-120,5320,5610-120,5610,5814-120,5810,7425-120,7425)]))
Ti_PTR<-(Ti_PTR-as.numeric(seconds(90)))-as.numeric(minutes(177))
numericTime_NH3<-round(as.numeric(Time_NH3_B1))
Pos<-list()
min_dif<-1
for (i in 1:(length(Ti_PTR))){
  min_dif[i]<-min(abs(numericTime_NH3-Ti_PTR[i]))
  Pos[[i]]<-which(numericTime_NH3-Ti_PTR[i]==min_dif[i])
  Pos[[i]]<-Pos[[i]][1]
}
t<-unlist(Pos)
### For Publication--> save means for G2103 ###
write.table(t, file = "B1_G2103_intervals.txt", sep = "\t", row.names = TRUE, col.names = NA)
########################################################

## Put the calculated positions in G2103
m_NH3r_NH3<-1
sd_NH3r_NH3<-1
length_NH3<-1
m_NH3d_NH3<-1
sd_NH3d_NH3<-1
m_H2O_NH3<-1
sd_H2O_NH3<-1
Tinterval_NH3_B1<-1

for (i in 1:(length(Ti_PTR)-1)){
  m_NH3r_NH3[i]<-mean(NH3r_NH3_B1[t[i]:t[i+1]])
  sd_NH3r_NH3[i]<-sd(NH3r_NH3_B1[t[i]:t[i+1]])
  length_NH3[i]<-length(NH3r_NH3_B1[t[i]:t[i+1]])
  m_NH3d_NH3[i]<-mean(NH3d_NH3_B1[t[i]:t[i+1]])
  sd_NH3d_NH3[i]<-sd(NH3d_NH3_B1[t[i]:t[i+1]])
  m_H2O_NH3[i]<-mean(H2O_NH3_B1[t[i]:t[i+1]])
  sd_H2O_NH3[i]<-sd(H2O_NH3_B1[t[i]:t[i+1]])
  Tinterval_NH3_B1[i]<-difftime(Time_NH3_B1[t[i]],Time_NH3_B1[t[i+1]])
}
#Only odd positions are correct (Even positions are from t(2)to t(3))
m_NH3r_NH3_B1<-m_NH3r_NH3[c(TRUE,FALSE)]
sd_NH3r_NH3_B1<-sd_NH3r_NH3[c(TRUE,FALSE)]
length_NH3_B1<-length_NH3[c(TRUE,FALSE)]
m_NH3d_NH3_B1<-m_NH3d_NH3[c(TRUE,FALSE)]
sd_NH3d_NH3_B1<-sd_NH3d_NH3[c(TRUE,FALSE)]
m_H2O_NH3_B1<-m_H2O_NH3[c(TRUE,FALSE)]
sd_H2O_NH3_B1<-sd_H2O_NH3[c(TRUE,FALSE)]
Tinterval_NH3_B1<-Tinterval_NH3_B1[c(TRUE,FALSE)]

#Select same time intervals in B1kPack as in PTR-TOF
Ti_PTR<-round(as.numeric(Time_PTR_B1[c(1800-120,1800,2500-120,2500,3090-120,3090,3490-120,3490,3890-120,3890,4240-120,4240,4550-120,4550,4880,5000,5320-120,5320,5610-120,5610,5814-120,5810,7425-120,7425)]))
Ti_PTR<-(Ti_PTR-as.numeric(seconds(90)))-as.numeric(minutes(119))
numericTime_BP<-round(as.numeric(Time_BP_B1))
Pos<-list()
min_dif<-1
for (i in 1:(length(Ti_PTR))){
  min_dif[i]<-min(abs(numericTime_BP-Ti_PTR[i]))
  Pos[[i]]<-which(numericTime_BP-Ti_PTR[i]==min_dif[i])
  Pos[[i]]<-Pos[[i]][1]
}
t<-unlist(Pos)
### For Publication--> save means for G4301 ###
write.table(t, file = "B1_G4301_intervals.txt", sep = "\t", row.names = TRUE, col.names = NA)
########################################################

## Put the calculated positions in G4301
m_CH4_BP<-1
sd_CH4_BP<-1
length_BP<-1
m_H2O_BP<-1
sd_H2O_BP<-1
m_CH4d_BP<-1
sd_CH4d_BP<-1
Tinterval_BP_B1<-1

for (i in 1:(length(Ti_PTR)-1)){
  m_CH4_BP[i]<-mean(CH4_BP_B1[t[i]:t[i+1]])
  sd_CH4_BP[i]<-sd(CH4_BP_B1[t[i]:t[i+1]])
  length_BP[i]<-length(CH4_BP_B1[t[i]:t[i+1]])
  m_CH4d_BP[i]<-mean(CH4d_BP_B1[t[i]:t[i+1]])
  sd_CH4d_BP[i]<-sd(CH4d_BP_B1[t[i]:t[i+1]])
  m_H2O_BP[i]<-mean(H2O_BP_B1[t[i]:t[i+1]])
  sd_H2O_BP[i]<-sd(H2O_BP_B1[t[i]:t[i+1]])
  Tinterval_BP_B1[i]<-difftime(Time_BP_B1[t[i]],Time_BP_B1[t[i+1]])
}
#Only odd positions are correct (Even positions are from t(2)to t(3))
m_CH4_BP_B1<-m_CH4_BP[c(TRUE,FALSE)]
sd_CH4_BP_B1<-sd_CH4_BP[c(TRUE,FALSE)]
length_BP_B1<-length_BP[c(TRUE,FALSE)]
m_CH4d_BP_B1<-m_CH4d_BP[c(TRUE,FALSE)]
sd_CH4d_BP_B1<-sd_CH4d_BP[c(TRUE,FALSE)]
m_H2O_BP_B1<-m_H2O_BP[c(TRUE,FALSE)]
sd_H2O_BP_B1<-sd_H2O_BP[c(TRUE,FALSE)]
Tinterval_BP_B1<-Tinterval_BP_B1[c(TRUE,FALSE)]

## Create and join all points into one data frame ##
#NH3
y_NH3<-c(m_NH3_All_B1,m_NH3r_NH3_B1,m_NH3d_NH3_B1)
x_NH3<-c(m_B1_PTR,m_B1_PTR,m_B1_PTR)
sd_NH3<-c(sd_NH3_All_B1,sd_NH3r_NH3_B1,sd_NH3d_NH3_B1)
Color_NH3<-c(rep("NH3_G2508",12),rep("NH3_raw",12),rep("NH3_dry",12))
df_NH3_B1<-cbind.data.frame(x_NH3,y_NH3,sd_NH3,Color_NH3)
#CH4
y_CH4<-c(m_CH4_All_B1,m_CH4_BP_B1,m_CH4d_BP_B1)
x_CH4<-c(m_B1_PTR,m_B1_PTR,m_B1_PTR)
sd_CH4<-c(sd_CH4_All_B1,sd_CH4_BP_B1,sd_CH4d_BP_B1)
Color_CH4<-c(rep("CH4_G2508",12),rep("CH4",12),rep("CH4_dry",12))
df_CH4_B1<-cbind.data.frame(x_CH4,y_CH4,sd_CH4,Color_CH4)
#N2O
y_N2O<-c(m_N2O_All_B1)
x_N2O<-c(m_B1_PTR)
sd_N2O<-c(sd_N2O_All_B1)
Color_N2O<-c(rep("N2O_G2508",12))
df_N2O_B1<-cbind.data.frame(x_N2O,y_N2O,sd_N2O,Color_N2O)

##1-propanol
Ac0_PTR<-mean(P1_PTR[(400-110):400])
Ac1_PTR<-mean(P1_PTR[(1250-110):1250])
Ac2_PTR<-mean(P1_PTR[(1450-110):1450])
Ac3_PTR<-mean(P1_PTR[(1690-110):1690])
Ac4_PTR<-mean(P1_PTR[(1940-110):1940])
Ac5_PTR<-mean(P1_PTR[(2130-110):2130])
Ac6_PTR<-mean(P1_PTR[(2415-110):2415])
Ac7_PTR<-mean(P1_PTR[(2740-110):2740])
Ac8_PTR<-mean(P1_PTR[2940:3050])
Ac9_PTR<-mean(P1_PTR[3190:3300])
Ac10_PTR<-mean(P1_PTR[(3580-110):3580])
Ac11_PTR<-mean(P1_PTR[(3800-110):3800])
Ac12_PTR<-mean(P1_PTR[(4030-110):4030])
Ac13_PTR<-mean(P1_PTR[(4790-110):4790])

sd_Ac0_PTR<-sd(P1_PTR[(400-110):400])
sd_Ac1_PTR<-sd(P1_PTR[(1250-110):1250])
sd_Ac2_PTR<-sd(P1_PTR[(1450-110):1450])
sd_Ac3_PTR<-sd(P1_PTR[(1690-110):1690])
sd_Ac4_PTR<-sd(P1_PTR[(1940-110):1940])
sd_Ac5_PTR<-sd(P1_PTR[(2130-110):2130])
sd_Ac6_PTR<-sd(P1_PTR[(2415-110):2415])
sd_Ac7_PTR<-sd(P1_PTR[(2740-110):2740])
sd_Ac8_PTR<-sd(P1_PTR[2940:3050])
sd_Ac9_PTR<-sd(P1_PTR[3190:3300])
sd_Ac10_PTR<-sd(P1_PTR[(3580-110):3580])
sd_Ac11_PTR<-sd(P1_PTR[(3800-110):3800])
sd_Ac12_PTR<-sd(P1_PTR[(4030-110):4030])
sd_Ac13_PTR<-sd(P1_PTR[(4790-110):4790])

length_Ac0_PTR<-length(P1_PTR[(400-110):400])
length_Ac1_PTR<-length(P1_PTR[(1250-110):1250])
length_Ac2_PTR<-length(P1_PTR[(1450-110):1450])
length_Ac3_PTR<-length(P1_PTR[(1690-110):1690])
length_Ac4_PTR<-length(P1_PTR[(1940-110):1940])
length_Ac5_PTR<-length(P1_PTR[(2130-110):2130])
length_Ac6_PTR<-length(P1_PTR[(2415-110):2415])
length_Ac7_PTR<-length(P1_PTR[(2740-110):2740])
length_Ac8_PTR<-length(P1_PTR[2940:3050])
length_Ac9_PTR<-length(P1_PTR[3190:3300])
length_Ac10_PTR<-length(P1_PTR[(3580-110):3580])
length_Ac11_PTR<-length(P1_PTR[(3800-110):3800])
length_Ac12_PTR<-length(P1_PTR[(4030-110):4030])
length_Ac13_PTR<-length(P1_PTR[(4790-110):4790])

Tinterval_Ac0_PTR<-difftime(Time_PTR_P1[(400-110)],Time_PTR_P1[400])
Tinterval_Ac1_PTR<-difftime(Time_PTR_P1[(1250-110)],Time_PTR_P1[1250])
Tinterval_Ac2_PTR<-difftime(Time_PTR_P1[(1450-110)],Time_PTR_P1[1450])
Tinterval_Ac3_PTR<-difftime(Time_PTR_P1[(1690-110)],Time_PTR_P1[1690])
Tinterval_Ac4_PTR<-difftime(Time_PTR_P1[(1940-110)],Time_PTR_P1[1940])
Tinterval_Ac5_PTR<-difftime(Time_PTR_P1[(2130-110)],Time_PTR_P1[2130])
Tinterval_Ac6_PTR<-difftime(Time_PTR_P1[(2415-110)],Time_PTR_P1[2415])
Tinterval_Ac7_PTR<-difftime(Time_PTR_P1[(2740-110)],Time_PTR_P1[2740])
Tinterval_Ac8_PTR<-difftime(Time_PTR_P1[2940],Time_PTR_P1[3050])
Tinterval_Ac9_PTR<-difftime(Time_PTR_P1[3190],Time_PTR_P1[3300])
Tinterval_Ac10_PTR<-difftime(Time_PTR_P1[(3580-110)],Time_PTR_P1[3580])
Tinterval_Ac11_PTR<-difftime(Time_PTR_P1[(3800-110)],Time_PTR_P1[3800])
Tinterval_Ac12_PTR<-difftime(Time_PTR_P1[(4030-110)],Time_PTR_P1[4030])
Tinterval_Ac13_PTR<-difftime(Time_PTR_P1[(4790-110)],Time_PTR_P1[4790])

m_P1_PTR<-c(Ac0_PTR,Ac1_PTR,Ac2_PTR,Ac3_PTR,Ac4_PTR,Ac5_PTR,Ac6_PTR,Ac7_PTR,Ac8_PTR,Ac9_PTR,Ac10_PTR,Ac11_PTR,Ac12_PTR,Ac13_PTR)
sd_P1_PTR<-c(sd_Ac0_PTR,sd_Ac1_PTR,sd_Ac2_PTR,sd_Ac3_PTR,sd_Ac4_PTR,sd_Ac5_PTR,sd_Ac6_PTR,sd_Ac7_PTR,sd_Ac8_PTR,sd_Ac9_PTR,sd_Ac10_PTR,sd_Ac11_PTR,sd_Ac12_PTR,sd_Ac13_PTR)
length_P1_PTR<-c(length_Ac0_PTR,length_Ac1_PTR,length_Ac2_PTR,length_Ac3_PTR,length_Ac4_PTR,length_Ac5_PTR,length_Ac6_PTR,length_Ac7_PTR,length_Ac8_PTR,length_Ac9_PTR,length_Ac10_PTR,length_Ac11_PTR,length_Ac12_PTR,length_Ac13_PTR)
Tinterval_P1_PTR<-c(Tinterval_Ac0_PTR,Tinterval_Ac1_PTR,Tinterval_Ac2_PTR,Tinterval_Ac3_PTR,Tinterval_Ac4_PTR,Tinterval_Ac5_PTR,Tinterval_Ac6_PTR,Tinterval_Ac7_PTR,Tinterval_Ac8_PTR,Tinterval_Ac9_PTR,Tinterval_Ac10_PTR,Tinterval_Ac11_PTR,Tinterval_Ac12_PTR,Tinterval_Ac13_PTR)

#Select same time intervals in G2508 as in PTR-TOF
Ti_PTR<-round(as.numeric(Time_PTR_P1[c(400-110,400,1250-110,1250,1450-110,1450,1690-110,1690,1940-110,1940,2130-110,2130,2415-110,2415,2740-110,2740,2940,3050,3190,3300,3580-110,3580,3800-110,3800,4030-110,4030,4790-110,4790)]))
Ti_PTR<-Ti_PTR-as.numeric(seconds(115))
numericTime_All<-round(as.numeric(Time_All_P1))
Pos<-list()
min_dif<-1
for (i in 1:(length(Ti_PTR))){
  min_dif[i]<-min(abs(numericTime_All-Ti_PTR[i]))
  Pos[[i]]<-which(numericTime_All-Ti_PTR[i]==min_dif[i])
  Pos[[i]]<-Pos[[i]][1]
}
t<-unlist(Pos)
### For Publication--> save means for borrowed G2508 ###
write.table(t, file = "P1_G2509_intervals.txt", sep = "\t", row.names = TRUE, col.names = NA)
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
Tinterval_All_P1<-1

for (i in 1:(length(Ti_PTR)-1)){
  m_NH3_All[i]<-mean(NH3_All_P1[t[i]:t[i+1]])
  sd_NH3_All[i]<-sd(NH3_All_P1[t[i]:t[i+1]])
  length_All[i]<-length(NH3_All_P1[t[i]:t[i+1]])
  m_CH4_All[i]<-mean(CH4_All_P1[t[i]:t[i+1]])
  sd_CH4_All[i]<-sd(CH4_All_P1[t[i]:t[i+1]])
  m_N2O_All[i]<-mean(N2O_All_P1[t[i]:t[i+1]])
  sd_N2O_All[i]<-sd(N2O_All_P1[t[i]:t[i+1]])
  m_H2O_All[i]<-mean(H2O_All_P1[t[i]:t[i+1]])
  sd_H2O_All[i]<-sd(H2O_All_P1[t[i]:t[i+1]])
  Tinterval_All_P1[i]<-difftime(Time_All_P1[t[i]],Time_All_P1[t[i+1]])
}
#Only odd positions are correct (Even positions are from t(2)to t(3))
m_NH3_All_P1<-m_NH3_All[c(TRUE,FALSE)]
sd_NH3_All_P1<-sd_NH3_All[c(TRUE,FALSE)]
length_All_P1<-length_All[c(TRUE,FALSE)]
m_CH4_All_P1<-m_CH4_All[c(TRUE,FALSE)]
sd_CH4_All_P1<-sd_CH4_All[c(TRUE,FALSE)]
m_N2O_All_P1<-m_N2O_All[c(TRUE,FALSE)]
sd_N2O_All_P1<-sd_N2O_All[c(TRUE,FALSE)]
m_N2O_All_P1<-m_N2O_All[c(TRUE,FALSE)]
sd_N2O_All_P1<-sd_N2O_All[c(TRUE,FALSE)]
m_H2O_All_P1<-m_H2O_All[c(TRUE,FALSE)]
sd_H2O_All_P1<-sd_H2O_All[c(TRUE,FALSE)]
Tinterval_All_P1<-Tinterval_All_P1[c(TRUE,FALSE)]

#Select same time intervals in NH3 Picarro as in PTR-TOF
Ti_PTR<-round(as.numeric(Time_PTR_P1[c(400-110,400,1250-110,1250,1450-110,1450,1690-110,1690,1940-110,1940,2130-110,2130,2415-110,2415,2740-110,2740,2940,3050,3190,3300,3580-110,3580,3800-110,3800,4030-110,4030,4790-110,4790)]))
Ti_PTR<-(Ti_PTR-as.numeric(seconds(115)))-as.numeric(minutes(177))
numericTime_NH3<-round(as.numeric(Time_NH3_P1))
Pos<-list()
min_dif<-1
for (i in 1:(length(Ti_PTR))){
  min_dif[i]<-min(abs(numericTime_NH3-Ti_PTR[i]))
  Pos[[i]]<-which(abs(numericTime_NH3-Ti_PTR[i])==min_dif[i])
  Pos[[i]]<-Pos[[i]][1]
}
t<-unlist(Pos)
### For Publication--> save means for G2103 ###
write.table(t, file = "P1_G2103_intervals.txt", sep = "\t", row.names = TRUE, col.names = NA)
########################################################


## Put the calculated positions in G2103
m_NH3r_NH3<-1
sd_NH3r_NH3<-1
length_NH3<-1
m_NH3d_NH3<-1
sd_NH3d_NH3<-1
m_H2O_NH3<-1
sd_H2O_NH3<-1
Tinterval_NH3_P1<-1

for (i in 1:(length(Ti_PTR)-1)){
  m_NH3r_NH3[i]<-mean(NH3r_NH3_P1[t[i]:t[i+1]])
  sd_NH3r_NH3[i]<-sd(NH3r_NH3_P1[t[i]:t[i+1]])
  length_NH3[i]<-length(NH3r_NH3_P1[t[i]:t[i+1]])
  m_NH3d_NH3[i]<-mean(NH3d_NH3_P1[t[i]:t[i+1]])
  sd_NH3d_NH3[i]<-sd(NH3d_NH3_P1[t[i]:t[i+1]])
  m_H2O_NH3[i]<-mean(H2O_NH3_P1[t[i]:t[i+1]])
  sd_H2O_NH3[i]<-sd(H2O_NH3_P1[t[i]:t[i+1]])
  Tinterval_NH3_P1[i]<-difftime(Time_NH3_P1[t[i]],Time_NH3_P1[t[i+1]])
}
#Only odd positions are correct (Even positions are from t(2)to t(3))
m_NH3r_NH3_P1<-m_NH3r_NH3[c(TRUE,FALSE)]
sd_NH3r_NH3_P1<-sd_NH3r_NH3[c(TRUE,FALSE)]
length_NH3_P1<-length_NH3[c(TRUE,FALSE)]
m_NH3d_NH3_P1<-m_NH3d_NH3[c(TRUE,FALSE)]
sd_NH3d_NH3_P1<-sd_NH3d_NH3[c(TRUE,FALSE)]
m_H2O_NH3_P1<-m_H2O_NH3[c(TRUE,FALSE)]
sd_H2O_NH3_P1<-sd_H2O_NH3[c(TRUE,FALSE)]
Tinterval_NH3_P1<-Tinterval_NH3_P1[c(TRUE,FALSE)]

#Select same time intervals in BackPack as in PTR-TOF
Ti_PTR<-round(as.numeric(Time_PTR_P1[c(400-110,400,1250-110,1250,1450-110,1450,1690-110,1690,1940-110,1940,2130-110,2130,2415-110,2415,2740-110,2740,2940,3050,3190,3300,3580-110,3580,3800-110,3800,4030-110,4030,4790-110,4790)]))
Ti_PTR<-(Ti_PTR-as.numeric(seconds(115)))-as.numeric(minutes(119))
numericTime_BP<-round(as.numeric(Time_BP_P1))
Pos<-list()
min_dif<-1
for (i in 1:(length(Ti_PTR))){
  min_dif[i]<-min(abs(numericTime_BP-Ti_PTR[i]))
  Pos[[i]]<-which(numericTime_BP-Ti_PTR[i]==min_dif[i])
  Pos[[i]]<-Pos[[i]][1]
}
t<-unlist(Pos)
### For Publication--> save means for borrowed G4301 ###
write.table(t, file = "P1_G4301_intervals.txt", sep = "\t", row.names = TRUE, col.names = NA)
########################################################


## Put the calculated positions in G4301
m_CH4_BP<-1
sd_CH4_BP<-1
length_BP<-1
m_H2O_BP<-1
sd_H2O_BP<-1
m_CH4d_BP<-1
sd_CH4d_BP<-1
Tinterval_BP_P1<-1

for (i in 1:(length(Ti_PTR)-1)){
  m_CH4_BP[i]<-mean(CH4_BP_P1[t[i]:t[i+1]])
  sd_CH4_BP[i]<-sd(CH4_BP_P1[t[i]:t[i+1]])
  length_BP[i]<-length(CH4_BP_P1[t[i]:t[i+1]])
  m_CH4d_BP[i]<-mean(CH4d_BP_P1[t[i]:t[i+1]])
  sd_CH4d_BP[i]<-sd(CH4d_BP_P1[t[i]:t[i+1]])
  m_H2O_BP[i]<-mean(H2O_BP_P1[t[i]:t[i+1]])
  sd_H2O_BP[i]<-sd(H2O_BP_P1[t[i]:t[i+1]])
  Tinterval_BP_P1[i]<-difftime(Time_BP_P1[t[i]],Time_BP_P1[t[i+1]])
}
#Only odd positions are correct (Even positions are from t(2)to t(3))
m_CH4_BP_P1<-m_CH4_BP[c(TRUE,FALSE)]
sd_CH4_BP_P1<-sd_CH4_BP[c(TRUE,FALSE)]
length_BP_P1<-length_BP[c(TRUE,FALSE)]
m_CH4d_BP_P1<-m_CH4d_BP[c(TRUE,FALSE)]
sd_CH4d_BP_P1<-sd_CH4d_BP[c(TRUE,FALSE)]
m_H2O_BP_P1<-m_H2O_BP[c(TRUE,FALSE)]
sd_H2O_BP_P1<-sd_H2O_BP[c(TRUE,FALSE)]
Tinterval_BP_P1<-Tinterval_BP_P1[c(TRUE,FALSE)]


## Create and join all points into one data frame ##
#NH3
y_NH3<-c(m_NH3_All_P1,m_NH3r_NH3_P1,m_NH3d_NH3_P1)
x_NH3<-c(m_P1_PTR,m_P1_PTR,m_P1_PTR)
sd_NH3<-c(sd_NH3_All_P1,sd_NH3r_NH3_P1,sd_NH3d_NH3_P1)
Color_NH3<-c(rep("NH3_G2508",14),rep("NH3_raw",14),rep("NH3_dry",14))
df_NH3_P1<-cbind.data.frame(x_NH3,y_NH3,sd_NH3,Color_NH3)
#CH4
y_CH4<-c(m_CH4_All_P1,m_CH4_BP_P1,m_CH4d_BP_P1)
x_CH4<-c(m_P1_PTR,m_P1_PTR,m_P1_PTR)
sd_CH4<-c(sd_CH4_All_P1,sd_CH4_BP_P1,sd_CH4d_BP_P1)
Color_CH4<-c(rep("CH4_G2508",14),rep("CH4",14),rep("CH4_dry",14))
df_CH4_P1<-cbind.data.frame(x_CH4,y_CH4,sd_CH4,Color_CH4)
#N2O
y_N2O<-c(m_N2O_All_P1)
x_N2O<-c(m_P1_PTR)
sd_N2O<-c(sd_N2O_All_P1)
Color_N2O<-c(rep("N2O_G2508",14))
df_N2O_P1<-cbind.data.frame(x_N2O,y_N2O,sd_N2O,Color_N2O)

##Acetaldehyde
Ac0_PTR<-mean(AA_PTR[(550-118):550])
Ac1_PTR<-mean(AA_PTR[(1050-118):1050])
Ac2_PTR<-mean(AA_PTR[(1260-118):1260])
Ac3_PTR<-mean(AA_PTR[(1550-118):1550])
Ac4_PTR<-mean(AA_PTR[(1760-118):1760])
Ac5_PTR<-mean(AA_PTR[(1940-118):1940])
Ac6_PTR<-mean(AA_PTR[1970:2088])
Ac7_PTR<-mean(AA_PTR[(2280-118):2280])
Ac8_PTR<-mean(AA_PTR[(2700-118):2700])
Ac9_PTR<-mean(AA_PTR[(3760-118):3760])
Ac10_PTR<-mean(AA_PTR[(3980-118):3980])
Ac11_PTR<-mean(AA_PTR[(4360-118):4360])
Ac12_PTR<-mean(AA_PTR[(4546-118):4546])

sd_Ac0_PTR<-sd(AA_PTR[(550-118):550])
sd_Ac1_PTR<-sd(AA_PTR[(1050-118):1050])
sd_Ac2_PTR<-sd(AA_PTR[(1260-118):1260])
sd_Ac3_PTR<-sd(AA_PTR[(1550-118):1550])
sd_Ac4_PTR<-sd(AA_PTR[(1760-118):1760])
sd_Ac5_PTR<-sd(AA_PTR[(1940-118):1940])
sd_Ac6_PTR<-sd(AA_PTR[1970:2088])
sd_Ac7_PTR<-sd(AA_PTR[(2280-118):2280])
sd_Ac8_PTR<-sd(AA_PTR[(2700-118):2700])
sd_Ac9_PTR<-sd(AA_PTR[(3760-118):3760])
sd_Ac10_PTR<-sd(AA_PTR[(3980-118):3980])
sd_Ac11_PTR<-sd(AA_PTR[(4360-118):4360])
sd_Ac12_PTR<-sd(AA_PTR[(4546-118):4546])

length_Ac0_PTR<-length(AA_PTR[(550-118):550])
length_Ac1_PTR<-length(AA_PTR[(1050-118):1050])
length_Ac2_PTR<-length(AA_PTR[(1260-118):1260])
length_Ac3_PTR<-length(AA_PTR[(1550-118):1550])
length_Ac4_PTR<-length(AA_PTR[(1760-118):1760])
length_Ac5_PTR<-length(AA_PTR[(1940-118):1940])
length_Ac6_PTR<-length(AA_PTR[1970:2088])
length_Ac7_PTR<-length(AA_PTR[(2280-118):2280])
length_Ac8_PTR<-length(AA_PTR[(2700-118):2700])
length_Ac9_PTR<-length(AA_PTR[(3760-118):3760])
length_Ac10_PTR<-length(AA_PTR[(3980-118):3980])
length_Ac11_PTR<-length(AA_PTR[(4360-118):4360])
length_Ac12_PTR<-length(AA_PTR[(4546-118):4546])

Tinterval_Ac0_PTR<-difftime(Time_PTR_AA[(550-118)],Time_PTR_AA[550])
Tinterval_Ac1_PTR<-difftime(Time_PTR_AA[(1050-118)],Time_PTR_AA[1050])
Tinterval_Ac2_PTR<-difftime(Time_PTR_AA[(1260-118)],Time_PTR_AA[1260])
Tinterval_Ac3_PTR<-difftime(Time_PTR_AA[(1550-118)],Time_PTR_AA[1550])
Tinterval_Ac4_PTR<-difftime(Time_PTR_AA[(1760-118)],Time_PTR_AA[1760])
Tinterval_Ac5_PTR<-difftime(Time_PTR_AA[(1940-118)],Time_PTR_AA[1940])
Tinterval_Ac6_PTR<-difftime(Time_PTR_AA[1970],Time_PTR_AA[2088])
Tinterval_Ac7_PTR<-difftime(Time_PTR_AA[(2280-118)],Time_PTR_AA[2280])
Tinterval_Ac8_PTR<-difftime(Time_PTR_AA[(2700-118)],Time_PTR_AA[2700])
Tinterval_Ac9_PTR<-difftime(Time_PTR_AA[(3760-118)],Time_PTR_AA[3760])
Tinterval_Ac10_PTR<-difftime(Time_PTR_AA[(3980-118)],Time_PTR_AA[3980])
Tinterval_Ac11_PTR<-difftime(Time_PTR_AA[(4360-118)],Time_PTR_AA[4360])
Tinterval_Ac12_PTR<-difftime(Time_PTR_AA[(4546-118)],Time_PTR_AA[4546])


m_AA_PTR<-c(Ac0_PTR,Ac1_PTR,Ac2_PTR,Ac3_PTR,Ac4_PTR,Ac5_PTR,Ac6_PTR,Ac7_PTR,Ac8_PTR,Ac9_PTR,Ac10_PTR,Ac11_PTR,Ac12_PTR)
sd_AA_PTR<-c(sd_Ac0_PTR,sd_Ac1_PTR,sd_Ac2_PTR,sd_Ac3_PTR,sd_Ac4_PTR,sd_Ac5_PTR,sd_Ac6_PTR,sd_Ac7_PTR,sd_Ac8_PTR,sd_Ac9_PTR,sd_Ac10_PTR,sd_Ac11_PTR,sd_Ac12_PTR)
length_AA_PTR<-c(length_Ac0_PTR,length_Ac1_PTR,length_Ac2_PTR,length_Ac3_PTR,length_Ac4_PTR,length_Ac5_PTR,length_Ac6_PTR,length_Ac7_PTR,length_Ac8_PTR,length_Ac9_PTR,length_Ac10_PTR,length_Ac11_PTR,length_Ac12_PTR)
Tinterval_AA_PTR<-c(Tinterval_Ac0_PTR,Tinterval_Ac1_PTR,Tinterval_Ac2_PTR,Tinterval_Ac3_PTR,Tinterval_Ac4_PTR,Tinterval_Ac5_PTR,Tinterval_Ac6_PTR,Tinterval_Ac7_PTR,Tinterval_Ac8_PTR,Tinterval_Ac9_PTR,Tinterval_Ac10_PTR,Tinterval_Ac11_PTR,Tinterval_Ac12_PTR)

#Select same time intervals in G2508 as in PTR-TOF
Ti_PTR<-round(as.numeric(Time_PTR_AA[c(550-118,550,1050-118,1050,1260-118,1260,1550-118,1550,1760-118,1760,1940-118,1940,1970,2080,2280-118,2288,2700-118,2700,3760-118,3760,3980-118,3980,4360-118,4360,4546-118,4546)]))
Ti_PTR<-Ti_PTR-as.numeric(seconds(115))
numericTime_All<-round(as.numeric(Time_All_AA))
Pos<-list()
min_dif<-1
for (i in 1:(length(Ti_PTR))){
  min_dif[i]<-min(abs(numericTime_All-Ti_PTR[i]))
  Pos[[i]]<-which(numericTime_All-Ti_PTR[i]==min_dif[i])
  Pos[[i]]<-Pos[[i]][1]
}
t<-unlist(Pos)
### For Publication--> save means for borrowed G2508 ###
write.table(t, file = "AA_G2509_intervals.txt", sep = "\t", row.names = TRUE, col.names = NA)
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
Tinterval_All_AA<-1

for (i in 1:(length(Ti_PTR)-1)){
  m_NH3_All[i]<-mean(NH3_All_AA[t[i]:t[i+1]])
  sd_NH3_All[i]<-sd(NH3_All_AA[t[i]:t[i+1]])
  length_All[i]<-length(NH3_All_AA[t[i]:t[i+1]])
  m_CH4_All[i]<-mean(CH4_All_AA[t[i]:t[i+1]])
  sd_CH4_All[i]<-sd(CH4_All_AA[t[i]:t[i+1]])
  m_N2O_All[i]<-mean(N2O_All_AA[t[i]:t[i+1]])
  sd_N2O_All[i]<-sd(N2O_All_AA[t[i]:t[i+1]])
  m_H2O_All[i]<-mean(H2O_All_AA[t[i]:t[i+1]])
  sd_H2O_All[i]<-sd(H2O_All_AA[t[i]:t[i+1]])
  Tinterval_All_AA[i]<-difftime(Time_All_AA[t[i]],Time_All_AA[t[i+1]])
}
#Only odd positions are correct (Even positions are from t(2)to t(3))
m_NH3_All_AA<-m_NH3_All[c(TRUE,FALSE)]
sd_NH3_All_AA<-sd_NH3_All[c(TRUE,FALSE)]
length_All_AA<-length_All[c(TRUE,FALSE)]
m_CH4_All_AA<-m_CH4_All[c(TRUE,FALSE)]
sd_CH4_All_AA<-sd_CH4_All[c(TRUE,FALSE)]
m_N2O_All_AA<-m_N2O_All[c(TRUE,FALSE)]
sd_N2O_All_AA<-sd_N2O_All[c(TRUE,FALSE)]
m_N2O_All_AA<-m_N2O_All[c(TRUE,FALSE)]
sd_N2O_All_AA<-sd_N2O_All[c(TRUE,FALSE)]
m_H2O_All_AA<-m_H2O_All[c(TRUE,FALSE)]
sd_H2O_All_AA<-sd_H2O_All[c(TRUE,FALSE)]
Tinterval_All_AA<-Tinterval_All_AA[c(TRUE,FALSE)]

#Select same time intervals in NH3 Picarro as in PTR-TOF
Ti_PTR<-round(as.numeric(Time_PTR_AA[c(550-118,550,1050-118,1050,1260-118,1260,1550-118,1550,1760-118,1760,1940-118,1940,1970,2080,2280-118,2288,2700-118,2700,3760-118,3760,3980-118,3980,4360-118,4360,4546-118,4546)]))
Ti_PTR<-(Ti_PTR-as.numeric(seconds(115)))-as.numeric(minutes(177))
numericTime_NH3<-round(as.numeric(Time_NH3_AA))
Pos<-list()
min_dif<-1
for (i in 1:(length(Ti_PTR))){
  min_dif[i]<-min(abs(numericTime_NH3-Ti_PTR[i]))
  Pos[[i]]<-which(abs(numericTime_NH3-Ti_PTR[i])==min_dif[i])
  Pos[[i]]<-Pos[[i]][1]
}
t<-unlist(Pos)
### For Publication--> save means for G2103 ###
write.table(t, file = "AA_G2103_intervals.txt", sep = "\t", row.names = TRUE, col.names = NA)
########################################################


## Put the calculated positions in G2103
m_NH3r_NH3<-1
sd_NH3r_NH3<-1
length_NH3<-1
m_NH3d_NH3<-1
sd_NH3d_NH3<-1
m_H2O_NH3<-1
sd_H2O_NH3<-1
Tinterval_NH3_AA<-1

for (i in 1:(length(Ti_PTR)-1)){
  m_NH3r_NH3[i]<-mean(NH3r_NH3_AA[t[i]:t[i+1]])
  sd_NH3r_NH3[i]<-sd(NH3r_NH3_AA[t[i]:t[i+1]])
  length_NH3[i]<-length(NH3r_NH3_AA[t[i]:t[i+1]])
  m_NH3d_NH3[i]<-mean(NH3d_NH3_AA[t[i]:t[i+1]])
  sd_NH3d_NH3[i]<-sd(NH3d_NH3_AA[t[i]:t[i+1]])
  m_H2O_NH3[i]<-mean(H2O_NH3_AA[t[i]:t[i+1]])
  sd_H2O_NH3[i]<-sd(H2O_NH3_AA[t[i]:t[i+1]])
  Tinterval_NH3_AA[i]<-difftime(Time_NH3_AA[t[i]],Time_NH3_AA[t[i+1]])
}
#Only odd positions are correct (Even positions are from t(2)to t(3))
m_NH3r_NH3_AA<-m_NH3r_NH3[c(TRUE,FALSE)]
sd_NH3r_NH3_AA<-sd_NH3r_NH3[c(TRUE,FALSE)]
length_NH3_AA<-length_NH3[c(TRUE,FALSE)]
m_NH3d_NH3_AA<-m_NH3d_NH3[c(TRUE,FALSE)]
sd_NH3d_NH3_AA<-sd_NH3d_NH3[c(TRUE,FALSE)]
m_H2O_NH3_AA<-m_H2O_NH3[c(TRUE,FALSE)]
sd_H2O_NH3_AA<-sd_H2O_NH3[c(TRUE,FALSE)]
Tinterval_NH3_AA<-Tinterval_NH3_AA[c(TRUE,FALSE)]

#Select same time intervals in BackPack as in PTR-TOF
Ti_PTR<-round(as.numeric(Time_PTR_AA[c(550-118,550,1050-118,1050,1260-118,1260,1550-118,1550,1760-118,1760,1940-118,1940,1970,2080,2280-118,2288,2700-118,2700,3760-118,3760,3980-118,3980,4360-118,4360,4546-118,4546)]))
Ti_PTR<-(Ti_PTR-as.numeric(seconds(115)))-as.numeric(minutes(119))
numericTime_BP<-round(as.numeric(Time_BP_AA))
Pos<-list()
min_dif<-1
for (i in 1:(length(Ti_PTR))){
  min_dif[i]<-min(abs(numericTime_BP-Ti_PTR[i]))
  Pos[[i]]<-which(numericTime_BP-Ti_PTR[i]==min_dif[i])
  Pos[[i]]<-Pos[[i]][1]
}
t<-unlist(Pos)
### For Publication--> save means for G4301 ###
write.table(t, file = "AA_G4301_intervals.txt", sep = "\t", row.names = TRUE, col.names = NA)
########################################################


## Put the calculated positions in G4301
m_CH4_BP<-1
sd_CH4_BP<-1
length_BP<-1
m_H2O_BP<-1
sd_H2O_BP<-1
m_CH4d_BP<-1
sd_CH4d_BP<-1
Tinterval_BP_AA<-1

for (i in 1:(length(Ti_PTR)-1)){
  m_CH4_BP[i]<-mean(CH4_BP_AA[t[i]:t[i+1]])
  sd_CH4_BP[i]<-sd(CH4_BP_AA[t[i]:t[i+1]])
  length_BP[i]<-length(CH4_BP_AA[t[i]:t[i+1]])
  m_CH4d_BP[i]<-mean(CH4d_BP_AA[t[i]:t[i+1]])
  sd_CH4d_BP[i]<-sd(CH4d_BP_AA[t[i]:t[i+1]])
  m_H2O_BP[i]<-mean(H2O_BP_AA[t[i]:t[i+1]])
  sd_H2O_BP[i]<-sd(H2O_BP_AA[t[i]:t[i+1]])
  Tinterval_BP_AA[i]<-difftime(Time_BP_AA[t[i]],Time_BP_AA[t[i+1]])
}
#Only odd positions are correct (Even positions are from t(2)to t(3))
m_CH4_BP_AA<-m_CH4_BP[c(TRUE,FALSE)]
sd_CH4_BP_AA<-sd_CH4_BP[c(TRUE,FALSE)]
length_BP_AA<-length_BP[c(TRUE,FALSE)]
m_CH4d_BP_AA<-m_CH4d_BP[c(TRUE,FALSE)]
sd_CH4d_BP_AA<-sd_CH4d_BP[c(TRUE,FALSE)]
m_H2O_BP_AA<-m_H2O_BP[c(TRUE,FALSE)]
sd_H2O_BP_AA<-sd_H2O_BP[c(TRUE,FALSE)]
Tinterval_BP_AA<-Tinterval_BP_AA[c(TRUE,FALSE)]

## Create and join all points into one data frame ##
#NH3
y_NH3<-c(m_NH3_All_AA,m_NH3r_NH3_AA,m_NH3d_NH3_AA)
x_NH3<-c(m_AA_PTR,m_AA_PTR,m_AA_PTR)
sd_NH3<-c(sd_NH3_All_AA,sd_NH3r_NH3_AA,sd_NH3d_NH3_AA)
Color_NH3<-c(rep("NH3_G2508",13),rep("NH3_raw",13),rep("NH3_dry",13))
df_NH3_AA<-cbind.data.frame(x_NH3,y_NH3,sd_NH3,Color_NH3)
#CH4
y_CH4<-c(m_CH4_All_AA,m_CH4_BP_AA,m_CH4d_BP_AA)
x_CH4<-c(m_AA_PTR,m_AA_PTR,m_AA_PTR)
sd_CH4<-c(sd_CH4_All_AA,sd_CH4_BP_AA,sd_CH4d_BP_AA)
Color_CH4<-c(rep("CH4_G2508",13),rep("CH4",13),rep("CH4_dry",13))
df_CH4_AA<-cbind.data.frame(x_CH4,y_CH4,sd_CH4,Color_CH4)
#N2O
y_N2O<-c(m_N2O_All_AA)
x_N2O<-c(m_AA_PTR)
sd_N2O<-c(sd_N2O_All_AA)
Color_N2O<-c(rep("N2O_G2508",13))
df_N2O_AA<-cbind.data.frame(x_N2O,y_N2O,sd_N2O,Color_N2O)

##Butanone
Ac0_PTR<-mean(Bne_PTR[(700-76):700])
Ac1_PTR<-mean(Bne_PTR[(935-76):935])
Ac2_PTR<-mean(Bne_PTR[(1190-76):1190])
Ac3_PTR<-mean(Bne_PTR[1220:1296])
Ac4_PTR<-mean(Bne_PTR[(1410-76):1410])
Ac5_PTR<-mean(Bne_PTR[(1560-76):1560])
Ac6_PTR<-mean(Bne_PTR[(2200-76):2200])
Ac7_PTR<-mean(Bne_PTR[(2430-76):2430])
Ac8_PTR<-mean(Bne_PTR[(3330-76):3330])
Ac9_PTR<-mean(Bne_PTR[(4754-76):4754])

sd_Ac0_PTR<-sd(Bne_PTR[(700-76):700])
sd_Ac1_PTR<-sd(Bne_PTR[(935-76):935])
sd_Ac2_PTR<-sd(Bne_PTR[(1190-76):1190])
sd_Ac3_PTR<-sd(Bne_PTR[1220:1296])
sd_Ac4_PTR<-sd(Bne_PTR[(1410-76):1410])
sd_Ac5_PTR<-sd(Bne_PTR[(1560-76):1560])
sd_Ac6_PTR<-sd(Bne_PTR[(2200-76):2200])
sd_Ac7_PTR<-sd(Bne_PTR[(2430-76):2430])
sd_Ac8_PTR<-sd(Bne_PTR[(3330-76):3330])
sd_Ac9_PTR<-sd(Bne_PTR[(4754-76):4754])

length_Ac0_PTR<-length(Bne_PTR[(700-76):700])
length_Ac1_PTR<-length(Bne_PTR[(935-76):935])
length_Ac2_PTR<-length(Bne_PTR[(1190-76):1190])
length_Ac3_PTR<-length(Bne_PTR[1220:1296])
length_Ac4_PTR<-length(Bne_PTR[(1410-76):1410])
length_Ac5_PTR<-length(Bne_PTR[(1560-76):1560])
length_Ac6_PTR<-length(Bne_PTR[(2200-76):2200])
length_Ac7_PTR<-length(Bne_PTR[(2430-76):2430])
length_Ac8_PTR<-length(Bne_PTR[(3330-76):3330])
length_Ac9_PTR<-length(Bne_PTR[(4754-76):4754])

Tinterval_Ac0_PTR<-difftime(Time_PTR_Bne[(700-76)],Time_PTR_Bne[700])
Tinterval_Ac1_PTR<-difftime(Time_PTR_Bne[(935-76)],Time_PTR_Bne[935])
Tinterval_Ac2_PTR<-difftime(Time_PTR_Bne[(1190-76)],Time_PTR_Bne[1190])
Tinterval_Ac3_PTR<-difftime(Time_PTR_Bne[1220],Time_PTR_Bne[1296])
Tinterval_Ac4_PTR<-difftime(Time_PTR_Bne[(1410-76)],Time_PTR_Bne[1410])
Tinterval_Ac5_PTR<-difftime(Time_PTR_Bne[(1560-76)],Time_PTR_Bne[1560])
Tinterval_Ac6_PTR<-difftime(Time_PTR_Bne[(2200-76)],Time_PTR_Bne[2200])
Tinterval_Ac7_PTR<-difftime(Time_PTR_Bne[(2430-76)],Time_PTR_Bne[2430])
Tinterval_Ac8_PTR<-difftime(Time_PTR_Bne[(3330-76)],Time_PTR_Bne[3330])
Tinterval_Ac9_PTR<-difftime(Time_PTR_Bne[(4754-76)],Time_PTR_Bne[4754])


m_Bne_PTR<-c(Ac0_PTR,Ac1_PTR,Ac2_PTR,Ac3_PTR,Ac4_PTR,Ac5_PTR,Ac6_PTR,Ac7_PTR,Ac8_PTR,Ac9_PTR)
sd_Bne_PTR<-c(sd_Ac0_PTR,sd_Ac1_PTR,sd_Ac2_PTR,sd_Ac3_PTR,sd_Ac4_PTR,sd_Ac5_PTR,sd_Ac6_PTR,sd_Ac7_PTR,sd_Ac8_PTR,sd_Ac9_PTR)
length_Bne_PTR<-c(length_Ac0_PTR,length_Ac1_PTR,length_Ac2_PTR,length_Ac3_PTR,length_Ac4_PTR,length_Ac5_PTR,length_Ac6_PTR,length_Ac7_PTR,length_Ac8_PTR,length_Ac9_PTR)
Tinterval_Bne_PTR<-c(Tinterval_Ac0_PTR,Tinterval_Ac1_PTR,Tinterval_Ac2_PTR,Tinterval_Ac3_PTR,Tinterval_Ac4_PTR,Tinterval_Ac5_PTR,Tinterval_Ac6_PTR,Tinterval_Ac7_PTR,Tinterval_Ac8_PTR,Tinterval_Ac9_PTR)

#Select same time intervals in G2508 as in PTR-TOF
Ti_PTR<-round(as.numeric(Time_PTR_Bne[c(700-76,700,935-76,935,1190-76,1190,1220,1296,1410-76,1410,1560-76,1560,2200-76,2200,2430-76,2430,3330-76,3330,4754-76,4754)]))
Ti_PTR<-Ti_PTR-as.numeric(seconds(90))
numericTime_All<-round(as.numeric(Time_All_Bne))
Pos<-list()
min_dif<-1
for (i in 1:(length(Ti_PTR))){
  min_dif[i]<-min(abs(numericTime_All-Ti_PTR[i]))
  Pos[[i]]<-which(numericTime_All-Ti_PTR[i]==min_dif[i])
  Pos[[i]]<-Pos[[i]][1]
}
t<-unlist(Pos)
### For Publication--> save means for borrowed G2508 ###
write.table(t, file = "Bne_G2509_intervals.txt", sep = "\t", row.names = TRUE, col.names = NA)
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
Tinterval_All_Bne<-1

for (i in 1:(length(Ti_PTR)-1)){
  m_NH3_All[i]<-mean(NH3_All_Bne[t[i]:t[i+1]])
  sd_NH3_All[i]<-sd(NH3_All_Bne[t[i]:t[i+1]])
  length_All[i]<-length(NH3_All_Bne[t[i]:t[i+1]])
  m_CH4_All[i]<-mean(CH4_All_Bne[t[i]:t[i+1]])
  sd_CH4_All[i]<-sd(CH4_All_Bne[t[i]:t[i+1]])
  m_N2O_All[i]<-mean(N2O_All_Bne[t[i]:t[i+1]])
  sd_N2O_All[i]<-sd(N2O_All_Bne[t[i]:t[i+1]])
  m_H2O_All[i]<-mean(H2O_All_Bne[t[i]:t[i+1]])
  sd_H2O_All[i]<-sd(H2O_All_Bne[t[i]:t[i+1]])
  Tinterval_All_Bne[i]<-difftime(Time_All_Bne[t[i]],Time_All_Bne[t[i+1]])
}
#Only odd positions are correct (Even positions are from t(2)to t(3))
m_NH3_All_Bne<-m_NH3_All[c(TRUE,FALSE)]
sd_NH3_All_Bne<-sd_NH3_All[c(TRUE,FALSE)]
length_All_Bne<-length_All[c(TRUE,FALSE)]
m_CH4_All_Bne<-m_CH4_All[c(TRUE,FALSE)]
sd_CH4_All_Bne<-sd_CH4_All[c(TRUE,FALSE)]
m_N2O_All_Bne<-m_N2O_All[c(TRUE,FALSE)]
sd_N2O_All_Bne<-sd_N2O_All[c(TRUE,FALSE)]
m_N2O_All_Bne<-m_N2O_All[c(TRUE,FALSE)]
sd_N2O_All_Bne<-sd_N2O_All[c(TRUE,FALSE)]
m_H2O_All_Bne<-m_H2O_All[c(TRUE,FALSE)]
sd_H2O_All_Bne<-sd_H2O_All[c(TRUE,FALSE)]
Tinterval_All_Bne<-Tinterval_All_Bne[c(TRUE,FALSE)]

#Select same time intervals in NH3 Picarro as in PTR-TOF
Ti_PTR<-round(as.numeric(Time_PTR_Bne[c(700-76,700,935-76,935,1190-76,1190,1220,1296,1410-76,1410,1560-76,1560,2200-76,2200,2430-76,2430,3330-76,3330,4754-76,4754)]))
Ti_PTR<-(Ti_PTR-as.numeric(seconds(90)))-as.numeric(minutes(177))
numericTime_NH3<-round(as.numeric(Time_NH3_Bne))
Pos<-list()
min_dif<-1
for (i in 1:(length(Ti_PTR))){
  min_dif[i]<-min(abs(numericTime_NH3-Ti_PTR[i]))
  Pos[[i]]<-which(abs(numericTime_NH3-Ti_PTR[i])==min_dif[i])
  Pos[[i]]<-Pos[[i]][1]
}
t<-unlist(Pos)
### For Publication--> save means for borrowed G2103 ###
write.table(t, file = "Bne_G2103_intervals.txt", sep = "\t", row.names = TRUE, col.names = NA)
########################################################

## Put the calculated positions in G2103
m_NH3r_NH3<-1
sd_NH3r_NH3<-1
length_NH3<-1
m_NH3d_NH3<-1
sd_NH3d_NH3<-1
m_H2O_NH3<-1
sd_H2O_NH3<-1
Tinterval_NH3_Bne<-1

for (i in 1:(length(Ti_PTR)-1)){
  m_NH3r_NH3[i]<-mean(NH3r_NH3_Bne[t[i]:t[i+1]])
  sd_NH3r_NH3[i]<-sd(NH3r_NH3_Bne[t[i]:t[i+1]])
  length_NH3[i]<-length(NH3r_NH3_Bne[t[i]:t[i+1]])
  m_NH3d_NH3[i]<-mean(NH3d_NH3_Bne[t[i]:t[i+1]])
  sd_NH3d_NH3[i]<-sd(NH3d_NH3_Bne[t[i]:t[i+1]])
  m_H2O_NH3[i]<-mean(H2O_NH3_Bne[t[i]:t[i+1]])
  sd_H2O_NH3[i]<-sd(H2O_NH3_Bne[t[i]:t[i+1]])
  Tinterval_NH3_Bne[i]<-difftime(Time_NH3_Bne[t[i]],Time_NH3_Bne[t[i+1]])
}
#Only odd positions are correct (Even positions are from t(2)to t(3))
m_NH3r_NH3_Bne<-m_NH3r_NH3[c(TRUE,FALSE)]
sd_NH3r_NH3_Bne<-sd_NH3r_NH3[c(TRUE,FALSE)]
length_NH3_Bne<-length_NH3[c(TRUE,FALSE)]
m_NH3d_NH3_Bne<-m_NH3d_NH3[c(TRUE,FALSE)]
sd_NH3d_NH3_Bne<-sd_NH3d_NH3[c(TRUE,FALSE)]
m_H2O_NH3_Bne<-m_H2O_NH3[c(TRUE,FALSE)]
sd_H2O_NH3_Bne<-sd_H2O_NH3[c(TRUE,FALSE)]
Tinterval_NH3_Bne<-Tinterval_NH3_Bne[c(TRUE,FALSE)]

#Select same time intervals in BackPack as in PTR-TOF
Ti_PTR<-round(as.numeric(Time_PTR_Bne[c(700-76,700,935-76,935,1190-76,1190,1220,1296,1410-76,1410,1560-76,1560,2200-76,2200,2430-76,2430,3330-76,3330,4754-76,4754)]))
Ti_PTR<-(Ti_PTR-as.numeric(seconds(90)))-as.numeric(minutes(119))
numericTime_BP<-round(as.numeric(Time_BP_Bne))
Pos<-list()
min_dif<-1
for (i in 1:(length(Ti_PTR))){
  min_dif[i]<-min(abs(numericTime_BP-Ti_PTR[i]))
  Pos[[i]]<-which(numericTime_BP-Ti_PTR[i]==min_dif[i])
  Pos[[i]]<-Pos[[i]][1]
}
t<-unlist(Pos)
### For Publication--> save means for borrowed G4301 ###
write.table(t, file = "Bne_G4301_intervals.txt", sep = "\t", row.names = TRUE, col.names = NA)
########################################################

## Put the calculated positions in G4301
m_CH4_BP<-1
sd_CH4_BP<-1
length_BP<-1
m_H2O_BP<-1
sd_H2O_BP<-1
m_CH4d_BP<-1
sd_CH4d_BP<-1
Tinterval_BP_Bne<-1

for (i in 1:(length(Ti_PTR)-1)){
  m_CH4_BP[i]<-mean(CH4_BP_Bne[t[i]:t[i+1]])
  sd_CH4_BP[i]<-sd(CH4_BP_Bne[t[i]:t[i+1]])
  length_BP[i]<-length(CH4_BP_Bne[t[i]:t[i+1]])
  m_CH4d_BP[i]<-mean(CH4d_BP_Bne[t[i]:t[i+1]])
  sd_CH4d_BP[i]<-sd(CH4d_BP_Bne[t[i]:t[i+1]])
  m_H2O_BP[i]<-mean(H2O_BP_Bne[t[i]:t[i+1]])
  sd_H2O_BP[i]<-sd(H2O_BP_Bne[t[i]:t[i+1]])
  Tinterval_BP_Bne[i]<-difftime(Time_BP_Bne[t[i]],Time_BP_Bne[t[i+1]])
}
#Only odd positions are correct (Even positions are from t(2)to t(3))
m_CH4_BP_Bne<-m_CH4_BP[c(TRUE,FALSE)]
sd_CH4_BP_Bne<-sd_CH4_BP[c(TRUE,FALSE)]
length_BP_Bne<-length_BP[c(TRUE,FALSE)]
m_CH4d_BP_Bne<-m_CH4d_BP[c(TRUE,FALSE)]
sd_CH4d_BP_Bne<-sd_CH4d_BP[c(TRUE,FALSE)]
m_H2O_BP_Bne<-m_H2O_BP[c(TRUE,FALSE)]
sd_H2O_BP_Bne<-sd_H2O_BP[c(TRUE,FALSE)]
Tinterval_BP_Bne<-Tinterval_BP_Bne[c(TRUE,FALSE)]

## Create and join all points into one data frame ##
#NH3
y_NH3<-c(m_NH3_All_Bne,m_NH3r_NH3_Bne,m_NH3d_NH3_Bne)
x_NH3<-c(m_Bne_PTR,m_Bne_PTR,m_Bne_PTR)
sd_NH3<-c(sd_NH3_All_Bne,sd_NH3r_NH3_Bne,sd_NH3d_NH3_Bne)
Color_NH3<-c(rep("NH3_G2508",10),rep("NH3_raw",10),rep("NH3_dry",10))
df_NH3_Bne<-cbind.data.frame(x_NH3,y_NH3,sd_NH3,Color_NH3)
#CH4
y_CH4<-c(m_CH4_All_Bne,m_CH4_BP_Bne,m_CH4d_BP_Bne)
x_CH4<-c(m_Bne_PTR,m_Bne_PTR,m_Bne_PTR)
sd_CH4<-c(sd_CH4_All_Bne,sd_CH4_BP_Bne,sd_CH4d_BP_Bne)
Color_CH4<-c(rep("CH4_G2508",10),rep("CH4",10),rep("CH4_dry",10))
df_CH4_Bne<-cbind.data.frame(x_CH4,y_CH4,sd_CH4,Color_CH4)
#N2O
y_N2O<-c(m_N2O_All_Bne)
x_N2O<-c(m_Bne_PTR)
sd_N2O<-c(sd_N2O_All_Bne)
Color_N2O<-c(rep("N2O_G2508",10))
df_N2O_Bne<-cbind.data.frame(x_N2O,y_N2O,sd_N2O,Color_N2O)

##Diacetyl
Ac0_PTR<-mean(DA_PTR[(340-130):340])
Ac1_PTR<-mean(DA_PTR[(556-130):556])
Ac2_PTR<-mean(DA_PTR[570:700])
Ac3_PTR<-mean(DA_PTR[(1000-130):1000])
Ac4_PTR<-mean(DA_PTR[(1220-130):1220])
Ac5_PTR<-mean(DA_PTR[(1390-130):1390])
Ac6_PTR<-mean(DA_PTR[(1540-130):1540])
Ac7_PTR<-mean(DA_PTR[(1770-130):1770])
Ac8_PTR<-mean(DA_PTR[(1990-130):1990])
Ac9_PTR<-mean(DA_PTR[(2550-130):2550])
Ac10_PTR<-mean(DA_PTR[(2820-130):2820])
Ac11_PTR<-mean(DA_PTR[(3230-130):3230])

sd_Ac0_PTR<-sd(DA_PTR[(340-130):340])
sd_Ac1_PTR<-sd(DA_PTR[(556-130):556])
sd_Ac2_PTR<-sd(DA_PTR[570:700])
sd_Ac3_PTR<-sd(DA_PTR[(1000-130):1000])
sd_Ac4_PTR<-sd(DA_PTR[(1220-130):1220])
sd_Ac5_PTR<-sd(DA_PTR[(1390-130):1390])
sd_Ac6_PTR<-sd(DA_PTR[(1540-130):1540])
sd_Ac7_PTR<-sd(DA_PTR[(1770-130):1770])
sd_Ac8_PTR<-sd(DA_PTR[(1990-130):1990])
sd_Ac9_PTR<-sd(DA_PTR[(2550-130):2550])
sd_Ac10_PTR<-sd(DA_PTR[(2820-130):2820])
sd_Ac11_PTR<-sd(DA_PTR[(3230-130):3230])

length_Ac0_PTR<-length(DA_PTR[(340-130):340])
length_Ac1_PTR<-length(DA_PTR[(556-130):556])
length_Ac2_PTR<-length(DA_PTR[570:700])
length_Ac3_PTR<-length(DA_PTR[(1000-130):1000])
length_Ac4_PTR<-length(DA_PTR[(1220-130):1220])
length_Ac5_PTR<-length(DA_PTR[(1390-130):1390])
length_Ac6_PTR<-length(DA_PTR[(1540-130):1540])
length_Ac7_PTR<-length(DA_PTR[(1770-130):1770])
length_Ac8_PTR<-length(DA_PTR[(1990-130):1990])
length_Ac9_PTR<-length(DA_PTR[(2550-130):2550])
length_Ac10_PTR<-length(DA_PTR[(2820-130):2820])
length_Ac11_PTR<-length(DA_PTR[(3230-130):3230])

Tinterval_Ac0_PTR<-difftime(Time_PTR_DA[(340-130)],Time_PTR_DA[340])
Tinterval_Ac1_PTR<-difftime(Time_PTR_DA[(556-130)],Time_PTR_DA[556])
Tinterval_Ac2_PTR<-difftime(Time_PTR_DA[570],Time_PTR_DA[700])
Tinterval_Ac3_PTR<-difftime(Time_PTR_DA[(1000-130)],Time_PTR_DA[1000])
Tinterval_Ac4_PTR<-difftime(Time_PTR_DA[(1220-130)],Time_PTR_DA[1220])
Tinterval_Ac5_PTR<-difftime(Time_PTR_DA[(1390-130)],Time_PTR_DA[1390])
Tinterval_Ac6_PTR<-difftime(Time_PTR_DA[(1540-130)],Time_PTR_DA[1540])
Tinterval_Ac7_PTR<-difftime(Time_PTR_DA[(1770-130)],Time_PTR_DA[1770])
Tinterval_Ac8_PTR<-difftime(Time_PTR_DA[(1990-130)],Time_PTR_DA[1990])
Tinterval_Ac9_PTR<-difftime(Time_PTR_DA[(2550-130)],Time_PTR_DA[2550])
Tinterval_Ac10_PTR<-difftime(Time_PTR_DA[(2820-130)],Time_PTR_DA[2820])
Tinterval_Ac11_PTR<-difftime(Time_PTR_DA[(3230-130)],Time_PTR_DA[3230])

m_DA_PTR<-c(Ac0_PTR,Ac1_PTR,Ac2_PTR,Ac3_PTR,Ac4_PTR,Ac5_PTR,Ac6_PTR,Ac7_PTR,Ac8_PTR,Ac9_PTR,Ac10_PTR,Ac11_PTR)
sd_DA_PTR<-c(sd_Ac0_PTR,sd_Ac1_PTR,sd_Ac2_PTR,sd_Ac3_PTR,sd_Ac4_PTR,sd_Ac5_PTR,sd_Ac6_PTR,sd_Ac7_PTR,sd_Ac8_PTR,sd_Ac9_PTR,sd_Ac10_PTR,sd_Ac11_PTR)
length_DA_PTR<-c(length_Ac0_PTR,length_Ac1_PTR,length_Ac2_PTR,length_Ac3_PTR,length_Ac4_PTR,length_Ac5_PTR,length_Ac6_PTR,length_Ac7_PTR,length_Ac8_PTR,length_Ac9_PTR,length_Ac10_PTR,length_Ac11_PTR)
Tinterval_DA_PTR<-c(Tinterval_Ac0_PTR,Tinterval_Ac1_PTR,Tinterval_Ac2_PTR,Tinterval_Ac3_PTR,Tinterval_Ac4_PTR,Tinterval_Ac5_PTR,Tinterval_Ac6_PTR,Tinterval_Ac7_PTR,Tinterval_Ac8_PTR,Tinterval_Ac9_PTR,Tinterval_Ac10_PTR,Tinterval_Ac11_PTR)

#Select same time intervals in G2508 as in PTR-TOF
Ti_PTR<-round(as.numeric(Time_PTR_DA[c(340-130,340,556-130,556,570,700,1000-130,1000,1220-130,1220,1390-130,1390,1540-130,1540,1770-130,1770,1990-130,1990,2550-130,2550,2820-130,2820,3230-130,3230)]))
Ti_PTR<-Ti_PTR-as.numeric(seconds(90))
numericTime_All<-round(as.numeric(Time_All_DA))
Pos<-list()
min_dif<-1
for (i in 1:(length(Ti_PTR))){
  min_dif[i]<-min(abs(numericTime_All-Ti_PTR[i]))
  Pos[[i]]<-which(numericTime_All-Ti_PTR[i]==min_dif[i])
  Pos[[i]]<-Pos[[i]][1]
}
t<-unlist(Pos)
### For Publication--> save means for borrowed G2508 ###
write.table(t, file = "DA_G2509_intervals.txt", sep = "\t", row.names = TRUE, col.names = NA)
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
Tinterval_All_DA<-1

for (i in 1:(length(Ti_PTR)-1)){
  m_NH3_All[i]<-mean(NH3_All_DA[t[i]:t[i+1]])
  sd_NH3_All[i]<-sd(NH3_All_DA[t[i]:t[i+1]])
  length_All[i]<-length(NH3_All_DA[t[i]:t[i+1]])
  m_CH4_All[i]<-mean(CH4_All_DA[t[i]:t[i+1]])
  sd_CH4_All[i]<-sd(CH4_All_DA[t[i]:t[i+1]])
  m_N2O_All[i]<-mean(N2O_All_DA[t[i]:t[i+1]])
  sd_N2O_All[i]<-sd(N2O_All_DA[t[i]:t[i+1]])
  m_H2O_All[i]<-mean(H2O_All_DA[t[i]:t[i+1]])
  sd_H2O_All[i]<-sd(H2O_All_DA[t[i]:t[i+1]])
  Tinterval_All_DA[i]<-difftime(Time_All_DA[t[i]],Time_All_DA[t[i+1]])
}
#Only odd positions are correct (Even positions are from t(2)to t(3))
m_NH3_All_DA<-m_NH3_All[c(TRUE,FALSE)]
sd_NH3_All_DA<-sd_NH3_All[c(TRUE,FALSE)]
length_All_DA<-length_All[c(TRUE,FALSE)]
m_CH4_All_DA<-m_CH4_All[c(TRUE,FALSE)]
sd_CH4_All_DA<-sd_CH4_All[c(TRUE,FALSE)]
m_N2O_All_DA<-m_N2O_All[c(TRUE,FALSE)]
sd_N2O_All_DA<-sd_N2O_All[c(TRUE,FALSE)]
m_N2O_All_DA<-m_N2O_All[c(TRUE,FALSE)]
sd_N2O_All_DA<-sd_N2O_All[c(TRUE,FALSE)]
m_H2O_All_DA<-m_H2O_All[c(TRUE,FALSE)]
sd_H2O_All_DA<-sd_H2O_All[c(TRUE,FALSE)]
Tinterval_All_DA<-Tinterval_All_DA[c(TRUE,FALSE)]

#Select same time intervals in NH3 Picarro as in PTR-TOF
Ti_PTR<-round(as.numeric(Time_PTR_DA[c(340-130,340,556-130,556,570,700,1000-130,1000,1220-130,1220,1390-130,1390,1540-130,1540,1770-130,1770,1990-130,1990,2550-130,2550,2820-130,2820,3230-130,3230)]))
Ti_PTR<-Ti_PTR-as.numeric(seconds(90))
numericTime_All<-round(as.numeric(Time_All_DA))
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
Tinterval_All_DA<-1

for (i in 1:(length(Ti_PTR)-1)){
  m_NH3_All[i]<-mean(NH3_All_DA[t[i]:t[i+1]])
  sd_NH3_All[i]<-sd(NH3_All_DA[t[i]:t[i+1]])
  length_All[i]<-length(NH3_All_DA[t[i]:t[i+1]])
  m_CH4_All[i]<-mean(CH4_All_DA[t[i]:t[i+1]])
  sd_CH4_All[i]<-sd(CH4_All_DA[t[i]:t[i+1]])
  m_N2O_All[i]<-mean(N2O_All_DA[t[i]:t[i+1]])
  sd_N2O_All[i]<-sd(N2O_All_DA[t[i]:t[i+1]])
  m_H2O_All[i]<-mean(H2O_All_DA[t[i]:t[i+1]])
  sd_H2O_All[i]<-sd(H2O_All_DA[t[i]:t[i+1]])
  Tinterval_All_DA[i]<-difftime(Time_All_DA[t[i]],Time_All_DA[t[i+1]])
}
#Only odd positions are correct (Even positions are from t(2)to t(3))
m_NH3_All_DA<-m_NH3_All[c(TRUE,FALSE)]
sd_NH3_All_DA<-sd_NH3_All[c(TRUE,FALSE)]
length_All_DA<-length_All[c(TRUE,FALSE)]
m_CH4_All_DA<-m_CH4_All[c(TRUE,FALSE)]
sd_CH4_All_DA<-sd_CH4_All[c(TRUE,FALSE)]
m_N2O_All_DA<-m_N2O_All[c(TRUE,FALSE)]
sd_N2O_All_DA<-sd_N2O_All[c(TRUE,FALSE)]
m_N2O_All_DA<-m_N2O_All[c(TRUE,FALSE)]
sd_N2O_All_DA<-sd_N2O_All[c(TRUE,FALSE)]
m_H2O_All_DA<-m_H2O_All[c(TRUE,FALSE)]
sd_H2O_All_DA<-sd_H2O_All[c(TRUE,FALSE)]
Tinterval_All_DA<-Tinterval_All_DA[c(TRUE,FALSE)]

#Select same time intervals in NH3 Picarro as in PTR-TOF
Ti_PTR<-round(as.numeric(Time_PTR_DA[c(340-130,340,556-130,556,570,700,1000-130,1000,1220-130,1220,1390-130,1390,1540-130,1540,1770-130,1770,1990-130,1990,2550-130,2550,2820-130,2820,3230-130,3230)]))
Ti_PTR<-(Ti_PTR-as.numeric(seconds(90)))-as.numeric(minutes(177))
numericTime_NH3<-round(as.numeric(Time_NH3_DA))
Pos<-list()
min_dif<-1
for (i in 1:(length(Ti_PTR))){
  min_dif[i]<-min(abs(numericTime_NH3-Ti_PTR[i]))
  Pos[[i]]<-which(abs(numericTime_NH3-Ti_PTR[i])==min_dif[i])
  Pos[[i]]<-Pos[[i]][1]
}
t<-unlist(Pos)
### For Publication--> save means for G2103 ###
write.table(t, file = "DA_G2103_intervals.txt", sep = "\t", row.names = TRUE, col.names = NA)
########################################################


## Put the calculated positions in G2103
m_NH3r_NH3<-1
sd_NH3r_NH3<-1
length_NH3<-1
m_NH3d_NH3<-1
sd_NH3d_NH3<-1
m_H2O_NH3<-1
sd_H2O_NH3<-1
Tinterval_NH3_DA<-1

for (i in 1:(length(Ti_PTR)-1)){
  m_NH3r_NH3[i]<-mean(NH3r_NH3_DA[t[i]:t[i+1]])
  sd_NH3r_NH3[i]<-sd(NH3r_NH3_DA[t[i]:t[i+1]])
  length_NH3[i]<-length(NH3r_NH3_DA[t[i]:t[i+1]])
  m_NH3d_NH3[i]<-mean(NH3d_NH3_DA[t[i]:t[i+1]])
  sd_NH3d_NH3[i]<-sd(NH3d_NH3_DA[t[i]:t[i+1]])
  m_H2O_NH3[i]<-mean(H2O_NH3_DA[t[i]:t[i+1]])
  sd_H2O_NH3[i]<-sd(H2O_NH3_DA[t[i]:t[i+1]])
  Tinterval_NH3_DA[i]<-difftime(Time_NH3_DA[t[i]],Time_NH3_DA[t[i+1]])
}
#Only odd positions are correct (Even positions are from t(2)to t(3))
m_NH3r_NH3_DA<-m_NH3r_NH3[c(TRUE,FALSE)]
sd_NH3r_NH3_DA<-sd_NH3r_NH3[c(TRUE,FALSE)]
length_NH3_DA<-length_NH3[c(TRUE,FALSE)]
m_NH3d_NH3_DA<-m_NH3d_NH3[c(TRUE,FALSE)]
sd_NH3d_NH3_DA<-sd_NH3d_NH3[c(TRUE,FALSE)]
m_H2O_NH3_DA<-m_H2O_NH3[c(TRUE,FALSE)]
sd_H2O_NH3_DA<-sd_H2O_NH3[c(TRUE,FALSE)]
Tinterval_NH3_DA<-Tinterval_NH3_DA[c(TRUE,FALSE)]

#Select same time intervals in BackPack as in PTR-TOF
Ti_PTR<-round(as.numeric(Time_PTR_DA[c(340-130,340,556-130,556,570,700,1000-130,1000,1220-130,1220,1390-130,1390,1540-130,1540,1770-130,1770,1990-130,1990,2550-130,2550,2820-130,2820,3230-130,3230)]))
Ti_PTR<-(Ti_PTR-as.numeric(seconds(90)))-as.numeric(minutes(119))
numericTime_BP<-round(as.numeric(Time_BP_DA))
Pos<-list()
min_dif<-1
for (i in 1:(length(Ti_PTR))){
  min_dif[i]<-min(abs(numericTime_BP-Ti_PTR[i]))
  Pos[[i]]<-which(numericTime_BP-Ti_PTR[i]==min_dif[i])
  Pos[[i]]<-Pos[[i]][1]
}
t<-unlist(Pos)
### For Publication--> save means for G4301 ###
write.table(t, file = "DA_G4301_intervals.txt", sep = "\t", row.names = TRUE, col.names = NA)
########################################################


## Put the calculated positions in G4301
m_CH4_BP<-1
sd_CH4_BP<-1
length_BP<-1
m_H2O_BP<-1
sd_H2O_BP<-1
m_CH4d_BP<-1
sd_CH4d_BP<-1
Tinterval_BP_DA<-1

for (i in 1:(length(Ti_PTR)-1)){
  m_CH4_BP[i]<-mean(CH4_BP_DA[t[i]:t[i+1]])
  sd_CH4_BP[i]<-sd(CH4_BP_DA[t[i]:t[i+1]])
  length_BP[i]<-length(CH4_BP_DA[t[i]:t[i+1]])
  m_CH4d_BP[i]<-mean(CH4d_BP_DA[t[i]:t[i+1]])
  sd_CH4d_BP[i]<-sd(CH4d_BP_DA[t[i]:t[i+1]])
  m_H2O_BP[i]<-mean(H2O_BP_DA[t[i]:t[i+1]])
  sd_H2O_BP[i]<-sd(H2O_BP_DA[t[i]:t[i+1]])
  Tinterval_BP_DA[i]<-difftime(Time_BP_DA[t[i]],Time_BP_DA[t[i+1]])
}
#Only odd positions are correct (Even positions are from t(2)to t(3))
m_CH4_BP_DA<-m_CH4_BP[c(TRUE,FALSE)]
sd_CH4_BP_DA<-sd_CH4_BP[c(TRUE,FALSE)]
length_BP_DA<-length_BP[c(TRUE,FALSE)]
m_CH4d_BP_DA<-m_CH4d_BP[c(TRUE,FALSE)]
sd_CH4d_BP_DA<-sd_CH4d_BP[c(TRUE,FALSE)]
m_H2O_BP_DA<-m_H2O_BP[c(TRUE,FALSE)]
sd_H2O_BP_DA<-sd_H2O_BP[c(TRUE,FALSE)]
Tinterval_BP_DA<-Tinterval_BP_DA[c(TRUE,FALSE)]



## Create and join all points into one data frame ##
#NH3
y_NH3<-c(m_NH3_All_DA,m_NH3r_NH3_DA,m_NH3d_NH3_DA)
x_NH3<-c(m_DA_PTR,m_DA_PTR,m_DA_PTR)
sd_NH3<-c(sd_NH3_All_DA,sd_NH3r_NH3_DA,sd_NH3d_NH3_DA)
Color_NH3<-c(rep("NH3_G2508",12),rep("NH3_raw",12),rep("NH3_dry",12))
df_NH3_DA<-cbind.data.frame(x_NH3,y_NH3,sd_NH3,Color_NH3)
#CH4
y_CH4<-c(m_CH4_All_DA,m_CH4_BP_DA,m_CH4d_BP_DA)
x_CH4<-c(m_DA_PTR,m_DA_PTR,m_DA_PTR)
sd_CH4<-c(sd_CH4_All_DA,sd_CH4_BP_DA,sd_CH4d_BP_DA)
Color_CH4<-c(rep("CH4_G2508",12),rep("CH4",12),rep("CH4_dry",12))
df_CH4_DA<-cbind.data.frame(x_CH4,y_CH4,sd_CH4,Color_CH4)
#N2O
y_N2O<-c(m_N2O_All_DA)
x_N2O<-c(m_DA_PTR)
sd_N2O<-c(sd_N2O_All_DA)
Color_N2O<-c(rep("N2O_G2508",12))
df_N2O_DA<-cbind.data.frame(x_N2O,y_N2O,sd_N2O,Color_N2O)




### For publication ###
## Remove NH3_raw & CH4
##Add standard deviation of VOC

## 1-Butanol
#NH3
y_NH3<-c(m_NH3_All_B1,m_NH3d_NH3_B1)
x_NH3<-c(m_B1_PTR,m_B1_PTR)
sd_NH3<-c(sd_NH3_All_B1,sd_NH3d_NH3_B1)
sd_B1<-c(sd_B1_PTR,sd_B1_PTR)
Color_NH3<-c(rep("G2509",12),rep("G2103",12))
df_NH3_B1<-cbind.data.frame(x_NH3,y_NH3,sd_NH3,Color_NH3)
#CH4
y_CH4<-c(m_CH4_All_B1,m_CH4d_BP_B1)
x_CH4<-c(m_B1_PTR,m_B1_PTR)
sd_CH4<-c(sd_CH4_All_B1,sd_CH4d_BP_B1)
Color_CH4<-c(rep("G2509",12),rep("G4301",12))
df_CH4_B1<-cbind.data.frame(x_CH4,y_CH4,sd_CH4,Color_CH4)
#N2O
y_N2O<-c(m_N2O_All_B1)
x_N2O<-c(m_B1_PTR)
sd_N2O<-c(sd_N2O_All_B1)
Color_N2O<-c(rep("G2509",12))
df_N2O_B1<-cbind.data.frame(x_N2O,y_N2O,sd_N2O,Color_N2O)

df_Control_B1<-cbind.data.frame(length_B1_PTR,length_BP_B1,length_NH3_B1,length_All_B1,
                                Tinterval_B1_PTR,Tinterval_BP_B1,Tinterval_NH3_B1,Tinterval_All_B1,m_H2O_BP_B1,m_H2O_NH3_B1,m_H2O_All_B1)

## 1-Propanol
#NH3
y_NH3<-c(m_NH3_All_P1,m_NH3d_NH3_P1)
x_NH3<-c(m_P1_PTR,m_P1_PTR)
sd_NH3<-c(sd_NH3_All_P1,sd_NH3d_NH3_P1)
sd_P1<-c(sd_P1_PTR,sd_P1_PTR)
Color_NH3<-c(rep("G2509",14),rep("G2103",14))
df_NH3_P1<-cbind.data.frame(x_NH3,y_NH3,sd_NH3,Color_NH3)
#CH4
y_CH4<-c(m_CH4_All_P1,m_CH4d_BP_P1)
x_CH4<-c(m_P1_PTR,m_P1_PTR)
sd_CH4<-c(sd_CH4_All_P1,sd_CH4d_BP_P1)
Color_CH4<-c(rep("G2509",14),rep("G4301",14))
df_CH4_P1<-cbind.data.frame(x_CH4,y_CH4,sd_CH4,Color_CH4)
#N2O
y_N2O<-c(m_N2O_All_P1)
x_N2O<-c(m_P1_PTR)
sd_N2O<-c(sd_N2O_All_P1)
Color_N2O<-c(rep("G2509",14))
df_N2O_P1<-cbind.data.frame(x_N2O,y_N2O,sd_N2O,Color_N2O)

df_Control_P1<-cbind.data.frame(length_P1_PTR,length_BP_P1,length_NH3_P1,length_All_P1,
                                Tinterval_P1_PTR,Tinterval_BP_P1,Tinterval_NH3_P1,Tinterval_All_P1,m_H2O_BP_P1,m_H2O_NH3_P1,m_H2O_All_P1)

## Acetaldehyde
#NH3
y_NH3<-c(m_NH3_All_AA,m_NH3d_NH3_AA)
x_NH3<-c(m_AA_PTR,m_AA_PTR)
sd_NH3<-c(sd_NH3_All_AA,sd_NH3d_NH3_AA)
sd_AA<-c(sd_AA_PTR,sd_AA_PTR)
Color_NH3<-c(rep("G2509",13),rep("G2103",13))
df_NH3_AA<-cbind.data.frame(x_NH3,y_NH3,sd_NH3,Color_NH3)
#CH4
y_CH4<-c(m_CH4_All_AA,m_CH4d_BP_AA)
x_CH4<-c(m_AA_PTR,m_AA_PTR)
sd_CH4<-c(sd_CH4_All_AA,sd_CH4d_BP_AA)
Color_CH4<-c(rep("G2509",13),rep("G4301",13))
df_CH4_AA<-cbind.data.frame(x_CH4,y_CH4,sd_CH4,Color_CH4)
#N2O
y_N2O<-c(m_N2O_All_AA)
x_N2O<-c(m_AA_PTR)
sd_N2O<-c(sd_N2O_All_AA)
Color_N2O<-c(rep("G2509",13))
df_N2O_AA<-cbind.data.frame(x_N2O,y_N2O,sd_N2O,Color_N2O)

df_Control_AA<-cbind.data.frame(length_AA_PTR,length_BP_AA,length_NH3_AA,length_All_AA,
                                Tinterval_AA_PTR,Tinterval_BP_AA,Tinterval_NH3_AA,Tinterval_All_AA,m_H2O_BP_AA,m_H2O_NH3_AA,m_H2O_All_AA)

## Butanone
#NH3
y_NH3<-c(m_NH3_All_Bne,m_NH3d_NH3_Bne)
x_NH3<-c(m_Bne_PTR,m_Bne_PTR)
sd_NH3<-c(sd_NH3_All_Bne,sd_NH3d_NH3_Bne)
sd_Bne<-c(sd_Bne_PTR,sd_Bne_PTR)
Color_NH3<-c(rep("G2509",10),rep("G2103",10))
df_NH3_Bne<-cbind.data.frame(x_NH3,y_NH3,sd_NH3,Color_NH3)
#CH4
y_CH4<-c(m_CH4_All_Bne,m_CH4d_BP_Bne)
x_CH4<-c(m_Bne_PTR,m_Bne_PTR)
sd_CH4<-c(sd_CH4_All_Bne,sd_CH4d_BP_Bne)
Color_CH4<-c(rep("G2509",10),rep("G4301",10))
df_CH4_Bne<-cbind.data.frame(x_CH4,y_CH4,sd_CH4,Color_CH4)
#N2O
y_N2O<-c(m_N2O_All_Bne)
x_N2O<-c(m_Bne_PTR)
sd_N2O<-c(sd_N2O_All_Bne)
Color_N2O<-c(rep("G2509",10))
df_N2O_Bne<-cbind.data.frame(x_N2O,y_N2O,sd_N2O,Color_N2O)

df_Control_Bne<-cbind.data.frame(length_Bne_PTR,length_BP_Bne,length_NH3_Bne,length_All_Bne,
                                 Tinterval_Bne_PTR,Tinterval_BP_Bne,Tinterval_NH3_Bne,Tinterval_All_Bne,m_H2O_BP_Bne,m_H2O_NH3_Bne,m_H2O_All_Bne)

## Diacetyl
#NH3
y_NH3<-c(m_NH3_All_DA,m_NH3d_NH3_DA)
x_NH3<-c(m_DA_PTR,m_DA_PTR)
sd_NH3<-c(sd_NH3_All_DA,sd_NH3d_NH3_DA)
sd_DA<-c(sd_DA_PTR,sd_DA_PTR)
Color_NH3<-c(rep("G2509",12),rep("G2103",12))
df_NH3_DA<-cbind.data.frame(x_NH3,y_NH3,sd_NH3,Color_NH3)
#CH4
y_CH4<-c(m_CH4_All_DA,m_CH4d_BP_DA)
x_CH4<-c(m_DA_PTR,m_DA_PTR)
sd_CH4<-c(sd_CH4_All_DA,sd_CH4d_BP_DA)
Color_CH4<-c(rep("G2509",12),rep("G4301",12))
df_CH4_DA<-cbind.data.frame(x_CH4,y_CH4,sd_CH4,Color_CH4)
#N2O
y_N2O<-c(m_N2O_All_DA)
x_N2O<-c(m_DA_PTR)
sd_N2O<-c(sd_N2O_All_DA)
Color_N2O<-c(rep("G2509",12))
df_N2O_DA<-cbind.data.frame(x_N2O,y_N2O,sd_N2O,Color_N2O)

df_Control_DA<-cbind.data.frame(length_DA_PTR,length_BP_DA,length_NH3_DA,length_All_DA,
                                Tinterval_DA_PTR,Tinterval_BP_DA,Tinterval_NH3_DA,Tinterval_All_DA,m_H2O_BP_DA,m_H2O_NH3_DA,m_H2O_All_DA)




##Diacetyl
write.table(df_N2O_DA, file = "DA_N2O.txt", sep = "\t", row.names = TRUE, col.names = NA)
write.table(df_NH3_DA, file = "DA_NH3.txt", sep = "\t", row.names = TRUE, col.names = NA)
write.table(df_CH4_DA, file = "DA_CH4.txt", sep = "\t", row.names = TRUE, col.names = NA)
write.table(sd_DA_PTR, file = "sd_DA.txt", sep = "\t", row.names = TRUE, col.names = NA)
write.table(Dat_All, file = "DA_G2509_CH4.txt", sep = "\t", row.names = TRUE, col.names = NA)
write.table(Dat_BP, file = "DA_G4301_CH4.txt", sep = "\t", row.names = TRUE, col.names = NA)

##Butanone
write.table(df_N2O_Bne, file = "Bne_N2O.txt", sep = "\t", row.names = TRUE, col.names = NA)
write.table(df_NH3_Bne, file = "Bne_NH3.txt", sep = "\t", row.names = TRUE, col.names = NA)
write.table(df_CH4_Bne, file = "Bne_CH4.txt", sep = "\t", row.names = TRUE, col.names = NA)
write.table(sd_Bne_PTR, file = "sd_Bne.txt", sep = "\t", row.names = TRUE, col.names = NA)
write.table(Dat_All, file = "Bne_G2509_CH4.txt", sep = "\t", row.names = TRUE, col.names = NA)
write.table(Dat_BP, file = "Bne_G4301_CH4.txt", sep = "\t", row.names = TRUE, col.names = NA)

##1-butanol
write.table(df_N2O_B1, file = "B1_N2O.txt", sep = "\t", row.names = TRUE, col.names = NA)
write.table(df_NH3_B1, file = "B1_NH3.txt", sep = "\t", row.names = TRUE, col.names = NA)
write.table(df_CH4_B1, file = "B1_CH4.txt", sep = "\t", row.names = TRUE, col.names = NA)
write.table(sd_B1_PTR, file = "sd_B1.txt", sep = "\t", row.names = TRUE, col.names = NA)
write.table(Dat_All, file = "B1_G2509_CH4.txt", sep = "\t", row.names = TRUE, col.names = NA)
write.table(Dat_BP, file = "B1_G4301_CH4.txt", sep = "\t", row.names = TRUE, col.names = NA)

##1-propanol
write.table(df_N2O_P1, file = "P1_N2O.txt", sep = "\t", row.names = TRUE, col.names = NA)
write.table(df_NH3_P1, file = "P1_NH3.txt", sep = "\t", row.names = TRUE, col.names = NA)
write.table(df_CH4_P1, file = "P1_CH4.txt", sep = "\t", row.names = TRUE, col.names = NA)
write.table(sd_P1_PTR, file = "sd_P1.txt", sep = "\t", row.names = TRUE, col.names = NA)
write.table(Dat_All, file = "P1_G2509_CH4.txt", sep = "\t", row.names = TRUE, col.names = NA)
write.table(Dat_BP, file = "P1_G4301_CH4.txt", sep = "\t", row.names = TRUE, col.names = NA)

##Acetaldehyde
write.table(df_N2O_AA, file = "AA_N2O.txt", sep = "\t", row.names = TRUE, col.names = NA)
write.table(df_NH3_AA, file = "AA_NH3.txt", sep = "\t", row.names = TRUE, col.names = NA)
write.table(df_CH4_AA, file = "AA_CH4.txt", sep = "\t", row.names = TRUE, col.names = NA)
write.table(sd_AA_PTR, file = "sd_AA.txt", sep = "\t", row.names = TRUE, col.names = NA)
write.table(Dat_All, file = "AA_G2509_CH4.txt", sep = "\t", row.names = TRUE, col.names = NA)
write.table(Dat_BP, file = "AA_G4301_CH4.txt", sep = "\t", row.names = TRUE, col.names = NA)
