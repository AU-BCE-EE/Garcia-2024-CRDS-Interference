library(dplyr)
library(ggplot2)
library(anytime)
library(lubridate)
library(writexl)
library("readxl")


### Interferences 2021-07-14###   
##Propionic acid

##Extract data from NH3 picarro##  
setwd(".../VOC Interferences/NH3 Picarro")
list_of_files <- list.files(path="NH3 Picarro",pattern = "*.dat",recursive=TRUE) ##Read data from Picarro
L<-length(list_of_files)

setwd(".../VOC Interferences/NH3 Picarro/14")

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



##Extract data from own picarro##  
setwd(".../VOC Interferences/G2508 own")
list_of_files <- list.files(path="G2508 own",pattern = "*.dat",recursive=TRUE) ##Read data from Picarro
L<-length(list_of_files)

setwd(".../VOC Interferences/G2508 own/14")

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
PAc<-read_excel("Propionic acid.xlsx")
PAc<-as.data.frame(PAc)


###Data to plot all the calibration###
##Acetic acid##
Time_PTR_PAc<-c(PAc$AbsTime)
PAc_PTR<-c(PAc$`m57.0335 (C3H4OH+)`+PAc$`m75.0441 (C3H6O2H+)`)

#Sincronize all Picarros and PTR
TAc_PTR<-round(as.numeric(Time_PTR_PAc[c(1,length(Time_PTR_PAc))]))
TAc_PTR<-TAc_PTR-as.numeric(seconds(90))
TAc_PTRn<-TAc_PTR-as.numeric(minutes(177))
TAc_PTRb<-TAc_PTR-as.numeric(minutes(119))
TAc_PTRo<-TAc_PTR-as.numeric(minutes(57))
TAc_NH3<-round(as.numeric(Dat_NH3$date.time))
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
  min_dif_AcN[i]<-min(abs(TAc_NH3-TAc_PTRn[i]))
  PAc_N[[i]]<-which(TAc_NH3-TAc_PTRn[i]==min_dif_AcN[i])
  PAc_N[[i]]<-PAc_N[[i]][1]
  #min_dif_AcC[i]<-min(abs(TAc_BP-TAc_PTRb[i]))
  #PAc_C[[i]]<-which(TAc_BP-TAc_PTRb[i]==min_dif_AcC[i])
  #PAc_C[[i]]<-PAc_C[[i]][1]
  min_dif_AcO[i]<-min(abs(TAc_Own-TAc_PTRo[i]))
  PAc_O[[i]]<-which(TAc_Own-TAc_PTRo[i]==min_dif_AcO[i])
  PAc_O[[i]]<-PAc_O[[i]][1]
}
tAc<-unlist(PAc)
tAcn<-unlist(PAc_N)
tAcc<-unlist(PAc_C)
tAco<-unlist(PAc_O)

#Time_BP_PAc<-c(Dat_BP$date.time[tAcc[1]:tAcc[2]])
Time_NH3_PAc<-c(Dat_NH3$date.time[tAcn[1]:tAcn[2]])
Time_own_PAc<-c(Dat_own$date.time[tAco[1]:tAco[2]])
#H2O_BP_PAc<-c(Dat_BP$H2O[tAcc[1]:tAcc[2]])
H2O_NH3_PAc<-c(Dat_NH3$H2O[tAcn[1]:tAcn[2]])
H2O_own_PAc<-c(Dat_own$H2O[tAco[1]:tAco[2]])
NH3r_NH3_PAc<-c(Dat_NH3$NH3_Raw[tAcn[1]:tAcn[2]])
NH3d_NH3_PAc<-c(Dat_NH3$NH3_dry[tAcn[1]:tAcn[2]])
NH3_own_PAc<-c(Dat_own$NH3[tAco[1]:tAco[2]])
#CH4_BP_PAc<-c(Dat_BP$CH4[tAcc[1]:tAcc[2]])
#CH4d_BP_PAc<-c(Dat_BP$CH4_dry[tAcc[1]:tAcc[2]])
CH4_own_PAc<-c(Dat_own$CH4_dry[tAco[1]:tAco[2]])
N2O_own_PAc<-c(Dat_own$N2O_dry[tAco[1]:tAco[2]])
#### Save the intervals for publication ####
setwd("C:/Users/au615105/OneDrive - Aarhus universitet/Pablo Garc?a/Papers/Interferences pictures/VOCs")
Interval_PAc<-cbind.data.frame(tAco,tAcn)
write.table(Interval_PAc, file = "PAc_Interval.txt", sep = "\t", row.names = TRUE, col.names = NA)


###Data to plot the interferences at each concentration###
##Propionic acid
Ac0_PTR<-mean(PAc_PTR[(750-135):750])
Ac1_PTR<-mean(PAc_PTR[(1080-135):1080])
Ac2_PTR<-mean(PAc_PTR[(1260-135):1260])
Ac3_PTR<-mean(PAc_PTR[(1462-135):1462])
Ac4_PTR<-mean(PAc_PTR[(1490):1625])
Ac5_PTR<-mean(PAc_PTR[(1835-135):1835])
Ac6_PTR<-mean(PAc_PTR[(2460-135):2460])

sd_Ac0_PTR<-sd(PAc_PTR[(750-135):750])
sd_Ac1_PTR<-sd(PAc_PTR[(1080-135):1080])
sd_Ac2_PTR<-sd(PAc_PTR[(1260-135):1260])
sd_Ac3_PTR<-sd(PAc_PTR[(1462-135):1462])
sd_Ac4_PTR<-sd(PAc_PTR[(1490):1625])
sd_Ac5_PTR<-sd(PAc_PTR[(1835-135):1835])
sd_Ac6_PTR<-sd(PAc_PTR[(2460-135):2460])

length_Ac0_PTR<-length(PAc_PTR[(750-135):750])
length_Ac1_PTR<-length(PAc_PTR[(1080-135):1080])
length_Ac2_PTR<-length(PAc_PTR[(1260-135):1260])
length_Ac3_PTR<-length(PAc_PTR[(1462-135):1462])
length_Ac4_PTR<-length(PAc_PTR[(1490):1625])
length_Ac5_PTR<-length(PAc_PTR[(1835-135):1835])
length_Ac6_PTR<-length(PAc_PTR[(2460-135):2460])

Tinterval_Ac0_PTR<-difftime(Time_PTR_PAc[(750-135)],Time_PTR_PAc[750])
Tinterval_Ac1_PTR<-difftime(Time_PTR_PAc[(1080-135)],Time_PTR_PAc[1080])
Tinterval_Ac2_PTR<-difftime(Time_PTR_PAc[(1260-135)],Time_PTR_PAc[1260])
Tinterval_Ac3_PTR<-difftime(Time_PTR_PAc[(1462-135)],Time_PTR_PAc[1462])
Tinterval_Ac4_PTR<-difftime(Time_PTR_PAc[(1490)],Time_PTR_PAc[1625])
Tinterval_Ac5_PTR<-difftime(Time_PTR_PAc[(1835-135)],Time_PTR_PAc[1835])
Tinterval_Ac6_PTR<-difftime(Time_PTR_PAc[(2460-135)],Time_PTR_PAc[2460])


m_PAc_PTR<-c(Ac0_PTR,Ac1_PTR,Ac2_PTR,Ac3_PTR,Ac4_PTR,Ac5_PTR,Ac6_PTR)
sd_PAc_PTR<-c(sd_Ac0_PTR,sd_Ac1_PTR,sd_Ac2_PTR,sd_Ac3_PTR,sd_Ac4_PTR,sd_Ac5_PTR,sd_Ac6_PTR)
length_PAc_PTR<-c(length_Ac0_PTR,length_Ac1_PTR,length_Ac2_PTR,length_Ac3_PTR,length_Ac4_PTR,length_Ac5_PTR,length_Ac6_PTR)
Tinterval_PAc_PTR<-c(Tinterval_Ac0_PTR,Tinterval_Ac1_PTR,Tinterval_Ac2_PTR,Tinterval_Ac3_PTR,Tinterval_Ac4_PTR,Tinterval_Ac5_PTR,Tinterval_Ac6_PTR)


#Select same time intervals in NH3 Picarro as in PTR-TOF
Ti_PTR<-round(as.numeric(Time_PTR_PAc[c(750-135,750,1080-135,1080,1260-135,1260,1462-135,1462,1490,1625,1835-135,1835,2460-135,2460)]))
Ti_PTR<-(Ti_PTR-as.numeric(seconds(120)))-as.numeric(minutes(177))
numericTime_NH3<-round(as.numeric(Time_NH3_PAc))
Pos<-list()
min_dif<-1
for (i in 1:(length(Ti_PTR))){
  min_dif[i]<-min(abs(numericTime_NH3-Ti_PTR[i]))
  Pos[[i]]<-which(numericTime_NH3-Ti_PTR[i]==min_dif[i])
  Pos[[i]]<-Pos[[i]][1]
}
t<-unlist(Pos)
### For Publication--> save means for borrowed G2103 ###
write.table(t, file = "PAc_G2103_intervals.txt", sep = "\t", row.names = TRUE, col.names = NA)
########################################################


## Put the calculated positions in G2103
m_NH3r_NH3<-1
sd_NH3r_NH3<-1
length_NH3<-1
m_NH3d_NH3<-1
sd_NH3d_NH3<-1
m_H2O_NH3<-1
sd_H2O_NH3<-1
Tinterval_NH3_PAc<-1

for (i in 1:(length(Ti_PTR)-1)){
  m_NH3r_NH3[i]<-mean(NH3r_NH3_PAc[t[i]:t[i+1]])
  sd_NH3r_NH3[i]<-sd(NH3r_NH3_PAc[t[i]:t[i+1]])
  length_NH3[i]<-length(NH3r_NH3_PAc[t[i]:t[i+1]])
  m_NH3d_NH3[i]<-mean(NH3d_NH3_PAc[t[i]:t[i+1]])
  sd_NH3d_NH3[i]<-sd(NH3d_NH3_PAc[t[i]:t[i+1]])
  m_H2O_NH3[i]<-mean(H2O_NH3_PAc[t[i]:t[i+1]])
  sd_H2O_NH3[i]<-sd(H2O_NH3_PAc[t[i]:t[i+1]])
  Tinterval_NH3_PAc[i]<-difftime(Time_NH3_PAc[t[i]],Time_NH3_PAc[t[i+1]])
}
#Only odd positions are correct (Even positions are from t(2)to t(3))
m_NH3r_NH3_PAc<-m_NH3r_NH3[c(TRUE,FALSE)]
sd_NH3r_NH3_PAc<-sd_NH3r_NH3[c(TRUE,FALSE)]
length_NH3_PAc<-length_NH3[c(TRUE,FALSE)]
m_NH3d_NH3_PAc<-m_NH3d_NH3[c(TRUE,FALSE)]
sd_NH3d_NH3_PAc<-sd_NH3d_NH3[c(TRUE,FALSE)]
m_H2O_NH3_PAc<-m_H2O_NH3[c(TRUE,FALSE)]
sd_H2O_NH3_PAc<-sd_H2O_NH3[c(TRUE,FALSE)]
Tinterval_NH3_PAc<-Tinterval_NH3_PAc[c(TRUE,FALSE)]


#Select same time intervals in own all Picarro as in PTR-TOF

Ti_PTR<-round(as.numeric(Time_PTR_PAc[c(750-135,750,1080-135,1080,1260-135,1260,1462-135,1462,1490,1625,1835-135,1835,2460-135,2460)]))
Ti_PTR<-(Ti_PTR-as.numeric(seconds(120)))-as.numeric(minutes(57))
numericTime_own<-round(as.numeric(Time_own_PAc))
Pos<-list()
min_dif<-1
for (i in 1:(length(Ti_PTR))){
  min_dif[i]<-min(abs(numericTime_own-Ti_PTR[i]))
  Pos[[i]]<-which(abs(numericTime_own-Ti_PTR[i])==min_dif[i])
  Pos[[i]]<-Pos[[i]][1]
}
t<-unlist(Pos)
### For Publication--> save means for borrowed G2508 ###
write.table(t, file = "PAc_G2509_intervals.txt", sep = "\t", row.names = TRUE, col.names = NA)
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
Tinterval_own_PAc<-1

for (i in 1:(length(Ti_PTR)-1)){
  m_NH3_own[i]<-mean(NH3_own_PAc[t[i]:t[i+1]])
  sd_NH3_own[i]<-sd(NH3_own_PAc[t[i]:t[i+1]])
  length_own[i]<-length(NH3_own_PAc[t[i]:t[i+1]])
  m_CH4_own[i]<-mean(CH4_own_PAc[t[i]:t[i+1]])
  sd_CH4_own[i]<-sd(CH4_own_PAc[t[i]:t[i+1]])
  m_N2O_own[i]<-mean(N2O_own_PAc[t[i]:t[i+1]])
  sd_N2O_own[i]<-sd(N2O_own_PAc[t[i]:t[i+1]])
  m_H2O_own[i]<-mean(H2O_own_PAc[t[i]:t[i+1]])
  sd_H2O_own[i]<-sd(H2O_own_PAc[t[i]:t[i+1]])
  Tinterval_own_PAc[i]<-difftime(Time_own_PAc[t[i]],Time_own_PAc[t[i+1]])
}
#Only odd positions are correct (Even positions are from t(2)to t(3))
m_NH3_own_PAc<-m_NH3_own[c(TRUE,FALSE)]
sd_NH3_own_PAc<-sd_NH3_own[c(TRUE,FALSE)]
length_own_PAc<-length_own[c(TRUE,FALSE)]
m_CH4_own_PAc<-m_CH4_own[c(TRUE,FALSE)]
sd_CH4_own_PAc<-sd_CH4_own[c(TRUE,FALSE)]
m_N2O_own_PAc<-m_N2O_own[c(TRUE,FALSE)]
sd_N2O_own_PAc<-sd_N2O_own[c(TRUE,FALSE)]
m_N2O_own_PAc<-m_N2O_own[c(TRUE,FALSE)]
sd_N2O_own_PAc<-sd_N2O_own[c(TRUE,FALSE)]
m_H2O_own_PAc<-m_H2O_own[c(TRUE,FALSE)]
sd_H2O_own_PAc<-sd_H2O_own[c(TRUE,FALSE)]
Tinterval_own_PAc<-Tinterval_own_PAc[c(TRUE,FALSE)]


## Create and join all points into one data frame ##
#NH3
y_NH3<-c(m_NH3d_NH3_PAc,m_NH3_own_PAc)
x_NH3<-c(m_PAc_PTR,m_PAc_PTR)
sd_NH3<-c(sd_NH3d_NH3_PAc,sd_NH3_own_PAc)
Color_NH3<-c(rep("G2103",7),rep("G2509_2",7))
df_NH3_PAc<-cbind.data.frame(x_NH3,y_NH3,sd_NH3,Color_NH3)
sd_PAc<-c(sd_PAc_PTR,sd_PAc_PTR)

#CH4
y_CH4<-c(m_CH4_own_PAc)
x_CH4<-c(m_PAc_PTR)
sd_CH4<-c(sd_CH4_own_PAc)
Color_CH4<-c(rep("G2509_2",7))
df_CH4_PAc<-cbind.data.frame(x_CH4,y_CH4,sd_CH4,Color_CH4)
#N2O
y_N2O<-c(m_N2O_own_PAc)
x_N2O<-c(m_PAc_PTR)
sd_N2O<-c(sd_N2O_own_PAc)
Color_N2O<-c(rep("G2509_2",7))
df_N2O_PAc<-cbind.data.frame(x_N2O,y_N2O,sd_N2O,Color_N2O)

df_Control_PAc<-cbind.data.frame(length_PAc_PTR,length_NH3_PAc,length_own_PAc,
                                 Tinterval_PAc_PTR,Tinterval_NH3_PAc,Tinterval_own_PAc,
                                 m_H2O_NH3_PAc,m_H2O_own_PAc)



write.table(df_CH4_PAc, file = "PAc_CH4.txt", sep = "\t", row.names = TRUE, col.names = NA)
write.table(df_N2O_PAc, file = "PAc_N2O.txt", sep = "\t", row.names = TRUE, col.names = NA)
write.table(df_NH3_PAc, file = "PAc_NH3.txt", sep = "\t", row.names = TRUE, col.names = NA)
write.table(sd_PAc_PTR, file = "sd_PAc.txt", sep = "\t", row.names = TRUE, col.names = NA)
write.table(Dat_own, file = "PAc_G2509_CH4.txt", sep = "\t", row.names = TRUE, col.names = NA)
