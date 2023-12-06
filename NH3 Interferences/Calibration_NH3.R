library(dplyr)
library(ggplot2)
library(anytime)
library(lubridate)
library(writexl)


### Calibration of G2508 borrowed ###   1ST ROUND

##NH3 (10ppm in the cylinder)##  
setwd("O:/Tech_BCE/Environmental Engineering/Air Quality Engineering/Picarro interferences (Anna&Pablo)/Picarro calibration/G2508 borrowed")
list_of_files <- list.files(path="23",pattern = "*.dat",recursive=TRUE) ##Read data from Picarro
L<-length(list_of_files)

setwd("O:/Tech_BCE/Environmental Engineering/Air Quality Engineering/Picarro interferences (Anna&Pablo)/Picarro calibration/G2508 borrowed/23")

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
Dat<-getstats(Picarro = Pic_id) #Able to call each Picarro file
Dat<-bind_rows(Dat, .id = "column_label") #Join all elements of the list in one dataframe

#Change format of date and time to be plotted
Dat$date.time<-paste(Dat$DATE,Dat$TIME)
Dat$date.time<-as.POSIXct(Dat$date.time,format="%Y-%m-%d %H:%M:%S")

##Select NH3 data##
Time_N<-c(Dat$date.time[2000:4600],Dat$date.time[5050:6590],Dat$date.time[7000:8430],Dat$date.time[9050:9978],Dat$date.time[10750:11460])
NH3_full<-c(Dat$NH3[2000:4600],Dat$NH3[5050:6590],Dat$NH3[7000:8430],Dat$NH3[9050:9978],Dat$NH3[10750:11460])
Measured_NH3<-c(mean(Dat$NH3[4100:4200]),mean(Dat$NH3[6500:6590]),mean(Dat$NH3[8250:8430]),mean(Dat$NH3[9900:9978]),mean(Dat$NH3[11250:11450]))
Sd_NH3<-c(sd(Dat$NH3[4100:4200]),sd(Dat$NH3[6500:6590],na.rm=TRUE),sd(Dat$NH3[8250:8430]),sd(Dat$NH3[9900:9978]),sd(Dat$NH3[11250:11450]))
Expected_NH3<-c(1000,5000,10000,15000,20000)
Corrected_NH3<-c(1210,5343,10870,16459,21541)
N_points<-c(length(Dat$NH3[4100:4200]),length(Dat$NH3[6500:6590]),length(Dat$NH3[8250:8430]),length(Dat$NH3[9900:9978]),length(Dat$NH3[11250:11450]))
##Select NH3 interferencees on N2O data##
Time_NOi<-c(Dat$date.time[2000:4600],Dat$date.time[5050:6590],Dat$date.time[7000:8430],Dat$date.time[9050:9978],Dat$date.time[10750:11460])
N2O_full<-c(Dat$N2O_dry[2000:4600],Dat$N2O_dry[5050:6590],Dat$N2O_dry[7000:8430],Dat$N2O_dry[9050:9978],Dat$N2O_dry[10750:11460])
Measured_N2O<-c(mean(Dat$N2O_dry[4100:4200]),mean(Dat$N2O_dry[6500:6590]),mean(Dat$N2O_dry[8250:8430]),mean(Dat$N2O_dry[9900:9978]),mean(Dat$N2O_dry[11250:11450]))
Sd_N2O<-c(sd(Dat$N2O_dry[4100:4200]),sd(Dat$N2O_dry[6500:6590],na.rm=TRUE),sd(Dat$N2O_dry[8250:8430]),sd(Dat$N2O_dry[9900:9978]),sd(Dat$N2O_dry[11250:11450]))
NOi_points<-c(length(Dat$N2O_dry[4100:4200]),length(Dat$N2O_dry[6500:6590]),length(Dat$N2O_dry[8250:8430]),length(Dat$N2O_dry[9900:9978]),length(Dat$N2O_dry[11250:11450]))

df_N2O<-cbind.data.frame(Measured_N2O,Sd_N2O,Measured_NH3,Sd_NH3)

##Save file for NH3 interferences on N2O
setwd("C:/Users/au615105/OneDrive - Aarhus universitet/Pablo Garc?a/Papers")
write.table(df_N2O, file = "NH3i_cal100_2.txt", sep = "\t", row.names = TRUE, col.names = NA)



#Select only NH3 data
Rows<-1:nrow(Dat)
Dat<-cbind.data.frame(anytime(Dat$date.time),Rows,Dat$NH3)

#Transfer the data to an excel sheet
setwd("O:/Tech_BCE/Air Quality Engineering/Picarro interferences (Anna&Pablo)/Picarro calibration/G2508 borrowed")
#write_xlsx(Dat,"2021_06_23_NH3.xlsx")


##N2O (10ppm in the cylinder)##
setwd("O:/Tech_BCE/Air Quality Engineering/Picarro interferences (Anna&Pablo)/Picarro calibration/G2508 borrowed")
list_of_files <- list.files(path="24",pattern = "*.dat",recursive=TRUE) ##Read data from Picarro
L<-length(list_of_files)

setwd("O:/Tech_BCE/Air Quality Engineering/Picarro interferences (Anna&Pablo)/Picarro calibration/G2508 borrowed/24")

###Read all Picarro from the folder
getstats<- function(Picarro){    #Open each Dat file in a list 
  
  listofdfs <- list() 
  
  for(i in 1:length(Picarro)){ #Loop through the numbers of ID's instead of the ID's
    
    
    
    Pic_Data<-read.table(list_of_files[i],header=TRUE)
    listofdfs[[i]] <- Pic_Data # save your dataframes into the list
  }
  
  return(listofdfs) #Return the list of dataframes.
}

Pic_id<- as.character(c(1:L))
Dat<-getstats(Picarro = Pic_id) #Able to call each Picarro file
Dat<-bind_rows(Dat, .id = "column_label") #Join all elements of the list in one dataframe

#Change format of date and time to be plotted
Dat$date.time<-paste(Dat$DATE,Dat$TIME)
Dat$date.time<-as.POSIXct(Dat$date.time,format="%Y-%m-%d %H:%M:%S")
#Select only N2O data
Rows<-1:nrow(Dat)
Dat<-cbind.data.frame(anytime(Dat$date.time),Rows,Dat$N2O)
#Transfer the data to an excel sheet
setwd("O:/Tech_BCE/Air Quality Engineering/Picarro interferences (Anna&Pablo)/Picarro calibration/G2508 borrowed")
write_xlsx(Dat,"2021_06_24_N2O.xlsx")

##CH4 (100ppm in the cylinder)##
setwd("O:/Tech_BCE/Air Quality Engineering/Picarro interferences (Anna&Pablo)/Picarro calibration/G2508 borrowed")
list_of_files <- list.files(path="30",pattern = "*.dat",recursive=TRUE) ##Read data from Picarro
L<-length(list_of_files)

setwd("O:/Tech_BCE/Air Quality Engineering/Picarro interferences (Anna&Pablo)/Picarro calibration/G2508 borrowed/30")

###Read all Picarro from the folder
getstats<- function(Picarro){    #Open each Dat file in a list 
  
  listofdfs <- list() 
  
  for(i in 1:length(Picarro)){ #Loop through the numbers of ID's instead of the ID's
    
    
    
    Pic_Data<-read.table(list_of_files[i],header=TRUE)
    listofdfs[[i]] <- Pic_Data # save your dataframes into the list
  }
  
  return(listofdfs) #Return the list of dataframes.
}

Pic_id<- as.character(c(1:L))
Dat<-getstats(Picarro = Pic_id) #Able to call each Picarro file
Dat<-bind_rows(Dat, .id = "column_label") #Join all elements of the list in one dataframe

#Change format of date and time to be plotted
Dat$date.time<-paste(Dat$DATE,Dat$TIME)
Dat$date.time<-as.POSIXct(Dat$date.time,format="%Y-%m-%d %H:%M:%S")
#Select only N2O data
Rows<-1:nrow(Dat)
Dat<-cbind.data.frame(anytime(Dat$date.time),Rows,Dat$CH4)
#Transfer the data to an excel sheet
setwd("O:/Tech_BCE/Air Quality Engineering/Picarro interferences (Anna&Pablo)/Picarro calibration/G2508 borrowed")
#write_xlsx(Dat,"2021_06_30_CH4.xlsx")

### Calibration of NH3 Picarro ###  1st ROUND

##NH3 (10ppm in the cylinder)##
setwd("O:/Tech_BCE/Air Quality Engineering/Picarro interferences (Anna&Pablo)/Picarro calibration/NH3 Picarro")
list_of_files <- list.files(path="23",pattern = "*.dat",recursive=TRUE) ##Read data from Picarro
L<-length(list_of_files)

setwd("O:/Tech_BCE/Air Quality Engineering/Picarro interferences (Anna&Pablo)/Picarro calibration/NH3 Picarro/23")

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
Dat<-getstats(Picarro = Pic_id) #Able to call each Picarro file
Dat<-bind_rows(Dat, .id = "column_label") #Join all elements of the list in one dataframe

#Change format of date and time to be plotted
Dat$date.time<-paste(Dat$DATE,Dat$TIME)
Dat$date.time<-as.POSIXct(Dat$date.time,format="%Y-%m-%d %H:%M:%S")
#Select only NH3 data
Rows<-1:nrow(Dat)
Dat<-cbind.data.frame(anytime(Dat$date.time),Rows,Dat$NH3_dry,Dat$NH3_Raw)

#Transfer the data to an excel sheet
setwd("O:/Tech_BCE/Air Quality Engineering/Picarro interferences (Anna&Pablo)/Picarro calibration/NH3 Picarro")
#write_xlsx(Dat,"2021_06_23_NH3.xlsx")

### Calibration of BackPack Picarro ###  1st ROUND

##CH4 (100ppm in the cylinder)##
setwd("O:/Tech_BCE/Air Quality Engineering/Picarro interferences (Anna&Pablo)/Picarro calibration/BackPack")
list_of_files <- list.files(path="30",pattern = "*.dat",recursive=TRUE) ##Read data from Picarro
L<-length(list_of_files)

setwd("O:/Tech_BCE/Air Quality Engineering/Picarro interferences (Anna&Pablo)/Picarro calibration/BackPack/30")

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
Dat<-getstats(Picarro = Pic_id) #Able to call each Picarro file
Dat<-bind_rows(Dat, .id = "column_label") #Join all elements of the list in one dataframe

#Change format of date and time to be plotted
Dat$date.time<-paste(Dat$DATE,Dat$TIME)
Dat$date.time<-as.POSIXct(Dat$date.time,format="%Y-%m-%d %H:%M:%S")
#Select only NH3 data
Rows<-1:nrow(Dat)
Dat<-cbind.data.frame(anytime(Dat$date.time),Rows,Dat$CH4_dry,Dat$CH4)

#Transfer the data to an excel sheet
setwd("O:/Tech_BCE/Air Quality Engineering/Picarro interferences (Anna&Pablo)/Picarro calibration/BackPack")
#write_xlsx(Dat,"2021_06_30_CH4.xlsx")

### Calibration of G2508 own ###   2nd ROUND

##NH3 (100ppm in the cylinder)##  
setwd("O:/Tech_BCE/Air Quality Engineering/Picarro interferences (Anna&Pablo)/Picarro calibration")
list_of_files <- list.files(path="G2508 own",pattern = "*.dat",recursive=TRUE) ##Read data from Picarro
L<-length(list_of_files)

setwd("O:/Tech_BCE/Air Quality Engineering/Picarro interferences (Anna&Pablo)/Picarro calibration/G2508 own")

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
Dat<-getstats(Picarro = Pic_id) #Able to call each Picarro file
Dat<-bind_rows(Dat, .id = "column_label") #Join all elements of the list in one dataframe

#Change format of date and time to be plotted
Dat$date.time<-paste(Dat$DATE,Dat$TIME)
Dat$date.time<-as.POSIXct(Dat$date.time,format="%Y-%m-%d %H:%M:%S")
#Select only NH3 data
Dat_NH3<-rbind.data.frame(Dat[40000:70000,],Dat[130000:160000,])
Rows<-1:nrow(Dat_NH3)
Dat_NH3<-cbind.data.frame(anytime(Dat_NH3$date.time),Rows,Dat_NH3$NH3)

#Transfer the data to an excel sheet
setwd("O:/Tech_BCE/Air Quality Engineering/Picarro interferences (Anna&Pablo)/Picarro calibration/G2508 own")
write_xlsx(Dat_NH3,"2021_07_08_NH3.xlsx")

##N2O (10ppm in the cylinder)##

#Select only N2O data
Dat_N2O<-rbind.data.frame(Dat[40000:45000,],Dat[163000:165000,])
Rows<-1:nrow(Dat_N2O)
Dat_N2O<-cbind.data.frame(anytime(Dat_N2O$date.time),Rows,Dat_N2O$N2O_dry,Dat_N2O$N2O)
#Transfer the data to an excel sheet
setwd("O:/Tech_BCE/Air Quality Engineering/Picarro interferences (Anna&Pablo)/Picarro calibration/G2508 own")
write_xlsx(Dat_N2O,"2021_07_08_N2O.xlsx")

##CH4 (100ppm in the cylinder)##

#Select only CH4 data
Dat_CH4<-Dat[30000:45000,]
Rows<-1:nrow(Dat_CH4)
Dat_CH4<-cbind.data.frame(anytime(Dat_CH4$date.time),Rows,Dat_CH4$CH4_dry,Dat_CH4$CH4)
#Transfer the data to an excel sheet
setwd("O:/Tech_BCE/Air Quality Engineering/Picarro interferences (Anna&Pablo)/Picarro calibration/G2508 own")
write_xlsx(Dat_CH4,"2021_07_08_CH4.xlsx")

### Calibration of BackPack Picarro ###  2nd ROUND

##CH4 (100ppm in the cylinder)##
setwd("O:/Tech_BCE/Air Quality Engineering/Picarro interferences (Anna&Pablo)/Picarro calibration/BackPack")
list_of_files <- list.files(path="08",pattern = "*.dat",recursive=TRUE) ##Read data from Picarro
L<-length(list_of_files)

setwd("O:/Tech_BCE/Air Quality Engineering/Picarro interferences (Anna&Pablo)/Picarro calibration/BackPack/08")

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
Dat<-getstats(Picarro = Pic_id) #Able to call each Picarro file
Dat<-bind_rows(Dat, .id = "column_label") #Join all elements of the list in one dataframe

#Change format of date and time to be plotted
Dat$date.time<-paste(Dat$DATE,Dat$TIME)
Dat$date.time<-as.POSIXct(Dat$date.time,format="%Y-%m-%d %H:%M:%S")
#Select only CH4 data
Rows<-1:nrow(Dat)
Dat<-cbind.data.frame(anytime(Dat$date.time),Rows,Dat$CH4_dry,Dat$CH4)

#Transfer the data to an excel sheet
setwd("O:/Tech_BCE/Air Quality Engineering/Picarro interferences (Anna&Pablo)/Picarro calibration/BackPack")
write_xlsx(Dat,"2021_07_08_CH4.xlsx")

### Calibration of NH3 Picarro ###  2nd ROUND

##NH3 (100ppm in the cylinder)##
setwd("O:/Tech_BCE/Air Quality Engineering/Picarro interferences (Anna&Pablo)/Picarro calibration/NH3 Picarro")
list_of_files <- list.files(path="08",pattern = "*.dat",recursive=TRUE) ##Read data from Picarro
L<-length(list_of_files)

setwd("O:/Tech_BCE/Air Quality Engineering/Picarro interferences (Anna&Pablo)/Picarro calibration/NH3 Picarro/08")

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
Dat<-getstats(Picarro = Pic_id) #Able to call each Picarro file
Dat<-bind_rows(Dat, .id = "column_label") #Join all elements of the list in one dataframe

#Change format of date and time to be plotted
Dat$date.time<-paste(Dat$DATE,Dat$TIME)
Dat$date.time<-as.POSIXct(Dat$date.time,format="%Y-%m-%d %H:%M:%S")
#Select only NH3 data
Rows<-1:nrow(Dat)
Dat<-cbind.data.frame(anytime(Dat$date.time),Rows,Dat$NH3_dry,Dat$NH3_Raw)

#Transfer the data to an excel sheet
setwd("O:/Tech_BCE/Air Quality Engineering/Picarro interferences (Anna&Pablo)/Picarro calibration/NH3 Picarro")
write_xlsx(Dat,"2021_07_08_NH3.xlsx")