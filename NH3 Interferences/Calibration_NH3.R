ibrary(lubridate)
library(ggplot2)
library(anytime)
library(dplyr)
library(writexl)

### Extract data of own G2509 ###

### 8th of July
setwd("...NH3 Interferences/G2508 own")
list_of_files <- list.files(path="08",pattern = "*.dat",recursive=TRUE) ##Read data from Picarro
L<-length(list_of_files)

setwd("...NH3 Interferences/G2508 own/08")

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
Dat_Own<-getstats(Picarro = Pic_id) #Able to call each Picarro file
Dat_Own<-bind_rows(Dat_Own, .id = "column_label") #Join all elements of the list in one dataframe

#Change format of date and time to be plotted
Dat_Own$date.time<-paste(Dat_Own$DATE,Dat_Own$TIME)
Dat_Own$date.time<-as.POSIXct(Dat_Own$date.time,format="%Y-%m-%d %H:%M:%S")

### 9th of July
setwd("...NH3 Interferences/G2508 own")
list_of_files <- list.files(path="09",pattern = "*.dat",recursive=TRUE) ##Read data from Picarro
L<-length(list_of_files)

setwd("...NH3 Interferences/G2508 own/09")

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
Dat_Own09<-getstats(Picarro = Pic_id) #Able to call each Picarro file
Dat_Own09<-bind_rows(Dat_Own09, .id = "column_label") #Join all elements of the list in one dataframe

#Change format of date and time to be plotted
Dat_Own09$date.time<-paste(Dat_Own09$DATE,Dat_Own09$TIME)
Dat_Own09$date.time<-as.POSIXct(Dat_Own09$date.time,format="%Y-%m-%d %H:%M:%S")

### Extract data of own G4301 ###

### 8th of July
setwd("...NH3 Interferences/BackPack")
list_of_files <- list.files(path="08",pattern = "*.dat",recursive=TRUE) ##Read data from Picarro
L<-length(list_of_files)

setwd("...NH3 Interferences/BackPack/08")

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

### 9th of July
setwd("...NH3 Interferences/BackPack")
list_of_files <- list.files(path="09",pattern = "*.dat",recursive=TRUE) ##Read data from Picarro
L<-length(list_of_files)

setwd("...NH3 Interferences/BackPack/09")

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
Dat_BP9<-getstats(Picarro = Pic_id) #Able to call each Picarro file
Dat_BP9<-bind_rows(Dat_BP9, .id = "column_label") #Join all elements of the list in one dataframe

#Change format of date and time to be plotted
Dat_BP9$date.time<-paste(Dat_BP9$DATE,Dat_BP9$TIME)
Dat_BP9$date.time<-as.POSIXct(Dat_BP9$date.time,format="%Y-%m-%d %H:%M:%S")

### Extract data from G2103 ###

### 9th of July 
setwd("...NH3 Interferences/NH3 Picarro")
list_of_files <- list.files(path="09",pattern = "*.dat",recursive=TRUE) ##Read data from Picarro
L<-length(list_of_files)

setwd("...NH3 Interferences/NH3 Picarro/09")
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


##G2508##

##Select N2O data##
Time_N<-Dat_Own$date.time[41000:45000]
N2O_full<-Dat_Own$N2O_dry[41000:45000]
Measured_N2O<-c(mean(Dat_Own$N2O_dry[42100:42400]),mean(Dat_Own$N2O_dry[42500:42690]),mean(Dat_Own$N2O_dry[42700:42770]),mean(Dat_Own$N2O_dry[42780:42860]),mean(Dat_Own$N2O_dry[42920:43170]),mean(Dat_Own$N2O_dry[43180:43340]),mean(Dat_Own$N2O_dry[43380:43550]),mean(Dat_Own$N2O_dry[43570:43680]),mean(Dat_Own$N2O_dry[43700:43930]),mean(Dat_Own$N2O_dry[43940:44040]),mean(Dat_Own$N2O_dry[44060:44180]),mean(Dat_Own$N2O_dry[44200:44280]),mean(Dat_Own$N2O_dry[44300:44400]),mean(Dat_Own$N2O_dry[44420:44540]),mean(Dat_Own$N2O_dry[44550:44670]),mean(Dat_Own$N2O_dry[44690:44870]),mean(Dat_Own09$N2O_dry[52590:52740]))
Sd_N2O<-c(sd(Dat_Own$N2O_dry[42100:42400]),sd(Dat_Own$N2O_dry[42500:42690]),sd(Dat_Own$N2O_dry[42700:42770]),sd(Dat_Own$N2O_dry[42780:42860]),sd(Dat_Own$N2O_dry[42920:43170]),sd(Dat_Own$N2O_dry[43180:43340]),sd(Dat_Own$N2O_dry[43380:43550]),sd(Dat_Own$N2O_dry[43570:43680]),sd(Dat_Own$N2O_dry[43700:43930]),sd(Dat_Own$N2O_dry[43940:44040]),sd(Dat_Own$N2O_dry[44060:44180]),sd(Dat_Own$N2O_dry[44200:44280]),sd(Dat_Own$N2O_dry[44300:44400]),sd(Dat_Own$N2O_dry[44420:44540]),sd(Dat_Own$N2O_dry[44550:44670]),sd(Dat_Own$N2O_dry[44690:44870]),sd(Dat_Own09$N2O_dry[52590:52740]))
N_points<-c(length(Dat_Own$N2O_dry[42100:42400]),length(Dat_Own$N2O_dry[42500:42690]),length(Dat_Own$N2O_dry[42700:42770]),length(Dat_Own$N2O_dry[42780:42860]),length(Dat_Own$N2O_dry[42920:43170]),length(Dat_Own$N2O_dry[43180:43340]),length(Dat_Own$N2O_dry[43380:43550]),length(Dat_Own$N2O_dry[43570:43680]),length(Dat_Own$N2O_dry[43700:43930]),length(Dat_Own$N2O_dry[43940:44040]),length(Dat_Own$N2O_dry[44060:44180]),length(Dat_Own$N2O_dry[44200:44280]),length(Dat_Own$N2O_dry[44300:44400]),length(Dat_Own$N2O_dry[44420:44540]),length(Dat_Own$N2O_dry[44550:44670]),length(Dat_Own$N2O_dry[44690:44870]),length(Dat_Own09$N2O_dry[52590:52740]))

##Select NH3 data##
Time_NH<-Dat_Own09$date.time[25000:47000]
NH3_full<-Dat_Own09$NH3[25000:47000]
Measured_NH3<-c(mean(Dat_Own09$NH3[25000:26500]),mean(Dat_Own09$NH3[34500:36280]),mean(Dat_Own09$NH3[36500:38180]),mean(Dat_Own09$NH3[38500:39360]),mean(Dat_Own09$NH3[39500:41360]),mean(Dat_Own09$NH3[41500:42970]),mean(Dat_Own09$NH3[43000:43470]),mean(Dat_Own09$NH3[43500:43880]),mean(Dat_Own09$NH3[43900:44190]),mean(Dat_Own09$NH3[44260:44450]),mean(Dat_Own09$NH3[44480:44750]),mean(Dat_Own09$NH3[44780:45070]),mean(Dat_Own09$NH3[45480:45550]))
Sd_NH3<-c(sd(Dat_Own09$NH3[25000:26500]),sd(Dat_Own09$NH3[34500:36280]),sd(Dat_Own09$NH3[36500:38180]),sd(Dat_Own09$NH3[38500:39360]),sd(Dat_Own09$NH3[39500:41360]),sd(Dat_Own09$NH3[41500:42970]),sd(Dat_Own09$NH3[43000:43470]),sd(Dat_Own09$NH3[43500:43880]),sd(Dat_Own09$NH3[43900:44190]),sd(Dat_Own09$NH3[44260:44450]),sd(Dat_Own09$NH3[44480:44750]),sd(Dat_Own09$NH3[44780:45070]),sd(Dat_Own09$NH3[45480:45550]))
NH_points<-c(length(Dat_Own09$NH3[25000:26500]),length(Dat_Own09$NH3[34500:36280]),length(Dat_Own09$NH3[36500:38180]),length(Dat_Own09$NH3[38500:39360]),length(Dat_Own09$NH3[39500:41360]),length(Dat_Own09$NH3[41500:42970]),length(Dat_Own09$NH3[43000:43470]),length(Dat_Own09$NH3[43500:43880]),length(Dat_Own09$NH3[43900:44190]),length(Dat_Own09$NH3[44260:44450]),length(Dat_Own09$NH3[44480:44750]),length(Dat_Own09$NH3[44780:45070]),length(Dat_Own09$NH3[45480:45550]))

##Select CH4 data##
Time_CH<-Dat_Own$date.time[33000:43000]
CH4_full<-Dat_Own$CH4_dry[33000:43000]
Measured_CH4<-c(mean(Dat_Own$CH4_dry[33000:34900]),mean(Dat_Own$CH4_dry[35050:35300]),mean(Dat_Own$CH4_dry[35610:35780]),
                mean(Dat_Own$CH4_dry[35810:35991]),mean(Dat_Own$CH4_dry[36010:36165]),mean(Dat_Own$CH4_dry[36210:36365]),
                mean(Dat_Own$CH4_dry[36380:36530]),mean(Dat_Own$CH4_dry[36540:36800]),mean(Dat_Own$CH4_dry[36820:36920]),
                mean(Dat_Own$CH4_dry[36930:37060]),
                mean(Dat_Own$CH4_dry[37085:37200]),mean(Dat_Own$CH4_dry[37240:37370]),mean(Dat_Own$CH4_dry[37390:37490]),
                mean(Dat_Own$CH4_dry[37590:37700]),mean(Dat_Own$CH4_dry[37730:37810]),mean(Dat_Own$CH4_dry[37832:37940]),
                mean(Dat_Own$CH4_dry[37952:38110]),mean(Dat_Own$CH4_dry[38130:38260]),mean(Dat_Own$CH4_dry[38285:38380]),
                mean(Dat_Own$CH4_dry[38395:38510]),mean(Dat_Own$CH4_dry[38525:38690]))
Sd_CH4<-c(sd(Dat_Own$CH4_dry[33000:34900]),sd(Dat_Own$CH4_dry[35050:35300]),sd(Dat_Own$CH4_dry[35610:35780]),
          sd(Dat_Own$CH4_dry[35810:35991]),sd(Dat_Own$CH4_dry[36010:36165]),sd(Dat_Own$CH4_dry[36210:36365]),
          sd(Dat_Own$CH4_dry[36380:36530]),sd(Dat_Own$CH4_dry[36540:36800]),sd(Dat_Own$CH4_dry[36820:36920]),
          sd(Dat_Own$CH4_dry[36930:37060]),
          sd(Dat_Own$CH4_dry[37085:37200]),sd(Dat_Own$CH4_dry[37240:37370]),sd(Dat_Own$CH4_dry[37390:37490]),
          sd(Dat_Own$CH4_dry[37590:37700]),sd(Dat_Own$CH4_dry[37730:37810]),sd(Dat_Own$CH4_dry[37832:37940]),
          sd(Dat_Own$CH4_dry[37952:38110]),sd(Dat_Own$CH4_dry[38130:38260]),sd(Dat_Own$CH4_dry[38285:38380]),
          sd(Dat_Own$CH4_dry[38395:38510]),sd(Dat_Own$CH4_dry[38525:38690]))
CH_points<-c(length(Dat_Own$CH4_dry[33000:34900]),length(Dat_Own$CH4_dry[35050:35300]),length(Dat_Own$CH4_dry[35610:35780]),
             length(Dat_Own$CH4_dry[35810:35991]),length(Dat_Own$CH4_dry[36010:36165]),length(Dat_Own$CH4_dry[36210:36365]),
             length(Dat_Own$CH4_dry[36380:36530]),length(Dat_Own$CH4_dry[36540:36800]),length(Dat_Own$CH4_dry[36820:36920]),
             length(Dat_Own$CH4_dry[36930:37060]),
             length(Dat_Own$CH4_dry[37085:37200]),length(Dat_Own$CH4_dry[37240:37370]),length(Dat_Own$CH4_dry[37390:37490]),
             length(Dat_Own$CH4_dry[37590:37700]),length(Dat_Own$CH4_dry[37730:37810]),length(Dat_Own$CH4_dry[37832:37940]),
             length(Dat_Own$CH4_dry[37952:38110]),length(Dat_Own$CH4_dry[38130:38260]),length(Dat_Own$CH4_dry[38285:38380]),
             length(Dat_Own$CH4_dry[38395:38510]),length(Dat_Own$CH4_dry[38525:38690]))

##G4301##
##Select CH4 data##
Time_BP<-Dat_BP$date.time[1000:7000]
BP_full<-Dat_BP$CH4_dry[1000:7000]
Measured_BP<-c(mean(Dat_BP$CH4_dry[1000:1800]),mean(Dat_BP$CH4_dry[1900:2100]),mean(Dat_BP$CH4_dry[2350:2440]),mean(Dat_BP$CH4_dry[2490:2600]),
               mean(Dat_BP$CH4_dry[2620:2725]),mean(Dat_BP$CH4_dry[2740:2875]),mean(Dat_BP$CH4_dry[2895:3000]),
               mean(Dat_BP$CH4_dry[3015:3200]),mean(Dat_BP$CH4_dry[3215:3280]),mean(Dat_BP$CH4_dry[3315:3390]),
               mean(Dat_BP$CH4_dry[3415:3500]),mean(Dat_BP$CH4_dry[3525:3620]),mean(Dat_BP$CH4_dry[3655:3715]),
               mean(Dat_BP$CH4_dry[3745:3865]),mean(Dat_BP$CH4_dry[3895:3950]),mean(Dat_BP$CH4_dry[3970:4040]),
               mean(Dat_BP$CH4_dry[4060:4170]),mean(Dat_BP$CH4_dry[4196:4270]),mean(Dat_BP$CH4_dry[4296:4360]),
               mean(Dat_BP$CH4_dry[4380:4459]),mean(Dat_BP$CH4_dry[4480:4595]))
Sd_BP<-c(sd(Dat_BP$CH4_dry[1000:1800]),sd(Dat_BP$CH4_dry[1900:2100]),sd(Dat_BP$CH4_dry[2350:2440]),sd(Dat_BP$CH4_dry[2490:2600]),
         sd(Dat_BP$CH4_dry[2620:2725]),sd(Dat_BP$CH4_dry[2740:2875]),sd(Dat_BP$CH4_dry[2895:3000]),
         sd(Dat_BP$CH4_dry[3015:3200]),sd(Dat_BP$CH4_dry[3215:3280]),sd(Dat_BP$CH4_dry[3315:3390]),
         sd(Dat_BP$CH4_dry[3415:3500]),sd(Dat_BP$CH4_dry[3525:3620]),sd(Dat_BP$CH4_dry[3655:3715]),
         sd(Dat_BP$CH4_dry[3745:3865]),sd(Dat_BP$CH4_dry[3895:3950]),sd(Dat_BP$CH4_dry[3970:4040]),
         sd(Dat_BP$CH4_dry[4060:4170]),sd(Dat_BP$CH4_dry[4196:4270]),sd(Dat_BP$CH4_dry[4296:4360]),
         sd(Dat_BP$CH4_dry[4380:4459]),sd(Dat_BP$CH4_dry[4480:4595]))
BP_points<-c(length(Dat_BP$CH4_dry[1000:1800]),length(Dat_BP$CH4_dry[1900:2100]),length(Dat_BP$CH4_dry[2350:2440]),length(Dat_BP$CH4_dry[2490:2600]),
             length(Dat_BP$CH4_dry[2620:2725]),length(Dat_BP$CH4_dry[2740:2875]),length(Dat_BP$CH4_dry[2895:3000]),
             length(Dat_BP$CH4_dry[3015:3200]),length(Dat_BP$CH4_dry[3215:3280]),length(Dat_BP$CH4_dry[3315:3390]),
             length(Dat_BP$CH4_dry[3415:3500]),length(Dat_BP$CH4_dry[3525:3620]),length(Dat_BP$CH4_dry[3655:3715]),
             length(Dat_BP$CH4_dry[3745:3865]),length(Dat_BP$CH4_dry[3895:3950]),length(Dat_BP$CH4_dry[3970:4040]),
             length(Dat_BP$CH4_dry[4060:4170]),length(Dat_BP$CH4_dry[4196:4270]),length(Dat_BP$CH4_dry[4296:4360]),
             length(Dat_BP$CH4_dry[4380:4459]),length(Dat_BP$CH4_dry[4480:4595]))
##G2103##
##Select NH3 data##
Time_NH32<-Dat_NH3$date.time[9000:20400]
NH3_full2<-Dat_NH3$NH3_dry[9000:20400]
Measured_NH32<-c(mean(Dat_NH3$NH3_dry[9000:10400]),mean(Dat_NH3$NH3_dry[14600:14740]),mean(Dat_NH3$NH3_dry[15400:15630]),mean(Dat_NH3$NH3_dry[15950:16160]),mean(Dat_NH3$NH3_dry[16670:17030]),mean(Dat_NH3$NH3_dry[17470:17700]),mean(Dat_NH3$NH3_dry[17790:17910]),mean(Dat_NH3$NH3_dry[17990:18070]),mean(Dat_NH3$NH3_dry[18140:18210]),mean(Dat_NH3$NH3_dry[18252:18283]),mean(Dat_NH3$NH3_dry[18334:18372]),mean(Dat_NH3$NH3_dry[18410:18466]),mean(Dat_NH3$NH3_dry[18570:18596]))
Sd_NH32<-c(sd(Dat_NH3$NH3_dry[9000:10400]),sd(Dat_NH3$NH3_dry[14600:14740]),sd(Dat_NH3$NH3_dry[15400:15630]),sd(Dat_NH3$NH3_dry[15950:16160]),sd(Dat_NH3$NH3_dry[16670:17030]),sd(Dat_NH3$NH3_dry[17470:17700]),sd(Dat_NH3$NH3_dry[17790:17910]),sd(Dat_NH3$NH3_dry[17990:18070]),sd(Dat_NH3$NH3_dry[18140:18210]),sd(Dat_NH3$NH3_dry[18252:18283]),sd(Dat_NH3$NH3_dry[18334:18372]),sd(Dat_NH3$NH3_dry[18410:18466]),sd(Dat_NH3$NH3_dry[18570:18596]))
NH_points2<-c(length(Dat_NH3$NH3_dry[9000:10400]),length(Dat_NH3$NH3_dry[14600:14740]),length(Dat_NH3$NH3_dry[15400:15630]),length(Dat_NH3$NH3_dry[15950:16160]),length(Dat_NH3$NH3_dry[16670:17030]),length(Dat_NH3$NH3_dry[17470:17700]),length(Dat_NH3$NH3_dry[17790:17910]),length(Dat_NH3$NH3_dry[17990:18070]),length(Dat_NH3$NH3_dry[18140:18210]),length(Dat_NH3$NH3_dry[18252:18283]),length(Dat_NH3$NH3_dry[18334:18372]),length(Dat_NH3$NH3_dry[18410:18466]),length(Dat_NH3$NH3_dry[18570:18596]))

##Potential interferences##
## For NH3 interferences on background N2O
Time_NH3i<-Dat_Own09$date.time[25000:47000]
NH3i_full<-Dat_Own09$N2O_dry[25000:47000]
Measured_N2O_dry<-c(mean(Dat_Own09$N2O_dry[25000:26500]),mean(Dat_Own09$N2O_dry[34500:36280]),mean(Dat_Own09$N2O_dry[36500:38180]),mean(Dat_Own09$N2O_dry[38500:39360]),mean(Dat_Own09$N2O_dry[39500:41360]),mean(Dat_Own09$N2O_dry[41500:42970]),mean(Dat_Own09$N2O_dry[43000:43470]),mean(Dat_Own09$N2O_dry[43500:43880]),mean(Dat_Own09$N2O_dry[43900:44190]),mean(Dat_Own09$N2O_dry[44260:44450]),mean(Dat_Own09$N2O_dry[44480:44750]),mean(Dat_Own09$N2O_dry[44780:45070]),mean(Dat_Own09$N2O_dry[45480:45550]))
Sd_N2O_dry<-c(sd(Dat_Own09$N2O_dry[25000:26500]),sd(Dat_Own09$N2O_dry[34500:36280]),sd(Dat_Own09$N2O_dry[36500:38180]),sd(Dat_Own09$N2O_dry[38500:39360]),sd(Dat_Own09$N2O_dry[39500:41360]),sd(Dat_Own09$N2O_dry[41500:42970]),sd(Dat_Own09$N2O_dry[43000:43470]),sd(Dat_Own09$N2O_dry[43500:43880]),sd(Dat_Own09$N2O_dry[43900:44190]),sd(Dat_Own09$N2O_dry[44260:44450]),sd(Dat_Own09$N2O_dry[44480:44750]),sd(Dat_Own09$N2O_dry[44780:45070]),sd(Dat_Own09$N2O_dry[45480:45550]))
NH3i_points<-c(length(Dat_Own09$N2O_dry[25000:26500]),length(Dat_Own09$N2O_dry[34500:36280]),length(Dat_Own09$N2O_dry[36500:38180]),length(Dat_Own09$N2O_dry[38500:39360]),length(Dat_Own09$N2O_dry[39500:41360]),length(Dat_Own09$N2O_dry[41500:42970]),length(Dat_Own09$N2O_dry[43000:43470]),length(Dat_Own09$N2O_dry[43500:43880]),length(Dat_Own09$N2O_dry[43900:44190]),length(Dat_Own09$N2O_dry[44260:44450]),length(Dat_Own09$N2O_dry[44480:44750]),length(Dat_Own09$N2O_dry[44780:45070]),length(Dat_Own09$N2O_dry[45480:45550]))

## For NH3 interferences on background CH4
##Syncronize
#G2508
Time_CHi<-Dat_Own09$date.time[25000:47000]
CH4i_full<-Dat_Own09$CH4_dry[25000:47000]
Measured_CH4i<-c(mean(Dat_Own09$CH4_dry[25000:26500]),mean(Dat_Own09$CH4_dry[34500:36280]),mean(Dat_Own09$CH4_dry[36500:38180]),mean(Dat_Own09$CH4_dry[38500:39360]),mean(Dat_Own09$CH4_dry[39500:41360]),mean(Dat_Own09$CH4_dry[41500:42970]),mean(Dat_Own09$CH4_dry[43000:43470]),mean(Dat_Own09$CH4_dry[43500:43880]),mean(Dat_Own09$CH4_dry[43900:44190]),mean(Dat_Own09$CH4_dry[44260:44450]),mean(Dat_Own09$CH4_dry[44480:44750]),mean(Dat_Own09$CH4_dry[44780:45070]),mean(Dat_Own09$CH4_dry[45480:45550]))
Sd_CH4i<-c(sd(Dat_Own09$CH4_dry[25000:26500]),sd(Dat_Own09$CH4_dry[34500:36280]),sd(Dat_Own09$CH4_dry[36500:38180]),sd(Dat_Own09$CH4_dry[38500:39360]),sd(Dat_Own09$CH4_dry[39500:41360]),sd(Dat_Own09$CH4_dry[41500:42970]),sd(Dat_Own09$CH4_dry[43000:43470]),sd(Dat_Own09$CH4_dry[43500:43880]),sd(Dat_Own09$CH4_dry[43900:44190]),sd(Dat_Own09$CH4_dry[44260:44450]),sd(Dat_Own09$CH4_dry[44480:44750]),sd(Dat_Own09$CH4_dry[44780:45070]),sd(Dat_Own09$CH4_dry[45480:45550]))
Time_sync<-c(Dat_Own09$date.time[c(25000,26500)],Dat_Own09$date.time[c(34500,36280)],Dat_Own09$date.time[c(36500,38180)],Dat_Own09$date.time[c(38500,39360)],Dat_Own09$date.time[c(39500,41360)],Dat_Own09$date.time[c(41500,42970)],Dat_Own09$date.time[c(43000,43470)],Dat_Own09$date.time[c(43500,43880)],Dat_Own09$date.time[c(43900,44190)],Dat_Own09$date.time[c(44260,44450)],Dat_Own09$date.time[c(44480,44750)],Dat_Own09$date.time[c(44780,45070)],Dat_Own09$date.time[c(45480,45550)],Dat_Own09$date.time[c(47000)])
adsf
#G4301
A<-list()
B<-1
C<-list()
for (i in 1:length(Time_sync)){
  A[[i]]<-round(as.numeric(anytime(Dat_BP9$date.time))-as.numeric(anytime(Time_sync[i])))
  B[i]<-min(abs(A[[i]]),na.rm=TRUE)
  C[[i]]<-which(abs(A[[i]])==B[i])
  C[i]<-(C[[i]][1])
}
C<-unlist(C)

T_eq<-Dat_BP9$date.time[C]
Time_align<-T_eq-minutes(62)

A<-list()
B<-1
C<-list()
for (i in 1:length(Time_align)){
  A[[i]]<-round(as.numeric(anytime(Dat_BP9$date.time))-as.numeric(anytime(Time_align[i])))
  B[i]<-min(abs(A[[i]]),na.rm=TRUE)
  C[[i]]<-which(abs(A[[i]])==B[i])
  C[i]<-(C[[i]][1])
}
C<-unlist(C)

Time_BPi<-Dat_BP9$date.time[C[1]:C[length(C)]]
BPi_full<-Dat_BP9$CH4_dry[C[1]:C[length(C)]]

Measured_BPi<-list()
Sd_BPi<-list()
for (i in 1:(length(Time_align)-1)){
  Measured_BPi[[i]]<-mean(Dat_BP9$CH4_dry[C[i]:C[i+1]])
  Sd_BPi[[i]]<-sd(Dat_BP9$CH4_dry[C[i]:C[i+1]])
}
Measured_BPi<-Measured_BPi[c(TRUE,FALSE)]
Sd_BPi<-Sd_BPi[c(TRUE,FALSE)]
Measured_BPi<-unlist(Measured_BPi)
Sd_BPi<-unlist(Sd_BPi)

##Join the data frames
N2O<-cbind.data.frame(Measured_N2O,Sd_N2O,N_points)
NH3<-cbind.data.frame(Measured_NH3,Sd_NH3,NH_points)
NH3i<-cbind.data.frame(Measured_N2O_dry,Sd_N2O_dry,Measured_NH3,Sd_NH3)
CH4<-cbind.data.frame(Measured_CH4,Sd_CH4,CH_points)
BP<-cbind.data.frame(Measured_BP,Sd_BP,BP_points)
NH32<-cbind.data.frame(Measured_NH32,Sd_NH32,NH_points2)
CH4i<-cbind.data.frame(Measured_CH4i,Sd_CH4i,Measured_BPi,Sd_BPi, Measured_NH3,Sd_NH3)


##Save file for NH3 interferences on N2O
setwd(".../NH3 Interferences")
write.table(NH3i, file = "NH3i_cal100.txt", sep = "\t", row.names = TRUE, col.names = NA)

##Save file for NH3 interferences on CH4
write.table(CH4i, file = "CH4i_cal100.txt", sep = "\t", row.names = TRUE, col.names = NA)

##Create an excel file
setwd("C:/Users/au615105/OneDrive - Aarhus universitet/Pablo Garc?a/Picarro interferences (Anna&Pablo)/Picarro calibration/Pictures")
write_xlsx(N2O,"NOc.xlsx")
write_xlsx(NH3,"NHc.xlsx")
write_xlsx(CH4,"CHc.xlsx")
write_xlsx(BP,"BP.xlsx")
write_xlsx(NH32,"NH2c.xlsx")