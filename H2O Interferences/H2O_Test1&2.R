library(dplyr)
library(ggplot2)
library(anytime)
library(lubridate)
library(writexl)


### Interferences of H2O###   

##Extract data from borrowed picarro##  
setwd(".../H2O Interferences") #Fullfil
list_of_files <- list.files(path="G2508 borrowed",pattern = "*.dat",recursive=TRUE) ##Read data from Picarro
L<-length(list_of_files)

setwd(".../H2O Interferences/G2508 borrowed")

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
setwd(".../H2O Interferences") 
list_of_files <- list.files(path="NH3 Picarro",pattern = "*.dat",recursive=TRUE) ##Read data from Picarro
L<-length(list_of_files)

setwd(".../H2O Interferences/NH3 Picarro")

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
setwd(".../H2O Interferences")
list_of_files <- list.files(path="BackPack",pattern = "*.dat",recursive=TRUE) ##Read data from Picarro
L<-length(list_of_files)

setwd(".../H2O Interferences/BackPack")

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


### Low to high water + backpack
Time_All_lowhigh<-c(Dat_All$date.time[130000:172120])
Time_BP_lowhigh<-c(Dat_BP$date.time[100000:122335])
Time_NH3_lowhigh<-c(Dat_NH3$date.time[55000:71776])
H2O_All_lowhigh<-c(Dat_All$H2O[130000:172120])
H2O_BP_lowhigh<-c(Dat_BP$H2O[100000:122335])
H2O_NH3_lowhigh<-c(Dat_NH3$H2O[55000:71776])
NH3_All_lowhigh<-c(Dat_All$NH3[130000:172120])
NH3r_NH3_lowhigh<-c(Dat_NH3$NH3_Raw[55000:71776])
NH3d_NH3_lowhigh<-c(Dat_NH3$NH3_dry[55000:71776])
CH4_All_lowhigh<-c(Dat_All$CH4_dry[130000:172120])
CH4_BP_lowhigh<-c(Dat_BP$CH4[100000:122335])
CH4d_BP_lowhigh<-c(Dat_BP$CH4_dry[100000:122335])
N2O_All_lowhigh<-c(Dat_All$N2O_dry[130000:172120])

##Same number of data in all water points
#H2O
Mean_H2O_All2<-c(mean(Dat_All$H2O[(150000-609):150000]),mean(Dat_All$H2O[161390:161999]),mean(Dat_All$H2O[(163050-609):163050]),mean(Dat_All$H2O[(166230-609):166230]),mean(Dat_All$H2O[(167795-609):167795]),mean(Dat_All$H2O[(169250-609):169250]),mean(Dat_All$H2O[(171120-609):171120]))
Mean_H2O_BP2<-c(mean(Dat_BP$H2O[(111000-435):111000]),mean(Dat_BP$H2O[114678:115113]),mean(Dat_BP$H2O[(115850-435):115850]),mean(Dat_BP$H2O[(118000-435):118000]),mean(Dat_BP$H2O[(119050-435):119050]),mean(Dat_BP$H2O[(120049-435):120049]),mean(Dat_BP$H2O[(121335-435):121335]))
Mean_H2O_NH32<-c(mean(Dat_NH3$H2O[(64000-263):64000]),mean(Dat_NH3$H2O[66589:66852]),mean(Dat_NH3$H2O[(67308-263):67308]),mean(Dat_NH3$H2O[(68675-263):68675]),mean(Dat_NH3$H2O[(69348-263):69348]),mean(Dat_NH3$H2O[(69972-263):69972]),mean(Dat_NH3$H2O[(70776-263):70776]))
sd_H2O_All2<-c(sd(Dat_All$H2O[(150000-609):150000]),sd(Dat_All$H2O[161390:161999]),sd(Dat_All$H2O[(163050-609):163050]),sd(Dat_All$H2O[(166230-609):166230]),sd(Dat_All$H2O[(167795-609):167795]),sd(Dat_All$H2O[(169250-609):169250]),sd(Dat_All$H2O[(171120-609):171120]))
sd_H2O_BP2<-c(sd(Dat_BP$H2O[(111000-435):111000]),sd(Dat_BP$H2O[114678:115113]),sd(Dat_BP$H2O[(115850-435):115850]),sd(Dat_BP$H2O[(118000-435):118000]),sd(Dat_BP$H2O[(119050-435):119050]),sd(Dat_BP$H2O[(120049-435):120049]),sd(Dat_BP$H2O[(121335-435):121335]))
sd_H2O_NH32<-c(sd(Dat_NH3$H2O[(64000-263):64000]),sd(Dat_NH3$H2O[66589:66852]),sd(Dat_NH3$H2O[(67308-263):67308]),sd(Dat_NH3$H2O[(68675-263):68675]),sd(Dat_NH3$H2O[(69348-263):69348]),sd(Dat_NH3$H2O[(69972-263):69972]),sd(Dat_NH3$H2O[(70776-263):70776]))
length_All2<-c(length(Dat_All$H2O[(150000-609):150000]),length(Dat_All$H2O[161390:161999]),length(Dat_All$H2O[(163050-609):163050]),length(Dat_All$H2O[(166230-609):166230]),length(Dat_All$H2O[(167795-609):167795]),length(Dat_All$H2O[(169250-609):169250]),length(Dat_All$H2O[(171120-609):171120]))
length_BP2<-c(length(Dat_BP$H2O[(111000-435):111000]),length(Dat_BP$H2O[114678:115113]),length(Dat_BP$H2O[(115850-435):115850]),length(Dat_BP$H2O[(118000-435):118000]),length(Dat_BP$H2O[(119050-435):119050]),length(Dat_BP$H2O[(120049-435):120049]),length(Dat_BP$H2O[(121335-435):121335]))
length_NH32<-c(length(Dat_NH3$H2O[(64000-263):64000]),length(Dat_NH3$H2O[66589:66852]),length(Dat_NH3$H2O[(67308-263):67308]),length(Dat_NH3$H2O[(68675-263):68675]),length(Dat_NH3$H2O[(69348-263):69348]),length(Dat_NH3$H2O[(69972-263):69972]),length(Dat_NH3$H2O[(70776-263):70776]))
Tinterval_All<-c(difftime(Dat_All$date.time[(150000-609)],Dat_All$date.time[150000]),difftime(Dat_All$date.time[161390],Dat_All$date.time[161999]),difftime(Dat_All$date.time[(163050-609)],Dat_All$date.time[163050]),difftime(Dat_All$date.time[(166230-609)],Dat_All$date.time[166230]),difftime(Dat_All$date.time[(167795-609)],Dat_All$date.time[167795]),difftime(Dat_All$date.time[(169250-609)],Dat_All$date.time[169250]),difftime(Dat_All$date.time[(171120-609)],Dat_All$date.time[171120]))
Tinterval_BP2<-c(difftime(Dat_BP$date.time[(111000-435)],Dat_BP$date.time[111000]),difftime(Dat_BP$date.time[114678],Dat_BP$date.time[115113]),difftime(Dat_BP$date.time[(115850-435)],Dat_BP$date.time[115850]),difftime(Dat_BP$date.time[(118000-435)],Dat_BP$date.time[118000]),difftime(Dat_BP$date.time[(119050-435)],Dat_BP$date.time[119050]),difftime(Dat_BP$date.time[(120049-435)],Dat_BP$date.time[120049]),difftime(Dat_BP$date.time[(121335-435)],Dat_BP$date.time[121335]))
Tinterval_NH32<-c(difftime(Dat_NH3$date.time[(64000-263)],Dat_NH3$date.time[64000]),difftime(Dat_NH3$date.time[66589],Dat_NH3$date.time[66852]),difftime(Dat_NH3$date.time[(67308-263)],Dat_NH3$date.time[67308]),difftime(Dat_NH3$date.time[(68675-263)],Dat_NH3$date.time[68675]),difftime(Dat_NH3$date.time[(69348-263)],Dat_NH3$date.time[69348]),difftime(Dat_NH3$date.time[(69972-263)],Dat_NH3$date.time[69972]),difftime(Dat_NH3$date.time[(70776-263)],Dat_NH3$date.time[70776]))
Tcritical_All<-c(Dat_All$date.time[(150000-609)],Dat_All$date.time[150000],Dat_All$date.time[161390],Dat_All$date.time[161999],Dat_All$date.time[(163050-609)],Dat_All$date.time[163050],Dat_All$date.time[(166230-609)],Dat_All$date.time[166230],Dat_All$date.time[(167795-609)],Dat_All$date.time[167795],Dat_All$date.time[(169250-609)],Dat_All$date.time[169250],Dat_All$date.time[(171120-609)],Dat_All$date.time[171120])
Tcritical_BP2<-c(Dat_BP$date.time[(111000-435)],Dat_BP$date.time[111000],Dat_BP$date.time[114678],Dat_BP$date.time[115113],Dat_BP$date.time[(115850-435)],Dat_BP$date.time[115850],Dat_BP$date.time[(118000-435)],Dat_BP$date.time[118000],Dat_BP$date.time[(119050-435)],Dat_BP$date.time[119050],Dat_BP$date.time[(120049-435)],Dat_BP$date.time[120049],Dat_BP$date.time[(121335-435)],Dat_BP$date.time[121335])
Tcritical_NH32<-c(Dat_NH3$date.time[(64000-263)],Dat_NH3$date.time[64000],Dat_NH3$date.time[66589],Dat_NH3$date.time[66852],Dat_NH3$date.time[(67308-263)],Dat_NH3$date.time[67308],Dat_NH3$date.time[(68675-263)],Dat_NH3$date.time[68675],Dat_NH3$date.time[(69348-263)],Dat_NH3$date.time[69348],Dat_NH3$date.time[(69972-263)],Dat_NH3$date.time[69972],Dat_NH3$date.time[(70776-263)],Dat_NH3$date.time[70776])
#NH3
Mean_NH3_All2<-c(mean(Dat_All$NH3[(150000-609):150000]),mean(Dat_All$NH3[161390:161999]),mean(Dat_All$NH3[(163050-609):163050]),mean(Dat_All$NH3[(166230-609):166230]),mean(Dat_All$NH3[(167795-609):167795]),mean(Dat_All$NH3[(169250-609):169250]),mean(Dat_All$NH3[(171120-609):171120]))
Mean_NH3_NH32<-c(mean(Dat_NH3$NH3_dry[(64000-263):64000]),mean(Dat_NH3$NH3_dry[66589:66852]),mean(Dat_NH3$NH3_dry[(67308-263):67308]),mean(Dat_NH3$NH3_dry[(68675-263):68675]),mean(Dat_NH3$NH3_dry[(69348-263):69348]),mean(Dat_NH3$NH3_dry[(69972-263):69972]),mean(Dat_NH3$NH3_dry[(70776-263):70776]))
sd_NH3_All2<-c(sd(Dat_All$NH3[(150000-609):150000]),sd(Dat_All$NH3[161390:161999]),sd(Dat_All$NH3[(163050-609):163050]),sd(Dat_All$NH3[(166230-609):166230]),sd(Dat_All$NH3[(167795-609):167795]),sd(Dat_All$NH3[(169250-609):169250]),sd(Dat_All$NH3[(171120-609):171120]))
sd_NH3_NH32<-c(sd(Dat_NH3$NH3_dry[(64000-263):64000]),sd(Dat_NH3$NH3_dry[66589:66852]),sd(Dat_NH3$NH3_dry[(67308-263):67308]),sd(Dat_NH3$NH3_dry[(68675-263):68675]),sd(Dat_NH3$NH3_dry[(69348-263):69348]),sd(Dat_NH3$NH3_dry[(69972-263):69972]),sd(Dat_NH3$NH3_dry[(70776-263):70776]))
#CH4
Mean_CH4_All2<-c(mean(Dat_All$CH4_dry[(150000-609):150000]),mean(Dat_All$CH4_dry[161390:161999]),mean(Dat_All$CH4_dry[(163050-609):163050]),mean(Dat_All$CH4_dry[(166230-609):166230]),mean(Dat_All$CH4_dry[(167795-609):167795]),mean(Dat_All$CH4_dry[(169250-609):169250]),mean(Dat_All$CH4_dry[(171120-609):171120]))
Mean_CH4_BP2<-c(mean(Dat_BP$CH4_dry[(111000-435):111000]),mean(Dat_BP$CH4_dry[114678:115113]),mean(Dat_BP$CH4_dry[(115850-435):115850]),mean(Dat_BP$CH4_dry[(118000-435):118000]),mean(Dat_BP$CH4_dry[(119050-435):119050]),mean(Dat_BP$CH4_dry[(120049-435):120049]),mean(Dat_BP$CH4_dry[(121335-435):121335]))
sd_CH4_All2<-c(sd(Dat_All$CH4_dry[(150000-609):150000]),sd(Dat_All$CH4_dry[161390:161999]),sd(Dat_All$CH4_dry[(163050-609):163050]),sd(Dat_All$CH4_dry[(166230-609):166230]),sd(Dat_All$CH4_dry[(167795-609):167795]),sd(Dat_All$CH4_dry[(169250-609):169250]),sd(Dat_All$CH4_dry[(171120-609):171120]))
sd_CH4_BP2<-c(sd(Dat_BP$CH4_dry[(111000-435):111000]),sd(Dat_BP$CH4_dry[114678:115113]),sd(Dat_BP$CH4_dry[(115850-435):115850]),sd(Dat_BP$CH4_dry[(118000-435):118000]),sd(Dat_BP$CH4_dry[(119050-435):119050]),sd(Dat_BP$CH4_dry[(120049-435):120049]),sd(Dat_BP$CH4_dry[(121335-435):121335]))
#N2O
Mean_N2O_All2<-c(mean(Dat_All$N2O_dry[(150000-609):150000]),mean(Dat_All$N2O_dry[161390:161999]),mean(Dat_All$N2O_dry[(163050-609):163050]),mean(Dat_All$N2O_dry[(166230-609):166230]),mean(Dat_All$N2O_dry[(167795-609):167795]),mean(Dat_All$N2O_dry[(169250-609):169250]),mean(Dat_All$N2O_dry[(171120-609):171120]))
sd_N2O_All2<-c(sd(Dat_All$N2O_dry[(150000-609):150000]),sd(Dat_All$N2O_dry[161390:161999]),sd(Dat_All$N2O_dry[(163050-609):163050]),sd(Dat_All$N2O_dry[(166230-609):166230]),sd(Dat_All$N2O_dry[(167795-609):167795]),sd(Dat_All$N2O_dry[(169250-609):169250]),sd(Dat_All$N2O_dry[(171120-609):171120]))

### High to low water NO backpack
Time_All_highlow<-c(Dat_All$date.time[180265:185750])
Time_NH3_highlow<-c(Dat_NH3$date.time[74716:77088])
H2O_All_highlow<-c(Dat_All$H2O[180265:185750])
H2O_NH3_highlow<-c(Dat_NH3$H2O[74716:77088])
NH3_All_highlow<-c(Dat_All$NH3[180265:185750])
NH3r_NH3_highlow<-c(Dat_NH3$NH3_Raw[74716:77088])
NH3d_NH3_highlow<-c(Dat_NH3$NH3_dry[74716:77088])
CH4_All_highlow<-c(Dat_All$CH4_dry[180265:185750])
N2O_All_highlow<-c(Dat_All$N2O_dry[180265:185750])

##Same number of data in all water points
#H2O
Mean_H2O_All3<-c(mean(Dat_All$H2O[(182760-370):182760]),mean(Dat_All$H2O[(183727-370):183727]),mean(Dat_All$H2O[(183735):184105]),mean(Dat_All$H2O[(184130):184500]),mean(Dat_All$H2O[(184910-370):184910]),mean(Dat_All$H2O[(185320-370):185320]),mean(Dat_All$H2O[(185750-370):185750]))
Mean_H2O_NH33<-c(mean(Dat_NH3$H2O[(75800-167):75800]),mean(Dat_NH3$H2O[(76210-167):76210]),mean(Dat_NH3$H2O[(76380-167):76380]),mean(Dat_NH3$H2O[(76560-167):76560]),mean(Dat_NH3$H2O[(76728-167):76728]),mean(Dat_NH3$H2O[(76900-167):76900]),mean(Dat_NH3$H2O[(77088-167):77088]))
sd_H2O_All3<-c(sd(Dat_All$H2O[(182760-370):182760]),sd(Dat_All$H2O[(183727-370):183727]),sd(Dat_All$H2O[(183735):184105]),sd(Dat_All$H2O[(184130):184500]),sd(Dat_All$H2O[(184910-370):184910]),sd(Dat_All$H2O[(185320-370):185320]),sd(Dat_All$H2O[(185750-370):185750]))
sd_H2O_NH33<-c(sd(Dat_NH3$H2O[(74716):75800]),sd(Dat_NH3$H2O[(75841):76210]),sd(Dat_NH3$H2O[(76213):76372]),sd(Dat_NH3$H2O[(76385):76541]),sd(Dat_NH3$H2O[(76561):76725]),sd(Dat_NH3$H2O[(76729):76900]),sd(Dat_NH3$H2O[(76909):77088]))
length_All3<-c(length(Dat_All$H2O[(182760-370):182760]),length(Dat_All$H2O[(183727-370):183727]),length(Dat_All$H2O[(183735):184105]),length(Dat_All$H2O[(184130):184500]),length(Dat_All$H2O[(184910-370):184910]),length(Dat_All$H2O[(185320-370):185320]),length(Dat_All$H2O[(185750-370):185750]))
length_NH33<-c(length(Dat_NH3$H2O[(75800-167):75800]),length(Dat_NH3$H2O[(76210-167):76210]),length(Dat_NH3$H2O[(76380-167):76380]),length(Dat_NH3$H2O[(76560-167):76560]),length(Dat_NH3$H2O[(76728-167):76728]),length(Dat_NH3$H2O[(76900-167):76900]),length(Dat_NH3$H2O[(77088-167):77088]))
Tinterval_All3<-c(difftime(Dat_All$date.time[(182760-370)],Dat_All$date.time[182760]),difftime(Dat_All$date.time[(183727-370)],Dat_All$date.time[183727]),difftime(Dat_All$date.time[(183735)],Dat_All$date.time[184105]),difftime(Dat_All$date.time[(184130)],Dat_All$date.time[184500]),difftime(Dat_All$date.time[(184910-370)],Dat_All$date.time[184910]),difftime(Dat_All$date.time[(185320-370)],Dat_All$date.time[185320]),difftime(Dat_All$date.time[(185750-370)],Dat_All$date.time[185750]))
Tinterval_NH33<-c(difftime(Dat_NH3$date.time[(75800-167)],Dat_NH3$date.time[75800]),difftime(Dat_NH3$date.time[(76210-167)],Dat_NH3$date.time[76210]),difftime(Dat_NH3$date.time[(76380-167)],Dat_NH3$date.time[76380]),difftime(Dat_NH3$date.time[(76560-167)],Dat_NH3$date.time[76560]),difftime(Dat_NH3$date.time[(76728-167)],Dat_NH3$date.time[76728]),difftime(Dat_NH3$date.time[(76900-167)],Dat_NH3$date.time[76900]),difftime(Dat_NH3$date.time[(77088-167)],Dat_NH3$date.time[77088]))
Tcritical_All3<-c(Dat_All$date.time[(182760-370)],Dat_All$date.time[182760],Dat_All$date.time[(183727-370)],Dat_All$date.time[183727],Dat_All$date.time[(183735)],Dat_All$date.time[184105],Dat_All$date.time[(184130)],Dat_All$date.time[184500],Dat_All$date.time[(184910-370)],Dat_All$date.time[184910],Dat_All$date.time[(185320-370)],Dat_All$date.time[185320],Dat_All$date.time[(185750-370)],Dat_All$date.time[185750])
Tcritical_NH33<-c(Dat_NH3$date.time[(75800-167)],Dat_NH3$date.time[75800],Dat_NH3$date.time[(76210-167)],Dat_NH3$date.time[76210],Dat_NH3$date.time[(76380-167)],Dat_NH3$date.time[76380],Dat_NH3$date.time[(76560-167)],Dat_NH3$date.time[76560],Dat_NH3$date.time[(76728-167)],Dat_NH3$date.time[76728],Dat_NH3$date.time[(76900-167)],Dat_NH3$date.time[76900],Dat_NH3$date.time[(77088-167)],Dat_NH3$date.time[77088])
#NH3
Mean_NH3_All3<-c(mean(Dat_All$NH3[(182760-370):182760]),mean(Dat_All$NH3[(183727-370):183727]),mean(Dat_All$NH3[(183735):184105]),mean(Dat_All$NH3[(184130):184500]),mean(Dat_All$NH3[(184910-370):184910]),mean(Dat_All$NH3[(185320-370):185320]),mean(Dat_All$NH3[(185750-370):185750]))
Mean_NH3_NH33<-c(mean(Dat_NH3$NH3_dry[(75800-167):75800]),mean(Dat_NH3$NH3_dry[(76210-167):76210]),mean(Dat_NH3$NH3_dry[(76380-167):76380]),mean(Dat_NH3$NH3_dry[(76560-167):76560]),mean(Dat_NH3$NH3_dry[(76728-167):76728]),mean(Dat_NH3$NH3_dry[(76900-167):76900]),mean(Dat_NH3$NH3_dry[(77088-167):77088]))
sd_NH3_All3<-c(sd(Dat_All$NH3[(182760-370):182760]),sd(Dat_All$NH3[(183727-370):183727]),sd(Dat_All$NH3[(183735):184105]),sd(Dat_All$NH3[(184130):184500]),sd(Dat_All$NH3[(184910-370):184910]),sd(Dat_All$NH3[(185320-370):185320]),sd(Dat_All$NH3[(185750-370):185750]))
sd_NH3_NH33<-c(sd(Dat_NH3$NH3_dry[(74716):75800]),sd(Dat_NH3$NH3_dry[(75841):76210]),sd(Dat_NH3$NH3_dry[(76213):76372]),sd(Dat_NH3$NH3_dry[(76385):76541]),sd(Dat_NH3$NH3_dry[(76561):76725]),sd(Dat_NH3$NH3_dry[(76729):76900]),sd(Dat_NH3$NH3_dry[(76909):77088]))
#CH4
Mean_CH4_All3<-c(mean(Dat_All$CH4_dry[(182760-370):182760]),mean(Dat_All$CH4_dry[(183727-370):183727]),mean(Dat_All$CH4_dry[(183735):184105]),mean(Dat_All$CH4_dry[(184130):184500]),mean(Dat_All$CH4_dry[(184910-370):184910]),mean(Dat_All$CH4_dry[(185320-370):185320]),mean(Dat_All$CH4_dry[(185750-370):185750]))
sd_CH4_All3<-c(sd(Dat_All$CH4_dry[(182760-370):182760]),sd(Dat_All$CH4_dry[(183727-370):183727]),sd(Dat_All$CH4_dry[(183735):184105]),sd(Dat_All$CH4_dry[(184130):184500]),sd(Dat_All$CH4_dry[(184910-370):184910]),sd(Dat_All$CH4_dry[(185320-370):185320]),sd(Dat_All$CH4_dry[(185750-370):185750]))
#N2O
Mean_N2O_All3<-c(mean(Dat_All$N2O_dry[(182760-370):182760]),mean(Dat_All$N2O_dry[(183727-370):183727]),mean(Dat_All$N2O_dry[(183735):184105]),mean(Dat_All$N2O_dry[(184130):184500]),mean(Dat_All$N2O_dry[(184910-370):184910]),mean(Dat_All$N2O_dry[(185320-370):185320]),mean(Dat_All$N2O_dry[(185750-370):185750]))
sd_N2O_All3<-c(sd(Dat_All$N2O_dry[(182760-370):182760]),sd(Dat_All$N2O_dry[(183727-370):183727]),sd(Dat_All$N2O_dry[(183735):184105]),sd(Dat_All$N2O_dry[(184130):184500]),sd(Dat_All$N2O_dry[(184910-370):184910]),sd(Dat_All$N2O_dry[(185320-370):185320]),sd(Dat_All$N2O_dry[(185750-370):185750]))

##Save the data
#Means
df_control<-cbind.data.frame(Mean_H2O_All2,sd_H2O_All2,Mean_NH3_All2,sd_NH3_All2,Mean_CH4_All2,sd_CH4_All2,
                             Mean_N2O_All2,sd_N2O_All2,length_All2,Tinterval_All,
                             Mean_H2O_NH32,sd_H2O_NH32,Mean_NH3_NH32,sd_NH3_NH32,
                             length_NH32,Tinterval_NH32,
                             Mean_H2O_BP2,sd_H2O_BP2,Mean_CH4_BP2,sd_CH4_BP2,
                             length_BP2,Tinterval_BP2,Mean_H2O_All3,sd_H2O_All3,Mean_NH3_All3,sd_NH3_All3,Mean_CH4_All3,sd_CH4_All3,
                             Mean_N2O_All3,sd_N2O_All3,length_All3,Tinterval_All3,
                             Mean_H2O_NH33,sd_H2O_NH33,Mean_NH3_NH33,sd_NH3_NH33,
                             length_NH33,Tinterval_NH33)
#Raw data

n <- max(length(Time_All_lowhigh), length(Time_BP_lowhigh), length(Time_NH3_lowhigh)) #All same length
length(Time_BP_lowhigh) <- n                      
length(Time_NH3_lowhigh) <- n
length(H2O_BP_lowhigh) <- n                      
length(H2O_NH3_lowhigh) <- n
length(CH4d_BP_lowhigh) <- n                      
length(NH3d_NH3_lowhigh) <- n #Trick to make it all the same length
#Low to high water
df_Allpoints_lh<-cbind.data.frame(Time_All_lowhigh,Time_BP_lowhigh,Time_NH3_lowhigh,
                                  H2O_All_lowhigh,H2O_BP_lowhigh,H2O_NH3_lowhigh,
                                  NH3_All_lowhigh,NH3d_NH3_lowhigh,CH4_All_lowhigh,
                                  CH4d_BP_lowhigh,N2O_All_lowhigh)
n <- max(length(Time_All_highlow), length(Time_NH3_highlow)) #All same length
length(Time_NH3_highlow) <- n
length(H2O_NH3_highlow) <- n
length(NH3d_NH3_highlow) <- n #Trick to make it all the same length
#Low to high water
df_Allpoints_hl<-cbind.data.frame(Time_All_highlow,Time_NH3_highlow,
                                  H2O_All_highlow,H2O_NH3_highlow,
                                  NH3_All_highlow,NH3d_NH3_highlow,CH4_All_highlow,
                                  N2O_All_highlow)
df_Tcritical<-cbind.data.frame(Tcritical_All,Tcritical_All3,Tcritical_BP2,Tcritical_NH32,Tcritical_NH33)
setwd(".../H2O Interferences") 

## Treated text files to be read
write.table(df_control, file = "Control_H2O interferences.txt", sep = "\t", row.names = TRUE, col.names = NA)
write.table(df_Allpoints_lh, file = "All_H2O interferences_lh.txt", sep = "\t", row.names = TRUE, col.names = NA)
write.table(df_Allpoints_hl, file = "All_H2O interferences_hl.txt", sep = "\t", row.names = TRUE, col.names = NA)
write.table(df_Tcritical, file = "Tcritical.txt", sep = "\t", row.names = TRUE, col.names = NA)

##Read Previous files
setwd(".../H2O Interferences")
df_H2O_means<-read.table(file="control_H2O interferences.txt",header=TRUE, sep = "\t")
df_H2O_low<-read.table(file="All_H2O interferences_lh.txt",header=TRUE, sep = "\t")
df_H2O_high<-read.table(file="All_H2O interferences_hl.txt",header=TRUE, sep = "\t")
df_Tcritical<-read.table(file="Tcritical.txt",header=TRUE, sep = "\t")
df_N2O_means_rep<-read.table(file="H2O_N2O.txt",header=TRUE, sep = "\t")
df_NH3_means_rep<-read.table(file="H2O_NH3.txt",header=TRUE, sep = "\t")
df_CH4_means_rep<-read.table(file="H2O_CH4.txt",header=TRUE, sep = "\t")

df_H2O_low$Time_All_lowhigh<-anytime(df_H2O_low$Time_All_lowhigh)
df_H2O_low$Time_BP_lowhigh<-anytime(df_H2O_low$Time_BP_lowhigh)
df_H2O_high$Time_All_highlow<-anytime(df_H2O_high$Time_All_highlow)
df_H2O_low$Time_NH3_lowhigh<-anytime(df_H2O_low$Time_NH3_lowhigh)
df_H2O_high$Time_NH3_highlow<-anytime(df_H2O_high$Time_NH3_highlow)

x_T<-1:length(df_H2O_low$Time_All_lowhigh) ##Needed to make the polynomic function work
x_T1<-1:length(df_H2O_high$Time_All_highlow) ##Needed to make the polynomic function work
x_T2<-1:length(df_H2O_low$Time_BP_lowhigh) ##Needed to make the polynomic function work


### Detrend CH4 ###
## ~G2509 ## 
## Low-->High
CH4_All_low<-df_H2O_low$CH4_All_lowhigh[10000:42121] #So polynomic function fits better (delete first 10000 points of background)
Eq_Clow_dt<-lm(formula = CH4_All_low ~ poly(as.numeric(x_T[10000:42121]),18,raw=TRUE)) #polynomial
Coeff_dt<-coefficients(Eq_Clow_dt)
CH4_All_low_detrend<-CH4_All_low-predict(Eq_Clow_dt)
CH4_All_low_detrend<-CH4_All_low_detrend+mean(CH4_All_low)
#Control plot
##Duration vector##
d_Alldt<-1
for (i in 1:length(df_H2O_low$Time_All_lowhigh[10000:42121])){
  d_Alldt[i]<-difftime(df_H2O_low$Time_All_lowhigh[9999+i],df_H2O_low$Time_All_lowhigh[10000],units="mins")
}
df_H2O_low$d_Alldt[10000:42121]<-d_Alldt
###################
Cp1<-ggplot(data=df_H2O_low[10000:42121,])+geom_point(aes(d_Alldt,CH4_All_lowhigh,color="original"),shape=1)+
  geom_line(aes(d_Alldt,predict(Eq_Clow_dt),color="Fit"),size=1)+
  geom_point(aes(d_Alldt,CH4_All_lowhigh-predict(Eq_Clow_dt)+mean(CH4_All_lowhigh),color="detrended"),shape=1)+
  theme(panel.border =element_rect(colour = "grey", fill=NA))+
  theme(axis.line = element_line(colour = "black"))+
  theme(panel.spacing.x=unit(2, "lines"))+
  theme_bw()+theme(panel.grid=element_blank())+
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        legend.text=element_text(size=6),
        legend.title=element_blank(),
        legend.position = c(0.15,0.8))+
  xlab("")+
  scale_color_manual("",values=c("grey1","blue","grey50"),
                     guide = guide_legend(override.aes = list(
                       linetype = c("blank", "solid", "blank"),
                       shape = c(1, NA, 1))))+
  scale_y_continuous(name = expression(paste("CH"[4]* " concentration, ppm")))
#Cp1<-tag_facet(Cp1,tag_pool = letters[1])
ggsave(plot=Cp1,"Detrend control_CH4-H2O_18low.png", width = 8, height = 5)

## High-->Low
Eq_Chigh_dt<-lm(formula = df_H2O_high$CH4_All_highlow ~ poly(as.numeric(x_T1),18,raw=TRUE)) #Linear but grade 18 for same fit as before
Coeff_dt<-coefficients(Eq_Chigh_dt)
CH4_All_high_detrend<-df_H2O_high$CH4_All_highlow-predict(Eq_Chigh_dt)
CH4_All_high_detrend<-CH4_All_high_detrend+mean(df_H2O_high$CH4_All_highlow)

#Control plot
##Duration vector##
d_Alldt2<-1
for (i in 1:length(df_H2O_high$Time_All_highlow)){
  d_Alldt2[i]<-difftime(df_H2O_high$Time_All_highlow[0+i],df_H2O_high$Time_All_highlow[1],units="mins")
}
df_H2O_high$d_Alldt2<-d_Alldt2
###################
Cp2<-ggplot(data=df_H2O_high)+geom_point(aes(d_Alldt2,CH4_All_highlow,color="original"),shape=1)+
  geom_line(aes(d_Alldt2,predict(Eq_Chigh_dt),color="Fit"),size=1)+
  geom_point(aes(d_Alldt2,CH4_All_highlow-predict(Eq_Chigh_dt)+mean(CH4_All_highlow),color="detrended"),shape=1)+
  theme(panel.border =element_rect(colour = "grey", fill=NA))+
  theme(axis.line = element_line(colour = "black"))+
  theme(panel.spacing.x=unit(2, "lines"))+
  theme_bw()+theme(panel.grid=element_blank())+
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        legend.text=element_text(size=6),
        legend.title=element_text(size=8),
        legend.position = "none")+
  xlab("Time (min)")+
  scale_color_manual("",values=c("grey1","blue","grey50"),
                     guide = guide_legend(override.aes = list(
                       linetype = c("blank", "solid", "blank"),
                       shape = c(1, NA, 1))))+
  scale_y_continuous(name = expression(paste("CH"[4]* " concentration, ppm")),labels = number_format(accuracy = 0.01))
#Cp2<-tag_facet(Cp2,tag_pool = letters[3])

ggsave(plot=Cp2,"Detrend control_CH4-H2O_18high.png", width = 8, height = 5)

## ~G4301 ## 
## Low-->High
CH4_BP_low<-df_H2O_low$CH4d_BP_lowhigh #So polynomic function fits better (delete first 10000 points of background)
Eq_CBP_dt<-lm(formula = CH4_BP_low ~ poly(as.numeric(x_T2),18,raw=TRUE)) #polynomial
Coeff_dt<-coefficients(Eq_CBP_dt)
CH4_All_Mlcontrol<-CH4_BP_low[1:22336]-predict(Eq_CBP_dt)
CH4_All_Mlcontrol<-CH4_All_Mlcontrol+mean(CH4_BP_low,na.rm=TRUE)
##Duration vector##
d_BPdt<-1
for (i in 1:length(df_H2O_low$Time_BP_lowhigh)){
  d_BPdt[i]<-difftime(df_H2O_low$Time_BP_lowhigh[0+i],df_H2O_low$Time_BP_lowhigh[1],units="mins")
}
df_H2O_low$d_BPdt<-d_BPdt
###################
#Control plot
CpBP<-ggplot(data=df_H2O_low[1:22336,])+geom_point(aes(d_BPdt,CH4d_BP_lowhigh,color="original"),shape=1)+
  geom_line(aes(d_BPdt,predict(Eq_CBP_dt),color="Fit"),size=1)+
  geom_point(aes(d_BPdt,CH4d_BP_lowhigh-predict(Eq_CBP_dt)+mean(CH4d_BP_lowhigh),color="detrended"),shape=1)+
  theme(panel.border =element_rect(colour = "grey", fill=NA))+
  theme(axis.line = element_line(colour = "black"))+
  theme(panel.spacing.x=unit(2, "lines"))+
  theme_bw()+theme(panel.grid=element_blank())+
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        legend.text=element_text(size=6),
        legend.title=element_text(size=8),
        legend.position = "none")+
  xlab("")+
  scale_color_manual("",values=c("grey1","blue","grey50"),
                     guide = guide_legend(override.aes = list(
                       linetype = c("blank", "solid", "blank"),
                       shape = c(1, NA, 1))))+
  scale_y_continuous(name = expression(paste("")))
#CpBP<-tag_facet(CpBP,tag_pool = letters[2])
ggsave(plot=CpBP,"Detrend control_CH4-H2O_18low_BP.png", width = 8, height = 5)

### New CH4 values ###
## Detect same intervals

#Backpack Low-->High
A<-list()
for (i in 1:length(df_Tcritical$Tcritical_BP2)){
  A[[i]]<-which(df_H2O_low$Time_BP_lowhigh==df_Tcritical$Tcritical_BP2[i])
}
A<-unlist(A)
C<-1
Sd<-1
Mean<-1
Std<-1
for (i in 1:13){
  C[i]<-mean(df_H2O_low$CH4d_BP_lowhigh[A[i]:A[i+1]])
  Sd[i]<-sd(df_H2O_low$CH4d_BP_lowhigh[A[i]:A[i+1]])
  Mean[i]<-mean(CH4_All_Mlcontrol[A[i]:A[i+1]])
  Std[i]<-sd(CH4_All_Mlcontrol[A[i]:A[i+1]])
}
CH4_BP_Mcontrol<-C[c(TRUE,FALSE)] #Only odd values from vector are correct
CH4_BP_Sdcontrol<-Sd[c(TRUE,FALSE)] #Only odd values from vector are correct
CH4_BP_Mean_detrend<-Mean[c(TRUE,FALSE)] #Only odd values from vector are correct
CH4_BP_Sd_detrend<-Std[c(TRUE,FALSE)] #Only odd values from vector are correct

#G2509 Low-->High
A<-list()
for (i in 1:length(df_Tcritical$Tcritical_All)){
  A[[i]]<-which(df_H2O_low$Time_All_lowhigh==df_Tcritical$Tcritical_All[i])
}
A<-unlist(A)
A<-A[-c(1,4,5,7,12,14,15,18,19,22,24,26)]
A<-A-9999
C<-1
Length<-1
for (i in 1:13){
  C[i]<-mean(CH4_All_low[A[i]:A[i+1]])
  Sd[i]<-sd(CH4_All_low[A[i]:A[i+1]])
  Length[i]<-length(CH4_All_low[A[i]:A[i+1]])
  Mean[i]<-mean(CH4_All_low_detrend[A[i]:A[i+1]])
  Std[i]<-sd(CH4_All_low_detrend[A[i]:A[i+1]])
}
CH4_All_Mlcontrol<-C[c(TRUE,FALSE)] #Only odd values from vector are correct
CH4_All_SDlcontrol<-Sd[c(TRUE,FALSE)] #Only odd values from vector are correct
CH4_All_Low_Mean_detrend<-Mean[c(TRUE,FALSE)] #Only odd values from vector are correct
CH4_All_Low_Sd_detrend<-Std[c(TRUE,FALSE)] #Only odd values from vector are correct
Length_Control<-Length[c(TRUE,FALSE)]

#G2509 High-->Low
A<-list()
for (i in 1:length(df_Tcritical$Tcritical_All3)){
  A[[i]]<-which(df_H2O_high$Time_All_highlow==df_Tcritical$Tcritical_All3[i])
}
A<-unlist(A)
A<-A[-c(1,4,6,8,10,12,15,16,22)]

C<-1
H<-1
for (i in 1:13){
  C[i]<-mean(df_H2O_high$CH4_All_highlow[A[i]:A[i+1]])
  H[i]<-mean(df_H2O_high$H2O_All_highlow[A[i]:A[i+1]])
  Sd[i]<-sd(df_H2O_high$CH4_All_highlow[A[i]:A[i+1]])
  Length[i]<-length(df_H2O_high$CH4_All_highlow[A[i]:A[i+1]])
  Mean[i]<-mean(CH4_All_high_detrend[A[i]:A[i+1]])
  Std[i]<-sd(CH4_All_high_detrend[A[i]:A[i+1]])
}
CH4_All_Mhcontrol<-C[c(TRUE,FALSE)] #Only odd values from vector are correct
CH4_All_SDhcontrol<-Sd[c(TRUE,FALSE)] #Only odd values from vector are correct
CH4_All_High_Mean_detrend<-Mean[c(TRUE,FALSE)] #Only odd values from vector are correct
CH4_All_High_Sd_detrend<-Std[c(TRUE,FALSE)] #Only odd values from vector are correct
Length_Control<-Length[c(TRUE,FALSE)]

### Control that the intervals are the same ###
Ctrend<-round(sum(CH4_BP_Mcontrol-df_H2O_means$Mean_CH4_BP2,CH4_All_Mhcontrol-df_H2O_means$Mean_CH4_All3,
                  CH4_All_Mlcontrol-df_H2O_means$Mean_CH4_All2),10)

### Create data frames for main ###
## Creating data frames ##
##NH3
x<-c(df_H2O_means$Mean_H2O_All2,df_H2O_means$Mean_H2O_NH32,df_H2O_means$Mean_H2O_All3,df_H2O_means$Mean_H2O_NH33,df_NH3_means_rep$x)
y<-c(df_H2O_means$Mean_NH3_All2,df_H2O_means$Mean_NH3_NH32,df_H2O_means$Mean_NH3_All3,df_H2O_means$Mean_NH3_NH33,df_NH3_means_rep$y)
sd_N<-c(df_H2O_means$sd_NH3_All2,df_H2O_means$sd_NH3_NH32,df_H2O_means$sd_NH3_All3,df_H2O_means$sd_NH3_NH33,df_NH3_means_rep$sd_NH3)
sd_H<-c(df_H2O_means$sd_H2O_All2,df_H2O_means$sd_H2O_NH32,df_H2O_means$sd_H2O_All3,df_H2O_means$sd_H2O_NH33,df_NH3_means_rep$sd_H2O)
Type<-c(rep("G2509",7),rep("G2103",7),rep("G2509",7),rep("G2103",7),rep("G2509",21))
Test<-c(rep("Test 1",7),rep("Test 1",7),rep("Test 2",7),rep("Test 2",7),rep("Test 3*",21))
df_NH3<-cbind.data.frame(x,y,Type,Test)

##N2O
x<-c(df_H2O_means$Mean_H2O_All2,df_H2O_means$Mean_H2O_All3,df_N2O_means_rep$x)
y<-c(df_H2O_means$Mean_N2O_All2*1000,df_H2O_means$Mean_N2O_All3*1000,df_N2O_means_rep$y*1000)
sd_NO<-c(df_H2O_means$sd_N2O_All2*1000,df_H2O_means$sd_N2O_All3*1000,df_N2O_means_rep$sd_N2O_dry*1000)
sd_HNO<-c(df_H2O_means$sd_H2O_All2,df_H2O_means$sd_H2O_All3,df_N2O_means_rep$sd_H2O)
Type<-c(rep("G2509",14),rep("G2509",21))
Test<-c(rep("Test 1",7),rep("Test 2",7),rep("Test 3*",21))
df_N2O<-cbind.data.frame(x,y,Type,Test)

##CH4
x<-c(df_H2O_means$Mean_H2O_All2,df_H2O_means$Mean_H2O_BP2,df_H2O_means$Mean_H2O_All3,df_CH4_means_rep$x)
y<-c(CH4_All_Low_Mean_detrend,CH4_BP_Mean_detrend,CH4_All_High_Mean_detrend,df_CH4_means_rep$y)
sd_C<-c(CH4_All_Low_Sd_detrend,CH4_BP_Sd_detrend,CH4_All_High_Sd_detrend,df_CH4_means_rep$sd_CH4_dry)
sd_HC<-c(df_H2O_means$sd_H2O_All2,df_H2O_means$sd_H2O_BP2,df_H2O_means$sd_H2O_All3,df_CH4_means_rep$sd_H2O)
Type<-c(rep("G2509",7),rep("G4301",7),rep("G2509",7),rep("G2509",21))
Test<-c(rep("Test 1",7),rep("Test 1",7),rep("Test 2",7),rep("Test 3*",21))
df_CH4<-cbind.data.frame(x,y,Type,Test)

## Main material ##
library(egg)

#NH3 ~ H2O
pNH3<-ggplot(data=df_NH3,aes(x,y,color=Test,shape=Type))+
  geom_point(size=3)+
  theme_bw()+theme(panel.grid=element_blank())+
  theme(panel.border =element_rect(colour = "grey", fill=NA))+
  theme(axis.line = element_line(colour = "black"))+
  theme(panel.spacing.x=unit(2, "lines"))+
  geom_errorbar(aes(ymin=y-sd_N, ymax=y+sd_N), 
                width=0.1,position=position_dodge(0))+
  geom_errorbar(aes(xmin=x-sd_H, xmax=x+sd_H), 
                width=0.1,position=position_dodge(0))+
  scale_y_continuous(name = expression(NH[3]~"Concentration (ppb)"))+
  scale_x_continuous(name = expression(H[2]*O~"Concentration (%)"),
                     breaks = seq(0, 3, 0.5))+
  scale_colour_manual("",values=c("red","darkgreen","dodgerblue"))+
  scale_shape_manual("CRDS",values=c(1,2))+
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=10),
        legend.text=element_text(size=6),
        legend.title=element_text(size=10),
        legend.position = "none")
pNH3<-tag_facet(pNH3)

ggsave(plot=pNH3,"H2O vs NH3.png", width = 8, height = 5)


#N2O ~ H2O
pN2O<-ggplot(data=df_N2O,aes(x,y,color=Test,shape=Type))+
  geom_point(size=3)+
  theme_bw()+theme(panel.grid=element_blank())+
  theme(panel.border =element_rect(colour = "grey", fill=NA))+
  theme(axis.line = element_line(colour = "black"))+
  theme(panel.spacing.x=unit(2, "lines"))+
  geom_errorbar(aes(ymin=y-sd_NO, ymax=y+sd_NO), 
                width=0.1,position=position_dodge(0))+
  geom_errorbar(aes(xmin=x-sd_HNO, xmax=x+sd_HNO), 
                width=1.1,position=position_dodge(0))+
  scale_y_continuous(name = expression(N[2]*O~"Concentration (ppb)"))+
  scale_x_continuous(name = expression(H[2]*O~"Concentration (%)"),
                     breaks = seq(0, 3, 0.5))+
  scale_colour_manual("",values=c("red","darkgreen","dodgerblue"))+
  scale_shape_manual("CRDS",values=c(2))+
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=10),
        legend.text=element_text(size=6),
        legend.title=element_text(size=10),
        legend.position = "none")
pN2O<-tag_facet(pN2O,tag_pool = letters[2])
ggsave(plot=pN2O,"H2O vs N2O.png", width = 8, height = 5)

#CH4 ~ H2O
pCH4<-ggplot(data=df_CH4,aes(x,y,color=Test,shape=Type))+
  geom_point(size=3)+
  theme_bw()+theme(panel.grid=element_blank())+
  theme(panel.border =element_rect(colour = "grey", fill=NA))+
  theme(axis.line = element_line(colour = "black"))+
  theme(panel.spacing.x=unit(2, "lines"))+
  geom_errorbar(aes(ymin=y-sd_C, ymax=y+sd_C), 
                width=0.1,position=position_dodge(0))+
  geom_errorbar(aes(xmin=x-sd_HC, xmax=x+sd_HC), 
                width=0.001,position=position_dodge(0))+
  scale_y_continuous(name = expression(CH[4]~"Detrended concentration (ppm)"))+
  scale_x_continuous(name = expression(H[2]*O~"Concentration (%)"),
                     breaks = seq(0, 3, 0.5))+
  scale_colour_manual("",values=c("red","darkgreen","dodgerblue"))+
  scale_shape_manual("CRDS",values=c(2,0))+
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=10),
        legend.text=element_text(size=6),
        legend.title=element_text(size=10),
        legend.position = "none")
pCH4<-tag_facet(pCH4,tag_pool = letters[3])
ggsave(plot=pCH4,"H2O vs CH4.png", width = 8, height = 5)

##Plot in same figure with common legends
#Function for common legends
x<-1:3
y<-1:3
CRDS<-c("G2509", "G2103", "G4301")
Test<-c("Test 1", "Test 2","Test 3*")
df_legend<-cbind.data.frame(x,y,CRDS,Test)
plot1_legend <- ggplot(df_legend,
                       aes(x = x, y = y, color=Test,shape=CRDS)) +
  geom_point(size=3)+theme_bw()+
  scale_colour_manual("",values=c("red","darkgreen","dodgerblue"))+
  scale_shape_manual("",values=c(1,2,0))+
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
B<-grid.arrange(arrangeGrob(pNH3,pN2O,pCH4, ncol = 3,nrow=1),
                legend, nrow = 2, heights = c(10, 1))
ggsave(plot=B,"Main1_H2O_Interferences.png", width = 8, height = 5)