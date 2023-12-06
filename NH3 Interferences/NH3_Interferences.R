library(dplyr)
library(ggplot2)
library(anytime)
library(lubridate)
library(writexl)


### Interferences of NH3-N2O###   

##Extract data from our own picarro##  
setwd(".../NH3 Interferences")
list_of_files <- list.files(path="Own all Picarro",pattern = "*.dat",recursive=TRUE) ##Read data from Picarro
L<-length(list_of_files)

setwd(".../NH3 Interferences/Own all Picarro")

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

##Extract data from our own picarro from an NH3 calibration##  
setwd(".../NH3 Interferences")
list_of_files <- list.files(path="G2508 own",pattern = "*.dat",recursive=TRUE) ##Read data from Picarro
L<-length(list_of_files)

setwd(".../NH3 Interferences/G2508 own")
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
Dat_Cal<-getstats(Picarro = Pic_id) #Able to call each Picarro file
Dat_Cal<-bind_rows(Dat_Cal, .id = "column_label") #Join all elements of the list in one dataframe

#Change format of date and time to be plotted
Dat_Cal$date.time<-paste(Dat_Cal$DATE,Dat_Cal$TIME)
Dat_Cal$date.time<-as.POSIXct(Dat_Cal$date.time,format="%Y-%m-%d %H:%M:%S")

##Extract data from NH3 picarro##  
setwd(".../NH3 Interferences")
list_of_files <- list.files(path="NH3 Picarro",pattern = "*.dat",recursive=TRUE) ##Read data from Picarro
L<-length(list_of_files)

setwd(".../NH3 Interferences/NH3 Picarro")

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
setwd(".../NH3 Interferences")
list_of_files <- list.files(path="BackPack",pattern = "*.dat",recursive=TRUE) ##Read data from Picarro
L<-length(list_of_files)

setwd(".../NH3 Interferences/BackPack")

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


### N2O fixed, NH3 changing ###
##### Data treatment for publication #####
Time_All_N2ONH3<-c(Dat_All$date.time[25200:27789])
NH3_All_N2ONH3<-c(Dat_All$NH3[25200:27789])
N2O_All_N2ONH3<-c(Dat_All$N2O_dry[25200:27789])
Time_Cal_N2ONH3<-c(Dat_Cal$date.time[24000:46000])
NH3_Cal_N2ONH3<-c(Dat_Cal$NH3[24000:46000])
N2O_Cal_N2ONH3<-c(Dat_Cal$N2O_dry[24000:46000])

## Make intervals of the same length
mean_N2O<-c(mean(Dat_All$N2O_dry[(25810-34):25810]),mean(Dat_All$N2O_dry[(25900-34):25900]),mean(Dat_All$N2O_dry[(25990-34):25990]),mean(Dat_All$N2O_dry[(26130-34):26130]),mean(Dat_All$N2O_dry[26144:26178]),mean(Dat_All$N2O_dry[(26647-34):26647]),mean(Dat_All$N2O_dry[(26700-34):26700]),mean(Dat_All$N2O_dry[(26760-34):26760]),mean(Dat_All$N2O_dry[(26830-34):26830]),mean(Dat_All$N2O_dry[(26885-34):26885]),mean(Dat_All$N2O_dry[(27453-34):27453]),mean(Dat_All$N2O_dry[(27530-34):27530]),mean(Dat_All$N2O_dry[(27605-34):27605]),mean(Dat_All$N2O_dry[(27725-34):27725]),mean(Dat_All$N2O_dry[(27789-34):27789]))
mean_NH3<-c(mean(Dat_All$NH3[(25810-34):25810]),mean(Dat_All$NH3[(25900-34):25900]),mean(Dat_All$NH3[(25990-34):25990]),mean(Dat_All$NH3[(26130-34):26130]),mean(Dat_All$NH3[26144:26178]),mean(Dat_All$NH3[(26647-34):26647]),mean(Dat_All$NH3[(26700-34):26700]),mean(Dat_All$NH3[(26760-34):26760]),mean(Dat_All$NH3[(26830-34):26830]),mean(Dat_All$NH3[(26885-34):26885]),mean(Dat_All$NH3[(27453-34):27453]),mean(Dat_All$NH3[(27530-34):27530]),mean(Dat_All$NH3[(27605-34):27605]),mean(Dat_All$NH3[(27725-34):27725]),mean(Dat_All$NH3[(27789-34):27789]))
sd_N2O<-c(sd(Dat_All$N2O_dry[(25810-34):25810]),sd(Dat_All$N2O_dry[(25900-34):25900]),sd(Dat_All$N2O_dry[(25990-34):25990]),sd(Dat_All$N2O_dry[(26130-34):26130]),sd(Dat_All$N2O_dry[26144:26178]),sd(Dat_All$N2O_dry[(26647-34):26647]),sd(Dat_All$N2O_dry[(26700-34):26700]),sd(Dat_All$N2O_dry[(26760-34):26760]),sd(Dat_All$N2O_dry[(26830-34):26830]),sd(Dat_All$N2O_dry[(26885-34):26885]),sd(Dat_All$N2O_dry[(27453-34):27453]),sd(Dat_All$N2O_dry[(27530-34):27530]),sd(Dat_All$N2O_dry[(27605-34):27605]),sd(Dat_All$N2O_dry[(27725-34):27725]),sd(Dat_All$N2O_dry[(27789-34):27789]))
sd_NH3<-c(sd(Dat_All$NH3[(25810-34):25810]),sd(Dat_All$NH3[(25900-34):25900]),sd(Dat_All$NH3[(25990-34):25990]),sd(Dat_All$NH3[(26130-34):26130]),sd(Dat_All$NH3[26144:26178]),sd(Dat_All$NH3[(26647-34):26647]),sd(Dat_All$NH3[(26700-34):26700]),sd(Dat_All$NH3[(26760-34):26760]),sd(Dat_All$NH3[(26830-34):26830]),sd(Dat_All$NH3[(26885-34):26885]),sd(Dat_All$NH3[(27453-34):27453]),sd(Dat_All$NH3[(27530-34):27530]),sd(Dat_All$NH3[(27605-34):27605]),sd(Dat_All$NH3[(27725-34):27725]),sd(Dat_All$NH3[(27789-34):27789]))
length_N2O<-c(length(Dat_All$N2O_dry[(25810-34):25810]),length(Dat_All$N2O_dry[(25900-34):25900]),length(Dat_All$N2O_dry[(25990-34):25990]),length(Dat_All$N2O_dry[(26130-34):26130]),length(Dat_All$N2O_dry[26144:26178]),length(Dat_All$N2O_dry[(26647-34):26647]),length(Dat_All$N2O_dry[(26700-34):26700]),length(Dat_All$N2O_dry[(26760-34):26760]),length(Dat_All$N2O_dry[(26830-34):26830]),length(Dat_All$N2O_dry[(26885-34):26885]),length(Dat_All$N2O_dry[(27453-34):27453]),length(Dat_All$N2O_dry[(27530-34):27530]),length(Dat_All$N2O_dry[(27605-34):27605]),length(Dat_All$N2O_dry[(27725-34):27725]),length(Dat_All$N2O_dry[(27789-34):27789]))
Tinterval_N2O<-c(difftime(Dat_All$date.time[(25810-34)],Dat_All$date.time[25810]),difftime(Dat_All$date.time[(25900-34)],Dat_All$date.time[25900]),difftime(Dat_All$date.time[(25990-34)],Dat_All$date.time[25990]),difftime(Dat_All$date.time[(26130-34)],Dat_All$date.time[26130]),difftime(Dat_All$date.time[(26144)],Dat_All$date.time[26178]),difftime(Dat_All$date.time[(26647-34)],Dat_All$date.time[26647]),difftime(Dat_All$date.time[(26700-34)],Dat_All$date.time[26700]),difftime(Dat_All$date.time[(26760-34)],Dat_All$date.time[26760]),difftime(Dat_All$date.time[(26830-34)],Dat_All$date.time[26830]),difftime(Dat_All$date.time[(26885-34)],Dat_All$date.time[26885]),difftime(Dat_All$date.time[(27453-34)],Dat_All$date.time[27453]),difftime(Dat_All$date.time[(27530-34)],Dat_All$date.time[27530]),difftime(Dat_All$date.time[(27605-34)],Dat_All$date.time[27605]),difftime(Dat_All$date.time[(27725-34)],Dat_All$date.time[27725]),difftime(Dat_All$date.time[(27789-34)],Dat_All$date.time[27789]))
mean_N2O_cal<-c(mean(Dat_Cal$N2O_dry[(38193-34):38193]),mean(Dat_Cal$N2O_dry[(41364-34):41364]),mean(Dat_Cal$N2O_dry[(42974-34):42974]),mean(Dat_Cal$N2O_dry[(43478-34):43478]),mean(Dat_Cal$N2O_dry[(43884-34):43884]))
mean_NH3_cal<-c(mean(Dat_Cal$NH3[(38193-34):38193]),mean(Dat_Cal$NH3[(41364-34):41364]),mean(Dat_Cal$NH3[(42974-34):42974]),mean(Dat_Cal$NH3[(43478-34):43478]),mean(Dat_Cal$NH3[(43884-34):43884]))
sd_N2O_cal<-c(sd(Dat_Cal$N2O_dry[(38193-34):38193]),sd(Dat_Cal$N2O_dry[(41364-34):41364]),sd(Dat_Cal$N2O_dry[(42974-34):42974]),sd(Dat_Cal$N2O_dry[(43478-34):43478]),sd(Dat_Cal$N2O_dry[(43884-34):43884]))
sd_NH3_cal<-c(sd(Dat_Cal$NH3[(38193-34):38193]),sd(Dat_Cal$NH3[(41364-34):41364]),sd(Dat_Cal$NH3[(42974-34):42974]),sd(Dat_Cal$NH3[(43478-34):43478]),sd(Dat_Cal$NH3[(43884-34):43884]))
length_NH3_cal<-c(length(Dat_Cal$NH3[(38193-34):38193]),length(Dat_Cal$NH3[(41364-34):41364]),length(Dat_Cal$NH3[(42974-34):42974]),length(Dat_Cal$NH3[(43478-34):43478]),length(Dat_Cal$NH3[(43884-34):43884]))
Tinterval_NH3_cal<-c(difftime(Dat_Cal$date.time[(38193-34)],Dat_Cal$date.time[38193]),difftime(Dat_Cal$date.time[(41364-34)],Dat_Cal$date.time[41364]),difftime(Dat_Cal$date.time[(42974-34)],Dat_Cal$date.time[42974]),difftime(Dat_Cal$date.time[(43478-34)],Dat_Cal$date.time[43478]),difftime(Dat_Cal$date.time[(43884-34)],Dat_Cal$date.time[43884]))


##Save the data
#Means
df_control<-cbind.data.frame(mean_N2O,mean_NH3,sd_N2O,sd_NH3,length_N2O,Tinterval_N2O)
df_control_cal<-cbind.data.frame(mean_N2O_cal,mean_NH3_cal,sd_N2O_cal,sd_NH3_cal,length_NH3_cal,Tinterval_NH3_cal)
#Raw data
df_Allpoints<-cbind.data.frame(Time_All_N2ONH3,NH3_All_N2ONH3,N2O_All_N2ONH3)
df_Calpoints<-cbind.data.frame(Time_Cal_N2ONH3,NH3_Cal_N2ONH3,N2O_Cal_N2ONH3)

setwd(".../NH3 Interferences")
##Save text files
write.table(df_control, file = "Control_NH3N2O interferences.txt", sep = "\t", row.names = TRUE, col.names = NA)
write.table(df_Allpoints, file = "All_NH3N2O interferences.txt", sep = "\t", row.names = TRUE, col.names = NA)
write.table(df_control_cal, file = "Control_NH3N2O_0 interferences.txt", sep = "\t", row.names = TRUE, col.names = NA)
write.table(df_Calpoints, file = "All_NH3N2O_0 interferences.txt", sep = "\t", row.names = TRUE, col.names = NA)

##Read text files
df_N2ONH3_means<-read.table(file="Control_NH3N2O interferences.txt",header=TRUE, sep = "\t")
df_N2ONH3<-read.table(file="All_NH3N2O interferences.txt",header=TRUE, sep = "\t")
df_N2ONH3_means_cal<-read.table(file="Control_NH3N2O_0 interferences.txt",header=TRUE, sep = "\t")
df_N2ONH3_cal<-read.table(file="All_NH3N2O_0 interferences.txt",header=TRUE, sep = "\t")
df_N2ONH3i_cal100<-read.table(file="NH3i_cal100.txt",header=TRUE, sep = "\t")
df_N2ONH3$Time_All<-anytime(df_N2ONH3$Time_All_N2ONH3)
df_N2ONH3_cal$Time_Cal<-anytime(df_N2ONH3_cal$Time_Cal_N2ONH3)
df_CH4NH3<-read.table(file="CH4i_cal100.txt",header=TRUE, sep = "\t")

### Create data frames for main ###
##NH3 on N2O
#Normalize the N2O by dividing by the maximum on each concentration
x<-c(df_N2ONH3_means$mean_NH3/1000)
y<-c(df_N2ONH3_means$mean_N2O[1:5]/max(df_N2ONH3_means$mean_N2O[1:5]),df_N2ONH3_means$mean_N2O[6:10]/max(df_N2ONH3_means$mean_N2O[6:10]),df_N2ONH3_means$mean_N2O[11:15]/max(df_N2ONH3_means$mean_N2O[11:15]))
sd_NH<-c(df_N2ONH3_means$sd_NH3)
relSD_NO<-c(df_N2ONH3_means$sd_N2O/df_N2ONH3_means$mean_N2O*100)
relSD_plot<-relSD_NO/100
SD_maxNO<-relSD_NO[which(y==max(y))]
relSD_maxNO<-c(rep(relSD_NO[1],5),rep(relSD_NO[2],5),rep(relSD_NO[3],5))
relsd_N2O<-sqrt(relSD_NO^2+relSD_maxNO^2)
sd_N2O<-relsd_N2O/100*df_N2ONH3_means$mean_N2O
sd_NH3<-c(df_N2ONH3_means$sd_NH3/1000)
sd_N2O_new<-c(df_N2ONH3_means$sd_N2O[1:5]/max(df_N2ONH3_means$mean_N2O[1:5]),df_N2ONH3_means$sd_N2O[6:10]/max(df_N2ONH3_means$mean_N2O[6:10]),df_N2ONH3_means$sd_N2O[11:15]/max(df_N2ONH3_means$mean_N2O[11:15]))
Concentration<-c(rep("1 ppm",5),rep("2 ppm",5),rep("4 ppm",5))
df_NH3plot<-cbind.data.frame(x,y,Concentration)

MainN2O<-ggplot(data=df_NH3plot,aes(x,y,color=Concentration,shape=Concentration))+
  geom_point(size=3)+
coord_cartesian(ylim = c(0.9, 1.1))+
  theme_bw()+theme(panel.grid=element_blank())+
  theme(panel.border =element_rect(colour = "grey", fill=NA))+
  theme(axis.line = element_line(colour = "black"))+
  theme(panel.spacing.x=unit(2, "lines"))+
  geom_errorbar(aes(xmin=x-sd_NH3, xmax=x+sd_NH3), 
                width=0.0008,position=position_dodge(0))+
  geom_errorbar(aes(ymin=y-sd_N2O_new, ymax=y+sd_N2O_new), 
                width=0.1,position=position_dodge(0))+
  scale_x_continuous(name = expression(NH[3]~"Concentration (ppm)"))+
  scale_y_continuous(name = expression("Normalized "~N[2]*O~"Concentration"))+
  scale_shape_manual('Concentration',values = c(1,2,0),breaks=c("1 ppm","2 ppm","4 ppm"))+
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=10),
        legend.text=element_text(size=8),
        legend.title=element_text(size=10))
#coord_cartesian(ylim=c(0.9,1.1))
ggsave(plot=MainN2O,"Main2f_N2O vs NH3.png", width = 8, height = 5)
#MainN2O<-tag_facet(MainN2O,tag_pool = letters[1])

### NH3 Interferences on background CH4 ###
Group<-c(rep("G2508",13),rep("G4301",13))
x<-c(df_CH4NH3$Measured_NH3,df_CH4NH3$Measured_NH3)
y<-c(df_CH4NH3$Measured_CH4i,df_CH4NH3$Measured_BPi)
Sd_NH3<-c(df_CH4NH3$Sd_NH3,df_CH4NH3$Sd_NH3)
Sd_CH4<-c(df_CH4NH3$Sd_CH4i,df_CH4NH3$Sd_BPi)
df_CH4_BGlab<-cbind.data.frame(x,y,Sd_CH4,Sd_NH3,Group)

### NH3 Interferences on background N2O ###
x<-c(df_N2ONH3i_cal100$Measured_NH3)
y<-c(df_N2ONH3i_cal100$Measured_N2O)
Sd_N2O<-c(df_N2ONH3i_cal100$Sd_N2O)
Sd_NH3<-c(df_N2ONH3i_cal100$Sd_NH3)
Group<-rep("N2O",nrow(df_N2ONH3i_cal100))

df_N2O_BGlab<-cbind.data.frame(x,y,Sd_N2O,Sd_NH3,Group)



### What is the Background concentration we expect ###
#AQ8: 1000mL/min, calibrated for NH3 with soap
x_AQ8<-c(0,100,100,150,100,150,200,250,300,350,500,750,1000)
AQ8<-1.119880808*x_AQ8+13.11867169
AQ8[1]<-0
#AQ11: 50L/min, calibrated for air with flow meter
x_AQ11<-c(5,15,10,10,5,5,5,5,5,5,5,5,5)
AQ11<-0.991710391*x_AQ11+0.281032686


#N2O
Expected_BG_N2O<-(AQ11*df_N2ONH3i_cal100$Measured_N2O_dry[1]+AQ8*0)/(AQ11+AQ8/1000)
Uncertainty_N2Obg<-sqrt((T1N/(AQ11*df_N2ONH3i_cal100$Measured_N2O_dry[1])*100)^2+(T2N/(AQ11+AQ8/1000))^2)/100*Expected_BG_N2O

df_N2O_BGlab$Expected<-df_N2O_BGlab$y-Expected_BG_N2O
#CH4
Expected_BG_CH4<-(AQ11*df_CH4_BGlab$y[1]+AQ8*0)/(AQ11+AQ8/1000)

Expected_BG_BP<-(AQ11*df_CH4_BGlab$y[14]+AQ8*0)/(AQ11+AQ8/1000)

df_CH4_BGlab$Expected<-c(df_CH4_BGlab$y[1:13]-Expected_BG_CH4,df_CH4_BGlab$y[14:26]-Expected_BG_BP)


CH4_BG<-ggplot(data=df_CH4_BGlab,aes(x,Expected,color=Group,shape=Group))+
  geom_point(size=3,shape=1)+
  #geom_point(aes(x,Norm_Expected_CH4))+
  #geom_line(aes(x,Expected_BG_CH4,linetype="Expected concentration"))+
  #geom_ribbon(aes(ymin=Min_Expected, ymax=Max_Expected), alpha = 0.1,linetype=0,show.legend = FALSE)+
  theme_bw()+theme(panel.grid=element_blank())+
  theme(panel.border =element_rect(colour = "grey", fill=NA))+
  theme(axis.line = element_line(colour = "black"))+
  theme(panel.spacing.x=unit(2, "lines"))+
  geom_errorbar(aes(xmin=x-Sd_NH3, xmax=x+Sd_NH3), 
                width=0.008,position=position_dodge(0))+
  geom_errorbar(aes(ymin=Expected-Sd_CH4, ymax=Expected+Sd_CH4), 
                width=1000,position=position_dodge(0))+
  geom_line(aes(x,rep(0,26)),linetype=2,color="black")+
  scale_x_continuous(name = expression())+
  scale_y_continuous(name = expression(""))+
  #scale_linetype_manual('',values = c("dashed"))+
  scale_color_manual('',labels = c(expression(G2509~CH['4']),
                                   expression(G4301~CH['4'])),values = c("red","blue"))+
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=10),
        legend.text=element_text(size=8),
        legend.title=element_text(size=10),
        legend.position = c(.2,.25))
#CH4_BG<-tag_facet(CH4_BG,tag_pool = letters[2])


MainN2O<-MainN2O +  geom_text(aes(label = "(a)"),
                              x = -Inf, y = Inf,
                              vjust = 1, hjust = 0,
                              size = 4,
                              color="black")

CH4_BG<-CH4_BG +  geom_text(aes(label = "(b)"),
                            x = -Inf, y = Inf,
                            vjust = 1, hjust = 0,
                            size = 4,
                            color="black")

N2O_BG<-N2O_BG +  geom_text(aes(label = "(c)"),
                            x = -Inf, y = Inf,
                            vjust = 1, hjust = 0,
                            size = 4,
                            color="black")
NH3_BG<-grid.arrange(arrangeGrob(CH4_BG,N2O_BG, ncol = 1,nrow=2),
                     left = textGrob(expression(paste("Meaured - Expeced concentration (ppm)")), rot = 90
                                     , vjust = 0.5,gp = gpar(fontsize = 10)))
B<-grid.arrange(arrangeGrob(MainN2O,NH3_BG, ncol = 2,nrow=1))
ggsave(plot=B,"Main2_NH3-BG.png", width = 10, height = 5)