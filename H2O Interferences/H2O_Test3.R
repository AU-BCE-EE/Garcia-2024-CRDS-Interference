library(dplyr)
library(ggplot2)
library(anytime)
library(lubridate)
library(writexl)
library(scales)
library(egg)

### Interferences of H2O###   

##Extract data from own picarro##  
setwd("C:/Users/au615105/OneDrive - Aarhus universitet/Pablo Garc?a/Picarro interferences (Anna&Pablo)/Interferences/H2O")
list_of_files <- list.files(path="G2508 Own",pattern = "*.dat",recursive=TRUE) ##Read data from Picarro
L<-length(list_of_files)

setwd("C:/Users/au615105/OneDrive - Aarhus universitet/Pablo Garc?a/Picarro interferences (Anna&Pablo)/Interferences/H2O/G2508 own")

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


### Increasing H2O in steps dry-humid-dry-humid... ###
###Data to plot all the calibration 16-05-2023
Time_All<-c(Dat$date.time[80000:123920])
H2O_All<-c(Dat$H2O[80000:123920])
NH3_All<-c(Dat$NH3[80000:123920])
CH4_dry_All<-c(Dat$CH4_dry[80000:123920])
N2O_dry_All<-c(Dat$N2O_dry[80000:123920])
CH4_All<-c(Dat$CH4[80000:123920])
N2O_All<-c(Dat$N2O[80000:123920])

### Select stable intervals means

Mean_H2O<-c(mean(Dat$H2O[80000:90000]),mean(Dat$H2O[91400:92050]),mean(Dat$H2O[92300:93970]),mean(Dat$H2O[94100:95469]),
            mean(Dat$H2O[95500:97499]),mean(Dat$H2O[97700:99140]),mean(Dat$H2O[99160:100830]),mean(Dat$H2O[100970:102630]),
            mean(Dat$H2O[102690:104240]),mean(Dat$H2O[104470:106085]),mean(Dat$H2O[106106:107765]),mean(Dat$H2O[108002:109494]),
            mean(Dat$H2O[109514:110977]),mean(Dat$H2O[111306:112947]),mean(Dat$H2O[112990:114630]),mean(Dat$H2O[114981:116291]),
            mean(Dat$H2O[116329:118139]),mean(Dat$H2O[118760:120064]),mean(Dat$H2O[120095:121684]),mean(Dat$H2O[121835:123340]),
            mean(Dat$H2O[123375:123920]))

Mean_NH3<-c(mean(Dat$NH3[80000:90000]),mean(Dat$NH3[91400:92050]),mean(Dat$NH3[92300:93970]),mean(Dat$NH3[94100:95469]),
            mean(Dat$NH3[95500:97499]),mean(Dat$NH3[97700:99140]),mean(Dat$NH3[99160:100830]),mean(Dat$NH3[100970:102630]),
            mean(Dat$NH3[102690:104240]),mean(Dat$NH3[104470:106085]),mean(Dat$NH3[106106:107765]),mean(Dat$NH3[108002:109494]),
            mean(Dat$NH3[109514:110977]),mean(Dat$NH3[111306:112947]),mean(Dat$NH3[112990:114630]),mean(Dat$NH3[114981:116291]),
            mean(Dat$NH3[116329:118139]),mean(Dat$NH3[118760:120064]),mean(Dat$NH3[120095:121684]),mean(Dat$NH3[121835:123340]),
            mean(Dat$NH3[123375:123920]))

Mean_N2O_dry<-c(mean(Dat$N2O_dry[80000:90000]),mean(Dat$N2O_dry[91400:92050]),mean(Dat$N2O_dry[92300:93970]),mean(Dat$N2O_dry[94100:95469]),
                mean(Dat$N2O_dry[95500:97499]),mean(Dat$N2O_dry[97700:99140]),mean(Dat$N2O_dry[99160:100830]),mean(Dat$N2O_dry[100970:102630]),
                mean(Dat$N2O_dry[102690:104240]),mean(Dat$N2O_dry[104470:106085]),mean(Dat$N2O_dry[106106:107765]),mean(Dat$N2O_dry[108002:109494]),
                mean(Dat$N2O_dry[109514:110977]),mean(Dat$N2O_dry[111306:112947]),mean(Dat$N2O_dry[112990:114630]),mean(Dat$N2O_dry[114981:116291]),
                mean(Dat$N2O_dry[116329:118139]),mean(Dat$N2O_dry[118760:120064]),mean(Dat$N2O_dry[120095:121684]),mean(Dat$N2O_dry[121835:123340]),
                mean(Dat$N2O_dry[123375:123920]))

Mean_CH4_dry<-c(mean(Dat$CH4_dry[80000:90000]),mean(Dat$CH4_dry[91400:92050]),mean(Dat$CH4_dry[92300:93970]),mean(Dat$CH4_dry[94100:95469]),
                mean(Dat$CH4_dry[95500:97499]),mean(Dat$CH4_dry[97700:99140]),mean(Dat$CH4_dry[99160:100830]),mean(Dat$CH4_dry[100970:102630]),
                mean(Dat$CH4_dry[102690:104240]),mean(Dat$CH4_dry[104470:106085]),mean(Dat$CH4_dry[106106:107765]),mean(Dat$CH4_dry[108002:109494]),
                mean(Dat$CH4_dry[109514:110977]),mean(Dat$CH4_dry[111306:112947]),mean(Dat$CH4_dry[112990:114630]),mean(Dat$CH4_dry[114981:116291]),
                mean(Dat$CH4_dry[116329:118139]),mean(Dat$CH4_dry[118760:120064]),mean(Dat$CH4_dry[120095:121684]),mean(Dat$CH4_dry[121835:123340]),
                mean(Dat$CH4_dry[123375:123920]))

Mean_CH4<-c(mean(Dat$CH4[80000:90000]),mean(Dat$CH4[91400:92050]),mean(Dat$CH4[92300:93970]),mean(Dat$CH4[94100:95469]),
            mean(Dat$CH4[95500:97499]),mean(Dat$CH4[97700:99140]),mean(Dat$CH4[99160:100830]),mean(Dat$CH4[100970:102630]),
            mean(Dat$CH4[102690:104240]),mean(Dat$CH4[104470:106085]),mean(Dat$CH4[106106:107765]),mean(Dat$CH4[108002:109494]),
            mean(Dat$CH4[109514:110977]),mean(Dat$CH4[111306:112947]),mean(Dat$CH4[112990:114630]),mean(Dat$CH4[114981:116291]),
            mean(Dat$CH4[116329:118139]),mean(Dat$CH4[118760:120064]),mean(Dat$CH4[120095:121684]),mean(Dat$CH4[121835:123340]),
            mean(Dat$CH4[123375:123920]))

Mean_NH3<-c(mean(Dat$NH3[80000:90000]),mean(Dat$NH3[91400:92050]),mean(Dat$NH3[92300:93970]),mean(Dat$NH3[94100:95469]),
            mean(Dat$NH3[95500:97499]),mean(Dat$NH3[97700:99140]),mean(Dat$NH3[99160:100830]),mean(Dat$NH3[100970:102630]),
            mean(Dat$NH3[102690:104240]),mean(Dat$NH3[104470:106085]),mean(Dat$NH3[106106:107765]),mean(Dat$NH3[108002:109494]),
            mean(Dat$NH3[109514:110977]),mean(Dat$NH3[111306:112947]),mean(Dat$NH3[112990:114630]),mean(Dat$NH3[114981:116291]),
            mean(Dat$NH3[116329:118139]),mean(Dat$NH3[118760:120064]),mean(Dat$NH3[120095:121684]),mean(Dat$NH3[121835:123340]),
            mean(Dat$NH3[123375:123920]))

sd_H2O<-c(sd(Dat$H2O[80000:90000]),sd(Dat$H2O[91400:92050]),sd(Dat$H2O[92300:93970]),sd(Dat$H2O[94100:95469]),
          sd(Dat$H2O[95500:97499]),sd(Dat$H2O[97700:99140]),sd(Dat$H2O[99160:100830]),sd(Dat$H2O[100970:102630]),
          sd(Dat$H2O[102690:104240]),sd(Dat$H2O[104470:106085]),sd(Dat$H2O[106106:107765]),sd(Dat$H2O[108002:109494]),
          sd(Dat$H2O[109514:110977]),sd(Dat$H2O[111306:112947]),sd(Dat$H2O[112990:114630]),sd(Dat$H2O[114981:116291]),
          sd(Dat$H2O[116329:118139]),sd(Dat$H2O[118760:120064]),sd(Dat$H2O[120095:121684]),sd(Dat$H2O[121835:123340]),
          sd(Dat$H2O[123375:123920]))

sd_NH3<-c(sd(Dat$NH3[80000:90000]),sd(Dat$NH3[91400:92050]),sd(Dat$NH3[92300:93970]),sd(Dat$NH3[94100:95469]),
          sd(Dat$NH3[95500:97499]),sd(Dat$NH3[97700:99140]),sd(Dat$NH3[99160:100830]),sd(Dat$NH3[100970:102630]),
          sd(Dat$NH3[102690:104240]),sd(Dat$NH3[104470:106085]),sd(Dat$NH3[106106:107765]),sd(Dat$NH3[108002:109494]),
          sd(Dat$NH3[109514:110977]),sd(Dat$NH3[111306:112947]),sd(Dat$NH3[112990:114630]),sd(Dat$NH3[114981:116291]),
          sd(Dat$NH3[116329:118139]),sd(Dat$NH3[118760:120064]),sd(Dat$NH3[120095:121684]),sd(Dat$NH3[121835:123340]),
          sd(Dat$NH3[123375:123920]))

sd_N2O_dry<-c(sd(Dat$N2O_dry[80000:90000]),sd(Dat$N2O_dry[91400:92050]),sd(Dat$N2O_dry[92300:93970]),sd(Dat$N2O_dry[94100:95469]),
              sd(Dat$N2O_dry[95500:97499]),sd(Dat$N2O_dry[97700:99140]),sd(Dat$N2O_dry[99160:100830]),sd(Dat$N2O_dry[100970:102630]),
              sd(Dat$N2O_dry[102690:104240]),sd(Dat$N2O_dry[104470:106085]),sd(Dat$N2O_dry[106106:107765]),sd(Dat$N2O_dry[108002:109494]),
              sd(Dat$N2O_dry[109514:110977]),sd(Dat$N2O_dry[111306:112947]),sd(Dat$N2O_dry[112990:114630]),sd(Dat$N2O_dry[114981:116291]),
              sd(Dat$N2O_dry[116329:118139]),sd(Dat$N2O_dry[118760:120064]),sd(Dat$N2O_dry[120095:121684]),sd(Dat$N2O_dry[121835:123340]),
              sd(Dat$N2O_dry[123375:123920]))

sd_CH4_dry<-c(sd(Dat$CH4_dry[80000:90000]),sd(Dat$CH4_dry[91400:92050]),sd(Dat$CH4_dry[92300:93970]),sd(Dat$CH4_dry[94100:95469]),
              sd(Dat$CH4_dry[95500:97499]),sd(Dat$CH4_dry[97700:99140]),sd(Dat$CH4_dry[99160:100830]),sd(Dat$CH4_dry[100970:102630]),
              sd(Dat$CH4_dry[102690:104240]),sd(Dat$CH4_dry[104470:106085]),sd(Dat$CH4_dry[106106:107765]),sd(Dat$CH4_dry[108002:109494]),
              sd(Dat$CH4_dry[109514:110977]),sd(Dat$CH4_dry[111306:112947]),sd(Dat$CH4_dry[112990:114630]),sd(Dat$CH4_dry[114981:116291]),
              sd(Dat$CH4_dry[116329:118139]),sd(Dat$CH4_dry[118760:120064]),sd(Dat$CH4_dry[120095:121684]),sd(Dat$CH4_dry[121835:123340]),
              sd(Dat$CH4_dry[123375:123920]))

sd_CH4<-c(sd(Dat$CH4[80000:90000]),sd(Dat$CH4[91400:92050]),sd(Dat$CH4[92300:93970]),sd(Dat$CH4[94100:95469]),
          sd(Dat$CH4[95500:97499]),sd(Dat$CH4[97700:99140]),sd(Dat$CH4[99160:100830]),sd(Dat$CH4[100970:102630]),
          sd(Dat$CH4[102690:104240]),sd(Dat$CH4[104470:106085]),sd(Dat$CH4[106106:107765]),sd(Dat$CH4[108002:109494]),
          sd(Dat$CH4[109514:110977]),sd(Dat$CH4[111306:112947]),sd(Dat$CH4[112990:114630]),sd(Dat$CH4[114981:116291]),
          sd(Dat$CH4[116329:118139]),sd(Dat$CH4[118760:120064]),sd(Dat$CH4[120095:121684]),sd(Dat$CH4[121835:123340]),
          sd(Dat$CH4[123375:123920]))

sd_NH3<-c(sd(Dat$NH3[80000:90000]),sd(Dat$NH3[91400:92050]),sd(Dat$NH3[92300:93970]),sd(Dat$NH3[94100:95469]),
          sd(Dat$NH3[95500:97499]),sd(Dat$NH3[97700:99140]),sd(Dat$NH3[99160:100830]),sd(Dat$NH3[100970:102630]),
          sd(Dat$NH3[102690:104240]),sd(Dat$NH3[104470:106085]),sd(Dat$NH3[106106:107765]),sd(Dat$NH3[108002:109494]),
          sd(Dat$NH3[109514:110977]),sd(Dat$NH3[111306:112947]),sd(Dat$NH3[112990:114630]),sd(Dat$NH3[114981:116291]),
          sd(Dat$NH3[116329:118139]),sd(Dat$NH3[118760:120064]),sd(Dat$NH3[120095:121684]),sd(Dat$NH3[121835:123340]),
          sd(Dat$NH3[123375:123920]))

length_H2O<-c(length(Dat$H2O[80000:90000]),length(Dat$H2O[91400:92050]),length(Dat$H2O[92300:93970]),length(Dat$H2O[94100:95469]),
              length(Dat$H2O[95500:97499]),length(Dat$H2O[97700:99140]),length(Dat$H2O[99160:100830]),length(Dat$H2O[100970:102630]),
              length(Dat$H2O[102690:104240]),length(Dat$H2O[104470:106085]),length(Dat$H2O[106106:107765]),length(Dat$H2O[108002:109494]),
              length(Dat$H2O[109514:110977]),length(Dat$H2O[111306:112947]),length(Dat$H2O[112990:114630]),length(Dat$H2O[114981:116291]),
              length(Dat$H2O[116329:118139]),length(Dat$H2O[118760:120064]),length(Dat$H2O[120095:121684]),length(Dat$H2O[121835:123340]),
              length(Dat$H2O[123375:123920]))

length_NH3<-c(length(Dat$NH3[80000:90000]),length(Dat$NH3[91400:92050]),length(Dat$NH3[92300:93970]),length(Dat$NH3[94100:95469]),
              length(Dat$NH3[95500:97499]),length(Dat$NH3[97700:99140]),length(Dat$NH3[99160:100830]),length(Dat$NH3[100970:102630]),
              length(Dat$NH3[102690:104240]),length(Dat$NH3[104470:106085]),length(Dat$NH3[106106:107765]),length(Dat$NH3[108002:109494]),
              length(Dat$NH3[109514:110977]),length(Dat$NH3[111306:112947]),length(Dat$NH3[112990:114630]),length(Dat$NH3[114981:116291]),
              length(Dat$NH3[116329:118139]),length(Dat$NH3[118760:120064]),length(Dat$NH3[120095:121684]),length(Dat$NH3[121835:123340]),
              length(Dat$NH3[123375:123920]))

length_N2O_dry<-c(length(Dat$N2O_dry[80000:90000]),length(Dat$N2O_dry[91400:92050]),length(Dat$N2O_dry[92300:93970]),length(Dat$N2O_dry[94100:95469]),
                  length(Dat$N2O_dry[95500:97499]),length(Dat$N2O_dry[97700:99140]),length(Dat$N2O_dry[99160:100830]),length(Dat$N2O_dry[100970:102630]),
                  length(Dat$N2O_dry[102690:104240]),length(Dat$N2O_dry[104470:106085]),length(Dat$N2O_dry[106106:107765]),length(Dat$N2O_dry[108002:109494]),
                  length(Dat$N2O_dry[109514:110977]),length(Dat$N2O_dry[111306:112947]),length(Dat$N2O_dry[112990:114630]),length(Dat$N2O_dry[114981:116291]),
                  length(Dat$N2O_dry[116329:118139]),length(Dat$N2O_dry[118760:120064]),length(Dat$N2O_dry[120095:121684]),length(Dat$N2O_dry[121835:123340]),
                  length(Dat$N2O_dry[123375:123920]))

length_CH4_dry<-c(length(Dat$CH4_dry[80000:90000]),length(Dat$CH4_dry[91400:92050]),length(Dat$CH4_dry[92300:93970]),length(Dat$CH4_dry[94100:95469]),
                  length(Dat$CH4_dry[95500:97499]),length(Dat$CH4_dry[97700:99140]),length(Dat$CH4_dry[99160:100830]),length(Dat$CH4_dry[100970:102630]),
                  length(Dat$CH4_dry[102690:104240]),length(Dat$CH4_dry[104470:106085]),length(Dat$CH4_dry[106106:107765]),length(Dat$CH4_dry[108002:109494]),
                  length(Dat$CH4_dry[109514:110977]),length(Dat$CH4_dry[111306:112947]),length(Dat$CH4_dry[112990:114630]),length(Dat$CH4_dry[114981:116291]),
                  length(Dat$CH4_dry[116329:118139]),length(Dat$CH4_dry[118760:120064]),length(Dat$CH4_dry[120095:121684]),length(Dat$CH4_dry[121835:123340]),
                  length(Dat$CH4_dry[123375:123920]))

length_CH4<-c(length(Dat$CH4[80000:90000]),length(Dat$CH4[91400:92050]),length(Dat$CH4[92300:93970]),length(Dat$CH4[94100:95469]),
              length(Dat$CH4[95500:97499]),length(Dat$CH4[97700:99140]),length(Dat$CH4[99160:100830]),length(Dat$CH4[100970:102630]),
              length(Dat$CH4[102690:104240]),length(Dat$CH4[104470:106085]),length(Dat$CH4[106106:107765]),length(Dat$CH4[108002:109494]),
              length(Dat$CH4[109514:110977]),length(Dat$CH4[111306:112947]),length(Dat$CH4[112990:114630]),length(Dat$CH4[114981:116291]),
              length(Dat$CH4[116329:118139]),length(Dat$CH4[118760:120064]),length(Dat$CH4[120095:121684]),length(Dat$CH4[121835:123340]),
              length(Dat$CH4[123375:123920]))

length_NH3<-c(length(Dat$NH3[80000:90000]),length(Dat$NH3[91400:92050]),length(Dat$NH3[92300:93970]),length(Dat$NH3[94100:95469]),
              length(Dat$NH3[95500:97499]),length(Dat$NH3[97700:99140]),length(Dat$NH3[99160:100830]),length(Dat$NH3[100970:102630]),
              length(Dat$NH3[102690:104240]),length(Dat$NH3[104470:106085]),length(Dat$NH3[106106:107765]),length(Dat$NH3[108002:109494]),
              length(Dat$NH3[109514:110977]),length(Dat$NH3[111306:112947]),length(Dat$NH3[112990:114630]),length(Dat$NH3[114981:116291]),
              length(Dat$NH3[116329:118139]),length(Dat$NH3[118760:120064]),length(Dat$NH3[120095:121684]),length(Dat$NH3[121835:123340]),
              length(Dat$NH3[123375:123920]))

Tinterval<-c(difftime(Dat$date.time[80000],Dat$date.time[90000]),difftime(Dat$date.time[91400],Dat$date.time[92050]),difftime(Dat$date.time[92300],Dat$date.time[93970]),difftime(Dat$date.time[94100],Dat$date.time[95469]),
             difftime(Dat$date.time[95500],Dat$date.time[97499]),difftime(Dat$date.time[97700],Dat$date.time[99140]),difftime(Dat$date.time[99160],Dat$date.time[100830]),difftime(Dat$date.time[100970],Dat$date.time[102630]),
             difftime(Dat$date.time[102690],Dat$date.time[104240]),difftime(Dat$date.time[104470],Dat$date.time[106085]),difftime(Dat$date.time[106106],Dat$date.time[107765]),difftime(Dat$date.time[108002],Dat$date.time[109494]),
             difftime(Dat$date.time[109514],Dat$date.time[110977]),difftime(Dat$date.time[111306],Dat$date.time[112947]),difftime(Dat$date.time[112990],Dat$date.time[114630]),difftime(Dat$date.time[114981],Dat$date.time[116291]),
             difftime(Dat$date.time[116329],Dat$date.time[118139]),difftime(Dat$date.time[118760],Dat$date.time[120064]),difftime(Dat$date.time[120095],Dat$date.time[121684]),difftime(Dat$date.time[121835],Dat$date.time[123340]),
             difftime(Dat$date.time[123375],Dat$date.time[123920]))

df_Control<-cbind.data.frame(Tinterval,length_NH3)

### Data frames ###
x<-Mean_H2O
y<-Mean_NH3
Group<-rep("NH3",length(Mean_H2O))
df_NH3<-cbind.data.frame(x,y,Group,sd_H2O,sd_NH3)

x<-Mean_H2O
y<-Mean_N2O_dry
Group<-rep("N2O",length(Mean_H2O))
df_N2O<-cbind.data.frame(x,y,Group,sd_H2O,sd_N2O_dry)

x<-Mean_H2O
y<-Mean_CH4_dry
Group<-rep("CH4",length(Mean_H2O))
df_CH4<-cbind.data.frame(x,y,Group,sd_H2O,sd_CH4_dry)

#Detrend CH4
x_T<-1:length(CH4_dry_All) ##Needed to make the polynomic function work
CH4<-CH4_dry_All[1:43920] 
Eq_C_dt<-lm(formula = CH4 ~ poly(as.numeric(x_T[1:43920]),18,raw=TRUE)) #polynomial
Coeff_dt<-coefficients(Eq_C_dt)
CH4_detrend<-CH4-predict(Eq_C_dt)
CH4_detrend<-CH4_detrend+mean(CH4)

setwd("C:/Users/au615105/OneDrive - Aarhus universitet/Pablo Garc?a/Papers/Interferences pictures/H2O repetition")

#Control plot
##Duration vector##
df_H2O<-cbind.data.frame(Time_All,CH4_dry_All,H2O_All)

d_Alldt3<-1
for (i in 1:length(df_H2O$Time_All)){
  d_Alldt3[i]<-difftime(df_H2O$Time_All[0+i],df_H2O$Time_All[1],units="mins")
}
df_H2O$d_Alldt3<-d_Alldt3
###################
Cp3<-ggplot(data=df_H2O[1:43920,])+geom_point(aes(d_Alldt3,CH4_dry_All,color="original"),shape=1)+
  geom_line(aes(d_Alldt3,predict(Eq_C_dt),color="Fit"),size=1)+
  geom_point(aes(d_Alldt3,CH4_dry_All-predict(Eq_C_dt)+mean(CH4_dry_All),color="detrended"),shape=1)+
  theme(panel.border =element_rect(colour = "grey", fill=NA))+
  theme(axis.line = element_line(colour = "black"))+
  theme(panel.spacing.x=unit(2, "lines"))+
  theme_bw()+theme(panel.grid=element_blank())+
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        legend.text=element_text(size=6),
        legend.title=element_text(size=8),
        legend.position = "none")+
  xlab("Time, min")+
  scale_color_manual("",values=c("grey1","blue","grey50"),
                     guide = guide_legend(override.aes = list(
                       linetype = c("blank", "solid", "blank"),
                       shape = c(1, NA, 1))))+
  scale_y_continuous(name = expression(paste("")),labels = number_format(accuracy = 0.01))
Cp3<-tag_facet(Cp3,tag_pool = letters[4])

ggsave(plot=Cp3,"Detrend control_CH4-H2O.png", width = 8, height = 5)

#New CH4 detrended data frame
mean_CH4_dry_detrend<-c(mean(CH4_detrend[(80000-79999):(89000-79999)]),mean(CH4_detrend[(91400-79999):(92050-79999)]),mean(CH4_detrend[(92300-79999):(93970-79999)]),mean(CH4_detrend[(94100-79999):(95469-79999)]),
                        mean(CH4_detrend[(95500-79999):(97499-79999)]),mean(CH4_detrend[(97700-79999):(99140-79999)]),mean(CH4_detrend[(99160-79999):(100830-79999)]),mean(CH4_detrend[(100970-79999):(102630-79999)]),
                        mean(CH4_detrend[(102690-79999):(104240-79999)]),mean(CH4_detrend[(104470-79999):(106085-79999)]),mean(CH4_detrend[(106106-79999):(107765-79999)]),mean(CH4_detrend[(108002-79999):(109494-79999)]),
                        mean(CH4_detrend[(109514-79999):(110977-79999)]),mean(CH4_detrend[(111306-79999):(112947-79999)]),mean(CH4_detrend[(112990-79999):(114630-79999)]),mean(CH4_detrend[(114981-79999):(116291-79999)]),
                        mean(CH4_detrend[(116329-79999):(118139-79999)]),mean(CH4_detrend[(118760-79999):(120064-79999)]),mean(CH4_detrend[(120095-79999):(121684-79999)]),mean(CH4_detrend[(121835-79999):(123340-79999)]),
                        mean(CH4_detrend[(123375-79999):(123920-79999)]))

mean_H2O_detrend<-c(mean(H2O_All[(80000-79999):(89000-79999)]),mean(H2O_All[(91400-79999):(92050-79999)]),mean(H2O_All[(92300-79999):(93970-79999)]),mean(H2O_All[(94100-79999):(95469-79999)]),
                    mean(H2O_All[(95500-79999):(97499-79999)]),mean(H2O_All[(97700-79999):(99140-79999)]),mean(H2O_All[(99160-79999):(100830-79999)]),mean(H2O_All[(100970-79999):(102630-79999)]),
                    mean(H2O_All[(102690-79999):(104240-79999)]),mean(H2O_All[(104470-79999):(106085-79999)]),mean(H2O_All[(106106-79999):(107765-79999)]),mean(H2O_All[(108002-79999):(109494-79999)]),
                    mean(H2O_All[(109514-79999):(110977-79999)]),mean(H2O_All[(111306-79999):(112947-79999)]),mean(H2O_All[(112990-79999):(114630-79999)]),mean(H2O_All[(114981-79999):(116291-79999)]),
                    mean(H2O_All[(116329-79999):(118139-79999)]),mean(H2O_All[(118760-79999):(120064-79999)]),mean(H2O_All[(120095-79999):(121684-79999)]),mean(H2O_All[(121835-79999):(123340-79999)]),
                    mean(H2O_All[(123375-79999):(123920-79999)]))

sd_CH4_dry_detrend<-c(sd(CH4_detrend[(80000-79999):(89000-79999)]),sd(CH4_detrend[(91400-79999):(92050-79999)]),sd(CH4_detrend[(92300-79999):(93970-79999)]),sd(CH4_detrend[(94100-79999):(95469-79999)]),
                      sd(CH4_detrend[(95500-79999):(97499-79999)]),sd(CH4_detrend[(97700-79999):(99140-79999)]),sd(CH4_detrend[(99160-79999):(100830-79999)]),sd(CH4_detrend[(100970-79999):(102630-79999)]),
                      sd(CH4_detrend[(102690-79999):(104240-79999)]),sd(CH4_detrend[(104470-79999):(106085-79999)]),sd(CH4_detrend[(106106-79999):(107765-79999)]),sd(CH4_detrend[(108002-79999):(109494-79999)]),
                      sd(CH4_detrend[(109514-79999):(110977-79999)]),sd(CH4_detrend[(111306-79999):(112947-79999)]),sd(CH4_detrend[(112990-79999):(114630-79999)]),sd(CH4_detrend[(114981-79999):(116291-79999)]),
                      sd(CH4_detrend[(116329-79999):(118139-79999)]),sd(CH4_detrend[(118760-79999):(120064-79999)]),sd(CH4_detrend[(120095-79999):(121684-79999)]),sd(CH4_detrend[(121835-79999):(123340-79999)]),
                      sd(CH4_detrend[(123375-79999):(123920-79999)]))

df_CH4$y<-mean_CH4_dry_detrend
df_CH4$sd_CH4_dry<-sd_CH4_dry_detrend
setwd("C:/Users/au615105/OneDrive - Aarhus universitet/Pablo Garc?a/Papers/Interferences pictures/H2O repetition")
### PLOTS ###
## Supplementary material, Full calibration ##
A<-ggplot()+
  geom_line(aes(x=Time_All,y=NH3_All,color="NH3"))+
  geom_line(aes(x=Time_All,y=H2O_All,color="H2O"))+
  scale_y_continuous(name = expression(paste("NH"[3]* " concentration, ppb")), sec.axis = sec_axis(~ . *1 ,expression(paste("H"[2]* "O concentration, ppb"))))+
  xlab("Time")+
  theme(panel.border =element_rect(colour = "grey", fill=NA))+
  theme(axis.line = element_line(colour = "black"))+
  theme(panel.spacing.x=unit(2, "lines"))+
  theme(panel.border =element_rect(colour = "grey", fill=NA))+
  theme(axis.line = element_line(colour = "black"))+
  theme(panel.spacing.x=unit(2, "lines"))+
  theme(legend.title=element_blank())
ggsave(plot=A,"S1rep_NH3.png", width = 12, height = 5)

A<-ggplot()+
  geom_line(aes(x=Time_All,y=N2O_dry_All,color="N2O_dry"))+
  geom_line(aes(x=Time_All,y=N2O_All,color="N2O"))+
  geom_line(aes(x=Time_All,y=H2O_All/20+0.3,color="H2O"))+
  scale_y_continuous(name = expression(paste("N"[2]* "O concentration, ppb")), sec.axis = sec_axis(~ (.-0.3)*20 ,expression(paste("H"[2]* "O concentration, ppb"))))+
  xlab("Time")+
  theme(panel.border =element_rect(colour = "grey", fill=NA))+
  theme(axis.line = element_line(colour = "black"))+
  theme(panel.spacing.x=unit(2, "lines"))+
  theme(panel.border =element_rect(colour = "grey", fill=NA))+
  theme(axis.line = element_line(colour = "black"))+
  theme(panel.spacing.x=unit(2, "lines"))+
  theme(legend.title=element_blank())
ggsave(plot=A,"S2rep_N2O.png", width = 12, height = 5)

A<-ggplot()+
  geom_line(aes(x=Time_All,y=CH4_dry_All,color="CH4_dry"))+
  geom_line(aes(x=Time_All,y=CH4_All,color="CH4"))+
  geom_line(aes(x=Time_All,y=H2O_All/20+2,color="H2O"))+
  scale_y_continuous(name = expression(paste("CH"[4]* " concentration, ppb")), sec.axis = sec_axis(~ (.-2)*20 ,expression(paste("H"[2]* "O concentration, ppb"))))+
  xlab("Time")+
  theme(panel.border =element_rect(colour = "grey", fill=NA))+
  theme(axis.line = element_line(colour = "black"))+
  theme(panel.spacing.x=unit(2, "lines"))+
  theme(panel.border =element_rect(colour = "grey", fill=NA))+
  theme(axis.line = element_line(colour = "black"))+
  theme(panel.spacing.x=unit(2, "lines"))+
  theme(legend.title=element_blank())
ggsave(plot=A,"S3rep_CH4.png", width = 12, height = 5)

pNH3<-ggplot(data=df_NH3,aes(x,y,color=Group))+
  #geom_point(size=3)+
  theme_bw()+theme(panel.grid=element_blank())+
  theme(panel.border =element_rect(colour = "grey", fill=NA))+
  theme(axis.line = element_line(colour = "black"))+
  theme(panel.spacing.x=unit(2, "lines"))+
  geom_errorbar(aes(ymin=y-sd_NH3, ymax=y+sd_NH3), 
                width=0.1,position=position_dodge(0))+
  geom_errorbar(aes(xmin=x-sd_H2O, xmax=x+sd_H2O), 
                width=0.1,position=position_dodge(0))+
  scale_y_continuous(name = expression(NH[3]~"Concentration, ppb"))+
  scale_x_continuous(name = expression(H[2]*O~"Concentration, %"))+
  scale_colour_manual("CRDS",values=c("grey1"))+
  scale_shape_manual("CRDS",values=c(NaN))+
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        legend.text=element_text(size=6),
        legend.title=element_text(size=8),
        legend.position = "none")
ggsave(plot=pNH3,"H2O vs NH3.png", width = 8, height = 5)

pN2O<-ggplot(data=df_N2O,aes(x,y,color=Group))+
  #geom_point(size=3)+
  theme_bw()+theme(panel.grid=element_blank())+
  theme(panel.border =element_rect(colour = "grey", fill=NA))+
  theme(axis.line = element_line(colour = "black"))+
  theme(panel.spacing.x=unit(2, "lines"))+
  geom_errorbar(aes(ymin=y-sd_N2O_dry, ymax=y+sd_N2O_dry), 
                width=0.1,position=position_dodge(0))+
  geom_errorbar(aes(xmin=x-sd_H2O, xmax=x+sd_H2O), 
                width=0.001,position=position_dodge(0))+
  scale_y_continuous(name = expression(N[2]*O~"Concentration, ppb"))+
  scale_x_continuous(name = expression(H[2]*O~"Concentration, %"))+
  scale_colour_manual("CRDS",values=c("grey1"))+
  scale_shape_manual("CRDS",values=c(NaN))+
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        legend.text=element_text(size=6),
        legend.title=element_text(size=8),
        legend.position = "none")
ggsave(plot=pN2O,"H2O vs N2O.png", width = 8, height = 5)

pCH4<-ggplot(data=df_CH4,aes(x,y,color=Group))+
  #geom_point(size=3)+
  theme_bw()+theme(panel.grid=element_blank())+
  theme(panel.border =element_rect(colour = "grey", fill=NA))+
  theme(axis.line = element_line(colour = "black"))+
  theme(panel.spacing.x=unit(2, "lines"))+
  geom_errorbar(aes(ymin=y-sd_CH4_dry, ymax=y+sd_CH4_dry), 
                width=0.1,position=position_dodge(0))+
  geom_errorbar(aes(xmin=x-sd_H2O, xmax=x+sd_H2O), 
                width=0.001,position=position_dodge(0))+
  scale_y_continuous(name = expression(CH[4]~"Concentration, ppb"))+
  scale_x_continuous(name = expression(H[2]*O~"Concentration, %"))+
  scale_colour_manual("CRDS",values=c("grey1"))+
  scale_shape_manual("CRDS",values=c(NaN))+
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        legend.text=element_text(size=6),
        legend.title=element_text(size=8),
        legend.position = "none")
ggsave(plot=pCH4,"H2O vs CH4.png", width = 8, height = 5)

setwd("C:/Users/au615105/OneDrive - Aarhus universitet/Pablo Garc?a/Papers")
write.table(df_N2O, file = "H2O_N2O.txt", sep = "\t", row.names = TRUE, col.names = NA)
write.table(df_NH3, file = "H2O_NH3.txt", sep = "\t", row.names = TRUE, col.names = NA)
write.table(df_CH4, file = "H2O_CH4.txt", sep = "\t", row.names = TRUE, col.names = NA)

## Test 3
##Make a duration vector
d_All3<-1
for (i in 1:length(Time_All)){
  d_All3[i]<-difftime(Time_All[0+i],Time_All[1],units="mins")
}

####################################
pH2O3<-ggplot()+
  geom_line(aes(x=d_All3,y=H2O_All,color="G2509"))+
  scale_y_continuous(name = expression(paste("")),labels = number_format(accuracy = 0.01))+
  xlab("Time (min)")+
  theme(panel.border =element_rect(colour = "grey", fill=NA))+
  theme(axis.line = element_line(colour = "black"))+
  theme(panel.spacing.x=unit(2, "lines"))+
  theme_bw()+theme(panel.grid=element_blank(),legend.position = "none")+
  scale_colour_manual("",values=c("#00BA38","#619CFF"))+
  theme(legend.title=element_blank())

pNH33<-ggplot()+
  geom_line(aes(x=d_All3,y=NH3_All,color="G2509"))+
  scale_y_continuous(name = expression(paste("")),labels = number_format(accuracy = 0.01))+
  xlab("")+
  theme(panel.border =element_rect(colour = "grey", fill=NA))+
  theme(axis.line = element_line(colour = "black"))+
  theme(panel.spacing.x=unit(2, "lines"))+
  theme_bw()+theme(panel.grid=element_blank(),legend.position = "none")+
  theme(legend.title=element_blank())+
  scale_colour_manual("",values=c("#00BA38","#619CFF"))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
pNH33<-tag_facet(pNH33,tag_pool = letters[3])


pCH43<-ggplot()+
  geom_line(aes(x=Time_All,y=CH4_dry_All,color="G2509"))+
  scale_y_continuous(name = expression(paste("")),labels = number_format(accuracy = 0.01))+
  xlab("")+
  theme(panel.border =element_rect(colour = "grey", fill=NA))+
  theme(axis.line = element_line(colour = "black"))+
  theme(panel.spacing.x=unit(2, "lines"))+
  theme_bw()+theme(panel.grid=element_blank(),legend.position = "none")+
  theme(legend.title=element_blank())+
  scale_colour_manual("",values=c("#00BA38","#619CFF"))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

pN2O3<-ggplot()+
  geom_line(aes(x=Time_All,y=N2O_All,color="G2509"))+
  scale_y_continuous(name = expression(paste("")))+
  xlab("")+
  theme(panel.border =element_rect(colour = "grey", fill=NA))+
  theme(axis.line = element_line(colour = "black"))+
  theme(panel.spacing.x=unit(2, "lines"))+
  theme_bw()+theme(panel.grid=element_blank(),legend.position = "none")+
  theme(legend.title=element_blank())+
  scale_colour_manual("",values=c("#00BA38","#619CFF"))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

S<-grid.arrange(arrangeGrob(pNH33,pN2O3,pCH43,pH2O3, ncol = 1,nrow=4))
ggsave(plot=S,"S1-3_H2O_Interferences.png", width = 8, height = 8)