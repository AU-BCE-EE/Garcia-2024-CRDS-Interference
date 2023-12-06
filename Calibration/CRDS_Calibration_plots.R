library(readxl)
library(lubridate)
library(ggplot2)
library(anytime)
library(dplyr)
library(writexl)

### Plot calibrations together ###

setwd(".../Calibration")
df_Cal<-read_excel("Calibration_R.xlsx")
A<-which(df_Cal$Compound=="NH3")
B<-which(df_Cal$Compound=="N2O")
C<-which(df_Cal$Compound=="CH4")
df_Cal$Measured[A]<-df_Cal$Measured[A]/1000
df_Cal$Expected_MFC[A]<-df_Cal$Expected_MFC[A]/1000
df_Cal$Sd[A]<-df_Cal$Sd[A]/1000
df_Cal$text<-df_Cal$Expected
df_Cal$text[A]<-3
df_Cal$text[B]<-2
df_Cal$text[C]<-1

Eq_BP<-lm(df_Cal$Measured[1:21] ~ df_Cal$Expected_MFC[1:21]) 
Coeff_BP<-coefficients(Eq_BP)

Eq_CH4<-lm(df_Cal$Measured[22:42] ~ df_Cal$Expected_MFC[22:42]) 
Coeff_CH4<-coefficients(Eq_CH4)

Eq_NH3<-lm(df_Cal$Measured[43:55] ~ df_Cal$Expected_MFC[43:55]) 
Coeff_NH3<-coefficients(Eq_NH3)

Eq_NH32<-lm(df_Cal$Measured[56:68] ~ df_Cal$Expected_MFC[56:68]) 
Coeff_NH32<-coefficients(Eq_NH32)

Eq_N2O<-lm(df_Cal$Measured[69:85] ~ df_Cal$Expected_MFC[69:85]) 
Coeff_N2O<-coefficients(Eq_N2O)


### Plot all calibration together ###
dat_text <- data.frame(
  label = c("y == (0.887 %+-% 0.002)*x + (0.251 %+-% 0.018)",
            "R^2 == 0.9999",
            "y == (0.896 %+-% 0.002)*x + (0.239 %+-% 0.019)",
            "R^2 == 0.9999",
            "y == (0.956 %+-% 0.016)*x - (0.00306 %+-% 0.02067)", 
            "R^2 == 0.9959",
            "y == (0.942 %+-% 0.010)*x + (35.6 %+-% 78.0)",
            "R^2 == 0.9987",
            "y == (0.936 %+-% 0.010)*x + (37.8 %+-% 77.6)",
            "R^2 == 0.9987"),
  
  text = c(1,1,1,1,2,2,3,3,3,3),
  x     = c(8.7,8.7, 8.7,8.7,1.5,1.5, 6.5,6.5,6.5,6.5),
  y     = c(18.4, 17.4,15.9,14.9,2.8,2.6, 16,15,13.5,12.5),
  Group = c("G4301","G4301","G2509","G2509","G2509","G2509","G2103","G2103","G2509","G2509"),
  Compound = c("CH4","CH4","CH4","CH4","N2O","N2O","NH3","NH3","NH3","NH3")
)
my_labeller <- as_labeller(c("1"="CH[4]", "2"="N[2]*O", "3"="NH[3]"),
                           default = label_parsed)

pcal<-ggplot(data=df_Cal,aes(Expected_MFC,Measured,color=Group))+
  geom_point(shape=1,size=3)+
  facet_wrap(~text, ncol=3,scales="free",
             labeller = my_labeller)+
  theme_bw()+theme(panel.grid=element_blank())+
  theme(panel.border =element_rect(colour = "grey", fill=NA))+
  theme(axis.line = element_line(colour = "black"))+
  theme(panel.spacing.x=unit(2, "lines"))+
  theme(strip.text = element_text(size=20),
        axis.text = element_text(size=14),
        axis.title=element_text(size=18),
        legend.text = element_text(size=14))+
  scale_x_continuous(name = expression("Expected concentration (ppm)"))+
  scale_y_continuous(name = expression("Measured concentration (ppm)"))+
  scale_color_manual('',values = c("#F8766D","#619CFF","#00BA38"))+
  #scale_shape_manual('Compound',values = c(0,1,2))+
  geom_errorbar(aes(ymin=Measured-Sd, ymax=Measured+Sd), 
                width=0.1,position=position_dodge(0))+
  geom_abline(data = data.frame(xint=1,text=1), aes(intercept = Coeff_BP[1],slope=Coeff_BP[2]), linetype = "dashed",color="#00BA38")+
  geom_abline(data = data.frame(xint=1,text=1), aes(intercept = Coeff_CH4[1],slope=Coeff_CH4[2]), linetype = "dashed",color="#619CFF")+
  geom_abline(data = data.frame(xint=1,text=2), aes(intercept = Coeff_N2O[1],slope=Coeff_N2O[2]), linetype = "dashed",color="#619CFF")+
  geom_abline(data = data.frame(xint=1,text=3), aes(intercept = Coeff_NH3[1],slope=Coeff_NH3[2]), linetype = "dashed",color="#619CFF")+
  geom_abline(data = data.frame(xint=1,text=3), aes(intercept = Coeff_NH32[1],slope=Coeff_NH32[2]), linetype = "dashed",color="#F8766D")+
  geom_text(data  = dat_text,parse=TRUE, mapping = aes(x = x, y = y, label = label),size=4.5,show.legend  = FALSE)


ggsave(plot=pcal,"S1_Picarro_calibration.png", width = 14.8, height = 6)


