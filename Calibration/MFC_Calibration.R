library(readxl)
library(ggplot2)
#Read file with MFC calibration
setwd(".../Calibration")
  Dat<-read_excel("R_MFC_cal.xlsx")

aq8<-ggplot(data=Dat,aes(Expected_AQ8,Measured_AQ8))+geom_point(size=2.5,shape=1)+
  geom_abline(intercept =  13.11867169, slope = 1.120332402,linetype="dashed")+
  xlab("Expected flow (mL/min)")+ylab("Measured flow (mL/min)")+
  theme_bw()+theme(panel.grid=element_blank())+
  theme(panel.border =element_rect(colour = "grey", fill=NA))+
  theme(axis.line = element_line(colour = "black"))+
  theme(panel.spacing.x=unit(2, "lines"))+
  ggtitle("1000 mL/min mass flow controller")+
  #annotate("text", x=250, y=1000, label="y = (1.1 ? 0.6)x")
  annotate("text",
           x = 100, y = 1100, label = "y =  (1.120 ? 0.006)x + (13.12 ? 3.62)",
           color = "black", hjust = 0, vjust = -.1,size=2.9)+
  annotate("text",
           x = 100, y = 1100, label = "R^{2} == 0.9995   ",
           parse = TRUE,
           color = "black", hjust = 0, vjust = 1.1,size=2.9) 
aq8<-tag_facet(aq8,tag_pool = letters[1])

aq5<-ggplot(data=Dat,aes(Expected_AQ5,Measured_AQ5))+geom_point(size=2.5,shape=1)+
  geom_abline(intercept = 0.0163, slope = 1.0663 ,linetype="dashed")+
  xlab("Expected flow (L/min)")+ylab("Measured flow (L/min)")+
  theme_bw()+theme(panel.grid=element_blank())+
  theme(panel.border =element_rect(colour = "grey", fill=NA))+
  theme(axis.line = element_line(colour = "black"))+
  theme(panel.spacing.x=unit(2, "lines"))+
  ggtitle("5 L/min mass flow controller")+
  #annotate("text", x=250, y=1000, label="y = (1.1 ? 0.6)x")
  annotate("text",
           x = 1, y = 6, label = "y =  (1.0663 ? 0.0041)x + (0.0163 ? 0.0136)",
           color = "black", hjust = 0, vjust = 2.1,size=2.9)+
  annotate("text",
           x = 1, y = 6, label = "R^{2} == 0.99995",
           parse = TRUE,
           color = "black", hjust = 0, vjust = 3.1,size=2.9)  
aq5<-tag_facet(aq5,tag_pool = letters[2])

aq11<-ggplot(data=Dat,aes(Expected_AQ11,Measured_AQ11))+geom_point(size=2.5,shape=1)+
  geom_abline(intercept = 0.281032686, slope = 0.991710391 ,linetype="dashed")+
  xlab("Expected flow (L/min)")+ylab("Measured flow (L/min)")+
  theme_bw()+theme(panel.grid=element_blank())+
  theme(panel.border =element_rect(colour = "grey", fill=NA))+
  theme(axis.line = element_line(colour = "black"))+
  theme(panel.spacing.x=unit(2, "lines"))+
  ggtitle("50 L/min mass flow controller")+
  #annotate("text", x=250, y=1000, label="y = (1.1 ? 0.6)x")
  annotate("text",
           x = 4, y = 10, label = "y =  (0.99171 ? 0.00311)x + (0.28103 ? 0.02105)",
           color = "black", hjust = 0, vjust = 1.7,size=2.9)+
  annotate("text",
           x = 4, y = 10, label = "R^{2} == 0.99996",
           parse = TRUE,
           color = "black", hjust = 0, vjust = 2.7,size=2.9)
aq11<-tag_facet(aq11,tag_pool = letters[3])

library(egg)
B<-grid.arrange(arrangeGrob(aq8,aq5,aq11, ncol = 3,nrow=1))
ggsave(plot=B,"S3_MFC_Calibration.png", width = 10, height = 5)