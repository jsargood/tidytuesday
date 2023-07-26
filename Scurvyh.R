library(dplyr)
library(tidyr)
library(ggplot2)
library(extrafont)
library(extrafontdb)
## Loading data in 

scurvy <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-25/scurvy.csv')

## Data Wrangling 

scurvy<-scurvy %>% separate_wider_delim(gum_rot_d6, delim = "_", names = c("Gum Rot Likert", "Gum Rot Severity"))
scurvy<-scurvy %>% separate_wider_delim(skin_sores_d6, delim = "_", names = c("Skin Sores Likert", "Skin Sores Severity"))
scurvy<-scurvy %>% separate_wider_delim(weakness_of_the_knees_d6, delim = "_", names = c("WOK Likert", "WOK Severity"))
scurvy<-scurvy %>% separate_wider_delim(lassitude_d6, delim = "_", names = c("Lassitude Likert", "Lassitude Severity"))
scurvy<-scurvy %>% separate_wider_delim(fit_for_duty_d6, delim = "_", names = c("Fit for Duty Likert", "Fit for Duty Severity"))

scurvy<-data.frame(scurvy,Gum_Rot_coord=rep(1,12),Skin_Sore_coord=rep(2,12),WOK_coord=rep(3,12),Lassitude_coord=rep(4,12))

##Plotting of the data

ggplot(data=scurvy)+
  geom_tile(aes(x=Gum_Rot_coord,y=study_id,fill=Gum.Rot.Likert,width=1,height=1),colour="#23395d")+
  geom_tile(aes(x=Skin_Sore_coord,y=study_id,fill=Skin.Sores.Likert,width=1,height=1),colour="#23395d")+
  geom_tile(aes(x=WOK_coord,y=study_id,fill=WOK.Likert,width=1,height=1),colour="#23395d")+
  geom_tile(aes(x=Lassitude_coord,y=study_id,fill=Lassitude.Likert,width=1,height=1),colour="#23395d")+
  scale_fill_manual(values=c("green3","yellow3","orange3","red3"),breaks=c(0,1,2,3),labels=c("None","Mild","Moderate","Severe"))+
  scale_color_manual(values=fill)+
  labs(title = "Scurvy Remedies",
       subtitle = "In 1753, James Lind (aboard the HMS Salsibury) adminstered treatment to 12 sailors all of whom had contracted Scurvy.\n He tried 6 different remedies on the similarly afflicted sailors and then recorded the severity of these side effects after 6 days had passed. ",
       fill="Severity after 6 days",
       alt = "A tile plot displaying the severity of the symptoms from sailors suffering from scurvy. On the x axis we have the symptoms suffered and on the y axis are the different remedies admisntered.",
       caption = "Data: medicaldata R package.")+
  scale_x_continuous(name="Symptom", 
                     breaks=1:4,
                     labels = c("1"="Gum Rot", "2"="Skin Sores","3"="WOK","4"="Lassitude"))+
  scale_y_continuous(name="Remedy",
                     breaks=c(1.5,3.5,5.5,7.5,9.5,11.5), 
                     labels=c("1.5"="Cider","3.5"="Dilute Sulfuric Acid","5.5"="Vinegar","7.5"="Sea Water","9.5"="Citrus","11.5"="Purgative Mix"))+
  geom_hline(yintercept=c(11.5,10.5,9.5,8.5,7.5,6.5,5.5,4.5,3.5,2.5,1.5), 
             colour="#23395d",linewidth=4)+
  geom_hline(yintercept=c(10.5,8.5,6.5,4.5,2.5),
             linetype="dashed",
             colour="white")+
  geom_vline(xintercept = c(1.5,2.5,3.5),
             colour="#23395d",
             linewidth=4)+
  geom_vline(xintercept=c(1.5,2.5,3.5),
             linetype="dashed",
             colour="white")+
  theme( plot.title = element_text(hjust=0.5,family ="Georgia",size=30,face="bold",colour="lightblue3"),
         plot.subtitle = element_text(hjust=0.5,family="Georgia",colour="lightblue3",size=15),
    panel.grid.major = element_line(colour="#23395d"),
    panel.grid.minor = element_line(colour="#23395d"),
    panel.background = element_rect(fill="#23395d",colour = "#23395d"),
    plot.background = element_rect(fill="#23395d",colour = "#23395d"),
    legend.title = element_text(family = "Georgia",size=20, face="italic",colour="lightblue3"),
    legend.background = element_rect(fill="#23395d"),
    legend.text = element_text(size=15,family = "Georgia",colour = "lightblue3"),
    axis.ticks.x=element_blank(),
    axis.ticks.y=element_blank(),
    axis.text = element_text(family="Georgia",colour="lightblue", size=15),
    axis.title = element_text(family="Georgia",colour="lightblue",size=15, face="bold.italic"),
    plot.caption  =element_text(family="Georgia", colour="lightblue4",size=13, hjust=1)
  )