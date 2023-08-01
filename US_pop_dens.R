library(dplyr)
library(ggplot2)
library(extrafont)

## Loading in the data

states <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-08-01/states.csv')
state_name_etymology <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-08-01/state_name_etymology.csv')

## Calculate Population density

states_new<-data.frame(states, densty=states$population_2020/states$land_area_mi2)

## Renaming 

states_new$state[states_new$state=="North Carolina"]<-"N.Carolina"
states_new$state[states_new$state=="South Carolina"]<-"S.Carolina"

## Create dataframe for 20 most densely populate states

top_states<-slice_max(.data=states_new,n=20,order_by = densty)

## Plot the data

ggplot(data=top_states,
       aes(x=densty,y=reorder(state,+densty),fill=state))+
  geom_vline(xintercept = c(250,500,750,1000,1250),
             linetype="dashed")+
  geom_col()+
  geom_point(aes(x=0,y=state,colour=state),
             size=7)+
  geom_point(aes(x=densty,y=state,colour=state),
             size=7)+
  labs(title = "Jam-packed",
       subtitle = "The 20 most densely populated US states determined by the average number of residents per \n square mile of land. Any of the state covered by water was not included.", 
       x="Average Residents per square mile.",
       caption="Data: Wikipedia",
       alt = "A horizontal bar chart that displays the 20 most densely populated US states determined by the average number of residents per sqaure mile")+
  geom_text(aes(label=state),
            hjust=1.1, 
            family=".SF Compact",
            colour="black")+
  theme(
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    plot.title = element_text(hjust=0.5, family=".SF Compact",size=20),
    plot.subtitle = element_text(hjust=0.5,size=15),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill="#F6E3BA"),
    panel.background = element_rect(fill="#F6E3BA"),
    legend.position = "none",
    axis.text.x = element_text(family=".SF Compact",size=15),
    axis.title.x = element_text(family = ".SF Compact",size=20),
    plot.caption = element_text(hjust=1,size=13,family=".SF Compact")
  )
