library(tidyverse)
library(ggplot2)
library(maps)
library(gganimate)
library(ggtext)

tuesdata <- tidytuesdayR::tt_load('2023-05-16')
tornados <- tuesdata$tornados


us_map <- map_data("state")
continental_states <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", 
                                      "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", 
                                      "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", 
                                      "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", 
                                      "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")



tornados %>% 
  filter(fat>0) %>% 
  filter(st %in% continental_states) %>% 
  filter(yr>=2015) %>% 
  ggplot(aes(y=slat, x=slon))+
  geom_polygon(data = us_map, aes(x = long, y = lat, group = group), 
               color = "white", fill = "#EEEEEE") +
  geom_point(aes(size = fat), alpha=0.6, color='#081248',fill='#081248', stroke=0)+
  scale_size(range = c(1, 10))+
  coord_map()+
  
  # legend
  geom_segment(x=-75, xend=-70, y=31.2, yend=31.2, size=0.2, color='grey')+
  geom_point(aes(x = -70, y = 30.3, size = 100), shape = 21, fill = "white", color = "#081248", stroke=0.5)+
  geom_segment(x=-75, xend=-70, y=30.1, yend=30.1, size=0.2, color='grey')+
  geom_point(aes(x = -70, y = 29.8, size = 10), shape = 21, fill = "white", color = "#081248", stroke=0.5)+
  annotate('text', label='100', y=31.7, x=-75, size=4, hjust=0, color = '#1a130b', )+
  annotate('text', label='10', y=30.5, x=-75, size=4, hjust=0, color = '#1a130b', )+
  annotate('text', label='Deaths', y=33, x=-75, size=4, hjust=0, color = '#1a130b', )+
  
  # add title, subtitle and caption - subtitle shows years as animation progresses
  labs(subtitle = "Year: {substr(closest_state, 1, 4)}", title='Deadly tornadoes in the United States since 2015',
       caption = 'Source: NOAA<br><i>by Luca Picci <b>@lpicci96</b>')+
       
  # theme
  theme_void()+
  theme(legend.position = 'none',
        plot.background = element_rect(color='white', fill='white'),
        plot.title=element_markdown(color='#1a130b', hjust=0, margin = margin(t=5, b=5, l=20), face='bold', size=20),
        plot.subtitle=element_markdown(color='#1a130b', hjust=0, margin = margin(t=5, b=5, l=20), face='bold', size=16),
        plot.caption=element_markdown(color='#1a130b', hjust=0, margin = margin(t=5, b=5, l=20), size=10)
        )+
  
  
  
  # amimation
  transition_states(
    date, transition_length = 0
  )+
  shadow_mark() # make dots persist
