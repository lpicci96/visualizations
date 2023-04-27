library(ggplot2)
library(tidyverse)
library(scales)
library(ggtext)

tuesdaydata <- tidytuesdayR::tt_load('2023-04-25')
london_marathon <- tuesdaydata$london_marathon


london_marathon %>% 
  mutate(notStarted = Applicants-Starters) %>% 
  select(Year, notStarted, Starters) %>% 
  filter(Year>=2010) %>% 
  filter(Year<=2020) %>% 
  pivot_longer(c(notStarted, Starters)) %>% 
  ggplot(aes(y=value, x=factor(Year), fill=name))+
  geom_bar(position="stack", stat="identity", alpha=0.9)+
  
  scale_fill_manual(values=c("#4b6292", "#a4516b"))+
  scale_y_continuous(labels=comma, expand = c(0,0), limits=c(0, 499000))+
  
  labs(title="<p>The London Marathon is becoming more and more popular",
       caption="Source: Nicola Rennie, LondonMarathon R package<br>
       Applicants and runners in the London Marathon since 2010<br>
       @lpicci96",
       subtitle="457 thousand people <b style='color:#4b6292'>applied</b> to run in <b>2020</b> but only 77 
       actually <b style='color:#a4516b'>started the race</b>")+
  ylab("Number of People")+

  theme_minimal()+
  theme(
    panel.background = element_rect(fill="#faf2e5", color="#faf2e5"),
    plot.background = element_rect(fill="#faf2e5", color="#faf2e5"),
    axis.text.x = element_text(angle = 30, hjust = 1, color="#5e5853"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(angle = 0, vjust = 1, hjust = 0.5,
                                margin = margin(r=-60), size=10),
    axis.ticks.x = element_line(linewidth = 0.5, color="#cbc3b8"),
    axis.ticks.length = unit(0.2, "cm"),
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color="#cbc3b8"),
    plot.title=element_markdown(color='#1a130b', hjust=0, margin = margin(t=5, b=5, l=-60), face='bold', size=16),
    plot.caption = element_markdown(color='#5f554c', hjust=0, margin = margin(t=10, l=-60)),
    plot.subtitle = element_markdown(color='#5f554c', hjust=0, margin = margin(t=5, b=20, l=-60), size=14 ),
    legend.position = "none"
   )->
  p
