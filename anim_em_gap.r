#----------------
# Dynamic plot of current policies versus 1.5 Paris compatible
# Data from Climate Action Tracker
# https://climateactiontracker.org/global/cat-emissions-gaps/
# Andrew Peterson

# NB: Because I was doing this for a video where I wanted to be able to 
# show them separately, I create two animations and overlay them.

rm(list=ls(all=T))

library(dplyr)
library(ggplot2)
library(gganimate)
library(reshape2)
library(av)# mpeg out
library(readxl)

len <- length

df <- read_excel("CAT_2021-05_PublicData_EmissionGap.xlsx", skip=14)

#setwd("~/Documents/")

df <- df[1:11,3:43] # skip empty rows and cols 1,2 with scenario info

# transpose 
dft <- as_tibble(cbind(nms = names(df), t(df)))
head(dft)
names(dft)<-  c("year", "Post-COVID-19", "Current_Policy","Pledges_Targets_high",
                "Pledges_Targets_low", "deg2high", 'deg2med','deg2low',
                'deg15high', 'deg15med','deg15low',
                "Historical")
rm(df)

setwd("~/Desktop")

head(dft)

text.size = 40
lab.size = 15 # diff scale somehow...

pink.opp <- '#1B9E77'
light.green.opp <- '#E7298A'
dirty.yellow.opp <- '#7570B3'
yellow.opp <- '44CCFF'
lightblue.opp <- '#D95F02'
teal.opp <- "#F8766D"
salmon.opp <- "#7CAE00"
lime.green.opp <-  "#FF44DD" 
magenta.opp <- "004499" 
green.opp <- "#FF00FF"
dark.green.opp <- "#FF88FF"
red.opp <- "#00FFFF"

#--- Let's compare just 1.5°C consistent med scenario against Current policy.

#goal <- 'deg15med'
compare <- 'Current_Policy'

# include ranges:
df <- dft[,c('year', "deg15high","deg15med","deg15low", "Current_Policy",'Historical')]


names(df)<- c('year',"deg15high","deg15med","deg15low",'compare','historical')

# drop goal for observed data:
df$deg15med[df$year<2015]<- NA
df$deg15low[df$year<2015]<- NA
df$deg15high[df$year<2015]<- NA
df$historical[df$year>2015]<- NA

#-----------------
# hist + goal

mlt <- melt(df, id.vars=c('year'))
mlt$value <- as.numeric(mlt$value)
mlt$year <- as.numeric(mlt$year)

mlt.ribbon <- melt(df[,c('year','compare','historical','deg15med')], id.vars=c('year'))
mlt.ribbon$value <- as.numeric(mlt.ribbon$value)
mlt.ribbon$year <- as.numeric(mlt.ribbon$year)
mlt.ribbon$deg15high <- c(rep(NA, 82), df$deg15high)
mlt.ribbon$deg15low <- c(rep(NA, 82), df$deg15low)

mlt.ribbon <- mlt.ribbon[mlt.ribbon$variable!='compare',]
mlt.ribbon <- mlt.ribbon[!is.na(mlt.ribbon$value),]


mlt.ribbon$deg15low <- as.numeric(mlt.ribbon$deg15low)
mlt.ribbon$deg15high <- as.numeric(mlt.ribbon$deg15high)

#--- static plot -------
goal.color <- dark.green.opp
color.values.nopolicy <- c("historical"='black', 
                           'deg15med'=goal.color,
                           'deg15low'=green.opp,
                           'deg15high'=green.opp)



ggplot(mlt.ribbon, aes(year, value, group=variable, color =variable)) + 
  geom_ribbon(aes(ymin =deg15low, ymax = deg15high), fill = light.green.opp)+
  geom_line(aes(color=variable, size=2)) +  geom_point(size = 2)+
  scale_x_continuous(limits=c(1990, 2030), breaks=c(1990, 1995, 2000, 2005,2010,2015, 2020, 2025,2030))+
  scale_y_continuous(limits=c(20, 58), breaks=c(20, 25, 30, 35, 40, 45, 50, 55))+
  labs(title = '', y = expression('Gigatons CO'[2]*' equivalent'), x='',
       caption = "Visualization by Andrew Peterson\nData from Climate Action Tracker") + 
  theme_minimal() + theme(legend.position = "none", 
                          text=element_text(size=text.size),
                          plot.caption=element_text(size=20),
                          axis.title=element_text(size=35),
                          axis.text=element_text(size=40),# size of number
                          plot.margin=margin(40, 40, 40, 40))+
  #transition_reveal(year) + #view_follow()+
  scale_colour_manual(values = color.values.nopolicy)+
  geom_text(x=2000, y=48, label="Historical",
            color="black", size=lab.size)+
  geom_text(x=2023, y=58, label="1.5°C Compatible",
            color=goal.color,  size=lab.size)


#--- animated plot -------

p.hg <- ggplot(mlt.ribbon, aes(year, value, group=variable, color =variable)) + 
  geom_ribbon(aes(ymin =deg15low, ymax = deg15high), fill = light.green.opp)+
  geom_line(aes(color=variable, size=2)) +  geom_point(size = 2) + 
  scale_x_continuous(limits=c(1990, 2030), breaks=c(1990, 1995, 2000, 2005,2010,2015, 2020, 2025,2030))+
  scale_y_continuous(limits=c(20, 58), breaks=c(20, 25, 30, 35, 40, 45, 50, 55))+
  labs(title = '', y = expression('Gigatons CO'[2]*' equivalent'), x='',
       caption = "Visualization by Andrew Peterson\nData from Climate Action Tracker") + 
  theme_minimal() + theme(legend.position = "none", 
                          text=element_text(size=text.size),
                          plot.caption=element_text(size=20),
                          axis.title=element_text(size=35),
                          axis.text=element_text(size=40),# size of number
                          plot.margin=margin(40, 40, 40, 40))+
  transition_reveal(year) + #view_follow()+
  scale_colour_manual(values = color.values.nopolicy)+
  geom_text(x=2000, y=48, label="Historical",
            color="black", size=lab.size)+
  geom_text(x=2023, y=58, label="1.5°C Compatible",
            color=goal.color,  size=lab.size)



myrenderer <- av_renderer('em_gap_1pt5_compatible.mp4', 
                          vfilter = 'negate=1') 
dd <- animate(p.hg, renderer=myrenderer, 
              width=1920, height=1080, res=100, fps=10)


#---------------------------
# now with current policy
#---------------------------

mlt.real <- melt(df[,c('year','compare','historical', 'deg15med')], id.vars=c('year'))
mlt.real$value <- as.numeric(mlt.real$value)
mlt.real$year <- as.numeric(mlt.real$year)

mlt.real$deg15high <- c(rep(NA, 82), df$deg15high)
mlt.real$deg15low <- c(rep(NA, 82), df$deg15low)
mlt.real <- mlt.real[!is.na(mlt.ribbon$value),]

mlt.real$deg15low <- as.numeric(mlt.real$deg15low)
mlt.real$deg15high <- as.numeric(mlt.real$deg15high)

ggplot(mlt.real, aes(year, value, group=variable, color =variable)) + 
  geom_ribbon(aes(ymin =deg15low, ymax = deg15high), fill = light.green.opp)+
  geom_line(aes(color=variable, size=2))

dev.off()


color.values.currpolicy <- c("historical"='black',  # last color will be on top
                             "compare"=red.opp)



#-----
mlt.real <- mlt.real[mlt.real$variable!='deg15med',]
# for overlay
p.currpol <- ggplot(mlt.real, aes(year, value, group=variable, color =variable)) + 
  geom_line(aes(color=variable, size=2)) +  geom_point(size = 2) + 
  scale_x_continuous(limits=c(1990, 2030), breaks=c(1990, 1995, 2000, 2005,2010,2015, 2020, 2025,2030))+
  scale_y_continuous(limits=c(20, 58), breaks=c(20, 25, 30, 35, 40, 45, 50, 55))+
  labs(title = '', y = expression('Gigatons CO'[2]*' equivalent'), x='',
       caption = "Visualization by Andrew Peterson\nData from Climate Action Tracker") + 
  theme_minimal() + theme(legend.position = "none", 
                          text=element_text(size=text.size),
                          plot.caption=element_text(size=20),
                          axis.title=element_text(size=35),
                          axis.text=element_text(size=40),# size of number
                          plot.margin=margin(40, 40, 40, 40))+
  transition_reveal(year) + #view_follow()+
  scale_colour_manual(values = color.values.currpolicy)+
  geom_text(x=2000, y=48, label="Historical",
            color="black", size=lab.size)+
  geom_text(x=2028, y=45, label="Current\nPolicy",
            color=red.opp,  size=lab.size)

myrenderer <- av_renderer('em_gap_currpol.mp4', 
                          vfilter = 'negate=1')
dd <- animate(p.currpol, renderer=myrenderer, 
              width=1920, height=1080, res=100, fps=10)

