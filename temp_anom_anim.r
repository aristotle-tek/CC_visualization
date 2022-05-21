#-----------------------------------
# Dynamic plot of temperature anomalies with data from
# Morice, Colin P., et al. "Quantifying uncertainties in global and 
# regional temperature change using an ensemble of observational estimates:
# The HadCRUT4 data set." Journal of Geophysical Research: Atmospheres 117.D8 (2012).
# https://www.metoffice.gov.uk/hadobs/hadcrut4/data/current/download.html

# visualization by Andrew Peterson

library(ggplot2)
library(gganimate)
library(reshape2)

library(av)# (for mpeg out)


# Column 1 is the date.
# Column 2 is the median of the 100 ensemble member time series.
# Columns 3 and 4 are the lower and upper bounds of the 95% confidence interval of bias uncertainty computed from the 100 member ensemble.
# Columns 5 and 6 are the lower and upper bounds of the 95% confidence interval of measurement and sampling uncertainties around the ensemble median. These are the combination of fully uncorrelated measurement and sampling uncertainties and partially correlated uncertainties described by the HadCRUT4 error covariance matrices.
# Columns 7 and 8 are the lower and upper bounds of the 95% confidence interval of coverage uncertainties around the ensemble median.
# Columns 9 and 10 are the lower and upper bounds of the 95% confidence interval of the combination of measurement and sampling and bias uncertainties.
# Columns 11 and 12 are the lower and upper bounds of the 95% confidence interval of the combined effects of all the uncertainties described in the HadCRUT4 error model (measurement and sampling, bias and coverage uncertainties).


df <- read.table("temp_dev_HadCRUT_annual_ns_avg_smooth.txt")

df <- df[,c("V1", "V2", "V11","V12")]
names(df) <- c('year', 'median', 'lower95','upper95')


mlt <- melt(df[,c('year','median','lower95','upper95')], id.vars=c('year'))


# simple static plot:
p <- ggplot() + geom_line(aes(y=value, x=year, group=variable, color=variable), data=mlt)+theme_bw()+
  scale_x_continuous(breaks = c(1860, 1880, 1900, 1920, 1940, 1960, 1980, 2000, 2019 ))+
  scale_y_continuous(breaks = c(-0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8)) +
  ylab("temperature anomalies (deg C) relative to 1961-1990") + xlab("")

col.scale <- c("median" = "black", "lower95" = "lightgrey", "upper95"='lightgrey')
p + scale_colour_manual(values = col.scale)


#-------------------------
# Dynamic plot using transition_reveal 
# view follow - axes dynamically revealed.
#------------------------

# NB: Will reverse color scheme for dark

red.opp <- "#00FFFF"

col.scale <- c("median" = red.opp, "lower95" = "lightgrey", "upper95"='lightgrey')


p <- ggplot(mlt, aes(year, value, color = variable)) + 
  geom_line() +  geom_point(size = 2) + 
  transition_reveal(year) + view_follow()+
  scale_x_continuous(breaks = c(1860, 1880, 1900, 1920, 1940, 1960, 1980, 2000, 2019 ))+
  scale_y_continuous(breaks = c(-0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8)) +xlab("") +
  labs(title = '', y = 'Temperature anomalies (°C), relative to 1961-1990',
       caption = "Visualization by Andrew Peterson\nData from Hadley Centre, HadCRUT4; 95% confidence intervals from full error model\n(measurement, sampling, bias and coverage uncertainties); decadally smoothed.") + 
  theme_minimal() + theme(legend.position = "none")+
  scale_colour_manual(values = col.scale)


myrenderer <- av_renderer('temperature_anoms.mp4', 
                          vfilter = 'negate=1, fade=in:0:15:color=black')
dd <- animate(p, renderer=myrenderer, 
              width=480, height=600, res=100, fps=10)
