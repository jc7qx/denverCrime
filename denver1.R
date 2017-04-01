library(dplyr)  
library(lubridate)  
library(ggplot2)  
options("stringsAsFactors" = TRUE) 
CWD = getwd()  

data = read.csv(paste(CWD,'/data/crime.csv',sep=''))
data$date = as.Date(data$FIRST_OCCURRENCE_DATE)
#Create new columns for grouping
#Create new columns for grouping
data$year = year(data$date)  
data$month = month(data$date)  
data$day = day(data$date)  
data$hour = hour(data$FIRST_OCCURRENCE_DATE)

print(colnames(data)) #same as names(data)

#Sum up all incidents IS_CRIME AND IS_TRAFFIC
maxYear = max(data$year)  
maxMonthYTD = max(data$month[data$year==maxYear])

df = data %>%  
  group_by(year,month) %>%
  filter(month < maxMonthYTD) %>%
  summarise(incidents = sum(IS_CRIME) + sum(IS_TRAFFIC)) %>%
  arrange(month)


p = ggplot(df)  
p + geom_bar(aes(x = factor(year), weight = incidents)) + 
  ggtitle('Incidents Reported by Year') + xlab('Year') + ylab('Incidents') + 
  theme(plot.title = element_text(hjust = 0.5))

df1 = data %>%  
  group_by(year,month) %>%
  filter(month < 13) %>%
  summarise(incidents = sum(IS_CRIME) + sum(IS_TRAFFIC)) %>%
  arrange(month)


p = ggplot(df1,aes(x=factor(year),y=incidents,fill=factor(month)))
p + geom_bar(stat='identity') + ggtitle('Incidents Reported by Year') + 
  xlab('Year') + ylab('Incidents') + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  guides(fill = guide_legend(title='Month')) 

tmp= data  
tmp$crimeType[tmp$IS_CRIME == 1] = 'Crime'  
tmp$crimeType[tmp$IS_CRIME == 0] = 'Traffic'  
tmp$crimeType = factor(tmp$crimeType)

df2 = tmp %>%  
  group_by(year,crimeType) %>%
  filter(month < maxMonthYTD) %>%
  summarise(crimeIncidents = sum(IS_CRIME) + sum(IS_TRAFFIC)) %>%
  arrange(year)

p = ggplot(df2,aes(x=factor(year),y=crimeIncidents,fill=crimeType))  
p + geom_bar(stat='identity') + ggtitle('Incidents Reported by Year') + 
  xlab('Year') + ylab('Incidents') + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  guides(fill = guide_legend(title='Incident Type'))

