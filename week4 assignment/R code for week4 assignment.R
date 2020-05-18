#https://rpubs.com/vinod-desireddy/616194
url = 'https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2'
download.file(url, 'data', 'curl')
data = read.csv('data.csv')

str(data)

library(dplyr)
library(lubridate)

data1 = data %>% 
      select(EVTYPE, MAG, FATALITIES, INJURIES, 
             PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP)
unique(data1$PROPDMGEXP)
table(data1$PROPDMGEXP)

data1[!tolower(data1$PROPDMGEXP) %in% c('k','m','b'), 'PROPDMGEXP'] = 0
data1$PROPDMGEXP = gsub('k', 1000, x = tolower(data1$PROPDMGEXP))
data1$PROPDMGEXP = gsub('m', 1000000, x = tolower(data1$PROPDMGEXP))
data1$PROPDMGEXP = gsub('b', 1000000000, x = tolower(data1$PROPDMGEXP))
data1$PROPDMGEXP = as.numeric(data1$PROPDMGEXP)

data1$propdmgexp = data1$PROPDMG * data1$PROPDMGEXP
data1$PROPDMG = NULL
data1$PROPDMGEXP = NULL

unique(data1$CROPDMGEXP)
table(data1$CROPDMGEXP)

data1[!tolower(data1$CROPDMGEXP) %in% c('k','m','b'), 'CROPDMGEXP'] = 0
data1$CROPDMGEXP = gsub('k', 1000, x = tolower(data1$CROPDMGEXP))
data1$CROPDMGEXP = gsub('m', 1000000, x = tolower(data1$CROPDMGEXP))
data1$CROPDMGEXP = gsub('b', 1000000000, x = tolower(data1$CROPDMGEXP))
data1$CROPDMGEXP = as.numeric(data1$CROPDMGEXP)

data1$cropdmgexp = data1$CROPDMG * data1$CROPDMGEXP
data1$CROPDMG = NULL
data1$CROPDMGEXP = NULL

q1 = data1 %>% group_by(EVTYPE) %>% 
      summarise(total_property_damage = sum(propdmgexp, na.rm = T),
                total_crop_damage = sum(cropdmgexp, na.rm = T)) %>%
      mutate(total_damage = total_property_damage + total_crop_damage)

library(tidyr)
q2 = gather(q1, key = 'damagetype', value = 'damagevalue', -EVTYPE)
q3 = q2 %>% group_by(damagetype) %>% summarise(damagevalue = max(damagevalue))
q3 = as.data.frame(q3)
q4 = q2[(q2$damagetype %in% q3[,'damagetype']) & (q2$damagevalue %in% q3[,'damagevalue']),]
q4$damagevalue = round(q4$damagevalue/10^9, 1)
q4 = as.data.frame(q4)
rm(q1, q2, q3)

library(ggplot2)
ggplot(data = q4, aes(x = damagetype, y = damagevalue))+
      geom_point() +
      geom_text(aes(label = paste(EVTYPE, damagevalue, 'Billion USD', sep = '-')), 
                vjust = 1.3) +
      xlab('type of damage') +
      ylab('damage value in billions USD') +
      ggtitle('Event with max damage in property, crops, property+crops')


r1 = data1 %>% group_by(EVTYPE) %>% 
      summarise(total_fatalities = sum(FATALITIES, na.rm = T),
                total_injuries = sum(INJURIES, na.rm = T)) %>%
      mutate(total_health_damage = total_fatalities + total_injuries)
r2 = gather(r1, key = 'damagetype', value = 'damagevalue', -EVTYPE)
r3 = r2 %>% group_by(damagetype) %>% summarise(damagevalue = max(damagevalue))
r3 = as.data.frame(r3)
r4 = r2[(r2$damagetype %in% r3[,'damagetype']) & (r2$damagevalue %in% r3[,'damagevalue']),]
r4 = as.data.frame(r4)
r4 = arrange(r4, damagevalue)
rm(r1, r2, r3)

ggplot(data = r4, aes(x = damagetype, y = damagevalue))+
      geom_point() +
      geom_text(aes(label = paste(EVTYPE, damagevalue, sep = '-')), 
                vjust = 1.3) +
      xlab('type of health damage') +
      ylab('No of persons affected') +
      ggtitle('Event with max damage in health, fatalities, fatalities+health')