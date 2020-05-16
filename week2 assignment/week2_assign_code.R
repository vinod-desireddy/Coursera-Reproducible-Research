url = 'https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip'
#download.file(url, 'data.zip', 'curl')
#a = unzip('data.zip', list = T)$Name
data = read.csv(unz('data.zip', 'activity.csv'))

colSums(is.na(data))
summary(data)
str(data)
data$date = as.Date(data$date, format = '%Y-%m-%d')
str(data)

library(dplyr)
stepspd = data %>% group_by(date) %>% summarise(spd = sum(steps, na.rm = T))

hist(stepspd$spd, main = 'Histogram of steps per day', xlab = 'steps per day')

meanspd = mean(stepspd$spd)
medianspd = median(stepspd$spd)

stepspi = data %>% group_by(interval) %>% summarise(spi = mean(steps, na.rm = T))
with(stepspi, plot(interval, spi, type = 'l',
                   ylab = 'Avg steps per interval',
                   main = 'Avg steps per interval averaged across all days'))

maxspi = stepspi[stepspi$spi == max(stepspi$spi),'interval']

numna = colSums(is.na(data))
stepsna = data
stepsna = merge(stepsna, stepspi, by.x = 'interval', by.y = 'interval', all.x = T)
stepsna[is.na(stepsna$steps), "steps"] = stepsna[is.na(stepsna$steps), "spi"]
stepsna$spi = NULL

stepspdna = stepsna %>% group_by(date) %>% summarise(spd = sum(steps))
hist(stepspdna$spd, main = 'Histogram of steps per day after filling NA values', xlab = 'steps per day')
meanspdna = mean(stepspdna$spd)
medianspdna = median(stepspdna$spd)
#after imputing the missing data with average of that interval, the total daily number of steps increased

stepsna = stepsna %>% mutate(day = weekdays(date))
stepsna$day = gsub('^(Monday|Tuesday|Wednesday|Thursday|Friday)', 'weekday', stepsna$day)
stepsna$day = gsub('^(Saturday|Sunday)', 'weekend', stepsna$day)
stepsna$day = as.factor(stepsna$day)

stepspina = stepsna %>% group_by(day, interval) %>% summarise(spi = mean(steps))

library(ggplot2)
ggplot(data = stepspina, aes(x = interval, y = spi)) +
      geom_line() +
      facet_grid(day~., scales = 'free') +
      ylab('Average steps') +
      ggtitle('Avg steps per interval averaged across weekday/weekend')
