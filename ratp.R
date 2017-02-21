library(ggplot2)

tmp = read.csv(file = 'qualite-de-lair-mesuree-dans-la-station-chatelet.csv', header = T, sep = ";", na.strings = c('<5','<2','ND','#REF!'))
hours = c('00','01','10','11','12','13','14','15','16','17','18','19','02','20','21','22','23','03','04','05','06','07','08','09')
levels(tmp$HEURE) = hours
tmp$HEURE = factor(tmp$HEURE, levels = hours[order(hours)])

# obvious outliers
tmp = subset(tmp, CO2<1500)

# for clarity (outliers ?)
tmp = subset(tmp, PM10<500)
tmp$TEMP = strtoi(tmp$TEMP)

# date
tmp$DATE = as.character(tmp$DATE)
for (i in 1:nrow(tmp))
{
  if (length(grep('-',tmp[i,'DATE']))>0)
  {
    separator = '-'
  } else if (length(grep('/',tmp[i,'DATE']))>0)
  {
    separator = '/'
  }
  tmp[i,'DAY'] = strsplit(tmp[i,'DATE'],split = separator)[[1]][1]
  tmp[i,'MONTH'] = strsplit(tmp[i,'DATE'],split = separator)[[1]][2]
  tmp[i,'YEAR'] = strsplit(tmp[i,'DATE'],split = separator)[[1]][3]
}

tmp[tmp$MONTH=='janv','MONTH'] = 1
tmp[tmp$MONTH=='févr','MONTH'] = 2
tmp[tmp$MONTH=='mars','MONTH'] = 3
tmp[tmp$MONTH=='avr','MONTH'] = 4
tmp[tmp$MONTH=='mai','MONTH'] = 5
tmp[tmp$MONTH=='juin','MONTH'] = 6
tmp[tmp$MONTH=='juil','MONTH'] = 7
tmp[tmp$MONTH=='août','MONTH'] = 8
tmp[tmp$MONTH=='sept','MONTH'] = 9
tmp[tmp$MONTH=='oct','MONTH'] = 10
tmp[tmp$MONTH=='nov','MONTH'] = 11
tmp[tmp$MONTH=='déc','MONTH'] = 12
tmp$MONTH = strtoi(tmp$MONTH)
tmp$MONTH = as.factor(tmp$MONTH)

# various plots for CO2
ggplot(data = tmp, aes(x=HEURE,y=CO2,color=HEURE)) + geom_jitter() + geom_violin()
ggplot(data = tmp, aes(x=HEURE,y=CO2)) + geom_point()
ggplot(data = tmp, aes(x=DATE,y=CO2)) + geom_point()

# various plots for PM10
ggplot(data = tmp, aes(x=HEURE,y=PM10,color=HEURE)) + geom_jitter() + geom_violin()
ggplot(data = tmp, aes(x=HEURE,y=PM10)) + geom_boxplot(outlier.size = 0)

# plot for temperature
tmp_sub = subset(tmp, !is.na(MONTH))
ggplot(data = tmp_sub, aes(x=MONTH,y=TEMP,color=HEURE)) + geom_jitter(width = .3) 

