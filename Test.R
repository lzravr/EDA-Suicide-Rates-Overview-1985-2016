library(ggplot2)

data <- read.csv("data/master.csv", stringsAsFactors = F)
suicidesByYear = aggregate(data$suicides_no, by=list(year=data$year), FUN=sum)

plot(suicidesByYear$year, suicidesByYear$x)

p<-ggplot(data=suicidesByYear, aes(x=year, y=x)) +
  geom_line(stat="identity") +
  theme_bw()
p
