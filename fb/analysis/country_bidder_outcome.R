setwd('E:/Interest/kaggle/fb/analysis')
data <- read.csv('country_bidder_outcome.csv', sep = ',', header = TRUE, stringsAsFactors = FALSE)
summary(data)

df <- data.frame(data)
names(df) <- c("country", "outcome", "bidders")
table(df$country)

head(df)

df.human <- subset(df, outcome == 0)
df.bot <- subset(df, outcome == 1)
df.bot$country
df.human$country
country.bot.only <- setdiff(df.bot$country, df.human$country)
country.bot.only
country.human.only <- setdiff(df.human$country, df.bot$country)
country.human.only

write.csv(file = "C:/Users/quinzh/OneDrive/项目/Personal Interest/Machine Learning/Kaggle/FB/sql/country.human.only.txt", country.human.only)

library(ggplot2)
ggplot(df, aes(x = country, y = bidders))
