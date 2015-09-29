setwd('E:/Interest/kaggle/coupon/script')

# Registery
user_reg_count = read.csv("../data/intermediate/user_count_reg_month.csv", as.is=T)
nrow(user_reg_count)
names(user_reg_count) = c('USER_COUNT', 'REG_MONTH', 'PREF', 'LAT', 'LOG')

# User flow
user_flow = read.csv("../data/intermediate/user_count_reg_withdraw_month.csv", as.is=T)
names(user_flow) = c('PREF', 'MONTH', 'REG_USER_COUNT', 'WITHDRAW_USER_COUNT', 'LAT', 'LOG')

library(ggplot2)
  
# Get map
library(ggmap)
jpMap <- get_map(location = 'Japan', zoom = 5)

# Paint flow map
user_flow$LOG <- as.numeric(user_flow$LOG)
user_flow$LAT <- as.numeric(user_flow$LAT)
user_flow$MONTH <- as.Date(user_flow$MONTH)
# Net increased user count per month
# user_flow$USER_COUNT <- user_flow$REG_USER_COUNT - user_flow$WITHDRAW_USER_COUNT


# Normalize net increase by max net increase per PREF
max_reg_per_pref <- aggregate(REG_USER_COUNT ~ PREF, user_flow, max)

names(max_reg_per_pref) <- c('PREF', 'MAX_REG_USER_COUNT')
user_flow = merge(user_flow, max_reg_per_pref, by.x="PREF", by.y="PREF", all.x=T)
user_flow$REG_USER_COUNT_RATIO <- user_flow$REG_USER_COUNT / user_flow$MAX_REG_USER_COUNT
write.csv(user_flow, "../data/intermediate/user_flow_enriched.csv", row.names = F)

# Paint user flow per month
library("RColorBrewer")
myPalette <- colorRampPalette(rev(brewer.pal(3, "Spectral")))
sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(0, 1))
ss <- scale_size_continuous(limits= c(0, 24)) # max reg user count is 550
flowMap <- ggmap(jpMap, xlim = c(127, 153), ylim = c(25, 47)) + ggtitle(paste("Registry Heat - ", curr_month, sep = "")) + geom_point(data = subset(user_flow, MONTH == curr_month), aes(x = LOG, y = LAT, size = sqrt(REG_USER_COUNT), col = REG_USER_COUNT_RATIO)) + sc + ss
print(flowMap)
ggsave("../plot/userFlowMapPerMonth_2010-07-01.jpg", width = 12, height = 9, dpi = 150)

library(DescTools)
start_month <- min(user_flow$MONTH)
end_month <- max(user_flow$MONTH)
curr_month <- start_month
while (curr_month <= end_month) {
  # print(curr_month)
  flowMap <- ggmap(jpMap, xlim = c(127, 153), ylim = c(25, 47)) + ggtitle(paste("Registry Heat - ", curr_month, sep = "")) + geom_point(data = subset(user_flow, MONTH == curr_month), aes(x = LOG, y = LAT, size = sqrt(REG_USER_COUNT), col = REG_USER_COUNT_RATIO)) + sc + ss
  # print(flowMap)
  ggsave(paste("../plot/userFlowMapPerMonth_", curr_month, ".jpg", sep = ""), width = 12, height = 9, dpi = 150)
  curr_month = AddMonths(curr_month, 1)
}

# Line with Time series
# Tokyo
tokyoFlow <- subset(user_flow, PREF == 'Tokyo')
ggplot(data = tokyoFlow) + geom_line(aes(x = MONTH, y = REG_USER_COUNT)) + geom_line(aes(x = MONTH, y = WITHDRAW_USER_COUNT))
ggplot(data = tokyoFlow) + geom_line(aes(x = MONTH, y = USER_COUNT, col = SEX))

# Kyoto
kyotoFlow <- subset(user_flow, PREF == 'Kyoto')
ggplot(data = kyotoFlow) + geom_line(aes(x = MONTH, y = REG_USER_COUNT)) + geom_line(aes(x = MONTH, y = WITHDRAW_USER_COUNT))
ggplot(data = kyotoFlow) + geom_line(aes(x = MONTH, y = USER_COUNT, col = SEX))

# Overall
user_flow_plot_part1 <- ggplot(data = subset(user_flow, PREF > 'O')) + geom_line(aes(x = MONTH, y = USER_COUNT, col = SEX)) + facet_grid(PREF ~ .)
