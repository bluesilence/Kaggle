setwd('E:/Interest/kaggle/coupon/script')

demoDistrib = read.csv("../data/intermediate/MostPopularCouponInTokyoOverDemographics.csv", as.is=T)

head(demoDistrib)
names(demoDistrib) <- c('PurchaseMonth', 'Sex', 'Age', 'PurchasedCouponCount', 'PurchasedUserCount')

library(ggplot2)
ggplot(data = demoDistrib, aes(x = Age, y = PurchasedCouponCount, col = Sex)) + geom_line()

start_month <- min(demoDistrib$PurchaseMonth)
end_month <- max(demoDistrib$PurchaseMonth)
curr_month <- start_month
while (curr_month <= end_month) {
  # print(curr_month)
  demoDistribGraph <- ggplot(data = demoDistrib, aes(x = Age, y = PurchasedCouponCount, col = Sex)) + geom_line() + ggtitle(curr_month)
  ggsave(paste("../plot/demoDistribGraph_a262c7ff56a5cd3de3c5c40443f3018c_", curr_month, ".jpg", sep = ""), width = 12, height = 9, dpi = 150)
  curr_month = AddMonths(curr_month, 1)
}