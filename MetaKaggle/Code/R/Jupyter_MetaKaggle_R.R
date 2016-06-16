library(data.table)
library(ggplot2)

users_submissions <- fread('D:/Kaggle/MetaKaggle/Data/Output/users_submissions.tsv', sep = "\t", stringsAsFactors = FALSE)
head(users_submissions)

class(users_submissions$DateSubmitted)

users_submissions$DateSubmitted = as.Date(users_submissions$DateSubmitted)

users_submissions_plot = ggplot(data = users_submissions, aes(x = DateSubmitted, y = UserRanking))
users_submissions_plot = users_submissions_plot + stat_bin2d(binwidth = c(7, 500)) + scale_fill_gradientn(colors = colorRampPalette(c("white", "red"))(100))
users_submissions_plot

png(filename = "D:/Kaggle/MetaKaggle/Plot/users_submissions_by_time.png")
plot(users_submissions_plot)
dev.off()