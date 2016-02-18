#Original post: http://www.stat.columbia.edu/~martin/W2024/R3.pdf
pain = c(4, 5, 4, 3, 2, 4, 3, 4, 4, 6, 8, 4, 5, 4, 6, 5, 8, 6, 6, 7, 6, 6, 7, 5, 6, 5, 5)
drug = c(rep("A", 9), rep("B", 9), rep("C", 9))
migraine = data.frame(pain, drug)
names(migraine) = c("Pain", "Drug")
migraine

#Get the mean Pain for each group of Drug
aggregate(Pain ~ Drug, migraine, mean)

plot(pain ~ drug, data = migraine)
results = aov(pain ~ drug, data = migraine)
summary(results)

pairwise.t.test(pain, drug, p.adjust = "bonferroni")
TukeyHSD(results, conf.level = 0.95)
