#Original post: http://www.r-bloggers.com/one-way-analysis-of-variance-anova/
require(ggplot2)
plant.df = PlantGrowth
plant.df$group = factor(plant.df$group, labels = c("Control", "Treatment 1", "Treatment 2"))
ggplot(plant.df, aes(x = group, y = weight)) +
  geom_boxplot(fill = "grey80", colour = "blue") +
  scale_x_discrete() + xlab("Treatment Group") +
  ylab("Dried weight of plants")

plant.mod1 = lm(weight ~ group, data = plant.df)
summary(plant.mod1)
anova(plant.mod1)
confint(plant.mod1)

plant.mod = data.frame(Fitted = fitted(plant.mod1), Residuals = resid(plant.mod1), Treatment = plant.df$group)
ggplot(plant.mod, aes(Fitted, Residuals, colour = Treatment)) + geom_point()
