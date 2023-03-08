## MULTI WAY ANOVA IN CLASS ASSIGNMENT ## 

# Load the libraries
library(data.table)
library(ggplot2)

# Load 'Interactions_Categorical.csv' data set
icdf <- read.csv("Interactions_Categorical.csv")
str(icdf)
summary(icdf)

# Convert data set to data.table
icdf <- as.data.table(icdf)

# Check structure of dt
str(icdf)

# Convert Food and Condiment to factors
icdf$Food <- as.factor(icdf$Food)
icdf$Condiment <- as.factor(icdf$Condiment)


# Want to see which food and condiment people enjoy most. Do people enjoy hot dogs or ice cream more and which condiment do
# people prefer with their food. It depends! People probably do not want mustard with ice cream and chocolate sauce with 
# hot dogs

# Plot histogram of Enjoyment
hist(icdf$Enjoyment, prob = TRUE, xlab = "Enjoyment Rate", main = "Enjoyment Distribution", col = "light green", data = icdf)



# Plot Enjoyment vs the 2 other factors
plot(Enjoyment ~ Food, data = icdf, main = "Food Enjoyment")
points(icdf[, mean(Enjoyment), by=Food], col = "red")

plot(Enjoyment ~ Condiment, data = icdf, main = "Condiment Enjoyment")
points(icdf[, mean(Enjoyment), by=Condiment], col = "red")
 
# Plot Individual Boxplots with means
points(icdf[, mean(Enjoyment), by=Food], col = "red")
points(icdf[, mean(Enjoyment), by=Condiment], col = "red")



# Create interaction plot looking at Condiment and Food
library(ggpubr)
ggboxplot(icdf, x = "Food", y ="Enjoyment" , color = "Condiment")



# Build ANOVA model - the * is giving interactions. Show anova fit summary
icdfaov1 <- aov(Enjoyment ~., data = icdf)
summary(icdfaov1)

icdfaov2 <- aov(Enjoyment ~ Food*Condiment, data = icdf)
summary(icdfaov2)

icdfaov3 <- aov(Enjoyment ~ Condiment + Food:Condiment, data = icdf)
summary(icdfaov3)



# Based on the model people like hot dogs and ice cream the same.  There is a food enjoyment depends on condiment and
# food and condiment together interact and affect people enjoyment


interaction.plot(x.factor = icdf$Food,
                 trace.factor = icdf$Condiment, 
                 response = icdf$Enjoyment,
                 fun = mean, 
                 type = "b",  # shows each point
                 main = "Food and Condiment Interaction Plot",
                 legend = TRUE,
                 trace.label = "Condiment",
                 xlab = "Food",
                 ylab="Number of cancer cases",
                 pch=c(1, 2),
                 col = c("light blue", "light green"))



# Perform TukeyHSD to check if which interactions have a significant difference
THSD <- TukeyHSD(icdfaov3, which = "Condiment:Food")
THSD1 <- as.data.table(resTHSD$"Condiment:Food", keep.rownames = TRUE)
THSD1[resTHSD1$`p adj` < .05]

# Plot the residuals of the fit
par(mfrow = c(2,2))
plot(icdfaov3)


# Perform Shapiro test to see if residuals are normally distributed.
?shapiro.test()
shapiro.test(residuals(icdfaov3))
hist(residuals(icdfaov3), breaks=40)


