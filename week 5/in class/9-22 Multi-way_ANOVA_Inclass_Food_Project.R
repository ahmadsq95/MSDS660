## MULTI WAY ANOVA IN CLASS ASSIGNMENT ## 

# Load the libraries

library(devtools)
library(qdata)
library(data.table)

# Load 'Interactions_Categorical.csv' data set

Interactions_Categorical <- read.csv("~/Python_class/Interactions_Categorical.csv")

# Convert data set to data.table

dt <- as.data.table(Interactions_Categorical)

# Check structure of dt

str(dt)

# Convert Food and Condiment to factors

dt$Food <- as.factor(dt$Food)
dt$Condiment <- as.factor(dt$Condiment)

# Want to see which food and condiment people enjoy most.  Do people enjoy hot dogs or ice cream more and which condiment do
# people prefer with their food.  It depends! People probably do not want mustard with ice cream and chocolate sauce with 
# hot dogs

# Plot histogram of Enjoyment

hist(dt$Enjoyment)

# Plot Enjoyment vs the 2 other factors 

plot.design(Enjoyment ~ ., data =dt)

# Plot Individual Boxplots with means

boxplot(Enjoyment ~ Food, data = dt, ylab = 'Enjoyment Factor', xlab = 'Food')
points(dt[, mean(Enjoyment), by=Food])

boxplot(Enjoyment ~ Condiment, data = dt, ylab = 'Enjoyment Factor', xlab = 'Condiment')
points(dt[, mean(Enjoyment), by=Food])

# Create interaction plot looking at Condiment and Food

interaction.plot(x.factor = dt$Food,
                 trace.factor = dt$Condiment, 
                 response = dt$Enjoyment,
                 fun = mean, 
                 type = "b",  # shows each point
                 main = "Interaction Plot",
                 legend = TRUE,
                 trace.label = "Condiment",
                 xlab = "Food",
                 ylab="Enjoyment",
                 pch=c(1))

# Build ANOVA model - the * is giving interactions. Show anova fit summary

fit <- aov(Enjoyment ~ Food * Condiment, data=dt)
summary(fit)

# Based on the model people like hot dogs and ice cream the same.  There is a food enjoyment depends on condiment and
# food and condiment together interact and affect people enjoyment

# Perform TukeyHSD to check if which interactions have a significant difference

TukeyHSD(fit)

# Plot the residuals of the fit

par(mfrow = c(2,2))
plot(fit)

# Perform Shapiro test to see if residuals are normal distributed.
shapiro.test(residuals(fit))
hist(residuals(fit), breaks=40)

# We do not reject our null hypothesis here
# We assume our residuals are normally distributed
