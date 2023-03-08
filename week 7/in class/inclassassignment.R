##### NON-PARAMETRIC in class assignment #####

library(data.table)
library(BSDA)
library(nortest)
library(pspearman)

# Flu shot
flu <- fread('C:\\Users\\Ahmad\\Desktop\\MSDS\\MSDS660\\week 7\\in class\\new_flu_shot.csv')

mpg <- fread('C:\\Users\\Ahmad\\Desktop\\MSDS\\MSDS660\\week 7\\in class\\mpg.tsv')
# vocab training 
vocab <- fread('C:\\Users\\Ahmad\\Desktop\\MSDS\\MSDS660\\week 7\\in class\\vocab.csv')

View(flu)
View(mpg)
View(vocab)



# we're using SIGN test for flu dataset because we want to see if the medians of old vaccine and new vaccine are significantly different. 


flu$new.vaccine <- as.numeric(flu$new.vaccine)
SIGN.test(x = flu$old.vaccine, y = flu$new.vaccine, alternative = 'greater')
# p-value = 0.39 so we fail to reject Null hypothesis. 



# normality test to show if the data in normal
ad.test(flu$old.vaccine)
# p-value = 0.003 so data is normal  



# Kolmogorov-Smirnov 
# we used kolmogorov-smirnov test to show if there is difference in median between V1 and V2 in both ways.
ks.test(mpg$V1, mpg$V2, alternative = 'greater')
#p-value = 0.06621 so we fail to reject null hypothises.



# we gonna use spearman test to show correlation between before training and after training variables

cor(vocab[, c('before.training', 'after.training'), with=F], method = 'spearman')
# 0.925 correlation so two variables are strongly correlated 
spearman.test(vocab$before.training, vocab$after.training)
# rho 0.925 which means variables are correlated.