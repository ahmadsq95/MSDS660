
library(data.table)
library(BSDA)

drug <- fread('C:\\Users\\Ahmad\\Desktop\\MSDS\\MSDS660\\week 7\\assignment\\placebo_new_drug.csv')

View(drug)

median(drug$Placebo)
median(drug$New.Drug)


SIGN.test(x = drug$Placebo, y = drug$New.Drug, alternative = 'greater')
