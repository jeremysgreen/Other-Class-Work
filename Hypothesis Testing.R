library(readxl)


setwd("/Users/jeremygreen/Desktop/")
GR <- read_excel('Gordon_Ramsay.xlsx', range = 'b5:c205')

head(GR)
#Gordon Ramsay PDF
#Do diners spend on average more than $40 on appetizers? 

#Greater than 40 is the alternative hypothesis because it is what you are testing
#Less than 40 is the null hypothesis

#Upper tail test because alternative hypothesis is 'greater than'

#If p-value is small enough, alternative hypothesis is true
#The larger the p-value is the more likely that the null hypothesis is true

x_bar = mean(GR$`App. Spend`)
s = sd(GR$`App. Spend`)
n = nrow(GR)


t = (x_bar-40)/ (s /sqrt(n))
pvalue = pt(t,df = n-1, lower.tail = F)
pvalue

#pvalue is .002, which means there is evidence for the alternative hypothesis
#or diners do spend more than $40 on appetizers

#pt() instead of pnorm()

#BSDA is easier way to find same answer
test1 = t.test(GR$`App. Spend`, alternative = 'greater', mu = 40)
test1$p.value
#this can also give you the pvalue which leads you to the same conclusion
#this only works with imported data 

#Babyboom PDF
x_bar = 343
s = 18
n = 30


z = (x_bar-340)/ (s /sqrt(n))
pvalue = pnorm(z, lower.tail = F)
pvalue
#in this question the pvalue is 0.18 meaning 
#the alternative hypothesis is rejected

#default p-value is 0.05-

#higher than this means the alternative is rejected
#lower than this means the alternative is proven true
#p-value could be specified as something different in the question


#Loyalty Program PDF
x_bar = 130
s = 40
n = 80

t = (x_bar-120)/ (s /sqrt(n))
pvalue = pt(t,df = n-1, lower.tail = F)
pvalue

#this pvalue is 0.014, which is lower than 0.05(default), proving the alternative
#hypothesis true

#type 1 and type 2 errors
#probability of type 1 error is 0.05










