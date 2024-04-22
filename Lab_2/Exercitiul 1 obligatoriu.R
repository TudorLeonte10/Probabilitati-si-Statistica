tablou =  read.csv("life_expect.csv", header=T, sep=',')

male=tablou[['male']]

interval = seq(60, 89, 4)
hist(male, breaks=interval, right=T, freq=F)

female=tablou[['female']]

interval1 = seq (72, 86, 2)
hist(female, breaks=interval1, right=T, freq=F)
