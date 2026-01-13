apes <- read_table("orangutanCSV.csv")
apes[apes$location == "Borneo",]

males <- apes[apes$sex == "male",]
males
males[rev(order(males$weight.kg)),]
females<- apes[apes$sex == "female",]
range(females$weight.kg)

sumatra<- apes[apes$location == "Sumatra",]
mean(sumatra$weight.kg)
colnames(apes)

sum(apes$Tool.use == 'TRUE')
sum(females$weight.kg>40)
min(apes$weight.kg)
females<- females[rev(order(females$weight.kg)),]
x<- females[1:3,]
sum(x$weight.kg)

new_vec = c("ant", "bee", "wasp", "fly", "flea", "tick", "lice", "mite", "worm", "moth")
sample(new_vec, 4, replace = T)
sample(new_vec, 4, replace = T)
sample(new_vec, 4, replace = F)
sample(new_vec, 4, replace = F)


baseball[order(baseball$Win.Loss.Percent),]
baseball[rev(order(baseball$Win.Loss.Percent)),]

diff<- baseball$Wins-baseball$Losses
diff

which(diff == 203)

ratio<- baseball$Runs.Allowed/baseball$Runs.Scored
plot(ratio, baseball$Win.Loss.Percent)
mod<-lm(baseball$Win.Loss.Percent~ratio)
abline(mod)
summary(mod)

ggplot(baseball, aes (x = Runs.Allowed/Runs.Scored , y = Win.Loss.Percent )) + geom_point() + geom_smooth(method = lm)
