library(readr)
AVONET <- read_csv("GitHub/Bio585/CompBio/AVONET.csv")


songbirds<- AVONET[AVONET$Order3 == 'Passeriformes',]


songbirds1<- songbirds %>% dplyr::select(Species3, Family3, Order3, Female,Male, Beak.Length_Culmen,Beak.Width, Beak.Depth)

complexbirds <- songbirds1[songbirds1$Species3 %in% c( "Vireo olivaceus", "Vireo philadelphicus", "Pheucticus ludovicianus", "Aimophila aestivalis"
,"Emberiza schoeniclus", "Sturnella magna", "Piranga ludoviciana","Piranga olivacea", "Piranga rubra", "Carduelis chloris", "Acrocephalus arundinaceus",
"Acrocephalus palustris", "Acrocephalus schoenobaenus" , "Acrocephalus scirpaceus", "Phylloscopus trochilus", "Cistothorus platensis" , "Thryothorus ludovicianus",
"Troglodytes aedon" , "Dumetella carolinensis", "Mimus polyglottos", "Luscinia megarhynchos", "Erithacus rubecula", "Turdus merula" , "Turdus migratorius" , 
"Turdus philomelos" , "Turdus viscivorus"),]

complexbirds$RepSize<- c(43, 25, 300, 20, 20, 70, 300, 300, 300, 300, 300, 300, 300, 300, 31, 110, 28, 300, 300,300, 220, 300, 32, 300, 171, 300)

hist(complexbirds$Beak.Length_Culmen, xlab = "Beak Culmen Length", main = " ")
hist(complexbirds$Beak.Width, xlab = "Beak Width", main = " ")
hist(complexbirds$RepSize, xlab = "Song Repertoire Size", main = " ")


complexbirds$tbeaklengthculmen<- log(complexbirds$Beak.Length_Culmen)
hist(complexbirds$tbeaklengthculmen)

complexbirds$tbeakwidth<- log(complexbirds$Beak.Width)
hist(complexbirds$tbeakwidth)

complexbirds$trepsize<- log10(complexbirds$RepSize)
hist(complexbirds$trepsize)


complex_lm<- lm(RepSize ~ tbeaklengthculmen+ tbeakwidth, data = complexbirds)
summary(complex_lm)


figure1.lm<- lm(RepSize ~ tbeaklengthculmen, complexbirds)
plot(complexbirds$tbeaklengthculmen,complexbirds$RepSize, xlab ="Beak Culmen Length",  ylab="Song Repertoire")
abline(figure1.lm)
cor(complexbirds$RepSize, complexbirds$tbeaklengthculmen)
text(x = 3.0, y = 230, "R = 0.24")

figure2.lm<- lm(RepSize ~ tbeakwidth, complexbirds)
plot(complexbirds$tbeakwidth, complexbirds$RepSize, xlab = "Beak Width", ylab="Song Repertoire")
abline(figure2.lm)
cor(complexbirds$RepSize, complexbirds$tbeakwidth)
text(x = 1.4, y = 220, "p = 0.09")


##trying out k means clustring
complex_bird<- complexbirds %>% select(Beak.Length_Culmen, Beak.Width)
complexbirdssscaled<- scale(complex_bird)

k.analysis<- kmeans(complexbirdssscaled, centers = 2, nstart =20)
k.analysis
library(factoextra)

complexbirds$cluster<- c("Big", "Big", "Big" ,"Big" , "Small", "Small", "Big", "Big","Small", "Big", "Small", "Small", "Big", "Small", "Small", "Small", "Small" ,"Small", "Small", "Small", "Big", "Big", "Big", "Big", "Small", "Small")

boxplot(complexbirds$RepSize~ complexbirds$cluster, xlab= "Cluster", ylab = "Song Repertoire Size")
options("install.lock"=FALSE)

fviz_cluster(k.analysis, complexbirdssscaled )


vb<- aov(RepSize ~ cluster, data = complexbirds)
summary(vb)



