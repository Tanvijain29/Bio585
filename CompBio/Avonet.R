summary(Avonet)
?prcomp

##make sure PCA data are numeric, continuous, not correlated, not missing etc

data_col_names<- c("Beak.Length_Culmen", "Beak.Length_Nares",  "Beak.Width", "Beak.Depth", "Tarsus.Length", "Wing.Length", 
  "Kipps.Distance", "Secondary1", "Hand-Wing.Index", "Tail.Length", "Mass")

class(data_col_names)

avonet_data<- AVONET[,data_col_names]


##NO SORTING


#check if your data is complete

sum(complete.cases(avonet_data))
nrow(avonet_data)
#data is complete

#if data is incomplete: avonet_data[which(complete.cases(avonet_data)==TRUE),]



#checking if data is correlated
library(corrplot)
cor(avonet_data)
corrplot::corrplot(cor(avonet_data), method = "number", diag = F)


##taking out the correlated variables: beak length nares or culmen, wing length, either beak width or length
remove<- c("Beak.Length_Nares", "Beak.Depth", "Wing.Length")

avonet_data_uncor<- avonet_data[, -which(colnames(avonet_data)%in% remove)]


##doing the PCA

avonet_pca <- prcomp(x=avonet_data_uncor,center=TRUE,scale.=TRUE)

avonet_rotation <- avonet_pca$rotation
avonet_stdevs <- avonet_pca$sdev
avonet_centers <- avonet_pca$center
avonet_scales <- avonet_pca$scale
avonet_importance <- summary(avonet_pca)


##extract PC from PCA

avonet_pcdata <- as.data.frame(avonet_pca$x)

##glue pcs back to the original data
#do not sort
avonet_pca_plus_data <- cbind(Avonet,avonet_pcdata)



plot(avonet_pca_plus_data$PC1, avonet_pca_plus_data$PC2)



##doing the PCA without scale and center

avonet_pca2 <- prcomp(x=avonet_data_uncor,center=F,scale.=F)

avonet_rotation2 <- avonet_pca2$rotation
avonet_stdevs2 <- avonet_pca2$sdev
avonet_centers2 <- avonet_pca2$center
avonet_scales2 <- avonet_pca2$scale
avonet_importance2 <- summary(avonet_pca2)


##extract PC from PCA

avonet_pcdata2 <- as.data.frame(avonet_pca2$x)

##glue pcs back to the original data
#do not sort
avonet_pca_plus_data2 <- cbind(Avonet,avonet_pcdata2)

plot(avonet_pca_plus_data2$PC1, avonet_pca_plus_data2$PC2)


# broken stick
broken_stick <- function(P) {
  sequence<- 1:P
  divided <- 1/sequence
  seq_sums<- sapply(sequence,FUN=function(x){
    subset <- divided[x:P]
    subset_sum <-  sum(subset)
    i <- subset_sum/P
    return(i)
  })
  return(seq_sums)
}


broken_stick(P=8)
emp_imp<- avonet_importance$importance[2,]
null_imp<- broken_stick(P=8)

plot(null_imp, emp_imp)
abline(a=0, b=1)

plot(avonet_pca_plus_data$PC1, avonet_pca_plus_data$PC2)

#contrast categorical data with numeric data, like PCA data
##using ANOVA

my_aov <- aov(avonet_pca_plus_data$PC1 ~ avonet_pca_plus_data$Trophic.Level)
summary(my_aov)

boxplot(avonet_pca_plus_data$PC1 ~ avonet_pca_plus_data$Trophic.Level, xlab = "Trophic Level", ylab = "PC1", col = c("Brown", "Blue", "Pink", "Purple"))

TukeyHSD(my_aov)

my_lm<- lm(avonet_pca_plus_data$PC1 ~ log10(avonet_pca_plus_data$Range.Size))
summary(my_lm)

plot(log10(avonet_pca_plus_data$Range.Size), avonet_pca_plus_data$PC1, xlab = "Log10 Range Size", ylab = "PC1")
abline(my_lm)
unique(Avonet$Primary.Lifestyle)


Trophiclvl<- as.data.frame(Avonet[Avonet$Trophic.Level != "Scavenger",])

boxplot(Trophiclvl$Beak.Depth~Trophiclvl$Trophic.Level)


anotheraov<- aov(Trophiclvl$Beak.Depth~Trophiclvl$Trophic.Level)
summary(anotheraov)
TukeyHSD(anotheraov)



options("install.lock"=FALSE)
#read in the data
my_tps<- ("C:/Users/tanvi/Downloads/salamanders.tps")
mydata <- readland.tps(my_tps, specID="imageID")

dim(mydata)
#look at one individual
mydata[,,1]

#loading existing data set
data(plethodon)
#look at the structure of the data set
str(plethodon)

#plotallspecimens takes landmarks data and where to draw the links between the data under links
plotAllSpecimens(plethodon$land, links = plethodon$links)


#doing a procrustes analysis using gpagen
Y.gpa <- gpagen(plethodon$land, print.progress = F)

#plot this again
plotAllSpecimens(Y.gpa$coords, links = plethodon$links, label = TRUE, plot_param = list(txt.col = "red", txt.cex= 2.5))

#conducting a pca on this data using geomorph pca function
PCA <- gm.prcomp(Y.gpa$coords)
summary(PCA)
PCA$rotation
PCA$rotation[,1]
PCA$rotation[,2]
sort(abs(PCA$rotation[,1]))
sort(abs(PCA$rotation[,2]))
##lowest value is X.12, highest is 11.x; 1,3, 2, 11, 12 are important


plo1<- plot(PCA, col=as.numeric(as.factor(plethodon$species)), pch=as.numeric(as.factor(plethodon$species)))
legend(x = "topleft", legend = c("P. jordani", "P. teyahalee"),col= my_species_number, pch = my_species_number)




my_species_for_legend<- levels(as.factor(plethodon$species))
my_species_number<- 1:length(my_species_for_legend)

plot2<- plot(PCA, col=as.numeric(as.factor(plethodon$site)), pch=as.numeric(as.factor(plethodon$site)))
legend(x = "topleft", legend = c("Allopatric", "Sympatric"), col= c("black", "pink"), pch = 1:2)




plot3<- plot(PCA, col=as.numeric(as.factor(plethodon$species)), pch=as.numeric(as.factor(plethodon$site)) )


my_species_col_forplot3 <- as.numeric(as.factor(plethodon$species)) 
my_sites__pch_forplot3 <- as.numeric(as.factor(plethodon$site))

plot(PCA, col=my_species_col_forplot3,  pch=my_sites__pch_forplot3)
legend("topleft", legend = c("P. jordani Allopatric", "P. jordani Sympatric", "P. teyahalee Allopatric", "P. teyahalee Sympatric"), col = c("black", "black", "pink", "pink"), pch = c(1, 2, 1,2))




