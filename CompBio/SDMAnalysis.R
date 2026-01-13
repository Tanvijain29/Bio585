options("install.lock"=FALSE)
library(dismo)
library(sf)
library(terra)
library(spThin)
library(maxnet)
library(ENMTools)
library(maps)
library(fs)
library(Rcpp)

##importing occurrence data

library(readr)
manatee <- read_csv("Manateerecs.csv")
View(manatee)
manatee<- manatee[-6,]#removed duplicate value

##importing raster data
raster_layer_path<- "C:/Users/tanvi/Documents/ClimateData/ManateeAnaysis/Layers"
raster_filenames<-list.files(path = raster_layer_path, pattern = "*.asc", recursive=FALSE, full.names=TRUE)
Env <- stack(raster_filenames)



#ensuring latitudes and longitudes are numeric

manatee$Latitude = as.numeric(manatee$Latitude)
manatee$Longitude = as.numeric(manatee$Longitude)

extr = raster::extract(Env, manatee[,c("Longitude", "Latitude")]) 

manatee_clean = manatee[!is.na(extr[,1]),]

##thinning data
?spThin::thin
manatee_thin <- spThin::thin(manatee_clean, lat.col = "Latitude",long.col = "Longitude", spec.col = "Species", thin.par =10, reps = 1, locs.thinned.list.return = TRUE, write.files = TRUE, max.files = 1, write.log.file = FALSE, out.dir=getwd(), out.base = "thinned_data")
manatee_thin_1 <- manatee_thin[[1]]
plot(manatee_thin_1)

##convert latitudes and longitudes
localities <- manatee_thin_1[, c("Longitude", "Latitude")]
localities_pol <- terra::vect(as.matrix(localities), "points")
class(localities_pol)


##generating background

bg1 <- ENMTools::background.buffer(points=localities_pol,buffer.width = 1, n = (100*nrow(localities_pol)), mask = as(Env[[1]], "SpatRaster"), buffer.type="circles", return.type="points")
bg1 <- terra::as.data.frame(bg1, geom="XY")

plot(bg1, col = "black", pch = 1, xlab =  "Longitude", ylab = "Latitude")
points(manatee_thin_1$Longitude, manatee_thin_1$Latitude, col = "red", pch = 0)


##run SDM using maxent in R
## set values for regularization to test
RMvalues <- seq(0.5, 4, 0.5)
## set types of models to test: linear, linear+quadratic, hinge
fc <- c("L", "LQ", "H")
## set partition method: block for training-testing split
method <- "block"


#correcting error
colnames(bg1)<- c("Longitude", "Latitude")

#converting Env to SpatVector from RasterStack

Env<- rast(Env)
terraOptions(memfrac=0.2)
res <- ENMeval::ENMevaluate(occs=manatee_thin_1, bg = bg1, envs = Env, partitions=method, algorithm="maxnet",numCores = 1,  tune.args=list(fc=fc, rm=RMvalues))
print(res)


##workaround
## glue the environmental data to the occurrence data

manatee_thin_1.z<- cbind(manatee_thin_1, terra::extract(Env, manatee_thin_1, ID = FALSE))
bg1.z<- cbind(bg1,terra::extract(Env, bg1, ID = FALSE))

## remove duplicates
manatee_thin_1.z <- unique(manatee_thin_1.z)
bg1.z <- unique(bg1.z)

## write out the data for use later
write.table(manatee_thin_1.z,"~/manatee_thin_1.z.csv",sep=",")
write.table(bg1.z,"~/bg1.z.csv",sep=",")

## if starting over, re-read in the data
manatee_thin_1.z <- read.table("~/manatee_thin_1.z.csv",sep=",",header=T)
bg1.z <- read.table("~/bg1.z.csv",sep=",",header=T)

## run model
res.swd <- ENMeval::ENMevaluate(occs=manatee_thin_1.z,bg=bg1.z,partitions=method,algorithm="maxnet",tune.args=list(fc=fc, rm=RMvalues))


##looking at results
my_results <- eval.results(res.swd)
my_results


##extracting model as raster
my_best_model <- eval.models(res.swd)$fc.LQ_rm.0.5
raster::plot(my_best_model)



##generating a map from response curves
output_raster <- enm.maxnet@predict(my_best_model,Env,list(pred.type = "cloglog",	doClamp=TRUE))
pal <- colorRampPalette(c("pink","red"))
raster::plot(output_raster,col=pal(100))
points(manatee_thin_1$Longitude,manatee_thin_1$Latitude)

##saving raster as ascii to open in GIS
writeRaster(output_raster, filename="output_raster.asc")

##repeat the whole thing using bioclim

## run model
res.swd2 <- ENMeval::ENMevaluate(occs=manatee_thin_1.z,bg=bg1.z,partitions=method,algorithm="bioclim",tune.args=list(fc=fc, rm=RMvalues))


##looking at results
my_results2 <- eval.results(res.swd2)
my_results2


##extracting model as raster
my_best_model2 <- eval.models(res.swd2)$fc.L_rm.0.5
raster::plot(my_best_model2)



##generating a map from response curves
output_raster2 <- enm.maxnet@predict(my_best_model2,Env,list(pred.type = "cloglog",	doClamp=TRUE))
pal <- colorRampPalette(c("pink","red"))
raster::plot(output_raster2,col=pal(100))
points(manatee_thin_1$Longitude,manatee_thin_1$Latitude)
 
##saving raster as ascii to open in GIS
writeRaster(output_raster2, filename="output_raster2.asc")


##downloading package from github
library(devtools)
devtools::install_github('kaiyaprovost/subsppLabelR')
library(subsppLabelR)



#importing occurence data and combining it
maca <- read_csv("e_m_maca.csv")
flav <- read_csv("e_m_flav.csv")
loc<- rbind(flav, maca)

#importing and stacking environmental data
raster_layer_path2<- "C:/Users/tanvi/OneDrive/Documents/GitHub/Bio585/CompBio/Lemur_layers/Lemur_layers"
raster_filenames2<-list.files(path = raster_layer_path2, pattern = "*.asc", recursive=FALSE, full.names=TRUE)
Env2 <- stack(raster_filenames2)


#calculating background points
colnames(loc) = c("species","Longitude","Latitude")
loc_bg = generateBackgroundForPCA(localities = loc[,2:3],e=Env2)
bg_dat = loc_bg$bgenv
bg_bg = loc_bg$bgpoints
loc_perspp_bg = generateBackgroundPerSpecies(localities = loc,e=Env2,name="species")
bgenv_flav = loc_perspp_bg$bgenv_by_subspecies$E_m_flavifrons
bgenv_maca = loc_perspp_bg$bgenv_by_subspecies$E_m_macaco
bgpts_flav = loc_perspp_bg$bgpoints_by_subspecies$E_m_flavifrons
bgpts_maca = loc_perspp_bg$bgpoints_by_subspecies$E_m_macaco


#generating PCA 
pcaOutput = createPcaToCompare(loc_bg,loc_perspp_bg,"E_macaco_ssp")

#niche overlap
pca_grid_clim = pcaOutput$grid_clim

overlap_df = pairwiseNicheOverlap(pca_grid_clim)
print(overlap_df)


pairwiseNicheEquivalence(pca_grid_clim, rep1=10, rep2=1000, species="E_macaco_ssp")

