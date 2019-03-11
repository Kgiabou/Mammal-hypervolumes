library(raster)
library(corrplot)

bins <-c(seq(10,22,1), seq(24,48,2))

## Climate raster stacks per time interval ###
for(b in seq_along(bins))
{
  layers <- grep(bins[b], dir(pattern = ".asc"))
  clim_st <- stack()
  for(tim in seq_along(layers))
  {
    ras <- raster(dir(pattern = ".asc")[layers[tim]])
    clim_st <- stack(clim_st, ras)
  }
  for (i in 1:nlayers(clim_st))
  {
  names(clim_st)[[i]] <- paste(strsplit(names(clim_st)[[i]], split="_")[[1]][2], "_", 
                          strsplit(names(clim_st)[[i]], split="_")[[1]][3], sep = "")
  }
  assign(paste("climatelayers_", bins[b], "k", sep = ""), clim_st)
}

### Assess the correlation between climate variables per time interval ###
par(mfrow=c(3,5), mar=c(2,2,2,2))
for(cli in seq_along(ls(pattern = "climatelayers")))
{
  cl<- get(ls(pattern = "climatelayers")[cli])
  val<- getValues(cl)
  val2 <- na.omit(val)
  cor_clim <- cor(val2, use="complete.obs")
  diag(cc) <-NA
  cc[upper.tri(cc)] <-NA
  ### Make correlation plot figure in pdf file ###
  pdf(paste("Correlation plot_", ls(pattern = "climatelayers")[cli],".pdf", sep=""))
  corrplot(cor_clim, type="lower", mar=c(0,0,1,0), method="number", order="original", tl.col="black", outline=FALSE, is.corr=TRUE, title = ls(pattern = "climatelayers")[cli])
  dev.off()
}

#### Z score (scaled) climatic variables based on the 3 selected low correlation variables ####

dir.create("Z-score seasonal maps for Analyses")
for(cli in seq_along(ls(pattern = "climatelayers"))[-26])
{
  setwd("C:/Users/Ksq450/Desktop/Project 2 PhD/2nd Chpapter ANALYSES/Seasonal maps_Eurasia")
  cl<- get(ls(pattern = "climatelayers")[cli])
  cl2 <- subset(cl, subset=c(3,4,7))
  for (i in 1:nlayers(cl2)) 
  {  
    cl2[[i]] <- (cl2[[i]] - cellStats(cl2[[i]], 'mean')) / cellStats(cl2[[i]], 'sd')
    names(cl2)[[i]] <- paste("Z_", strsplit(ls(pattern = "climatelayers")[cli], split="_")[[1]][2], "_", names(cl2)[[i]], sep="")
  }
  assign(paste("Z_score_variables_", strsplit(ls(pattern = "climatelayers")[cli], split="_")[[1]][2], sep=""), cl2)
  setwd("C:/Users/Ksq450/Desktop/Project 2 PhD/2nd Chpapter ANALYSES/Seasonal maps_Eurasia/Z-score seasonal maps for Analyses")
  writeRaster(cl2, filename = names(cl2), format="ascii", bylayer=TRUE)
}

dir.create("Z-score seasonal maps for Analyses")
for(cli in seq_along(ls(pattern = "climatelayers"))[-26])
{
  setwd("C:/Users/Ksq450/Desktop/Project 2 PhD/2nd Chpapter ANALYSES/Seasonal maps_Eurasia")
  cl<- get(ls(pattern = "climatelayers")[cli])
  cl2 <- subset(cl, subset=c(3,4,7))
  for (i in 1:nlayers(cl2)) 
  {  
    cl2[[i]] <- (cl2[[i]] - cellStats(cl2[[i]], 'mean')) / cellStats(cl2[[i]], 'sd')
    names(cl2)[[i]] <- paste("Z_", strsplit(ls(pattern = "climatelayers")[cli], split="_")[[1]][2], "_", names(cl2)[[i]], sep="")
  }
  assign(paste("Z_score_variables_", strsplit(ls(pattern = "climatelayers")[cli], split="_")[[1]][2], sep=""), cl2)
  setwd("C:/Users/Ksq450/Desktop/Project 2 PhD/2nd Chpapter ANALYSES/Seasonal maps_Eurasia/Z-score seasonal maps for Analyses")
  writeRaster(cl2, filename = names(cl2), format="ascii", bylayer=TRUE)
}
