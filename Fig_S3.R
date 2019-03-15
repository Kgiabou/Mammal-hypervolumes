setwd("C:/Users/kgiab/Desktop/Project 2 PhD/2nd Chpapter ANALYSES/Human localities again/BENS SUGGESTION HYPERVOLUME/Resampling")

polylims = function(xdata, ydata, ydata2) {
  # A function to find the beginings and endings of each run of real numbers
  # in a set of data, in order to create polygons for plotting. The assumption
  # is that ydata, ydata2, and xdata vectors are the same length, and that 
  # the ydata and ydata2 vectors contain NA values for missing data. The 
  # same values in ydata and ydata2 must be missing. The output will be 
  # a list of data frames of x and y values suitable for plotting polygons.
  
  # Use rle function to find contiguous real numbers
  rl = rle(is.na(ydata))
  starts = vector()
  ends = vector()
  indx = 1
  for (i in 1:length(rl$lengths)){
    if (rl$values[i]){
      # Value was NA, advance index without saving the numeric values
      indx = indx + rl$lengths[i]
    } else {
      # Value was a real number, extract and save those values
      starts = c(starts,indx)
      ends = c(ends, (indx + rl$lengths[i] - 1))
      indx = indx + rl$lengths[i]
    }	
  }
  
  # At this point the lengths of the vectors 'starts' and 'ends' should be
  # equal, and each pair of values represents the starting and ending indices
  # of a continuous set of data in the ydata vector.
  
  # Next separate out each set of continuous ydata, and the associated xdata,
  # and format them for plotting as a polygon.
  polylist = list()
  for (i in 1:length(starts)){
    temp = data.frame(x = c(xdata[starts[i]],xdata[starts[i]:ends[i]],
                            rev(xdata[starts[i]:ends[i]])),
                      y = c(ydata[starts[i]],ydata2[starts[i]:ends[i]],
                            rev(ydata[starts[i]:ends[i]])))
    polylist[[i]] = temp	
  }
  polylist
  # You can iterate through the items in polylist and plot them as 
  # polygons on your plot. Use code similar to the following:
  #	for (i in 1:length(polylist)){
  #			polygon(polylist[[i]]$x, polylist[[i]]$y, 
  #				col = rgb(0.5,0.5,0.5,0.5), border = NA)
  #	}
}


setwd("C:/Users/kgiab/Desktop/Project 2 PhD/2nd Chpapter ANALYSES/Human localities again/BENS SUGGESTION HYPERVOLUME/Resampling")
library(fBasics)
cols <- "grey90"
sams <- c(6,10,15,20)
nam <- as.vector(NULL)
for(ss in seq_along(dir(pattern = "6_occurrences_volumes.txt")[-c(7,11,14)]))
{
  n <- strsplit(dir(pattern = "6_occurrences_volumes.txt")[-c(7,11,14)][ss], split="_6")[[1]][1]
  nam <- c(nam, n)
}

species_list <- nam 
par(mfrow=c(3,5), mar=c(4,4,4,4))
for (sp in seq_along(species_list))
{
  if(sp == 8)
  {
    bins <- c(seq(8,21,1), seq(22,48,2))
  }
  else
  {
    bins <-c(seq(13,21,1), seq(22,48,2))
  }
  setwd("C:/Users/kgiab/Desktop/Project 2 PhD/2nd Chpapter ANALYSES/Human localities again/BENS SUGGESTION HYPERVOLUME")
  origin <- read.delim(paste(species_list[sp], "_volumes.txt", sep=""), h=T, sep="\t")
  timeX <- as.vector(NULL) 
  for(tt in 1:nrow(origin))
  {
    timeX <- c(timeX, as.numeric(strsplit(strsplit(rownames(origin), split="_")[[tt]][3], split="k")))
  }
  origin2 <- cbind.data.frame(Time=timeX, origin)
  d.cor <- data.frame(matrix(NA, nrow=4, ncol=1))
  colnames(d.cor) <- "Correlations"
  rownames(d.cor) <- paste(species_list[sp], "_", sams, sep="")
  setwd("C:/Users/kgiab/Desktop/Project 2 PhD/2nd Chpapter ANALYSES/Human localities again/BENS SUGGESTION HYPERVOLUME/Resampling")
  for(j in seq_along(sams))
  {
    spec_vol <- read.delim(paste(species_list[sp],"_", sams[j],"_occurrences_volumes.txt", sep=""), h=T, sep="\t")
    timeX2 <- as.vector(NULL)
    for(cc in 1:ncol(spec_vol))
    {
      timeX2 <- c(timeX2, as.numeric(strsplit(strsplit(colnames(spec_vol), split="_")[[cc]][3], split="k")))
    }
    colnames(spec_vol) <- timeX2
    spec_med <- data.frame(Time=as.integer(timeX2))
    for(i in 1:ncol(spec_vol))
    {
      mm <- median(spec_vol[,i])
      spec_med$Med_volume[i] <- mm
    }
    meds <- merge(spec_med, origin2, by.x="Time", by.y="Time")
    if (nrow(meds) > 1)
    {
      cors<- round(cor(meds$Volume, meds$Med_volume),3)
      d.cor[j,1] <- cors
    }
    spec_vol2 <- data.frame(matrix(NA, nrow=100, ncol=length(bins)))
    colnames(spec_vol2) <-as.numeric(bins)
    spec_met <- data.frame(Time=bins)
    spec_met2 <- merge(spec_met, origin2, by.x="Time", by.y="Time", all.x=TRUE)
    for(co in seq_along(colnames(spec_vol)))
    {
      for (bi in seq_along(bins))
      {
        if(bins[bi] == as.numeric(colnames(spec_vol)[co]))
        {
          spec_vol2[,bi] <- spec_vol[,co]
          spec_met[bi,2] <- origin2[co,2]
          qu <- quantile(spec_vol[,co], na.rm=TRUE, names=TRUE)
          spec_met[bi,3] <- qu[1]
          spec_met[bi,4] <- qu[3]
          spec_met[bi,5] <- qu[5]
        }
      }
    }
    colnames(spec_met) <- c("Time", "Volume", "lower", "med", "upper")
    assign(paste0("spec_met_", sams[j], sep=""), spec_met)
    assign(paste("sk_", sams[j], sep=""), sams[j])
    write.table(spec_met, file=paste0("spec_met_", sams[j], ".txt", sep=""), sep="\t", quote=FALSE, col.names = TRUE, row.names = FALSE)
  }
  write.table(d.cor, file=paste0(species_list[sp], "_hypervolume_correlations.txt", sep=""), col.names = TRUE, row.names = TRUE, quote=FALSE, sep="\t")
  min_vol <- subset(spec_met2, Volume==min(na.omit(spec_met2$Volume)))
  max_vol <- subset(spec_met2, Volume==max(na.omit(spec_met2$Volume)))
  row_min <- as.numeric(rownames(subset(spec_met2, Volume==min(na.omit(spec_met2$Volume)))))
  row_max <- as.numeric(rownames(subset(spec_met2, Volume==max(na.omit(spec_met2$Volume)))))
  if (sp==8)
  {
    plot(spec_met_6$Time, spec_met_6$Volume, type="n", xlim=range(bins), ylim = c(-10,310),xlab = NA, xaxs="i", yaxs="i", ylab = NA, las = 1, axes=F, main=species_list[sp], cex.main = 0.9)
    axis(2, at=seq(-10,310,20), col="black", col.ticks="black", labels=c("", seq(0,300,20)), cex.axis = 1)
	axis(1, at=bins, labels=bins, col="black", col.ticks = "black", cex.axis = 1)
  polys_6 <- polylims(spec_met_6$Time, spec_met_6$upper, spec_met_6$lower)
  for(i in 1:length(polys_6)){
    polygon(polys_6[[i]]$x, polys_6[[i]]$y, col = "grey90", border = NA)
  }
  polys_10 <- polylims(spec_met_10$Time, spec_met_10$upper, spec_met_10$lower)
  for(i in 1:length(polys_10)){
  polygon(polys_10[[i]]$x, polys_10[[i]]$y, col = "grey75", border = NA)
  }
  polys_15<- polylims(spec_met_15$Time, spec_met_15$upper, spec_met_15$lower)
  for(i in 1:length(polys_15)){
  polygon(polys_15[[i]]$x, polys_15[[i]]$y, col = "grey65", border = NA)
  }
  polys_20<- polylims(spec_met_20$Time, spec_met_20$upper, spec_met_20$lower)
  for(i in 1:length(polys_20)){
  polygon(polys_20[[i]]$x, polys_20[[i]]$y, col = "grey50", border = NA)
  }
  par(new=T)
  plot(spec_met_6$Time, spec_met_6$Volume, type="b", pch=19, col="black", ylim=c(-10,310), xlab = NA, xaxs="i", yaxs="i", ylab = NA, las = 1, axes=F) 
  }
  else if (sp==9)
  {
    plot(spec_met_6$Time, spec_met_6$Volume, type="n", xlim=range(bins), ylim = c(-10,210),xlab = NA, xaxs="i", yaxs="i", ylab = NA, las = 1, axes=F, main=species_list[sp], cex.main = 0.9)
    axis(2, at=seq(-10,210,20), col="black", col.ticks="black", labels=c("", seq(0,200,20)), cex.axis = 1)
	axis(1, at=bins, labels=bins, col="black", col.ticks = "black", cex.axis = 1)
  polys_6 <- polylims(spec_met_6$Time, spec_met_6$upper, spec_met_6$lower)
  for(i in 1:length(polys_6)){
    polygon(polys_6[[i]]$x, polys_6[[i]]$y, col = "grey90", border = NA)
  }
  polys_10 <- polylims(spec_met_10$Time, spec_met_10$upper, spec_met_10$lower)
  for(i in 1:length(polys_10)){
  polygon(polys_10[[i]]$x, polys_10[[i]]$y, col = "grey75", border = NA)
  }
  polys_15<- polylims(spec_met_15$Time, spec_met_15$upper, spec_met_15$lower)
  for(i in 1:length(polys_15)){
  polygon(polys_15[[i]]$x, polys_15[[i]]$y, col = "grey65", border = NA)
  }
  polys_20<- polylims(spec_met_20$Time, spec_met_20$upper, spec_met_20$lower)
  for(i in 1:length(polys_20)){
  polygon(polys_20[[i]]$x, polys_20[[i]]$y, col = "grey50", border = NA)
  }
  par(new=T)
  plot(spec_met_6$Time, spec_met_6$Volume, type="b", pch=19, col="black", ylim=c(-10,210), xlab = NA, xaxs="i", yaxs="i", ylab = NA, las = 1, axes=F) 
  }
  else
  {
    plot(spec_met_6$Time, spec_met_6$Volume, type="n", xlim=range(bins), ylim = c(-10,140),xlab = NA, xaxs="i", yaxs="i", ylab = NA, las = 1, axes=F, main=species_list[sp], cex.main = 0.9)
    axis(2, at=seq(-10,140,10), col="black", col.ticks="black", labels=c("", seq(0,140, 10)), cex.axis = 1)
	axis(1, at=bins, labels=bins, col="black", col.ticks = "black", cex.axis = 1)
  polys_6 <- polylims(spec_met_6$Time, spec_met_6$upper, spec_met_6$lower)
  for(i in 1:length(polys_6)){
    polygon(polys_6[[i]]$x, polys_6[[i]]$y, col = "grey90", border = NA)
  }
  polys_10 <- polylims(spec_met_10$Time, spec_met_10$upper, spec_met_10$lower)
  for(i in 1:length(polys_10)){
  polygon(polys_10[[i]]$x, polys_10[[i]]$y, col = "grey75", border = NA)
  }
  polys_15<- polylims(spec_met_15$Time, spec_met_15$upper, spec_met_15$lower)
  for(i in 1:length(polys_15)){
  polygon(polys_15[[i]]$x, polys_15[[i]]$y, col = "grey65", border = NA)
  }
  polys_20<- polylims(spec_met_20$Time, spec_met_20$upper, spec_met_20$lower)
  for(i in 1:length(polys_20)){
  polygon(polys_20[[i]]$x, polys_20[[i]]$y, col = "grey50", border = NA)
  }
  par(new=T)
  plot(spec_met_6$Time, spec_met_6$Volume, type="b", pch=19, col="black", ylim=c(-10,140), xlab = NA, xaxs="i", yaxs="i", ylab = NA, las = 1, axes=F) 
  }	
  rm(list=ls(pattern = "spec_met_"))
  rm(list=ls(pattern = "sk_"))
  file.remove(dir(pattern = "spec_met_"))
  if(sp == 12)	
  {
    mtext(side = 1, line = 2.3, cex=0.9, "Time(ka)")
  }
  if(sp == 6)
  {
    mtext(side = 2, line = 2.3, cex=0.9, "Niche Hypervolume(Std³)")
  }
}
