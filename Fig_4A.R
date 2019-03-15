setwd("C:/Users/kgiab/Desktop/Project 2 PhD/2nd Chpapter ANALYSES/Human localities again/BENS SUGGESTION HYPERVOLUME")
require(vioplot)
require(lattice)

vol_bins <- data.frame(Time=c(seq(8,21,1), seq(22,46,2)))
occ <- as.vector(NULL)
nam <- as.vector(NULL)
ints <- as.vector(NULL)
for(ss in seq_along(dir(pattern = "_volumes.txt")[-c(7,11,12,15)]))
{
n <- strsplit(dir(pattern = "_volumes.txt")[-c(7,11,12,15)][ss], split="_volumes.txt")[[1]][1]
nam <- c(nam, n)
}
species_list <- nam
name <- as.vector(NULL)
for(vv in seq_along(species_list))
{
Vol <- read.delim(paste(species_list[vv], "_volumes.txt", sep=""), h=T, sep="\t")
Ocs <- read.delim(paste(species_list[vv],"_Occurrences.txt", sep=""), h=T, sep="\t")
timeX2 <- vector()
for(tt in seq_along(rownames(Vol)))
{
timeX2 <- c(timeX2, as.numeric(strsplit(strsplit(rownames(Vol), split="_")[[tt]][3], split="k")))
}
for(j in seq_along(timeX2))
{
for(rr in seq_along(vol_bins$Time))
{
if(timeX2[j] == vol_bins$Time[rr])
{
vol_bins[rr,c(1+vv)] <- Vol[j,1]
}
}
}
name<- c(name, species_list[vv])
colnames(vol_bins) <- c("Time",name)
occ <- c(occ, sum(Ocs$Occurrences))
}
colos <- c("indianred4", "palegreen4", "indianred4", "palegreen4", "palegreen4", "indianred4", "palegreen4", "lightgoldenrod1", "palegreen4", "indianred4", "palegreen4", "lightgoldenrod1", "lightgoldenrod1")

bins2 <- vol_bins[,2:14]
require(plotrix)

vec_med <- vector()
vec_max <- vector()
for (l in 1:ncol(bins2))
{
upp <- max(na.exclude(bins2[,l]))
vec_max <- c(vec_max, upp)
med <- median(na.exclude(bins2[,l]))
vec_med <- c(vec_med, med)
}
vec_med2 <- order(vec_med, decreasing=FALSE) 
vec_max2 <- order(vec_max, decreasing=FALSE)
vec_med3 <- vec_med[vec_med2] ### rearrange the median values vector based on the order ranking for plotting the lines of median values
vec_max3 <- vec_max[vec_max2] ### simlarly for maximum values
bins_med <- bins2[,c(vec_med2)] ### rearrange the columns for all species
bins_max <- bins2[,c(vec_max2)]
cols_med <- colos[c(vec_med2)]
nam_med <- nam[vec_med2]
nam_max <- nam[vec_max2]
posit <- c(1:13)
occ_med2 <- occ[vec_med2] ## rearrange the occurrence values to match the order of volumes
occ_med3 <- cbind.data.frame(posit, occ_med2)
occ_med4 <- as.matrix(occ_med3) 
par(mfrow=c(3,1), mar=c(4,4,4,4), oma=c(2,2,2,2))
plot(1, 1, xlim = c(0,14), ylim = c(0,40), type = 'n', xlab=NA, ylab = NA, xaxt = 'n', yaxt='n')
for (co in 1:ncol(bins_med))
{
vioplot(na.exclude(bins_med[,co]),range=1.5, at=co, add=T, names= colnames(bins_med)[co], col=cols_med[co])
}
axis(2, at=seq(0,40, by=5), labels=TRUE, cex.axis=0.95)
axis(1, at=seq(1,13, by=1), las=2, srt=45, labels = c(nam_med), cex.axis=0.75)
par(new=T)
plot(occ_med4, xlim=c(0,14), axes=F, col="sienna3", pch=19, cex=1.3, ylim=c(0,6000), yaxs="i", xlab=NA, ylab=NA)
axis(4, at=c(seq(0,1000,200), seq(5000,6000,500)), labels=TRUE, cex.axis=0.95, col="black")
axis.break(axis=4,breakpos=1500,bgcol="white",breakcol="black",
style="slash",brw=0.02)
mtext(side = 2, line = 3, cex=1, "Niche Volume", col="black")


over_bins <- data.frame(Time=c(seq(8,21,1), seq(22,46,2)))
occ <- as.vector(NULL)
nam <- as.vector(NULL)
ints <- as.vector(NULL)
for(ss in seq_along(dir(pattern = "_humans_overlap.txt")[-c(7,10,13)]))
{
n <- strsplit(dir(pattern = "_humans_overlap.txt")[-c(7,10,13)][ss], split="_humans_overlap.txt")[[1]][1]
nam <- c(nam, n)
}
species_list <- nam
name <- as.vector(NULL)
for(ov in seq_along(species_list))
{
Over <- read.delim(paste(species_list[ov], "_humans_overlap.txt", sep=""), h=T, sep="\t")
Ocs <- read.delim(paste(species_list[ov],"_Occurrences.txt", sep=""), h=T, sep="\t")
timeX2 <- vector()
for(tt in seq_along(rownames(Over)))
{
timeX2 <- c(timeX2, as.numeric(strsplit(strsplit(rownames(Over), split="_")[[tt]][3], split="k")))
}
for(j in seq_along(timeX2))
{
for(rr in seq_along(over_bins$Time))
{
if(timeX2[j] == over_bins$Time[rr])
{
over_bins[rr,c(1+ov)] <- Over[j,2]
}
}
}
name<- c(name, species_list[ov])
colnames(over_bins) <- c("Time",name)
occ <- c(occ, sum(Ocs$Occurrences))
ints <- c(ints, nrow(Over))
}

over_colos <- c("indianred4", "palegreen4", "indianred4", "palegreen4", "palegreen4", "indianred4", "palegreen4", "palegreen4", "indianred4", "palegreen4", "lightgoldenrod1", "lightgoldenrod1")
over_bins2 <- over_bins[,2:13]
vec_med <- vector()
vec_max <- vector()
for (l in 1:ncol(over_bins2))
{
upp <- max(na.exclude(over_bins2[,l]))
vec_max <- c(vec_max, upp)
med <- median(na.exclude(over_bins2[,l]))
vec_med <- c(vec_med, med)
}
vec_med2 <- order(vec_med, decreasing=FALSE) 
vec_max2 <- order(vec_max, decreasing=FALSE)
vec_med3 <- vec_med[vec_med2] 
vec_max3 <- vec_max[vec_max2]
over_bins_med <- over_bins2[,c(vec_med2)] 
over_bins_max <- over_bins2[,c(vec_max2)]
nam_med <- nam[vec_med2]
nam_max <- nam[vec_max2]
over_colos_med <- over_colos[c(vec_med2)]
posit <- c(1:12)
occ_med2 <- occ[vec_med2] ## rearrange the occurrence values to match the order of volumes
occ_med3 <- cbind.data.frame(posit, occ_med2)
occ_med4 <- as.matrix(occ_med3) ### tansform to matrix for plotting
plot(1, 1, xlim = c(0,13), ylim = c(0,1),xlab=NA, ylab=NA, type = 'n', xaxt = 'n', yaxt='n')
for (co in 1:ncol(over_bins_med))
{
vioplot(na.exclude(over_bins_med[,co]),range=1.5, at=co, add=T, names= colnames(over_bins_med)[co], col=over_colos_med[co])
}
axis(2, at=seq(0,1, by=0.1), labels=TRUE, cex.axis=0.95)
axis(1, at=seq(1,12, by=1), las=2, srt=45, labels = c(nam_med), cex.axis=0.75)
par(new=T)
plot(occ_med4, xlim=c(0,13), axes=F, col="sienna3", pch=19, cex=1.3, ylim=c(0,1600), yaxs="i", xlab=NA, ylab=NA)
axis(4, at=c(seq(0,1600,200)), labels=TRUE, cex.axis=0.95, col="black")
mtext(side = 4, line = 3, cex=1, "No of Occurrences", col="sienna3")
mtext(side = 2, line = 3, cex=1, "Hypervolume Overlap", col="black")

setwd("C:/Users/kgiab/Desktop/Project 2 PhD/2nd Chpapter ANALYSES/Human localities again/Geographic Overlap per time inerval/Geographic oVerlap 3 Variables")
geog_bins <- data.frame(Time=c(seq(8,21,1), seq(22,46,2)))
occ <- as.vector(NULL)
nam <- as.vector(NULL)
ints <- as.vector(NULL)
for(ss in seq_along(dir(pattern = "_humans.txt")[-c(7,10,13)]))
{
n <- strsplit(strsplit(dir(pattern = "_humans.txt")[-c(7,10,13)][ss], split="_humans.txt")[[1]][1], split = "Geographic_overlap_")[[1]][2]
nam <- c(nam, n)
}
species_list <- nam
name <- as.vector(NULL)
for(gg in seq_along(species_list))
{
Geog <- read.delim(paste("Geographic_overlap_", species_list[gg], "_humans.txt", sep=""), h=T, sep="\t")
Ocs <- read.delim(paste(species_list[gg],"_Occurrences.txt", sep=""), h=T, sep="\t")
timeX2 <- as.numeric(Geog$Time)
for(j in seq_along(timeX2))
{
for(rr in seq_along(geog_bins$Time))
{
if(timeX2[j] == geog_bins$Time[rr])
{
geog_bins[rr,c(1+gg)] <- Geog[j,2]
}
}
}
name<- c(name, species_list[gg])
colnames(geog_bins) <- c("Time",name)
occ <- c(occ, sum(Ocs$Occurrences))
ints <- c(ints, nrow(Geog))
}
geog_colos <- c("indianred4", "palegreen4", "indianred4", "palegreen4", "palegreen4", "indianred4", "palegreen4", "palegreen4", "indianred4", "palegreen4", "lightgoldenrod1", "lightgoldenrod1")

geog_bins2 <- geog_bins[,2:13]
vec_med <- vector()
vec_max <- vector()
for (l in 1:ncol(geog_bins2))
{
upp <- max(na.exclude(geog_bins2[,l]))
vec_max <- c(vec_max, upp)
med <- median(na.exclude(geog_bins2[,l]))
vec_med <- c(vec_med, med)
}
vec_med2 <- order(vec_med, decreasing=FALSE) 
vec_max2 <- order(vec_max, decreasing=FALSE)
vec_med3 <- vec_med[vec_med2] 
vec_max3 <- vec_max[vec_max2]
geog_bins_med <- geog_bins2[,c(vec_med2)]
geog_bins_max <- geog_bins2[,c(vec_max2)]
nam_med <- nam[vec_med2]
nam_max <- nam[vec_max2]
geog_colos_med <- geog_colos[c(vec_med2)]
posit <- c(1:12)
occ_med2 <- occ[vec_med2] ## rearrange the occurrence values to match the order of volumes
occ_med3 <- cbind.data.frame(posit, occ_med2)
occ_med4 <- as.matrix(occ_med3) ### tansform to matrix for plotting
plot(1, 1, xlim = c(0,13), ylim = c(0,1), xlab=NA, ylab=NA, type = 'n', xaxt = 'n', yaxt='n')
for (co in 1:ncol(geog_bins_med))
{
vioplot(na.exclude(geog_bins_med[,co]),range=1.5, at=co, add=T, names= colnames(geog_bins_med)[co], col=geog_colos_med[co])
}
axis(2, at=seq(0,1, by=0.1), labels=TRUE, cex.axis=0.95)
axis(1, at=seq(1,12, by=1), las=2, srt=45, labels = c(nam_med), cex.axis=0.75)
par(new=T)
plot(occ_med4, xlim=c(0,13), axes=F, col="sienna3", pch=19, cex=1.3, ylim=c(0,1600), yaxs="i", xlab=NA, ylab=NA)
axis(4, at=c(seq(0,1600,200)), labels=TRUE, cex.axis=0.95, col="black")
