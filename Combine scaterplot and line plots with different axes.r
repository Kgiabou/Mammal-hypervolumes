library(fBasics)

plot.new()
br=seq(10,50,5)
par(mfrow=c(4,4), oma=c(2,2,2,2), mar=(c(2,2,2,2)))
colX <- seqPalette(length(unique(dir(pattern = "_volume"))) + 6, "Greys")
colX2 <- colX[c(8:(length(unique(dir(pattern = "_volume"))) + 6))]
colY <- seqPalette(length(unique(ls(pattern = "_Occurrences"))) + 6, "Oranges")
colY2 <- paste(colY[c(8:(length(unique(ls(pattern = "_Occurrences"))) + 6))], "70", sep="")
count <- 1
for (fil2 in seq_along(dir(pattern = "_volume")))
{
  temp <- as.matrix(read.delim(dir(pattern = "_volume")[fil2], h=T, sep="\t", stringsAsFactors=FALSE))
  occur <- get(ls(pattern = "_Occurrences")[fil2])
  timeX <- vector()
  for(nam in seq_along(rownames(temp)))
  {
    timeX <- c(timeX, as.numeric(strsplit(strsplit(rownames(temp), split="_")[[nam]][3], split="k")))
  }
  plot(occur, type="l", xlim=c(10, 50), ylim=c(0,200), cex.axis=0.7, col=colY2[11], lwd=1.5, yaxs="i", axes=F, ylab=NA, xlab=NA)
  axis(side=4, col=colY2[11], at=seq(0,200,50), labels=NA, lwd=1, col.ticks=colY2[11], cex.axis=0.7) 
  if(fil2==4 || fil2==8 || fil2==12 || fil2==16)
  {
    axis(side=4, col=colY2[11], lwd=1, col.ticks=colY2[11], cex.axis=0.7)
  }
  par(new=TRUE)
  plot(x=timeX, y=temp, type="b", col=colX2[10], ylim=c(0,80), xaxs="i", yaxs="i", axes=F, bty="l", yaxt="n", lwd=1.3, pch=16, xlim=c(10, 50), xlab=NA, ylab=NA, main=strsplit(dir(pattern = "_volumes")[fil2], split="_volumes.txt")[[1]][1], cex.main=0.7)
  axis(side=1, col="black", at=seq(10,50,5), lwd=1, col.ticks="black", cex.axis=0.7)
  axis(side=2, col=colX2[10], at=seq(0,80,10), labels=NA, lwd=1, col.ticks=colX2[10], cex.axis=0.7)
  if(fil2==1 || fil2==5 || fil2==9 || fil2==13)
  {
    axis(side=2, col=colX2[10], at=seq(0,80,10), lwd=1, col.ticks=colX2[10], cex.axis=0.7)
  } 
  if(fil2==15)
  {
    mtext(side = 1, line = 2.4, cex=0.8, "Time(ka)")
  }
  if(fil2==5)
  {
    mtext(side = 2, line = 2.4, cex=0.8, "Niche Hypervolume Size")
  } 
  if(fil2==8)
  {
    mtext(side = 4, line = 2.4, cex=0.8, "No Occurrences")
  } 
  count <- count + 1
}
