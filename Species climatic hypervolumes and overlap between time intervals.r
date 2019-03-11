library(hypervolume)
library(raster)
library(maps)

DB <- read.delim("Species_environmental_variables_2.txt", h=T, sep="\t")
species_list <- unique(DB$Species) ## get species names - in our case can be time bins as well
num_species <- length(species_list)
n_var <- 3  ### Specify how many variables we are going to use


for (s in seq_along(species_list))
{
  DB_species <- subset(DB, Species==species_list[s]) ### Subset the DATABASE per species
  Species_time_bins <- sort(unique(DB_species$Interval)) ### Sort the time intervals for the selected species
  vec <- vector()
  for (i in seq_along(Species_time_bins))
  { 
    Occurrences_per_bin <- subset(DB_species, Interval==Species_time_bins[i])	 ## create a subset data.frame for each time bin ###
    vec <- c(vec, nrow(Occurrences_per_bin)) ### Construct a vector with the number of Occurrences for each time interval
    list_len <- length(vec[log(vec) >= n_var])
    hv_bins <- Species_time_bins[log(vec) >= n_var] ### Select the time intervals with enough occurrences to run the analysis
  }
  if (list_len > 0)
  {
    N_Occur <- matrix(NA, nrow=list_len, ncol=2)
    colnames(N_Occur) <- paste(c("Time", "Occurrences"))
    N_Occur[,1] <- hv_bins
    N_Occur[,2] <- vec[log(vec) >= n_var]	   
    assign(paste(species_list[s], "_Occurrences", sep=""), N_Occur)
    write.table(N_Occur, file=paste(species_list[s], "_Occurrences.txt", sep=""), col.names=TRUE, quote=FALSE, row.names = FALSE, sep="\t" )
    hv_species_list=new("HypervolumeList") ##construct an empty obeject of class list to put hypervolumes
    hv_species_list@HVList = vector(mode="list", length=list_len)
    convex_species_list <- new("HypervolumeList")
    convex_species_list@HVList = vector(mode="list", length=list_len)
    Aver_position <- matrix(NA, nrow=list_len, ncol=n_var)
    dimnames(Aver_position) <- list(paste(species_list[s], "_", hv_bins, "k", sep=""), c("Prec_Su", "Prec_Wi", "Temp_Su"))
    Volume <- matrix(NA, nrow=list_len, ncol=1)
    dimnames(Volume) <- list(paste(species_list[s], "_", hv_bins, "k", sep=""), paste("Volume"))
    for (i in seq_along(hv_bins))
    {
      time_bin <- subset(DB_species, Interval==hv_bins[i])
      time_bin2 <- time_bin[, c("Prec_Su", "Prec_Wi", "Temp_Su")] ## Select the environmental variables to be used for the hypervolume
      band <- estimate_bandwidth(time_bin2, method="silverman") 
      hv_species_list@HVList[[i]] <- hypervolume(time_bin2, repsperpoint = 10000, bandwidth= ifelse(band > 0, band, 0.1), quantile=0.1, name=paste(species_list[s],"_", hv_bins[i], "k", sep=""), verbose=TRUE, warnings=TRUE)
      convex_species_list@HVList[[i]] <- expectation_convex(hv_species_list@HVList[[i]], npoints_inhull = 100000, npoints_onhull = 1000, userandom = TRUE, check_memory=FALSE)
      ### Compute species average positions ####
      assign(paste(species_list[s], "hypervolume", sep="_"), hv_species_list)
      assign(paste(species_list[s], "expectation_convex", sep="_"), convex_species_list)
      Centroid <- colMeans(hv_species_list@HVList[[i]]@RandomUniformPointsThresholded) ### getthe average poisiton of the species
      dimnames(Aver_position) <- list(paste(species_list[s], "_", hv_bins, "k", sep=""), colnames(time_bin2))		 
      Aver_position[i,] <- Centroid
      write.table(Aver_position, file=paste(species_list[s], "_centroids.txt", sep=""), col.names=TRUE, quote=FALSE, row.names = TRUE, sep="\t")
      Volume[i,] <- hv_species_list@HVList[[i]]@Volume
      write.table(Volume, file=paste(species_list[s], "_volumes.txt", sep=""), col.names=TRUE, quote=FALSE, row.names = TRUE, sep="\t")  
      # compute all pairwise overlaps ##
      overlap <- matrix(NA, nrow=list_len, ncol=list_len)
      dimnames(overlap) <- list(paste(species_list[s], "_", hv_bins, "k", sep=""), paste(species_list[s], "_", hv_bins, "k", sep=""))
      distances <- matrix(NA, nrow=list_len, ncol=list_len)
      dimnames(distances) <- list(paste(species_list[s], "_", hv_bins, "k", sep=""), paste(species_list[s], "_", hv_bins, "k", sep=""))
    }
    for (i in seq_along(hv_bins))
    {
      for (j in seq_along(hv_bins))
      {
        if (i!=j)
        {
          # compute set operations on each pair
          this_set <- hypervolume_set(hv_species_list@HVList[[i]], hv_species_list@HVList[[j]], npoints_max=NULL, verbose = TRUE, check_memory = FALSE, distance_factor = 1) ##why the reudction factor doesnt work as an argument??
          # calculate a Sorensen overlap index (2 x shared volume / sum of |hv1| + |hv2|)
          overlap[i,j] <- hypervolume_sorensen_overlap(this_set)
          distances[i,j] <- hypervolume_distance(hv_species_list@HVList[[i]], hv_species_list@HVList[[j]], type = "centroid", check_memory = TRUE)
        }
      }
    }
    assign(paste(species_list[s], "overlaps", sep="_"), overlap)
    write.table(overlap, file=paste(species_list[s], "overlaps.txt", sep="_"), col.names = TRUE, quote=FALSE, row.names = TRUE, sep="\t")
    assign(paste(species_list[s], "distances", sep="_"), distances)
    write.table(distances, file=paste(species_list[s], "distances.txt", sep="_"), col.names = TRUE, quote=FALSE, row.names = TRUE, sep="\t")
  }
}
