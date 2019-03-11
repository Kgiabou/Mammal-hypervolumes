library(rgdal)

dat<-read.delim("my_DB2.txt", h=T, sep="\t", stringsAsFactors=FALSE)
species_list <- unique(dat$Species)
### define the number of env variables
n_var <- 3
Extracted_data <-as.data.frame(matrix(NA, ncol=11 + n_var)) ## Create an empty dataframe to put the results ##
colnames(Extracted_data) <-c(colnames(dat) , "Interval", "Cells", "Prec_Su", "Prec_Wi","Temp_Su")
bins <-c(seq(10,22,1), seq(24,48,2)) 
for (s in seq_along(species_list))
{
  ### Subset the database per Species ##
  DB_species <- subset(dat, Species==species_list[s])
  #### Construct an empty data frame with no of columns equal to the no of columns of the database plus the no of variables we want to extract ## 
  spec_points <- as.data.frame(matrix(nrow=nrow(DB_species), ncol=ncol(DB_species)+ n_var + 2))
  colnames(spec_points) <- c(colnames(DB_species) , "Interval", "Cells", "Prec_Su", "Prec_Wi", "Temp_Su") ###The name sof the env variables
  counter1 <- 1	
  for (i in seq_along(bins)[-26])
  {
    #### Subset each Species data per each time bin and take the coordinates to extract the variables for each occurrence ###
    records_per_bin <- data.frame(DB_species[which(DB_species$Mean_Age > as.numeric(paste(bins[i], "000", sep="")) & DB_species$Mean_Age <= as.numeric(paste(bins[i+1], "000", sep=""))),])
    extr_points <- records_per_bin[, c("Longitude", "Latitude")]
    if(nrow(records_per_bin) > 0)
    { 
      spec_points[(counter1:(counter1 + nrow(records_per_bin)-1)), 1] <- paste(species_list[s])
      spec_points[(counter1:(counter1 + nrow(records_per_bin)-1)), 2:9] <- records_per_bin[,2:9]
      spec_points[(counter1:(counter1 + nrow(records_per_bin)-1)), which(colnames(spec_points)=="Interval")] <- bins[i+1]
      spec_points[(counter1:(counter1 + nrow(records_per_bin)-1)), c(which(colnames(spec_points)=="Cells"), which(colnames(spec_points)=="Prec_Su"))] <- extract(Clim_prec_su, layer=grep(bins[i+1], names(Clim_prec_su)), extr_points, nl=1, method="simple", na.rm=TRUE, cellnumbers=TRUE)
      spec_points[(counter1:(counter1 + nrow(records_per_bin)-1)), which(colnames(spec_points)=="Prec_Wi")] <- extract(Clim_prec_wi, layer=grep(bins[i+1], names(Clim_prec_wi)), nl=1, extr_points, method="simple", na.rm=TRUE)
      spec_points[(counter1:(counter1 + nrow(records_per_bin)-1)), which(colnames(spec_points)=="Temp_Su")] <- extract(Clim_temp_su, layer=grep(bins[i+1], names(Clim_temp_su)), nl=1, extr_points, method="simple", na.rm=TRUE)
      counter1 <-counter1 + nrow(records_per_bin)
    }
  }
  assign(paste(species_list[s], "variables", sep="_"), spec_points)
  Extracted_data <- rbind(Extracted_data, get(paste(species_list[s], "variables", sep="_")))
}	

### Delete data with NA in Longitude, Latitude or the Climatic Variables ##
full_vector <- as.vector(NULL)
for (i in seq_along(Extracted_data$Latitude)){     ### find NAs in either Long or Lat or both or in either of the variables
  if (is.na(Extracted_data$Longitude[i]) || is.na(Extracted_data$Latitude[i]) || is.na(Extracted_data$Prec_Su[i])){
    full_vector <- c(full_vector,i)
  }
}
Extracted_data2 <- Extracted_data[-full_vector,]

## Calculate the data with NA values ##
NA_points <- Extracted_data[full_vector, c("Longitude", "Latitude")]

write.table(Extracted_data2, file= "Species_environmental_variables_2.txt", row.names=FALSE, col.names=TRUE, quote=FALSE, sep="\t") ### including all occurrences
