################################################################
########## General Cardinal extention Functions ################
################################################################

#' Function to create data matrix from MSI object
#'
#' @param MSIobject MSI object from Cardinal
#' @param inputNA Whether to convert 0's in matrix to NA (default = T)
#' @return dataframe with rows = ? cols = ?
#'
#' @export

createDatamatrix <- function(MSIobject, inputNA = T){

  MSIobject = as(MSIobject, "MSContinuousImagingExperiment")

  df <- data.frame(spectra(MSIobject))

  if(rownames(df)[1] == "1"){ # No row names
    df <- df %>%
      add_column(mz=as.numeric(mz(MSIobject))) %>%
      column_to_rownames(var = "mz") %>%
      rename_all(~sprintf("pixel_%s", 1:ncol(MSIobject)))
  }

  if(inputNA){
    df <- replace(df, df==0, NA)
  }

  return(df)
}



#' Function for median normalisation
#'
#' @param x vector (relating to pixel spectrum)
#'
#' @return normalised spectrum vector
#'
#' @export
medianNorm <- function(x){

  pixelInt <- as.vector(x) %>%
    ifelse(.==0, NA, .)

  #print(pixelInt)

  med <- median(pixelInt, na.rm=T)

  #print(med)

  newInt <- x / med
  #print(newInt)

  x[1:length(pixelInt)] <- newInt
}
#test <- daphProc[,1 ]



#' Imputes Missing Values and scales MSI object
#' @param MSIobject MSI object from Cardinal
#'
#' @return MSIobject with missing values imputed and datamatrix scaled.
#'
#' @export
impute_scale <- function(MSIobject){

  dataMatrix <- createDatamatrix(MSIobject)
  print(nrow(dataMatrix)) # features
  print(ncol(dataMatrix)) # pixels / samples
  MVimputed <- missing_values_df(dataMatrix, rowmax = 0.99, colmax = 0.99)
  #View(MVimputed[1:11, 1:11])
  print(nrow(MVimputed))
  print(ncol(MVimputed))

  # Pareto scale (variance stabilisation)
  scaled <- t(scale(t(MVimputed), center = T, scale = sqrt(apply(t(MVimputed), 2, sd, na.rm=T))))
  # mean center (to mean of all features in sample)
  # DIvide each feature by sd of feature - to stop high intense features skewing data (SD of intense features higher)

  #View(scaled[1:11, 1:11])

  #Create new object with intensity etc
  MSIobject <- MSImagingExperiment(imageData=as.matrix(scaled), featureData=fData(MSIobject), pixelData=pData(MSIobject))

  return(as(MSIobject, "MSProcessedImagingExperiment"))
}



#' Function to check colocalisation of suspected adduct pair
#' 
#' @param MSIobject MSI object from Cardinal
#' @param mz1 First mz value to run colocalisation against
#' @param mz2 Second mz value to check colocalisation score above given value
#' @param threshold Threshold score to define whether features colocalise
#' @param method Method to use for scoring colocalisation (Default = c("M1"))
#' @param topn Number of top colocalised features to check (default = 20)
#'
#' @return Boolean whether mz feature colocalise
#' @export

checkAdductPair <- function(MSIobject, mz1, mz2, threshold, method = "M1", topn=20){

  coloc <- data.frame( colocalized(MSIobject, mz = mz1, tolerance=20,
                                    units="ppm", n= topn, sort.by = c(method) ) ) %>%
    subset(correlation > threshold)

  #View(coloc)

  return(mz2 %in% coloc$mz)
}


#' Function to create x,y matrix for each feature - for feature selection.
#' @param MSIobject MSI object from Cardinal
#' @param mz mz channel to generate matrix for
#'
#' @return Data matrix for given mz; rows = y coordinate, cols = x-coordinate in the image
#' @export

create_xyMatrix <- function(MSIobject, mz){
  matrix = slice(MSIobject, mz=mz) %>%
    ifelse(. == 0, NA, .) %>%
    ifelse(is.na(.), median(., na.rm=T), .) # impute MVs - using median
  return(matrix)
}
#matrix = create_xyMatrix(MSIobject, mz=782.57)


#' Function to create dataframe with information about specific pixel in specific anatomical region of MSI object
#' @param MSIobject MSI object from Cardinal
#' @param pixel pixel to select spectrum from
#' @param anatomy label for the anatomical region pixel belongs to
#' @param normalisation Which normalisation step has been applied to 'MSIobject'
#' @param medianDF median spectrum to compare to
#'
#' @return Dataframe with headers: pixel, anatomy, mz, intensity (of mz feature), median, FC (to the median spectrum), normalisation
#' @export

pixelDF_NormCompare <- function(MSIobject, pixel, anatomy, normalisation, medianDF){

  subsetObject <- MSIobject[, pixel]  # Subset data, to only include sample

  pixelDF <- tibble(pixel=pixel, Anatomy = anatomy, mz= fData(subsetObject)@mz,
                    intensity = createDatamatrix(subsetObject)[,1]) %>%
    subset(mz %in% medianDF$mz) %>%
    mutate(median = medianDF$median,
           FC = intensity / median,
           normalisation = normalisation)

  return(pixelDF)
}

#' Function to create dataframe with information about all pixels in defined anatomical region of MSI object
#'
#' @param MSIobject MSI object from Cardinal
#' @param pixels pixels to select spectra from
#' @param anatomy label for the anatomical region pixels belongs to
#' @param normalisation Which normalisation step has been applied to 'MSIobject'
#' @param medianDF median spectrum to compare to
#'
#' @return Dataframe with headers: pixel, anatomy, mz, intensity (of mz feature), median, FC (to the median spectrum)
#' @export

pixelsDF_NormCompare <- function(MSIobject, pixels, anatomy, normalisation, medianDF){

  pixelsDF <- tibble(pixel=numeric(), Anatomy = character(), mz= numeric(),
                     intensity = numeric(), median = numeric(), FC = numeric())

  for(pixel in pixels){
    print(pixel)

    pixelDF <- pixelDF_NormCompare(MSIobject, pixel, anatomy, normalisation, medianDF)
    pixelsDF <- rbind(pixelsDF, pixelDF)
    #print(head(pixelsDF))
  }

  return(pixelsDF)
}


#' Function to create dataframe with information about all pixels in several defined anatomical regions of MSI object
#' Specifically used to compare distribution of feature distributions in different anatomical regions after normalisation
#'
#' @param MSIobject MSI object from Cardinal
#' @param pixel_list list of pixel vectors from different anatomical regions
#' @param anatomies vector of labels for the different anatomical regions pixels belongs to (same order as 'pixel_list')
#' @param normalisation Which normalisation step has been applied to 'MSIobject'
#' @param medianDF median spectrum to compare to
#'
#' @return Dataframe with headers: pixel, anatomy, mz, intensity (of mz feature), median, FC (to the median spectrum)
#' @export
normPixelsDF_anatomies <- function(MSIobject, pixel_list, anatomies, normalisation, medianDF){

  pixelsDF <- tibble(pixel=numeric(), Anatomy = character(), mz= numeric(),
                     intensity = numeric(), median = numeric(), FC = numeric())

  for(i in 1:length(anatomies)){

    pixels <- pixel_list[[i]]
    anatomy <- anatomies[i]

    print(anatomy)

    tempDF <- pixelsDF_NormCompare(MSIobject = MSIobject, pixels = pixels, anatomy=anatomy,
                                   normalisation=normalisation, medianDF= medianDF)

    pixelsDF <- rbind(pixelsDF, tempDF)
  }
  return(pixelsDF)
}


