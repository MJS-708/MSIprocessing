#####################################################
################ General Functions ##################
#####################################################

#' Function to calculate median of vector excluding 0s
#' @param x vector to calculate median of
#'
#' @examples
#' vec <- c(0,1,0,2,5, 0, 0)
#' myMed(vec)
#'
#' @return Median value
#' @export

myMed <- function(x){ #
  x = ifelse(x==0, NA, x)
  x = median(x, na.rm=T)
  return(x)
}

#' Function to calculate median of vector excluding 0s and return NA if more 0s than given threshold
#'
#' @param x vector to calculate median of
#' @param maxNAs Fraction of missing values allowed in vector, if > 0s than `maxNAs` an NA is returned
#'
#' @examples
#' vec <- c(0,1,0,2,5, 0, 0)
#' med(vec)
#'
#' @return Median value
#' @export

med <- function(x, maxNAs = 1){
  x = ifelse(x==0, NA, x)
  if(mean(is.na(x)) >= maxNAs){
    x = NA
  }
  else{x= median(x, na.rm=T)}
  return(x)
}


#' Function to calculate mean of vector excluding 0s
#' @param x vector to calculate mean of
#'
#' @examples
#' vec <- c(0,1,0,2,5, 0, 0)
#' myMean(vec)
#'
#' @return Mean value
#' @export

myMean <- function(x){ #
  x = ifelse(x==0, NA, x)
  x = mean(x, na.rm=T)
  return(x)
}

#' Function to calculate tolerance m/z values from m/z value and ppm tolerance
#'
#' @param mz mz of interest
#' @param ppm_tol ppm tolerance to calculate lower and upper limits
#'
#' @examples
#' ppm_limits(400, 30)
#'
#' @return Vector of lower, upper mz values and mz of interest respectively.
#' @export

ppm_limits <- function(mz, ppm_tol){
  lower <- (mz - (mz * ppm_tol *1e-6))
  upper <- (mz + (mz * ppm_tol *1e-6))
  limits <- c(lower, upper, mz)
  return(limits)}


#' Function to calculate ppm error between mz value and theoretical mass
#'
#' @param theoretical theoretical mz of a molecule
#' @param measured measured mz
#'
#' @examples
#' error(400, 400.01)
#'
#' @return ppm error
#' @export

error <- function(theoretical, measured){
  val <- (1e6 * (measured - theoretical)) / theoretical
  return(val)
}


#' Function to imputes Missing Values into data matrix using knn
#' @param df dataframe with rows = ?, cols = ?
#' @param k number of nearest neighbours for knn (default = 10)
#' @param rowmax maximum number of missing values in row of df (default = 0.5)
#' @param colmax maximum number of missing values in col of df (default = 0.8)
#' @param maxp Check `impute::impute.knn` (default = NULL)
#'
#' @return df with imputed values - rows = ?, cols = ?
#' @export

missing_values_df <- function(df, k=10, rowmax=0.5, colmax=0.8, maxp=NULL){

  rm <- as.numeric(which(colMeans(is.na(df)) > colmax))

  if(length(rm) > 0){
    df <- df[,-(rm)]}

  if (is.null(maxp)){
    maxp <- max(dim(df))  }

  tryCatch(
    {
      obj <- suppressWarnings(impute::impute.knn(as.matrix(df), k=k, rowmax=rowmax, colmax=colmax,maxp = maxp))
      df <- obj$data

      df <- as.data.frame(df)

      return(df)
    },
    error=function(err){
      print('FAIL!')
      print("error:")
      message(err)
      print("")
      return(NA)
    })
}


#' Function to Calculate svd of specific mz feature - using output from x,y matrix. Used for feature selection.
#' @param matrix xyMatrix as output from `create_xyMatrix`
#'
#' @return Vector with svd percentage and squared svd percentage respectively
#' @export

calc_svds <- function(matrix){
  svd_object = svd(matrix, 0, 0)
  svd_perc = (svd_object$d[1]/sum(svd_object$d)) *100
  square_svd_perc = (svd_object$d[1]^2/sum(svd_object$d^2)) *100
  return(c(svd_perc, square_svd_perc))
}
#calc_svds(test2)
