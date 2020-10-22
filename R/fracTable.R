#' Table of fractions' attributes
#'
#'@description Counts number of elements in each bin based on given attribute for every fraction.
#'
#'@details For a given 'finite_bin_limits' it creates bins considering also the arguments 'include_minInf' and 'include_Inf'.
#' For every fraction, it counts number of non-NA elements which fall into each bin. Lower boundary of a bin is included and
#' upper boundary is excluded. If 'NA' values are present, their number per fraction is reported.
#'
#'@param data Data frame with columns 'column_to_bin' and 'fraction_column'. Every row is considered to be
#'an unique element used for binning.
#'@param column_to_bin Column whose values are used for binning.S
#'@param fraction_column Column used to identify the fraction number. Default value is 'Fraction_Number'.
#'@param finite_bin_limits Finite delimiters between bin
#'@param include_minInf Include minus infinity as the lower boundary of the first bin. Default is 'FALSE'.
#'@param include_Inf Include infinity as the upper boundary of the last bin. Default is 'FALSE'.
#'
#'@return A data frame with fractions as rows and bins as columns. Cells count number of elements in fraction
#'which 'column_to_bin' is in the corresponding bin's range. Additional columns are 'NA_values', 'total',
#''min' and 'max'. The last row reports total number of elements per bin.
#'
#'@export
fracTable <- function(data, fraction_column="Fraction_Number", column_to_bin, finite_bin_limits, include_minInf=FALSE, include_Inf=FALSE){
  if(!inherits(data, "data.frame")){
    stop("'data' must be data frame")
  }
  if(any(!c(column_to_bin, fraction_column) %in% colnames(data))){
    stop("Wrong column names.")
  }
  if(any(!inherits(c(include_minInf, include_Inf), "logical"))){
    stop("Unclear inclusion of infinity.")
  }

  fraction_names <- sort(unique(data[,fraction_column])) #we extract all fraction names
  number_of_fractions <- length(fraction_names)

  # we will name columns with their range, e.g."bin:-1 to 0"
  number_of_bins <- length(finite_bin_limits) + include_minInf + include_Inf -1
  bins_lowerBoundaries <- c(-Inf, finite_bin_limits)[c(include_minInf, rep(TRUE, length(finite_bin_limits)-1), include_Inf)]
  bins_upperBoundaries <- c(finite_bin_limits, Inf)[c(include_minInf, rep(TRUE, length(finite_bin_limits)-1), include_Inf)]
  bins_names <- paste('bin:', bins_lowerBoundaries, '-', bins_upperBoundaries, sep='')

  # checking if there are any NA values in the column of interest
  na_values_present <- any(is.na(data[,column_to_bin]))
  if(na_values_present){
    warning("NA values are present in 'column_to_bin'. There will be excluded from count and reported in the output column 'NA_values'.")
  }

  # creating the new dataframe
  new_table <- data.frame(matrix(nrow=number_of_fractions, ncol=number_of_bins+4+na_values_present))
  if(na_values_present){
    colnames(new_table) <- c(fraction_column, bins_names, 'NA_values', 'total', paste('min', column_to_bin, sep='_'), paste('max', column_to_bin, sep='_'))
  }else{
    colnames(new_table) <- c(fraction_column, bins_names, 'total', paste('min', column_to_bin, sep='_'), paste('max', column_to_bin, sep='_'))
  }


  # filling the new dataframe
  new_table[fraction_column] <- fraction_names
  for (i in 1:number_of_fractions){
    fraction_i <- fraction_names[i] # name of the ith fraction
    numbers_to_bin_i <- data[data[,fraction_column]==fraction_i, column_to_bin] # we extract numbers which we want to bin and which belong to the ith fraction
    # if there are NA values, we extract the number of NA values in ith fraction and then remove them before binning
    if(na_values_present){
      new_table[i, 'NA_values'] <- sum(is.na(numbers_to_bin_i))
      numbers_to_bin_i <- numbers_to_bin_i[!is.na(numbers_to_bin_i)]
    }
    # now we fill in the values of the ith row:
    new_table[i, 2:(number_of_bins+1)] <- sapply(1:number_of_bins, function(x) sum((numbers_to_bin_i >= bins_lowerBoundaries[x]) & (numbers_to_bin_i < bins_upperBoundaries[x])))
    new_table[i, 'total'] <- sum(new_table[i, 2:(number_of_bins+na_values_present+1)])
    new_table[i, number_of_bins+3+na_values_present] <- min(numbers_to_bin_i)
    new_table[i, number_of_bins+4+na_values_present] <- max(numbers_to_bin_i)
  }
  new_table['total',2:(number_of_bins+2+na_values_present)] <- apply(new_table[,2:(number_of_bins+2+na_values_present)], 2, sum) # add total sum per bin

  return(new_table)

}
