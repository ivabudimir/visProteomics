#' Data frame of Venn regions
#'
#' @description Returns a data frame with description of each Venn diagram region.
#' @details This function gives a detailed explanation of each Venn diagram region. Sets can be given as list
#' of vectors or as list of data frames. In case of data frames, the column "by" defines elements of Venn
#' diagram. The result is a data frame with all elements listed in rows and column "venn_region" specifying the
#' corresponding Venn region for each element.
#' The additional columns of given data frames can be used to further describe each element of Venn regions,
#' as specified by "columns_to_keep". In case the specified column for an element has the same value in all data frames,
#' a unique value will be returned. Otherwise, all values are returned. \cr
#' If instead of detailed description, we wish to know only number of elements in each Venn diagram region,
#' we should set argument "only_counts" to TRUE.
#'
#' @param sets A list of sets. Sets must be vectors or data frames. If they are given as data frames,
#' an argument "by" must be specified.
#' @param by The name of elements in Venn diagram. If sets are data frames, it specifies a column used to
#' construct Venn regions. Default value is "element".
#' @param columns_to_keep Defines which columns of data frames to keep in the result data frame. If NULL,
#' only "by" column is kept. If "all", all columns are kept. Defaults to NULL.
#' @param only_counts If TRUE, instead of data frame function returns only number of elements in each Venn region.
#' Defaults to FALSE.
#'
#' @return A data frame of all elements from Venn regions. Column named with "by" argument lists all elements from
#' union of all sets. Column "venn_region" is a corresponding disjoint Venn region to which the element belongs.
#' Intersections are described with "&" character between sets' names. Additional columns are defined with
#' "column_to_keep" argument. If argument "only_counts" is TRUE, instead of data frame, the function returns a
#' elements in each Venn region.
#'
#' @export
vennDataFrame <- function(sets, by="element", columns_to_keep=NULL, only_counts=FALSE){
  if(!inherits(sets, "list")){
    stop('Parameter "sets" must be list.')
  }
  n_sets <- length(sets)

  # check sets' names: assigns names if not assigned; checks for duplicates
  if(is.null(names(sets))){
    if(n_sets>26){
      stop("You have to name your sets.")
    }
    names(sets) <- LETTERS[1:n_sets]
  }else if(any(duplicated(names(sets)))){
      warning("There are duplicated names of sets. Only the first set from duplicates is kept.")
      sets <- sets[!duplicated(names(sets))]
      n_sets <- length(sets)
    }

  # check if list elements are vectors or data frames
  if(all(sapply(sets, function(x) inherits(x, c("character", "numeric", "logical"))))){
    is_dataframe=FALSE
  }else if(all(sapply(sets, function(x) inherits(x, "data.frame")))){
    sets_df <- sets
    sets_columns <- lapply(sets, function(x) colnames(x))
    # check if all sets have 'by' column
    if(all(sapply(sets_columns, function(x) by %in% x))){
      sets <- lapply(sets, function(x) x[,by])
      is_dataframe=TRUE
    }else{
      stop('All data frames must have "by" column.')
    }
  }else{
    stop("All sets must be data frames or all sets must be vectors.")
  }

  # create data frame with two columns: 'by' and "venn_region"
  sets_names <- names(sets)
  elements_all <- Reduce(union, sets)
  incidence_matrix <- as.data.frame(outer(elements_all, sets_names, Vectorize(function(el, set) el %in% sets[[set]])))
  rownames(incidence_matrix) <- elements_all
  colnames(incidence_matrix) <- sets_names
  incidence_matrix <- incidence_matrix[do.call('order', c(incidence_matrix[0:n_sets], list(decreasing=TRUE))),]
  incidence_matrix["n_sets"] <- rowSums(incidence_matrix) #just for sorting
  incidence_matrix <- incidence_matrix[with(incidence_matrix, order(n_sets)),]
  incidence_matrix <- incidence_matrix[0:n_sets]
  venn_df <- data.frame(matrix(nrow=length(elements_all), ncol=2))
  colnames(venn_df) <- c(by, "venn_region")
  venn_df[by] <- rownames(incidence_matrix)
  venn_df["venn_region"] <- unlist(apply(incidence_matrix, 1, function(x) paste(sets_names[x], collapse="&")))

  #check if we only return number of elements in Venn regions
  if(only_counts){
    return(table(factor(venn_df[,"venn_region"], levels=unique(venn_df[,"venn_region"]))))
  }

  #check if we need to add columns in data frame
  if(is_dataframe & !is.null(columns_to_keep)){
    columns_all <- Reduce(union, sets_columns)
    columns_all <- columns_all[columns_all!=by]
    columns_to_keep <- intersect(columns_to_keep, columns_all)
    if(length(columns_to_keep)==0){
      warning('"columns_to_keep" are not columns of the given data frames.')
      return(venn_df)
    }
    # add columns to venn_df
    # if all data frames have the same value, we report that value
    # if different data frames have different values, we report all of them: e.g. A=5;B=7
    for (i in 1:length(columns_to_keep)){
      col_i <- columns_to_keep[i]
      sets_with_col <- sapply(sets_columns, function(x) col_i %in% x)
      col_description <- rownames(incidence_matrix)
      for(j in 1:length(col_description)){
        el_j <- col_description[j]
        sets_j <- colnames(incidence_matrix)[unlist(incidence_matrix[j,]) & sets_with_col]
        if(length(sets_j)==0){
          col_description[j] <- NA
          next
        }
        values_j <- sapply(sets_df[sets_j], function(x) as.character(x[x[by]==el_j, col_i]))
        if((length(unique(values_j))==1) & all(incidence_matrix[j,] <= sets_with_col)){
          col_description[j] <- values_j[1]
        }else{
          col_description[j] <- paste(sets_j, values_j, sep="=", collapse=";")
        }
      }
      venn_df[col_i] <- col_description
    }
  }
  return(venn_df)
}
