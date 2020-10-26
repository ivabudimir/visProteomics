#' Scatter plot of proteins in fractions
#'
#'@description Scatter plot based on a data frame. Data frame gives the list of proteins found in fractions. Points
#'can additionally be colored by the other column of given data frame.
#'
#'@param data Data frame with
#'@param protein_column Column used to identify the proteins. Default value is 'Accession'.
#'@param fraction_column Column used to identify the fraction number. Default value is 'Fraction_Number'.
#'@param color_column Column used to color the points of scatter plot. If it is missing, the plot will be in black.
#'@param list_of_proteins List of proteins we want to plot. In case we want to plot only a subset of all proteins. If it is missing,
#'all proteins in 'data' are plotted.
#'@param fraction_position Orientation of the plot. For 'x', fractions are plotted on x-axis. For 'y', fractions are plotted on y-axis.
#'Deafult is 'x'.
#'@param mark_size Size of point. Passed to 'ggplot2'. Default is 2.
#'@param mark_shape Shape of points. Passed to 'ggplot2'. Default is 18, a diamond shape.
#'@param protein_labels_size Size of axis labels for proteins. If missing, it is set automatically.
#'@param fraction_labels_size Size of axis labels for fractions. If missing, it is set automatically.
#'@param protein_name Name of axis. Default is "Protein".
#'@param fraction_name Name of axis. Default is "Fraction number"
#'@param color_name Name of color legend. By default, it is equal to 'color_column'
#'
#'@returns Returns 'ggplot2' object, the scatter plot in which every point represents a protein found in a fraction. Points can be
#'colored by 'color_column' and the color scale is shown in the legend. The plot can easily be saved using 'ggsave' function.
#'
#'
#'@export
fracPlot <- function(data, protein_column="Accession", fraction_column="Fraction_Number", color_column, list_of_proteins, fraction_position="x", mark_size=2, mark_shape=18, protein_labels_size, fraction_labels_size, protein_name="Protein", fraction_name="Fraction number", color_name=color_column){
  if(!inherits(data, "data.frame")){
    stop("'data' must be data frame")
  }
  if(any(!c(protein_column, fraction_column) %in% colnames(data))){
    stop("Wrong column names.")
  }
  if(!missing(color_column))
    if(!(color_column %in% colnames(data))){
      stop("Wrong 'color_column' name.")
    }
    if(!is.numeric(data[,color_column])){
      stop("'color_column' must be numeric.")
    }
  if(!fraction_position %in% c('x', 'y')){
    stop("Wrong position argument.")
  }

  #number_of_fractions <- length(unique(data[,fraction_column]))

  # set x/y labels size
  if(missing(protein_labels_size)){
    if(fraction_position=="y"){
      protein_labels_size <- 7
    }else{
      protein_labels_size <- 4
    }
  }
  if(missing(fraction_labels_size)){
    fraction_labels_size <- 11
  }
  xy_data <- c(fraction_column, protein_column)
  xy_text_size <- c(fraction_labels_size, protein_labels_size)
  xy_labels <- c(fraction_name, protein_name)
  angle_defined <- 0
  if(fraction_position=="y"){
    xy_data <- rev(xy_data)
    xy_text_size <- rev(xy_text_size)
    xy_labels <- rev(xy_labels)
    angle_defined <- 90
  }

  # if we want to plot only some proteins
  if(!missing(list_of_proteins)){
    data <- data[data[, protein_column] %in% list_of_proteins,]
  }

  if(missing(color_column)){
    sp <- ggplot2::ggplot(data, ggplot2::aes_string(x=xy_data[1], y=xy_data[2])) +
      ggplot2::geom_point(shape=mark_shape, size=mark_size, show.legend = TRUE, alpha=1, color='black') +
      ggplot2::theme(plot.margin=ggplot2::margin(1,1,1,1, "cm")) +
      ggplot2::theme(axis.title.x = ggplot2::element_text(size=15, color="black", vjust=-2)) +
      ggplot2::theme(axis.title.y = ggplot2::element_text(size=15, color="black", vjust=5)) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(size=xy_text_size[1], color="black", angle=angle_defined)) +
      ggplot2::theme(axis.text.y = ggplot2::element_text(size=xy_text_size[2], color="black")) +
      ggplot2::labs(x=xy_labels[1], y=xy_labels[2])
  }else{
    sp <- ggplot2::ggplot(data, ggplot2::aes_string(x=xy_data[1], y=xy_data[2], color=color_column)) +
      ggplot2::geom_point(shape=mark_shape, size=mark_size, show.legend = TRUE, alpha=1) +
      ggplot2::theme(plot.margin=ggplot2::margin(1,1,1,1, "cm")) +
      ggplot2::theme(axis.title.x = ggplot2::element_text(size=15, color="black", vjust=-2)) +
      ggplot2::theme(axis.title.y = ggplot2::element_text(size=15, color="black", vjust=5)) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(size=xy_text_size[1], color="black", angle=angle_defined)) +
      ggplot2::theme(axis.text.y = ggplot2::element_text(size=xy_text_size[2], color="black")) +
      ggplot2::labs(x=xy_labels[1], y=xy_labels[2], colour=color_name) +
      viridis::scale_color_viridis(option = 'magma', direction=-1, end=0.8, discrete=FALSE)
  }

  if(fraction_position=="y"){
    sp <- sp + ggplot2::scale_y_continuous(breaks=sort(unique(data()[,fraction_column])))
  }else{
    sp <- sp + ggplot2::scale_x_continuous(breaks=sort(unique(data[,fraction_column])))
  }

  return(sp)

}

