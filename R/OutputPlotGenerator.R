#' OutputPlotGenerator.create_density_plot
#' Function to plot the true and predicted densities in one plot.
#'
#' @param yValues vector of the true outcome values 
#' @param estimated_probabilities vector of the estimated probabilities
#' @param estimated_probabilities vector of the estimated y_values (i.e., sampled values)
#' @param output string with the filename
#' @param dir the directory to plot to
#' @export
OutputPlotGenerator.create_density_plot = function(yValues, estimated_probabilities, estimated_y_values = NULL, output, dir = '~/tmp/osl/') {
  ## plot densitity first:
  vals <- unique(yValues)
  if(length(vals) == 2 ) {
    ## If the outcome is binary, we can see how well it managed to predict the whole distribution
    ## This error term should be small (~ 0.001)
    abs(mean(estimated_probabilities[yValues == vals[1] ]) - mean(yValues == vals[1]))
    abs(mean(estimated_probabilities[yValues == vals[2] ]) - mean(yValues == vals[2]))
  }
  true_density <- density(yValues)

  ## Normalize the density to 1
  ##true_density$y <- diff(true_density$x) * true_density$y

  ## Draw a line by default, instead of dots
  type = "l"

  if (is.null(estimated_y_values)) {
    estimated_y_values <- yValues 
    type = "p"
  }

  ## Normalize to sum to one, to make it an actual density

  ## Save the output in a dir so we can access it later
  date <- format(Sys.time(), "%y%m%d%H%M")
  full_dir <- paste(dir, date, '/', sep ='')
  dir.create(full_dir, showWarnings = FALSE, recursive = TRUE)

  pdf(paste(full_dir,output,'.pdf',sep = ''))
  plot(true_density, ylim=c(0,max(estimated_probabilities)+.5))
  lines(estimated_y_values, estimated_probabilities, type = type, cex = .3, col = "red",
        ylim=c(0,max(estimated_probabilities)+.5))
  dev.off()
}

#' OutputPlotGenerator.create_convergence_plot
#' Function to plot the convergence to the parameter of interest of both the truth and the estimation.
#'
#' @param truth_approximation the truth (i.e., as found using simulation / monte carlo approximation)
#' @param estimated_approximation the estimate (i.e., as found using machine learning and monte carlo approximation)
#' @export
OutputPlotGenerator.create_convergence_plot = function(truth_approximation, estimated_approximation, output, dir = '~/tmp/osl/') {
  y1 <- cumsum(truth_approximation)/seq(along=truth_approximation)
  y2 <- cumsum(estimated_approximation)/seq(along=estimated_approximation)

  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  pdf(paste(dir,output,'.pdf',sep = ''))
  plot(y1, ylim=range(c(y1, y2)))
  par(new=TRUE)
  plot(y2, ylim=range(c(y1, y2)), col="red", axes = FALSE, xlab = "", ylab = "")
  dev.off()
}
