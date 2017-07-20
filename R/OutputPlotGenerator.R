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
#' @import ggplot2
#' @import reshape2
#' @export
OutputPlotGenerator.create_convergence_plot = function(data, output, dir = '~/tmp/osl/') {
  labels = names(data)
  colors <- OutputPlotGenerator.get_colors(length(labels))
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  pdf(paste(dir,output,'.pdf',sep = ''))

  data <- lapply(data, function(dat) cumsum(dat)/seq(along=dat)) %>%
    as.data.frame

  data$epoch <- seq(nrow(data))
  data <- melt(data, id.vars='epoch')

  plotje <- ggplot(data,aes(epoch,value,color=variable))+
    geom_line() + 
    scale_color_manual(values = colors[seq_along(labels)])+
    theme(panel.background = element_rect(fill = 'transparent', colour = 'black', size=1))+
    scale_y_continuous(name="")+
    theme(axis.text.y = element_text(colour = "black") ) +
    theme(axis.text.x = element_text(colour = "black") ) +
    theme(axis.title.x = element_text(vjust = -0.5)) +
    theme(legend.title = element_blank())+
    theme(legend.position="bottom")+
    theme(legend.background = element_rect(colour="transparent"))+
    theme(legend.key = element_rect(fill = "transparent", colour = "transparent")) +
    theme(legend.key.size= unit(3,"lines"))

  plot(plotje)
  dev.off()
}


OutputPlotGenerator.create_risk_plot = function(risks, output, dir = '~/tmp/osl/') {

}

OutputPlotGenerator.get_colors = function(number_of_variables) {
  list(
    tol1qualitative=c("#4477AA"),
    tol2qualitative=c("#4477AA", "#CC6677"),
    tol3qualitative=c("#4477AA", "#DDCC77", "#CC6677"),
    tol4qualitative=c("#4477AA", "#117733", "#DDCC77", "#CC6677"),
    tol5qualitative=c("#332288", "#88CCEE", "#117733", "#DDCC77", "#CC6677"),
    tol6qualitative=c("#332288", "#88CCEE", "#117733", "#DDCC77", "#CC6677","#AA4499"),
    tol7qualitative=c("#332288", "#88CCEE", "#44AA99", "#117733", "#DDCC77", "#CC6677","#AA4499"),
    tol8qualitative=c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#CC6677","#AA4499"),
    tol9qualitative=c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#CC6677", "#882255", "#AA4499"),
    tol10qualitative=c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#661100", "#CC6677", "#882255", "#AA4499"),
    tol11qualitative=c("#332288", "#6699CC", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#661100", "#CC6677", "#882255", "#AA4499"),
    tol12qualitative=c("#332288", "#6699CC", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#661100", "#CC6677", "#AA4466", "#882255", "#AA4499")
  )[[number_of_variables]]
}
