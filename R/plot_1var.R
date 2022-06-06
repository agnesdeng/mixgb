#' Histogram with density plots for multiply imputed values for a single numeric variable
#' @description Plot histograms with density curves of observed values versus m sets of imputed values for a specified numeric variable using \pkg{ggplot2}.
#' @param imputation.list A list of \code{m} imputed datasets returned by the \code{mixgb} imputer, or other package.
#' @param var.name The name of a numeric variable of interest.
#' @param original.data The original data with missing values.
#' @param true.data The true data without missing values. In general, this is unknown. Only use for simulation studies.
#' @param color.pal A vector of hex color codes for the observed and m sets of imputed values panels. The vector should be of length \code{m+1}. Default: NULL (use "gray40" for the observed panel, use ggplot2 default colors for other panels.)
#' @importFrom scales hue_pal
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data
#' @importFrom grDevices nclass.Sturges
#' @importFrom ggplot2 ggplot aes vars geom_histogram geom_density facet_grid labs scale_color_manual scale_fill_manual guides theme element_text
#' @return Histogram with density plots
#' @export
#' @examples
#' #obtain m multiply datasets
#' imputed.data <- mixgb(data = nhanes3, m = 3)
#'
#' #plot the multiply imputed values for variable "BMPHEAD"
#' plot_hist(imputation.list = imputed.data, var.name = "BMPHEAD",
#'   original.data = nhanes3)
plot_hist <- function(imputation.list, var.name, original.data, true.data = NULL, color.pal = NULL) {

  Types <- feature_type(imputation.list[[1]])
  if (Types[var.name] == "binary" | Types[var.name] == "multiclass") {
    stop(paste("The variable", var.name, " is a factor. Please use plot_bar() instead."))
  }

  imp.sum <- summary1var(imputation.list = imputation.list, var.name = var.name, original.data = original.data, true.data = true.data, color.pal = color.pal)
  all.dt <-imp.sum$all.dt
  color.pal<-imp.sum$color.pal

  breaks <- pretty(range(all.dt[[var.name]]),
                   n = nclass.Sturges(all.dt[[var.name]]),
                   min.n = 1
  )

  ggplot(data = all.dt, aes(x = .data[[var.name]])) +
    geom_histogram(alpha = 0.5, aes(fill = m.set, color = m.set, y = ..density..), breaks = breaks) +
    geom_density(size = 1, alpha = 0.6, aes(color = m.set)) +
    facet_grid(cols = vars(m.set)) +
    labs(title = "Histogram with density curve", subtitle = paste(" imputed sets for variable: ", var.name)) +
    scale_color_manual(values = color.pal) +
    scale_fill_manual(values = color.pal) +
    guides(color = "none", fill = "none", linetype = "none") +
    theme(
      strip.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(face = "bold")
    )
}




#' Boxplots with data points for multiply imputed values for a single numeric variable
#' @description Plot boxplots with data points of observed values versus m sets of imputed values for a specified numeric variable using \pkg{ggplot2}.
#' @param imputation.list A list of \code{m} imputed datasets.
#' @param var.name The name of a numeric variable of interest.
#' @param original.data The original data with missing values.
#' @param true.data The true data without missing values. In general, this is unknown. Only use for simulation studies.
#' @param color.pal A vector of hex color codes for the observed and m sets of imputed values panels. The vector should be of length \code{m+1}. Default: NULL (use "gray40" for the observed panel, use ggplot2 default colors for other panels.)
#' @importFrom scales hue_pal
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot aes vars geom_point geom_jitter position_jitter geom_boxplot facet_grid labs scale_color_manual scale_fill_manual guides theme element_text element_blank
#' @return Boxplots with data points for a numeric variable
#' @export
#' @examples
#' #obtain m multiply datasets
#' imputed.data <- mixgb(data = nhanes3, m = 3)
#'
#' #plot the multiply imputed values for variable "BMPHEAD"
#' plot_box(imputation.list = imputed.data, var.name = "BMPHEAD",
#'   original.data = nhanes3)
plot_box <- function(imputation.list, var.name, original.data, true.data = NULL, color.pal = NULL) {

  Types <- feature_type(imputation.list[[1]])
  if (Types[var.name] == "binary" | Types[var.name] == "multiclass") {
    stop(paste("The variable", var.name, " is a factor. Please use plot_bar() instead."))
  }

  imp.sum <- summary1var(imputation.list = imputation.list, var.name = var.name, original.data = original.data, true.data = true.data, color.pal = color.pal)
  all.dt <-imp.sum$all.dt
  color.pal<-imp.sum$color.pal



  if (Types[var.name] == "integer") {
    gp <- ggplot(all.dt, aes(x = obs, y = .data[[var.name]])) +
      geom_point(alpha = 0.6, aes(color = m.set))
  } else {
    gp <- ggplot(all.dt, aes(x = obs, y = .data[[var.name]])) +
      geom_jitter(alpha = 0.6, position = position_jitter(), aes(color = m.set))
  }
  gp +
    geom_boxplot(alpha = 0.5, aes(fill = m.set), outlier.shape = NA) +
    facet_grid(cols = vars(m.set), scales = "free_x") +
    labs(title = "Box plots with data points", subtitle = paste(" imputed sets for variable: ", var.name)) +
    scale_color_manual(values = color.pal) +
    scale_fill_manual(values = color.pal) +
    guides(color = "none", fill = "none") +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      strip.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold")
    )
}

#' Bar plots for multiply imputed values for a single factor variable
#' @description Plot bar plots of observed values versus m sets of imputed values for a specified factor variable using \pkg{ggplot2}.
#' @param imputation.list A list of \code{m} imputed datasets returned by the \code{mixgb} imputer
#' @param var.name The name of a factor variable of interest
#' @param original.data The original data with missing data
#' @param true.data The true data without missing values. In general, this is unknown. Only use for simulation studies.
#' @param color.pal A vector of hex color codes for the observed and m sets of imputed values panels. The vector should be of length \code{m+1}. Default: NULL (use "gray40" for the observed panel, use ggplot2 default colors for other panels.)
#' @importFrom scales hue_pal
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot aes vars geom_bar scale_y_continuous ylab facet_grid labs scale_color_manual scale_fill_manual guides theme element_text
#' @return Bar plots for a factor variable
#' @export
#' @examples
#' #create some extra missing values in a factor variable "HSSEX" (originally fully observed)
#' nhanes3_NA<-createNA(nhanes3,var.names="HSSEX",p=0.1)
#'
#' #obtain m multiply datasets
#' imputed.data <- mixgb(data = nhanes3_NA, m = 3)
#'
#' #plot the multiply imputed values for variable "HSSEX"
#' plot_bar(imputation.list = imputed.data, var.name = "HSSEX",
#'   original.data = nhanes3_NA)
plot_bar <- function(imputation.list, var.name, original.data, true.data = NULL, color.pal = NULL) {

  Types <- feature_type(imputation.list[[1]])
  if (Types[var.name] == "numeric") {
    stop(paste("The variable", var.name, " is a numeric variable. Please use plot_hist() or plot_box() instead."))
  }

  imp.sum <- summary1var(imputation.list = imputation.list, var.name = var.name, original.data = original.data, true.data = true.data, color.pal = color.pal)
  all.dt <-imp.sum$all.dt
  color.pal<-imp.sum$color.pal

  if (Types[var.name] == "integer") {
    warning("The variable ", var.name, " is of integer type. It will be treated as a factor for bar plots.\n")
    all.dt[[var.name]] <- factor(all.dt[[var.name]])
  }

  ggplot(all.dt, aes(x = .data[[var.name]])) +
    geom_bar(stat = "count", alpha = 0.8, width = 0.8, aes(color = m.set, fill = m.set, y = ..prop.., group = m.set)) +
    scale_y_continuous(labels = scales::percent) +
    ylab("Proportion") +
    facet_grid(cols = vars(m.set)) +
    labs(title = "Bar plots", subtitle = paste(" imputed sets for variable: ", var.name)) +
    scale_fill_manual(values = color.pal) +
    scale_color_manual(values = color.pal) +
    guides(color = "none", fill = "none") +
    theme(
      strip.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold")
    )
}


#extract results for plotting one variable
summary1var<-function(imputation.list, var.name, original.data, true.data, color.pal){
  if(!identical(dim(imputation.list[[1]]),dim(original.data))){
    stop("The dimension of the imputed dataset needs to be the same as the dimension of the data specified in `original.data`.")
  }
  Names <- colnames(original.data)
  if (!any(Names == var.name)) {
    stop("The variable ", var.name, " does not exist in the original data.")
  }

  missing.vars <- Names[colSums(is.na(original.data)) != 0]
  if (!any(missing.vars == var.name)) {
    stop(paste("The variable ", var.name, " has no missing value."))
  }

  if (!is.null(true.data)){
    if(any(is.na(true.data[[var.name]]))){
      stop(paste("The variable ", var.name, " in `true.data` contains missing values."))
    }
  }

  na.idx <- which(is.na(original.data[[var.name]]))
  imp.l <- lapply(imputation.list, function(dt) dt[[var.name]][na.idx])
  N.imp <- length(imp.l)
  M <- paste("m", 1:N.imp, sep = "")
  names(imp.l) <- M
  imp.df <- do.call(cbind.data.frame, imp.l)
  imp.dt<-as.data.table(imp.df)
  #imp.df$obs <- 1:nrow(imp.df)
  imp.dt = melt(imp.dt, measure.vars = M, variable.name = "m.set", value.name = var.name, value.factor = TRUE)
  #imp.df <- tidyr::pivot_longer(data = imp.df, cols=everything(), names_to = "m.set", values_to = var.name)
  #imp.df <- tidyr::pivot_longer(data = imp.df, cols =!obs, names_to = "m.set", values_to = var.name)
  #imp.df$m.set <- factor(imp.df$m.set, levels = M)

  # Observed
  observed <- original.data[[var.name]][-na.idx]
  N.obs <- length(observed)
  N.mis<-length(na.idx)
  #observed.df <- data.frame(1:N.obs, rep("Observed", N.obs), observed)
  #colnames(observed.df) <- c("obs", "m.set", var.name)



  if (class(imp.dt[[var.name]])[1] != class(original.data[[var.name]])[1]) {
    # Special case: original ordinal-> imputed as integer
    observed <- fac2int(observed)
  }

  if(!is.null(true.data)){
    #with true.data
    true<-true.data[[var.name]][na.idx]
    if (class(imp.dt[[var.name]])[1] != class(original.data[[var.name]])[1]) {
      true<-fac2int(true)
    }
    other.dt<-data.table(m.set=c(rep("Observed",N.obs),rep("MaskedTrue",N.mis)),var.name=c(observed,true))
    all.dt<- rbind(other.dt,imp.dt,use.names=FALSE)
    all.dt[,obs:=c(1:N.obs,rep(1:N.mis,N.imp+1))]
    if (is.null(color.pal)) {
      color.pal <- c("gray40","gray20", scales::hue_pal()(N.imp))
    }
  }else{
    #without true.data
    other.dt<-data.table(m.set=c(rep("Observed",N.obs)),var.name=observed)
    all.dt<- rbind(other.dt,imp.dt,use.names=FALSE)
    all.dt[,obs:=c(1:N.obs,rep(1:N.mis,N.imp))]
    if (is.null(color.pal)) {
      color.pal <- c("gray40", scales::hue_pal()(N.imp))
    }
  }

  setnames(all.dt,"var.name",var.name)
  #colnames(all.dt)[2]<-var.name
  return(list("all.dt"=all.dt,"color.pal"=color.pal))
}
