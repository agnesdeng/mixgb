#' Histogram with density plots for multiply imputed values for a single numeric variable
#' @description Plot histograms with density curves of observed values versus m sets of imputed values for a specified numeric variable using \pkg{ggplot2}.
#' @param imputation.list A list of \code{m} imputed datasets returned by the \code{mixgb} imputer, or other package
#' @param var.num The name of a numeric variable of interest
#' @param original.data The original data with missing data
#' @param color.pal A vector of hex color codes for the observed and m sets of imputed values panels. The vector should be of length \code{m+1}. Default: NULL (use "gray40" for the observed panel, use ggplot2 default colors for other panels.)
#' @importFrom scales hue_pal
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data
#' @importFrom grDevices nclass.Sturges
#' @importFrom ggplot2 ggplot aes vars geom_histogram geom_density facet_grid labs scale_color_manual scale_fill_manual guides theme element_text
#' @return Histogram with density plots
#' @export
#' @examples
#' \donttest{
#'
#' #obtain m multiply datasets
#' imputed.data <- mixgb(data = nhanes3_newborn, m = 5)
#'
#' #plot the multiply imputed values for variable "BMPHEAD"
#' plot_hist(imputation.list = imputed.data, var.num = "BMPHEAD",
#'   original.data = nhanes3_newborn)
#' }
plot_hist <- function(imputation.list, var.num, original.data, color.pal = NULL) {
  if(!identical(dim(imputation.list[[1]]),dim(original.data))){
    stop("The dimension of the imputed dataset needs to be the same as the dimension of the data specified in `original.data`.")
  }
  Names <- colnames(original.data)
  if (!any(Names == var.num)) {
    stop("The variable ", var.num, " does not exist in the original data.")
  }

  missing.vars <- Names[colSums(is.na(original.data)) != 0]
  if (!any(missing.vars == var.num)) {
    stop(paste("The variable ", var.num, " has no missing value."))
  }


  Types <- feature_type(imputation.list[[1]])
  if (Types[var.num] == "binary" | Types[var.num] == "multiclass") {
    stop(paste("The variable", var.num, " is a factor. Please use plot_bar() instead."))
  }

  na.idx <- which(is.na(original.data[[var.num]]))
  result.l <- lapply(imputation.list, function(dt) dt[[var.num]][na.idx])
  N_imp <- length(result.l)
  M <- paste("m", 1:N_imp, sep = "")
  names(result.l) <- M
  result.df <- do.call(cbind.data.frame, result.l)
  result.df$obs <- 1:nrow(result.df)
  result.df <- tidyr::pivot_longer(data = result.df, cols =!obs, names_to = "m.set", values_to = var.num)
  result.df$m.set <- factor(result.df$m.set, levels = M)

  # Observed
  observed <- original.data[[var.num]][-na.idx]
  N.obs <- length(observed)
  observed.df <- data.frame(1:N.obs, rep("Observed", N.obs), observed)
  colnames(observed.df) <- c("obs", "m.set", var.num)

  if (class(result.df[[var.num]])[1] == class(observed.df[[var.num]])[1]) {
    result.df <- rbind(result.df, observed.df)
    result.df$m.set <- factor(result.df$m.set, levels = c("Observed", M))
  } else {
    # Special case: original ordinal-> imputed as integer
    observed.df[[var.num]] <- fac2int(observed.df[[var.num]])
    result.df <- rbind(result.df, observed.df)
    result.df$m.set <- factor(result.df$m.set, levels = c("Observed", M))
  }

  if (is.null(color.pal)) {
    color.pal <- c("gray40", scales::hue_pal()(N_imp))
  }

  breaks <- pretty(range(result.df[[var.num]]),
    n = nclass.Sturges(result.df[[var.num]]),
    min.n = 1
  )


  ggplot(data = result.df, aes(x = .data[[var.num]])) +
    geom_histogram(alpha = 0.5, aes(fill = m.set, color = m.set, y = ..density..), breaks = breaks) +
    geom_density(size = 1, alpha = 0.6, aes(color = m.set)) +
    facet_grid(cols = vars(m.set)) +
    labs(title = "Histogram with density curve", subtitle = paste(N_imp, " imputed sets for variable: ", var.num)) +
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
#' @param imputation.list A list of \code{m} imputed datasets
#' @param var.num The name of a numeric variable of interest
#' @param original.data The original data with missing data
#' @param color.pal A vector of hex color codes for the observed and m sets of imputed values panels. The vector should be of length \code{m+1}. Default: NULL (use "gray40" for the observed panel, use ggplot2 default colors for other panels.)
#' @importFrom scales hue_pal
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot aes vars geom_point geom_jitter position_jitter geom_boxplot facet_grid labs scale_color_manual scale_fill_manual guides theme element_text element_blank
#' @return Boxplots with data points for a numeric variable
#' @export
#' @examples
#' \donttest{
#'
#' #obtain m multiply datasets
#' imputed.data <- mixgb(data = nhanes3_newborn, m = 5)
#'
#' #plot the multiply imputed values for variable "BMPHEAD"
#' plot_box(imputation.list = imputed.data, var.num = "BMPHEAD",
#'   original.data = nhanes3_newborn)
#' }
plot_box <- function(imputation.list, var.num, original.data, color.pal = NULL) {
  if(!identical(dim(imputation.list[[1]]),dim(original.data))){
    stop("The dimension of the imputed dataset needs to be the same as the dimension of the data specified in `original.data`.")
  }
  Names <- colnames(original.data)
  if (!any(Names == var.num)) {
    stop("The variable ", var.num, " does not exist in the original data.")
  }

  missing.vars <- Names[colSums(is.na(original.data)) != 0]
  if (!any(missing.vars == var.num)) {
    stop(paste("The variable ", var.num, " has no missing value."))
  }


  Types <- feature_type(imputation.list[[1]])
  if (Types[var.num] == "binary" | Types[var.num] == "multiclass") {
    stop(paste("The variable", var.num, " is a factor. Please use plot_bar() instead."))
  }

  na.idx <- which(is.na(original.data[[var.num]]))
  result.l <- lapply(imputation.list, function(dt) dt[[var.num]][na.idx])
  N_imp <- length(result.l)
  M <- paste("m", 1:N_imp, sep = "")
  names(result.l) <- M
  result.df <- do.call(cbind.data.frame, result.l)
  result.df$obs <- 1:nrow(result.df)
  result.df <- tidyr::pivot_longer(data = result.df, cols = !obs, names_to = "m.set", values_to = var.num)
  result.df$m.set <- factor(result.df$m.set, levels = M)

  # Observed
  observed <- original.data[[var.num]][-na.idx]
  N.obs <- length(observed)
  observed.df <- data.frame(1:N.obs, rep("Observed", N.obs), observed)
  colnames(observed.df) <- c("obs", "m.set", var.num)


  if (class(result.df[[var.num]])[1] == class(observed.df[[var.num]])[1]) {
    result.df <- rbind(result.df, observed.df)
    result.df$m.set <- factor(result.df$m.set, levels = c("Observed", M))
  } else {
    # original ordinal-> imputed integer
    observed.df[[var.num]] <- fac2int(observed.df[[var.num]])
    result.df <- rbind(result.df, observed.df)
    result.df$m.set <- factor(result.df$m.set, levels = c("Observed", M))
  }


  if (is.null(color.pal)) {
    color.pal <- c("gray40", scales::hue_pal()(N_imp))
  }

  if (Types[var.num] == "integer") {
    gp <- ggplot(result.df, aes(x = obs, y = .data[[var.num]])) +
      geom_point(alpha = 0.6, aes(color = m.set))
  } else {
    gp <- ggplot(result.df, aes(x = obs, y = .data[[var.num]])) +
      geom_jitter(alpha = 0.6, position = position_jitter(), aes(color = m.set))
  }
  gp +
    geom_boxplot(alpha = 0.5, aes(fill = m.set), outlier.shape = NA) +
    facet_grid(cols = vars(m.set), scales = "free_x") +
    labs(title = "Box plots with data points", subtitle = paste(N_imp, " imputed sets for variable: ", var.num)) +
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
#' @param var.fac The name of a factor variable of interest
#' @param original.data The original data with missing data
#' @param color.pal A vector of hex color codes for the observed and m sets of imputed values panels. The vector should be of length \code{m+1}. Default: NULL (use "gray40" for the observed panel, use ggplot2 default colors for other panels.)
#' @importFrom scales hue_pal
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot aes vars geom_bar scale_y_continuous ylab facet_grid labs scale_color_manual scale_fill_manual guides theme element_text
#' @return Bar plots for a factor variable
#' @export
#' @examples
#' \donttest{
#'
#' #obtain m multiply datasets
#' imputed.data <- mixgb(data = nhanes3_newborn, m = 5)
#'
#' #plot the multiply imputed values for variable "HFF1"
#' plot_bar(imputation.list = imputed.data, var.fac = "HFF1",
#'   original.data = nhanes3_newborn)
#' }
#'
plot_bar <- function(imputation.list, var.fac, original.data, color.pal = NULL) {
  if(!identical(dim(imputation.list[[1]]),dim(original.data))){
    stop("The dimension of the imputed dataset needs to be the same as the dimension of the data specified in `original.data`.")
  }
  Names <- colnames(original.data)
  if (!any(Names == var.fac)) {
    stop("The variable ", var.fac, " does not exist in the original data.")
  }

  missing.vars <- Names[colSums(is.na(original.data)) != 0]
  if (!any(missing.vars == var.fac)) {
    stop(paste("The variable ", var.fac, " has no missing value."))
  }

  Types <- feature_type(imputation.list[[1]])
  if (Types[var.fac] == "numeric") {
    stop(paste("The variable", var.fac, " is a numeric variable. Please use plot_hist() or plot_box() instead."))
  }

  na.idx <- which(is.na(original.data[[var.fac]]))
  result.l <- lapply(imputation.list, function(dt) dt[[var.fac]][na.idx])
  N_imp <- length(result.l)
  M <- paste("m", 1:N_imp, sep = "")
  names(result.l) <- M
  result.df <- do.call(cbind.data.frame, result.l)
  result.df$obs <- 1:nrow(result.df)
  result.df <- tidyr::pivot_longer(data = result.df, cols = !obs, names_to = "m.set", values_to = var.fac)
  result.df$m.set <- factor(result.df$m.set, levels = M)

  # Observed
  observed <- original.data[[var.fac]][-na.idx]
  N.obs <- length(observed)
  observed.df <- data.frame(1:N.obs, rep("Observed", N.obs), observed)
  colnames(observed.df) <- c("obs", "m.set", var.fac)

  if (class(result.df[[var.fac]])[1] == class(observed.df[[var.fac]])[1]) {
    result.df <- rbind(result.df, observed.df)
    result.df$m.set <- factor(result.df$m.set, levels = c("Observed", M))
  } else {
    # original ordinal-> imputed integer
    observed.df[[var.fac]] <- fac2int(observed.df[[var.fac]])
    result.df <- rbind(result.df, observed.df)
    result.df$m.set <- factor(result.df$m.set, levels = c("Observed", M))
  }

  if (is.null(color.pal)) {
    color.pal <- c("gray40", scales::hue_pal()(N_imp))
  }

  if (Types[var.fac] == "integer") {
    warning("The variable ", var.fac, " is of integer type. It will be treated as a factor for bar plots.\n")
    result.df[[var.fac]] <- factor(result.df[[var.fac]])
  }

  ggplot(result.df, aes(x = .data[[var.fac]])) +
    geom_bar(stat = "count", alpha = 0.8, width = 0.8, aes(color = m.set, fill = m.set, y = ..prop.., group = m.set)) +
    scale_y_continuous(labels = scales::percent) +
    ylab("Proportion") +
    facet_grid(cols = vars(m.set)) +
    labs(title = "Bar plots", subtitle = paste(N_imp, " imputed sets for variable: ", var.fac)) +
    scale_fill_manual(values = color.pal) +
    scale_color_manual(values = color.pal) +
    guides(color = "none", fill = "none") +
    theme(
      strip.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold")
    )
}
