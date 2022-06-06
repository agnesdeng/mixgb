#' Scatter plots for two imputed numeric variables
#' @description Plot observed values vesus m sets of imputed values for two specified numeric variables using \pkg{ggplot2}.
#' @param imputation.list A list of \code{m} imputed datasets returned by the \code{mixgb} imputer
#' @param var.x A numeric variable on the x-axis
#' @param var.y A numeric variable on the y-axis
#' @param original.data The original data with missing data
#' @param true.data The true data without missing values. In general, this is unknown. Only use for simulation studies.
#' @param color.pal A vector of hex color codes for the observed and m sets of imputed values panels. The vector should be of length \code{m+1}. Default: NULL (use "gray40" for the observed panel, use ggplot2 default colors for other panels.)
#' @param shape Whether to plot shapes for different types of missing values. By default, this is set to FALSE to speed up plotting. We only recommend using `shape = TRUE` for small datasets.
#' @importFrom scales hue_pal
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot aes vars geom_point facet_grid labs scale_shape_manual scale_color_manual scale_fill_manual guides theme guide_legend element_text element_rect
#' @return Scatter plots for two numeric/integer variable
#' @export
#' @examples
#' # obtain m multiply datasets
#' imputed.data <- mixgb(data = nhanes3, m = 2)
#'
#' # plot the multiply imputed values for variables "BMPRECUM" versus "BMPHEAD"
#' plot_2num(
#'   imputation.list = imputed.data, var.x = "BMPHEAD", var.y = "BMPRECUM",
#'   original.data = nhanes3
#' )
plot_2num <- function(imputation.list, var.x, var.y, original.data, true.data = NULL, color.pal = NULL, shape = FALSE) {
  Types <- feature_type(imputation.list[[1]])

  if (Types[var.x] != "numeric" & Types[var.x] != "integer") {
    stop(paste("The variable", var.x, "is a factor. Please use plot_1num1fac() or plot_2fac()."))
  }


  if (Types[var.y] != "numeric" & Types[var.y] != "integer") {
    stop(paste("The variable", var.y, "is a factor. Please use plot_1num1fac() or plot_2fac()."))
  }

  imp.sum <- summary2var(imputation.list = imputation.list, var.x = var.x, var.y = var.y, original.data = original.data, true.data = true.data, color.pal = color.pal, shape = shape)
  all.dt <- imp.sum$all.dt
  color.pal <- imp.sum$color.pal

  if (!shape) {
    gp <- ggplot(all.dt, aes(x = .data[[var.x]], y = .data[[var.y]])) +
      geom_point(alpha = 0.6, aes(shape = NULL, color = m.set, fill = m.set)) +
      facet_grid(cols = vars(m.set)) +
      labs(title = "Scatter plots of two numeric variables", subtitle = paste("Imputed sets: ", var.y, "vs", var.x))
  } else {
    if (is.null(true.data)) {
      # X:88, Y:89
      shapes <- c(23, 21, 88, 89)
    } else {
      # T: 84
      shapes <- c(23, 84, 21, 88, 89)
    }
    gp <- ggplot(all.dt, aes(x = .data[[var.x]], y = .data[[var.y]])) +
      geom_point(alpha = 0.6, aes(shape = NA.condition, color = m.set, fill = m.set)) +
      facet_grid(cols = vars(m.set)) +
      labs(title = "Scatter plots of two numeric variables", subtitle = paste("Imputed sets: ", var.y, "vs", var.x)) +
      scale_shape_manual(values = shapes, drop = FALSE)
  }

  gp +
    scale_color_manual(values = color.pal) +
    scale_fill_manual(values = color.pal) +
    guides(fill = "none", color = "none", shape = guide_legend(override.aes = list(size = 3))) +
    theme(
      strip.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(face = "bold"),
      legend.background = element_rect(size = 0.5),
      legend.title = element_text(color = "black", size = 12, face = "bold"),
      legend.text = element_text(color = "black", size = 10)
    )
}



#' Box plots with points for one numeric variable vs one factor (or integer) variable.
#' @description Plot observed values versus m sets of imputed values for one numeric variable vs one factor (or integer) variable using \pkg{ggplot2}.
#' @param imputation.list A list of \code{m} imputed datasets returned by the \code{mixgb} imputer
#' @param var.num A numeric variable
#' @param var.fac A factor variable
#' @param original.data The original data with missing data
#' @param true.data The true data without missing values. In general, this is unknown. Only use for simulation studies.
#' @param color.pal A vector of hex color codes for the observed and m sets of imputed values panels. The vector should be of length \code{m+1}. Default: NULL (use "gray40" for the observed panel, use ggplot2 default colors for other panels.)
#' @param shape Whether to plot shapes for different types of missing values. By default, this is set to FALSE to speed up plotting. We only recommend using `shape = TRUE` for small datasets.
#' @importFrom scales hue_pal
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot aes vars geom_point geom_jitter position_jitter geom_boxplot facet_grid labs scale_shape_manual scale_color_manual scale_fill_manual guides theme guide_legend element_text element_rect
#' @return Box plot with jittered data points for a numeric/integer variable; Bar plot for a categorical variable.
#' @export
#' @examples
#' # obtain m multiply datasets
#' imputed.data <- mixgb(data = nhanes3, m = 2)
#'
#' # plot the multiply imputed values for variables "BMPHEAD" versus "HSSEX"
#' plot_1num1fac(
#'   imputation.list = imputed.data, var.num = "BMPHEAD", var.fac = "HSSEX",
#'   original.data = nhanes3
#' )
plot_1num1fac <- function(imputation.list, var.num, var.fac, original.data, true.data = NULL, color.pal = NULL, shape = FALSE) {
  Types <- feature_type(imputation.list[[1]])

  if (Types[var.num] != "numeric" & Types[var.num] != "integer") {
    stop("Variable ", var.num, " is not a numeric or integer variable")
  }

  if (Types[var.fac] == "numeric") {
    stop("Variable ", var.fac, " is not a factor or integer variable")
  }

  imp.sum <- summary2var(imputation.list = imputation.list, var.x = var.num, var.y = var.fac, original.data = original.data, true.data = true.data, color.pal = color.pal, shape = shape)
  all.dt <- imp.sum$all.dt
  color.pal <- imp.sum$color.pal

  if (!shape) {
    if (Types[var.num] == "integer") {
      gp <- ggplot(all.dt, aes(x = .data[[var.fac]], y = .data[[var.num]])) +
        geom_point(alpha = 0.6, aes(color = m.set, fill = m.set))
    } else {
      gp <- ggplot(all.dt, aes(x = .data[[var.fac]], y = .data[[var.num]])) +
        geom_jitter(alpha = 0.6, position = position_jitter(), aes(color = m.set, fill = m.set))
    }
    gp <- gp +
      geom_boxplot(alpha = 0.5, aes(fill = m.set), outlier.shape = NA) +
      facet_grid(cols = vars(m.set)) +
      labs(title = "Boxplot with points for a numeric variable vs a factor", subtitle = paste("Imputed sets: ", var.num, "vs", var.fac))
  } else {
    # plot NA.condition with shapes
    if (is.null(true.data)) {
      # "N":78  "F":70
      shapes <- c(23, 21, 78, 70)
    } else {
      # T: 84
      shapes <- c(23, 84, 21, 78, 70)
    }

    if (Types[var.num] == "integer") {
      gp <- ggplot(all.dt, aes(x = .data[[var.fac]], y = .data[[var.num]])) +
        geom_point(alpha = 0.6, aes(color = m.set, fill = m.set, shape = NA.condition))
    } else {
      gp <- ggplot(all.dt, aes(x = .data[[var.fac]], y = .data[[var.num]])) +
        geom_jitter(alpha = 0.6, position = position_jitter(), aes(color = m.set, fill = m.set, shape = NA.condition))
    }
    gp <- gp +
      geom_boxplot(alpha = 0.5, aes(fill = m.set), outlier.shape = NA) +
      facet_grid(cols = vars(m.set)) +
      labs(title = "Boxplot with points for a numeric variable vs a factor", subtitle = paste("Imputed sets: ", var.num, "vs", var.fac)) +
      scale_shape_manual(values = shapes, drop = FALSE)
  }
  gp + scale_color_manual(values = color.pal) +
    scale_fill_manual(values = color.pal) +
    guides(fill = "none", color = "none", shape = guide_legend(override.aes = list(size = 3))) +
    theme(
      strip.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(face = "bold"),
      legend.background = element_rect(size = 0.5),
      legend.title = element_text(color = "black", size = 12, face = "bold"),
      legend.text = element_text(color = "black", size = 10)
    )
}


#' Bar plots for two imputed factor variables
#' @description Plot observed values with m sets of imputed values for two specified numeric variables using \pkg{ggplot2}.
#' @param imputation.list A list of \code{m} imputed datasets returned by the \code{mixgb} imputer
#' @param var.fac1 A factor variable
#' @param var.fac2 A factor variable
#' @param original.data The original data with missing data
#' @param true.data The true data without missing values. In general, this is unknown. Only use for simulation studies.
#' @param color.pal A vector of hex color codes for the observed and m sets of imputed values panels. The vector should be of length \code{m+1}. Default: NULL (use "gray40" for the observed panel, use ggplot2 default colors for other panels.)
#' @importFrom scales hue_pal
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot aes geom_bar facet_grid labs scale_color_manual scale_fill_manual scale_y_continuous guides theme sec_axis element_text
#' @return Scatter plots for two numeric/integer variable
#' @export
#' @examples
#' \donttest{
#' #create some extra missing values in factor variables "HSSEX" and "DMARETHN"
#' nhanes3_NA<-createNA(nhanes3, var.names = c("HSSEX","DMARETHN"), p = 0.1)
#'
#' # obtain m multiply datasets
#' imputed.data <- mixgb(data = nhanes3_NA, m = 2)
#'
#' # plot the multiply imputed values for variables "HSSEX" versus "DMARETHN"
#' plot_2fac(
#'   imputation.list = imputed.data, var.fac1 = "DMARETHN", var.fac2 = "HSSEX",
#'   original.data = nhanes3_NA
#' )
#' }
plot_2fac <- function(imputation.list, var.fac1, var.fac2, original.data, true.data = NULL, color.pal = NULL) {
  Types <- feature_type(imputation.list[[1]])

  if (Types[var.fac1] == "numeric") {
    stop("Variable ", var.fac1, " is not a factor or an integer variable")
  }


  if (Types[var.fac2] == "numeric") {
    stop("Variable ", var.fac2, " is not a factor or an integer variable")
  }

  imp.sum <- summary2var(imputation.list = imputation.list, var.x = var.fac1, var.y = var.fac2, original.data = original.data, true.data = true.data, color.pal = color.pal, shape = FALSE)
  all.dt <- imp.sum$all.dt
  color.pal <- imp.sum$color.pal



  propt <- prop.table(table(all.dt[[var.fac1]], all.dt[[var.fac2]], all.dt[["m.set"]]), margin = 3)
  prop.df <- as.data.frame(propt)


  ggplot(prop.df, aes(x = Var1, y = Freq)) +
    geom_bar(stat = "identity", position = "dodge2", aes(fill = Var3)) +
    facet_grid(Var2 ~ Var3) +
    labs(x = var.fac1, y = "Proportion", title = "Barplots of two factor variables", subtitle = paste("Imputed sets: ", var.fac2, "vs", var.fac1)) +
    scale_fill_manual(values = color.pal) +
    scale_y_continuous(sec.axis = sec_axis(~., name = var.fac2, breaks = NULL, labels = NULL)) +
    guides(color = "none", fill = "none") +
    theme(
      strip.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(face = "bold")
    )
}


# extract results for plotting two variables
summary2var <- function(imputation.list, var.x, var.y, original.data, true.data, color.pal, shape) {
  if (!identical(dim(imputation.list[[1]]), dim(original.data))) {
    stop("The dimension of the imputed dataset needs to be the same as the dimension of the data specified in `original.data`.")
  }
  Names <- colnames(original.data)

  if (!any(Names == var.x)) {
    stop("The variable ", var.x, " does not exist in the original data.")
  }

  if (!any(Names == var.y)) {
    stop("The variable ", var.y, " does not exist in the original data.")
  }

  missing.vars <- Names[colSums(is.na(original.data)) != 0]
  if (!any(missing.vars == var.x) & !any(missing.vars == var.y)) {
    stop("There is no missing value in both `var.x` and `var.y`.")
  }

  if (!is.null(true.data)){
    if(any(is.na(true.data[[var.x]]))){
      stop(paste("The variable ", var.x, " in `true.data` contains missing values."))
    }

    if(any(is.na(true.data[[var.y]]))){
      stop(paste("The variable ", var.y, " in `true.data` contains missing values."))
    }
  }


  na.x <- which(is.na(original.data[[var.x]]))
  na.y <- which(is.na(original.data[[var.y]]))
  # at least one missing (na.union: na.both+na.onlyx+na.onlyy)
  na.union <- union(na.x, na.y)
  N.mis <- length(na.union)
  N.obs <- nrow(original.data) - N.mis



  if (is.data.table(imputation.list[[1]])) {
    imp.l <- lapply(imputation.list, function(dt) dt[na.union, c(var.x, var.y), with = FALSE])
  } else {
    imp.l <- lapply(imputation.list, function(df) df[na.union, c(var.x, var.y)])
  }

  N.imp <- length(imp.l)
  M <- paste("m", 1:N.imp, sep = "")

  imp.dt <- rbindlist(imp.l)


  if (is.data.table(original.data)) {
    observed <- original.data[-na.union, c(var.x, var.y), with = FALSE]
  } else {
    observed <- original.data[-na.union, c(var.x, var.y)]
  }


  if (class(imp.dt[[var.x]])[1] != class(original.data[[var.x]])[1]) {
    # Special case: original ordinal-> imputed as integer
    observed[[var.x]] <- fac2int(observed[[var.x]])
  }

  if (class(imp.dt[[var.y]])[1] != class(original.data[[var.y]])[1]) {
    # Special case: original ordinal-> imputed as integer
    observed[[var.y]] <- fac2int(observed[[var.y]])
  }


  if (!is.null(true.data)) {
    # with true.data
    if (is.data.table(true.data)) {
      true <- true.data[na.union, c(var.x, var.y), with = FALSE]
    } else {
      true <- true.data[na.union, c(var.x, var.y)]
    }

    all.dt <- rbindlist(list(observed, true, imp.dt))
    all.dt[, m.set := factor(c(rep("Observed", N.obs), rep("MaskedTrue", N.mis), rep(M, each = N.mis)), levels = c("Observed", "MaskedTrue", M))]
    if (is.null(color.pal)) {
      color.pal <- c("gray40", "gray20", scales::hue_pal()(N.imp))
    }

    # imp.df$NA.condition <- c(rep(NA.condition[na.union], N.imp), NA.condition[-na.union], rep("masked.observed",N.mis))
    # imp.df$NA.condition <- factor(imp.df$NA.condition, levels = c("both.observed", "masked.observed", "both.missing", X, Y))
  } else {
    # without true.data
    all.dt <- rbindlist(list(observed, imp.dt))
    all.dt[, m.set := factor(c(rep("Observed", N.obs), rep(M, each = N.mis)), levels = c("Observed", M))]
    if (is.null(color.pal)) {
      color.pal <- c("gray40", scales::hue_pal()(N.imp))
    }

    # imp.df$NA.condition <- factor(imp.df$NA.condition, levels = c("both.observed", "both.missing", X, Y))
  }



  if (shape) {
    # use shapes
    # both missing
    na.both <- intersect(na.x, na.y)
    # missing in x but not y
    na.onlyx <- setdiff(na.x, na.y)
    # missing in y but not x
    na.onlyy <- setdiff(na.y, na.x)

    NA.condition <- rep("both.observed", nrow(original.data))
    # both missing
    NA.condition[na.both] <- "both.missing"
    # only var.x missing
    X <- paste(var.x, "missing", sep = ".")
    NA.condition[na.onlyx] <- X
    # only var.y missing
    Y <- paste(var.y, "missing", sep = ".")
    NA.condition[na.onlyy] <- Y
    if (!is.null(true.data)) {
      na.condition <- factor(c(NA.condition[-na.union], rep("masked.observed", N.mis), rep(NA.condition[na.union], N.imp)),
        levels = c("both.observed", "masked.observed", "both.missing", X, Y)
      )
    } else {
      na.condition <- factor(c(NA.condition[-na.union], rep(NA.condition[na.union], N.imp)),
        levels = c("both.observed", "both.missing", X, Y)
      )
    }
    all.dt[, NA.condition := na.condition]
  }

  return(list("all.dt" = all.dt, "color.pal" = color.pal))
}
