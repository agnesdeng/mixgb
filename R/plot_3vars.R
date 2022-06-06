#' Scatter plots for two imputed numeric variables condition on a factor
#' @description Plot observed values with m sets of imputed values for two specified numeric variables and a factor using \pkg{ggplot2}.
#' @param imputation.list A list of \code{m} imputed datasets returned by the \code{mixgb} imputer
#' @param var.x A numeric variable on the x-axis
#' @param var.y A numeric variable on the y-axis
#' @param con.fac A conditional factor
#' @param original.data The original data with missing data
#' @param true.data The true data without missing values. In general, this is unknown. Only use for simulation studies.
#' @param color.pal A vector of hex color codes for the observed and m sets of imputed values panels. The vector should be of length \code{m+1}. Default: NULL (use "gray40" for the observed panel, use ggplot2 default colors for other panels.)
#' @param shape Whether to plot shapes for different types of missing values. By default, this is set to FALSE to speed up plotting. We only recommend using `shape = TRUE` for small datasets.
#' @importFrom scales hue_pal
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot aes geom_point facet_grid labs scale_shape_manual scale_color_manual scale_fill_manual scale_y_continuous guides theme sec_axis guide_legend element_text element_rect
#' @return Scatter plots
#' @export
#' @examples
#' # obtain m multiply datasets
#' imputed.data <- mixgb(data = nhanes3, m = 2)
#'
#' # plot the multiply imputed values for variables "BMPRECUM" versus "BMPHEAD" conditional on "HSSEX"
#' plot_2num1fac(
#'   imputation.list = imputed.data, var.x = "BMPHEAD", var.y = "BMPRECUM",
#'   con.fac = "HSSEX", original.data = nhanes3
#' )
plot_2num1fac <- function(imputation.list, var.x, var.y, con.fac, original.data, true.data = NULL, color.pal = NULL, shape = FALSE) {
  Types <- feature_type(imputation.list[[1]])

  if (Types[var.x] != "numeric" & Types[var.x] != "integer") {
    stop(paste("Variable", var.x, "is a factor. Please use another plot function."))
  }


  if (Types[var.y] != "numeric" & Types[var.y] != "integer") {
    stop(paste("Variable", var.y, "is a factor. Please use another plot function."))
  }

  imp.sum <- summary3var(imputation.list = imputation.list, var.x = var.x, var.y = var.y, con.fac = con.fac, original.data = original.data, true.data = true.data, color.pal = color.pal, shape = shape)
  all.dt <- imp.sum$all.dt
  color.pal <- imp.sum$color.pal


  if (!shape) {
    gp <- ggplot(all.dt, aes(x = .data[[var.x]], y = .data[[var.y]])) +
      geom_point(alpha = 0.6, aes(color = m.set, fill = m.set)) +
      facet_grid(reformulate(termlabels = "m.set", response = con.fac)) +
      labs(title = "Scatter plots of two numeric variables conditional on one factor", subtitle = paste("Imputed sets: ", var.y, "vs", var.x, "|", con.fac))
  } else {
    if (is.null(true.data)) {
      # levels: all.observed, confac.observed, confac.missing
      shapes <- c(23, 21, 4)
    } else {
      # T: 84
      # levels: all.observed, masked.observed, confac.observed, confac.missing
      shapes <- c(23, 84, 21, 4)
    }
    gp <- ggplot(all.dt, aes(x = .data[[var.x]], y = .data[[var.y]])) +
      geom_point(alpha = 0.6, aes(shape = NA.condition, color = m.set, fill = m.set)) +
      facet_grid(reformulate(termlabels = "m.set", response = con.fac)) +
      labs(title = "Scatter plots of two numeric variables conditional on one factor", subtitle = paste("Imputed sets: ", var.y, "vs", var.x, "|", con.fac)) +
      scale_shape_manual(values = shapes, drop = FALSE)
  }

  gp +
    scale_color_manual(values = color.pal) +
    scale_fill_manual(values = color.pal) +
    scale_y_continuous(sec.axis = sec_axis(~., name = con.fac, breaks = NULL, labels = NULL)) +
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


#' Box plots with overlaying data points for a numeric variable vs a factor condition on another factor
#' @description Plot observed values versus m sets of imputed values for one specified numeric variable and two factors using \pkg{ggplot2}.
#' @param imputation.list A list of \code{m} imputed datasets returned by the \code{mixgb} imputer
#' @param var.fac A factor variable on the x-axis
#' @param var.num A numeric variable on the y-axis
#' @param con.fac A conditional factor
#' @param original.data The original data with missing data
#' @param true.data The true data without missing values. In general, this is unknown. Only use for simulation studies.
#' @param color.pal A vector of hex color codes for the observed and m sets of imputed values panels. The vector should be of length \code{m+1}. Default: NULL (use "gray40" for the observed panel, use ggplot2 default colors for other panels.)
#' @param shape Whether to plot shapes for different types of missing values. By default, this is set to FALSE to speed up plotting. We only recommend using `shape = TRUE` for small datasets.
#' @importFrom scales hue_pal
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot aes geom_point geom_jitter geom_boxplot facet_grid labs scale_shape_manual scale_color_manual scale_fill_manual scale_y_continuous guides theme sec_axis guide_legend element_text element_rect
#' @return Boxplots with overlaying data points
#' @export
#' @examples
#' \donttest{
#' # create some extra missing values in factor variables "HSSEX" and "DMARETHN"
#' nhanes3_NA <- createNA(nhanes3, var.names = c("HSSEX", "DMARETHN"), p = 0.1)
#' # obtain m multiply datasets
#' imputed.data <- mixgb(data = nhanes3_NA, m = 5)
#'
#' # plot the multiply imputed values for variables "BMPRECUM" versus "HSSEX" conditional on "DMARETHN"
#' plot_1num2fac(
#'   imputation.list = imputed.data, var.fac = "HSSEX", var.num = "BMPRECUM",
#'   con.fac = "DMARETHN", original.data = nhanes3_NA
#' )
#' }
plot_1num2fac <- function(imputation.list, var.fac, var.num, con.fac, original.data, true.data = NULL, color.pal = NULL, shape = FALSE) {
  Types <- feature_type(imputation.list[[1]])

  if (Types[var.num] != "numeric" & Types[var.num] != "integer") {
    stop(paste("The variable", var.num, "is a factor. Please use another plot function."))
  }


  if (Types[var.fac] == "numeric") {
    stop(paste("The variable", var.fac, "is numeric variable. Please use another plot function."))
  }

  imp.sum <- summary3var(imputation.list = imputation.list, var.x = var.fac, var.y = var.num, con.fac = con.fac, original.data = original.data, true.data = true.data, color.pal = color.pal, shape = shape)
  all.dt <- imp.sum$all.dt
  color.pal <- imp.sum$color.pal

  if (Types[var.fac] == "integer") {
    warning("Variable ", var.fac, " is of integer type. Will convert it to a factor for plotting.\n")
    all.dt[[var.fac]] <- factor(all.dt[[var.fac]])
  }


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
      facet_grid(reformulate(termlabels = "m.set", response = con.fac)) +
      labs(title = "Boxplots for one numeric variable vs one factor conditional on another factor ", subtitle = paste("Imputed sets: ", var.num, "vs", var.fac, "|", con.fac))
  } else {
    # plot NA.condition with shapes
    if (is.null(true.data)) {
      # levels: all.observed, confac.observed, confac.missing
      shapes <- c(23, 21, 4)
    } else {
      # T: 84
      # levels: all.observed, masked.observed, confac.observed, confac.missing
      shapes <- c(23, 84, 21, 4)
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
      facet_grid(reformulate(termlabels = "m.set", response = con.fac)) +
      labs(title = "Boxplots for one numeric variable vs one factor conditional on another factor ", subtitle = paste("Imputed sets: ", var.num, "vs", var.fac, "|", con.fac)) +
      scale_shape_manual(values = shapes, drop = FALSE)
  }

  gp +
    scale_color_manual(values = color.pal) +
    scale_fill_manual(values = color.pal) +
    scale_y_continuous(sec.axis = sec_axis(~., name = con.fac, breaks = NULL, labels = NULL)) +
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


# extract results for plotting three variables
summary3var <- function(imputation.list, var.x, var.y, con.fac, original.data, true.data, color.pal, shape) {
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

  if (!any(Names == con.fac)) {
    stop("The variable ", con.fac, " does not exist in the original data.")
  }

  missing.vars <- Names[colSums(is.na(original.data)) != 0]
  if (!any(missing.vars == var.x) & !any(missing.vars == var.y)) {
    stop("There is no missing value in both `var.name1` and `var.name2`.")
  }

  if (!is.null(true.data)) {
    if (any(is.na(true.data[[var.x]]))) {
      stop(paste("The variable ", var.x, " in `true.data` contains missing values."))
    }

    if (any(is.na(true.data[[var.y]]))) {
      stop(paste("The variable ", var.y, " in `true.data` contains missing values."))
    }

    if (any(is.na(true.data[[con.fac]]))) {
      stop(paste("The variable ", con.fac, " in `true.data` contains missing values."))
    }
  }


  na.x <- which(is.na(original.data[[var.x]]))
  na.y <- which(is.na(original.data[[var.y]]))
  na.con <- which(is.na(original.data[[con.fac]]))
  # at least one missing
  na.combine <- unique(c(na.x, na.y, na.con))
  N.mis <- length(na.combine)
  # number of all observed
  N.obs <- nrow(original.data) - length(na.combine)


  if (is.data.table(imputation.list[[1]])) {
    imp.l <- lapply(imputation.list, function(dt) dt[na.combine, c(var.x, var.y, con.fac), with = FALSE])
  } else {
    imp.l <- lapply(imputation.list, function(df) df[na.combine, c(var.x, var.y, con.fac)])
  }

  N.imp <- length(imp.l)
  M <- paste("m", 1:N.imp, sep = "")

  imp.dt <- rbindlist(imp.l)

  if (is.data.table(original.data)) {
    observed <- original.data[-na.combine, c(var.x, var.y, con.fac), with = FALSE]
  } else {
    observed <- original.data[-na.combine, c(var.x, var.y, con.fac)]
  }

  if (class(imp.dt[[var.x]])[1] != class(original.data[[var.x]])[1]) {
    # Special case: original ordinal-> imputed as integer
    observed[[var.x]] <- fac2int(observed[[var.x]])
  }

  if (class(imp.dt[[var.y]])[1] != class(original.data[[var.y]])[1]) {
    # Special case: original ordinal-> imputed as integer
    observed[[var.y]] <- fac2int(observed[[var.y]])
  }

  if (class(imp.dt[[con.fac]])[1] != class(original.data[[con.fac]])[1]) {
    # Special case: original ordinal-> imputed as integer
    observed[[con.fac]] <- fac2int(observed[[con.fac]])
  }
  ###
  if (!is.null(true.data)) {
    # with true.data
    if (is.data.table(true.data)) {
      true <- true.data[na.combine, c(var.x, var.y, con.fac), with = FALSE]
    } else {
      true <- true.data[na.combine, c(var.x, var.y, con.fac)]
    }

    all.dt <- rbindlist(list(observed, true, imp.dt))
    all.dt[, m.set := factor(c(rep("Observed", N.obs), rep("MaskedTrue", N.mis), rep(M, each = N.mis)), levels = c("Observed", "MaskedTrue", M))]
    if (is.null(color.pal)) {
      color.pal <- c("gray40", "gray20", scales::hue_pal()(N.imp))
    }
  } else {
    # without true.data
    all.dt <- rbindlist(list(observed, imp.dt))
    all.dt[, m.set := factor(c(rep("Observed", N.obs), rep(M, each = N.mis)), levels = c("Observed", M))]
    if (is.null(color.pal)) {
      color.pal <- c("gray40", scales::hue_pal()(N.imp))
    }
  }


  if (shape) {
    NA.condition <- rep("all.observed", nrow(original.data))
    # con.fac missing
    NA.condition[na.con] <- paste(con.fac, "missing", sep = ".")
    # con.fac observed
    NA.condition[setdiff(na.combine, na.con)] <- paste(con.fac, "observed", sep = ".")

    # use shape
    if (!is.null(true.data)) {
      na.condition <- factor(c(NA.condition[-na.combine], rep("masked.observed", N.mis), rep(NA.condition[na.combine], N.imp)),
        levels = c("all.observed", "masked.observed", paste(con.fac, c("observed", "missing"), sep = "."))
      )
    } else {
      na.condition <- factor(c(NA.condition[-na.combine], rep(NA.condition[na.combine], N.imp)),
        levels = c("all.observed", paste(con.fac, c("observed", "missing"), sep = "."))
      )
    }
    all.dt[, NA.condition := na.condition]
  }

  return(list("all.dt" = all.dt, "color.pal" = color.pal))
}
