#' Scatter plots for two imputed numeric variables condition on a factor
#' @description Plot observed values with m sets of imputed values for two specified numeric variables and a factor using \pkg{ggplot2}.
#' @param imputation.list A list of \code{m} imputed datasets returned by the \code{mixgb} imputer
#' @param var.x A numeric variable on the x-axis
#' @param var.y A numeric variable on the y-axis
#' @param con.fac A conditional factor
#' @param original.data The original data with missing data
#' @param color.pal A vector of hex color codes for the observed and m sets of imputed values panels. The vector should be of length \code{m+1}. Default: \code{NULL} (use "gray40" for the observed panel, use ggplot2 default colors for other panels.)
#' @importFrom scales hue_pal
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot aes geom_point facet_grid labs scale_shape_manual scale_color_manual scale_fill_manual scale_y_continuous guides theme sec_axis guide_legend element_text element_rect
#' @return Scatter plots
#' @export
#' @examples
#' \dontrun{
#'
#' MIXGB <- Mixgb$new(data = nhanes3_newborn)
#' imputed.data <- MIXGB$impute(m = 5)
#'
#' plot_2num1fac(imputation.list = imputed.data, var.x = "BMPHEAD", var.y = "BMPRECUM",
#'  var.fac = "HFF1", original.data = nhanes3_newborn)
#' }
plot_2num1fac <- function(imputation.list, var.x, var.y, con.fac, original.data, color.pal = NULL) {
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


  Types <- feature_type(imputation.list[[1]])

  if (Types[var.x] != "numeric" & Types[var.x] != "integer") {
    stop(paste("Variable", var.x, "is a factor. Please use another plot function."))
  }


  if (Types[var.y] != "numeric" & Types[var.y] != "integer") {
    stop(paste("Variable", var.y, "is a factor. Please use another plot function."))
  }

  na.x <- which(is.na(original.data[[var.x]]))
  na.y <- which(is.na(original.data[[var.y]]))
  na.con <- which(is.na(original.data[[con.fac]]))
  # at least one missing
  na.combine <- unique(c(na.x, na.y, na.con))
  # number of all observed
  N.obs <- nrow(original.data) - length(na.combine)

  NA.condition <- rep("all.observed", nrow(original.data))
  # con.fac missing
  NA.condition[na.con] <- paste(con.fac, "missing", sep = ".")
  # con.fac observed
  NA.condition[setdiff(na.combine, na.con)] <- paste(con.fac, "observed", sep = ".")

  if (is.data.table(imputation.list[[1]])) {
    result.l <- lapply(imputation.list, function(dt) dt[na.combine, c(var.x, var.y, con.fac), with = FALSE])
    observed <- original.data[-na.combine, c(var.x, var.y, con.fac), with = FALSE]
  } else {
    result.l <- lapply(imputation.list, function(df) df[na.combine, c(var.x, var.y, con.fac)])
    observed <- original.data[-na.combine, c(var.x, var.y, con.fac)]
  }

  N.imp <- length(result.l)
  M <- paste("m", 1:N.imp, sep = "")
  names(result.l) <- M
  result <- do.call(rbind, result.l)
  result <- rbind(result, observed)
  result.df <- as.data.frame(result)
  result.df$m.set <- factor(c(rep(M, each = length(na.combine)), rep("Observed", N.obs)), levels = c("Observed", M))
  result.df$NA.condition <- c(rep(NA.condition[na.combine], N.imp), NA.condition[-na.combine])
  result.df$NA.condition <- factor(result.df$NA.condition, levels = c("all.observed", paste(con.fac, c("observed", "missing"), sep = ".")))


  if (is.null(color.pal)) {
    color.pal <- c("gray40", scales::hue_pal()(N.imp))
  }


  # levels: all.observed, confac.observed, confac.missing
  shapes <- c(23, 21, 4)

  ggplot(result.df, aes(x = .data[[var.x]], y = .data[[var.y]])) +
    geom_point(alpha = 0.6, aes(shape = NA.condition, color = m.set, fill = m.set)) +
    facet_grid(reformulate(termlabels = "m.set", response = con.fac)) +
    labs(title = "Scatter plots of two numeric variables conditional on one factor", subtitle = paste(N.imp, " imputed sets: ", var.y, "vs", var.x, "|", con.fac)) +
    scale_shape_manual(values = shapes) +
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
#' @param color.pal A vector of hex color codes for the observed and m sets of imputed values panels. The vector should be of length \code{m+1}. Default: NULL (use "gray40" for the observed panel, use ggplot2 default colors for other panels.)
#' @importFrom scales hue_pal
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot aes geom_point geom_jitter geom_boxplot facet_grid labs scale_shape_manual scale_color_manual scale_fill_manual scale_y_continuous guides theme sec_axis guide_legend element_text element_rect
#' @return Boxplots with overlaying data points
#' @export
#' @examples
#' \dontrun{
#'
#' MIXGB <- Mixgb$new(data = nhanes3_newborn)
#' imputed.data <- MIXGB$impute(m = 5)
#'
#' plot_1num2fac(
#'   imputation.list = imputed.data, var.fac = "HFF1", var.num = "BMPRECUM",
#'   var.con = "HSSEX", original.data = nhanes3_newborn)
#' }
plot_1num2fac <- function(imputation.list, var.fac, var.num, con.fac, original.data, color.pal = NULL) {
  Names <- colnames(original.data)

  if (!any(Names == var.fac)) {
    stop("The variable ", var.fac, " does not exist in the original data.")
  }

  if (!any(Names == var.num)) {
    stop("The variable ", var.num, " does not exist in the original data.")
  }

  if (!any(Names == con.fac)) {
    stop("The variable ", con.fac, " does not exist in the original data.")
  }

  missing.vars <- Names[colSums(is.na(original.data)) != 0]
  if (!any(missing.vars == var.fac) & !any(missing.vars == var.num)) {
    stop("There is no missing value in both `var.fac` and `var.num`.")
  }


  Types <- feature_type(imputation.list[[1]])

  if (Types[var.num] != "numeric" & Types[var.num] != "integer") {
    stop(paste("The variable", var.num, "is a factor. Please use another plot function."))
  }


  if (Types[var.fac] == "numeric") {
    stop(paste("The variable", var.fac, "is numeric variable. Please use another plot function."))
  }



  na.x <- which(is.na(original.data[[var.fac]]))
  na.y <- which(is.na(original.data[[var.num]]))
  na.con <- which(is.na(original.data[[con.fac]]))
  # at least one missing
  na.combine <- unique(c(na.x, na.y, na.con))
  # number of all observed
  N.obs <- nrow(original.data) - length(na.combine)

  NA.condition <- rep("all.observed", nrow(original.data))
  # con.fac missing
  NA.condition[na.con] <- paste(con.fac, "missing", sep = ".")
  # con.fac observed
  NA.condition[setdiff(na.combine, na.con)] <- paste(con.fac, "observed", sep = ".")


  if (is.data.table(imputation.list[[1]])) {
    result.l <- lapply(imputation.list, function(dt) dt[na.combine, c(var.fac, var.num, con.fac), with = FALSE])
    observed <- original.data[-na.combine, c(var.fac, var.num, con.fac), with = FALSE]
  } else {
    result.l <- lapply(imputation.list, function(df) df[na.combine, c(var.fac, var.num, con.fac)])
    observed <- original.data[-na.combine, c(var.fac, var.num, con.fac)]
  }

  N.imp <- length(result.l)
  M <- paste("m", 1:N.imp, sep = "")
  names(result.l) <- M
  result <- do.call(rbind, result.l)
  result <- rbind(result, observed)
  result.df <- as.data.frame(result)
  result.df$m.set <- factor(c(rep(M, each = length(na.combine)), rep("Observed", N.obs)), levels = c("Observed", M))
  result.df$NA.condition <- c(rep(NA.condition[na.combine], N.imp), NA.condition[-na.combine])
  result.df$NA.condition <- factor(result.df$NA.condition, levels = c("all.observed", paste(con.fac, c("observed", "missing"), sep = ".")))


  if (is.null(color.pal)) {
    color.pal <- c("gray40", scales::hue_pal()(N.imp))
  }


  shapes <- c(23, 21, 4)


  if (Types[var.fac] == "integer") {
    warning("Variable ", var.fac, " is of integer type. Will convert it to a factor for plotting.\n")
    result.df[[var.fac]] <- factor(result.df[[var.fac]])
  }

  if (Types[var.num] == "integer") {
    gp <- ggplot(result.df, aes(x = .data[[var.fac]], y = .data[[var.num]])) +
      geom_point(alpha = 0.6, aes(color = m.set, fill = m.set, shape = NA.condition))
  } else {
    gp <- ggplot(result.df, aes(x = .data[[var.fac]], y = .data[[var.num]])) +
      geom_jitter(alpha = 0.6, position = position_jitter(), aes(color = m.set, fill = m.set, shape = NA.condition))
  }

  gp +
    geom_boxplot(alpha = 0.5, aes(fill = m.set), outlier.shape = NA) +
    facet_grid(reformulate(termlabels = "m.set", response = con.fac)) +
    labs(title = "Boxplots for one numeric variable vs one factor conditional on another factor ", subtitle = paste(N.imp, " imputed sets: ", var.num, "vs", var.fac, "|", con.fac)) +
    scale_shape_manual(values = shapes) +
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
