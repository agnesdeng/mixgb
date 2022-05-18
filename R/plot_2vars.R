#' Scatter plots for two imputed numeric variables
#' @description Plot observed values vesus m sets of imputed values for two specified numeric variables using \pkg{ggplot2}.
#' @param imputation.list A list of \code{m} imputed datasets returned by the \code{mixgb} imputer
#' @param var.x A numeric variable on the x-axis
#' @param var.y A numeric variable on the y-axis
#' @param original.data The original data with missing data
#' @param color.pal A vector of hex color codes for the observed and m sets of imputed values panels. The vector should be of length \code{m+1}. Default: NULL (use "gray40" for the observed panel, use ggplot2 default colors for other panels.)
#' @importFrom scales hue_pal
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot aes vars geom_point facet_grid labs scale_shape_manual scale_color_manual scale_fill_manual guides theme guide_legend element_text element_rect
#' @return Scatter plots for two numeric/integer variable
#' @export
#' @examples
#' \dontrun{
#'
#' MIXGB <- Mixgb$new(data = nhanes3_newborn)
#' imputed.data <- MIXGB$impute(m = 5)
#'
#' plot_2num(imputation.list = imputed.data, var.x = "BMPHEAD", var.y = "BMPRECUM",
#'   original.data = nhanes3_newborn)
#' }
plot_2num <- function(imputation.list, var.x, var.y, original.data, color.pal = NULL) {
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


  Types <- feature_type(imputation.list[[1]])

  if (Types[var.x] != "numeric" & Types[var.x] != "integer") {
    stop(paste("The variable", var.x, "is a factor. Please use plot_1num1fac() or plot_2fac()."))
  }


  if (Types[var.y] != "numeric" & Types[var.y] != "integer") {
    stop(paste("The variable", var.y, "is a factor. Please use plot_1num1fac() or plot_2fac()."))
  }



  na.x <- which(is.na(original.data[[var.x]]))
  na.y <- which(is.na(original.data[[var.y]]))
  # at least one missing (na.union: na.both+na.onlyx+na.onlyy)
  na.union <- union(na.x, na.y)
  N.obs <- nrow(original.data) - length(na.union)
  # both missing
  na.both <- intersect(na.x, na.y)
  # missing in x but not y
  na.onlyx <- setdiff(na.x, na.y)
  # missing in y but not x
  na.onlyy <- setdiff(na.y, na.x)

  if (is.data.table(imputation.list[[1]])) {
    result.l <- lapply(imputation.list, function(dt) dt[na.union, c(var.x, var.y), with = FALSE])
    observed <- original.data[-na.union, c(var.x, var.y), with = FALSE]
  } else {
    result.l <- lapply(imputation.list, function(df) df[na.union, c(var.x, var.y)])
    observed <- original.data[-na.union, c(var.x, var.y)]
  }

  N.imp <- length(result.l)
  M <- paste("m", 1:N.imp, sep = "")
  names(result.l) <- M
  result <- do.call(rbind, result.l)
  result <- rbind(result, observed)
  result.df <- as.data.frame(result)
  result.df$m.set <- factor(c(rep(M, each = length(na.union)), rep("Observed", N.obs)), levels = c("Observed", M))

  NA.condition <- rep("both.observed", nrow(original.data))
  # both missing
  NA.condition[na.both] <- "both.missing"
  # only var.num missing
  NA.condition[na.onlyx] <- "x.missing"
  # only var.num missing
  NA.condition[na.onlyy] <- "y.missing"

  result.df$NA.condition <- c(rep(NA.condition[na.union], N.imp), NA.condition[-na.union])
  result.df$NA.condition <- factor(result.df$NA.condition, levels = c("both.observed", "both.missing", "x.missing", "y.missing"))

  if (is.null(color.pal)) {
    color.pal <- c("gray40", scales::hue_pal()(N.imp))
  }


  # X:88, Y:89
  shapes <- c(23, 21, 88, 89)

  ggplot(result.df, aes(x = .data[[var.x]], y = .data[[var.y]])) +
    geom_point(alpha = 0.6, aes(shape = NA.condition, color = m.set, fill = m.set)) +
    facet_grid(cols = vars(m.set)) +
    labs(title = "Scatter plots of two numeric variables", subtitle = paste(N.imp, " imputed sets: ", var.y, "vs", var.x)) +
    scale_shape_manual(values = shapes) +
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
#' @param color.pal A vector of hex color codes for the observed and m sets of imputed values panels. The vector should be of length \code{m+1}. Default: NULL (use "gray40" for the observed panel, use ggplot2 default colors for other panels.)
#' @importFrom scales hue_pal
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot aes vars geom_point geom_jitter position_jitter geom_boxplot facet_grid labs scale_shape_manual scale_color_manual scale_fill_manual guides theme guide_legend element_text element_rect
#' @return Box plot with jittered data points for a numeric/integer variable; Bar plot for a categorical variable.
#' @export
#' @examples
#' \dontrun{
#'
#' MIXGB <- Mixgb$new(data = nhanes3_newborn)
#' imputed.data <- MIXGB$impute(m = 5)
#'
#' plot_1num1fac(imputation.list = imputed.data, var.num = "BMPHEAD", var.fac = "HFF1",
#'   original.data = nhanes3_newborn)
#' }
plot_1num1fac <- function(imputation.list, var.num, var.fac, original.data, color.pal = NULL) {
  Names <- colnames(original.data)

  if (!any(Names == var.num)) {
    stop("The variable ", var.num, " does not exist in the original data.")
  }

  if (!any(Names == var.fac)) {
    stop("The variable ", var.fac, " does not exist in the original data.")
  }

  missing.vars <- Names[colSums(is.na(original.data)) != 0]
  if (!any(missing.vars == var.num) & !any(missing.vars == var.fac)) {
    stop("There is no missing value in both `var.num` and `var.fac`.")
  }


  Types <- feature_type(imputation.list[[1]])

  if (Types[var.num] != "numeric" & Types[var.num] != "integer") {
    stop("Variable ", var.num, " is not a numeric or integer variable")
  }

  if (Types[var.fac] == "numeric") {
    stop("Variable ", var.fac, " is not a factor or integer variable")
  }


  na.num <- which(is.na(original.data[[var.num]]))
  na.fac <- which(is.na(original.data[[var.fac]]))

  # at least one missing (na.union=na.both+na.onlyx+na.onlyy)
  na.union <- union(na.num, na.fac)
  N.obs <- nrow(original.data) - length(na.union)
  # both missing
  na.both <- intersect(na.num, na.fac)
  # missing in x but not y
  na.onlynum <- setdiff(na.num, na.fac)
  # missing in y but not x
  na.onlyfac <- setdiff(na.fac, na.num)

  if (is.data.table(imputation.list[[1]])) {
    result.l <- lapply(imputation.list, function(dt) dt[na.union, c(var.num, var.fac), with = FALSE])
    observed <- original.data[-na.union, c(var.num, var.fac), with = FALSE]
  } else {
    result.l <- lapply(imputation.list, function(df) df[na.union, c(var.num, var.fac)])
    observed <- original.data[-na.union, c(var.num, var.fac)]
  }

  N.imp <- length(result.l)
  M <- paste("m", 1:N.imp, sep = "")
  names(result.l) <- M
  result <- do.call(rbind, result.l)
  result <- rbind(result, observed)
  result.df <- as.data.frame(result)
  result.df$m.set <- factor(c(rep(M, each = length(na.union)), rep("Observed", N.obs)), levels = c("Observed", M))


  NA.condition <- rep("both.observed", nrow(original.data))
  # both missing
  NA.condition[na.both] <- "both.missing"
  # only var.num missing
  NA.condition[na.onlynum] <- "num.missing"
  # only var.num missing
  NA.condition[na.onlyfac] <- "fac.missing"



  if (is.null(color.pal)) {
    color.pal <- c("gray40", scales::hue_pal()(N.imp))
  }

  if (Types[var.fac] == "integer") {
    warning("Variable ", var.fac, " is of integer type. Will convert it to a factor for plotting.\n")
    result.df[[var.fac]] <- factor(result.df[[var.fac]])
  }

  result.df$NA.condition <- c(rep(NA.condition[na.union], N.imp), NA.condition[-na.union])
  result.df$NA.condition <- factor(result.df$NA.condition, levels = c("both.observed", "both.missing", "num.missing", "fac.missing"))

  # "N":78  "F":70
  shapes <- c(23, 21, 78, 70)

  if (Types[var.num] == "integer") {
    gp <- ggplot(result.df, aes(x = .data[[var.fac]], y = .data[[var.num]])) +
      geom_point(alpha = 0.6, aes(color = m.set, fill = m.set, shape = NA.condition))
  } else {
    gp <- ggplot(result.df, aes(x = .data[[var.fac]], y = .data[[var.num]])) +
      geom_jitter(alpha = 0.6, position = position_jitter(), aes(color = m.set, fill = m.set, shape = NA.condition))
  }
  gp +
    geom_boxplot(alpha = 0.5, aes(fill = m.set), outlier.shape = NA) +
    facet_grid(cols = vars(m.set)) +
    labs(title = "Boxplot with points for a numeric variable vs a factor", subtitle = paste(N.imp, " imputed sets: ", var.num, "vs", var.fac)) +
    scale_shape_manual(values = shapes) +
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


#' Bar plots for two imputed factor variables
#' @description Plot observed values with m sets of imputed values for two specified numeric variables using \pkg{ggplot2}.
#' @param imputation.list A list of \code{m} imputed datasets returned by the \code{mixgb} imputer
#' @param var.fac1 A factor variable
#' @param var.fac2 A factor variable
#' @param original.data The original data with missing data
#' @param color.pal A vector of hex color codes for the observed and m sets of imputed values panels. The vector should be of length \code{m+1}. Default: NULL (use "gray40" for the observed panel, use ggplot2 default colors for other panels.)
#' @importFrom scales hue_pal
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot aes geom_bar facet_grid labs scale_color_manual scale_fill_manual scale_y_continuous guides theme sec_axis element_text
#' @return Scatter plots for two numeric/integer variable
#' @export
#' @examples
#' \dontrun{
#'
#' MIXGB <- Mixgb$new(data = nhanes3_newborn)
#' imputed.data <- MIXGB$impute(m = 5)
#'
#' plot_2fac(imputation.list = imputed.data, var.fac1 = "HFF1", var.fac2 = "DMARETHN",
#'   original.data = nhanes3_newborn)
#' }
plot_2fac <- function(imputation.list, var.fac1, var.fac2, original.data, color.pal = NULL) {
  Names <- colnames(original.data)
  if (!any(Names == var.fac1)) {
    stop("The variable ", var.fac1, " does not exist in the original data.")
  }

  if (!any(Names == var.fac2)) {
    stop("The variable ", var.fac2, " does not exist in the original data.")
  }
  missing.vars <- Names[colSums(is.na(original.data)) != 0]

  if (!any(missing.vars == var.fac1) & !any(missing.vars == var.fac2)) {
    stop("There is no missing value in both `var.fac1` and `var.fac2`.")
  }


  Types <- feature_type(imputation.list[[1]])

  if (Types[var.fac1] == "numeric") {
    stop("Variable ", var.fac1, " is not a factor or an integer variable")
  }


  if (Types[var.fac2] == "numeric") {
    stop("Variable ", var.fac2, " is not a factor or an integer variable")
  }


  na.x <- which(is.na(original.data[[var.fac1]]))
  na.y <- which(is.na(original.data[[var.fac2]]))

  # at least one missing (na.union=na.both+na.onlyx+na.onlyy)
  na.union <- union(na.x, na.y)
  N.obs <- nrow(original.data) - length(na.union)

  if (is.data.table(imputation.list[[1]])) {
    result.l <- lapply(imputation.list, function(dt) dt[na.union, c(var.fac1, var.fac2), with = FALSE])
    observed <- original.data[-na.union, c(var.fac1, var.fac2), with = FALSE]
  } else {
    result.l <- lapply(imputation.list, function(df) df[na.union, c(var.fac1, var.fac2)])
    observed <- original.data[-na.union, c(var.fac1, var.fac2)]
  }

  N.imp <- length(result.l)
  M <- paste("m", 1:N.imp, sep = "")
  names(result.l) <- M
  result <- do.call(rbind, result.l)
  result <- rbind(result, observed)
  result.df <- as.data.frame(result)
  result.df$m.set <- factor(c(rep(M, each = length(na.union)), rep("Observed", N.obs)), levels = c("Observed", M))


  if (is.null(color.pal)) {
    color.pal <- c("gray40", scales::hue_pal()(N.imp))
  }


  propt <- prop.table(table(result.df[[var.fac1]], result.df[[var.fac2]], result.df$m.set), margin = 3)
  prop.df <- as.data.frame(propt)


  ggplot(prop.df, aes(x = Var1, y = Freq)) +
    geom_bar(stat = "identity", position = "dodge2", aes(fill = Var3)) +
    facet_grid(Var2 ~ Var3) +
    labs(x = var.fac1, y = "Proportion", title = "Barplots of two factor variables", subtitle = paste(N.imp, " imputed sets: ", var.fac2, "vs", var.fac1)) +
    scale_fill_manual(values = color.pal) +
    scale_y_continuous(sec.axis = sec_axis(~., name = var.fac2, breaks = NULL, labels = NULL)) +
    guides(color = "none", fill = "none") +
    theme(
      strip.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(face = "bold")
    )
}
