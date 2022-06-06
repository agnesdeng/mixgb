# Checking save.vars
save_vars <- function(save.vars, origin.names, missing.vars) {
  # return extra.vars, the names of the extra variables need to be saved models for.

  # library(mice)
  # data(nhanes2)
  # origin.names <- c("age", "bmi", "hyp", "chl")
  # save_vars(save.vars = c(1, 3), origin.names = colnames(nhanes2), missing.vars = c("hyp", "bmi", "chl"))
  # save_vars(save.vars = c(1, 2, 3), origin.names = colnames(nhanes2), missing.vars = c("hyp", "bmi", "chl"))
  # save_vars(save.vars = c(2, 3, 4), origin.names = colnames(nhanes2), missing.vars = c("hyp", "bmi", "chl"))
  # save_vars(save.vars = c(1, 2, 3, 4), origin.names = colnames(nhanes2), missing.vars = c("hyp", "bmi", "chl"))
  # save_vars(save.vars=c("hyp","bmi","chl","age"),origin.names=colnames(nhanes2),missing.vars=c("hyp","bmi","chl"))

  # change indices to names
  if (is.numeric(save.vars)) {
    save.vars <- origin.names[save.vars]
  }


  if (!all(save.vars %in% origin.names)) {
    stop("Some variables specified in `save.vars` do not exist in the dataset. Please check again.")
  }

  # save.vars=c("hyp","age","bmi")
  # save.vars=c("hyp","chl")
  # save.vars="age"
  # save.vars=c("hyp", "bmi", "chl","age")
  # save.vars=c("chl","age")
  # missing.vars= c("hyp", "bmi", "chl")

  if (is.null(save.vars) | setequal(save.vars, missing.vars)) {
    # general case: save models for missing.vars
    # save.vars<-missing.vars
    extra.vars <- NULL
  } else {
    # check for other cases
    if (!all(missing.vars %in% save.vars)) {
      # not all save.vars is in the missing.vars
      stop("Some variables has missing values in the training data, but 'save.vars' does not contains all of them.Please re-specify `save.vars`.")
    } else {
      # save.vars contains all missing.vars
      extra.vars <- setdiff(save.vars, missing.vars)
    }
  }

  # currently only use and 'extra.vars'
  # return(list("save.vars"=save.vars,"extra.vars"=extra.vars))
  extra.vars
}
