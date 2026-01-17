# Checking save.vars
save_vars <- function(save.vars, origin.names, missing.vars) {
  # return extra.vars, the names of the extra variables need to be saved models for.

  # change indices to names
  if (is.numeric(save.vars)) {
    save.vars <- origin.names[save.vars]
  }


  if (!all(save.vars %in% origin.names)) {
    stop("Some variables specified in `save.vars` do not exist in the dataset. Please check again.")
  }


  if (is.null(save.vars) | setequal(save.vars, missing.vars)) {
    # general case: save models for missing.vars
    # save.vars<-missing.vars
    extra.vars <- NULL
  } else {
    # check for other cases
    if (!all(missing.vars %in% save.vars)) {
      # not all save.vars is in the missing.vars
      stop("Some variables has missing values in the training data, but 'save.vars' does not contains all of them. Please re-specify `save.vars`.")
    } else {
      # save.vars contains all missing.vars
      extra.vars <- setdiff(save.vars, missing.vars)
    }
  }

  # currently only use and 'extra.vars'
  # return(list("save.vars"=save.vars,"extra.vars"=extra.vars))
  extra.vars
}
