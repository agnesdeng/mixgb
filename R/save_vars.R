#' checking save.vars
#' @examples
#' origin.names <- c("age", "bmi", "hyp", "chl")
#' save_vars(save.vars = c(1, 3), origin.names = colnames(nhanes2), missing.vars = c("hyp", "bmi", "chl"))
#' save_vars(save.vars = c(1, 2, 3), origin.names = colnames(nhanes2), missing.vars = c("hyp", "bmi", "chl"))
#' save_vars(save.vars = c(2, 3, 4), origin.names = colnames(nhanes2), missing.vars = c("hyp", "bmi", "chl"))
#' save_vars(save.vars = c(1, 2, 3, 4), origin.names = colnames(nhanes2), missing.vars = c("hyp", "bmi", "chl"))
# "save_vars(save.vars=c("hyp","bmi","chl","age"),origin.names=colnames(nhanes2),missing.vars=c("hyp","bmi","chl"))
#' @export
#' @return extra.vars, the names of the extra variables need to be saved models for.
save_vars <- function(save.vars, origin.names, missing.vars) {

  # change indices to names
  if (class(save.vars) == "numeric") {
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



# for future development
save_vars2 <- function(save.vars, origin.names, missing.vars) {
  if (!is.null(save.vars)) {
    # change indices to names
    if (class(save.vars) == "numeric") {
      save.vars <- origin.names[save.vars]
    }

    if (!all(save.vars %in% origin.names)) {
      stop("Some variables specified in `save.vars` do not exist in the dataset. Please check again.")
    }

    # save.vars=c("hyp","chl","bmi")
    # save.vars=c("hyp","chl")
    # save.vars="age"
    # save.vars=c("hyp", "bmi", "chl","age")
    # save.vars=c("chl","age")
    # missing.vars= c("hyp", "bmi", "chl")
    # case 1: save.vars is the same set as missing.vars. Default case: save models for keep.vars, which equals to missing.vars
    if (setequal(save.vars, missing.vars)) {
      keep.vars <- missing.vars
      extra.vars <- NULL
      # Nmodels<-mp
    } else {
      # case 2: save.vars is a proper subset of missing.vars
      if (all(save.vars %in% missing.vars)) {
        # skip.vars<-setdiff(missing.vars,save.vars)
        # keep.vars: response variables in missing.vars that need to be saved. Only save models for keep.vars
        keep.vars <- save.vars
        extra.vars <- NULL
        action <- askYesNo(msg = "Some variables has missing values in the training data, but 'save.vars' does not contains all of them. Do you want to continue?")
        if (is.na(action) | action == FALSE) {
          stop("Please re-specify `save.vars`.")
        }

        # .............................................
      } else {
        keep.vars <- intersect(missing.vars, save.vars)
        if (length(keep.vars) == 0) {
          # case 3: save.vars and missing.vars are disjoint, don't need to keep any models for missing.vars. Only save models for extra.vars
          keep.vars <- NULL
          extra.vars <- save.vars

          action <- askYesNo(msg = "Some variables has missing values in the training data, but 'save.vars' does not contains all of them. Do you want to continue?")
          if (is.na(action) | action == FALSE) {
            stop("Please re-specify `save.vars`.")
          }

          # .............................................
        } else {
          # save.vars intersect missing.vars
          extra.vars <- setdiff(save.vars, keep.vars)

          if (length(keep.vars) == mp) {
            # setequal(keep.vars,missing.vars)
            # case 4: missing.vars is a proper subset of saves.vars. Including the case when models for all variables are saved
            keep.vars <- missing.vars
            Nmodels <- mp + length(extra.vars)
          } else {
            # case 5:save.vars intersect missing.vars: save models for variables in keep.vars and extra.vars
            action <- askYesNo(msg = "Some variables has missing values in the training data, but 'save.vars' does not contains all of them. Do you want to continue?")
            if (is.na(action) | action == FALSE) {
              stop("Please re-specify `save.vars`.")
            }
            # .............................................

            # Nmodels<-length(keep.vars)+length(extra.vars)
          }
        }
      }
    }
    save.vars <- c(keep.vars, extra.vars)
  } else {
    # default, save.vars=NULL
    save.vars <- missing.vars
    keep.vars <- missing.vars
    extra.vars <- NULL
  }

  # currently only use 'save.vars' and 'extra.vars'
  return(list("save.vars" = save.vars, "keep.vars" = keep.vars, "extra.vars" = extra.vars))
}
