# tells data.table that we are using it
.datatable.aware <- TRUE

# Avoid R CMD check : 'no visible binding for global variable' NOTE
#'@importFrom utils globalVariables
#'@keywords internal
utils::globalVariables(c("..density..", "..prop..", "Freq", "Var1", "Var3", "m.set", "obs", "NA.condition"))

