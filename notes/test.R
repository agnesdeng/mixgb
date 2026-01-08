devtools::load_all()
sessionInfo()

library(mixgb)


devtools::load_all()
devtools::document()


set.seed(2025)
#save.models.folder= "/Users/agnes/Desktop/mixgb/notes/models" )
mixgb_obj<- mixgb(data = nhanes3_newborn, m = 5, maxit=5, save.models = TRUE)
mixgb_obj



data = nhanes3_newborn
m = 5
maxit =5
ordinalAsInteger = FALSE
save.models = TRUE
save.vars = NULL
save.models.folder= "/Users/agnes/Desktop/mixgb/notes/models"

pmm.type = NULL
pmm.k = 5
pmm.link = "prob"
initial.num = "normal"
initial.int = "mode"
initial.fac = "mode"
verbose = F
xgb.params = list()
nrounds = 100
early_stopping_rounds = NULL
print_every_n = 10L
xgboost_verbose = 0



#imputed.data
show_var(imputed.data,var.name="BMPHEAD",original.data = nhanes3_newborn)



set.seed(2025)
withNA.df <- createNA(data = nhanes3_newborn, p = 0.3)
imputed.data <- mixgb(data = withNA.df, m = 5)
imputed.data

set.seed(2025)


data = withNA.df
m = 5
maxit = 1
ordinalAsInteger = FALSE
pmm.type = NULL
pmm.k = 5
pmm.link = "prob"
initial.num = "normal"
initial.int = "mode"
initial.fac = "mode"
save.models = FALSE
save.vars = NULL
save.models.folder = NULL
verbose = F
xgb.params = list()
nrounds = 100
early_stopping_rounds = NULL
print_every_n = 10L
xgboost_verbose = 0



colSums(is.na(nhanes3_newborn))
colSums(is.na(withNA.df))

str(withNA.df)




set.seed(2025)
imputed.data <- mixgb(data = nhanes3_newborn, m = 5)
imputed.data
show_var(imputed.data,var.name="BMPHEAD",original.data = nhanes3_newborn)

devtools::load_all()
devtools::document()


set.seed(2025)
imputed.data2 <- mixgb(data = nhanes3_newborn, m = 5)
imputed.data2
show_var(imputed.data2,var.name="BMPHEAD",original.data = nhanes3_newborn)


class(nhanes3_newborn)
class(addNA.df)

str(nhanes3_newborn)

str(addNA.df)

L=mixgb(data = addNA.df, m = 5)
L

colSums(is.na(nhanes3_newborn))
str(nhanes3_newborn)


params <- list(max_depth = 3, subsample = 0.7, nthread = 2)
cv.results <- mixgb_cv(data = nhanes3, xgb.params = params)
cv.results$best.nrounds

devtools::document()

colSums(is.na(nhanes3_newborn))
withNA.df<-createNA(data=nhanes3_newborn,var.names=c("HYD1"),p=0.1)

params <- list(max_depth = 3, subsample = 0.7, nthread = 2)

mixgb.data <- mixgb(data = withNA.df, m = 5, xgb.params = params,nrounds = 100, pmm.type = "auto")


mixgb.data


# Test: only 1 variable is missing ----------------------------------------


library(dplyr)
mtcars_miss <- mtcars |>
  mutate(mpg = if_else(am == 1 , NA, mpg),
         wt = if_else(vs == 1 , NA, wt))

library(dplyr)
mtcars_miss <- mtcars |>
  mutate(mpg = if_else(am == 1 , NA, mpg))

class(mtcars_miss)
colnames(mtcars_miss)
colSums(is.na(mtcars_miss))

params <- list(max_depth = 3, subsample = 0.7, nthread = 2)
mixgb.data <- mixgb(data = mtcars_miss)

head(mtcars_miss)
head(mixgb.data[[5]])
head(mtcars)
# Test: only 1 variable is missing ----------------------------------------


data<-mtcars_miss
m = 5
maxit = 1
ordinalAsInteger = FALSE
pmm.type = "auto"
pmm.k = 5
pmm.link = "prob"
initial.num = "normal"
initial.int = "mode"
initial.fac = "mode"
save.models = FALSE
save.vars = NULL
save.models.folder = NULL
verbose = F
xgb.params = params
nrounds = 100
early_stopping_rounds = NULL
print_every_n = 10L
xgboost_verbose = 0





