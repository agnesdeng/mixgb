devtools::load_all()
devtools::document()

colSums(is.na(nhanes3_newborn))
withNA.df<-createNA(data=nhanes3_newborn,var.names=c("HYD1"),p=0.1)
params <- list(max_depth = 3, subsample = 0.7, nthread = 2)
mixgb.data <- mixgb(data = withNA.df, m = 5, xgb.params = params, nrounds = 100,pmm.type = "auto")

plot2D(imputation.list = mixgb.data,var.x="HYD1",var.y="HFF1",original.data = withNA.df)

