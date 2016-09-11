# Install devtools if it's not already installed.
if (!require("devtools")) install.packages("devtools")

libs = c("arm", "arules", "bartMachine", "caret", "class", "cvAUC",
         "cvTools", "doMC", "doParallel", "doSNOW", "dplyr", "e1071", "earth",
         "foreach", "foreign", "gam", "ggplot2", "glmnet", "gplots", "haven",
         "histogram", "hopach", "ipred", "MASS", "multtest", "parallel", "party",
         "polspline", "qdapTools", "quantreg", "randomForest", "RColorBrewer",
         "rJava", "reader", "readstata13", "readxl", "ROCR",
         "rpart", "SparseM", "tidyr", "tmle", "xgboost", "xtable")

# Try to load each package, and save whether or not it succeeded.
suppressMessages({ result = sapply(libs, require, character.only=T, quietly=T) })

# Install any packages that could not be loaded.
if (sum(!result) > 0) {
  cat("These packages need to be installed:", paste(libs[!result], collapse=", "), "\n")
  cat("Auto-installing from repository:", getOption("repos")[1], "\n")
  install.packages(libs[!result], dependencies = T)
}

# Install bioconductor. We need to use http on Savio.
source("http://bioconductor.org/biocLite.R")
# Install bioconductor and update installed packages.
biocLite(ask=F)

# Install additional bioc packages if they aren't already installed.
sapply(c("hopach", "multtest"), function(pkg) {
  if (!require(pkg, character.only=T)) biocLite(pkg) || "new" else "old"
})

# Update any R packages that can be updated.
update.packages(ask = F, checkBuilt = T)

# Install ckTools and varImpact, and we need the latest SuperLearner from github.
devtools::install_github(c("ecpolley/SuperLearner", "ck37/ckTools", "ck37/varImpact"),
                         dependencies=T)

# Install the latest development version of xgboost.
if (!require("xgboost")) {
  drat:::addRepo("dmlc")
  install.packages("xgboost", repos="http://dmlc.ml/drat/", type = "source")
}
