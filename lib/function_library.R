#' Load all packages needed, installing any missing packages from CRAN.
#'
#' @param auto_install Install any packages that could not be loaded.
#' @param update Update any packages that can be updated.
#' @param java_mem Amount of RAM to allocate to rJava; must happen before
#'   library is loaded.
load_all_packages = function(auto_install = F, update = F, java_mem = "4g", verbose = F) {
  # Output R version so we know which package versions we're using.
  cat(R.version.string, "\n")

  # Allocate 4GB to Java for bartMachine; needs to happen before we load rJava library.
  options(java.parameters = paste0("-Xmx", java_mem))

  libs = c("arm", "arules",
          # "bartMachine",
           "caret", "class", "cvAUC",
           "cvTools", "doMC", "doParallel", "doSNOW", "dplyr", "e1071", "earth",
           "foreach", "foreign", "gam", "gbm", "ggplot2", "glmnet", "gplots", "haven",
           "histogram", "hopach", "ipred", "MASS", "multtest", "parallel", "party",
           "polspline", "qdapTools", "quantreg", "randomForest", "RColorBrewer",
           "rJava", "reader", "readstata13", "readxl", "ROCR", "rpart", "SparseM",
           "SuperLearner", "tidyr", "tmle", "xgboost", "xtable", "varImpact")

  # Code is not yet run. We run afterward, possibly with messages suppressed.
  expression = quote({

    # NOTE: may want to install the latest xgboost from github.
    # Can run this manually:
    if (!require("xgboost") && auto_install) {
      if (verbose) cat("No xgboost detected - installing.\n")
      install.packages("xgboost",
                       repos=c("http://dmlc.ml/drat/", getOption("repos")),
                       type="source")
    }

    # Install devtools if we don't already have it.
    if (!require("devtools") && auto_install) {
      install.packages("devtools")
      library(devtools)
    }

    # Install bioconductor. We need to use http on Savio.
    source("http://bioconductor.org/biocLite.R")
    # Install bioconductor and update installed packages.
    biocLite(ask=F)

    # Install additional bioc packages if they aren't already installed.
    sapply(c("hopach", "multtest"), function(pkg) {
      if (!require(pkg, character.only=T))
        biocLite(pkg) || "newly installed" else "already installed"
    })

    # Install ckTools and varImpact, and we need the latest SuperLearner from github.
    if (auto_install) {
      devtools::install_github(c("ecpolley/SuperLearner",
                                 "ck37/ck37r",
                                 "ck37/varImpact"))
    }

    invisible(sapply(c("SuperLearner", "ck37r", "varImpact"),
                     require, character.only = T))

    ck37r::load_packages(libs, auto_install, update, verbose = verbose)

  }) # end quote()

  # Now run the stored code either directly or with messages suppressed.
  if (verbose) {
    # Allow messages to be output.
    eval(expression)
  } else {
    # Supress messages.
    suppressMessages(eval(expression))
  }
}

#' Import all CSV files in a given directory and save them to a list.
import_csvs = function(directory = "", file_pattern = "\\.csv$") {

  file_names = list.files(path=directory, file_pattern, full.names=F, recursive=T)
  cat(paste0("Found ", length(file_names), " text files in \"", directory, "\" to import.\n"))

  if (length(file_names) == 0) {
    stop(paste("did not find any files to load. \nMake sure you have unzipped the ",
               "data into the inbound directory."))
  }

  system.time({
    files = list()
    for (file in file_names) {
      # Remove the file extension from the file.
      list_name = stringr::str_to_lower(gsub(file_pattern, "", file))
      # Import the csv file.
      data = reader::reader(file, directory, header=T, def = ",")
      # Lowercase the column names.
      colnames(data) = sapply(colnames(data), FUN=stringr::str_to_lower)
      files[[list_name]] = data
    }
  })

  # Double-check how many documents we loaded.
  cat("Total files imported:", length(files), "\n")

  # Return the result.
  return(files)
}

#' Load all R files in the lib directory.
load_all_code = function(lib_dir = "lib", exclude_files = c("function_library.R"), file_pattern = "\\.R$") {
  # Load all .R files in the lib directory.
  lib_files =  list.files(path=lib_dir, file_pattern, full.names=F, recursive=F)

  # Exclude any files that we don't need to load.
  lib_files = setdiff(lib_files, exclude_files)

  # Loop over file list and load each file.
  for (file in lib_files) {
    file_name = paste0(lib_dir, "/", file)
    cat("Sourcing", file_name, "\n")
    source(file_name)
  }
}

plot_all_results = function(results, Y, Y_name, libs, type="classification",
                            plot_dir = "", tex_dir = "",
                            lib_names = paste("Library", 1:length(results))) {
  new_results = list()

  # Loop over each library result.
  for (i in 1:length(results)) {
    cat("Processing result", i, "\n")
    if (class(results[[i]]) != "CV.SuperLearner") {
      cat("Result", i, "is not a CV.Superlearner, skipping.\n")
      # Skip this loop iteration.
      next
    }

    new_res = list()

    if (type == "classification") {
      # Generate AUC plot.
      title = paste0("Y = ", Y_name, ". ", lib_names[i], " with ", length(libs[[i]]$lib), " models")
      filename = paste0(plot_dir, Y_name, "-auc-model", i, ".png")
      # Generate ROC curve graph with AUC.
      roc_result = plot_auc(results[[i]], Y = Y, title, filename)


      new_res$auc = roc_result$auc
      new_res$auc_ci = roc_result$auc_ci
      cat("AUC", i, ":", round(new_res$auc, 4),
          paste0("(", paste(round(new_res$auc_ci, 4), collapse=" - "), ")"), "\n")

    }

    # Table to review SL weight distribution.
    wgt_table = cvSL_review_weights(results[[i]])

    # Save the table of weights before we start modifying it for display.
    new_res$weight_table = wgt_table

    # Display models with non-zero mean weight.
    wgt_table = wgt_table[wgt_table[,  1] > 0, , drop=F]

    # Sort in descending order of mean weight.
    wgt_table = wgt_table[order(wgt_table[, 1, drop=F], decreasing=T), , drop=F]

    # Remove extra cruft from rownames so table is prettier.
    rownames(wgt_table) = gsub("_All$", "", rownames(wgt_table))
    rownames(wgt_table) = gsub("^SL.", "", rownames(wgt_table))

    # Add rank to the rownames so we know each model's rank.
    rownames(wgt_table) = paste0(1:nrow(wgt_table), ". ", rownames(wgt_table))

    # Round mean to 4 digits.
    wgt_table[, 1] = round(wgt_table[, 1, drop=F], 4)
    # Round SD to 4 digits.
    wgt_table[, 2] = round(wgt_table[, 2, drop=F], 4)
    # Round min to 4 digits.
    wgt_table[, 3] = round(wgt_table[, 3, drop=F], 4)
    # Round max to 4 digits.
    wgt_table[, 4] = round(wgt_table[, 4, drop=F], 4)

    print(wgt_table)
    print(xtable(wgt_table, align="lrrrr", digits = 2,
                 #, caption = paste(Y_name, "SL cross-validated model weights, model", i)
    ),
    type="latex", floating=F,
    file=paste0(tex_dir, Y_name, "-weights-", i, ".tex"))

    # Save the prettier weight table.
    new_res$print_wgt_table = wgt_table

    new_results[[i]] = new_res
  }

  aucs = sapply(new_results, FUN=function(x) ifelse(is.null(x$auc), 0, x$auc))

  cis = sapply(new_results, FUN=function(x) {
    if (is.null(x$auc_ci) || x$auc_ci == c(0, 0)) {
      ""
    } else {
      paste0("(", round(x$auc_ci[1], 3), " - ", round(x$auc_ci[2], 3), ")")
    }
  })

  lib_sizes = sapply(results, FUN=function(x) length(x$libraryNames))

  # Create a summary table of library sizes, AUCs, and AUC CIs.
  auc_table = data.frame(lib_names, lib_sizes, round(aucs, 3), cis)

  colnames(auc_table) = c("", "Configs", "AUC", "95 CI")
  # Sort in descending order of AUC.
  auc_table = auc_table[order(auc_table$AUC, decreasing=T), ]
  # Remove rows where AUC = 0.
  auc_table = auc_table[auc_table$AUC != 0, ]
  rownames(auc_table) = 1:nrow(auc_table)
  print(auc_table)
  print(xtable(auc_table, align="clccc", digits = 3,
               #, caption = paste(Y_name, "SL cross-validated model weights, model", i)
  ),
  type="latex", floating=F, include.rownames=T,
  file=paste0(tex_dir, Y_name, "-summary.tex"))


  new_results$auc_table = auc_table

  return(invisible(new_results))
}

fit_model_libs = function(Y, data, libs, family=binomial(), sl_fn, cv_folds = 10,
                          cluster = NULL) {
  results = vector("list", length(libs))
  for (lib_i in 1:length(libs)) {
    lib_obj = libs[[lib_i]]
    sl_lib = lib_obj$lib
    cat("\nLib", lib_i, "with size:", length(sl_lib), "\n")
    # cat(paste(sl_lib), "\n")

    if ("sl_env" %in% names(lib_obj)) {
      #cat("Attaching custom SuperLearner environment.\n")
      attach(lib_obj$sl_env)
      if (!is.null(cluster) && !is.na(cluster)) {
        # Export custom learners across the cluster to each worker.
        cat(paste0("Attempting to export this learners (", length(ls(lib_obj$sl_env)), ") ",
                   paste(ls(lib_obj$sl_env), collapse = ", "), "\n"))
        # This doesn't seem to work:
        #snow::clusterExport(cluster, ls(lib_obj$sl_env), lib_obj$sl_env)
        # Export lib_obj, which is defined in this function's environment.
        snow::clusterExport(cluster, "lib_obj", environment())
        # Now attach the sl_env environment in each worker.
        snow::clusterEvalQ(cluster, "attach(lib_obj$sl_env)")
      }
    }


    # Create a default result in case we have an error during model fitting.
    result = list(is_error = T)

    # Can initially try without outer CV to speed up completion time.
    time = system.time({
      # Ideally we would save the fit library for future prediction, including
      # partial dependence plots. But it takes up too much memory :/

      # Use try() so that we can recover from potential errors.
      tryCatch({
        result = sl_fn(Y=Y, X = data, family = family, SL.library = sl_lib,
                       cvControl = list(V = cv_folds), control = list(saveFitLibrary = F))
        result$is_error = F
      }, error = function(e) {
        cat("Caught error and continuing.\n")
        print(e)
        # Try to deal with the error by creating a blank result.
        # May need to tweak this default.
        # TODO: This doesn't seem to work correctly - fix.
      })
    })

    if ("sl_env" %in% names(lib_obj)) {
      detach(lib_obj$sl_env)
    }

    if (result$is_error) {
      # Do nothing.
    } else if (class(result) == "SuperLearner") {
      print(result)
    } else {
      # CV.SuperLearner
      print(summary(result))
    }
    result$time = time
    print(result$time)

    # Save results.
    results[[lib_i]] = result
  }
  results
}

# Review meta-weights from a CV.SuperLearner
cvSL_review_weights = function(cv_sl) {
  meta_weights = coef(cv_sl)
  means = colMeans(meta_weights)
  sds = apply(meta_weights, MARGIN=2, FUN=function(col) { sd(col) })
  mins = apply(meta_weights, MARGIN=2, FUN=function(col) { min(col) })
  maxs = apply(meta_weights, MARGIN=2, FUN=function(col) { max(col) })
  # Combine the stats into a single matrix.
  sl_stats = cbind("mean(weight)"=means, "sd"=sds, "min"=mins, "max"=maxs)
  sl_stats
}

#' @param data_dims Dimensions of the training data (two-element vector).
#' @param glm Set glm to F if there are more columns than rows.
#' @param type Classification or regression
cumulative_library_seq = function(data_dims,
                                  glm = T,
                                  type="classification") {
  #extra_libs = c("SL.bartMachine")
  # This is pretty slow:
  #extra_libs = list(c("SL.glm", "screen.glmnet"))
  extra_libs = c()

  nc = data_dims[2]

  # If we have more columns than rows we need to disable glm
  if (data_dims[2] >= (data_dims[1] + 1)) {
    if (glm) {
      cat("Disabling glm because we have more columns than rows.\n")
      glm = F
    }
  }

  # Minimum library.
  lib0 = list(lib=list("SL.mean", "SL.glm"))#, c("SL.glm", "screen.glmnet")))
  cat("Total models:", length(lib0$lib), "\n")

  lib1 = list(lib=c(lib0$lib, "SL.glmnet"))
  cat("Total models:", length(lib1$lib), "\n")

  # lib2 = list(lib=c("SL.glmnet", "SL.mean", "SL.glm", "SL.stepAIC", "SL.earth", "SL.rpartPrune"))
  # Remove stepAIC
  #lib2 = list(lib=c(lib1$lib, "SL.earth", "SL.rpartPrune"))
  lib2 = list(lib=c("SL.glmnet", "SL.mean", "SL.glm", "SL.earth"))
  #cat("Total models:", length(lib2$lib), "\n")

  #lib3 = create_SL_lib(nc, type=type, xgb=F, glm=glm, rf=F, cf=F, earth=F, knn=F, glmnet=F, gam=F, detailed_names = T, extra_libs = list("SL.glmnet", extra_libs))
  lib3 = create_SL_lib(nc, type=type, xgb=F, glm=glm, rf=F, cf=F, earth=F, knn=F, glmnet=F, gam=F, detailed_names = T, extra_libs = c("SL.glmnet"))
  lib4 = create_SL_lib(nc, type=type, xgb=F, glm=glm, rf=F, cf=F, earth=F, knn=F, glmnet=T, glmnet_size=5, gam=F, extra_libs = extra_libs, detailed_names = T)
  # Temporarily removing RFs as they are resulting in NAs for some reason.
  lib5 = create_SL_lib(nc, type=type, xgb=F, glm=glm, rf=T, cf=F, earth=F, knn=F ,glmnet=T, glmnet_size=5, gam=F, extra_libs = extra_libs, detailed_names = T)
  # Temporarily removing GAMs as they are resulting in NAs for some reason.
  # lib6 = create_SL_lib(nc, xgb=F, rf=T, glmnet=T, glmnet_size=5, gam=F, extra_libs = extra_libs, detailed_names = T)
  # Temporarily removing RFs as they are resulting in NAs for some reason.
  lib7 = create_SL_lib(nc, type=type, xgb=F, glm=glm, rf=T, cf=F, earth=F, knn=F, glmnet=T, glmnet_size=11, gam=F, extra_libs = extra_libs, detailed_names = T)
  #lib7 = create_SL_lib(nc, type=type, xgb=F, glm=glm, rf=F, cf=T, earth=F, knn=F, glmnet=T, glmnet_size=11, gam=F, extra_libs = extra_libs, detailed_names = T)
  # TODO: one more version with a few XGB configs.
  lib8 = create_SL_lib(nc, type=type, xgb="small", glm=glm, rf=T, cf=F, earth=T, knn=F, glmnet=T, glmnet_size=11, gam=F, detailed_names = T)
  # Full library.
  lib9 = create_SL_lib(nc, type=type, xgb=T, glm=glm, rf=T, cf=T, earth=F, knn=F, glmnet=T, glmnet_size=11, gam=F, detailed_names = T)

  libs = list(lib0, lib1, lib2, lib3, lib4, lib5, lib6, lib7, lib8, lib9)
  #libs = list(lib0, lib1, lib2, lib3, lib4, lib5, lib7, lib8, lib9)
  #libs = list(lib0, lib1, lib3, lib4, lib5, lib7, lib8, lib9)
  #libs = list(lib0, lib1, lib3, lib4, lib5, lib7, lib8)
  if (!glm) {
    cat("Removing GLM from all libraries.")
    libs = lapply(libs, FUN=function(x) {
      #x$lib = setdiff(x$lib, "SL.glm")
      # We need a slightly more complicated exclusion to allow screeners.
      x$lib = x$lib[is.na(match(x$lib, "SL.glm"))]
      x
    })
  }
  libs
}

#' @param data_dims Dimensions of the training data (two-element vector).
#' @param glm Set glm to F if there are more columns than rows.
individual_library_seq = function(data_dims, glm = F, type = "classification") {
  num_cols = data_dims[2]
  libs = list(
    list(lib="SL.mean"),
    #list(lib="SL.rpart"),
    list(lib="SL.rpartPrune"),
    create_SL_lib(num_cols, glmnet=T, glmnet_size=5, other_libs = c(), detailed_names = T),
    create_SL_lib(num_cols, glmnet=T, glmnet_size=11, other_libs = c(), detailed_names = T),
    list(lib="SL.svm"),
    create_SL_lib(num_cols, earth=T, other_libs = c(), detailed_names = T),
    #create_SL_lib(num_cols, gam=T, other_libs = c()),
    list(lib="SL.polymars"),
    list(lib=list(c("SL.glm", "screen.glmnet"))),
    create_SL_lib(num_cols, rf=T, other_libs = c(), detailed_names = T),
    list(lib="SL.gbm"),
    create_SL_lib(num_cols, xgb = "small", other_libs = c(), detailed_names = T),
    create_SL_lib(num_cols, xgb = T, other_libs = c(), detailed_names = T)
  )
  libs
}

create_SL_lib = function(num_cols = NULL, type = "classification",
                         glm = F, xgb = F, rf = F, cf = F, dsa = F,
                         glmnet = F, gam=F, glmnet_size = 11, detailed_names = F,
                         earth = F, knn = F,
                         other_libs = c("SL.svm", "SL.mean", #"SL.bayesglm", "SL.ipredbagg"
                                        "SL.glm", "SL.polymars"),#, "SL.rpartPrune"), # removed  "SL.stepAIC",
                         extra_libs = c(), env = new.env()) {

  #sl_env = .GlobalEnv
  if (type == "regression") {
    # Remove certain algorithms that are intended for classification probblems.
    # SVM can actually be used for regression.
    # other_libs = setdiff(other_libs, c("SL.svm"))
  }

  # Remove GLM if necessary, e.g. if we have more columns than rows.
  if (!glm) {
    other_libs = setdiff(other_libs, "SL.glm")
  }

  glmnet_libs = c()
  if (glmnet) {
    alpha_params = seq(0, 1, length.out=glmnet_size)
    glmnet_results = create.SL.glmnet(alpha = alpha_params, detailed_names = T, env = env)
    glmnet_libs = glmnet_results$names
    cat("Glmnet:", length(glmnet_libs), "configurations. Alphas:", paste(alpha_params, collapse=", "), "\n")
  }

  # Create xgboost models.
  xgb_libs = c()
  xgb_grid = NA

  # Check for F to allow xgb to be T or a string.
  if (xgb != F) {

    if (xgb == "small") {
      xgb_tune = list(ntrees = c(50, 500, 3000), max_depth = c(1, 3), shrinkage = c(0.01, 0.1, 0.2, 0.4), minobspernode = c(10))
    } else {
      # Slower, ideal configuration search (intended for servers).
      xgb_tune = list(ntrees = c(50, 200, 500, 1000, 3000), max_depth = c(1, 2, 3), shrinkage = c(0.01, 0.1, 0.2, 0.4), minobspernode = c(10))
    }

    # Faster, less ideal configuration search (intended for laptops):
    # BUT disable for now - we have so few observations that we can use the server version.
    if (F && RhpcBLASctl::get_num_cores() < 8) {
      xgb_tune = list(ntrees = c(1000, 2000), max_depth = c(1, 2), shrinkage = c(0.1, 0.2), minobspernode = c(10))
    }

    # TODO: don't use global vars here.
    xgb_results = create.SL.xgboost(xgb_tune, detailed_names = detailed_names, env = env)
    xgb_grid = xgb_results$grid
    xgb_libs = xgb_results$names

    cat("XGBoost:", length(xgb_libs), "configurations.\n")
    print(xgb_grid)
  }

  rf_libs = c()
  rf_grid = NA
  if (rf) {
    if (!is.null(num_cols)) {
      # Much better is to send in how many columns are in the dataset.
      rf_tune = list(mtry = unique(round(exp(log(num_cols)*exp(c(-0.96, -0.71, -0.48, -0.4, -0.29, -0.2))))),
                     maxnodes = c("NULL", 10, 20))
    } else {
      rf_tune = list(mtry = c(1, 5, 10))
    }

    #rf_models = SuperLearner::create.SL.randomForest(rf_tune,
    #                            detailed_names = detailed_names, name_prefix = "SL_rf", env = sl_env)

    # Use 1,000 trees rather than 500.
    rf_params = list(ntree = 1000)

    rf_models = create.Learner("SL.randomForest", rf_params, tune = rf_tune,
                               detailed_names = detailed_names, name_prefix = "SL_rf",
                               env = env)

    rf_libs = rf_models$names
    rf_grid = rf_models$grid

    cat("Random Forest:", length(rf_libs), "configurations.\n")
    print(rf_grid)
  }

  cf_libs = c()
  cf_grid = NA
  if (cf) {
    if (!is.null(num_cols)) {
      # Much better is to send in how many columns are in the dataset.
      cf_tune = list(mtry = unique(round(exp(log(num_cols)*exp(c(-0.96, -0.71, -0.48, -0.4, -0.29, -0.2))))))
    } else {
      cf_tune = list(mtry = c(1, 5, 10))
    }

    # Use 1,000 trees rather than 500.
    cf_params = list(ntree = 1000)

    cf_models = create.Learner("SL.cforest", cf_params, tune = cf_tune,
                               detailed_names = detailed_names, name_prefix = "SL_cf",
                               env = env)

    cf_libs = cf_models$names
    cf_grid = cf_models$grid

    cat("Conditional Forest:", length(cf_libs), "configurations.\n")
    print(cf_grid)
  }

  gam_libs = c()
  if (gam) {
    gam_degrees = c(2, 3, 4)
    gam_tune = list(degree = gam_degrees)
    # gam_models = create.SL.gam(gam_degrees, env = env)
    gam_models = create.Learner("SL.gam", tune = gam_tune, env = env,
                                detailed_names = T)
    gam_libs = gam_models$names
    cat("GAM:", length(gam_libs), "configurations.\n")
    print(gam_models$grid)
  }

  earth_libs = c()
  if (earth) {
    earth_degrees = c(2, 3, 4)
    earth_tune = list(degree = earth_degrees)
    earth_models = create.Learner("SL.earth", tune = earth_tune, env = env,
                                  detailed_names = T)
    earth_libs = earth_models$names
    cat("Earth:", length(earth_libs), "configurations.\n")
    print(earth_models$grid)
  }

  knn_libs = c()
  if (knn) {
    k = c(10, 20, 40, 80)
    tune = list(k = k)
    knn_models = create.Learner("SL.knn", tune = tune, env = env,
                                detailed_names = T)
    knn_libs = knn_models$names
    cat("Knn:", length(knn_models$names), "configurations.\n")
    print(knn_models$grid)
  }

  # TODO: see if we want to tweak the hyperparameters of any of these singular models.
  # Remove "SL.polymars", for now. (CK 5/4/16)
  lib = c(glmnet_libs, xgb_libs, rf_libs, cf_libs, gam_libs, earth_libs,
          knn_libs, other_libs, extra_libs)

  if (dsa) {
    # WARNING: super duper slow :/
    lib = append(lib, "SL.DSA")
  }

  cat("Total models:", length(lib), "\n")
  results = list(lib = lib, xgb_grid = xgb_grid, xgb_libs = xgb_libs,
                 glmnet_libs = glmnet_libs, rf_grid = rf_grid, rf_libs = rf_libs,
                 cf_libs = cf_libs, cf_grid = cf_grid, earth_libs = earth_libs,
                 knn_libs = knn_libs,
                 other_libs = other_libs, extra_libs = extra_libs, sl_env = env)
  results
}

create.SL.glmnet <- function(alpha = c(0, 0.25, 0.50, 0.75, 1),
                             detailed_names = F,
                             name_prefix = "SL_glmnet",
                             env = .GlobalEnv, verbose = F) {
  grid = expand.grid(list(alpha = alpha))
  names = rep("", nrow(grid))
  for (i in seq(length(alpha))) {
    g = grid[i, , drop=F]

    if (detailed_names) {
      name = paste(name_prefix, g$alpha, sep="_")
    } else {
      name = paste(name_prefix, i, sep="_")
    }
    names[i] = name

    fn = paste0(name, '<- function(...) SL.glmnet(..., alpha = ', g$alpha, ')')
    eval(parse(text = fn), envir = env)
  }
  results = list(grid = grid, names = names)
  invisible(results)
}

get_codebook = function() {

  # "Data dictionary" should be sheet 1.
  codes = readxl::read_excel("inbound/PPMI_Data_Dictionary.xlsx")

  colnames(codes) = tolower(colnames(codes))

  colnames(codes)[2] = "field_name"
  colnames(codes)[4] = "nice_label"
  colnames(codes)

  codes$field_name = tolower(codes$field_name)

  # Remove rows that are NA, and reduce to the only columns that we need.
  codes = subset(codes, !is.na(field_name), select=c(field_name, nice_label))

  # Remove duplicate rows (these will be general fields hopefully).
  # This will generate a warning.
  codes = codes %>% dplyr::distinct(field_name, nice_label)

  codes
}

