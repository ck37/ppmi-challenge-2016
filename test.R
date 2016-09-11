
source("lib/function_library.R")

# Load other R source files in the lib directory.
load_all_code("lib")

# Load a bunch of required packages, installing missing ones as needed.
load_all_packages(auto_install = T)

# Set some standard settings.
conf = list(dir = ".",
            data_dir = "data",
            tex_dir = "tex",
            verbose = T)

conf$cluster = ckTools::parallelize(allow_multinode = T)

conf$method = "method.NNLS"

# conf$outer_cv_folds = 10
conf$outer_cv_folds = 20
conf$cv_folds = 10

# If the cluster object is null it means we're setup for multicore (or nuthin).
if (is.null(conf$cluster) || is.na(conf$cluster)) {
  # Use non-CV if we're running this in RStudio on a laptop.
  if (T || .Platform$GUI == "RStudio") {
    fn = "sl_fn"
  } else {
    fn = "cv_sl_fn"
  }
  if (T) {
    # Multicore version.
    conf$sl_fn = ckTools::gen_superlearner(parallel = "multicore",
                                           outer_cv_folds = conf$outer_cv_folds)[[fn]]
  } else {
    # Sequential version.
    conf$sl_fn = SuperLearner
  }
  rm(fn)
} else {
  # Multinode version.
  conf$sl_fn = ckTools::gen_superlearner(parallel = "snow",
                                         cluster = conf$cluster,
                                         outer_cv_folds = conf$outer_cv_folds)$cv_sl_fn
}

#test_fn = function() {
lib = create_SL_lib()

snow::clusterExport(conf$cluster, "lib_obj")#, environment())
# List the sl_env
snow::clusterEvalQ(conf$cluster, "ls(lib_obj$sl_env)")
# Now attach the sl_env environment in each worker.
snow::clusterEvalQ(conf$cluster, "attach(lib_obj$sl_env)")
