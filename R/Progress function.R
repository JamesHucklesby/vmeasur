# library(doParallel)
#
#
# para_function(import_file, folder_files)
#
# library(progressr) ## use progressr for procession updates
# library(doFuture)  ## attaches also foreach and future
#
#
# para_function = function(funct, runvector)
# {
#
# oldDoPar <- registerDoFuture()
#
#
# registerDoFuture() ## tell foreach to use futures
# plan(multisession) ## parallelize over a local PSOCK cluster
#
#
# xs <- runvector
#
# with_progress({
#   p <- progressor(along = xs) ## create a 5-step progressor
#   y <- foreach(x = xs, .export = c("funct")) %dopar% {
#     p()                       ## signal a progression update
#     Sys.sleep(0.1)
#     funct(x)
#   }
# })
#
# y.df = dplyr::bind_rows(y, .id = "file_id")
#
# on.exit(with(oldDoPar, foreach::setDoPar(fun=fun, data=data, info=info)), add = TRUE)
#
# return(y.df)
# }
