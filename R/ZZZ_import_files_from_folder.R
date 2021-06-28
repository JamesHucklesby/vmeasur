# function(){
#
#   library(future)
#   library(progressr)
#   library(foreach)
#   library(doFuture)
#
# csv_files = list.files("//files.auckland.ac.nz/research/ressci202000061-PROM-study/Full Dataset 2",
#                        recursive = TRUE, pattern = "\\_widths.csv$", full.names = TRUE)
#
# length(csv_files)
#
# # Completed files
# done = unique(basename(dirname(csv_files)))
# total = basename(list.dirs("//files.auckland.ac.nz/research/ressci202000061-PROM-study/Full Dataset 2"))
#
# length(done)
# length(total)
# length(done)/(length(total))
#
# done_dirs = unique(dirname(csv_files))
#
# # current_dir = "//files.auckland.ac.nz/research/ressci202000061-PROM-study/Full Dataset 2/SH22S1"
#
# oldDoPar <- registerDoFuture()
#
# cl <- parallel::makeCluster(15)
#
# registerDoFuture() ## tell foreach to use futures
# plan("cluster", workers = cl, gc = TRUE) ## parallelize over a local PSOCK cluster
#
# xs <- done_dirs
#
# handlers(list(
#   handler_progress(
#     format   = ":spin :current/:total (:message) [:bar] :percent in :elapsed ETA: :eta",
#     width    = 60,
#     complete = "+"
#   )
# ))
#
# with_progress({
#   p <- progressor(along = xs)
#   y <- foreach(x = xs, .export = c("import_folder_bin")) %dopar% {
#     fulldata_mean = import_folder_bin(x)
#     write.csv(fulldata_mean, paste(x, "/S",paste(unique(fulldata_mean$site), collapse = "_"),"_s_average.csv", sep = ""))
#     rm(fulldata_mean)
#     gc()
#     p(basename(x)) ## signal a progression update
#     return(TRUE)
#
#   }
# })
#
# stopCluster(cl)
#
# on.exit(with(oldDoPar, foreach::setDoPar(fun=fun, data=data, info=info)), add = TRUE)
#
#
#
#
#
#
# csv_files = list.files("//files.auckland.ac.nz/research/ressci202000061-PROM-study/Full Dataset 2",
#                        recursive = TRUE, full.names = TRUE)
#
# summary_csv = subset(csv_files,(str_count(csv_files, "_s_average.csv")>0))
#
# summary_data  = lapply(summary_csv, read.csv, as.is = TRUE)
#
#
# summary.df = summary_data[[1]]
#
# for(i in c(2:length(summary_data)))
# {
#   print(i)
#   summary.df = rbind(summary.df, summary_data[[i]])
# }
#
# vessel_list = summary.df %>% select(vessel, source_video, site) %>% distinct()
#
#
#
# # summary.df = summary.df %>% group_by(vessel, source_video) %>%
# #   mutate(smooth = ksmooth(c(1:length(p_mean)), p_mean, kernel = "normal", bandwidth = 3, n.points= nrow(p_mean))$y) %>%
# #   ungroup()
#
# summary.df = summary.df %>% group_by(vessel, source_video, ygroup) %>%
#   mutate(smooth = rollmean(p_mean, 5, fill = NA)) %>%
#   ungroup()
# }
