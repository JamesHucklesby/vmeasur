#'
#' working_folder = choose.dir()
#'
#' folders_to_process = list.dirs(working_folder, recursive = FALSE)
#'
#' graph_folder(folders_to_process[1])
#'
#'
#' library(doFuture)
#' registerDoFuture()
#' plan(multisession)
#'
#'
#' library(progressr)
#' handlers(list(
#'   handler_progress(
#'     format   = ":spin :current/:total (:message) [:bar] :percent in :elapsed ETA: :eta"
#'   )))
#'
#' my_fcn <- function(xs) {
#'   p <- progressor(steps = length(xs))
#'   y <- foreach(x = xs) %dopar% {
#'     graph_folder(x)
#'     p(message = sprintf("Completed %s", x), class = "sticky")
#'     gc()
#'   }
#' }
#'
#'
#'
#' with_progress(my_fcn(folders_to_process))
#'
#'
#' #' Title
#' #'
#' #' @param folder
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' graph_folder = function(folder)
#' {
#' # Import a whole folder of data, binned into ygroups
#'
#' var1 = import_folder_bin(folder)
#'
#' var2 = var1 %>% group_by(trace_id) %>% mutate(max_range = max(p_mean)-min(p_mean)) %>%
#'   group_by(vessel) %>% mutate(maxcont = max_range == max(max_range))
#'
#' # Plot out that data
#' ggplot(var2) + geom_line(aes(x = frame_id, y = p_mean, color = as.factor(maxcont), group = ygroup)) + facet_wrap(~ vessel)
#'
#'
#' quant_folder = paste(folder, "\\Graphs", sep = "")
#' dir.create(quant_folder)
#'
#'
#' var2.1 = var2
#'
#' animal_trt = paste("Animal", unique(var2.1$animal), "Treatment", paste(unique(var2.1$treatment)), sep = "_")[[1]]
#'
#' overall_plot = ggplot(var2.1) + geom_line(aes(x = frame_id, y = p_mean, color = as.factor(ygroup), group = ygroup)) +
#'   labs(title = animal_trt) + facet_wrap(~ vessel)
#'
#' overall_plot_file = paste(quant_folder, "\\",animal_trt, "_overall.pdf", sep = "")
#'
#' ggsave(overall_plot_file, overall_plot, width = 297, height = 210, units = "mm")
#'
#' for (trace in unique(var2.1$trace_id))
#' {
#' local_data = subset(var2.1, var2.1$trace_id == trace)
#'
#' title_data = local_data %>% select(animal, treatment, site, vessel, ygroup) %>% distinct()
#' title = paste(paste(colnames(title_data), title_data, sep = "_"), collapse = ",")
#'
#' peak_data = find_peaks(input_vector = local_data$p_mean, min_dist = 100, kband = 50, min_change = 0.5, nups = 10)
#'
#' write.csv(peak_data[2], paste(quant_folder,"\\", title, "_peaks.csv", sep = ""))
#'
#' breaks = local_data %>% group_by(source_video) %>% summarise(max_frame = max(frame_id), min_frame = min(frame_id))
#' breaks$minima = min(local_data$p_mean)
#'
#' if(!is.null(peak_data[[1]]))
#' {
#'  graph_output = peak_data[[1]] + labs(title = title) + geom_vline(aes(xintercept = breaks$max_frame))
#' }
#' else
#' {
#'   graph_output = ggplot(local_data) + geom_line(aes(x = frame_id, y = p_mean)) + labs(title = title) + geom_vline(xintercept = breaks$max_frame)
#' }
#'
#' ggsave(paste(quant_folder,"\\", title, "_peaks.pdf", sep = ""), graph_output, width = 297, height = 210, units = "mm")
#'
#' }
#'
#' pdf_list = list.files(quant_folder, pattern = "_peaks.pdf", full.names = TRUE)
#'
#' pdf_combine(c(overall_plot_file, pdf_list), paste(folder, "\\", animal_trt,"_combined.pdf", sep = ""))
#'
#' }
#'
#'
#'
#' peak_data[[1]] + labs(title = title) + geom_vline(aes(xintercept = breaks$max_frame)) +
#'   geom_text(aes(x = breaks$min_frame, y = breaks$minima, label = breaks$source_video), angle = 90)
#'
