#' Quantify multiple animals at the same time
#'
#' @param working_folder The folder containing the completed analysis
#'
#' @importFrom future plan multisession
#' @importFrom doFuture registerDoFuture
#' @importFrom progressr handlers handler_progress with_progress progressor
#' @importFrom foreach foreach %dopar%
#' @importFrom progressr progressor
#'
#' @return
#' @export
#'
#' @examples
quantify_folders = function(working_folder = choose.dir())
{

folders_to_process = list.dirs(working_folder, recursive = FALSE)

# graph_folder(folders_to_process[1])


registerDoFuture()
plan(multisession)

handlers(list(
  handler_progress(
    format   = ":spin :current/:total (:message) [:bar] :percent in :elapsed ETA: :eta"
  )))

with_progress({
  p <- progressor(steps = length(folders_to_process))
  y <- foreach(x = folders_to_process) %dopar% {
    quantify_folder(x)
    p(message = sprintf("Completed %s", x), class = "sticky")
    gc()
  }
})

}




#' Quantify all ROI in a single folder
#'
#' @param folder The folser to quantify
#'
#' @return
#' @export
#'
#' @importFrom pdftools pdf_combine
#' @importFrom ggplot2 aes facet_wrap ggsave
#' @importFrom readr write_csv
#' @importFrom dplyr select distinct
#' @importFrom utils choose.dir
#'
#'
#' @examples
quantify_folder = function(folder)
{
# Import a whole folder of data, binned into ygroups

var1 = import_folder_bin(folder)

var2 = var1 %>% group_by(trace_id) %>% mutate(max_range = max(p_mean)-min(p_mean)) %>%
  group_by(vessel) %>% mutate(maxcont = max_range == max(max_range))

# Plot out that data
ggplot(var2) + geom_line(aes(x = frame_id, y = p_mean, color = as.factor(maxcont), group = ygroup)) + facet_wrap(~ vessel)


quant_folder = paste(folder, "\\Graphs", sep = "")
dir.create(quant_folder)


var2.1 = var2

animal_trt = paste("Animal", unique(var2.1$animal), "Treatment", paste(unique(var2.1$treatment)), sep = "_")[[1]]

overall_plot = ggplot(var2.1) + geom_line(aes(x = frame_id, y = p_mean, color = as.factor(ygroup), group = ygroup)) +
  labs(title = animal_trt) + facet_wrap(~ vessel)

overall_plot_file = paste(quant_folder, "\\",animal_trt, "_overall.pdf", sep = "")
overall_data_file = paste(folder, "\\",animal_trt, "_overall.csv", sep = "")

ggsave(overall_plot_file, overall_plot, width = 297, height = 210, units = "mm")
write_csv(var2, overall_data_file)

for (trace in unique(var2.1$trace_id))
{
local_data = subset(var2.1, var2.1$trace_id == trace)

title_data = local_data %>% select(animal, treatment, site, vessel, ygroup) %>% distinct()
title = paste(paste(colnames(title_data), title_data, sep = "_"), collapse = ",")

peak_data = find_peaks(input_vector = local_data$p_mean, min_dist = 100, kband = 50, min_change = 0.5, nups = 10)

write.csv(peak_data[2], paste(quant_folder,"\\", title, "_peaks.csv", sep = ""))

breaks = local_data %>% group_by(source_video) %>% summarise(max_frame = max(frame_id), min_frame = min(frame_id))
breaks$minima = min(local_data$p_mean)

if(!is.null(peak_data[[1]]))
{
 graph_output = peak_data[[1]] + labs(title = title) + geom_vline(aes(xintercept = breaks$max_frame))
}
else
{
  graph_output = ggplot(local_data) + geom_line(aes(x = frame_id, y = p_mean)) + labs(title = title) + geom_vline(xintercept = breaks$max_frame)
}

ggsave(paste(quant_folder,"\\", title, "_peaks.pdf", sep = ""), graph_output, width = 297, height = 210, units = "mm")

}

pdf_list = list.files(quant_folder, pattern = "_peaks.pdf", full.names = TRUE)


pdf_combine(c(overall_plot_file, pdf_list), paste(folder, "\\", animal_trt,"_combined.pdf", sep = ""))


folder_files = list.files(quant_folder, recursive = TRUE, pattern = "\\_peaks.csv$", full.names = TRUE)

import_csv_with_source = function(csv_file)
{
  dataframe = read.csv(csv_file)
  dataframe$source_file = file_path_sans_ext(basename(csv_file))
  dataframe$X1 = NULL
  return(dataframe)
}


# Import them all with lapply and combine with dplyr
applied = lapply(folder_files, import_csv_with_source)
contraction_data = dplyr::bind_rows(applied, .id = "file_id")

overall_contraction_file = paste(folder, "\\",animal_trt, "_contractions.csv", sep = "")

write.csv(contraction_data, overall_contraction_file)


}


# list out the CSV files to import



#'
#'
#' peak_data[[1]] + labs(title = title) + geom_vline(aes(xintercept = breaks$max_frame)) +
#'   geom_text(aes(x = breaks$min_frame, y = breaks$minima, label = breaks$source_video), angle = 90)
#'
