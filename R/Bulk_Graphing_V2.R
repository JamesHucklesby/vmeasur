#' Quantify multiple animals at the same time
#'
#' @param working_folder The folder containing the completed analysis
#'
#' @importFrom future plan multisession
#' @importFrom doFuture registerDoFuture
#' @importFrom progressr handlers handler_progress with_progress progressor
#' @importFrom foreach foreach %dopar%
#' @importFrom progressr progressor handlers
#'
#' @return saves a pdf of a series of quantified folders in the root directory
#'
#' @export
#'
#' @examples
#' # Used interactivley only
#'
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
#' @param folder The folder to quantify
#' @param kband The k value to use in smoothing the data
#'
#' @return Saves a PDF file of the analyasis generated
#'
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
#' # Select a folder and it will be quantified
#'
#'
quantify_folder = function(folder, kband = 40)
{


  var1 = import_folder_bin(folder)

  var2 = var1 %>% group_by(trace_id) %>% mutate(max_range = max(p_mean)-min(p_mean)) %>%
    group_by(roi) %>% mutate(maxcont = max_range == max(max_range))

  # Plot out that data
  ggplot(var2) + geom_line(aes(x = frame_id, y = p_mean, color = as.factor(maxcont), group = ygroup)) + facet_wrap(~ roi)



  var2.1 = var2

  animal_trt = paste("Animal", unique(var2.1$animal), "Treatment", paste(unique(var2.1$treatment)), sep = "_")[[1]]

  quant_folder = paste(scratch_dir(), "\\", hash(paste(folder, Sys.time())), sep = "")
  dir.create(quant_folder)

  folder_files = list.files(folder, recursive = TRUE, pattern = "\\_widths.csv$", full.names = TRUE)

  for(one_region in folder_files)
  {
    heat_plot = plot_heatmap(one_region)

    overall_plot_heat_file = paste(quant_folder, "\\",animal_trt, file_path_sans_ext(basename(one_region)), "_heatmap.pdf", sep = "")

    ggsave(overall_plot_heat_file, heat_plot, width = 297, height = 210, units = "mm")

  }



# Import a whole folder of data, binned into ygroups



overall_plot = ggplot(var2.1) + geom_line(aes(x = frame_id, y = p_mean, color = as.factor(ygroup), group = ygroup)) +
  labs(title = animal_trt) + facet_wrap(~ roi)

overall_plot_file = paste(quant_folder, "\\",animal_trt, "_overall.pdf", sep = "")
overall_data_file = paste(folder, "\\",animal_trt, "_overall.csv", sep = "")

ggsave(overall_plot_file, overall_plot, width = 297, height = 210, units = "mm")
write_csv(var2, overall_data_file)

for (trace in unique(var2.1$trace_id))
{
local_data = subset(var2.1, var2.1$trace_id == trace)

title_data = local_data %>% select(animal, treatment, roi, ygroup) %>% distinct()
title = paste(paste(colnames(title_data), title_data, sep = "_"), collapse = ",")

peak_data = find_peaks(input_vector = local_data$p_mean, min_dist = 100, kband = kband, min_change = 0.5, nups = 10)

write.csv(peak_data[2], paste(quant_folder,"\\", title, "_peaks.csv", sep = ""))

breaks = local_data %>% group_by(video) %>% summarise(max_frame = max(frame_id), min_frame = min(frame_id))
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

heatmap_list = list.files(quant_folder, pattern = "_heatmap.pdf", full.names = TRUE)
pdf_list = list.files(quant_folder, pattern = "_peaks.pdf", full.names = TRUE)


pdf_combine(c(heatmap_list,overall_plot_file, pdf_list), paste(folder, "\\", animal_trt,"_combined.pdf", sep = ""))


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

unlink(quant_folder)


}



#' Plot a heatmap showing a vessel's contraction over time
#'
#' @param heatmap_file a vmeasur file of vessel diameters over time
#'
#' @return a ggplot2 heatmap
#'
#' @export
#'
#' @importFrom ggplot2 ggplot geom_raster aes scale_y_reverse scale_x_continuous labs scale_fill_gradient
#'
#' @examples
#'
#' #heatmap_file = file.choose()
#' #plot_heatmap(heatmap_file)
plot_heatmap = function(heatmap_file)
{
  heatmap_data = read.csv(heatmap_file)

  heatmap = (ggplot(heatmap_data) + geom_raster(aes(x = as.numeric(filename), y = y, fill = p_width)) +
    scale_y_reverse(expand = c(0,0))+
    scale_x_continuous(expand=c(0,0))+
    labs(y = "Vessel Position (pixels from top of image)", x = "Frame", fill = "Vessel Diameter (pixels)") +
    scale_fill_gradient(low = "white", high = "blue"))

  return(heatmap)
}

#' Plot a line graph of a vessel's change in diameter over time
#'
#' @param widths_file a vmeasur created file showing the widths of the vessel over time
#'
#' @return a ggplot2 of the vessel
#'
#' @export
#'
#' @importFrom dplyr group_by summarise
#' @importFrom ggplot2 ggplot geom_line aes
#' @importFrom stats sd
#'
#' @examples
#'
#' #widths_file = file.choose()
#' #plot_heatmap(heatmap_file)
plot_line = function(widths_file)
{
  width_data = import_file(widths_file)

  width_data_summary = width_data %>% group_by(filename) %>% summarise(mean_diameter = mean(p_width, na.rm = TRUE), sd_diameter = sd(p_width, na.rm = TRUE))

  heatmap = ggplot(width_data_summary) + geom_line(aes(x = as.numeric(filename), y = mean_diameter))

  return(heatmap)
}


