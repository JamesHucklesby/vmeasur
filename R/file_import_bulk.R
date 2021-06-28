#' Import a saved file
#'
#' @param filename the csv file to import from
#'
#' @return a cleaned up data frame
#'
#' @importFrom dplyr bind_rows mutate group_by ungroup
#' @importFrom tidyr extract_numeric
#' @importFrom utils read.csv
#' @importFrom stringr str_split str_remove
#' @importFrom readr parse_number
#'
#'
#' @export
#'
#' @examples
#' i = 1
#'
#'
import_file = function(filename)
{
  csvfile = read.csv(filename)
  csvfile$X = NULL
  csvfile$X.1 = NULL
  csvfile$file_id = NULL
  csvfile$frame = parse_number(str_remove(csvfile$filename, "test1"))

  # csvfile %>% select(y, excluded) %>% distinct()
  # toexclude = csvfile %>% group_by(y) %>% summarise(excluded = sum(excluded==1))

  csvfile = csvfile %>% group_by(`y`) %>%
    mutate(excluded = sum(excluded)>1) %>%
    ungroup() %>%
    mutate(p_width = ifelse(!`excluded`, `p_width`, NA))

  csvfile$filename = NULL

  filename = basename(tools::file_path_sans_ext(filename))

  csvfile$source_video = str_split(filename, "_")[[1]][[1]]
  csvfile$vessel = str_split(filename, "_")[[1]][[3]]

  animal_site = str_split(filename, "_")[[1]][[2]]

  csvfile$site = substr(animal_site, nchar(animal_site)-2, nchar(animal_site))

  animal_treatment = substr(animal_site,1, nchar(animal_site)-4)

  csvfile$animal = suppressWarnings(parse_number(animal_treatment))
  csvfile$treatment = str_remove(animal_treatment, as.character(parse_number(animal_treatment)))

  return(csvfile)

}



#' Parallel mass csv import code
#'
#' @param current_dir list of csv files to import
#' @param y_bin number of pixels to put in each ybin
#'
#'
#' @importFrom dplyr row_number
#'
#' @return A bulk list of imported csv files
#'
#' @export
#'
#' @examples
#'
import_folder_bin = function(current_dir, y_bin = 30)
{

  # list out the CSV files to import
  folder_files = list.files(current_dir, recursive = TRUE, pattern = "\\_widths.csv$", full.names = TRUE)

  # Import them all with lapply and combine with dplyr
  applied = lapply(folder_files, import_file)
  fulldata = dplyr::bind_rows(applied, .id = "file_id")


  fulldata = fulldata %>% group_by(y, vessel, site, animal, treatment) %>%
    mutate(frame_id = row_number())

  pixel_bin = y_bin

  fulldata_grouped = fulldata %>% mutate(ygroup = ((y-1) %/% pixel_bin) + 1)  %>%
    group_by(treatment, animal, site, vessel, ygroup) %>%
    mutate(max = max(y), min = min(y), npix = max-min +1, trace = cur_group_id()) %>%
    filter(npix == pixel_bin) %>%
    ungroup()

  fulldata_mean = fulldata_grouped %>% filter(!excluded) %>%
    group_by(`frame_id`, `frame`, `source_video`, `site`, `animal`, `treatment`, `vessel`, `ygroup`) %>%
    summarise(p_mean = mean(p_width, na.rm = TRUE), p_median = median(p_width, na.rm = TRUE)) %>%
    group_by(`site`, `animal`, `treatment`, `vessel`, `ygroup`) %>%
    mutate(trace_id = cur_group_id())

  return(fulldata_mean)

}






