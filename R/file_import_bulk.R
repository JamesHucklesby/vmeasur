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
  csvfile$frame = extract_numeric(csvfile$filename)-1000

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

  csvfile$animal = parse_number(animal_treatment)
  csvfile$treatment = str_remove(animal_treatment, as.character(parse_number(animal_treatment)))

  return(csvfile)

}



#' Parallel mass csv import code
#'
#' @param csv_files list of csv files to import
#'
#' @importFrom parallel makeCluster stopCluster
#' @importFrom parallel detectCores
#' @importFrom doParallel registerDoParallel
#' @importFrom pbmcapply progressBar
#' @importFrom utils setTxtProgressBar
#' @importFrom foreach `%dopar%` foreach
#'
#' @return A bulk list of imported csv files
#'
#' @export
#'
#' @examples
#'
#' # Test folder to come
#'
#'
import_all_folder = function(csv_files)
{

cl <- makeCluster(detectCores()-1)
registerDoParallel(cl)
iterations <- length(csv_files)
pb <- progressBar(max = iterations, style = "ETA")
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)
result <- foreach(i = 1:iterations, .options.snow = opts, .verbose  = FALSE) %dopar%
  {
      vmeasur::import_file(csv_files[[i]])
  }

close(pb)
stopCluster(cl)

fulldata = bind_rows(result, .id = "file_id")

return(fulldata)

}






