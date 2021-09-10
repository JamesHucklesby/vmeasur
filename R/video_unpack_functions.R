#' Title
#'
#' @param video_path
#'
#' @return
#' @export
#'
#' @examples
unpack_video = function(video_path)
{

  file_changetime = file.mtime(video_path) %>% str_replace_all(":", " ")
  file_changetime = paste(basename(file_path_sans_ext(video_path))," ", file_changetime, sep = "")

  scratch = paste(scratch_dir(), "/", file_changetime, "/", sep = "")

  if(!dir.exists(scratch))
  {
    dir.create(scratch)
    av::av_encode_video(video_path, paste(scratch, "/%03d.png", sep = ""), codec = "png")
  }

  image_list = list.files(scratch, full.names = TRUE)

  return(image_list)
}

#' Title
#'
#' @param image_list
#'
#' @return
#' @export
#'
#' @examples
remove_unpacked_video = function(image_list)
{
  temporary_directory =  unique(dirname(image_list))
  unlink(temporary_directory, recursive = TRUE, force = TRUE)
}
