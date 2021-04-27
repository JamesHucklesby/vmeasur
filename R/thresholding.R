#' Threshold an image
#'
#' @param file_path Path to the image for quantification
#' @param threshold Threshold to set for the inverse channel. Range 0-1.
#'
#' @return A list containing 1) the processed threshold image and 2) calculated widths
#'
#' @export
#'
#' @examples
#' # No examples
#'
threshold_image <- function(file_path, threshold, min_area = 100)
{

  # Load the image and split it into it's channels
  im <- imager::load.image(file_path)
  im.c = imager::imsplit(im,"c")[[1]]

  # Histogram plot if needed for debugging
  #hist(as.data.frame(im.c)$value)

  # Threshold the image

  px = im.c<threshold
  px = imager::clean(px,2)
  # plot(px)

  # If pixels have been located, split them into contiguous areas of greater than 100px in size

  if(sum(px)>0)
  {
    pxconn = imager::split_connected(px) %>% purrr::discard(~ sum(.) < min_area)
  } else
  {
    pxconn = list()
  }

  # If there is more than one area that passes, stick them together into a single mask

  if(length(pxconn)>1)
  {
    region =  pxconn %>% imager::parany %>% imager::plot
  } else
  {
    region = px
  }

  # Plot out the greyscale for debugging
  #hist(as.data.frame(grayscale(im))$value)


  # Find and expand the overlapped pixels
  maximal <- (im.c>0.99)
  maximal = imager::grow(maximal, 10)

  # Superimpose the two searches to find any overlap
  overlaid_regions = region+maximal

  # Combine the data into a visualisation
  boundary_lines = imager::imappend(imager::imlist(imager::boundary(region)==2,imager::boundary(region), maximal), "c")
  imagelist = imager::imlist(imager::as.cimg(boundary_lines), im)
  overlap_image = imager::parmax(imagelist)
  # plot(overlap_image)

  # Calculate overlaid pixels
  overlapping = as.data.frame(overlaid_regions)
  yoverlap = overlapping %>% dplyr::group_by(y) %>% dplyr::summarise(bubble = max(value)==2)

  # Calculate vessel widths
  output = as.data.frame(as.cimg(region))
  widths = output %>% dplyr::group_by(y) %>% dplyr::summarise(p_width = sum(value))

  # Unify data and add metadata
  widths$excluded = yoverlap$bubble
  widths$filename = basename(tools::file_path_sans_ext(file_path))

  return(list(overlap_image, widths))

}



#' Thresold a video with pre-determined parameters
#'
#' Using pre-determined values this function generates ROI from a video. If parameters are not known, use select_roi()
#' This function is optimized to run in parallel, so should be relatively rapid. If running slowly, check the scratch disk is set correctly.
#'
#' @param threshold The threshold for the red channel. Range 0-1.
#' @param roi_name Name assigned to the region of interest
#' @param video_path Locaton of the video file to process
#' @param radians Degrees to rotate the image, in radians
#' @param xlength Number of x pixels in the ROI
#' @param ylength Number of y pixels in the ROI
#' @param xstart ROI starting x co-ordinate
#' @param ystart ROI starting y co-ordinate
#'
#'
#' @return Saves the quantified CSV and overlaid video in the same directory as the video
#'
#' @importFrom snow makeCluster stopCluster
#' @importFrom doSNOW registerDoSnow
#' @importFrom pbmcapply progressBar
#' @importFrom utils setTxtProgressBar
#' @importFrom foreach `%dopar%` foreach
#' @importFrom tools file_path_sans_ext write.csv read.csv
#' @importFrom magrittr `%>%`
#' @importFrom imager as.cimg
#'
#'
#' @export
#'
#' @examples
threshold_apply = function(threshold = 0.5, roi_name = "test", video_path = 'image826.avi',radians = 0.217604550320612,xlength = 60,ylength = 242,xstart = 696,ystart = 323, image_list = NULL)
{


directory = scratch_dir()
video_folder = dirname(video_path)

video_name = paste(basename(tools::file_path_sans_ext(video_path)),"_",roi_name, sep = "")



if(!is.null(image_list))
{
  file_list = image_list
  temp_path = dirname(file_list[1])
}
else
{
  temp_path = paste(directory, "/", video_name, sep = "")
  dir.create(temp_path)

  filter_string = paste("rotate = '",radians,":out_w=rotw(",radians,"):out_h=roth(",radians,"):c = red',",
                        "crop=",xlength,":",ylength,":",xstart,":",ystart,"",
                        sep = "")

  av::av_encode_video(video_path, paste(temp_path, "/test1w%03d.png", sep = ""), vfilter = filter_string, codec = "png")


  file_list = list.files(temp_path, full.names = TRUE, pattern = "\\.png$")
}

library(doSNOW)
library(progress)
library(parallel)

cl <- makeCluster(16)
registerDoSNOW(cl)
iterations <- length(file_list)
pb <- progressBar(min = 0, max = iterations, initial = 0, style = "ETA", substyle = NA,
                  char = "=", width = NA, file = "")
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

result <- foreach(i = file_list, .combine = rbind,
                  .options.snow = opts, .export = "threshold_image") %dopar%
  {
    library(imager)
    library(dplyr)
    library(tools)

    processed_image = vmeasur::threshold_image(i, threshold)

    file_path = i

    save_image_path = paste(file_path_sans_ext(i), "_overlaid.png", sep = "")
    save_csv_path = paste(file_path_sans_ext(i), "_overlaid.csv", sep = "")

    save.image(processed_image[[1]], save_image_path)
    write.csv(processed_image[[2]], save_csv_path)
  }

close(pb)
stopCluster(cl)

file_list = list.files(temp_path, full.names = TRUE, pattern = "\\overlaid.png$")
av::av_encode_video(file_list, output = paste(temp_path, "/overlaid.avi", sep = ""),codec = "libx264", verbose = 24)

file_list = list.files(temp_path, full.names = TRUE, pattern = "\\overlaid.csv$")
ldf <- lapply(file_list , read.csv)
df.final <- do.call("rbind", ldf)

write.csv(df.final, paste(temp_path, "/widths.csv", sep = ""))

file.copy(paste(temp_path, "/overlaid.avi", sep = ""), paste(video_folder,"/",video_name,"_overlaid.avi", sep = ""))
file.copy(paste(temp_path, "/widths.csv", sep = ""), paste(video_folder,"/",video_name,"_widths.csv", sep = ""))

unlink(temp_path, recursive = TRUE)

}
