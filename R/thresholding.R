#' Title
#'
#' @param file_path
#' @param threshold
#'
#' @return
#' @export
#'
#' @examples
threshold_image <- function(file_path, threshold)
{

  im <- load.image(file_path)

  # d <- as.data.frame(im)
  # ##Subsamble, fit a linear model
  # m <- d %>% lm(value ~ x*y,data=.)
  # ##Correct by removing the trend, and invert the image
  # im.c <- 1-(im-predict(m,d))
  # # Grab the red channel

  im.c = imsplit(im,"c")[[1]]
  hist(as.data.frame(im.c)$value)

  px = im.c<threshold
  px = imager::clean(px,2)
  plot(px)

  # Find the biggest patch of detected vessel, and isolate it
  # spl <- split_connected(px)
  # # biggest_region = which.max(unlist(lapply(spl, sum)))
  # # region = spl[[biggest_region]] == 1

  if(sum(px)>0)
  {
    pxconn = split_connected(px) %>% purrr::discard(~ sum(.) < 100)
  } else
  {
    pxconn = list()
  }

  if(length(pxconn)>1)
  {
    region =  pxconn %>% parany %>% plot
  } else
  {
    region = px
  }


  hist(as.data.frame(grayscale(im))$value)

  maximal <- (im.c>0.99)
  maximal = grow(maximal, 10)

  overlaid_regions = region+maximal

  # Draw out

  boundary_lines = imappend(imlist(imager::boundary(region)==2,imager::boundary(region), maximal), "c")

  imagelist = imlist(as.cimg(boundary_lines), im)

  overlap_image = parmax(imagelist)

  plot(overlap_image)


  overlapping = as.data.frame(overlaid_regions)
  yoverlap = overlapping %>% group_by(y) %>% summarise(bubble = max(value)==2)

  output = as.data.frame(as.cimg(region))
  widths = output %>% group_by(y) %>% summarise(p_width = sum(value))

  widths$excluded = yoverlap$bubble
  widths$filename = basename(file_path_sans_ext(file_path))

  return(list(overlap_image, widths))

}





#' Title
#'
#' @param threshold
#' @param roi_name
#' @param video_path
#' @param radians
#' @param xlength
#' @param ylength
#' @param xstart
#' @param ystart
#'
#' @return
#' @export
#'
#' @examples
threshold_apply = function(threshold = 0.5, roi_name = "test", video_path = '\\\\files.auckland.ac.nz\\research\\ressci202000061-PROM-study\\Version 5\\image826.avi',radians = 0.217604550320612,xlength = 60,ylength = 243,xstart = 696,ystart = 323)
{

  ylength = ylength + ylength %% 2
  xlength = xlength + xlength %% 2


directory = scratch_dir()
video_name = paste(basename(file_path_sans_ext(video_path)),"_",roi_name, sep = "")

video_folder = dirname(video_path)

temp_path = paste(directory, "/", video_name, sep = "")
dir.create(temp_path)

filter_string = paste("rotate = '",radians,":out_w=rotw(",radians,"):out_h=roth(",radians,"):c = red',",
                      "crop=",xlength,":",ylength,":",xstart,":",ystart,"",
                      sep = "")

av_encode_video(video_path, paste(temp_path, "/test1w%03d.png", sep = ""), vfilter = filter_string, codec = "png")

# av_video_images(paste(temp_path, "/croprotate.avi", sep = ""), destdir = temp_path, format = "png", vfilter = filter_string)

file_list = list.files(temp_path, full.names = TRUE, pattern = "\\.png$")

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

    processed_image = threshold_image(i, threshold)

    file_path = i

    save_image_path = paste(file_path_sans_ext(file_path), "_overlaid.png", sep = "")
    save_csv_path = paste(file_path_sans_ext(file_path), "_overlaid.csv", sep = "")

    save.image(processed_image[[1]], save_image_path)
    write.csv(processed_image[[2]], save_csv_path)
  }

close(pb)
stopCluster(cl)

file_list = list.files(temp_path, full.names = TRUE, pattern = "\\overlaid.png$")
av_encode_video(file_list, output = paste(temp_path, "/overlaid.avi", sep = ""),codec = "libx264", verbose = 24)

file_list = list.files(temp_path, full.names = TRUE, pattern = "\\overlaid.csv$")
ldf <- lapply(file_list , read.csv)
df.final <- do.call("rbind", ldf)

write.csv(df.final, paste(temp_path, "/widths.csv", sep = ""))

file.copy(paste(temp_path, "/overlaid.avi", sep = ""), paste(video_folder,"/",video_name,"_overlaid.avi", sep = ""))
file.copy(paste(temp_path, "/widths.csv", sep = ""), paste(video_folder,"/",video_name,"_widths.csv", sep = ""))

unlink(temp_path, recursive = TRUE)

}
