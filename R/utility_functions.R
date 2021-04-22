#' Title
#'
#' @param file_path
#'
#' @return
#' @export
#'
#' @examples
calculate_auto_threshold = function(file_path)
{
  im = load.image(file_path)
  im.c = imsplit(im,"c")[[1]]

  threshold_mask = as.data.frame(as.cimg(threshold(im.c)))
  im.c.df = as.data.frame(im.c)

  im.c.df$masked = threshold_mask$value

  auto_threshold = min(subset(im.c.df, im.c.df$masked>0)$value)

  return(auto_threshold)

}


#' Title
#'
#' @param img
#' @param xstart
#' @param ystart
#' @param xlength
#' @param ylength
#'
#' @return
#' @export
#'
#' @examples
crop_dims = function(img, xstart, ystart, xlength, ylength)
{
  img.df = as.data.frame(img)
  img.df.c = img.df %>% filter(img.df$x>xstart,img.df$y>ystart,img.df$x<xstart+xlength,img.df$y<ystart+ylength)
  toreturn = autocrop(as.cimg(img.df.c))
  return(toreturn)
}


#' Title
#'
#' @param output_list
#' @param width
#'
#' @return
#' @export
#'
#' @examples
make_matrix = function(output_list, width = 2)
{
  total_images = length(output_list)
  current_col = imlist()
  overall_matrix = imlist()

  current_width = 0

  for(i in 1:total_images)
  {
    currentimg = output_list[i][[1]]
    currentimg = pad(currentimg, 5, "xy")

    current_col = ci(current_col, currentimg)
    current_width = current_width + 1

    if(width == current_width || i == total_images)
    {
      current_col_image = imappend(current_col, "x")
      overall_matrix = ci(overall_matrix, current_col_image)
      current_col = imlist()
      current_width = 0
    }
  }


  returnimage = imappend(overall_matrix, "y")

  return(returnimage)
}


#' Title
#'
#' @param set
#'
#' @return
#' @export
#'
#' @examples
#' scratch_dir()
#' scratch_dir("R:")
#'
#'
scratch_dir = function(set = NULL)
{

  if(!is.null(set))
  {
    options("quantifyvessel-scratch_dir"= set)
  }

  if(is.null(unlist(options("quantifyvessel-scratch_dir"))))
  {
    return(tempdir())
  }
  else
  {
    return(options("quantifyvessel-scratch_dir")[[1]])
  }
}

