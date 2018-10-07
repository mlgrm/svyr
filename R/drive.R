#' wrapper for drive_download to allow recursive downloading of directories
#' 
#' @export
drive_download_recursive <- function(f,...){
  if(is_folder(drive_get(f))){
    dir.create(f)
    aaply(drive_ls(f), drive_download_recursive, ..., .margins = 1)
  } else drive_download(f)
}