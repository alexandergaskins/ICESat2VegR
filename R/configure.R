#' Configure ICESat2VegR Python dependencies
#'
#' Installs the required Python packages (earthaccess, h5py, earthengine-api)
#' used by ICESat2VegR via reticulate.
#'
#' @export
ICESat2VegR_configure <- function() {
  reticulate::py_install(c("earthaccess", "h5py", "earthengine-api"))
}
