#' Load YAML file config
#'
#' @param yaml_name (chr) String file name
#'
#' @return (list) Contents of the config file
#'
#' @keywords internal
load_yaml_config <- function(yaml_name) {
  yaml_path <- system.file(yaml_name, package = "NHSEstats")

  if (yaml_path == "") stop(yaml_name, " not found in NHSEstats files.")

  yaml::read_yaml(yaml_path)
}
