#' @importFrom yaml yaml.load_file
.onLoad <- function(libname, pkgname) {
  config_file <- file.path(getwd(), "_sinewconfig.yml")
  
  if (file.exists(config_file)) {
    config <- yaml::yaml.load_file(config_file)
    if('add_fields'%in%names(config)){
      sinew_opts$append(config['add_fields'])
      config[['add_fields']]<-NULL
      }
    if(length(config)>0) sinew_opts$set(config)
  }
  
}

.onAttach <- function(libname, pkgname) {
  config_file <- file.path(getwd(), "_sinewconfig.yml")

  if (file.exists(config_file)){
  packageStartupMessage("Setting sinew_opts to _sinewconfig.yml specs\nCurrent add_fields: ",
                        paste0(sinew_opts$get('add_fields'),collapse=',')
                        )
    }
}