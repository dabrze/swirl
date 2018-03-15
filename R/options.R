#' Set global options for swirl
#' 
#' @description
#' Options can be set for swirl that have an effect on swirl's behavior. Options
#' are specified by the \code{name} of an option that corrsponds to the 
#' \code{value} of that option.
#' 
#' @param ... Any options can be defined using \code{name = value}.
#' @export
#' @examples
#' \dontrun{
#' 
#' # Install courses to the current directory
#' set_swirl_options(courses_dir = getwd())
#' 
#' # Install courses in the default course directory
#' set_swirl_options(courses_dir = file.path(system.file("Courses", package = "swirl.pl")))
#' }
set_swirl_options <- function(...){
  args <- list(...)
  if(length(args) == 0){
    stop("Podaj argumenty do ustawienia opcji.")
  }
  
  # Create new options frame
  args_vector <- unlist(args)
  new_opts <- data.frame(
    name = names(args_vector),
    value = as.vector(args_vector))
  
  # If the options file does not exist, create it
  # else update the old options file
  if(!file.exists(opts_path())){
    write.csv(new_opts, file = opts_path(), row.names = FALSE)
  } else {
    old_opts <- read.csv(opts_path(), stringsAsFactors = FALSE, 
                                  header = TRUE)
    new_opts_file <- rbind(new_opts, old_opts[!(old_opts$name %in% new_opts$name),])
    new_opts_file$name <- as.character(new_opts_file$name)
    new_opts_file$value <- as.character(new_opts_file$value)
    new_opts_file <- new_opts_file[order(new_opts_file$name),]
    write.csv(new_opts_file, file = opts_path(), row.names = FALSE)
  }
  #message("Opcje ustawione!")
  invisible()
}

#' Get a global swirl option
#' 
#' @description
#' Returns the \code{value} of a swirl option by providing its \code{name}.
#' 
#' @param name The \code{name} of the swirl option.
#' @export
#' @examples
#' \dontrun{
#' 
#' get_swirl_option("courses_dir")
#' }
get_swirl_option <- function(name){
  opts <- read.csv(opts_path(), stringsAsFactors = FALSE, 
                       header = TRUE)
  if(name %in% opts$name){
    opts[name == opts$name,]$value
  } else {
    stop(paste0("Option name '", name, "' not found."))
  }
}

#' Delete a global swirl option
#' 
#' @description
#' Deletes the \code{name} and \code{value} of a swirl option by providing 
#' its \code{name}.
#' 
#' @param name The \code{name} of the swirl option to be deleted.
#' @export
#' @examples
#' \dontrun{
#' 
#' delete_swirl_option("Brians_phone_number")
#' }
delete_swirl_option <- function(name){
  opts <- read.csv(opts_path(), stringsAsFactors = FALSE, 
                   header = TRUE)
  if(name %in% opts$name){
    opts <- opts[name != opts$name,]
    write.csv(opts, file = opts_path(), row.names = FALSE)
  } else {
    stop(paste0("Opcja '", name, "' nie znaleziona."))
  }
  message(paste0("Opcja '", name, "' usunięta z powodzeniem!"))
  invisible()
}

# Get the options file path
opts_path <- function(){
  # Find user data directory
  udd <- file.path(find.package("swirl.pl"), "user_data")
  
  # If the directory doesn't exist, create it
  if(!file.exists(udd)){
    dir.create(udd, recursive = TRUE)
  }
  
  # Construct path to swirl options
  file.path(udd, "swirl_options.csv")
}