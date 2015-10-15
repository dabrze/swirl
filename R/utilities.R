utilities.env <- new.env()
assign('convertEncodingToCp1250', TRUE, envir=utilities.env)

swirl_convert_encoding <- function(convert) {
  assign('convertEncodingToCp1250', convert, envir=utilities.env)
}

convert_encoding <- function(x){
  if (get('convertEncodingToCp1250', envir=utilities.env)) {
	  iconv(x, "UTF-8", "windows-1250")
  } else {
    x
  }
}

swirl_out <- function(..., skip_before=TRUE, skip_after=FALSE) {
  wrapped <- strwrap(paste(..., sep = " "),
                     width = getOption("width") - 2)
  mes <- paste("| ", wrapped, collapse = "\n")
  if(skip_before) mes <- paste0("\n", mes)
  if(skip_after) mes <- paste0(mes, "\n")
  Encoding(mes) <- "UTF-8"
  mes <- convert_encoding(mes)
  Encoding(mes) <- "windows-1250"
  message(mes)
}

swirl_readline <- function(prompt = "") {
  readline(convert_encoding(prompt))
}

swirl_select.list <- function(choices, preselect = NULL, multiple = FALSE,
                              title = NULL, graphics = getOption("menu.graphics")) {
	if (is.null(title)){ 
		select.list(convert_encoding(choices), preselect, multiple, NULL, graphics)
	} else {
		select.list(convert_encoding(choices), preselect, multiple, convert_encoding(title), graphics)
	}
}

# Takes a plain English name and turns it into a more proper 
# file or directory name
make_pathname <- function(name) {
  gsub(" ", "_", trimws(name))
}

xfer <- function(env1, env2){
  if(length(ls(env1))==0)return()
  lapply(ls(env1), function(var)getAssign(var, env1, env2))
}

getAssign <- function(var, env1, env2){
  assign(var, get(var, env1, inherits=FALSE), envir=env2)
}

cleanAdmin <- function(){
  udat <- file.path(find.package("swirl.pl"), "user_data", "swirladmin")
  file.remove(dir(udat, pattern="*[.]rda", full.names=TRUE))
  invisible()
}

mergeLists <- function(sourceList, destList){
 for (n in names(sourceList)){
   destList[[n]] <- sourceList[[n]]
 }
 return(destList)
}

# Evaluates a user's expression in a clean environment
# whose parent is a snapshot of the previous official 
# environment, i.e., the same environment in which
# the user entered the expression. Any values required
# for evaluation will be found in the snapshot. Any variables
# changed or created by the expression will appear in the 
# clean environment, even if nothing changes in the global. 
#
# For example, if x already has the value c(1, 2, 3) and
# the user enters x <- c(1, 2, 3), nothing will change
# in the global environment, but x with the value c(1, 2, 3)
# will appear in the clean environment.
#
# In case the user's expression involves random numbers, the
# values of variables which appear in the clean environment
# are copied from the global environment.
# 
# For example, if the user enters x <- rnorm(100), then
# evaluating the expression in a clean environment will create
# a variable named x, but it will have a different value
# than that created by the user.
#
safeEval <- function(expr, e){
  e1 <- cleanEnv(e$snapshot)
  ans <- list()
  temp <- capture.output(
    try(suppressMessages(suppressWarnings(eval(expr,e1))), silent=TRUE)
    )
  if(is(temp, "try-error"))return(ans)
  for (x in ls(e1)){
    if(exists(x,globalenv()))
      ans[[x]] <- get(x,globalenv())
  }
  return(ans)
}

# Creates a clean environment whose parent is
# a snapshot of the official environment in an
# earlier state. The snapshot itself is given the
# same parent as the global environment, which will
# consist of loaded namespaces and packages.
#
# Environments in R are subject to reference semantics, 
# i.e., all references refer to the same copy. Hence, 
# the state of an environment cannot be saved for later 
# comparison merely by creating a second reference. Any 
# change in the environment will affect all references. 
# Lists, however, have R's usual copy-on-modify semantics. 
# If snapshot <- as.list(globalenv()), a subsequent change 
# in the global environment will not cause a change in 
# the list (with the exotic exception of environments
# contained in the global environment.)
# 
# Clean environments can be used to detect variables 
# changed or created by a user, as in function safeEval.
# They can also be used to check the correctness of
# a value computed by a user.
#
# For example, if the user enters x <- 2*x, then the
# value of x in the global environment will have changed,
# but if the expression is evaluated in a clean environment
# the value of x on the right will be found in the snapshot
# hence will be the same as that found by the user. 
#
cleanEnv <- function(snapshot){
  # clone of snapshot
  pe <- if(length(snapshot) > 0){
    as.environment(as.list(snapshot))
  } else {
    new.env()  
  }
  parent.env(pe) <- globalenv()
  # return new environment whose parent is pe
  return(new.env(parent=pe))
}


# LESSON PACKAGE DEPENDENCY SUPPORT

# Load lesson package dependencies quietly
loadDependencies <- function(lesson_dir) {
  depends <- file.path(lesson_dir, "dependson.txt")
  if(file.exists(depends)) {
    packages_as_chars <- setdiff(readLines(depends, warn=FALSE), "")
    # If the dependson file is empty, then proceed with lesson
    if(length(packages_as_chars) == 0) return(TRUE)
    swirl_out("Próbuję załadować zależności potrzebne do realizacji lekcji...")
    for(p in packages_as_chars) {
      p <- gsub("^\\s+|\\s+$", "", p) # trim leading and trailing whitespace 
      if(suppressPackageStartupMessages(
        suppressWarnings(
          suppressMessages(require(p, character.only=TRUE, quietly=TRUE))))) {
        swirl_out("Paczka", sQuote(p), "została poprawnie załadowana!")
      } else {
        swirl_out("Ta lekcja wymaga paczki", sQuote(p), 
                  ". Czy mam ją zainstalować za Ciebie?")
        yn <- swirl_select.list(choices=c("Tak", "Nie"), graphics=FALSE)
        if(yn == "Tak") {
          swirl_out("Probuję zainstalowac paczkę", sQuote(p), "...")
          install.packages(p, quiet=TRUE)
          if(suppressPackageStartupMessages(
            suppressWarnings(
              suppressMessages(require(p, 
                                       character.only=TRUE, 
                                       quietly=TRUE))))) {
            swirl_out("Paczka", sQuote(p), "ostała poprawnie załadowana!")
          } else {
            swirl_out("Nie mogłem zainstalować paczki", paste0(sQuote(p), "!"))
            return(FALSE)
          }
        } else {
          return(FALSE)
        }
      }
    }
  }
  # If loop completes, then print a blank line and return TRUE
  cat("\n")
  return(TRUE)
}

# Execute correct answers for rows 1 through 'up_through' of lesson
complete_part <- function(e) {
  up_through <- e$test_from - 1
  # Get rows though 'up_through' argument
  les <- e$les[seq(up_through), ]
  # Execute previous correct answers in global env
  exec_cmd <- function(row) {
    if(row['Class'] == "cmd_question") {
      eval(parse(text = row['CorrectAnswer']), envir=globalenv())
    } else if(row['Class'] == "script") {
      orig_script_name <- row['Script']
      correct_script_name <- paste0(
        tools::file_path_sans_ext(orig_script_name), "-correct.R")
      correct_script_path <- file.path(e$path, "scripts", 
                                       correct_script_name)
      if(file.exists(correct_script_path)) {
        try(source(correct_script_path))
      } else {
        stop("Nie znaleziono poprawnego skryptu pod ścieżką ", correct_script_path)
      }
    }
  }
  message("Uzupełniam pierwszą część lekcji za Ciebie...\n")
  apply(les, 1, exec_cmd)
  invisible()
}
