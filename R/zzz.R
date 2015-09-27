.onLoad <- function(libname, pkgname){
  if(!file.exists(opts_path())){
    set_swirl_options(courses_dir = file.path(system.file("Courses", package = "swirl.pl")))
  }
  invisible()
}

.onAttach <- function(...) {
  if(length(ls(envir=globalenv())) > 0) {
    packageStartupMessage(
      make_pretty("Hej! Widzę że masz jakieś zmienne zapisane w swoim obszarze roboczym.",
      "Aby wszystko poszło gładko, proponuję abyś wyczyszcił środowisko",
      "przed uruchomieniem swirla.", skip_after=TRUE),
      make_pretty("Wpisz ls(), aby zobaczyć listę zmiennych w swoim obszarze roboczym.",
      "Następnie wpisz rm(list=ls()) aby wyczyścić obszar roboczy.", skip_after=TRUE),
      make_pretty("Wpisz swirl(), gdy będziesz gotowy.", skip_after=TRUE)
    )
  } else {
    packageStartupMessage(
      make_pretty("Witaj! Wpisz swirl(), gdy będziesz gotowy.",
                  skip_after=TRUE)
    )
  }
  invisible()
}

make_pretty <- function(..., skip_before=TRUE, skip_after=FALSE) {
  wrapped <- strwrap(paste(..., sep = " "),
                     width = getOption("width") - 2)
  mes <- paste("| ", wrapped, collapse = "\n")
  if(skip_before) mes <- paste0("\n", mes)
  if(skip_after) mes <- paste0(mes, "\n")
  iconv(mes, "UTF-8", "windows-1250")
}