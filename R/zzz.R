.onLoad <- function(libname, pkgname){
  if(!file.exists(opts_path())){
    set_swirl_options(courses_dir = file.path(system.file("Courses", package = "swirl.pl")))
  }
  invisible()
}

.onAttach <- function(...) {
  if(length(ls(envir=globalenv())) > 0) {
    packageStartupMessage(
      make_pretty("Hej! Widzê ¿e masz jakieœ zmienne zapisane w swoim obszarze roboczym.",
      "Aby wszystko posz³o g³adko, proponujê abyœ wyczyszci³ œrodowisko",
      "przed uruchomieniem swirla.", skip_after=TRUE),
      make_pretty("Wpisz ls(), aby zobaczyæ listê zmiennych w swoim obszarze roboczym.",
      "Nastêpnie wpisz rm(list=ls()) aby wyczyœciæ obszar roboczy.", skip_after=TRUE),
      make_pretty("Wpisz swirl(), gdy bêdziesz gotowy.", skip_after=TRUE)
    )
  } else {
    packageStartupMessage(
      make_pretty("Witaj! Wpisz swirl(), gdy bêdziesz gotowy.",
                  skip_after=TRUE)
    )
  }
  invisible()
}

make_pretty <- function(..., skip_before=TRUE, skip_after=FALSE) {
  wrapped <- strwrap(str_c(..., sep = " "),
                     width = getOption("width") - 2)
  mes <- str_c("| ", wrapped, collapse = "\n")
  if(skip_before) mes <- paste0("\n", mes)
  if(skip_after) mes <- paste0(mes, "\n")
  mes
}