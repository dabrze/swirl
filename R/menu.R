## Method declarations

mainMenu <- function(e, ...)UseMethod("mainMenu")
welcome <- function(e, ...)UseMethod("welcome")
chooseEncoding <- function(e, ...)UseMethod("chooseEncoding")
housekeeping <- function(e, ...)UseMethod("housekeeping")
inProgressMenu <- function(e, choices, ...)UseMethod("inProgressMenu")
courseMenu <- function(e, courses)UseMethod("courseMenu")
courseDir <- function(e)UseMethod("courseDir")
progressDir <- function(e)UseMethod("progressDir")
lessonMenu <- function(e, choices)UseMethod("lessonMenu")
restoreUserProgress <- function(e, selection)UseMethod("restoreUserProgress")
loadLesson <- function(e, ...)UseMethod("loadLesson")
loadInstructions <- function(e, ...)UseMethod("loadInstructions")

windows <- "Windows (CP1250)"
linux <- "Linux (UTF-8)"

# Default course and lesson navigation logic
# 
# This method implements default course and lesson navigation logic, 
# decoupling menu presentation from internal processing of user
# selections. It relies on several methods for menu presentation,
# namely welcome(e), housekeeping(e), inProgressMenu(e, lessons),
# courseMenu(e, courses), and lessonMenu(e, lessons). Defaults 
# are provided.
# 
# @param e persistent environment accessible to the callback
#'@importFrom yaml yaml.load_file
mainMenu.default <- function(e){
  encodingOption <- tryCatch({
    get_swirl_option("encoding")
  }, error = function(e) {
    as.character(NA)
  })
  
  if (is.na(encodingOption)) {
    encodingOption <- chooseEncoding()
  }
  
  if(identical(encodingOption, windows)) {
    set_swirl_options(encoding = windows)
    swirl_convert_encoding(TRUE)
  } else if(identical(encodingOption, linux)) {
    set_swirl_options(encoding = linux)
    swirl_convert_encoding(FALSE)
  } else {
    swirl_convert_encoding(TRUE)
    swirl_out("Błędna wartość. Będę konwertował znaki pod system Windows.")
  }
  
  # Welcome the user if necessary and set up progress tracking
  if(!exists("usr",e,inherits = FALSE)){
    e$usr <- welcome(e)
    udat <- file.path(progressDir(e), e$usr)
    if(!file.exists(udat)){
      housekeeping(e)
      dir.create(udat, recursive=TRUE)
    }
    e$udat <- udat
  }
  # If there is no active lesson, obtain one.
  if(!exists("les",e,inherits = FALSE)){
    # First, allow user to continue unfinished lessons
    # if there are any
    pfiles <- inProgress(e)
    response <- ""
    if(length(pfiles) > 0){
      response <- inProgressMenu(e, pfiles)
    }
    if(response != "" ){
      # If the user has chosen to continue, restore progress
      response <- gsub(" ", "_", response)
      response <- paste0(response,".rda")
      restoreUserProgress(e, response)
    } else {
      # Else load a new lesson.
      # Let user choose the course.
      coursesU <- dir(courseDir(e))
      # Eliminate empty directories
      idx <- unlist(sapply(coursesU, 
                    function(x)length(dir(file.path(courseDir(e),x)))>0))
      coursesU <- coursesU[idx]
      
      # If no courses are available, offer to install one
      if(length(coursesU)==0){
        suggestions <- yaml.load_file(file.path(courseDir(e), "suggested_courses.yaml"))
        choices <- sapply(suggestions, function(x)paste0(x$Course, ": ", x$Description))
        swirl_out("Aby rozpocząć, musisz mieć zainstalowany jakiś moduł. Mogę zainstalować za Ciebie",
                  "moduł z Internetu albo wysłać Cię na stronę internetową z modułami.",
                  "(Jeśli nie jesteś połączony z Internetem, wpisz 0 aby wyjść.)")
        choices <- c(choices, "Niczego za mnie nie instaluj. Sam to zrobię.")
        choice <- swirl_select.list(choices, graphics=FALSE)
        n <- which(choice == choices)
        if(length(n) == 0)return(FALSE)
        if(n < length(choices)){
          repeat {
            temp <- try(eval(parse(text=suggestions[[n]]$Install)), silent=TRUE)
            if(is(temp, "try-error")){
              swirl_out("Przepraszam, nie byłem w stanie ściągnąć ", sQuote(choice),
                        ". Czy jesteś pewien, że jesteś połączony z Internetem?",
                        "Jesli tak, czy mam spróbować jeszcze raz czy wolisz odwiedzić",
                        "repozytorium i spróbować zainstalować moduł ręcznie? Wpisz 0, aby wyjść")
              ch <- c("Spróbuj jeszcze raz!", 
                      "Przenieś mnie na stronę repozytorium - spróbuję sam zainstalować moduł.")
              resp <- swirl_select.list(ch, graphics=FALSE)
              if(resp == "") return(FALSE)
              if(resp == ch[2]) {
                swirl_out("OK. Otwieram repozytorium modułów w Twojej przeglądarce.")
                browseURL("https://github.com/dabrze/swirl_courses")
                return(FALSE)
              }
            } else {
              break # Break repeat loop if install is successful
            }
          }
          coursesU <- dir(courseDir(e))
          # Eliminate empty directories
          idx <- unlist(sapply(coursesU, 
                               function(x)length(dir(file.path(courseDir(e),x)))>0))
          coursesU <- coursesU[idx]
        } else {
          swirl_out("OK. Otwieram repozytorium modułów w Twojej przeglądarce.")
          browseURL("https://github.com/dabrze/swirl_courses")
          return(FALSE)
        }
      }
      # path cosmetics
      coursesR <- gsub("_", " ", coursesU)
      lesson <- ""
      while(lesson == ""){
        course <- courseMenu(e, coursesR)
        if(!is.null(names(course)) && names(course)=="repo") {
          swirl_out("OK. Otwieram repozytorium modułów w Twojej przeglądarce.")
          browseURL("https://github.com/dabrze/swirl_courses")
          return(FALSE)
        }
        if(course=="")return(FALSE)
        # Set temp course name since csv files don't carry attributes
        e$temp_course_name <- course
        # reverse path cosmetics
        courseU <- coursesU[course == coursesR]
        course_dir <- file.path(courseDir(e), courseU)
        # Get all files/folders from course dir, excluding MANIFEST
        lessons <- dir(course_dir)
        lessons <- lessons[lessons != "MANIFEST"]
        # If MANIFEST exists in course directory, then order courses
        man_path <- file.path(course_dir, "MANIFEST")
        if(file.exists(man_path)) {
          manifest <- get_manifest(course_dir)
          lessons <- order_lessons(current_order=lessons, 
                                   manifest_order=manifest)
        }
        # Clean up lesson names
        lessons_clean <- gsub("_", " ", lessons)
        # Let user choose the lesson.
        lesson_choice <- lessonMenu(e, lessons_clean)
        # Set temp lesson name since csv files don't have lesson name attribute
        e$temp_lesson_name <- lesson_choice
        # reverse path cosmetics
        lesson <- ifelse(lesson_choice=="", "",
                         lessons[lesson_choice == lessons_clean])
        # Return to the course menu if the lesson failed to load
        if(lesson == ""){
          if(exists("les", e, inherits=FALSE)){
            rm("les", envir=e, inherits=FALSE)
          }
          lesson <- ""
          next()
        } else {
          # Load the lesson and intialize everything
          e$les <- loadLesson(e, courseU, lesson)
        }
      }
      # For sourcing files which construct figures etc
      e$path <- file.path(courseDir(e), courseU, lesson)
      # If running in 'test' mode and starting partway through 
      # lesson, then complete first part
      if((is(e, "test") || is(e, "datacamp")) && e$test_from > 1) {
        complete_part(e)
      }
      
      # Remove temp lesson name and course name vars, which were surrogates
      # for csv attributes -- they've been attached via lesson() by now
      rm("temp_lesson_name", "temp_course_name", envir=e, inherits=FALSE)
      
      # Initialize the progress bar
      if(!is(e,"datacamp")) {
        e$pbar <- txtProgressBar(style=3)
      }
      e$pbar_seq <- seq(0, 1, length=nrow(e$les))
      
      # expr, val, ok, and vis should have been set by the callback.
      # The lesson's current row - could start after 1 if in 'test' mode
      if(is(e, 'test') || is(e, 'datacamp')) {
        e$row <- e$test_from
      } else {
        e$row <- 1
      }
      # The current row's instruction pointer
      e$iptr <- 1
      # A flag indicating we should return to the prompt
      e$prompt <- FALSE
      # The job of loading instructions for this "virtual machine"
      # is relegated to an S3 method to allow for different "programs."
      loadInstructions(e)
      # An identifier for the active row
      e$current.row <- NULL
      # Set up paths and files to save user progress
      # Make file path from lesson info
      fname <- progressName(attr(e$les,"course_name"), attr(e$les,"lesson_name"))
      # path to file 
      e$progress <- file.path(e$udat, fname)
      # indicator that swirl is not reacting to console input
      e$playing <- FALSE
      # create the file
      suppressMessages(suppressWarnings(saveRDS(e, e$progress)))
      # post initialization message
      post_init(e)
    }
  }
  return(TRUE)
}

welcome.test <- function(e, ...){
  "author"
}

# Default version.
welcome.default <- function(e, ...){
  swirl_out("Witaj w programie swirl!")
  swirl_out("Zaloguj się. Jeśli już wcześniej korzystałeś z swirla na tym komputerze, użyj tej samej nazwy użytkownika co poprzednio.\nJeśli jesteś tu nowy, wymyśl jakąś unikalną nazwę uzytkownika.", skip_after=TRUE)
  resp <- swirl_readline("Jak mam Cię zwać? ")
  while(str_detect(resp, '[[:punct:]]')) {
    swirl_out("Proszę nie używaj cudzysłów, ani innych znaków interpunkcyjnych w nazwie użytkownika.",
              skip_after = TRUE)
    resp <- swirl_readline("Jak mam Cię zwać? ")
  }
  return(resp)
}

chooseEncoding.default <- function() {
  swirl_out("Na jakim systemie pracujesz?")
  selection <- swirl_select.list(c(windows, linux), graphics=FALSE)
  if(identical(selection, linux)) {
    swirl_convert_encoding(FALSE)
    swirl_out("OK. Będę tekst wysyłany na konsolę kodował w UTF-8.")
  } else {
    swirl_convert_encoding(TRUE)
    swirl_out("OK. Będę konwertował znaki, aby poprawnie wyświetlały się w konsoli R na Windowsie.")
  }
  
  return(selection)
}

readEncoding.default <- function() {
  
}

writeEncoding.default <- 

# Presents preliminary information to a new user
# 
# @param e persistent environment used here only for its class attribute
# 
housekeeping.default <- function(e){
  swirl_out(paste0("Dzięki, ", e$usr,". Zanim zaczniesz pierwszą lekcję, ustalmy kilka spraw. Po pierwsze kiedy widzisz '...', oznacza to, że możesz wcisnąć Enter, by iść dalej.\n"))
  swirl_readline("...  <-- Tak, to właśnie ten moment")
  swirl_out("Hello, and, again, welcome to the Aperture Science Computer-Aided Enrichment Center.\n")
  swirl_readline("...")
  swirl_out("Hmmm... nie wiem skąd to się wzięło...\n Tak czy inaczej, gdy zobaczysz 'ODPOWIEDŹ:', kursor konsoli R (>) lub gdy zostaniesz poproszony o wybranie elementu z listy, oznacza to, że nadszedł czas na wprowadzenie odpowiedzi i wcisnięcie Enter.")
  swirl_select.list(c("Dalej.", "Kontynuuj.", "Jedziemy, jedziemy!"),
              title="\nWybierz 1, 2, lub 3 i wciśnij Enter", graphics=FALSE)
  swirl_out("Możesz wyjść ze swirla i wrócić do konsoli R w dowolnej chwili - wystaczy wcisnąć Esc. Jeśli jesteś w trybie wprowadzania odpowiedzi, wpisz bye() by wyjść i zachować swój postęp w lekcji.")
  info()
  swirl_out("Zaczynamy!\n", skip_before=FALSE)
  swirl_readline("...")
}

housekeeping.test <- function(e){}

# A stub. Eventually this should be a full menu
inProgressMenu.default <- function(e, choices){
  nada <- "Nie"
  swirl_out("Czy chciałbyś kontynuować którąś z tych lekcji?")
  selection <- swirl_select.list(c(choices, nada), graphics=FALSE)
  # return a blank if the user rejects all choices
  if(identical(selection, nada))selection <- ""
  return(selection)
}

inProgressMenu.test <- function(e, choices) {
  ""
}

# A stub. Eventually this should be a full menu
courseMenu.default <- function(e, choices){
  repo_option <- "Przenieś mnie do repozytorium swirl.pl!"
  choices <- c(choices, repo = repo_option)
  swirl_out("Wybierz kurs... lub wpisz 0 aby wyjść.")
  return(swirl_select.list(choices, graphics=FALSE))
}

courseMenu.test <- function(e, choices) {
  e$test_course
}

# A stub. Eventually this should be a full menu
lessonMenu.default <- function(e, choices){
  swirl_out("Wybierz lekcję lub wpisz 0 by powrócić do menu.")
  return(swirl_select.list(choices, graphics=FALSE))
}

lessonMenu.test <- function(e, choices) {
  e$test_lesson
}

loadLesson.default <- function(e, courseU, lesson){
  # Load the content file
  lesPath <- file.path(courseDir(e), courseU, lesson)
  shortname <- find_lesson(lesPath)
  dataName <- file.path(lesPath,shortname)
  # Handle dependencies
  if(!loadDependencies(lesPath))return(FALSE)
  # Initialize list of official variables
  e$snapshot <- list()
  # initialize course lesson, assigning lesson-specific variables
  initFile <- file.path(lesPath,"initLesson.R")
  if(file.exists(initFile))local({
    source(initFile, local=TRUE)
    # NOTE: the order of the next two statements is important,
    # since a reference to e$snapshot will cause e to appear in
    # local environment.
    xfer(environment(), globalenv())
    # Only add to the "official record" if are auto-detecting new variables
    if(isTRUE(customTests$AUTO_DETECT_NEWVAR)) {
      e$snapshot <- as.list(environment())
    }
  })
  # load any custom tests, returning FALSE if they fail to load
  clearCustomTests()
  loadCustomTests(lesPath)
  
  # Attached class to content based on file extension
  class(dataName) <- get_content_class(dataName)
  
  # Parse content, returning object of class "lesson"
  return(parse_content(dataName, e))
}

restoreUserProgress.default <- function(e, selection){
  # read the progress file
  temp <- readRDS(file.path(e$udat, selection))
  # transfer its contents to e
  xfer(temp, e)
  # Since loadDepencies will have worked once, we don't
  # check for failure here. Perhaps we should.
  loadDependencies(e$path)
  # source the initLesson.R file if it exists
  initf <- file.path(e$path, "initLesson.R")
  if(file.exists(initf))local({
    source(initf, local=TRUE)
    xfer(environment(), globalenv())
  })
  # transfer swirl's "official" list of variables to the
  # global environment.
  if(length(e$snapshot)>0){
    xfer(as.environment(e$snapshot), globalenv())
  }
  # load any custom tests
  clearCustomTests()
  loadCustomTests(e$path)
  # Restore figures which precede current row (Issue #44)
  idx <- 1:(e$row - 1)
  figs <- e$les[idx,"Figure"]
  # Check for missing Figure column (Issue #47) and omit NA's 
  if(is.null(figs) || length(figs) == 0)return()
  figs <- figs[!is.na(figs)]
  figs <- file.path(e$path, figs)
  lapply(figs, function(x)source(file=x, local=TRUE))
}

loadInstructions.default <- function(e){
  e$instr <- list(present, waitUser, testResponse)
}


# UTILITIES

progressName <- function(courseName, lesName){
  pn <- paste0(courseName, "_", lesName, ".rda")
  gsub(" ", "_", pn)
}

inProgress <- function(e){
  pfiles <- dir(e$udat)[grep("[.]rda$", dir(e$udat))]
  pfiles <- gsub("[.]rda", "", pfiles)
  pfiles <- trimws(gsub("_", " ", pfiles))
  return(pfiles)
}

completed <- function(e){
  pfiles <- dir(e$udat)[grep("[.]done$", dir(e$udat))]
  pfiles <- gsub("[.]done", "", pfiles)
  pfiles <- gsub("[.]rda", "", pfiles)
  pfiles <- trimws(gsub("_", " ", pfiles))
  return(pfiles)
}

get_manifest <- function(course_dir) {
  man <- readLines(file.path(course_dir, "MANIFEST"), warn=FALSE)
  # Remove leading and trailing whitespace
  man <- trimws(man)
  # Remove empty lines
  man <- man[which(man != "")]
}

# Take vector of lessons and return in order given by manifest.
# Any courses not included in manifest are excluded!
order_lessons <- function(current_order, manifest_order) {
  current_order[match(manifest_order, current_order)]
}

courseDir.default <- function(e){
  # e's only role is to determine the method used
  get_swirl_option("courses_dir")
}

progressDir.default <- function(e) {
  file.path(find.package("swirl.pl"), "user_data")
}

# Default for determining the user
getUser <- function()UseMethod("getUser")
getUser.default <- function(){"swirladmin"}
