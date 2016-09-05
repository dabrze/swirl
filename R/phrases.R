# Return random praise.
praise <- function() {
  swirl_is_fun <- getOption("swirl_is_fun")
  
  if(is.null(swirl_is_fun) || isTRUE(swirl_is_fun)) {
    phrases <- c("No racza!",
                 "Nieźle!",
                 "Dajesz, dajesz!",
                 "Proste!",
                 "Mniam!",
                 "Propsy!",
                 "Fejm",
                 "Idealnie! O to chodziło!",
                 "Jesteś geniuszem!",
                 "Wyjdź za mnie!",
                 "Krok po kroku, krok po kroczku...",
                 "Super!",
                 "Jedziemy dalej!",
                 "Yes, yes, yes!",
                 "Eeee tam... Pofarciło Ci się ;)",
                 "Brawo!",
                 "\\m/>.<\\m/",
                 "No proszę!",
                 "No Rejczel!",
                 "Się wie!",
                 "Ćwiczenie czyni mistrza!",
                 "Rządzisz!",
                 "Wyginasz blachę!",
                 "Czacha dymi!",
                 "Jaram się jak pochodnia!",
                 "No i o to chodzi!",
                 "Baja!",
                 "Ja! Uwielbiam Cię! Ty jesteś tu! ...")
  } else {
    phrases <- "Poprawna odpowiedź!"
  }
  sample(phrases, 1)
}

# Return random "try again" message.
tryAgain <- function() {
  swirl_is_fun <- getOption("swirl_is_fun")
  
  if(is.null(swirl_is_fun) || isTRUE(swirl_is_fun)) {
    phrases <- c("No prawie! Spróbuj jeszcze raz.",
                 "Już prawie się udało. Jeszcze raz.",
                 "Spróbuj ponownie.",
                 "Powtórz Waść, wstydu oszczędź!",
                 "Prawie dobrze. Spróbuj ponownie.",
                 "Nie do końca o to mi chodziło. Spróbuj jeszcze raz.",
                 "Niezła próba, ale nie o to mi chodziło. Spróbuj ponownie.",
                 "Próbuj dalej!",
                 "To nie to, ale widać że się uczysz! Spróbuj jeszcze raz.",
                 "Spróbuj jeszcze raz. Zgadywanie wszystkiego za pierwszym razem byłoby nudne!")
  } else {
    phrases <- "Niepoprawna odpowiedź. Spróbuj jeszcze raz."
  }
  sample(phrases, 1)
}
