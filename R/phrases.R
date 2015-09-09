# Return random praise.
praise <- function() {
  swirl_is_fun <- getOption("swirl_is_fun")
  
  if(is.null(swirl_is_fun) || isTRUE(swirl_is_fun)) {
    phrases <- c("No racza!",
                 "Nieźle!",
                 "Dajesz, dajesz!",
                 "Proste!",
                 "Mniam!",
                 "Idealnie! O to chodziło!",
                 "Jesteś geniuszem!",
                 "Wyjdź za mnie!",
                 "Jak tak dalej pójdzie to może zaliczysz ten przedmiot!",
                 "Krok po kroku, krok po kroczku...",
                 "Super!",
                 "Jedziemy dalej!",
                 "No dobra, ale na następne pytanie to już na pewno nie odpowiesz.",
                 "Eeee tam... Pofarciło Ci się...",
                 "Brawo!",
                 "\m/>.<\m/",
                 "No proszę!",
                 "Ćwiczenie czyni mistrza!",
                 "Doprawdy pyszna herbatka, nie sądzisz?",
                 "Jaram się jak pochodnia!",
                 "No i o to chodzi!",
                 "Baja!",
                 "Ja! Uwielbiam Cię! Ty jesteś tu! I dajesz radę!")
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
                 "Niezła próba, ale nie ot mi chodziło. Spróbuj ponownie.",
                 "Próbuj dalej!",
                 "That's not the answer I was looking for, but try again.",
                 "Prowadzący płakał jak sprawdzał. Spróbuj jeszcze raz.",
                 "Panie Turek! Niech pan tu kończy to spotkanie! Turku! Kończ ten mecz!",
                 "(-_-)zzz",
                 "Klops. Spróbuj jeszcze raz.",
                 "To nie to, ale widać że się uczysz! Spróbuj jeszcze raz.",
                 "Spróbuj jeszcze raz. Zgadywanie wszystkiego za pierwszym razem byłoby nudne!")
  } else {
    phrases <- "Niepoprawna odpowiedź. Spróbuj jeszcze raz."
  }
  sample(phrases, 1)
}
