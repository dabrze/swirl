# Return random praise.
praise <- function() {
  swirl_is_fun <- getOption("swirl_is_fun")
  
  if(is.null(swirl_is_fun) || isTRUE(swirl_is_fun)) {
    phrases <- c("No racza!",
                 "NieŸle!",
                 "Dajesz, dajesz!",
                 "Proste!",
                 "Mniam!",
                 "Idealnie! O to chodzi³o!",
                 "Jesteœ geniuszem!",
                 "WyjdŸ za mnie!",
                 "Jak tak dalej pójdzie to mo¿e zaliczysz ten przedmiot!",
                 "Krok po kroku, krok po kroczku...",
                 "Super!",
                 "Jedziemy dalej!",
                 "No dobra, ale na nastêpne pytanie to ju¿ na pewno nie odpowiesz.",
                 "Eeee tam... Pofarci³o Ci siê...",
                 "Brawo!",
                 "\\m/>.<\\m/",
                 "No proszê!",
                 "Æwiczenie czyni mistrza!",
                 "Doprawdy pyszna herbatka, nie s¹dzisz?",
                 "Jaram siê jak pochodnia!",
                 "No i o to chodzi!",
                 "Baja!",
                 "Ja! Uwielbiam Ciê! Ty jesteœ tu! I dajesz radê!")
  } else {
    phrases <- "Poprawna odpowiedŸ!"
  }
  sample(phrases, 1)
}

# Return random "try again" message.
tryAgain <- function() {
  swirl_is_fun <- getOption("swirl_is_fun")
  
  if(is.null(swirl_is_fun) || isTRUE(swirl_is_fun)) {
    phrases <- c("No prawie! Spróbuj jeszcze raz.",
                 "Ju¿ prawie siê uda³o. Jeszcze raz.",
                 "Spróbuj ponownie.",
                 "Powtórz Waœæ, wstydu oszczêdŸ!",
                 "Prawie dobrze. Spróbuj ponownie.",
                 "Nie do koñca o to mi chodzi³o. Spróbuj jeszcze raz.",
                 "Niez³a próba, ale nie ot mi chodzi³o. Spróbuj ponownie.",
                 "Próbuj dalej!",
                 "That's not the answer I was looking for, but try again.",
                 "Prowadz¹cy p³aka³ jak sprawdza³. Spróbuj jeszcze raz.",
                 "Panie Turek! Niech pan tu koñczy to spotkanie! Turku! Koñcz ten mecz!",
                 "(-_-)zzz",
                 "Klops. Spróbuj jeszcze raz.",
                 "To nie to, ale widaæ ¿e siê uczysz! Spróbuj jeszcze raz.",
                 "Spróbuj jeszcze raz. Zgadywanie wszystkiego za pierwszym razem by³oby nudne!")
  } else {
    phrases <- "Niepoprawna odpowiedŸ. Spróbuj jeszcze raz."
  }
  sample(phrases, 1)
}
