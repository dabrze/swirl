# swirl.pl (swirl for Polish students)

**This repository holds translations and original content for Polish students trying to learn R using swirl.pl. As the attached files are mainly translations, for an up-to-date version of swirl, please visit: [https://github.com/swirldev/swirl](https://github.com/swirldev/swirl).**

swirl.pl is the Polish translation of swirl, a platform for learning (and teaching) statistics and R simultaneously and interactively. It presents a choice of course lessons and interactively tutors a user through them. A user may be asked to watch a video, to answer a multiple-choice or fill-in-the-blanks question, or to enter a command in the R console precisely as if he or she were using R in practice. Emphasis is on the last, interacting with the R console. User responses are tested for correctness and hints are given if appropriate. Progress is automatically saved so that a user may quit at any time and later resume without losing work.

swirl, and thus swirl.pl, leans heavily on exercising a student's use of the R console. A callback mechanism, suggested and first demonstrated for the purpose by Hadley Wickham, is used to capture student input and to provide immediate feedback relevant to the course material at hand.

[swirlify](https://github.com/swirldev/swirlify) is a separate R package that provides a comprehensive toolbox for swirl instructors. Content is authored in [YAML](http://en.wikipedia.org/wiki/YAML) using the handy tools described on the [instructors page](http://swirlstats.com/instructors.html) of our website.

The program is initiated with `swirl()`. Functions which control swirl's behavior include `bye()` to quit, `skip()` to skip a question, `main()` to return to the main menu, `play()` to allow experimentation in the R console without interference from swirl, `nxt()` to resume interacting with swirl, and `info()` to display a help menu.


## Installing swirl.pl

Contrary to swirl, swirl.pl cannot be installed from CRAN. At the moment, you can only install and run the development version of swirl.pl using the [devtools](https://github.com/hadley/devtools) package:

```
install.packages("devtools")
devtools::install_github("dabrze/swirl")
library(swirl.pl)
swirl()
```

## Contributing to swirl(.pl)'s development

If you'd like to get involved in the Polish version of swirl, please fork this repository and submit a pull request with your proposed changes. For the one and only true swirl, please visit: [https://github.com/swirldev/swirl](https://github.com/swirldev/swirl). I am happy to chat if you have any questions about the source code.

## Using swirl in the classroom

Instructors around the world are using swirl in their classrooms. We think this is awesome. If you're an instructor, please feel free to do the same -- free of charge. While your students may be paying to take your course or attend your institution, we simply ask that you don't charge people *directly* for the use of our software or instructional content.

If you are not sure about a particular use case, don't hesitate to send us an email at info@swirlstats.com.