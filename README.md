## Empirical Industrial Organisation and Consumer Choice
## Sebastian Kranz, Ulm University

This Github repository contains material from the 2018 edition of my Master level course "Empirical Industrial Organisation and Consumer Choice" that I teach regularly at Ulm University.

Since we have quite a heterogeneous composition in our class and not every student has heard yet an econometrics class, the course starts at a very basic level and ends already at the famous BLP model that allows to estimate demand functions for differentiated products.

We also have a mini-excursion to machine learning in exercise sheet 4 and 5. Main goal is to contrast the approach to pure prediction problems of machine learning  with the estimation of causal relationships with structural econometric models.

If you like R programming, take a look at the folder `rtutor` that contains RTutor problemsets, that allow you to explore the contents of the early chapters in an interactive fashion. The RTutor problemsets 1a to 1c  mainly deal with the question of how we can estimate a very simple demand function from field data. You learn very slowly with simple Monte-Carlo simulations about endogeniety problems in this context and how one can attempt to solve them.

See [https://github.com/skranz/RTutor](https://github.com/skranz/RTutor) for more details about RTutor.


To install all required R packages run the code in `install packages.r` in the folder R.

The folder `slides` contains some lecture slides. Often I do some live programming in the class. The code is either part of the RTutor problem sets (early slides) or in the folder `r` (later slides). Alternatively, I show details on the black board. So stand-alone the slides have only limited use.

## License

- Except for the RTutor problem sets, you can use all materials under a [Creative Commmons Attribution-NonCommercial-ShareAlike 4.0 International (CC BY-NC-SA 4.0) license](https://ocw.mit.edu/terms/#cc) license.

- You can also use and share the RTutor problem sets similar to the (CC BY-NC-SA 4.0) license with one restriction: You should not share, make publicly available or provide a link to any solutions of these problem sets. (To motivate students to solve the problem sets, they are grade relevant to a small extend.)


## Sources & Attributions

- The data folder, contains a set about car sales, which was made available on [Frank Verbooven's Website](https://sites.google.com/site/frankverbo/data-and-software).

- The slides `EmpIO_2b.pdf` and the corresponding R code is based on [this Vignette](https://cran.r-project.org/web/packages/mlogit/vignettes/Exercises.pdf) of the R package `mlogit` by Yves Croissant who has adapted the R code from original Matlab exercises by Kenneth Train. (The R package is distributed under a GPL License)

- Exercise Sheet 3 is an adaption from [a problem set by Glenn Ellisions Industrial Organisation Course at the MIT](https://ocw.mit.edu/courses/economics/14-271-industrial-organization-i-fall-2005/assignments/ps2.pdf), which is available as part of MIT-OCW under a [Creative Commmons Attribution-NonCommercial-ShareAlike 4.0 International (CC BY-NC-SA 4.0) license](https://ocw.mit.edu/terms/#cc).
