# Overview

This folder/repo contains work created using R (ver. 3.6.0) and RStudio
(ver. 1.2.1335).

The purpose of this work was multi-fold:

  - Extend work done in 2013 and 2018 to model and forecast Seed giving
    data and create projections for 2018-2019 budgeting purposes.
  - Previous work was done using JMP which is not accessible to those
    without that software, including me now. Transferring this work into
    R allows sharing with others and my own re-use.
  - I’m using this exercise to document my own R/RStudio skills and to
    create examples. I am experimenting with a blend of [literate
    programming](https://en.wikipedia.org/wiki/Literate_programming) and
    coding-like project organization suggested by pavopax.\[1\]

This project uses [git](https://git-scm.com/) for version control and
[packrat](https://rstudio.github.io/packrat/) for package dependency
management.

# Setup

1.  Install RStudio Desktop:
    <https://www.rstudio.com/products/rstudio/download/>

2.  Clone this git repository:

<!-- end list -->

``` bash
$ git clone https://github.com/spdavern/2019_Financial_Planning
```

3.  Open the “2019 Financial Planning.Rproj” R Project in RStudio.  
4.  Then, open one of several [R notebooks](#nb) to follow and/or
    reproduce results or read generated reports documenting results
    (./reports folder).

# Directories and Their Contents

|   Folder    | Contents                                               |
| :---------: | ------------------------------------------------------ |
|     ./      | Root project directory (Contains the README notebook.) |
|   ./code    | R scripts created for the project                      |
|   ./data    | Source data for analysis as well as saved R Objects    |
| ./notebooks | R Notebooks covering each part of the project work     |
|  ./packrat  | Required R package repository                          |
|  ./reports  | Documentation of methods and results                   |

# R Notebooks

| Notebook (.Rmd)           | Description of Work Covered                                                                                                                 |
| ------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------- |
| README                    | This R Notebook that summarizes the contents of the project and that produces the github flavored MarkDown making GitHub easier to consume. |
| Transfer of 2013 Analysis | Convertion of modeling done in 2013 into R. Uses the model: \(\textit{Monthly Giving} = a+b_{year}+c_{month}\)                              |

Note that the corresponding notebook.nb.html is the html rendering of
the .Rmd file and can be opened in a browser.

# Footnotes

1.  [Paul Paczuski](https://github.com/pavopax) (pavopax), [Best
    Practices for Organizing RMarkdown
    Projects](https://community.rstudio.com/t/best-practices-for-organizing-rmarkdown-projects/914/11),
    9/20/2017.
