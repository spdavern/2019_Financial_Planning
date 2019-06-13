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
    create examples.

# Setup

Install RStudio Desktop:
<https://www.rstudio.com/products/rstudio/download/>

Clone this git repository: \<not pushed to github yet\>

``` bash
$ git clone https://github.com/spdavern/2019_Financial_Planning
```

Open the “2019 Financial Planning.Rproj” R Project.  
Then open one of several R notebooks (./notebooks folder) to follow
and/or reproduce results or read generated reports documenting results
(./reports folder).

# Directories and Their Contents

|   Folder    | Contents                                            |
| :---------: | --------------------------------------------------- |
|     ./      | Root project directory                              |
|   ./code    | R scripts created for the project                   |
|   ./data    | Source data for analysis as well as saved R Objects |
| ./notebooks | R Notebooks covering each part of the project work  |
|  ./packrat  | Required R package repository                       |
|  ./reports  | Documentation of methods and results                |

# R Notebooks

| Notebook (.Rmd)           | Description of Work Covered                |
| ------------------------- | ------------------------------------------ |
| Transfer of 2013 Analysis | Convertion of modeling done in 2013 into R |

Note that the corresponding notebook.nb.html is the html rendering of
the .Rmd file and can be opened in a browser.