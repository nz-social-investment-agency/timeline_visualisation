# Timeline Visualisation

An interactive visualisation tool for exploring journey timelines.

## Overview

This repsoitory contains the code necessary to run our's interactive timeline visualisation tool. Developed as part of the Social Investment Agency's cross-sector representative timeline modeling, however it can be used independently of our timeline construction methodology (https://github.com/nz-social-investment-agency/representative_timelines).

## License for timeline visualisation

Crown copyright (c) 2019, Social Investment Agency on behalf of the New Zealand Government.

GNU General Public License v3.0. See ![LICENSE.md](https://github.com/nz-social-investment-agency/timeline_visualisation/blob/master/LICENSE) for more details. In essence, you are free to copy, distribute and adapt the work, as long as you attribute the work to the New Zealand Government and abide by the other licence terms.

Please note that neither the New Zealand Government emblem nor the New Zealand Government logo may be used in any way which infringes any provision of the [Flags, Emblems, and Names Protection Act 1981](http://www.legislation.govt.nz/act/public/1981/0047/latest/whole.html) or would infringe such provision if the relevant use occurred within New Zealand. Attribution to the New Zealand Government should be in written form and not by reproduction of any emblem or the New Zealand Government logo.

## Installation and dependencies

Depending on your existing environment, expertise, and intended use of the visualisation app the steps you need to install the app may vary. Consider each of the following sections in turn.

### R and RStudio

The visualisation app is written in R and is easiest to run in the RStudio environment. If you do not have these programs installed already you will need to install them in order to use the timeline visualisation. R can be downloaded from https://cran.r-project.org and RStudio can be downloaded from https://www.rstudio.com. Both are free.

While different versions of R and RStudio are unlikely to prevent you using the timeline visuaisation, the versions that were used in the development of the visualisation app are R versions 3.5.2 (2018-12-20) - "Eggshell Igloo", and RStudio version 1.1.463.

### Packages

The visualsiation tool requires several R packages in order to run. These are not programs, but public code of a similar nature to the code files included in this repositry (but developed and reviewed by much larger teams of professional developers).

The four code packages are shiny, shinywidgets, tidyverse and readxl. All four of these packages are well established and reviewed. As the packages can be updated from time to time users experiencing difficulties may wish to download the exact same version of these packages as were used in the development of the interactive visualisation. The versions of each packages are as follows:

 - shiny, version 1.3.2
 - shinywidgets, version 0.4.8
 - tidyverse, version 1.2.1
 - readxl, version 1.3.1

The code for installing these specific versions of the packages is:
```
install.packages(devtools)
install_version("shiny", version = "1.3.2", repos = "http://cran.us.r-project.org")
install_version("shinywidgets", version = "0.4.8" repos = "http://cran.us.r-project.org")
install_version("tidyverse", version = "1.2.1", repos = "http://cran.us.r-project.org")
install_version("readxl", version = "1.3.1", repos = "http://cran.us.r-project.org")
```

If these packages are not installed, then the program will attempt to install them in order to provide the timeline visualisation. If the packages are unavailable and can not be installed then the program will fail, and stop running, during its initiation phase.

### Data

Although the interactive timeline visualsiation tool was developed in the context of a particular dataset, this data is not avaialble in the repository.

Without data to visualise the app is of minimal use, hence this code repository only includes artificial (made-up) data for demonstration purposes. To use the visualisation for another dataset you will either need to prepare your own data in the required format, or use a dataset that has already been prepared for you. To test the visualisation app with the example data you will first need to rename both data files, removing the "\_EXAMPLE" from the end.

Any data prepared according to the correct pattern can be read and displayed by the timeline visualisation tool. To prepare your own data for use in the visualisation you will need to arrange it in the same way that the example data files are set out. Two data files are required, **input_date.xlsx** that contains the data for display (e.g. timing of events), and **data_controls.xlsx** that contains information for how the data should be presented (e.g. labels, order, colours).

### Running the app

Once you have R and RStudio installed and the data prepared, complete the following steps to run the app:

 - Create a local copy of this repository on your computer.
 - Copy the data files into the **www** folder.
 - Open **global.R** of the version you want to run in RStudio.
 - If this is the first time you have run the app, ensure you have internet connectivity.
 - You should see a button labelled "Source" or "Run APP" click it.

A new window should open providing you with the interactive timeline visualisation.

The first time you run the app, it may take longer if it needs to download R code packages as described above.

## Folder and file descriptions
The folder contains the code needed to run two different versions of the visualisation. The base version allows for the viewing of a single group (e.g. people of Pacific ethnicity) with multiple roles (e.g. mothers and fathers) side-by-side. The comparison variant allows for the viewing of multiple groups (e.g. Maori, Pacific, Asian, and European ethnicities) side-by-side but is limited to only a single role (e.g. mother) at a time.

The key files and folders are:

 - **global.R** is one of the core files for the base version of the visualsation app, it is responsible for the initial setup and preparation.
- **server.R** is one of the core files for the base version of the visualsation app, it is responsible for the background calculations and data management.
- **ui.R** is one of the core files for the base version of the visualsation app, it is responsible for the display and responsiveness of the user interface.
- **reference_app.R** is a docmentation file, providing a demonstration of a less common coding technique used in the app. Developers seeking to modify the code are advised to first familiarse themselves with the contents of this demo.
- **comparison_variant** contains all the equivalent core files (global, server and ui) for the comparison variant of the visualisation app.
- **www** contains the data files loaded by the app when it is run. Users seeking to investigate different data files should place them here.

## Citation

Social Investment Agency (2019). Timeline visualisation. Source code. https://github.com/nz-social-investment-agency/timeline_visualisation

## Getting Help
If you have any questions email info@sia.govt.nz
