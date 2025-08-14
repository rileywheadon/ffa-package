---
title: "Flood Frequency Analysis Framework"
---

# FFA Framework Wiki

The **FFA Framework** is an open-source tool for flood frequency analysis (FFA) designed to support systematic and repeatable workflows for stationary and nonstationary analysis.
Development is ongoing at the University of Calgary and the University of Saskatchewan in Canada.

The most recent version of the framework is freely available as an **R package**.
The [original version](https://zenodo.org/records/8012096) was released as a stand-alone GUI with MATLAB source code and published in [Vidrio-Sahagún et al. (2024)](https://doi.org/10.1016/j.envsoft.2024.105940).
For a list of changes from the MATLAB version, see [here](articles/matlab-version.html).

## Installation Instructions

**Prerequisites**: Install [Git](https://git-scm.com/downloads) and [R](https://www.r-project.org/) (v4.4.0 or later).
Check that the installations were successful by executing the `git --version` and `R --version` commands in a terminal.

**Note**: Windows users may need to add Git and R to their `PATH` environment variable in order to run them from the command line.
The default locations for Git and R are `C:\Program Files\Git\bin` and `C:\Program Files\R\R-4.5.0\bin` (replace "4.5.0" with the version of R you have installed).
To update the `PATH`, edit the system environment variables from the settings menu.

### Instructions

1. Open a terminal and navigate to the directory where you want to install the package.
2. Clone the Github repository by running the following command:

        git clone https://github.com/rileywheadon/ffa-framework.git

3. Open the `R` console. Then install and load the `devtools` package (if not already installed):

        install.packages("devtools")
        library(devtools)

4. Install the `ffaframework` package:

        devtools::install("~/path/to/ffa-package")

    Replace `~/path/to/ffa-package` with the path to the cloned directory from step 2.

5. Exit the `R` console with `q()`. The `ffaframework` package is now installed and ready to use.


