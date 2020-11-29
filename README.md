# VirtExp
Code and data related to the manuscript "Disentangling climate change trends in Australian streamflow" (vervoort et al.), submitted to Hydrological Processes.
[![DOI](https://zenodo.org/badge/78632674.svg)](https://zenodo.org/badge/latestdoi/78632674)

The project is organised as follows:  

- **Documents** contains all the markdown and pdf files related to the project. This is the place to start if you want to read all the detailed analyses and repeat these for your own project. The files are numbered to make it easier to follow the steps. Note that some of the GAM modelling takes some time to run.
- **Data** contains all the original and baseline data for the project, which should allow you to rerun all the markdown files in order and recreate the outputs and the analyses. However, the intermediate data  (which is large and is generated as part of the project) is not stored on GitHub, but elsewhere. This can be generated from the baseline data, but if you desperately need access, please contact the author.
- **Rcode** contains all the additional Rcode, in particular the Rcode to run the HPC analyses, which are not documented in the markdown files. This consist of both daily modelling and monthly modelling. The *.sh* files are the HPC command files. Most of the files should speak for themselves. In this folder are two subfolders: **HPC** which are the files run on the HPC, and **SimHyd** which provides the implementation of Simhyd used in the project as this was not yet part of Hydromad.
- the final two files are this Readme file and a Rstudio project file


please contact willem: willemvervoort@gmail.com with any questions
