# Code files description

If you are interested in a specific part of the analysis, or you want to
rerun the code in chunks, or there is any other reason you don't want to
use the `Wrapper.R` script, here is a description of the code files and the
order in which they should be run.

## This directory

* `Figures.R`: this script processes the results of our calculations and
makes the figures used in the manusucript and the appendix.
* `Tables.R`: this script generates the tables (in flextable `.Rds` format)
that are included in the manuscript and the appendix.
* **Note that you need to run all of the analysis and model building scripts
before you can recreate the Figures, Tables, or Products.**

## Common-Functions

This folder contains scripts that define helper functions for use elsewhere.
They are defined here to make the code in other scripts easier to read.
*You do not need to run any of the scripts in this folder to reproduce the analysis*, they will be utilized automatically by the scripts that need them.

## Analysis

This scripts do the mathematical calculations for several parts of our analysis,
mainly calculating the CDR results for each patient, computing agreement
statistics with CIs, and sensitivity analyses for the choice of test/treat
thresholds. The scripts that calculate CIs may be computationally intensive to
run. **If you want to run the scripts manually (i.e. without using `wrapper.R`)
then you should run them in the order they appear in this list.**

1. `CPR-Calculations.R`: calculates the different CDRs used for this project.
(Note that at the beginning of this project we were using the acronym CPR
for clinical prediction rule instead of CDR. They are the same thing, we just
thought that CPR was too confusing for a clinical journal.)
1. `CPR-Validation.R`: calculates performance metrics for the various CDRs
on our dataset.
1. `Dx-Agreement.R`: calculates agreement statistics between the multiple
diagnostic methods used in our study. Note that clinicians were not blinded
to test results so these statistics are likely inflated.
1. `Score-Agreement.R`: calculates agreements between the CDRs on the patient
data and on the clinician data, i.e. comparing patient and clinician assessments
of the patetient's symptoms.
1. `Symptoms-IRR.R`: calculates agreement statistics between reports of
individual symptoms.
1. `thresholds-sensitivity-analysis.R`: **this script must be run after the
model-building scripts below!!**

## Model Building

These scripts fit multiple predictive models to the symptom data. The scripts
that implement machine learning models may be computationally intensive or
time-consuming to run. **If you run these scripts manually they may not
run unless you run them in the order specified here.**

1. `Data-Processing-for-Model-Building.R`: gets the data into the correct
format so it is ready to use with the `tidymodels` set of packages.
1. `…-Models-…-Data.R`: Fits the specified models to the specified data set.
You should be able to run these in any order, but we recommend running them
in the order they show up in the folder.
1. `ZZZ-Result-Processing.R`: processes the fitted model objects and wrangles
the model data into the correct format for making figures and tables.

## Code running instructions

1. Run the scripts in the `Analysis` folder (EXCEPT for `thresholds-sensitivity-analysis.R`)
1. Run the scripts in the `Model-Building` folder
1. Run `Analysis/thresholds-sensitivity-analysis.R`
1. Run the `Figures.R` and `Tables.R` scripts
1. Knit the manuscript and appendix files.
