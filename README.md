# CHI_analysis
Centre for Homeless Impact systematic review on case management for homelessness

There are three .xlsx files which contain quantitative data extracted from papers
1. CHIcontinuous.xlsx contains continuous outcome data
2. CHIbin.xlsx contains binary outcome data
3. CHIgeninv.xlsx contains between group effect estimates for generic inverse variance analysis

Summary.xlsx contains study level summary information

Extraction_notes.xlsx is a labbook style document detailing the thought process behind the data extraction for each paper. It includes screenshots of RevMan calculators and some images that were used to extract information. 

CHI_analysis.R contains code that runs all of the analyses (and saves all of the relevant graphs).

CHI_Quantitative_results.Rmd calls the analysis file and compiles a report based on all of the analyses
