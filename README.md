# Bayesian Data Analysis in R   
Part 1 - Oct 16, 2019   
Part 2 - Oct 23, 2019   

## Workshop descriptions

### Part 1

In this first of a two-part series, we learn the basics of running and interpreting a Bayesian data analysis in R. The workshop will center on comparing traditional (Frequentist) statistical analyses to "equivalent" Bayesian approaches. We write equivalent in quotes because as we'll see they’re not really equivalent. For example, a research question that would traditionally call for a t-test requires a different approach using Bayesian statistics. The workshop features very little math and emphasizes application. It is intended for all audiences! However some basic experience using R and some memory of an Introductory stats course will be helpful.

### Part 2

In part two of this two-part series, we dive deeper into statistical modeling from a Bayesian perspective. Once again we present the concepts by way of comparing them to traditional (Frequentist) modeling approaches. For example, what is the Bayesian approach for analyzing data that would normally call for logistic regression? How would the implementation and interpretation differ? We’ll also cover the basics of model checking and visualization. This workshop is intended for all audiences, but we highly recommend attending Part 1.

## Workshop materials

[Download Part 1 presentation slides](https://github.com/clayford/BDA/raw/master/bda_pres_part_1.pdf)

[Download Part 1 R script](https://github.com/clayford/BDA/raw/master/bda_part_1_script.R) **Right click and Save Link As...**

## Getting Ready

To participate in the workshop, please bring a laptop with R and RStudio installed. The workshop assumes basic knowledge of R and RStudio such as how to open an R script and submit R code to the console. 

We will use the following package in the workshop: rstanarm
 
I recommend installing the package well before the start of the workshop. In RStudio, go to Tools…Install Packages, enter the package name and click Install. Once the package is installed, type library(rstanarm) in the console and verify you can load the package. You should get a message like the following:

```
rstanarm (Version 2.19.2, packaged: 2019-10-01 20:20:33 UTC)
- Do not expect the default priors to remain the same in future rstanarm versions.
Thus, R scripts should specify priors explicitly, even if they are just the defaults.
- For execution on a local, multicore CPU with excess RAM we recommend calling
options(mc.cores = parallel::detectCores())
- bayesplot theme set to bayesplot::theme_default()
   * Does _not_ affect other ggplot2 plots
   * See ?bayesplot_theme_set for details on theme setting
```

If you do not see that message then the package is not loading properly. In this case make sure you are using the latest versions of R and RStudio. If not, upgrade to the latest versions and try again. I have provided some instructions to help with this: http://people.virginia.edu/~jcf2d/install_update_r_rstudio.html 

If you already have the rstanarm package installed, you may want to see if package updates are available. 

1.	Open RStudio
2.	In the menu, go to Tools…Check for Package Updates…
3.	If new versions are available for packages you already have installed, they will be presented in a window 
4.	It’s probably easiest to just click Select All and then click Install Updates. That will update all packages.
5.	If you get a message that asks “Do you want to install from sources the packages which need compilation?”, click No.

If at any point during package installation you get an error or encounter a problem, try entering the error message into Google and see if you can find a solution. Otherwise please arrive early the day of the workshop and I will do my best to help resolve the issue.  
 
