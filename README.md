# Bayesian Data Analysis in R   
Part 1 - Feb 17, 2022    
Part 2 - Feb 24, 2022   

## Workshop descriptions

### Part 1

In this first of a two-part series, we learn the basics of running and interpreting a Bayesian data analysis in R. The workshop will center on comparing traditional (Frequentist) statistical analyses to "equivalent" Bayesian approaches. We write equivalent in quotes because as we'll see they’re not really equivalent. For example, a research question that would traditionally call for a t-test requires a different approach using Bayesian statistics. The workshop features very little math and emphasizes application. It is intended for all audiences! However some basic experience using R and some memory of an Introductory stats course will be helpful.

### Part 2

In part two of this two-part series, we dive deeper into statistical modeling from a Bayesian perspective. Once again we present the concepts by way of comparing them to traditional (Frequentist) modeling approaches. For example, what is the Bayesian approach for analyzing data that would normally call for logistic regression? How would the implementation and interpretation differ? We’ll also cover the basics of model checking and visualization. This workshop is intended for all audiences, but we highly recommend attending Part 1.

## Workshop materials

[Download Part 1 presentation slides](https://github.com/clayford/BDA/raw/master/bda_pres_part_1.pdf)  
[Download Part 2 presentation slides](https://github.com/clayford/BDA/raw/master/bda_pres_part_2.pdf)

[Download Part 1 R script](https://github.com/clayford/BDA/raw/master/bda_part_1_script.R) **Right click and Save Link As...**   
[Download Part 2 R script](https://github.com/clayford/BDA/raw/master/bda_part_2_script.R) **Right click and Save Link As...**

## Getting Ready

To participate in the workshop, please bring a laptop with R and RStudio installed. The workshop assumes basic knowledge of R and RStudio such as how to open an R script and submit R code to the console. 

We will use the following packages in the workshop: rstanarm, ggeffects
 
I recommend installing the packages well before the start of the workshop. In RStudio, go to Tools…Install Packages, enter the package names separated by a space and click Install. Once the packages are installed, type library(rstanarm) in the console and verify you can load the package. You should get a message like the following:

```
This is rstanarm version 2.21.1
- See https://mc-stan.org/rstanarm/articles/priors for changes to default priors!
- Default priors may change, so it's safest to specify priors, even if equivalent to the defaults.
- For execution on a local, multicore CPU with excess RAM we recommend calling
  options(mc.cores = parallel::detectCores())
```

If you do not see that message then the package is not loading properly. In this case make sure you are using the latest versions of R and RStudio. If not, upgrade to the latest versions and try again. I have provided some instructions to help with this: [https://clayford.github.io/r_install/](https://clayford.github.io/r_install/)

If you already have the rstanarm package installed, you may want to see if package updates are available. 

1.	Open RStudio
2.	In the menu, go to Tools…Check for Package Updates…
3.	If new versions are available for packages you already have installed, they will be presented in a window 
4.	It’s probably easiest to just click Select All and then click Install Updates. That will update all packages.
5.	If you get a message that asks “Do you want to install from sources the packages which need compilation?”, click No.

If at any point during package installation you get an error or encounter a problem, try entering the error message into Google and see if you can find a solution. Otherwise please contact me and I’ll do my best to help resolve the error: clayford@virginia.edu    
 
