--- 
title: "Introduction to Statistical Methodology, Second Edition"
author: "Derek L. Sonderegger & Robert Buscaglia"
date: "October 08, 2020"
site: bookdown::bookdown_site
output: bookdown::gitbook
documentclass: book
bibliography: [book.bib, packages.bib]
biblio-style: apalike
link-citations: yes
github-repo: dereksonderegger/570
description: ""
---

# Preface{-}
The problem with most introductory statistics courses is that they don't prepare the student for the use of advanced statistics. Rote hand calculation is easy to test, easy to grade, and easy for students to learn to do, but is useless for actually understanding how to apply statistics. Since students pursuing a Ph.D. will likely be using statistics for the rest of their professional careers, we feel that this sort of course should attempt to steer away from a “cookbook” undergraduate pedagogy, and give the student enough theoretical background to continue their statistical studies at a high level while staying away from the painful mathematical details that statisticians must work through.

Statistical software has progressed by leaps and bounds over the last decades. Scientists need access to reliable software that is flexible enough to handle new problems, with minimal headaches. R has become a widely used, and extremely robust Open Source platform for statistical computing and most new methodologies will appear in R before being incorporated into commercial software. Second, data exploration is the first step of any analysis and a user friendly yet powerful mechanism for graphing is a critical component in a researchers toolbox. R succeeds in this area with the most flexible graphing library of any statistical software and and basic plotting that can be executed quickly and easily. The only downside is that there is a substantial learning curve to scripting, particularly for students without any programming background. The use of R software is introduced with as little pain as possible, but some frustration is inevitable. 

Because the mathematical and statistical background of physical science students varies widely, the course seems to have a split-personality disorder. We wish to talk about using calculus to maximize the likelihood function and define the expectation of a continuous random variable, but also must spend time defining how to calculate a mean. We attempt to address both audiences, but recognize that it is not ideal. 

These notes were originally written for an introductory statistics course for grad students in the physical sciences. Furthermore, the initial author of the notes primarily works in biological and ecological fields. As a result, many of the examples are biological situations. However there isn't any biological knowledge necessary. 

We hope you'll find these notes useful.

## Acknowledgements{-}
*Derek Sonderegger*: I have had the pleasure of interacting with a great number of talented mathematicians and statisticians in my schooling.  In particular I am deeply indebted to Dr Robert Boik and Dr Warren Esty as well as my Ph.D. adviser Dr Jan Hannig. 

As a professor at Northern Arizona University, I am grateful for the feedback and comradery of my fellow statisticians, particularly Dr St. Laurent and Dr. Buscaglia.

Finally, I am deeply appreciative of the support given to me by my wife, Aubrey.

*Robert Buscaglia*: I am thankful for Dr. Sonderegger allowing me to be a part of this online textbook.  I would like to thank my graduate advisers, Dr. Jonathan B. Chaires and Dr. Yiannis Kamarianakis, who helped me achieve my goal of becoming a professor at Northern Arizona University.  I would also be lost without the patience and kindness I receive from my family, especially my caring wife, Kelly.

