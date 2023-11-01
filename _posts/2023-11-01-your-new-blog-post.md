---
title: "Blogpost 3-How to find the right variable when building regression model-New lessons learnt that could be applied to an old story of my own"
author: "Siyuan"
date: "2023-10-31"
output: html_document
---

# *Eyeing* a regression model of orange taste using Excel.

When I was a graduate student back in Shunyi, a quiet town near Beijing, I used to go to grocery stores on my way back home from lab. I love eating oranges. Sadly, the quality of oranges in the grocery stores varied a lot, I want to make sure that I could tell whether the orange is tasty or not just from its appearance. My stipends were low at that time, so I went all way to find all possible traits of oranges, record them down in Excel, including:

color: orange/yellowish/redish;
size: big/small;
tightness when squeezing: 1-5;
whether there is a round structure at the bottom of the orange: yes/no;
...

Then I tasted the orange and recorded the score of orange in the excel in a 1-5 range. It could be further breakdown into 1)sweetness, 2)juiciness. 

I did not take any statitical class at that time, did not know how to build up a regression model, but I made several scatter plots and found the taste of orange is mostly correlated with tightness when squeezing (mostly with juiciness, the sloppier when squeezing the drier) and the round structure at the bottom of the orange (yes indicates sweeter taste). I proudly showed my regression result to my then girl friend (now wife), she said it was a waste of time and I should focus on my study (graduate). I now regretted quiting collecting more data.

After taking ST558 and reading through provided literatures, I recalled that experience and thought what I did before is merely the first step: to find correlation between each variant and the result. The strategy I took is to find the most correlated factor and did not even try to integrate the two most signficant factor into one single regression model. Considering the limited number of factors I have maybe it is a valid approach. If I were able to collect more data then I could divide the dataset into training and test group and test my findings...

# Precision medicine (precision orange picking)

Then I recalled my PhD advisor, who proudly classified himself as a analytical chemist although I think he is a biochemist somehow rushing into omics field. One project the lab had at that time was to measure thousands of protein abundance (each protein abundance could be viewed as a variant) across hundreds of patient samples (and also *normal* control), and then try to find a panel of proteins that could indicate the status of illness (yes or no, or even grade). I used to laugh at the idea because the results always lead to find a panel of **tens** of proteins with limited sensitivity and selectivity, not as good as my **orange picking strategy** that I could narrow it down to only two factors. What I did not foresee is the development of new technique enable routine detection of up to 200 different protein abundance under 30 mins, so tens of proteins seemed totally managable.

Take that precision medicine example, what we need here is to predict whether a person has a certain kind of disease according to his/her protein expression profile (number of factors, the less the better). This is very like our regression model. It is always the simpler the better. There are several points to take into consideration:

1. Background knowledge. Proteins were detected in the patient samples, but sometimes there would be human error in the sample processing steps, including introduction of human keratins, contamination from animal product etc. Those proteins and contaminates needs to be excluded from our model. *Caution* Do not overplay because focusing too much on background knowledge suppression the discovery of new mechanistic links.

2. Start with single factor analysis. Earlier days researcher always dream finding a single protein/metabolite that could precisely tell whether a person has cancer/diabetes, but that NEVER came true. More proteins are needed. But how?

3. Test different approaches for regression model building, including Backward elemination, forward selection, LASSO, best subset selection etc. The key is to find the smallest number of factors (proteins) that could nicely predict the status of illness. In the light of that I prefer more LASSO method because it poses penalties on overfitting and tested through a series of lamda to find the best fit. I would imagine it consumes a chuck of computing power once the dataset is relatively big but I would not bother consider that right now. Why not too many factors? This is from two different aspects, one is the worry of over-fitting, another is from practical perspective: less proteins are always easier and cheaper to measure for clinician, who do not (and do not wish to) know complicated biochemistry measurement! Frankly I do not know too much about how the panelties for non-fitting factors were set in the LASSO method were set but it seemed like the right method to use in this problem.

# Believe in data science
In my graduate years there is a very famous book “Big Data: A Revolution That Will Transform How We Live, Work, and Think” I forgot most of the book but only remember one thing the author mentioned: **do not find causation but correlation.** I used to buff at the idea mocking those people who said it are not real scientists. If no underlying reasons were found, although correlation leads to prediction, once the underlying reasons disappeared without notifying anyone then the correlations are gone, model collapsed, dollars wasted (or tax payers' dollar wasted, quoted my PhD advisor who asked his students to "ask not what the lab can do for you, ask what you could do for the lab").

Almost ten years have passed I believe more in the correlation senario. For one thing, it takes way longer time and way more dollars to find the causation than correlation, and data science should find ways to "quality control" before the correlation began show signs of weaning and model collapsing. For another, corelation should be broadly used to a tool to derive hypothesis to find causal relationship. Regression model as a powerful way to build strong correlation and prediction, should be able to unearth many new causations.
