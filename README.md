# ALY6015 Analytics Modules 1–5

Five weekly analytics assignments from Northeastern's ALY6015 course, in R.
Each module is a different statistical method applied to a different question —
they are not five versions of one project, so they sit together here as a series,
separate from the Boston 311 final project repo.

## In plain language

Statistics is not one tool. It is a toolbox, and each module practises a
different tool:

- **Module 1 — regression diagnostics:** before you trust a house-price model,
  check whether its assumptions actually hold.
- **Module 2 — chi-square and ANOVA:** do groups really differ, or could the
  difference just be chance? Tested on blood types, airline performance,
  baseball, crops, and more.
- **Module 3 — GLM and logistic regression:** predict a yes/no outcome — in
  this case, whether a US college is private or public — from its
  applications, graduation rate, spending, and other measures.
- **Module 4 — regularisation (ridge and lasso):** when a model has too many
  predictors, shrink the unhelpful ones so the model generalises instead of
  memorising the training data. Applied to college graduation rates.
- **Module 5 — nonparametric tests:** when the data do not follow a normal
  distribution, compare medians and ranks instead of means — sign tests,
  Wilcoxon tests, Kruskal–Wallis, Spearman correlation, and simulations
  (game attendance, lottery sales, baseball wins, maths literacy, and more).

## What is in this repo

### Ames House Prices: Checking a Regression Model Before Trusting It
Folder: `ames-house-prices-regression-diagnostics/`
Files: `Panchal_Assignment1_Code.R`, `Panchal_Assignment1_Report.docx`
Data: `data/AmesHousing.csv` — the file the code reads.
What it does: explores Ames house sale prices, handles missing values,
measures which variables correlate most with sale price, and runs regression
diagnostics (outliers, residual patterns, model comparison) to see whether the
model is safe to trust.

### Chi-Square and ANOVA: Do Groups Really Differ?
Folder: `chi-square-and-anova-group-differences/`
Files: `Panchal_Assignment2_Code.R`, `Panchal_Assignment2_Report.docx`
Data: `data/baseball.csv` — the file the code reads. Other small examples in
this module (blood types, airline performance) have their values typed
directly in the code.
What it does: chi-square tests for categorical counts and ANOVA for comparing
group means, each with a chart showing observed versus expected values.

### College Choice in Numbers: GLM and Logistic Regression on US Colleges
Folder: `college-graduation-logistic-regression/`
Files: `Panchal_Assignment3_Code.R`, `Panchal_Assignment3_Report.docx`
Data: none external — the code uses the built-in `College` dataset from R's
ISLR package, and saves a copy as an Excel file when it runs.
What it does: summarises US colleges by type, finds the variables most
associated with being private, fits logistic models, and checks them with
ROC curves and train/test splits.

### Ridge and Lasso: Keeping a College Graduation Model Honest
Folder: `ridge-and-lasso-college-graduation/`
Files: `Panchal_Assignment4_Code.R`, `Panchal_Assignment4_Report.docx`
Data: none external — again the built-in ISLR `College` dataset, split
80% train / 20% test in the code.
What it does: fits ridge and lasso regression for graduation rate, tunes the
penalty with cross-validation, compares coefficients and RMSE, and shows
which predictors survive shrinkage.

### Nonparametric Tests: Medians, Ranks and Simulations Without Normality Assumptions
Folder: `nonparametric-tests-medians-ranks-simulations/`
Files: `Panchal_Assignment5_Code.r`, `Panchal_Assignment5_Report.docx`
Data: none external — all values are typed in the code (for example, the
game-attendance claim is a median of 3000 across 20 games, and the lottery
claim is 200 tickets a day across 40 sampled days).
What it does: sign tests, Wilcoxon rank-sum and signed-rank tests,
Kruskal–Wallis, Spearman correlation, and simulation studies, each stated
as a hypothesis, a test, and a plain-language decision.

### Charts
Folder: `images/` — 15 representative charts, three per module, taken from
the original plot folders: regression diagnostics and sale-price density,
blood-type and airline distributions, applications versus graduation rate
and ROC curves, ridge/lasso comparisons and cross-validation curves, and
Wilcoxon scenarios with attendance and lottery histograms.

## How to use it

1. Install R and RStudio.
2. Put a module's code file in a folder with `data/` alongside it where the
   module needs a dataset (Modules 1 and 2 only — Modules 3, 4 and 5 need no
   external file, as noted above).
3. Open the code file in RStudio and run it top to bottom. Each module
   creates its own plots as it runs.
4. Read the matching `.docx` report for the written-up results and charts.

## Tools

R, R Markdown, ggplot2, dplyr, glmnet, caret, ISLR, BSDA, coin.

## What was left out, and why

Only the submitted version of each module (code + report) is included.
Working copies labelled Practice 1 / Practice 2 / Practice 3 in the course
folder are near-identical duplicates and are not double-uploaded. Professor
material, assignment briefs with rubrics, and RStudio project files are not
included. The Boston 311 final project is a separate repo, not part of this
series.

## Conclusion

Together, the five modules show the full arc of applied statistics in one
course: check a model, compare groups, predict a category, regularise a
model so it generalises, and fall back to rank-based tests when normality
cannot be assumed. A reader who works through them in order will have seen
each method on real, concrete data — house prices, colleges, sports, and
everyday claims about medians — rather than on abstract examples.
