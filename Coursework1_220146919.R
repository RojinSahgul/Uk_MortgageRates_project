#' ---
#' title: Time Series Week NNN
#' author: You
#' date: Today
#' ---

# 1. R Scratchpad ------------------------------------------------------------
install.packages("prophet")
remotes::install_github('facebook/prophet@*release', subdir='R')
1
sessionInfo()
unloadNamespace("cli")
search()
remotes::install_github('facebook/prophet@*release', subdir='R', force = TRUE)
4

list.files(path = ".", all.files = TRUE)
system("git status")


install.packages("cli")
install.packages("C:/Users/Sahgu/Downloads/cli_3.6.4.zip", repos = NULL, type = "win.binary")
unloadNamespace("cli")
rm(list = ls())
install.packages("remotes")

UK_Mortgage_Interest_Rates <- read_csv("data/UK Mortgage Interest Rates.csv")
head(UK_Mortgage_Interest_Rates)
mortgage_data <- UK_Mortgage_Interest_Rates[, c("Date", "Bank_Rate")]
head(mortgage_data)
mortgage_data$Date <- as.Date(mortgage_data$Date, format = "%d/%m/%Y")
head(mortgage_data)
library(dplyr)
mortgage_data <- mortgage_data %>%rename(ds = Date, y = Bank_Rate)
head(mortgage_data)
str(mortgage_data)
library(prophet)
str(mortgage_data)
InterestRateModel<-prophet(mortgage_data)
FutureRates=make_future_dataframe(InterestRateModel, periods=24, freq='month')
PredictedRates=predict(InterestRateModel,FutureRates)
plot(InterestRateModel, PredictedRates)


prophet_plot_components(InterestRateModel, PredictedRates)

library(ggplot2)
ggplot(mortgage_data, aes(x = ds, y = y)) +
    geom_line() +
    ggtitle("Mortgage Interest Rate Time Series")

install.packages("lmtest")
library(lmtest)
model_lm <- lm(y ~ ds, data = mortgage_data)
bp_test <- bptest(model_lm)
print(bp_test)

mortgage_data$log_y <- log(mortgage_data$y)
model_lm_log <- lm(log_y ~ ds, data = mortgage_data)
bptest(model_lm_log)


install.packages("sandwich")
library(sandwich)
library(lmtest)

coeftest(model_lm, vcov = vcovHC(model_lm, type = "HC3"))

# 2. Use Sectioning ----------------------------------------------------------
#   Comment lines start with #, they are not read by R
#   If you end comment lines with space and four minus signs -
#   they will be interpreted as section headings.
#   You can add more - to visually separate sections.
#   CTRL+SHIFT+R / ⌘+SHIFT+R creates a new section and adds the hyphens.
#
#   These sections are accessible in
#     - the drop-down list on the bottom left of the scripting area,
#       ALT+SHIFT+J / ⌘+SHIFT+J brings it up
#   and
#     - the outline section on the top-right corner of the scripting area
#       CTRL+SHIFT+O / ⌘+SHIFT+O brings it up

## 2.1 Subsection -----------------------------------
#   You can also have subsections
#   RStudio does not treat them differently from sections
#   but if you add a extra #, number or spaces they will look
#   different in the outline section.
#   This makes it easier to navigate your R file
#   I use less hyphens for subsections to help visually

### 2.1.1 Subsection -------------------
#   And sub-subsections,...

# 3. Folding sections -----------------------------------------------------
#   You can fold sections by clicking on the little grey down-arrow on the left
#   of the section heading. Or hitting ALT+L/⌘+ALT+L
#   This is useful to hide sections you are not working on
#   SHIFT+ALT+L / ⌘+SHIFT+⌥+L unfolds the section
#   ALT+O / ⌘+⌥+O folds all sections
#   SHIFT+ALT+O / ⌘+SHIFT+⌥+O unfolds all sections

# 4. Etiquette ------------------------------------------------------------
#   It is a good idea (valued in any business environment) to have a certain
#   etiquette when writing code (or anything else really).
#   For instance, I write a blank line before a section heading and not after
#   You can choose your own style, but be consistent, and have the least
#   amount of random variations in your style as possible.

