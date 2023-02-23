Dominance
================

Visualization to demonstrate the relative importance of predictors from
a regression model using general dominance weights.  
[@AJThurston](https://twitter.com/AJThurston)

``` r
library(summarytools)
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(lm.beta)
library(scales)
library(Cairo)
library(domir)
```

## Introduction

In academic contexts, explanation of a regression models typically focus
on the variance accounted for by the model and the beta weights
associated with each predictor. However, interpretation of beta weights
is difficult for lay audiences, and multicolinearity (i.e., highly
correlated predictors) can lead to misleading results (e.g., bouncing
betas). Further, non-technical audiences tend to give comparably less
weight to the relationships between predictors and criteria. Instead,
they tend to give more weight the rank order of the most important
predictors in order to prioritize direct attention or resources.

Relative importance analyses are tools for variance decomposition. They
address multicolinearity of predictors, offer non-technical audiences
with a ranking of the most important predictors and their relative
contribution to variance explained, and offer an intuitive visual aid
for describing incremental validity. There are two main relative
importance analyses: relative weights analysis (Johnson, 2000; Johnson &
Lebreton, 2004; Tonidandel & LeBreton, 2011) and dominance analysis
(Budescu, 1993; Azen & Traxel, 2009; Luchmann, 2014). In general,
relative weights analysis used to be preferred as it was less
computationally intensive; however, dominance analysis is preferred as
it is mathematically accurate, and should especially be preferred in
high-stakes testing settings or when statistically significant
differences between the relative contribution of predictors is the focus
of the test.

## Data Import

First, let’s import the example data and quickly orient ourselves to it.
There are three variables in the file:

1.  **id**: This is just an identifier or a unique participant ID
    (integer)
2.  **sex**: Participant sex (character)
3.  **eth**: Participant ethnicity (character)
4.  **race**: Participant race (character)
5.  **age**: Participant age in years (integer)
6.  **tenure**: Participant tenure in months (numeric)
7.  **gma**: Participant cognitive ability/general mental ability score
    (integer)
8.  **o**: Participant personality, openness scale total score (integer)
9.  **c**: Participant personality, conscientiousness scale total score
    (integer)
10. **e**: Participant personality, extraversion scale total score
    (integer)
11. **a**: Participant personality, agreeableness scale total score
    (integer)
12. **n**: Participant personality, emotional Stability scale total
    score (integer)
13. **sa**: Participant situation awareness test total score (integer)
14. **sp**: Participant safety performance total score (integer)

Note: all items are self-reported, but cognitive ability was measured
with an ability test. Variables 2 through 13 are predictors, safety
performance (sp) is the criterion.

``` r
# df <- read.csv("https://raw.githubusercontent.com/AJThurston/dominance/master/data/dominance.csv")
df <- read.csv("data/dominance.csv")
head(df)
```

    ##   id    sex          eth  race age tenure gma  o  c  e  a  n sa sp
    ## 1  1   Male Not-Hispanic White  34     36   9 33 37 35 37 39 44 20
    ## 2  2 Female Not-Hispanic White  29    125   7 29 34 34 35 39 41 20
    ## 3  3   Male Not-Hispanic White  37     50   3 27 36 28 27 28 28 23
    ## 4  4   Male Not-Hispanic White  54     13  10 22 26 21 23 29 33 15
    ## 5  5   Male Not-Hispanic White  47     16  12 39 12 23 10 12 41 17
    ## 6  6   Male Not-Hispanic White  44     15  15 38 32 19 35 39 42 21

## Regression Analysis (Baseline)

There are plenty of tools to conduct a linear regression in R. In this
example, we’re demonstrating incremental validity with two blocks
demonstrating the incremental validity of situation awareness over
demographic, cognitive, and personality predicting safety performance.

``` r
mod1 <- lm(data = df, 
           formula = sp ~ sex + eth + race + age + tenure + gma + o + c + e + a + n)

mod2 <- lm(data = df,
           formula = sp ~ sex + eth + race + age + tenure + gma + o + c + e + a + n + sa)

mod1 %>%
  lm.beta(.) %>%
  summary(.)

mod2 %>%
  lm.beta(.) %>%
  summary(.)
```

![](https://raw.githubusercontent.com/AJThurston/dominance/master/img/regression_table.PNG)

## Dominance Analysis

Brief description of dominance analysis here

Need to give them fair warning about the 2<sup>p-1</sup> thing

Given the compute time, if you are following along on a lower power
machine and just want to import the dominance analysis results, use the
`readRDS` lines to import those data.

``` r
# dom1 <- domin(data = df,
#               formula = sp ~ sex + eth + race + age + tenure + gma + o + c + e + a + n, 
#               reg = lm, 
#               fitstat = list(summary, "r.squared"))
# 
# dom2 <- domin(data = df,
#               formula = sp ~ sex + eth + race + age + tenure + gma + o + c + e + a + n + sa, 
#               reg = lm, 
#               fitstat = list(summary, "r.squared"))

dom1 <- readRDS("data/dom1.rds")
dom2 <- readRDS("data/dom2.rds")
```

Now would be the time to migrate these values into a PowerPoint if
you’re doing a one-off.

## Results Formatting

Tidy format and collapsing groups of predictors

``` r
res <- list(dom1$General_Dominance,dom2$General_Dominance) %>%
  do.call(bind_rows, .) %>%
  as.data.frame() %>%
  mutate(tenure = 0) %>% # cannot have negative dominance weights
  mutate(personality = rowSums(.[c("o","c","e","a","n")])) %>%
  mutate(demographic = rowSums(.[c("sex","eth","race","age","tenure")])) %>%
  mutate(block = as.factor(row_number())) %>%
  subset(., select = c(block,gma,sa,personality,demographic) ) %>%
  gather(var, val, -block) 

# Setting factor order
res$var <- res$var %>%
  factor(., levels = c("sa","personality","demographic","gma"))

res$var_fullname <- res$var %>%
  recode_factor(., 
                "sa" = "Situation Awareness",
                "personality" = "Personality",
                "gma" = "Cognitive Ability",
                "demographic" = "Demographics")

res$lbl <- paste(res$var_fullname,"\n", percent(round(res$val, digits = 3)), sep = "")

totals <- res %>%
  aggregate(val ~ block, ., sum)
totals$lbl <- paste("Total:", percent(round(totals$val, digits = 3)))
```

Description of the data analysis here and perhaps a bit on tidy format
data

## Plot Parameters

``` r
txt.siz <- 10                              #Size for all text in the plot
x.ll    <- 0                              #Y-axis lower limit          
x.ul    <- .6                              #Y-axis upper limit
p.title <- "Incremental validity of Situation Awareness predicting Safety Performance" # Plot title
s.title <- element_blank()
l.title <- "Predictors" # Legend Title
x.title <- expression(paste("Percentage of ", italic(R)^2," accounted for by predictor")) #X-axis title
y.title <- "Regression block"   #Y-axis title
```

## Dominance Plot

Dominance plot notes and explanation here

``` r
p <- ggplot(res)
p <- p + geom_bar(aes(x = val, y = block, color = var, fill = var, label = lbl),
  stat = "identity", width = .25)
p <- p + geom_text_repel(data = res %>% filter(block == "1"),
                         aes(x = val, y = block, color = var, label = lbl),
                         ylim = 2.25,
                         nudge_x = .065,
                         direction = "both",
                         force = 1)
p <- p + geom_text_repel(data = res %>% filter(block == "2"),
                         aes(x = val, y = block, color = var, label = lbl),
                         ylim = 1.5,
                         direction = "x",
                         force = 2)
p <- p + geom_text(totals, mapping = aes(x = val, y = block, label = lbl),
                   hjust = 0,
                   nudge_x = .01)
p <- p + scale_y_discrete(limits = rev)
p <- p + scale_x_continuous(limits = c(x.ll,x.ul), expand = c(0,0),labels = label_percent())

p <- p + scale_fill_manual(
  labels = c('sa' = 'Situation Awareness',
             'personality' = 'Personality',
             'demographic' = 'Demographic',
             'gma' = 'Cognitive Ability'),
  values = c('sa' = '#336666',
             'personality' = '#262626',
             'demographic' = '#7A918D',
             'gma' = '#ae98d7'))

p <- p + scale_color_manual(
  values = c('sa' = '#336666',
             'personality' = '#262626',
             'demographic' = '#7A918D',
             'gma' = '#ae98d7'))

p <- p + labs(title = p.title,
              subtitle = s.title,
              x = x.title,
              y = y.title,
              fill = l.title)

p <- p + theme(legend.position = "none",
               panel.background = element_rect(fill = "white", color = "black"),
               panel.grid = element_blank(),
               axis.text.y = element_text(color = 'black'),
               axis.text.x = element_text(color = 'black'))
# p
```

![](https://raw.githubusercontent.com/AJThurston/dominance/master/img/dominance.png)

## Export

These are some options for exporting your expectancy chart and the data
for use in other software programs.

``` r
ggsave("img/dominance.png",
       plot = p,
       scale = 1,
       width = 6.5,
       height = 4,
       units = "in",
       dpi = 300,
       type = "cairo-png")

write.csv(res, "data/da_results.csv")
```

## Limitations

> First, DA cannot account for sampling and measurement errors, as is
> true of other analyses (Braun et al., 2019; Tonidandel & LeBreton,
> 2011). Second, although DA was invented to deal with correlations
> among PVs, it cannot rectify multicollinearity when multicollinearity
> is caused by two or more variables measuring the same construct (i.e.,
> construct redundancy; Stadler et al., 2017). Third, DA is not a
> replacement for multiple regression analysis for selecting the best
> set of PVs for the regression formula. That is, DA is an indispensable
> supplement to multiple regression analysis for determining predictor
> importance, but not the other way around. (Mizumoto, 2022)

## References

Azen, R., & Traxel, N. (2009). Using Dominance Analysis to Determine
Predictor Importance in Logistic Regression. Journal of Educational and
Behavioral Statistics, 34(3), 319–347.
<https://doi.org/10.3102/1076998609332754>

Budescu, D. V. (1993). Dominance analysis: A new approach to the problem
of relative importance of predictors in multiple regression.
Psychological Bulletin, 114, 542-551.

Johnson, J. W. (2000). A heuristic method for estimating the relative
weight of predictor variables in multiple regression. Multivariate
Behavioral Research, 35(1), 1-19. <doi:10.1207/S15327906MBR3501_1>

Johnson, J. W., & LeBreton, J. M. (2004). History and use of relative
importance indices in organizational research. Organizational Research
Methods, 7(3), 238-257. <doi:10.1177/1094428104266510>

Luchman, J. N. (2014). Relative Importance Analysis With Multicategory
Dependent Variables: An Extension and Review of Best Practices.
Organizational Research Methods, 17(4), 452-471.
<https://doi.org/10.1177/1094428114544509>

Mizumoto, A. (2023). Calculating the relative importance of multiple
regression predictor variables using dominance analysis and random
forests. *Language Learning*, *73*(1), 161-196.

Tonidandel, S., & LeBreton, J. M. (2011). Relative importance analysis:
A useful supplement to regression analysis. Journal of Business and
Psychology, 26(1), 1-9. <doi:10.1007/s10869-010-9204-3>
