Final Proj Code
================
Valeria Duran
2022-12-05

``` r
library(tidytext)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(stringr)
library(textdata)
library(tidyverse)
```

    ## ── Attaching packages
    ## ───────────────────────────────────────
    ## tidyverse 1.3.2 ──

    ## ✔ ggplot2 3.4.0     ✔ readr   2.1.3
    ## ✔ tibble  3.1.8     ✔ purrr   0.3.5
    ## ✔ tidyr   1.2.1     ✔ forcats 0.5.2
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(caret)
```

    ## Loading required package: lattice
    ## 
    ## Attaching package: 'caret'
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     lift

``` r
library(ggplot2)
data2 <- read_csv("~/Library/CloudStorage/OneDrive-HarvardUniversity/MeTooHate.csv") #######need to change to make it able to knit
```

    ## Rows: 807174 Columns: 10
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (2): text, location
    ## dbl  (7): status_id, favorite_count, retweet_count, followers_count, friends...
    ## dttm (1): created_at
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
data2$index <- 1:nrow(data2) 
set.seed(11+16+2022)
dat<-data2[sample(nrow(data2), 40000), ]

# grouping tweets by the user 'newid'
dat2 <- dat |> unite(newid, location, followers_count, friends_count, remove=FALSE)
dat2$newid <- as.factor(dat2$newid)

#removing links and stop words 
links <- "https://t.co/[A-Za-z\\d]+|&amp;"
dat3 <- dat2 |> filter(!is.na(text)) |>
  mutate(text = str_replace_all(text, links, "")) |>
  unnest_tokens(word, text, token = "tweets")
```

    ## Using `to_lower = TRUE` with `token = 'tweets'` may not preserve URLs.

``` r
dat3 <- dat3 |> filter(!word %in% stop_words$word) 
head(dat3)
```

    ## # A tibble: 6 × 12
    ##   status_id created_at          favorite…¹ retwe…² newid locat…³ follo…⁴ frien…⁵
    ##       <dbl> <dttm>                   <dbl>   <dbl> <fct> <chr>     <dbl>   <dbl>
    ## 1   1.05e18 2018-10-04 19:36:06          1       0 Geor… Georgi…   17467   12938
    ## 2   1.05e18 2018-10-04 19:36:06          1       0 Geor… Georgi…   17467   12938
    ## 3   1.05e18 2018-10-04 19:36:06          1       0 Geor… Georgi…   17467   12938
    ## 4   1.05e18 2018-10-04 19:36:06          1       0 Geor… Georgi…   17467   12938
    ## 5   1.05e18 2018-10-04 19:36:06          1       0 Geor… Georgi…   17467   12938
    ## 6   1.05e18 2018-10-04 19:36:06          1       0 Geor… Georgi…   17467   12938
    ## # … with 4 more variables: statuses_count <dbl>, category <dbl>, index <int>,
    ## #   word <chr>, and abbreviated variable names ¹​favorite_count, ²​retweet_count,
    ## #   ³​location, ⁴​followers_count, ⁵​friends_count

``` r
#getting average sentiment 
afinn<-get_sentiments("afinn")
summary<- dat3 |> inner_join(afinn, by = "word") |> 
  group_by(newid) |>
  summarize(avgsent=mean(value))

#joining back together for a table that has account characteristics and the average sentiment score grouped by user 
inner_join(summary, dat2, by= 'newid') |> nrow() #27661
```

    ## [1] 27661

``` r
senttable <- inner_join(summary, dat2, by= 'newid')

#remove columns not needed 
senttable <- senttable |>
  subset(select = -c(status_id, created_at, favorite_count, retweet_count,statuses_count))
```

``` r
senttable <- senttable |>
  group_by(newid) |>
  mutate(prop = mean(category)) 
```

``` r
senttable <- senttable |> 
  group_by(newid) |>
  mutate(ntweet=n())
```

``` r
senttable |>
  group_by(newid) |>
  ggplot(aes(x=followers_count)) + 
  geom_histogram() +
  xlab('# of Followers') +
  labs(caption = 'Figure 1. The distribution of Followers by User') +
  theme(plot.caption=element_text(hjust = 0, face = "italic"))
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](testfinalprojoutput_files/figure-gfm/expoloratory%20part%201-1.png)<!-- -->

``` r
senttable |>
 group_by(newid) |>
  ggplot(aes(x=friends_count)) + 
  geom_histogram() +
  xlab('# of Accounts Following') +
  labs(caption = 'Figure 2. The distribution of Accounts Following by User') +
  theme(plot.caption=element_text(hjust = 0, face = "italic"))
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](testfinalprojoutput_files/figure-gfm/expoloratory%20part%201-2.png)<!-- -->

``` r
followeriqr<- IQR(senttable$followers_count)
summary(senttable$followers_count)
```

    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ##        0       97      504    49762     2677 42719326

``` r
504 + (1.5*followeriqr)
```

    ## [1] 4374

``` r
sum(senttable$followers_count>=4374) #5245
```

    ## [1] 5245

``` r
sub <- senttable |>
  filter(followers_count<=4374) 

followingiqr<- IQR(senttable$friends_count)
summary(senttable$friends_count)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##       0     150     508    7720    1709  521250

``` r
508 + (1.5*followingiqr)
```

    ## [1] 2846.5

``` r
sum(senttable$friends_count>=2846) #4829
```

    ## [1] 4829

``` r
sub <- sub |>
  filter(friends_count<=2846) 
nrow(sub) #20576
```

    ## [1] 20576

``` r
head(sub)
```

    ## # A tibble: 6 × 10
    ## # Groups:   newid [6]
    ##   newid        avgsent text  locat…¹ follo…² frien…³ categ…⁴  index  prop ntweet
    ##   <fct>          <dbl> <chr> <chr>     <dbl>   <dbl>   <dbl>  <int> <dbl>  <int>
    ## 1 - GAME OVER…    -1   "Mot… - GAME…       7      68       0 668030     0      1
    ## 2 -34.138895,…    -1   "\nw… -34.13…    2950    2401       0 323040     0      1
    ## 3 -Bermuda_16…    -4   "SO … -Bermu…      16      14       0 597276     0      1
    ## 4 ——— He/Him.…    -2   "Tha… ——— He…     549     165       0 601935     0      1
    ## 5 ‼ जननी,जन्मभू…    -3   "apa… ‼ जननी…     109    1861       0 206765     0      1
    ## 6 ???_7_35        -1.5 "Thi… ???           7      35       0 772084     0      1
    ## # … with abbreviated variable names ¹​location, ²​followers_count,
    ## #   ³​friends_count, ⁴​category

``` r
cor.test(sub$followers_count, sub$friends_count) #0.5652156, p-value < 2.2e-16
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  sub$followers_count and sub$friends_count
    ## t = 98.276, df = 20574, p-value < 2.2e-16
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  0.5558446 0.5744430
    ## sample estimates:
    ##       cor 
    ## 0.5652156

``` r
#ADD SCATTERPLOT WITH LINE
sub |>
  group_by(newid) |>
  ggplot(aes(x=followers_count, y=friends_count)) + 
  geom_point()+
  geom_smooth(method = "lm") + 
  xlab("# of Followers") +
  ylab("# of Following") +
  labs(caption = 'Figure 3. Follower count versus following count per user') +
  theme(plot.caption=element_text(hjust = 0, face = "italic"))
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](testfinalprojoutput_files/figure-gfm/exploratory%20part%202-1.png)<!-- -->

``` r
sub |> 
  group_by(newid) |>
  ggplot(aes(followers_count, prop, size=ntweet)) +
  geom_point() +
  ylab('Proportion of Hateful Tweets') +
  xlab('# of Followers') + 
  labs(caption = 'Figure 4. The proportion of hateful tweets versus number of followers by user') +
  theme(plot.caption=element_text(hjust = 0, face = "italic"))
```

![](testfinalprojoutput_files/figure-gfm/exploratory%20part%203-1.png)<!-- -->

``` r
sub |> 
  group_by(newid) |>
  ggplot(aes(friends_count, prop, size=ntweet)) +
  geom_point() +
  ylab('Proportion of Hateful Tweets') +
  xlab('# of Accounts Following') + 
  labs(caption = 'Figure 5. The proportion of hateful tweets versus number of following count by user') +
  theme(plot.caption=element_text(hjust = 0, face = "italic"))
```

![](testfinalprojoutput_files/figure-gfm/exploratory%20part%203-2.png)<!-- -->

``` r
sub <- sub |> 
  group_by(newid) |>
  mutate(prop = mean(category == 1)) |>
  mutate(hateful=case_when(
    prop > 0 ~ 1,
    prop == 0 ~ 0
  )) 

sub$hateful <- as.factor(sub$hateful)

sub |>
  ggplot(aes(x=hateful, y=avgsent)) + 
  geom_boxplot() +
  xlab('Hateful User') +
  ylab('Avergae Sentiment of Tweets per User') +
  scale_x_discrete(labels=c("1" = "Yes", "0" = "No"))+
  labs(caption = 'Figure 5. Average aentiment among Hateful and nonhateful users') +
  theme(plot.caption=element_text(hjust = 0, face = "italic"))
```

![](testfinalprojoutput_files/figure-gfm/making%20dichotomozed%20hateful%20user%20variable-1.png)<!-- -->

``` r
logit1 <- glm(hateful ~ avgsent + followers_count, data=sub, family='binomial')
summary(logit1)
```

    ## 
    ## Call:
    ## glm(formula = hateful ~ avgsent + followers_count, family = "binomial", 
    ##     data = sub)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -0.9366  -0.6947  -0.5980  -0.4759   2.3634  
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)     -1.533e+00  2.466e-02 -62.180  < 2e-16 ***
    ## avgsent         -1.922e-01  1.077e-02 -17.840  < 2e-16 ***
    ## followers_count -2.009e-04  2.553e-05  -7.867 3.63e-15 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 19765  on 20575  degrees of freedom
    ## Residual deviance: 19360  on 20573  degrees of freedom
    ## AIC: 19366
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
head(data2)
```

    ## # A tibble: 6 × 11
    ##   status_id text     created_at          favor…¹ retwe…² locat…³ follo…⁴ frien…⁵
    ##       <dbl> <chr>    <dttm>                <dbl>   <dbl> <chr>     <dbl>   <dbl>
    ## 1   1.05e18 "Entitl… 2018-09-30 01:17:15       5       1 McAlle…    2253    2303
    ## 2   1.05e18 "Thank … 2018-09-30 01:17:19       5       2 Tampa,…    2559    4989
    ## 3   1.05e18 "Knitti… 2018-09-30 01:17:19       0       0 St Clo…      16     300
    ## 4   1.05e18 "Yep ju… 2018-09-30 01:17:22       1       0 flyove…    3573    3732
    ## 5   1.05e18 "No, th… 2018-09-30 01:17:23       0       0 World       294     312
    ## 6   1.05e18 "Lock J… 2018-09-30 01:17:25       0       0 Rust B…      19      63
    ## # … with 3 more variables: statuses_count <dbl>, category <dbl>, index <int>,
    ## #   and abbreviated variable names ¹​favorite_count, ²​retweet_count, ³​location,
    ## #   ⁴​followers_count, ⁵​friends_count

``` r
head(sub)
```

    ## # A tibble: 6 × 11
    ## # Groups:   newid [6]
    ##   newid        avgsent text  locat…¹ follo…² frien…³ categ…⁴  index  prop ntweet
    ##   <fct>          <dbl> <chr> <chr>     <dbl>   <dbl>   <dbl>  <int> <dbl>  <int>
    ## 1 - GAME OVER…    -1   "Mot… - GAME…       7      68       0 668030     0      1
    ## 2 -34.138895,…    -1   "\nw… -34.13…    2950    2401       0 323040     0      1
    ## 3 -Bermuda_16…    -4   "SO … -Bermu…      16      14       0 597276     0      1
    ## 4 ——— He/Him.…    -2   "Tha… ——— He…     549     165       0 601935     0      1
    ## 5 ‼ जननी,जन्मभू…    -3   "apa… ‼ जननी…     109    1861       0 206765     0      1
    ## 6 ???_7_35        -1.5 "Thi… ???           7      35       0 772084     0      1
    ## # … with 1 more variable: hateful <fct>, and abbreviated variable names
    ## #   ¹​location, ²​followers_count, ³​friends_count, ⁴​category

``` r
dsub<- sub[!duplicated(sub$newid), ]
y <- as.factor(dsub$hateful)
test_index <- createDataPartition(y, times = 1, p = 0.2, list = FALSE)

test_set <- dsub[test_index, ]
train_set <- dsub[-test_index, ]

nrow(train_set) # 14933
```

    ## [1] 14933

``` r
nrow(test_set) # 3735
```

    ## [1] 3735

``` r
test_set$hateful <- as.factor(test_set$hateful)
train_set$hateful <- as.factor(train_set$hateful)
lm_fit <- mutate(train_set, y = hateful == 1)   
lm_fit2<- lm(y ~ avgsent, data = lm_fit) 

#PREDICTION RULE OF 0.5
p_hat <- predict(lm_fit2, test_set)
y_hat <- ifelse(p_hat > 0.5, '1', '0') |> as.factor()
hist(p_hat)
```

![](testfinalprojoutput_files/figure-gfm/using%20average%20user%20sentiment%20to%20predict%20hateful%20accounte-1.png)<!-- -->

``` r
confusionMatrix(y_hat, test_set$hateful)$overall[["Accuracy"]] #0.8353414
```

    ## Warning in confusionMatrix.default(y_hat, test_set$hateful): Levels are not in
    ## the same order for reference and data. Refactoring data to match.

    ## [1] 0.8353414

``` r
cm1 <- confusionMatrix(data=y_hat, reference = test_set$hateful)
```

    ## Warning in confusionMatrix.default(data = y_hat, reference = test_set$hateful):
    ## Levels are not in the same order for reference and data. Refactoring data to
    ## match.

``` r
cm1
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    0    1
    ##          0 3120  615
    ##          1    0    0
    ##                                           
    ##                Accuracy : 0.8353          
    ##                  95% CI : (0.8231, 0.8471)
    ##     No Information Rate : 0.8353          
    ##     P-Value [Acc > NIR] : 0.5108          
    ##                                           
    ##                   Kappa : 0               
    ##                                           
    ##  Mcnemar's Test P-Value : <2e-16          
    ##                                           
    ##             Sensitivity : 1.0000          
    ##             Specificity : 0.0000          
    ##          Pos Pred Value : 0.8353          
    ##          Neg Pred Value :    NaN          
    ##              Prevalence : 0.8353          
    ##          Detection Rate : 0.8353          
    ##    Detection Prevalence : 1.0000          
    ##       Balanced Accuracy : 0.5000          
    ##                                           
    ##        'Positive' Class : 0               
    ## 

``` r
fit_glm <- glm(hateful ~ avgsent + followers_count+friends_count, data=train_set, family = "binomial")
p_hat_glm <- predict(fit_glm, test_set, type="response")
hist(p_hat_glm)
```

![](testfinalprojoutput_files/figure-gfm/adding%20following%20and%20follower-1.png)<!-- -->

``` r
y_hat_glm <- factor(ifelse(p_hat_glm > 0.5, '1', '0')) 
confusionMatrix(y_hat_glm, test_set$hateful)$overall["Accuracy"] #0.8353
```

    ## Warning in confusionMatrix.default(y_hat_glm, test_set$hateful): Levels are not
    ## in the same order for reference and data. Refactoring data to match.

    ##  Accuracy 
    ## 0.8353414

``` r
cm2 <- confusionMatrix(data=y_hat_glm, reference = test_set$hateful)
```

    ## Warning in confusionMatrix.default(data = y_hat_glm, reference =
    ## test_set$hateful): Levels are not in the same order for reference and data.
    ## Refactoring data to match.

``` r
cm2
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    0    1
    ##          0 3120  615
    ##          1    0    0
    ##                                           
    ##                Accuracy : 0.8353          
    ##                  95% CI : (0.8231, 0.8471)
    ##     No Information Rate : 0.8353          
    ##     P-Value [Acc > NIR] : 0.5108          
    ##                                           
    ##                   Kappa : 0               
    ##                                           
    ##  Mcnemar's Test P-Value : <2e-16          
    ##                                           
    ##             Sensitivity : 1.0000          
    ##             Specificity : 0.0000          
    ##          Pos Pred Value : 0.8353          
    ##          Neg Pred Value :    NaN          
    ##              Prevalence : 0.8353          
    ##          Detection Rate : 0.8353          
    ##    Detection Prevalence : 1.0000          
    ##       Balanced Accuracy : 0.5000          
    ##                                           
    ##        'Positive' Class : 0               
    ## 

``` r
str(p_hat)
```

    ##  Named num [1:3735] 0.168 0.174 0.2 0.2 0.128 ...
    ##  - attr(*, "names")= chr [1:3735] "1" "2" "3" "4" ...

``` r
str(p_hat_glm)
```

    ##  Named num [1:3735] 0.17 0.151 0.178 0.209 0.13 ...
    ##  - attr(*, "names")= chr [1:3735] "1" "2" "3" "4" ...

``` r
dfp_hat <- data.frame(p_hat)
dfp_hat_glm <- data.frame(p_hat_glm)

dfp_hat_glm <- dfp_hat_glm |>
  rename("p" = "p_hat_glm")

dfp_hat <- dfp_hat |>
  rename("p" = "p_hat")

dfp <- rbind(dfp_hat, dfp_hat_glm)

ggplot(dfp, aes(x= p)) + 
  geom_histogram(data = dfp_hat, fill = "red", alpha = 0.2) + 
  geom_histogram(data = dfp_hat_glm, fill = "blue", alpha = 0.4) +
  theme()
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](testfinalprojoutput_files/figure-gfm/how%20did%20probability%20change?-1.png)<!-- -->

``` r
table(dsub$ntweet>=2,dsub$hateful==1)
```

    ##        
    ##         FALSE  TRUE
    ##   FALSE 14757  2758
    ##   TRUE    840   313

``` r
sub2 <- sub |>
  filter(ntweet>=2)
nrow(sub2) 
```

    ## [1] 3061

``` r
dsub2<- sub2[!duplicated(sub2$newid), ]
nrow(dsub2)
```

    ## [1] 1153

``` r
y <- as.factor(dsub2$hateful)
test_index <- createDataPartition(y, times = 1, p = 0.2, list = FALSE)

test_set <- dsub2[test_index, ]
train_set <- dsub2[-test_index, ]

class(dsub2$hateful)
```

    ## [1] "factor"

``` r
test_set$hateful <- as.factor(test_set$hateful)
train_set$hateful <- as.factor(train_set$hateful)
lm_fit <- mutate(train_set, y = hateful == 1)   
lm_fit2<- lm(y ~ avgsent, data = lm_fit) 

#PREDICTION RULE OF 0.5
p_hat <- predict(lm_fit2, test_set)
hist(p_hat)
```

![](testfinalprojoutput_files/figure-gfm/how%20does%20it%20change%20when%20we%20only%20take%20those%20that%20have%20tweeted%20more%20than%20once?-1.png)<!-- -->

``` r
y_hat <- ifelse(p_hat > 0.5, '1', '0') |> as.factor()
confusionMatrix(y_hat, test_set$hateful)$overall[["Accuracy"]] #0.7273
```

    ## Warning in confusionMatrix.default(y_hat, test_set$hateful): Levels are not in
    ## the same order for reference and data. Refactoring data to match.

    ## [1] 0.7272727

``` r
cm1 <- confusionMatrix(data=y_hat, reference = test_set$hateful)
```

    ## Warning in confusionMatrix.default(data = y_hat, reference = test_set$hateful):
    ## Levels are not in the same order for reference and data. Refactoring data to
    ## match.

``` r
cm1
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction   0   1
    ##          0 168  63
    ##          1   0   0
    ##                                          
    ##                Accuracy : 0.7273         
    ##                  95% CI : (0.665, 0.7836)
    ##     No Information Rate : 0.7273         
    ##     P-Value [Acc > NIR] : 0.5339         
    ##                                          
    ##                   Kappa : 0              
    ##                                          
    ##  Mcnemar's Test P-Value : 5.662e-15      
    ##                                          
    ##             Sensitivity : 1.0000         
    ##             Specificity : 0.0000         
    ##          Pos Pred Value : 0.7273         
    ##          Neg Pred Value :    NaN         
    ##              Prevalence : 0.7273         
    ##          Detection Rate : 0.7273         
    ##    Detection Prevalence : 1.0000         
    ##       Balanced Accuracy : 0.5000         
    ##                                          
    ##        'Positive' Class : 0              
    ## 

``` r
fit_glm <- glm(hateful ~ avgsent + followers_count+ friends_count, data=train_set, family = "binomial")
p_hat_glm <- predict(fit_glm, test_set, type="response")
hist(p_hat_glm)
```

![](testfinalprojoutput_files/figure-gfm/how%20does%20it%20change%20when%20we%20only%20take%20those%20that%20have%20tweeted%20more%20than%20once?-2.png)<!-- -->

``` r
y_hat_glm <- factor(ifelse(p_hat_glm > 0.5, '1', '0')) 
confusionMatrix(y_hat_glm, test_set$hateful)$overall["Accuracy"] #0.7272727 
```

    ## Warning in confusionMatrix.default(y_hat_glm, test_set$hateful): Levels are not
    ## in the same order for reference and data. Refactoring data to match.

    ##  Accuracy 
    ## 0.7272727

``` r
cm2 <- confusionMatrix(data=y_hat_glm, reference = test_set$hateful)
```

    ## Warning in confusionMatrix.default(data = y_hat_glm, reference =
    ## test_set$hateful): Levels are not in the same order for reference and data.
    ## Refactoring data to match.

``` r
cm2
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction   0   1
    ##          0 168  63
    ##          1   0   0
    ##                                          
    ##                Accuracy : 0.7273         
    ##                  95% CI : (0.665, 0.7836)
    ##     No Information Rate : 0.7273         
    ##     P-Value [Acc > NIR] : 0.5339         
    ##                                          
    ##                   Kappa : 0              
    ##                                          
    ##  Mcnemar's Test P-Value : 5.662e-15      
    ##                                          
    ##             Sensitivity : 1.0000         
    ##             Specificity : 0.0000         
    ##          Pos Pred Value : 0.7273         
    ##          Neg Pred Value :    NaN         
    ##              Prevalence : 0.7273         
    ##          Detection Rate : 0.7273         
    ##    Detection Prevalence : 1.0000         
    ##       Balanced Accuracy : 0.5000         
    ##                                          
    ##        'Positive' Class : 0              
    ## 
