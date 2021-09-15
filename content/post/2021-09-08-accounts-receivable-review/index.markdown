---
title: "Accounts Receivable Review"
author: "Brent Crossman"
date: '2021-09-08'
slug: accounts-receivable-review
categories: []
tags:
- due diligence
- accounts receivable
- revenue
subtitle: ''
summary: ''
authors: []
lastmod: '2021-09-08T08:12:32-04:00'
featured: no
image:
  caption: ''
  focal_point: ''
  preview_only: no
projects: []
---

<script src="{{< blogdown/postref >}}index_files/htmlwidgets/htmlwidgets.js"></script>
<script src="{{< blogdown/postref >}}index_files/pymjs/pym.v1.js"></script>
<script src="{{< blogdown/postref >}}index_files/widgetframe-binding/widgetframe.js"></script>
<script src="{{< blogdown/postref >}}index_files/htmlwidgets/htmlwidgets.js"></script>
<script src="{{< blogdown/postref >}}index_files/pymjs/pym.v1.js"></script>
<script src="{{< blogdown/postref >}}index_files/widgetframe-binding/widgetframe.js"></script>
<script src="{{< blogdown/postref >}}index_files/htmlwidgets/htmlwidgets.js"></script>
<script src="{{< blogdown/postref >}}index_files/pymjs/pym.v1.js"></script>
<script src="{{< blogdown/postref >}}index_files/widgetframe-binding/widgetframe.js"></script>
<script src="{{< blogdown/postref >}}index_files/htmlwidgets/htmlwidgets.js"></script>
<script src="{{< blogdown/postref >}}index_files/pymjs/pym.v1.js"></script>
<script src="{{< blogdown/postref >}}index_files/widgetframe-binding/widgetframe.js"></script>

This will hopefully be the first of many posts highlighting best practices in applying data science techniques to small/medium size business due diligence. The data is real world data that I’ve anonymized. I want to highlight for readers that although this blog focuses primarily on R and the application of data analysis tools, more than half of my work is still in Microsoft products, Excel, Word and PowerPoint. As time passes I might delve into the steps in my analysis that I tend to perform in those tools vs R but for now, most of these posts will focus on ways to “level up” your analysis using R.

## Accounts Receivable (Intro)

The balance sheet is where bad accounting goes to hide. If you want to get a better understanding of the quality of P&L reporting, diligence the balance sheet. When I think of “Quality of Earnings,” I’m trying to answer two main questions 1) Is what I’m seeing as reported earnings on the P&L, which is usually EBITDA as a starting point, likely to turn into cash 2) Can that level of earnings be expected to recur in the future under a relatively similar economic and competitive environment. To that extent, earnings starts with revenue and evidence on whether or not that revenue is likely to turn to cash and repeat in the future can be found via a deep dive into the company’s Accounts Receivable balance.

## Items to review

-   General trends in A/R over time on an absolute and relative basis
-   What is the composition of A/R Agings
-   How has that composition changed over time
-   Crude and less crude estimates of non-collectibility

## Things to look out for

-   Unusual spikes in A/R, particularly around key end of year or end of TTM periods that might imply channel stuffing or revenue recognition issues
-   YoY deterioration in A/R composition (older and older average A/R, or A/R beyond a certain cutoff)

## Reports needed

All the reports I’m using will be exported from QuickBooks. Obviously not all SMBs use QuickBooks but you’d be surprised how many do, and if your analysis works with reports out of QuickBooks it gives you a great starting point for translating them to other systems. (As an aside, I really like QuickBooks over most of the competing products I’ve had to use, and in my line of work I see a lot).

I tend to export all reports as excel files (CSV exports run into to data limits from QuickBooks so I tend to export as Excle files but I remove all the formatting options when exporting)

-   Monthly Balance Sheet
    -   (Reports –&gt; Company & Financial –&gt; Balance Sheet Standard –&gt; Show Columns: Month –&gt; Customize Report and set dates)
-   Monthly P&L
    -   (Reports –&gt; Company & Financial –&gt; Profit & Loss Standard –&gt; Show Columns: Month –&gt; Dates: Custom (enter dates))
-   Accrual Sales Detail
    -   (Reports –&gt; Sales –&gt; Sales by Customer Detail –&gt; Basis: Accrual –&gt; Customize: Column (add Account) –&gt; Dates: Custom (enter dates))
-   Cash Sales Detail
    -   (Reports –&gt; Sales –&gt; Sales by Customer Detail –&gt; Basis: Cash –&gt; Customize: Column (add Account) –&gt; Dates: Custom (enter dates))

I have various Looms I’ve created for clients showing how to do these exports that I can post if helpful.

These are just a few packages that we’ll be using as well as a function I created to present “excel-like” data tables from R.

## Package Load

``` r
library(tidyverse)
library(DT)
library(widgetframe)
library(testthat)
library(scales)
library(janitor)
library(readxl)
library(lubridate)
library(tsoutliers)
library(RcppRoll)
library(knitr)


##Functions
display_table <- function(x){
  table_output <- 
    DT::datatable(as.data.frame(x),
                  extensions = 'Buttons',
                  
                  options = list(
                    pageLength=5,
                    scrollX = TRUE,
                    dom = 'Bfrtip',
                    buttons = list(
                      list(extend = 'collection',
                           buttons = c('excel', 'csv'),
                           text = 'DOWNLOAD DATA')
                    )
                  )) 
  
  table_output
}
```

## Read and Tidy Data

We have to pull in the data from excel, clean up a lot of names, and then I prefer to make it long and tidier

``` r
beg_date <- mdy("01-01-2016")
end_date <- mdy("06-30-2020")

company_name <- "ABC Corp"

balance_sheet <- read_xlsx("./Financial_Data/monthly_balance_sheet.xlsx")
```

    ## New names:
    ## * `` -> ...1
    ## * `` -> ...2
    ## * `` -> ...3
    ## * `` -> ...4
    ## * `` -> ...5
    ## * ...

``` r
balance_sheet %>% head() %>%  knitr::kable()
```

| …1     | …2             | …3               | …4                            | …5  | …6  | Dec 31, 15 | Jan 31, 16 | Feb 29, 16 | Mar 31, 16 | Apr 30, 16 | May 31, 16 | Jun 30, 16 | Jul 31, 16 | Aug 31, 16 | Sep 30, 16 | Oct 31, 16 | Nov 30, 16 | Dec 31, 16 | Jan 31, 17 | Feb 28, 17 | Mar 31, 17 | Apr 30, 17 | May 31, 17 | Jun 30, 17 | Jul 31, 17 | Aug 31, 17 | Sep 30, 17 | Oct 31, 17 | Nov 30, 17 | Dec 31, 17 | Jan 31, 18 | Feb 28, 18 | Mar 31, 18 | Apr 30, 18 | May 31, 18 | Jun 30, 18 | Jul 31, 18 | Aug 31, 18 | Sep 30, 18 | Oct 31, 18 | Nov 30, 18 | Dec 31, 18 | Jan 31, 19 | Feb 28, 19 | Mar 31, 19 | Apr 30, 19 | May 31, 19 | Jun 30, 19 | Jul 31, 19 | Aug 31, 19 | Sep 30, 19 | Oct 31, 19 | Nov 30, 19 | Dec 31, 19 | Jan 31, 20 | Feb 29, 20 | Mar 31, 20 | Apr 30, 20 | May 31, 20 | Jun 30, 20 |
|:-------|:---------------|:-----------------|:------------------------------|:----|:----|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|
| ASSETS | NA             | NA               | NA                            | NA  | NA  |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |
| NA     | Current Assets | NA               | NA                            | NA  | NA  |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |
| NA     | NA             | Checking/Savings | NA                            | NA  | NA  |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |         NA |
| NA     | NA             | NA               | PPP Account                   | NA  | NA  |       0.00 |       0.00 |       0.00 |       0.00 |       0.00 |       0.00 |       0.00 |       0.00 |       0.00 |       0.00 |        0.0 |       0.00 |       0.00 |       0.00 |       0.00 |       0.00 |          0 |       0.00 |        0.0 |       0.00 |        0.0 |       0.00 |       0.00 |       0.00 |        0.0 |       0.00 |       0.00 |        0.0 |       0.00 |       0.00 |        0.0 |       0.00 |        0.0 |       0.00 |       0.00 |       0.00 |        0.0 |       0.00 |       0.00 |       0.00 |       0.00 |       0.00 |       0.00 |       0.00 |       0.00 |        0.0 |       0.00 |       0.00 |       0.00 |       0.00 |       0.00 |       0.00 |   64217.72 |   24510.37 |    4022.54 |
| NA     | NA             | NA               | People’s United Bank Checking | NA  | NA  |     113.00 |     113.00 |     113.00 |     113.00 |     113.00 |     113.00 |     113.00 |     113.00 |     113.00 |     113.00 |      113.0 |     113.00 |       0.00 |       0.00 |       0.00 |       0.00 |          0 |       0.00 |        0.0 |       0.00 |        0.0 |       0.00 |       0.00 |       0.00 |        0.0 |       0.00 |       0.00 |        0.0 |       0.00 |       0.00 |        0.0 |       0.00 |        0.0 |       0.00 |       0.00 |       0.00 |        0.0 |       0.00 |       0.00 |       0.00 |       0.00 |       0.00 |       0.00 |       0.00 |       0.00 |        0.0 |       0.00 |       0.00 |       0.00 |       0.00 |       0.00 |       0.00 |       0.00 |       0.00 |       0.00 |
| NA     | NA             | NA               | 1000 · Bank - Checkin         | NA  | NA  |     400.81 |   10383.47 |    2733.38 |   -4883.54 |   10359.86 |    3503.49 |   -2164.82 |    6507.44 |   12089.19 |   34911.03 |    41914.7 |   41997.49 |    2687.74 |   12207.86 |   20026.43 |   26516.25 |      41102 |   27109.37 |    20918.5 |   49212.76 |    14206.1 |   82950.93 |   92615.12 |   98023.27 |    53311.1 |   21867.89 |   39447.39 |    27178.2 |   65535.47 |   23053.64 |    17349.6 |   17892.09 |    31100.3 |   32813.26 |   20683.28 |    7423.15 |    18183.3 |   14393.14 |   24732.31 |   16497.07 |   19891.93 |   39016.92 |   -3765.61 |   27307.78 |   14313.38 |    21047.3 |   14510.22 |   19744.41 |   23589.55 |   20158.68 |    6403.32 |   16076.28 |   37526.86 |   43424.07 |   19542.45 |

``` r
##Need to rename and then concatenate all columns that might contain an account name
long_balance_sheet <- 
  balance_sheet %>% 
  rename_with(~paste0("acct",.),contains("..")) %>% 
  unite("Account", contains("acct"), na.rm = T) %>% 
  pivot_longer(-Account, names_to = "month", values_to = "amount") %>% 
  mutate(month = mdy(month)) %>% 
  filter(!is.na(amount))


profit_and_loss <- read_xlsx("./Financial_Data/monthly_profit_loss.xlsx")
```

    ## New names:
    ## * `` -> ...1
    ## * `` -> ...2
    ## * `` -> ...3
    ## * `` -> ...4
    ## * `` -> ...5
    ## * ...

``` r
profit_and_loss %>% head() %>%  knitr::kable()
```

| …1  | …2                      | …3  | …4     | …5                                 | …6  |  Jan 16 | Feb 16 | Mar 16 |  Apr 16 |   May 16 |   Jun 16 |   Jul 16 |   Aug 16 |   Sep 16 |   Oct 16 |  Nov 16 | Dec 16 |   Jan 17 | Feb 17 | Mar 17 | Apr 17 |  May 17 | Jun 17 |   Jul 17 |  Aug 17 |   Sep 17 |  Oct 17 | Nov 17 | Dec 17 | Jan 18 | Feb 18 | Mar 18 | Apr 18 |  May 18 |   Jun 18 |   Jul 18 |  Aug 18 | Sep 18 |  Oct 18 |   Nov 18 |   Dec 18 | Jan 19 | Feb 19 | Mar 19 | Apr 19 | May 19 |   Jun 19 |   Jul 19 | Aug 19 |   Sep 19 |   Oct 19 | Nov 19 |  Dec 19 |  Jan 20 | Feb 20 | Mar 20 | Apr 20 |   May 20 |  Jun 20 |     TOTAL |
|:----|:------------------------|:----|:-------|:-----------------------------------|:----|--------:|-------:|-------:|--------:|---------:|---------:|---------:|---------:|---------:|---------:|--------:|-------:|---------:|-------:|-------:|-------:|--------:|-------:|---------:|--------:|---------:|--------:|-------:|-------:|-------:|-------:|-------:|-------:|--------:|---------:|---------:|--------:|-------:|--------:|---------:|---------:|-------:|-------:|-------:|-------:|-------:|---------:|---------:|-------:|---------:|---------:|-------:|--------:|--------:|-------:|-------:|-------:|---------:|--------:|----------:|
| NA  | Ordinary Income/Expense | NA  | NA     | NA                                 | NA  |      NA |     NA |     NA |      NA |       NA |       NA |       NA |       NA |       NA |       NA |      NA |     NA |       NA |     NA |     NA |     NA |      NA |     NA |       NA |      NA |       NA |      NA |     NA |     NA |     NA |     NA |     NA |     NA |      NA |       NA |       NA |      NA |     NA |      NA |       NA |       NA |     NA |     NA |     NA |     NA |     NA |       NA |       NA |     NA |       NA |       NA |     NA |      NA |      NA |     NA |     NA |     NA |       NA |      NA |        NA |
| NA  | NA                      | NA  | Income | NA                                 | NA  |      NA |     NA |     NA |      NA |       NA |       NA |       NA |       NA |       NA |       NA |      NA |     NA |       NA |     NA |     NA |     NA |      NA |     NA |       NA |      NA |       NA |      NA |     NA |     NA |     NA |     NA |     NA |     NA |      NA |       NA |       NA |      NA |     NA |      NA |       NA |       NA |     NA |     NA |     NA |     NA |     NA |       NA |       NA |     NA |       NA |       NA |     NA |      NA |      NA |     NA |     NA |     NA |       NA |      NA |        NA |
| NA  | NA                      | NA  | NA     | 5056 · - EIDL Grant Funds -Taxable | NA  |     0.0 |      0 |      0 |    0.00 |     0.00 |     0.00 |     0.00 |     0.00 |     0.00 |     0.00 |    0.00 |      0 |     0.00 |      0 |   0.00 |      0 |     0.0 |      0 |     0.00 |     0.0 |     0.00 |     0.0 |      0 |      0 |      0 |      0 |      0 |      0 |     0.0 |     0.00 |     0.00 |     0.0 |      0 |     0.0 |     0.00 |     0.00 |      0 |      0 |    0.0 |    0.0 |      0 |     0.00 |     0.00 |      0 |     0.00 |     0.00 |    0.0 |     0.0 |    0.00 |    0.0 |      0 |      0 |  8000.00 |     0.0 |   8000.00 |
| NA  | NA                      | NA  | NA     | 4015 · Returned Check Charges      | NA  |     0.0 |      0 |      0 |    0.00 |     0.00 |     0.00 |     0.00 |     0.00 |     0.00 |     0.00 |    0.00 |      0 |     0.00 |      0 |   0.00 |      0 |     0.0 |      0 |     0.00 |     0.0 |     0.00 |     0.0 |      0 |      0 |      0 |      0 |      0 |      0 |     0.0 |     0.00 |     0.00 |     0.0 |      0 |     0.0 |     0.00 |     0.00 |      0 |      0 |   20.0 |    0.0 |      0 |     0.00 |     0.00 |      0 |     0.00 |     0.00 |    0.0 |     0.0 |    0.00 |    0.0 |      0 |      0 |     0.70 |     0.0 |     20.70 |
| NA  | NA                      | NA  | NA     | 4000 · Sales                       | NA  |   150.0 |    125 |      0 | 3560.44 |     0.00 |     0.00 |     0.00 |     0.00 |   265.00 |  3613.00 |  140.00 |  -6582 | -3856.00 |      0 |   0.00 |      0 |     0.0 |      0 |     0.00 |     0.0 |     0.00 |     0.0 |      0 |    285 |      0 |      0 |      0 |      0 |   345.0 |     0.00 |     0.00 |     0.0 |      0 |     0.0 |     0.00 |  1473.00 |      0 |      0 |  397.5 |    0.0 |      0 |     0.00 |   110.00 |     45 |     0.00 |     0.00 |    0.0 | -1474.0 |    0.00 |  115.0 |      0 |      0 |   278.11 |     0.0 |  -1009.95 |
| NA  | NA                      | NA  | NA     | 4001 · Landscaping                 | NA  | 10093.5 |    150 |      0 | 7197.90 | 21675.38 | 16086.47 | 19800.68 | 11576.25 | 81352.35 | 28714.14 | 8369.85 |   7303 | 43915.06 |    225 | 252.85 |      0 | 12191.5 |      0 | 74185.12 | 48057.3 | 44043.53 | 16078.6 |  18322 |      0 |   5351 |      0 |   1352 |    175 | 29963.5 | 24652.46 | 27568.11 | 16960.8 |  26728 | 11674.5 | 22521.84 | 10021.25 |    889 |    180 | 2343.5 |  503.5 |    150 | 12647.75 | 38554.53 |  32274 | 21313.35 | 46604.86 | 5167.8 |   675.5 | 2649.94 | 1111.5 |   1885 |   4655 | 12227.50 | 61802.2 | 892193.87 |

``` r
##Need to rename and then concatenate all columns that might contain an account name. 

long_profit_and_loss <- 
  profit_and_loss %>% 
  rename_with(~paste0("acct",.),contains("..")) %>% 
  unite("Account", contains("acct"), na.rm = T) %>% 
  pivot_longer(-Account, names_to = "month", values_to = "amount") %>% 
  mutate(month = ceiling_date(dmy(paste(1,month)), unit = "month")-1) %>% 
  filter(!is.na(amount)) %>% 
  filter(!is.na(month))  
```

    ## Warning: 88 failed to parse.

## Accounts Receivable Review

-   Time Series analysis of A/R with outlier detection
-   A/R Days (using 3 month moving average)
-   Seasonality review monthly

### A/R Trend

The following trending analysis uses the outlier detection function of the tsoutliers package to help highlight unusual A/R (and potentially unusual revenue months)

``` r
ar_acct <- "1200 · Accounts Receivable"

ar_history <- 
  long_balance_sheet %>% 
  filter(Account == ar_acct)

##Outlier Detection Function
find_outlier <- function (df, value_field, time_field) {
  # browser()
  out_ts = tsoutliers::tso(y = as.ts(df %>% pull({{ value_field }})), types = "AO")
  out = as.data.frame(out_ts$outliers)
  dups = duplicated(out)
  sum.dups = sum(dups)
  if (sum.dups > 0) 
    warning(paste(sum.dups, "duplicate outliers in tsoutliers() output"))
  out = out[!duplicated(out), ]
  i = out[, "ind"]
  df$outlier <- FALSE
  df$outlier[i] <- TRUE
  df <- df %>% filter(outlier)
  return(df)
}

outliers <- find_outlier(ar_history, amount, month)
```

    ## Warning in locate.outliers.iloop(resid = resid, pars = pars, cval = cval, :
    ## stopped when 'maxit.iloop' was reached

``` r
outliers
```

    ## # A tibble: 7 x 4
    ##   Account                    month       amount outlier
    ##   <chr>                      <date>       <dbl> <lgl>  
    ## 1 1200 · Accounts Receivable 2016-09-30 133177. TRUE   
    ## 2 1200 · Accounts Receivable 2017-03-31 195384. TRUE   
    ## 3 1200 · Accounts Receivable 2017-05-31 179594. TRUE   
    ## 4 1200 · Accounts Receivable 2018-06-30 148615. TRUE   
    ## 5 1200 · Accounts Receivable 2018-11-30  90274. TRUE   
    ## 6 1200 · Accounts Receivable 2019-01-31 158171. TRUE   
    ## 7 1200 · Accounts Receivable 2019-10-31 116345. TRUE

``` r
chart_data <- 
  ar_history %>%
  left_join(outliers %>% select(-amount), by = c("month"))

chart_data  %>% 
  ggplot(aes(month, amount)) + 
  geom_point(size = 1)+
  geom_smooth(se=F) +
  expand_limits(y = 0) + 
  theme(legend.position = "bottom") +
  {if(nrow(outliers)>0)geom_point( data=outliers, mapping=aes(color='Outliers'), size=3, shape=0)} +
  xlab('Month') + ylab('AR Balance ($)') +
  scale_x_date(date_labels = "%b %y", breaks = seq.Date(from = min(chart_data$month),
                                                        to = max(chart_data$month),
                                                        by = '3 month'),
               minor_breaks = '1 months')+
  scale_y_continuous(label=scales::comma)+
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))+
  ggtitle( paste(company_name, ' - Monthly AR'),
           subtitle='With outlier months marked.') +
  guides(color = guide_legend(""))
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-2-1.png" width="672" />

We can see the company is carrying a historically low level of A/R. A/R peaked around December 2017. Of particular importance are spikes or drops around key dates, such as TTM periods or fiscal year ends, as they are more likely to impact key financial metrics being used by the buyer. We are using automated outlier detection but don’t ignore what your eye sees. That spike around 12/31/17 may not be an outlier vs other points around it but it trips a lot of red flags and I’d research it

There might be some seasonal trends, which we can review a few ways, the first is a modified Days Sales Outstanding Review, the second is a layered seasonality chart.

#### Days Sales

``` r
num_months_roll <- 3
revenue_acct <- "Total Income"

rolling_avg_sales <- 
  long_profit_and_loss %>% 
  filter(Account == revenue_acct) %>% 
  mutate(x_month_avg_sales =  RcppRoll::roll_sum(amount, num_months_roll, align = "right", fill =NA)/2)

ar_history_sales <- 
  left_join(ar_history, 
            rolling_avg_sales %>%
              select(month, x_month_avg_sales))
```

    ## Joining, by = "month"

``` r
ar_history_sales_days <- 
  ar_history_sales %>% 
  mutate(amount = amount / x_month_avg_sales * (num_months_roll*30)) %>% 
  select(month, amount) %>% 
  filter(!is.na(amount))

outliers <- find_outlier(ar_history_sales_days, amount, month)

outliers
```

    ## # A tibble: 2 x 3
    ##   month      amount outlier
    ##   <date>      <dbl> <lgl>  
    ## 1 2017-06-30   234. TRUE   
    ## 2 2018-02-28   254. TRUE

``` r
chart_data <- 
  ar_history_sales_days %>%
  left_join(outliers %>% select(-amount), by = c("month"))


chart_data  %>% 
  ggplot(aes(month, amount)) + 
  geom_point(size = 1)+
  geom_smooth(se=F) +
  expand_limits(y = 0) + 
  theme(legend.position = "bottom") +
  {if(nrow(outliers)>0)geom_point( data=outliers, mapping=aes(color='Outliers'), size=3, shape=0)} +
  xlab('Month') + ylab('AR Days Sale') +
  scale_x_date(date_labels = "%b %y", breaks = seq.Date(from = min(chart_data$month),
                                                        to = max(chart_data$month),
                                                        by = '3 month'),
               minor_breaks = '1 months')+#, date_breaks = '3 months', minor_breaks = '1 months') +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))+
  scale_y_continuous(label=scales::comma)+
  ggtitle( paste(company_name, ' - Monthly AR')) +
  guides(color = guide_legend(""))
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-3-1.png" width="672" />

From this perspective, looking at A/R in relation to 3 months worth of sales, we see that some of the previous spikes were likely driven organically by revenue. It is still worth researching those periods, the largest A/R balances, oldest balances and largest sales in them but now we also have two more periods where the A/R was high compared to the sales. I’d add those to my punch list of questions for the seller as well, again reviewing the largest A/R balances and sales in those periods.

Let’s look at the seasonality of A/R next

#### Seasonal A/R Review

We’re going to initially normalize based on rolling TTM sales. This would still show seasonal trends but it would help adjust for long-term changes in the level of sales.

``` r
num_months_roll <- 12
revenue_acct <- "Total Income"

rolling_avg_sales <- 
  long_profit_and_loss %>% 
  filter(Account == revenue_acct) %>% 
  mutate(x_month_avg_sales =  RcppRoll::roll_sum(amount, num_months_roll, align = "right", fill =NA)/2)

ar_history_sales <- 
  left_join(ar_history, 
            rolling_avg_sales %>%
              select(month, x_month_avg_sales))
```

    ## Joining, by = "month"

``` r
ar_history_sales_days <- 
  ar_history_sales %>% 
  mutate(amount = amount / x_month_avg_sales * (num_months_roll*30)) %>% 
  select(month, amount) %>% 
  filter(!is.na(amount))

ar_history_sales_days %>% 
  ggplot(aes(month(month, label=TRUE, abbr=TRUE), 
             amount, 
             group=factor(year(month)), colour=factor(year(month)))) +
  geom_line() +
  geom_point() +
  xlab('Month') + ylab('AR Days') +
  scale_y_continuous(label=scales::comma)+
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))+
  ggtitle( paste(company_name, ' - Monthly AR')) +
  guides(color = guide_legend(""))
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-4-1.png" width="672" />

There might be some trends around the winter months being higher but it isn’t crystal clear and may still be being swamped by long-term trends.

## Bad Debt Reserve

At this point the general exploration of accounts receivable has been handy, particularly as it might highlight revenue recognition issues, but in the end an account receivable is a sale that you haven’t collected yet, and some level of sales may never be collected. Let’s look at the aging of A/R over time and then see how aging might impact collectability.

Our first issue is we’d love to get an A/R aging on a timely basis, daily perhaps, that would allow for flexible comparisons of any two time periods, however there is no way to export QuickBooks A/R agings in that way, only a specific date can be exported. Let’s see if we can recreate the A/R aging using the accrual and cash sales detail.

``` r
revenue_accrual <- 
  readxl::read_xlsx(path = "./Financial_Data/sales_detail_accrual.xlsx") %>% 
  select(Type:Amount) %>%
  mutate(Date = as.Date(Date)) %>% 
  filter(!is.na(Date)) %>% 
  mutate(Amount = if_else(is.na(Amount),0,Amount)) %>% 
  rename_all(tolower) %>% 
  mutate(month = lubridate::ceiling_date(date, unit = 'month')-1)
```

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting logical in A19476 / R19476C1: got 'TOTAL'

    ## New names:
    ## * `` -> ...1
    ## * `` -> ...2
    ## * `` -> ...3

``` r
revenue_cash <- 
  readxl::read_xlsx(path = "./Financial_Data/sales_detail_cash.xlsx") %>% 
  rename(Amount = `Paid Amount`) %>% 
  select(Type:Amount) %>%
  mutate(Date = as.Date(Date)) %>% 
  filter(!is.na(Date)) %>% 
  mutate(Amount = if_else(is.na(Amount),0,Amount)) %>% 
  rename_all(tolower) %>% 
  mutate(month = lubridate::ceiling_date(date, unit = 'month')-1)
```

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting logical in A21934 / R21934C1: got 'TOTAL'

    ## New names:
    ## * `` -> ...1
    ## * `` -> ...2
    ## * `` -> ...3

``` r
complete_daily_df <- 
  crossing(date = seq.Date(beg_date, end_date, by = "1 day"), name =unique(revenue_accrual$name)) %>% 
  left_join(revenue_accrual %>% select(date, name, num, amount) %>% 
              group_by(date, name, num) %>% 
              summarise(amount = sum(amount)),
            by = c("date","name")) %>%
  complete(date, nesting(name, num)) %>% 
  mutate(sales = if_else(is.na(amount),0, amount)) %>% 
  select(-amount) %>% 
  left_join(revenue_cash %>% select(date, name, num,  amount)%>% 
              group_by(date, name, num) %>% 
              summarise(amount = sum(amount)),
            by = c("date","name", "num")) %>% 
  mutate(collection = if_else(is.na(amount),0, -amount)) %>% 
  select(-amount) %>% 
  filter(!(sales==0 & collection ==0))
```

    ## `summarise()` has grouped output by 'date', 'name'. You can override using the `.groups` argument.

    ## `summarise()` has grouped output by 'date', 'name'. You can override using the `.groups` argument.

``` r
display_table(complete_daily_df) %>% widgetframe::frameWidget()
```

<div id="htmlwidget-1" style="width:100%;height:480px;" class="widgetframe html-widget"></div>
<script type="application/json" data-for="htmlwidget-1">{"x":{"url":"index_files/figure-html//widgets/widget_unnamed-chunk-5.html","options":{"xdomain":"*","allowfullscreen":false,"lazyload":false}},"evals":[],"jsHooks":[]}</script>

``` r
complete_daily_df %>% 
  # group_by(name, num) %>% 
  summarise(sales = sum(sales),
            collection = sum(collection)) %>% 
  mutate(AR = sales+collection)
```

    ## # A tibble: 1 x 3
    ##      sales collection      AR
    ##      <dbl>      <dbl>   <dbl>
    ## 1 3536114.  -3428062. 108052.

If we pull in the actual A/R aging we can compare the two

``` r
ar_aging_actual <- read_xlsx("./Financial_Data/ar_aging_2020_06_30.xlsx") %>% 
  rename(name = `...2`) %>% 
  rename_all(.funs = tolower) %>% 
  select(name:total)
```

    ## New names:
    ## * `` -> ...1
    ## * `` -> ...2

``` r
ar_aging <- 
  complete_daily_df %>% 
  group_by(name) %>% 
  summarise(sales = sum(sales),
            collection = sum(collection)) %>% 
  mutate(calc_ar = sales+collection) %>% 
  filter(calc_ar !=0) %>% 
  select(name, calc_ar)

ar_comparison <- 
  ar_aging_actual %>% 
  select(name, total) %>% 
  rename(actual_ar = total) %>% 
  full_join(ar_aging, by = c("name")) 


ar_comparison[is.na(ar_comparison)] <- 0

ar_comparison <- 
  ar_comparison %>% 
  filter(name != "TOTAL") %>% 
  mutate(dif = actual_ar-calc_ar) %>% 
  arrange(desc(dif)) 

display_table(ar_comparison %>% mutate_if(is.numeric,scales::comma)) %>% widgetframe::frameWidget()
```

<div id="htmlwidget-2" style="width:100%;height:480px;" class="widgetframe html-widget"></div>
<script type="application/json" data-for="htmlwidget-2">{"x":{"url":"index_files/figure-html//widgets/widget_unnamed-chunk-6.html","options":{"xdomain":"*","allowfullscreen":false,"lazyload":false}},"evals":[],"jsHooks":[]}</script>

So that came out particularly close, most of the differences actually relate to unusual negative A/R balances in the A/R aging. These would be worth researching to determine if it is understated accrual revenue or some form of prepaid revenue liability.

Let’s see the probability of an invoice being collected as it ages. We’ll only use sales data up to the last 6 months as a cutoff to exclude invoices that are likely to be collected but for which not enough time has passed.

``` r
daily_ar <- 
  complete_daily_df %>% 
  group_by(name, num) %>% 
  mutate(calc_ar = cumsum(sales)+cumsum(collection)) %>% 
  arrange(name, num, date)

complete_ar_history <- 
  daily_ar %>% expand(nesting(name,  num), date = seq.Date(beg_date, end_date %m-% months(6), by = "1 day")) %>% 
  left_join(daily_ar %>% select(-sales,-collection))
```

    ## Joining, by = c("name", "num", "date")

``` r
paid_ar <- 
  ar_aging <- 
  complete_daily_df %>% 
  group_by(name, num) %>% 
  summarise(sales = sum(sales),
            collection = sum(collection)) %>% 
  mutate(calc_ar = sales+collection) %>% 
  filter(calc_ar == 0) %>% 
  mutate(status = "paid")
```

    ## `summarise()` has grouped output by 'name'. You can override using the `.groups` argument.

``` r
ar_daily_df <- 
  complete_ar_history %>% 
  fill(calc_ar, .direction = "down") %>% 
  filter(!is.na(calc_ar)) %>%
  group_by(name, num) %>% 
  mutate(days_outstanding = date - date[1]) %>% 
  ungroup() %>% 
  left_join(paid_ar %>% select(name, num, status)) %>% 
  mutate(status = if_else(is.na(status), "outstanding", status))
```

    ## Joining, by = c("name", "num")

``` r
ar_daily_df %>% 
  group_by(days_outstanding, status) %>% 
  summarise(calc_ar = sum(calc_ar)) %>%
  group_by(days_outstanding) %>% 
  mutate(perc = calc_ar/sum(calc_ar)) %>% 
  ggplot(aes(days_outstanding, perc, fill = status)) +
  geom_area(stat = "identity")+
  # theme(legend.position = "bottom") +
  xlab('Days Outstanding') + ylab('% Eventually Collected') +
  scale_fill_viridis_d(option = "plasma")+
  scale_y_continuous(label=scales::percent)+
  ggtitle( paste(company_name, ' - Collection % by Days Outstanding')) 
```

    ## `summarise()` has grouped output by 'days_outstanding'. You can override using the `.groups` argument.

    ## Don't know how to automatically pick scale for object of type difftime. Defaulting to continuous.

    ## Warning: Removed 103 rows containing missing values (position_stack).

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-7-1.png" width="672" />

``` r
ar_summary <- 
  ar_daily_df %>% 
  group_by(days_outstanding, status) %>% 
  summarise(calc_ar = sum(calc_ar)) %>%
  group_by(days_outstanding) %>% 
  mutate(perc = calc_ar/sum(calc_ar))
```

    ## `summarise()` has grouped output by 'days_outstanding'. You can override using the `.groups` argument.

``` r
display_table(ar_summary)  %>% widgetframe::frameWidget()
```

<div id="htmlwidget-3" style="width:100%;height:480px;" class="widgetframe html-widget"></div>
<script type="application/json" data-for="htmlwidget-3">{"x":{"url":"index_files/figure-html//widgets/widget_unnamed-chunk-7.html","options":{"xdomain":"*","allowfullscreen":false,"lazyload":false}},"evals":[],"jsHooks":[]}</script>

``` r
ar_summary %>% 
  filter(status == "paid") %>% 
  ggplot(aes(days_outstanding, perc, group = status, color = status, weight = calc_ar)) +
  geom_point() +
  # theme(legend.position = "bottom") +
  xlab('Days Outstanding') + ylab('% Eventually Collected') +
  # theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))+
  scale_y_continuous(label=scales::percent)+
  scale_color_viridis_d(option="plasma")+
  ggtitle( paste(company_name, ' - Collection % by Days Outstanding')) 
```

    ## Don't know how to automatically pick scale for object of type difftime. Defaulting to continuous.

    ## Warning: Removed 103 rows containing missing values (geom_point).

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-7-3.png" width="672" />

This is a result that we’d expect, as the invoices age, they are less likely to be collected. There are some huge drops, and those might be due to small sample sizes after certain points. Or the company may have written invoices off and treated them as cash payments.

Also, from that table we can see that on the day an invoice was issued (day 0) there would be a 3% chance of it not being paid. This could be a reasonable estimate for bad debt expense as a percent of sales in any modeling/forecasting exercise but understand that figure is not regressed over time at all, meaning it may have worsened or improved recently.

One last note of warning, if the company retroactively edits or deletes invoices rather than using credit memos / bad debt and/or leaving them as outstanding A/R, it will appear that they have 100% collection and most statistical analysis to predict bad debt will fail. This is why a general discussion of the credit memo process is key to the due diligence

### Estimate Reserve / Bad Debt

I do want to highlight that a very classic analysis of A/R that breaks the A/R aging into reasonable time buckets, sets a reserve cutoff date (perhaps 90 days) and then accrues for additional bad debt based on an increase in “aged a/r” beyond that cutoff is a really reasonable approach and will be intuitive to your client and stakeholders, however we can perform a bit more complex regression and apply it against our data to get more accurate estimates of collectability (even if you’re the only one that uses them, or uses them as a check).

Since we have the ability, let’s put together a simple model to predict whether an invoice will be paid based on the balance outstanding and days outstanding. To start though, let’s look again at that last chart but change the x to a power scale to see if we can get a more linear relationship. Specifically we’ll take the Sqrt of Days Outstanding

``` r
ar_summary %>% 
   filter(status == "paid") %>% 
  ggplot(aes(sqrt((as.numeric(days_outstanding))), perc, group = status, color = status, weight = calc_ar)) +
  geom_point() +
  # theme(legend.position = "bottom") +
  xlab('Days Outstanding') + ylab('% Eventually Collected') +
  # theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))+
  scale_y_continuous(label=scales::percent)+
  scale_color_viridis_d(option="plasma")+
  ggtitle( paste(company_name, ' - Collection % by Days Outstanding')) 
```

    ## Warning: Removed 103 rows containing missing values (geom_point).

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-8-1.png" width="672" />

That does seem a more linear representation, let’s use that.

``` r
complete_ar_history <- 
  daily_ar %>% expand(nesting(name,  num), 
                      date = seq.Date(beg_date, end_date %m-% months(6), by = "1 day")) %>% 
  left_join(daily_ar)
```

    ## Joining, by = c("name", "num", "date")

``` r
ar_daily_df <- 
  complete_ar_history %>% 
  fill(calc_ar, .direction = "down") %>% 
  filter(!is.na(calc_ar)) %>% 
  group_by(name, num) %>% 
  mutate(days_outstanding = as.numeric(date - date[1])) %>% 
  mutate(collection = if_else(is.na(collection), 0, -collection)) %>% 
  mutate(future_collection = rev(cumsum(rev(collection)))) %>% 
  ungroup() %>% 
  filter(calc_ar > 1) %>% 
  mutate(sqrt_days = sqrt(days_outstanding)) %>% 
  mutate(perc_collection = pmin(future_collection / calc_ar,1))

fit <- lm(formula = future_collection ~ sqrt_days * calc_ar  , data =  ar_daily_df )

summary(fit)
```

    ## 
    ## Call:
    ## lm(formula = future_collection ~ sqrt_days * calc_ar, data = ar_daily_df)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -23960.9    -45.3     -6.7     27.9  18926.6 
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error  t value Pr(>|t|)    
    ## (Intercept)       -7.988e+00  6.203e+00   -1.288    0.198    
    ## sqrt_days          9.012e+00  6.253e-01   14.412   <2e-16 ***
    ## calc_ar            1.008e+00  1.870e-03  539.031   <2e-16 ***
    ## sqrt_days:calc_ar -2.080e-02  9.784e-05 -212.583   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1279 on 135912 degrees of freedom
    ## Multiple R-squared:  0.844,  Adjusted R-squared:  0.844 
    ## F-statistic: 2.451e+05 on 3 and 135912 DF,  p-value: < 2.2e-16

``` r
ar_daily_df$pred_collection <- round(predict(fit, newdata = ar_daily_df),2)

ar_summary <- 
  ar_daily_df %>% 
  group_by(days_outstanding) %>% 
  mutate(pred_collection = pred_collection) %>% 
  summarise(calc_ar = sum(calc_ar),
            future_collection = sum(future_collection),
            pred_collection = sum(pred_collection)) %>%
  group_by(days_outstanding) %>% 
  mutate(actual = future_collection/sum(calc_ar),
         prediction = pred_collection/sum(calc_ar)) %>% 
  select(days_outstanding, calc_ar, actual, prediction) %>% 
  pivot_longer(cols = actual:prediction, 
               names_to = "type", 
               values_to = "perc_collected")

ar_summary %>% 
  ggplot(aes(days_outstanding, perc_collected, group = type, color = type, weight = calc_ar)) +
  geom_point() +
  # theme(legend.position = "bottom") +
  xlab('Days Outstanding') + ylab('% Eventually Collected') +
  # theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))+
  scale_y_continuous(label=scales::percent)+
  scale_color_viridis_d(option="plasma")+
  ggtitle( paste(company_name, ' - Collection % by Days Outstanding')) 
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-9-1.png" width="672" />

That seems like a pretty good estimate for us to use. Let’s apply that prediction to the entire set of data and calculate an A/R reserve over time. The difference in the A/R reserve between any two time points is a reasonable estimate of additional bad debt expense that should have been accrued in that period.

``` r
complete_ar_history <- 
  daily_ar %>% expand(nesting(name,  num), 
                      date = seq.Date(beg_date, end_date, by = "1 day")) %>%   #took out 6 month cutback
  left_join(daily_ar)
```

    ## Joining, by = c("name", "num", "date")

``` r
ar_daily_df <- 
  complete_ar_history %>% 
  fill(calc_ar, .direction = "down") %>% 
  filter(!is.na(calc_ar)) %>% 
  group_by(name, num) %>% 
  mutate(days_outstanding = as.numeric(date - date[1])) %>% 
  filter(calc_ar > 1) %>% 
  mutate(sqrt_days = sqrt(days_outstanding))
 
ar_daily_df$pred_collection <- round(predict(fit, newdata = ar_daily_df),2)

ar_summary <- 
  ar_daily_df %>% 
  group_by(date) %>% 
  mutate(pred_collection = pred_collection) %>% 
  summarise(calc_ar = sum(calc_ar),
            pred_collection = sum(pred_collection)) %>%
  mutate(bad_debt_reserve = calc_ar-pred_collection)

ar_summary %>% 
  ggplot(aes(date, bad_debt_reserve)) +
  geom_point() +
  geom_line()+
  # theme(legend.position = "bottom") +
  xlab('Days Outstanding') + ylab('% Eventually Collected') +
  # theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))+
  scale_y_continuous(label=scales::comma)+
  scale_color_viridis_d(option="plasma")+
  ggtitle( paste(company_name, ' - Bad debt estimate')) 
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-10-1.png" width="672" />

``` r
t <- display_table(ar_summary) 
```

``` r
widgetframe::frameWidget(t)
```

<div id="htmlwidget-4" style="width:100%;height:480px;" class="widgetframe html-widget"></div>
<script type="application/json" data-for="htmlwidget-4">{"x":{"url":"index_files/figure-html//widgets/widget_unnamed-chunk-11.html","options":{"xdomain":"*","allowfullscreen":false,"lazyload":false}},"evals":[],"jsHooks":[]}</script>

``` r
htmlwidgets::saveWidget(frameableWidget(t),'ar_1.html')
```

<iframe seamless src="/ar_1.html" width="100%" height="500">
</iframe>

Those large drops in A/R jump out even more here. Again, worth researching those.

When it comes to your bad debt reserve, you have to be very precise on dates so let’s look at the last TTM periods and calculate the bad debt reserve.

``` r
ar_summary %>% 
  filter(month(date)==6 & day(date)==30) %>% 
  arrange(year(date)) %>% 
  mutate(bad_debt_expense = bad_debt_reserve - lag(bad_debt_reserve,1))
```

    ## # A tibble: 5 x 5
    ##   date       calc_ar pred_collection bad_debt_reserve bad_debt_expense
    ##   <date>       <dbl>           <dbl>            <dbl>            <dbl>
    ## 1 2016-06-30  71428.          68940.            2488.              NA 
    ## 2 2017-06-30 141718.         111240.           30478.           27990.
    ## 3 2018-06-30 155929.         134418.           21511.           -8967.
    ## 4 2019-06-30  84941.          59591.           25350.            3840.
    ## 5 2020-06-30 108072.          87543.           20529.           -4821.

So in the most recent TTM, the A/R composition has approved and we’d estimate better than expected collection, to the point we could even adjust EBITDA up 4.8K, vs the 2017 period which should have bad debt accrued for it in the range of 28K.

And then again as FYE

``` r
ar_summary %>% 
  filter(month(date)==12 & day(date)==30) %>% 
  arrange(year(date)) %>% 
  mutate(bad_debt_expense = bad_debt_reserve - lag(bad_debt_reserve,1))
```

    ## # A tibble: 4 x 5
    ##   date       calc_ar pred_collection bad_debt_reserve bad_debt_expense
    ##   <date>       <dbl>           <dbl>            <dbl>            <dbl>
    ## 1 2016-12-30 122919.         107247.           15672.              NA 
    ## 2 2017-12-30 180874.         134283.           46591.           30919.
    ## 3 2018-12-30 121151.          95860.           25291.          -21301.
    ## 4 2019-12-30  97806.          77421.           20384.           -4906.

Similar trends here, a bad 2017, with relatively good A/R composition in 2018-2019. Now these would be adjustments to the already expensed bad debt and not surprisingly with our negative bad adjustment they actually took an \$80K bad debt expense in 2018, hence why we saw such improvement in the A/R composition but what this analysis shows us is that expense should partially be spread back into 2017.

## Conclusion

I hope that lays out ways in which you can apply statistical analysis and data visualization techniques to the review of Accounts Receivable. As always, the goal of this type of high level review is to spur the follow-up questions that let you make an informed determination on the quality of the Company’s earnings.
