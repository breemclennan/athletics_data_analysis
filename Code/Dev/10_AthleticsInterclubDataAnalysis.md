Athletics Interclub Results Data Analysis
================

Objective & Purpose of the Analysis
-----------------------------------

The objective is to explore the interclub athletic competition results data for the complete 2017-18 season and identify: - How athletes are naturally grouped in terms of performance and other factors - Basic descriptive statistics across the entire dataset o How many competitions per athlete o How many events per athlete o Min, max, quartiles, medians for each event o Geography statistics: By whole of Victorian state and by competition zone/region - Further opportunities for athletics data analysis

The purpose of this project is to uncover and document valued actionable insights which are contained within the available source data for the benefit of the target audience.

Key questions to guide the analysis
-----------------------------------

-   What are the athlete attendance rates at competitions during the season? o Context: Competition pre-entry is a new initiative this season for 2017-18. Athletes are encouraged to pre-enter 3 days before the start of interclub competition. This allows officials sufficient time to create seedings and start lists for competition events. Of the number of pre-entries, can we link this to the actual performance results to identify if an athlete followed through? Gaps in this would indicate either “no-show” or athlete pre-entry on the day.
-   From the number of total registered athletes this season, how many athletes have competed in at least one competition? o Follow up competition; how many athletes per accumulated numbers of competition?
-   How many athletes compete at “away” venues. o Context: Amended rules for interclub 2017-18, athletes may now compete at other venues which are not their club’s “home” venue or region, without point scoring penalty. o Technical: In analysis, we can plot the geographical location for venues. We can construct a reference table for this.
-   Do athletes compete at more than one venue per round of interclub competition? o Do points get awarded?
-   Do athletes compete twice in the same event? Either as same age group or different?
-   Do particular track and field events group together in participation rates? What are the common event sequences and pathways? o Context: This will be bound by interclub round and program o Technical: We will need to overlay the program time tables by venue, round and program number
-   Do athletes who are being coached record better performances? o Technical: This will require a method of being able to link athlete with coach.
-   Does weather have an impact on competition performances? If so, by how much? o Technical: Wind readings will be available for some events, for general weather conditions we will need to link in weather history. Specific questions from Geelong Athletics
-   Can we calculate points based on finishing order to assist with our end of season awards? o Technical: We need to obtain and unpack the specific business rules Geelong Athletics use to achieve this.

Context specific process flow for this analysis
-----------------------------------------------

1.  Obtain target audience input and finalise project objective

2.  Obtain source data

3.  Risk assessment on source data with respect to objective

4.  Load data, prepare raw dataset

5.  Check data

6.  Prepare data

    • Check point: Descriptive statistics on raw data
7.  Merge data

    • Check point: Descriptive statistics on merged data
8.  Transform data

9.  Model data (iterative)

    • Consider feature engineering • Consider range and scaling of data
10. Prepare report
11. Peer review
12. Publish
13. Obtain feedback, review and apply updates where appropriate

You can include R code in the document as follows:

``` r
summary(cars)
```

    ##      speed           dist       
    ##  Min.   : 4.0   Min.   :  2.00  
    ##  1st Qu.:12.0   1st Qu.: 26.00  
    ##  Median :15.0   Median : 36.00  
    ##  Mean   :15.4   Mean   : 42.98  
    ##  3rd Qu.:19.0   3rd Qu.: 56.00  
    ##  Max.   :25.0   Max.   :120.00

Including Plots
---------------

You can also embed plots, for example:

![](10_AthleticsInterclubDataAnalysis_files/figure-markdown_github/pressure-1.png)

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
