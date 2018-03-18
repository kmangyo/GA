# google anlytics using r
# this package use api v3
# https://github.com/Tatvic/RGoogleAnalytics#installation
# https://developers.google.com/analytics/devguides/reporting/core/v3/
# https://developers.google.com/analytics/devguides/reporting/core/v3/reference#filters

library(RGoogleAnalytics)
library(dplyr)
library(stringr)
library(reshape2)
library(ggplot2)

client.id  <- 'xxxx'
client.secret <- 'xxxx'
token <- Auth(client.id,client.secret)

# save(token,file="./ga_token_file")
# load("./ga_token_file")

ValidateToken(token)

query.init <- Init(start.date = "2017-01-01",
                   end.date = "2017-12-31",
                   dimensions = c("ga:date","ga:pagePath"),
                   metrics = c("ga:pageviews","ga:uniquePageviews","ga:timeOnPage","ga:pageviewsPerSession","ga:avgTimeOnPage"),
                   max.results = 1000,
                   filters = "ga:pagePath=@/2017/",
                   sort = "-ga:pageviews",
                   table.id = "ga:114073268")

query <- QueryBuilder(query.init)
page_data_raw <- GetReportData(query, token)
page_data <- page_data_raw

page_data %>% group_by(pagePath) %>% summarise(pv=sum(pageviews)) %>% arrange (-pv)

# plot
duration_pv <- page_data %>% group_by(start_date) %>% summarise(pv=sum(pageviews))
duration_pv$duration <- with(duration_pv, start_date-as.Date('2018-01-01'))
ggplot(duration_pv, aes(duration, log(pv))) + geom_point()

# find first date
page_data$start <- str_sub(page_data$pagePath, start = 12, end = 21)
page_data$start_len <- str_count(page_data$start)

table(page_data$start_len)
page_data <- subset(page_data, start_len==10)

# interval
page_data_pv_start <- page_data %>% group_by(start=as.Date(start)) %>% summarise(start_date = min(as.Date(date)))
start_date <- as.Date(unique(page_data_pv_start$start_date))
start_intv <- list()

for (i in 1:20){
  start_intv[[i]] <- as.Date(start_date) + 7 * i
}

start_intv <- melt(start_intv)
start_intv <- cbind(start_intv, start=rep(start_date, 20))
names(start_intv) <- c('intv','week','start')
start_intv$week_pre <- as.Date(start_intv$intv) - 7

# matching
page_data$date <- as.Date(page_data$date,format="%Y%m%d")
page_data$start <- as.Date(page_data$start)
page_data <-left_join(page_data, page_data_pv_start, c('start'))

week <- list()
for (i in 1:nrow(page_data)){
  start <- subset(start_intv, start == page_data$start_date[i])
  week_intv <- subset(start, week_pre <= page_data$date[i] & page_data$date[i] < intv)
  week[[i]] <-week_intv$week
}

week <- melt(week)
names(week)<-c('week','seq')

page_data$seq <- 1
page_data$seq <- cumsum(page_data$seq)

page_data_join <- left_join(page_data, week, c('seq'))
page_data_join <- page_data_join[complete.cases(page_data_join), ]

# cohort data
page_cohort_all <- page_data_join %>% group_by(start, week) %>% summarise(PV=sum(pageviews))
ggplot(page_cohort_all %>% group_by(week) %>% summarise(PV=sum(PV)), aes(as.factor(week), log(PV))) + geom_bar(stat = "identity")

page_cohort_sum <- page_cohort %>% group_by(start) %>% summarise(sum_pv=sum(PV)) %>% arrange(-sum_pv) %>% head(5)
page_cohort <- right_join(page_cohort_all, page_cohort_sum, c('start'))

# cohort plot
ggplot(page_cohort %>% group_by(week) %>% summarise(PV=sum(PV)), aes(as.factor(week), log(PV))) + geom_bar(stat = "identity")
ggplot(page_cohort, aes(as.factor(week), log(PV))) + geom_bar(stat = "identity") + facet_grid(reorder(as.factor(start),-sum_pv) ~ .)

# title
title <- subset(page_data, start %in% page_cohort_sum$start)
unique(title$pagePath)
