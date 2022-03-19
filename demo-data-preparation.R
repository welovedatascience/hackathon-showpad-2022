
library(bupaR)
data(patients)

timeline <- patients
colnames(timeline) <- c("document","customer","channel","document_id","event", "time",".order")
attr(timeline,"case_id") <- "customer"
attr(timeline,"activity_id") <- "document"
attr(timeline,"activity_instance_id") <- "document_id"
attr(timeline,"lifecycle_id") <- "event"
attr(timeline,"resource_id") <- "channel"

levels(timeline$document) <- c("product A - details.pdf","order","working with us","survey.pdf","Company presentation.pdf","Product A","Product B.pdf")

# https://stackoverflow.com/questions/19410108/cleaning-up-factor-levels-collapsing-multiple-levels-labels
levels(timeline$channel) <- list(showpad=c("r1","r2","r3","r4"), email=c("r5","r6"), conversation="r7")

names(attr(timeline,"spec")$cols) <- c("document","customer","channel","document_id","event", "time")

set.seed(10)
timeline <- timeline[sample(1:nrow(timeline), replace=TRUE, prob=ifelse(timeline$document=="order",1,1.5)), size=round(0.7*nrow(timeline))]


timeline$channel <- sample(timeline$channel, replace=TRUE)
w <- rep(1,nrow(timeline))
w[timeline$document=="order" & timeline$channel=="showpad"] <- 3

w[timeline$document=="product A - details.pdf" & timeline$channel=="email"] <- 2
w[timeline$document=="survey.pdf" & timeline$channel=="email"] <- 4

w[timeline$document=="working with us" & timeline$channel=="email"] <- 0

timeline <-timeline[sample(1:nrow(timeline),nrow(timeline), replace = TRUE, prob=w),]
