#require(devtools)
#install_github('timeline','jbryer')
require(timeline)
require(plyr)

rTimeline <- read.csv(
    "https://docs.google.com/spreadsheet/pub?key=0AieeEIaS0AOsdHpPbmRaZkJtSW44M2pFQnRMaEJLcEE&single=true&gid=0&output=csv",
    stringsAsFactors = FALSE
)
#seems easier to me to get in the default column order
#expected by timeline
#label, group, start, end
rTimeline.clean <- rTimeline[,c(3,3,1,2)]

colnames(rTimeline.clean) <- c(
    "Release",
    "Group",
    "Start",
    "End"
)
#really don't have groups
#make same group or do by r.?
rTimeline.clean[,c(2)] <- substr(rTimeline.clean[,2],1,3)
#convert the text dates to dates
rTimeline.clean[,c(3,4)] <- lapply(
    rTimeline.clean[,c(3,4)],
    FUN = as.Date,
    format = "%m/%d/%Y"
)
#in the Google Doc, I have a phony date to provide a start slide
#get rid of this last row
rTimeline.clean <- rTimeline.clean[-nrow(rTimeline.clean),]
#sort by release date; spreadsheet is in decreasing order
rTimeline.clean <- rTimeline.clean[order(rTimeline.clean[,3]),]
#another way to do it shade r.? and then add each release as event
groupDates <- ddply(
    rTimeline.clean,
    .(Group),
    function(x){return(c(x[1,3],x[nrow(x),4]))}
)
groupDates[,4] <- rep("Release",nrow(groupDates))
colnames(groupDates) <- c("Release","Start","End","Group")
#define events for the timeline
events <- data.frame(
    rbind(
        c("ts","1999-08-27"),
        c("lattice/grid","2002-04-29"),
        c("zoo","2004-10-08"),
        c("zoo/lattice","2006-07-06"),
        c("PeformanceAnalytics","2007-02-02"),
        c("ggplot2","2007-06-10"),
        c("quantmod/TTR","2007-10-07"),
        c("xts","2008-02-17"),
        c("timeSeries","2009-05-17"),
        c("googleVis","2010-11-30"),
        c("xtsExtra","2012-05-30"),
        c("zoo/ggplot2","2012-10-14"),
        c("rCharts","2013-04-10")
    ),
    stringsAsFactors = FALSE
)
colnames(events) <- c("Package","Date")
events$Date <- as.Date(events$Date)

g2 <- timeline(
    df = groupDates,
    events = events,
    label.col = "Release",
    group.col ="Group",
    start.col = "Start",
    end.col="End",
    event.label.col = "Package",
    event.col = "Date",
    event.above = FALSE,
    text.position = "center",
    event.text.vjust = 1,
    event.text.size = 6,
    event.spots = 3
) + labs(title = "Significant Events of R Time Series Plotting")
g2
