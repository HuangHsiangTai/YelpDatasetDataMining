library("jsonlite")
library("dplyr")
library("ggplot2")
library("ggmap")
gen.small.data <- function(path = "yelp_dataset_challenge_academic_dataset", n = 10) {
  filenames = c("business", "checkin", "review", "tip", "user")
  filename<-lapply(as.list(filenames), function(x) {
    JSonFile <- paste0(path,"/yelp_academic_dataset_",x,".json")
    output <- paste0(path,"/yelp_academic_dataset_",x, n, ".json")
    src <- file(JSonFile)
    des <- file(output)
    head=readLines(src, n)
    close(src)
    writeLines(head, des)
    close(des)
    output
  })
  filename
}

load.data <- function(path = "yelp_dataset_challenge_academic_dataset") {
  filenames = c("business", "checkin", "review", "tip", "user")
  data<-lapply(as.list(filenames), function(x) {
    RDSFile <- paste0(x,".RDS")
    JSonFile <- paste0(path,"/yelp_academic_dataset_",x,".json")
    if(file.exists(RDSFile)) {
      dat<-readRDS(RDSFile)
    }else {
      ori<-stream_in(file(JSonFile))
      dat<-flatten(ori, recursive = TRUE)
      names(dat)<- gsub(" ","",names(dat))
      if(sum("categories" %in% names(dat))) {
        dat$categories <-sapply(dat$categories, function(x) paste0(x, collapse=","))
      }
      if(sum("neighborhoods" %in% names(dat))) {
        dat$neighborhoods <-sapply(dat$neighborhoods, function(x) paste0(x, collapse=","))
      }
      if(sum("friends" %in% names(dat))) {
        dat$friends <-sapply(dat$friends, function(x) paste0(x, collapse=","))
      }
      if(sum("elite" %in% names(dat))) {
        dat$elite <-sapply(dat$elite, function(x) paste0(x, collapse=","))
      }
      if(sum("attributes.AcceptsCreditCards" %in% names(dat))) {
        dat$attributes.AcceptsCreditCards <- sapply(dat$attributes.AcceptsCreditCards, function(x){
          out = NA
          if(is.logical(x)){
            out <-x
          }
          out
        } )
      }
      saveRDS(dat, file= RDSFile)
    }
    dat
  })
  names(data) <- filenames
  data
}

get.top.state <- function(business, num) {
  state <-names(head(sort(table(business$state),TRUE),num))
  gsub("AZ","Arizona",state)
}

check.business.location <- function(business, state.name) {
  df <- matrix(unlist(sapply(state.name, geocode)), nrow=2)
  df <-t(df)
  colnames(df) <- c("lon", "lat")
  df<-data.frame(df)
  df$state <- state.name
  str(df)
  usa_center = as.numeric(geocode("United States"))
   USAMap<-ggmap(get_googlemap(center=usa_center, scale=4, zoom=4), extent="normal")
   USAMap + geom_point(aes(lon,lat,color =state ), data=df , alpha=0.4,size=5)

  
  
}
get.category.ratio<- function() {
  
}

