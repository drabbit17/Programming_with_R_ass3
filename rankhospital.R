rankhospital <- function(state, outcome, num) {
        setwd("/Users/newaesthetic/data/R/coursera/programming_with_R/ass3/rprog-data-ProgAssignment3-data")
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        if (state %in% unique(data[, 7]) != 1) {stop("invalid state")}
        if ( outcome %in% c("heart attack", "heart failure","pneumonia") != 1) {stop("invalid outcome")}
        sample <- subset(data, data[, 7] == state)
        sample[sample == "Not Available"] <-"NA" 
        if (outcome == "heart attack") {
                temp <- suppressWarnings(data.frame(hosp = sample[, 2], n = as.numeric(sample[, 11])))
        } else { 
                if (outcome == "heart failure")  {
                        temp <- suppressWarnings(data.frame(hosp = sample[, 2], n = as.numeric(sample[, 17])))
                } else {
                        temp <- suppressWarnings(data.frame(hosp = sample[, 2], n = as.numeric(sample[, 23])))
                }
        }
        final <- data.frame(temp[order(temp$n, temp$hosp), ], rank = 1:nrow(temp))
        final_clean <- final[complete.cases(final), ]
        
        if (num == "best") {
                return(final_clean$hosp[1])
                } else {
                        if (num == "worst") {
                                return(final_clean$hosp[nrow(final_clean)])
                        } else {
                                if (num>nrow(final_clean)|!is.numeric(num)) {
                                return("NA")
                                } else {
                                        return(final_clean$hosp[num])}
                }
        }
}
        
        
