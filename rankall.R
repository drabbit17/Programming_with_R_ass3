rankall <- function(outcome, num=1) {
       
        # set up
        setwd("/Users/newaesthetic/data/R/coursera/programming_with_R/ass3/rprog-data-ProgAssignment3-data")
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        data <- suppressWarnings(data.frame(hospital = data[, 2], state = data[, 7], death_ha = as.numeric(data[, 11]), death_hf = as.numeric(data[, 17]), death_pn = as.numeric(data[, 23])))
        if ( outcome %in% c("heart attack", "heart failure", "pneumonia") != 1) {stop("invalid outcome")}
       
        # build up the data structure
        output <- data.frame(matrix(ncol = 2, nrow = 54))
        colnames(output) <- c("hospital", "state")
        output[, 2] <- unique(data[, 2])
        row.names(output) <- unique(data[, 2])
        
        # break down the dataset according to states and selecte the required outcome column
        rank <- matrix(0, ncol = 1, nrow=length(data$hospital))
        data <- data.frame(data, rank = rank)
        mydfs <- split(data, data$state)
        if (outcome == "heart attack") {
                test <- lapply(mydfs, function(x) x[order(x$death_ha,x$hospital),])
                test <- lapply(test, function(x) transform(x, rank=1:nrow(x)))
                test <- lapply(test, function(x) subset(x, rank!="NA", select = c(hospital, state, death_ha, rank)))
        } else { 
                if (outcome=="heart failure")  {
                        test <- lapply(mydfs, function(x) x[order(x$death_hf, x$hospital), ])
                        test <- lapply(test, function(x) transform(x, rank = 1:nrow(x)))
                        test <- lapply(test, function(x) subset(x, rank != "NA", select = c(hospital, state, death_hf, rank)))
                } else {
                        test <- lapply(mydfs, function(x) x[order(x$death_pn, x$hospital), ])
                        test <- lapply(test, function(x) transform(x, rank = 1:nrow(x)))
                        test <- lapply(test, function(x) subset(x, rank != "NA", select = c(hospital, state, death_pn, rank)))
                }
        }
        
        final <- unsplit(test,data$state)
        final <- final[order(final$rank, final$state), ]

        for (i in unique(data[, 2])) {
                temp <- subset(final, state == i)
                temp <- temp[order(temp$rank, temp$hospital), ]
                row.names(temp) <- as.numeric(temp$rank)
                if (num == "best") {
                        output[i, 1] <- temp[1, 1]
                } else {
                        if (num == "worst") {
                                output[i, 1] <- temp[max(sum(!is.na(temp[, 3]))), 1]
                        } else {
                                if (num >= length(temp$rank)) {
                                        output[i, 1] <- NA
                                } else {
                                        output[i, 1] <- temp[num, 1]
                                }
                        }
                }    
        }
        
        output <- output[order(output$state), ]
       return(output)
}
