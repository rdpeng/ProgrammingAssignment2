best <- function(state, outcome) {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

    ## Check that state is valid
    if (!state %in% data$State) {
        stop("invalid state")
    }

    ## Check that outcome is valid
    valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
    if (!outcome %in% valid_outcomes) {
        stop("invalid outcome")
    }

    ## Map outcomes to their respective column indices
    outcome_col <- c(
        "heart attack" = 11,
        "heart failure" = 17,
        "pneumonia" = 23
    )

    ## Filter data for the specified state
    state_data <- data[data$State == state, ]

    ## Extract and convert rates to numeric
    rates <- as.numeric(state_data[, outcome_col[outcome]])

    ## Filter out NA values
    valid <- !is.na(rates)
    state_data <- state_data[valid, ]
    rates <- rates[valid]

    ## Find the hospital with the lowest rate
    lowest <- min(rates)

    best_hospitals <- state_data$Hospital.Name[rates == lowest]

    ## Sort alphabetically in case of a tie and return the first one
    best_hospitals <- sort(best_hospitals)
    return(best_hospitals[1])
}
