rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

    ## Check that state and outcome are valid
    if (!state %in% data$State) {
        stop("invalid state")
    }

    valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
    if (!outcome %in% valid_outcomes) {
        stop("invalid outcome")
    }

    outcome_col <- c(
        "heart attack" = 11,
        "heart failure" = 17,
        "pneumonia" = 23
    )

    ## Filter by state and extract valid rate rows
    state_data <- data[data$State == state, ]
    rates <- as.numeric(state_data[, outcome_col[outcome]])

    valid <- !is.na(rates)
    state_data <- state_data[valid, ]
    rates <- rates[valid]

    ## Order hospitals by rate and then by name alphabetically
    ordered_indices <- order(rates, state_data$Hospital.Name)
    state_data <- state_data[ordered_indices, ]

    ## Handle num argument ("best", "worst", or numeric rank)
    total_hospitals <- nrow(state_data)

    if (num == "best") {
        target_row <- 1
    } else if (num == "worst") {
        target_row <- total_hospitals
    } else if (is.numeric(num)) {
        if (num > total_hospitals) {
            return(NA)
        }
        target_row <- num
    } else {
        stop("invalid num")
    }

    return(state_data$Hospital.Name[target_row])
}
