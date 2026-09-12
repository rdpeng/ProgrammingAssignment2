rankall <- function(outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

    valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
    if (!outcome %in% valid_outcomes) {
        stop("invalid outcome")
    }

    outcome_col <- c(
        "heart attack" = 11,
        "heart failure" = 17,
        "pneumonia" = 23
    )

    col_index <- outcome_col[outcome]
    states <- sort(unique(data$State))

    hospital_vector <- c()
    state_vector <- c()

    for (s in states) {
        state_data <- data[data$State == s, ]
        rates <- as.numeric(state_data[, col_index])

        valid <- !is.na(rates)
        state_data <- state_data[valid, ]
        rates <- rates[valid]

        ordered_indices <- order(rates, state_data$Hospital.Name)
        state_data <- state_data[ordered_indices, ]

        total_hospitals <- nrow(state_data)

        selected_hospital <- NA
        if (total_hospitals > 0) {
            if (num == "best") {
                selected_hospital <- state_data$Hospital.Name[1]
            } else if (num == "worst") {
                selected_hospital <- state_data$Hospital.Name[total_hospitals]
            } else if (is.numeric(num)) {
                if (num <= total_hospitals) {
                    selected_hospital <- state_data$Hospital.Name[num]
                }
            }
        }

        hospital_vector <- c(hospital_vector, selected_hospital)
        state_vector <- c(state_vector, s)
    }

    return(data.frame(hospital = hospital_vector, state = state_vector, row.names = state_vector))
}
