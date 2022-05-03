#' Adjust censoring times.
#'
#' \code{csh_censor} adjusts the censoring times given a follow-up window.
#'
#' @param df Data frame containing raw data for event status and event times.
#' @param csh List giving the model of each specific cause.
#' @param fuw The length of the follow-up window in time units.
#'
#' @details Each specific cause must have its own time and status variables.
#' The main cause of interest must be first.
#'
#' Specifically, for each specific cause, the list is required to have the
#' following elements:
#' \describe{
#'    \item{Cause}{The name of the specific cause}.
#'    \item{Time}{A string containing the name of the variable with the time.}
#'    \item{Status}{A string containing the name of the variable with the censoring status.}
#'    \item{vars}{A vector of covariates for the specific cause.}
#' }
#'
#' @export

csh_censor <- function(df, csh, fuw) {

    dfc <- df

    # Set the time variable for each cause to the minimum time observed.  If there was an event,
    # keep that record, otherwise, censor the time at the minimum time observed.

    min_time_obs <- do.call(pmin, df[, sapply(csh, "[[", "Time")])

    index <- which(rowSums(df[, unlist(lapply(csh, "[[", "Status"))]) == 0)

#    for (i in 1:length(csh)) {
#        dfc[index, csh[[i]]$Time] <- min_time_obs[index]
#        dfc[index, csh[[i]]$Status] <- ifelse(df[index, csh[[i]]$Time] == min_time_obs[index], df[index, csh[[i]]$Status], 0)
#    }

    for (j in index) {
        if(df[j, csh[[1]]$Time] != min_time_obs[j]) {
            for (k in 1:length(csh)) {
                dfc[j, csh[[k]]$Time] <- min_time_obs[j]
                dfc[j, csh[[k]]$Status] <- ifelse(df[j, csh[[k]]$Time] == min_time_obs[j], df[j, csh[[k]]$Status], 0)
            }
        }

        if(df[j, csh[[1]]$Time] == min_time_obs[j]) {
            for (k in 2:length(csh)) {
                for (l in 2:length(csh)) {
                    dfc[j, csh[[k]]$Time] <- min(dfc[j, csh[[k]]$Time], dfc[j, csh[[l]]$Time])
                }
            }
        }

    }

    for (j in setdiff(1:nrow(df), index)) {
        for (i in 2:length(csh)) {
            if (df[j, csh[[i]]$Time] < df[j, csh[[1]]$Time]) {
                dfc[j, csh[[1]]$Time] <- min(df[j, csh[[i]]$Time], dfc[j, csh[[1]]$Time])
                dfc[j, csh[[1]]$Status] <- 0
            }
            for (k in 2:length(csh)) {
                if (df[j, csh[[k]]$Time] < df[j, csh[[i]]$Time]) {
                    dfc[j, csh[[i]]$Time] <- min(df[j, csh[[k]]$Time], dfc[j, csh[[i]]$Time])
                    dfc[j, csh[[i]]$Status] <- 0
                }
            }
        }

        if (dfc[j, csh[[1]]$Status]==0) {
            for (i in 2:length(csh)) {
                if (df[j, csh[[1]]$Time] + fuw < df[j, csh[[i]]$Time]) {
                    dfc[j, csh[[i]]$Time] <- min(df[j, csh[[1]]$Time] + fuw, dfc[j, csh[[i]]$Time])
                    dfc[j, csh[[i]]$Status] <- 0
                }
            }
        }

        if (dfc[j, csh[[1]]$Status]==1) {
            for (i in 2:length(csh)) {
                if (df[j, csh[[1]]$Time] < df[j, csh[[i]]$Time]) {
                    dfc[j, csh[[i]]$Time] <- min(df[j, csh[[1]]$Time], dfc[j, csh[[i]]$Time])
                    dfc[j, csh[[i]]$Status] <- 0
                }
            }
        }

    }

    return(dfc)
}

#' Fit the cause-specific hazard model.
#'
#' \code{csh_fit} fits the cause-specific hazard model specified.
#'
#' @param dfc Data pre-processed to be censored properly.
#' @param csh List giving the model of each specific cause.
#'
#' @return Returns a list with two components:  the data used and the
#'    Cox proportional hazard model fits.
#'
#' @importFrom survival Surv
#'
#' @export

csh_fit <- function(dfc, csh) {

    # Check for weights and add them if not specified.

    if (!("Weight" %in% names(dfc))) {dfc$Weight <- 1}

    # Create a skinny data set.

    Data <- NULL

    for (i in 1:length(csh)) {

        Data <- rbind(
            Data,
            Data_i <- data.frame(
                Cause = i,
                Time = dfc[, csh[[i]]$Time],
                Status = dfc[, csh[[i]]$Status],
                Weight = dfc$Weight,
                dfc[, unique(unlist(lapply(csh, "[[", "vars")))]
            )
        )
    }

    # Order the data so that censored data stay in the risk set when
    # events are first considered.  The main event must be first.

    Data <- Data[order(Data$Time, Data$Cause, -Data$Status), ]

    # Fit the proportional hazards models for each hazard separately.

    Cox <- list()

    for (i in 1:length(csh)) {

        robust <- !(all(Data$Weight==1))

        f <- formula( paste("Surv(Time, Status==1) ~", paste(csh[[i]]$vars, collapse=" + ")) )

        Cox[[i]] <- survival::coxph(f, weights=Data$Weight[Data$Cause==i], ties="efron", robust=robust, data=Data[Data$Cause==i, ])

    }

    # Calculate linear predictors and their exponentiated values.

    Data$z_beta <- NA

    for (i in 1:length(csh)) {

        index <- which(Data$Cause==i)

        # Calculate linear predictors.

        Data$z_beta[index] <- as.matrix(Data[index, csh[[i]]$vars, drop=FALSE]) %*% coef(Cox[[i]])
    }

    Data$exp_z_beta <- exp(Data$z_beta)

    # Adjust them and weights for ties.

    for (i in 1:length(csh)) {

        # Perform the adjustment for ties here.  Replace weights with mean weights.
        # Replace exponentiated linear predictors with weighted means.
        #
        # TODO  Replace with a non-destructive step rather than overwriting.

        for (uTime in unique(Data[Data$Cause==i & Data$Status==1, "Time"])) {

            index <- which(Data$Cause==i & Data$Time==uTime & Data$Status==1)

            mean_Weight <- mean(Data$Weight[index])
            mean_exp_z_beta <- sum(Data$Weight[index] * Data$exp_z_beta[index]) /  sum(Data$Weight[index])

            Data$Weight[index] <- mean_Weight
            Data$exp_z_beta[index] <- mean_exp_z_beta

        }
    }

    rslt <- list(Data=Data, Cox=Cox)

    return(rslt)
}

#' Estimate the CIF at a single risk time.
#'
#' \code{csh_CIF} estimates the CIF.
#'
#' @param fit A fitted object.
#' @param Covariates A data set of covariates and risk times at which to estimate the hazard.
#' @param csh List giving the model of each specific cause.
#' @param risk_times A vector of times at which to estimate the CIF.
#' @param alpha The level of statistical significance.
#' @param method Method to use for confidence intervals.  Must be one of "original", "log", or
#' "loglog".
#'
#' @details The covariates must have an entry for each covariate used in the model
#'     as well as risk time.
#'
#' @return The CIF.
#'
#' @examples
#' data(bmtcrr)
#'
#' df <- data.frame(
#'     RTime = bmtcrr$ftime,
#'     RStatus = ifelse(bmtcrr$Status == 1, 1, 0),
#'     CTime = bmtcrr$ftime,
#'     CStatus = ifelse(bmtcrr$Status == 2, 1, 0),
#'     Age = bmtcrr$Age,
#'     Sex = ifelse(bmtcrr$Sex == "F", 1, 0),
#'     CR1 = ifelse(bmtcrr$Phase == "CR1", 1, 0),
#'     CR2 = ifelse(bmtcrr$Phase == "CR2", 1, 0),
#'     CR3 = ifelse(bmtcrr$Phase == "CR3", 1, 0),
#'     Source = ifelse(bmtcrr$Source == "PB", 1, 0)
#' )
#'
#' # Fit the model.
#' CSH <- list(
#'     list(
#'         Cause = "Relapse",
#'         Time = "RTime",
#'         Status = "RStatus",
#'         vars = c("Age", "Sex", "CR1", "CR2", "CR3")
#'     ),
#'     list(
#'         Cause = "Competing",
#'         Time = "CTime",
#'         Status = "CStatus",
#'         vars=c("Age", "Sex")
#'     )
#' )
#'
#' dfc <- csh_censor(df = df, csh = CSH, fuw = 1)
#' fit <- csh_fit(dfc, csh = CSH)
#' Covariates <- data.frame(Age = 30, Sex = 0, CR1 = 0, CR2 = 0, CR3 = 0, Source = 0)
#' csh_CIF(fit, Covariates, csh = CSH, risk_times = 50, alpha=0.05, method = "loglog")
#' csh_CIF(fit, Covariates, csh = CSH, risk_times = 50, alpha=0.05, method = "log")
#' csh_CIF(fit, Covariates, csh = CSH, risk_times = 50, alpha=0.05, method = "original")
#'
#' @importFrom stats coef formula qnorm vcov
#'
#' @export

csh_CIF <- function(fit, Covariates, csh, risk_times = NULL, alpha, method = "loglog") {

    Data <- fit$Data
    Cox <- fit$Cox

    # Perform simple error-checking.

    if (method != "original" & method != "log" & method != "loglog")
        stop("method must be one of original, log, or loglog")

    if (is.null(risk_times)) risk_times <- unique(Data$Time)

    # Iterate through the risk times.

    Results <- NULL

    for (risk_time in risk_times) {

        # Set up storage.

        n <- nrow(Data)

        z <- list()
        d_Lambda_0 <- list()
        d_Lambda <- list()
        E <- list()
        Psi <- list()
        cum_haz <- list()
        dCIFddN <- list()
        gradCIF <- list()

        # Extract the covariates into a list for future use.

        for (i in 1:length(csh)) {
            z[[i]] <- Covariates[, csh[[i]]$vars, drop=FALSE]
        }

        # Calculate Equations 17 and 18.

        for (i in 1:length(csh)) {

            # Calculate Equation 18.

            numerator <- ifelse(Data$Cause==i, 1, 0) * Data$Status * Data$Weight
            denominator <- rev(cumsum(rev(ifelse(Data$Cause==i, 1, 0) * Data$Weight * Data$exp_z_beta))) ### The sum from each row to the end.

            d_Lambda_0[[i]] <- numerator / denominator

            d_Lambda[[i]]   <- exp(sum(coef(Cox[[i]]) * z[[i]])) * d_Lambda_0[[i]]

            # Calculate Equation 17.

            n <- nrow(Data)

            numerator <- cumsum( (ifelse(Data$Cause==i, 1, 0) * Data$Weight * Data$exp_z_beta *
                                      Data[, csh[[i]]$vars, drop=FALSE])[n:1, , drop=FALSE] )[n:1, , drop=FALSE]

            E[[i]] <- numerator / denominator

            # Calculate Equation 16.

            Psi[[i]] <- cumsum( (as.list(z[[i]]) - E[[i]]) * d_Lambda[[i]] )

            # Calculate cumulative hazards.

            cum_haz[[i]] <- cumsum(d_Lambda[[i]])

        }

        # Calculate survival.

        surv <- exp(-rowSums(as.data.frame(cum_haz)))

        # Calculate Equation 14.  There is a little asymmetry now between the
        # main risk of interest and the competing risks.

        dCIFddN[[1]] <- rep(0, n)

        for (j in which(Data$Cause==1 & Data$Status==1 & Data$Time < risk_time)) {

            dCIFddN[[1]][j] <- surv[j] * d_Lambda[[1]][j]

            for (k in j:n) {

                if (Data$Time[k] <= risk_time) {

                    dCIFddN[[1]][j] <- dCIFddN[[1]][j] -  surv[k] * d_Lambda[[1]][j] * d_Lambda[[1]][k]

                }
            }
        }

        for (i in 2:length(csh)) {

            dCIFddN[[i]] <- rep(0, n)

            for (j in which(Data$Cause==i & Data$Status==1 & Data$Time < risk_time)) {

                dCIFddN[[i]][j] <- 0

                for (k in j:n) {

                    if (Data$Time[k] <= risk_time) {

                        dCIFddN[[i]][j] <- dCIFddN[[i]][j] -  surv[k] * d_Lambda[[i]][j] * d_Lambda[[1]][k]

                    }
                }
            }
        }

        # Calculate Equations 11 and 12.

        gradCIF[[1]] <- rep(0, length(csh[[1]]$vars))

        for (j in 1:n) {

            if (Data$Time[j] <= risk_time) {

                gradCIF[[1]] <- gradCIF[[1]] + surv[j] * ( z[[1]] - E[[1]][j, ] - Psi[[1]][j, ] ) * d_Lambda[[1]][j]

            }
        }

        for (i in 2:length(csh)) {

            gradCIF[[i]] <- rep(0, length(csh[[i]]$vars))

            for (j in 1:n) {

                if (Data$Time[j] <= risk_time) {

                    gradCIF[[i]] <- gradCIF[[i]] - surv[j] * Psi[[i]][j, ] * d_Lambda[[1]][j]

                }
            }
        }

        # Calculate the variance.

        var_CIF <- 0

        for (i in 1:length(csh)){

            M <- as.matrix(gradCIF[[i]])

            var_CIF <- var_CIF + M %*% vcov(Cox[[i]]) %*% t(M)

            var_CIF <- var_CIF + sum( (dCIFddN[[i]] * ifelse(Data$Cause==i & Data$Status==1, 1, 0))^2)

        }

        SD_CIF <- as.numeric(sqrt(var_CIF))

        # Calculate risk.

        index <- which(Data$Time <= risk_time)

        Risk <- sum(surv[index] * d_Lambda[[1]][index])

        # Calculate the confidence interval on the desired scale.

        if (method == "loglog") {

            rho <- log(-log(1 - Risk))
            SD_rho <- SD_CIF / abs( (1 - Risk) * log(1 - Risk))
            Risk_LCL <- 1 - exp(-exp(rho - qnorm(1 - alpha / 2) * SD_rho))
            Risk_UCL <- 1 - exp(-exp(rho + qnorm(1 - alpha / 2) * SD_rho))

        } else if (method == "log") {

            rho <- -log(1 - Risk)
            SD_rho <- SD_CIF / (1 - Risk)
            Risk_LCL <- 1 - exp(-rho + qnorm(1 - alpha / 2) * SD_rho)
            Risk_UCL <- 1 - exp(-rho - qnorm(1 - alpha / 2) * SD_rho)

        } else {

            Risk_LCL <- Risk - qnorm(1-alpha/2) * SD_CIF
            Risk_UCL <- Risk + qnorm(1-alpha/2) * SD_CIF

        }

        Result <- data.frame(
            Covariates,
            risk_time = risk_time,
            Risk = Risk,
            SE_Risk = SD_CIF,
            Risk_LCL = Risk_LCL,
            Risk_UCL = Risk_UCL
        )

        Results <- rbind(Results, Result)
    }


    return(Results)
}
