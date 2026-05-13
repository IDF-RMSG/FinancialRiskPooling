
#' Flexible samples
#'
#' This function is flexible for function name and samples based on input mle values. Used within the get_freq_sev_ci function.
#' @param mle_list_input
#' @param dist_name
#' @param n
#'
#' @return
#' @export
#'
#' @examples
#'
#' return_sample <- flexible_samples(n=number_of_simulations,
#'                                   dist_name = paste0(dist),
#'                                   mle_list_input= lapply(ci_mle_list[[peril]][[paste0(dist)]],"[",j))
#'                 flexible_samples(n=number_of_simulations,
#'                                  dist_name = "bern",
#'                                  mle_list_input=freq_boot[j])
flexible_samples <- function(mle_list_input,dist_name,n) {
  if (any(is.na(mle_list_input))) return(NA)
  fn <- match.fun(paste0("r",dist_name))
  do.call(fn,args = c(n = n,as.list(mle_list_input)))
}

# This function takes input data and returns the MLE samples from 1000 samples.
freqboot <- function(input_data,number_of_resamples,number_of_years = NULL) {
  freq_dat <- as.numeric(input_data > 0)
  if (is.null(number_of_years)) {
    num_years <- length(freq_data)
  } else {
    num_years <- number_of_years
  }
  if (length(freq_dat) < num_years) {
    freq_dat[(length(freq_dat)+1):num_years] <- 0
  }

  rdata <- sample(freq_dat, size=number_of_resamples*number_of_years, replace=TRUE)
  dim(rdata) <- c(number_of_resamples, number_of_years)
  apply(rdata,MARGIN=1,FUN=sum)/number_of_years
}



#' Get AIC
#'
#' This function computes the AIC score from the log likelihood for distributions that don't have built in R functions.
#' @param llhood Log-likelihood
#' @param number_of_parameters The number of parameters in the distribution
#'
#' @return A number.
#' @export
#'
get_aic <- function(llhood,number_of_parameters){
  aic <- (-2*llhood) + 2*number_of_parameters
}

#' Get AIC and MLE parameters
#'
#' This function takes the data and fits a distribution for each peril and returns the Maximum likelihood estimates of the parameters, along with the AIC score.
#' This also returns (in the case of a confidence interval fit) 1000 MLE fits. Used within the fit_distribution function.
#' @param dat
#' @param updateProgress
#'
#' @return Data frame with all the distribution fits for each peril.
#' @export
#'
#' @examples
#' out <- get_aic_mle(the_right_data, updateProgress)
get_aic_mle <- function(dat, updateProgress = NULL, data_type = "Historical", years){

  # get the names of the perils present in this dataset (ones with enough observations)
  present_perils <-
    dat |>
      dplyr::select("peril") |>
      unique() |>
      unlist()

  dat_list <- list()
  mle_param_list_perils <- vector("list")
  overall_bootstrap_sample <- vector("list")
  bootstrap <- if(data_type == "Model"){ FALSE }else{ TRUE }

  for(i in 1:length(present_perils)){

    mle_param_list <- vector("list") # this is the total number of distributions considered
    peril_name <- as.character(present_perils[i])
    overall_bootstrap_sample[[peril_name]] <- list()

    if (is.function(updateProgress)) {
      set.seed(501)
      updateProgress(detail = paste0("Calculating distribution fit for ",peril_name,", please wait."),value = i/length(present_perils), scale = length(present_perils))
    }

    sub_dat <- dat[dat$peril == peril_name,]

    if(length(sub_dat$value) < 3 | length(unique(sub_dat$value)) <= 1){
      too_short_indic = 1
    } else {
      too_short_indic = 0
    }

    # Frequency
    if(too_short_indic == 1) {

      sample_mean <- NA
      size <- NA

    } else {

      freq_data <-
        sub_dat |>
        dplyr::group_by(.data$year) |>
        dplyr::summarise(count = dplyr::n(), .groups = "drop") |>
        tidyr::complete(year = years[1]:years[2], fill = list(count = 0))

      sample_mean <- mean(freq_data$count)
      sample_var <- stats::var(freq_data$count)
      #over-dispersion factor

      odf <- sample_var / sample_mean

      #Use Poisson if odf<= 1, nbin otherwise

        if (odf >= 1) {
          size <- (sample_mean ^ 2) /  (sample_var - sample_mean)
          freq_dist <- fitdistrplus::fitdist(freq_data$count, distr =  "nbinom")
          mle1_select <- freq_dist$estimate[1]
          mle2_select <- freq_dist$estimate[2]
        } else {
          freq_dist <- fitdistrplus::fitdist(freq_data$count, distr =  "pois")
          mle1_select <- freq_dist$estimate[1]
          mle2_select <- NA
        }
    }

    if(bootstrap & too_short_indic == 0) {

      freq_boot <-
        fitdistrplus::bootdist(freq_dist, niter = 1000, bootmethod = "nonparam")
      # get bootstrap values
      if(freq_dist$distname == "nbinom") {
        freq_boot$estim <-
          freq_boot$estim |>
            dplyr::mutate(
              mu = tidyr::replace_na(size, 0),
              size = tidyr::replace_na(size, 1)
            )

         mle_1_lower <- freq_boot$CI[1, "2.5%"]
         mle_1_upper <- freq_boot$CI[1, "97.5%"]
         mle_2_lower <- freq_boot$CI[2, "2.5%"]
         mle_2_upper <- freq_boot$CI[2, "97.5%"]

         freq_resample <-
           purrr::pmap(
             list(freq_boot$estim$mu, freq_boot$estim$size),
             \(x, y) stats::rnbinom(mu = x, size = y, n = 1000)
           )

       } else {

         freq_boot$estim <-
           dplyr::mutate(
             freq_boot$estim ,
             lambda = tidyr::replace_na(lambda, 0)
           )

         freq_resample <-
           purrr::pmap(
             list(freq_boot$estim$lambda),
             \(x) stats::rpois(lambda = x, n = 1000)
           )

         mle_1_lower <- freq_boot$CI["2.5%"][1]
         mle_1_upper <- freq_boot$CI["97.5%"][1]
         mle_2_lower <- NA
         mle_2_upper <- NA
       }

      mle_param_list[["freq"]][[peril_name]] <- freq_boot$estim

    } else {
      mle1_select <- NA
      mle2_select <- NA
      mle_1_lower <- NA
      mle_1_upper <- NA
      mle_2_lower <- NA
      mle_2_upper <- NA
    }

    freq_dat <- data_frame(name = "Freq",
                           aic = NA,
                           mle_1 = mle1_select,
                           mle_2 = mle2_select,
                           mle_1_lower = mle_1_lower,
                           mle_2_lower = mle_2_lower,
                           mle_1_upper = mle_1_upper,
                           mle_2_upper = mle_2_upper)

    # fit log normal
    log_normal <-
      try(fitdistrplus::fitdist(sub_dat$value, distr =  "lnorm"), silent = TRUE)

    if(class(log_normal) == 'try-error' || too_short_indic == 1){
      log_normal <- NULL
      log_normal_aic <- NA
      mle_1 <- NA
      mle_2 <- NA
      mle_1_lower <- NA
      mle_2_lower <-NA
      mle_1_upper <- NA
      mle_2_upper <- NA
    } else {

      log_normal_aic <- round(log_normal$aic,2)

      if(bootstrap & too_short_indic == 0) {
        log_normal_boot <-
          fitdistrplus::bootdist(log_normal, niter = 1000, bootmethod = "nonparam")
        # get bootstrap values
        mle_param_list[[peril_name]][["lnorm"]] <- log_normal_boot$estim

        mle_1_lower <- log_normal_boot$CI[1, "2.5%"]
        mle_2_lower <- log_normal_boot$CI[2, "2.5%"]
        mle_1_upper <- log_normal_boot$CI[1, "97.5%"]
        mle_2_upper <- log_normal_boot$CI[2, "97.5%"]

        overall_bootstrap_sample[[peril_name]][["lnorm"]] <-
          list(
            freq_resample,
            log_normal_boot$estim[[1]],
            log_normal_boot$estim[[2]]
          ) |>
          purrr::pmap(
            \(x, y, z)
              purrr::map(x, \(x) sum(stats::rlnorm(x, y, z), na.rm = TRUE)
            )
          ) |>
          purrr::map(unlist)

      }

      mle_1 <- log_normal$estimate[1]
      mle_2 <- log_normal$estimate[2]
      mle_1_lower <- NA
      mle_2_lower <- NA
      mle_1_upper <- NA
      mle_2_upper <- NA

    }

    # create dat frame to store aic and MLEs
    log_normal_dat <- data_frame(name = 'log_normal',
                                 aic = log_normal_aic,
                                 mle_1 = mle_1,
                                 mle_2 = mle_2,
                                 mle_1_lower = mle_1_lower,
                                 mle_2_lower = mle_2_lower,
                                 mle_1_upper = mle_1_upper,
                                 mle_2_upper = mle_2_upper)

    # beta_fit <- try({mom_fit_beta <- beta_mom(sub_dat$value)
    # fitdistrplus::fitdist(data = sub_dat$value,distr = "Beta_ab",
    #                       start = list(shape1=mom_fit_beta$shape1,shape2 = mom_fit_beta$shape2,b=mom_fit_beta$b),
    #                       method="mle")},silent = TRUE)
    #
    # if(class(beta_fit) == 'try-error' || too_short_indic == 1){
    #   beta <- NULL
    #   beta_aic <- NA
    #   mle_1 <- NA
    #   mle_2 <- NA
    #   mle_3 <- NA
    #   mle_1_lower <- NA
    #   mle_2_lower <- NA
    #   mle_3_lower <- NA
    #   mle_1_upper <- NA
    #   mle_2_upper <- NA
    #   mle_3_upper <- NA
    #
    # } else {
    #   beta_aic <- beta_fit$aic
    #   beta_boot <- fitdistrplus::bootdist(beta_fit, niter = 1000,bootmethod = "nonparam")
    #
    #   # get bootstrap values
    #   mle_param_list[["Beta_ab"]] <- beta_boot$estim
    #   mle_1 <- beta_boot[[6]]$estimate[1]
    #   mle_2 <- beta_boot[[6]]$estimate[2]
    #   mle_3 <- beta_boot[[6]]$estimate[3]
    #   mle_1_lower <- beta_boot[[5]][[4 ]]
    #   mle_2_lower <- beta_boot[[5]][[5]]
    #   mle_3_lower <- beta_boot[[5]][[6]]
    #   mle_1_upper <- beta_boot[[5]][[7]]
    #   mle_2_upper <- beta_boot[[5]][[8]]
    #   mle_3_upper <- beta_boot[[5]][[9]]
    #
    # }
    # # create dat frame to store aic and MLEs
    # beta_dat <- data_frame(name = 'beta',
    #                        aic = beta_aic,
    #                        mle_1 = mle_1,
    #                        mle_2 = mle_2,
    #                        mle_3 = mle_3,
    #                        mle_1_lower = mle_1_lower,
    #                        mle_2_lower = mle_2_lower,
    #                        mle_3_lower = mle_3_lower,
    #                        mle_1_upper = mle_1_upper,
    #                        mle_2_upper = mle_2_upper,
    #                        mle_3_upper = mle_3_upper)

    # fit gammma with fitdistrplus so it is consistent with the fitting of the other distributions
    gamma <- try(fitdistrplus::fitdist((sub_dat$value),distr =  "gamma", lower=c(0,0)
                                       ,method = "mle"),silent = FALSE)
    if(class(gamma) == 'try-error' || too_short_indic == 1){
      gamma <- NULL
      gamma_aic <- NA
      mle_1 <- NA
      mle_2 <- NA
      mle_1_lower <- NA
      mle_2_lower <-NA
      mle_1_upper <- NA
      mle_2_upper <- NA
    } else {
      gamma_aic <- round(gamma$aic,2)

      if(bootstrap & too_short_indic == 0) {
        gamma_boot <- fitdistrplus::bootdist(gamma, niter = 1000, bootmethod = "nonparam")

        mle_1_lower <- gamma_boot$CI[1, "2.5%"]
        mle_2_lower <- gamma_boot$CI[2, "2.5%"]
        mle_1_upper <- gamma_boot$CI[1, "97.5%"]
        mle_2_upper <- gamma_boot$CI[2, "97.5%"]
        # get bootstrap values
        mle_param_list[["gamma"]][[peril_name]] <- gamma_boot$estim

        overall_bootstrap_sample[[peril_name]][["gamma"]] <-
          list(
            freq_resample,
            gamma_boot$estim[[1]],
            gamma_boot$estim[[2]]
          ) |>
            purrr::pmap(
              \(x, y, z)
                purrr::map(x, \(x) sum(stats::rgamma(x, y, z), na.rm = TRUE)
              )
            ) |>
            purrr::map(unlist)

      } else {
        mle_1_lower <- NA
        mle_2_lower <-NA
        mle_1_upper <- NA
        mle_2_upper <- NA
      }

      mle_1 <- gamma$estimate[1]
      mle_2 <- gamma$estimate[2]

    }
    # create dat frame to store aic and MLEs

    gamma_dat <- data_frame(name = 'gamma',
                            aic = gamma_aic,
                            mle_1 = mle_1,
                            mle_2 = mle_2,
                            mle_1_lower = mle_1_lower,
                            mle_2_lower = mle_2_lower,
                            mle_1_upper = mle_1_upper,
                            mle_2_upper = mle_2_upper)

    # fit the gumbel distribution
    # gumbel_fit <- try({
    #   # Need a good first estimate for gumbel
    #   sigma_start <- 0.7797 * var(sub_dat$value)^0.5
    #   mu_start <- sum(exp(-sub_dat$value/sigma_start))
    #   mu_start <- log(1/(nrow(sub_dat)) *mu_start) * -sigma_start
    #
    #   fitdistrplus::fitdist(sub_dat$value, "gumbel", start=list(mu=mu_start, s=sigma_start), method="mle")}, silent = TRUE)
    #
    # if(class(gumbel_fit) == 'try-error' || too_short_indic == 1){
    #   gumbel_fit <- NULL
    #   gumbel_aic <- NA
    #   mle_1 <- NA
    #   mle_2 <- NA
    #   mle_1_lower <- NA
    #   mle_2_lower <-NA
    #   mle_1_upper <- NA
    #   mle_2_upper <- NA
    #
    # } else {
    #   gumbel_aic <- round(gumbel_fit$aic,2)
    #   gumbel_boot <- fitdistrplus::bootdist(gumbel_fit, niter = 1000,bootmethod = "nonparam")
    #
    #   # get bootstrap values
    #   mle_param_list[["gumbel"]] <- gumbel_boot$estim
    #   mle_1 <- gumbel_boot[[6]]$estimate[1]
    #   mle_2 <- gumbel_boot[[6]]$estimate[2]
    #   mle_1_lower <- gumbel_boot[[5]][[3]]
    #   mle_2_lower <- gumbel_boot[[5]][[4]]
    #   mle_1_upper <- gumbel_boot[[5]][[5]]
    #   mle_2_upper <- gumbel_boot[[5]][[6]]
    # }
    #
    # # create dat frame to store aic and MLEs
    # gumbel_dat <- data_frame(name = 'gumbel',
    #                          aic = gumbel_aic,
    #                          mle_1 = mle_1,
    #                          mle_2 = mle_2,
    #                          mle_1_lower =mle_1_lower,
    #                          mle_2_lower = mle_2_lower,
    #                          mle_1_upper = mle_1_upper,
    #                          mle_2_upper = mle_2_upper)


    # fit weibull
    weibull <-
      try(fitdistrplus::fitdist(sub_dat$value, "weibull", method = "mle"), silent = TRUE)
    if(class(weibull) == 'try-error' || too_short_indic == 1){
      weibull <- NULL
      weibull_aic <- NA
      mle_1 <- NA
      mle_2 <- NA
      mle_1_lower <- NA
      mle_2_lower <-NA
      mle_1_upper <- NA
      mle_2_upper <- NA
    } else {
      weibull_aic <- round(weibull$aic, 2)

      if(bootstrap) {
        weibull_boot <-
          fitdistrplus::bootdist(weibull, niter = 1000, bootmethod = "nonparam")

        mle_1_lower <- weibull_boot$CI[1, "2.5%"]
        mle_2_lower <- weibull_boot$CI[2, "2.5%"]
        mle_1_upper <- weibull_boot$CI[1, "97.5%"]
        mle_2_upper <- weibull_boot$CI[2, "97.5%"]
        # get bootstrap values
        overall_bootstrap_sample[[peril_name]][["weibull"]] <-
          list(
            freq_resample,
            weibull_boot$estim[[1]],
            weibull_boot$estim[[2]]
          ) |>
          purrr::pmap(\(x, y, z)
                      purrr::map(
                        x, \(x) sum(stats::rweibull(x, y, z), na.rm = TRUE)
                      )
                    ) |>
          purrr::map(unlist)

        mle_param_list[["weibull"]][[peril_name]] <- weibull_boot$estim

      } else {
        mle_1_lower <- NA
        mle_2_lower <-NA
        mle_1_upper <- NA
        mle_2_upper <- NA

      }

      mle_1 <- weibull$estimate[1]
      mle_2 <- weibull$estimate[2]
    }
    # create dat frame to store aic and MLEs
    weibull_dat <- data_frame(name = 'weibull',
                              aic = weibull_aic,
                              mle_1 = mle_1,
                              mle_2 = mle_2,
                              mle_1_lower = mle_1_lower,
                              mle_2_lower = mle_2_lower,
                              mle_1_upper = mle_1_upper,
                              mle_2_upper = mle_2_upper)

    # fit pareto
    # pareto <- try({
    #   start_b = min(sub_dat$value)
    #   start_a <- nrow(sub_dat) / (sum(log(sub_dat$value)) - nrow(sub_dat) * log(start_b))
    #   fitdistrplus::fitdist(sub_dat$value, "pareto", start=list(shape=start_a, scale=start_b),lower=c(0,0), method="mle")}, silent = TRUE)
    #
    # # If first parameter is less than 1 we have an infinite mean - so we reject this solution.
    # if (!class(pareto) == "try-error") {
    #   if (pareto$estimate[[1]] <= 1) {
    #     class(pareto) <- "try-error"
    #   }
    # }
    #
    # if(class(pareto) == 'try-error' || too_short_indic == 1){
    #   pareto <- NULL
    #   pareto_aic <- NA
    #   mle_1 <- NA
    #   mle_2 <- NA
    #   mle_1_lower <- NA
    #   mle_2_lower <-NA
    #   mle_1_upper <- NA
    #   mle_2_upper <- NA
    # } else {
    #   pareto_aic <- round(pareto$aic, 2)
    #
    #   if(bootstrap & too_short_indic == 0) {
    #     pareto_boot <-
    #       fitdistrplus::bootdist(pareto, niter = 1000, bootmethod = "nonparam")
    #
    #     mle_1_lower <- pareto_boot$CI[1, "2.5%"]
    #     mle_2_lower <- pareto_boot$CI[2, "2.5%"]
    #     mle_1_upper <- pareto_boot$CI[1, "97.5%"]
    #     mle_2_upper <- pareto_boot$CI[2, "97.5%"]
    #     # get bootstrap values
    #
    #     overall_bootstrap_sample[[peril_name]][["pareto"]] <-
    #       list(
    #         freq_resample,
    #         pareto_boot$estim[[1]],
    #         pareto_boot$estim[[2]]
    #       ) |>
    #       purrr::pmap(\(x, y, z)
    #                   purrr::map(
    #                     x, \(x) sum(actuar::rpareto(x, y, z), na.rm = TRUE)
    #                   )
    #                 ) |>
    #       purrr::map(unlist)
    #
    #     mle_param_list[["pareto"]] <- pareto_boot$estim
    #
    #   } else {
    #     mle_1_lower <- NA
    #     mle_2_lower <-NA
    #     mle_1_upper <- NA
    #     mle_2_upper <- NA
    #   }
    #
    #   mle_1 <- pareto$estimate[1]
    #   mle_2 <- pareto$estimate[2]
    # }
    #
    # # create dat frame to store aic and MLEs
    # pareto_dat <- data_frame(name = 'pareto',
    #                          aic = pareto_aic,
    #                          mle_1 = mle_1,
    #                          mle_2 = mle_2,
    #                          mle_1_lower = mle_1_lower,
    #                          mle_2_lower = mle_2_lower,
    #                          mle_1_upper = mle_1_upper,
    #                          mle_2_upper = mle_2_upper)

    # fit frechet - we will use the fact that if X~Frechet(alpha,s) then X^-1 ~ Weibull(lambda=s^-1,k=alpha)
    # in order to provide a reasonable guess (where available)
    # weibull_inverted <- try({
    #   sub_dat_inverted <- sub_dat
    #   sub_dat_inverted$value <- 1/sub_dat_inverted$value
    #   fitdistrplus::fitdist(sub_dat_inverted$value, "weibull",method="mle")},silent = TRUE)
    #
    # if (class(weibull_inverted) == "try-error") {
    #   start_frechet = list(alpha=1,s=1)
    # } else {
    #   start_frechet = list(alpha=weibull_inverted$estimate[[1]],s=1/weibull_inverted$estimate[[2]])
    # }
    #
    # frechet <- try(fitdistrplus::fitdist(sub_dat$value, "frechet", start = start_frechet, lower = c(0,0) , method="mle"),
    #                silent = TRUE)
    #
    # # If alpha is less than than 1 then we have an infinite mean and hence we want to avoid this
    # if (!class(frechet) == "try-error") {
    #   if (frechet$estimate[[1]] <= 1) {
    #     class(frechet) <- "try-error"
    #   }
    # }
    #
    # if(class(frechet) == 'try-error' || too_short_indic == 1){
    #   frechet <- NULL
    #   frechet_aic <- NA
    #   mle_1 <- NA
    #   mle_2 <- NA
    #   mle_1_lower <- NA
    #   mle_2_lower <-NA
    #   mle_1_upper <- NA
    #   mle_2_upper <- NA
    # } else {
    #   frechet_aic <- round(frechet$aic,2)
    #   frechet_boot <- fitdistrplus::bootdist(frechet, niter = 1000,bootmethod = "nonparam")
    #
    #   # get bootstrap values
    #   mle_param_list[["frechet"]] <- frechet_boot$estim
    #   mle_1 <- frechet_boot[[6]]$estimate[1]
    #   mle_2 <- frechet_boot[[6]]$estimate[2]
    #   mle_1_lower <- frechet_boot[[5]][[3]]
    #   mle_2_lower <- frechet_boot[[5]][[4]]
    #   mle_1_upper <- frechet_boot[[5]][[5]]
    #   mle_2_upper <- frechet_boot[[5]][[6]]
    # }
    #
    # # create dat frame to store aic and MLEs
    # frechet_dat <- data_frame(name = 'frechet',
    #                           aic = frechet_aic,
    #                           mle_1 = mle_1,
    #                           mle_2 = mle_2,
    #                           mle_1_lower =mle_1_lower,
    #                           mle_2_lower = mle_2_lower,
    #                           mle_1_upper = mle_1_upper,
    #                           mle_2_upper = mle_2_upper)

    # create a dat frame out of dat results
    aic_mle_dat <- bind_rows(
      freq_dat,
      log_normal_dat,
      gamma_dat,
      # beta_dat,
      # frechet_dat,
      # gumbel_dat,
      weibull_dat
      # ,pareto_dat
    )

    aic_mle_dat <-
      aic_mle_dat[, c("name","aic","mle_1","mle_2","mle_1_lower","mle_2_lower","mle_1_upper","mle_2_upper")]

    # change names of variable
    names(aic_mle_dat) <- c('distribution', 'aic', 'mle1', 'mle2', 'mle1_lower', 'mle2_lower', 'mle1_upper', 'mle2_upper')

    # capitalize and remove underscore of distribution
    aic_mle_dat$distribution <- Hmisc::capitalize(aic_mle_dat$distribution)
    aic_mle_dat$distribution <- gsub('_', ' ', aic_mle_dat$distribution)
    aic_mle_dat$aic <- round(aic_mle_dat$aic, 2)
    aic_mle_dat$peril <- peril_name

    dat_list[[i]] <- aic_mle_dat

  }

  aic_dat <-
    dplyr::bind_rows(dat_list) |>
      dplyr::mutate(data_type = data_type)

  list(aic_dat, overall_bootstrap_sample)

}

#' Fit distribution
#'
#' This function takes the data, after all user inputs and checks to see if the data is ready to be fit. If the data is ready to be fit, then it uses the get_aic_mle function on the data.
#' @param the_right_data
#' @param updateProgress
#'
#'
#' @return Data frame with all the distribution fits for each peril.
#' @export
#'
#' @examples
#' rd <- get_right_data()
#' rd <- rd[[1]]
#' temp <- fit_distribution(rd, updateProgress)
fit_distribution <- function(the_right_data = NULL, updateProgress, data_type, years){

  require(tidyverse)
  # the right data should be either scaled, detrended or core data, depending on inputs

  print("RUNNING!!!")
  out <- NULL
  ok <- FALSE
  if(!is.null(the_right_data)){
    if(nrow(the_right_data) > 0){
      ok <- TRUE
    }
  }
  if(ok){
    the_right_data <-
      the_right_data |>
        dplyr::filter(.data$value > 0 & !is.na(.data$value ))

    out <- get_aic_mle(the_right_data, updateProgress, data_type, years)
  }
  return(out)
}



#' Filter distribution
#'
#' This function finds the distribution with the smallest aic score from fitted_distribution.
#' @param fitted_distribution 5 column data frame produced by the fit_distribution function.
#'
#' @return Filtered data frame with the distribution with the smallest AIC score for each peril.
#' @export
#'
#' @examples
#' fd <- fitted_distribution()[[1]]
#' filter_distribution(fd)
filter_distribution <- function(fitted_distribution = NULL){
  # fitted_distribution should be a 5 column df as produced by
  # fit_distribution

  out <- NULL
  ok <- FALSE
  if(!is.null(fitted_distribution)){
    if(nrow(fitted_distribution) > 0){
      ok <- TRUE
    }
  }
  if(ok){

    sev_dists <-
      fitted_distribution |>
        dplyr::filter(.data$distribution != "Freq") |>
        dplyr::group_by(.data$peril)  |>
        dplyr::mutate(min_aic = min(.data$aic, na.rm = TRUE))  |>
        dplyr::filter(!is.na(.data$aic)) |>
        dplyr::ungroup() |>
        dplyr::distinct(peril, .keep_all = TRUE) |>
        dplyr::select(-"min_aic")

    freq_dists <-
      fitted_distribution |>
        dplyr::filter(.data$distribution == "Freq", !is.na(.data$mle1))

    out <- dplyr::bind_rows(sev_dists, freq_dists)
  } else {
    out <- NULL
  }
  out
}
