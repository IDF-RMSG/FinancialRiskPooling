
#' plot_simulations
#'
#' This function takes the simulated data and the original loss data and plots two overlapping densities, comparing the distribution of the loss data with the simulated data for each peril.
#' @param rs Simulated data from the run_simulations function.
#' @param right_data Data frame, which is either the scaled, detrended or core data, depending on the inputs selected
#' @param peril Selected peril type
#' @param overlap
#'
#' @return
#' @export
#'
#' @examples
#'  plot_simulations(rs = rs,
#'  right_data = rd,
#'  peril = ips,
#'  overlap = ioc) + xlab(paste0('value ',currency_code())) +
#'    coord_cartesian(xlim = c(0, max(observed_data$value * 1.25))) +
#'    scale_fill_manual(values=fill_colours) +
#'    theme(legend.title=element_blank())
plot_simulations <- function(rs = NULL,
                             right_data = NULL,
                             chosen_peril = NULL,
                             overlap = NULL){

  # ran_simulations gets generated in run_simulations
  # it should be a 3 column df with columns key, value and freq
  # peril must be one of the peril types

  if(is.null(chosen_peril)){
    chosen_peril <- 'Flood'
  }
  if(is.null(overlap)){
    overlap <- c()
  }
  out <- ggplot() + theme_bw()
  rd <- right_data
  # Just keep the first part (since it's showing the values only)
  rd <- rd[[1]]
  rs <- rs$Event
  if(!is.null(rs) && nrow(rs) > 0 && !is.null(rd) && nrow(rd) > 0) {
    # Filter
    pd <-
      rs |>
      dplyr::filter(.data$key == chosen_peril) |>
      dplyr::mutate(data_type = 'Simulated data')  |>
      dplyr::select("value", "data_type")

    # Add the observed data
    observed_data <-
      rd  |>
        dplyr::filter(peril == chosen_peril) |>
        dplyr::filter(value > 0) |>
        dplyr::select(value) |>
        dplyr::mutate(data_type = 'Observed data')

    pd <-
      pd |>
        dplyr::bind_rows(observed_data) |>
        dplyr::filter(.data$data_type %in% overlap)
  }
  if(nrow(pd) == 0){
    out <- ggplot() +
      labs(title = paste0('No data to show for ', peril)) +
      theme_bw()
  }
  else {
    out <- ggplot(data = pd,
                  aes(x = value)) +
      geom_density(aes(fill = data_type),
                   alpha = 0.7) +
      scale_fill_brewer(name = 'Data type',
                        type = 'qual') +
      theme(legend.position = 'bottom') +
      theme_bw()
  }
  return(out)
}



#' quant_that
#'
#' Function that creates the Output data frame for Estimates of annual loss with Long-term average, 1 in 5 years, 1 in 10 years, 1 in 25 years, 1 in 50 years, 1 in 100 years, Highest historical annual loss and Most recent annual loss.
#' @param dat_sim Data frame of simulated values from the run_simulations function.
#' @param dat Dataframe combines the original loss data based on the perils selected.
#' @param combined_ci
#'
#' @return Data frame for Output 1.
#' @export
#'
#' @examples
#' sub_dat <- quant_that(dat_sim = dat_sim,
#' dat = dat,
#' combined_ci = perils_ci)
#' sub_dat$variable <- factor(sub_dat$variable, levels = c('1 in 5 Years', '1 in 10 Years', '1 in 25 Years', '1 in 50 Years', '1 in 100 Years',
#'                                                         'Long-term average', 'Highest historical annual loss', 'Most recent annual loss'))
quant_that <- function(dat_sim,
                       dat = NULL,
                       combined_ci = NULL){

  if(is.null(dat)){
    dat <- dat_sim
  }

  # get normal point estimate
  output <- quantile(dat_sim$value,c(0.8,0.9, 0.96,0.98,0.99))
  annual_avg = round(mean(dat_sim$value), 2)

  #write.csv(dat_sim, "data_sim.csv")

  # create sub_data frame sub_dat to store output with chart labels
  sub_dat <- data_frame(`Long-term average` = annual_avg,
                        `1 in 5 Years` = output[1],
                        `1 in 10 Years` = output[2],
                        `1 in 25 Years` = output[3],
                        `1 in 50 Years` = output[4],
                        `1 in 100 Years` = output[5],
                        `Highest historical annual loss` = max(dat$value),
                        `Most recent annual loss` = dat$value[nrow(dat)])

  # Convert to long format without reshape2 to avoid extra deployment dependency.
  sub_dat <- tidyr::pivot_longer(sub_dat,
                                 cols = dplyr::everything(),
                                 names_to = "variable",
                                 values_to = "value")

  if (!is.null(combined_ci) & !any(is.na(combined_ci))) {
    number_of_quantiles <- length(combined_ci$average)

    # get upper point estimate
    annual_avg_upper <- round(combined_ci$average[[number_of_quantiles]])
    output_upper <- combined_ci$percentiles[number_of_quantiles,c("80%","90%","96%","98%","99%")]

    # lower point estimate
    annual_avg_lower <- round(combined_ci$average[[1]])
    output_lower <- combined_ci$percentiles[1,c("80%","90%","96%","98%","99%")]

    sub_dat$value_lower <- c(annual_avg_lower, output_lower[1], output_lower[2], output_lower[3], output_lower[4],
                             output_lower[5], max(dat$value), dat$value[nrow(dat)])
    sub_dat$value_upper <- c(annual_avg_upper, output_upper[1], output_upper[2], output_upper[3], output_upper[4],
                             output_upper[5], max(dat$value), dat$value[nrow(dat)])
  } else {
    sub_dat$value_lower <- NA
    sub_dat$value_upper <- NA
  }

  return(sub_dat)
}



# for output 4, creates funding gap curve
#' get_gap_curve
#'
#' A function that creates the funding gap curve for Output 4
#' @param sim_vector Vector of simulated values for the selected peril.
#' @param budget Selected budget
#' @param is_ci If user has selected to show confidence intervals, default = FALSE.
#'
#' @return Data frame for funding gap curve.
#' @export
#'
#' @examples
#' # Create funding gap curve
#' curve <- get_gap_curve(dat_sim$value, budget = budget)
#'
#' if (!is.null(perils_ci) & !any(is.na(perils_ci))) {
#'   number_of_quantiles <- length(perils_ci$average)
#'
#'   curve_lower <- get_gap_curve(perils_ci$percentiles[1,paste0(seq(0.5,0.98,by=0.01)*100,"%")],
#'                                budget = budget,
#'                                is_ci = TRUE)
#'
#'   curve_upper <- get_gap_curve(perils_ci$percentiles[number_of_quantiles,paste0(seq(0.5,0.98,by=0.01)*100,"%")],
#'                                budget = budget,
#'                                is_ci = TRUE)
#'
#'   curve$value_lower <- curve_lower$`Funding gap`
#'   curve$value_upper <- curve_upper$`Funding gap`
#' } else {
#'   curve$value_lower <- NA
#'   curve$value_upper <- NA
#' }
get_gap_curve <- function(sim_vector, budget = budget, is_ci = FALSE){
  if (is_ci) {
    funding_gap_curve <- as.data.frame(sim_vector)
  } else {
    funding_gap_curve <- as.data.frame(quantile(sim_vector,seq(0.5, 0.99, by=0.01), na.rm = TRUE))
  }
  funding_gap_curve$x <- rownames(funding_gap_curve)
  rownames(funding_gap_curve) <- NULL
  names(funding_gap_curve)[1] <- 'y'
  # remove percent and turn numeric
  funding_gap_curve$x <- gsub('%', '', funding_gap_curve$x)
  funding_gap_curve$x <- as.numeric(funding_gap_curve$x)/100
  # divide y by 100k, so get data in millions
  funding_gap_curve$x <- (1 - funding_gap_curve$x)
  # funding_gap_curve$y <- funding_gap_curve$y/scale_by
  names(funding_gap_curve)[2] <- 'Probability of exceeding loss'
  names(funding_gap_curve)[1] <- 'Funding gap'
  funding_gap_curve$`Funding gap` <- -funding_gap_curve$`Funding gap`
  funding_gap_curve$`Funding gap` <- funding_gap_curve$`Funding gap` + budget
  return(funding_gap_curve)
}

