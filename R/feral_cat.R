library(tidyverse)
#' @import ggplot2

#' @export
max_lambda <- function(x) {
  Re((eigen(x)$values)[1])
}

#' @export
max_r <- function(x) {
  log(max_lambda(x))
}

#' @export
stable_stage_dist <- function(x) {
  real_first_eigen_vector <- Re((eigen(x)$vectors)[, 1])
  parallel_matrix <- x %*% real_first_eigen_vector
  (parallel_matrix / (sum(parallel_matrix)))[, 1]
}

# Generation length function
# reproductive value (r_0) where leslie_matrix = Leslie matrix; age_max = maximum age of females
#' @export
total_female_offspring_per_female <- function(leslie_matrix, age_max) {
  # define the transition matrix
  transition_matrix <- leslie_matrix[1:age_max, 1:age_max]
  transition_matrix[1, 1:(age_max)] <- 0

  # define the fertility matrix
  fertility_matrix <- leslie_matrix[1:age_max, 1:age_max]
  diag(fertility_matrix[2:age_max, 1:(age_max - 1)]) <- 0

  # define the identity matrix
  identity_matrix <- matrix(data = 0, nrow = age_max, ncol = age_max)
  diag(identity_matrix) <- 1

  # define the fundamental matrix
  n_fund <- MASS::ginv(identity_matrix - transition_matrix)

  # define the reproductive matrix
  reproductive_matrix <- fertility_matrix %*% n_fund

  # define r_0 (number of female offspring produced per female during lifetime)
  r_0 <- Re((eigen(reproductive_matrix)$values)[1])

  return(r_0)
}

# Mean generation time function
# where leslie_matrix is a Leslie Matrix
#' @export
g_val <- function(leslie_matrix, age_max) {
  mean_generation_time <- (log(total_female_offspring_per_female(leslie_matrix, age_max))) / (log(Re((eigen(leslie_matrix)$values)[1])))
  return(mean_generation_time)
}

#' @export
coefficients_proportion_realized_survival <- function(k_vec, red_vec) {
  k_red_dat <- data.frame(k_vec, red_vec)
  param_init <- c(1, 15000, 2.5)
  fit_lp <- nls(red_vec ~ a / (1 + (k_vec / b)^c),
    data = k_red_dat,
    algorithm = "port",
    start = c(a = param_init[1], b = param_init[2], c = param_init[3]),
    trace = TRUE,
    nls.control(maxiter = 1000, tol = 1e-05, minFactor = 1 / 1024)
  )
  coefficients <- clean_coefficients(fit_lp)
}

clean_coefficients <- function(fit_lp) {
  a_lp <- coef(fit_lp)[1]
  b_lp <- coef(fit_lp)[2]
  c_lp <- coef(fit_lp)[3]
  names(a_lp) <- NULL
  names(b_lp) <- NULL
  names(c_lp) <- NULL
  coefficients <- list(a_lp = a_lp, b_lp = b_lp, c_lp = c_lp)
}

#' @export
survival_modifier <- function(tot_n_i, coefficients) {
  pred_red <- coefficients$a_lp / (1 + (tot_n_i / coefficients$b_lp)^coefficients$c_lp)
}

#' @export
modifier_survival_probability <- function(tot_n_i, coefficients, survival_probability) {
  pred_red <- popdyn::survival_modifier(tot_n_i, coefficients)
  modified_survival_probability <- survival_probability * pred_red
  return(modified_survival_probability)
}

#' @export
matrix_leslie <- function(fertility, survival_probability) {
  age_max <- length(fertility)
  popmat <- matrix(data = 0, nrow = age_max, ncol = age_max)
  diag(popmat[2:age_max, ]) <- survival_probability
  popmat[age_max, age_max] <- 0
  popmat[1, ] <- fertility
  return(popmat)
}

#' @export
Population <- R6::R6Class("Population",
  public = list(
    survival = NULL,
    n_mat = NULL,
    sequence_years = NULL,
    initialize = function(survival) {
      self$survival <- survival
    }
  ),
  private = list()
)

#' @export
Runner_Population <- R6::R6Class("Runner_Population",
  public = list(
    population = NULL,
    n_mat = NULL,
    sequence_years = NULL,
    initialize = function(population) {
      self$population <- population
    },
    run_generations = function(interval_time, initial_population) {
      n_mat <- private$setup_variables(interval_time, initial_population)
      for (year in 1:private$years) {
        year_eradication <- private$run_a_year(n_mat[, year])
        n_mat[, year + 1] <- year_eradication$population
      }
      self$n_mat <- n_mat
    }
  ),
  private = list(
    years = NULL,
    setup_variables = function(interval_time, initial_population) {
      private$setup_temporal_variables(interval_time)
      n_mat <- private$setup_matrix_population(initial_population)
      return(n_mat)
    },
    setup_temporal_variables = function(interval_time) {
      private$years <- interval_time$get_years()
      self$sequence_years <- interval_time$get_time_sequence()
    },
    setup_matrix_population = function(initial_population) {
      age_max <- length(self$population$survival$get_fertility())
      n_mat <- matrix(0, nrow = age_max, ncol = (private$years + 1))
      popmat <- matrix_leslie(self$population$survival$get_fertility(), self$population$survival$get_survival())
      ssd <- popdyn::stable_stage_dist(popmat)
      classes_age_population <- ssd * initial_population
      n_mat[, 1] <- classes_age_population
      return(n_mat)
    },
    run_a_year = function(n_mat) {
      popmat <- matrix_leslie(self$population$survival$get_fertility(), self$population$survival$get_survival())
      population_next_year <- popmat %*% n_mat
      year_eradication <- list(population = population_next_year)
      return(year_eradication)
    }
  )
)

#' @export
Plotter_Population <- R6::R6Class("Plotter_Population",
  public = list(
    initialize = function() {
    },
    plot = function(population) {
      individuals <- private$setup_variables(population)
      y_ticks <- private$setup_y_ticks(individuals)
      private$make_plot(individuals, y_ticks)
      return(private$plot_population)
    },
    plot_carry_capacity = function(Carry_Capacity) {
      private$plot_population +
        geom_hline(
          aes(yintercept = Carry_Capacity$k_max, linetype = "Capacidad de carga"),
          color = "red"
        )
    },
    save = function(path) {
      ggsave(path)
    }
  ),
  private = list(
    plot_population = NULL,
    setup_variables = function(population) {
      n_pred <- colSums(population$n_mat)
      individuals <- tibble::tibble(yrs = as.character(population$sequence_years), n_pred)
      return(individuals)
    },
    setup_y_ticks = function(individuals) {
      marcasEjeY <- pretty(c(0, max(individuals$n_pred)))
      return(marcasEjeY)
    },
    make_plot = function(individuals, y_ticks) {
      private$plot_population <- ggplot(data = individuals, aes(x = yrs, y = n_pred)) +
        geom_point(shape = 19) +
        geom_line(linetype = "dashed") +
        scale_y_continuous(
          expand = c(0, 0),
          limits = range(y_ticks),
          breaks = y_ticks
        ) +
        labs(x = "", y = "Number of individuals (cats)") +
        theme_classic()
    }
  )
)

#' @export
Carry_Capacity <- R6::R6Class("Carry_Capacity",
  public = list(
    red_vec = c(1, 0.965, 0.89, 0.79, 0.71),
    k_max = NULL,
    initialize = function() {
    },
    coefficients_model = function(half_capacity) {
      self$k_max <- 2 * half_capacity
      k_vec <- c(1, half_capacity / 2, half_capacity, 0.75 * self$k_max, self$k_max)
      coefficients <- coefficients_proportion_realized_survival(k_vec, self$red_vec)
      return(coefficients)
    }
  ),
  private = list()
)

Abstract_Interval_Time <- R6::R6Class("Abstract_Interval_Time",
  public = list(
    initialize = function(initial_year, final_year) {
      private$initial_year <- initial_year
      private$final_year <- final_year
    },
    get_years = function() {
      stop("This is an abstract method from the Abstract_Interval_Time class. Please implement this method.")
    },
    get_time_sequence = function() {
      stop("This is an abstract method from the Abstract_Interval_Time class. Please implement this method.")
    }
  ),
  private = list(
    initial_year = NULL,
    final_year = NULL
  )
)

#' @export
Interval_Time <- R6::R6Class("Interval_Time",
  inherit = Abstract_Interval_Time,
  public = list(
    get_years = function() {
      diff_years <- private$final_year - private$initial_year
      return(diff_years)
    },
    get_time_sequence = function() {
      sequence_years <- seq(private$initial_year, private$final_year, 1)
      return(sequence_years)
    }
  ),
  private = list()
)

#' @export
Monthly_Interval_Time <- R6::R6Class("Monthly_Interval_Time",
  inherit = Abstract_Interval_Time,
  public = list(
    get_years = function() {
      diff_years <- (private$final_year - private$initial_year) * 12
      return(diff_years)
    },
    get_time_sequence = function() {
      sequence_years <- seq(private$initial_year, private$final_year, 1 / 12)
      return(sequence_years)
    }
  ),
  private = list()
)

#' @export
Runner_Population_With_CC <- R6::R6Class("Runner_Population_With_CC",
  inherit = Runner_Population,
  public = list(
    coefficients = NULL,
    initialize = function(population, coefficients) {
      self$population <- population
      self$coefficients <- coefficients
    }
  ),
  private = list(
    run_a_year = function(n_mat) {
      tot_n_i <- sum(n_mat)
      modified_survival_probability <- modifier_survival_probability(tot_n_i, self$coefficients, self$population$survival$get_survival())
      popmat <- matrix_leslie(self$population$survival$get_fertility(), modified_survival_probability)
      population_next_year <- popmat %*% n_mat
      year_eradication <- list(population = population_next_year)
      return(year_eradication)
    }
  )
)

#' @export
Runner_Population_With_CC_harvest <- R6::R6Class("Runner_Population_With_CC_harvest",
  inherit = Runner_Population,
  public = list(
    coefficients = NULL,
    harvest = NULL,
    k_mat = NULL,
    initialize = function(population, coefficients, harvest) {
      self$population <- population
      self$coefficients <- coefficients
      self$harvest <- harvest
    },
    run_generations = function(interval_time, initial_population) {
      n_mat <- private$setup_variables(interval_time, initial_population)
      k_mat <- private$set_k_mat()
      for (year in 1:private$years) {
        year_eradication <- private$run_a_year(n_mat[, year])
        n_mat[, year + 1] <- year_eradication$population
        k_mat[, year + 1] <- year_eradication$cats
      }
      self$n_mat <- n_mat
      self$k_mat <- k_mat
    }
  ),
  private = list(
    run_a_year = function(n_mat) {
      tot_n_i <- sum(n_mat)
      modified_survival_probability <- modifier_survival_probability(tot_n_i, self$coefficients, self$population$survival$get_survival())
      popmat <- matrix_leslie(self$population$survival$get_fertility(), modified_survival_probability)
      population_next_year <- popmat %*% n_mat
      ssd <- popdyn::stable_stage_dist(popmat)
      hunted_cats <- round(ssd * round(sum(population_next_year) * self$harvest$get_harvest(), 0), 0)
      population_next_year <- population_next_year - hunted_cats
      population_next_year[which(population_next_year < 0)] <- 0
      year_eradication <- list(population = population_next_year, cats = hunted_cats)
      return(year_eradication)
    },
    set_k_mat = function() {
      age_max <- length(self$population$survival$get_fertility())
      k_mat <- matrix(0, nrow = age_max, ncol = (private$years + 1))
    }
  )
)

#' @export
Annualy_Harvest <- R6::R6Class("Annualy_Harvest",
  public = list(
    proportion = NULL,
    initialize = function(proportion) {
      self$proportion <- proportion
    },
    get_harvest = function() {
      return(self$proportion)
    }
  ),
  private = list()
)

#' @export
Many_Harvest <- R6::R6Class("Many_Harvest",
  public = list(
    proportion = NULL,
    initialize = function(proportion) {
      self$proportion <- proportion
    },
    get_harvest = function() {
      private$year <- private$year + 1
      return(self$proportion[private$year])
    }
  ),
  private = list(
    year = 0
  )
)
