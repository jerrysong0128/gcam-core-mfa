# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' Native-R ODYM dynamic stock model
#'
#' Provide an R6 implementation of the ODYM DynamicStockModel API used by the
#' material-flow-analysis modules in the GCAM data system.
#'
#' @details This native-R implementation follows the principles, model
#' structure, and data organization of the ODYM framework. It supports the
#' inflow-driven and stock-driven dynamic stock calculations required by the
#' GCAM iron and steel modules without runtime dependencies on Python,
#' reticulate, NumPy, or SciPy.
#' @references Pauliuk, S., and N. Heeren. 2020. "ODYM--An open software
#' framework for studying dynamic material systems: Principles,
#' implementation, and data structures." Journal of Industrial Ecology
#' 24: 446--458. \doi{10.1111/jiec.12952}.
#' @author Jerry
#' @keywords internal
NULL
#
# Usage (from input/gcamdata):
#   source(file.path("R", "odym_r.R"))
#   model <- DSM(
#     t = 2000:2050,
#     i = rep(1, 51),
#     lt = list(Type = "Weibull", Shape = 3, Scale = 30)
#   )
#   stock_by_cohort <- model$compute_s_c_inflow_driven()
#   stock_total <- model$compute_stock_total()

if (!requireNamespace("R6", quietly = TRUE)) {
  stop(
    "The native-R DynamicStockModel requires the R6 package. ",
    "Install it with install.packages('R6').",
    call. = FALSE
  )
}

dynamic_stock_model_version <- function() {
  c(
    version = "1.0-r",
    description = paste(
      "Native-R R6 implementation of the ODYM DynamicStockModel methods",
      "used by the LMFA scripts."
    )
  )
}

DynamicStockModel <- R6::R6Class(
  classname = "DynamicStockModel",

  public = list(
    t = NULL,
    i = NULL,
    o = NULL,
    s = NULL,
    lt = NULL,
    s_c = NULL,
    o_c = NULL,
    name = NULL,
    pdf = NULL,
    sf = NULL,

    initialize = function(t = NULL, i = NULL, o = NULL, s = NULL, lt = NULL,
                          s_c = NULL, o_c = NULL, name = "DSM", pdf = NULL,
                          sf = NULL) {
      self$t <- private$numeric_or_null(t)
      self$i <- private$numeric_or_null(i)
      self$o <- private$numeric_or_null(o)
      self$s <- private$numeric_or_null(s)
      self$s_c <- private$matrix_or_null(s_c)
      self$o_c <- private$matrix_or_null(o_c)
      self$name <- as.character(name)[1]
      self$pdf <- private$matrix_or_null(pdf)
      self$sf <- private$matrix_or_null(sf)

      if (!is.null(lt)) {
        if (is.null(self$t)) {
          stop("`t` must be supplied when `lt` is supplied.", call. = FALSE)
        }
        if (is.null(lt$Type)) {
          stop("`lt$Type` must be supplied.", call. = FALSE)
        }
        lt$Type <- as.character(lt$Type)[1]
        parameter_names <- setdiff(names(lt), "Type")
        for (parameter_name in parameter_names) {
          value <- as.numeric(lt[[parameter_name]])
          if (length(value) == 1L) {
            value <- rep(value, length(self$t))
          }
          if (length(value) != length(self$t)) {
            stop(
              sprintf(
                "Lifetime parameter `lt$%s` has length %d; expected 1 or %d.",
                parameter_name, length(value), length(self$t)
              ),
              call. = FALSE
            )
          }
          lt[[parameter_name]] <- value
        }
      }
      self$lt <- lt

      private$validate_time_series_lengths()
      invisible(self)
    },

    dimension_check = function() {
      n <- if (is.null(self$t)) 0L else length(self$t)
      report <- paste0("<br><b> Checking dimensions of dynamic stock model ", self$name, ".")
      report <- paste0(
        report,
        if (is.null(self$t)) "Time vector is not present.<br>" else
          sprintf("Time vector is present with %d years.<br>", n),
        if (is.null(self$i)) "Inflow is not present.<br>" else
          sprintf("Inflow vector is present with %d years.<br>", length(self$i)),
        if (is.null(self$s)) "Total stock is not present.<br>" else
          sprintf("Total stock is present with %d years.<br>", length(self$s)),
        if (is.null(self$s_c)) "Stock by cohorts is not present.<br>" else
          sprintf(
            "Stock by cohorts is present with %d years and %d cohorts.<br>",
            nrow(self$s_c), ncol(self$s_c)
          ),
        if (is.null(self$o)) "Total outflow is not present.<br>" else
          sprintf("Total outflow is present with %d years.<br>", length(self$o)),
        if (is.null(self$o_c)) "Outflow by cohorts is not present.<br>" else
          sprintf(
            "Outflow by cohorts is present with %d years and %d cohorts.<br>",
            nrow(self$o_c), ncol(self$o_c)
          ),
        if (is.null(self$lt)) "Lifetime distribution is not present.<br>" else
          sprintf("Lifetime distribution is present with type %s.<br>", self$lt$Type)
      )
      report
    },

    compute_stock_change = function() {
      if (is.null(self$s)) return(NULL)
      c(self$s[1], diff(self$s))
    },

    check_stock_balance = function() {
      if (is.null(self$i) || is.null(self$o) || is.null(self$s)) return(NULL)
      self$i - self$o - self$compute_stock_change()
    },

    compute_stock_total = function() {
      if (!is.null(self$s)) return(self$s)
      if (is.null(self$s_c)) return(NULL)
      self$s <- rowSums(self$s_c)
      self$s
    },

    compute_outflow_total = function() {
      if (!is.null(self$o)) return(self$o)
      if (is.null(self$o_c)) return(NULL)
      self$o <- rowSums(self$o_c)
      self$o
    },

    compute_outflow_mb = function() {
      if (is.null(self$i) || is.null(self$s)) return(NULL)
      self$o <- self$i - self$compute_stock_change()
      self$o
    },

    compute_sf = function() {
      if (!is.null(self$sf)) return(self$sf)
      if (is.null(self$t) || is.null(self$lt)) return(NULL)

      n <- length(self$t)
      self$sf <- matrix(0, nrow = n, ncol = n)
      lifetime_type <- self$lt$Type
      supported_types <- c("Fixed", "Normal", "FoldedNormal", "LogNormal", "Weibull")
      if (!lifetime_type %in% supported_types) {
        stop(
          sprintf(
            "Unsupported lifetime type `%s`. Supported types: %s.",
            lifetime_type, paste(supported_types, collapse = ", ")
          ),
          call. = FALSE
        )
      }

      for (cohort in seq_len(n)) {
        ages <- 0:(n - cohort)
        rows <- cohort:n

        if (lifetime_type == "Fixed") {
          private$require_lt(c("Mean"))
          self$sf[rows, cohort] <- as.numeric(ages < self$lt$Mean[cohort])
        } else if (lifetime_type == "Normal") {
          private$require_lt(c("Mean", "StdDev"))
          if (self$lt$Mean[cohort] != 0) {
            self$sf[rows, cohort] <- stats::pnorm(
              ages,
              mean = self$lt$Mean[cohort],
              sd = self$lt$StdDev[cohort],
              lower.tail = FALSE
            )
          }
        } else if (lifetime_type == "FoldedNormal") {
          private$require_lt(c("Mean", "StdDev"))
          if (self$lt$Mean[cohort] != 0) {
            mu <- self$lt$Mean[cohort]
            sigma <- self$lt$StdDev[cohort]
            folded_cdf <- stats::pnorm((ages - mu) / sigma) -
              stats::pnorm((-ages - mu) / sigma)
            self$sf[rows, cohort] <- 1 - folded_cdf
          }
        } else if (lifetime_type == "LogNormal") {
          private$require_lt(c("Mean", "StdDev"))
          if (self$lt$Mean[cohort] != 0) {
            mean_lifetime <- self$lt$Mean[cohort]
            std_dev <- self$lt$StdDev[cohort]
            mu_log <- log(mean_lifetime / sqrt(1 + mean_lifetime^2 / std_dev^2))
            sigma_log <- sqrt(log(1 + mean_lifetime^2 / std_dev^2))
            self$sf[rows, cohort] <- stats::plnorm(
              ages,
              meanlog = mu_log,
              sdlog = sigma_log,
              lower.tail = FALSE
            )
          }
        } else if (lifetime_type == "Weibull") {
          private$require_lt(c("Shape", "Scale"))
          shape <- self$lt$Shape[cohort]
          scale <- self$lt$Scale[cohort]
          if (shape != 0) {
            if (scale <= 0) {
              stop("All nonzero-shape Weibull `Scale` values must be positive.", call. = FALSE)
            }
            self$sf[rows, cohort] <- exp(-((ages / scale)^shape))
          }
        }
      }
      self$sf
    },

    compute_outflow_pdf = function() {
      if (!is.null(self$pdf)) return(self$pdf)
      sf <- self$compute_sf()
      if (is.null(sf)) return(NULL)

      n <- nrow(sf)
      self$pdf <- matrix(0, nrow = n, ncol = n)
      diag(self$pdf) <- 1 - diag(sf)
      if (n > 1L) {
        for (cohort in seq_len(n - 1L)) {
          rows <- (cohort + 1L):n
          self$pdf[rows, cohort] <- -diff(sf[cohort:n, cohort])
        }
      }
      self$pdf
    },

    compute_s_c_inflow_driven = function() {
      if (is.null(self$i) || is.null(self$lt)) return(NULL)
      sf <- self$compute_sf()
      self$s_c <- sweep(sf, MARGIN = 2L, STATS = self$i, FUN = "*")
      self$s_c
    },

    compute_o_c_from_s_c = function() {
      if (is.null(self$s_c)) return(NULL)
      if (!is.null(self$o_c)) return(self$o_c)
      if (is.null(self$i)) {
        stop("`i` is required to compute cohort outflow from cohort stock.", call. = FALSE)
      }

      n <- nrow(self$s_c)
      self$o_c <- matrix(0, nrow = n, ncol = ncol(self$s_c))
      if (n > 1L) {
        self$o_c[2:n, ] <- self$s_c[1:(n - 1L), , drop = FALSE] -
          self$s_c[2:n, , drop = FALSE]
      }
      diag(self$o_c) <- self$i - diag(self$s_c)
      self$o_c
    },

    compute_i_from_s = function(InitialStock) {
      if (!is.null(self$i)) return(NULL)
      if (length(InitialStock) != length(self$t)) return(NULL)

      sf <- self$compute_sf()
      final_survival <- sf[nrow(sf), ]
      self$i <- ifelse(final_survival != 0, as.numeric(InitialStock) / final_survival, 0)
      self$i
    },

    compute_evolution_initialstock = function(InitialStock, SwitchTime) {
      if (is.null(self$lt)) return(self$s_c)
      n <- length(self$t)
      switch_index <- as.integer(SwitchTime) + 1L
      historic_columns <- seq_len(as.integer(SwitchTime))
      if (switch_index > n || length(InitialStock) != length(historic_columns)) {
        stop("`SwitchTime` or `InitialStock` has an incompatible length.", call. = FALSE)
      }

      self$s_c <- matrix(0, n, n)
      self$o_c <- matrix(0, n, n)
      sf <- self$compute_sf()
      shares_left <- sf[switch_index, historic_columns]
      self$s_c[switch_index, historic_columns] <- InitialStock
      for (row in switch_index:n) {
        valid <- shares_left != 0
        self$s_c[row, historic_columns[valid]] <-
          InitialStock[valid] * sf[row, historic_columns[valid]] / shares_left[valid]
      }
      self$s_c
    },

    compute_stock_driven_model = function(NegativeInflowCorrect = FALSE) {
      if (is.null(self$s) || is.null(self$lt)) return(list(NULL, NULL, NULL))

      n <- length(self$t)
      self$s_c <- matrix(0, n, n)
      self$o_c <- matrix(0, n, n)
      self$i <- numeric(n)
      sf <- self$compute_sf()

      if (sf[1, 1] != 0) self$i[1] <- self$s[1] / sf[1, 1]
      self$s_c[, 1] <- self$i[1] * sf[, 1]
      self$o_c[1, 1] <- self$i[1] - self$s_c[1, 1]

      if (n > 1L) {
        for (year in 2:n) {
          previous_cohorts <- seq_len(year - 1L)
          self$o_c[year, previous_cohorts] <-
            self$s_c[year - 1L, previous_cohorts] -
            self$s_c[year, previous_cohorts]

          inflow_test <- self$s[year] - sum(self$s_c[year, ])

          if (isTRUE(NegativeInflowCorrect) && inflow_test < 0) {
            delta <- -inflow_test
            remaining_stock <- sum(self$s_c[year, ])
            delta_percent <- if (remaining_stock != 0) delta / remaining_stock else 0

            self$i[year] <- 0
            self$o_c[year, ] <- self$o_c[year, ] +
              self$s_c[year, ] * delta_percent
            self$s_c[year:n, previous_cohorts] <-
              self$s_c[year:n, previous_cohorts, drop = FALSE] *
              (1 - delta_percent)
          } else {
            if (sf[year, year] != 0) {
              self$i[year] <- inflow_test / sf[year, year]
            }
            self$s_c[year:n, year] <- self$i[year] * sf[year:n, year]
            self$o_c[year, year] <- self$i[year] * (1 - sf[year, year])
          }
        }
      }

      # A plain R list intentionally matches reticulate's Python-tuple access:
      # outputs[[1]], outputs[[2]], and outputs[[3]].
      list(self$s_c, self$o_c, self$i)
    }
  ),

  private = list(
    numeric_or_null = function(x) {
      if (is.null(x)) NULL else as.numeric(x)
    },

    matrix_or_null = function(x) {
      if (is.null(x)) NULL else as.matrix(x)
    },

    validate_time_series_lengths = function() {
      if (is.null(self$t)) return(invisible(NULL))
      n <- length(self$t)
      for (field in c("i", "o", "s")) {
        value <- self[[field]]
        if (!is.null(value) && length(value) != n) {
          stop(
            sprintf("`%s` has length %d; expected %d.", field, length(value), n),
            call. = FALSE
          )
        }
      }
      for (field in c("s_c", "o_c", "pdf", "sf")) {
        value <- self[[field]]
        if (!is.null(value) && !identical(dim(value), c(n, n))) {
          stop(sprintf("`%s` must be a %d x %d matrix.", field, n, n), call. = FALSE)
        }
      }
      invisible(NULL)
    },

    require_lt = function(parameter_names) {
      missing_names <- parameter_names[!vapply(
        parameter_names,
        function(parameter_name) !is.null(self$lt[[parameter_name]]),
        logical(1)
      )]
      if (length(missing_names) > 0L) {
        stop(
          sprintf(
            "Lifetime type `%s` requires parameter(s): %s.",
            self$lt$Type, paste(missing_names, collapse = ", ")
          ),
          call. = FALSE
        )
      }
      invisible(NULL)
    }
  )
)

# Compatibility factory: existing LMFA code can continue to call DSM(...),
# exactly as it did with the Python class imported through reticulate.
DSM <- function(...) {
  DynamicStockModel$new(...)
}
