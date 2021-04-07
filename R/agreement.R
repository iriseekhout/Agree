#' Agreement
#'
#' The default function returns the proportion of overall agreement between two
#' or more raters. Specific agreements can be obtained as well.
#' Specific agreement (averages over discordant cells to correct for random rater combinations).
#' When there are 2 categories, this is equal to the positive/negative agreement. When there are
#' more than two categories, one can either look at the agreement for one category versus the
#' others, for example very satisfied versus rest or for one category versus one specific other
#' category, for example very satisfies versus not satisfied.
#'
#' De confidence intervals are obtained and adjusted with a Fleis continuity correction. For the specific agreements with polytomous data, the confidence intervals are bootstrapped.
#'
#' @param data A data frame or table with equal number of columns and rows. Or a
#'  data frame that contains the scores for each rater in each column.
#' @param specific A character vector indicating the category for which specific
#'  agreement should be calculated. If \code{length(specific) == 1}, the named
#'  factor level is compared to all others, if \code{length(specific) == 2} the
#'  specific agreement is of the first category level is compared to the second
#'  category level. If \code{specific = "positive"} or
#'  \code{specific = "negative"} and scores are dichotomous, the positive or
#'  negative agreement will be returned.
#' @param confint logical vector if confidence interval for agreements are computed.
#' @param alpha the confidence interval level
#' @param n sample size; default \code{n = nrow(data)}
#' @param k number of raters; default \code{k = ncol(data)}
#' @param b number of bootstrap iterations for bootstrapped CI
#' @param \dots options for sumtable if \code{data = data.frame}
#' @return An S3 object containing the proportion of overall agreement.
#' @export
#'
#' @examples
#' #dichotomous
#' df <- data.frame(r1=factor(c(1,0,1,0,0,1,1,0,0,0,1,1,0,1,1)),
#'                  r2=factor(c(1,1,1,1,0,1,1,0,0,0,1,1,0,1,0)),
#'                  r3=factor(c(1,1,1,0,0,0,1,1,1,0,0,1,0,1,1)),
#'                  r4=factor(c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)))
#' table <- sumtable(df=df, ratings=c("r1", "r2", "r3", "r4"),
#' levels=c("0","1"))
#' agreement(table)
#' agreement(table, specific = "1")
#' agreement(table, specific = "1", confint = TRUE)
#' agreement(table, specific = "positive")
#' agreement(df)
#' agreement(df, specific = "1")
#' agreement(df, specific = "positive")
#' agreement(df, specific = "negative")
#' #polytomous
#' df <- data.frame(r1=factor(c(1,2,2,0,3,3,1,0,3,0,2,2,0,3,1)),
#'                  r2=factor(c(1,1,1,0,3,3,1,0,1,0,2,2,0,2,1)),
#'                  r3=factor(c(1,1,1,3,3,2,1,0,1,0,2,2,0,3,1)),
#'                  r4=factor(c(1,2,1,0,3,3,1,0,3,0,2,2,0,2,1)))
#' table <- sumtable(df=df, ratings=c("r1", "r2", "r3", "r4"),
#' levels=c("0","1", "2", "3"))
#' agreement(table, specific = c("3", "1"))
#' agreement(table, specific = c("3", "1", "2"))
#' agreement(table, specific = c(3))
#' agreement(table, specific = c("3", "1"), confint = TRUE)
#' agreement(df, specific = c("3", "1"), confint = TRUE)
agreement <- function(data,
                      specific = NULL,
                      confint = FALSE,
                      alpha = 0.05,
                      n = nrow(data),
                      k = ncol(data),
                      b = 1000,
                      ...){
  if (is.data.frame(data)) {
    table <- Agree::sumtable(data, ...)
    if(is.null(n)) n <- nrow(data)
    if(is.null(k)) k <- ncol(data)
  }
  if (nrow(data) == ncol(data)) {
    table <- data

    if(confint){
    if(ncol(table) > 2 & !is.null(specific)){#if table input and more than 2 categories and specific agreement toggle confint.
      warning("input should be a data.frame to obtain confidence intervals for specific agreement with more than 2 factor levels; confinterval are not returned")
      confint <- FALSE}

    if(is.null(n)){stop("n not found")} #can be calculated from rater-number and table counts
    if(is.null(k)){stop("k not found")}
    }
  }

  ## General CI function:
  ci <- function(p, n, alpha) {
    a <- qnorm(1 - ((alpha) / 2))
    CCI <-
      p + c(-(a * (sqrt(1 / n * (
        p * (1 - p)
      ))) - 1 / (2 * n)), 0, (a * (sqrt(1 / n * (
        p * (1 - p)
      ))) + 1 / (2 * n))) #continuity
    FCIlow <-
      ((2 * n * p + (a * a) - 1) - a * sqrt((a * a) - (2 + (1 / n)) + 4 * p *
                                              (n * (1 - p) + 1))) / (2 * ((a * a) + n)) #Fleis
    FCIhigh <-
      ((2 * n * p + (a * a) - 1) + a * sqrt((a * a) - (2 + (1 / n)) + 4 * p *
                                              (n * (1 - p) + 1))) / (2 * ((a * a) + n))#Fleis
    if (p < 0.5) {
      CI <-  c(FCIlow, CCI[3])
    }
    if (p >= 0.5) {
      CI <-  c(CCI[1], FCIhigh)
    }
    names(CI) <- c("lower", "upper")
    CI
  }

  ##overall agreement
  agreement <- sum(diag(table) / sum(table))
  names(agreement) <- "overall agreement"

  if(confint){
    n <- n*sqrt(k-1) #n for CI for overall agreement
    CI <- ci(p = agreement, n = n, alpha = alpha)
    agreement <- c(agreement, CI)
  }

  if (is.null(specific)) {
    return(agreement)
  }

   ## alle specific agreements
  if (!is.null(specific)) {
    #if specific categories not in data or in positive negative (when ncol ==2)
    if(!all(specific %in% c(colnames(table), "positive", "negative"))){
      stop("Category levels are not present in the data.frame or table")
    }
    if(any(specific %in% c("positive", "negative")) & ncol(table) > 2){
      stop("Positive or negative agreement can only be obtained for 2x2 tables.")
    }



    if (length(specific > 1)) {
      if (length(specific) > 2) {
        warning(
          paste("The specific agreement cannot be obtained for more than two categories; only the first argument is used and compared to all other category levels available. To avoid this warning use: specific =", specific[1])
        )
        specific <- specific[1]
      }
      if (length(specific) == 2) {
        #if there is a cat2, then cat1 versus cat2
        sa <-
          (2 * table[specific[1], specific[1]]) / (2 * table[specific[1], specific[1]] + table[specific[1], specific[2]] + table[specific[2], specific[1]])
        names(sa) <-
          paste("specific agreement:", specific[1], "vs", specific[2])
        agr <- c(agreement, sa)

        if(confint & ncol(table) == 2){# CI specific for dichotomous
          n <- ((((table[specific[1], specific[1]]+table[specific[1], specific[2]])+(table[specific[1], specific[1]]+table[specific[2], specific[1]]))/2) /(k*(k-1)/2))*sqrt(k-1)
          CI <-  ci(p = sa, n = n, alpha = alpha)
          sa <- c(sa, CI)
          agr <- rbind(agreement, sa)
          rownames(agr) <- c(names(agreement)[1], names(sa)[1])
          colnames(agr)[1] <- "p"
        }

        if(confint & ncol(table) > 2){ #for more than 2 levels use bootstapping
          sa.agreement <- function(tab, s){ #specific agreement 1 vs all function
            sa <- (2 * tab[s[1], s[1]]) / (2 * tab[s[1], s[1]] + tab[s[1], s[2]] + tab[s[2], s[1]])
            sa
          }

          agree.boot <- function(data,x) {
            sa.agreement(sumtable(data[x,]), s = specific)}
          res1a <- boot::boot(data,agree.boot,b)
          BCI_agr <-  quantile(res1a$t,c((alpha)/2,(1-alpha)+((alpha)/2)), na.rm=TRUE)
          CI <- c(lower_boot=BCI_agr[1], upper_boot=BCI_agr[2])
          sa <- c(sa, CI)
          agr <- rbind(agreement, sa)
          rownames(agr) <- c(names(agreement)[1], names(sa)[1])
          colnames(agr)[1] <- "p"
        }


        }
      }


    if (length(specific) == 1) {
      ##positive/negative agreement
      if (specific == "positive" & ncol(table) == 2) {
        sa <- (2 * (table[1, 1])) / ((2 * (table[1, 1])) + (table[1, 2]) + (table[2, 1]))
        names(sa) <- paste("positive agreement")
        agr <- c(agreement, sa)

        if(confint){# CI positive for dichotomous
          n <- ((table[1,1]+(sum(table[1,])-table[1, 1]))/(k*(k-1)/2))*sqrt(k-1)
          CI <-  ci(p = sa, n = n, alpha = alpha)
          sa <- c(sa, CI)
          agr <- rbind(agreement, sa)
          rownames(agr) <- c(names(agreement)[1], names(sa)[1])
          colnames(agr)[1] <- "p"
        }

      } else
        if (specific == "negative" & ncol(table) == 2) {
          sa <- (2 * (table[2, 2])) / ((2 * (table[2, 2])) + (table[1, 2]) + (table[2, 1]))
          names(sa) <- paste("negative agreement")
          agr <- c(agreement, sa)

          if(confint){# CI positive for dichotomous
            n <- ((table[2,2]+(sum(table[2,])-table[2, 2]))/(k*(k-1)/2))*sqrt(k-1)
            CI <-  ci(p = sa, n = n, alpha = alpha)
            sa <- c(sa, CI)
            agr <- rbind(agreement, sa)
            rownames(agr) <- c(names(agreement)[1], names(sa)[1])
            colnames(agr)[1] <- "p"
          }


        } else{
          ##specific agreement
          sa <- (2 * table[specific, specific]) /
            (2 * table[specific, specific] +
               (sum(table[, specific]) - table[specific, specific]) +
               (sum(table[specific, ]) - table[specific, specific]))
        names(sa) <- paste("specific agreement:", specific)
        agr <- c(agreement, sa)

        if(confint & ncol(table) == 2){# CI positive for dichotomous
          n <- ((table[specific,specific]+(sum(table[specific,])-table[specific,specific]))/(k*(k-1)/2))*sqrt(k-1)
          CI <-  ci(p = sa, n = n, alpha = alpha)
          sa <- c(sa, CI)
          agr <- rbind(agreement, sa)
          rownames(agr) <- c(names(agreement)[1], names(sa)[1])
          colnames(agr)[1] <- "p"
        }

        if(confint & ncol(table) > 2){ #for more than 2 levels use bootstapping

          sa.agreement <- function(tab, s){ #specific agreement 1 vs all function
            sa <- (2 * tab[s, s]) /
              (2 * tab[s, s] +
                 (sum(tab[, s]) - tab[s, s]) +
                 (sum(tab[s, ]) - tab[s, s]))
            sa
          }

          agree.boot <- function(data,x) {
            sa.agreement(sumtable(data[x,]), s = specific)}
          res1a <- boot::boot(data,agree.boot,b)
          BCI_agr <-  quantile(res1a$t,c((alpha)/2,(1-alpha)+((alpha)/2)), na.rm=TRUE)
          CI <- c(lower_boot=BCI_agr[1], upper_boot=BCI_agr[2])
          sa <- c(sa, CI)
          agr <- rbind(agreement, sa)
          rownames(agr) <- c(names(agreement)[1], names(sa)[1])
          colnames(agr)[1] <- "p"
        }


        }
    }



    return(agr)

  }

}
