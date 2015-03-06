wilcox.sign <- function (x, mu = 0, exact = NULL, correct = TRUE, conf.int = TRUE, 
          conf.level = 0.95, ...) 
{
  
  if (!missing(mu) && ((length(mu) > 1L) || !is.finite(mu))) 
    stop("'mu' must be a single number")
  if (conf.int) {
    if (!((length(conf.level) == 1L) && is.finite(conf.level) && 
            (conf.level > 0) && (conf.level < 1))) 
      stop("'conf.level' must be a single number between 0 and 1")
  }
  if (!is.numeric(x)) 
    stop("'x' must be numeric")
  
  DNAME <- deparse(substitute(x))
  x <- x[is.finite(x)]
  if (length(x) < 1L) 
    stop("not enough (finite) 'x' observations")
  CORRECTION <- 0
  
    METHOD <- "Wilcoxon signed rank test"
    x <- x - mu
    ZEROES <- any(x == 0)
    if (ZEROES) 
      x <- x[x != 0]
    n <- as.double(length(x))
    if (is.null(exact)) 
      exact <- (n < 50)
    r <- rank(abs(x))
    STATISTIC <- setNames(sum(r[x > 0]), "V")
    TIES <- length(r) != length(unique(r))
    if (exact && !TIES && !ZEROES) {
      
        p <- if (STATISTIC > (n * (n + 1)/4)) psignrank(STATISTIC - 
                                                          1, n, lower.tail = FALSE) else psignrank(STATISTIC, 
                                                                                                   n)
        PVAL <- min(2 * p, 1)
      
      if (conf.int) {
        x <- x + mu
        alpha <- 1 - conf.level
        diffs <- outer(x, x, "+")
        diffs <- sort(diffs[!lower.tri(diffs)])/2
        qu <- qsignrank(alpha/2, n)
        if (qu == 0) qu <- 1
        ql <- n * (n + 1)/2 - qu
        achieved.alpha <- 2 * psignrank(trunc(qu) - 1, n)
        cint <- c(diffs[qu], diffs[ql + 1])
        
        if (achieved.alpha - alpha > alpha/2) {
          warning("requested conf.level not achievable")
          conf.level <- 1 - signif(achieved.alpha, 2)
        }
        attr(cint, "conf.level") <- conf.level
        ESTIMATE <- c(`(pseudo)median` = median(diffs))
      }
    }
    else {
      NTIES <- table(r)
      z <- STATISTIC - n * (n + 1)/4
      SIGMA <- sqrt(n * (n + 1) * (2 * n + 1)/24 - sum(NTIES^3 - 
                                                         NTIES)/48)
      if (correct) {
        CORRECTION <- sign(z) * 0.5
        METHOD <- paste(METHOD, "with continuity correction")
      }
      z <- (z - CORRECTION)/SIGMA
      PVAL <-  2 * min(pnorm(z), pnorm(z, lower.tail = FALSE))
      if (conf.int) {
        x <- x + mu
        alpha <- 1 - conf.level
        mumin <- min(x)
        mumax <- max(x)
        wdiff <- function(d, zq) {
          xd <- x - d
          xd <- xd[xd != 0]
          nx <- length(xd)
          dr <- rank(abs(xd))
          zd <- sum(dr[xd > 0]) - nx * (nx + 1)/4
          NTIES.CI <- table(dr)
          SIGMA.CI <- sqrt(nx * (nx + 1) * (2 * nx + 1)/24 - sum(NTIES.CI^3 - NTIES.CI)/48)
          if (SIGMA.CI == 0) 
            stop("cannot compute confidence interval when all observations are tied", 
                 call. = FALSE)
          CORRECTION.CI <- if (correct) {
             sign(zd) *  0.5
          }
          else 0
          (zd - CORRECTION.CI)/SIGMA.CI - zq
        }
        cint <- repeat {
            mindiff <- wdiff(mumin, zq = qnorm(alpha/2, 
                                               lower.tail = FALSE))
            maxdiff <- wdiff(mumax, zq = qnorm(alpha/2))
            if (mindiff < 0 || maxdiff > 0) alpha <- alpha * 
              2 else break
          }
          if (1 - conf.level < alpha * 0.75) {
            conf.level <- 1 - alpha
            warning("requested conf.level not achievable")
          }
          l <- uniroot(wdiff, c(mumin, mumax), tol = 1e-04, 
                       zq = qnorm(alpha/2, lower.tail = FALSE))$root
          u <- uniroot(wdiff, c(mumin, mumax), tol = 1e-04, 
                       zq = qnorm(alpha/2))$root
          c(l, u)
        }
        attr(cint, "conf.level") <- conf.level
        correct <- FALSE
        ESTIMATE <- c(`(pseudo)median` = uniroot(wdiff, 
                                                 c(mumin, mumax), tol = 1e-04, zq = 0)$root)
      }
      if (exact && TIES) {
        warning("cannot compute exact p-value with ties")
        if (conf.int) 
          warning("cannot compute exact confidence interval with ties")
      }
      if (exact && ZEROES) {
        warning("cannot compute exact p-value with zeroes")
        if (conf.int) 
          warning("cannot compute exact confidence interval with zeroes")
      }
    
  names(mu) <-  "location"
  RVAL <- list(statistic = STATISTIC, parameter = NULL, p.value = as.numeric(PVAL), 
               null.value = mu,  method = METHOD, 
               data.name = DNAME)
  if (conf.int) 
    RVAL <- c(RVAL, list(conf.int = cint, estimate = ESTIMATE))
  class(RVAL) <- "htest"
  RVAL
  
}
