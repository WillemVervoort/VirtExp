\name{simhyd.sim}
\alias{simhyd.sim}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
simhyd.sim(DATA, INSC, COEFF, SQ, SMSC, SUB, CRAK, K, etmult = 0.15, return_state = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{DATA}{
%%     ~~Describe \code{DATA} here~~
}
  \item{INSC}{
%%     ~~Describe \code{INSC} here~~
}
  \item{COEFF}{
%%     ~~Describe \code{COEFF} here~~
}
  \item{SQ}{
%%     ~~Describe \code{SQ} here~~
}
  \item{SMSC}{
%%     ~~Describe \code{SMSC} here~~
}
  \item{SUB}{
%%     ~~Describe \code{SUB} here~~
}
  \item{CRAK}{
%%     ~~Describe \code{CRAK} here~~
}
  \item{K}{
%%     ~~Describe \code{K} here~~
}
  \item{etmult}{
%%     ~~Describe \code{etmult} here~~
}
  \item{return_state}{
%%     ~~Describe \code{return_state} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (DATA, INSC, COEFF, SQ, SMSC, SUB, CRAK, K, etmult = 0.15, 
    return_state = FALSE) 
{
    stopifnot(c("P", "E") \%in\% colnames(DATA))
    stopifnot(INSC >= 0)
    stopifnot(COEFF >= 0)
    stopifnot(SQ >= 0)
    stopifnot(SMSC >= 0)
    stopifnot(SUB >= 0)
    stopifnot(CRAK >= 0)
    stopifnot(K >= 0)
    xpar <- c(INSC, COEFF, SQ, SMSC, SUB, CRAK, K)
    inAttr <- attributes(DATA[, 1])
    DATA <- as.ts(DATA)
    P <- DATA[, "P"]
    E <- etmult * DATA[, "E"]
    bad <- is.na(P) | is.na(E)
    P[bad] <- 0
    E[bad] <- 0
    COMPILED <- (hydromad.getOption("pure.R.code") == FALSE)
    if (COMPILED) {
        ans <- simhyd_sim(P, E, INSC, COEFF, SQ, SMSC, SUB, CRAK, 
            K)
        U <- ans$U
        ET <- ans$ET
    }
    else {
        U <- IMAX <- INT <- INR <- RMO <- IRUN <- ET <- SRUN <- REC <- SMF <- POT <- BAS <- SMS <- GW <- rep(NA_real_, 
            length(P))
        GWt1 <- 0
        SMSt1 <- 0.5 * SMSC
        for (t in seq(1, length(P))) {
            IMAX[t] <- min(INSC, E[t])
            INT[t] <- min(IMAX[t], P[t])
            INR[t] <- P[t] - INT[t]
            RMO[t] <- min(COEFF * exp(-SQ * SMSt1/SMSC), INR[t])
            IRUN[t] <- INR[t] - RMO[t]
            SRUN[t] = SUB * SMSt1/SMSC * RMO[t]
            REC[t] <- CRAK * SMSt1/SMSC * (RMO[t] - SRUN[t])
            SMF[t] <- RMO[t] - SRUN[t] - REC[t]
            POT[t] <- E[t] - INT[t]
            ET[t] <- min(10 * SMSt1/SMSC, POT[t])
            SMS[t] <- SMSt1 + SMF[t] - ET[t]
            if (SMS[t] > SMSC) {
                SMS[t] <- SMSC
                REC[t] <- REC[t] + SMS[t] - SMSC
            }
            SMSt1 <- SMS[t]
            BAS[t] <- K * GWt1
            GW[t] <- GWt1 + REC[t] - BAS[t]
            GWt1 <- GW[t]
            U[t] <- IRUN[t] + SRUN[t] + BAS[t]
        }
    }
    attributes(U) <- inAttr
    attributes(ET) <- inAttr
    U[bad] <- NA
    ET[bad] <- NA
    if (return_state == TRUE) {
        return(merge(U = U, ET = ET))
    }
    else {
        return(U)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
