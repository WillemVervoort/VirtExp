\name{simhyd.sim}
\alias{simhyd.sim}
\alias{simhydrouting.sim}
\title{
SimHyd
}
\description{
Hydromad implementation of the rainfall runoff model SimHyd, originally developed by F. Chiew at the University of Melbourne. 
It is a conceptual rainfall-runoff model that has 7 parameters and divides the input rainfall in to surface and subsurface flow paths. 
The model includes an interception component. The routing function is the "routing component" that was added later in several studies. 
This simply follows a muskinghum routing function
}
\usage{
simhyd.sim(DATA, INSC, COEFF, SQ, SMSC, SUB, CRAK, K, etmult = 0.15, return_state = FALSE)
}
\usage{
simhydrouting.sim(U, DELAY=1, X_m=0.2,epsilon = hydromad.getOption("sim.epsilon"),return_components = FALSE
}
\arguments{
  \item{DATA}{
    a \code{\link{ts}}-like object with named columns:
    \describe{
      \item{\code{P}}{ time series of areal rainfall depths, usually in mm. }
      \item{\code{E}}{ time series of potential evapo-transpiration, or more typically,
	temperature as an indicator of this. }
    }
}
 \item{U}{effective rainfall series.}
  \item{INSC}{
	interception store capacity (mm)
}
  \item{COEFF}{
	maximum infiltration loss
}
  \item{SQ}{
	infiltration loss exponent
}
  \item{SMSC}{
	soil moisture store capacity
}
  \item{SUB}{
	constant of proportionality in interflow equation 
}
  \item{CRAK}{
	constant of proportionality in groundwater flow equation
}
  \item{K}{
	baseflow linear recession parameter
}
  \item{etmult}{
	Multiplier for the \code{E} input data.
}
	\item{DELAY} {
	Delay term in the Muskinghum equation (days), or sometimes seen as the ratio between inflows and discharge. The expression for the storage delay in the Muskinghum equation is:
			S=DELAY*[X_m*U + (1 - X_m)*Q]
	{
 \item{X_m} {
	Parameter in the muskinghum equation that balances between inflow (U) and outflows (Q). 
	A value of 0.2 means that 20% of the past outflow is taken into account in calculating the future outflow
	}
  \item{return_state}{
	to return the series U, S (storage) and ET (evapotranspiration).
}  \item{return_components}{
    to return the series Xr, Xd and R (reservoir level).
  }
  \item{epsilon}{
    values smaller than this in the output will be set to zero.
  }
}
\details{
	Running Simhyd with routing=NULL will fit the original SimHyd model (i.e. Chiew, 2006)
}
\value{
  the simulated effective rainfall or runoff (in case of using also simhydrouting.sim()), a time series of the same length as
  the input series.}
\references{
Chiew et al 2009 WATER RESOURCES RESEARCH, VOL. 45, W10414, doi:10.1029/2008WR007338, 2009
Chiew (2006) Hydrological Sciences Journal, 51:4, 613-625, DOI: 10.1623/hysj.51.4.613
}
\author{
Willem Vervoort \email{willemvervoort@gmail.com} and Joseph Guillaume \email{josephguillaume@gmail.com}
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 \code{\link{hydromad}(sma = "simhyd", routing = "simhydrouting")} to work
  with models as objects (recommended).
}
\examples{
require(hydromad)

## view default parameter ranges:
str(c(hydromad.getOption("gr4j"),
      hydromad.getOption("gr4jrouting")))

data(Cotter)

# fix some of the parameters
testSH <- hydromad(Cotter[1:1000,],sma="simhyd", routing="simhydrouting",
                   COEFF=200, SQ=1.5, 
                   etmult=0.15, K=0.3)
# show model structure
print(testSH)

# Fit using FitByOptim
testSH.fit <- fitByOptim(testSH,method="PORT",samples=100)
# show fitting result
summary(testSH.fit)
# show fitted model in a standard plot
xyplot(testSH.fit)

# prediction with a fully specified model also works
FitQ <- predict(testSH.fit, return_state = TRUE)
xyplot(window(cbind(Cotter[1:1000,1:2], simhyd = FitQ))) 

# run SimHyd but no routing
testSH_nr <- hydromad(Cotter[1:5000,],sma="simhyd", routing=NULL,
                   COEFF=200, SQ=1.5, 
                   etmult=0.15, K=0.3)
print(testSH_nr)

# Fit without routing
testSH_nr.fit <- fitByOptim(testSH_nr,method="PORT",samples=100)
summary(testSH_nr.fit)
}
\keyword{models}
