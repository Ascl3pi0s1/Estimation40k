\name{jet_de_bless}
\alias{jet_de_bless}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Jet de blessure de l'attaque simuler
%%  ~~function to do ... ~~
}
\description{Jet de blessure de l'attaque simuler
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
jet_de_bless(JetTouch, For, End, rrBless = 0, mod_Bless = 0, Transu = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{JetTouch}{
%%     ~~Describe \code{JetTouch} here~~
}
  \item{For}{
%%     ~~Describe \code{For} here~~
}
  \item{End}{
%%     ~~Describe \code{End} here~~
}
  \item{rrBless}{
%%     ~~Describe \code{rrBless} here~~
}
  \item{mod_Bless}{
%%     ~~Describe \code{mod_Bless} here~~
}
  \item{Transu}{
%%     ~~Describe \code{Transu} here~~
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
function (JetTouch, For, End, rrBless = 0, mod_Bless = 0, Transu = 0) 
{
    touche = JetTouch[[1]]
    x = sample(1:6, size = touche, replace = TRUE)
    B = max(min(6, max(Bless_rate(For, End) - mod_Bless, 2)), 
        Transu)
    as = sum(x == 1)
    Bless = sum(x > B - 1)
    miss = sum(x < B)
    if (rrBless == 1) {
        x = sample(1:6, size = as, replace = TRUE)
        Bless = Bless + sum(x > B - 1)
    }
    if (rrBless == 2) {
        x = sample(1:6, size = miss, replace = TRUE)
        Bless = Bless + sum(x > B - 1)
    }
    as = sum(x == 1)
    miss = sum(x < B)
    Bless + JetTouch[[2]]
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
