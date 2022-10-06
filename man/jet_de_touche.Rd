\name{jet_de_touche}
\alias{jet_de_touche}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{jet de touche de l'attaque
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
jet_de_touche(Nb_attack, CT, rrTouch = 0, mod_Touch = 0, Bless_auto = 7)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Nb_attack}{
%%     ~~Describe \code{Nb_attack} here~~
}
  \item{CT}{
%%     ~~Describe \code{CT} here~~
}
  \item{rrTouch}{
%%     ~~Describe \code{rrTouch} here~~
}
  \item{mod_Touch}{
%%     ~~Describe \code{mod_Touch} here~~
}
  \item{Bless_auto}{
%%     ~~Describe \code{Bless_auto} here~~
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
function (Nb_attack, CT, rrTouch = 0, mod_Touch = 0, Bless_auto = 7) 
{
    x = sample(1:6, size = Nb_attack, replace = TRUE)
    as = sum(x == 1)
    CT = min(6, max(CT - mod_Touch, 2))
    touch = sum(x > CT - 1)
    miss = sum(x < CT)
    six = sum(x >= Bless_auto)
    if (rrTouch == 1) {
        x = sample(1:6, size = as, replace = TRUE)
        touch = touch + sum(x > CT - 1)
        six = six + sum(x >= Bless_auto)
    }
    if (rrTouch == 2) {
        x = sample(1:6, size = miss, replace = TRUE)
        touch = touch + sum(x > CT - 1)
        six = six + sum(x >= Bless_auto)
    }
    as = sum(x == 1)
    miss = sum(x < CT)
    list(touch = touch, bless_auto = six)
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
