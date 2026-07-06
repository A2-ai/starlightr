% File src/library/base/man/options.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2026 R Core Team
% Distributed under GPL 2 or later
% Pulled from https://github.com/wch/r-source/blob/393ace9767744ae26c938cb56ac1cf4867ce64fa/src/library/base/man/options.Rd
% This is an excerpt, not the full file: \name/\alias/\title/\description/\usage
% are copied verbatim from lines 6-29, and the \tabular block demonstrating the
% factory-default settings is copied verbatim from lines 519-541 (originally
% the tail end of a much longer \section{Options used in base R}, whose ~470
% lines of per-option \arguments-style documentation are omitted here). The
% \section{Options set in package \pkg{grDevices}} header is kept to show a
% second \pkg usage, with its \describe body (unrelated to this excerpt)
% omitted.
\name{options}
\alias{options}
\alias{.Options}
\alias{getOption}

\title{Options Settings}
\description{
  Allow the user to set and examine a variety of global \emph{options}
  which affect the way in which \R computes and displays its results.
}
\usage{
options(\dots)

getOption(x, default = NULL)

.Options
}
\section{Options used in base \R}{
  The \sQuote{factory-fresh} default settings of some of these options are (alphabetically)
  \tabular{ll}{
    \code{add.smooth} \tab \code{TRUE}\cr
    \code{check.bounds} \tab \code{FALSE}\cr
    \code{continue} \tab \code{"+ "}\cr
    \code{digits} \tab \code{7}\cr
    \code{echo} \tab \code{TRUE}\cr
    \code{encoding} \tab \code{"native.enc"}\cr
    \code{error} \tab \code{NULL}\cr
    \code{expressions} \tab \code{5000}\cr
    \code{keep.source} \tab \code{interactive()}\cr
    \code{keep.source.pkgs} \tab \code{FALSE}\cr
    \code{max.print} \tab \code{99999}\cr
    \code{OutDec} \tab \code{"."}\cr
    \code{prompt} \tab \code{"> "}\cr
    \code{quiet}   \tab \code{FALSE}\cr
    \code{scipen} \tab \code{0} \cr
    \code{show.error.messages} \tab \code{TRUE}\cr
    \code{timeout} \tab \code{60}\cr
    \code{verbose} \tab \code{FALSE}\cr
    \code{warn} \tab \code{0}\cr
    \code{warning.length} \tab \code{1000}\cr
    \code{width}  \tab \code{80}\cr
  }
  Others are set from environment variables or are platform-dependent.
}

\section{Options set in package \pkg{grDevices}}{
  These will be set when package \pkg{grDevices} (or its namespace)
  is loaded if not already set.
}
\keyword{utilities}
