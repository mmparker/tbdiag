\name{qft.interp}
\alias{qft.interp}
\title{Interpret the results of the Cellestis Quantiferon Gold In-Tube assay for latent tuberculosis infection.
Given vectors of nil, TB antigen, and mitogen results in IU/mL,
this function computes QFT qualitative interpretations.  The function
uses the Cellestis USA criterion by default; other criteria
sets can be created as methods for the qft.criteria function.}
\usage{
  qft.interp(nil, tb, mito, criteria = "cellestis.usa",
    verbosity = "verbose", ...)
}
\description{
  Interpret the results of the Cellestis Quantiferon Gold
  In-Tube assay for latent tuberculosis infection. Given
  vectors of nil, TB antigen, and mitogen results in IU/mL,
  this function computes QFT qualitative interpretations.
  The function uses the Cellestis USA criterion by default;
  other criteria sets can be created as methods for the
  qft.criteria function.
}

