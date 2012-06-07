\name{tspot.interp}
\alias{tspot.interp}
\title{Interpret the results of the Oxford Immunotec TSPOT.TB assay for latent tuberculosis infection.
Given vectors of nil, TB antigen (panels A and B), and mitogen results
in spots, this function computes TSPOT qualitative interpretations.
The function uses the Oxford Immunotec North America criterion by default;
alternative criteria sets can be created as methods for the
tspots.criteria function}
\usage{
  tspot.interp(nil, panel_a, panel_b, mito,
    criteria = "oxford.usa", verbosity = "terse", ...)
}
\description{
  Interpret the results of the Oxford Immunotec TSPOT.TB
  assay for latent tuberculosis infection. Given vectors of
  nil, TB antigen (panels A and B), and mitogen results in
  spots, this function computes TSPOT qualitative
  interpretations. The function uses the Oxford Immunotec
  North America criterion by default; alternative criteria
  sets can be created as methods for the tspots.criteria
  function
}

