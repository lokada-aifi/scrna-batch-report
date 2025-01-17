% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rmarkdown_formatting.R
\name{make_subchunk}
\alias{make_subchunk}
\title{Knit a plot as a subchunk}
\usage{
make_subchunk(
  g,
  subchunk_name,
  quiet_knit = TRUE,
  chunk_opt_list = list(fig.height = 7, fig.width = 5, warning = TRUE)
)
}
\arguments{
\item{g}{The plot object}

\item{subchunk_name}{Character value. Unique name of Rmarkdown subchunk to be generated.}

\item{quiet_knit}{Logical value, default TRUE. Passed to \code{knitr::knit()}, should the subchunk
be knit "quietly" (no progress bar or messages)}

\item{chunk_opt_list}{Named list of chunk options for the subchunk. Can take any chunk
options available to a normal chunk.}
}
\description{
Knit a plot as a subchunk so its dimensions can be specified individually. Based
on code found here: http://michaeljw.com/blog/post/subchunkify/
}
\details{
Allows individual plots within the same chunk to be knit as subchunks with
unique chunk options. Plots can be output in a loop with each plot using
different dimensions, ie dynamic dimensions based on number of x and/or y
category levels. Parent chunk should have chunk options 'results = "as-is"'
to ensure proper output. Note that this will create a "figures" directory in
the directory containing the Rmarkdown file containing the chunk plots. Ensure
that Rmarkdown  yaml has 'self_contained: true' in html document section (or equivalent)
}
\examples{
\dontrun{
# This will generate a file in 'figures' subdirectory of working directory
library(ggplot2)
g_example <- ggplot(data.frame(x=1:10, y = 1:10), aes(x, y)) + geom_point()
chunk_opt_l <- list(fig.height=10, fig.width=12, warning=TRUE)
make_subchunk(g_example, "test_chunk", chunk_opt_list = chunk_opt_l)
}

}
