\name{make_map_background}
\alias{make_map_background}
\title{Creates a map background}
\usage{
make_map_background(selected_map_region,
                    geom_sf_color = "grey66",
                    geom_sf_fill = "#009EFF",
                    label_size = 5,
                    alpha = .2,
                    axis_text_size = 12,
                    axis_color = "grey",
                    axis_line_size = .5,
                    title_text_size = 16,
                    panel_color = "grey",
                    panel_fill = "white",
                    legend_title_size = 12,
                    legend_text_size = 12,
                    ...
                    )
}
\arguments{
\item{selected_map_region}{is required to provide selected regions for the map background.

The current version accepts any of the region codes in the FAO Major Fishing Area 21

(i.e., the Northwest Atlantic). The region codes include

c("0A", "0B", "1A", "1B", "1C", "1D", "1E", "1F", "2G", "2H", "2J", "3K", "3L",

"3M", "3N", "3O", "3Pn", "3Ps", "4R", "4S", "4T", "4Vn", "4Vs", "4W", "4X", "5Y",

"5Ze", "5Zw", "6A", "6B", "6C", "6D", "6E", "6F", "6G", "6H")
}
\item{geom_sf_color}{is assigned to be "grey66" by default, and alterable}

\item{geom_sf_fill}{is assigned to be "#009EFF" by default, and alterable}

\item{label_size}{is assigned to be 5 by default, and alterable}

\item{alpha}{is assigned to be 0.2 by default, and alterable}

\item{axis_text_size}{is assigned to be 12 by default, and alterable}

\item{axis_color}{is assigned to be ""grey" by default, and alterable}

\item{axis_line_size}{is assigned to be 0.5 by default, and alterable}

\item{title_text_size}{is assigned to be 16 by default, and alterable}

\item{panel_color}{is assigned to be "grey" by default, and alterable}

\item{panel_fill}{is assigned to be "white" by default, and alterable}

\item{legend_title_size}{is assigned to be 12 by default, and alterable}

\item{legend_text_size}{is assigned to be 12 by default, and alterable}
}
\value{
Returns a map background object
}
\description{
Creates a map background.
}
\examples{
make_map_background(selected_map_region = c("3K", "3L", "3N", "3M", "3O", "3Ps", "3Pn"))
}
