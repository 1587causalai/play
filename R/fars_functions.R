# Declare global variables to get rid of the Travis R CMD check NOTEs
globalVariables(c("MONTH","n","STATE","year"))

#' Reads a delimited data file
#'
#' \code{fars_read} reads a delimited csv file and
#' convert the result to a data frame.
#'
#' @param filename A character string prividing the name of the file or
#'    the path to a file.
#'
#' @return If the file is found then the output will be a data frame object
#'   containing the content of the file. If the file does not exist an
#'   error message will be printed stating that the file does not exist.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @examples
#' \dontrun{fars_read('accident_2013.csv.bz2')}
#' \dontrun{fars_read('/var/tmp/accident_2014.csv.bz2')}
#' 
fars_read <- function(filename) {
        file <- system.file(paste0("extdata/",filename), package="fars")
        if(!file.exists(file))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(file, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' Creates a file name
#'
#' \code{make_filename} generates the name of an input data file
#'   based on the \code{year} argument provided in a form
#'   "accident_<year>.csv.bz2"
#'
#' @param year The input value for the year
#'
#' @return This function returns a string in the form
#'  "accident_<year>.csv.bz2" that can be used as a file
#'  name that contains the data for that particular \code{year}
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Reads month and year values from accident files
#'
#' \code{fars_read_years} reads multiple yearly data files
#'   and returns a list of tibbles with MONTH and year
#'   columns from the data in "accident_<year>.csv.bz2"
#'   files
#'
#'
#' @param years vector of years to get data for
#'
#' @return list of tibbles, one per year, with two columns
#'   MONTH and year. If any of the years from the input vector
#'   does not corresponds to an existing data file for that
#'   particular year, NULL and \code{invalid year} warning message
#'   will be returned
#'
#' @importFrom dplyr mutate select
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{fars_read_years('2015')}
#' \dontrun{fars_read_years(c('2013','2014'))}
fars_read_years <- function(years) {
        lapply(years, function(year) {
                file <- make_filename(year)
                tryCatch({
                        dat <- fars_read(file)
                        dplyr::mutate(dat, year = year) %>%
                                dplyr::select(MONTH, year)
                }, error = function(e) {
                        warning("invalid year: ", year)
                        return(NULL)
                })
        })
}

#' Summarizes the number of accidents/month for each year
#'
#' \code{fars_summarize_years} diplays a summary of the number of US
#'   accidents per month for each year
#'
#' @param years vector of years to get accidents/month summary data for
#'
#' @return a matrix containing 12 rows (for each month) and one column
#'   for each year with the total number of accidents for that
#'   particular month in that particular year. A warning is returned
#'   for each year that does not have a corresponding "accident_<year>.csv.bz2"
#'   file.
#'
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr spread
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{fars_summarize_years(2015)}
#' \dontrun{fars_summarize_years(c(2013,2014))}
#'
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' Plots a state map of accidents for a specific year
#'
#' \code{fars_map_state} Plots a state map with all the accident
#'   locations that had happened during the input year. All
#'   longitudinal and latitudinal values are filtered to make sure
#'   they fall within standard intervals.
#'
#' @param state.num integer representing the state number for which
#'   the accident map will be generated
#' @param year input value of the year for which we'll plot the
#'   accidents for the selected state
#'
#' @return A plot of all accident locations rendered on the map of
#'   the input state. A \code{invalid STATE number} message will be
#'   displayed if \code{state.num} input value is not found in the
#'   input file. If no accidents happened in that state during that
#'   calendar year, a \code{no accidents to plot} will be displayed
#'   instead of the plot.
#'
#' @importFrom dplyr %>% filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @examples
#' \dontrun{fars_map_state(1,2014)}
#' \dontrun{fars_map_state(3,2015)}
#'
#' @export
fars_map_state <- function(state.num, year) {
        filename <- make_filename(year)
        data <- fars_read(filename)
        state.num <- as.integer(state.num)

        if(!(state.num %in% unique(data$STATE)))
                stop("invalid STATE number: ", state.num)
        data.sub <- dplyr::filter(data, STATE == state.num)
        if(nrow(data.sub) == 0L) {
                message("no accidents to plot")
                return(invisible(NULL))
        }
        is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
        is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
        with(data.sub, {
                maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
                          xlim = range(LONGITUD, na.rm = TRUE))
                graphics::points(LONGITUD, LATITUDE, pch = 46)
        })
}
