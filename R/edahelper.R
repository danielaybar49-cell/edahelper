
#' Tests if an object is of class numeric
#'
#' Returns TRUE if x is not of class numeric
#' @param x Object to test
#' @returns Logical `TRUE` or `FALSE`
#' @seealso [is.numeric()] opposite of non.numeric from base code
#' @examples
#' x <- data.frame(var1 = c(1,2,3,4), var2 = c('a','b','c','d'))
#' non.numeric(x$var1)
#' non.numeric(x$var2)
#' @export
non.numeric <- function(x){
  ifelse(is.numeric(x),FALSE,TRUE)
}




##### eda_boxplots ####

#' Create Boxplots for Numeric Variables
#'
#' Generates boxplots for each numeric variable in a data frame.
#' Plots can be created using either ggplot2 or base R graphics.
#' Users can customize plot colors, titles, and other plotting options.
#'
#' @param df A data frame containing the variables to plot.
#' @param subset An optional data frame to use instead of `df`.
#'   Defaults to `NULL`, in which case `df` is used.
#' @param unique_color Logical. If `TRUE`, assigns a different color
#'   to each boxplot. If `FALSE` (default), all boxplots are blue.
#' @param color_list An optional vector of colors to use when
#'   `unique_color = TRUE`. Its length must equal the number of
#'   numeric variables in the data frame. If `NULL`, colors are
#'   generated automatically.
#' @param use_ggplot Logical. If `TRUE` (the default), creates plots
#'   using ggplot2. If `FALSE`, uses base R graphics.
#' @param title An optional character vector of titles for ggplot2
#'   plots. If `NULL`, titles are generated from variable names.
#' @param main An optional character vector of titles for base R
#'   plots. If `NULL`, titles are generated from variable names.
#' @param ylab An optional character vector of y-axis titles for both base R and
#' ggplot objects. If `NULL` y labels are generated from variable names.
#' @param ... Additional arguments passed to `geom_boxplot()` when
#'   using ggplot2 or `boxplot()` when using base R graphics.
#'
#' @return When `use_ggplot = TRUE`, returns a combined patchwork
#'   object containing the boxplots. When `use_ggplot = FALSE`,
#'   displays the boxplots using base R graphics.
#'
#' @seealso [ggplot2::geom_boxplot()], [graphics::boxplot()]
#'
#' @examples
#' # Create boxplots for all numeric variables in iris
#' eda_boxplots(iris)
#'
#' # Create boxplots with different colors
#' eda_boxplots(iris, unique_color = TRUE)
#'
#' # Use base R graphics
#' eda_boxplots(iris, use_ggplot = FALSE)
#'
#' # Plot a single numeric variable using a custom color
#' eda_boxplots(
#'   iris,
#'   subset = iris["Sepal.Width"],
#'   unique_color = TRUE,
#'   color_list = "green",
#'   title = "Sepal Width Boxplot"
#' )
#'
#' @export
eda_boxplots <- function(df,subset=NULL,unique_color = FALSE,color_list = NULL,use_ggplot = TRUE,title=NULL,main=NULL,ylab=NULL,...){
  # Subset Argument
  if(is.null(subset)==FALSE) {
    df <- subset
  }

  # Storing var names and length
  length_num <- sum(sapply(df,is.numeric))
  num_vars <- names(df[sapply(df,is.numeric)])
  if(length_num == 0){
    stop('ERROR! No Numeric Variables')
  }

  # Unique Color Argument
  if(unique_color == TRUE){
    if(is.null(color_list)){
      color_list <- grDevices::rainbow(length_num)
    } else if(length(color_list)!= length_num){
      stop('ERROR! Lenght of Color List Does Not Match Length of Numeric Variables')
    }
  }else{
    color_list <- rep('blue',length_num)
  }

  if(is.null(ylab)){
    ylab = num_vars
  }

  # use_ggplot Argument
  if(use_ggplot==TRUE){
    p <- vector('list',length = length_num)
    if(is.null(title)){
      title = paste("Boxplot of", num_vars)
    }
    for (i in 1:length_num) {
      p[[i]] <- ggplot2::ggplot(df, ggplot2::aes(y = .data[[num_vars[i]]])) +
        ggplot2::geom_boxplot(fill = color_list[i], ...) +
        ggplot2::labs(title = title[i], x = NULL, y = ylab[i]) +
        ggplot2::theme_bw()
    }

    return(patchwork::wrap_plots(p,ncol = ceiling(sqrt(length_num))))
  } else{
    on.exit(graphics::par(mfrow = c(1,1)))
    graphics::par(mfcol = c(
      ceiling(length_num / ceiling(sqrt(length_num))),
      ceiling(sqrt(length_num))), mar = c(3,3,2,1),oma=c(0,0,2,0))

    if(is.null(main)){
      main <- paste("Boxplot of", num_vars)
    }
    for (i in 1:length_num) {
      graphics::boxplot(
        df[[num_vars[i]]],
        col = color_list[i],
        main = main[i],
        ylab = ylab[i],
        ...
      )
    }

  }
}

##### eda_group_boxplots #####


#' Create Boxplots Grouped by Categorical Variables
#'
#' Generates boxplots for a numeric target variable, grouped by
#' one or more non-numeric variables in a data frame. By default,
#' the function creates plots grouped separately by each
#' non-numeric variable.
#'
#' @param df A data frame containing the variables to plot.
#' @param target A character string specifying the numeric variable
#'   to display in the boxplots.
#' @param subset An optional data frame to use instead of `df`.
#'   Defaults to `NULL`, in which case `df` is used.
#' @param group.by An optional character vector specifying the
#'   non-numeric variables used to group the boxplots. If `NULL`,
#'   all non-numeric variables in the data frame are used.
#' @param use_ggplot Logical. If `TRUE` (the default), uses ggplot2.
#'   If `FALSE`, uses base R graphics.
#' @param title An optional character vector of titles for ggplot2
#'   plots. If `NULL`, titles are generated from the target and
#'   grouping variable names.
#' @param main An optional character vector of titles for base R
#'   plots. If `NULL`, titles are generated from the target and
#'   grouping variable names.
#' @param ylab An optional character vector of y-axis titles for both base R and
#'   ggplot objects. If `NULL` y labels are generated from variable names.
#' @param ... Additional arguments passed to `geom_boxplot()` or
#'   `boxplot()`, depending on the plotting method.
#'
#' @return When `use_ggplot = TRUE`, returns a combined patchwork
#'   object containing the plots. When `use_ggplot = FALSE`,
#'   displays the plots using base R graphics.
#'
#' @seealso [ggplot2::geom_boxplot()], [graphics::boxplot()]
#'
#' @examples
#' # Group Sepal Width by Species
#' eda_group_boxplots(
#'   iris,
#'   target = "Sepal.Width",
#'   group.by = "Species"
#' )
#'
#' # Group Sepal Width by Species for a subset of the data
#' eda_group_boxplots(
#'   iris,
#'   target = "Sepal.Width",
#'   subset = subset(iris, Sepal.Length > 5),
#'   group.by = "Species",
#'   use_ggplot = FALSE,
#'   col = "blue"
#' )
#'
#' @export
eda_group_boxplots <- function(df,target,subset=NULL,group.by = NULL,use_ggplot=TRUE,title=NULL,main=NULL,ylab=NULL,...){

  if(is.null(subset)==FALSE) {
    df <- subset
  }
  if(is.null(group.by)){
    length_vars <- sum(sapply(df,non.numeric))
    vars <- names(df[sapply(df,non.numeric)])
  }
  else {
    length_vars <- length(group.by)
    vars <- as.character(group.by)
    for (i in 1:length_vars) {
      if(is.numeric(df[[vars[i]]])){
        stop('ERROR! Group must be non-numeric')
      }else{
        next
      }
    }
  }
  if(length_vars == 0){
    stop('ERROR! No Grouping Variables: Grouping vars must be non-numeric')
  }
  if(is.null(ylab)){
    ylab <- target
  }

  if(use_ggplot == TRUE){
    p <- vector('list',length_vars)
    if(is.null(title)){
      title = paste("Boxplot of", target,'for groups of',vars)
    }
    for (i in 1:length_vars) {
      p[[i]]<- ggplot2::ggplot(df,ggplot2::aes(.data[[target]],color = .data[[vars[i]]]))+
        ggplot2::geom_boxplot(...)+
        ggplot2::labs(title = title, x = NULL, y = ylab) +
        ggplot2::theme_bw()
    }
    return(patchwork::wrap_plots(p,ncol = ceiling(sqrt(length_vars))))
  } else{
    on.exit(graphics::par(mfrow = c(1,1)))
    graphics::par(mfcol = c(
      ceiling(length_vars / ceiling(sqrt(length_vars))),
      ceiling(sqrt(length_vars))
    ))
    if(is.null(main)){
      main <- paste("Boxplot of", target,'For Groups of',vars)
    }
    for (i in seq_along(vars)) {
      graphics::boxplot(
        df[[target]]~df[[vars[i]]],
        main = main[i],
        ylab = ylab,
        xlab = vars[i],
        ...)
    }
  }
}


##### eda_histograms #####

#' Create Histograms for Numeric Variables
#'
#' Generates a histogram for each numeric variable in a data frame.
#' Plots can be created using either ggplot2 or base R graphics,
#' with options for customizing colors, titles, and binning.
#'
#' @param df A data frame containing the variables to plot.
#' @param subset An optional data frame to use instead of `df`.
#'   Defaults to `NULL`, in which case `df` is used.
#' @param unique_color Logical. If `TRUE`, assigns a different color
#'   to each histogram. If `FALSE` (default), all histograms
#'   are blue.
#' @param color_list An optional vector of colors to use when
#'   `unique_color = TRUE`. Its length must equal the number of
#'   numeric variables. If `NULL`, colors are generated automatically.
#' @param use_ggplot Logical. If `TRUE` (default), uses ggplot2.
#'   If `FALSE`, uses base R graphics.
#' @param bins An integer specifying the number of bins for
#'   ggplot2 histograms. Defaults to `25`.
#' @param title An optional character vector of titles for ggplot2
#'   plots. If `NULL`, titles are generated from the variable names.
#' @param main An optional character vector of titles for base R
#'   plots. If `NULL`, titles are generated from the variable names.
#' @param xlab An optional character vector of x-axis titles for both base R and
#'   ggplot objects. If `NULL` x labels are generated from variable names.
#' @param ... Additional arguments passed to `geom_histogram()` or
#'   `hist()`, depending on the plotting method.
#'
#' @return When `use_ggplot = TRUE`, returns a combined patchwork
#'   object containing the histograms. When `use_ggplot = FALSE`,
#'   displays the histograms using base R graphics.
#'
#' @seealso [ggplot2::geom_histogram()], [graphics::hist()]
#'
#' @examples
#' # Create histograms for all numeric variables
#' eda_histograms(iris)
#'
#' # Create histograms with distinct colors
#' eda_histograms(iris, unique_color = TRUE)
#'
#' # Plot a single numeric variable
#' eda_histograms(
#'   iris,
#'   subset = iris["Sepal.Width"],
#'   color_list = "green",
#'   unique_color = TRUE,
#'   title = "Sepal Width Histogram"
#' )
#'
#' @export
eda_histograms <- function(df,subset=NULL,unique_color = FALSE,color_list = NULL,use_ggplot = TRUE,bins = 25,title=NULL,main=NULL,xlab=NULL,...){
  # Subset Argument
  if(is.null(subset)==FALSE) {
    df <- subset
  }

  # Storing var names and #
  length_num <- sum(sapply(df,is.numeric))
  num_vars <- names(df[sapply(df,is.numeric)])
  if(length_num == 0){
    stop('ERROR! No Numeric Variables')
  }

  # Unique Color Argument
  if(unique_color == TRUE){
    if(is.null(color_list)){
      color_list <- grDevices::rainbow(length_num)
    } else if(length(color_list)!= length_num){
      stop('ERROR! Lenght of Color List Does Not Match Length of Numeric Variables')
    }
  }else{
    color_list <- rep('blue',length_num)
  }

  if(is.null(xlab)){
    xlab <- num_vars
  }

  # use_ggplot Argument
  if(use_ggplot==TRUE){
    p <- vector('list',length = length_num)
    if(is.null(title)){
      title = paste("Histogram of", num_vars)
    }
    for (i in 1:length_num) {
      p[[i]] <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[num_vars[i]]])) +
        ggplot2::geom_histogram(fill = color_list[i],bins=bins, ...) +
        ggplot2::labs(title = title[i], x = xlab[i], y = 'Count') +
        ggplot2::theme_bw()
    }

    return(patchwork::wrap_plots(p,ncol = ceiling(sqrt(length_num))))
  }

  # base plot, setting par
  else{
    on.exit(graphics::par(mfrow = c(1,1)))
    graphics::par(mfcol = c(
      ceiling(length_num / ceiling(sqrt(length_num))),
      ceiling(sqrt(length_num))), mar = c(3,3,2,1),oma=c(0,0,2,0))

    # creating the histogram
    if(is.null(main)){
      main <- paste("Histogram of", num_vars)
    }
    for (i in 1:length_num) {
      graphics::hist(
        df[[num_vars[i]]],
        col = color_list[i],
        main = main[i],
        xlab = xlab[i],
        ylab = 'Count',
        bins = bins,
        ...
      )
    }
  }
}

##### eda_barplots #####


#' Create Barplots for Categorical Variables
#'
#' Generates barplots for each non-numeric variable in a data frame.
#' Plots can be created using either ggplot2 or base R graphics.
#' Bar heights can represent counts or proportions.
#'
#' @param df A data frame containing the variables to plot.
#' @param subset An optional data frame to use instead of `df`.
#'   Defaults to `NULL`, in which case `df` is used. Numeric
#'   variables in the selected data frame are excluded from plotting.
#' @param unique_color Logical. If `TRUE`, assigns a different color
#'   to each barplot. If `FALSE` (default), all barplots are blue.
#' @param color_list An optional vector of colors to use when
#'   `unique_color = TRUE`. Its length must equal the number of
#'   non-numeric variables. If `NULL`, colors are generated automatically.
#' @param use_ggplot Logical. If `TRUE` (default), uses ggplot2.
#'   If `FALSE`, uses base R graphics.
#' @param title An optional character vector of titles for ggplot2
#'   plots. If `NULL`, titles are generated from the variable names.
#' @param main An optional character vector of titles for base R
#'   plots. If `NULL`, titles are generated from the variable names.
#' @param stat A character string specifying the bar heights.
#'   Use `"count"` (default) for observation counts or `"prop"`
#'   for proportions. In the base R implementation, proportions
#'   are multiplied by 100 and displayed as percentages.
#' @param xlab An optional character vector of x-axis titles for both base R and
#'   ggplot objects. If `NULL` x labels are generated from variable names.
#' @param ... Additional arguments passed to `geom_bar()` or
#'   `barplot()`, depending on the plotting method.
#'
#' @return When `use_ggplot = TRUE`, returns a combined patchwork
#'   object containing the barplots. When `use_ggplot = FALSE`,
#'   displays the barplots using base R graphics.
#'
#' @seealso [ggplot2::geom_bar()], [graphics::barplot()]
#'
#' @examples
#' # Barplots showing counts
#' eda_barplots(iris)
#'
#' # Barplots with distinct colors
#' eda_barplots(iris, unique_color = TRUE)
#'
#' # Barplots for a filtered data frame using base R graphics
#' eda_barplots(
#'   iris,
#'   subset = subset(iris, Sepal.Length > 5),
#'   use_ggplot = FALSE
#' )
#'
#' # Barplots showing proportions
#' # This example requires the stat = "prop" implementation to be fixed.
#' # eda_barplots(iris, stat = "prop")
#'
#' @export

eda_barplots<- function(df,subset=NULL,unique_color = FALSE,color_list = NULL,use_ggplot = TRUE,title = NULL, main = NULL,stat='count',xlab=NULL,...){
  # Subset Argument
  if(is.null(subset)==FALSE) {
    df <- subset
    if(sum(sapply(df,is.numeric))>0){
      warning('Warning: Numeric Variables From the Subset Will Be Dropped')
    }
  }

  # Storing var names and #
  length_vars <- sum(sapply(df,non.numeric))
  vars <- names(df[sapply(df,non.numeric)])
  if(length_vars == 0){
    stop('ERROR! No Non-Numeric Variables')
  }

  # Unique Color Argument
  if(unique_color == TRUE){
    if(is.null(color_list)){
      color_list <- grDevices::rainbow(length_vars)
    } else if(length(color_list)!= length_vars){
      stop('ERROR! Lenght of Color List Does Not Match Length of Numeric Variables')
    }
  }else{
    color_list <- rep('blue',length_vars)
  }

  if(is.null(xlab)){
    xlab <- vars
  }

  # use_ggplot Argument
  if(use_ggplot==TRUE){
    p <- vector('list',length = length_vars)
    if(is.null(title)){
      title = paste("Boxplot of", vars)
    }
    if(stat=='count'){
      for (i in 1:length_vars) {
        p[[i]] <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[vars[i]]])) +
          ggplot2::geom_bar(fill = color_list[i], ...)+
          ggplot2::labs(title = title[i], x = xlab[i], y = 'Counts')+
          ggplot2::theme_bw()
      }
    } else if(stat=='prop'){
      for (i in 1:length_vars) {
        p[[i]] <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[vars[i]]],y=ggplot2::after_stat(prop),group=1)) +
          ggplot2::geom_bar(fill = color_list[i], ...)+
          ggplot2::scale_y_continuous(labels = scales::percent)+
        ggplot2::labs(title = title[i], x = xlab[i], y = 'Proportion')+
          ggplot2::theme_bw()
      }
    }else{
      stop('ERROR! Input for stat = must be either "count" or "prop"')
    }

    return(patchwork::wrap_plots(p,ncol = ceiling(sqrt(length_vars))))
  }

  # base plot, setting par
  else{
    on.exit(graphics::par(mfrow = c(1,1)))
    graphics::par(mfcol = c(
      ceiling(length_vars / ceiling(sqrt(length_vars))),
      ceiling(sqrt(length_vars))), mar = c(3,3,2,1),oma=c(0,0,2,0))

    # creating the histogram
    if(is.null(main)){
      main <- paste("Barplot of", vars)
    }
    if(stat=='count'){
      for (i in 1:length_vars) {
        graphics::barplot(
          table(df[[vars[i]]]),
          col = color_list[i],
          main = main[i],
          xlab = xlab[i],
          ylab = 'counts',
          ...
        )
      }
    } else if(stat=='prop'){
      for (i in 1:length_vars) {
        graphics::barplot(
          100*prop.table(table(df[[vars[i]]])),
          col = color_list[i],
          main = main[i],
          ylab = 'Proportion %',
          ...
        )
      }
    } else{
      stop('ERROR! Input for stat = must be either "count" or "prop"')
    }
  }
}

##### eda_group_table #####



#' Calculate Summary Statistics by Group
#'
#' Calculates summary statistics for a numeric target variable,
#' grouped by one or more variables in a data frame. By default,
#' the function groups by all non-numeric variables.
#'
#' The summary statistics include the mean, standard deviation,
#' minimum, first quartile, median, third quartile, and maximum.
#'
#' @param df A data frame containing the variables to summarize.
#' @param target A character string specifying the numeric variable
#'   for which summary statistics are calculated.
#' @param group.by An optional character vector specifying the
#'   grouping variables. If `NULL`, all non-numeric variables
#'   in `df` are used.
#' @param subset An optional data frame to use instead of `df`.
#'   Defaults to `NULL`, in which case `df` is used.
#' @param ... Additional arguments. Currently, these arguments
#'   are not used by the function.
#'
#' @return A data frame containing one row for each group and
#'   columns containing the requested summary statistics for `target`.
#'
#' @seealso [dplyr::group_by()], [dplyr::summarise()]
#'
#' @examples
#' # Summarize Sepal Width by Species
#' eda_group_table(
#'   iris,
#'   target = "Sepal.Width",
#'   group.by = "Species"
#' )
#'
#' # Summarize Sepal Width for observations with Sepal Length > 5
#' eda_group_table(
#'   iris,
#'   target = "Sepal.Width",
#'   subset = subset(iris, Sepal.Length > 5),
#'   group.by = "Species"
#' )
#'
#' @export

eda_group_table <- function(df,target,group.by = NULL,subset=NULL,...){
  if(is.null(subset)==FALSE) {
    df <- subset
  }
  if(is.null(group.by)){
    vars <- names(df[sapply(df,non.numeric)])
  } else{
    vars <- group.by
    if(sum(sapply(df[[vars]],is.numeric))>0){
      warning('group.by contains at least 1 numeric variable. Converting to factor or character may yield better results')
    }
  }
  stats <- list(
    Mean = ~ mean(.x, na.rm=TRUE),
    SD = ~ sd(.x, na.rm=TRUE),
    Min = ~ min(.x, na.rm=TRUE),
    Q1 = ~ quantile(.x, 0.25, na.rm=TRUE),
    Median = ~ median(.x, na.rm=TRUE),
    Q3 = ~ quantile(.x, 0.75, na.rm=TRUE),
    Max = ~ max(.x, na.rm=TRUE)
  )

  df_summary<- df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(vars))) |>
    dplyr::summarise(
      dplyr::across(dplyr::all_of(target),stats, .names = "{.fn} { .col}")
    )

  return(df_summary)
}

###### eda_scatterplots #####



#' Create Scatterplots for Numeric Variables
#'
#' Generates scatterplots comparing a specified numeric target
#' variable with every other numeric variable in a data frame.
#' Plots can be created using either ggplot2 or base R graphics,
#' with options for customizing colors and titles.
#'
#' @param df A data frame containing the variables to plot.
#' @param target A character string specifying the numeric target
#'   variable to plot on the y-axis.
#' @param subset An optional data frame to use instead of `df`.
#'   Defaults to `NULL`, in which case `df` is used.
#' @param unique_color Logical. If `TRUE`, assigns a different color
#'   to each scatterplot. If `FALSE` (the default), all plots are blue.
#' @param color_list An optional vector of colors to use when
#'   `unique_color = TRUE`. Its length must equal the number of
#'   other numeric variables plotted. If `NULL`, colors are generated
#'   automatically.
#' @param use_ggplot Logical. If `TRUE` (the default), uses ggplot2.
#'   If `FALSE`, uses base R graphics.
#' @param title An optional character vector of titles for ggplot2
#'   plots. If `NULL`, titles are generated from the target and
#'   comparison variable names.
#' @param main An optional character vector of titles for base R
#'   plots. If `NULL`, titles are generated from the target and
#'   comparison variable names.
#' @param ylab An optional character vector of y-axis titles for both base R and
#'   ggplot objects. If `NULL` y labels are generated from target variable names.
#' @param xlab An optional character vector of x-axis titles for both base R and
#'   ggplot objects. If `NULL` x labels are generated from variable names.
#' @param ... Additional arguments passed to `geom_point()` or
#'   `plot()`, depending on the plotting method.
#'
#' @return When `use_ggplot = TRUE`, returns a combined patchwork
#'   object containing the scatterplots. When `use_ggplot = FALSE`,
#'   displays the plots using base R graphics.
#'
#' @seealso [ggplot2::geom_point()], [graphics::plot()]
#'
#' @examples
#' # Compare Sepal Width with all other numeric variables
#' eda_scatterplots(iris, target = "Sepal.Width")
#'
#' # Create scatterplots with distinct colors
#' eda_scatterplots(
#'   iris,
#'   target = "Sepal.Width",
#'   unique_color = TRUE
#' )
#'
#' # Compare Sepal Width with Petal Width only
#' eda_scatterplots(
#'   iris,
#'   target = "Sepal.Width",
#'   subset = iris[, c("Sepal.Width", "Petal.Width")],
#'   color_list = "green",
#'   unique_color = TRUE,
#'   title = "Sepal Width vs Petal Width"
#' )
#'
#' @export

eda_scatterplots <- function(df,target,subset=NULL,unique_color = FALSE,color_list = NULL,use_ggplot = TRUE,title=NULL,main=NULL,ylab=NULL,xlab=NULL,...){
  # Subset Argument
  if(is.null(subset)==FALSE) {
    df <- subset
  }

  # Storing var names and length
  num_vars <- names(df[sapply(df,is.numeric)])
  num_vars <- num_vars[num_vars != target]
  length_num <- length(num_vars)
  if(length_num == 0){
    stop('ERROR! No Numeric Variables')
  }

  # Unique Color Argument
  if(unique_color == TRUE){
    if(is.null(color_list)){
      color_list <- grDevices::rainbow(length_num)
    } else if(length(color_list)!= length_num){
      stop('ERROR! Lenght of Color List Does Not Match Length of Numeric Variables')
    }
  }else{
    color_list <- rep('blue',length_num)
  }

  if(is.null(xlab)){
    xlab <- num_vars
  }
  if(is.null(ylab)){
    ylab <- target
  }

  # use_ggplot Argument
  if(use_ggplot==TRUE){
    p <- vector('list',length = length_num)
    if(is.null(title)){
      title = paste(target,"vs", num_vars)
    }
    for (i in 1:length_num) {
      p[[i]] <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[num_vars[i]]], y = .data[[target]])) +
        ggplot2::geom_point(color = color_list[i], ...) +
        ggplot2::labs(title = title[i], x = xlab[i], y = ylab) +
        ggplot2::theme_bw()
    }

    return(patchwork::wrap_plots(p,ncol = ceiling(sqrt(length_num))))
  } else{
    on.exit(graphics::par(mfrow = c(1,1)))
    graphics::par(mfcol = c(
      ceiling(sqrt(length_num)),
      ceiling(sqrt(length_num))), mar = c(3,3,2,1),oma=c(0,0,2,0))
    if(is.null(main)){
      main <- paste(target,"vs", num_vars)
    }
    for (i in 1:length_num) {
      plot(
        df[[num_vars[i]]],df[[target]],
        col = color_list[i],
        main = main[i],
        xlab = xlab[i],
        ylab = ylab,
        ...
      )
    }
  }
}



