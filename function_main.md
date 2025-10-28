Assignment B1 Function
================
Mark Liu
2025-10-28

## Preamble

This R mark down document will include the worked solution to all
exercises required by STAT545B’s assignment B1. The solutions will be
presented in order of appearance in the assignment document.

## Exercise 1: Make a Function

For the purpose of this exercise, we will create a function that, once
given a data-frame and a selection of columns, returns a summary that
details the number of unique values in the aforementioned columns.

``` r
#' @title Count Unique Observations
#'
#' @description Given a data-frame, the function returns another data-frame that include the number of unique values in the input data-frame's columns. The user may specify which ones of the data-frame's columns should have its number of unique values counted.
#'
#' @param df a data-frame. It is named such as df is its standard shorthand in R
#' @param col_names a vector. This input is a vector of column names in df. When given, the function will count the number of unique values only in the mentioned columns. Give a value is not given, the function will count the number of unique values for all columns of df. This parameter is named such as "col" is a short hand for columns, and the term "names".
#' @param ignore_na a boolean. If specified as TRUE, the function will treat NA as if it is a unique value. It is named such because when one ignore NAs (i.e. pretend they don't exist), they are not considered a unique value. By default, this is false.
#' @param ... additional instructions for the unique() base-R function. Should there be a need, the user may override the default parameters for unique(). This argument is unnamed.
#' 
#' @return a data-frame of 1 row. This data-frame contain the number of unique values in columns mentioned in col_names from df. Column names used in the input data-frame is preserved in the output.

unique_observations <- function(df, col_names = everything(), ignore_na = FALSE,...){
  
  #When ignore_na is FALSE, summarize directly
  if(ignore_na == FALSE){
    ans <- df %>% #piping the input data-frame
    summarize(across(all_of(col_names), #produce a summary over specified columns
                     ~ length(unique(.x,...)))) #counting the unique values
  return(ans) #returning the summary
  }
  
  #When ignore_na is TRUE, apply unique operation after deleting the NAs
  if(ignore_na == TRUE){
    ans <- df %>% #piping the input data-frame
    summarize(across(all_of(col_names), #produce a summary over specified columns
                     ~ length(unique(na.omit(.x),...)))) 
    #counting the unique values after omitting the NAs
  return(ans)
  }
  
  #If ignore_na is either TRUE nor FALSE
  stop("ignore_na has to be a boolean value (TRUE/FALSE)")
}
```

## The Examples

Having completed the function, let us go through a few examples.

### Example 1: Esophegeal Cancer dataset

In this example, the function will directly produce a summary of the
number of unique values in each column of the built-in esoph data set,
treating any instances of NA as a unique value, should they occur.

``` r
unique_observations(esoph)
```

    ##   agegp alcgp tobgp ncases ncontrols
    ## 1     6     4     4     10        27

### Example 2: Mtcars dataset

In this example, the function will directly produce a summary of the
number of unique values in the “cyl” and “gear” columns of the built-in
mtcars data set, treating any instances of NA as a unique value, should
they occur.

``` r
unique_observations(mtcars, c("cyl", "gear"))
```

    ##   cyl gear
    ## 1   3    3

### Example 3: Penguins dataset

In this example, the function will directly produce a summary of the
number of unique values in the “island”, “sex”, and “species” columns of
the penguins data set from the palmerpenguins package, treating any
instances of NA as a unique value, should they occur.

``` r
unique_observations(penguins, c("island", "species", "sex"))
```

    ## # A tibble: 1 × 3
    ##   island species   sex
    ##    <int>   <int> <int>
    ## 1      3       3     3

### Example 4: Penguins dataset

This example is identical to the example above, except that NAs are no
longer counted as a unique value.

``` r
unique_observations(penguins, c("island", "species", "sex"), ignore_na = TRUE)
```

    ## # A tibble: 1 × 3
    ##   island species   sex
    ##    <int>   <int> <int>
    ## 1      3       3     2

### Example 5: col_names not contained in df

In this example, we will attempt to get the number of unique values for
the “World” column from the Iris dataset. Note that the dataset does not
contain a column named “World”. As such, an error was thrown
automatically by R.

``` r
unique_observations(iris, "world")
```

    ## Error in `summarize()`:
    ## ℹ In argument: `across(all_of(col_names), ~length(unique(.x, ...)))`.
    ## Caused by error in `across()`:
    ## ℹ In argument: `all_of(col_names)`.
    ## Caused by error in `all_of()`:
    ## ! Can't subset elements that don't exist.
    ## ✖ Element `world` doesn't exist.

### Example 6: ignore_na is not boolean

In this example, we will repeat example 2 by instead state that
ignore_na = 10. As 10 is not a boolean, the function will throw a custom
error which states just that.

``` r
unique_observations(mtcars, c("cyl", "gear"), ignore_na = 10)
```

    ## Error in unique_observations(mtcars, c("cyl", "gear"), ignore_na = 10): ignore_na has to be a boolean value (TRUE/FALSE)

## The Tests

### The setup

Having went through the several examples above, let us run a few tests
with regards to this function. For the purpose of this test, we will
create a 3x5 data-frame with three columns, named “A”, “B”, and “C”.
They content are as follows:

- “A” contains 4 instances of the number 1 alongside 1 NA.
- “B” contains 3 instances of the string “cat” and 2 instances of the
  string “human”
- “C” contains the numbers 1,2,3,4,5

``` r
A <- c(rep(1,4), NA)
```

### Test 1 and solution

### Test 2 and solution

### Test 3 and solution

### Running the tests

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.
