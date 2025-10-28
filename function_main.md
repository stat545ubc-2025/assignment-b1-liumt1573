Assignment B1 Function
================
Mark Liu
2025-10-28

``` r
#Loading the required packages
library(testthat)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following object is masked from 'package:testthat':
    ## 
    ##     matches

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(palmerpenguins)
```

    ## 
    ## Attaching package: 'palmerpenguins'

    ## The following objects are masked from 'package:datasets':
    ## 
    ##     penguins, penguins_raw

## Preamble

This R mark down document will include the worked solution to all
exercises required by STAT545B’s assignment B1. The solutions will be
presented in order of appearance in the assignment document.

## Make a Function with Documentation

For the purpose of this exercise, we will create a function that, once
given a data-frame and a selection of columns, returns a summary that
details the number of unique values in the aforementioned columns. Some
of the descriptions for individual inputs can be rather length, please
do scroll the box below side-ways to review it.

``` r
#' @title Count Unique Observations
#'
#' @description Given a data-frame, the function returns another data-frame that include the number of unique values in the input data-frame's columns. The user may specify which ones of the data-frame's columns should have its number of unique values counted.
#'
#' @param df a data-frame (or tibble). It is named such as df is its standard shorthand in R
#' @param col_names a vector. This input is a vector of column names appearing in df. When given, the function will count the number of unique values only in the mentioned columns. Give a value is not given, the function will count the number of unique values for all columns of df. This parameter is named so because "col" is a short hand for columns, and the term "names" is suggestive of what this parameter is looking for.
#' @param ignore_na a boolean. If specified as FALSE, the function will treat NA as if it is a unique value. It is named such because when one ignore NAs (i.e. pretend they don't exist), they are not considered a unique value. By default, this is false.
#' @param ... additional instructions for the unique() base-R function. Should there be a need, the user may override the default parameters for unique(). This argument is unnamed.
#' 
#' @return a data-frame of 1 row. This data-frame contain the number of unique values in columns mentioned in col_names from df. Column names used in the input data-frame is preserved in the output.

count_unique_observations <- function(df, col_names = everything(), ignore_na = FALSE,...){
  
  #When ignore_na is FALSE, summarize directly
  if(ignore_na == FALSE){
    ans <- df %>% #piping the input data-frame
    summarize(across(all_of(col_names), #produce a summary over specified columns
                     ~ length(unique(.x,...)))) #counting the unique values
  return(as.data.frame(ans)) #returning the summary
  }
  
  #When ignore_na is TRUE, apply unique operation after deleting the NAs
  if(ignore_na == TRUE){
    ans <- df %>% #piping the input data-frame
    summarize(across(all_of(col_names), #produce a summary over specified columns
                     ~ length(unique(na.omit(.x),...)))) 
    #counting the unique values after omitting the NAs
  return(as.data.frame(ans))
  }
  
  #If ignore_na is neither TRUE nor FALSE, throw a custom error
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
count_unique_observations(esoph)
```

    ##   agegp alcgp tobgp ncases ncontrols
    ## 1     6     4     4     10        27

### Example 2: Mtcars dataset

In this example, the function will directly produce a summary of the
number of unique values in the “cyl” and “gear” columns of the built-in
mtcars data set, treating any instances of NA as a unique value, should
they occur. The cars in questions have either 4, 6, or 8 cylinder
engines, and they also have 3, 4, or 5 gears.

As a result, we expect 3 unique values for cylinder and 3 for gear.

There are no NAs for these columns

``` r
count_unique_observations(mtcars, c("cyl", "gear"))
```

    ##   cyl gear
    ## 1   3    3

### Example 3: Penguins dataset

In this example, the function will directly produce a summary of the
number of unique values in the “island”, “sex”, and “species” columns of
the penguins data set from the palmerpenguins package, treating any
instances of NA as a unique value, should they occur.

In this dataset, data has been collected from 3 different islands for 3
different species. There are no NAs for these two columns.

The sex of a penguin may be either male or female. There are also some
observations with missing value for sex, so we also have 3 unique values
for this column.

``` r
count_unique_observations(penguins, c("island", "species", "sex"))
```

    ##   island species sex
    ## 1      3       3   3

### Example 4: Penguins dataset

This example is identical to the example above, except that NAs are no
longer counted as a unique value.

In this case, we expect only 2 unique values for sex, that being male
and female.

``` r
count_unique_observations(penguins, c("island", "species", "sex"), ignore_na = TRUE)
```

    ##   island species sex
    ## 1      3       3   2

### Example 5: col_names not contained in df

In this example, we will attempt to get the number of unique values for
the “World” column from the Iris dataset. Note that the dataset does not
contain a column named “World”. As such, an error was thrown
automatically by R.

``` r
count_unique_observations(iris, "world")
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
count_unique_observations(mtcars, c("cyl", "gear"), ignore_na = 10)
```

    ## Error in count_unique_observations(mtcars, c("cyl", "gear"), ignore_na = 10): ignore_na has to be a boolean value (TRUE/FALSE)

## The Tests

### The setup

Having went through the several examples above, let us run a few tests
with regards to this function. For the purpose of this test, we will
create a 5 x 4 data-frame with four columns, named “A”, “B”, “C”, and
“D”. Their content are as follows:

- “A” contains 4 instances of the number 1 alongside 1 NA.
- “B” contains 3 instances of the string “cat” and 2 instances of the
  string “human”
- “C” contains the numbers 1,2,3,4,5
- “D” contains 4 instances of the number 7 and 1 instance of the number
  8

``` r
#creating the columns
A <- c(rep(1,4), NA)
B <- c(rep("cat", 3), rep("human", 2))
C <- c(seq(1:5))
D <- c(rep(7,4), 8)

#creating the data-frame
test_data_frame <- as.data.frame(cbind(A,B,C,D))

#Since cbind changed the object type for vectors A and C into strings, they will be re-encoded as numbers
test_data_frame$A <- as.numeric(test_data_frame$A)
test_data_frame$C <- as.numeric(test_data_frame$C)
test_data_frame$D <- as.numeric(test_data_frame$D)

#Here is the data-frame to be used for testing
test_data_frame
```

    ##    A     B C D
    ## 1  1   cat 1 7
    ## 2  1   cat 2 7
    ## 3  1   cat 3 7
    ## 4  1 human 4 7
    ## 5 NA human 5 8

### Test 1 (column with numerical data, no NA)

In the first test, we will try to get the number of unique values within
the two columns, “C” and “D” that has numerical data and does not have
any missing values. In theory, the code
“count_unique_observations(test_data_frame, c(”C”, “D”))” should produce
the desired output.

Clearly, in this set up, there are 5 unique values in “C”, and 2 in “D”.

Before running the test, let us first write down the expected outcome:

``` r
#number of unique values in C, D, respectively
C = 5
D = 2
test_1_sol <- as.data.frame(cbind(C,D))

test_1_sol
```

    ##   C D
    ## 1 5 2

### Test 2.1 (a column with NA)

In the second test, we will try to get the number of unique values in
column “A”, ignoring NAs for now, and not counting them as a unique
value.

In theory, the code “count_unique_observations(test_data_frame,”A”,
TRUE)” should produce the desired output.

Clearly, in this set up, the function should tell us that there is 1
unique value in “A”, returning the data-frame below:

``` r
#number of unique values in A
A = 1
test_2.1_sol <- as.data.frame(cbind(A))

test_2.1_sol
```

    ##   A
    ## 1 1

### Test 2.2 (a column with NA)

Conversely, if we used “count_unique_observations(test_data_frame,”A”,
FALSE)“, telling the function to NOT ignore the missing values, we
should expect it to identify the column”A” has having 2 unique values.

In this case, the function ought to return a slightly different
data-frame:

``` r
#number of unique values in A
A = 2
test_2.2_sol <- as.data.frame(cbind(A))

test_2.2_sol
```

    ##   A
    ## 1 2

### Test 3 (Columns with string)

In the third test, we will try to get the number of unique values in
column B, which contains strings as opposed to numbers, as seen in the
cases above. In theory, the code
“count_unique_observations(test_data_frame,”B”)” should produce the
desired output.

It is evident that there are 2 unique values in “B”

Again, we will first write down expected output:

``` r
#number of unique values in B
B = 2
test_3_sol <- as.data.frame(cbind(B))

test_3_sol
```

    ##   B
    ## 1 2

### Running the tests

We will now run the aforementioned tests.

``` r
test_that("Can handle numerical columns, no NAs", {
  expect_equal(count_unique_observations(test_data_frame, c("C", "D")), test_1_sol)
})
```

    ## Test passed 😀

``` r
test_that("Ignore_NA works correctly pt 1", {
  expect_equal(count_unique_observations(test_data_frame, "A", ignore_na = TRUE), test_2.1_sol)
})
```

    ## Test passed 🥳

``` r
test_that("Ignore_NA works correctly pt 2", {
  expect_equal(count_unique_observations(test_data_frame, "A", ignore_na = FALSE), test_2.2_sol)
})
```

    ## Test passed 🎊

``` r
test_that("Can handle columns with strings", {
  expect_equal(count_unique_observations(test_data_frame, "B"), test_3_sol)
})
```

    ## Test passed 🥇

Fortunately, the function managed to pass all of the required tests!
