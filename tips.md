‘Advanced R’ Tips & Tricks
================

## Use `base::switch()` to make choices!

You can often employ the use of `base::switch()` as an alternative to a
long series of `if (...) {...} else if (...) {...} ...` statements:

``` r
# Acceptable (but not ideal)
x_option <- function(x) {
  if (x == "a") {
    "option 1"
  } else if (x == "b") {
    "option 2" 
  } else if (x == "c") {
    "option 3"
  } else {
    stop("Invalid `x` value")
  }
}

# Optimal
x_option <- function(x) {
  switch(x,
    a = "option 1",
    b = "option 2",
    c = "option 3",
    stop("Invalid `x` value")
  )
}
```

## Do not ‘grow’ data in loops!

You often see people attempt to store the result of an iteration in a
loop onto the end of an existing R object (iteratively). This should be
avoided at all costs as it can lead to severe performance bottlenecks.

Instead, you should **initialise** an empty vector equal to the length
of the number of iterations you want to run and *then* populate each
entry iteratively. Observe the difference between the following:

``` r
# Not even acceptable: this is plain bad!
n_iterations <- 10000
rnorm_means <- c()
for (i in seq_len(n_iterations)) {
  rnorm_means <- c(rnorm_means, mean(rnorm(n = 100))) 
}

# Optimal
n_iterations <- 10000
rnorm_means <- vector(mode = "numeric", length = n_iterations)
for (i in seq_len(n_iterations)) {
  rnorm_means[[i]] <- mean(rnorm(n = 100))
}
```

Note that you should also avoid using the syntax `1:length(x)` inside a
for loop, since the infix operator `:` can create reverse sequences as
well as forwards ones (i.e. `1:0` is just as valid as `1:2`). You
normally *don’t* want your loops to go backwards. As a result, making
use of `base::seq_along(x)` or `base::seq_len()` is much more stable.

## Function jargon

There are many important terms surrounding the use of functions which
can be important to understand. These include:

-   The `body()`, `formals()` and `environment()` properties of
    functions, which refer to the source code inside a function, its
    arguments and its execution environment, respectively
-   The idea that R functions are **first-class**: this means that R
    functions are objects in their own right
-   That there are so-called **primitive** functions in R (e.g. `sum()`)
    which are written in C
-   That an R function ‘encloses’ its environment, leading to the common
    terminology of **closures** as functions
-   That R functions do not necessarily require a name! You can write
    **anonymous** functions whenever you feel that a specific function
    doesn’t warrant a name (i.e. a small function without much re-use
    value)

## Lazy evaluation

Note that in R, function arguments are only evaluated if they are
accessed. For example, the following code snippet does *not* return an
error, despite what you might expect:

``` r
h <- function(x) {
  10
}

h(x = stop("Error!"))
```

    ## [1] 10

The reason this is the case is that whenever you run a function in R,
the arguments will be interpreted and stored as a **promise**, which is
an idiosyncratic data structure specific to R. Each argument, as a
‘promise’, will be assigned three properties upon function execution:

-   An expression. In the snippet above, this expression is
    `stop("Error!")`
-   An environment for the function to be executed in i.e. the
    environment where the function is called. In the snippet above, the
    function is being defined in the global environment so that is where
    the expression will be evaluated (but *only* if the argument is
    accessed, see the next bullet)
-   A value. This value is only calculated if the argument is accessed
    in the function. It is calculated once and is stored in a virtual
    cache for the remainder of execution. In the snippet above, we never
    try to access the argument `x` so the promise remains a promise with
    only two of three properties: its expression and a potential
    environment for the expression to be evaluated in.

## Exiting functions

Generally speaking, it is preferable to include an **explicit**
`return()` call at the end of the function. This is preferable because
it helps other people to understand the purpose of the function which
makes it easier to trace back the calculation.

Sometimes, your functions will require a change to the global state of
your environment (for example, if you’ve ever used `setwd()` before,
you’ll know that this function alters the working directory
permanently). To deal with this change upon exiting a function, you
might want to add a call to `on.exit(expr, add = TRUE)` in order to deal
with any required changes just before the function exits execution!

## Ignoring errors

Note that you can ignore errors by simply writing:

``` r
err_fn <- function(x) {
  try(log(x))
  print('Did the log work?')
}

err_fn("a") # notice how execution continues within the function environment
```

    ## Error in log(x) : non-numeric argument to mathematical function
    ## [1] "Did the log work?"

## Handling errors

You can handle errors with `tryCatch()`:

``` r
tryCatch(
  error = function(cnd) {
    print('Now we have transferred control to here...')
  },{
    print('This is fine...')
    stop()
  }
)
```

    ## [1] "This is fine..."
    ## [1] "Now we have transferred control to here..."

Note that `rlang::abort()` captures extra information about errors, at
least in comparison to `base::stop()`:

``` r
cnd1 <- rlang::catch_cnd(stop("an error"))
cnd2 <- rlang::catch_cnd(rlang::abort("an error"))

str(cnd1)
```

    ## List of 2
    ##  $ message: chr "an error"
    ##  $ call   : language force(expr)
    ##  - attr(*, "class")= chr [1:3] "simpleError" "error" "condition"

``` r
str(cnd2)
```

    ## List of 3
    ##  $ message: chr "an error"
    ##  $ trace  :List of 3
    ##   ..$ calls  :List of 25
    ##   .. ..$ : language rmarkdown::render("/Users/Johnny/Documents/Programming/R/advanced-r/tips.Rmd",      encoding = "UTF-8")
    ##   .. ..$ : language knitr::knit(knit_input, knit_output, envir = envir, quiet = quiet)
    ##   .. ..$ : language knitr:::process_file(text, output)
    ##   .. ..$ : language base::withCallingHandlers(if (tangle) process_tangle(group) else process_group(group),      error = function(e) { ...
    ##   .. ..$ : language knitr:::process_group(group)
    ##   .. ..$ : language knitr:::process_group.block(group)
    ##   .. ..$ : language knitr:::call_block(x)
    ##   .. ..$ : language knitr:::block_exec(params)
    ##   .. ..$ : language knitr:::in_dir(input_dir(), evaluate(code, envir = env, new_device = FALSE,      keep_warning = !isFALSE(options$| __truncated__ ...
    ##   .. ..$ : language knitr:::evaluate(code, envir = env, new_device = FALSE, keep_warning = !isFALSE(options$warning),      keep_messa| __truncated__ ...
    ##   .. ..$ : language evaluate::evaluate(...)
    ##   .. ..$ : language evaluate:::evaluate_call(expr, parsed$src[[i]], envir = envir, enclos = enclos,      debug = debug, last = i == l| __truncated__ ...
    ##   .. ..$ : language evaluate:::timing_fn(handle(ev <- withCallingHandlers(withVisible(eval(expr,      envir, enclos)), warning = wHan| __truncated__
    ##   .. ..$ : language base:::handle(ev <- withCallingHandlers(withVisible(eval(expr, envir, enclos)),      warning = wHandler, error = | __truncated__
    ##   .. ..$ : language base::withCallingHandlers(withVisible(eval(expr, envir, enclos)), warning = wHandler,      error = eHandler, message = mHandler)
    ##   .. ..$ : language base::withVisible(eval(expr, envir, enclos))
    ##   .. ..$ : language base::eval(expr, envir, enclos)
    ##   .. ..$ : language base::eval(expr, envir, enclos)
    ##   .. ..$ : language rlang::catch_cnd(rlang::abort("an error"))
    ##   .. ..$ : language rlang::eval_bare(rlang::expr(tryCatch(!!!handlers, {     force(expr) ...
    ##   .. ..$ : language base::tryCatch(condition = function (x)  x, { ...
    ##   .. ..$ : language base:::tryCatchList(expr, classes, parentenv, handlers)
    ##   .. ..$ : language base:::tryCatchOne(expr, names, parentenv, handlers[[1L]])
    ##   .. ..$ : language base:::doTryCatch(return(expr), name, parentenv, handler)
    ##   .. ..$ : language base::force(expr)
    ##   ..$ parents: int [1:25] 0 1 2 3 3 3 6 7 8 8 ...
    ##   ..$ indices: int [1:25] 1 2 3 4 5 6 7 8 9 10 ...
    ##   ..- attr(*, "class")= chr "rlang_trace"
    ##   ..- attr(*, "version")= int 1
    ##  $ parent : NULL
    ##  - attr(*, "class")= chr [1:3] "rlang_error" "error" "condition"

## Custom conditions

Let’s create custom conditions for `stats::rnorm()`:

``` r
rnorm(n = "a")
```

    ## Warning in rnorm(n = "a"): NAs introduced by coercion

    ## Error in rnorm(n = "a"): invalid arguments

``` r
rnorm(n = "a", mean = "b", sd = 2) 
```

    ## Error in rnorm(n = "a", mean = "b", sd = 2): invalid arguments

Let’s be more specific about *which* arguments are problematic. In the
second example, the `sd` argument is actually valid, whilst the other
arguments are supplied as (problematic) character values.

The following wrapper around `rlang::abort()` constructs more detailed
condition objects with extra metadata, including the argument which was
problematic, what it must be instead, and what it should not be! In a
productionised system, these artifacts can then be extracted for
specific error handling:

``` r
abort_bad_argument <- function(arg, must, not = NULL) {
  msg <- glue::glue("`{arg}` must {must}")
  if (!is.null(not)) {
    not <- typeof(not)
    msg <- glue::glue("{msg}; not {not}.")
  }
  
  rlang::abort("error_bad_argument", 
    message = msg, # constructed from `arg`, `must` and (potentially) `not`
    arg = arg, 
    must = must, 
    not = not
  )
}

my_rnorm <- function(n, mean, sd) {
  if (!is.numeric(n)) {
    abort_bad_argument("n", must = "be numeric", not = n)
  }
  if (!is.numeric(mean)) {
    abort_bad_argument("mean", must = "be numeric", not = mean)
  }
  if (!is.numeric(sd)) {
    abort_bad_argument("sd", must = "be numeric", not = sd)
  }
  
  stats::rnorm(n = n, mean = mean, sd = sd)
}

my_rnorm(n = 100, mean = "b", sd = 10)
```

    ## Error: `mean` must be numeric; not character.

## Using conditions more effectively

There are many ways in which one can leverage the R condition system
more effectively.

# Failure values

Sometimes functions we want to return a value regardless of whether the
code succeeded or not.

A good example of this would be if you had a list of ages that you
wanted to apply log to, say, and one of them was negative. Rather than
halt the process, you might decide to replace it with a sensible value
(such as `NA_real_`)

``` r
test_ages <- rnorm(n = 10, mean = 30, sd = 10)
test_ages <- c(test_ages, -20)

fail_with <- function(expr, failure_val = NA_real_) {
  tryCatch(error = function(cnd) {
    failure_val
  },
  log(expr))
}

suppressWarnings(purrr::map_dbl(test_ages, ~ fail_with(log(.))))
```

    ##  [1] 1.1197068 1.1688267 0.9575591 1.2140634 1.2009798 1.2394067 1.0322743
    ##  [8]       NaN 1.3326117 1.1985176       NaN

A minor adaptation of the above code can allow us to add a little more
metadata to our return values:

``` r
safely_exec <- function(expr) {
  tryCatch(
    error = function(cnd) {
      list(result = NULL, error = cnd)
    },
    list(result = expr, error = NULL)
  )
}

str(safely_exec(2 + 2)) # fine
```

    ## List of 2
    ##  $ result: num 4
    ##  $ error : NULL

``` r
str(safely_exec(log("a"))) # error
```

    ## List of 2
    ##  $ result: NULL
    ##  $ error :List of 2
    ##   ..$ message: chr "non-numeric argument to mathematical function"
    ##   ..$ call   : language log("a")
    ##   ..- attr(*, "class")= chr [1:3] "simpleError" "error" "condition"

## Recording conditions, for example messages

Suppose you are running a productionised piece of code which requires
some sort of log.

You could scatter relevant messages throughout your script and then wrap
the execution of the script into the function presented below. This
function will collect all of the messages issued during the execution of
the script - you can then opt to transform the resultant list of
messages into a more convenient data structure that is more readable for
a human being:

``` r
with_log <- function(module) {
  messages <- c()
  append_message <- function(cnd) {
    messages <<- append(messages, cnd$message)
    rlang::cnd_muffle(cnd)
  }
  
  # The code below ensures that message handlers are registered upon running our code. This means that whenever a message is
  # signaled, control will be transferred to the 'append_message()' function which will append the message to our list
  withCallingHandlers(
    message = append_message,
    module
  )
  
  
  return(list(module_results = module, module_log = messages))
}

execute_module <- function() {
  message("Initiating module...")
  result <- 4 + 4
  message("Terminating module...")
  
  return(result)
}

module <- with_log(execute_module())
```

## Using `purrr::safely()` to impute error values

You can use `purrr::safely()` to successfully exit an invocation of
`purrr::map_*()` functions where there might be errors in the process.
For example,

``` r
x <- list(
  c(0.512, 0.165, 0.717),
  c(0.064, 0.781, 0.427),
  c(0.890, 0.785, 0.495),
  "oops"
)

x_out <- purrr::map(x, purrr::safely(sum))

str(x_out)
```

    ## List of 4
    ##  $ :List of 2
    ##   ..$ result: num 1.39
    ##   ..$ error : NULL
    ##  $ :List of 2
    ##   ..$ result: num 1.27
    ##   ..$ error : NULL
    ##  $ :List of 2
    ##   ..$ result: num 2.17
    ##   ..$ error : NULL
    ##  $ :List of 2
    ##   ..$ result: NULL
    ##   ..$ error :List of 2
    ##   .. ..$ message: chr "invalid 'type' (character) of argument"
    ##   .. ..$ call   : language .Primitive("sum")(..., na.rm = na.rm)
    ##   .. ..- attr(*, "class")= chr [1:3] "simpleError" "error" "condition"

The function `purrr::safely()` is, in common R parlance, referred to as
a *function operator* since it takes a function as an input and returns
a function as an output.

By the way, you can transpose the list presented above via
`purrr::transpose()`!

``` r
x_out_trans <- purrr::transpose(purrr::map(x, purrr::safely(sum)))

str(x_out_trans)
```

    ## List of 2
    ##  $ result:List of 4
    ##   ..$ : num 1.39
    ##   ..$ : num 1.27
    ##   ..$ : num 2.17
    ##   ..$ : NULL
    ##  $ error :List of 4
    ##   ..$ : NULL
    ##   ..$ : NULL
    ##   ..$ : NULL
    ##   ..$ :List of 2
    ##   .. ..$ message: chr "invalid 'type' (character) of argument"
    ##   .. ..$ call   : language .Primitive("sum")(..., na.rm = na.rm)
    ##   .. ..- attr(*, "class")= chr [1:3] "simpleError" "error" "condition"

This is a particularly neat coupling of several `purrr` functions which
allows you to run a batch process in spite of the occasional failure.
Indeed, you might be required to run a series of models on a series of
datasets; some of these models might fail due to unforeseen data issues
(maybe, for instance, one of the columns has a negative number for which
a square root transformation is required). Now, you don’t want the
entire batch to fail just because one of the iterations might not work.
So, you often find that code akin to the below can be quite helpful to
run in this scenario:

``` r
execute_engine <- function(input_data) {
  execute_module1()
  execute_module2()
  ..
}

engine_runs <- transpose(map(datasets, safely(execute_engine)))
ok <- map_lgl(engine_runs$error, is.null)

# which datasets failed to pass preliminary checks?
datasets[!ok]

# which runs were successful?
engine_runs[ok]
```
