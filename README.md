---
output: github_document
---

# spindler

<!-- badges: start -->
<!-- badges: end -->

The goal of spindler is to publish Twitter threads based on an rmarkdown document. 

## Installation

You can install 

<!-- the released version of spindler from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("spindler")
```

And  --> the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("eliocamp/spindler")
```

## Setup

Spinder uses [rtweet](https://rtweet.info) to publish your tweets, so you'll need to 
go over its basic configuration of [obtaining and using access tokens](https://rtweet.info/articles/auth.html).

## Example

### Manually crafted:

```{r, eval=FALSE}
birds <- spindler::thread$new()
birds$add_post("Hey, people, I want to tell you how awesome birds are!")$
  add_post("They have feathers, and (most of them) can fly!")$
  add_post("And look how cute they ares", media = "pictures/birds/penguin1.png")

birds$publish()
```

Oh, no I made a typo. Quick, delete the whole thing!
```{r, eval=FALSE}
birds$destroy()
```

Let's start over
```{r, eval=FALSE}
birds$clear()$
  add_post("Nooo! I had an awesome thread about birds, but I messed up.")$
  add_post("So here's the jist of it: birds rock and they are better than monkeys!")$
  publish
```

Look at the finished product
```{r, eval=FALSE}
birds$show()
```

### From knitr

Just add 

````
```{r, setup}
this_thread <- spindler::thread$new()
spindler::knitr_thread(this_thread, TRUE)
```
````
