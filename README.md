
# spindler <img src='man/figures/logo.png' align="right" height="276" />

<!-- badges: start -->
<!-- badges: end -->

The goal of spindler is to publish Twitter threads based on an rmarkdown document. 

## Installation

You can install the development version from [GitHub](https://github.com) with:

<!-- the released version of spindler from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("spindler")
```

And  --> 

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
  publish()
```

Look at the finished product
```{r, eval=FALSE}
birds$show()
```

### From knitr

Create a new thread object and use `knitr_thread()` 

````
```{r, setup}
this_thread <- spindler::thread$new(tag = "tw_status", publish = TRUE)
```
````

Now if you add the `tw_status` option to a chunk with the text you want to tweet, 
it will be added to the thread along with its first figure (if there is one) during
the rendering process.

````
```{r, tw_status = "The relationship between pressure and temperature is cool!"}
plot(pressure)
```
````

You can also use `this_thread$new_post()` inside your knitr document to add posts 
manually. 

When the document is done rendering, your thread with be published and shown in a 
browser window. 

**Be careful**! Since the rendering process runs on an independent session, you won't
get the thread object. This means that you cannot use `this_thread$destroy()` to 
remove all the new posts, you'll need to do it manually. That's why `thread$new()`
has the safety argument `publish`.
