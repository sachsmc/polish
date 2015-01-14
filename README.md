# Polish: turn functions into simple shiny apps

This function takes a function, parses its arguments, and creates a simple [Shiny](http://shiny.rstudio.com/) application using the function arguments and defaults as inputs. Currently it supports numeric, text, and logical arguments. All arguments must be named and have defaults. On the output side, it creates text output from the function, or a plot by using `plot = TRUE`. This is in early development, it may not work in all cases, and is expected to change in the future. 

## Installation

This isn't an `R` package yet. Source the code like so:

```r
devtools::source_url("https://raw.githubusercontent.com/sachsmc/polish/master/polish.R")
```

## Examples

```r
sizfunc<-function(RR = 0.711, B = 1000, x = 40, conprob = 0.94, title = "Test", plot = FALSE){
  rowout<-matrix(NA, nrow = B, ncol = 2)
  for(i in 1:B){
    y1<-rbinom(x, 1, conprob)
    y2<-rbinom(x, 1, conprob*RR)
    outtest<-exact2x2::binomMeld.test(sum(y1), x, sum(y2), x, parmtype = c("ratio"))
    flag<-ifelse(outtest$p.value<0.05, 1, 0)
    rowout[i, ]<-cbind(flag, 1-outtest$estimate)
  }
  return(round(colMeans(rowout), 2))
}

polish(sizfunc)
```


```r
fnplot <- function(title = "Hello"){

  hist(rnorm(100), main = title)

}

polish(fnplot, plot = TRUE)
```

## License

The MIT License (MIT)

Copyright (c) 2015 Michael Sachs

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
