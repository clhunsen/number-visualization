## MIT License
##
## Copyright (c) 2014, 2016 Claus Hunsen
##
## Permission is hereby granted, free of charge, to any person obtaining a copy
## of this software and associated documentation files (the "Software"), to deal
## in the Software without restriction, including without limitation the rights
## to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
## copies of the Software, and to permit persons to whom the Software is
## furnished to do so, subject to the following conditions:
##
## The above copyright notice and this permission notice shall be included in all
## copies or substantial portions of the Software.
##
## THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
## IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
## FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
## AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
## LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
## OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
## SOFTWARE.

## Theis plot is inspired by the following article:
## https://www.visualnews.com/2013/07/09/the-art-of-pi-a-colorful-data-visualization/


## SOURCE ESSENTIALS
source("globals.R")
source("numbers.R")


## LIBRARIES

library(plyr) # rbind.fill
library(sqldf) # sqldf
library(circlize) # circos plots


## NUMBERS!

numbers = get.pi.digits()
numbers.amount = length(numbers)


### CONFIGURATION

MAXDIGITS = 2000
COLS = 50
ROWS = MAXDIGITS / COLS

## TODO find a good way to encode symbol and description for all kinds of numbers
DESCRIPTION = substitute(
    paste("The First ", MAXDIGITS, " Digits of ", pi),
    list(MAXDIGITS = format(MAXDIGITS, big.mark=",") )
)

### if set to "topdown", the digits start in the top-left corner; otherwise bottom-left
DIRECTION = "topdown" # "normal"

### set colors for the plots
COLORS = COLORS1

### size of dots
DOTSIZE = 1.5


## FUNCTIONS

### plot given numbers into a shape

dotplot = function(digits) {

    xlist = c()
    ylist = c()
    colorlist = c()

    for (i in 1:MAXDIGITS) {

        ## get current digit
        digit = digits[i]

        ## get current col and row
        ## (get zero-based numbers by substraction of 1)
        col = (i - 1) %% COLS
        row = (i - 1) %/% COLS

        ## invert col and row
        if (DIRECTION == "topdown") {
            # col = COLS - col - 1
            row = ROWS - row - 1
        }

        ## debug
        # cat(digits[i], ":", row, ":", col, "\n")

        xlist = c(xlist, col)
        ylist = c(ylist, row)
        colorlist = c(colorlist, COLORS[digit + 1])

    }

    ## set plotting settings
    par(
        oma = c(2, 0, 0, 0),
        mai = c(0, 0.15, 0, 0.15),
        bg = COLORS.BACKGROUND
    ) #  Bottom, Left, Top, Right.

    ## plot!
    plot(
        x = xlist, y = ylist, col = colorlist, pch = 19, cex = DOTSIZE,
        # xaxs = "i", yaxs = "i", xpd = TRUE,
        axes = FALSE, xlab = "", ylab = NULL, main = NULL
    )

    # text(x = xlist, y = ylist, labels = digits[1:MAXDIGITS])
}


### plot given numbers as connected lines

lineplot = function(digits) {

    for (i in 1:MAXDIGITS) {

        # digits = pi_digits
        # i = 361

        ## get current digit
        digit = digits[i]

        ## get current col and row
        ## (get zero-based numbers by substraction of 1)
        col = (i - 1) %% COLS
        row = (i - 1) %/% COLS

        ## invert col and row
        if (DIRECTION == "topdown") {
            # col = COLS - col - 1
            row = ROWS - row - 1
        }

        ## get connected neighbors
        neighbors = get.connected.neighbors(row, col, digits, digit)

        # cat(digits[i], ":", row, ":", col, "\n")

        if(length(neighbors) != 0) {
            for(j in 1:length(neighbors)) {
                neighbor = neighbors[[j]]
                neighborRow = neighbor[1]
                neighborCol = neighbor[2]

                lines(x = c(col, neighborCol), y = c(row, neighborRow), col = COLORS[digit + 1])
            }
        }

    } ## for (i in 1:MAXDIGITS)

}

get.connected.neighbors = function(row, col, digits, digit) {
    # row = 0; col = 0
    candidates = get.neighbors(row, col)

    ## create list of real indices of digits
    digit.indices = lapply(candidates, function(x) {
        if (DIRECTION == "topdown") {
            (ROWS - x[1] - 1) * COLS + (x[2] + 1)
        } else {
            (x[1]) * COLS + (x[2] + 1)
        }
    })

    ## get those indices that are connected
    digit.indices.connected = Filter(function(x) digits[x] == digit, digit.indices)

    if (length(digit.indices.connected) == 0) {
        return(list())
    }

    ## find list index for connected digit indices
    candidate.indices = lapply(digit.indices.connected, function(x) {
        which(digit.indices == x)
    })

    ## map the connected indices to the candidate list
    candidates.connected = candidates[unlist(candidate.indices)]

    return(candidates.connected)
}


# get neighboring digits for given one
get.neighbors = function(row, col) {

    neighbors = list()

    currentRow = get.prev.and.next(row, col)
    neighbors = c(neighbors, currentRow)

    if (row > 0) {
        prevRow = get.prev.and.next(row - 1, col)
        neighbors = c(neighbors, prevRow)
    }

    if (row < ROWS - 1) {
        nextRow = get.prev.and.next(row + 1, col)
        neighbors = c(neighbors, nextRow)
    }

    return(neighbors)
}

get.prev.and.next = function(row, col) {
    neighbors = list()

    # add previous point
    if (col > 0) {
        neighbors = c(neighbors, list(c(row, col - 1)))
    }

    # add the point itself
    neighbors = c(neighbors, list(c(row, col)))

    #add next point
    if (col < COLS - 1) {
        neighbors = c(neighbors, list(c(row, col + 1)))
    }

    return(neighbors)
}



### RUN

pdf(get.output.path("dots.pdf"), width = 11.6, height = 16.5, bg = COLORS.BACKGROUND)
# svg(get.output.path("dots.svg"), width = 12.125, height = 24.409, bg = COLORS.BACKGROUND)

dotplot(numbers)
lineplot(numbers)

mtext(DESCRIPTION, side = 1, line = -1.1, col = COLORS.TEXT_TITLE, cex = 1.5)
mtext("Claus Hunsen, 2014", side = 1, line = 0, col = COLORS.TEXT_SUB, cex = 1)

dev.off()
