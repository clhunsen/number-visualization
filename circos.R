## MIT License
##
## Copyright (c) 2016 Claus Hunsen
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

## These plots are inspired by the following article:
## http://www.independent.co.uk/news/science/maths-experts-stunned-as-they-crack-a-pattern-for-prime-numbers-a6933156.html


## SOURCE ESSENTIALS
source("globals.R")
source("numbers.R")


## LIBRARIES

library(plyr) # rbind.fill
library(sqldf) # sqldf
library(circlize) # circos plots


## NUMBERS!

numbers = get.primes.last.digits()


## CONFIGURATION

MAXDIGITS = 100

## TODO find a good way to encode symbol and description for all kinds of numbers
DESCRIPTION = substitute(
    paste("The First ", MAXDIGITS, " Consecutive Ending Digits of ", Primes),
    list(MAXDIGITS = format(MAXDIGITS, big.mark=",") )
)

### set colors for the plots
COLORS = COLORS1
COLORS.BY = "from" # indicator for color, either "from" or "to"


## COMPUTE LINKS BETWEEN CONSECUTIVE NUMBERS

numbers = numbers[1:MAXDIGITS]

numbers.links.raw = data.frame(
    from = numbers[1:(MAXDIGITS-1)],
    to   = numbers[2:MAXDIGITS]
)


## PLOT-RELATED FUNCTIONS

compute.plot.segments = function(data, base.col, default = 0) {
    data = split(data, data[[base.col]])
    data = lapply(data, default = default, FUN = function(df, default = 0) {
        df["fraction"] = df[["amount"]] / df[["total"]]
        df[paste0(base.col, ".circle.end")] = normalize(cumsum(df[["fraction"]])) + default
        if (nrow(df) == 1)
            df[paste0(base.col, ".circle.start")] = c(default)
        else
            df[paste0(base.col, ".circle.start")] = c(default, df[[paste0(base.col, ".circle.end")]][1:(nrow(df) - 1)])

        return(df)
    })
    data = rbind.fill(data)

    return(data)
}


## COMPUTE FROM-SEGMENTS to TO-SEEGMENTS IN CIRCOS PLOT

numbers.links = sqldf("SELECT `from`, `to`, COUNT(*) AS `amount` FROM `numbers.links.raw` GROUP BY `from`, `to`")
numbers.linked = unique(c(numbers.links[["from"]], numbers.links[["to"]]))

fractions.from = sqldf("SELECT `from`, SUM(`amount`) as `total` FROM `numbers.links` GROUP BY `from` ORDER BY `from`, `to`")
fractions.from = merge(numbers.links, fractions.from, by = "from")
fractions.from = compute.plot.segments(fractions.from, "from", default = 1)

fractions.to = sqldf("SELECT `to`, SUM(`amount`) as `total` FROM `numbers.links` GROUP BY `to` ORDER BY `to`, `from`")
fractions.to = merge(numbers.links, fractions.to, by = "to")
fractions.to = compute.plot.segments(fractions.to, "to", default = 0)

### sanity check
stopifnot(nrow(fractions.from) == nrow(fractions.to))

fractions = merge(
    subset(fractions.from, select = c("from", "to", "from.circle.start", "from.circle.end")),
    subset(fractions.to, select = c("from", "to", "to.circle.start", "to.circle.end")),
    by = c("from", "to")
)

### debug
# print(fractions)


## CONSTRUCT CIRCOS PLOT

pdf(get.output.path("circos.pdf"), width = 12, height = 12, bg = COLORS.BACKGROUND)
# svg(get.output.path("circos.svg"), width = 12, height = 12, bg = COLORS.BACKGROUND)

### plot initialization

par(mar = c(4, 2, 2, 2), bg = COLORS.BACKGROUND)
circos.par(cell.padding = c(0, 0, 0, 0), start.degree = 270)
circos.initialize(numbers.linked, xlim = c(0, 2))


### outer circle

circos.trackPlotRegion(track.height = 0.1, ylim = c(0, 1), bg.border = NA, panel.fun = function(x, y) {
    sector.name = get.cell.meta.data("sector.index")
    xlim = get.cell.meta.data("xlim")
    ylim = get.cell.meta.data("ylim")
    circos.rect(xlim[1], 0, xlim[2], 0.5, col = COLORS[as.numeric(sector.name) + 1])
    circos.text(mean(xlim), ylim[2], sector.name, cex = 2, facing = "inside", niceFacing = TRUE, col = COLORS.BACKGROUND.INVERT)
})

### links

for (i in 1:nrow(fractions)) {
    row = fractions[i,]
    color.index = as.numeric(row[[COLORS.BY]]) + 1

    circos.link(
        sector.index1 = row[["from"]],
        point1 = c(row[["from.circle.start"]], row[["from.circle.end"]]),
        sector.index2 = row[["to"]],
        point2 = c(row[["to.circle.start"]], row[["to.circle.end"]]),
        col = paste0(COLORS[color.index], COLORS.OPACITY),
        border = COLORS.BACKGROUND.INVERT, lwd = 1,
        h = 1, w = 1
    )
}

### description

mtext(DESCRIPTION, side = 1, line = 0, col = COLORS.TEXT_TITLE, cex = 1.5)
mtext("Claus Hunsen, 2016", side = 1, line = 1.1, col = COLORS.TEXT_SUB, cex = 1)

### end plot

circos.clear()
dev.off()


## CONSTRUCT CIRCOS CHORD PLOT

pdf(get.output.path("circos_chord.pdf"), width = 12, height = 12, bg = COLORS.BACKGROUND)
# svg(get.output.path("circos_chord.svg"), width = 12, height = 12, bg = COLORS.BACKGROUND)

### plot initialization

par(mar = c(4, 2, 2, 2), bg = COLORS.BACKGROUND)
circos.par(cell.padding = c(0, 0, 0, 0), start.degree = 270)

## compute colors for edge list

colors.df = as.data.frame(COLORS)
colors.df[[COLORS.BY]] = rownames(colors.df)
COLORS.LINKS = merge(fractions.from, colors.df, by = COLORS.BY)[["COLORS"]]

### plot chord diagram

chordDiagramFromDataFrame(
    fractions.from,
    grid.col = COLORS,
    col = COLORS.LINKS, link.border = COLORS.BACKGROUND.INVERT, transparency = COLORS.OPACITY.DEC,
    annotationTrack = NULL, preAllocateTracks = list(track.height = 0.1)
)

### outer circle

circos.trackPlotRegion(track.index = 1, bg.border = NA, panel.fun = function(x, y) {
    sector.name = get.cell.meta.data("sector.index")
    xlim = get.cell.meta.data("xlim")
    ylim = get.cell.meta.data("ylim")
    circos.rect(xlim[1], 0, xlim[2], 0.5, col = COLORS[as.numeric(sector.name) + 1])
    circos.text(mean(xlim), ylim[2], sector.name, cex = 2, facing = "inside", niceFacing = TRUE, col = COLORS.BACKGROUND.INVERT)
})

### description

mtext(DESCRIPTION, side = 1, line = 0, col = COLORS.TEXT_TITLE, cex = 1.5)
mtext("Claus Hunsen, 2016", side = 1, line = 1.1, col = COLORS.TEXT_SUB, cex = 1)

### end plot

circos.clear()
dev.off()
