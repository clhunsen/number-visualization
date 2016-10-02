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


## GLOBAL OPTIONS

options(stringsAsFactors = FALSE)


## FOLDERS

DATAFOLDER = "numbers-data"
OUTPUTFOLDER = "output"

get.output.path = function(file) {
    return(file.path(OUTPUTFOLDER, file))
}


## COLORS

COLORS1 = c(
    "#283998", # darkblue
    "#00B0F6", # lightblue
    "#94268D", # purple
    "#FF3E35", # red
    "#FEF62A", # yellow
    "#FFBD3B", # orange-yellow
    "#F61E7C", # violet/pink
    "#FF6620", # orange
    "#37B947", # darkgreen
    "#93CE3B"  # lightgreen
)
names(COLORS1) = 0:9

COLORS2 = c(
    "#FFA500", "#FF0000", "#00FF00", "#0000FF", "#A020F0", "#FFFF00", "#737373", "#7CFC00", "#000000", "#BFBFBF"
    # "orange", "red", "green", "blue", "purple", "yellow", "gray45", "lawngreen", "black", "gray75"
)
names(COLORS2) = 0:9

# COLORS3 = c(
#   "#8c51c3", "#3f1e4e", "#e36903", "#945200", "#e70f19", "#690404", "#46cf64", "#4d7719", "#32a4e7", "#0f425a"
#   )
# names(COLORS3) = 0:9
#
# COLORS4 = c(
#   "#f0bda5", "#cd5b18", "#b288e6", "#b9309b", "#96fbfb", "#18f0e6", "#6badff", "#1c96fc", "#c0e1b1", "#73ae3c"
#   )
# names(COLORS4) = 0:9

COLORS.OPACITY = "9F"
COLORS.OPACITY.DEC = 1 - (strtoi(COLORS.OPACITY, base = 16) / 255)

COLORS.BACKGROUND = "black" # transparent
COLORS.BACKGROUND.INVERT = "white"

COLORS.TEXT_TITLE = "gray75"
COLORS.TEXT_SUB = "gray50"


## NORMALIZE TO RANGE 0:1

normalize = function(value) {
    max = max(value)
    normalized = value / max
    return(normalized)
}
