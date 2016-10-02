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

## There are two types of numbers (both with a list of instances) that can be accessed with
## this file:
## 1) single-number instances, and
## 2) multiple-numbers instances.
## Right now, for 1), we have pi; for 2), we have a list of prime numbers.
## Corresponding functions can be found below.
## All numbers are presented as character, except for functions containing the word "digit(s)".


## FILE NAMES

FILE.PI = file.path(DATAFOLDER, "pi_1m.txt")
FILE.E = file.path(DATAFOLDER, "e_1m.txt")
FILE.PHI = file.path(DATAFOLDER, "phi_1m.txt")
FILE.PRIMES = file.path(DATAFOLDER, "primes_500k.txt")


## LIBRARIES

library(stringr) # str_sub, str_split


## FUNCTIONS

# converts input numbers into their digits
split.to.digits <- function(x) {
    digits = as.numeric(unlist(str_split(x, pattern = "")))
    return(digits)
}


## 1) PI
## http://mkweb.bcgsc.ca/pi/numbers.tgz

get.pi = function() {
    pi = readLines(FILE.PI)
    return(pi)
}

get.pi.digits = function() {
    pi = get.pi()
    pi.digits = split.to.digits(pi)
    return(pi.digits)
}


## 1) E
## http://mkweb.bcgsc.ca/pi/numbers.tgz

get.e = function() {
    e = readLines(FILE.E)
    return(e)
}

get.e.digits = function() {
    e = get.e()
    e.digits = split.to.digits(e)
    return(e.digits)
}


## 1) PHI
## http://mkweb.bcgsc.ca/pi/numbers.tgz

get.phi = function() {
    phi = readLines(FILE.PHI)
    return(phi)
}

get.phi.digits = function() {
    phi = get.phi()
    phi.digits = split.to.digits(phi)
    return(phi.digits)
}


## 2) PRIME NUMBERS
## https://www.mathsisfun.com/numbers/prime-number-lists.html

get.primes = function() {
    primes = readLines(con = FILE.PRIMES)
    return(primes)
}

get.primes.last.digits = function() {
    primes = get.primes()
    primes.last = as.numeric(str_sub(primes, -1))
    return(primes.last)
}
