# number-visualization - Visualizing numbers in beautiful plots

Numbers and their inherit digits can be visualized beautifully by different means.
In this project, several ideas are implemented in the following files:

## dots.R

In this script, consecutive digits of a number are visualized as colored dots.
If neighboring dots have the same color, they are linked.
The resulting patterns are interesting and very random.

An example can be found [here](output/dots.pdf).

## circos.R

In this script, numbers get visualized in a manner that is known from genome plotting.
The idea is to link consecutive digits and visualize how the links between distinct digits are distributed.

Examples can be found [here](output/circos.pdf) and [here](output/circos_chord.pdf).

## Inspiration 

This project is heavily inspired by a couple of websites and information hubs, such as:
- https://www.visualnews.com/2013/07/09/the-art-of-pi-a-colorful-data-visualization/
- http://mkweb.bcgsc.ca/pi/
- https://www.youtube.com/watch?v=NPoj8lk9Fo4
- http://www.independent.co.uk/news/science/maths-experts-stunned-as-they-crack-a-pattern-for-prime-numbers-a6933156.html
