# Effective Data Visualization with R: Volume I - Base Graphics


# 2 Introduction to plotting with base graphics /pg24

# Preparing Data /pg26

# attaching dataset
data(gapminder, package = 'gapminder')
# inspecting the data
head(gapminder)
# preparing the data
gapminder_2007 <- subset(gapminder, year == 2007, select = -3)
head(gapminder_2007)


# 2.1 The plot() function /pg27

# producing a plot using x and y coordinates - gráfico 1
plot(y = gapminder_2007$lifeExp, x = gapminder_2007$gdpPercap)

# producing a plot using formula - gráfico 1
plot(lifeExp ~ gdpPercap, gapminder_2007)

# defining variables
a <- gapminder_2007$lifeExp
b <- gapminder_2007$gdpPercap
# plotting variables - gráfico 1
plot(a ~ b)

# using with() - avoid using the dollar sign $ - gráfico 1
with(gapminder_2007, plot(gdpPercap, lifeExp))


# 2.1.1 Filtering data /pg30

# argument "subset" is used to filter data 
# producing a scatter plot for African countries only for 2007 - gráfico 2
plot(lifeExp ~ gdpPercap,
     data = gapminder,
     subset = continent == 'Africa' & year == 2007)


# 2.2 Shape /pg32

# argument "pch" is used to control shapes
# viewing all 25 shapes
y = c(rep(5, 5),rep(4, 5), rep(3, 5), rep(2, 5), rep(1, 5))
x = c(rep(1:5, 5))
plot(y ~ x, pch = 1:25, ylim = c(0.8, 5.5), cex = 1.5)
text(y = y, x = x, label = 1:25, pos = 3, cex = 0.8, col = 'red')

# using an integer - gráfico 3
plot(lifeExp ~ gdpPercap,
     gapminder_2007,
     pch = 19)

# using an ASCII character - gráfico 3
plot(lifeExp ~ gdpPercap,
     gapminder_2007,
     pch = '*')

# using an ASCII character by passing an integer - gráfico 3
plot(lifeExp ~ gdpPercap,
     gapminder_2007,
     pch = 42)


# 2.2.1 Shape by groups (Categories) /pg36

shapes <- 15:19
# shapes by continent - gráfico 4
plot(lifeExp ~ gdpPercap,
     gapminder_2007,
     pch = shapes[continent])

# using integers - gráfico 4
plot(lifeExp ~ gdpPercap,
     gapminder_2007,
     pch = as.integer(continent))


# 2.3 Size /pg39
# argument "cex" is used to control size

# gráfico 5
plot(lifeExp ~ gdpPercap,
     gapminder_2007,
     pch = 19,
     cex = 1.5)

# 2.3.1 Size as variable
# pass the column to be sized - gráfico 5
plot(lifeExp ~ gdpPercap,
     gapminder_2007,
     pch = 19,
     cex = pop/200e6)


# 2.4 Colour /pg41
# colours(), colors()

# get all the colour names available in R
colours()[1:20]
# get built-in colours - 657
length(colours())

# function show_col() from package "scales" allow view colours
library(scales)
# viewing colours
show_col(c('red', 'yellow', 'blue', 'green', 'purple',
           'orange', 'brown', 'lightblue', 'lightgreen'))

# viewing all colours
show_col(colours(), labels = FALSE, border = "white")

# viewing just a section of colours
show_col(colours()[32:47], border = "white")

# default colours are 8
show_col(1:8, border = "white")

# recycling of default colours
show_col(1:16, border = "white")

# argument "col" is used to add colours to a plot
# using colour names - gráfico 6
plot(
  lifeExp ~ gdpPercap,
  gapminder_2007,
  pch = 19,
  cex = 1.2,
  col = 'green'
)

# using hex codes - gráfico 6
plot(
  lifeExp ~ gdpPercap,
  gapminder_2007,
  pch = 19,
  cex = 1.2,
  col = '#00FF00'
)

# using colours(). Because colours() returns a vector, it can be indexed - gráfico 6
plot(
  lifeExp ~ gdpPercap,
  gapminder_2007,
  pch = 19,
  cex = 1.2,
  col = colours()[254]
)

# using integers (1 to 8) after which recycling occurs - gráfico 6
plot(
  lifeExp ~ gdpPercap,
  gapminder_2007,
  pch = 19,
  cex = 1.2,
  col = 3
)

# the shapes 21 to 25 have both an interior and a border
# argument "col" is used to colour their border
# argument "bg" is used to fill their interior
# argument "lwd" is used to control outline or border size

# col colours borders - gráfico 7
plot(
  lifeExp ~ gdpPercap,
  gapminder_2007,
  cex = 1.3,
  pch = 21,
  col = 'lightgreen'
)

# bg fills the shape - gráfico 7
plot(
  lifeExp ~ gdpPercap,
  gapminder_2007,
  cex = 1.3,
  pch = 21,
  bg = 'lightgreen'
)

# using bg, col and lwd - gráfico 7
plot(
  lifeExp ~ gdpPercap,
  gapminder_2007,
  pch = 21,
  cex = 1.5,
  bg = 'lightgreen',
  col = 'darkgreen',
  lwd = 2
)

# if we do not want any colour, we use NA instead
plot(
  lifeExp ~ gdpPercap,
  gapminder_2007,
  pch = 21,
  cex = 1.3,
  bg = 'lightgreen',
  col = NA
)


# 2.4.1 Gray colours /pg52

# gray() or grey()
# it takes a numeric value
# 0 - white
# 1 - black

# generating a vector of grey colours
grey(seq(0, 1, length.out = 16))
#> [1] "#000000" "#111111" "#222222" "#333333" "#444444" "#555555" "#666666
#> [8] "#777777" "#888888" "#999999" "#AAAAAA" "#BBBBBB" "#CCCCCC" "#DDDDDD
#> [15] "#EEEEEE" "#FFFFFF"
show_col(grey(seq(0, 1, length.out = 16)))

# using a grey colour
plot(
  lifeExp ~ gdpPercap,
  gapminder_2007,
  pch = 21,
  cex = 1.5,
  bg = grey(0.7),
  col = grey(0.2)
)

# argument "alpha" is used to control transparency
# 0 - no transparency (black)
# 1 - full transparency (white)

# adding alpha
grey(seq(0, 1, length.out = 16), alpha = 0.5)
#> [1] "#00000080" "#11111180" "#22222280" "#33333380" "#44444480" "#555555
#> [7] "#66666680" "#77777780" "#88888880" "#99999980" "#AAAAAA80" "#BBBBBB
#> [13] "#CCCCCC80" "#DDDDDD80" "#EEEEEE80" "#FFFFFF80"
show_col(grey(seq(0, 1, length.out = 16), alpha = 0.5))

# adding transparency
plot(
  lifeExp ~ gdpPercap,
  gapminder_2007,
  pch = 21,
  cex = 1.5,
  bg = grey(0.7, alpha = 0.5),
  col = grey(0.2)
)


# 2.4.2 RGB /pg56

# Red Green Blue
# rgb() converts RGB values to HEX code

# converting from RGB to hex code
rgb(0, 1, 0)
rgb(0, 1, 0, alpha = 0.5)

# adding colour by using RGB
plot(
  lifeExp ~ gdpPercap,
  gapminder_2007,
  pch = 21,
  cex = 1.5,
  bg = rgb(0, 1, 0)
)

# adding transparency
plot(
  lifeExp ~ gdpPercap,
  gapminder_2007,
  pch = 21,
  cex = 1.5,
  bg = rgb(0, 1, 0, alpha = 0.5)
)

# in real life, RGB are specified by using 0 - 255
# converting from RGB to hex code
rgb(0, 255, 0, maxColorValue = 255)

plot(
  lifeExp ~ gdpPercap,
  gapminder_2007,
  pch = 21,
  cex = 1.5,
  bg = rgb(0, 255, 0, maxColorValue = 255)
)


# 2.4.3 HSV /pg59
# Hue, Saturation and Value 
# hsv() converts from HSV to hex code

# converting from HSV to hex code
hsv(0.3, 1, 1)
hsv(0.3, 1, 1, alpha = 0.5)

# adding colours by using HSV
plot(
  lifeExp ~ gdpPercap,
  gapminder_2007,
  pch = 21,
  cex = 1.5,
  bg = hsv(0.3, 1, 1, 0.5)
)


# 2.4.4 Converting between different colour models /pg60
# col2rgb() converts from colour to RGB
col2rgb('green')

# rgb2hsv() converts from RGB to HSV
rgb2hsv(0, 1, 0)


# 2.4.5 Colouring points by groups (categories) /pg61
# pass a categorical column or variable to the "col" argument

plot(
  lifeExp ~ gdpPercap,
  gapminder_2007,
  pch = 19,
  cex = 1.3,
  col = continent
)

# ifelse() select colours for each category
plot(
  lifeExp ~ gdpPercap,
  gapminder_2007,
  pch = 19,
  cex = 1.5,
  col = ifelse(
    continent == 'Africa',
    'brown',
    ifelse(
      continent == 'Asia',
      'darkgreen',
      ifelse(
        continent == 'Americas',
        'purple',
        ifelse(continent == 'Europe', 'orange', 'blue')
      )
    )
  )
)

# creating a vector of colours - gráfico 8
colours <- c('brown', 'darkgreen', 'purple', 'orange', 'blue')
plot(
  lifeExp ~ gdpPercap,
  gapminder_2007,
  pch = 19,
  cex = 1.5,
  col = colours[continent]
)

# using integers - gráfico 8
plot(
  lifeExp ~ gdpPercap,
  gapminder_2007,
  pch = 19,
  cex = 1.5,
  col = as.integer(continent)
)


# 2.4.6 Colouring by a continuous variable /pg64

# viewing the default colour palette (viridis)
show_col(hcl.colors(16))

# contiguous green colours
show_col(hcl.colors(16, 'Greens', rev = TRUE))

# contiguous grey colours
show_col(grey.colors(16, start = 0.1, end = 0.9))

# colouring by gdpPercap (green) - gráfico 9
colour <- hcl.colors(5, 'Greens', rev = TRUE)
plot(
  lifeExp ~ gdpPercap,
  gapminder_2007,
  pch = 19,
  cex = 1.3,
  col = colour[cut(gdpPercap, 5)]
)

# colouring by gdpPercap (grey) - gráfico 9
grays <- grey.colors(5, start = 0.9, end = 0.1)
plot(
  lifeExp ~ gdpPercap,
  gapminder_2007,
  pch = 19,
  cex = 1.3,
  col = grays[cut(gdpPercap, 5)]
)

# 2.4.7 Transparency /pg68

# creating a transparency function
col_transparency <-
  function(colour = 'lightblue',
           transparency = 50) {
    rgb_col <- col2rgb(colour)
    rgb(
      rgb_col[1, ],
      rgb_col[2, ],
      rgb_col[3, ],
      max = 255,
      alpha = (100 - transparency) * 255 / 100
    )
  }

# using the function
col_transparency()
col_transparency('lightgreen', 75)
colour <- c('#66C2A5', '#FC8D62', '#8DA0CB', '#E78AC3', '#A6D854', '#E5C494')
col_transparency(colour)

plot(
  lifeExp ~ gdpPercap,
  gapminder_2007,
  cex = 1.2,
  pch = 21,
  bg = col_transparency('lightgreen', 75),
  col = 'lightgreen'
)

plot(
  lifeExp ~ gdpPercap, 
  gapminder_2007,
  cex = 1.2,
  pch = 21,
  bg = col_transparency(colour)[continent],
  col = colour[continent]
)

# modifying transparency with alpha()
scales::alpha(colour, 0.5)

plot(
  lifeExp ~ gdpPercap,
  gapminder_2007,
  cex = 1.2,
  pch = 21,
  bg = scales::alpha(colour, 0.5)[continent],
  col = colour[continent]
  )


# 2.5 Colour palettes /pg72

# 2.5.1 R Default colour palette

# getting the default colour palette
palette()
length(palette())

# viewing the default colours
show_col(palette(), border = "white")

# setting the default colour palette
palette(c('yellow', 'orange', 'red', 
          'pink', 'purple', 'blue', 
          'green', 'brown', 'black'))
palette()

# viewing the new default colours
show_col(palette(), border = "white")

# reset to the default colour palette
palette('default')
palette()

# R 4.0.0 and later has two functions
# get predefined palettes
palette.pals()

# get colours from predefined palettes
palette.colors()

palette.colors(palette = 'Dark 2')
palette.colors(palette = 'Tableau 10')

# viewing some predefined palettes
show_col(palette.colors())
show_col(palette.colors(palette = 'Set 2'))
show_col(palette.colors(palette = 'Dark 2'))
show_col(palette.colors(n = 8, palette = 'Tableau 10'))

# setting the default palette to Tableau 10
palette('Tableau 10')
# viewing the default palette
show_col(palette())
# using the default palette
plot(
  lifeExp ~ gdpPercap,
  gapminder_2007,
  pch = 19,
  cex = 1.3,
  col = continent
)

# reset to the default colour palette
palette('default')


# 2.5.2 The grDevices colour palettes /pg79


