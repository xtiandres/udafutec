# Effective Data Visualization with R: Volume I - Base Graphics


# Preparing Data

# attaching dataset - pag 26
data(gapminder, package = 'gapminder')
# inspecting the data
#head(gapminder)
gapminder_2007 <- subset(gapminder, year == 2007, select = -3)
#head(gapminder_2007)


# Función plot()

# producing a plot using x and y coordinates - gráfico 1
plot(y = gapminder_2007$lifeExp, x = gapminder_2007$gdpPercap)

# producing a plot using formula - gráfico 1
plot(lifeExp ~ gdpPercap, gapminder_2007)

# defining variables
a <- gapminder_2007$lifeExp
b <- gapminder_2007$gdpPercap
# plotting variables - gráfico 1
plot(a ~ b)

# using with() - avoid using the $ - gráfico 1
with(gapminder_2007, plot(gdpPercap, lifeExp))


# Filtering data

# producing a scatter plot for African countries only for 2007 - gráfico 2
plot(lifeExp ~ gdpPercap,
     data = gapminder,
     subset = continent == 'Africa' & year == 2007)


# Shape - pch

# viewing all 25 shapes
y = c(rep(5, 5),rep(4, 5), rep(3, 5), rep(2, 5), rep(1, 5))
x = c(rep(1:5, 5))
plot(y ~ x, pch = 1:25, ylim = c(0.8, 5.5), cex = 1.5)
text(y = y, x = x, label = 1:25, pos = 3, cex = 0.8, col = 'red')

# using an integer - gráfico 3
plot(lifeExp ~ gdpPercap,
     gapminder_2007,
     pch = 23)

# using an ASCII character - gráfico 3
plot(lifeExp ~ gdpPercap,
     gapminder_2007,
     pch = '*')

# using an ASCII character by passing an integer - gráfico 3
plot(lifeExp ~ gdpPercap,
     gapminder_2007,
     pch = 42)


# Shape by groups - Categories

shapes <- 15:19
# shapes by continent - gráfico 4
plot(lifeExp ~ gdpPercap,
     gapminder_2007,
     pch = shapes[continent])

# using integers - gráfico 4
plot(lifeExp ~ gdpPercap,
     gapminder_2007,
     pch = as.integer(continent))


# Size - cex
# gráfico 5
plot(lifeExp ~ gdpPercap,
     gapminder_2007,
     pch = 19,
     cex = 1.5)

# size as variable, pass the column to be sized - gráfico 5
plot(lifeExp ~ gdpPercap,
     gapminder_2007,
     pch = 19,
     cex = pop/200e6)


# Colour - colours() colors()

# get all the colour names available in
colours()[1:20]
length(colours())

library(scales)
# viewing colours
show_col(c('red', 'yellow', 'blue', 'green', 'purple',
           'orange', 'brown', 'lightblue', 'lightgreen'))

# viewing all colours
show_col(colours(), labels = FALSE, border = "white")

# viewing just a section of colours
show_col(colours()[32:47], border = "white")

# default colours
show_col(1:8, border = "white")

# recycling of default colours
show_col(1:16, border = "white")

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

# using bg, col and lwd
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


# RGB rgb()

# converting from RGB to hex code
rgb(0, 1, 0)
#> [1] "#00FF00"
rgb(0, 1, 0, alpha = 0.5)
#> [1] "#00FF0080"
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

# converting from RGB to hex code
rgb(0, 255, 0, maxColorValue = 255)
#> [1] "#00FF00"
plot(
  lifeExp ~ gdpPercap,
  gapminder_2007,
  pch = 21,
  cex = 1.5,
  bg = rgb(0, 255, 0, maxColorValue = 255)
)


# HSV, hue, saturation and value hsv()

# converting from HSV to hex code
hsv(0.3, 1, 1)
#> [1] "#33FF00"
hsv(0.3, 1, 1, alpha = 0.5)
#> [1] "#33FF0080"
# adding colours by using HSV
plot(
  lifeExp ~ gdpPercap,
  gapminder_2007,
  pch = 21,
  cex = 1.5,
  bg = hsv(0.3, 1, 1, 0.5)
)


# Converting between different colour models - col2rgb()

col2rgb('green')

# rgb2hsv() converts from RGB to HSV
rgb2hsv(0, 1, 0)










