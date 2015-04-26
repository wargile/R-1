# Trying out things from the Trigonometry book by Michael Corral
# --------------------------------------------------------------

# Angles:
# Recall the following definitions from elementary geometry:
# (a) An angle is acute if it is between 0 and 90 degrees
# (b) An angle is a right angle if it equals 90 degrees
# (c) An angle is obtuse if it is between 90 and 180
# (d) An angle is a straight angle if it equals 180

# In elementary geometry, angles are always considered to be positive and not larger than 360 degrees.

# Two angles that sum to 90 degrees: Complementary angles
# Two angles that sum to 180 degrees: Supplementary angles
# Two angles that sum to 360 degrees: Conjugate/explementary angles

# The sum of the angles in a triangle equals 180 degrees
# An Isosceles Triangle is a triangle with two sides of equal length
# in a right triangle one of the angles is 90 degrees and the other two angles are acute angles whose sum is 90 degrees
#   (i.e. the other two angles are complementary angles)

SetStandardOptions()

DegreesToRadians <- function(d) return(d * (pi / 180))
RadiansToDegrees <- function(r) return((r * 180) / pi)

FindAngleOfVector <- function(data) {
  result <- atan(data[2] / data[1]) * 180 / pi
  plot(c(0,data[1]), c(0, data[2]), type="o", asp=1, col="blue")
  if ((result + 180) > 360)
    return (result - 180)
  else
    return (result + 180) # Nope....
}
FindAngleOfVector(c(3,4)) # <x,y>
FindAngleOfVector(c(-3,4)) # <x,y>
FindAngleOfVector(c(3,-4)) # <x,y>
FindAngleOfVector(c(-3,-4)) # <x,y>
FindAngleOfVector(c(0,-4)) # <x,y>
FindAngleOfVector(c(0,4)) # <x,y>
FindAngleOfVector(c(4,0)) # <x,y>
FindAngleOfVector(c(-4,0)) # <x,y>
