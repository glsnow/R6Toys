# From chatgpt
# Spirograph-style plot using simple turtle graphics in base R

# --- Turtle helpers ---
turtle <- new.env()
turtle$x <- 0
turtle$y <- 0
turtle$angle <- 0
turtle$pen <- TRUE

turtle_init <- function(x = 0, y = 0, angle = 0) {
  turtle$x <- x
  turtle$y <- y
  turtle$angle <- angle
}

turtle_move <- function(x2, y2, col = "black", lwd = 1) {
  if (turtle$pen) {
    segments(turtle$x, turtle$y, x2, y2, col = col, lwd = lwd)
  }
  turtle$x <- x2
  turtle$y <- y2
}

# --- Spirograph curve generator ---
spirograph_turtle <- function(R = 8, r = 3, d = 5, turns = 20, steps = 4000,
                              col = "steelblue", lwd = 1) {
  t <- seq(0, turns * 2 * pi, length.out = steps)
  
  # Hypotrochoid / spirograph curve
  x <- (R - r) * cos(t) + d * cos(((R - r) / r) * t)
  y <- (R - r) * sin(t) - d * sin(((R - r) / r) * t)
  
  # Set up plot
  plot.new()
  plot.window(xlim = range(x), ylim = range(y), asp = 1)
  title("Spirograph Turtle Plot")
  axis(1); axis(2); box()
  
  # Draw with the turtle
  turtle_init(x[1], y[1])
  for (i in 2:length(x)) {
    turtle_move(x[i], y[i], col = col, lwd = lwd)
  }
}

# Example
set.seed(1)
spirograph_turtle(R = 10, r = 5.5, d = 7, turns = 18, steps = 5000,
                  col = "purple", lwd = 1.2)


## python code from gemini ----

import math
import turtle

# 1. Configuration Constants
R = 150  # Radius of fixed outer circle
r = 90  # Radius of rolling inner circle
d = 70  # Distance of pen from inner circle's center

# Integration step sizes
dt = 0.05  # Time step size (smaller = smoother, larger = faster)
# Calculate total steps needed to close the loop based on the fraction GCD
total_rotations = r // math.gcd(int(R), int(r))
max_t = 2 * math.PI * total_rotations

# 2. Derivative Helper Functions
def dx_dt(t):
  return -(R - r) * math.sin(t) - (d * (R - r) / r) * math.sin(((R - r) / r) * t)


def dy_dt(t):
  return (R - r) * math.cos(t) - (d * (R - r) / r) * math.cos(((R - r) / r) * t)


# 3. Setup Turtle Environment
screen = turtle.Screen()
screen.setup(width=600, height=600)
screen.title("Hypotrochoid via Differential Step Engine")

t_bot = turtle.Turtle()
t_bot.speed(0)  # Fastest drawing speed
t_bot.penup()

# 4. Teleport Turtle to Starting Parametric Coordinate (t = 0)
x0 = (R - r) * math.cos(0) + d * math.cos(0)
y0 = (R - r) * math.sin(0) - d * math.sin(0)
t_bot.goto(x0, y0)

# Face the initial absolute tangent direction
phi_initial = math.atan2(dy_dt(0), dx_dt(0))
t_bot.setheading(math.degrees(phi_initial))
t_bot.pendown()

# 5. Iterative Angle/Length Generation Loop
t = 0
current_phi = phi_initial

while t < max_t:
  # A. Calculate the step length (ds) using the speed formula
  # Factor in 'dt' to turn velocity into a small distance step
  speed = (R - r) * math.sqrt(
    1 + (d / r) ** 2 - (2 * d / r) * math.cos((R / r) * t)
  )
ds = speed * dt

# B. Move to the next time step to calculate the angle change
t += dt

# C. Calculate the next absolute heading angle
next_phi = math.atan2(dy_dt(t), dx_dt(t))

# D. Calculate the relative angle change (d_phi)
d_phi = next_phi - current_phi

# E. Normalize the angle to avoid artificial 360-degree flip errors
d_phi = math.atan2(math.sin(d_phi), math.cos(d_phi))

# F. Map math radians to turtle degrees
d_phi_degrees = math.degrees(d_phi)

# G. Command the Turtle exclusively using delta angles and lengths
t_bot.left(d_phi_degrees)
t_bot.forward(ds)

# Keep track of the current direction
current_phi = next_phi

# Keep the window open when finished
turtle.done()


