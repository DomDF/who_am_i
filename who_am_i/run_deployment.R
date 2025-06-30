# Deployment script with R version workaround
# Run this after setting up your account

# Force compatibility mode
options(rsconnect.force.compatible.r = TRUE)

# Try deployment with minimal configuration
rsconnect::deployApp(
  appDir = ".",
  appName = "who_am_i",
  account = "domdf",
  server = "shinyapps.io",
  lint = FALSE,
  forceUpdate = TRUE,
  launch.browser = FALSE
)