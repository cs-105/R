library(owmr)

# first of all you have to set up your api key
owmr_settings("ac66c8209bdf887068a2a79e4fdbca33")

# or store it in an environment variable called OWM_API_KEY (recommended)
Sys.setenv(OWM_API_KEY = "ac66c8209bdf887068a2a79e4fdbca33") # if not set globally

result <- get_forecast(lat = -22.90278, lon = -22.90278, cnt = 3, units = "metric")
result
