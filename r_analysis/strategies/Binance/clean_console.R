# Clear console
cat("\014")  # For Windows
# cat("\f")  # For Mac/Linux (uncomment if needed)

# Clear global environment
rm(list = ls())

# Verify environment is empty
ls()  # This should show nothing
