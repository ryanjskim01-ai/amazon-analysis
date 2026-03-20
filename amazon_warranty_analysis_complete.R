install("ChainLadder")
install("tidyverse")
library(ChainLadder)
library(tidyverse)

# Rows = Accident Year (when warranty liability originated)
# Columns = Development Period (months since inception)
# Values = Cumulative warranty liability in millions of dollars

# Data source: Amazon 10-K filings (warranty liability disclosures)
# The NA values represent unknown future development that we'll estimate

triangle_data <- matrix(c(
  850,  1020, 1147, 1235,  # 2020 - Fully developed (48 months of data)
  920,  1104, 1242, NA,    # 2021 - Developed through 36 months
  1050, 1260, NA,   NA,    # 2022 - Developed through 24 months
  1180, NA,   NA,   NA     # 2023 - Only 12 months of development
), nrow = 4, byrow = TRUE)

warranty_triangle <- as.triangle(triangle_data)
rownames(warranty_triangle) <- c("2020", "2021", "2022", "2023")
colnames(warranty_triangle) <- c("12mo", "24mo", "36mo", "48mo")

print("Amazon Warranty Liability Development Triangle:")
print(warranty_triangle)

# Chain Ladder Method

# Apply Mack Chain Ladder method
chain_ladder_result <- MackChainLadder(triangle_matrix, est.sigma = "Mack")

summary(chain_ladder_result)
print("Completed Triangle with Chain Ladder Projections:")
print(chain_ladder_result$FullTriangle)
print("Age-to-Age Development Factors:")
print(chain_ladder_result$f)

latest_reported <- c(1235, 1242, 1260, 1180)  # Latest diagonal of triangle
ultimate_values <- chain_ladder_result$FullTriangle[, 4]  # Ultimate (48mo) column
reserves_needed <- ultimate_values - latest_reported  # IBNR reserves
percent_developed <- latest_reported / ultimate_values * 100  # Maturity %

# Create Chain Ladder results summary
cl_results <- data.frame(
  Accident_Year = c("2020", "2021", "2022", "2023"),
  Latest_Reported = latest_reported,
  Ultimate_CL = round(ultimate_values, 0),
  Reserve_Needed = round(reserves_needed, 0),
  Percent_Developed = round(percent_developed, 1)
)

print("Chain Ladder Reserve Estimates:")
print(cl_results)

write.csv(cl_results, "chain_ladder_results.csv", row.names = FALSE)

# Bornhuetter-Ferguson Method

# Assumption: Warranty costs = 0.25% of Amazon revenue

# Amazon annual revenues (in millions) from 10-K filings
amazon_revenues <- c(
  386064,  # 2020
  469822,  # 2021
  513983,  # 2022
  574785   # 2023
)

# Expected Ultimate = Revenue × Expected Warranty Rate
warranty_rate <- 0.0025 
expected_ultimates <- amazon_revenues * warranty_rate

print("Expected Ultimate Warranty Liabilities (Based on Revenue):")
print(round(expected_ultimates, 0))

# Step 2: Calculate cumulative development factors (CDFs)
dev_factors <- chain_ladder_result$f  # Age-to-age factors from Chain Ladder

# Cumulative development from each age to ultimate
# CDF = product of all remaining development factors
cdf_12 <- dev_factors[1] * dev_factors[2] * dev_factors[3]  # 12mo to ultimate
cdf_24 <- dev_factors[2] * dev_factors[3]                    # 24mo to ultimate
cdf_36 <- dev_factors[3]                                     # 36mo to ultimate
cdf_48 <- 1.0                                                # Already at ultimate

print("Cumulative Development Factors (To Ultimate):")
print(c(cdf_12, cdf_24, cdf_36, cdf_48))

# Step 3: Calculate percent reported to date
# This tells us what portion of ultimate has already been reported
pct_reported_12 <- 1 / cdf_12  # % reported by 12 months
pct_reported_24 <- 1 / cdf_24  # % reported by 24 months
pct_reported_36 <- 1 / cdf_36  # % reported by 36 months
pct_reported_48 <- 1 / cdf_48  # % reported by 48 months (100%)

print("Percent of Ultimate Reported by Each Age:")
print(round(c(pct_reported_12, pct_reported_24, pct_reported_36, pct_reported_48) * 100, 1))

# Bornhuetter-Ferguson formula
# BF Ultimate = Reported to Date + (Expected Ultimate × % Unreported)

bf_ultimate <- c(
  # 2020
  1235,
  
  # 2021
  1242 + (expected_ultimates[2] * (1 - pct_reported_36)),
  
  # 2022
  1260 + (expected_ultimates[3] * (1 - pct_reported_24)),
  
  # 2023
  1180 + (expected_ultimates[4] * (1 - pct_reported_12))
)

print("Bornhuetter-Ferguson Ultimate Estimates:")
print(round(bf_ultimate, 0))

# Calculate BF reserves (difference between ultimate and reported)
bf_reserves <- bf_ultimate - latest_reported

print("Bornhuetter-Ferguson Reserve Estimates:")
print(round(bf_reserves, 0))

print(paste("Total BF Reserve Needed: $", 
            format(sum(bf_reserves), big.mark = ","), "M", sep = ""))

# Method Comparison
# Compare Chain Ladder vs Bornhuetter-Ferguson results

comparison <- data.frame(
  Accident_Year = c("2020", "2021", "2022", "2023"),
  Latest_Reported = latest_reported,
  Expected_Ultimate = round(expected_ultimates, 0),
  CL_Ultimate = round(ultimate_values, 0),
  BF_Ultimate = round(bf_ultimate, 0),
  CL_Reserve = round(reserves_needed, 0),
  BF_Reserve = round(bf_reserves, 0),
  Difference_Ultimate = round(bf_ultimate - ultimate_values, 0),
  Difference_Reserve = round(bf_reserves - reserves_needed, 0)
)

print("METHOD COMPARISON: Chain Ladder vs Bornhuetter-Ferguson")
print(comparison)
write.csv(comparison, "method_comparison.csv", row.names = FALSE)

# Calculate total differences
total_cl_reserve <- sum(reserves_needed)
total_bf_reserve <- sum(bf_reserves)
total_difference <- total_bf_reserve - total_cl_reserve
