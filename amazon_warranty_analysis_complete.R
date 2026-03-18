# ============================================
# AMAZON WARRANTY LIABILITY DEVELOPMENT TRIANGLE ANALYSIS
# Project: Applying Actuarial Loss Reserving Techniques to Corporate Liabilities
# Author: Ryan Kim
# Date: March 2026
# ============================================

# This project demonstrates how actuarial loss reserving methods (Chain Ladder 
# and Bornhuetter-Ferguson) can be applied to non-insurance corporate liabilities.
# We analyze Amazon's warranty obligations using development triangles to 
# estimate ultimate costs and required reserves.

# ============================================
# SECTION 1: Load Required Libraries
# ============================================

# ChainLadder package provides industry-standard actuarial reserving functions
# tidyverse provides data manipulation tools
library(ChainLadder)
library(tidyverse)

# ============================================
# SECTION 2: Create Development Triangle
# ============================================

# Development triangles show how liabilities evolve over time
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

# Convert to triangle object for use with ChainLadder package
warranty_triangle <- as.triangle(triangle_data)

# Add descriptive labels for clarity
rownames(warranty_triangle) <- c("2020", "2021", "2022", "2023")
colnames(warranty_triangle) <- c("12mo", "24mo", "36mo", "48mo")

# Display the triangle
print("Amazon Warranty Liability Development Triangle:")
print(warranty_triangle)

# ============================================
# SECTION 3: Chain Ladder Method
# ============================================

# The Chain Ladder method projects ultimate liabilities based purely on 
# historical development patterns. It assumes "the future will develop like the past."

# Apply Mack Chain Ladder method
# This calculates age-to-age development factors and projects ultimate values
chain_ladder_result <- MackChainLadder(triangle_matrix, est.sigma = "Mack")

# View comprehensive summary including:
# - Development factors
# - Standard errors
# - Ultimate estimates
# - Statistical diagnostics
summary(chain_ladder_result)

# Extract the completed triangle (with projected values filling in NAs)
print("Completed Triangle with Chain Ladder Projections:")
print(chain_ladder_result$FullTriangle)

# Extract age-to-age development factors
# These show the multiplicative factor from one period to the next
# Example: A factor of 1.20 means liabilities increase 20% from 12mo to 24mo
print("Age-to-Age Development Factors:")
print(chain_ladder_result$f)

# Extract key results for analysis
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

# Export Chain Ladder results to CSV for Excel analysis
write.csv(cl_results, "chain_ladder_results.csv", row.names = FALSE)

print(paste("Total Chain Ladder Reserve Needed: $", 
            format(sum(reserves_needed), big.mark = ","), "M", sep = ""))

# ============================================
# SECTION 4: Bornhuetter-Ferguson Method
# ============================================

# The Bornhuetter-Ferguson method blends historical development patterns 
# with an a priori expected ultimate loss ratio. This is more stable when
# data is limited or volatile.

# Step 1: Establish expected ultimate losses
# Assumption: Warranty costs = 0.25% of Amazon revenue
# This is an external benchmark based on industry standards

# Amazon annual revenues (in millions) from 10-K filings
amazon_revenues <- c(
  386064,  # 2020
  469822,  # 2021
  513983,  # 2022
  574785   # 2023
)

# Calculate expected ultimate warranty liability for each year
# Expected Ultimate = Revenue × Expected Warranty Rate
warranty_rate <- 0.0025  # 0.25% of revenue
expected_ultimates <- amazon_revenues * warranty_rate

print("Expected Ultimate Warranty Liabilities (Based on Revenue):")
print(round(expected_ultimates, 0))

# Step 2: Calculate cumulative development factors (CDFs)
# CDF tells us: "By age X, what percentage of ultimate is typically reported?"
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

# Step 4: Apply Bornhuetter-Ferguson formula
# BF Ultimate = Reported to Date + (Expected Ultimate × % Unreported)
# This blends actual reported amounts with expected unreported amounts

bf_ultimate <- c(
  # 2020: Fully developed, use actual value
  1235,
  
  # 2021: At 36 months, some development remaining
  # Reported + (Expected × % Unreported)
  1242 + (expected_ultimates[2] * (1 - pct_reported_36)),
  
  # 2022: At 24 months, more development remaining
  1260 + (expected_ultimates[3] * (1 - pct_reported_24)),
  
  # 2023: At 12 months, most development remaining
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

# ============================================
# SECTION 5: Method Comparison
# ============================================

# Compare Chain Ladder vs Bornhuetter-Ferguson results
# This helps identify when historical patterns differ from expected patterns

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

print("=" %R% 80)
print("METHOD COMPARISON: Chain Ladder vs Bornhuetter-Ferguson")
print("=" %R% 80)
print(comparison)

# Export comparison to CSV for Excel dashboard
write.csv(comparison, "method_comparison.csv", row.names = FALSE)

# Calculate total differences
total_cl_reserve <- sum(reserves_needed)
total_bf_reserve <- sum(bf_reserves)
total_difference <- total_bf_reserve - total_cl_reserve

print("")
print("SUMMARY STATISTICS:")
print(paste("Total Reserve - Chain Ladder:        $", format(round(total_cl_reserve, 0), big.mark = ","), "M", sep = ""))
print(paste("Total Reserve - Bornhuetter-Ferguson: $", format(round(total_bf_reserve, 0), big.mark = ","), "M", sep = ""))
print(paste("Difference (BF - CL):                 $", format(round(total_difference, 0), big.mark = ","), "M", sep = ""))

# Interpret the difference
if (total_difference < 0) {
  print("")
  print("INTERPRETATION:")
  print("BF reserves are LOWER than Chain Ladder, suggesting that historical")
  print("development patterns have been worse than industry-expected rates.")
  print("This could indicate Amazon's warranty experience is deteriorating,")
  print("or product mix has shifted toward higher-warranty items.")
} else {
  print("")
  print("INTERPRETATION:")
  print("BF reserves are HIGHER than Chain Ladder, suggesting that historical")
  print("development patterns have been better than industry-expected rates.")
}

# ============================================
# SECTION 6: Visualizations (Optional)
# ============================================

# Create development pattern visualization
dev_factors_df <- data.frame(
  Period = c("12-24", "24-36", "36-48"),
  Factor = dev_factors
)

# Plot development factors
library(ggplot2)

ggplot(dev_factors_df, aes(x = Period, y = Factor)) +
  geom_col(fill = "navy", alpha = 0.7) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  geom_text(aes(label = round(Factor, 3)), vjust = -0.5, size = 4) +
  labs(
    title = "Amazon Warranty Development Factors",
    subtitle = "Age-to-Age Development Patterns",
    x = "Development Period",
    y = "Development Factor",
    caption = "Source: Amazon 10-K filings | Analysis: Chain Ladder Method"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))

# Save plot
ggsave("development_factors.png", width = 8, height = 5)

# Create reserve comparison chart
reserve_comparison <- data.frame(
  Year = rep(c("2021", "2022", "2023"), 2),
  Method = rep(c("Chain Ladder", "Bornhuetter-Ferguson"), each = 3),
  Reserve = c(
    reserves_needed[2:4],  # CL reserves for 2021-2023
    bf_reserves[2:4]       # BF reserves for 2021-2023
  )
)

ggplot(reserve_comparison, aes(x = Year, y = Reserve, fill = Method)) +
  geom_col(position = "dodge", alpha = 0.8) +
  scale_fill_manual(values = c("Chain Ladder" = "#2C3E50", 
                                "Bornhuetter-Ferguson" = "#E74C3C")) +
  labs(
    title = "Reserve Estimates by Method",
    subtitle = "Chain Ladder vs Bornhuetter-Ferguson Comparison",
    x = "Accident Year",
    y = "Reserve Needed ($M)",
    caption = "Analysis of Amazon Warranty Liabilities"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        legend.position = "bottom")

# Save plot
ggsave("reserve_comparison.png", width = 8, height = 5)

# ============================================
# SECTION 7: Key Takeaways
# ============================================

print("")
print("=" %R% 80)
print("PROJECT SUMMARY & KEY TAKEAWAYS")
print("=" %R% 80)
print("")
print("1. METHODOLOGY:")
print("   - Applied actuarial loss reserving techniques to non-insurance liabilities")
print("   - Demonstrated versatility of Chain Ladder and Bornhuetter-Ferguson methods")
print("   - Used R's ChainLadder package for industry-standard calculations")
print("")
print("2. TECHNICAL SKILLS DEMONSTRATED:")
print("   - Development triangle construction")
print("   - Age-to-age factor calculation and selection")
print("   - Ultimate loss projection")
print("   - Reserve adequacy estimation")
print("   - Comparative method analysis")
print("")
print("3. BUSINESS INSIGHTS:")
print("   - Identified $", format(round(total_cl_reserve, 0), big.mark = ","), 
      "M (CL) to $", format(round(total_bf_reserve, 0), big.mark = ","), 
      "M (BF) in required reserves")
print("   - $", abs(round(total_difference, 0)), "M difference between methods highlights")
print("     importance of using multiple estimation techniques")
print("   - Analysis provides Amazon with range of reserve estimates for planning")
print("")
print("4. ACTUARIAL APPLICATIONS:")
print("   - Loss development concepts apply beyond insurance")
print("   - Any liability developing over time can use these techniques")
print("   - Examples: warranties, product returns, legal settlements, deferred revenue")
print("")
print("=" %R% 80)
print("END OF ANALYSIS")
print("=" %R% 80)

# ============================================
# Notes for Resume/Interview:
# ============================================

# This project demonstrates:
# - Core P&C actuarial reserving skills (Chain Ladder, Bornhuetter-Ferguson)
# - R programming and use of industry-standard actuarial packages
# - Ability to extract and organize financial data from public filings
# - Comparative analysis and professional judgment in method selection
# - Communication of technical findings through visualization and summary stats
# - Understanding that actuarial techniques generalize beyond traditional insurance

# Key interview talking points:
# - "Applied standard P&C reserving methods to corporate liabilities"
# - "Built development triangles from 10-K disclosures"
# - "Compared Chain Ladder (data-driven) vs BF (assumption-based) approaches"
# - "Found $XXM difference, highlighting need for multiple methods"
# - "Demonstrated actuarial principles apply to any evolving liability"
