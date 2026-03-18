# Amazon Warranty Liability Development Triangle Analysis

Actuarial loss reserving analysis applying Chain Ladder and Bornhuetter-Ferguson methods to Amazon's corporate warranty liabilities.

# Overview

This project demonstrates how actuarial loss development techniques, traditionally used for insurance claims reserving, can be applied to non-insurance corporate liabilities. Using Amazon's warranty obligation disclosures from 10-K filings, I built development triangles and applied two standard actuarial reserving methods to estimate ultimate costs and required reserves.

# Data Source
Multi-year warranty liability data extracted from Amazon's SEC 10-K filings (2020-2023)

# Development Triangle
Constructed cumulative development triangle showing how warranty liabilities evolve from 12 to 48 months of maturity

# Reserving Methods Applied

1. Chain Ladder Method**
- Pure data-driven approach
- Calculates age-to-age development factors from historical patterns
- Projects ultimate liabilities assuming future develops like past
- Result: $896M total reserves needed

2. Bornhuetter-Ferguson Method**
- Blends historical development with expected ultimate loss ratio
- Uses Amazon revenue and industry warranty cost benchmarks (0.25% of revenue)
- More stable when data is limited or volatile
- Result: $756M total reserves needed

# Findings 
- Learned the difference between Chain Ladder and Bornhuetter-Ferguson Methods! 
- BF Method is more conservative.
- Chain Ladder relies past patterns, while BF incorporates an "expected ultimate" based on Amazon's revenue and typical warranty cost ratios.
- BF < CL -> warranty development is worse or recent products have higher warranty exposure
