# Factor-Analysis
This project is aimed at analyzing factor performance using factor returns data from the Global Factor Data repository (https://jkpfactors.com/).  The selected factors for the analysis includes market equity, R&amp;D to sales, and Liquidity of book assets. The analysis included factor interpretation, performance assessment, and visualizations.

# Factor Selection & Economic Rationale
The study focused on three primary factors:

Market Equity (Size)
Small firms tend to earn higher returns due to greater risk exposure, constrained access to capital, and illiquidity.

R&D to Sales
Captures firms’ innovation intensity. R&D is often underpriced due to its intangible nature, leading to potential alpha generation.

Liquidity of Book Assets
Reflects firms' ability to raise funds without external financing. High liquidity is advantageous during financial distress.

# Historical Performance & Risk Analysis
Sharpe Ratio Trends

Market Equity: Highly cyclical with wide variance across macroeconomic regimes (e.g., oil crisis, recovery).

R&D to Sales: Strong performance during tech optimism (1990s), weak during recessionary periods.

Liquidity: Performs well in credit-stressed environments, underperforms when liquidity is abundant.

Post-Publication Performance Decay

All three factors saw reduced Sharpe ratios after their academic introduction.

Suggests possible overexploitation or changes in market dynamics post-publication.

Comparison with Gold & Benchmarks

Gold outperformed many traditional factors across countries and time periods.

Served as a stable alternative with stronger Sharpe ratios in certain contexts.

# Spanning Regression Models
Standard Models Applied:

CAPM: Market risk premium significant.

Fama-French 3-Factor (FF3): Market, SMB, and HML all significant.

Hou et al. q-Factor Model: Included size, investment, ROE, and expected growth.

Adjusted R² values varied across models and factors, indicating varying levels of explanatory power.

# Model Extensions
Issue: FF3 model had limited explanatory power for R&D to Sales and Liquidity factors.

Solution: Introduced two new variables:

R&D Capital to Book Assets (RDC) – A firm-level innovation proxy.

Amihud Illiquidity Measure (AMI) – Captures market liquidity risk.

Extended Model: FF3 + RDC + AMI

Delivered significant improvements:

R² for R&D-to-Sales rose from 0.41 → 0.86

R² for Liquidity rose from 0.66 → 0.73

Market Equity remained strong (0.86 → 0.87)

