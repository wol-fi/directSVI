# directSVI
Direct Least-Squares Method for the Stochastic Volatility Inspired (SVI) equation for implied volatilities. The code is based on my working paper "Direct Fit for SVI Implied Volatilities". Instead of non-linear multi-parameter optimization I propose a closed-form solution for fitting slices of implied volatility (strike domain) directly. 

## Concept:
- the SVI equation can be fully linearized when re-writing it as a conic section (hyperbola)
- once linearized, the SVI equation represents a quadratically constrained Eigenvalue problem
- which is easily solved

## Code:
I'm planning to make the code available by summer 2023 for the following languages:
- R (package)
- Python
- Matlab
