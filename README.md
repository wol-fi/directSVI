# directSVI
Direct Least-Squares Method for the Stochastic Volatility Inspired (SVI) equation for implied volatilities. The code is based on my paper ["Direct Fit for SVI Implied Volatilities"](https://github.com/wol-fi/directSVI/blob/main/directSVI.pdf) (forthcoming in J.Derivatives). Instead of commonly used iterative routines I propose a closed-form solution to fit slices of implied volatility directly. 

## Examples:
Fitted SVI curve for mid- implied volatilities of different stocks (4/7/2023).
![](stocks_call.png)

## Concept:
- the SVI equation can be fully linearized when re-writing it as a conic section (hyperbola)
- once linearized, the SVI equation represents a quadratically constrained Eigenvalue problem
- which is easily solved

## Code:
Available here::
- [R](https://github.com/wol-fi/directSVI/tree/main/R)
- [Python](https://github.com/wol-fi/directSVI/tree/main/python)
