# Interest Rate Risk Hedging Tool

## Introduction
This analytical tool evaluates two primary methods for hedging interest rate risk: Vanilla Swap and Duration Hedging. It facilitates interactive customization for users through sliders and visualizes data with graphs and tables. The tool is intended as a preliminary guide to understanding and forecasting hedging strategy performance.

## Strategies
### Vanilla Swap Strategy
- **Overview**: This strategy entails a fixed-to-floating rate payment exchange, helping users lock in interest expenses.
- **Customization**: Inputs like notional amount, rates, and fees are adjustable through sliders.
- **Outputs**: Includes detailed swap analytics, breakeven points, and risk metrics with visual aids.

### Duration Hedging Strategy
- **Purpose**: Aligns the interest rate sensitivity of assets and liabilities.
- **Customization**: Users adjust inputs related to liabilities, bond prices, and durations.
- **Outputs**: Provides the required number of asset contracts and portfolio cost analysis.

## Comparative Analysis
A side-by-side evaluation of both strategies based on inputs like notional amounts and market rates is provided, showcasing total potential savings.

## Technical Overview

### Data
- The tool utilizes SOFR data from FRED, retrieved via the fredr API.
- The memoise package is used to restrict data fetching, maintaining FRED's API policy compliance.

### App Structure
- `Ui.R`: Constructs a responsive layout with user interaction elements.
- `Server.R`: Handles strategy logic and real-time output updates.

### Hosting
- Hosted on Shinyapps.io for global, no-installation access.

## Appendix

### Swap Output Explanation
- **Break-Even Analysis**: Calculates when the swap benefits the fixed-rate payer.
- **Risk Metrics**: Includes PV and DV01 to assess swap exposure to interest rate changes.

### Cash Flow Analysis
- Analyzes expected cash flows, presenting an interactive plot and summarized table.

## Getting Started
For full access to the tool and in-depth analyses, visit [Interest Rate Hedging Tool Link](#).

### Installation
```sh
library(shiny)
runGitHub("username/repo")
