# Transport Economics Analysis

## Cost Recovery, Crowding and Passenger Welfare in Public Transport

This repository contains quantitative research on public transport
economics, focusing on the trade-offs between passenger welfare,
crowding, demand and cost recovery in Bogotá's TransMilenio BRT system.

The project combines a theoretical welfare model with empirical
analysis of passenger flows to examine how pricing and service
provision affect public transport outcomes.

## Research Questions

- How do fare levels and crowding affect passenger welfare?
- How do pricing and service-frequency policies affect demand?
- How does the degree of cost recovery affect welfare outcomes?
- How are passenger flows distributed across stations during peak
  periods?

## Methods

The analysis combines:

- Welfare economics
- Binary choice demand modelling
- Crowding disutility
- Scenario-based simulations
- Calibration of passenger demand
- Empirical analysis of station-level passenger flows
- Sensitivity analysis
- Quantitative analysis in R

## Empirical Case: Bogotá TransMilenio

A separate empirical component analyzes boarding and alighting
patterns on Trunk Line F during the morning peak period.

The analysis calculates:

- Station-level entries
- Station-level exits
- Boarding-to-alighting ratios
- Relative passenger flows across stations

## Repository Structure

```text
Transport-Economics-Analysis/
│
├── README.md
├── R/
│   ├── main_analysis.R
│   └── line_F_analysis.R
│
├── data/
│   └── README.md
│
└── results/
