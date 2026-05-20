# AntRoMAP
Antarctic Robust Multi-Action Prioritization (AntRoMAP) is a decision-support framework for Antarctic terrestrial conservation under risk uncertainty.

## Objective
Identify:
1. **Where to act** (spatial prioritization of sites), and
2. **Which management actions to deploy** (multi-action portfolios),
to reduce multiple interacting risks while meeting conservation goals.

## Decision framework
AntRoMAP combines:
- **Structured decision-making (PrOACT)** to define the decision context:
  - **Pr**oblem framing
  - **O**bjectives
  - **A**lternatives (management actions)
  - **C**onsequences
  - **T**rade-offs
- **Chance-constrained optimization** to enforce robustness under uncertainty by requiring that key constraints are satisfied with a specified confidence level.

## Core optimization concept
Given:
- candidate sites \(i \in I\),
- management actions \(a \in A\),
- uncertain risk states \(\omega \in \Omega\),

AntRoMAP selects action-site decisions \(x_{i,a}\) that maximize expected conservation value (or minimize expected residual risk), subject to:
- budget/logistic limits,
- ecological feasibility,
- and chance constraints such as:

\[
\Pr\left(g(x,\omega) \le 0\right) \ge 1 - \alpha
\]

where \(\alpha\) is the allowed violation probability (risk tolerance).

## Expected outputs
- Ranked priority sites under uncertainty
- Recommended action portfolios per site
- Trade-off diagnostics across objectives and risk tolerances
- Transparent PrOACT-aligned decision traceability
