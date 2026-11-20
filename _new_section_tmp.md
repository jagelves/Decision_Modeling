### Choosing p and q {.unnumbered}

Before estimating an ARIMA model we must choose how many AR terms (p) and how many MA terms (q) to include. The ACF and PACF are the main tools for this choice, because each process leaves a recognizable signature on them:

| Process | ACF | PACF |
|:--|:--|:--|
| AR(p) | Decays gradually (may alternate in sign or oscillate) | Cuts off: significant spikes up to lag p, then nothing |
| MA(q) | Cuts off: significant spikes up to lag q, then nothing | Decays gradually (may alternate in sign) |
| ARMA(p,q) | Decays gradually | Decays gradually |
| White noise | No significant spikes | No significant spikes |

The AR(1) and MA(1) simulations earlier in this chapter are the simplest cases of the first two rows. Real data are noisier than these textbook patterns, so the table gives us a starting guess, not a final answer. The final answer comes from checking the errors.

A useful way to think about any ARIMA model is that it splits the series into two parts:

<center>$y_t = \underbrace{c + \phi_1 y_{t-1} + \dots + \phi_p y_{t-p} + \theta_1 \epsilon_{t-1} + \dots + \theta_q \epsilon_{t-q}}_{\text{what the model explains}} + \underbrace{\epsilon_t}_{\text{what is left over}}$</center>

<br>

Every AR or MA term we include is a tool for reproducing part of the correlation pattern we see in the ACF and PACF. Once the model is estimated, that part of the pattern is "subtracted" from the series, and only what the model could not reproduce stays behind in the errors $\epsilon_t$. This gives a simple rule for judging our choice of p and q:

-   **Too few terms:** some of the correlation is not modeled, so it stays in the errors. The ACF and PACF of the residuals will still show significant spikes, typically at the lags we left out.
-   **The right terms:** all of the systematic correlation is captured and the residuals look like white noise. There is nothing left to model.
-   **Too many terms:** the residuals also look like white noise, but the extra coefficients add nothing except estimation noise. Between two models with white-noise residuals, we prefer the simpler one, which is the one with the lower BIC.

Besides looking at the residual ACF, we can test formally whether a group of residual autocorrelations is jointly zero with the **Ljung-Box test**. A large p-value (above 0.05) means we cannot reject that the errors are white noise. In R, the test is applied to the residuals of a fitted model with `augment(fit) %>% features(.innov, ljung_box, lag = 10)`.

The lab below lets you practice this process. It generates a mystery series from a random AR, MA, or ARMA process. Row 1 shows the series with its ACF and PACF. When you choose an AR order and an MA order, the model is fitted to the series and row 2 shows the ACF and PACF that your model can reproduce. The same pattern appears as orange dots on top of the series correlations in row 1, so you can see how much of each bar your model accounts for. Row 3 shows the residuals: what is left after your model's part has been removed. All correlation panels share the same scale, so you can compare them directly from top to bottom.

::: {.content-visible when-format="html"}
{{< include _arma-widget.qmd >}}
:::

::: {.content-visible unless-format="html"}
*An interactive ARMA identification lab is available in the online version of this chapter.*
:::

Some things to try:

1.  Start with p = 0 and q = 0. The model is just the mean, so it explains none of the correlation, and the residual ACF and PACF are identical to those of the series. Everything is left in the errors.
2.  Use the table above to make a first guess from the series ACF and PACF. Add one term at a time and watch the red spikes in the residual panels disappear as the orange dots fill in the series bars.
3.  Choose an order that is too small (for example, an AR(1) when the PACF shows two spikes). The spike you did not model remains in the residuals and the Ljung-Box test fails.
4.  Choose an order that is too large (for example, p = 3 and q = 2). The residuals still look like white noise, but the BIC is higher than for the simpler model that also passes. More terms are not better if they explain nothing new.
5.  Click **Reveal true process** to check your answer, then generate a new series and try again.

Note that the correlations do not literally subtract bar by bar (the ACF of the residuals is not exactly the series ACF minus the model ACF), but the idea holds: the correlation your model reproduces is removed, and the correlation it does not reproduce stays in the errors.

