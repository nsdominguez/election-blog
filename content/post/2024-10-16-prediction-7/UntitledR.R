bucket1 <- all_pred %>%
  select(state, pred.1, lwr.1, upr.1,
         pred.2, lwr.2, upr.2,
         pred.4, lwr.4, upr.4,
         pred.4, lwr.4, upr.6)

bucket2 <- all_pred %>%
  select(state, pred.1, lwr.1, upr.1,
         pred.2, lwr.2, upr.2,
         pred.4, lwr.4, upr.4,
         pred.6, lwr.6, upr.6)

bucket3 <- all_pred %>%
  select(state, pred.5, lwr.5, upr.5,
         pred.rf)

bucket4 <- all_pred %>%
  select(state, pred.1, lwr.1, upr.1,
         pred.2, lwr.2, upr.2,
         pred.4, lwr.4, upr.4,
         pred.5, lwr.5, upr.5, 
         pred.6, lwr.6, upr.6, pred.rf)