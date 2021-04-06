library(webuse)
library(lmtest)
library(sandwich)
library(margins)
library(dplyr)


nlswork <- filter(nlswork_orig, idcode <= 100) %>%
  select(idcode, year, ln_wage, age, tenure, union) %>%
  mutate(union = as.integer(union),
         idcode = as.factor(idcode)) %>%
  filter(complete.cases(.))
str(nlswork)

count(nlswork, idcode)
count(nlswork, union)

# fixed effects model
m1 <- lm(ln_wage ~ age + tenure + union + tenure:union + idcode,
         data = nlswork)
summary(m1)
m1coeffs_std <- data.frame(summary(m1)$coefficients)
m1coeffs_std[!startsWith(row.names(m1coeffs), 'idcode'),]

heatmap(vcov(m1), Rowv = NA, Colv = NA)

# clustered standard errors
coi_indices <- which(!startsWith(row.names(m1coeffs_cl), 'idcode'))
m1coeffs_cl <- coeftest(m1, vcov = vcovCL, cluster = ~idcode)     # , fix = TRUE ?
m1coeffs_cl[coi_indices,]

coefci(m1, parm = coi_indices, vcov = vcovCL, cluster = ~idcode)
coefci(m1, parm = coi_indices)

# alternative 1: first calculating the vcov, then plugging it into coeftest and margins
cl_vcov_mat <- vcovCL(m1, cluster = ~idcode)
heatmap(cl_vcov_mat, Rowv = NA, Colv = NA)

m1coeffs_cl2 <- coeftest(m1, vcov = cl_vcov_mat)
m1coeffs_cl2[coi_indices,]

# margins with correct CIs
margins(m1, vcov = cl_vcov_mat, variables = 'tenure', at = list(union = 0:1)) %>% summary()


# alternative 2: using lm.cluster frome miceadds

library(miceadds)

m2 <- lm.cluster(ln_wage ~ age + tenure + union + tenure:union + idcode,
                 cluster = 'idcode',
                 data = nlswork)
m2coeffs <- data.frame(summary(m2))
m2coeffs[!startsWith(row.names(m2coeffs), 'idcode'),]
# -> different p-value for intercept o_O

# margins with correct CIs
margins(m2$lm_res, vcov = m2$vcov, variables = 'tenure', at = list(union = 0:1), data = nlswork) %>% summary()   # here need to pass data again via `data = nlswork`

# alternative 3: using lm_robust from estimatr https://declaredesign.org/r/estimatr/articles/getting-started.html#lm_robust
