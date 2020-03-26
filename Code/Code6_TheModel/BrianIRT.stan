////////////////////////////////////////////
// "Entities" are citizens, policy-makers
// and governments (aka countries).
// "Actors"  mean citizens
// and policy makers.
// "Questions" are items asked in surveys,
// or policies transformed to match those.
////////////////////////////////////////////
data {
 int N_acy; //Nr. of actor-country-years
 int N_gy; //Nr. of government-years
 int N_cq; //Nr. of country-questions
 int N_cp; // Nr. of country-(nonquestion)policies
 int N_c; //Nr. of countries
 int N_q; //Nr. of question items
 int N_p; //Nr. of (non-question) policies
 int N_acyq; //Nr. of actor-country-year-questions
 int N_gyq; //Nr. of government-year-questions
 int N_gyp; //Nr. of government-year-policies
 int<lower=1, upper=N_acy> ii[N_acyq]; //Select right actor and expand to N_acyq
 int<lower=1, upper=N_cq> qq_1[N_acyq]; //Select right country-question and expand to N_acyq
 int<lower=1, upper=N_cq> pp_1[N_gyq]; //Select right country-policy and expand to N_gyq
 int<lower=1, upper=N_gy> gg_1[N_gyq]; //Select right government and expand to N_gyq
 int<lower=1, upper=N_cp> pp_2[N_gyp]; //Select right country-policy and expand to N_gyp
 int<lower=1, upper=N_gy> gg_2[N_gyp]; //Select right government and expand to N_gyp
 int<lower=1, upper=N_c> cc[N_cq]; //Select right country and expand to N_cq
 int<lower=1, upper=N_c> cc_2[N_cp]; //Select right country and expand to N_cp
 int<lower=1, upper=N_q> qq[N_cq]; //Select right question and expand to N_cq
 int<lower=1, upper=N_p> pp_3[N_gyp]; //Select right policy and expand to N_gyp
 int<lower=1, upper=N_p> pp[N_cp]; //Select right policy and expand to N_cp
 real<lower=0, upper=1> y[N_acyq]; //Data: Percentage of pro-market answers
 int K; //Nr. of policy categories
 int<lower=0, upper=1> z1[N_gyq]; //Data: Policies (question)
 int<lower=1, upper=K> z2[N_gyp]; //Data: Policies (non-question)
}
parameters {
  vector[N_acy] mu_a; //Latent mean market moods
  vector<lower=0>[N_acy] sigma; //Heterogeneity of actor market moods
  vector[N_gy] mu_g; //Latent market orientation of policy (government)
  vector<lower=0>[N_cq] lambda; //Discrimination parameters of all question-items
  vector<lower=0>[N_cq] alpha; //Difficulty parameters of question-items
  vector<lower=0>[N_cp] lambda2; //Discrimination parameters of all policy-items
  ordered[K-1] tau[N_p]; //Difficulty parameters of policy-items
  real<lower=0> b; //Concentration of beta
  vector<lower=0>[N_c] nu_lc;
  vector<lower=0>[N_c] nu_l2c;
  vector<lower=0>[N_q] nu_lq;
  vector<lower=0>[N_p] nu_lp;
  vector[N_c] nu_ac;
  vector[N_q] nu_aq;
}
transformed parameters {
  vector<lower=1e-16>[N_acyq] a;
  vector<lower=0, upper=1>[N_acyq] m;
  vector[N_cq] lambdasq;
  vector[N_acy] sigmasq;
  vector[N_gyq] eta_1;
  vector[N_gyp] eta_2;

  for(cq in 1:N_cq){
    lambdasq[cq] = pow(lambda[cq], 2.0);
  }
  for(acy in 1:N_acy){
    sigmasq[acy] = pow(sigma[acy], 2.0);
  }
  m = 0.999989 * (Phi((mu_a[ii] - alpha[qq_1]) ./ sqrt(lambdasq[qq_1] + sigmasq[ii])) - 1.0) + 0.99999;
  a = b * (m ./ (1.0 - m));
  eta_1 = (1 ./ lambda[pp_1]) .* (mu_g[gg_1] - alpha[pp_1]);
  eta_2 = (1 ./ lambda2[pp_2]) .* mu_g[gg_2];
}
model {
  //Moods
  y ~ beta(a, b);
  //Policies for common questions
  z1 ~ bernoulli_logit(eta_1);
  //Policies all other questions
  for(gyp in 1:N_gyp){
    z2[gyp] ~ ordered_logistic(eta_2[gyp], tau[pp_3[gyp]]);
  }
  ////(Hyper-)Priors
  mu_a ~ normal(0, 1);
  mu_g ~ normal(0, 1);
  lambda ~ gamma(nu_lc[cc] + nu_lq[qq], 2);
  nu_lc ~ gamma(2, 2);
  nu_lq ~ gamma(2, 2);
  alpha ~ normal(nu_ac[cc] + nu_aq[qq], 2);
  nu_ac ~ normal(0, 1);
  nu_aq ~ normal(0, 1);
  sigma ~ gamma(2, 1);
  lambda2 ~ gamma(nu_l2c[cc_2] + nu_lp[pp], 2);
  nu_l2c ~ gamma(2, 2);
  nu_lp ~ gamma(2, 2);
  b ~ uniform(0, 10);
  for(p in 1:N_p){
    for(k in 1:(K-1)){
      tau[p][k] ~ normal(0,2);
    }
  }
}

