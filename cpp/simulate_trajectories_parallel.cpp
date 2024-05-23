#include <Rcpp.h>
#include <omp.h>
using namespace Rcpp;

// Function for the underlying mathematical model
// [[Rcpp::export]]
NumericVector simulate_trajectory_cpp(
    int t_max, double t0_ind, double tp_ind, double ts_ind, double m1_ind, double m2_ind, double m3_ind) {
  NumericVector mu(t_max + 1);

#pragma omp parallel for
  for(int t = 0; t <= t_max; t++) {
    double value;
    if(t <= tp_ind) {
      value = t0_ind + m1_ind * t;
    } else if(t <= ts_ind) {
      value = t0_ind + m1_ind * tp_ind + m2_ind * (t - tp_ind);
    } else {
      value = t0_ind + m1_ind * tp_ind + m2_ind * (ts_ind - tp_ind) + m3_ind * (t - ts_ind);
    }
    mu[t] = std::max(value, 0.0);  // Ensure non-negative value
  }
  return mu;
}

// [[Rcpp::export]]
DataFrame simulation_wrapper_cpp(const DataFrame &person_params) {
  // Extract parameters from DataFrame
  IntegerVector stan_id = person_params["stan_id"];
  NumericVector t0_ind = person_params["t0_ind"];
  NumericVector tp_ind = person_params["tp_ind"];
  NumericVector ts_ind = person_params["ts_ind"];
  NumericVector m1_ind = person_params["m1_ind"];
  NumericVector m2_ind = person_params["m2_ind"];
  NumericVector m3_ind = person_params["m3_ind"];
  IntegerVector t_max_individual = person_params["t_max"];
  IntegerVector titre_type = person_params["titre_type"];
  IntegerVector draw = person_params["draw"];

  int total_size = 0;
  for(int p = 0; p < stan_id.size(); p++) {
    total_size += t_max_individual[p] + 1;
  }

  // Preallocate vectors with the total size
  std::vector<int> out_id(total_size), out_titre_type(total_size), out_draw(total_size), out_t(total_size);
  std::vector<double> out_mu(total_size);

#pragma omp parallel for
  for(int p = 0; p < stan_id.size(); p++) {
    int curr_id = stan_id[p];
    int curr_titre_type = titre_type[p];
    int curr_draw = draw[p];
    int t_max = t_max_individual[p];

    NumericVector mu = simulate_trajectory_cpp(
      t_max, t0_ind[p], tp_ind[p], ts_ind[p], m1_ind[p], m2_ind[p], m3_ind[p]);

    int start_index = 0;
    for(int i = 0; i < p; i++) {
      start_index += t_max_individual[i] + 1;
    }

    for(int t = 0; t <= t_max; t++) {
      int index = start_index + t;
      out_id[index] = curr_id;
      out_titre_type[index] = curr_titre_type;
      out_draw[index] = curr_draw;
      out_t[index] = t;
      out_mu[index] = mu[t];
    }
  }

  return DataFrame::create(_["stan_id"] = out_id,
                           _["titre_type_num"] = out_titre_type,
                           _["draw"] = out_draw,
                           _["t"] = out_t,
                           _["mu"] = out_mu);
}