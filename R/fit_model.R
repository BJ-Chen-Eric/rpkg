#' @title fit_model
#' @description fit_model
#'
#' @param formula formula
#' @param data data
#'
#' @return model
#' @details DETAILS
#' 
#' @seealso 
#'  \code{\link[lme4]{glmer}}
#'  \code{\link[sjPlot]{tab_model}}
#' @rdname fit_models
#' @export 
#' @import dplyr
#' @importFrom lme4 glmer
#' @importFrom sjPlot tab_model
fit_models <- function(formula, data) {
  data <- data %>% rename('offset'='tj')
  fit.glmm <- lme4::glmer(formula, 
                          data = data, 
                          family = "poisson", 
                          offset = log(offset))
  
  sjPlot::tab_model(fit.glmm)
  # Example usage of rlang::abort
  # rlang::abort("An error occurred")
  
  
}

# example of how to use the function
# fit_models(formula = y ~ trt*post + (1|subject), data = df_epil)
