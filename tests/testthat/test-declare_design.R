context("Check that basic code works")

rm(list=ls())

test_that("1=1", {
  
  
  # Examples
  design_1 <- declare_design(N = 100, m=50)
  design_1
  design_1$ra_fun()
  
  design_2 <- declare_design(N = 100, m_each = c(30, 40, 30), 
                             condition_names=c("control", "placebo", "treatment"))
  design_2
  design_2$ra_fun()
  
  block_var <- rep(c("A", "B","C"), times=c(50, 100, 200))
  design_3 <- declare_design(block_var = block_var)
  design_3
  design_3$ra_fun()
  
  block_m <- rbind(c(10, 20, 20),
                   c(30, 50, 20),
                   c(50, 75, 75))
  
  design_4 <- declare_design(block_var = block_var, block_m = block_m)
  design_4
  design_4$ra_fun()
  
  
  clust_var <- rep(letters, times=1:26)
  design_5 <- declare_design(clust_var = clust_var)
  design_5
  design_5$ra_fun()
  
  design_6 <- declare_design(clust_var=clust_var, m_each=c(7, 7, 12),
                             condition_names=c("control", "placebo", "treatment"))
  design_6
  design_6$ra_fun()
  
  
  cluster_ra(clust_var=clust_var, m_each=c(7, 7, 12),
             condition_names=c("control", "placebo", "treatment"))
  
  clust_var <- rep(letters, times=1:26)
  block_var <- rep(rep(1:13, each=2), times=1:26)
  design_7 <- declare_design(clust_var=clust_var, block_var = block_var, num_arms=4)
  design_7
  design_7$ra_fun()
  
  
})
