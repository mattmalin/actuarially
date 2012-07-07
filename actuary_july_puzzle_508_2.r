################################################################################
# New approach: start small!
################################################################################

# The approach taken was to calculate a few relationships (instead of working out 
# all necessary equations, will work from a few and attempt to find working solutions.

# Although there can be probably be an exhaustive set of calculations leading to 
# some form of order of size, any assumptions on size are based on adjacent squares 
# and following lines directly, not from comparing printed sizes, so may be missing out
# on easily excluding some more impossible values this way.

# Looking at the square and coming up with a few rules hoping to be exhaustive enough:

# * Unique integer values
# * o > n
# * m = n + o
# * q = m + o
# * l = m + q
# * s = 0.25 * q 
#   => 4 | q (as all values are integers) 
#   => 4 | m + o = (n + o) + o = n + 2o 
#   => 4 | n + 2o => 2 | n (i.e. n is even)
#  Derivation: 
#   * t + s = l + q => (u + s) + s = l + q => u + 2s = l + q => (k + s) + 2s = l + q
#     => k + 3s = l + q => (l + s) + 3s = l + q => l + 4s = l + q => 4s = q)
# * k = l + s
# * 

# First decide on maximum values to take across our loops 
# Using loops here, despite inefficiency, for ease of understanding):
max_n_value <- 50
max_o_value <- 100

# Now set up the set of possible values for our first guess (in this case, square n)
possible_n <- 1:max_n_value

# Instead of stopping the loop, will save the combinations in a list outside the loop: 
looped_possible <- list()
i <- 1

# Now cycle through the possible values for n
for(n_value in possible_n) {
  possible_o <- {n_value + 1}:max_o_value  # includes that o > n)
  for(o_value in possible_o) {
    m_value <- n_value + o_value
	q_value <- m_value + o_value
	l_value <- m_value + q_value
	if(any(duplicated(c(n_value, o_value, m_value, q_value, l_value)))) next
	s_value <- 
	possible_s <- 1:{l_value - 1} # includes that s < l
	possible_s <- possible_s[!{possible_s %in% c(n_value, o_value, m_value, q_value, l_value)}] # uniques
	if(length(possible_s) == 0) next
	for(s_value in possible_s) {
	  k_value <- l_value + s_value
	  u_value <- k_value + s_value
	  t_value <- u_value + s_value
      if(any(duplicated(c(n_value, o_value, m_value, q_value, l_value, s_value, k_value, u_value, t_value)))) next
	  possible_p <- {o_value + 1}:{l_value - n_value - 1} # as o < p < l - n
	  possible_p <- possible_p[!{possible_p %in% c(n_value, o_value, m_value, q_value, l_value, s_value, k_value, u_value, t_value)}] # uniques
	  if(length(possible_p) == 0) next
	  for(p_value in possible_p) {
	    r_value <- l_value + t_value - n_value - p_value
		j_value <- r_value - p_value
		i_value <- j_value - p_value
		g_value <- q_value + p_value - i_value - n_value - m_value
		f_value <- g_value - n_value
		h_value <- g_value - i_value
		e_value <- i_value + j_value - h_value
		d_value <- e_value - h_value
		c_value <- d_value + e_value
		b_value <- c_value + d_value
		a_value <- b_value + f_value
		
        all_values <- c(
		  a = a_value, b = b_value, c = c_value, d = d_value, e = e_value, 
		  f = f_value, g = g_value, h = h_value, i = i_value, j = j_value, 
          k = k_value, l = l_value, m = m_value, n = n_value, o = o_value, 
          p = p_value, q = q_value, r = r_value, s = s_value, t = t_value, 
		  u = u_value)
		  
		if(any(duplicated(all_values))) next
		if(a_value + b_value + c_value != u_value + t_value + r_value) next
		if(b_value + c_value != k_value + u_value) next
		if(b_value + t_value != k_value + e_value + j_value) next
		
		looped_possible[[i]] <- all_values
		  
	    print(looped_possible[[i]])
		i <- i + 1
	  }
	}
  }
}

print(looped_possible)
saveRDS(looped_possible, "c:/matt/temp/looped_possible.rds")
