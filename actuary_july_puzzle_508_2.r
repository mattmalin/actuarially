################################################################################
# New approach: start small!
################################################################################
looped_possible <- list()
i <- 1

max_n_value <- 10
max_o_value <- 20

possible_n <- 1:max_n_value
for(n_value in possible_n) {
  possible_o <- {n_value + 1}:max_o_value  # includes that o > n)
  for(o_value in possible_o) {
    m_value <- n_value + o_value
	q_value <- m_value + o_value
	l_value <- m_value + q_value
	if(any(duplicated(c(n_value, o_value, m_value, q_value, l_value)))) next
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
	    message("n: ", n_value, ", o: ", o_value, ", s: ", s_value, ", p: ", p_value)
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
        if(any(duplicated(c(
          a = a_value, 
          b = b_value, 
          c = c_value, 
          d = d_value, 
          e = e_value, 
          f = f_value, 
          g = g_value, 
          h = h_value, 
          i = i_value, 
          j = j_value, 
          k = k_value, 
          l = l_value, 
          m = m_value, 
          n = n_value, 
          o = o_value, 
          p = p_value, 
          q = q_value, 
          r = r_value, 
          s = s_value, 
          t = t_value, 
          u = u_value)))) next
		if(a_value + b_value + c_value != u_value + t_value + r_value) next
		if(b_value + c_value != k_value + u_value) next
		if(b_value + t_value != k_value + e_value + j_value) next
		looped_possible[[i]] <- c(
          a = a_value, 
          b = b_value, 
          c = c_value, 
          d = d_value, 
          e = e_value, 
          f = f_value, 
          g = g_value, 
          h = h_value, 
          i = i_value, 
          j = j_value, 
          k = k_value, 
          l = l_value, 
          m = m_value, 
          n = n_value, 
          o = o_value, 
          p = p_value, 
          q = q_value, 
          r = r_value, 
          s = s_value, 
          t = t_value, 
          u = u_value)
	    print(looped_possible[[i]])
		i <- i + 1
	  }
	}
  }
}

print(looped_possible)
