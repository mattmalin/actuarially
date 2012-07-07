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
# * u = k + s
# * t = u + s
# * o < p < l - n
# * r = l + t - n -p
# * j = r - p
# * i = j - p
# * g = q + p - i - n - m
# * f = g - n
# * h = g - i
# * e = i + j - h
# * d = e - h
# * c = d + e
# * b = c + d
# * a = b + f
#
# Then do a few checks that the total length of each side is consistent:
#
# * a + b + c = u + t + r
# * b + c = k + u
# * b + t = k + e + j
#
# Also, the total area of the small squares should sum the total area:
# * (a + b + c)^2 = a^2 + b^2 + c^2 + d^2 + e^2 + f^2 + g^2 + h^2 + i^2 + j^2 +
#                   k^2 + l^2 + m^2 + n^2 + o^2 + p^2 + q^2 + r^2 + s^2 + t^2 + u^2


#
# First decide on maximum values to take across our loops 
# Using loops here, despite inefficiency, for ease of understanding):
max_nn <- 20
max_oo <- 100

# Now set up the set of possible values for our first guess (in this case, square n)
possible_nn <- 2 * 1:{max_nn / 2}

# Instead of stopping the loop, will save the combinations in a list outside the loop: 
possible_solutions <- list()
i <- 1

# Now cycle through the possible values for n
for(nn in possible_nn) {
  possible_oo <- {nn + 1}:max_oo  # includes that o > n)
  for(oo in possible_oo) {
    mm <- nn + oo
	qq <- mm + oo
	if({qq %% 4} != 0) next
	ll <- mm + qq
	if(any(duplicated(c(nn, oo, mm, qq, ll)))) next
	ss <- qq / 4
	kk <- ll + ss
	uu <- kk + ss
	tt <- uu + ss
    if(any(duplicated(c(nn, oo, mm, qq, ll, ss, kk, uu, tt)))) next
	possible_p <- {oo + 1}:{ll - nn - 1} 
	possible_p <- possible_p[!{possible_p %in% c(nn, oo, mm, qq, ll, ss, kk, uu, tt)}] #uniques
	if(length(possible_p) == 0) next
	for(pp in possible_p) {
	  rr <- ll + tt - nn - pp
	  jj <- rr - pp
	  ii <- jj - pp
	  gg <- qq + pp - ii - nn - mm
	  ff <- gg - nn
	  hh <- gg - ii
	  ee <- ii + jj - hh
	  dd <- ee - hh
	  cc <- dd + ee
	  bb <- cc + dd
	  aa <- bb + ff
	  
      all_values <- c(
	    a = aa, b = bb, c = cc, d = dd, e = ee, f = ff, g = gg, h = hh, i = ii,
		j = jj, k = kk, l = ll, m = mm, n = nn, o = oo, p = pp, q = qq, r = rr,
		s = ss, t = tt, u = uu)
	    
	  if(any(duplicated(all_values))) next
	  if(aa + bb + cc != uu + tt + rr) next
	  if(bb + cc != kk + uu) next
	  if(bb + tt != kk + ee + jj) next

	  if({aa + bb + cc}^2 != sum(all_values^2)) next
	  
	  # if it's reached here, suits all of defined criteria (so possibly a solution)
	  possible_solutions[[i]] <- all_values
	    
	  print(possible_solutions[[i]])
	  i <- i + 1
	}
  }
}

print(possible_solutions)
saveRDS(possible_solutions, "c:/matt/temp/possible_solutions.rds")
possible_solutions <- readRDS("c:/matt/temp/possible_solutions.rds")
