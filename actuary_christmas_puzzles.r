{which(LETTERS == "C") + which(LETTERS == "S")} / 2
which(LETTERS == "K")

{which(LETTERS == "J") + which(LETTERS == "V")} / 2
which(LETTERS == "P")

{which(LETTERS == "H") + which(LETTERS == "P")} / 2
which(LETTERS == "L")
# which(LETTERS == "W") == {which(LETTERS == "U") + which(LETTERS == "?")} / 2
# => which(LETTERS == "?") = 2 * which(LETTERS == "W") - which(LETTERS == "U")
2 * which(LETTERS == "W") - which(LETTERS == "U") # 25 = "Y"


{which(LETTERS == "U") + which(LETTERS == "Y")} / 2
which(LETTERS == "W")


# so day 1: ? = "Y"
