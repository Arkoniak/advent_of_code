inside1(t1, t2) = t1[1] >= t2[1] && t1[2] <= t2[2]
inside(t1, t2) = inside1(t1, t2) || inside1(t2, t1)
tot(s) = tuple(parse.(Int, split(s, "-"))...)

# Part 1
sum(line -> inside(tot.(split(line, ","))...), eachline("input.txt"))

# Part 2
overlap(t1, t2) = !((t1[2] < t2[1]) || (t1[1] > t2[2]))
sum(line -> overlap(tot.(split(line, ","))...), eachline("input.txt"))
