############################
# Part 1
op(x, y) = x[1]*y[2] - x[2]*y[1]
sp(x, y) = x[1]*y[1] + x[2]*y[2]

function ev(a, j)
		res = 0
		b = ones(Int, size(a)[1])
		b[j] = 0
		for i in 1:length(b)
				res += (sum(a[:, i]) - 1)*b[i]
				b[a[:, i] .== 1] .= 0
		end

		res
end

function part1()
		data = readlines("input.txt")
		dd = hcat(collect.(data)...)
		asts = [[i[1], i[2]] for i in CartesianIndices(dd) if dd[i] == '#']

		dd1 = op.(asts, asts')
		dd2 = sp.(asts, asts')

		res = maximum([
				 length(asts) - 1 - ev(map(x -> Int(x > 0), dd2 .- dd2[i, :]' .- dd2[:, i] .+ dd2[i, i]) .*
				map(x -> Int(x == 0), dd1 .- dd1[i, :]' .- dd1[: , i]), i) for i in 1:length(asts)])

		println("Part 1: ", res)
end

part1()

###################################33
# Part 2

function part2()
    data = readlines("input.txt")
    dd = hcat(collect.(data)...)
    asts = [[i[1], i[2]] for i in CartesianIndices(dd) if dd[i] == '#']

    id = 266
    ox = [0, 1]
    oy = [-1, 0]

    asts2 = [[a[1] - asts[id][1], a[2] - asts[id][2]] for a in asts]
    asts3 = [[sp(a, ox), sp(a, oy)] for a in asts2]
    asts4 = [[a[1]^2 + a[2]^2, atand(a[2], a[1]) + 180] for a in asts3]
    asts4 = [[a[1], a[2] == 360.0 ? 0.0 : a[2]] for a in asts4]
    
    p1 = sortperm(asts4, lt = (x, y) -> x[2] < y[2] || (x[2] == y[2] && x[1] < y[1]))
    asts5 = foldl((x, y) -> push!(x, x[end][1] == y[2] ? [y[2], x[end][2] + 1] : [y[2], 1]), asts4[p1]; init = [[asts4[p1][1][2], 0]])[2:end]
    p2 = sortperm(asts5, lt = (x, y) -> x[2] < y[2] || (x[2] == y[2] && x[1] < y[1]))

    z = asts[p1][p2][200]
    println("Part 2: ", (z[1] - 1)*100 + (z[2] - 1))
end

part2()
