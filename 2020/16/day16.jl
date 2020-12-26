########################################
# Part 1
########################################
function part1(file)
    s = split(read(file, String), "\n\n")
    fields = Vector{BitVector}()
    for x in readlines(IOBuffer(s[1]))
        m = match(r".*: (\d+)-(\d+) or (\d+)-(\d+)", x)
        m = parse.(Int, m.captures)
        y = falses(m[4])
        y[m[1]:m[2]] .= true
        y[m[3]:m[4]] .= true
        push!(fields, y)
    end

    tickets = split.(split(s[3], "\n", keepempty = false)[2:end], ",")
    res = 0
    for ticket in tickets
        ticket = parse.(Int, ticket)
        for t in ticket
            res += any(t > 0 && t <= length(field) && field[t] for field in fields) ? 0 : t
        end
    end
    
    return res
end

println("Part 1: ", part1("input.txt"))

########################################
# Part 2
########################################
function part2(file)
    s = split(read(file, String), "\n\n")
    fields = Vector{BitVector}()
    fieldnames = String[]
    for x in readlines(IOBuffer(s[1]))
        m1 = match(r"(.*): (\d+)-(\d+) or (\d+)-(\d+)", x)
        m = parse.(Int, m1.captures[2:end])
        y = falses(m[4])
        y[m[1]:m[2]] .= true
        y[m[3]:m[4]] .= true
        push!(fields, y)
        push!(fieldnames, m1[1])
    end

    myticket = parse.(Int, split(split(s[2], "\n")[2], ","))
    tickets = split.(split(s[3], "\n", keepempty = false)[2:end], ",")
    res = Vector{Vector{Int}}()
    for ticket in tickets
        ticket = parse.(Int, ticket)
        if all(any(t > 0 && t <= length(field) && field[t] for field in fields) for t in ticket)
            push!(res, ticket)
        end
    end

    vs = Vector{Vector{Int}}()
    for i in 1:length(fieldnames)
        push!(vs, map(x -> x[i], res))
    end

    xs = Vector{Vector{Int}}()
    for (j, v) in enumerate(vs)
        x = Int[]
        for (i, field) in enumerate(fields)
            if all(field[t] for t in v)
                push!(x, i)
            end
        end
        push!(xs, x)
    end
    
    ps = sortperm(xs, by = length)
    res = zeros(Int, length(fieldnames))
    for x in ps
        res[x] = only(setdiff(xs[x], res))
    end
    res, myticket, fieldnames

    deps = startswith.(fieldnames, "departure")
    fin = 1
    for (i, j) in enumerate(res)
        if deps[j]
            fin *= myticket[i]
        end
    end
    fin
end

println("Part 2: ", part2("input.txt"))

########################################
# Misc
########################################

########################################
# Tests
########################################
module Tests
using ReTest
# using Main: part1

@testset "Misc" begin
end

@testset "Part 1" begin
end

@testset "Part 2" begin
end

end # module

Tests.runtests(Tests)
