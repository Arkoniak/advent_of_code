###########################3
# Part 1

using Combinatorics

cd(dirname(@__FILE__))
struct Knights
    nodes::Dict{String, Int}
    edges::Dict{Tuple{Int, Int}, Int}
end

function Knights(ss)
    nodes = Dict()
    edges = Dict()
    k = 0
    for s in ss
        m = match(r"^([^ ]+) would (gain|lose) ([0-9]+) happiness units by sitting next to (.*).$", s)
        if !(m[1] in keys(nodes))
            k += 1
            nodes[m[1]] = k
        end
        if !(m[4] in keys(nodes))
            k += 1
            nodes[m[4]] = k
        end
        d = m[2] == "gain" ? parse(Int, m[3]) : -parse(Int, m[3])
        edges[(nodes[m[1]], nodes[m[4]])] = d
    end
    Knights(nodes, edges)
end

function happiness(knights, perm)
    perm = vcat([perm[end]], perm, [perm[1]])
    h = 0
    for i in 2:(length(perm) - 1)
        h += knights.edges[(perm[i], perm[i - 1])]
        h += knights.edges[(perm[i], perm[i + 1])]
    end
    h
end

function find_part1()
    knights = Knights(readlines("input.txt"))
    res = 0
    for x in permutations(1:8)
        h = happiness(knights, x)
        if h > res res = h end
    end
    res
end

function part1()
    println("Part 1: ", find_part1())
end

part1()

###################################
# Part 2

function find_part2()
    knights = Knights(readlines("input.txt"))
    knights.nodes["Me"] = 9
    for i in 1:8
        knights.edges[(i, 9)] = 0
        knights.edges[(9, i)] = 0
    end
    res = 0
    for x in permutations(1:9)
        h = happiness(knights, x)
        if h > res res = h end
    end
    res
end

function part2()
    println("Part 2: ", find_part2())
end

part2()
