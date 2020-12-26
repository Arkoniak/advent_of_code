########################################
# Part 1
########################################
import Base: +

const Cell = Tuple{Int, Int, Int}
Base.:+(c1::Cell, c2::Cell) = c1 .+ c2

const NEIGHB = [(i, j, k) for i in -1:1 for j in -1:1 for k in -1:1 if (i, j, k) != (0, 0, 0)]

function get_data(file)
    cells = Set{Cell}()
    for (i, s) in enumerate(eachline(file))
        for (j, c) in enumerate(s)
            c == '#' && (push!(cells, (i, j, 0)))
        end
    end
    cells
end

function shag(c1::Set{T}, neighb = NEIGHB) where T
    c2 = Set{T}()
    for c in c1
        n = Ref(c) .+ neighb
        nb = length(intersect(n, c1))
        2 <= nb <= 3 && push!(c2, c)
        for nc in n
            nc in c1 && continue
            nc in c2 && continue
            n2 = Ref(nc) .+ neighb
            nb = length(intersect(n2, c1))
            nb == 3 && push!(c2, nc)
        end
    end
    c2
end

let file = "input.txt", neighb = NEIGHB
    c1 = get_data(file)
    for _ in 1:6
        c1 = shag(c1, neighb)
    end

    println("Part 1: ", length(c1))
end

########################################
# Part 2
########################################
const Cell4 = Tuple{Int, Int, Int, Int}
Base.:+(c1::Cell4, c2::Cell4) = c1 .+ c2

const NEIGHB4 = [(i, j, k, m) for i in -1:1 for j in -1:1 for k in -1:1 for m in -1:1 if (i, j, k, m) != (0, 0, 0, 0)]

function get_data4(file)
    cells = Set{Cell4}()
    for (i, s) in enumerate(eachline(file))
        for (j, c) in enumerate(s)
            c == '#' && (push!(cells, (i, j, 0, 0)))
        end
    end
    cells
end

let file = "input.txt", neighb = NEIGHB4
    c1 = get_data4(file)
    for _ in 1:6
        c1 = shag(c1, neighb)
    end

    println("Part 2: ", length(c1))
end

########################################
# Misc
########################################

using BenchmarkTools
let file = "input.txt", neighb = NEIGHB4
    c1 = get_data4(file)

    function f(c)
        for _ in 1:6
            c = shag(c, neighb)
        end
        return length(c)
    end
    @btime $f($c1)
end



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
