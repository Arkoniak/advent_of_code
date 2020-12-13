using Transducers
using Underscores

########################################
# Part 1
########################################
function part1(filename)
    v = sort(vcat(parse.(Int, readlines(filename)), 0))
    x1 = count(==(1), v[2:end] - v[1:end-1])
    x2 = count(==(3), v[2:end] - v[1:end-1])
    x1*(x2 + 1)
end

println("Part 1: ", part1("input.txt"))

########################################
# Part 2
########################################
function part2(filename)
    v = sort(vcat(parse.(Int, readlines(filename)), 0))
    v = v[2:end] - v[1:end - 1]
    comb(x, y) = begin
        if x[end][1] == y
            x[end] = (y, x[end][2] + 1)
        else
            push!(x, (y, 1))
        end
        x
    end
    @_ foldl(comb, v; init = [(1, 0)]) |> filter(_[1] == 1, __) |>
        mapreduce(_[2] == 1 ? 1 : 
                     _[2] == 2 ? 2 : 
                        _[2] == 3 ? 4 :
                            _[2] == 4 ? 7 : Inf, *, __)
end

println("Part 2: ", part2("input.txt"))

########################################
# Misc
########################################
let v = sort(vcat(parse.(Int, readlines("input_test1.txt")), 0))
    @assert count(==(2), v[2:end] - v[1:end-1]) == 0
    v2 = v[2:end] - v[1:end - 1]
    println(v)
    println(v2)
    c = 0
    x0 = 1
    d = 0
    for x in v2
        if x == 1
            if x0 == 1
                d += 1
            else
                x0 = 1
                d = 1
            end
        else
            if x0 == 1
                x0 = 3
                c = max(d, c)
            end
        end
    end
    c
end
let v = sort(vcat(parse.(Int, readlines("input.txt")), 0))
    v = v[2:end] - v[1:end - 1]
    comb(x, y) = begin
        if x[end][1] == y
            x[end] = (y, x[end][2] + 1)
        else
            push!(x, (y, 1))
        end
        x
    end
    @_ foldl(comb, v; init = [(1, 0)]) |> filter(_[1] == 1, __) |>
        mapreduce(_[2] == 1 ? 1 : 
                     _[2] == 2 ? 2 : 
                        _[2] == 3 ? 4 :
                            _[2] == 4 ? 7 : Inf, *, __)
end

let v = [0, 1, 2, 3, 4, 7]
    v[2:end] - v[1:end-1]
end

########################################
# Tribonacci
########################################

let
    a1 = (19 + 3*sqrt(33))^1/3
    a2 = (19 - 3*sqrt(33))^1/3
    b = (586 + 102*sqrt(33))^1/3

    tri(n) = 3b*(1/3*(a1 + a2 + 1))^n/(b^2 - 2b + 4) |> round |> Int
    foreach(n -> println("n: ", n, " tri(n): ", tri(n)), 1:10)
end

########################################
# Tests
########################################

module Tests
using ReTest
using Main: part1

@testset "Day 10" begin
    @testset "Part 1" begin
        @test part1("input_test1.txt") == 35
        @test part1("input_test2.txt") == 220
    end
end

end # module

Tests.runtests(Tests)
