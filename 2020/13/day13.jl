########################################
# Part 1
########################################
using Underscores

function part1(file)
    input = readlines(file)
    n = parse(Int, input[1])
    x = @_ split(input[2], ",") |> filter(_ != "x", __) |> parse.(Int, __)
    w, id = findmin(cld.(n, x) .* x .- n)
    w * x[id]
end

println("Part 1: ", part1("input.txt"))

########################################
# Part 2
########################################
function solve(v)
    x = v[1][1]
    offset = x
    for i in 2:length(v)
        y, d = v[i]
        r = mod(-d - offset, y)
        offset = mod(invmod(x, y)*r, y) * x + offset
        x = lcm(x, y)
    end

    return offset
end

function solve(v::T) where {T <: AbstractString}
    res = []
    for (i, x) in enumerate(split(v, ","))
        x == "x" && continue
        push!(res, (parse(Int, x), i - 1))
    end

    return solve(identity.(res))
end

println("Part 2: ", solve(readlines("input.txt")[2]))

########################################
# Misc
########################################

part1("input_test.txt")

########################################
# Tests
########################################
module Tests
using ReTest
using Main: part1, solve

@testset "Misc" begin
end

@testset "Part 1" begin
end

@testset "Part 2" begin
    @test solve([(5, 0), (7, 1), (11, 3)]) == 195
    @test solve([(17, 0), (13, 2), (19, 3)]) == 3417
    @test solve("67,7,59,61") == 754018
    @test solve("67,x,7,59,61") == 779210
    @test solve("67,7,x,59,61") == 1261476
    @test solve("1789,37,47,1889") == 1202161486
    @test solve("7,13,x,x,59,x,31,19") == 1068781
end

end # module

Tests.runtests(Tests)
