########################################
# Part 1
########################################
function part1(s, m = 2020)
    d = Dict{Int, Tuple{Int, Int}}()
    idx = 0
    last = -1
    for k in parse.(Int, split(s, ","))
        idx += 1
        d[k] = (idx, idx)
        last = k
    end
    while true
        idx += 1
        if d[last][1] == d[last][2]
            last = 0
        else
            last = d[last][1] - d[last][2]
        end
        pos = get(d, last, (idx, idx))
        d[last] = (idx, pos[1])
        idx == m && return last
    end
end

println("Part 1: ", part1(readline("input.txt"), 2020))

########################################
# Part 2
########################################
println("Part 2: ", part1(readline("input.txt"), 30000000))

########################################
# Misc
########################################
using BenchmarkTools

@benchmark part1(readline("input.txt"), 30_000_000)

########################################
# Tests
########################################
module Tests
using ReTest
using Main: part1

@testset "Misc" begin
end

@testset "Part 1" begin
    @test part1("0,3,6", 4) == 0
    @test part1("0,3,6", 5) == 3
    @test part1("0,3,6", 6) == 3
    @test part1("0,3,6", 7) == 1
    @test part1("0,3,6", 8) == 0
    @test part1("0,3,6", 9) == 4
    @test part1("0,3,6", 10) == 0
    @test part1("0,3,6", 2020) == 436

    @test part1("1,3,2", 2020) == 1
    @test part1("3,1,2", 2020) == 1836

end

@testset "Part 2" begin
end

end # module

Tests.runtests(Tests)
