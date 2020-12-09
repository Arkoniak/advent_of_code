using ReTest
using DataStructures

########################################
# Part 1
########################################
ispairable(v, x) = any(y -> x - y ∈ v, v)

function part1(io, preamble = 25)
    data = Int[]
    buf = CircularBuffer{Int}(preamble)
    for line in eachline(io)
        x = parse(Int, line)
        push!(data, x)
        if length(buf) < preamble
            push!(buf, x)
            continue
        end
        if any(y -> x - y ∈ buf, buf)
            push!(buf, x)
        else
            return (; ans = x, data = data[1:end - 1])
        end
    end
    return (; ans = -1, data = data)
end

println("Part 1: ", part1(open("input.txt")).ans)

########################################
# Part 2
########################################

function part2(io, preamble = 25)
    ans, data = part1(io, preamble)
    stack = map(i -> (i, i+1, data[i] + data[i+1]), 1:length(data) - 1)
    while !isempty(stack)
        s, f, v = pop!(stack)
        f == length(data) && continue
        v > ans && continue
        v == ans && return sum(extrema(@view data[s:f]))
        push!(stack, (s, f + 1, v + data[f + 1]))
    end
    return -1
end

println("Part 2: ", part2(open("input.txt")))

########################################
# Tests
########################################

module Tests
using ..ReTest
using Main: ispairable, part1, part2

@testset "Day 09" begin
    @testset "Basic" begin
        v = [10, 4, 12]
        @test ispairable(v, 16)
        @test !ispairable(v, 50)
    end

    @testset "Part 1" begin
        res = part1(open("input_test.txt"), 5)
        @test res.ans == 127
        @test length(res.data) == 14
    end

    @testset "Part 2" begin
        @test part2(open("input_test.txt"), 5) == 62
    end
end

end # module

runtests(Tests)
