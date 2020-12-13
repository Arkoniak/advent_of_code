using ReTest
using DataStructures
using Setfield
using UnPack

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
    x = (; s = length(data) - 1, f = length(data), v = 0)
    @set! x.v = data[x.s] + data[x.f]
    while true
        x.v == ans && return sum(extrema(@view data[x.s:x.f]))
        x.s == 1 && x.f == 2 && return -1
        if x.s == 1 || x.v > ans
            @set! x.f -= 1
            @set! x.s = x.f - 1
            @set! x.v = data[x.s] + data[x.f]
            continue
        end
        @set! x.s -= 1
        @set! x.v += data[x.s]
    end
end

println("Part 2: ", part2(open("input.txt")))

# Backward search
function part2a(ans, data)
    x = (; s = length(data) - 1, f = length(data), v = 0)
    @set! x.v = data[x.s] + data[x.f]
    while true
        x.v == ans && return sum(extrema(@view data[x.s:x.f]))
        x.s == 1 && x.f == 2 && return -1
        if x.s == 1 || x.v > ans
            @set! x.f -= 1
            @set! x.s = x.f - 1
            @set! x.v = data[x.s] + data[x.f]
            continue
        end
        @set! x.s -= 1
        @set! x.v += data[x.s]
    end
end

function part2c(ans, data)
    x = (; s = length(data) - 1, f = length(data), v = 0)
    @set! x.v = data[x.s] + data[x.f]
    while true
        x.v == ans && return sum(extrema(@view data[x.s:x.f]))
        x.s == 1 && x.f == 2 && return -1
        if x.v > ans
            @set! x.v -= data[x.f]
            @set! x.f -= 1
        else
            @set! x.s -= 1
            @set! x.v += data[x.s]
        end
    end
end


# Forward search
function part2b(ans, data)
    x = (; s = 1, f = 2, v = data[1] + data[2])
    l = length(data)
    while true
        x.v == ans && return sum(extrema(@view data[x.s:x.f]))
        x.s == l - 1  && x.f == l && return -1
        if x.f == l || x.v > ans
            @set! x.s += 1
            @set! x.f = x.s + 1
            @set! x.v = data[x.s] + data[x.f]
            continue
        end
        @set! x.f += 1
        @set! x.v += data[x.f]
    end
end

using BenchmarkTools

x = part1(open("input.txt"))

@btime part2a($x.ans, $x.data)
# 656.344 ns (0 allocations: 0 bytes)
@btime part2b($x.ans, $x.data)
# 79.229 μs (0 allocations: 0 bytes)
@btime part2c($x.ans, $x.data)
# 268.846 ns (0 allocations: 0 bytes)

########################################
# Tests
########################################

module Tests
using ..ReTest
using Main: ispairable, part1, part2, part2a, part2b, part2c

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

    @testset "Part 2 4 benchmark" begin
        ans, data = part1(open("input_test.txt"), 5)
        @test part2a(ans, data) == 62
        @test part2b(ans, data) == 62
        @test part2c(ans, data) == 62
    end
end

end # module

runtests(Tests)
