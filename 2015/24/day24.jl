using ReTest
using Underscores

get_data(input) = parse.(Int, readlines(input))

function part1(data, n)
    val = sum(data) ÷ n
    cs::Vector{Vector{Int}} = Vector{Int}[]
    queue = [Int[]]
    while !isempty(queue)
        m2 = isempty(cs) ? typemax(Int) : minimum(length.(cs))
        v = popfirst!(queue)
        length(v) >= m2 && continue
        s = isempty(v) ? 0 : sum(v)
        m1 = isempty(v) ? 0 : maximum(v)
        for x in data
            x <= m1 && continue
            s2 = s + x
            s2 > val && continue
            if s2 == val
                push!(cs, vcat(copy(v), x))
            else
                push!(queue, vcat(copy(v), x))
            end
        end
    end

    let m = minimum(length.(cs)), cs = filter(x -> length(x) == m, cs)
        cs = prod.(cs)
        return minimum(cs)
    end
end

@_ get_data("input.txt") |> part1(__, 3)
@_ get_data("input.txt") |> part1(__, 4)

@testset "day 23" begin
input = """1
2
3
4
5
7
8
9
10
11
"""
    @testset "getdata" begin
        @test sum(get_data(IOBuffer(input))) == 60
    end

    @testset "part1" begin
        data = get_data(IOBuffer(input))
        @test part1(data) == 99
    end
end

runtests()
