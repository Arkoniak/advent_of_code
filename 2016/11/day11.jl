using Setfield
# using IterTools

########################################
# Part 1
########################################
function initial_state(filename, ispart2 = false)
    res = []
    dict = Dict{String, Int}()
    if ispart2
        dict["elerium"] = 1
        dict["dilithium"] = 2
        push!(res, (; floor = 1, tid = 1, obj = 1))
        push!(res, (; floor = 1, tid = 2, obj = 1))
        push!(res, (; floor = 1, tid = 1, obj = 2))
        push!(res, (; floor = 1, tid = 2, obj = 2))
    end

    for (i, line) in enumerate(eachline(filename))
        i == 4 && break
        while true
            m = match(r"a ([a-z\-]+) (microchip|generator)(.*)", line)
            isnothing(m) && break
            if m[2] == "microchip"
                t = split(m[1], "-")[1]
            else
                t = m[1]
            end
            tid = get!(dict, t, length(dict) + 1)
            push!(res, (; floor = i, tid = tid, obj = m[2] == "microchip" ? 1 : 2))
            line = m[3]
        end
    end

    sort!(res, by = x -> (x.obj, x.tid))
    res = (map(x -> x.floor, res)..., 1)
    return res, dict
end

function isvalid(state)
    ms = length(state) >> 1
    for i in 1:ms
        m = state[i]
        out = true
        for k in ms+1:length(state) - 1
            state[k] == m || continue
            if k == i + ms
                out = true
                break
            else
                out = false
            end
        end
        out || return false
    end

    return true
end

function process!(memo, states, validated, state, n)
    if !haskey(validated, state)
        validated[state] = isvalid(state)
    end
    validated[state] || return
    get(memo, state, n+1) > n || return
    memo[state] = n
    push!(states, state)
    return
end

function part1(state0, statef)
    memo = Dict(statef => 0)
    states = [statef]
    validated = Dict(statef => true)

    while !isempty(states)
        state = popfirst!(states)
        n = memo[state] + 1
        k = length(state)
        for delta in (-1, +1)
            1 <= state[end] + delta <= 4 || continue
            # 1 item
            for i in 1:k-1
                state[i] == state[end] || continue
                state1 = @set state[end] += delta
                state1 = @set state1[i] += delta
                process!(memo, states, validated, state1, n)
            end
            # 2 items
            for i in 1:k-1
                state[i] == state[end] || continue
                for j in i+1:k-1
                    state[j] == state[end] || continue
                    state1 = @set state[end] += delta
                    state1 = @set state1[i] += delta
                    state1 = @set state1[j] += delta
                    process!(memo, states, validated, state1, n)
                end
            end
        end
    end

    return memo[state0]
end

function part1(filename)
    state0, dict = initial_state(filename)
    statef = ntuple(i -> 4, length(state0))

    part1(state0, statef)
end

println("Part 1: ", part1("input.txt"))

########################################
# Part 2
########################################
function part2(filename)
    state0, dict = initial_state(filename, true)
    statef = ntuple(i -> 4, length(state0))

    part1(state0, statef)
end

@time println("Part 2: ", part2("input.txt"))

########################################
# Misc
########################################
# using BenchmarkTools
# s = (1, 2, 3)
# function f(s)
#     s[1:end-1]
# end
# @btime f(Ref($s)[])
#
# let filename = "input.txt"
#     state0, dict = initial_state(filename, true)
#     statef = ntuple(i -> 4, length(state0))

#     @code_warntype part1(state0, statef)
# end

########################################
# Tests
########################################
module Tests
using ReTest
using Main: isvalid, isfinal
using Main: part1

@testset "Misc" begin
    @testset "Validity" begin
        state = (1, 1, 1, 1, 1)
        @test isvalid(state)

        state = (2, 1, 1, 1, 1)
        @test isvalid(state)

        state = (2, 1, 2, 1, 1)
        @test isvalid(state)

        state = (1, 1, 2, 1, 1)
        @test !isvalid(state)
    end
end

@testset "Part 1" begin
    @test part1("input_test.txt") == 11
end

@testset "Part 2" begin
end

end # module

Tests.runtests(Tests)
