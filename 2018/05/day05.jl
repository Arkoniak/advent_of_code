using ReTest

function react(s)
    io = Char[]

    for c in s
        if isempty(io)
            push!(io, c)
            continue
        end
        if abs(io[end] - c) == 32
            pop!(io)
        else
            push!(io, c)
        end
    end

    return join(io)
end

readline("input.txt") |> react |> length |> x -> println("Part 1: ", x)

########################################
# Part 2
########################################
readline("input.txt") |> collect |> unique

function part2(s)
    mapreduce(min, 'a':'z') do c
        filter(x -> x ∉ (c, uppercase(c)), s) |> react |> length
    end
end

part2(readline("input.txt"))

########################################
# Tests
########################################
@testset "Reactions" begin
    @test isempty(react("abBA"))
    @test isempty(react("aA"))
    @test react("abAB") == "abAB"
    @test react("aabAAB") == "aabAAB"
    @test react("dabAcCaCBAcCcaDA") == "dabCBAcaDA"
end

runtests()
