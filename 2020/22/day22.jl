########################################
# Part 1
########################################
function get_decks(file)
    p1, p2 = split(read(file, String), "\n\n")
    d1 = parse.(Int, split(p1, "\n", keepempty = false)[2:end])
    d2 = parse.(Int, split(p2, "\n", keepempty = false)[2:end])
    d1, d2
end

function shag!(d1, d2)
    c1, c2 = popfirst!(d1), popfirst!(d2)
    if c1 > c2
        push!(d1, c1)
        push!(d1, c2)
    else
        push!(d2, c2)
        push!(d2, c1)
    end

    d1, d2
end

score(d) = isempty(d) ? 0 : sum(d .* (length(d):-1:1))

let file = "input.txt"
    d1, d2 = get_decks(file)
    while !isempty(d1) & !isempty(d2)
        shag!(d1, d2)
    end
    score(d1) + score(d2)
end

let
    d = []
    score(d)
end

########################################
# Part 2
########################################

########################################
# Misc
########################################

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
