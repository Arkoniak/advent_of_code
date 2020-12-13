########################################
# Part 1
########################################
using StaticArrays

const ROT = let R = SA[0 -1; 1 0]
    Dict(
    "L90" => R,
    "L180" => R * R,
    "L270" => -R,
    "R90" => -R,
    "R180" => R * R,
    "R270" => R,
)
end

const SHIFT = Dict(
    'E' => SA[1, 0],
    'N' => SA[0, 1],
    'W' => SA[-1, 0],
    'S' => SA[0, -1],
)

function part1(file, rots = ROT, shift = SHIFT)
    pos = SA[0, 0]
    dir = SA[1, 0]
    for line in eachline(file)
        if (line[1] == 'R') | (line[1] == 'L')
            dir = rots[line] * dir
        elseif line[1] == 'F'
            n = parse(Int, line[2:end])
            pos += n * dir
        else
            n = parse(Int, line[2:end])
            pos += n * shift[line[1]]
        end
    end
    abs(pos[1]) + abs(pos[2])
end

println("Part 1: ", part1("input.txt"))

########################################
# Part 2
########################################
function part2(file, rots = ROT, shift = SHIFT)
    pos = SA[0, 0]
    wp = SA[10, 1]
    for line in eachline(file)
        if (line[1] == 'R') | (line[1] == 'L')
            wp = rots[line] * wp
        elseif line[1] == 'F'
            n = parse(Int, line[2:end])
            pos += n * wp
        else
            n = parse(Int, line[2:end])
            wp += n * shift[line[1]]
        end
    end
    abs(pos[1]) + abs(pos[2])
end

println("Part 2: ", part2("input.txt"))

########################################
# Misc
########################################
data = readlines("input.txt")
filter(x -> x[1] in ('R', 'L'), data) |> unique |> sort
 "L180"
 "L270"
 "L90"
 "R180"
 "R270"
 "R90"

 [[0, 1] [1, 0]] * (1, 0)
#
R = SA[0 -1; 1 0]
-R
R * R * R * SA[1, 0]
SA[0 -1; 1 0] * SA[1, 0]

SA[0 1; 1 0] * SA[1, 0]
SVector(1, 0)

SA[1, 0] + SA[1, 10]
 
########################################
# Tests
########################################
module Tests
using ReTest
using Main: part1, part2

@testset "Misc" begin
end

@testset "Part 1" begin
    @test part1("input_test.txt") == 25
end

@testset "Part 2" begin
    @test part2("input_test.txt") == 286
end

end # module

Tests.runtests(Tests)
