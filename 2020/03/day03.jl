using ReTest
using CircularArrays
using Underscores

get_data(filename) = @_ readlines(filename) .|> 
    collect .|> (_ .== '#') |> 
    hcat(__...) |> CircularArray

get_data(filename) = mapreduce(collect, hcat, eachline(filename)) |> CircularArray 

data = get_data("input.txt")

part1(data, slope = (3, 1)) = count(1:size(data, 2) ÷ slope[2]) do i
    idx = (1, 1) .+ slope .* i
    data[idx...] == '#'
end

@_ get_data("input.txt") |> part1 |> println("Part 1 answer: ", __)

part2(data) = @_ [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)] |> 
    part1.(Ref(data), __) |> prod

@_ get_data("input.txt") |> part2 |> println("Part 2 answer: ", __)

runtests()

@testset "basic" begin
    data = get_data("input_test.txt")
    @test part1(data) == 7

    @test part2(data) == 336
end
