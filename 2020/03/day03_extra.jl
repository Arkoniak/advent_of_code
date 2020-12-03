using ReTest
using CircularArrays
using Underscores

get_data(filename) = @_ readlines(filename) .|> 
    collect .|> (_ .== '#') |> 
    hcat(__...) |> CircularArray

part1(data, slope = (3, 1)) = count(1:size(data, 2)) do i
    idx = (1, 1) .+ slope .* i
    data[idx...]
end

part3(data) = @_ axes(data, 1) .|> (_, 1) |> 
    part1.(Ref(data), __) |> 
    (__[argmin(__)], argmin(__))

@_ get_data("input_test.txt") |> part3(__)
@_ get_data("input.txt") |> part3(__)

########################################
# Custom get_index
########################################

struct Circular
    v::Vector{UInt8}
    width::Int
    height::Int
end

function Circular(v)
    height = count(==(0x0a), v)
    width = length(v) ÷ height - 1
    Circular(v, width, height)
end

Base.getindex(c::Circular, i, j) = c.v[(j - 1) * (c.width + 1) + mod1(i, c.width)]

data = read("input_test.txt") |> Circular
count(1:data.height) do i
    data[((1, 1) .+ (3, 1) .* (i - 1))...] == UInt8('#')
end

10*12 + 11
data


UInt8('.')
count(==(0x0a), data)
unique(data)

Circular(data)
