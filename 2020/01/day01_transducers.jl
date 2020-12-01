using Underscores
using Transducers
using BenchmarkTools

get_data(filename) = @_ readlines(filename) |> parse.(Int, __)

function compl(data, vals)
    idx = searchsortedfirst(data, 2020 - sum(vals))
    z = data[idx]
    z + sum(vals) == 2020
end

function part(data, k)
    sort!(data)
    n = length(data)
    tr = Enumerate(1)
    for i in 1:k - 2
        tr = tr |> MapCat(x -> x[1] + 1:n |> Map(y -> (y, (data[y], x[2]...))))
    end
    tr = tr |>
        Filter(x -> sum(x[2]) < 2020) |> 
        Filter(x -> compl(data, x[2])) |>
        Take(1) |>
        Map(x -> prod(x[2]) * (2020 - sum(x[2])))

    res = @_ data |> tr |> collect |> first
end

data = get_data("input.txt");

@btime part(d, 2) setup=(d = copy($data)) evals = 1
# 2.507 μs (34 allocations: 2.02 KiB)
@btime part(d, 3) setup=(d = copy($data)) evals = 1
# 9.008 μs (430 allocations: 20.92 KiB)
@btime part(d, 4) setup=(d = copy($data)) evals = 1
# 2.216 μs (36 allocations: 2.75 KiB)

part(data, 2)
part(data, 3)
part(data, 4)

########################################
# Misc
########################################

function part2(data)
    sort!(data)
    n = length(data)
    @_ data |> 
        Enumerate(1) |> 
        MapCat(x -> x[1] + 1:n |> Map(y -> (data[y], x[2]))) |>
        Filter(x -> x[1] + x[2] < 2020) |> 
        Filter(x -> compl(data, x)) |>
        Take(1) |>
        Map(x -> prod(x)*(2020 - sum(x))) |>
        collect |> first
end

data = get_data("input.txt");
@btime part2(d) setup=(d = copy($data)) evals = 1

function part3(data)
    sort!(data)
    n = length(data)
    @_ data |> 
        Enumerate(1) |> 
        MapCat(x -> x[1] + 1:n |> Map(y -> (y, (data[y], x[2]...)))) |>
        MapCat(x -> x[1] + 1:n |> Map(y -> (y, (data[y], x[2]...)))) |>
        MapCat(x -> x[1] + 1:n |> Map(y -> (y, (data[y], x[2]...)))) |>
        Filter(x -> sum(x[2]) < 2020) |> 
        Filter(x -> compl(data, x[2])) |>
        Take(1) |>
        collect
        # Map(x -> prod(x)*(2020 - sum(x))) |>
        # collect |> first
end
