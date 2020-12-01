########################################
# Part 1
########################################
using Underscores
get_data(filename) = @_ readlines(filename) |> parse.(Int, __)

function part1(data)
    sort!(data)
    set = Set(data)
    for num in data
        if 2020 - num in set
            return (true, num*(2020 - num))
            break
        end
    end
    
    return (false, 0)
end

function part1(; filename = "input.txt", data = get_data(filename))
    found, res = part1(data)
    if found
        println("Part 1: ", res)
    else
        println("Part 1: Solution not found, there is an error in your algorithm or input data.")
    end
end

# part1(filename = "input_test.txt")
part1()

########################################
# Part 2
########################################

function part2(data)
    sort!(data)
    set = Set(data)
    n1, n2, n3 = 0, 0, 0
    found = false
    for i in eachindex(data)
        n1 = data[i]
        j = i + 1
        while true
            n2 = data[j]
            n1 + n2 > 2020 && break
            n3 = 2020 - n1 - n2
            if n3 in set 
                found = true
                break
            end
            j += 1
            j > length(data) && break
        end
        found && break
    end
    
    return (found, n1*n2*n3)
end

function part2(; filename = "input.txt", data = get_data(filename))
    found, res = part2(data)
    if found
        println("Part 2: ", res)
    else
        println("Part 2: Solution not found, there is an error in your algorithm or input data.")
    end
end

part2(filename = "input.txt")

########################################
# Benchmarking
########################################
using BenchmarkTools
data = get_data("input.txt");
@btime part1(d) setup=(d = copy($data)) evals = 1
#  2.884 μs (7 allocations: 5.07 KiB)

data = get_data("input.txt");
@btime part2(d) setup=(d = copy($data)) evals = 1
#  3.651 μs (7 allocations: 5.07 KiB)

function naive_part1(data)
    for x in data
        for y in data
            if x + y == 2020
                return true
            end
        end
    end

    return false
end

@btime naive_part1($data)
