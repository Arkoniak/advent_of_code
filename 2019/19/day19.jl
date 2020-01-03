############################
# Part 1

include("../intcode/intcode.jl")

function part1()
    vm = Prog(readline("input.txt"))
    img_txt = run(vm, [50, 50])
    arr = zeros(Int, 50, 50)
    for idx in CartesianIndices(arr)
        vm = Prog(readline("input.txt"))
        arr[idx] = run(vm, [idx[1] - 1, idx[2] - 1])[1]
    end
    println("Part 1: ", sum(arr))
end

part1()

############################
# Part 2
# Didn't quite automated it
# Details can be found in jupyter notebook

f1(x, a) = Int(ceil(a*x))
f2(x, a) = Int(floor(a*x))

function find_edge()
    i = 50
    while true
        if f2(i - 99, 0.7163649835856631) < f1(i, 0.5717967599630356) + 99
            i += 1
        else
            return i
        end
    end
end

function part2()
    y = find_edge()
    x = f1(y, 0.5717967599630356)

    println("Part 2: ", x*10000 + (y - 99))
end

part2()
