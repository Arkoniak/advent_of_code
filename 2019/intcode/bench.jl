using Test
using Revise
using ProfileView
using BenchmarkTools
include("../intcode/intcode.jl")
using .IntCode

function primes()
    s = "3,100,1007,100,2,7,1105,-1,87,1007,100,1,14,1105,-1,27,101,-2,100,100,101,1,101,101,1105,1,9,101,105,101,105,101,2,104,104,101,1,102,102,1,102,102,103,101,1,103,103,7,102,101,52,1106,-1,87,101,105,102,59,1005,-1,65,1,103,104,104,101,105,102,83,1,103,83,83,7,83,105,78,1106,-1,35,1101,0,1,-1,1105,1,69,4,104,99"
    vm = Prog(s, mem = 100000)
    IntCode.run(vm, 100000)
end

# BenchmarkTools.Trial:
#   memory estimate:  198.16 MiB
#   allocs estimate:  1941394
#   --------------
#   minimum time:     257.946 ms (6.15% GC)
#   median time:      261.397 ms (7.02% GC)
#   mean time:        263.782 ms (6.86% GC)
#   maximum time:     298.805 ms (8.14% GC)
#   --------------
#   samples:          19
#   evals/sample:     1

# BenchmarkTools.Trial:
#   memory estimate:  1.53 MiB
#   allocs estimate:  115
#   --------------
#   minimum time:     133.295 ms (0.00% GC)
#   median time:      135.116 ms (0.00% GC)
#   mean time:        136.462 ms (0.01% GC)
#   maximum time:     145.529 ms (0.00% GC)
#   --------------
#   samples:          37
#   evals/sample:     1

#
# BenchmarkTools.Trial:
#   memory estimate:  1.62 MiB
#   allocs estimate:  1181
#   --------------
#   minimum time:     72.673 ms (0.00% GC)
#   median time:      74.725 ms (0.00% GC)
#   mean time:        75.349 ms (0.08% GC)
#   maximum time:     84.514 ms (0.00% GC)
#   --------------
#   samples:          67
#   evals/sample:     1

# BenchmarkTools.Trial:
#   memory estimate:  1.62 MiB
#   allocs estimate:  1177
#   --------------
#   minimum time:     15.829 ms (0.00% GC)
#   median time:      16.972 ms (0.00% GC)
#   mean time:        17.235 ms (0.23% GC)
#   maximum time:     25.914 ms (0.00% GC)
#   --------------
#   samples:          290
#   evals/sample:     1

# BenchmarkTools.Trial:
#   memory estimate:  2.24 MiB
#   allocs estimate:  572
#   --------------
#   minimum time:     10.755 ms (0.00% GC)
#   median time:      11.742 ms (0.00% GC)
#   mean time:        11.986 ms (0.31% GC)
#   maximum time:     15.620 ms (0.00% GC)
#   --------------
#   samples:          417
#   evals/sample:     1

@test primes()[1] == 454396537

@code_warntype primes()
@benchmark primes()
#= println(@benchmark primes($vm)) =#
@profview (for _ in 1:5; primes(); end)
@profview primes()

s = "3,100,1007,100,2,7,1105,-1,87,1007,100,1,14,1105,-1,27,101,-2,100,100,101,1,101,101,1105,1,9,101,105,101,105,101,2,104,104,101,1,102,102,1,102,102,103,101,1,103,103,7,102,101,52,1106,-1,87,101,105,102,59,1005,-1,65,1,103,104,104,101,105,102,83,1,103,83,83,7,83,105,78,1106,-1,35,1101,0,1,-1,1105,1,69,4,104,99"
vm = Prog(s, mem = 100000)

@benchmark Prog($s, mem = $100000)

##################

function f1(n)
    modes_code = n
    x1 = 0
    for i in 1:3
        x1 += mod(modes_code, 10)
        modes_code = div(modes_code, 10)
    end
    x1, modes_code
end

function f2(n)
    parse.(Int, collect(string(n)))
end

n = 123
@benchmark f1($n)
@benchmark f2($n)

digits(12309)

modes = [0, 0, 0]
op_code = 2002
ds = digits(op_code)
op = ds[2]*10 + ds[1]
for i in 3:length(ds)
    modes[length(ds) - i + 1] = ds[i]
end

modes

for x in zip('0':'2', '0':'2', '0':'2')
    println(parse(Int, join(x)))
end

for i in 0:2, j in 0:2, k in 0:2
    println(100*i + 10*j + k)
end


################################

function factors()
    s = "3,1,109,583,108,0,1,9,1106,-1,14,4,1,99,107,0,1,19,1105,-1,27,104,-1,102,-1,1,1,21101,0,38,0,20101,0,1,1,1105,1,138,2101,1,1,41,101,596,41,45,1101,1,596,77,1101,0,1,53,101,1,77,77,101,1,53,53,7,45,77,67,1105,-1,128,108,1,1,74,1105,-1,128,1005,-1,54,1,53,77,93,7,45,93,88,1105,-1,101,1101,0,1,-1,1,53,93,93,1105,1,83,21101,0,116,0,20101,0,1,1,20101,0,53,2,1105,1,235,1205,2,54,4,53,2101,0,1,1,1105,1,101,108,1,1,133,1105,-1,137,4,1,99,22101,0,1,2,22101,0,1,1,21101,0,163,3,22101,0,1,4,22101,0,2,5,109,3,1105,1,198,109,-3,22102,-1,1,1,22201,1,4,3,22102,-1,1,1,1208,3,0,182,2105,-1,0,1208,3,1,189,2105,-1,0,22101,0,4,1,1105,1,146,1207,1,1,203,2105,-1,0,21101,0,222,3,22101,0,2,4,22101,0,1,5,109,3,1105,1,235,109,-3,22201,1,4,1,21101,0,2,2,1105,1,235,1105,0,280,101,383,236,243,1107,-1,583,247,1106,-1,276,101,383,236,256,102,1,275,-1,102,2,275,275,1007,275,0,266,1105,-1,280,101,1,236,236,1105,1,238,1,101,-1,236,236,101,383,236,286,207,1,-1,289,1106,-1,-1,22101,0,1,3,2102,1,2,363,2102,-1,2,369,22102,0,1,1,22102,0,2,2,101,1,236,320,101,-1,320,320,1107,-1,0,324,2105,-1,0,22102,2,2,2,101,383,320,336,207,3,-1,339,1105,-1,361,22101,1,2,2,22102,-1,3,3,101,383,320,354,22001,-1,3,3,22102,-1,3,3,1207,2,-1,366,1105,-1,315,22101,-1,2,2,101,383,320,377,22001,-1,1,1,1105,1,315"
    vm = Prog(s, mem = 100000)
    IntCode.run(vm, 2147483647)
end

@test factors()[1] == 2147483647
@benchmark factors()

@profview factors()

###################

struct A
    x::Dict{Int, Int}
end

A() = A(Dict(
    1 => 1,
    2 => 2,
    3 => 3,
    4 => 4,
    5 => 5
))

function f1(a::A, i)
    a.x[i]
end

function f2(a::A, i)
    if i == 1
        return 1
    elseif i == 2
        return 2
    elseif i == 3
        return 3
    elseif i == 4
        return 4
    elseif i == 5
        return 5
    end
end

a = A()
i = 5

@benchmark f1($a, $i)

@benchmark f2($a, $i)

s = "3,1,109,583,108,0,1,9,1106,-1,14,4,1,99,107,0,1,19,1105,-1,27,104,-1,102,-1,1,1,21101,0,38,0,20101,0,1,1,1105,1,138,2101,1,1,41,101,596,41,45,1101,1,596,77,1101,0,1,53,101,1,77,77,101,1,53,53,7,45,77,67,1105,-1,128,108,1,1,74,1105,-1,128,1005,-1,54,1,53,77,93,7,45,93,88,1105,-1,101,1101,0,1,-1,1,53,93,93,1105,1,83,21101,0,116,0,20101,0,1,1,20101,0,53,2,1105,1,235,1205,2,54,4,53,2101,0,1,1,1105,1,101,108,1,1,133,1105,-1,137,4,1,99,22101,0,1,2,22101,0,1,1,21101,0,163,3,22101,0,1,4,22101,0,2,5,109,3,1105,1,198,109,-3,22102,-1,1,1,22201,1,4,3,22102,-1,1,1,1208,3,0,182,2105,-1,0,1208,3,1,189,2105,-1,0,22101,0,4,1,1105,1,146,1207,1,1,203,2105,-1,0,21101,0,222,3,22101,0,2,4,22101,0,1,5,109,3,1105,1,235,109,-3,22201,1,4,1,21101,0,2,2,1105,1,235,1105,0,280,101,383,236,243,1107,-1,583,247,1106,-1,276,101,383,236,256,102,1,275,-1,102,2,275,275,1007,275,0,266,1105,-1,280,101,1,236,236,1105,1,238,1,101,-1,236,236,101,383,236,286,207,1,-1,289,1106,-1,-1,22101,0,1,3,2102,1,2,363,2102,-1,2,369,22102,0,1,1,22102,0,2,2,101,1,236,320,101,-1,320,320,1107,-1,0,324,2105,-1,0,22102,2,2,2,101,383,320,336,207,3,-1,339,1105,-1,361,22101,1,2,2,22102,-1,3,3,101,383,320,354,22001,-1,3,3,22102,-1,3,3,1207,2,-1,366,1105,-1,315,22101,-1,2,2,101,383,320,377,22001,-1,1,1,1105,1,315"
vm = Prog(s, mem = 100000)

@code_warntype IntCode.step(vm)
