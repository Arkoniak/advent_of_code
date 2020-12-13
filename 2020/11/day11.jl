########################################
# Part 1
########################################
function load_data(filename)
    data = readlines(filename)
    w = length(data[1]) + 2
    h = length(data) + 2
    b1 = fill(Int8(3), w, h)
    for (i, line) in enumerate(data)
        for (j, c) in enumerate(line)
            b1[j + 1, i + 1] = c == 'L' ? 1 : 0
        end
    end
    b2 = deepcopy(b1)

    return (; b1, b2)
end

function shag1!(b1, b2)
    w, h = size(b1)
    for i in 2:w - 1, j in 2:h - 1
        occ = count(==(2), @view b2[i-1:i+1, j-1:j+1])
        occ -= b2[i, j] == 2
        if b2[i, j] == 1 && occ == 0
            b1[i, j] = 2
        elseif b2[i, j] == 2 && occ >= 4
            b1[i, j] = 1
        else
            b1[i, j] = b2[i, j]
        end
    end

    return (b1, b2)
end

function part(filename, shag)
    b1, b2 = load_data(filename)
    while true
        b2, b1 = shag(b1, b2)
        b1 == b2 && return count(==(2), b1)
    end
end

println("Part 1: ", part("input.txt", shag1!))

########################################
# Part 2
########################################
const DIRS = [(1, 0), 
              (-1, 0),
              (0, 1),
              (0, -1),
              (1, 1),
              (1, -1),
              (-1, 1),
              (-1, -1)]

function shag2!(b1, b2, dirs = DIRS)
    w, h = size(b1)
    for i in 2:w - 1, j in 2:h - 1
        occ = sum(dirs) do dir
            p = (i, j)
            while true
                p = p .+ dir
                b2[p...] == 1 && return 0
                b2[p...] == 2 && return 1
                b2[p...] == 3 && return 0
            end
        end
        if b2[i, j] == 1 && occ == 0
            b1[i, j] = 2
        elseif b2[i, j] == 2 && occ >= 5
            b1[i, j] = 1
        else
            b1[i, j] = b2[i, j]
        end
    end

    return (b1, b2)
end

println("Part 2: ", part("input.txt", shag2!))

########################################
# Misc
########################################
function countdir(b, dir, p)
    while true
        p = p .+ dir
        b[p...] == 1 && return 0
        b[p...] == 2 && return 1
        b[p...] == 3 && return 0
    end
end

function showb(b)
    w, h = size(b)

    for j in 2:h - 1
        for i in 2:w-1
            print(b[i, j] == 0 ? '.' : b[i, j] == 1 ? 'L' : '#')
        end
        print("\n")
    end
end

let d = load_data("input_test.txt")
    b1, b2 = d
    b2, b1 = shag2!(b1, b2)

    println("++++++++++++++++++++++++++")
    showb(b1)
    println("==========================")
    showb(b2)
    println("++++++++++++++++++++++++++")
    b2, b1 = shag2!(b1, b2)
    println("++++++++++++++++++++++++++")
    showb(b1)
    println("==========================")
    showb(b2)
    println("++++++++++++++++++++++++++")
end

########################################
# Tests
########################################

module Tests
using ReTest
using Main: part, shag1!, shag2!

@testset "Day 11" begin
    @testset "Part 1" begin
        @test part("input_test.txt", shag1!) == 37
    end

    @testset "Part 2" begin
        @test part("input_test.txt", shag2!) == 26
    end
end

end # module

Tests.runtests(Tests)
