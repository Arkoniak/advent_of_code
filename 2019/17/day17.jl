######################################
# Part 1

include("../intcode/intcode.jl")

# 46 = '.'
# 35 = '#'
# 10 = '\n'
# 94 = '^'

function part1()
    vm = Prog(readline("input.txt"))
    img_txt = run(vm)

    img_arr = argmax(img_txt .== 10) |> x -> reshape(img_txt[1:(end-1)], x, (length(img_txt) - 1) ÷ x)[1:(end - 1), :]
    h, w = size(img_arr)
    mask = reshape(2 .^ (0:8), 3, 3)
    filtered = sum(map(x -> mask[x]*map(y -> y == 46 ? 0 : 1, @view img_arr[x[1]:(h + x[1] - 3), x[2]:(w + x[2] - 3)]), 
        CartesianIndices(mask))) .== 186
    println("Part 1: ", sum([x[1]*x[2]*filtered[x] for x in CartesianIndices(filtered)]))
end

part1()

################### Part 2 ####################################

mutable struct Droid
    pos::CartesianIndex{2}
    dir::Vector{Int}
end

const rotations = Dict(
    'R' => [0 -1; 1 0],
    'M' => [1 0; 0 1],
    'L' => [0 1; -1 0]
)

Droid(pos) = Droid(pos, [0, -1])
rotation(droid::Droid, dir::Char) = rotations[dir] * droid.dir
rotate(droid::Droid, dir::Char) = droid.dir = rotations[dir] * droid.dir
move(droid::Droid) = droid.pos += CartesianIndex(droid.dir...)
sense(droid, arr) = map(x -> arr[droid.pos + CartesianIndex(rotation(droid, x)...)], ['L', 'M', 'R']) .!= 46

function construct_path(droid::Droid, arr)
    res = ""
    cnt = 0
    while true
        vars = sense(droid, arr)
        if vars[2]
            cnt += 1
            move(droid)
        else
            res *= "," * string(cnt)
            if sum(vars) == 0 return res[4:end] end
            cnt = 0
            res *= vars[1] ? ",L" : ",R"
            rotate(droid, vars[1] ? 'L' : 'R')
        end
    end
end


function part2()
    vm = Prog(readline("input.txt"))
    img_txt = run(vm)

    img_arr = argmax(img_txt .== 10) |> x -> reshape(img_txt[1:(end-1)], x, (length(img_txt) - 1) ÷ x)[1:(end - 1), :]
    h, w = size(img_arr)
    padding = fill(46, h + 2, w + 2)
    padding[2:(h + 1), 2:(w + 1)] .= img_arr

    droid = Droid(CartesianIndices(padding)[padding .== 94][1])
    path = construct_path(droid, padding)

    ### Ah, manual solution :-(((

    res = "A,A,B,C,B,C,B,C,B,A"
    a = "R,6,L,12,R,6"
    b = "L,12,R,6,L,8,L,12"
    c = "R,12,L,10,L,10"

    res1 = [Int.(collect(res)); 10]
    a1 = [Int.(collect(a)); 10]
    b1 = [Int.(collect(b)); 10]
    c1 = [Int.(collect(c)); 10]

    code = collect(readline("input.txt"))
    code[1] = '2'
    code = join(code)
    vm = Prog(code)
    out = run(vm)
    print(join(Char.(out)))
    out = run(vm, res1)
    print(join(Char.(out)))
    out = run(vm, a1)
    print(join(Char.(out)))
    out = run(vm, b1)
    print(join(Char.(out)))
    out = run(vm, c1)
    print(join(Char.(out)))
    out = run(vm, [Int('n'), 10])

    println("Part 2: ", out[end])
end

part2()

####### Update ######
# Amazing solution from https://www.reddit.com/r/adventofcode/comments/ebr7dg/2019_day_17_solutions/fb7ymcw/

#m = match(r"^(.{1,21})\1*(.{1,21})(?:\1|\2)*(.{1,21})(?:\1|\2|\3)*$", path * ",")
#res = replace(replace(replace(s, m[1] => "A"), m[2] => "B"), m[3] => "C")
#res1 = [Int.(collect(res)); 10]
#a1 = [Int.(collect(m[1])[1:(end-1)]); 10]
#b1 = [Int.(collect(m[2])[1:(end-1)]); 10]
#c1 = [Int.(collect(m[3])[1:(end-1)]); 10]
