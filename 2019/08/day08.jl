#############################
# Part 1

function part1()
    data = parse.(Int, collect(readline("input.txt")))
    layers = [data[(25*6*(i - 1) + 1):(25*6*i)] for i in 1:div(length(data), 25*6)]
    zeromin = argmin(map(x -> sum(x .== 0), layers))
    println("Part 1: ", sum(layers[zeromin] .== 1) * sum(layers[zeromin] .== 2))
end

part1()

############################
# Part 2

transpose(layers) = [[layer[i] for layer in layers] for i in 1:(25*6)]

function part2()
    data = parse.(Int, collect(readline("input.txt")))
    layers = [data[(25*6*(i - 1) + 1):(25*6*i)] for i in 1:div(length(data), 25*6)]
    tlayers = transpose(layers)
    pic0 = [l[findall( l .!= 2)[1]] for l in tlayers]
    pic0 = map(x -> x == 0 ? '⬛' : '⬜', pic0)
    println("Part 2: ")
    [println(join(pic0[((i-1)*25 + 1):(i*25)])) for i in 1:6]
end

part2()
