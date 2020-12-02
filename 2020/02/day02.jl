function get_data(filename)
    readlines(filename) .|>
    x -> (match(r"([\d]+)-([\d]+) (.): (.*)", x) |> m -> (; r = parse(Int, m[1]):parse(Int, m[2]), c = m[3][1], p = m[4]))
end

function part1(data)
    data .|> (x -> reduce((c1, c2) -> c1 += c2 == x.c, x.p; init = 0) in x.r) |> sum
end

function part2(data)
    data .|> 
        (x -> (x.p[x.r[1]] == x.c) + (x.p[x.r[end]] == x.c)) |>
        x -> sum(x .== 1)
end

data = get_data("input.txt")
part1(data)
part2(data)
