function getdata(filename="test_input.txt")
    split(read(open(filename, "r"), String), "\n\n")
end

conv2map(line) = map.(c -> c == '.' ? 0 : 1, collect.(split(line, "\n", keepempty=false)))
rev(x) = collect.(zip(x...))

function findmirrorline(data)
    v = map(((i, c), ) -> (i % 2 == 0 ? 1 : -1) * c, enumerate(data))
    map([(0, identity), (1, reverse)]) do (c, f)
        findfirst(iszero, cumsum(f(v))) |> x -> isnothing(x) ? 0 : ((x ÷ 2) + c * (length(v) - x))
    end |> x -> x[1] == 0 ? x[2] : x[1]
end

line = "#..#\n####\n#..#"
findmirrorline(rev(conv2map(line)))

conv2map.(getdata()) |> x -> map(((c, f), ) -> c * sum(findmirrorline.(f.(x))), [(100, identity), (1, rev)]) |> sum
conv2map.(getdata("input.txt")) |> x -> map(((c, f), ) -> c * sum(findmirrorline.(f.(x))), [(100, identity), (1, rev)]) |> sum

conv2map.(getdata("input.txt")) |> x -> map(((c, f), ) -> c * sum(findmirrorline.(f.(x))), [(100, identity), (1, rev)])
conv2map.(getdata("input.txt")) |> x -> map(((c, f), ) -> c * sum(findmirrorline.(f.(x))), [(100, identity), (1, rev)])

conv2map.(getdata()) .|> (x -> findmirrorline(identity(x))) |> sum
conv2map.(getdata("input.txt")) .|> (x -> findmirrorline(identity(x))) |> sum
conv2map.(getdata("input.txt")) .|> (x -> findmirrorline(rev(x))) |> sum

356*100 + 289
289*100 + 356

|> x -> map(((c, f), ) -> c * sum(findmirrorline.(f.(x))), [(100, identity), (1, rev)]) |> sum

findmirrorline.(conv2map.(getdata()))
findmirrorline.(rev.(conv2map.(getdata())))

findmirrorline(d2)

tonum(s) = reduce((acc, (i, d)) -> acc + 2^(i-1) * d, enumerate(s), init = 0)
v = map(((i, c), ) -> (i % 2 == 0 ? 1 : -1) * c, enumerate(d2)) |> cumsum

findfirst(iszero, v) |> x -> (length(v) - x) + (x ÷ 2)

findfirst(iszero, v)
v

function 


datum = getdata()[1]
datum2 = getdata()[2]
d2 = hcat(map.(c -> c == '.' ? 0 : 1, collect.(split(datum, "\n")))...)
d2 = map.(c -> c == '.' ? 0 : 1, collect.(split(datum2, "\n", keepempty=false)))
println(datum2)
d2 = collect.(zip(map.(c -> c == '.' ? 0 : 1, collect.(split(datum2, "\n", keepempty=false)))...))

reduce.( (acc, (i, d)) -> acc + 2^(i-1) * d, enumerate.(d2), init = 0)


for idx in CartesianIndices(d2)
    println(idx)
end
t(idx) = CartesianIndex(idx[2], idx[1])

map(idx -> idx[1] % 2 == 0 ? transpose(d2)[idx] : -transpose(d2)[idx], CartesianIndices(transpose(d2)))
d2
println(datum)
