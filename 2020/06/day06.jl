using ReTest
using Underscores

function part(data, f)
    sum(split(rstrip(data), "\n\n")) do s
        split(s, "\n", keepempty = false) .|> Set |> x -> f(x...) |> length
    end
end

let data = read("input.txt", String)
    for (i, f) in enumerate((union, intersect))
        println("Part ", i, ": ", part(data, f))
    end
end

########################################
# Tests
########################################
@testset "Basic validations" begin
    data = read("input_test.txt", String)
    @test part(data, union) == 11
    @test part(data, intersect) == 6
end

runtests()
