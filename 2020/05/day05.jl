using ReTest
using Underscores

# Part 1
    seatid(s) = reduce((x, y) -> (x << 1) | ((y == 'R') | (y == 'B')), s; init = 0)
    println("Part 1: ", mapreduce(seatid, max, eachline("input.txt")))

seatid(s) = @_ reduce((_1 << 1) | ((_2 == 'R') | (_2 == 'B')), s; init = 0)

eachline("input_test.txt") .|> seatid |> maximum
# Part 1
eachline("input.txt") |>
   z -> mapreduce((x, y) -> (x << 1) | ((y == 'R') | (y == 'B')), z; init = 0) |>
   maximum

########################################
# Part 2
########################################
let
    seats = sort(seatid.(eachline("input.txt")))
    prev = seats[1]
    for seat in seats
        if seat - prev == 2
            println("Part 2: ", prev + 1)
            break
        else
            prev = seat
        end
    end
end


########################################
# Test section
########################################
@testset "Basic" begin
    @test seatid("RLR") == 5
    @test seatid("FBFBBFFRLR") == 357
end

runtests()
