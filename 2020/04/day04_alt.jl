const input = split(String(read("input.txt")), "\n\n")

const fields1 = (r"byr", r"iyr", r"eyr", r"hgt", r"hcl", r"ecl", r"pid")
const fields2 = (
    r"\bbyr:(19[2-9][0-9]|200[0-2])\b",
    r"\biyr:20(1[0-9]|20)\b",
    r"\beyr:20(2[0-9]|30)\b",
    r"\bhgt:(1([5-8][0-9]|9[0-3])cm|(59|6[0-9]|7[0-6])in)\b",
    r"\bhcl:#[0-9a-f]{6}\b",
    r"\becl:(amb|blu|brn|gry|grn|hzl|oth)\b",
    r"\bpid:\d{9}\b"
)

# Part 1
count(p -> all(t -> contains(p, t), fields1), input)

# Part 2
count(p -> all(t -> contains(p, t), fields2), input)

function part2(data)
    input = split(String(data), "\n\n")
    count(p -> all(t -> contains(p, t), fields2), input)
end

input = split(String(read("input.txt")), "\n\n")
function part3(input)
    count(p -> all(t -> contains(p, t), fields2), input)
end


using BenchmarkTools
data = read("input.txt")
part2(data)

@btime part2(d) setup=(d = copy($data)) evals = 1
part3(input)
@btime part3($input)
