using Setfield
import Automa
import Automa.RegExp: @re_str
const re = Automa.RegExp

machine = let
    field = re"[a-z][a-z][a-z]"
    value = re"[0-9a-z#]+"
    record = field * re":" * value
    line = record * re.rep(re" " * record)
    passport = re.rep(line * re"\n")
    passports = passport * re.rep(re"\n" * passport)

    field.actions[:enter] = [:field_enter]
    field.actions[:exit] = [:field_exit]
    value.actions[:exit] = [:value_exit]
    passport.actions[:enter] = [:passport_enter]
    passport.actions[:exit] = [:passport_exit]

    Automa.compile(passports)
end

actions1 = Dict(
   :field_enter => quote
       mark = p
   end,
   :field_exit => quote
       fld = String(data[mark:p-1])
       if fld == "ecl"
           @set! s.ecl += 1
       elseif fld == "pid"
           @set! s.pid += 1
       elseif fld == "eyr"
           @set! s.eyr += 1
       elseif fld == "hcl"
           @set! s.hcl += 1
       elseif fld == "byr"
           @set! s.byr += 1
       elseif fld == "iyr"
           @set! s.iyr += 1
       elseif fld == "hgt"
           @set! s.hgt += 1
       end
   end,
   :value_exit => quote
   end,
   :passport_enter => quote
       s = (; ecl = 0, pid = 0, eyr = 0, hcl = 0, byr = 0, iyr = 0, hgt = 0)
       valid = true
   end,
   :passport_exit => quote
       cnt += sum(s) == 7
       total += 1
   end
)


context = Automa.CodeGenContext();
@eval function parse1(data::Union{String,Vector{UInt8}})
    $(Automa.generate_init_code(context, machine))
    
    # p_end and p_eof were set to 0 and -1 in the init code,
    # we need to set them to the end of input, i.e. the length of `data`.
    p_end = p_eof = lastindex(data)
    
    mark = 0
    cnt = 0
    total = 0
    local fld
    local s
    local valid
    $(Automa.generate_exec_code(context, machine, actions1))

    # We need to make sure that we reached the accept state, else the 
    # input did not parse correctly
    iszero(cs) || error("failed to parse on byte ", p, "; State: ", cs)
    return (; answer = cnt, total = total)
end;


input = read("input_test.txt");
println("Part 1: ", parse1(input).answer)

input = read("input.txt");
println("Part 1: ", parse1(input).answer)

########################################
# Part 2
########################################

isnumber(s) = count(x -> x in '0':'9', s) == length(s)
kdigits(s, k) = isnumber(s) & (length(s) == k)
isyear(s, y1, y2) = tryparse(Int, s) |> x -> isnothing(x) ? false : y1 <= x <= y2

function isheight(s)
    length(s) < 4 && return false
    dm = s[end-1:end]
    dm in ["cm", "in"] || return false
    isnumber(s[1:end-2]) || return false
    num = parse(Int, s[1:end-2])
    if dm == "cm"
        return 150 <= num <= 193
    else
        return 59 <= num <= 76
    end
end

actions2 = Dict(
   :field_enter => quote
       mark = p
   end,
   :field_exit => quote
       fld = String(data[mark:p - 1])
       mark = p
   end,
   :value_exit => quote
       val = String(data[mark+1:p - 1])
       if fld == "ecl"
           @set! s.ecl += 1
           valid &= val in ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
       elseif fld == "pid"
           @set! s.pid += 1
           valid &= !isnothing(match(r"^\d{9}$", val))
       elseif fld == "eyr"
           @set! s.eyr += 1
           valid &= isyear(val, 2020, 2030)
       elseif fld == "hcl"
           @set! s.hcl += 1
           valid &= !isnothing(match(r"^#[a-f0-9]{6}$", val))
       elseif fld == "byr"
           @set! s.byr += 1
           valid &= isyear(val, 1920, 2002)
       elseif fld == "iyr"
           @set! s.iyr += 1
           valid &= isyear(val, 2010, 2020)
       elseif fld == "hgt"
           @set! s.hgt += 1
           valid &= isheight(val)    
       end
   end,
   :passport_enter => quote
       s = (; ecl = 0, pid = 0, eyr = 0, hcl = 0, byr = 0, iyr = 0, hgt = 0)
       valid = true
   end,
   :passport_exit => quote
       cnt += ((sum(s) == 7) & valid)
       total += 1
   end
)

@eval function parse2(data::Union{String,Vector{UInt8}})
    $(Automa.generate_init_code(context, machine))
    
    # p_end and p_eof were set to 0 and -1 in the init code,
    # we need to set them to the end of input, i.e. the length of `data`.
    p_end = p_eof = lastindex(data)
    
    mark = 0
    cnt = 0
    total = 0
    local fld
    local s
    local valid
    $(Automa.generate_exec_code(context, machine, actions2))

    # We need to make sure that we reached the accept state, else the 
    # input did not parse correctly
    iszero(cs) || error("failed to parse on byte ", p, "; State: ", cs)
    return (; answer = cnt, total = total)
end;

input = read("input_p2_valid.txt");
parse2(input)

input = read("input_p2_invalid.txt");
parse2(input)

input = read("input.txt");
println("Part 2: ", parse2(input).answer)

using BenchmarkTools
@btime parse2($input);
