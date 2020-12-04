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

context = Automa.CodeGenContext();
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
       @inbounds if valid
           if (data[mark] == UInt8('b')) & (data[mark + 1] == UInt8('y')) & (data[mark + 2] == UInt8('r'))
               fld = BYR
           elseif (data[mark] == UInt8('i')) & (data[mark + 1] == UInt8('y')) & (data[mark + 2] == UInt8('r'))
               fld = IYR
           elseif (data[mark] == UInt8('e')) & (data[mark + 1] == UInt8('y')) & (data[mark + 2] == UInt8('r'))
               fld = EYR
           elseif (data[mark] == UInt8('h')) & (data[mark + 1] == UInt8('g')) & (data[mark + 2] == UInt8('t'))
               fld = HGT
           elseif (data[mark] == UInt8('h')) & (data[mark + 1] == UInt8('c')) & (data[mark + 2] == UInt8('l'))
               fld = HCL
           elseif (data[mark] == UInt8('e')) & (data[mark + 1] == UInt8('c')) & (data[mark + 2] == UInt8('l'))
               fld = ECL
           elseif (data[mark] == UInt8('p')) & (data[mark + 1] == UInt8('i')) & (data[mark + 2] == UInt8('d'))
               fld = PID
           elseif (data[mark] == UInt8('c')) & (data[mark + 1] == UInt8('i')) & (data[mark + 2] == UInt8('d'))
               fld = CID
           else
               fld = JUNK
           end
           mark = p
       end
   end,
   :value_exit => quote
       if valid
           if fld == ECL
               @set! s.ecl += 1
               valid &= valecl(data, mark + 1, p - 1)
           elseif fld == PID
               @set! s.pid += 1
               valid &= valdigits(data, mark + 1, p - 1, 9)
           elseif fld == EYR
               @set! s.eyr += 1
               valid &= valyear(data, mark + 1, p - 1, 2020, 2030)
           elseif fld == HCL
               @set! s.hcl += 1
               valid &= valhcl(data, mark + 1, p - 1)
           elseif fld == BYR
               @set! s.byr += 1
               valid &= valyear(data, mark + 1, p - 1, 1920, 2002)
           elseif fld == IYR
               @set! s.iyr += 1
               valid &= valyear(data, mark + 1, p - 1, 2010, 2020)
           elseif fld == HGT
               @set! s.hgt += 1
               valid &= valhgt(data, mark + 1, p - 1)
           end
       end
   end,
   :passport_enter => quote
       s = (; ecl = 0, pid = 0, eyr = 0, hcl = 0, byr = 0, iyr = 0, hgt = 0)
       valid = true
       fld = JUNK
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

########################################
# Digits Machine
########################################

digitmachine = let
    digit = re"[0-9]"
    digits = re.rep(digit)

    Automa.compile(digits)
end

@eval function valdigits(data::Union{String,Vector{UInt8}}, p0, p1, k)
    $(Automa.generate_init_code(context, digitmachine))
    
    # p_end and p_eof were set to 0 and -1 in the init code,
    # we need to set them to the end of input, i.e. the length of `data`.
    k != p1 - p0 + 1 && return false
    p = p0
    p_end = p1
    p_eof = p1
    
    $(Automa.generate_exec_code(context, digitmachine, Dict{Symbol, Expr}()))

    # We need to make sure that we reached the accept state, else the 
    # input did not parse correctly
    iszero(cs)
end;

########################################
# ECL Machine
########################################

eclmachine = let
    amb = re"amb"
    blu = re"blu"
    brn = re"brn"
    gry = re"gry"
    grn = re"grn"
    hzl = re"hzl"
    oth = re"oth"

    ecl = re.alt(amb, blu, brn, gry, grn, hzl, oth)

    Automa.compile(ecl)
end

@eval function valecl(data::Union{String,Vector{UInt8}}, p0, p1)
    $(Automa.generate_init_code(context, eclmachine))
    
    # p_end and p_eof were set to 0 and -1 in the init code,
    # we need to set them to the end of input, i.e. the length of `data`.
    2 != p1 - p0 && return false
    p = p0
    p_end = p1
    p_eof = p1
    
    $(Automa.generate_exec_code(context, eclmachine, Dict{Symbol, Expr}()))

    # We need to make sure that we reached the accept state, else the 
    # input did not parse correctly
    iszero(cs)
end;

########################################
# Year Machine
########################################

yearmachine = let
    digit = re"[0-9]"
    digits = re.rep(digit)

    digit.actions[:enter] = [:enter]
    Automa.compile(digits)
end

yearaction = Dict(
    :enter => quote
        @inbounds x = 10*x + (UInt8(data[p]) - UInt8('0'))
    end
)

@eval function valyear(data::Union{String,Vector{UInt8}}, p0, p1, y1, y2)
    $(Automa.generate_init_code(context, yearmachine))
    
    # p_end and p_eof were set to 0 and -1 in the init code,
    # we need to set them to the end of input, i.e. the length of `data`.
    3 != p1 - p0 && return false
    p = p0
    p_end = p1
    p_eof = p1
    
    x = 0
    $(Automa.generate_exec_code(context, yearmachine, yearaction))

    # We need to make sure that we reached the accept state, else the 
    # input did not parse correctly
    !iszero(cs) && return false
    y1 <= x <= y2
end;

########################################
# HCL machine
########################################
hclmachine = let
    head = re"#"
    digit = re"[0-9a-f]"
    hex = head*re.rep(digit)

    Automa.compile(hex)
end

@eval function valhcl(data::Union{String,Vector{UInt8}}, p0, p1)
    $(Automa.generate_init_code(context, hclmachine))
    
    # p_end and p_eof were set to 0 and -1 in the init code,
    # we need to set them to the end of input, i.e. the length of `data`.
    6 != p1 - p0 && return false
    p = p0
    p_end = p1
    p_eof = p1
    
    $(Automa.generate_exec_code(context, hclmachine, Dict{Symbol, Expr}()))

    # We need to make sure that we reached the accept state, else the 
    # input did not parse correctly
    iszero(cs)
end;

########################################
# HGT Machine
########################################

hgtmachine = let
    digit = re"[0-9]"
    cm = re"cm"
    inch = re"in"
    dm = re.alt(cm, inch)
    height = digit * re.rep1(digit) * dm

    digit.actions[:enter] = [:digit_enter]
    dm.actions[:enter] = [:dm_enter]

    Automa.compile(height)
end

hgtactions = Dict(
    :digit_enter => quote
        @inbounds x = 10*x + UInt8(data[p]) - UInt8('0')
    end,
    :dm_enter => quote
        @inbounds dm = UInt8(data[p]) == UInt8('i') ? 1 : 2
    end
)

@eval function valhgt(data::Union{String,Vector{UInt8}}, p0, p1)
    $(Automa.generate_init_code(context, hgtmachine))
    
    # p_end and p_eof were set to 0 and -1 in the init code,
    # we need to set them to the end of input, i.e. the length of `data`.
    p1 - p0 < 3 && return false
    p = p0
    p_end = p1
    p_eof = p1
    
    x = 0
    dm = 0
    $(Automa.generate_exec_code(context, hgtmachine, hgtactions))

    # We need to make sure that we reached the accept state, else the 
    # input did not parse correctly
    !iszero(cs) && return false
    dm == 0 && return false
    if dm == 1
        return 59 <= x <= 76
    else
        return 150 <= x <= 193
    end
end;
