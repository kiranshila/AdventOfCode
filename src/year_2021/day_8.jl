using DelimitedFiles, Bijections
import Base.-

inputs = readlines("resources/2021/8/input")

function parse_line(line)
    signals, output = split(line, '|')
    signals = split(signals, " "; keepempty=false)
    output = split(output, " "; keepempty=false)
    return signals, output
end

part_1(input) = sum(length(filter(x -> x ∈ [2, 3, 4, 7], map(length, parse[2]))) for parse in parse_line.(input))

-(A::Set, B::Set) = setdiff(A, B)

find_make_set(count, chars) = Set(chars[findfirst(x -> length(x) == count, chars)])

function solve_scrambled(signals, outputs)
    digits = Bijection(Dict(1 => find_make_set(2, signals), 4 => find_make_set(4, signals),
                            7 => find_make_set(3, signals), 8 => find_make_set(7, signals)))
    sets = Set.(signals)

    a = digits[7] - digits[1]
    g = symdiff(map(x -> x - (a ∪ digits[4]), sets)...)
    d = symdiff(map(x -> x - (g ∪ digits[7]), sets)...)
    e = digits[8] - digits[4] - a - g
    b = digits[4] - d - digits[1]
    f = symdiff(map(x -> x ∩ digits[1], sets)...)
    c = digits[1] - f

    digits[0] = digits[8] - d
    digits[3] = digits[8] - b - e
    digits[9] = digits[8] - e
    digits[6] = digits[8] - c
    digits[2] = digits[8] - b - f
    digits[5] = digits[8] - c - e
    return parse(Int,string([digits(Set(output)) for output in outputs]...))
end

part_2(inputs) = sum([solve_scrambled(parse_line(line)...) for line in inputs])
