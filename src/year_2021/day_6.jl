using LinearAlgebra, DelimitedFiles, StatsBase, MKL, LoopVectorization

freqs = countmap(vec(readdlm("resources/2021/6/input", ',', Int)))
input = [try
             freqs[i]
         catch _
             0
         end
         for i in 0:8]

const table = [6703087164 6206821033 5617089148 5217223242 4726100874 4368232009 3989468462 3649885552 3369186778]

function fastest_solution(input)
    out = 0
    @turbo for i in 1:9
        out += table[i] * input[i]
    end
    return out
end
