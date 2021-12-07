using LinearAlgebra, DelimitedFiles, StatsBase

freqs = countmap(vec(readdlm("resources/2021/6/input", ',', Int)))
input = [try freqs[i] catch _ 0 end for i ∈ 0:8]

step = [0 1 0 0 0 0 0 0 0;
        0 0 1 0 0 0 0 0 0;
        0 0 0 1 0 0 0 0 0;
        0 0 0 0 1 0 0 0 0;
        0 0 0 0 0 1 0 0 0;
        0 0 0 0 0 0 1 0 0;
        1 0 0 0 0 0 0 1 0;
        0 0 0 0 0 0 0 0 1;
        1 0 0 0 0 0 0 0 0]

sum(step^256 * input)
