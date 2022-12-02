using Statistics, DelimitedFiles, Optim

input = readdlm("resources/2021/7/input", ',', Int)

fuel1(pos) = sum(abs.(pos .- median(pos)))

triangle(n) = n*(n+1)/2
fuel2(pos,target) = sum(triangle.(abs.(pos .- target)))

res = optimize(x->fuel2(input,x),[0.0],LBFGS())

fuel2(input,round(res.minimizer[1]))