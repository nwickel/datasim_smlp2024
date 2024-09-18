using MixedModels
using MixedModelsSim
using DataFrames
using Random
using Statistics

fact = Dict(:cond => ["cond1", "cond2"])
simdat = DataFrame(simdat_crossed(100, 10; both_win = fact))
sort!(simdat, :subj)

###
### LMM
###

beta = [500, 10]
sigma = 200
theta = [20, 10] ./ sigma

m1 = let contrasts, form
    contrasts = Dict(:cond => DummyCoding(base = "cond1"))
    form = @formula(dv ~ 1 + cond + (1 | subj) + (1 | item))
    MixedModel(form, simdat; contrasts)
end

simulate!(MersenneTwister(1205), m1; β = beta, θ = theta, σ = sigma)
fit!(m1)

p1 = parametricbootstrap(MersenneTwister(1207), 1000, m1; β = beta, θ = theta, σ = sigma)

df_cond = groupby(DataFrame(p1.coefpvalues), :coefname)[2]

# Parameter recovery
mean(df_cond.β)
# 9.88028354621717

# Bootstrap SE
std(df_cond.β)
# 9.215839607694505

# Mean SE of Wald test
mean(df_cond.se)
# 8.933467054773258

###
### GLMM
###

beta = [2, 1.5]
theta = [.5, .3]

fill!(simdat.dv, 1)

m1 = let contrasts, form
    contrasts = Dict(:cond => DummyCoding(base = "cond1"))
    form = @formula(dv ~ 1 + cond + (1 | subj) + (1 | item))
    MixedModel(form, simdat, Bernoulli(); contrasts)
end

simulate!(MersenneTwister(1205), m1; β = beta, θ = theta)
fit!(m1)

p1 = parametricbootstrap(MersenneTwister(1207), 1000, m1; β = beta, θ = theta)

df_cond = groupby(DataFrame(p1.coefpvalues), :coefname)[2]

# Parameter recovery
mean(df_cond.β)
# 1.49215021992646

# Bootstrap SE
std(df_cond.β)
# 0.20076403275710816

# Mean SE of Wald test
mean(df_cond.se)
# 0.2011388047050821


