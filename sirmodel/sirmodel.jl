import LinearAlgebra: I, UpperTriangular
import Plots: plot

t = 100

Ys, Yi, Yr, betas = run_sim(tₑ=t)
plot(1:t, betas)

function run_sim(tₑ=10, β=0.3, γ=0.3, N=100, I=1)
    # Initialise a random distance matrix between agents (TODO: inertia between t_i & t_i+1?)
    D = begin
            t = rand(N, N)
            t = t - t .* I(N)
            UpperTriangular(t)
    end
    X = 1:tₑ
    S = N - I
    R = 0

    # boolean matrix containing number of close people, normalised by Pop.
    d = sum((D .<= 0.15) .* (D .!= 0)) / N

    Ys = []
    Yi = []
    Yr = []
    betas = []
    
    for tᵢ=1:tₑ
        push!(Ys, S)
        push!(Yi, I)
        push!(Yr, R)

        println("t = $(tᵢ)")

	# adapt β by calculating how many agents do not distance properly
	# (distance of 0.15 and less)
        βₑ = β * d
        push!(betas, βₑ)

        dS = -β * S * I / N
        dI = β * I * S / N - γ * I
        dR = γ * I
	# Random fluctuations? -> |d_i,j| afterwards
        # D .+= rand(Uniform(-0.1,0.1), N, N)
        D .*= rand(N, N)

        println("\tS: $(S) -> $(S += dS)")
        println("\tS: $(I) -> $(I += dI)")
        println("\tS: $(R) -> $(R += dR)")
    end
    Ys, Yi, Yr, betas
end

