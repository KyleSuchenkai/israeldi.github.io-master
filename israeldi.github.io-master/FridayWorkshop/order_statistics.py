import random

# INPUTS
NUM_OBSERVATIONS_PER_SIM = 9
NUM_SIMS = 10000
k = 3

# CODE
# sum up all the quantiles instead of keeping track of the uniform values
quantile_sum = 0
for _ in range(NUM_SIMS):
	# perform one simulation
	uniforms = []
	for _ in range(NUM_OBSERVATIONS_PER_SIM):
		# draw a random number
		uniforms.append(random.random()) =
	uniforms.sort()

	quantile_sum += uniforms[k-1] 



# OUTPUTS

print(quantile_sum / NUM_SIMS)