from mrjob.job import MRJob
import functools

class MRSummaryStats(MRJob):

    def mapper(self, _, line):
        # yield label as the key, and a tuple of values necessary for computation of mean and sample variance
        label, value = line.split()
        value = float(value)
        yield (label, (1, value, value**2))

    def combiner(self, label, values):
        # calculate our sample mean and sample variance
        n, sumX, sumX2 = functools.reduce(lambda x,y:[x[i] + y[i] for i in range(len(y))], values)
        yield label, (n, sumX / n, (sumX2 / n) - (sumX / n)**2)

    def reducer(self, label, values):
        # calculate our sample mean and sample variance
        n, sumX, sumX2 = functools.reduce(lambda x,y:[x[i] + y[i] for i in range(len(y))], values)
        yield label, (n, sumX / n, (sumX2 / n) - (sumX / n)**2)


if __name__ == '__main__':
    MRSummaryStats.run()