from mrjob.job import MRJob
import string


class MRWordCounts(MRJob):

    def mapper(self, _, line):
        # yield each word in the line
        for word in line.split():
        	yield (word.strip(string.punctuation).lower(), 1)

    def combiner(self, word, counts):
        # optimization: sum the words we've seen so far
        yield (word, sum(counts))

    def reducer(self, word, counts):
        # optimization: sum the words we've seen so far
        yield (word, sum(counts))


if __name__ == '__main__':
    MRWordCounts.run()