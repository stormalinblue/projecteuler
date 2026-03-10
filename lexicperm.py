import math

'''
1
023456789
17
02345689
178
0234569
1783
024569
17839
02456
178392
0456
1783925
046
1783925604
'''

def nth_perm(n, seq):
    if n == 0:
        return seq
    if not seq:
        return []
    f = math.factorial(len(seq) - 1)
    pos = n // f
    rem = n % f
    return [seq[pos]] + nth_perm(rem, seq[:pos] + seq[pos + 1:])

def all_perms(seq):
    for i in range(math.factorial(len(seq))):
        yield nth_perm(i, seq)

if __name__ == '__main__':
    seq = list(range(10))
    for i in range(0, 1000000 + 1):
        print(i, nth_perm(i, seq))
    