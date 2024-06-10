n = int(input())
sieve = [True] * n

for i in range(2, n):
    if sieve[i]:
        print(i)
        for j in range(i+i, n, i):
            sieve[j] = False
