n = int(input())

for _ in range(n):
    a = int(input())

    i = 1
    while i*i <= a:
        if a % i == 0:
            print(i)
            if i != a:
                print(a//i)
        i += 1
