n = int(input())

list_ = []
for _ in range(n):
    list_.append(float(input()))

for i in range(n-1):
    for j in range(n-1-i):
        if list_[j] > list_[j+1]:
            list_[j], list_[j+1] = list_[j+1], list_[j]

for i in range(n):
    print(list_[i])
