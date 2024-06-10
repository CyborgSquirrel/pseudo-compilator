n = int(input())

list_ = []
for _ in range(n):
    list_.append(int(input()))

for i in range(n//2):
    list_[i], list_[n-1-i] = list_[n-1-i], list_[i]

for i in range(n):
    print(list_[i])
