import random
import itertools

output = set()
pairs = set()

while True:
    a = random.randrange(1, 2020)
    b = random.randrange(1, 2020 - a)

    output.update([a, b, 2020 - a, 2020 - a - b])

    if len(output) != 4:
        output.clear()
    else:
        pairs.update(a + b for a, b in itertools.combinations(output, 2))
        break

while len(output) != 800:
    new = random.randrange(1, 2020)
    print(2020 - (len(pairs) + len(output)))
    if new not in output and new not in pairs:
        new = 2020 - new
        for n in output:
            if n + new < 2020:
                pairs.add(n + new)
        output.add(new)

print(*output, sep="\n")

len(np.unique(list(pairs)))
print(*pairs, sep="\n")

import numpy as np
out = list(output)
len(np.unique(list(output)))

cnt = 0
for x in out:
    for y in out:
        if (x + y == 2020) and (x != y):
            cnt += 1
cnt

cnt = 0
for x in out:
    for y in out:
        for z in out:
            if (x != y) and (x != z) and (y != z) and (x + y + z == 2020):
                cnt += 1
cnt

cnt = 0
for x in out:
    for y in out:
        for z in out:
            for h in out:
                if ((x != y) and (x != z) and (y != z) and
                    (h != x) and (h != y) and (h != z) and
                    (x + y + z + h == 2020):
                    cnt += 1
cnt
