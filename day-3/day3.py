from dataclasses import dataclass

@dataclass
class Object:
    x: int
    y: int
    width: int
    value: int
    symbol: str

from pathlib import Path

eenput = Path("input.txt").read_text().splitlines()

objects: [Object] = []

def parseNextInt(line, x) -> (int, int):
    xi = x
    while (xi < len(line)) and line[xi].isdigit():
        xi += 1
    return (int(line[x:xi]), xi)

for yi, line in enumerate(eenput):
    xi = 0
    while xi < len(line):
        if (line[xi].isdigit()):
            res = parseNextInt(line, xi)
            objects.append(Object(xi, yi, res[1] - xi, res[0], ''))
            xi = res[1]
            continue
        elif line[xi] != '.':
            objects.append(Object(xi, yi, 1, -1, line[xi]))
        xi += 1

def coordinatesAroundObject(obj) -> [(int, int)]:
    coords = []
    # top
    for xi in range(obj.x - 1, obj.x + obj.width + 1):
        coords.append((xi, obj.y - 1))

    # bottom
    for xi in range(obj.x - 1, obj.x + obj.width + 1):
        coords.append((xi, obj.y + 1))

    # left
    coords.append((obj.x - 1, obj.y))

    # right
    coords.append((obj.x + obj.width, obj.y))

    return list(filter(lambda c: c[0] >= 0 and c[1] >= 0 and c[0] < len(eenput[0]) and c[1] < len(eenput), coords))

res = 0
for obj in objects:
    if obj.value == -1:
        continue

    coords = coordinatesAroundObject(obj)
    cont = False

    for c in coords:
        if cont:
            break
        for o in objects:
            if c[0] == o.x and c[1] == o.y and o.value == -1:
                res += obj.value
                cont = True
                break
print("part 1: ", res)

numbers = list(filter(lambda o: o.value != -1, objects))

def objectCoordinates(obj) -> [(int, int)]:
    coords = []
    for xi in range(obj.x, obj.x + obj.width):
        coords.append((xi, obj.y))
    return coords

res = 0

for obj in objects:
    if obj.value != -1 or obj.symbol != '*':
        continue
    coords = coordinatesAroundObject(obj)

    nums = []
    for n in numbers:
        nc = objectCoordinates(n)
        for c in nc:
            if c in coords:
                nums.append(n.value)
                break
    if len(nums) == 2:
        res += nums[0] * nums[1]

print("part 2: ", res)
