r1 = 0
e1 = 0
r2 = 0
e2 = 0
for _ in range(10):
    import subprocess

    with open("log2.log", "w") as log2:
        subprocess.run(
            args="stack run -- /home/hengdiye/datasets/cgt/source/Modified_fbee9d7a84ce6a1b85f934d965db4c0b.sol --contract Slot --test-mode exploration --format text --crytic-args --solc=/home/hengdiye/.solcix/artifacts/solc-0.4.25/solc-0.4.25 --state-machine-json-file /home/hengdiye/datasets/cgt/source/fbee9d7a84ce6a1b85f934d965db4c0b/output_20250213_fbee9d7a84ce6a1b85f934d965db4c0b.sol.json --timeout 10000 --workers 1".split(),
            stdout=log2,
        )
    with open("log2.log", "r") as log2:
        for line in log2:
            if line.startswith("Reverted"):
                r2 += 1
            elif line.startswith("Executed"):
                e2 += 1
    with open("log1.log", "w") as log1:
        subprocess.run(
            args="stack run -- /home/hengdiye/datasets/cgt/source/Modified_fbee9d7a84ce6a1b85f934d965db4c0b.sol --contract Slot --test-mode exploration --format text --crytic-args --solc=/home/hengdiye/.solcix/artifacts/solc-0.4.25/solc-0.4.25 --timeout 10000 --workers 1".split(),
            stdout=log1,
        )
    with open("log1.log", "r") as log1:
        for line in log1:
            if line.startswith("Reverted"):
                r1 += 1
            elif line.startswith("Executed"):
                e1 += 1


print(f"r1: {r1}")
print(f"e1: {e1}")
print(f"r2: {r2}")
print(f"e2: {e2}")
