import subprocess
from statistics import mean

if __name__ == '__main__':
    datasets = [15000, 30000, 60000]
    thread_counts = [1, 2, 4, 8, 12, 16, 24, 32, 48, 64]
    out = []

    for ds in datasets:
        print(f'Running dataset: {ds}')
        out.append(30*'=' + f"\nDataset: {ds}\n" + 30*'=' + "\n\n")

        result = subprocess.run(['go', 'run', 'go/seqTotientRange/seqTotientRange.go', '1', str(ds)],
                                stdout=subprocess.PIPE)

        out.append(f"Sequential run\n")
        out.append(result.stdout.decode('utf-8'))

        for thread_count in thread_counts:

            run_times = []

            for run in range(3):
                print(f'Running num threads: {thread_count} | run {run}')
            
                result = subprocess.run(['go', 'run', 'go/totientRange/totientRange.go', '1', str(ds), str(thread_count)],
                                        stdout=subprocess.PIPE)

                result_time = result.stdout.decode('utf-8').split()[-1]
                s = float(result_time[:-1])
                run_times.append(s)

            out.append(f"\nNum threads: {thread_count}\n")
            out.append(f"Elapsed time: {mean(run_times):.10f}s\n")
        
        out.append("\n")

    with open('DPT_coursework_go_logs.txt', 'w+') as file:
        for log in out:
            file.write(log)
