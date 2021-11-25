input_dict = {
    1: 1,
    2: {
        10: 10,
        20: {
            100: 100
        },
        30: 30
    }
}

def iterdict(d, out_list, loop_idx=0):
    