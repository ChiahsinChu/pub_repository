input_dict = {
    1: 2,
    3: {
        10: 11,
        20: {
            100: 101
        },
        30: 31
    }
}

def iterdict(input_dict, out_list, loop_idx):
    for k,v in input_dict.items():
        k=str(k) # cast key into string
        #if value is dictionary
        if isinstance(v, dict):
            out_list.insert(-1-loop_idx, k)
            out_list.insert(-1-loop_idx, k)
            iterdict(v, out_list, loop_idx+1)
        else:
            v = str(v)
            out_list.insert(-1-loop_idx, k)
            out_list.insert(-1-loop_idx, v)


out_list = ["\n", "\n"]
iterdict(input_dict, out_list, 0)
print(out_list)