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

def iterdict(input_dict, out_list=["\n","\n"], loop_idx=0):
    for k,v in input_dict.items():
        k=str(k) # cast key into string
        #if value is dictionary
        if isinstance(v, dict):
            out_list.insert(k, -1-loop_idx)
            out_list.insert(k, -1-loop_idx)
            iterdict(v, out_list, loop_idx+1)
        else:
            out_list.insert(k, -1-loop_idx)
            out_list.insert(v, -1-loop_idx)


out_list = ["\n", "\n"]
iterdict(input_dict, out_list)
print(out_list)