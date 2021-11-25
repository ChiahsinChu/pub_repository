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

def iterdict(input_dict, out_list=["\n", "\n"], loop_idx=0):
    for k,v in input_dict.items():
        k=str(k) # cast key into string
        #if value is dictionary
        if isinstance(v, dict):
            out_list.insert(-1-loop_idx, k)
            out_list.insert(-1-loop_idx, "end "+k)
            iterdict(v, out_list, loop_idx+1)
        elif isinstance(v, list):
            n_repeat = len(v)
            break
            for i in range(n_repeat):
                for _k, _v in input_dict.item():
        else:
            v = str(v)
            out_list.insert(-1-loop_idx, k+" "+v)
            #out_list.insert(-1-loop_idx, v)
    if isinstance()
    return out_list

out_list = iterdict(input_dict)
print(out_list)