input_dict = {
    1: 2,
    3: {
        10: 11,
        20: {
            100: 101
        },
        30: 31,
        40:{
            100: [1000, 2000, 3000],
            200: [1000, 2000, 3000]
        },
        100: 101
    }
}

def iterdict(input_dict, out_list=["\n", "\n"], loop_idx=0):
    n_repeat = -1
    for k,v in input_dict.items():
        k=str(k) # cast key into string
        #if value is dictionary
        if isinstance(v, dict):
            out_list.insert(-1-loop_idx, "start "+k)
            out_list.insert(-1-loop_idx, "end "+k)
            iterdict(v, out_list, loop_idx+1)
        elif isinstance(v, list):
            n_repeat = len(v)
            print(loop_idx)
            print(input_dict)
            print(n_repeat)
            break
        else:
            v = str(v)
            out_list.insert(-1-loop_idx, k+": "+v)
            #out_list.insert(-1-loop_idx, v)
    if n_repeat >= 0 :
        tmp_str_2 = out_list[-loop_idx]
        del out_list[-loop_idx]
        tmp_str_1 = out_list[-loop_idx]
        del out_list[-loop_idx]
        for i in range(n_repeat):
            tmp_dict = {}
            for k, v in input_dict.items():
                k=str(k)
                tmp_dict[k] = v[i]
            out_list.insert(-loop_idx, tmp_str_1)
            out_list.insert(-loop_idx, tmp_str_2) 
            out_list.insert(-1-loop_idx, k)
            out_list.insert(-1-loop_idx, "end "+k)
            iterdict(tmp_dict, out_list, loop_idx+1)
    return out_list

out_list = iterdict(input_dict)
print(out_list)