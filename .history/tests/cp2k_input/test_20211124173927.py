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

def iterdict(input_dict, out_list=["\n","\n"], loop_idx=0):
    for k,v in input_dict.items():
        k=str(k) # cast key into string
        #if value is dictionary
        if isinstance(v, dict):
            out_list.insert(k, -1-loop_idx)
            out_list.insert(k, -1-loop_idx)
                iterdict(v, out_list, k)
            ddd 
        else:
            ddd



    return out_list