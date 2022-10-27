import sys
import pandas as pd
import re

if __name__ == "__main__":
    file_path = sys.argv[1]
    option = sys.argv[2]
    col_maps = sys.argv[3]

    file_df = pd.read_csv(file_path, index_col="record_id")
    
    if option == "map":
        col_maps = col_maps.split(",")
        for vals in col_maps:
            map = vals.split(":")
            orig = map[0]
            new = map[1]
            file_df = file_df.rename(columns=lambda s: re.sub("^" + orig + "_", new + "_", s))
    elif option == "replace":
        file_df.columns = col_maps.split(",")
    file_df.to_csv(file_path)