import pandas as pd

convert = {
    'Jan': 1,
    'Feb': 2,
    'Mar': 3,
    'Apr': 4,
    'May': 5,
    'Jun': 6,
    'Jul': 7,
    'Aug': 8,
    'Sep': 9,
    'Oct': 10,
    'Nov': 11,
    'Dec': 12
}

revert = {str(v): k for k, v in convert.items()}

def char_to_num(df):
    """
    INPUT:
    - df: pandas dataframe to be converted

    converts third column (month-year) into a numerical value
    """
    df['Month'] = df['Month'].apply(lambda x: int(x[-4:] + str(convert[x[:3]])))

def num_to_char(df):
    """
    INPUT:
    - df: pandas dataframe to be converted

    converts third column (month-year) back into the original format
    """
    df['Month'] = df['Month'].apply(lambda x: str(revert[str(x)[4:5]]) + str(x)[:4])

if __name__ == "__main__":
    import pandas as pd
    data = {
    'Month': ['Jan2004', 'Feb2004', 'Mar2004', 'Apr2004', 'May2004', 'Jun2004', 'Jul2004', 'Aug2004']
    }

    print(convert,'\n',revert)
    df = pd.DataFrame(data)
    print(df,"\n")
    char_to_num(df)
    print(df,"\n")
    num_to_char(df)
    print(df)