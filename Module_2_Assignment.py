"""
<Project Title>


Copyright (c) 2021 -- This is the 2021 Spring B version of the Template
Licensed
Written by Tanya Fitkiriwala <---- PLEASE, WRITE YOUR NAME HERE

# you can also rely on the docstring documentation from pandas on how to format dosctrings:
# https://pandas.pydata.org/pandas-docs/stable/development/contributing_docstring.html

"""
import json
import pandas as pd

from pathlib import Path
from pprint import pprint

def read_csv(filepath: Path, separator: str | None = ',', dtype: type | dict=None, index_col: int | str=None) -> pd.DataFrame:
    """
    Read a csv file and return a pandas DataFrame.

    Parameters
    ----------
    filepath : Path
        The path of the csv file. 
    separator : str, default ','
        The separator that is used in the csv file, using ',' as default.
    dtype : type, default None
        Data type for data or columns.
        E.g. str, int, float, np.float64, np.int32, Int32, etc.
        Or we can pass column name with its datatype in a dictionary
        E.g. {'a': np.float64, 'b': np.int32, 'c': 'Int64'}
    index_col : int, str, sequence of int / str, or False, optional, default None
        Column(s) to use as the row labels of the DataFrame,
        either given as string name or column index.
        If a sequence of int / str is given, a MultiIndex is used.
        
         Returns
    -------
    pd.DataFrame
        A pandas DataFrame with the contents of the csv file.
    
    Raises
    ------
    Exception
        Raises generic Exception()
    """

    if filepath.suffix != '.csv': raise Exception('File not of type .csv')
    
    return pd.read_csv(
        filepath,
        sep=separator,
        dtype=dtype,
        index_col=index_col
    )

def read_txt(filepath: Path, separator: str | None = ',', dtype: type | dict=None, index_col: int | str=None) -> pd.DataFrame:
    """
    Read a txt file and return a pandas DataFrame.

    Parameters
    ----------
    filepath : Path
        The path of the txt file. This could be either relative or absolute path.
    separator : str, default ','
        The separator that is used in the txt file, using ',' as default.
    dtype : type, default None
        Data type for data or columns.
        E.g. str, int, float, np.float64, np.int32, Int32, etc.
        Or we can pass column name with its datatype in a dictionary
        E.g. {'a': np.float64, 'b': np.int32, 'c': 'Int64'}
    index_col : int, str, sequence of int / str, or False, optional, default None
        Column(s) to use as the row labels of the DataFrame,
        either given as string name or column index.
        If a sequence of int / str is given, a MultiIndex is used.

    Returns
    -------
    pd.DataFrame
        A pandas DataFrame with the contents of the txt file.
    
    Raises
    ------
    Exception
        Raises generic Exception()
    """
    
    if filepath.suffix != '.txt': 
        raise Exception('File not of type .txt')
    
    return pd.read_csv(
        filepath,
        sep=separator,
        dtype=dtype,
        index_col=index_col
    )


def read_json(filepath: Path, return_type: pd.DataFrame | dict=pd.DataFrame) -> pd.DataFrame | dict:
    """
    Read a json file and return a pandas DataFrame or a python dictionary.

    Parameters
    ----------
    filepath : Path
        The path of the json file. This could be either relative or absolute path.
    return_type : type, default pd.DataFrame
        The datatype in which you want the json file be loaded into. A python dictionary or a pandas DataFrame.

    Returns
    -------
    dict, pd.DataFrame
        A python dictionary or a pandas DataFrame with the contents of the json file.
    
    Raises
    ------
    Exception
        Raises generic Exception()
    """
    if filepath.suffix != '.json': raise Exception('File not of type .json')

    if return_type == dict:
        return json.loads(
            open((filepath), 'rb').read()
        )
    elif return_type == pd.DataFrame:
        return pd.read_json(
            filepath
        )


def read_xlsx(filepath: Path, sheet_name: str='Sheet 1', skiprows: int=None, usecols: str | list=None) -> pd.DataFrame:
    """
    Read a xlsx file and return a pandas DataFrame.

    Parameters
    ----------
    filepath : Path
        The path of the xlsx file. This could be either relative or absolute path.
    sheet_name : str, default 'Sheet 1'
        The name of worksheet where the data is located
    skiprows : int, default None
        Number of rows to skip
    usecols : str or list, default None
        Which columns to get the data from. E.g.: 'M, AA:AS'

    Returns
    -------
    pd.DataFrame
        A pandas DataFrame with the contents of the xlsx file.
    
    Raises
    ------
    Exception
        Raises generic Exception()
    """
    
    if filepath.suffix != '.xlsx': raise Exception('File not of type .xlsx')

    return pd.read_excel(
        filepath,
        sheet_name=sheet_name,
        skiprows=skiprows,
        usecols=usecols
    )


def dict_func(dictionary: dict, input_key: str) -> list:
    """
    This function does three thing:
    1. Converts all the elements in the dict values to upper case and prints it.
    2. Checks whether an "input_key" exists in the dictionary (prints True or False).
    3. Converts the dictionary to a list of key, value tuple pairs and returns it.

    Parameters
    ----------
    dictionary : dict
        A generic python dictionary
    input_key : str
        A key that is contained in the dict dictionary passed to the function

    Returns
    -------
    list
        A list of key, value tuple pairs (list of named tuples) contained in the dict dictionary.
    """

    # Convert all the elements in the dict values to upper case and print it
    print(f'\nUppercased Values in the Dictionary:\n{[[value.upper() for value in list] for list in dictionary.values()]}\n')

    # Check whether an "input_key" exists in the dictionary (print True or False)
    print(f'input_key in dictionary: {input_key in dictionary.keys()}\n')

    # Convert the dictionary to a list of key, value tuple pairs and return it
    return list(dictionary.items())


if __name__ == "__main__":
    # Main functions to Run
    neural_csv = read_csv(Path('./Neural_data.csv'), index_col='Number_Object_Number')
 

    network_txt = read_txt(Path('./network_data.txt'), separator='|')
    )

    nested_json = read_json(Path('./nested_data.json'), return_type=pd.DataFrame)
   
    nested_json = read_json(Path('./nested_data.json'), return_type=dict)

    excel_xlsx = read_xlsx(Path('./Excel_report.xlsx'), sheet_name='financials', skiprows=24, usecols='M, AA:AD, AH:AJ, AL:AM, AO:AS')
 
    Canada = {
        'Indigenous groups': [
            'First Nations',
            'Inuit',
            'Métis',
            'Anishinaabe',
            'Algonquin'
        ],
        'National parks': [
            'Wood Buffalo',
            'Quttinirpaaq',
            'Nahanni',
            'Sirmilik',
            'Prince Albert'
        ],
        'Provinces': [
            'Ontario',
            'Quebec',
            'Nova Scotia',
            'Manitoba',
            'British Columbia',
            'Alberta'
        ]
    }

    items = dict_func(Canada, 'Indigenous groups')
